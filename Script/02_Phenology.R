rm(list = ls())

source("Script/00_Helper.R")
load("Data/00_AOI.RData")


# Section 01 read and deal with maize phenology data into data.table ----------------------------------------------

aoi_cale = list.files(path = "G:/Basic/Phenology/Phenology", pattern = "S2019", full.names = T) %>% 
  lapply(fread) %>% 
  rbindlist() %>% 
  # 删除发育日期中的空值，以便后续整理日期
  .[C56003 %!in% c(-999, -9999)] %>% 
  # 提取玉米物候期资料
  .[C56001 %in% c("春玉米", "夏玉米")] %>% 
  # 删除未记录物候信息
  .[C56002 %!in% c("未进", "无", "-9999")] %>% 
  .[, V01000:C56004] %>% 
  .[, c("month", "day") := tstrsplit(C56003, "月")] %>% 
  .[, `:=`(month = as.integer(month), day = as.integer(str_remove(day, "日")), C56003 = NULL)] %>% 
  .[, `:=`(V04002 = NULL, V04202 = NULL)] %>% 
  setNames(c("site", "year", "type", "stage", "prop", "month", "day")) %>% 
  # 严格控制数据 必须为普遍期
  .[prop == "普遍期"] %>% 
  # 删除发育状况，生成日期
  .[, `:=`(doy = yday(make_date(year, month, day)))] %>% 
  .[, `:=`(month = NULL, day = NULL, prop = NULL)] %>% 
  # remove duplicate
  unique()

# each stage with more than to record
# recd <- aoi_cale[, .(.N), .(site, year, type, stage)] %>% 
#   .[N > 1]
# merge(aoi_cale, recd) %>% View()
# after checking data, del the smaller one if duplicate exist

# aoi_cale[, `:=`(N = .N), .(site, year, croptype, stage)]
lbls = c("播种", "出苗", "三叶", "七叶", "拔节", "抽穗", "抽雄", "乳熟", "成熟")
fcts = c("sowing", "seeding", "v3", "v7", "jointing", "heading", "heading", "milky", "maturity")

aoi_cale = aoi_cale[, .(doy = max(doy)), .(site, year, type, stage)] %>% 
  .[, stage := factor(stage, lbls, fcts)] %>% 
  .[year >= 2000] %>% 
  dcast.data.table(site+year+type~stage, value.var = "doy") %>% 
  .[, type := ifelse(type == "春玉米", "spring", "summer")]

## filter records with at least five years and heading date recordered more than five years 
sts = aoi_cale[, .(.N, heading = sum(!is.na(heading)), 
                   maturity = sum(!is.na(maturity))), .(type, site)] %>% 
  .[N >= 5 & heading >= 5 & maturity >= 5] %>% 
  .[, .(site, type)]

aoi_cale = merge(aoi_cale, sts, by = c("type", "site")) %>% 
  .[, .(heading = mean(heading, na.rm = T), 
        maturity = mean(maturity, na.rm = T)), .(type, site)] %>% 
  .[, `:=`(heading = floor(heading),
           maturity = ceiling(maturity))] 

# Section 02 Add spatial attribution and extract for study region --------------------------------------------------

aoi_cale = merge(aoi_cale, siteinfo, by = "site") %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_intersection(., maize_shp) %>%
  data.table() %>% 
  # deal with data drift
  .[zone == "NCSM", heading := heading + 10]

# glance numbers of different maize type over regions
## check the numbers of site for different maize type and keep spring maize
## in NCSM, summer maize in HHHSM, spring and summer maize in SWCM
aoi_cale %>% 
  .[,.(.N), .(type, zone)] %>% 
  ggplot(mapping = aes(x = zone, y = N, fill = type, label = N)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(position = position_dodge(width = 0.85)) +
  scale_y_continuous(limits = c(0, 109), expand = c(0, 0)) +
  labs(x = NULL, y = "Numbers of agrometeorological station", fill = NULL) +
  scale_fill_discrete(labels = c("Spring maize", "Summer maize")) +
  coord_cartesian() +
  theme_basic() +
  theme(legend.position = c(1, 1), legend.justification = c(1,1), legend.key = element_blank())

## get box plot information for different maize over regions
my.summary = function(x) {
  bx = boxplot(x, plot = F)
  return(as.vector(bx$stats))
}

phead_aoi_cale <- aoi_cale %>% 
  .[, as.list(my.summary(heading)), .(type, zone)] %>% 
  setnames(paste0("V", 1:5), c("lower", "q25", "median", "q75", "upper")) %>% 
  .[order(zone, type)] %>% 
  .[, variable := "heading"]

pmat_aoi_cale <- aoi_cale %>% 
  .[, as.list(my.summary(maturity)), .(type, zone)] %>% 
  setnames(paste0("V", 1:5), c("lower", "q25", "median", "q75", "upper")) %>% 
  .[order(zone, type)] %>% 
  .[, variable := "maturity"]

p_aoi_cale <- rbindlist(list(phead_aoi_cale, pmat_aoi_cale))

p_aoi_cale[type == "summer" & zone == "NCSM", lower := NA]
p_aoi_cale[type == "spring" & zone == "HHHSM", lower := NA]
p_aoi_cale <- na.omit(p_aoi_cale)


# Section 03 Extract large scale heading date over regions and types ----------------------------------------------
# extract heading and maturity date
phen_date <- list()
for (i in c("HE", "MA")) {
  # find files and rast
  aoi_glass <- list.files(path = path$calendar, pattern = "Maize") %>% 
    grep(pattern = paste0(2000:2014, collapse = "|"), value = T) %>% 
    grep(pattern = i, value = T) %>% 
    paste0(path$calendar, .) %>% 
    rast()
  # Extract using weight_mean by using area
  chn_gcal <- exact_extract(x = aoi_glass, y = st_transform(chn03, st_crs(aoi_glass)), fun = "weighted_mean", weights = "area") %>% 
    data.table() %>% 
    .[, id03 := chn03$id03] %>% 
    melt.data.table(id.vars = "id03") %>% 
    .[, .(value = mean(value, na.rm = T)), .(id03)] %>% 
    .[, value := ceiling(value)] %>% 
    merge(chn03, by = "id03") %>% 
    .[, .(id03, value, lon, lat, geometry)]
   ## find neighbors of counties with NA
   ids = aoi$id03
   aoi_gcal = chn_gcal[id03 %in% ids, .(id03, value)] %>% 
     merge(aoi, ., by = "id03") %>% 
     data.table()
   # fill NA with its neighbor
   nans <- which(is.na(aoi_gcal$value)) 
   nebs <-  st_intersects(st_as_sf(aoi_gcal[nans,]), st_as_sf(chn_gcal))
   vals <- sapply(1:length(nans), function(x) {
     floor(chn_gcal[nebs[[x]], mean(value, na.rm = T)])
   })
   aoi_gcal[nans, value := vals]
   phen_date[[i]] <- aoi_gcal %>% pull(value)
}
aoi_gcal$heading = phen_date$HE
aoi_gcal$maturity = phen_date$MA
aoi_gcal$value = NULL

# ## class counties as spring or summer maize
# aoi_gcal[, type := "summer"]
# aoi_gcal[zone == "NCSM", type := "spring"]
# aoi_gcal[zone  == "SWCM" & heading <= 181, type := "spring"]
# aoi_gcal[, mon := "8"]
# aoi_gcal[zone == "SWCM" & type == "spring", mon := 6]
# aoi_gcal[zone == "SWCM" & type == "summer", mon := 7]
# aoi_gcal[zone == "NCSM", mon := 7:8]

save(list = c("aoi_cale", "aoi_gcal", "p_aoi_cale"), file = "./Data/01_AOI_Phenology.RData")

aoi_gcal[, value := maturity - heading] %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = value)) +
  scale_fill_stepsn(breaks = seq(30, 80, 10), colors = pals::brewer.ylgnbu(5))

