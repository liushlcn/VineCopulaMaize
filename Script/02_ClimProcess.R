## Helper Functions
## Author: Shengli Liu @ ZZU
## Email:  liushlcn@gmail.com
## Date:   2022/06/27

rm(list = ls())
source("./Script/00_Helper.R")
source("./Script/00_Functions.R")

cl <- detectCores() - 1
registerDoParallel(cl = cl)
runs <- TRUE

# Step 01 Match sites over counties -------------------------------------------------------------------------------

load("Data/00_AOI.RData")

files.hist <- list.files(path = path$histclim, pattern = glob2rx("*.txt.gz"), full.names = T)
site <- siteinfo %>%
  .[, `:=`(x = lon, y = lat, Iruns = 1:.N)] %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)

sts <- sapply(st_covers(aoi, site), str_c, collapse = "|")

sts[nchar(sts) == 0] <- aoi %>%
  filter(nchar(sts) == 0) %>%
  st_centroid() %>%
  st_distance(x = ., site) %>%
  units::drop_units() %>%
  apply(1, which.min) %>%
  {
    site[., ]
  } %>%
  pull(Iruns)

aoi$Iruns <- sts

sts <- aoi$Iruns %>%
  str_split(pattern = "[|]") %>%
  unlist() %>%
  as.integer() %>%
  unique()
site <- site %>% filter(Iruns %in% sts)
save(list = "site", file = "Data/00_Site.RData")

# Step 02 Extract critical period for maize -----------------------------------------------------------------------
if (runs) {
  files.cale <- path$calendar %>%
    list.files(pattern = "tif$") %>%
    str_subset("CHN_Maize_HE") %>%
    str_subset(pattern = str_c(2000:2014, collapse = "|")) %>%
    paste0(path$calendar, .)
  foreach(x = files.cale, .packages = c("dplyr", "stringr")) %dopar% {
    mkdirs("./temp")
    system(paste0("wsl gdalwarp -t_srs EPSG:4326 '", path.mnt(x), "' ./temp/", str_remove_all(basename(x), "[(|)|&]")))
  }

  files.cale <- list.files("./temp/", full.names = T)
  headday <- extract(rast(files.cale), vect(aoi), mean, na.rm = T)
  ## s1 = extract(rast(files.cale), vect(aoi), sd, na.rm = T)
  # check the phenology data over counties
  apply(headday[, -1], 1, na.count) %>%
    table() %>%
    barplot()
  headday <- cbind(
    apply(headday[, -1], 1, min, na.rm = T),
    apply(headday[, -1], 1, max, na.rm = T),
    apply(headday[, -1], 1, median, na.rm = T)
  )
  sts <- st_distance(
    aoi %>% filter(!complete.cases(headday)) %>% st_centroid(),
    aoi %>% filter(complete.cases(headday)) %>% st_centroid()
  ) %>%
    units::drop_units() %>%
    apply(MARGIN = 1, which.min)
  # note sts here stands for the complete headdays
  headday[!complete.cases(headday), ] <- headday[complete.cases(headday), ][sts, ]
  headday <- headday %>%
    data.table() %>%
    .[, lapply(.SD, floor)] %>%
    setNames(c("hdmin", "hdmax", "hdavg")) %>%
    .[, id03 := aoi$id03]
  save(list = "headday", file = "Data/00_HeadDate.RData")
  unlink("./temp/")
} else {
  load(file = "Data/00_HeadDate.RData", verbose = T)
}

aoi <- merge(aoi, headday)

# Step 02 Calculate climate indices over counties -----------------------------------------------------------------
make_calc <- function(file, plant, harvest, hdate) {
  dt <- fread(file) %>%
    .[year(date) %in% 1981:2014] %>%
    .[, `:=`(year = year(date), month = month(date), day = yday(date))]

  merge(
    dt[month >= plant & month <= harvest] %>%
      .[, .(
        tas = mean(tas, na.rm = T),
        tasmax = mean(tasmax, na.rm = T),
        tasmin = mean(tasmin, na.rm = T),
        pr = sum(pr, na.rm = T),
        et0 = sum(et0, na.rm = T)
      ), .(year)],
    dt[day > hdate - 20 & day < hdate + 20] %>%
      .[, .(
        hdy = hdd_days(tasmax),
        hdi = hdd_intensity(tasmax),
        hdx = hdd_max(tasmax),
        hds = hdd_severity(tasmax)
      ), .(year)]
  )
}


if (runs) {
  dta_clim <- foreach(
    i = 1:nrow(aoi), .inorder = FALSE, .combine = rbind,
    .packages = c("data.table", "dplyr", "stringr", "sf")
  ) %dopar% {
    file <- aoi$Iruns[i] %>%
      str_split(pattern = "[|]") %>%
      unlist() %>%
      as.integer() %>%
      {
        site[site$Iruns %in% ., "site"]
      } %>%
      pull(site) %>%
      paste(collapse = "|") %>%
      str_subset(files.hist, pattern = .)
    lapply(file, make_calc, plant = aoi$plant[i], harvest = aoi$harvest[i], hdate = aoi$hdavg[i]) %>%
      rbindlist() %>%
      .[, lapply(.SD, mean), .(year), .SDcols = !"year"] %>%
      .[, id03 := aoi$id03[i]]
  } %>% 
    .[, pet := pr - et0]
  dta_stats <- merge(
    dta_clim[, ldply(lapply(.SD, mean), .id = "variable"), .(id03), .SDcols = !c("id03", "year")] %>%
      setnames("V1", "value"),
    dta_clim[, ldply(lapply(.SD, mkTrend), .id = "variable"), .(id03), .SDcols = !c("id03", "year")],
    by = c("id03", "variable")
  )

  save(list = c("dta_clim", "dta_stats"), file = "Data/01_ClimInf.RData")
} else {
  load("Data/01_ClimInf.RData")
}

stopImplicitCluster()






# library(snowfall)
# sfInit(parallel = T, cpus = detectCores() - 1)
# sfLibrary(data.table)
# sfLibrary(dplyr)
# sfLibrary(stringr)
# sfLibrary(sf)
#
# sfExport("aoi")
# sfExport("files.hist")
# sfExport("site")
# sfExport("make_calc")
# sfSource("Script/02_function_heatstress.R")
#
# sfLapply(x = 1:5, fun = function(i, files, aoi) {
#   files = files.hist
#   irun <- aoi$Iruns[i] %>%
#     str_split(pattern = "[|]") %>%
#     unlist() %>%
#     as.integer()
#   file = site %>% filter(Iruns %in% irun) %>%
#     pull(site) %>%
#     paste(collapse = "|") %>%
#     str_subset(files, pattern = .)
#   lapply(file, make_calc, plant = aoi$plant[i], harvest = aoi$harvest[i], hdate = aoi$hdavg[i]) %>%
#     rbindlist() %>%
#     .[, lapply(.SD, mean), .(year), .SDcols = !"year"] %>%
#     .[, id03 := aoi$id03[i]]
# }, files = files.hist, aoi = aoi)
#
# sfStop()
