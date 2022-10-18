library(data.table)
library(dplyr)
library(stringr)
library(MapData)
library(sf)
library(raster)
library(ggpmisc)
library(ggpubr)
library(ggplot2)
library(cowplot)
load("Data/Maize_shp.RData")
load("Data/Study_cty.RData")
if (F) {
  source("Script/00_Helper_function.R")
  load("Data/InputData/st840.rda")
  
  obs.path = "G:/ClimateData/CMA/02_ValidatedData/BySite"
  
  file = dir(obs.path) %>% sprintf("%s/%s", obs.path, .) %>% 
    str_subset(pattern = st840$site %>% paste(collapse = "|"))
  
  st <- file %>% str_extract("\\d{5}") %>% as.integer()
  
  site <- st840[site %in% st] %>% 
    .[,`:=`(x = lon, y = lat)] %>% 
    st_as_sf(coords = c("x", "y"), crs = 4326)
  
  load("Data/Maize_shp.RData")
  load("Data/Study_cty.RData")
  
  maize_county
  sts <- maize_county %>% 
    filter(id03 %in% id) %>% 
    st_covers(site) %>% 
    unlist() 
  
  site <- site %>% slice(sts)
  sts <- site$site
  
  file <- file %>% str_subset(pattern = paste(sts, collapse = "|"))  
  
  library(snowfall)
  library(doParallel)
  cl = makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  
  dt_site <- foreach(i = file, .packages = c("data.table", "dplyr", "stringr")) %dopar% {
    fread(i) %>% 
      .[, .(date, pr, et0)] %>% 
      .[, `:=`(year = year(date),  month = month(date))] %>% 
      .[, .(pr = sum(pr), et0 = sum(et0)), .(year, month)] %>% 
      .[year %in% 1981:2014] %>% 
      .[,site := as.integer(str_extract(i, "\\d{5}"))]
  } %>% 
    rbindlist()
  
  
  df_rs_pet <- stack("Data/00_OriginalData/cru_ts4.02.1901.2017.pet.dat.nc") %>% 
    {subset(., grep(pattern = paste(1981:2014, collapse = "|"), x = names(.)))} %>% 
    extract(site) %>% 
    data.table() %>% 
    .[, site := site$site] %>% 
    melt(id.vars = "site") %>% 
    .[, variable := as.Date(variable, format = "X%Y.%m.%d")] %>% 
    .[, dys := lubridate::days_in_month(variable)] %>% 
    .[, `:=`(year = year(variable), month = month(variable),
             value = value * dys)] %>% 
    rename(et0_rs = value) %>% 
    select(-variable, -dys)
  
  df_rs_pr <- stack("Data/00_OriginalData/cru_ts4.02.1901.2017.pre.dat.nc", varname = "pre") %>% 
    {subset(., grep(pattern = paste(1981:2014, collapse = "|"), x = names(.)))} %>% 
    extract(site) %>% 
    data.table() %>% 
    .[, site := site$site] %>% 
    melt(id.vars = "site") %>% 
    .[, variable := as.Date(variable, format = "X%Y.%m.%d")] %>% 
    .[, `:=`(year = year(variable), month = month(variable))] %>% 
    rename(pr_res = value) %>% 
    select(-variable)
  
  df_rs <- merge(df_rs_pr, df_rs_pet, by = c("site", "year", "month"))
  
  df_comp <- merge(dt_site, df_rs, by = c("site", "year", "month"))
  
  p1 <- ggplot(df_comp[pr < 600], aes(x = pr, pr_res)) +
    geom_point(alpha = 0.3) +
    scale_x_continuous(limits = c(0, 600)) +
    scale_y_continuous(limits = c(0, 600)) +
    geom_abline(intercept = 0, slope = 1) +
    stat_smooth(method = "lm", se = F, linetype = 2) +
    stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
    coord_fixed(ratio = 1) +
    theme_basic() +
    labs(x = "Observed precipitation (mm)", y = "CRU precipitation (mm)")
  
  p2 <- ggplot(df_comp, aes(x = et0, et0_rs)) +
    geom_point(alpha = 0.5) +
    scale_x_continuous(limits = c(0, 250)) +
    scale_y_continuous(limits = c(0, 250)) +
    geom_abline(intercept = 0, slope = 1) +
    stat_smooth(method = "lm", se = F, linetype = 2) +
    stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
    coord_fixed(ratio = 1) +
    theme_basic() +
    labs(x = "Observed PET (mm)", y = "CRU PET (mm)")
  
  # pf <- plot_grid(p1, p2, nrow = 1, align = "hv")  
  # make_fig(figname = "EUJAFigSS", height = 4, width = 7)
  # print(pf)
  # dev.off()
  
  
  
  rs_pr <- stack("Data/00_OriginalData/cru_ts4.02.1901.2017.pre.dat.nc", varname = "pre") %>% 
    {subset(., grep(pattern = paste(1981:2010, collapse = "|"), x = names(.)))} %>% 
    stackApply(indices = rep(1:30, each = 12), sum) %>% 
    calc(mean) %>% 
    crop(chn01) %>% 
    mask(chn01)
  
  rs_pet <- stack("Data/00_OriginalData/cru_ts4.02.1901.2017.pet.dat.nc") %>% 
    {subset(., grep(pattern = paste(1981:2010, collapse = "|"), x = names(.)))} %>% 
    crop(chn01) %>% 
    mask(chn01) %>% 
    rasterToPoints() %>% 
    data.table() %>% 
    melt(id.vars = c("x", "y")) %>% 
    .[, variable := as.Date(variable, format = "X%Y.%m.%d")] %>% 
    .[, dys := lubridate::days_in_month(variable)] %>% 
    .[, `:=`(year = year(variable), month = month(variable),
             value = value * dys)] %>% 
    .[,.(value = sum(value)), .(x, y, year)] %>% 
    .[,.(value = mean(value)), .(x, y)] %>% 
    rasterFromXYZ(crs = 4326)
  
  rs_pet <- extend(rs_pet, rs_pr)
  
  rs_wat <- rs_pr - rs_pet
  rs_wat <- rs_wat %>% mask(maize_shp)
  library(stars)
  rs_wat <- projectRaster(rs_wat, crs = MapData::crs) %>% 
    st_as_stars()
  
  p3 <- ggplot() +
    geom_stars(data = rs_wat) +
    geom_sf(data = bdy, mapping = aes(), lwd = 0.3) +
    geom_sf(data = maize_shp, mapping = aes(geometry = geometry), lwd = 0.3, fill = NA) +
    geom_sf(data = site, mapping = aes(), size = 0.5) +
    coord_sf(crs = MapData::crs) +
    scale_fill_stepsn(colours = pals::brewer.brbg(11), breaks = c(-1100,seq(-1000, 600, 200), 700), na.value = NA,
                      limits = c(-1100, 700),  guide = guide_coloursteps(even.steps = T, show.limits = T)) +
    # guides(fill = guide_legend(title.position = "top", title.hjust = 0.5,label.position = "bottom", nrow = 1)) +
    scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
    scale_y_continuous(limits = c(2195234.1, 5921292.3)) +
    theme_map() + 
    ggtitle("Climate water balance (mm)") +
    theme(title = element_text(hjust = 0.5)) +
    
    # theme(legend.position = "bottom", legend.text = element_text(colour = "black", size = 12, angle = 45, vjust = 0)) +
    labs(x = NULL, y = NULL, fill = NULL) 
  
  pf1 <- plot_grid(p1, p2)
  pf2 <- plot_grid(pf1, p3, ncol = 1, labels = c("(a)", "(b)"),label_size = 12, label_fontface = "plain")
  make_fig(figname = "EJAFigS2", height = 8, width = 9)
  print(pf2)
  dev.off()
  doParallel::stopImplicitCluster()
}
library(terra)
maize_county <- maize_county %>% filter(id03 %in% id) %>% vect()
rarea <- terra::rast('Data/ReviseInput/gmia_v5_aei_ha.asc')
# rarea <- raster(rarea)
rgw <- terra::rast("Data/ReviseInput/1_7_aei_gw/pct_aei2_gw/w001001.adf") %>% 
  resample(rarea) %>% 
  crop(maize_county)
rarea <- crop(rarea, maize_county)

{
  library(stars)
  ra <- mask(rarea, maize_county) %>% 
    raster() %>% 
    projectRaster(crs = MapData::crs) %>% 
    st_as_stars()
  rg <- mask(rgw, maize_county) %>% 
    raster() %>% 
    projectRaster(crs = MapData::crs) %>% 
    st_as_stars()
  breaks =  c(seq(0, 1000, 200), 1000, 1500, 2000, 3000, 4000, 5000, 6000)
  p1 <- ggplot() +
    geom_stars(data = ra) +
    geom_sf(data = maize_shp, mapping = aes(), fill = NA, lwd = 0.5) +
    geom_sf(data = bdy, mapping = aes(), lwd = 0.5) +
    scale_fill_stepsn(colours = pals::brewer.greens(50), breaks = breaks,
                         limits = c(0, 7000), na.value = NA, 
                      guide = guide_colorsteps(show.limits = T, ticks = T, frame.colour = "black", title.position = "left", ticks.colour = "black")) +
    coord_sf(crs = MapData::crs) +
    # guides(fill = guide_legend(title.position = "left")) +
    scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
    scale_y_continuous(limits = c(2195234.1, 5921292.3)) +
    theme_basic() +
    theme(legend.key.height = unit(1.2, 'cm'), legend.title = element_text(angle = 90, hjust = 0.5)) +
    labs(x = NULL, y = NULL, fill = "Irrigation areas (ha)")
  
  breaks  = seq(0, 90, 10)
  p2 <- ggplot() +
    geom_stars(data = rg) +
    geom_sf(data = maize_shp, mapping = aes(), fill = NA, lwd = 0.5) +
    geom_sf(data = bdy, mapping = aes(), lwd = 0.5) +
    scale_fill_stepsn(colours = pals::brewer.greens(50), breaks = breaks,
                      limits = c(0, 85), na.value = NA, 
                      guide = guide_colorsteps(show.limits = T, ticks = T, frame.colour = "black", title.position = "left", ticks.colour = "black")) +
    coord_sf(crs = MapData::crs) +
    # guides(colour = guide_legend(keyheight = unit(4, "in"))) +
    scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
    scale_y_continuous(limits = c(2195234.1, 5921292.3)) +
    theme_basic() +
    theme(legend.key.height = unit(1.2, 'cm'), legend.title = element_text(angle = 90, hjust = 0.5)) +
    labs(x = NULL, y = NULL, fill = "Percentage of irrigated area\n with groundwater (%)")
  pf <- plot_grid(p1, p2, align = "h", labels = c("(a)", "(b)"), label_size = 12, label_fontface = "plain")
  make_fig(figname = "EJAFigS3", width = 10, height = 5)
  print(pf)
  dev.off()

}

# rgw <- raster(rgw)
library(terra)
rs_area <- extract(rarea, maize_county, sum, na.rm = TRUE)
rs_gw <- extract(rarea*rgw, maize_county, sum, na.rm = T)/100

result_irrigation <- cbind(rs_area[,2], rs_gw[,2]) %>% 
  data.table() %>% 
  setNames(c("irri", "gdw")) %>% 
  cbind(maize_county %>% st_as_sf(), .)

acty <- st_area(result_irrigation) %>% as.vector() %>% {./10000}

result_irrigation <- result_irrigation %>% mutate(acty = acty) %>% 
  mutate(irri = irri/acty, gdw = gdw/acty) %>%
  st_set_geometry(value = NULL) %>% 
  select(id03, irri, gdw, zone)


save(list = "result_irrigation", file = "Data/R1OutputData/result_irrigation.RData")

load("Data/R1OutputData/Result_copula.RData", verbose = T)
load("Data/R1OutputData/Result_prob1.RData", verbose = T)
load("Data/R1OutputData/Result.RData", verbose = T)

zones %>% mutate(zone = "Whole") %>% 
  bind_rows(zones) %>% 
  mutate(zone = factor(zone, levels = c("Whole","NCSM", "HHHSM", "SWCM")))

maize_county$zone <- factor(maize_county$zone, levels = c("North_SpringMaize", "Huanghuahai_SummerMaize", "Southwest_Maize",
                                                          "Northwest_Maize", "South_Maize", "Tibetan_Maize"))

rs <- result %>% 
  merge(result_irrigation, by = "id03") %>% 
  data.table() %>%
  slice(str_which(type, "Dry"))

p <- rbind(rs, rs %>% mutate(zone = "Whole")) %>%
  .[, .(cor = cor(prob, irri), pvalue =  cor.test(prob, irri)$p.value), .(type, zone)] %>% 
  mutate(type = str_replace(type, "D", "d")) %>% 
  mutate(zone = factor(zone, c("Whole", "North_SpringMaize", "Huanghuahai_SummerMaize", "Southwest_Maize"), c("Whole", "NCSM", "HHHSM", "SWCM")),
         type = factor(type, c("Moderately dry", "Severely dry", "Extreme dry"), c(c("Moderately dry", "Severe dry", "Extreme dry"))),
         pvalue = ifelse(pvalue < 0.05, ifelse(pvalue < 0.01, "**", "*"), "")) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = zone, y = cor, fill = type), stat = "identity", 
           position = position_dodge2()) +
    geom_text(aes(x = zone, y = cor - 0.01, group = type, label = pvalue), position = position_dodge(width = 0.9)) +
    scale_fill_manual(values = c("#2C7BB6", "#ABD9E8", "#FDAE61")) +
    scale_y_continuous(limits = c(-0.26, 0), expand = c(0,0)) +
    theme_basic() +
    theme(legend.position = c(1,0), legend.justification = c(1,0)) +
    labs(x = NULL, y = "Correlation coefficient", fill = NULL)

make_fig(figname = "EJAFigS8", width = 5, height = 4.5)
print(p)
dev.off()
