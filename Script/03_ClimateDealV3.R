## Deal with climate dataset
## Author: Shengli Liu @ agroscope
## Email:  liushlcn@gmail.com
## Date:   2022/12/12
rm(list = ls())
source("Script/00_Helper.R")
source("Script/00_Functions.R")
load("Data/01_AOI_Phenology.RData")
load("Data/00_AOI.RData")


path$clim = "d:/BaiduNetdiskWorkspace/CMA/02_ValidatedData/BySite/"
sts = list.files(path = path$clim) %>% str_extract("\\d{5}") %>% 
  as.numeric()

# system.time({
#   siteinfo[site %in% sts] %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
#     st_intersection(., aoi)
# })
aoi_gcal %>% st_as_sf() %>% 
  ggplot() +
  geom_sf(mapping = aes()) +
  geom_sf(data = siteinfo[site %in% sts] %>% st_as_sf(coords = c("lon", "lat"), crs = 4326), mapping = aes())

siteinfo[site %in% sts] %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_covers(x = aoi, y = .) %>% 
  unlist() %>% length()
siteinfo <- siteinfo[site %in% sts] %>% 
  .[,`:=`(x = lon, y = lat)] %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326)
res = st_distance(siteinfo, aoi) %>%
  drop_units() %>% 
  apply(2, which.min) %>% 
  siteinfo[.,] %>% 
  pull(site)
aoi_site <- aoi %>% mutate(site = res)


# 



