# file <- list.files(path = "X:/DATA/Harea/jbs44b2hrk-2", pattern = "*.tif$", full.names = T) %>% 
#   str_subset(pattern = "Maize")
# year <- as.integer(str_extract(file, "\\d{4}"))
# pts <- which(year %in% 2006:2010)
# file <- file[pts]
# dir.create("Data/AuxiliaryData", recursive = T)
# lapply(file, function(x) file.copy(x, to = "Data/AuxiliaryData/"))


library(raster)
file <- list.files(path = "Data/AuxiliaryData/", pattern = "*.tif$", full.names = T)
rast <- stack(file)
rast <- calc(rast, mean, na.rm = T)
rast <- projectRaster(rast, crs = "+proj=longlat +datum=WGS84 +no_defs")

dta <- maize_county %>% 
  inner_join(yield_spei, by = "id03") 
# Extract
library(exactextractr)
area <- exact_extract(rast, dta, "sum")

dta$area <- area
dta %>% 
  filter(area < 1) %>% 
  ggplot() +
  geom_sf(aes(fill = area)) +
  scale_fill_gradientn(colours = pals::brewer.greens(200)) +
  geom_sf(data = bdy, aes())
