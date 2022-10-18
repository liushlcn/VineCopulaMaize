## Helper Functions
## Author: Shengli Liu @ ZZU
## Email:  liushlcn@gmail.com
## Date:   2022/06/27

rm(list = ls())
source("./Script/00_Helper.R")
source("./Script/00_Functions.R")
load("Data/00_AOI.RData")
# cl <- detectCores() - 1
# registerDoParallel(cl = cl)
# runs <- TRUE
period <- "1980_2014"
years <- 1980:2014
# Step 01 Match sites over counties -------------------------------------------------------------------------------

## transfer string to date
str_todate <- function(x) {
  str_extract(x, "\\d{4}.\\d{2}.\\d{2}") %>%
    str_replace_all("[.]", "-") %>%
    as.Date()
}

## direct use stack crop mask result in NaN values for some counties
# change all the process into "nc - matrxi - raster" 
# cost more time but can be acceptable using "exactextractr" package
temp.file = list.files(path =  path$temp, pattern = "tmx", full.names = T)
nc <- nc_open(filename = temp.file)
varname <- netcdf.calendar(nc = nc, time.variable = "time") %>%
  format("%Y-%m-%d") %>% 
  grep(pattern = paste0(years, collapse = "|"), x = ., value = T)
lyrs <- netcdf.calendar(nc = nc, time.variable = "time") %>%
  format("%Y-%m-%d") %>% 
  grep(pattern = paste0(years, collapse = "|"), x = .)
rotate <- function(x) apply(t(x), 2, rev)
system.time({
  aoi_temp <- lapply(lyrs, FUN = function(i) {
    x <- ncvar_get(nc = nc, nc$var$tmx$name, start = c(1, 1, i), count = c(-1, -1, 1))
    x <- rotate(x)
    x <- raster(x = x, xmn = -180, xmx = 180, ymn = -90, ymx = 90, crs = 4326)
    # x = rast(x = x)
    exactextractr::exact_extract(x, y = aoi, "weighted_mean", weights = "area",  progress = F)
  }) %>%
    do.call(cbind, .) %>%
    data.table() %>% 
    setNames(varname) %>% 
    .[, id03 := aoi$id03] %>% 
    melt.data.table(id.vars = "id03", variable.name = "date", value.name = "tmax") %>% 
    .[, date := str_todate(date)] %>% 
    .[order(id03, date)] %>% 
    .[,`:=`(year = year(date), month = month(date), date = NULL)]
})

nc_close(nc)
# 
# aoi_temp <- list.files(path = path$temp, pattern = "tmx", full.names = T) %>%
#     stack() %>%
#     {subset(., grep(pattern = paste0(years, collapse = "|"), names(.), value = T)) } %>%
#     crop(aoi) %>%
#     mask(aoi) %>%
#     {exactextractr::exact_extract(x = ., y = aoi, "weighted_mean", weights = "area", progress = F)} %>% 
#     data.table() %>% 
#     .[, id03 := aoi$id03] %>% 
#     melt.data.table(id.vars = "id03", variable.name = "date", value.name = "tmx") %>% 
#     .[, date := str_todate(date)] %>% 
#     .[order(id03, date)] %>% 
#   .[,`:=`(year = year(date), month = month(date), date = NULL)]

somi.file <- list.files(path = path$somi, pattern = "root", full.names = T)
nc <- nc_open(filename = somi.file)
varname <- netcdf.calendar(nc = nc, time.variable = "time") %>%
  format("%Y-%m-%d") %>% 
  grep(pattern = paste0(years, collapse = "|"), x = ., value = T)
lyrs <- varname %>%
  grep(pattern = paste0(years, collapse = "|"), x = .)
# Error in { : task 1 failed - "error returned from C call"
# note error for parallel run using foreach
# exactextractr works and more efficient
aoi_somi <- lapply(lyrs, FUN = function(i) {
  x <- ncvar_get(nc = nc, nc$var$SMroot$name, start = c(1, 1, i), count = c(-1, -1, 1))
  x <- raster(x = x, xmn = -180, xmx = 180, ymn = -90, ymx = 90, crs = 4326)
  # x = rast(x = x)
  exactextractr::exact_extract(x, y = aoi, "weighted_mean", weights = "area", progress = F)
}) %>%
  do.call(cbind, .) %>%
  data.table() %>% 
  setNames(varname) %>% 
  .[, id03 := aoi$id03] %>% 
  melt.data.table(id.vars = "id03", variable.name = "date", value.name = "somi") %>% 
  .[, date := str_todate(date)] %>% 
  .[order(id03, date)] %>% 
  .[,`:=`(year = year(date), month = month(date), date = NULL)]

nc_close(nc)

aoi_data <- reduce(.x = list(aoi_temp, aoi_somi), .f = merge, by = c("id03", "year", "month"))

aoi_clim <- aoi_data[month == 8] %>% 
  .[, `:=`(dtmax = detrending(tmax, detrending_type = "ssa"),
           dsomi = detrending(somi, detrending_type = "ssa")), by = .(id03)]

save(list = "aoi_data", file = "Data/01_ClimateData.RData", compress = "xz", compression_level = 9)
save(list = "aoi_clim", file = "Data/01_AOI_clim.RData", compress = "xz", compression_level = 9)
