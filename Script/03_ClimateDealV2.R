## Helper Functions
## Author: Shengli Liu @ ZZU
## Email:  liushlcn@gmail.com
## Date:   2022/06/27

rm(list = ls())
source("./Script/00_Helper.R")
source("./Script/00_Functions.R")
load("Data/00_AOI.RData")
load("./Data/01_AOI_Phenology.RData")

# cl <- detectCores() - 1
# registerDoParallel(cl = cl)
# runs <- TRUE
period <- "2011_2014"
years <- 2011:2014
# Step 01 Match sites over counties -------------------------------------------------------------------------------

## transfer string to date
str_todate <- function(x) {
  str_extract(x, "\\d{4}.\\d{2}.\\d{2}") %>%
    str_replace_all("[.]", "-") %>%
    as.Date()
}

# https://github.com/isciences/exactextractr/blob/master/README.md
# https://github.com/isciences/exactextract/blob/master/README.md 
## direct use stack crop mask result in NaN values for some counties
# change all the process into "nc - matrxi - raster" 
# cost more time but can be acceptable using "exactextractr" package

system.time({
  temp.file = list.files(path =  path$temp, pattern = "tmp", full.names = T)
  nc <- nc_open(filename = temp.file)
  varname <- netcdf.calendar(nc = nc, time.variable = "time") %>%
    format("%Y-%m-%d") %>% 
    grep(pattern = paste0(years, collapse = "|"), x = ., value = T)
  lyrs <- netcdf.calendar(nc = nc, time.variable = "time") %>%
    format("%Y-%m-%d") %>% 
    grep(pattern = paste0(years, collapse = "|"), x = .)
  rotate <- function(x) apply(t(x), 2, rev)
  aoi_temp <- lapply(lyrs, FUN = function(i) {
    x <- ncvar_get(nc = nc, nc$var$tmp$name, start = c(1, 1, i), count = c(-1, -1, 1))
    x <- rotate(x)
    x <- raster(x = x, xmn = -180, xmx = 180, ymn = -90, ymx = 90, crs = 4326)
    # x = rast(x = x)
    exactextractr::exact_extract(x, y = aoi, "weighted_mean", weights = "area",  progress = F)
  }) %>%
    do.call(cbind, .) %>%
    data.table() %>% 
    setNames(varname) %>% 
    .[, id03 := aoi$id03] %>% 
    setnames(names(.), str_sub(names(.), 1,7)) %>% 
    melt.data.table(id.vars = "id03", measure.vars = patterns("\\d{4}-\\d{2}"), 
                    variable.name = "variable", value.name = "value") %>% 
    .[, c("year", "month") := tstrsplit(variable, "-")] %>% 
    dcast.data.table(id03+year~month, value.var = "value") %>% 
    .[, year := as.integer(year)]
  
  spline_clim <- function(x, y, year, start, end) {
    x = yday(str_subset(x, pattern = as.character(year)))
    n = 365
    if (leap_year(year)) n = 366
    tmp <- spline(x, y, n = n, xmin = 1, xmax = n)
    mean(tmp$y[start:end])
  }
  xx = as.matrix(aoi_temp[, !c("id03", "year")])
  aoi_temp %<>% 
    merge(aoi_gcal[,.(zone, id03, heading, maturity)], by = "id03")
  yrs = aoi_temp$year
  start = aoi_temp$heading
  end = aoi_temp$maturity
  tmax_gs = sapply(1:nrow(xx), FUN = function(i) {
      spline_clim(x = varname, y = xx[i,], year = yrs[i], start = start[i], end = end[i])
    })
  # melt.data.table(id.vars = "id03", variable.name = "date", value.name = "tmax") %>% 
  #   .[, date := str_todate(date)] %>% 
  #   .[order(id03, date)] %>% 
  #   .[,`:=`(year = year(date), month = month(date))]
  nc_close(nc)
})



# 

system.time({
  somi.file <- list.files(path = path$somi, pattern = "root", full.names = T)
  nc <- nc_open(filename = somi.file)
  varname <- netcdf.calendar(nc = nc, time.variable = "time") %>%
    format("%Y-%m-%d") %>% 
    grep(pattern = paste0(years, collapse = "|"), x = ., value = T)
  lyrs <- netcdf.calendar(nc = nc, time.variable = "time") %>%
    format("%Y-%m-%d") %>% 
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
})


aoi_data <- reduce(.x = list(aoi_temp, aoi_somi), .f = merge, by = c("id03", "year", "month")) %>% 
  dcast.data.table(id03+year~month, value.var = c("tmax", "somi"))

save(list = "aoi_data", file = "Data/00_ClimateData.RData", compress = "xz", compression_level = 9)
load("Data/00_ClimateData.RData")

# load("Data/01_AOI_Phenology.RData")
# dt = aoi_data %>% 
#   merge(aoi_gcal[,.(zone, id03, heading, maturity)], by = "id03")
# # dt = aoi_data %>% merge(aoi_gcal[,.(zone, id03, heading, maturity)], by = "id03")
# tmax = dt[, .SD, .SDcols = patterns("tmax")] %>% as.matrix()
# somi = dt[, .SD, .SDcols = patterns("somi")] %>% as.matrix()
# yrs = dt$year
# start = dt$heading
# end = dt$maturity
# 
# spline_clim <- function(x, y, year, start, end) {
#   x = yday(str_subset(x, pattern = as.character(year)))
#   n = 365
#   if (leap_year(year)) n = 366
#   tmp <- spline(x, y, n = n, xmin = 1, xmax = n)
#   mean(tmp$y[start:end])
# }
# 
# tmax_gs = sapply(1:nrow(tmax), FUN = function(i) {
#   spline_clim(x = varname, y = tmax[i,], year = yrs[i], start = start[i], end = end[i])
# })
# somi_gs = sapply(1:nrow(somi), FUN = function(i) {
#   spline_clim(x = varname, y = somi[i,], year = yrs[i], start = start[i], end = end[i])
# })
# 
# aoi_clim = dt[, .(id03, year)] %>% 
#   .[,`:=`(tmax = tmax_gs, somi = somi_gs)] %>% 
#   .[, `:=`(dtmax = detrending(tmax, detrending_type = "ssa"),
#            dsomi = detrending(somi, detrending_type = "ssa")), by = .(id03)]

# aoi_clim <- aoi_data[month == 8] %>% 
#   .[, `:=`(dtmax = detrending(tmax, detrending_type = "ssa"),
#            dsomi = detrending(somi, detrending_type = "ssa")), by = .(id03)]

save(list = "aoi_clim", file = "Data/01_AOI_clim.RData", compress = "xz", compression_level = 9)
