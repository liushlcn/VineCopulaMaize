## Helper Functions
## Author: Shengli Liu @ ZZU
## Email:  liushlcn@gmail.com
## Date:   2022/04/15

# Section 00 Load required packages ---------------------------------------------------------------------------------------------------
Sys.setlocale(category = "LC_ALL", locale = "Chinese")
suppressMessages({
  ## for data manipulation
  library(dplyr)
  library(readr)
  library(tidyr)
  library(broom)
  library(stringr)
  library(lubridate)
  library(magrittr)
  library(data.table)
  library(openxlsx)
  library(readxl)
  library(purrr)
  library(rtrend)
  ## for parallel
  library(foreach)
  library(doParallel)
  library(parallel)
  ## for data visulation
  library(cowplot)
  library(ggpubr)
  library(ggrepel)
  library(ggpmisc)
  library(ggspatial)
  ## for spatial data manipulation
  library(sf)
  library(raster)
  library(terra)
  library(ncdf4)
  library(exactextractr)
  library(PCICt)
  library(MapDatas)
  library(maptools)
  ## for copula analysis
  library(kde1d)
  library(VineCopula)
  library(copula)
})

select <- dplyr::select
extract <- terra::extract
transpose <- data.table::transpose
shift <- data.table::shift
ldply <- plyr::ldply

# Section 01 Basic operation for folders and plots ------------------------------------------------------------------------------------

mkdirs <- function(fp) {
  if (!file.exists(fp)) {
    mkdirs(dirname(fp))
    dir.create(fp, showWarnings = F, mode = "777")
  }
  return(fp)
}

path.mnt <- function(path) {
  sep <- substr(path, 2, 2)
  if (sep == ":") {
    pan <- substr(path, 1, 1) %>% tolower()
    path <- sprintf("/mnt/%s/%s", pan, substr(path, 4, nchar(path)))
  }
  path
}

make_fig <- function(path = "Figures/", figname = "", pdf = T, ...) {
  if (pdf) {
    fig <- paste0(path, figname, ".pdf")
    pdf(fig, ...)
  } else {
    fig <- paste0(path, figname, ".TIF")
    tiff(filename = fig, units = "in", compression = "lzw", res = 600, ...)
  }
}

na.count <- function(x) sum(is.na(x))

# Section 02 Function operation for polygon with sf package ---------------------------------------------------------------------------

st_dissolve <- function(x, field = NULL) {
  if (is.null(field)) {
    IDs <- rep(1, nrow(x))
  } else {
    IDs <- st_set_geometry(x[, field], value = NULL) %>% unlist()
  }
  reg <- maptools::unionSpatialPolygons(as_Spatial(x), IDs)
  return(st_as_sf(reg))
}

netcdf.calendar <- function(nc, time.variable = "time") {
  time.calendar <- ncatt_get(nc, time.variable, "calendar")$value
  time.units <- ncatt_get(nc, time.variable, "units")$value
  time.values <- ncvar_get(nc, time.variable)
  
  origin.pcict <- as.PCICt(strsplit(time.units, " ")[[1]][3],
                           cal = time.calendar
  )
  
  if (time.calendar == "noleap") {
    time.calendar <- "365_day"
  } else if (time.calendar == 0 || time.calendar == "standard") {
    time.calendar <- "gregorian"
  }
  if (grepl("days", time.units)) {
    time.values <- time.values * 86400
  } else if (grepl("hours", time.units)) {
    time.values <- time.values * 3600
  }
  origin.pcict + time.values
}

tcrs <- "+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

coord_crs <- function(crs = tcrs,
                      ...) {
  coord_sf(crs = crs, ...)
}

theme_map <- function() {
  theme_bw() +
    theme(
      panel.grid = element_line(linetype = 2),
      text = element_text(colour = "black"),
      axis.text = element_text(colour = "black")
    )
}

theme_basic <- function(.) {
  theme_bw() +
    theme(
      panel.grid = element_blank(),
      plot.background = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, size = 0.3),
      strip.background = element_rect(colour = "black", size = 0.3),
      legend.background = element_rect(fill = NA, colour = NA),
      legend.box.background = element_rect(fill = NA, colour = NA),
      legend.key = element_blank(),
      axis.ticks.length = unit(x = 1.5, units = "mm"),
      axis.line = element_blank(),
      axis.ticks = element_line(colour = "black", size = 0.3),
      plot.margin = unit(rep(0.5, 4), "lines"),
      axis.text = element_text(colour = "black", size = 13),
      axis.title = element_text(colour = "black", size = 13),
      legend.title = element_text(colour = "black", size = 13),
      legend.text = element_text(colour = "black", size = 13),
      strip.text = element_text(colour = "black", size = 13),
      plot.title = element_text(face = "bold", colour = "black", size = 13, hjust = 0.5)
    )
}


# Section 04 Mathematical statistical analysis ----------------------------------------------------------------------------------------
RMSEFun <- function(simvalue, obsvalue) {
  round(sqrt(mean((simvalue - obsvalue)^2)), 2)
}

RRMSEFun <- function(simvalue, obsvalue) {
  round(100 * sqrt(mean((simvalue - obsvalue)^2)) / mean(obsvalue), 2)
}

EFFun <- function(simvalue, obsvalue) {
  1 - sum((simvalue - obsvalue)^2) / sum((obsvalue - mean(obsvalue))^2)
}

DvalueFun <- function(simvalue, obsvalue) {
  1 - sum((simvalue - obsvalue)^2) / sum((abs(simvalue - mean(obsvalue)) + abs(obsvalue - mean(simvalue)))^2)
}

MBEFun <- function(simvalue, obsvalue) {
  round(mean(abs(simvalue - obsvalue)), 2)
}

R2Fun <- function(simvalue, obsvalue) {
  round(summary(lm(simvalue ~ 1 + obsvalue))$r.squared, 3)
}

R2Funadj <- function(simvalue, obsvalue) {
  summary(lm(simvalue ~ 1 + obsvalue))$adj.r.squared
}

InterceptFun <- function(simvalue, obsvalue) {
  coef(summary(lm(simvalue ~ 1 + obsvalue)))[1, 1]
}

Slopfun <- function(simvalue, obsvalue) {
  coef(summary(lm(simvalue ~ 1 + obsvalue)))[2, 1]
}

eqnF <- function(r) {
  eq <- substitute(~ ~ R^2 ~ "=" ~ r2, list(r2 = format(r, digits = 2)))
  as.character(as.expression(eq))
}

# Section 05 Basic information for subsequent analysis --------------------------------------------------------------------------------

path <- list()
path$temp <- "F:/ClimateData/CRU4.04/"
path$somi <- "F:/ClimateData/GLEAM/monthly/"
path$futclim <- "F:/ClimateData/CMIP6/SiteData/02_Bias/"
path$calendar <- "F:/Basic/Phenology/GLASS/Source/"
# path$calendar <- "G:/Basic/LandUse/GLASS-GLC/"
# path$crop <- "G:/Vegetation/Global GIMMS NDVI3g (1981-2015)/"
path$soil <- "F:/Basic/Soils/BNUsoil/"
# path$land <- "G:/Basic/LandUse/GLASS-GLC/"
