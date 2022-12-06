## Helper Functions
## Author: Shengli Liu @ ZZU
## Email:  liushlcn@gmail.com
## Date:   2022/04/15

# Section 00 Load required packages ---------------------------------------------------------------------------------------------------
# Sys.setlocale(category = "LC_ALL", locale = "Chinese")
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
  # library(MapDatas)
  # library(maptools)
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
tcrs = "+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
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
rdist.earth <- function(x1, x2 = x1) {
  R <- 6378.388
  # coslat1 <- cos(x1[, 2])
  # sinlat1 <- sin(x1[, 2])
  # coslon1 <- cos(x1[, 1])
  # sinlon1 <- sin(x1[, 1])
  coslat1 <- cos((x1[, 1] * pi)/180)
  sinlat1 <- sin((x1[, 1] * pi)/180)
  coslon1 <- cos((x1[, 2] * pi)/180)
  sinlon1 <- sin((x1[, 2] * pi)/180)
  
  coslat2 <- cos((x2[, 1] * pi)/180)
  sinlat2 <- sin((x2[, 1] * pi)/180)
  coslon2 <- cos((x2[, 2] * pi)/180)
  sinlon2 <- sin((x2[, 2] * pi)/180)
  pp <- cbind(coslat1 * coslon1, coslat1 * sinlon1, sinlat1) %*%
    t(cbind(coslat2 * coslon2, coslat2 * sinlon2, sinlat2))
  return(R * acos(ifelse(abs(pp) > 1, 1 * sign(pp), pp)))
}

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

theme_map <- function(.) {
  theme_bw() +
    theme(
      panel.grid = element_line(linetype = 2, linewidth = 0.3),
      plot.background = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.3),
      strip.background = element_rect(colour = "black", linewidth = 0.3),
      legend.background = element_rect(fill = NA, colour = NA),
      legend.box.background = element_rect(fill = NA, colour = NA),
      legend.key = element_blank(),
      axis.ticks.length = unit(x = 2, units = "mm"),
      axis.line = element_blank(),
      axis.ticks = element_line(colour = "black", linewidth = 0.3),
      plot.margin = unit(rep(0.5, 4), "cm"),
      axis.text = element_text(colour = "black", size = 12),
      axis.title = element_text(colour = "black", size = 12),
      legend.title = element_text(colour = "black", size = 12),
      legend.text = element_text(colour = "black", size = 12),
      strip.text = element_text(colour = "black", size = 12),
      plot.title = element_text(face = "bold", colour = "black", size = 12, hjust = 0.5)
    )
}

theme_basic <- function(.) {
  theme_bw() +
    theme(
      panel.grid = element_blank(),
      plot.background = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.3),
      strip.background = element_rect(colour = "black", linewidth = 0.3),
      legend.background = element_rect(fill = NA, colour = NA),
      legend.box.background = element_rect(fill = NA, colour = NA),
      legend.key = element_blank(),
      axis.ticks.length = unit(x = 2, units = "mm"),
      axis.line = element_blank(),
      axis.ticks = element_line(colour = "black", linewidth = 0.3),
      plot.margin = unit(rep(0.5, 4), "cm"),
      axis.text = element_text(colour = "black", size = 12),
      axis.title = element_text(colour = "black", size = 12),
      legend.title = element_text(colour = "black", size = 12),
      legend.text = element_text(colour = "black", size = 12),
      strip.text = element_text(colour = "black", size = 12),
      plot.title = element_text(face = "bold", colour = "black", size = 12, hjust = 0.5)
    )
}

## add triangle legend 
ggplot_triangle <- function(brks, name = NULL, cols = "brewer.rdylgn", 
                            direction = "both", ratio = 1, size = 12) {
  brks1 = seq_along(brks)
  min = brks1[1]
  max = brks1[length(brks1)]
  seqs = 1
  if (inherits(try(get(cols), silent = T), "function"))  cols = get(cols)(length(brks1))
  df <- data.frame(x1 = brks1[-length(brks1)], x2 = brks1[-1], y1 = 0, y2 = 1, col = cols)
  dl <- data.frame(x = c(min - seqs, min, min, min - seqs), y = c(0.5, 0.0, 1, 0.5))
  dr <- data.frame(x = c(max, max + seqs, max + seqs, max), y = c(0.0, 0.5, 0.5, 1))
  dw <- switch(direction, 
               "both" = data.frame(x = c(min - seqs, min, max, max + seqs, max + seqs, max, min, min - seqs),
                                   y = c(0.5, 0, 0, 0.5, 0.5, 1, 1, 0.5)),
               "left" = data.frame(x = c(min - seqs, min, max, max, min, min - seqs), y = c(0.5, 0, 0, 1, 1, 0.5)),
               "right" = data.frame(x = c(min, max, max + seqs, max + seqs, max, min), y = c(0, 0, 0.5, 0.5, 1, 1)),
               "none" = data.frame(x = c(min, max, max, min), y = c(0, 0, 1, 1)))
  l_cols <- ifelse(direction %in% c("left", "both"), cols[1], NA)
  r_cols <- ifelse(direction %in% c("right", "both"), cols[length(cols)], NA)
  ggplot() + 
    geom_polygon(dl, mapping = aes(x, y), fill = l_cols) +
    geom_polygon(dr, mapping = aes(x, y), fill = r_cols) +
    geom_rect(df, mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = cols)) + scale_fill_identity() +
    geom_polygon(dw, mapping = aes(x, y), fill = NA, color = "black") +
    scale_x_continuous(breaks = brks1, labels = brks) +
    scale_y_continuous(expand = c(0,0.01)) +
    labs(x = name) +
    coord_fixed(ratio = ratio) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.length.x = unit(1.5, "mm"),
      axis.ticks = element_line(color = "black"),
      axis.text.x = element_text(colour = "black", size = size),
      axis.title.x = element_text(colour = "black", size = size)
    )
}


# ## add triangle legend 
# ggplot_triangle <- function(brks, cols = "brewer.rdylgn", 
#                             direction = "both", rotate = TRUE, ratio = 1, size = 12) {
#   brks1 = seq_along(brks)
#   min = brks1[1]
#   max = brks1[length(brks1)]
#   seqs = 1
#   if (inherits(try(get(cols), silent = T), "function"))  cols = get(cols)(length(brks1))
#   df <- data.frame(x1 = brks1[-length(brks1)], x2 = brks1[-1], y1 = 0, y2 = 1, col = cols)
#   dl <- data.frame(x = c(min - seqs, min, min, min - seqs), y = c(0.5, 0.0, 1, 0.5))
#   dr <- data.frame(x = c(max, max + seqs, max + seqs, max), y = c(0.0, 0.5, 0.5, 1))
#   dw <- switch(direction, 
#                "both" = data.frame(x = c(min - seqs, min, max, max + seqs, max + seqs, max, min, min - seqs),
#                                    y = c(0.5, 0, 0, 0.5, 0.5, 1, 1, 0.5)),
#                "left" = data.frame(x = c(min - seqs, min, max, max, min, min - seqs), y = c(0.5, 0, 0, 1, 1, 0.5)),
#                "right" = data.frame(x = c(min, max, max + seqs, max + seqs, max, min), y = c(0, 0, 0.5, 0.5, 1, 1)),
#                "none" = data.frame(x = c(min, max, max, min), y = c(0, 0, 1, 1)))
#   l_cols <- ifelse(direction %in% c("left", "both"), cols[1], NA)
#   r_cols <- ifelse(direction %in% c("right", "both"), cols[length(cols)], NA)
#   
#   if (!rotate) {
#     ggplot() + 
#       geom_polygon(dl, mapping = aes(y, x), fill = l_cols) +
#       geom_polygon(dr, mapping = aes(y, x), fill = r_cols) +
#       geom_rect(df, mapping = aes(xmin = y1, xmax = y2, ymin = x1, ymax = x2, fill = cols)) + scale_fill_identity() +
#       geom_polygon(dw, mapping = aes(y, x), fill = NA, color = "black") +
#       scale_y_continuous(breaks = brks1, labels = brks, position = "right") +
#       scale_x_continuous(expand = c(0,0.01)) +
#       coord_fixed(ratio = ratio) +
#       theme(
#         panel.grid = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank(),
#         axis.text.x = element_blank(),
#         axis.title.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.length.y = unit(1.5, "mm"),
#         axis.ticks = element_line(color = "black"),
#         axis.text.y.right = element_text(colour = "black", size = size),
#         axis.title.y.right = element_blank()
#       )
#   } else {
#     ggplot() + 
#       geom_polygon(dl, mapping = aes(x, y), fill = l_cols) +
#       geom_polygon(dr, mapping = aes(x, y), fill = r_cols) +
#       geom_rect(df, mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = cols)) + scale_fill_identity() +
#       geom_polygon(dw, mapping = aes(x, y), fill = NA, color = "black") +
#       scale_x_continuous(breaks = brks1, labels = brks) +
#       scale_y_continuous(expand = c(0,0.01)) +
#       coord_fixed(ratio = ratio) +
#       theme(
#         panel.grid = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.ticks.length.x = unit(1.5, "mm"),
#         axis.ticks = element_line(color = "black"),
#         axis.text.x = element_text(colour = "black", size = size),
#         axis.title.x = element_text(colour = "black", size = size))
#   }
#   
# }

# Section 04 Mathematical statistical analysis ----------------------------------------------------------------------------------------
"%!in%" <- Negate("%in%")

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

#' clamp
#' 
#' clamp values in the range of `lims`
#' 
#' @param x Numeric vector
#' @param lims limits
#' @param fill.na If true, values of lims are set to NA; else, values are just 
#' constrained in the range of `lims`.
#' 
#' @examples
#' clamp(1:10, lims = c(4, 7), fill.na = TRUE) 
#' @export
clamp <- function(x, lims = c(0, 1), fill.na = FALSE){
  if (fill.na) {
    x[x < lims[1]] <- NA_real_
    x[x > lims[2]] <- NA_real_
  } else {
    x[x < lims[1]] <- lims[1]
    x[x > lims[2]] <- lims[2]
  }
  x
}

#' @rdname clamp
#' @param x
#' @param value
#' @export
clamp_min <- function(x, value = 0){
  x[x < value] <- value
  x
}

#' @rdname clamp
#' @param x
#' @param value
#' @export
clamp_max <- function(x, value = 0){
  x[x > value] <- value
  x
}

# Section 05 Basic information for subsequent analysis --------------------------------------------------------------------------------

path <- list()
path$temp <- "G:/ClimateData/CRU4.04/"
path$somi <- "G:/ClimateData/GLEAM/monthly/"
path$futclim <- "G:/ClimateData/CMIP6/SiteData/02_Bias/"
path$calendar <- "G:/Basic/Phenology/GLASS/Source/"
# path$calendar <- "G:/Basic/LandUse/GLASS-GLC/"
# path$crop <- "G:/Vegetation/Global GIMMS NDVI3g (1981-2015)/"
path$soil <- "G:/Basic/Soils/BNUsoil/"
# path$land <- "G:/Basic/LandUse/GLASS-GLC/"
