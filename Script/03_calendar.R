#' @importFrom sf st_drop_geometry st_coordinates
#' @export 
as.data.table.sf <- function(x) {
  loc = st_coord(x)
  x %>% st_drop_geometry() %>% cbind(loc, .)
}

#' @inheritParams sf::st_coordinates
#' @export 
st_coord <- function(x) {
  st_coordinates(x) %>%
    set_colnames(c("lon", "lat")) %>%
    as.data.table()
}

#' rdist.earth
#' 
#' @param x1 A position matrix `[lat, lon]`, degree unit.
#' @param x2 same as x1
#' 
#' @export
rdist.earth <- function(x1, x2 = NULL){
  R <- 6378.388
  # coslat1 <- cos(x1[, 2])
  # sinlat1 <- sin(x1[, 2])
  # coslon1 <- cos(x1[, 1])
  # sinlon1 <- sin(x1[, 1])
  coslat1 <- cos((x1[, 2] * pi)/180)
  sinlat1 <- sin((x1[, 2] * pi)/180)
  coslon1 <- cos((x1[, 1] * pi)/180)
  sinlon1 <- sin((x1[, 1] * pi)/180)
  
  coslat2 <- cos((x2[, 2] * pi)/180)
  sinlat2 <- sin((x2[, 2] * pi)/180)
  coslon2 <- cos((x2[, 1] * pi)/180)
  sinlon2 <- sin((x2[, 1] * pi)/180)
  pp <- cbind(coslat1 * coslon1, coslat1 * sinlon1, sinlat1) %*%
    t(cbind(coslat2 * coslon2, coslat2 * sinlon2, sinlat2))
  return(R * acos(ifelse(abs(pp) > 1, 1 * sign(pp), pp)))
}




mz_cale <- list.files(path$calendar, pattern = "Maize", full.names = T) %>% 
  str_subset("HE") %>% 
  grep(pattern = paste0(years, collapse = "|"), value = T) %>% 
  rast() %>% 
  exact_extract(y = aoi, "mean") %>% 
  # data.table() %>% 
  {apply(., 1, mean, na.rm = T)}

which(is.na(mz_cale))
which(!is.na(mz_cale))
lonlat <- st_centroid(aoi) %>% 
  select(geometry) %>% 
  st_coord() %>% 
  as.matrix()

pos <- rdist.earth(lonlat[which(is.na(mz_cale)),], lonlat[which(!is.na(mz_cale)),]) %>% 
  apply(1, which.min) %>% 
  {which(!is.na(mz_cale))[.]}
mz_cale[is.na(mz_cale)] <- mz_cale[pos]

cale <- data.table(id03 = aoi$id03, hdate = ceiling(mz_cale)) %>% 
  .[, c("mon", "day", "year") := chron::month.day.year(jul = hdate)]

cale$mon %>% unique()

aoi %>% left_join(cale, "id03") %>% 
  ggplot() + 
  geom_sf(data = bdy, mapping = aes()) + 
  geom_sf(data = maize_shp, mapping = aes(), fill = "lightgray") +
  geom_sf(mapping = aes(fill = factor(mon)), color = NA) +
  geom_sf(data = maize_shp, mapping = aes(), fill = NA, color = "black") +
  coord_sf(crs = tcrs) +
  # scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
  # scale_y_continuous(limits = c(2195234.1, 5921292.3)) +
  theme_map() 
