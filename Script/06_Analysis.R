library(raster)
rr <- list.files("./Data/AuxiliaryData", pattern = "spam", full.names = T) %>% 
  enframe(name = NULL) %>% 
  mutate(file = str_remove(basename(value), ".tif")) %>% 
  separate(col = file, into = c("version", "zone", "type", "crop", "attr"), sep = "_") %>%
  filter(type == "H", attr %in% c("A","I")) %>% 
  pull(value) %>% 
  stack() %>% 
  crop(maize_county) %>% 
  mask(maize_county) 
rr <- stack(rr, area(rr)*100)
# rr <- rr/area(rr)/100
names(rr) <- c("all", "irrigate", "area")
# cellStats(rr[[1]]/rr[[2]], range)
rr <- raster::extract(x = rr, y = maize_county, fun = sum, na.rm = T, sp = T)
rr <- st_as_sf(rr)
# area <- st_area(rr) %>% units::drop_units() 
# area <- area/10^4
head(rr)
p1 <- rr %>% mutate(pop =  all/area) %>%
  filter(pop > 0.02) %>% 
  filter(id03 %in% id) %>% 
  mutate(pop = cut(pop, seq(0, 1.5, 0.01))) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = pop), color = NA, lwd = 0.01, show.legend = F) +
  # guides(fill = guide_legend(override.aes = list(color = NA)),
  #        color = FALSE) +
  scale_fill_manual(values = pals::brewer.ylgn(150)) +
  geom_sf(maize_shp, mapping = aes(), fill = NA, lwd = 0.5) +
  geom_sf(bdy, mapping = aes()) +
  coord_sf(crs = crs) +
  labs(fill = NULL) +
  scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
  scale_y_continuous(limits = c(2195234.1, 5921292.3))

p2 <- rr %>% mutate(pop =  all/area) %>%
  filter(pop > 0.02) %>% 
  filter(id03 %in% id) %>% 
  mutate(ratio = irrigate/all) %>%
  mutate(ratio = cut(ratio, seq(0, 1, 0.01))) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = ratio), color = NA, lwd = 0.01, show.legend = F) +
  # guides(fill = guide_legend(override.aes = list(color = NA)),
  #        color = FALSE) +
  scale_fill_manual(values = pals::brewer.ylgn(100)) +
  geom_sf(maize_shp, mapping = aes(), fill = NA, lwd = 0.5) +
  geom_sf(bdy, mapping = aes()) +
  coord_sf(crs = crs) +
  labs(fill = NULL) +
  scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
  scale_y_continuous(limits = c(2195234.1, 5921292.3))

plot_grid(p1, p2)


result_prob
yield_spei %>% 
  filter(id03 %in% ids)
  filter(correlation < 0)
