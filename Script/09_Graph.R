## Visualization
## Author: Shengli Liu @ ZZU
## Email:  liushlcn@gmail.com
## Date:   2022/06/28

ggplot() +
  geom_sf(maize_shp, mapping = aes(), fill = "gray", color = "black", lwd = 0.5) +
  geom_sf(data = aoi, mapping = aes(fill = zone), color = NA) +
  geom_sf(data = maize_shp, aes(), fill = NA, color = "black", lwd = 0.5) +
  geom_sf(bdy, mapping = aes(), lwd = 0.6) +
  annotation_north_arrow(
    which_north = "grid", height = unit(1, "cm"), width = unit(0.5, "cm"),
    pad_x = unit(2.5, "in"), pad_y = unit(0.8, "in"), location = "tl"
  ) +
  annotation_scale(pad_x = unit(1, "in"), pad_y = unit(1, "in")) +
  scale_fill_manual(values = c("#2C7BB6", "#ABD9E8", "#FDAE61")) +
  guides(fill = guide_legend(override.aes = list(color = NA))) +
  labs(fill = "Zones") +
  coord_crs() +
  theme_void() +
  theme(legend.position = c(0.3, 0.3))

ggplot() +
  geom_sf(maize_shp, mapping = aes(), lwd = 0.5, fill = "lightgray") +
  geom_sf(data = site, mapping = aes(), size = 1) +
  geom_sf(bdy, mapping = aes(), lwd = 0.6) +
  coord_crs() +
  scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
  scale_y_continuous(limits = c(2195234.1, 5921292.3)) +
  theme_map()

