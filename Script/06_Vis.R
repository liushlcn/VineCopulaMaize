## Vis 
## Author: Shengli Liu @ ZZU
## Email:  liushlcn@gmail.com
## Date:   2022/10/17

source("./Script/00_Helper.R")
load("Data/00_AOI.RData")
load("Data/01_AOI_Yield.RData")


# Section 01 plot yield regression of maize -----------------------------------------------------------------------

p = aoi_yld[id03 == 130431] %>%
  ggplot() +
  geom_line(mapping = aes(year, yield, color = "Observed")) +
  geom_line(mapping = aes(year, ay_ssa, color = "SSA")) +
  geom_line(mapping = aes(year, ay_best, color = "Best")) +
  scale_color_manual(breaks = c("Observed", "SSA", "Best"), 
                     values = c("Observed" = "black", "SSA" = "red", "Best" = "blue")) +
  scale_y_continuous(limits = c(0, 9), breaks = seq(0, 8, 2)) +
  scale_x_continuous(breaks = seq(1980, 2015, 5)) +
  labs(x = "Year", y = "Yield (t/ha)", color = NULL) +
  theme_half_open() +
  theme(legend.position = c(1, 0.1), legend.justification = c(1,0))

make_fig(figname = "FigA4b", height = 4, width = 5)
print(p)
dev.off()

brks = seq(0, 1, 0.1)
cols = pals::brewer.orrd(length(brks) - 1)
p1 = merge(aoi, aoi_yld_sts, by = "id03") %>% 
  mutate(col = cut(R2_best, brks, cols)) %>% 
  ggplot() +
  geom_sf(maize_shp, mapping = aes(), fill = "lightgray", linewidth = 0.2) +
  geom_sf(mapping = aes(fill = col), color = NA) +
  geom_sf(bdy, mapping = aes(), linewidth = 0.2) +
  scale_fill_identity() +
  coord_crs(crs = tcrs) +
  labs(fill = NULL) +
  scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
  scale_y_continuous(limits = c(2195234.1, 5921292.3)) +
  theme_basic()

p2 = ggplot_triangle(brks = brks, cols = cols, direction = "none", name = bquote(italic(R)^2))  



brks = c(seq(0, 1, 0.2), c(1.5, 2, 2.5))
cols = pals::brewer.orrd(length(brks) - 1)
p3 = merge(aoi, aoi_yld_sts, by = "id03") %>% 
  mutate(col = cut(RMSE_best, brks, cols)) %>% 
  ggplot() +
  geom_sf(maize_shp, mapping = aes(), fill = "lightgray", linewidth = 0.2) +
  geom_sf(mapping = aes(fill = col), color = NA) +
  geom_sf(bdy, mapping = aes(), linewidth = 0.2) +
  scale_fill_identity() +
  coord_crs(crs = tcrs) +
  labs(fill = NULL) +
  scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
  scale_y_continuous(limits = c(2195234.1, 5921292.3)) +
  theme_basic()
p4 = ggplot_triangle(brks = brks, cols = cols, direction = "none", name = "RMSE")

pf1 <- plot_grid(p1, p2, ncol = 1, rel_heights = c(7, 1))
pf2 <- plot_grid(p3, p4, ncol = 1, rel_heights = c(7, 1))
pf <- plot_grid(pf1, pf2, nrow = 1)

make_fig(figname = "FigA4", width = 10, height = 7)
print(pf)
dev.off()



# Section 04 visualization for maize phenology spatial ------------------------------------------------------------
{
  load("Data/01_AOI_Phenology.RData")
  
  ## study region and phenology stations
  p1 = aoi_cale %>% 
    st_as_sf() %>% 
    ggplot() +
    geom_sf(data = maize_shp, mapping = aes(), linewidth = 0.1) +
    geom_sf(data = aoi, mapping = aes(fill = zone), color = NA) +
    geom_sf(data = rchn, mapping = aes(), linewidth = 0.1) +
    geom_sf(mapping = aes(shape = type)) +
    scale_fill_manual(values = pals::tableau20(3)) +
    # scale_color_identity() +
    coord_sf(crs = tcrs) +
    scale_x_continuous(limits = c(-2700000, 2850000), expand = c(0, 0)) +
    scale_y_continuous(limits = c(1800000, 6000000), expand = c(0, 0)) +
    scale_shape_manual(values = c("spring" = 16, "summer" = 17), labels = c("Spring maize", "Summer maize")) +
    theme_map() +
    theme(legend.position = c(0.0, 0.0), 
          legend.justification = c(0,0),
          legend.key = element_blank(), 
          # legend.justification = c(1, 0.5), 
          legend.background = element_blank(),
          # legend.direction = "horizontal",
          legend.text = element_text(size = 12, color = "black")) +
    labs(fill = "Zones", shape = NULL)
  
  # spatial variations of heading over site and counties
  legs = data.frame(x = rep(2000, 9), y = c(5, 6, 6, 6, 7, 7, 7, 8, 8), z = c(31, 10, 20, 30, 10, 20, 31, 10, 20)) %>% 
    mutate(date = make_date(x, y, z)) %>% 
    mutate(doy = yday(date)) %>% 
    mutate(lbls = paste0(month.abb[y], "-", z))
  brks = legs$doy
  lbls = legs$lbls
  cols = pals::brewer.greens(length(lbls))[-1]
  
  p2 = aoi_cale %>% 
    .[, col := clamp_min(heading, brks[1])] %>% 
    .[, col := clamp_max(col, brks[length(brks)])] %>%
    .[, col := cut(col, brks, cols, include.lowest = T)] %>% 
    st_as_sf() %>% 
    ggplot() +
    geom_sf(data = maize_shp, mapping = aes(), linewidth = 0.1) +
    geom_sf(data = rchn, mapping = aes(), linewidth = 0.1) +
    # geom_sf(mapping = aes(shape = type, color = col), show.legend = F) +
    geom_sf(mapping = aes(shape = type, color = col)) +
    scale_color_identity() +
    coord_sf(crs = tcrs) +
    scale_x_continuous(limits = c(-2700000, 2850000), expand = c(0, 0)) +
    scale_y_continuous(limits = c(1800000, 6000000), expand = c(0, 0)) +
    theme_map() +
    theme(legend.position = c(0.2, 0.2), legend.key = element_blank(), 
          # legend.justification = c(1, 0.5), 
          legend.background = element_blank(),
          legend.text = element_text(size = 12, color = "black")) +
    labs(shape = "Maize type")
  p3 = aoi_gcal %>% 
    .[, col := clamp_min(heading, brks[1])] %>% 
    .[, col := clamp_max(col, brks[length(brks)])] %>%
    .[, col := cut(col, brks, cols, include.lowest = T)] %>% 
    st_as_sf() %>% 
    ggplot() +
    geom_sf(data = maize_shp, mapping = aes(), linewidth = 0.1) +
    geom_sf(data = rchn, mapping = aes(), linewidth = 0.1) +
    # geom_sf(mapping = aes(shape = type, color = col), show.legend = F) +
    geom_sf(mapping = aes(fill = col), color = NA) +
    scale_fill_identity() +
    coord_sf(crs = tcrs) +
    scale_x_continuous(limits = c(-2700000, 2850000), expand = c(0, 0)) +
    scale_y_continuous(limits = c(1800000, 6000000), expand = c(0, 0)) +
    theme_map()
  
  # legend for p2
  p4 = ggplot_triangle(brks = lbls, cols = cols, ratio = 1/3, size = 12, direction = "both")
  
  pf1 <- plot_grid(p2, p3, ncol = 2)
  pf1 <- plot_grid(pf1, NULL, p4, ncol = 1, rel_heights = c(8, -1, 1))
  
  ## 
  p5 <- ggplot(p_aoi_cale %>% .[order(zone, type)] %>% 
                 .[variable == "heading"] %>% 
                 .[, y := seq(-0.04, -0.01, 0.01)]) +
    geom_hline(yintercept = 0, linetype = 2, linewidth = 0.5) +
    geom_density(aoi_gcal, mapping = aes(x = heading, color = zone), show.legend = F) +
    geom_linerange(mapping = aes(xmin = lower, xmax = upper, y = y, color = zone), linewidth = 1) +
    geom_linerange(mapping = aes(xmin = q25, xmax = q75, y = y, color = zone),linewidth = 3, show.legend = F) +
    geom_point(mapping = aes(x = median, y = y, shape = type), size = 3) +
    scale_color_manual(values = pals::tableau20(3)) +
    scale_y_continuous(limits = c(-0.05, 0.2), breaks = seq(0, 0.20, 0.05)) +
    scale_x_continuous(limits = c(140, 240), breaks = brks, labels = lbls) +
    theme_basic() +
    theme(legend.position = c(0.1,1), legend.justification = c(0,1)) +
    labs(x = "Heading date", y = "Density", color = "Zones", shape = "Maize type")
}





# p6 = aoi_gcal %>% 
#   st_as_sf() %>% 
#   ggplot() +
#   geom_sf(data = maize_shp, mapping = aes(), linewidth = 0.1) +
#   geom_sf(data = rchn, mapping = aes(), linewidth = 0.1) +
#   # geom_sf(mapping = aes(shape = type, color = col), show.legend = F) +
#   geom_sf(mapping = aes(fill = type), color = NA) +
#   coord_sf(crs = tcrs) +
#   scale_x_continuous(limits = c(-2700000, 2850000), expand = c(0, 0)) +
#   scale_y_continuous(limits = c(1800000, 6000000), expand = c(0, 0)) +
#   theme_map() +
#   theme(legend.position = c(0.2, 0.2), legend.key = element_blank(), 
#         # legend.justification = c(1, 0.5), 
#         legend.background = element_blank(),
#         legend.text = element_text(size = 12, color = "black")) +
#   labs(fill = "Maize type")



make_fig(figname = "FigA1", height = 5, width = 6)
print(p1)
dev.off()

make_fig(figname = "FigA2", height = 6, width = 8)
print(p4)
dev.off()

make_fig(figname = "FigA3", height = 5, width = 6, pdf = F)
print(p6)
dev.off()