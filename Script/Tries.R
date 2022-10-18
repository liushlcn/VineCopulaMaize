## Copula Analysis
## Author: Shengli Liu @ ZZU
## Email:  liushlcn@gmail.com
## Date:   2022/06/30

rm(list = ls())
source("./Script/00_Helper.R")
source("./Script/00_Functions.R")

load("Data/01_ClimInf.RData")
load("Data/01_YieldInf.RData")
load("Data/00_AOI.RData")
load("Data/00_HeadDate.RData")
load("Data/00_Site.RData")

dta <- merge(yield, dta_clim, by = c("year", "id03"))

# I try the heading degree days for maize but cannot get the result, how about using mean temperature instead
# now check the heat stress days and plot with a glances

dta_clim_year <- dta_clim[, lapply(.SD, mean), .(id03), .SDcols = !c("year", "id03")]


p1 <- merge(aoi, dta_clim_year, by = "id03") %>%
  ggplot() +
  geom_sf(mapping = aes(fill = hdy > 2), color = NA) +
  geom_sf(data = maize_shp, mapping = aes(), fill = NA, color = "gray50", lwd = 0.5) +
  geom_sf(data = bdy, mapping = aes(), fill = NA, color = "gray50", lwd = 0.5) +
  scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
  scale_y_continuous(limits = c(2195234.1, 5921292.3)) +
  coord_crs() +
  theme_map()

p2 <- merge(aoi, dta_clim_year, by = "id03") %>%
  ggplot() +
  geom_sf(mapping = aes(fill = cut(hdy, seq(0,24, 2), include.lowest = FALSE)), color = NA) +
  geom_sf(data = maize_shp, mapping = aes(), fill = NA, color = "gray50", lwd = 0.5) +
  geom_sf(data = bdy, mapping = aes(), fill = NA, color = "gray50", lwd = 0.5) +
  scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
  scale_y_continuous(limits = c(2195234.1, 5921292.3)) +
  labs(fill = NULL) +
  coord_crs() +
  theme_map()

pf = plot_grid(p1, p2, nrow = 1)
ggsave(filename = "days.tiff", pf, height = 6, width = 12, units = "in", dpi = 600, compression = "lzw" )

p <- ggplot() +
  geom_sf(maize_shp, mapping = aes(), lwd = 0.5, color = NA, fill = "lightgray") +
  geom_sf(aoi, mapping = aes(fill = zone), lwd = 0.5, color = NA) +
  geom_sf(data = site, mapping = aes(), size = 0.75) +
  geom_sf(maize_shp, mapping = aes(), lwd = 0.5, color = "black", fill = NA) +
  geom_sf(bdy, mapping = aes(), lwd = 0.6) +
  scale_fill_manual(values = c("#2C7BB6", "#ABD9E8", "#FDAE61")) +
  coord_crs() +
  scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
  scale_y_continuous(limits = c(2195234.1, 5921292.3)) +
  theme_map()
p
ggsave(filename = "region.tiff", p, height = 6, width = 6, units = "in", dpi = 600, compression = "lzw" )

p1 <- headday[, mon := chron::month.day.year(hdavg)$month] %>% 
  merge(aoi, ., by = "id03") %>% 
  ggplot() +
  geom_sf(maize_shp, mapping = aes(), lwd = 0.5, color = NA, fill = "lightgray") +
  geom_sf(mapping = aes(fill = hdavg), lwd = 0.5, color = NA) +
  geom_sf(maize_shp, mapping = aes(), lwd = 0.5, color = "black", fill = NA) +
  geom_sf(bdy, mapping = aes(), lwd = 0.6) +
  # scale_fill_manual(values = c("#2C7BB6", "#ABD9E8", "#FDAE61")) +
  coord_crs() +
  scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
  scale_y_continuous(limits = c(2195234.1, 5921292.3)) +
  theme_map()

p2 <- headday[, mon := chron::month.day.year(hdavg)$month] %>% 
  merge(aoi, ., by = "id03") %>% 
  ggplot() +
  geom_sf(maize_shp, mapping = aes(), lwd = 0.5, color = NA, fill = "lightgray") +
  geom_sf(mapping = aes(fill = factor(mon)), lwd = 0.5, color = NA) +
  geom_sf(maize_shp, mapping = aes(), lwd = 0.5, color = "black", fill = NA) +
  geom_sf(bdy, mapping = aes(), lwd = 0.6) +
  # scale_fill_manual(values = c("#2C7BB6", "#ABD9E8", "#FDAE61")) +
  coord_crs() +
  scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
  scale_y_continuous(limits = c(2195234.1, 5921292.3)) +
  theme_map()

pf <- plot_grid(p1, p2, nrow = 1)

ggsave(filename = "headingdate.tiff", pf, height = 6, width = 12, units = "in", dpi = 600, compression = "lzw" )


## 检查相关性判定后续工作
tiff(filename = "correlation.tiff", width = 5, height = 5, units = "in", compression = "lzw", res = 600)
merge(yield, dta_clim) %>% 
  .[, .(cor = cor(yd_pssa, hdi)), .(id03)] %>% {hist(.$cor, main = NULL, ann = F)}
abline(v = 0)
abline(v = critical.r.function(alpha = c(0.05, 0.1), n.pairs = 34, r.null = 0), lty = c(2,3))
legend("topleft", lty = 2:3, legend = c(0.05, 0.1), box.col = NA, border = NA)
box()
dev.off()

p1 <- merge(yield, dta_clim) %>% 
  .[, .(cor = cor(yd_pssa, hdi)), .(id03)] %>% 
  merge(aoi, ., by = "id03") %>% 
  ggplot() +
  geom_sf(maize_shp, mapping = aes(), lwd = 0.5, color = NA, fill = "lightgray") +
  geom_sf(mapping = aes(fill = abs(cor) > abs(critical.r.function(0.05, 34, 0)) ), lwd = 0.5, color = NA) +
  geom_sf(maize_shp, mapping = aes(), lwd = 0.5, color = "black", fill = NA) +
  geom_sf(bdy, mapping = aes(), lwd = 0.6) +
  # scale_fill_manual(values = c("#2C7BB6", "#ABD9E8", "#FDAE61")) +
  labs(fill = NULL) +
  coord_crs() +
  scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
  scale_y_continuous(limits = c(2195234.1, 5921292.3)) +
  theme_map()

p2 <- merge(yield, dta_clim) %>% 
  .[, .(cor = cor(yd_pssa, hdi)), .(id03)] %>% 
  merge(aoi, ., by = "id03") %>% 
  ggplot() +
  geom_sf(maize_shp, mapping = aes(), lwd = 0.5, color = NA, fill = "lightgray") +
  geom_sf(mapping = aes(fill = cor < 0  ), lwd = 0.5, color = NA) +
  geom_sf(maize_shp, mapping = aes(), lwd = 0.5, color = "black", fill = NA) +
  geom_sf(bdy, mapping = aes(), lwd = 0.6) +
  # scale_fill_manual(values = c("#2C7BB6", "#ABD9E8", "#FDAE61")) +
  labs(fill = NULL) +
  coord_crs() +
  scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
  scale_y_continuous(limits = c(2195234.1, 5921292.3)) +
  theme_map()

pf <- plot_grid(p1, p2, nrow = 1)

ggsave(filename = "correlations.tiff", pf, height = 6, width = 12, units = "in", dpi = 600, compression = "lzw" )


## 检查相关性判定后续工作
tiff(filename = "correlation_tasmax.tiff", width = 5, height = 5, units = "in", compression = "lzw", res = 600)
merge(yield, dta_clim) %>% 
  .[, .(cor = cor(yd_pssa, tasmax)), .(id03)] %>% {hist(.$cor, main = NULL, ann = F)}
abline(v = 0)
abline(v = critical.r.function(alpha = c(0.05, 0.1), n.pairs = 34, r.null = 0), lty = c(2,3))
legend("topleft", lty = 2:3, legend = c(0.05, 0.1), box.col = NA, border = NA)
box()
dev.off()




# ggplot() +
#   geom_sf(maize_shp, mapping = aes(), lwd = 0.5, fill = "lightgray") +
#   geom_sf(data = site, mapping = aes(), size = 1) +
#   geom_sf(bdy, mapping = aes(), lwd = 0.6) +
#   coord_crs() +
#   scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
#   scale_y_continuous(limits = c(2195234.1, 5921292.3)) +
#   theme_map()
