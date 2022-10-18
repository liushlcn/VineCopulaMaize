## Helper Functions
## Author: Shengli Liu @ ZZU
## Email:  liushlcn@gmail.com
## Date:   2022/10/17

rm(list = ls())
source("./Script/00_Helper.R")
source("./Script/00_Functions.R")

load("Data/00_AOI.RData")
load("Data/01_ClimateData.RData")
load("Data/01_YieldInf.RData")

aoi_clim <- aoi_data[month == 8] %>% 
  .[, `:=`(dtmax = detrending(tmax, detrending_type = "ssa"),
           dsomi = detrending(somi, detrending_type = "ssa")), by = .(id03)]

dt <- merge(yield, aoi_clim, by = c("id03", "year")) %>% 
  .[, `:=`(sdy_ssa = yd_ssa/sd(yd_ssa),
           sdtmax = dtmax/sd(dtmax),
           sdsomi = dsomi/sd(somi)), by = .(id03)]


p = ggplot(dt, mapping = aes(x = sdtmax, y = sdsomi)) +
  geom_rect(xmin = 1, xmax = Inf, ymin = -Inf, ymax = -1, fill = "lightgray", linetype = 2) +
  geom_smooth(method = "lm") +
  geom_smooth(data = dt[sdtmax > 0 & sdsomi < 0], mapping = aes(sdtmax, sdsomi), method = "lm") +
  geom_hline(yintercept = 0, linetype = 2, color = "gray") +
  geom_vline(xintercept = 0, linetype = 2, color = "gray") +
  scale_x_continuous(limits = c(-3,4), breaks = seq(-3, 4, 1), 
                     labels = paste0(-3:4, "\u03C3")) +
  scale_y_continuous(limits = c(-4.5, 3.5)) +
  coord_fixed() +
  theme_basic()



ggsave("Plot.pdf", plot = p)


# aoi %>% left_join(yield_stats, by = "id03") %>% 
#   ggplot() +
#   geom_sf(maize_shp, mapping = aes(), color = "black", fill = "#F2F2F2") +
#   geom_sf(mapping = aes(fill = R2_ssa), color = NA) +
#   scale_fill_stepsn(breaks = seq(0.1,1, 0.1), colours = pals::brewer.orrd(10)[-1]) +
#   geom_sf(maize_shp, mapping = aes(), color = "black", fill = NA, lwd = 1) +
#   coord_sf(crs = tcrs) +
#   # geom_sf(data = bdy, mapping = aes()) +
#   theme_void()



s = aoi_data[month == 8]
s[is.na(s$tmx)] %>% pull(id03) %>% unique()
plot(maize_shp$geometry)
plot(aoi$geometry, add = T)
plot(aoi %>% filter(id03 %in% c(152822, 220623, 522624, 522625, 522732)) %>% st_geometry(), add = T, col = "red")

