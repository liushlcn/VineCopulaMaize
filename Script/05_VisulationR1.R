rm(list = ls())
library(tidyverse)
library(sf)
library(ggspatial)

library(cowplot)
library(openxlsx)

source("Script/00_Helper_function.R")
select <- dplyr::select
extract <- raster::extract
crs = "+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
load("Data/Maize_shp.RData", verbose = T)
load("Data/Study_cty.RData", verbose = T)

######################################################################################################################################################
## Figure 1 Location of the three main maize cropping region in China and the harvest fraction across China                                         ##
######################################################################################################################################################
maize_county$zone <- factor(maize_county$zone, levels = c("North_SpringMaize", "Huanghuahai_SummerMaize", "Southwest_Maize",
                                                          "Northwest_Maize", "South_Maize", "Tibetan_Maize"))
p <- ggplot() +
  #geom_sf(data = chn02, aes(), color = NA, fill = "lightgrey") +
  geom_sf(data = maize_shp, aes(), fill = "gray", color = "black", lwd = 0.5) +  
  geom_sf(data = maize_county %>% filter(id03 %in% id), aes(fill = zone), color = NA,
          show.legend = T) +
  geom_sf(data = maize_shp, aes(), fill = NA, color = "black", lwd = 0.5) +  
  geom_sf(data = bdy, aes(), color = "black", lwd = 0.6) +
  annotation_north_arrow(which_north = "grid", height = unit(1, "cm"), width = unit(0.5, "cm"), 
                         pad_x = unit(2.5,"in"), pad_y = unit(0.8, "in"), location = "tl") + 
  annotation_scale(pad_x = unit(1, "in"), pad_y = unit(1, "in")) +
  scale_fill_manual(values = c("#2C7BB6", "#ABD9E8", "#FDAE61"), labels = c("NCSM", "HHHSM", "SWCM")) +
  guides(fill = guide_legend(override.aes = list(color = NA)), 
         color = FALSE) + 
  labs(fill = "Zone") +
  coord_sf(crs = crs) +
  theme_void() +
  theme(legend.position = c(0.3, 0.3))

make_fig(figname = "EJAFig1", width = 5, height = 6)
print(p)
dev.off()

######################################################################################################################################################
## Figure 2 Impact of SPEI-based precipitation anomaly on maize yield in China.                                                                     ##
######################################################################################################################################################
load("Data/InputData/Yield_copula.RData", verbose = T)
load("Data/InputData/Yield_information.RData", verbose = T)
# load("Maize_fraction.RData")

droughttype <- data.frame(type = c("Extreme Dry", "Severely Dry", "Moderately Dry", "Near Normal",
                                   "Moderately wet", "Severely Wet", "Extreme Wet"),
                          thx1 = c(-2, -1.5, -1, 1, 1.5, 2, Inf),
                          thx2 = c(-Inf, -2, -1.5, -1, 1, 1.5, 2))

## bootstrap

sample_n_groups = function(tbl, size, replace = FALSE, weight = NULL) {
  # regroup when done
  grps = tbl %>% groups %>% lapply(as.character) %>% unlist
  # check length of groups non-zero
  keep = tbl %>% summarise() %>% ungroup() %>% sample_n(size, replace, weight)
  # keep only selected groups, regroup because joins change count.
  # regrouping may be unnecessary but joins do something funky to grouping variable
  tbl %>% right_join(keep, by = grps) %>% group_by_(.dots = grps)
}

set.seed(12356)
B = 1000

dta <- yield_season %>% 
  mutate(type = cut(spei, breaks = c(-Inf, -2,-1.5,-1,1,1.5,2,Inf),
                    labels = c("Extreme Dry", "Severely Dry", "Moderately Dry", "Near Normal",
                               "Moderately wet", "Severely Wet", "Extreme Wet"))) %>% 
  group_by(id03)
dta$zone <- droplevels(dta$zone)
dta$zone <- factor(dta$zone, levels = c("North_SpringMaize", "Huanghuahai_SummerMaize", "Southwest_Maize"), labels = c("NCSM", "HHHSM", "SWCM"))
ychg <- dta %>% group_by(type) %>% 
  summarise(yd = mean(x = yd_ano))
# summarise(yd = weighted.mean(x = yd_ano, w = fraction))

percent <- dta %>% 
  mutate(count = 1) %>% 
  group_by(type) %>% 
  summarise(count = sum(count)) %>% 
  mutate(count = count/sum(count)*100) %>% 
  slice(1:4)

ychg_interval <- list()
for (i in 1:B) {
  ychg_interval[[i]] <- dta %>% # filter(zone == "HHHSM") %>% 
    sample_n_groups(size = length(id), replace = T) %>% 
    group_by(type) %>% 
    summarise(yd = mean(x = yd_ano))
  # summarise(yd = weighted.mean(x = yd_ano, w = fraction))
}

ychg_interval <- do.call(rbind, ychg_interval) %>% 
  group_by(type) %>% 
  na.omit() %>% 
  summarize(plower = quantile(yd, .05),
            pupper = quantile(yd, .95))

ychg <- left_join(ychg, ychg_interval, by = "type")

percent <- percent %>% mutate(type = as.character(type)) %>% 
  mutate(type = factor(x = type, levels = c("Extreme Dry", "Severely Dry", "Moderately Dry", "Near Normal"),
                       labels = c("Extreme Dry", "Severe Dry", "Moderate Dry", "Near Normal") )) %>% 
  as.data.frame()
ychg <- ychg %>% slice(1:4) %>% mutate(type = as.character(type)) %>% 
  mutate(type = factor(x = type, levels = c("Extreme Dry", "Severely Dry", "Moderately Dry", "Near Normal"),
                       labels = c("Extreme Dry", "Severe Dry", "Moderate Dry", "Near Normal") )) %>% 
  as.data.frame()

if (F) {
  make_fig(figname = "EJAFig2", width = 5, height = 4, bg = "white")
  par(mar = c(3,3,1,1), cex = 0.8, bty = "l")
  px <- barplot(yd~type, data = ychg, col = "lightgrey", border = NA, ylim = c(-12.5, 2), axes = F, ann = F, xaxt = "n")
  segments(x0 = px, y0 = ychg$plower, px, ychg$pupper)
  segments(x0 = px - 0.1, ychg$plower, px + 0.1, ychg$plower)
  segments(x0 = px - 0.1, ychg$pupper, px + 0.1, ychg$pupper)
  axis(1, px, labels = ychg$type)
  axis(2, seq(-12,11,3), las = 1)
  abline(h = 0)
  text(px, -12, paste0(round(percent$count,2), "%"), cex = 0.8)
  title( xlab = NA,  ylab = "Yield change (%)", line = 2)
  box()
  dev.off()
}


p <- inner_join(ychg, percent) %>% 
  ggplot(data = ., mapping = aes(x = type, y = yd, ymin = plower, ymax = pupper)) +
  geom_bar(stat = "identity", fill = "lightgrey") +
  geom_errorbar(width = 0.2) +
  geom_text(mapping = aes(y = -12, label = paste0(round(count,2), "%"))) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-12.5, 2), breaks = seq(-12, 0, 3)) +
  labs(x = NULL, y = "Yield change (%)") +
  theme_classic() +
  theme(
        axis.ticks.length = unit(x=1.5, units="mm"), 
        # axis.line = element_blank(),
        axis.ticks = element_line(colour = "black", size = 0.5),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12))
make_fig(figname = "EJAFig2", width = 5, height = 4)
print(p)
dev.off()


write_out(percent, filename = "EJAFig2a")
write_out(ychg, filename = "EJAFig2b")

######################################################################################################################################################
## Figure 3 and relevant supplement figures Impact of SPEI-based precipitation anomaly on maize yield in China.                                     ##
######################################################################################################################################################

## Figure 3 and relevant Supplement Figures
## 20200928修改，添加区域进行分析
load("./Data/InputData/Result_Yield_spei_scale.RData")
dta <- maize_county %>% 
  inner_join(yield_spei, by = "id03") 
dta$zone <- droplevels(dta$zone)
dta$zone <- factor(dta$zone, levels = c("North_SpringMaize", "Huanghuahai_SummerMaize", "Southwest_Maize"), labels = c("NCSM", "HHHSM", "SWCM"))

#################################
## Figs Crop calendar information
p <- dta %>% 
  gather(variable, month, plant, harvest) %>% 
  mutate(variable = factor(variable, levels = c("plant", "harvest"), labels = c("Plant month", "Harvest month"))) %>% 
  ggplot() +
  # geom_sf(mapping = aes(fill = month.abb[month], color = month.abb[month]), lwd = 0.01) +
  # guides(fill = guide_legend(override.aes = list(color = NA)), 
  #        color = FALSE) + 
  geom_sf(mapping = aes(fill = month.abb[month]), color = NA, lwd = 0.01) +
  # guides(fill = guide_legend(override.aes = list(color = NA)), 
  #        color = FALSE) + 
  geom_sf(maize_shp, mapping = aes(), fill = NA, lwd = 0.5) +
  geom_sf(bdy, mapping = aes()) +
  coord_sf(crs = crs) +
  facet_wrap(~variable) +
  labs(fill = NULL) +
  scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
  scale_y_continuous(limits = c(2195234.1, 5921292.3)) +
  theme_basic()
make_fig(figname = "EJAFigS1", width = 10, height = 6)
print(p)
dev.off()

################################
## Figs end month and time scale
p1 <- dta %>% 
  mutate(month = factor(x = month, levels = 5:9, labels = month.abb[5:9])) %>% 
  ggplot() +
  # geom_sf(mapping = aes(fill = month, color = month), lwd = 0.01) +
  # guides(fill = guide_legend(override.aes = list(color = NA)), 
  #        color = FALSE) + 
  geom_sf(mapping = aes(fill = month), color = NA, lwd = 0.01) +
  # guides(fill = guide_legend(override.aes = list(color = NA)), 
  #        color = FALSE) + 
  geom_sf(maize_shp, mapping = aes(), fill = NA, lwd = 0.5) +
  geom_sf(bdy, mapping = aes()) +
  coord_sf(crs = crs) +
  labs(fill = NULL) +
  scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
  scale_y_continuous(limits = c(2195234.1, 5921292.3)) +
  theme_basic() +
  theme(legend.position = c(0.9, 0.25)) +
  ggtitle("Critical month")

p2 <- dta %>% 
  ggplot() +
  # geom_sf(mapping = aes(fill = factor(scale), color = factor(scale)), lwd = 0.01) +
  # guides(fill = guide_legend(override.aes = list(color = NA)), 
  #        color = FALSE) + 
  geom_sf(mapping = aes(fill = factor(scale)), color = NA, lwd = 0.01) +
  # guides(fill = guide_legend(override.aes = list(color = NA)), 
  #        color = FALSE) + 
  scale_fill_viridis_d() +
  # scale_color_viridis_d() +
  geom_sf(maize_shp, mapping = aes(), fill = NA, lwd = 0.5) +
  geom_sf(bdy, mapping = aes()) +
  coord_sf(crs = crs) +
  labs(fill = NULL) +
  scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
  scale_y_continuous(limits = c(2195234.1, 5921292.3)) +
  theme_basic() +
  theme(legend.position = c(0.9, 0.25)) +
  ggtitle("Best timescale")
library(ggpubr)
p <- ggarrange(p1, p2, ncol = 2)
make_fig(figname = "EJAFigS3", width = 10, height = 6)
print(p)
dev.off()

###########################################
## Plot the time scale with different month
p <- dta %>% 
  st_set_geometry(value = NULL) %>% 
  mutate(pvalue = ifelse(pvalue < 0.1, "Sig", "InSig")) %>% 
  group_by(scale, month, pvalue) %>% 
  count() %>% 
  mutate(month = factor(month, levels = 5:9, labels = month.abb[5:9])) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = pvalue, y = n, fill = month, group = pvalue), stat = "identity") +
  # scale_x_continuous(breaks = seq(1, 12, 1)) +
  facet_wrap(~scale, ncol = 9, strip.position = "bottom") +
  scale_x_discrete(labels = c(substitute(paste(italic('p'), ">0.1")), substitute(paste(italic('p'), "<0.1")))) +
  scale_y_continuous(limits = c(0, 280), expand = c(0,0), breaks = seq(0, 250, 50)) +
  labs(x = "Time scale (month)", y = "Number of counties", fill = NULL) +
  theme_classic() +
  guides(fill = guide_legend(nrow = 1)) +
  theme(strip.placement = "outside",
        #panel.background = element_blank(),
        # strip.background.x = element_rect(),
        axis.text = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.line.x = element_line(colour = "black"),
        panel.spacing = unit(x = 0, units = "pt"),
        legend.position = c(0.7, 0.7),
        legend.background = element_blank())
make_fig(figname = "EJAFigS4", width = 9, height = 5)
print(p)
dev.off()

#########################################
dta %>% 
  st_set_geometry(value = NULL) %>% 
  mutate(pvalue = ifelse(pvalue < 0.1, "Sig", "InSig")) %>% 
  group_by(zone, scale, month, pvalue) %>% 
  count() %>% 
  mutate(month = factor(month, levels = 5:9, labels = month.abb[5:9])) %>% 
  reshape2::dcast(month+scale~pvalue+zone, value.var = "n") %>% 
  write_out(filename = "EJAFigS4")

dta_sig <- dta %>% 
  filter(pvalue < 0.1) %>% 
  as_Spatial() %>% 
  sp::coordinates() %>% 
  as_tibble() %>% 
  setNames(c("lon", "lat")) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
brks = seq(-1, 1, 0.25)
cols = rev(pals::brewer.rdbu(length(brks) - 1))

p1 <- dta %>% 
  mutate(pcols = cut(correlation, brks, cols)) %>% 
  mutate(pcols = as.character(pcols)) %>% 
  ggplot() +
  # geom_sf(mapping = aes(fill = pcols, color = pcols), lwd = 0.01) +
  # scale_fill_identity() +
  # scale_color_identity() +
  # guides(fill = guide_legend(override.aes = list(color = NA)), 
  #        color = FALSE) + 
  geom_sf(mapping = aes(fill = pcols), color = NA, lwd = 0.01) +
  scale_fill_identity() +
  geom_sf(maize_shp, mapping = aes(), fill = NA, lwd = 0.5) +
  geom_sf(data = dta_sig, mapping = aes(geometry = geometry), shape = "+", size = 2, color = "black") +
  geom_sf(bdy, mapping = aes()) +
  coord_sf(crs = crs) +
  scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
  scale_y_continuous(limits = c(2195234.1, 5921292.3)) +
  theme_basic()

dt <- data.frame(x1 = brks[-length(brks)], x2 = brks[-1], y1 = 0, y2 = 1, col = cols)
p2 <- ggplot() +
  geom_rect(dt, mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = col)) +
  scale_fill_identity() +
  geom_rect(mapping = aes(xmin = min(brks), xmax = max(brks), ymin = 0, ymax = 1), fill = NA, color = "black") +
  geom_vline(xintercept = c(0.31, -0.31), linetype = 1, color = "darkgrey") +
  # scale_x_continuous(breaks = brks, labels = brks, expand = c(0.2, 0.2)) +
  scale_y_continuous(expand = c(0.0,0.01)) +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11), 
        axis.text.y = element_blank() ,
        axis.ticks.length.x = unit(2, "pt"),
        axis.ticks.x = element_line(colour = "black")
  ) +
  # coord_fixed(ratio = 1) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.length.y = unit(0, 'pt')) +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  ggtitle(label = "Maximum correlation coefficient")

p <- ggarrange(p1, p2, nrow = 2, widths = c(5, 3), heights = c(6, 1))
make_fig(figname = "EJAFigS6", width = 4.5, height = 5, bg = "white")
print(p)
dev.off()

#########################################
## Plot fig for the amount of correlation
dta_percent <- list()
dta_percent[[1]] <- dta %>% st_set_geometry(value = NULL) %>%
  mutate(correlation = ifelse(correlation > 0, "Positive", "Negative"),
         pvalue = ifelse(pvalue > 0.05, "Non-Significant", "Significant"),
         value = 1) %>%
  # select(zone, correlation, pvalue) %>%
  group_by(zone, correlation, pvalue) %>%
  summarise(value = sum(value)) %>%
  group_by(zone) %>%
  mutate(value = value/sum(value)*100) %>%
  unite("correlation", c("correlation", "pvalue"), sep = " ") %>%
  as.data.frame()  %>%
  mutate(zone = as.character(zone)) 

dta_percent[[2]] <- dta %>% st_set_geometry(value = NULL) %>%
  mutate(correlation = ifelse(correlation > 0, "Positive", "Negative"),
         pvalue = ifelse(pvalue > 0.05, "Non-Significant", "Significant"),
         value = 1) %>%
  # select(zone, correlation, pvalue) %>%
  group_by(correlation, pvalue) %>%
  summarise(value = sum(value)) %>%
  as.data.frame() %>% 
  mutate(value = value/sum(value)*100) %>%
  unite("correlation", c("correlation", "pvalue"), sep = " ") %>%
  mutate(zone = "Whole")
if (F) {
  p <- bind_rows(dta_percent) %>% 
    mutate(correlation = factor(correlation, 
                                levels = c("Negative Significant", "Negative Non-Significant","Positive Non-Significant","Positive Significant"))) %>% 
    mutate(zone = factor(zone, levels = c("Whole","NCSM", "HHHSM", "SWCM"))) %>% 
    ggplot(mapping = aes(x = zone, y = value, fill = correlation)) +
    geom_bar(stat = "identity", width = 0.8, position = position_stack(), show.legend = T) +
    # coord_polar("y") + 
    scale_fill_manual(values = c("#0571B0", "#92C5DE", "#F4A582", "#CA0020")) +
    geom_vline(xintercept = 1.5, linetype = 2) +
    scale_y_continuous(breaks = seq(0, 100, 25), expand = c(0.01, 0.01)) +
    labs(x = NULL, y = "Percentage of counties (%)", fill = NULL) +
    guides(fill = guide_legend(nrow = 2, label.hjust = 0)) +
    theme_basic() +
    theme(legend.position = "bottom")
  
} else {
  df <- bind_rows(dta_percent) %>% 
    mutate(correlation = factor(correlation, 
                                levels = c( "Negative Non-Significant", "Negative Significant", "Positive Non-Significant","Positive Significant"))) %>% 
    mutate(zone = factor(zone, levels = c("Whole","NCSM", "HHHSM", "SWCM"))) %>%
    mutate(value = ifelse(str_detect(correlation, "Negative"), -value, value))
  p <- ggplot() +
    geom_bar(filter(df, value > 0), mapping = aes(x = zone, y = value, fill = correlation), stat = "identity", width = 0.8, position = position_stack(), show.legend = T) +
    geom_bar(filter(df, value < 0), mapping = aes(x = zone, y = value, fill = correlation), stat = "identity", width = 0.8, position = position_stack(), show.legend = T) +
    scale_fill_manual(values = c("#92C5DE", "#0571B0", "#F4A582", "#CA0020")) +
    geom_hline(yintercept = 0, linetype = 2) +
    theme_basic() +
    scale_y_continuous(limits = c(-40, 80), breaks = seq(-40, 80, 20), labels = abs(seq(-40, 80, 20))) +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 2, label.hjust = 0)) +
    labs(x = NULL, y = "Percentage of counties (%)", fill = NULL)
}
make_fig(figname = "EJAFig3", width = 5.5, height = 5)
print(p)
dev.off()

bind_rows(dta_percent) %>% 
  mutate(zone = factor(zone, levels = c("Whole","NCSM", "HHHSM", "SWCM"))) %>% 
  write_out(x = ., filename = "EJAFig3")


#########################################
load("./Data/R1OutputData/Result_copula.RData", verbose = T)
p <- result_copula %>% 
  inner_join(maize_county, by = "id03") %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = familyname), color = NA, lwd = 0.01, alpha = 0.8) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  guides(fill = guide_legend(override.aes = list(color = NA)), 
         color = FALSE) + 
  geom_sf(maize_shp, mapping = aes(), fill = NA, lwd = 0.5) +
  geom_sf(bdy, mapping = aes()) +
  labs(fill = NULL) +
  coord_sf(crs = crs) +
  scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
  scale_y_continuous(limits = c(2195234.1, 5921292.3)) +
  theme_basic()

make_fig(figname = "EJAFigS5", width = 5.5, height = 4.5)
print(p)
dev.off()

zones <- dta %>% st_set_geometry(value = NULL) %>% 
  select(id03, zone)
result_copula %>% 
  inner_join(zones) %>% 
  group_by(zone, familyname) %>% 
  count() %>% 
  spread(familyname, n) %>% 
  write_out(filename = "EJAFigS4")


######################################################################################################################################################
## Figure 4 The difference in probabilities of the crop yield loss under dry and wet conditions.                                                    ##
######################################################################################################################################################
if (F) {
  load("Data/R1OutputData/Result_prob.RData", verbose = T)
  result_prob
  brks <- seq(-100, 100, 20)
  cols = rev(pals::brewer.rdbu(length(brks) - 1))
  p1 <- result_prob %>% 
    mutate(prob = prob*100) %>% 
    mutate(pcols = cut(x = prob, breaks = brks, labels = cols)) %>% 
    mutate(pcols = as.character(pcols)) %>% 
    inner_join(x = maize_county, by = "id03") %>% 
    ggplot() +
    geom_sf(mapping = aes(fill = pcols), lwd = 0.01, show.legend = T, color = NA) +
    scale_fill_identity() +
    geom_sf(data = maize_shp, aes(), fill = NA, color = "black", lwd = 0.5) +
    geom_sf(data = bdy, mapping = aes(), lwd = 0.5) +
    labs(x = "", y = "", fill = "") +
    coord_sf(crs = crs) +
    scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
    scale_y_continuous(limits = c(2195234.1, 5921292.3)) +
    theme_basic()
  
  
  zones <- zones %>% mutate(zone = "Whole") %>% 
    bind_rows(zones) %>% 
    mutate(zone = factor(zone, levels = c("Whole","NCSM", "HHHSM", "SWCM")))
  
  
  p2 <- result_prob %>% 
    mutate(prob = prob*100) %>% 
    mutate(pcols = cut(x = prob, breaks = brks, labels = 1:(length(brks) - 1))) %>% 
    mutate(pcols = as.numeric(pcols)) %>% 
    inner_join(zones) %>% 
    count(zone, pcols) %>% 
    group_by(zone) %>% 
    mutate(n = n/sum(n)*100) %>% 
    ggplot() +
    geom_bar(mapping = aes(x = pcols, y = n, fill = cols[pcols]), stat = "identity") +
    scale_fill_identity() +
    facet_wrap(~zone) +
    scale_x_continuous(limits = c(1, length(brks) - 1), breaks = 1:length(brks) - 0.5, labels = brks) +
    scale_y_continuous(expand = c(0,0), limits = c(0, 50)) +
    labs(x = "Differences in yield loss probability (%)",
         y = "Percentage of counties (%)") +
    theme_basic() 
  p <- ggarrange(p1, p2, widths = c(1, 1.3), heights = c(1,1), labels = c("(a)", "(b)"))
  make_fig(figname = "EJAFig4", height = 6, width = 11)
  print(p)
  dev.off()
  result_prob %>% 
    mutate(prob = prob*100) %>% 
    mutate(pcols = cut(x = prob, breaks = brks)) %>% 
    # mutate(pcols = as.numeric(pcols)) %>% 
    inner_join(zones) %>% 
    count(zone, pcols) %>% 
    group_by(zone) %>% 
    mutate(prob = round(n/sum(n)*100, 2)) %>% 
    # spread(pcols, n) %>% 
    write_out(filename = "EJAFig4")
}





# new add for plot in r -------------------------------------------------------------------------------------------
if (T) {
  load("Data/R1OutputData/Result_prob1.RData", verbose = T)
  result_prob1 %>% pull(prob) %>% range()
  brks <- seq(-40, 60, 20)
  cols = rev(pals::brewer.rdbu(length(brks)))[-1]
  p1 <- result_prob1 %>% 
    mutate(prob = prob*100) %>% 
    mutate(pcols = cut(x = prob, breaks = brks, labels = cols)) %>% 
    mutate(pcols = as.character(pcols)) %>% 
    inner_join(x = maize_county, by = "id03") %>% 
    ggplot() +
    geom_sf(mapping = aes(fill = pcols), lwd = 0.01, show.legend = T, color = NA) +
    scale_fill_identity() +
    geom_sf(data = maize_shp, aes(), fill = NA, color = "black", lwd = 0.5) +
    geom_sf(data = bdy, mapping = aes(), lwd = 0.5) +
    labs(x = "", y = "", fill = "") +
    coord_sf(crs = crs) +
    scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
    scale_y_continuous(limits = c(2195234.1, 5921292.3)) +
    theme_basic()
  # cols = c("#67A9CF", "#D1E5F0", "#FDDBC7", "#EF8A62", "#B2182B")
  result_prob1 %>% 
    mutate(prob = prob*100) %>% 
    mutate(prob = cut(prob, breaks = seq(-40, 60, 20), labels = 1:5)) %>% 
    mutate(prob = as.numeric(prob)) %>% 
    mutate(prob = cols[prob]) %>% 
    inner_join(x = maize_county, by = "id03") %>% 
    ggplot() +
    geom_sf(mapping = aes(fill = prob, geometry = geometry), lwd = 0.01, show.legend = T, color = NA) +
    scale_fill_identity() +
    geom_sf(data = maize_shp, aes(), fill = NA, color = "black", lwd = 0.5) +
    geom_sf(data = bdy, mapping = aes(), lwd = 0.5) +
    labs(x = "", y = "", fill = "") +
    coord_sf(crs = crs) +
    scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
    scale_y_continuous(limits = c(2195234.1, 5921292.3)) +
    theme_basic()
  zones <- zones %>% mutate(zone = "Whole") %>% 
    bind_rows(zones) %>% 
    mutate(zone = factor(zone, levels = c("Whole","NCSM", "HHHSM", "SWCM")))
  
  p2 <- result_prob1 %>% 
    mutate(prob = prob*100) %>% 
    mutate(prob = cut(prob, breaks = seq(-40, 60, 20), labels = 1:5)) %>% 
    mutate(prob = as.numeric(prob)) %>% 
    inner_join(unique(zones)) %>% 
    count(zone, prob) %>% 
    group_by(zone) %>% 
    mutate(n = n/sum(n)*100) %>% 
    mutate(n = ifelse(prob < 3, -n, n))
  data.table::fwrite(p2, "Data/R1OutputData/EJAFig4.txt")
  p2 <- ggplot() + 
    geom_bar(data = filter(p2, n < 0), aes(x = zone, y = n, fill = cols[prob]),stat = "identity") +
    geom_bar(data = filter(p2, n > 0), aes(x = zone, y = n, fill = cols[prob]),stat = "identity") +
    scale_fill_identity() +
    geom_hline(yintercept = 0) +
    theme_basic() +
    labs(y = "Percentage of counties (%)", x = NULL)
  
  dt <- data.frame(x1 = brks[-length(brks)], x2 = brks[-1], y1 = 0, y2 = 1, col = cols)
  p3 <- ggplot(dt) +
  geom_rect(dt, mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = col)) +
    scale_fill_identity() +
    geom_rect(mapping = aes(xmin = min(brks), xmax = max(brks), ymin = 0, ymax = 1), fill = NA, color = "black") +
    # scale_x_continuous(breaks = brks, labels = brks, expand = c(0.2, 0.2)) +
    scale_y_continuous(expand = c(0.0,0.01)) +
    theme_bw() +
    theme(axis.text.x = element_text(colour = "black", size = 12), 
          axis.text.y = element_blank() ,
          axis.ticks.length.x = unit(2, "pt"),
          axis.ticks.x = element_line(colour = "black")
    ) +
    # coord_fixed(ratio = 1) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.ticks.length.y = unit(0, 'pt')) +
    theme(plot.title = element_text(hjust = 0.5, size = 12)) +
    ggtitle(label = "Differences in yield loss probability (%)")
  
  pf <- plot_grid(p1, p2, rel_heights = c(1, 0.8), labels = c("(a)", "(b)"), label_size = 12, label_fontface = "plain")
  pf <- plot_grid(pf, p3, rel_heights = c(5, 1), rel_widths = c(5, 1), nrow = 2)
  make_fig(figname = "EJAFig4", height = 5, width = 8)
  print(pf)
  dev.off()
}


######################################################################################################################################################
## Figur 5 The conditional probability of yield loss risk based on the derived copula simulations under different drought conditions.               ##
######################################################################################################################################################
levels <- paste0("Yield loss|", c("Moderate Dry", "Severe Dry", "Extreme Dry"))
load("./Data/R1OutputData/Result.RData")

###############
# p1 <- result %>% 
#   filter(str_sub(type, -3,-1) == "Dry") %>% 
#   mutate(type = str_remove(type, "ly")) %>% 
#   mutate(type = paste0("Yield loss|", type)) %>% 
#   mutate(type = factor(type, levels = levels)) %>% 
#   # mutate(prob = cut(prob*100, seq(0, 100, 10), seq(10, 100, 10))) %>% 
#   inner_join(maize_county, by = "id03") %>% 
#   ggplot() +
#   geom_sf(mapping = aes(geometry = geometry, fill = prob*100), lwd = 0.1, color = NA) +
#   facet_wrap(~type) +
#   geom_sf(data = maize_shp, aes(), fill = NA, color = "black", lwd = 0.4) +
#   geom_sf(data = bdy, mapping = aes(), lwd = 0.1) +
#   geom_ellipse(aes(x0 = 115, y0 = 39, a = 10, b = 4, angle = pi / 5), linetype = 2, lwd = 0.5, color = "black") +
#   scale_fill_distiller(palette = "OrRd", direction = 1, limits = c(0,100), breaks = seq(0, 100, 10)) +
#   # scale_fill_manual(values = pals::brewer.orrd(10)) +
#   guides(fill = guide_colorbar(title.position = "top", nbin = 11, frame.colour = "black",ticks.colour = "black",
#                                title.hjust = 0.5, barwidth = unit(5, "in"))) +
#   # guides(fill = guide_legend(ncol = 10, label.position = "bottom", title.position = "top", title.hjust = 0.5, label.hjust = 1.2)) +
#   scale_x_continuous(breaks = seq(100, 130, 10)) + 
#   scale_y_continuous(breaks = seq(20, 50, 10)) + 
#   labs(x = "", y = "", fill = "Conditional probability (%)") +
#   coord_sf(xlim = c(95, 136), ylim = c(20, 54)) +
#   theme_map() +
#   theme(legend.position = "bottom")
# 
# make_fig(figname = "R1Fig5", height = 5, width = 11)
# print(p1)
# dev.off()
############

brks = seq(0, 100, 10)
cols = pals::brewer.orrd(length(brks) - 1)

p1 <- result %>%
  filter(str_sub(type, -3,-1) == "Dry") %>%
  mutate(type = str_remove(type, "ly")) %>%
  mutate(type = paste0("Yield loss|", type)) %>%
  mutate(type = factor(type, levels = levels)) %>%
  mutate(prob = prob*100) %>% 
  inner_join(maize_county, by = "id03") %>% 
  mutate(pcols = cut(x = prob, breaks = brks, labels = cols)) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = pcols), color = NA, lwd = 0.01) +
  scale_fill_identity() +
  geom_sf(data = maize_shp, aes(), fill = NA, color = "black", lwd = 0.5) +
  geom_sf(data = bdy, mapping = aes(), lwd = 0.5) +
  labs(x = "", y = "", fill = "") +
  coord_sf(crs = crs) +
  scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
  scale_y_continuous(limits = c(2195234.1, 5921292.3)) +
  facet_wrap(~type) +
  theme_basic()

dt <- data.frame(x1 = brks[-length(brks)], x2 = brks[-1], y1 = 0, y2 = 1, col = cols)
p2 <- ggplot() +
  geom_rect(dt, mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = col)) +
  scale_fill_identity() +
  geom_rect(mapping = aes(xmin = min(brks), xmax = max(brks), ymin = 0, ymax = 1), fill = NA, color = "black") +
  scale_x_continuous(breaks = brks, labels = brks, expand = c(0.1, 0.1)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_basic() +
  # coord_fixed(ratio = 1) +
  theme(plot.title = element_text(hjust = 0.5,size = 12, face = "plain"),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.length.y = unit(0, 'pt')) +
  ggtitle(label = "Yield loss probability (%)")

p <- ggarrange(p1, p2, nrow = 2, heights = c(6, 1), widths = c(4, 3))

make_fig(figname = "EJAFigS7", height = 6, width = 12, bg = "white")
print(p)
dev.off()


p1 <- result %>%
  filter(str_sub(type, -3,-1) == "Dry") %>%
  mutate(type = str_remove(type, "ly")) %>%
  mutate(type = paste0("Yield loss|", type)) %>%
  mutate(type = factor(type, levels = levels)) %>%
  mutate(prob = prob*100) %>% 
  inner_join(zones, by = "id03") %>% 
  select(zone, id03, type, prob) %>% 
  mutate(pcols = cut(x = prob, breaks = brks, labels = cols)) %>% 
  group_by(zone, type, pcols) %>% 
  count() %>% 
  group_by(zone, type) %>% 
  mutate(n = (n/sum(n)*100)) %>% 
  mutate(pcols = as.character(pcols)) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = type, y = n, fill = pcols), width = 0.6, lwd = 0.3, color = "black", stat = "identity") +
  facet_grid(zone~.) +
  # scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, 20), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_identity()  +
  coord_flip() +
  labs(y = "Perentage of couties (%)", x = NULL) +
  theme_basic() +
  theme(# axis.ticks.y = element_blank(),
        # panel.border = element_blank(),
        # panel.grid = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = 2), 
        # strip.background = element_blank(),
        panel.spacing = unit(x = 5, units = "mm"),
        strip.text = element_text(colour = "black", size = 11),
        axis.text = element_text(colour = "black"))

p <- plot_grid(p2, p1, rel_widths = c(0.8, 1), rel_heights = c(0.2, 1), ncol = 1)


make_fig(figname = "EJAFig5", height = 5, width = 8)
print(p)
dev.off()


result %>%
  filter(str_sub(type, -3,-1) == "Dry") %>%
  mutate(type = str_remove(type, "ly")) %>%
  mutate(type = paste0("Yield loss|", type)) %>%
  mutate(type = factor(type, levels = levels)) %>%
  mutate(prob = prob*100) %>% 
  inner_join(zones, by = "id03") %>% 
  select(zone, id03, type, prob) %>% 
  mutate(pcols = cut(x = prob, breaks = brks)) %>% 
  group_by(zone, type, pcols) %>% 
  count() %>% 
  spread(pcols, n) %>% 
  write_out(filename = "EJAFig5")


####### 桑基图 不可用 看不到规律
# library(ggalluvial)
# 
# brks <- c(0, 30, 60, 80, 100)
# cols = pals::brewer.orrd(length(brks) - 1)
# p <- list()
# for (rg in unique(zones$zone)) {
#   tmp <- result %>%
#     filter(str_sub(type, -3,-1) == "Dry") %>%
#     mutate(type = str_remove(type, "ly")) %>%
#     mutate(type = paste0("Yield loss|", type)) %>%
#     mutate(type = factor(type, levels = levels)) %>%
#     mutate(prob = prob*100) %>%
#     inner_join(zones, by = "id03") %>%
#     mutate(pcols = cut(x = prob, breaks = brks, labels = cols)) %>%
#     filter(zone == rg) %>%
#     as_tibble() %>%
#     select(-prob) %>%
#     spread(key = type, pcols) %>%
#     mutate(news = str_c(`Yield loss|Moderate Dry`, `Yield loss|Severe Dry`, `Yield loss|Extreme Dry`)) %>%
#     gather(type, pcols, -id03, -zone, -news) %>%
#     group_by(news, type, pcols) %>%
#     count() %>%
#     mutate(type = factor(x = type, levels = c("Yield loss|Moderate Dry", "Yield loss|Severe Dry", "Yield loss|Extreme Dry"))) %>%
#     group_by(type) %>%
#     mutate(n = n/sum(n)*100)
#   # p[[rg]] <- #
#     ggplot(tmp,
#          aes(x = type, stratum = pcols, alluvium = news,
#              y = n,
#              fill = pcols)) +
#     scale_x_discrete(expand = c(.01, .01)) +
#     geom_flow(curve_type = "sine", alpha = 0.8) +
#     geom_stratum() +
#     scale_fill_identity() +
#     coord_flip() +
#     theme_minimal() +
#     theme(panel.grid = element_blank()) +
#     labs(x = rg, y = NULL)
# }
# 
# ggarrange(plotlist = p, nrow = 4, labels = sprintf("(%s)", letters[1:4]))
# p2 <- result %>% 
#   filter(str_sub(type, -3,-1) == "Dry") %>% 
#   mutate(type = str_remove(type, "ly")) %>% 
#   mutate(type = paste0("Yield loss|", type)) %>% 
#   mutate(type = factor(type, levels = levels)) %>% 
#   # inner_join(maize_county, by = "id03") %>% 
#   mutate(prob = cut(prob*100, seq(0, 100, 10), seq(10, 100, 10))) %>% 
#   # mutate(prob = as.numeric(prob)) %>% 
#   # st_set_geometry(value = NULL) %>% 
#   group_by(prob, type) %>% 
#   summarise(pb = n()) %>% 
#   ggplot() +
#   geom_bar(aes(x = type, y = pb, fill = prob), stat = "identity", position = position_fill(reverse = T)) +
#   scale_fill_manual(values = pals::brewer.orrd(10)) +
#   scale_y_continuous(labels = scales::percent) + 
#   labs(x = NULL, y = "Percentage of county", fill = "Conditional Probability (%)") +
#   guides(fill = guide_legend(label.vjust = 0, title.position = "left", title.hjust = 0.5)) +
#   coord_flip() +
#   theme_bw() +
#   theme(legend.position = "none")

# make_convert(figname = "R2Fig6a")

######################################################################################################################################################
## Figure 6 Boxplots for the distributions of occurrence likelihood under different drought type across different region in China.                  ##
######################################################################################################################################################
dta <- result %>%
  filter(str_sub(type, -3,-1) == "Dry") %>%
  mutate(type = str_remove(type, "ly")) %>%
  mutate(type = paste0("Yield loss|", type)) %>%
  mutate(type = factor(type, levels = levels)) %>%
  mutate(prob = prob*100) %>% 
  inner_join(zones, by = "id03")

library(agricolae)

dt <- lapply(unique(dta$zone), function(x) {
  model <- aov(prob~type, data = dta %>% filter(zone == x))
  out <- LSD.test(model,"type", p.adj = "none")
  out$groups %>% 
    as.data.frame() %>% 
    mutate(type = rownames(.),
           zone = x) %>% 
    select(-prob)
}) %>% 
  do.call(rbind, .) %>% 
  mutate(zone = factor(x = zone, levels = c("Whole",  "NCSM", "HHHSM", "SWCM")),
         type = factor(x = type, levels = levels)) 

# #################
# p <- ggplot(data = dta, aes(x = zone, y = prob, fill = factor(type))) +
#   stat_boxplot(geom = 'errorbar', width = 0.2, position = position_dodge(width = 0.75)) +
#   geom_boxplot(width = 0.6, position = position_dodge(width = 0.75)) +
#   # stat_compare_means(aes(label = paste0("p = ", ..p.format..))) +
#   geom_text(data = dd, aes(x = var_facet, y = prob, label = Letters),
#             vjust = -1.5, hjust = -.5,position = position_dodge(width = 0.75), show.legend = F) +
#   stat_summary(fun.y = mean, geom = "point", position = position_dodge(width = 0.75), show.legend = F) +
#   geom_vline(xintercept = 1.5, linetype = 2) +
#   scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2), labels = seq(0,100, 20)) +
#   # scale_x_discrete(breaks = zone, labels = c("Whole", "HHHSM", "NCSM", "SWCM")) +
#   scale_fill_manual(values = c("#2C7BB6", "#ABD9E8", "#FDAE61")) +
#   labs(x = "Maize growing region", y = "Conditional probabilities (%)", 
#        fill = NULL) +
#   theme_bw() +
#   theme(panel.grid.major.y = element_line(colour = "grey", linetype = 2),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.text = element_text(colour = "black"), 
#         legend.position = "top")
# make_fig(figname = "R1Fig6", height = 6, width = 6)
# plot(NA, ann = F, axes = F, xlim = c(0.5, 12.5), ylim = c(-0.1, 1.1))
# abline(h = seq(0, 1, 0.2), lty = 2, col = "lightgrey")
# p <- boxplot(prob~type+zone, data = dta, col = c("#2C7BB6", "#ABD9E8", "#FDAE61"), add = T,
#              xaxs = "n", ann = F, axes = F, whisklty = 1, medlwd = 1)
# 
# points(1:12, aggregate(prob~type+zone, data = dta, FUN = mean)[,3], pch = 19)
# text(1:12,  p$stats[5,] + yinch(0.1), labels = c("a", "a", "b", "a", "ab", "b", "a", "a", "b", 
#                                                  "a","a","a"))
# axis(1, c(2,5,8,11), c("Whole", "HHHSM", "NCSM", "SWCM"))
# axis(2, seq(0, 1, 0.2), labels = seq(0, 100, 20), las = 2)
# abline(v = 3.5, col = "black", lty = 2)
# legend(0.5, yinch(0.05), pch = 15, col = c("#2C7BB6", "#ABD9E8", "#FDAE61"), legend = levels, 
#        box.col = NA, ncol = 3, bg = NA, cex = 0.7)
# title(xlab = "Maize growing region", ylab = "Conditional probabilities (%)", line = 2)
# box()
# dev.off()
#############################
## Plan B for Fig 6 bootstrap prob calculation
# make_fig(figname = "R2Fig6b", height = 5, width = 4)
rt <- list()
set.seed(13457)
for (i in c("Whole", "HHHSM", "NCSM", "SWCM")) {
  tmp <- dta %>%
    tidyr::spread(type, prob) %>%
    filter(zone == i)
  rs <- matrix(data = NA, nrow = 1000, ncol = 3)
  for (j in 1:1000) {
    x <- sample(1:nrow(tmp), size = nrow(tmp), replace = T)
    rs[j,] <- apply(tmp[x, 3:5], 2, mean)
  }
  rt[[i]] <- rbind(apply(tmp[, 3:5], 2, mean),
                       apply(rs, 2, quantile, probs = c(0.025, 0.975))) %>%
    t %>%
    as.data.frame() %>%
    mutate(type = rownames(.)) %>%
    setNames(c("mean", "lower", "upper", "type")) %>%
    mutate(zone = i)
}
rt <- do.call(rbind, rt) %>%
  mutate(type = factor(x = type, levels = levels),
         zone = factor(x = zone, levels = c("Whole", "NCSM", "HHHSM",  "SWCM")))

p <- inner_join(dt, rt) %>% 
  ggplot(data = ., mapping = aes(zone, mean, fill = type)) +
  geom_col(position = position_dodge(width = 0.9), color = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.9), width = 0.25) +
  geom_text(aes(y = upper + 4, group = type, label = groups), position = position_dodge(width = 0.9)) +
  geom_vline(xintercept = 1.5, linetype = 2) +
  scale_y_continuous(limits = c(0, 82), breaks = seq(0, 80, 20), expand = c(0, 0)) +
  scale_fill_manual(values = c("#2C7BB6", "#ABD9E8", "#FDAE61")) +
  labs(x = NULL, y = "Yield loss probability (%)", fill = NULL) +
  theme_basic() +
  theme(legend.position = "top")
p
make_fig(figname = "EJAFig6", width = 7, height = 5)
print(p)
dev.off()

inner_join(dt, rt) %>% 
  mutate_at(vars(mean, lower, upper), .funs = list(~round(., 2))) %>%
  write_out(filename = "EJAFig6")


## convert figures in pdf file to TIFF format
# make_convert <- function(path = "Figures/", figname) {
#   fig_in <- paste0(path, figname, ".pdf")
#   fig_out <- paste0(path, figname, ".TIFF")
#   command = paste0("magick.exe convert -density 600 ", fig_in, " -quality 100 -resize 60%x60% -compress zip ", fig_out)
#   # -quality 100 
#   system(command)
# }
# 
# sapply(
#   list.files(path = "./Figures/", pattern = "R4") %>% str_subset(pattern = "pdf") %>% str_remove(".pdf"),
#   make_convert, path = "./Figures/")



## Plot FigS7 to support the probability and the correlation
load("Data/R1OutputData/Result_prob1.RData")
load("./Data/InputData/Result_Yield_spei_scale.RData")
dta <- maize_county %>% 
  inner_join(yield_spei, by = "id03") 
dta$zone <- droplevels(dta$zone)
dta$zone <- factor(dta$zone, levels = c("North_SpringMaize", "Huanghuahai_SummerMaize", "Southwest_Maize"), labels = c("NCSM", "HHHSM", "SWCM"))

library(ggpubr)

formula <- y ~x
p <- dta %>% inner_join(result_prob1) %>% 
  mutate(cols = ifelse(prob > 0, "red", "blue"),
         sign = ifelse(pvalue > 0.1, "P > 0.1", "P < 0.1")) %>% 
  ggplot(mapping = aes(x = correlation, y = prob)) +
  geom_point(aes(color = cols, shape = sign), alpha = 0.5) +
  # geom_smooth(method = "lm", color = "black", se = T) +
  stat_smooth(aes(correlation, prob), method = "lm", formula = formula, color = "black", lwd = 0.5) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),show.legend = F,
    formula = formula 
    ) +
  geom_hline(yintercept = 0, linetype = 1, color = "grey") +
  geom_vline(xintercept = c(-0.31, 0.31), linetype = 2, color = "grey") +
  scale_color_identity() +
  # scale_color_manual(values = c("blue", "red")) +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.25)) +
  scale_y_continuous(limits = c(-0.6, 0.6), breaks = seq(-0.6, 0.6, 0.2), labels = round(seq(-0.6, 0.6, 0.2)*100)) +
  labs(x = "Maximum correlation coefficient", y = "Difference in yield loss probability (%)", shape = NULL) +
  theme_basic() +
  theme(legend.position = c(0.8, 0.2))
make_fig(figname = "EJAFigS10", width = 5, height = 5)
print(p)
dev.off()

