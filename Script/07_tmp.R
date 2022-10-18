rm(list = ls())
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(sf)
library(raster)
library(lubridate)
library(tibble)
select <- dplyr::select
extract <- raster::extract
cor.pvalue <- function(x, y, alternative = "two.sided", method = "pearson") {
  p <- cor.test(x, y, alternative = alternative, method = method)
  p$p.value
}


# crs = "+proj=aea +lat_1=25 +lat_2=47 +lat_0=40 +lon_0=105 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84"
crs = "+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
## Ways to show the attribute of RData
# load("mydata.RData", verbose=TRUE) 
# load( "mydata.RData",ex<-new.env() )
# content=ls.str(ex)
## Assuming there is only one object saved in mydata.RData
#bar <- get(load("mydata.RData"))
## for many object in a RData
# temp.space <- new.env()
# bar <- load('mydata.RData', temp.space)
# the.object <- get(bar, temp.space)
# rm(temp.space)


load("./Data/InputData/Result_Yield_spei_scale.RData", verbose = T)
load("./Data/InputData/Yield_information.RData", verbose = T)
load("Data/InputData/Yield_copula.RData", verbose = T)
load("Data/InputData/Season_climate.RData", verbose = T)
load("Data/Maize_shp.RData", verbose = T)
load("Data/Study_cty.RData", verbose = T)
head(yield_basic)
head(yield_spei)
head(yield_copula)
head(spei_climate)

## 判断所有县域比例是否大于2%
rr <- list.files("./Data/AuxiliaryData/", pattern = "spam", full.names = T) %>% 
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
rr <- filter(rr, all > 0)
# area <- st_area(rr) %>% units::drop_units() 
# area <- area/10^4
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
p1

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

library(cowplot)
plot_grid(p1, p2)

## 筛选种植比例所得到的可能不用的县数据
rr %>% 
  mutate(pop = all/area) %>% 
  mutate(pop = ifelse(pop > 0.02, '1', "0"),
         ratio = ifelse(irrigate/all > 0.1, "1", "0")) %>%
  select(id03, pop, ratio) %>% 
  st_set_geometry(value = NULL) -> ids_pop

ids_pop %>% filter(pop == "1") %>% pull(id03) -> ids
save(list = "ids", file = "./Data/InputData/ids.RData")

## 查看可能不用县的相关系数情况
yield_spei %>% 
  # filter(!(id03 %in% ids_pop)) %>% 
  mutate(corr = ifelse(correlation > 0, "+", "-")) %>% 
  inner_join(maize_county) %>% 
  inner_join(ids_pop) %>% 
  group_by(zone, corr, pop, ratio) %>% 
  count() %>% 
  view()


## 进行各种数据的筛选与验证



inner_join(spei_climate, yield_final, by = c("id03", "year")) %>% 
  group_by(id03, month, scale) %>%
  summarise(correlation = cor(yd_ano, spei),
            pvalue = cor.pvalue(yd_ano, spei)) %>% 
  group_by(id03) %>% 
  filter(abs(correlation) == max(abs(correlation))) %>% 
  # filter(correlation == max(correlation)) %>% 
  pull(correlation) -> s

sum(s > 0) / length(s)

yield_final %>% 
  group_by(id03) %>% 
  mutate(yd_max = max(abs(yd_ano))) %>% 
  filter(yd_max < 200) %>% 
  pull(id03) %>% unique() %>% length()

inner_join(spei_climate, yield_final, by = c("id03", "year")) %>% 
  group_by(id03) %>% 
  mutate(yd_max = max(abs(yd_ano))) %>% 
  filter(yd_max < 100) %>% 
  group_by(id03, month, scale) %>%
  summarise(correlation = cor(yd_ano, spei),
            pvalue = cor.pvalue(yd_ano, spei)) %>% 
  group_by(id03) %>% 
  # filter(abs(correlation) == max(abs(correlation))) %>% 
  filter(correlation == max(correlation)) %>% 
  filter(correlation < 0) %>% 
  pull(id03) -> ids


yield_final %>% filter(id03 %in% ids) %>% 
  ggplot() +
  geom_line(mapping = aes(year, yd_ano)) +
  facet_wrap(~id03)

yield_final %>% filter(id03 == 522636) %>% 
  ggplot(mapping = aes(year, yield)) +
  geom_line() +
  geom_smooth(method = "lm")
library(broom)
dy <- yield_final %>% filter(id03 == 522636) %>% 
  do(model = augment(lm(yield~year, data = .))) %>% 
  unnest(model) %>% 
  ggplot(mapping = aes(year, `.resid`)) +
  geom_line() +
  geom_smooth(method = "lm")


spei_climate %>% filter(id03 == 522636) %>% 
  ggplot() +
  geom_line(aes(x = year, spei, color = factor(scale))) +
  geom_line(data = dy, mapping = aes(year, `.resid`)) +
  facet_wrap(~month)
