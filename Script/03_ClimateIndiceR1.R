rm(list = ls())

library(tidyverse)
library(magrittr)
library(sf)

select <- dplyr::select
extract <- raster::extract

load("Data/Maize_shp.RData")
load("Data/InputData/Climate.RData")

## Step 01 Calculate the SPEI of different scale and summarize season information
calc_spei <- function(data, scale, ...) {
  dr <- SPEI::spei(data = data, scale = scale, ...)
  as.vector(dr$fitted)
}

calc_spi <- function(data, scale, ...) {
  dr <- SPEI::spi(data = data, scale = scale, ...)
  as.vector(dr$fitted)
}


calendar <- maize_county %>% 
  select(zone, id03, plant, harvest) %>% 
  sf::st_set_geometry(value = NULL)

# ggplot() +
#   geom_sf(data = chn02, aes(), fill = NA) +
#   geom_sf(data = maize_shp, aes(), fill = NA, color = "red")

## n should be the value range of plant and harvest
## here we set as 6 
# range(calendar$plant)
# range(calendar$harvest)
n = 6 

for (i in 1:n) {
  climate %<>%  mutate(!!paste0("spei_", i) := calc_spei(data = bal, scale = i))
}

## summarize seasonal climate information for 
season_climate <- climate %>% 
  left_join(calendar, by = "id03") %>% 
  filter(month >= plant, month <= harvest) %>% 
  group_by(zone, id03, year) %>% 
  summarize_at(.vars = vars(c("pre", "pet")), .funs = list(sum))


## 
## this is not correct and need action
spei_climate <- climate %>% 
  left_join(calendar, by = "id03") %>% 
  filter(month >= plant, month <= harvest) %>% 
  select(zone, id03, year, month, plant, harvest, starts_with("spei")) %>% 
  gather(scale, spei, starts_with("spei")) %>% 
  mutate(scale = as.integer(str_extract(string = scale, pattern = "\\d{1}"))) %>%
  ## filter scale belong to crop calendar at county level
  filter(scale <= harvest - plant + 1) %>%
  ## this line determine the scales and start month for the spei 
  filter(month - scale + 1 >= plant) %>% 
  # mutate(month = month - scale + 1) %>% 
  select(-plant, -harvest)

save(list = c("season_climate", "spei_climate"), file = "Data/InputData/Season_climate.RData")

## Step 02 Deal with yearly maize yield 
cty <- calendar %>% pull(id03)
yield <- openxlsx::read.xlsx(xlsxFile = "Data/InputData/M1_Yield_data_calibrated.xlsx") %>% 
  gather(year, yield, -id03) %>% 
  mutate(year = as.integer(str_extract(year, "\\d{4}"))) %>% 
  filter(year >= 1981) %>% 
  inner_join(calendar, by = "id03") %>% 
  select(-plant, -harvest) %>% 
  select(zone, everything())

## yield trend and yield anomoly

threshold <- function(x, na.rm = FALSE)  {
  -sd(x, na.rm = na.rm)/mean(x, na.rm = na.rm)
}

yield_basic <- yield %>% 
  group_by(zone, id03) %>% 
  summarize(yd_avg = mean(yield), 
            yd_loss = threshold(yield, na.rm = T))

model <- yield %>% 
  group_by(id03) %>% 
  do(model = lm(yield~I(year-1980), data = .))

yield_rsquare <- model %>% 
  broom::glance(model) %>% 
  select(id03, `r.squared`)

yield_trend <- model %>% 
  broom::tidy(model) %>% 
  filter(term != "(Intercept)") %>% 
  select(id03, estimate, `p.value`)

yield_basic %<>%
  left_join(yield_rsquare, by = "id03") %>% 
  left_join(yield_trend, by = "id03")

yield_final <- model %>% 
  broom::augment(model) %>% 
  ## Quantify extreme climate impacts on crop yield as Li et al., (2019), GCB
  mutate(yd_ano = (yield - `.fitted`)/`.fitted`*100) %>% 
  select(id03, `I.year...1980.`, yield, yd_ano) %>% 
  rename(year = `I.year...1980.`) %>% 
  mutate(year = year + 1980)

save(list = c("yield_basic", "yield_final"), file = "Data/InputData/Yield_information.RData")

## Yield anomaly data combine with SPEI

cor.pvalue <- function(x, y, alternative = "two.sided", method = "pearson") {
  p <- cor.test(x, y, alternative = alternative, method = method)
  p$p.value
}

yield_spei <- inner_join(spei_climate, yield_final, by = c("id03", "year")) %>% 
  group_by(id03, month, scale) %>%
  summarise(correlation = cor(yd_ano, spei),
            pvalue = cor.pvalue(yd_ano, spei)) %>% 
  group_by(id03) %>% 
  filter(abs(correlation) == max(abs(correlation)))
  # %>% mutate(month = month - scale + 1)

yield_season <- spei_climate %>% 
  group_by(id03) %>% 
  filter(month == max(month), scale == max(scale)) %>% 
  inner_join(yield_final, by = c("id03", "year"))

yield_copula <- inner_join(spei_climate, yield_final, by = c("id03", "year")) %>% 
  inner_join(yield_spei, by = c("id03", "month", "scale")) %>% 
  select(zone, id03, year, spei, yield, yd_ano) %>% 
  as.data.frame()

save(list = c("yield_spei", "yield_season","yield_copula"), file = "Data/InputData/Yield_copula.RData")

## Note that the information about start month was not estimated