rm(list = ls())
source("./Script/00_Helper.R")
source("Script/00_Functions.R")

# Step 01 Load necessary data -------------------------------------------------------------------------------------
zones <- c("NCSM", "HHHSM", "SWCM")

load("Data/Input/GeoInfo.RData")

aoi <- maize_county %>%
  filter(id03 %in% ids) %>%
  mutate(zone = as.character(zone)) %>%
  mutate(zone = factor(zone, labels = c("HHHSM", "NCSM", "SWCM"))) %>%
  mutate(zone = factor(zone, levels = zones))

ids <- aoi$id03

# Step 02 Deal with yearly maize yield ----------------------------------------------------------------------------

# 01 detrending yield with different method

aoi_yld <- openxlsx::read.xlsx(xlsxFile = "Data/Input/M1_Yield_data_calibrated.xlsx", sheet = "Maize") %>%
  data.table() %>% 
  melt.data.table(id.vars = "id03", variable.name = "year", value.name = "yield") %>% 
  .[, year := as.integer(str_extract(year, "\\d{4}"))] %>% 
  .[order(id03, year)] %>% 
  .[, `:=`(
    dy_ssa = detrending(yield, detrending_type = "ssa"),
    dy_best = detrending(yield, detrending_type = "best_fit"),
    ## Quantify extreme climate impacts on crop yield as Li et al., (2019), GCB
    py_ssa = detrending(yield, detrending_type = "ssa_pct"),
    py_best = detrending(yield, detrending_type = "best_fit_pct")
  ), .(id03)] %>%
  .[, `:=`(
    ay_ssa = yield - dy_ssa,
    ay_best = yield - dy_best
  )] %>% 
  .[id03 %in% ids]

# simple glance of detrending result
aoi_yld[id03 == 130431] %>%
  ggplot() +
  geom_line(mapping = aes(year, yield)) +
  geom_line(mapping = aes(year, ay_ssa), color = "red") +
  geom_line(mapping = aes(year, ay_best), color = "blue")

# 02 Statistical inference of detrending

aoi_yld_sts <- aoi_yld %>%
  .[, .(
    R2_ssa = R2Fun(ay_ssa, yield),
    R2_best = R2Fun(ay_best, yield),
    R2adj_ssa = R2Funadj(ay_ssa, yield),
    R2adj_best = R2Funadj(ay_best, yield),
    RMSE_ssa = RMSEFun(ay_ssa, yield),
    RMSE_best = RMSEFun(ay_best, yield)
  ), .(id03)]


save(list = c("aoi_yld", "aoi_yld_sts"), file = "Data/01_AOI_Yield.RData")
save(list = c("aoi", "maize_shp", "zones"), file = "Data/00_AOI.RData")


