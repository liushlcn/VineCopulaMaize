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


# Step 02 Deal with yearly maize yield ----------------------------------------------------------------------------

# 01 detrending yield with different method

yield <- openxlsx::read.xlsx(xlsxFile = "Data/Input/M1_Yield_data_calibrated.xlsx") %>%
  gather(year, yield, -id03) %>%
  mutate(year = as.integer(str_extract(year, "\\d{4}"))) %>%
  # filter(year >= 1981) %>%
  data.table() %>%
  .[, `:=`(
    yd_ssa = detrending(yield, detrending_type = "ssa"),
    yd_best = detrending(yield, detrending_type = "best_fit"),
    ## Quantify extreme climate impacts on crop yield as Li et al., (2019), GCB
    yd_pssa = detrending(yield, detrending_type = "ssa_pct"),
    yd_pbest = detrending(yield, detrending_type = "best_fit_pct")
  ), .(id03)] %>%
  .[, `:=`(
    yp_ssa = yield - yd_ssa,
    yp_best = yield - yd_ssa
  )]

yield[,yd_ssa] %>% range()
yield[abs(yd_ssa) > 2] %>% pull(id03) %>% unique()

# simple glance of detrending result
yield[id03 == 652722] %>%
  ggplot() +
  geom_line(mapping = aes(year, yield)) +
  geom_line(mapping = aes(year, yp_ssa), color = "red") +
  geom_line(mapping = aes(year, yp_best), color = "blue")

# 02 Statistical inference of detrending

yield_stats <- yield %>%
  .[, .(
    R2_ssa = R2Fun(yp_ssa, yield),
    R2_best = R2Fun(yp_best, yield),
    R2adj_ssa = R2Funadj(yp_ssa, yield),
    R2adj_best = R2Funadj(yp_best, yield),
    RMSE_ssa = RMSEFun(yp_ssa, yield),
    RMSE_best = RMSEFun(yp_best, yield)
  ), .(id03)]


save(list = c("yield", "yield_stats"), file = "Data/01_YieldInf.RData")
save(list = c("aoi", "maize_shp", "zones"), file = "Data/00_AOI.RData")
