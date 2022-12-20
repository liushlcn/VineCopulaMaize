rm(list = ls())
source("./Script/00_Helper.R")
source("Script/00_Functions.R")

# Step 01 Load necessary data -------------------------------------------------------------------------------------
load("Data/00_AOI.RData")
ids <- aoi$id03
# Step 02 Deal with yearly maize yield ----------------------------------------------------------------------------
standardised <- function(x) x/sd(x)

# 01 detrending yield with different method
system.time({
  aoi_yld <- openxlsx::read.xlsx(xlsxFile = "Data/Input/M1_Yield_data_calibrated.xlsx", sheet = "Maize") %>%
    data.table() %>% 
    melt.data.table(id.vars = "id03", variable.name = "year", value.name = "yld") %>% 
    .[, year := as.integer(str_extract(year, "\\d{4}"))] %>% 
    .[order(id03, year)] %>% 
    .[, `:=`(
      ## we calculate the detrending yield of maize during 1981-2014 with different methods
      ## Quantify extreme climate impacts on crop yield as Li et al., (2019), GCB
      yldssa = detrending(yld, detrending_type = "ssa"),
      yldlin = detrending(yld, detrending_type = "lin"),
      yldquad = detrending(yld, detrending_type = "quad"),
      yldbest = detrending(yld, detrending_type = "best_fit")
    ), .(id03)] %>%
    .[, c("syldssa", "syldlin", "syldquad", "syldbest") := lapply(.SD, standardised), .(id03), 
      .SDcols = c("yldssa", "yldlin", "yldquad", "yldbest")] %>% 
    .[id03 %in% ids]
})

idd = 211481  
aoi_yld[id03 == idd] %>% {plot(.$year, .$yld, type = "l", ylim = c(-3, 8))}
aoi_yld[id03 == idd] %>% {lines(.$year, .$yld - .$yldbest, lty = 2)}
aoi_yld[id03 == idd] %>% {lines(.$year, .$syldssa, type = "l")}
abline(h = 0, lty = 2, col = "gray")

aoi_yld[id03 == idd] %>% {plot(.$year, .$yld)}
aoi_yld[id03 == idd] %>% {lines(.$year, .$yld - .$yldssa)}
aoi_yld[id03 == idd] %>% {lines(.$year, .$yld - .$yldlin)}
aoi_yld[id03 == idd] %>% {lines(.$year, .$yld - .$yldquad)}
aoi_yld[id03 == idd] %>% {lines(.$year, .$yld - .$yldbest)}

aoi_yld[, .(mean(yldssa)), .(id03)] %>% pull(V1) %>% hist()
aoi_yld %>% pull(yldssa) %>% ecdf() %>% plot()

# simple glance of detrending result
aoi_yld[id03 == idd] %>%
  ggplot(mapping = aes(year, yld)) +
  geom_line() +
  geom_smooth(method = "loess", se = F, mapping = aes(color = "Loess")) +
  geom_smooth(method = "gam", se = F, mapping = aes(color = "GAM")) +
  geom_line(mapping = aes( y = yld - yldssa, color = "SSA"))
  # geom_line(mapping = aes(year, yldssa), color = "red") +
  # geom_line(mapping = aes(year, yldbest), color = "blue")

# 02 Statistical inference of detrending

aoi_yld_sts <- aoi_yld %>%
  .[, .(
    R2_ssa = R2Fun(yld - yldssa, yld),
    R2_best = R2Fun(yld - yldbest, yld),
    R2adj_ssa = R2Funadj(yld - yldssa, yld),
    R2adj_best = R2Funadj(yld - yldbest, yld),
    RMSE_ssa = RMSEFun(yld - yldssa, yld),
    RMSE_best = RMSEFun(yld - yldbest, yld)
  ), .(id03)]


save(list = c("aoi_yld", "aoi_yld_sts"), file = "Data/01_AOI_Yield.RData")

