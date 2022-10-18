## Copula Analysis
## Author: Shengli Liu @ ZZU
## Email:  liushlcn@gmail.com
## Date:   2022/06/30

rm(list = ls())
source("./Script/00_Helper.R")
# source("./Script/00_Functions.R")

load("Data/01_ClimInf.RData")
load("Data/01_YieldInf.RData")
load("Data/00_AOI.RData")
head(dta_clim)
head(yield)
head(aoi)

dta <- merge(yield, dta_clim, by = c("year", "id03"))

# I try the heading degree days for maize but cannot get the result, how about using mean temperature instead
# now check the heat stress days and plot with a glances

dta_clim_year <- dta_clim[,lapply(.SD, mean), .(id03), .SDcols = !c("year", "id03")] 


merge(aoi, dta_clim_year, by = "id03") %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = hdy > 2), color = NA) +
  geom_sf(data = maize_shp, mapping = aes(), fill = NA, color = "gray50", lwd = 0.5) +
  geom_sf(data = bdy, mapping = aes(), fill = NA, color = "gray50", lwd = 0.5) +
  scale_x_continuous(limits = c(-762166.4, 2207086.5)) +
  scale_y_continuous(limits = c(2195234.1, 5921292.3)) +
  coord_crs() +
  theme_map()





# u <- dta[id03 == 110116, .(yd_pbest, hdi)]
# u <- as.matrix(u)
# bivariate_copula <- function(u, family = 1:6) {
#   ufit <- apply(u, 2, kde1d)
#   up <- lapply(X = 1:ncol(u), FUN = function(x) pkde1d(u[,x], ufit[[x]])) %>% 
#     do.call(cbind, .)
#   cop <- BiCopSelect(u1 = up[,1], u2 = up[,2], familyset = family, rotations = F)
#   c(cop$familyname, cop$p.value.indeptest, cop$logLik, cop$AIC, cop$BIC)
#   BiCopCDF(
#     rep(pkde1d(q = 0, obj = ufit[[2]]),5), pkde1d(q = quantile(u[,2]), obj = ufit[[2]]), cop
#   )
#   cor(u)
#   plot()
#   
# }
# names(dta)
# dta[,.(cor = cor(yd_pssa, hdi)), .(zone, id03)]
