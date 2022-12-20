## Helper Functions
## Author: Shengli Liu @ ZZU
## Email:  liushlcn@gmail.com
## Date:   2022/10/17

rm(list = ls())
source("./Script/00_Helper.R")
source("./Script/00_Functions.R")

load("Data/00_AOI.RData")
load("Data/01_AOI_Yield.RData")
load("Data/01_AOI_clim.RData")

head(aoi_yld)

dt <- merge(aoi_yld, aoi_clim, by = c("id03", "year")) %>% 
  .[, `:=`(sdy_ssa = dy_ssa/sd(dy_ssa),
           sdtmax = dtmax/sd(dtmax),
           sdsomi = dsomi/sd(somi)), by = .(id03)] %>% 
  merge(aoi[,c("zone", "id03")] %>% st_set_geometry(value = NULL), by = "id03")



pthres <- critical.r.function(alpha = 0.1, n.pairs = 34, r.null = 0)
dt
dt[,.(coryt = cor(dy_ssa, dtmax),
      corys = cor(dy_ssa, dsomi),
      corts = cor(dtmax, dsomi)
      ), .(id03, zone)] %>% 
  # .[abs(coryt) > max(pthres) | abs(corts) > max(pthres)]
  melt.data.table(id.vars = c("id03", "zone"), measure.vars = c("coryt", "corys"), variable.name = "variable", value.name = "value") %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = zone, y = value, fill = variable), position = position_dodge()) +
  geom_hline(yintercept = pthres)

aoi_clim[id03 == 110111] %>% 
  ggplot(mapping = aes(year, tmax)) +
  geom_point() +
  geom_smooth() +
  geom_line(mapping = aes(y = tmax - dtmax))


dt[id03 == 210401,] %>%
  ggplot() +
  geom_point(mapping = aes(x = sdtmax,  y = sdsomi, color = sdy_ssa < 0)) +
  theme_basic()

dt[id03 == 210401,] %>%
  ggplot(mapping = aes(x = year, y = yield)) +
  geom_point() +
  geom_line() +
  geom_line(mapping = aes(x = year, y = ay_ssa))



dt[id03 == 210401,] %>%
  ggplot(mapping = aes(x = year)) +
  geom_line(mapping = aes(y = tmax )) +
  geom_line(mapping = aes(y = tmax - dtmax), color = "blue")


dt[id03 == 210401,] %>%
  ggplot(mapping = aes(x = year)) +
  geom_line(mapping = aes(y = sdy_ssa)) +
  geom_line(mapping = aes(y = sdtmax, color = "Tmax")) +
  geom_line(mapping = aes(y = sdsomi, color = "SOMI")) +
  geom_hline(yintercept = 0, linetype = 2)


dt[id03 == 210401,] %>%
  ggplot(mapping = aes(x = sdtmax, y = sdy_ssa)) +
  geom_point() +
  scale_x_continuous(limits = c(-3, 3)) +
  scale_y_continuous(limits = c(-3, 3)) +
  geom_smooth(method = "lm", se = F)

dt[id03 == 210401,] %>%
  ggplot(mapping = aes(x = sdsomi, y = sdy_ssa)) +
  geom_point() +
  scale_x_continuous(limits = c(-3, 3)) +
  scale_y_continuous(limits = c(-3, 3)) +
  geom_smooth(method = "lm", se = F)

dt[id03 == 210401,] %>%
  ggplot(mapping = aes(x = sdtmax, y = sdsomi)) +
  geom_point() +
  scale_x_continuous(limits = c(-3, 3)) +
  scale_y_continuous(limits = c(-3, 3)) +
  geom_smooth(method = "lm", se = F)



p1 = ggplot(data = dt, mapping = aes(sdtmax, sdy_ssa)) +
  # geom_point() +
  geom_density_2d_filled(show.legend = F) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.3) +
  scale_fill_grey(start = 1, end = 0.1) +
  geom_smooth(method = "lm", se = F, mapping = aes(color = "All included"), linewidth = 0.3) +
  geom_smooth(data = dt[sdtmax > 1], mapping = aes(sdtmax, sdy_ssa, color = "Extreme heat"), 
              method = "lm", se = FALSE, linewidth = 0.3) +
  scale_x_continuous(limits = c(-5, 5.1), breaks = seq(-4, 4, 2), 
                     labels = c(expression("-4"*italic("σ")), expression("-2"*italic("σ")), 0, 
                                expression("2"*italic("σ")), expression("4"*italic("σ")))) +
  scale_y_continuous(limits = c(-5, 5.1), breaks = seq(-4, 4, 2), 
                     labels = c(expression("-4"*italic("σ")), expression("-2"*italic("σ")), 0, 
                                expression("2"*italic("σ")), expression("4"*italic("σ")))) +
  scale_color_manual(breaks = c("All included", "Extreme heat"), 
                     values = c("All included" = "black", "Extreme heat" = "red")) +
  theme_basic() +
  theme(legend.position = c(1,1), legend.justification = c(1,1)) +
  labs(x = "Temperature deviation", y = "Yield deviation", color = NULL)

p1

p2 = ggplot(data = dt, mapping = aes(sdsomi, sdy_ssa)) +
  # geom_point() +
  geom_density_2d_filled(show.legend = F) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.3) +
  scale_fill_grey(start = 1, end = 0.1) +
  geom_smooth(method = "lm", se = F, mapping = aes(color = "All included"), linewidth = 0.3) +
  geom_smooth(data = dt[sdsomi < -1], mapping = aes(sdsomi, sdy_ssa, color = "Extreme drought"), 
              method = "lm", se = FALSE, linewidth = 0.3) +
  scale_x_continuous(limits = c(-5, 5.1), breaks = seq(-4, 4, 2), 
                     labels = c(expression("-4"*italic("σ")), expression("-2"*italic("σ")), 0, 
                                expression("2"*italic("σ")), expression("4"*italic("σ")))) +
  scale_y_continuous(limits = c(-5, 5.1), breaks = seq(-4, 4, 2), 
                     labels = c(expression("-4"*italic("σ")), expression("-2"*italic("σ")), 0, 
                                expression("2"*italic("σ")), expression("4"*italic("σ")))) +
  scale_color_manual(breaks = c("All included", "Extreme drought"), 
                     values = c("All included" = "black", "Extreme drought" = "red")) +
  theme_basic() +
  theme(legend.position = c(1,1), legend.justification = c(1,1)) +
  labs(x = "Soil moisture deviation", y = "Yield deviation", color = NULL)

p3 = ggplot(data = dt, mapping = aes(x = sdtmax, y = sdsomi)) +
  # geom_point() +
  geom_density_2d_filled(show.legend = F) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.3) +
  scale_fill_grey(start = 1, end = 0.1) +
  geom_smooth(method = "lm", se = F, linewidth = 0.3) +
  geom_abline(slope = -1, intercept = 0, linetype = 2, linewidth = 0.3) +
  scale_x_continuous(limits = c(-5, 5.1), breaks = seq(-4, 4, 2), 
                     labels = c(expression("-4"*italic("σ")), expression("-2"*italic("σ")), 0, 
                                expression("2"*italic("σ")), expression("4"*italic("σ")))) +
  scale_y_continuous(limits = c(-5, 5.1), breaks = seq(-4, 4, 2), 
                     labels = c(expression("-4"*italic("σ")), expression("-2"*italic("σ")), 0, 
                                expression("2"*italic("σ")), expression("4"*italic("σ")))) +
  theme_basic() +
  theme(legend.position = c(1,1), legend.justification = c(1,1)) +
  labs(x = "Temperature deviation", y = "Soil moisture deviation")

pf = plot_grid(p3, p1, p2, nrow = 1, labels = sprintf("(%s)", letters[1:3]), label_size = 13)

make_fig(figname = "Fig1", height = 4, width = 12, pdf = F)
print(pf)
dev.off()

pop <- function(dat, th) {
  value <- c(
    # 计算高温干旱情景下减产概率 以产量低于0计算
    nrow(dat[sdtmax > th & sdsomi < -th & sdy_ssa < 0]) / nrow(dat[sdtmax > th & sdsomi < -th]),
    nrow(dat[sdtmax > th & sdy_ssa < 0]) / nrow(dat[sdtmax > th]),
    nrow(dat[sdsomi < -th & sdy_ssa < 0]) / nrow(dat[sdsomi < -th])
  )
  names(value) = c("Compound event", "Heat event", "Drought event")
  return(value)
}


prob_calc <- function(dat, th) {
  
  yloss <- dat[,.(zone, id03, sdy_ssa, sdtmax, sdsomi)] %>%
    split(x = ., .$zone) %>% 
    lapply(., pop, th = th) %>% 
    bind_rows(.id = "region") %>% 
    bind_rows(pop(dat[,.(zone, id03, sdy_ssa, sdtmax, sdsomi)], th = th)) %>% 
    data.table()
  
  yloss <- yloss[is.na(region), region := "Whole"] %>% 
    melt.data.table(id.vars = "region", variable.name = "type", value.name = "prob") %>% 
    .[, `:=`(region = factor(region, levels = c("Whole", "NCSM", "HHHSM", "SWCM")),
             type = factor(type, levels = c("Heat event", "Drought event", "Compound event")))]
  
  p <- ggplot(data = yloss) + 
    geom_bar(mapping = aes(x = region, y = prob, fill = type), stat = "identity",
             position = position_dodge()) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), labels = seq(0, 100, 20)) +
    scale_fill_manual(values = pals::tableau20(3)) +
    theme_basic() +
    theme(legend.position = c(1,1), legend.justification = c(1,1)) +
    labs(x = "Zones", y = "Yield loss probability (%)", fill = NULL)
  return(list(yloss, p))
}

res <- prob_calc(dat = dt, th = 1)
res[[1]]
res[[2]] +
  geom_hline(yintercept = 0.5)


