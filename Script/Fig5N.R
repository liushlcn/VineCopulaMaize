make_fig(figname = "R2Fig52", height = 7, width = 8, bg = "white")
brks = seq(0, 100, 10)
cols = pals::brewer.orrd(length(brks) - 1)
plot_ellipse <- function(xc = 115, yc = 39, a = 10, b = 4, phi = pi/5) {
  t <- seq(0, 2*pi, 0.01) 
  x <- xc + a*cos(t)*cos(phi) - b*sin(t)*sin(phi)
  y <- yc + a*cos(t)*cos(phi) + b*sin(t)*cos(phi)
  lines(x,y, col = "black", lty = 2)
}
layout(matrix(c(1,1,3,3,1,1,3,3,2,2,4,5,2,2,4,5), 4, 4), widths = c(1,1,1.6,0.4), heights = c(1,1,1.2, 0.8))

par(mar = c(0.5, 0.5, 1, 0.5))
# layout(mat = matrix(c(1:4), 2, 2, byrow = T))
for (tp in levels) {
  dta <- result %>%
    filter(str_sub(type, -3,-1) == "Dry") %>%
    mutate(type = str_remove(type, "ly")) %>%
    mutate(type = paste0("Yield loss|", type)) %>%
    mutate(type = factor(type, levels = levels)) %>%
    mutate(prob = prob*100) %>% 
    inner_join(maize_county, by = "id03") %>% 
    filter(type == tp)
  xx <- cut(dta$prob, seq(0, 100, 10)) %>% as.numeric()
  par(mar = c(0.1,0.1,2.1,0.1))
  plot(NA, axes = F, ann = F, xlim = c(97, 136), ylim = c(18, 55), xaxs = "i", yaxs = "i")
  abline(h = seq(20, 50, 10), v = seq(100, 130, 10), lty = 2, col = "lightgrey")
  plot(bdy$geometry, add = T, border = 0.1)
  plot(dta$geometry, col = cols[xx], add = T, border = NA)
  plot(maize_shp$geometry, add = T, lwd = 1)
  plot_ellipse()
  text(99, 53, sprintf("(%s)", letters[which(tp == levels)]))
  box()
  mtext(3, text = tp)
}

par(mar = c(2,0.5,2,0.5))
pbar <- result %>%
  filter(str_sub(type, -3,-1) == "Dry") %>%
  mutate(type = str_remove(type, "ly")) %>%
  mutate(type = paste0("Yield loss|", type)) %>%
  mutate(type = factor(type, levels = levels)) %>%
  mutate(prob = cut(prob*100, seq(0, 100, 10), seq(10, 100, 10))) %>% 
  inner_join(maize_county, by = "id03") %>% 
  select(-geometry) %>% 
  mutate(zone = factor(zone, labels = c("HS", "NCS", "SCM"))) %>% 
  mutate(zone = as.character(zone))

pbar <- pbar %>% mutate(zone = "Whole") %>% 
  bind_rows(pbar,.) %>% 
  group_by(zone, prob, type) %>% 
  summarise(pb = n()) %>% 
  group_by(zone, type) %>% 
  mutate(pb = pb/sum(pb)*100) %>% 
  as.data.frame()
zone = sort(unique(pbar$zone))
###########
# par(mar = c(2,1,2,1))
# barplot(pb~prob+type, data = pbar[pbar$zone == zone[1],], space = 0.1, col = pals::brewer.orrd(10), horiz = T, xlim = c(-5, 101), ylim = c(0, 18),
#         border = NA, ann = F,axisnames = F, axes = F )
# barplot(pb~prob+type, data = pbar[pbar$zone == zone[2],], space = c(4, 0.1,0.1,0.1), col = pals::brewer.orrd(10), horiz = T,
#         border = NA, add = T, ann = F,axisnames = F, axes = F )
# barplot(pb~prob+type, data = pbar[pbar$zone == zone[3],], space = c(8, 0.1,0.1,0.1), col = pals::brewer.orrd(10), horiz = T,
#         border = NA, add = T, ann = F,axisnames = F, axes = F )
# barplot(pb~prob+type, data = pbar[pbar$zone == zone[4],], space = c(12, 0.1,0.1,0.1), col = pals::brewer.orrd(10), horiz = T,
#         border = NA, add = T, ann = F,axisnames = F, axes = F )
# xr <- seq(20, 80, length.out = 11)
# rect(xr[-length(xr)], ybottom = 16.5, xr[-1], 17, col = pals::brewer.orrd(10), border = "transparent")
# rect(20, ybottom = 16.5, 80, 17, col = NA, border = "black")
# text(median(xr), 17.5, "Conditional probability (%)")
# segments(xr, 16.2, xr, 16.5)
# text(xr, 15.6, seq(0, 100, 10), cex = 0.6)
# text(2, 17.5, "(d)")
# text(50, c(1:3+0.1, 5:7+0.1, 9:11+0.1, 13:15+0.1) - 0.4, levels, cex = 0.8, adj = 0.5)
# text(-3, c(2, 6, 10, 14), zone, cex = 0.8, srt = 90)
# axis(1, seq(0, 100, 20), line = 0, cex.axis = 0.8, padj = -1)
# mtext(side = 1, text = "Percentage of county (%)", line = 1, cex = 0.8)
##############
pbar <- pbar %>% 
  group_by(zone, type) %>% 
  mutate(pbs = cumsum(pb))
xr <- seq(20, 80, length.out = 11)
plot(NA, xlim = c(-2, 130), ylim = c(0, 17), axes = F, ann = F)
t = 0
z = 0
for (i in zone) {
  for (j in levels) {
    y <- pbar %>% filter(zone == i, type == j) %>% pull(pbs) %>% sort() %>% c(0,.)
    rect(y[-length(y)], t + z + 0, y[-1], t + z + 0.8, col = cols, border = "transparent")
    rect(0, t + z + 0, 100, t + z + 0.8, border = "black")
    text(115, t + z + 0.4, j, cex = 0.6)
    t = t + 1
  }
  z = z + 0.5
}
rect(xr[-length(xr)], ybottom = 15, xr[-1], 15.5, col = pals::brewer.orrd(10), border = "transparent")
rect(20, ybottom = 15, 80, 15.5, col = NA, border = "black")
text(median(xr), 16.5, "Conditional probability (%)")
segments(xr, 14.7, xr, 15)
text(xr, 14.3, seq(0, 100, 10), cex = 0.6)
text(0, 17, "(d)")
text(-3, c(1.5, 5, 8.5, 12), zone, cex = 0.8, srt = 90)
axis(1, seq(0, 100, 20), line = 0, cex.axis = 0.8, padj = -1)
# mtext(side = 1, text = "Percentage of county (%)", line = 1.2, cex = 0.6)


## 2020/8/19添加 所有图片合在一起

pbar <- pbar %>% 
  mutate(prob = as.numeric(as.character(prob))) %>% 
  mutate(ntype = cut(prob, breaks = c(0,30,60,100), labels = c("Low risk", "Medium risk", "High risk"))) %>% 
  group_by(zone, type, ntype) %>% 
  summarise(pb = sum(pb)) %>% 
  arrange(zone, ntype)
y <- c(c(1:3, 5:7, 9:11), c(1:3, 5:7, 9:11) + 13, c(1:3, 5:7, 9:11) + 26, c(1:3, 5:7, 9:11) + 39)
par(mar = c(2,2, 0.5, 0.5), cex = 0.8, cex.axis = 0.8, tcl = -0.3)
plot(NA, xlim = c(1, 50), ylim = c(0, 94), ann = F, axes = F, yaxs = "i")
rect(y - 0.5, 0, y + 0.5, pbar$pb, col = c("#2C7BB6", "#ABD9E8", "#FDAE61"))
axis(1, c(2,6,10, 15, 19, 23, 28, 32, 36, 41, 45, 49), rep(c("Low", "Mild", "High"), 4), padj = -2, cex.axis = 0.7)
abline(v = c(12.5, 25.5, 38.5), lty = 2, col = "grey")
text(c(6, 19, 32, 45), 88, c("HS", "NCS", "SCM", "Whole"), cex = 0.7)

axis(2, las = 1, hadj = 0, cex.axis = 0.8)
mtext("Percentage of counties (%)", 3, line = 0, cex = 0.8)
mtext("Risk of yield failure", 1, line = 1, cex = 0.8)
legend(2, 85, pch = 15, col = c("#2C7BB6", "#ABD9E8", "#FDAE61"), legend = levels,
       box.col = NA, ncol = 3, bg = NA, cex = 0.6)
box()
mtext("(e)", side = 3, line = 0, adj = -0.05, cex = 0.8)
# par(mar = c(0,0,0,0))
# plot(NA, xlim = c(0,1), ylim = c(0,1), axes = F, ann = F)

dev.off()
make_convert(figname = "R2Fig52")
