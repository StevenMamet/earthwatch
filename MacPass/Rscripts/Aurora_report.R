library(dplyr)
library(lubridate)
library(pscl)
library(lmtest)
library(scales)
library(tidyverse)
library(wesanderson)

rm(list = ls())

visual_output <- "~/Desktop/Workspace/earthwatch/Macpass/figures"

#_______________________----
# Thaw depths ----

## Read in the data ----
thaw.mm <- read.csv(file = "~/Desktop/Workspace/earthwatch/Macpass/data/thaw.mm.csv", header = TRUE)

## Add 95% CIs ----
thaw.mm$ci <- thaw.mm$se*qt(0.975, thaw.mm$n-1)
thaw.mm$c5 <- thaw.mm$mean - thaw.mm$ci
thaw.mm$c95 <- thaw.mm$mean + thaw.mm$ci

## Subset sites ----
d2 <- subset(thaw.mm, site == "D2")
d6 <- subset(thaw.mm, site == "D6")
beaver <- subset(thaw.mm, site == "Beaver")
hare <- subset(thaw.mm, site == "Hare")
goose <- subset(thaw.mm, site == "Goose")
snow <- subset(thaw.mm, site == "Snow")
snow <- rbind.data.frame(snow[c(1:8),],
                         data.frame(year = seq(2002,2005,1),
                                    site = rep("Snow",4),
                                    min = rep(NA,4),
                                    mean = rep(NA,4),
                                    max = rep(NA,4),
                                    sd = rep(NA,4),
                                    se = rep(NA,4),
                                    n = rep(NA,4),
                                    ci = rep(NA,4),
                                    c5 = rep(NA,4),
                                    c95 = rep(NA,4)),
                         snow[c(9:25),])
porsild <- subset(thaw.mm, site == "Porsild")
porsild.1 <- subset(thaw.mm, site == "Porsild.1")
porsild.2 <- subset(thaw.mm, site == "Porsild.2")
pipeline <- subset(thaw.mm, site == "Pipeline")
pp.control.p <- subset(thaw.mm, site == "PP.Control.P")
pp.pipeline.p <- subset(thaw.mm, site == "PP.Pipeline.P")
pp.track.t <- subset(thaw.mm, site == "PP.Track.T")

## Linear models by site ----
snow.lm <- lm(snow$mean~snow$year)
pipeline.lm <- lm(pipeline$mean~pipeline$year)
goose.lm <- lm(goose$mean~goose$year)
d2.lm <- lm(d2$mean~d2$year)
d6.lm <- lm(d6$mean~d6$year)
porsild.lm <- lm(porsild$mean~porsild$year)
porsild.1.lm <- lm(porsild.1$mean~porsild.1$year)
porsild.2.lm <- lm(porsild.2$mean~porsild.2$year)
beaver.lm <- lm(beaver$mean~beaver$year)
hare.lm <- lm(hare$mean~hare$year)
pp.control.p.lm <- lm(pp.control.p$mean~pp.control.p$year)
pp.pipeline.p.lm <- lm(pp.pipeline.p$mean~pp.pipeline.p$year)
pp.track.t.lm <- lm(pp.track.t$mean~pp.track.t$year)
summary(snow.lm) # Sig
summary(pipeline.lm) # Sig
summary(goose.lm) # Sig
summary(d2.lm) # Sig
summary(d6.lm) # --
summary(porsild.lm) # Sig
summary(porsild.1.lm) # --
summary(porsild.2.lm) # Sig
summary(beaver.lm) # Sig
summary(hare.lm) # Sig
summary(pp.control.p.lm) # Sig
summary(pp.pipeline.p.lm) # Sig
summary(pp.track.t.lm) # Sig
snow.slope <- round(coef(snow.lm)[[2]], 3)
pipeline.slope <- round(coef(pipeline.lm)[[2]], 3)
goose.slope <- round(coef(goose.lm)[[2]], 3)
d2.slope <- round(coef(d2.lm)[[2]], 3)
d6.slope <- round(coef(d6.lm)[[2]], 3)
porsild.slope <- round(coef(porsild.lm)[[2]], 3)
porsild.1.slope <- round(coef(porsild.1.lm)[[2]], 3)
porsild.2.slope <- round(coef(porsild.2.lm)[[2]], 3)
beaver.slope <- round(coef(beaver.lm)[[2]], 3)
hare.slope <- round(coef(hare.lm)[[2]], 3)
pp.control.slope <- round(coef(pp.control.p.lm)[[2]], 3)
pp.pipeline.slope <- round(coef(pp.pipeline.p.lm)[[2]], 3)
pp.track.slope <- round(coef(pp.track.t.lm)[[2]], 3)

## Site-wide thaw depth figure ----
setwd(visual_output)

jpeg("LTEMS_thaw_2022.jpeg", width = 7, height = 7, units = "in", res = 300)
par(mar = c(0.3, 2.3, 1, 2), oma = c(3.2,1,0,1.6), mfrow = c(4,2))

plot(hare$year, hare$mean, type='n', xlim = c(1990,2022), ylim = rev(c(0,120)), axes = F, xlab = "", ylab = "")
polygon(c(rev(hare$year), hare$year), c(rev(hare$c95), hare$c5), col = alpha("darkred",0.6), border = NA)
lines(hare$year, hare$mean, col = 'darkred', lwd = 2)
points(hare$year[c(32,33)], hare$mean[c(32,33)], col = 'darkred', pch = 16)
hare.Y <- predict(hare.lm, newdata=data.frame(year=hare$year))
lines(hare$year, hare.Y, lty = 2, lwd = 2, col = "darkred")
axis(2, at = seq(0,120,20), labels = seq(0,120,20))
par(new = T)
plot(hare$year, hare$n, col = 'darkred', type = "l", xlim = c(1990,2022), ylim = c(15,100), xlab = "", ylab = "", axes = F)
points(hare$year[c(32,33)], hare$n[c(32,33)], col = 'darkred', pch = 17)
legend("topleft", "HF (1260 m.a.s.l.)", bty = "n")#, inset = c(-0_08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(hare.slope) ~ "cm yr"^-1), bty = "n")
box()
axis(1, labels = F)
axis(side = 4, at = seq(0,40,10))

plot(beaver$year, beaver$mean, type='n', xlim = c(1990,2022), ylim = rev(c(0,120)), axes = F, xlab = "", ylab = "")
polygon(c(rev(beaver$year), beaver$year), c(rev(beaver$c95), beaver$c5), col = alpha("red",0.6), border = NA)
lines(beaver$year, beaver$mean, col = 'red', lwd = 2)
points(beaver$year[c(32,33)], beaver$mean[c(32,33)], col = 'red', pch = 16)
beaver.Y <- predict(beaver.lm, newdata=data.frame(year=beaver$year))
lines(beaver$year, beaver.Y, lty = 2, lwd = 2, col = "red")
axis(2, at = seq(0,120,20), labels = seq(0,120,20))
par(new = T)
plot(beaver$year, beaver$n, col = 'red', type = "l", xlim = c(1990,2022), ylim = c(15,100), xlab = "", ylab = "", axes = F)
points(beaver$year[c(32,33)], beaver$n[c(32,33)], col = 'red', pch = 17)
box()
axis(1, labels = F)
axis(side = 4, at = seq(0,80,20))
legend("topleft", "BP (1272 m.a.s.l.)", bty = "n")#, inset = c(-0_08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(beaver.slope) ~ "cm yr"^-1), bty = "n")

plot(porsild.1$year, porsild.1$mean, type='n', xlim = c(1990,2022), ylim = rev(c(0,120)), axes = F, xlab = "", ylab = "")
polygon(c(rev(porsild.1$year), porsild.1$year), c(rev(porsild.1$c95), porsild.1$c5), col = alpha("violet",0.6), border = NA)
lines(porsild.1$year, porsild.1$mean, col = 'violet', lwd = 2)
points(porsild.1$year[c(16,17)], porsild.1$mean[c(16,17)], col = 'violet', pch = 16)
axis(2, at = seq(0,120,20), labels = seq(0,120,20))
par(new = T)
plot(porsild.1$year, porsild.1$n, col = 'violet', type = "l", xlim = c(1990,2022), ylim = c(15,100), axes = F,xlab = "", ylab = "")
points(porsild.1$year[c(16,17)], porsild.1$n[c(16,17)], col = 'violet', pch = 17)
legend("topleft", "PF1 (1380 m.a.s.l.)", bty = "n")#, inset = c(-0_08,-0.15))
box()
axis(1, labels = F)
axis(side = 4, at = seq(0,80,20), labels = T)

plot(porsild.2$year, porsild.2$mean, type='n', xlim = c(1990,2022), ylim = rev(c(0,120)), axes = F, xlab = "", ylab = "")
polygon(c(rev(porsild.2$year), porsild.2$year), c(rev(porsild.2$c95), porsild.2$c5), col = alpha("violet",0.6), border = NA)
lines(porsild.2$year, porsild.2$mean, col = 'violet', lwd = 2)
points(porsild.2$year[c(16,17)], porsild.2$mean[c(16,17)], col = 'violet', pch = 16)
porsild.2.Y <- predict(porsild.2.lm, newdata=data.frame(year=porsild.2$year))
lines(porsild.2$year, porsild.2.Y, lty = 2, lwd = 2, col = "violet")
axis(2, at = seq(0,120,20), labels = seq(0,120,20))
par(new = T)
plot(porsild.2$year, porsild.2$n, col = 'violet', type = "l", xlim = c(1990,2022), ylim = c(5,100), axes = F, xlab = "", ylab = "")
points(porsild.2$year[c(16,17)], porsild.2$n[c(16,17)], col = 'violet', pch = 17)
legend("topleft", "PF2 (1380 m.a.s.l.)", bty = "n")#, inset = c(-0_08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(porsild.2.slope) ~ "cm yr"^-1), bty = "n")
box()
axis(1, labels = F)
axis(side = 4, at = seq(0,80,20))

plot(d6$year, d6$mean, type='n', xlim = c(1990,2022), ylim = rev(c(0,150)), axes = F, xlab = "", ylab = "")
polygon(c(rev(d6$year), d6$year), c(rev(d6$c95), d6$c5), col = alpha("orange",0.6), border = NA)
lines(d6$year, d6$mean, col = 'orange', lwd = 2)
points(d6$year[c(32,33)], d6$mean[c(32,33)], col = 'orange', pch = 16)
axis(2, at = seq(0,140,20), labels = seq(0,140,20))
par(new = T)
plot(d6$year, d6$n, col = 'orange', type = "l", xlim = c(1990,2022), ylim = c(15,120), xlab = "", ylab = "", axes = F)
points(d6$year[c(32,33)], d6$n[c(32,33)], col = 'orange', pch = 17)
legend("topleft", "D6 (1473 m.a.s.l.)", bty = "n")#, inset = c(-0_08,-0.15))
box()
axis(1, labels = F)
axis(side = 4, at = seq(0,80,20))

plot(d2$year, d2$mean, type='n', xlim = c(1990,2022), ylim = rev(c(0,150)), axes = F, xlab = "", ylab = "")
polygon(c(rev(d2$year), d2$year), c(rev(d2$c95), d2$c5), col = alpha("yellow3",0.6), border = NA)
lines(d2$year, d2$mean, col = 'yellow3', lwd = 2)
points(d2$year[c(32,33)], d2$mean[c(32,33)], col = 'yellow3', pch = 16)
d2.Y <- predict(d2.lm, newdata=data.frame(year=d2$year))
lines(d2$year, d2.Y, lty = 2, lwd = 2, col = "yellow3")
axis(2, at = seq(0,140,20), labels = seq(0,140,20))
par(new = T)
plot(d2$year, d2$n, col = 'yellow3', type = "l", xlim = c(1990,2022), ylim = c(35,120), xlab = "", ylab = "", axes = F)
points(d2$year[c(32,33)], d2$n[c(32,33)], col = 'yellow3', pch = 17)
legend("topleft", "D2 (1477 m.a.s.l.)", bty = "n")#, inset = c(-0_08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(d2.slope) ~ "cm yr"^-1), bty = "n")
box()
axis(1, labels = F)
axis(side = 4, at = seq(0,80,20))

plot(goose$year, goose$mean, type='n', xlim = c(1990,2022), ylim = rev(c(0,180)), axes = F, xlab = "", ylab = "")
polygon(c(rev(goose$year), goose$year), c(rev(goose$c95), goose$c5), col = alpha("blue",0.6), border = NA)
lines(goose$year, goose$mean, col = 'blue', lwd = 2)
points(goose$year[c(32,33)], goose$mean[c(32,33)], col = 'blue', pch = 16)
goose.Y <- predict(goose.lm, newdata=data.frame(year=goose$year))
lines(goose$year, goose.Y, lty = 2, lwd = 2, col = "blue")
axis(2, at = seq(0,180,30), labels = seq(0,180,30))
par(new = T)
plot(goose$year, goose$n, col = 'blue', type = "l", xlim = c(1990,2022), ylim = c(5,160), xlab = "", ylab = "", axes = F)
points(goose$year[c(32,33)], goose$n[c(32,33)], col = 'blue', pch = 17)
legend("topleft", "GF (1621 m.a.s.l.)", bty = "n")#, inset = c(-0_08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(goose.slope) ~ "cm yr"^-1), bty = "n", inset = c(0,0.2))
box()
axis(1, labels = T)
axis(side = 4, at = seq(0,80,20))

plot(snow$year, snow$mean, type='n', xlim = c(1990,2022), ylim = rev(c(0,180)), axes = F, xlab = "", ylab = "")
polygon(c(rev(snow$year[c(1:8)]), snow$year[c(1:8)]), c(rev(snow$c95[c(1:8)]), snow$c5[c(1:8)]), col = alpha("royalblue1",0.6), border = NA)
polygon(c(rev(snow$year[c(13:28)]), snow$year[c(13:28)]), c(rev(snow$c95[c(13:28)]), snow$c5[c(13:28)]), col = alpha("royalblue1",0.6), border = NA)
lines(snow$year, snow$mean, col = 'royalblue1', lwd = 2)
points(snow$year[c(28,29)], snow$mean[c(28,29)], col = 'royalblue1', pch = 16)
snow.Y <- predict(snow.lm, newdata=data.frame(year=snow$year))
lines(snow$year, snow.Y, lty = 2, lwd = 2, col = "royalblue1")
axis(2, at = seq(0,180,30), labels = seq(0,180,30))
par(new = T)
plot(snow$year, snow$n, col = 'royalblue1', type = "l", xlim = c(1990,2022), ylim = c(5,220), xlab = "", ylab = "", axes = F)
points(snow$year[c(28,29)], snow$n[c(28,29)], col = 'royalblue1', pch = 17)
legend("topleft", "SF (1660 m.a.s.l.)", bty = "n")#, inset = c(-0_08,-0.15))
legend("bottomright", legend=bquote("Slope = " ~ .(snow.slope) ~ "cm yr"^-1), bty = "n")
box()
axis(1, labels = T)
axis(side = 4, at = seq(0,80,20))

mtext("Thaw depth (cm)", side=2, cex = 0.7, line = 0, outer = T)
mtext("Probe points (n)", side=4, cex = 0.7, line = 0, outer = T)
mtext("Year", side=1,  cex = 0.7, line = 1.8, outer = T)
dev.off()

##

# Export as 5 x 7
# jpeg("Pipeline.jpeg", width = 5, height = 7, units = "in", res = 300)
# par(mfrow = c(3, 1))
# par(cex = 0.75)
# par(mar = c(1.3, 2.3, 1, 2.3), oma = c(2, 1, 0, 1)+0.2)

## Pipeline thaw figure ----
jpeg("Pipeline_2022.jpeg", width = 4, height = 6, units = "in", res = 300)
par(mar = c(0.3, 2.3, 1, 2), oma = c(3.2,1,0,1.6), mfrow = c(3,1))

plot(pp.control.p$year, pp.control.p$mean, type='n', xlim = c(1990,2022), ylim = rev(c(40,120)), axes = F, xlab = "", ylab = "")
polygon(c(rev(pp.control.p$year), pp.control.p$year), c(rev(pp.control.p$c95), pp.control.p$c5), col = alpha("firebrick1",0.6), border = NA)
lines(pp.control.p$year, pp.control.p$mean, col = 'firebrick4', lwd = 2)
points(pp.control.p$year[c(12,13)], pp.control.p$mean[c(12,13)], col = 'firebrick4', pch = 16)
pp.control.p.Y <- predict(pp.control.p.lm, newdata=data.frame(year=pp.control.p$year))
lines(pp.control.p$year, pp.control.p.Y, lty = 2, lwd = 2, col = "firebrick4")
axis(2, at = seq(20,200,20), labels = seq(20,200,20))
par(new = T)
plot(pp.control.p$year, pp.control.p$n, col = 'firebrick4', type = "l", xlim = c(1990,2022), ylim = c(40,350), xlab = "", ylab = "", axes = F)
points(pp.control.p$year[c(12,13)], pp.control.p$n[c(12,13)], col = 'firebrick4', pch = 17)
box()
axis(1, labels = F)
axis(side = 4, at = seq(0,100,20))
legend("topleft", "PP: pipeline-control (1623 m.a.s.l.)", bty = "n")#, inset = c(-0_08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(pp.control.slope) ~ "cm yr"^-1), bty = "n")

plot(pp.pipeline.p$year, pp.pipeline.p$mean, type='n', xlim = c(1990,2022), ylim = rev(c(40,120)), axes = F, xlab = "", ylab = "")
polygon(c(rev(pp.pipeline.p$year), pp.pipeline.p$year), c(rev(pp.pipeline.p$c95), pp.pipeline.p$c5), col = alpha("firebrick1",0.6), border = NA)
lines(pp.pipeline.p$year, pp.pipeline.p$mean, col = 'firebrick4', lwd = 2)
points(pp.pipeline.p$year[c(12,13)], pp.pipeline.p$mean[c(12,13)], col = 'firebrick4', pch = 16)
pp.pipeline.p.Y <- predict(pp.pipeline.p.lm, newdata=data.frame(year=pp.pipeline.p$year))
lines(pp.pipeline.p$year, pp.pipeline.p.Y, lty = 2, lwd = 2, col = "firebrick4")
axis(2, at = seq(20,200,20), labels = seq(20,200,20))
par(new = T)
plot(pp.pipeline.p$year, pp.pipeline.p$n, col = 'firebrick4', type = "l", xlim = c(1990,2022), ylim = c(40,350), xlab = "", ylab = "", axes = F)
points(pp.pipeline.p$year[c(12,13)], pp.pipeline.p$n[c(12,13)], col = 'firebrick4', pch = 17)
box()
axis(1, labels = F)
axis(side = 4, at = seq(0,100,20))
legend("topleft", "PP: pipeline (1623 m.a.s.l.)", bty = "n")#, inset = c(-0_08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(pp.pipeline.slope) ~ "cm yr"^-1), bty = "n")

plot(pp.track.t$year, pp.track.t$mean, type='n', xlim = c(1990,2022), ylim = rev(c(40,120)), axes = F, xlab = "", ylab = "")
polygon(c(rev(pp.track.t$year), pp.track.t$year), c(rev(pp.track.t$c95), pp.track.t$c5), col = alpha("firebrick1",0.6), border = NA)
lines(pp.track.t$year, pp.track.t$mean, col = 'firebrick4', lwd = 2)
points(pp.track.t$year[c(12,13)], pp.track.t$mean[c(12,13)], col = 'firebrick4', pch = 16)
pp.track.t.Y <- predict(pp.track.t.lm, newdata=data.frame(year=pp.track.t$year))
lines(pp.track.t$year, pp.track.t.Y, lty = 2, lwd = 2, col = "firebrick4")
axis(2, at = seq(20,200,20), labels = seq(20,200,20))
par(new = T)
plot(pp.track.t$year, pp.track.t$n, col = 'firebrick4', type = "l", xlim = c(1990,2022), ylim = c(40,350), xlab = "", ylab = "", axes = F)
points(pp.track.t$year[c(12,13)], pp.track.t$n[c(12,13)], col = 'firebrick4', pch = 17)
box()
axis(1, labels = T)
axis(side = 4, at = seq(0,100,20))
legend("topleft", "PP: track (1623 m.a.s.l.)", bty = "n")#, inset = c(-0_08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(pp.track.slope) ~ "cm yr"^-1), bty = "n")

mtext("Thaw depth (cm)", side=2, cex = 0.7, line = 0, outer = T)
mtext("Probe points (n)", side=4, cex = 0.7, line = 0, outer = T)
mtext("Year", side=1,  cex = 0.7, line = 1.8, outer = T)
dev.off()

#_______________________----
# Microclimate ----

## Read in the data ----
mm <- read.csv("~/Desktop/Workspace/Earthwatch/Macpass/data/microclimate_19900721_20220818_filled.csv", header = T)#[-1]

## Convert dates ----
mm$Date <- ymd(mm$Date, tz = "Canada/Mountain")

## Add season-year column to group data ----
mm <- mm %>%
  mutate(
    season_year = ifelse(
      month(Date) == 10 | month(Date) == 11 | month(Date) == 12,
      year(Date) + 1,
      year(Date)
    ),
    season = case_when(
      month(Date) %in% c(10, 11, 12, 1, 2, 3, 4, 5) ~ "cool",
      month(Date) %in% c(6, 7, 8, 9) ~ "warm",
      T ~ NA_character_
    )
  )

## Air T trends ----

### Overall ----
mm_air_agg <- mm %>% 
  reframe(across(ends_with("_150"), mean, .unpack = TRUE), .by = season_year) %>% 
  relocate(any_of(c("season_year","hf_150", "bp_150", "d6_150", "d2_150", "gf_150"))) %>% 
  slice(-1)

### Seasonal ----
mm_agg <- mm %>% 
  reframe(across(any_of(c("hf.150", "hf.0", "hf.neg150",
                          "bp.150", "bp.0", "bp.neg150", 
                          "d6.150", "d6.0", "d6.neg150", 
                          "d2.150", "d2.0", "d2.neg150",  
                          "gf.150", "gf.0", "gf.neg150")),
                 mean, .unpack = TRUE), .by = c(season_year, season)) %>% 
  arrange(season, season_year)
mm_temp_w <- subset(mm_agg, season == "warm")
mm_temp_c <- subset(mm_agg, season == "cool")

## Ground surface
mm_agg_grd_season <- mm %>%
  select(season_year, season, bp.0, hf.0, d6.0, d2.0, gf.0) %>%
  group_by(season_year, season) %>%
  summarise(across(bp.0:gf.0, mean))
mm_agg_grd_warm <- mm_agg_grd_season[mm_agg_grd_season$season == "warm" & mm_agg_grd_season$season_year %in% c(1991:2021),]
mm_agg_grd_warm$grdT <- rowMeans(mm_agg_grd_warm[,c(3:7)]) 
mm_agg_grd_warm[,c(2:7)] <- NULL
mm_agg_grd_cool <- mm_agg_grd_season[mm_agg_grd_season$season == "cool",]
mm_agg_grd_cool$grdT <- rowMeans(mm_agg_grd_cool[,c(3:7)]) 
mm_agg_grd_cool[,c(2:7)] <- NULL
plot(grdT ~ season_year, mm_agg_grd_warm, type = "l")
plot(grdT ~ season_year, mm_agg_grd_cool, type = "l")
summary(lm(grdT ~ season_year, data = mm_agg_grd_warm))
summary(lm(grdT ~ season_year, data = mm_agg_grd_cool))

## Permafrost
mm_agg_perm_season <- mm %>%
  select(season_year, season, bp.150, hf.150, d6.150, d2.150, gf.150) %>%
  group_by(season_year, season) %>%
  summarise(across(bp.150:gf.150, mean))
mm_agg_perm.warm <- mm_agg_perm_season[mm_agg_perm_season$season == "warm" & mm_agg_perm_season$season_year %in% c(1991:2020),]
mm_agg_perm.warm$permT <- rowMeans(mm_agg_perm.warm[,c(3:7)]) 
mm_agg_perm.warm[,c(2:7)] <- NULL
mm_agg_perm.cool <- mm_agg_perm_season[mm_agg_perm_season$season == "cool",]
mm_agg_perm.cool$permT <- rowMeans(mm_agg_perm.cool[,c(3:7)]) 
mm_agg_perm.cool[,c(2:7)] <- NULL
plot(permT ~ season_year, mm_agg_perm.warm, type = "l")
plot(permT ~ season_year, mm_agg_perm.cool, type = "l")
summary(lm(permT ~ season_year, data = mm_agg_perm.warm))
summary(lm(permT ~ season_year, data = mm_agg_perm.cool))

## Air temperature regressions
summary(bp_150_w_lm <- lm(bp.150 ~ season_year, data = mm_temp_w[-c(1,33),]))  # b1 = 0_046, P = 0_00705 **
summary(hf_150_w_lm <- lm(hf.150 ~ season_year, data = mm_temp_w[-c(1,33),]))  # b1 = 0_033, P = 0_0437 *
summary(d6_150_w_lm <- lm(d6.150 ~ season_year, data = mm_temp_w[c(8:32),]))   # b1 = 0_011, P = 0.5497
summary(d2_150_w_lm <- lm(d2.150 ~ season_year, data = mm_temp_w[-c(1,33),]))  # b1 = 0_026, P = 0.127
summary(gf_150_w_lm <- lm(gf.150 ~ season_year, data = mm_temp_w[-c(1,33),]))  # b1 = 0_043, P = 0_0277 *

summary(bp_150_c_lm <- lm(bp.150 ~ season_year, data = mm_temp_c[-32,]))       # b1 = 0_027, P = 0.462
summary(hf_150_c_lm <- lm(hf.150 ~ season_year, data = mm_temp_c[-32,]))       # b1 = 0_026, P = 0.5368
summary(d6_150_c_lm <- lm(d6.150 ~ season_year, data = mm_temp_c[c(6:31),]))   # b1 = 0_051, P = 0.281
summary(d2_150_c_lm <- lm(d2.150 ~ season_year, data = mm_temp_c[-32,]))       # b1 = 0_025, P = 0.440
summary(gf_150_c_lm <- lm(gf.150 ~ season_year, data = mm_temp_c[-32,]))       # b1 = 0_047, P = 0.1067  

## Ground surface regressions
summary(bp_0_w_lm <- lm(bp.0 ~ season_year, data = mm_temp_w[-c(1,33),]))  # b1 = 0_027, P = 0_0818 .
summary(hf_0_w_lm <- lm(hf.0 ~ season_year, data = mm_temp_w[-c(1,33),]))  # b1 = -0_014, P = 0.345
summary(d6_0_w_lm <- lm(d6.0 ~ season_year, data = mm_temp_w[c(8:32),]))   # b1 = 0_018, P = 0.563
summary(d2_0_w_lm <- lm(d2.0 ~ season_year, data = mm_temp_w[-c(1,33),]))  # b1 = 0_025, P = 0.112
summary(gf_0_w_lm <- lm(gf.0 ~ season_year, data = mm_temp_w[-c(1,33),]))  # b1 = 0_075, P = 0_000933 ***

summary(bp_0_c_lm <- lm(bp.0 ~ season_year, data = mm_temp_c[-32,]))       # b1 = 0_060, P = 0_0441 *
summary(hf_0_c_lm <- lm(hf.0 ~ season_year, data = mm_temp_c[-32,]))       # b1 = 0_081, P = 0_0241 *
summary(d6_0_c_lm <- lm(d6.0 ~ season_year, data = mm_temp_c[c(6:31),]))   # b1 = -0_031, P = 0.135
summary(d2_0_c_lm <- lm(d2.0 ~ season_year, data = mm_temp_c[-32,]))       # b1 = 0_030, P = 0.473
summary(gf_0_c_lm <- lm(gf.0 ~ season_year, data = mm_temp_c[-32,]))       # b1 = 0.166, P = 2.85e-05 ***

## Permafrost regressions
summary(bp_neg150_w_lm <- lm(bp.neg150 ~ season_year, data = mm_temp_w[-c(1,33),])) # b1 = 0_021, P = 0_0001665***
summary(hf_neg150_w_lm <- lm(hf.neg150 ~ season_year, data = mm_temp_w[-c(1,33),])) # b1 = 0_017, P = 4_90e-07 ***
summary(d6_neg150_w_lm <- lm(d6.neg150 ~ season_year, data = mm_temp_w[c(8:32),])) # b1 = -0_003, P = 0_012*
summary(d2_neg150_w_lm <- lm(d2.neg150 ~ season_year, data = mm_temp_w[-c(1,33),])) # b1 = 0_010, P = 0_0469*
summary(gf_neg150_w_lm <- lm(gf.neg150 ~ season_year, data = mm_temp_w[-c(1,33),])) # b1 = 0_016, P = 4_237e-08***

summary(bp_neg150_c_lm <- lm(bp.neg150 ~ season_year, data = mm_temp_c[-32,])) # b1 = 0_033, P = 0_00387 **
summary(hf_neg150_c_lm <- lm(hf.neg150 ~ season_year, data = mm_temp_c[-32,])) # b1 = 0_030, P = 0_0521 _
summary(d6_neg150_c_lm <- lm(d6.neg150 ~ season_year, data = mm_temp_c[c(6:31),])) # b1 = -0_004, P = 0_391
summary(d2_neg150_c_lm <- lm(d2.neg150 ~ season_year, data = mm_temp_c[-32,])) # b1 = 0_036, P = 0_0601 _
summary(gf_neg150_c_lm <- lm(gf.neg150 ~ season_year, data = mm_temp_c[-32,])) # b1 = 0_048, P = 9_54e-09 ***

warm.mods <- rbind.data.frame(coef(summary(bp_150_w_lm))[2,],
                              coef(summary(hf_150_w_lm))[2,],
                              coef(summary(d6_150_w_lm))[2,],
                              coef(summary(d2_150_w_lm))[2,],
                              coef(summary(gf_150_w_lm))[2,],
                              coef(summary(bp_0_w_lm))[2,],
                              coef(summary(hf_0_w_lm))[2,],
                              coef(summary(d6_0_w_lm))[2,],
                              coef(summary(d2_0_w_lm))[2,],
                              coef(summary(gf_0_w_lm))[2,],
                              coef(summary(bp_neg150_w_lm))[2,],
                              coef(summary(hf_neg150_w_lm))[2,],
                              coef(summary(d6_neg150_w_lm))[2,],
                              coef(summary(d2_neg150_w_lm))[2,],
                              coef(summary(gf_neg150_w_lm))[2,])

names(warm.mods) <- c("estimate", "se","tval", "pval") 
warm.mods <- data.frame(var = paste(rep(c("bp","hf","d6","d2","gf"), 3), 
                                    rep(c("150","0","neg150"), each = 5), sep = "."),
                        warm.mods)
warm.mods <- warm.mods %>%
  mutate(sigif = ifelse(pval < 0.05, "sig", ""))

cool.mods <- rbind.data.frame(coef(summary(bp_150_c_lm))[2,],
                              coef(summary(hf_150_c_lm))[2,],
                              coef(summary(d6_150_c_lm))[2,],
                              coef(summary(d2_150_c_lm))[2,],
                              coef(summary(gf_150_c_lm))[2,],
                              coef(summary(bp_0_c_lm))[2,],
                              coef(summary(hf_0_c_lm))[2,],
                              coef(summary(d6_0_c_lm))[2,],
                              coef(summary(d2_0_c_lm))[2,],
                              coef(summary(gf_0_c_lm))[2,],
                              coef(summary(bp_neg150_c_lm))[2,],
                              coef(summary(hf_neg150_c_lm))[2,],
                              coef(summary(d6_neg150_c_lm))[2,],
                              coef(summary(d2_neg150_c_lm))[2,],
                              coef(summary(gf_neg150_c_lm))[2,])

names(cool.mods) <- c("estimate", "se","tval", "pval") 
cool.mods <- data.frame(var = paste(rep(c("bp","hf","d6","d2","gf"), 3), 
                                    rep(c("150","0","neg150"), each = 5), sep = "."),
                        cool.mods)
cool.mods <- cool.mods %>%
  mutate(sigif = ifelse(pval < 0.05, "sig", ""))

## Export at 6 x 3.5

# Warm
jpeg("microclimate_2022.jpg", width = 7, height = 7, units = "in", res = 300)
par(xpd = F)
par(mar = c(1,2,0.5,0.5), oma = c(3,2,0,0))
par(mfcol = c(3,2))

matplot(mm_temp_w$season_year[-c(1,33)], mm_temp_w[-c(1,33),c(3,6,12,15)], type = "l", col = c("darkred","red","yellow","blue"), 
        lty = 1, ylab = "", xlab = "", ylim = c(3,10), axes = F)
lines(mm_temp_w$season_year[-c(1:7,33)], mm_temp_w[-c(1:7,33),9], col = "orange")
mtext("Air (°C)", side = 2, line = 2.5, cex = 0.7)
abline(coef(hf_150_w_lm),lty=2, col = "darkred")
abline(coef(bp_150_w_lm),lty=2, col = "red")
abline(coef(gf_150_w_lm),lty=2, col = "blue")
axis(1, labels = F)
axis(2)
box()

matplot(mm_temp_w$season_year[-c(1,33)], mm_temp_w[-c(1,33),c(4,7,13,16)], type = "l", col = c("darkred","red","yellow","blue"), 
        lty = 1, ylab = "", xlab = "", ylim = c(2,11), axes = F)
lines(mm_temp_w$season_year[-c(1:7,33)], mm_temp_w[-c(1:7,33),10], col = "orange")
mtext("Ground surface (°C)", side = 2, line = 2.5, cex = 0.7)
abline(coef(gf_0_w_lm),lty=2, col = "blue")
axis(1, labels = F)
axis(2)
box()

matplot(mm_temp_w$season_year[-c(1,33)], mm_temp_w[-c(1,33),c(5,8,14,17)], type = "l", col = c("darkred","red","yellow","blue"), 
        lty = 1, ylab = "", xlab = "", ylim = c(-2,0.5))
lines(mm_temp_w$season_year[-c(1:7,33)], mm_temp_w[-c(1:7,33),11], col = "orange")
mtext("Subsurface (°C)", side = 2, line = 2.5, cex = 0.7)
abline(coef(hf_neg150_w_lm),lty=2, col = "darkred")
abline(coef(bp_neg150_w_lm),lty=2, col = "red")
abline(coef(d6_neg150_w_lm),lty=2, col = "orange")
abline(coef(d2_neg150_w_lm),lty=2, col = "yellow")
abline(coef(gf_neg150_w_lm),lty=2, col = "blue")

matplot(mm_temp_c$season_year[-32], mm_temp_c[-32,c(3,6,12,15)], type = "l", col = c("darkred","red","yellow","blue"), 
        lty = 1, ylab = "", xlab = "", ylim = c(-18,-8), axes = F)
lines(mm_temp_c$season_year[-c(1:6,32)], mm_temp_c[-c(1:6,32),9], col = "orange")
axis(1, labels = F)
axis(2)
box()


matplot(mm_temp_c$season_year[-32], mm_temp_c[-32,c(4,7,13,16)], type = "l", col = c("darkred","red","yellow","blue"), 
        lty = 1, ylab = "", xlab = "", ylim = c(-12,0), axes = F)
lines(mm_temp_c$season_year[-c(1:6,32)], mm_temp_c[-c(1:6,32),10], col = "orange")
abline(coef(hf_0_c_lm),lty=2, col = "darkred")
abline(coef(bp_0_c_lm),lty=2, col = "red")
abline(coef(gf_0_c_lm),lty=2, col = "blue")
axis(1, labels = F)
axis(2)
box()

matplot(mm_temp_c$season_year[-32], mm_temp_c[-32,c(5,8,14,17)], type = "l", col = c("darkred","red","yellow","blue"), 
        lty = 1, ylab = "", xlab = "", ylim = c(-5,0))
lines(mm_temp_c$season_year[-c(1:6,32)], mm_temp_c[-c(1:6,32),11], col = "orange")
abline(coef(hf_neg150_c_lm),lty=2, col = "darkred")
abline(coef(gf_neg150_c_lm),lty=2, col = "blue")
axis(1, labels = T)
axis(2)
box()
mtext("Year", side = 1, line = 1, cex = 0.7, outer = T)

par(xpd = NA)
legend(1955.5,7.5, c("HF", "BP", "D6", "D2","GF"), 
       col = c("darkred","red","orange","yellow","blue"), 
       horiz = T, lty = 1, cex = 1, bty = "n", y.intersp = 1, text.width = 2)
dev.off()

# mm_temp_c$hf_150[mm_temp_c$season_year == 1992] <- mm_temp_c$hf_150[mm_temp_c$season_year == 1992]-2

# Cool
jpeg("coolT_2022.jpg", width = 5, height = 7, units = "in", res = 300)
par(xpd = F)
par(mar = c(2,2,1,1), oma = c(2,2,0,0))
par(mfrow = c(3,1))

matplot(mm_temp_c$season_year[-32], mm_temp_c[-32,c(3,6,12,15)], type = "l", col = c("darkred","red","yellow","blue"), 
        lty = 1, ylab = "", xlab = "", ylim = c(-18,-8))
lines(mm_temp_c$season_year[-c(1:6,32)], mm_temp_c[-c(1:6,32),9], col = "orange")
mtext("Air (°C)", side = 2, line = 2.5, cex = 0.7)
# abline(coef(hf_150_c_lm),lty=2, col = "darkred")
# abline(coef(bp_150_c_lm),lty=2, col = "red")
# abline(coef(d6_150_c_lm),lty=2, col = "orange")
# abline(coef(d2_150_c_lm),lty=2, col = "yellow")
# abline(coef(gf_150_c_lm),lty=2, col = "blue")

matplot(mm_temp_c$season_year[-32], mm_temp_c[-32,c(4,7,13,16)], type = "l", col = c("darkred","red","yellow","blue"), 
        lty = 1, ylab = "", xlab = "", ylim = c(-12,0))
lines(mm_temp_c$season_year[-c(1:6,32)], mm_temp_c[-c(1:6,32),10], col = "orange")
mtext("Ground surface (°C)", side = 2, line = 2.5, cex = 0.7)
abline(coef(hf_0_c_lm),lty=2, col = "darkred")
abline(coef(bp_0_c_lm),lty=2, col = "red")
# abline(coef(d6_0_c_lm),lty=2, col = "orange")
# abline(coef(d2_0_c_lm),lty=2, col = "yellow")
abline(coef(gf_0_c_lm),lty=2, col = "blue")

matplot(mm_temp_c$season_year[-32], mm_temp_c[-32,c(5,8,14,17)], type = "l", col = c("darkred","red","yellow","blue"), 
        lty = 1, ylab = "", xlab = "", ylim = c(-5,0))
lines(mm_temp_c$season_year[-c(1:6,32)], mm_temp_c[-c(1:6,32),11], col = "orange")
mtext("Subsurface (°C)", side = 2, line = 2.5, cex = 0.7)
mtext("Year", side = 1, line = 2.5, cex = 0.7)
abline(coef(hf_neg150_c_lm),lty=2, col = "darkred")
# abline(coef(bp.neg150_c_lm),lty=2, col = "red")
# abline(coef(d6.neg150_c_lm),lty=2, col = "orange")
# abline(coef(d2.neg150_c_lm),lty=2, col = "yellow")
abline(coef(gf_neg150_c_lm),lty=2, col = "blue")

par(xpd = NA)
legend(1994,8.8, c("HF", "BP", "D6", "D2","GF"), 
       col = c("darkred","red","orange","yellow","blue"), 
       horiz = T, lty = 1, cex = 1, bty = "n", y.intersp = 1, text.width = 2)
dev.off()

