library(scales) # Add transparency to colors in plotting
library(tidyverse)

rm(list = ls())

## Calculate means, sd, n, se, and 95% CIs. 
## Then subset the data by site and plot means versus year with 95% CI

# Read in the data ----
snow <- read.csv("~/Desktop/Workspace/Earthwatch/Churchill/data/ltems_snow.csv")

# Data wrangling ----
df <- snow %>% 
  as_tibble() %>%
  mutate(across(c(depth, density, swe, htc, date), as.numeric)) %>% 
  group_by(site, date) %>% 
  mutate(depth = ifelse(is.na(depth), mean(depth, na.rm = TRUE), depth),
         density = ifelse(is.na(density), mean(density, na.rm = TRUE), density),
         swe = ifelse(is.na(swe), mean(swe, na.rm = TRUE), swe),
         htc = ifelse(is.na(htc), mean(htc, na.rm = TRUE), htc)) %>% 
  ungroup() %>% 
  group_by(site, date) %>% 
  summarise(n = n(), 
            across(depth:htc, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(depth_ci = (depth_sd / sqrt(n))*qnorm(0.975),
         density_ci = (density_sd / sqrt(n))*qnorm(0.975),
         swe_ci = (swe_sd / sqrt(n))*qnorm(0.975),
         htc_ci = (htc_sd / sqrt(n))*qnorm(0.975),
         depth_c5 = depth_mean - depth_ci,
         depth_c95 = depth_mean + depth_ci,
         density_c5 = density_mean - density_ci,
         density_c95 = density_mean + density_ci,
         swe_c5 = swe_mean - swe_ci,
         swe_c95 = swe_mean + swe_ci,
         htc_c5 = htc_mean - htc_ci,
         htc_c95 = htc_mean + htc_ci) %>% 
  ungroup() %>% 
  rename(year = date)

# Subset the data by site ----
# Tree island
tis <- subset(df, site == "TIS")
# Treed areas
air <- subset(df, site == "AIR")
bsw <- subset(df, site == "BSW")
wsu <- subset(df, site == "WSU")
# Disturbed
bfr <- subset(df, site == "BFR")
pfr <- subset(df, site == "PFR") 
bwp <- subset(df, site == "BWP")
# Exposed
fen <- subset(df, site == "FEN")
ppa <- subset(df, site == "PPA")
ppd <- subset(df, site == "PPD")
tun <- subset(df, site == "TUN")

# Linear models ----
## Linear models to determine if there are significant trends through time.
## Any significant trends will be plotted on the corresponding figure.
# Depth
summary(tis_depth <- lm(tis$depth_mean ~ tis$year)) # SIG
summary(lm(air$depth_mean ~ air$year)) # n.s.
summary(lm(bsw$depth_mean ~ bsw$year)) # n.s.
summary(lm(wsu$depth_mean ~ wsu$year)) # n.s.
summary(lm(bfr$depth_mean ~ bfr$year)) # n.s.
summary(lm(pfr$depth_mean ~ pfr$year)) # n.s.
summary(bwp_depth <- lm(bwp$depth_mean ~ bwp$year)) # SIG
summary(lm(fen$depth_mean ~ fen$year)) # n.s.
summary(lm(ppa$depth_mean ~ ppa$year)) # n.s.
summary(lm(ppd$depth_mean ~ ppd$year)) # n.s.
summary(lm(tun$depth_mean ~ tun$year)) # n.s.
# Density
summary(lm(tis$density_mean ~ tis$year)) # n.s.
summary(air_density <- lm(air$density_mean ~ air$year)) # SIG
summary(lm(bsw$density_mean ~ bsw$year)) # n.s.
summary(lm(wsu$density_mean ~ wsu$year)) # n.s.
summary(bfr_density <- lm(bfr$density_mean ~ bfr$year)) # SIG
summary(lm(pfr$density_mean ~ pfr$year)) # n.s.
summary(bwp_density <- lm(bwp$density_mean ~ bwp$year)) # SIG
summary(lm(fen$density_mean ~ fen$year)) # n.s.
summary(lm(ppa$density_mean ~ ppa$year)) # n.s.
summary(lm(ppd$density_mean ~ ppd$year)) # n.s.
summary(lm(tun$density_mean ~ tun$year)) # n.s.
# SWE
summary(lm(tis$swe_mean ~ tis$year)) # n.s.
summary(lm(air_swe <- air$swe_mean ~ air$year)) # SIG
summary(lm(bsw$swe_mean ~ bsw$year)) # n.s.
summary(lm(wsu$swe_mean ~ wsu$year)) # n.s.
summary(lm(bfr$swe_mean ~ bfr$year)) # n.s.
summary(lm(pfr$swe_mean ~ pfr$year)) # n.s.
summary(bwp_swe <- lm(bwp$swe_mean ~ bwp$year)) # SIG
summary(lm(fen$swe_mean ~ fen$year)) # n.s.
summary(lm(ppa$swe_mean ~ ppa$year)) # n.s.
summary(lm(ppd$swe_mean ~ ppd$year)) # n.s.
summary(lm(tun$swe_mean ~ tun$year)) # n.s.
# HTC
summary(tis_htc <- lm(tis$htc_mean ~ tis$year)) # SIG
summary(lm(air$htc_mean ~ air$year)) # n.s.
summary(lm(bsw$htc_mean ~ bsw$year)) # n.s.
summary(lm(wsu$htc_mean ~ wsu$year)) # n.s.
summary(lm(bfr$htc_mean ~ bfr$year)) # n.s.
summary(pfr_htc <- lm(pfr$htc_mean ~ pfr$year)) # SIG
summary(bwp_htc <- lm(bwp$htc_mean ~ bwp$year)) # SIG
summary(lm(fen$htc_mean ~ fen$year)) # n.s.
summary(lm(ppa$htc_mean ~ ppa$year)) # n.s.
summary(lm(ppd$htc_mean ~ ppd$year)) # n.s.
summary(lm(tun$htc_mean ~ tun$year)) # n.s.

# ______________________________________


# Figures ----

## Depth ----
##### Export at 5 x 9
jpeg("~/Desktop/Workspace/Earthwatch/Churchill/figures/snow_depth_2023.jpg", 
     height = 9, width = 5, units = "in", res = 150)
par(mfrow = c(4,1), mar = c(1,1,1,1), oma = c(3,3,0,0))
## TIS
plot(tis$year, tis$depth_mean, type='n', xlim = c(2000,2023), ylim = c(40,200), axes = F, xlab = "", ylab = "")
polygon(c(tis$year, rev(tis$year)), c(tis$depth_c95, rev(tis$depth_c5)), col = alpha("lightgreen",0.5), border = NA)
lines(tis$year, tis$depth_mean, col = alpha("green4",0.6), lwd = 2)
abline(coef(tis_depth), col = alpha("green4",0.5), lty = 3, lwd = 2)
box()
axis(side = 1, at = seq(2000,2023,2), labels = NA)
axis(side = 2, at = seq(0,200,50))
legend("bottomleft", "TIS", fill = "lightgreen", border = NA, bty = "n", inset = c(0,0))

## Treed areas
plot(air$year, air$depth_mean, type="n", xlim = c(2000,2023), ylim = c(40,100), 
     axes = F, xlab = "", ylab = "")
# AIR
polygon(c(air$year, rev(air$year)), c(air$depth_c95, rev(air$depth_c5)), col = alpha("lightgreen",0.5), border = NA)
lines(air$year, air$depth_mean, lwd=2, col = alpha("green4",0.6))
# BSW
polygon(c(bsw$year, rev(bsw$year)), c(bsw$depth_c95, rev(bsw$depth_c5)), col = alpha("cadetblue1",0.5), border = NA)
lines(bsw$year, bsw$depth_mean, lwd=2, col = alpha("deepskyblue1",0.6))
# WSU
polygon(c(wsu$year, rev(wsu$year)), c(wsu$depth_c95, rev(wsu$depth_c5)), col = alpha("darksalmon",0.5), border = NA)
lines(wsu$year, wsu$depth_mean, lwd=2, col = alpha("darkorange3",0.6))
box()
axis(side = 1, at = seq(2000,2023,2), labels = NA)
axis(side = 2, at = seq(0,100,25))
legend("topleft", c("AIR","BSW","WSU"), 
       fill = c("lightgreen","cadetblue1","darksalmon"), 
       border = NA, bty = "n", inset = c(0,0), horiz = F)

## Disturbed areas
plot(bwp$year, bwp$depth_mean, type="n", xlim = c(2000,2023), ylim = c(0,100), 
     axes = F, xlab = "", ylab = "")
# BWP
polygon(c(bwp$year, rev(bwp$year)), c(bwp$depth_c95, rev(bwp$depth_c5)), col = alpha("lightgreen",0.5), border = NA)
lines(bwp$year, bwp$depth_mean, lwd=2, col = alpha("green4",0.6))
# abline(coef(bwp_depth), lty = 2, col = alpha("green4"))
abline(coef(bwp_depth), col = alpha("green4",0.5), lty = 3, lwd = 2)
# PFR
polygon(c(pfr$year, rev(pfr$year)), c(pfr$depth_c95, rev(pfr$depth_c5)), col = alpha("cadetblue1",0.5), border = NA)
lines(pfr$year, pfr$depth_mean, lwd=2, col = alpha("deepskyblue1",0.6))
# BFR
polygon(c(bfr$year, rev(bfr$year)), c(bfr$depth_c95, rev(bfr$depth_c5)), col = alpha("darksalmon",0.5), border = NA)
lines(bfr$year, bfr$depth_mean, lwd=2, col = alpha("darkorange3",0.6))
box()
axis(side = 1, at = seq(2000,2023,2), labels = NA)
axis(side = 2, at = seq(0,200,50))
legend("topleft", c("BWP","BFR","PFR"), 
       fill = c("lightgreen","darksalmon","cadetblue1"), 
       border = NA, bty = "n", inset = c(0,0), horiz = F)

## Exposed areas
plot(tun$year, tun$depth_mean, type="n", xlim = c(2000,2023), ylim = c(0,50), 
     axes = F, xlab = "", ylab = "")
# PPD
polygon(c(ppd$year, rev(ppd$year)), c(ppd$depth_c95, rev(ppd$depth_c5)), col = alpha("mediumorchid1",0.5), border = NA)
lines(ppd$year, ppd$depth_mean, lwd=2, col = alpha("mediumorchid4",0.6))
# PPA
polygon(c(ppa$year, rev(ppa$year)), c(ppa$depth_c95, rev(ppa$depth_c5)), col = alpha("darksalmon",0.5), border = NA)
lines(ppa$year, ppa$depth_mean, lwd=2, col = alpha("darkorange3",0.6))
# TUN
polygon(c(tun$year, rev(tun$year)), c(tun$depth_c95, rev(tun$depth_c5)), col = alpha("lightgreen",0.5), border = NA)
lines(tun$year, tun$depth_mean, lwd=2, col = alpha("green4",0.6))
# FEN
polygon(c(fen$year, rev(fen$year)), c(fen$depth_c95, rev(fen$depth_c5)), col = alpha("cadetblue1",0.5), border = NA)
lines(fen$year, fen$depth_mean, lwd=2, col = alpha("deepskyblue1",0.6))
box()
axis(side = 1, at = seq(2000,2023,2), labels = seq(2000,2023,2))
axis(side = 2, at = seq(0,50,10))
legend("topleft", c("TUN","FEN","PPA","PPD"), 
       fill = c("lightgreen","cadetblue1","darksalmon","mediumorchid1"), 
       border = NA, bty = "n", inset = c(0,0), horiz = F)
mtext(side = 1, "Year", line = 1.5, cex = 0.75, outer = T)
mtext(side = 2, "Depth (cm)", line = 1.5, cex = 0.75, outer = T)
dev.off()

# ______________________________________

## Density ----
jpeg("~/Desktop/Workspace/Earthwatch/Churchill/figures/snow_density_2023.jpg", 
     height = 9, width = 5, units = "in", res = 150)
par(mfrow = c(4,1), mar = c(1,1,1,1), oma = c(3,3,0,0))
## TIS
plot(tis$year, tis$density_mean, type='n', xlim = c(2000,2023), ylim = c(100,500), axes = F, xlab = "", ylab = "")
polygon(c(tis$year, rev(tis$year)), c(tis$density_c95, rev(tis$density_c5)), col = alpha("lightgreen",0.5), border = NA)
lines(tis$year, tis$density_mean, col = alpha("green4",0.6), lwd = 2)
box()
axis(side = 1, at = seq(2000,2023,2), labels = NA)
axis(side = 2, at = seq(0,500,100))
legend("bottomleft", "TIS", fill = "lightgreen", border = NA, bty = "n", inset = c(0,0))

## Treed areas
plot(air$year, air$density_mean, type="n", xlim = c(2000,2023), ylim = c(100,500), 
     axes = F, xlab = "", ylab = "")
# AIR
polygon(c(air$year, rev(air$year)), c(air$density_c95, rev(air$density_c5)), col = alpha("lightgreen",0.5), border = NA)
lines(air$year, air$density_mean, lwd=2, col = alpha("green4",0.6))
abline(coef(air_density), col = alpha("green4",0.5), lty = 3, lwd = 2)
# BSW
polygon(c(bsw$year, rev(bsw$year)), c(bsw$density_c95, rev(bsw$density_c5)), col = alpha("cadetblue1",0.5), border = NA)
lines(bsw$year, bsw$density_mean, lwd=2, col = alpha("deepskyblue1",0.6))
# WSU
polygon(c(wsu$year, rev(wsu$year)), c(wsu$density_c95, rev(wsu$density_c5)), col = alpha("darksalmon",0.5), border = NA)
lines(wsu$year, wsu$density_mean, lwd=2, col = alpha("darkorange3",0.6))
box()
axis(side = 1, at = seq(2000,2023,2), labels = NA)
axis(side = 2, at = seq(0,500,100))
legend("topleft", c("AIR","BSW","WSU"), 
       fill = c("lightgreen","cadetblue1","darksalmon"), 
       border = NA, bty = "n", inset = c(0,0), horiz = F)

## Disturbed areas
plot(bwp$year, bwp$density_mean, type="n", xlim = c(2000,2023), ylim = c(100,500), 
     axes = F, xlab = "", ylab = "")
# BWP
polygon(c(bwp$year, rev(bwp$year)), c(bwp$density_c95, rev(bwp$density_c5)), col = alpha("lightgreen",0.5), border = NA)
lines(bwp$year, bwp$density_mean, lwd=2, col = alpha("green4",0.6))
abline(coef(bwp_density), col = alpha("green4",0.5), lty = 3, lwd = 2)
# PFR
polygon(c(pfr$year, rev(pfr$year)), c(pfr$density_c95, rev(pfr$density_c5)), col = alpha("cadetblue1",0.5), border = NA)
lines(pfr$year, pfr$density_mean, lwd=2, col = alpha("deepskyblue1",0.6))
# BFR
polygon(c(bfr$year, rev(bfr$year)), c(bfr$density_c95, rev(bfr$density_c5)), col = alpha("darksalmon",0.5), border = NA)
lines(bfr$year, bfr$density_mean, lwd=2, col = alpha("darkorange3",0.6))
abline(coef(bfr_density), col = alpha("green4",0.5), lty = 3, lwd = 2)
box()
axis(side = 1, at = seq(2000,2023,2), labels = NA)
axis(side = 2, at = seq(0,500,100))
legend("topleft", c("BWP","BFR","PFR"), 
       fill = c("lightgreen","darksalmon","cadetblue1"), 
       border = NA, bty = "n", inset = c(0,0), horiz = F)

## Exposed areas
plot(tun$year, tun$density_mean, type="n", xlim = c(2000,2023), ylim = c(100,500), 
     axes = F, xlab = "", ylab = "")
# PPD
polygon(c(ppd$year, rev(ppd$year)), c(ppd$density_c95, rev(ppd$density_c5)), col = alpha("mediumorchid1",0.5), border = NA)
lines(ppd$year, ppd$density_mean, lwd=2, col = alpha("mediumorchid4",0.6))
# PPA
polygon(c(ppa$year, rev(ppa$year)), c(ppa$density_c95, rev(ppa$density_c5)), col = alpha("darksalmon",0.5), border = NA)
lines(ppa$year, ppa$density_mean, lwd=2, col = alpha("darkorange3",0.6))
# TUN
polygon(c(tun$year, rev(tun$year)), c(tun$density_c95, rev(tun$density_c5)), col = alpha("lightgreen",0.5), border = NA)
lines(tun$year, tun$density_mean, lwd=2, col = alpha("green4",0.6))
# abline(coef(lm(tun[,4] ~ tun$year)), col = alpha("green4",0.5), lty = 3, lwd = 2)
# FEN
polygon(c(fen$year, rev(fen$year)), c(fen$density_c95, rev(fen$density_c5)), col = alpha("cadetblue1",0.5), border = NA)
lines(fen$year, fen$density_mean, lwd=2, col = alpha("deepskyblue1",0.6))
box()
axis(side = 1, at = seq(2000,2023,2), labels = seq(2000,2023,2))
axis(side = 2, at = seq(0,500,100))
legend("topleft", c("TUN","FEN","PPA","PPD"), 
       fill = c("lightgreen","cadetblue1","darksalmon","mediumorchid1"), 
       border = NA, bty = "n", inset = c(0,0), horiz = F)
mtext(side = 1, "Year", line = 1.5, cex = 0.75, outer = T)
mtext(expression(paste("Density (kg m"^"-3",")")), side = 2, line=1.5, cex = 0.75, outer = T)
dev.off()

# ______________________________________

## SWE ----
jpeg("~/Desktop/Workspace/Earthwatch/Churchill/figures/snow_swe_2023.jpg", 
     height = 9, width = 5, units = "in", res = 150)
par(mfrow = c(4,1), mar = c(1,1,1,1), oma = c(3,3,0,0))
## TIS
plot(tis$year, tis$swe_mean, type='n', xlim = c(2000,2023), ylim = c(0,800), axes = F, xlab = "", ylab = "")
polygon(c(tis$year, rev(tis$year)), c(tis$swe_c95, rev(tis$swe_c5)), col = alpha("lightgreen",0.5), border = NA)
lines(tis$year, tis$swe_mean, col = alpha("green4",0.6), lwd = 2)
box()
axis(side = 1, at = seq(2000,2023,2), labels = NA)
axis(side = 2, at = seq(0,800,200))
legend("bottomleft", "TIS", fill = "lightgreen", border = NA, bty = "n", inset = c(0,0))

## Treed areas
plot(air$year, air$swe_mean, type="n", xlim = c(2000,2023), ylim = c(0,200), 
     axes = F, xlab = "", ylab = "")
# AIR
polygon(c(air$year, rev(air$year)), c(air$swe_c95, rev(air$swe_c5)), col = alpha("lightgreen",0.5), border = NA)
lines(air$year, air$swe_mean, lwd=2, col = alpha("green4",0.6))
abline(coef(air_swe), col = alpha("green4",0.5), lty = 3, lwd = 2)
# BSW
polygon(c(bsw$year, rev(bsw$year)), c(bsw$swe_c95, rev(bsw$swe_c5)), col = alpha("cadetblue1",0.5), border = NA)
lines(bsw$year, bsw$swe_mean, lwd=2, col = alpha("deepskyblue1",0.6))
# WSU
polygon(c(wsu$year, rev(wsu$year)), c(wsu$swe_c95, rev(wsu$swe_c5)), col = alpha("darksalmon",0.5), border = NA)
lines(wsu$year, wsu$swe_mean, lwd=2, col = alpha("darkorange3",0.6))
box()
axis(side = 1, at = seq(2000,2023,2), labels = NA)
axis(side = 2, at = seq(0,200,50))
legend("topleft", c("AIR","BSW","WSU"), 
       fill = c("lightgreen","cadetblue1","darksalmon"), 
       border = NA, bty = "n", inset = c(0,0), horiz = F)

## Disturbed areas
plot(bwp$year, bwp$swe_mean, type="n", xlim = c(2000,2023), ylim = c(0,200), 
     axes = F, xlab = "", ylab = "")
# BWP
polygon(c(bwp$year, rev(bwp$year)), c(bwp$swe_c95, rev(bwp$swe_c5)), col = alpha("lightgreen",0.5), border = NA)
lines(bwp$year, bwp$swe_mean, lwd=2, col = alpha("green4",0.6))
abline(coef(bwp_swe), col = alpha("green4",0.5), lty = 3, lwd = 2)
# PFR
polygon(c(pfr$year, rev(pfr$year)), c(pfr$swe_c95, rev(pfr$swe_c5)), col = alpha("cadetblue1",0.5), border = NA)
lines(pfr$year, pfr$swe_mean, lwd=2, col = alpha("deepskyblue1",0.6))
# BFR
polygon(c(bfr$year, rev(bfr$year)), c(bfr$swe_c95, rev(bfr$swe_c5)), col = alpha("darksalmon",0.5), border = NA)
lines(bfr$year, bfr$swe_mean, lwd=2, col = alpha("darkorange3",0.6))
# abline(coef(lm(bfr[,5] ~ bfr$year)), col = alpha("green4",0.5), lty = 3, lwd = 2)
box()
axis(side = 1, at = seq(2000,2023,2), labels = NA)
axis(side = 2, at = seq(0,200,50))
legend("topleft", c("BWP","BFR","PFR"), 
       fill = c("lightgreen","darksalmon","cadetblue1"), 
       border = NA, bty = "n", inset = c(0,0), horiz = F)

## Exposed areas
plot(tun$year, tun$swe_mean, type="n", xlim = c(2000,2023), ylim = c(0,200), 
     axes = F, xlab = "", ylab = "")
# PPD
polygon(c(ppd$year, rev(ppd$year)), c(ppd$swe_c95, rev(ppd$swe_c5)), col = alpha("mediumorchid1",0.5), border = NA)
lines(ppd$year, ppd$swe_mean, lwd=2, col = alpha("mediumorchid4",0.6))
# PPA
polygon(c(ppa$year, rev(ppa$year)), c(ppa$swe_c95, rev(ppa$swe_c5)), col = alpha("darksalmon",0.5), border = NA)
lines(ppa$year, ppa$swe_mean, lwd=2, col = alpha("darkorange3",0.6))
# TUN
polygon(c(tun$year, rev(tun$year)), c(tun$swe_c95, rev(tun$swe_c5)), col = alpha("lightgreen",0.5), border = NA)
lines(tun$year, tun$swe_mean, lwd=2, col = alpha("green4",0.6))
# FEN
polygon(c(fen$year, rev(fen$year)), c(fen$swe_c95, rev(fen$swe_c5)), col = alpha("cadetblue1",0.5), border = NA)
lines(fen$year, fen$swe_mean, lwd=2, col = alpha("deepskyblue1",0.6))
box()
axis(side = 1, at = seq(2000,2023,2), labels = seq(2000,2023,2))
axis(side = 2, at = seq(0,200,50))
legend("topleft", c("TUN","FEN","PPA","PPD"), 
       fill = c("lightgreen","cadetblue1","darksalmon","mediumorchid1"), 
       border = NA, bty = "n", inset = c(0,0), horiz = F)
mtext(side = 1, "Year", line = 1.5, cex = 0.75, outer = T)
mtext(side = 2, "SWE (mm)", line = 1.5, cex = 0.75, outer = T)
dev.off()

# ______________________________________

## HTC ----
jpeg("~/Desktop/Workspace/Earthwatch/Churchill/figures/snow_htc_2023.jpg", 
     height = 9, width = 5, units = "in", res = 150)
par(mfrow = c(4,1), mar = c(1,1,1,1), oma = c(3,3,0,0))
## TIS
plot(tis$year, tis$htc_mean, type='n', xlim = c(2000,2023), ylim = c(0,1), axes = F, xlab = "", ylab = "")
polygon(c(tis$year, rev(tis$year)), c(tis$htc_c95, rev(tis$htc_c5)), col = alpha("lightgreen",0.5), border = NA)
lines(tis$year, tis$htc_mean, col = alpha("green4",0.6), lwd = 2)
abline(coef(tis_htc), col = alpha("green4",0.5), lty = 3, lwd = 2)
box()
axis(side = 1, at = seq(2000,2023,2), labels = NA)
axis(side = 2, at = seq(0,1,0.2))
legend("bottomleft", "TIS", fill = "lightgreen", border = NA, bty = "n", inset = c(0,0))

## Treed areas
plot(air$year, air$htc_mean, type="n", xlim = c(2000,2023), ylim = c(0,1), 
     axes = F, xlab = "", ylab = "")
# AIR
polygon(c(air$year, rev(air$year)), c(air$htc_c95, rev(air$htc_c5)), col = alpha("lightgreen",0.5), border = NA)
lines(air$year, air$htc_mean, lwd=2, col = alpha("green4",0.6))
# BSW
polygon(c(bsw$year, rev(bsw$year)), c(bsw$htc_c95, rev(bsw$htc_c5)), col = alpha("cadetblue1",0.5), border = NA)
lines(bsw$year, bsw$htc_mean, lwd=2, col = alpha("deepskyblue1",0.6))
# WSU
polygon(c(wsu$year, rev(wsu$year)), c(wsu$htc_c95, rev(wsu$htc_c5)), col = alpha("darksalmon",0.5), border = NA)
lines(wsu$year, wsu$htc_mean, lwd=2, col = alpha("darkorange3",0.6))
box()
axis(side = 1, at = seq(2000,2023,2), labels = NA)
axis(side = 2, at = seq(0,1,0.2))
legend("topleft", c("AIR","BSW","WSU"), 
       fill = c("lightgreen","cadetblue1","darksalmon"), 
       border = NA, bty = "n", inset = c(0,0), horiz = F)

## Disturbed areas
plot(bwp$year, bwp$htc_mean, type="n", xlim = c(2000,2023), ylim = c(0,5), 
     axes = F, xlab = "", ylab = "")
# BWP
polygon(c(bwp$year, rev(bwp$year)), c(bwp$htc_c95, rev(bwp$htc_c5)), col = alpha("lightgreen",0.5), border = NA)
lines(bwp$year, bwp$htc_mean, lwd=2, col = alpha("green4",0.6))
abline(coef(bwp_htc), col = alpha("green4",0.5), lty = 3, lwd = 2)
# PFR
polygon(c(pfr$year, rev(pfr$year)), c(pfr$htc_c95, rev(pfr$htc_c5)), col = alpha("cadetblue1",0.5), border = NA)
lines(pfr$year, pfr$htc_mean, lwd=2, col = alpha("deepskyblue1",0.6))
abline(coef(pfr_htc), col = alpha("deepskyblue1",0.5), lty = 3, lwd = 2)
# BFR
polygon(c(bfr$year, rev(bfr$year)), c(bfr$htc_c95, rev(bfr$htc_c5)), col = alpha("darksalmon",0.5), border = NA)
lines(bfr$year, bfr$htc_mean, lwd=2, col = alpha("darkorange3",0.6))
box()
axis(side = 1, at = seq(2000,2023,2), labels = NA)
axis(side = 2, at = seq(0,5,1))
legend("topleft", c("BWP","BFR","PFR"), 
       fill = c("lightgreen","darksalmon","cadetblue1"), 
       border = NA, bty = "n", inset = c(0,0), horiz = F)

## Exposed areas
plot(tun$year, tun$htc_mean, type="n", xlim = c(2000,2023), ylim = c(0,12), 
     axes = F, xlab = "", ylab = "")
# PPD
polygon(c(ppd$year, rev(ppd$year)), c(ppd$htc_c95, rev(ppd$htc_c5)), col = alpha("mediumorchid1",0.5), border = NA)
lines(ppd$year, ppd$htc_mean, lwd=2, col = alpha("mediumorchid4",0.6))
# PPA
polygon(c(ppa$year, rev(ppa$year)), c(ppa$htc_c95, rev(ppa$htc_c5)), col = alpha("darksalmon",0.5), border = NA)
lines(ppa$year, ppa$htc_mean, lwd=2, col = alpha("darkorange3",0.6))
# TUN
polygon(c(tun$year, rev(tun$year)), c(tun$htc_c95, rev(tun$htc_c5)), col = alpha("lightgreen",0.5), border = NA)
lines(tun$year, tun$htc_mean, lwd=2, col = alpha("green4",0.6))
# FEN
polygon(c(fen$year, rev(fen$year)), c(fen$htc_c95, rev(fen$htc_c5)), col = alpha("cadetblue1",0.5), border = NA)
lines(fen$year, fen$htc_mean, lwd=2, col = alpha("deepskyblue1",0.6))
box()
axis(side = 1, at = seq(2000,2023,2), labels = seq(2000,2023,2))
axis(side = 2, at = seq(0,12,2))
legend("topleft", c("TUN","FEN","PPA","PPD"), 
       fill = c("lightgreen","cadetblue1","darksalmon","mediumorchid1"), 
       border = NA, bty = "n", inset = c(0,0), horiz = F)
mtext(side = 1, "Year", line = 1.5, cex = 0.75, outer = T)
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=1.5, cex = 0.75, outer = T)
dev.off()





ch.snow <- read.csv(file = "~/Desktop/Workspace/Earthwatch/ch.snow.csv", header = TRUE)
levels(ch.snow[ch.snow$type == "island",]$plotting)

# A	baydjarakh	17
# B	plateau	9
# C	tundra	18
# D	beach	18
# E	disturbance	14
# F	wedge	18
# G	fen	16
# H	forest	18
# I	palsa	18
# J	burn	14
# K	baydjarakh wedge	18
# L	shrub 18
# M	island	18

## Plot the snow data and color according to veg cover
# Export at 14 x 8
par(ps = 14, cex = 1, cex.axis = 1)
par(mfrow = c(2, 1))
par(mar = c(0, 2, 1, 1), oma = c(4,2,1,1))

colors <- c(rep('darkgreen',13),rep('forestgreen',14),rep('green3',14),rep('green',10),
            rep('greenyellow',14),rep('lightgreen',14),rep('olivedrab1',12),rep('yellow',14),
            rep('orange',10),rep('darkorange3',14),rep('red',14),rep('deeppink4',5),rep('darkred',14))

bp1 <- boxplot(swe ~ plotting, data = ch.snow, outline=FALSE, ylim = c(0,1200), col = colors, xaxt = "n")
points(bp1$group, bp1$out, type = "p", pch=1, cex = 0.75)
axis(1, at = 1:171, labels = NA, tick = TRUE)
mtext(side=2, "SWE (mm) ",adj=0.5, line=2.5)

bp2 <- boxplot(htc ~ plotting, data = ch.snow, outline=FALSE, ylim = c(0,10), col = colors, xaxt = "n")
points(bp2$group, bp2$out, type = "p", pch=1, cex = 0.75)
axis(1, at = 1:171, labels = NA, tick = TRUE)
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2.5)


#### Plotting needle data

# needles.2013 <- read.csv(file = "~/Desktop/Workspace/Desiccation/needles2013.csv", header = TRUE)
needles.2016 <- read.csv(file = "~/Desktop/Workspace/Desiccation/needles2016.csv", header = TRUE)
needles.2016 <- subset(needles.2016, gmin > 0)

forest <- subset(needles.2016, zone == "Forest")
ecotone <- subset(needles.2016, zone == "Ecotone")
tundra <- subset(needles.2016, zone == "Tundra")
tree.island <- subset(needles.2016, zone == "Tree island")

# Remove F and T aspects and drop the unused levels

needles2 <- droplevels(subset(needles.2016, aspect != "F" & aspect != "T")) 
levels(needles2$aspect)
needles2$year <- as.factor(needles2$year)
levels(needles2$year)

forest2 <- droplevels(subset(forest, aspect != "F" & aspect != "T")) 
levels(forest2$aspect)
levels(forest2$year)
forest2$year <- as.factor(forest2$year)

ecotone2 <- droplevels(subset(ecotone, aspect != "F" & aspect != "T")) 
levels(ecotone2$aspect)
levels(ecotone2$year)
ecotone2$year <- as.factor(ecotone2$year)

tree.island2 <- droplevels(subset(tree.island, aspect != "F" & aspect != "T")) 
levels(tree.island2$aspect)
levels(tree.island2$year)
tree.island2$year <- as.factor(tree.island2$year)

tundra2 <- droplevels(subset(tundra, aspect != "F" & aspect != "T")) 
levels(tundra2$aspect)
levels(tundra2$year)
tundra2$year <- as.factor(tundra2$year)

## ANOVAs for all, forest, ecotone, tundra, tree island

## All
aov.all1 <- aov(gmin ~ zone, data = needles2[needles2$year=="2014",])
summary(aov.all1)
TukeyHSD(aov.all1)
all.pairs <- glht(aov.all1, linfct = mcp(zone = "Tukey"))
all.cld <- cld(all.pairs)
all.letters <- all.cld$mcletters$Letters
aov.all2 <- aov(gmin ~ zone, data = needles2[needles2$year=="2013",])
summary(aov.all2)
TukeyHSD(aov.all2)
all.pairs <- glht(aov.all2, linfct = mcp(zone = "Tukey"))
all.cld <- cld(all.pairs)
all.letters <- all.cld$mcletters$Letters

## Forest
aov.for1 <- aov(gmin ~ year*height, data = forest2) 
summary(aov.for1)
# No significant differences among heights, just among years.
aov.for2 <- aov(gmin ~ year*aspect, data = forest2) 
summary(aov.for2)
# No significant differences among aspects, just among years.
aov.for3 <- aov(gmin ~ year, data = forest2) 
TukeyHSD(aov.for3)
for.pairs <- glht(aov.for3, linfct = mcp(year = "Tukey"))
for.cld <- cld(for.pairs)
for.letters <- for.cld$mcletters$Letters

## Ecotone
aov.eco1 <- aov(gmin ~ year*height, data = ecotone2) 
summary(aov.eco1)
# No significant differences among heights, just among years.
aov.eco2 <- aov(gmin ~ year*aspect, data = ecotone2) 
summary(aov.eco2)
# Significant differences among aspects and years.
aov.eco3 <- aov(gmin ~ year, data = ecotone2) 
TukeyHSD(aov.eco3)
eco.pairs <- glht(aov.eco3, linfct = mcp(year = "Tukey"))
eco.cld <- cld(eco.pairs)
eco.letters <- eco.cld$mcletters$Letters

## tree.island
aov.tis1 <- aov(gmin ~ year*height, data = tree.island2) 
summary(aov.tis1)
# Significant differences among heights and years, plus an interaction between the two.
aov.tis2 <- aov(gmin ~ year*aspect, data = tree.island2) 
summary(aov.tis2)
# Significant differences among aspects and years, plus an interaction between the two.
aov.tis3 <- aov(gmin ~ year, data = tree.island2) 
summary(aov.tis3)
aov.tis4 <- aov(gmin ~ island.side, tree.island2[tree.island2$year == "2014",])
summary(aov.tis4)
aov.tis4 <- aov(gmin ~ island.side*aspect*year, data = tree.island2)
summary(aov.tis4)
aov.tis4 <- aov(gmin ~ island.side*height, data = tree.island2[tree.island2$year == "2014",])
summary(aov.tis4)
boxplot(gmin ~ aspect*height*island.side, data = tree.island2[tree.island2$year == "2013",])
boxplot(gmin ~ island.side*aspect, data = tree.island2[tree.island2$year == "2013",])



TukeyHSD(aov.tis3)
tis.pairs <- glht(aov.tis3, linfct = mcp(year = "Tukey"))
tis.cld <- cld(tis.pairs)
tis.letters <- tis.cld$mcletters$Letters

## tundra
aov.tun1 <- aov(gmin ~ year*height, data = tundra2) 
summary(aov.tun1)
# Significant differences among heights and years.
aov.tun2 <- aov(gmin ~ year*aspect, data = tundra2) 
summary(aov.tun2)
# Significant differences among years but not aspects.
aov.tun3 <- aov(gmin ~ year, data = tundra2) 
summary(aov.tun3)
TukeyHSD(aov.tun3)
tun.pairs <- glht(aov.tun3, linfct = mcp(year = "Tukey"))
tun.cld <- cld(tun.pairs)
tun.letters <- tun.cld$mcletters$Letters


# x1 <- factor(forest2$height, levels = c("C", "A", "S"))

## Export at 7 x 12 for individual figure
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(1, 2, 1.75, 3), oma = c(3,2,1,1))

x1 <- factor(forest2$year, levels = c("2008","2009","2010","2011","2012","2013","2014"))
bp1 <- boxplot(gmin ~ x1, ylim = c(0,0.000189),data = forest2, col = "darkgreen")
# mtext(expression(italic("g")[min] ~ (m ~ s^{-1} ~ 10^{-5})),side=2, adj=0.5, line=2.5)
# mtext(side=1,"Year", adj=0.5, line=2.5)
text(0.5,0.00018,"(a) Forest", adj = c(0,0))
for.y <- t(aggregate(forest2$gmin, by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:3),y=for.y[1:3]+0.00002,for.letters[1:3])
text(6,0.00008,"a")
text(7,0.0001827,"b")

x2 <- factor(ecotone2$year, levels = c("2008","2009","2010","2011","2012","2013","2014"))
bp1 <- boxplot(gmin ~ x2, ylim = c(0,0.000189),data = ecotone2,col = "chartreuse4")
# mtext(expression(italic("g")[min] ~ (m ~ s^{-1} ~ 10^{-5})),side=2, adj=0.5, line=2.5)
# mtext(side=1,"Year", adj=0.5, line=2.5)
text(0.5,0.00018,"(b) Ecotone", adj = c(0,0))
eco.y <- t(aggregate(ecotone2$gmin, by=list(x2), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:3),y=eco.y[1:3]+0.00002,eco.letters[1:3])
text(6,0.0001,"a")
text(7,0.000145,"b")

x3 <- factor(tree.island2$year, levels = c("2008","2009","2010","2011","2012","2013","2014"))
bp1 <- boxplot(gmin ~ x3, ylim = c(0,0.000189),data = tree.island2,col = "chartreuse3")
# mtext(expression(italic("g")[min] ~ (m ~ s^{-1} ~ 10^{-5})),side=2, adj=0.5, line=2.5)
# mtext(side=1,"Year", adj=0.5, line=2.5)
text(0.5,0.00018,"(c) Tree island", adj = c(0,0))
tis.y <- t(aggregate(tree.island2$gmin, by=list(x3), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(6:7),y=tis.y+0.00002,tis.letters)

x4 <- factor(tundra2$year, levels = c("2008","2009","2010","2011","2012","2013","2014"))
bp1 <- boxplot(gmin ~ x4, ylim = c(0,0.000189),data = tundra2,col = "yellow")
# mtext(expression(italic("g")[min] ~ (m ~ s^{-1} ~ 10^{-5})),side=2, adj=0.5, line=2.5)
mtext(side=1,"Year", adj=0.5, line=2.5)
text(0.5,0.00018,"(d) Tundra", adj = c(0,0))
tun.y <- t(aggregate(tundra2$gmin, by=list(x4), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:3),y=tun.y+0.00002,tun.letters)

mtext(expression(italic("g")[min] ~ (m ~ s^{-1} ~ 10^{-5})),side=2, adj=0.5, line=0.5, outer = TRUE)

x5 <- factor(needles2$year.zone, levels = c("Forest.2008","Forest.2009","Forest.2010","Forest.2011",
                                            "Forest.2012","Forest.2013","Forest.2014",
                                            "Ecotone.2008","Ecotone.2009","Ecotone.2010","Ecotone.2011",
                                            "Ecotone.2012","Ecotone.2013","Ecotone.2014",
                                            "Tree island.2008","Tree island.2009","Tree island.2010","Tree island.2011",
                                            "Tree island.2012","Tree island.2013","Tree island.2014",
                                            "Tundra.2008","Tundra.2009","Tundra.2010","Tundra.2011",
                                            "Tundra.2012","Tundra.2013","Tundra.2014"))
bp1 <- boxplot(gmin ~ zone, ylim = c(0,0.000189),data = needles2[needles2$year=="2014",])
mtext(expression(italic("g")[min] ~ (m ~ s^{-1} ~ 10^{-5})),side=2, adj=0.5, line=2.5)
mtext(side=1,"Year", adj=0.5, line=2.5)
# text(0.5,0.00018,"(d) Tundra", adj = c(0,0))
all.y <- t(aggregate(needles2$gmin, by=list(x5), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:3),y=all.y[1:3]+0.00002, all.letters[6:8])
text(x=c(6:10),y=all.y[1:3]+0.00002, all.letters[6:8])








boxplot(gmin ~ year*aspect, data = ecotone)
boxplot(gmin ~ year*height, data = ecotone)



needles.trans <- decostand(needles.2016[,c(8,10:11,14)], "standardize")
gmin.pca <- rda(needles.trans, na.action = 'na.omit')
summary(pg.pca)
pg.load<-loadings.pca(pg.pca, dim = min(nrow(rda.1),ncol(rda.1)))
vpch<-c(21,22,24)
plot(x=pg.pca$scores[,1], y=pg.pca$scores[,2], xlab="", ylab="", 
     cex.lab=1.2, cex.main=1.5, xlim=c(-5,5), ylim=c(-5,5),  
     cex = 0.75)
abline(h=0,lty=3, col = "gray48")
abline(v=0,lty=3, col = "gray48")
mtext("PC2 (25.6%)", side=2, line=2, cex=1, outer=FALSE)
mtext("PC1 (74.4%)", side=1, line=2, cex=1, outer=FALSE)
pg.vec<-envfit(pg.pca$scores, rda.3, na.rm=T)
pg.vec
plot(pg.vec, add=T, col="black", cex=0.9)






# qplot(aspect, gmin, data = needles.2013, geom = c("boxplot"), aes(fill = factor(aspect)), 
#       fill = zone, main = "Horse shit", xlab = "Diarrhea", ylab = "Gmin", ylim = c(0,0.0004)) 
# + theme_bw() + facet_wrap(~aspect,scales = "free_x")
# par(cex = 0.75)
# par(mar = c(1.5, 4, 0.5, 0), oma = c(2, 2, 0.5, 2))
# boxplot(gmin~site+aspect, data=needles.2013, ylim = c(0, 0.0004), 
#         col = c("darkviolet","forestgreen","firebrick1","green","orange"))
# 
# needle.data <- transform(needles.2013, zone = reorder(zone, order(gmin, decreasing = TRUE)))
# needle.data <- transform(needles.2013, zone = reorder(zone, gmin, mean))
# par(cex = 0.75)
# par(mar = c(1.5, 4, 0.5, 0), oma = c(2, 2, 0.5, 2))
# boxplot(gmin ~ height + zone, data = needle.data, ylim = c(0, 0.0004), col = c("red", "blue", "yellow"))

#***************************#
#### Plotting snow core data

snow.2016 <- read.csv(file = "~/Desktop/Workspace/EW/SnowCores2016.csv", header = TRUE)
snow.2016$depth <- as.numeric(snow.2016$depth)
snow.2016$density <- as.numeric(snow.2016$density)
snow.2016$swe <- as.numeric(snow.2016$swe)
snow.2016$htc <- as.numeric(snow.2016$htc)
snow.2016$date <- as.factor(snow.2016$date)

# air <- subset(snow.2016, site == "AIR")
# bfr <- subset(snow.2016, site == "BFR")
# bsw <- subset(snow.2016, site == "BSW")
# bwp <- subset(snow.2016, site == "BWP")
# fen <- subset(snow.2016, site == "FEN")
# pfr <- subset(snow.2016, site == "PFR")
# PPA <- subset(snow.2016, site == "PPA")
# ppd <- subset(snow.2016, site == "PPD")
# tis <- subset(snow.2016, site == "TIS")
# tun <- subset(snow.2016, site == "TUN")
# wsu <- subset(snow.2016, site == "WSU")

## Airport

# Airport fen
airfd.aov <- aov(depth ~ date, data = snow.2016[snow.2016$pit=="AIRf",])
summary(airfd.aov)
TukeyHSD(airfd.aov)
airfd.pairs <- glht(airfd.aov, linfct = mcp(date = "Tukey"))
airfd.cld <- cld(airfd.pairs)
airfd.letters <- airfd.cld$mcletters$Letters

airfp.aov <- aov(density ~ date, data = snow.2016[snow.2016$pit=="AIRf",])
summary(airfp.aov)
TukeyHSD(airfp.aov)
airfp.pairs <- glht(airfp.aov, linfct = mcp(date = "Tukey"))
airfp.cld <- cld(airfp.pairs)
airfp.letters <- airfp.cld$mcletters$Letters

airfs.aov <- aov(swe ~ date, data = snow.2016[snow.2016$pit=="AIRf",])
summary(airfs.aov)
TukeyHSD(airfs.aov)
airf.pairs <- glht(airfs.aov, linfct = mcp(date = "Tukey"))
airfs.cld <- cld(airf.pairs)
airfs.letters <- airfs.cld$mcletters$Letters

airfh.aov <- aov(htc ~ date, data = snow.2016[snow.2016$pit=="AIRf",])
summary(airfh.aov)
TukeyHSD(airfh.aov)
airfh.pairs <- glht(airfh.aov, linfct = mcp(date = "Tukey"))
airfh.cld <- cld(airfh.pairs)
airfh.letters <- airfh.cld$mcletters$Letters

## Export at 8 x 12 for individual figure
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(1, 2, 1.75, 3), oma = c(3,2,1,1))

x1 <- factor(snow.2016[snow.2016$pit=="AIRf","date"], levels = c("2007","2008","2009","2010","2011","2012",
                                                                 "2013","2014","2015","2016"))
bp1 <- boxplot(depth ~ x1, ylim = c(20,170), data = snow.2016[snow.2016$pit=="AIRf",])
mtext(side=2,"Depth (cm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="AIRf","depth"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:10),y=all.y + 15, airfd.letters)

bp1 <- boxplot(density ~ x1, ylim = c(100,450), data = snow.2016[snow.2016$pit=="AIRf",])
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="AIRf","density"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:10),y=all.y + 25, airfp.letters)

bp1 <- boxplot(swe ~ x1, ylim = c(50,450), data = snow.2016[snow.2016$pit=="AIRf",])
mtext(side=2,"SWE (mm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="AIRf","swe"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:10),y=all.y + 35, airfs.letters)

bp1 <- boxplot(htc ~ x1, ylim = c(0,1.2), data = snow.2016[snow.2016$pit=="AIRf",])
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="AIRf","htc"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:10),y=all.y + 0.1, airfh.letters)
mtext(side=1,"Year", adj=0.5, line=2.5)

# Airport palsa
airpd.aov <- aov(depth ~ date, data = snow.2016[snow.2016$pit=="AIRp",])
summary(airpd.aov)
TukeyHSD(airpd.aov)
airpd.pairs <- glht(airpd.aov, linfct = mcp(date = "Tukey"))
airpd.cld <- cld(airpd.pairs)
airpd.letters <- airpd.cld$mcletters$Letters

airpp.aov <- aov(density ~ date, data = snow.2016[snow.2016$pit=="AIRp",])
summary(airpp.aov)
TukeyHSD(airpp.aov)
airpp.pairs <- glht(airpp.aov, linfct = mcp(date = "Tukey"))
airpp.cld <- cld(airpp.pairs)
airpp.letters <- airpp.cld$mcletters$Letters

airps.aov <- aov(swe ~ date, data = snow.2016[snow.2016$pit=="AIRp",])
summary(airps.aov)
TukeyHSD(airps.aov)
airp.pairs <- glht(airps.aov, linfct = mcp(date = "Tukey"))
airps.cld <- cld(airp.pairs)
airps.letters <- airps.cld$mcletters$Letters

airph.aov <- aov(htc ~ date, data = snow.2016[snow.2016$pit=="AIRp",])
summary(airph.aov)
TukeyHSD(airph.aov)
airph.pairs <- glht(airph.aov, linfct = mcp(date = "Tukey"))
airph.cld <- cld(airph.pairs)
airph.letters <- airph.cld$mcletters$Letters

## Export at 8 x 12 for individual figure
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(1, 2, 1.75, 3), oma = c(3,2,1,1))

x1 <- factor(snow.2016[snow.2016$pit=="AIRp","date"], levels = c("2007","2008","2009","2010","2011","2012",
                                                                 "2013","2014","2015","2016"))
bp1 <- boxplot(depth ~ x1, ylim = c(20,170), data = snow.2016[snow.2016$pit=="AIRp",])
mtext(side=2,"Depth (cm)", adj=0.5, line=2.5)
# mtext(side=1,"Year", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="AIRp","depth"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:10),y=all.y + 15, airpd.letters)

bp1 <- boxplot(density ~ x1, ylim = c(50,550), data = snow.2016[snow.2016$pit=="AIRp",])
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="AIRp","density"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:10),y=all.y + 35, airpp.letters)

bp1 <- boxplot(swe ~ x1, ylim = c(0,500), data = snow.2016[snow.2016$pit=="AIRp",])
mtext(side=2,"SWE (mm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="AIRp","swe"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:10),y=all.y + 35, airps.letters)

bp1 <- boxplot(htc ~ x1, ylim = c(0,2.3), data = snow.2016[snow.2016$pit=="AIRp",])
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="AIRp","htc"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:6),y=all.y[1:6] + 0.2, airph.letters[1:6])
text(x=10,y=all.y[10] + 0.2, airph.letters[10])
text(x = c(7:9), y = c(1.2,2.2,1), c("bc","c","ab"))
mtext(side=1,"Year", adj=0.5, line=2.5)

##########################
# Burned forest
bfrd.aov <- aov(depth ~ date, data = snow.2016[snow.2016$pit=="BFR",])
summary(bfrd.aov)
TukeyHSD(bfrd.aov)
bfrd.pairs <- glht(bfrd.aov, linfct = mcp(date = "Tukey"))
bfrd.cld <- cld(bfrd.pairs)
bfrd.letters <- bfrd.cld$mcletters$Letters

bfrp.aov <- aov(density ~ date, data = snow.2016[snow.2016$pit=="BFR",])
summary(bfrp.aov)
TukeyHSD(bfrp.aov)
bfrp.pairs <- glht(bfrp.aov, linfct = mcp(date = "Tukey"))
bfrp.cld <- cld(bfrp.pairs)
bfrp.letters <- bfrp.cld$mcletters$Letters

bfrs.aov <- aov(swe ~ date, data = snow.2016[snow.2016$pit=="BFR",])
summary(bfrs.aov)
TukeyHSD(bfrs.aov)
bfr.pairs <- glht(bfrs.aov, linfct = mcp(date = "Tukey"))
bfrs.cld <- cld(bfr.pairs)
bfrs.letters <- bfrs.cld$mcletters$Letters

bfrh.aov <- aov(htc ~ date, data = snow.2016[snow.2016$pit=="BFR",])
summary(bfrh.aov)
TukeyHSD(bfrh.aov)
bfrh.pairs <- glht(bfrh.aov, linfct = mcp(date = "Tukey"))
bfrh.cld <- cld(bfrh.pairs)
bfrh.letters <- bfrh.cld$mcletters$Letters

## Export at 8 x 12 for individual figure
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(1, 2, 1.75, 3), oma = c(3,2,1,1))

x1 <- factor(snow.2016[snow.2016$pit=="BFR","date"], levels = c("2003","2004","2005","2006","2007",
                                                                "2008","2009","2010","2011","2012",
                                                                "2013","2014","2015","2016"))
bp1 <- boxplot(depth ~ x1, ylim = c(10,120), data = snow.2016[snow.2016$pit=="BFR",])
mtext(side=2,"Depth (cm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="BFR","depth"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 15, bfrd.letters)

bp1 <- boxplot(density ~ x1, ylim = c(50,610), data = snow.2016[snow.2016$pit=="BFR",])
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="BFR","density"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 40, bfrp.letters)

bp1 <- boxplot(swe ~ x1, ylim = c(20,350), data = snow.2016[snow.2016$pit=="BFR",])
mtext(side=2,"SWE (mm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="BFR","swe"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 35, bfrs.letters)

bp1 <- boxplot(htc ~ x1, ylim = c(0,2.3), data = snow.2016[snow.2016$pit=="BFR",])
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="BFR","htc"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 0.2, bfrs.letters)
mtext(side=1,"Year", adj=0.5, line=2.5)

##########################
# Black spruce wetland
bswd.aov <- aov(depth ~ date, data = snow.2016[snow.2016$pit=="BSW",])
summary(bswd.aov)
TukeyHSD(bswd.aov)
bswd.pairs <- glht(bswd.aov, linfct = mcp(date = "Tukey"))
bswd.cld <- cld(bswd.pairs)
bswd.letters <- bswd.cld$mcletters$Letters

bswp.aov <- aov(density ~ date, data = snow.2016[snow.2016$pit=="BSW",])
summary(bswp.aov)
TukeyHSD(bswp.aov)
bswp.pairs <- glht(bswp.aov, linfct = mcp(date = "Tukey"))
bswp.cld <- cld(bswp.pairs)
bswp.letters <- bswp.cld$mcletters$Letters

bsws.aov <- aov(swe ~ date, data = snow.2016[snow.2016$pit=="BSW",])
summary(bsws.aov)
TukeyHSD(bsws.aov)
bsw.pairs <- glht(bsws.aov, linfct = mcp(date = "Tukey"))
bsws.cld <- cld(bsw.pairs)
bsws.letters <- bsws.cld$mcletters$Letters

bswh.aov <- aov(htc ~ date, data = snow.2016[snow.2016$pit=="BSW",])
summary(bswh.aov)
TukeyHSD(bswh.aov)
bswh.pairs <- glht(bswh.aov, linfct = mcp(date = "Tukey"))
bswh.cld <- cld(bswh.pairs)
bswh.letters <- bswh.cld$mcletters$Letters

## Export at 8 x 12 for individual figure
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(1, 2, 1.75, 3), oma = c(3,2,1,1))

x1 <- factor(snow.2016[snow.2016$pit=="BSW","date"], levels = c("2003","2004","2005","2006","2007",
                                                                "2008","2009","2010","2011","2012",
                                                                "2013","2014","2015","2016"))
bp1 <- boxplot(depth ~ x1, ylim = c(20,120), data = snow.2016[snow.2016$pit=="BSW",])
mtext(side=2,"Depth (cm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="BSW","depth"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 15, bswd.letters)

bp1 <- boxplot(density ~ x1, ylim = c(40,450), data = snow.2016[snow.2016$pit=="BSW",])
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="BSW","density"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 40, bswp.letters)

bp1 <- boxplot(swe ~ x1, ylim = c(10,300), data = snow.2016[snow.2016$pit=="BSW",])
mtext(side=2,"SWE (mm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="BSW","swe"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 35, bsws.letters)

bp1 <- boxplot(htc ~ x1, ylim = c(0,1.1), data = snow.2016[snow.2016$pit=="BSW",])
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="BSW","htc"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 0.2, bsws.letters)
mtext(side=1,"Year", adj=0.5, line=2.5)


##########################
# Borrow pit
bwpd.aov <- aov(depth ~ date, data = snow.2016[snow.2016$pit=="BWP",])
summary(bwpd.aov)
TukeyHSD(bwpd.aov)
bwpd.pairs <- glht(bwpd.aov, linfct = mcp(date = "Tukey"))
bwpd.cld <- cld(bwpd.pairs)
bwpd.letters <- bwpd.cld$mcletters$Letters

bwpp.aov <- aov(density ~ date, data = snow.2016[snow.2016$pit=="BWP",])
summary(bwpp.aov)
TukeyHSD(bwpp.aov)
bwpp.pairs <- glht(bwpp.aov, linfct = mcp(date = "Tukey"))
bwpp.cld <- cld(bwpp.pairs)
bwpp.letters <- bwpp.cld$mcletters$Letters

bwps.aov <- aov(swe ~ date, data = snow.2016[snow.2016$pit=="BWP",])
summary(bwps.aov)
TukeyHSD(bwps.aov)
bwp.pairs <- glht(bwps.aov, linfct = mcp(date = "Tukey"))
bwps.cld <- cld(bwp.pairs)
bwps.letters <- bwps.cld$mcletters$Letters

bwph.aov <- aov(htc ~ date, data = snow.2016[snow.2016$pit=="BWP",])
summary(bwph.aov)
TukeyHSD(bwph.aov)
bwph.pairs <- glht(bwph.aov, linfct = mcp(date = "Tukey"))
bwph.cld <- cld(bwph.pairs)
bwph.letters <- bwph.cld$mcletters$Letters

## Export at 8 x 12 for individual figure
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(1, 2, 1.75, 3), oma = c(3,2,1,1))

x1 <- factor(snow.2016[snow.2016$pit=="BWP","date"], levels = c("2003","2004","2005","2006","2007",
                                                                "2008","2009","2010","2011","2012",
                                                                "2013","2014","2015","2016"))
bp1 <- boxplot(depth ~ x1, ylim = c(0,100), data = snow.2016[snow.2016$pit=="BWP",])
mtext(side=2,"Depth (cm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="BWP","depth"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 15, bwpd.letters)

bp1 <- boxplot(density ~ x1, ylim = c(20,550), data = snow.2016[snow.2016$pit=="BWP",])
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="BWP","density"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 40, bwpp.letters)

bp1 <- boxplot(swe ~ x1, ylim = c(0,230), data = snow.2016[snow.2016$pit=="BWP",])
mtext(side=2,"SWE (mm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="BWP","swe"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 35, bwps.letters)

bp1 <- boxplot(htc ~ x1, ylim = c(0,25), data = snow.2016[snow.2016$pit=="BWP",])
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="BWP","htc"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 2, bwps.letters)
mtext(side=1,"Year", adj=0.5, line=2.5)

##########################
# Fen
fend.aov <- aov(depth ~ date, data = snow.2016[snow.2016$pit=="FEN",])
summary(fend.aov)
TukeyHSD(fend.aov)
fend.pairs <- glht(fend.aov, linfct = mcp(date = "Tukey"))
fend.cld <- cld(fend.pairs)
fend.letters <- fend.cld$mcletters$Letters

fenp.aov <- aov(density ~ date, data = snow.2016[snow.2016$pit=="FEN",])
summary(fenp.aov)
TukeyHSD(fenp.aov)
fenp.pairs <- glht(fenp.aov, linfct = mcp(date = "Tukey"))
fenp.cld <- cld(fenp.pairs)
fenp.letters <- fenp.cld$mcletters$Letters

fens.aov <- aov(swe ~ date, data = snow.2016[snow.2016$pit=="FEN",])
summary(fens.aov)
TukeyHSD(fens.aov)
fen.pairs <- glht(fens.aov, linfct = mcp(date = "Tukey"))
fens.cld <- cld(fen.pairs)
fens.letters <- fens.cld$mcletters$Letters

fenh.aov <- aov(htc ~ date, data = snow.2016[snow.2016$pit=="FEN",])
summary(fenh.aov)
TukeyHSD(fenh.aov)
fenh.pairs <- glht(fenh.aov, linfct = mcp(date = "Tukey"))
fenh.cld <- cld(fenh.pairs)
fenh.letters <- fenh.cld$mcletters$Letters

## Export at 8 x 12 for individual figure
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(1, 2, 1.75, 3), oma = c(3,2,1,1))

x1 <- factor(snow.2016[snow.2016$pit=="FEN","date"], levels = c("2005","2006","2007",
                                                                "2008","2009","2010","2011","2012",
                                                                "2013","2014","2015","2016"))
bp1 <- boxplot(depth ~ x1, ylim = c(0,70), data = snow.2016[snow.2016$pit=="FEN",])
mtext(side=2,"Depth (cm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="FEN","depth"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:12),y=all.y + 10, fend.letters)

bp1 <- boxplot(density ~ x1, ylim = c(20,800), data = snow.2016[snow.2016$pit=="FEN",])
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="FEN","density"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:12),y=all.y + 60, fenp.letters)

bp1 <- boxplot(swe ~ x1, ylim = c(0,220), data = snow.2016[snow.2016$pit=="FEN",])
mtext(side=2,"SWE (mm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="FEN","swe"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:12),y=all.y + 35, fens.letters)

bp1 <- boxplot(htc ~ x1, ylim = c(0,25), data = snow.2016[snow.2016$pit=="FEN",])
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="FEN","htc"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:12),y=all.y + 2, fens.letters)
mtext(side=1,"Year", adj=0.5, line=2.5)

##########################
# Planted forest
pfrd.aov <- aov(depth ~ date, data = snow.2016[snow.2016$pit=="PFR",])
summary(pfrd.aov)
TukeyHSD(pfrd.aov)
pfrd.pairs <- glht(pfrd.aov, linfct = mcp(date = "Tukey"))
pfrd.cld <- cld(pfrd.pairs)
pfrd.letters <- pfrd.cld$mcletters$Letters

pfrp.aov <- aov(density ~ date, data = snow.2016[snow.2016$pit=="PFR",])
summary(pfrp.aov)
TukeyHSD(pfrp.aov)
pfrp.pairs <- glht(pfrp.aov, linfct = mcp(date = "Tukey"))
pfrp.cld <- cld(pfrp.pairs)
pfrp.letters <- pfrp.cld$mcletters$Letters

pfrs.aov <- aov(swe ~ date, data = snow.2016[snow.2016$pit=="PFR",])
summary(pfrs.aov)
TukeyHSD(pfrs.aov)
pfr.pairs <- glht(pfrs.aov, linfct = mcp(date = "Tukey"))
pfrs.cld <- cld(pfr.pairs)
pfrs.letters <- pfrs.cld$mcletters$Letters

pfrh.aov <- aov(htc ~ date, data = snow.2016[snow.2016$pit=="PFR",])
summary(pfrh.aov)
TukeyHSD(pfrh.aov)
pfrh.pairs <- glht(pfrh.aov, linfct = mcp(date = "Tukey"))
pfrh.cld <- cld(pfrh.pairs)
pfrh.letters <- pfrh.cld$mcletters$Letters

## Export at 8 x 12 for individual figure
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(1, 2, 1.75, 3), oma = c(3,2,1,1))

x1 <- factor(snow.2016[snow.2016$pit=="PFR","date"], levels = c("2003","2004","2005","2006","2007",
                                                                "2008","2009","2010","2011","2012",
                                                                "2013","2014","2015","2016"))
bp1 <- boxplot(depth ~ x1, ylim = c(10,140), data = snow.2016[snow.2016$pit=="PFR",])
mtext(side=2,"Depth (cm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="PFR","depth"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:10),y=all.y[1:10] + 15, pfrd.letters[1:10])
text(x=c(12:14),y=all.y[11:13] + 15, pfrd.letters[11:13])

bp1 <- boxplot(density ~ x1, ylim = c(20,500), data = snow.2016[snow.2016$pit=="PFR",])
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="PFR","density"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:10),y=all.y[1:10] + 50, pfrp.letters[1:10])
text(x=c(12:14),y=all.y[11:13] + 50, pfrp.letters[11:13])

bp1 <- boxplot(swe ~ x1, ylim = c(0,410), data = snow.2016[snow.2016$pit=="PFR",])
mtext(side=2,"SWE (mm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="PFR","swe"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:10),y=all.y[1:10] + 25, pfrs.letters[1:10])
text(x=c(12:14),y=all.y[11:13] + 25, pfrs.letters[11:13])

bp1 <- boxplot(htc ~ x1, ylim = c(0,4), data = snow.2016[snow.2016$pit=="PFR",])
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="PFR","htc"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:10),y=all.y[1:10] + 0.4, pfrh.letters[1:10])
text(x=c(12:14),y=all.y[11:13] + 0.4, pfrh.letters[11:13])
mtext(side=1,"Year", adj=0.5, line=2.5)

##########################
# Polygonal peat plateau aggrading - centres
ppacd.aov <- aov(depth ~ date, data = snow.2016[snow.2016$pit=="PPAc",])
summary(ppacd.aov)
TukeyHSD(ppacd.aov)
ppacd.pairs <- glht(ppacd.aov, linfct = mcp(date = "Tukey"))
ppacd.cld <- cld(ppacd.pairs)
ppacd.letters <- ppacd.cld$mcletters$Letters

ppacp.aov <- aov(density ~ date, data = snow.2016[snow.2016$pit=="PPAc",])
summary(ppacp.aov)
TukeyHSD(ppacp.aov)
ppacp.pairs <- glht(ppacp.aov, linfct = mcp(date = "Tukey"))
ppacp.cld <- cld(ppacp.pairs)
ppacp.letters <- ppacp.cld$mcletters$Letters

ppacs.aov <- aov(swe ~ date, data = snow.2016[snow.2016$pit=="PPAc",])
summary(ppacs.aov)
TukeyHSD(ppacs.aov)
ppac.pairs <- glht(ppacs.aov, linfct = mcp(date = "Tukey"))
ppacs.cld <- cld(ppac.pairs)
ppacs.letters <- ppacs.cld$mcletters$Letters

ppach.aov <- aov(htc ~ date, data = snow.2016[snow.2016$pit=="PPAc",])
summary(ppach.aov)
TukeyHSD(ppach.aov)
ppach.pairs <- glht(ppach.aov, linfct = mcp(date = "Tukey"))
ppach.cld <- cld(ppach.pairs)
ppach.letters <- ppach.cld$mcletters$Letters

## Export at 8 x 12 for individual figure
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(1, 2, 1.75, 3), oma = c(3,2,1,1))

x1 <- factor(snow.2016[snow.2016$pit=="PPAc","date"], levels = c("2003","2004","2005","2006","2007",
                                                                 "2008","2009","2010","2011","2012",
                                                                 "2013","2014","2015","2016"))
bp1 <- boxplot(depth ~ x1, ylim = c(0,45), data = snow.2016[snow.2016$pit=="PPAc",])
mtext(side=2,"Depth (cm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="PPAc","depth"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 5, ppacd.letters)

bp1 <- boxplot(density ~ x1, ylim = c(20,650), data = snow.2016[snow.2016$pit=="PPAc",])
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="PPAc","density"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 80, ppacp.letters)

bp1 <- boxplot(swe ~ x1, ylim = c(0,120), data = snow.2016[snow.2016$pit=="PPAc",])
mtext(side=2,"SWE (mm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="PPAc","swe"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 10, ppacs.letters)

bp1 <- boxplot(htc ~ x1, ylim = c(0,130), data = snow.2016[snow.2016$pit=="PPAc",])
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="PPAc","htc"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 10, ppach.letters)
mtext(side=1,"Year", adj=0.5, line=2.5)

##########################
# Polygonal peat plateau aggrading - centres
ppawd.aov <- aov(depth ~ date, data = snow.2016[snow.2016$pit=="PPAw",])
summary(ppawd.aov)
TukeyHSD(ppawd.aov)
ppawd.pairs <- glht(ppawd.aov, linfct = mcp(date = "Tukey"))
ppawd.cld <- cld(ppawd.pairs)
ppawd.letters <- ppawd.cld$mcletters$Letters

ppawp.aov <- aov(density ~ date, data = snow.2016[snow.2016$pit=="PPAw",])
summary(ppawp.aov)
TukeyHSD(ppawp.aov)
ppawp.pairs <- glht(ppawp.aov, linfct = mcp(date = "Tukey"))
ppawp.cld <- cld(ppawp.pairs)
ppawp.letters <- ppawp.cld$mcletters$Letters

ppaws.aov <- aov(swe ~ date, data = snow.2016[snow.2016$pit=="PPAw",])
summary(ppaws.aov)
TukeyHSD(ppaws.aov)
ppaw.pairs <- glht(ppaws.aov, linfct = mcp(date = "Tukey"))
ppaws.cld <- cld(ppaw.pairs)
ppaws.letters <- ppaws.cld$mcletters$Letters

ppawh.aov <- aov(htc ~ date, data = snow.2016[snow.2016$pit=="PPAw",])
summary(ppawh.aov)
TukeyHSD(ppawh.aov)
ppawh.pairs <- glht(ppawh.aov, linfct = mcp(date = "Tukey"))
ppawh.cld <- cld(ppawh.pairs)
ppawh.letters <- ppawh.cld$mcletters$Letters

## Export at 8 x 12 for individual figure
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(1, 2, 1.75, 3), oma = c(3,2,1,1))

x1 <- factor(snow.2016[snow.2016$pit=="PPAw","date"], levels = c("2003","2004","2005","2006","2007",
                                                                 "2008","2009","2010","2011","2012",
                                                                 "2013","2014","2015","2016"))
bp1 <- boxplot(depth ~ x1, ylim = c(0,70), data = snow.2016[snow.2016$pit=="PPAw",])
mtext(side=2,"Depth (cm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="PPAw","depth"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 5, ppawd.letters)

bp1 <- boxplot(density ~ x1, ylim = c(20,600), data = snow.2016[snow.2016$pit=="PPAw",])
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="PPAw","density"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 80, ppawp.letters)

bp1 <- boxplot(swe ~ x1, ylim = c(0,200), data = snow.2016[snow.2016$pit=="PPAw",])
mtext(side=2,"SWE (mm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="PPAw","swe"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 20, ppaws.letters)

bp1 <- boxplot(htc ~ x1, ylim = c(0,15), data = snow.2016[snow.2016$pit=="PPAw",])
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="PPAw","htc"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 2, ppawh.letters)
mtext(side=1,"Year", adj=0.5, line=2.5)

##########################
# Polygonal peat plateau degrading - centres
ppdcd.aov <- aov(depth ~ date, data = snow.2016[snow.2016$pit=="PPDc",])
summary(ppdcd.aov)
TukeyHSD(ppdcd.aov)
ppdcd.pairs <- glht(ppdcd.aov, linfct = mcp(date = "Tukey"))
ppdcd.cld <- cld(ppdcd.pairs)
ppdcd.letters <- ppdcd.cld$mcletters$Letters

ppdcp.aov <- aov(density ~ date, data = snow.2016[snow.2016$pit=="PPDc",])
summary(ppdcp.aov)
TukeyHSD(ppdcp.aov)
ppdcp.pairs <- glht(ppdcp.aov, linfct = mcp(date = "Tukey"))
ppdcp.cld <- cld(ppdcp.pairs)
ppdcp.letters <- ppdcp.cld$mcletters$Letters

ppdcs.aov <- aov(swe ~ date, data = snow.2016[snow.2016$pit=="PPDc",])
summary(ppdcs.aov)
TukeyHSD(ppdcs.aov)
ppdc.pairs <- glht(ppdcs.aov, linfct = mcp(date = "Tukey"))
ppdcs.cld <- cld(ppdc.pairs)
ppdcs.letters <- ppdcs.cld$mcletters$Letters

ppdch.aov <- aov(htc ~ date, data = snow.2016[snow.2016$pit=="PPDc",])
summary(ppdch.aov)
TukeyHSD(ppdch.aov)
ppdch.pairs <- glht(ppdch.aov, linfct = mcp(date = "Tukey"))
ppdch.cld <- cld(ppdch.pairs)
ppdch.letters <- ppdch.cld$mcletters$Letters

## Export at 8 x 12 for individual figure
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(1, 2, 1.75, 3), oma = c(3,2,1,1))

x1 <- factor(snow.2016[snow.2016$pit=="PPDc","date"], levels = c("2003","2004","2005","2006","2007",
                                                                 "2008","2009","2010","2011","2012",
                                                                 "2013","2014","2015","2016"))
bp1 <- boxplot(depth ~ x1, ylim = c(0,45), data = snow.2016[snow.2016$pit=="PPDc",])
mtext(side=2,"Depth (cm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="PPDc","depth"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 5, ppdcd.letters)

bp1 <- boxplot(density ~ x1, ylim = c(20,650), data = snow.2016[snow.2016$pit=="PPDc",])
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="PPDc","density"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 80, ppdcp.letters)

bp1 <- boxplot(swe ~ x1, ylim = c(0,120), data = snow.2016[snow.2016$pit=="PPDc",])
mtext(side=2,"SWE (mm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="PPDc","swe"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 10, ppdcs.letters)

bp1 <- boxplot(htc ~ x1, ylim = c(0,130), data = snow.2016[snow.2016$pit=="PPDc",])
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="PPDc","htc"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 10, ppdch.letters)
mtext(side=1,"Year", adj=0.5, line=2.5)

##########################
# Polygonal peat plateau degrading - wedges
ppdwd.aov <- aov(depth ~ date, data = snow.2016[snow.2016$pit=="PPDw",])
summary(ppdwd.aov)
TukeyHSD(ppdwd.aov)
ppdwd.pairs <- glht(ppdwd.aov, linfct = mcp(date = "Tukey"))
ppdwd.cld <- cld(ppdwd.pairs)
ppdwd.letters <- ppdwd.cld$mcletters$Letters

ppdwp.aov <- aov(density ~ date, data = snow.2016[snow.2016$pit=="PPDw",])
summary(ppdwp.aov)
TukeyHSD(ppdwp.aov)
ppdwp.pairs <- glht(ppdwp.aov, linfct = mcp(date = "Tukey"))
ppdwp.cld <- cld(ppdwp.pairs)
ppdwp.letters <- ppdwp.cld$mcletters$Letters

ppdws.aov <- aov(swe ~ date, data = snow.2016[snow.2016$pit=="PPDw",])
summary(ppdws.aov)
TukeyHSD(ppdws.aov)
ppdw.pairs <- glht(ppdws.aov, linfct = mcp(date = "Tukey"))
ppdws.cld <- cld(ppdw.pairs)
ppdws.letters <- ppdws.cld$mcletters$Letters

ppdwh.aov <- aov(htc ~ date, data = snow.2016[snow.2016$pit=="PPDw",])
summary(ppdwh.aov)
TukeyHSD(ppdwh.aov)
ppdwh.pairs <- glht(ppdwh.aov, linfct = mcp(date = "Tukey"))
ppdwh.cld <- cld(ppdwh.pairs)
ppdwh.letters <- ppdwh.cld$mcletters$Letters

## Export at 8 x 12 for individual figure
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(1, 2, 1.75, 3), oma = c(3,2,1,1))

x1 <- factor(snow.2016[snow.2016$pit=="PPDw","date"], levels = c("2003","2004","2005","2006","2007",
                                                                 "2008","2009","2010","2011","2012",
                                                                 "2013","2014","2015","2016"))
bp1 <- boxplot(depth ~ x1, ylim = c(0,120), data = snow.2016[snow.2016$pit=="PPDw",])
mtext(side=2,"Depth (cm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="PPDw","depth"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 8, ppdwd.letters)

bp1 <- boxplot(density ~ x1, ylim = c(20,600), data = snow.2016[snow.2016$pit=="PPDw",])
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="PPDw","density"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 80, ppdwp.letters)

bp1 <- boxplot(swe ~ x1, ylim = c(0,400), data = snow.2016[snow.2016$pit=="PPDw",])
mtext(side=2,"SWE (mm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="PPDw","swe"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 30, ppdws.letters)

bp1 <- boxplot(htc ~ x1, ylim = c(0,13), data = snow.2016[snow.2016$pit=="PPDw",])
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="PPDw","htc"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 2, ppdwh.letters)
mtext(side=1,"Year", adj=0.5, line=2.5)

##########################
# Tree island
tisd.aov <- aov(depth ~ date, data = snow.2016[snow.2016$pit=="TIS",])
summary(tisd.aov)
TukeyHSD(tisd.aov)
tisd.pairs <- glht(tisd.aov, linfct = mcp(date = "Tukey"))
tisd.cld <- cld(tisd.pairs)
tisd.letters <- tisd.cld$mcletters$Letters

tisp.aov <- aov(density ~ date, data = snow.2016[snow.2016$pit=="TIS",])
summary(tisp.aov)
TukeyHSD(tisp.aov)
tisp.pairs <- glht(tisp.aov, linfct = mcp(date = "Tukey"))
tisp.cld <- cld(tisp.pairs)
tisp.letters <- tisp.cld$mcletters$Letters

tiss.aov <- aov(swe ~ date, data = snow.2016[snow.2016$pit=="TIS",])
summary(tiss.aov)
TukeyHSD(tiss.aov)
tis.pairs <- glht(tiss.aov, linfct = mcp(date = "Tukey"))
tiss.cld <- cld(tis.pairs)
tiss.letters <- tiss.cld$mcletters$Letters

tish.aov <- aov(htc ~ date, data = snow.2016[snow.2016$pit=="TIS",])
summary(tish.aov)
TukeyHSD(tish.aov)
tish.pairs <- glht(tish.aov, linfct = mcp(date = "Tukey"))
tish.cld <- cld(tish.pairs)
tish.letters <- tish.cld$mcletters$Letters

## Export at 8 x 12 for individual figure
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(1, 2, 1.75, 3), oma = c(3,2,1,1))

x1 <- factor(snow.2016[snow.2016$pit=="TIS","date"], levels = c("2004","2005","2006","2007",
                                                                "2008","2009","2010","2011","2012",
                                                                "2013","2014","2015","2016"))
bp1 <- boxplot(depth ~ x1, ylim = c(0,300), data = snow.2016[snow.2016$pit=="TIS",])
mtext(side=2,"Depth (cm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="TIS","depth"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:13),y=all.y + 20, tisd.letters)

bp1 <- boxplot(density ~ x1, ylim = c(20,580), data = snow.2016[snow.2016$pit=="TIS",])
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="TIS","density"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:13),y=all.y + 60, tisp.letters)

bp1 <- boxplot(swe ~ x1, ylim = c(0,1400), data = snow.2016[snow.2016$pit=="TIS",])
mtext(side=2,"SWE (mm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="TIS","swe"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:13),y=all.y + 80, tiss.letters)

bp1 <- boxplot(htc ~ x1, ylim = c(0,2), data = snow.2016[snow.2016$pit=="TIS",])
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="TIS","htc"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:13),y=all.y + 0.2, tish.letters)
text(x = 13,y = 1.7,"b")
mtext(side=1,"Year", adj=0.5, line=2.5)

##########################
# Tundra
tund.aov <- aov(depth ~ date, data = snow.2016[snow.2016$pit=="TUN",])
summary(tund.aov)
TukeyHSD(tund.aov)
tund.pairs <- glht(tund.aov, linfct = mcp(date = "Tukey"))
tund.cld <- cld(tund.pairs)
tund.letters <- tund.cld$mcletters$Letters

tunp.aov <- aov(density ~ date, data = snow.2016[snow.2016$pit=="TUN",])
summary(tunp.aov)
TukeyHSD(tunp.aov)
tunp.pairs <- glht(tunp.aov, linfct = mcp(date = "Tukey"))
tunp.cld <- cld(tunp.pairs)
tunp.letters <- tunp.cld$mcletters$Letters

tuns.aov <- aov(swe ~ date, data = snow.2016[snow.2016$pit=="TUN",])
summary(tuns.aov)
TukeyHSD(tuns.aov)
tun.pairs <- glht(tuns.aov, linfct = mcp(date = "Tukey"))
tuns.cld <- cld(tun.pairs)
tuns.letters <- tuns.cld$mcletters$Letters

tunh.aov <- aov(htc ~ date, data = snow.2016[snow.2016$pit=="TUN",])
summary(tunh.aov)
TukeyHSD(tunh.aov)
tunh.pairs <- glht(tunh.aov, linfct = mcp(date = "Tukey"))
tunh.cld <- cld(tunh.pairs)
tunh.letters <- tunh.cld$mcletters$Letters

## Export at 8 x 12 for individual figure
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(1, 2, 1.75, 3), oma = c(3,2,1,1))

x1 <- factor(snow.2016[snow.2016$pit=="TUN","date"], levels = c("2003","2004","2005","2006","2007",
                                                                "2008","2009","2010","2011","2012",
                                                                "2013","2014","2015","2016"))
bp1 <- boxplot(depth ~ x1, ylim = c(0,50), data = snow.2016[snow.2016$pit=="TUN",])
mtext(side=2,"Depth (cm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="TUN","depth"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 5, tund.letters)

bp1 <- boxplot(density ~ x1, ylim = c(20,580), data = snow.2016[snow.2016$pit=="TUN",])
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="TUN","density"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 60, tunp.letters)

bp1 <- boxplot(swe ~ x1, ylim = c(0,150), data = snow.2016[snow.2016$pit=="TUN",])
mtext(side=2,"SWE (mm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="TUN","swe"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 15, tuns.letters)

bp1 <- boxplot(htc ~ x1, ylim = c(0,80), data = snow.2016[snow.2016$pit=="TUN",])
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="TUN","htc"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 10, tunh.letters)
mtext(side=1,"Year", adj=0.5, line=2.5)

##########################
# White spruce upland
wsud.aov <- aov(depth ~ date, data = snow.2016[snow.2016$pit=="WSU",])
summary(wsud.aov)
TukeyHSD(wsud.aov)
wsud.pairs <- glht(wsud.aov, linfct = mcp(date = "Tukey"))
wsud.cld <- cld(wsud.pairs)
wsud.letters <- wsud.cld$mcletters$Letters

wsup.aov <- aov(density ~ date, data = snow.2016[snow.2016$pit=="WSU",])
summary(wsup.aov)
TukeyHSD(wsup.aov)
wsup.pairs <- glht(wsup.aov, linfct = mcp(date = "Tukey"))
wsup.cld <- cld(wsup.pairs)
wsup.letters <- wsup.cld$mcletters$Letters

wsus.aov <- aov(swe ~ date, data = snow.2016[snow.2016$pit=="WSU",])
summary(wsus.aov)
TukeyHSD(wsus.aov)
wsu.pairs <- glht(wsus.aov, linfct = mcp(date = "Tukey"))
wsus.cld <- cld(wsu.pairs)
wsus.letters <- wsus.cld$mcletters$Letters

wsuh.aov <- aov(htc ~ date, data = snow.2016[snow.2016$pit=="WSU",])
summary(wsuh.aov)
TukeyHSD(wsuh.aov)
wsuh.pairs <- glht(wsuh.aov, linfct = mcp(date = "Tukey"))
wsuh.cld <- cld(wsuh.pairs)
wsuh.letters <- wsuh.cld$mcletters$Letters

## Export at 8 x 12 for individual figure
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(1, 2, 1.75, 3), oma = c(3,2,1,1))

x1 <- factor(snow.2016[snow.2016$pit=="WSU","date"], levels = c("2003","2004","2005","2006","2007",
                                                                "2008","2009","2010","2011","2012",
                                                                "2013","2014","2015","2016"))
bp1 <- boxplot(depth ~ x1, ylim = c(20,100), data = snow.2016[snow.2016$pit=="WSU",])
mtext(side=2,"Depth (cm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="WSU","depth"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 5, wsud.letters)

bp1 <- boxplot(density ~ x1, ylim = c(20,500), data = snow.2016[snow.2016$pit=="WSU",])
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="WSU","density"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 60, wsup.letters)

bp1 <- boxplot(swe ~ x1, ylim = c(0,300), data = snow.2016[snow.2016$pit=="WSU",])
mtext(side=2,"SWE (mm)", adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="WSU","swe"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 20, wsus.letters)

bp1 <- boxplot(htc ~ x1, ylim = c(0,2), data = snow.2016[snow.2016$pit=="WSU",])
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
all.y <- t(aggregate(snow.2016[snow.2016$pit=="WSU","htc"], by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:14),y=all.y + 0.2, wsuh.letters)
mtext(side=1,"Year", adj=0.5, line=2.5)



###########################################################################
###########################################################################
###########################################################################


############################

ch.snow <- read.csv(file = "~/Desktop/Workspace/Earthwatch/ch.snow.csv", header = TRUE)
levels(ch.snow[ch.snow$type == "island",]$plotting)
ch.snow$type


# M	baydjarakh	        17
# L	plateau	                9
# K	tundra	                18
# J	beach	                18
# I	disturbance	        14
# H	wedge	                18
# G	fen	                16
# F	forest	                18
# E	palsa	                18
# D	burn	                14
# C	baydjarakh wedge	18
# B	shrub                   18
# A	island	                18
num.plot <- c(17,9,18,18,14,18,16,18,18,14,18,18,18)
ch.snow <- ch.snow[order(ch.snow$plotting),]

## Plot the snow data and color according to veg cover
# Export at 14 x 8
par(ps = 14, cex = 1, cex.axis = 1)
par(mfrow = c(2, 1))
par(mar = c(0, 2, 1, 1), oma = c(4,2,1,1))

ch.snow$letter <- substr(as.character(ch.snow$plotting),1,1)

colors <- c(rep('darkgreen',num.plot[1]),rep('forestgreen',num.plot[2]),rep('green3',num.plot[3]),rep('green',num.plot[4]),
                    rep('greenyellow',num.plot[5]),rep('lightgreen',num.plot[6]),rep('olivedrab1',num.plot[7]),rep('yellow',num.plot[8]),
                    rep('orange',num.plot[9]),rep('darkorange3',num.plot[10]),rep('red',num.plot[11]),rep('deeppink4',num.plot[12]),
                    rep('darkred',num.plot[13]))

col.df <- cbind.data.frame(letter = LETTERS[1:13],
                           col = c('darkgreen','forestgreen','green3','green','greenyellow',
                                   'lightgreen','olivedrab1','yellow','orange','darkorange3',
                                   'red','deeppink4','darkred'))
col.df$col <- as.character(col.df$col)

snow.df <- merge(ch.snow, col.df, by = "letter")
snow.df$col <- as.character(snow.df$col)


bp1 <- boxplot(swe ~ plotting, data = snow.df, outline=FALSE, ylim = c(0,1200), col = colors, xaxt = "n")
bp1$col <- rep(NA,827)
tmp.group <- cbind.data.frame(group = bp1$group, col = rep(NA, 827))
tmp.group[tmp.group$group %in% as.numeric(as.character(levels(as.factor(bp1$group))[1:num.plot[1]])),"col"] <- col.df[1,2]
tmp.group[tmp.group$group %in% as.numeric(as.character(levels(as.factor(bp1$group))[(1+num.plot[1]):(sum(num.plot[1:2]))])),"col"] <- col.df[2,2]
tmp.group[tmp.group$group %in% as.numeric(as.character(levels(as.factor(bp1$group))[(1+sum(num.plot[1:2])):(sum(num.plot[1:3]))])),"col"] <- col.df[3,2]
tmp.group[tmp.group$group %in% as.numeric(as.character(levels(as.factor(bp1$group))[(1+sum(num.plot[1:3])):(sum(num.plot[1:4]))])),"col"] <- col.df[4,2]
tmp.group[tmp.group$group %in% as.numeric(as.character(levels(as.factor(bp1$group))[(1+sum(num.plot[1:4])):(sum(num.plot[1:5]))])),"col"] <- col.df[5,2]
tmp.group[tmp.group$group %in% as.numeric(as.character(levels(as.factor(bp1$group))[(1+sum(num.plot[1:5])):(sum(num.plot[1:6]))])),"col"] <- col.df[6,2]
tmp.group[tmp.group$group %in% as.numeric(as.character(levels(as.factor(bp1$group))[(1+sum(num.plot[1:6])):(sum(num.plot[1:7]))])),"col"] <- col.df[7,2]
tmp.group[tmp.group$group %in% as.numeric(as.character(levels(as.factor(bp1$group))[(1+sum(num.plot[1:7])):(sum(num.plot[1:8]))])),"col"] <- col.df[8,2]
tmp.group[tmp.group$group %in% as.numeric(as.character(levels(as.factor(bp1$group))[(1+sum(num.plot[1:8])):(sum(num.plot[1:9]))])),"col"] <- col.df[9,2]
tmp.group[tmp.group$group %in% as.numeric(as.character(levels(as.factor(bp1$group))[(1+sum(num.plot[1:9])):(sum(num.plot[1:10]))])),"col"] <- col.df[10,2]
tmp.group[tmp.group$group %in% as.numeric(as.character(levels(as.factor(bp1$group))[(1+sum(num.plot[1:10])):(sum(num.plot[1:11]))])),"col"] <- col.df[11,2]
tmp.group[tmp.group$group %in% as.numeric(as.character(levels(as.factor(bp1$group))[(1+sum(num.plot[1:11])):(sum(num.plot[1:12]))])),"col"] <- col.df[12,2]
tmp.group[tmp.group$group %in% as.numeric(as.character(levels(as.factor(bp1$group))[(1+sum(num.plot[1:12])):(sum(num.plot[1:13]))])),"col"] <- col.df[13,2]

length()


points(bp1$group, bp1$out, type = "p", pch=16, cex = 0.5, col = colors)
axis(1, at = seq(1,nrow(ch.snow),1), labels = NA, tick = TRUE)
mtext(side=2, "SWE (mm) ",adj=0.5, line=2.5)

bp2 <- boxplot(log(htc) ~ plotting, data = ch.snow, outline=FALSE, ylim = c(-5,8), col = colors, xaxt = "n")
points(bp2$group, bp2$out, type = "p", pch=1, cex = 0.75)
axis(1, at = 1:171, labels = NA, tick = TRUE)
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2.5)

