library(scales) # Add transparency to colors in plotting
library(tidyverse)
library(broom)

rm(list = ls())

## Calculate means, sd, n, se, and 95% CIs. 
## Then subset the data by site and plot means versus year with 95% CI

# Constants ----
year <- 2024

# Read in the data ----
snow <- read_csv("~/Desktop/Workspace/Earthwatch/Churchill/data/ltems_snow.csv")

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
df %>%
  nest(data = -site) %>%
  mutate(fit = map(data, ~ lm(depth_mean ~ year, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied) %>%
  filter(term == "year", p.value <= 0.05)
# SIG: BWP

# Density
df %>%
  nest(data = -site) %>%
  mutate(fit = map(data, ~ lm(density_mean ~ year, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied) %>%
  filter(term == "year", p.value <= 0.05)
# SIG: AIR, BFR, BWP, TUN

# SWE
df %>%
  nest(data = -site) %>%
  mutate(fit = map(data, ~ lm(swe_mean ~ year, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied) %>%
  filter(term == "year", p.value <= 0.05)
# SIG: AIR, BWP

# HTC
df %>%
  nest(data = -site) %>%
  mutate(fit = map(data, ~ lm(htc_mean ~ year, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied) %>%
  filter(term == "year", p.value <= 0.05)
# SIG: BWP, PFR, PPA, TIS

#_____________________________----

# Figures ----

## Depth ----
### Export at 5 x 9
jpeg(sprintf("~/Desktop/Workspace/Earthwatch/Churchill/figures/snow_depth_%s.jpg", year),
     height = 9, width = 5, units = "in", res = 150)
par(mfrow = c(4,1), mar = c(1,1,1,1), oma = c(3,3,0,0))
## TIS
plot(tis$year, tis$depth_mean, type='n', xlim = c(2000,year), ylim = c(40,200), axes = F, xlab = "", ylab = "")
polygon(c(tis$year, rev(tis$year)), c(tis$depth_c95, rev(tis$depth_c5)), col = alpha("lightgreen",0.5), border = NA)
lines(tis$year, tis$depth_mean, col = alpha("green4",0.6), lwd = 2)
# abline(coef(tis_depth), col = alpha("green4",0.5), lty = 3, lwd = 2)
box()
axis(side = 1, at = seq(2000,year,2), labels = NA)
axis(side = 2, at = seq(0,200,50))
legend("bottomleft", "TIS", fill = "lightgreen", border = NA, bty = "n", inset = c(0,0))

## Treed areas
plot(air$year, air$depth_mean, type="n", xlim = c(2000,year), ylim = c(40,100), 
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
axis(side = 1, at = seq(2000,year,2), labels = NA)
axis(side = 2, at = seq(0,100,25))
legend("topleft", c("AIR","BSW","WSU"), 
       fill = c("lightgreen","cadetblue1","darksalmon"), 
       border = NA, bty = "n", inset = c(0,0), horiz = F)

## Disturbed areas
plot(bwp$year, bwp$depth_mean, type="n", xlim = c(2000,year), ylim = c(0,100), 
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
axis(side = 1, at = seq(2000,year,2), labels = NA)
axis(side = 2, at = seq(0,200,50))
legend("topleft", c("BWP","BFR","PFR"), 
       fill = c("lightgreen","darksalmon","cadetblue1"), 
       border = NA, bty = "n", inset = c(0,0), horiz = F)

## Exposed areas
plot(tun$year, tun$depth_mean, type="n", xlim = c(2000,year), ylim = c(0,50), 
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
axis(side = 1, at = seq(2000,year,2), labels = seq(2000,year,2))
axis(side = 2, at = seq(0,50,10))
legend("topleft", c("TUN","FEN","PPA","PPD"), 
       fill = c("lightgreen","cadetblue1","darksalmon","mediumorchid1"), 
       border = NA, bty = "n", inset = c(0,0), horiz = F)
mtext(side = 1, "Year", line = 1.5, cex = 0.75, outer = T)
mtext(side = 2, "Depth (cm)", line = 1.5, cex = 0.75, outer = T)
dev.off()

# ______________________________________

## Density ----
jpeg(sprintf("~/Desktop/Workspace/Earthwatch/Churchill/figures/snow_density_%s.jpg", year),
     height = 9, width = 5, units = "in", res = 150)
par(mfrow = c(4,1), mar = c(1,1,1,1), oma = c(3,3,0,0))
## TIS
plot(tis$year, tis$density_mean, type='n', xlim = c(2000,year), ylim = c(100,500), axes = F, xlab = "", ylab = "")
polygon(c(tis$year, rev(tis$year)), c(tis$density_c95, rev(tis$density_c5)), col = alpha("lightgreen",0.5), border = NA)
lines(tis$year, tis$density_mean, col = alpha("green4",0.6), lwd = 2)
box()
axis(side = 1, at = seq(2000,year,2), labels = NA)
axis(side = 2, at = seq(0,500,100))
legend("bottomleft", "TIS", fill = "lightgreen", border = NA, bty = "n", inset = c(0,0))

## Treed areas
plot(air$year, air$density_mean, type="n", xlim = c(2000,year), ylim = c(100,500), 
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
axis(side = 1, at = seq(2000,year,2), labels = NA)
axis(side = 2, at = seq(0,500,100))
legend("topleft", c("AIR","BSW","WSU"), 
       fill = c("lightgreen","cadetblue1","darksalmon"), 
       border = NA, bty = "n", inset = c(0,0), horiz = F)

## Disturbed areas
plot(bwp$year, bwp$density_mean, type="n", xlim = c(2000,year), ylim = c(100,500), 
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
axis(side = 1, at = seq(2000,year,2), labels = NA)
axis(side = 2, at = seq(0,500,100))
legend("topleft", c("BWP","BFR","PFR"), 
       fill = c("lightgreen","darksalmon","cadetblue1"), 
       border = NA, bty = "n", inset = c(0,0), horiz = F)

## Exposed areas
plot(tun$year, tun$density_mean, type="n", xlim = c(2000,year), ylim = c(100,500), 
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
axis(side = 1, at = seq(2000,year,2), labels = seq(2000,year,2))
axis(side = 2, at = seq(0,500,100))
legend("topleft", c("TUN","FEN","PPA","PPD"), 
       fill = c("lightgreen","cadetblue1","darksalmon","mediumorchid1"), 
       border = NA, bty = "n", inset = c(0,0), horiz = F)
mtext(side = 1, "Year", line = 1.5, cex = 0.75, outer = T)
mtext(expression(paste("Density (kg m"^"-3",")")), side = 2, line=1.5, cex = 0.75, outer = T)
dev.off()

# ______________________________________

## SWE ----
jpeg(sprintf("~/Desktop/Workspace/Earthwatch/Churchill/figures/snow_swe_%s.jpg", year),
     height = 9, width = 5, units = "in", res = 150)
par(mfrow = c(4,1), mar = c(1,1,1,1), oma = c(3,3,0,0))
## TIS
plot(tis$year, tis$swe_mean, type='n', xlim = c(2000,year), ylim = c(0,800), axes = F, xlab = "", ylab = "")
polygon(c(tis$year, rev(tis$year)), c(tis$swe_c95, rev(tis$swe_c5)), col = alpha("lightgreen",0.5), border = NA)
lines(tis$year, tis$swe_mean, col = alpha("green4",0.6), lwd = 2)
box()
axis(side = 1, at = seq(2000,year,2), labels = NA)
axis(side = 2, at = seq(0,800,200))
legend("bottomleft", "TIS", fill = "lightgreen", border = NA, bty = "n", inset = c(0,0))

## Treed areas
plot(air$year, air$swe_mean, type="n", xlim = c(2000,year), ylim = c(0,200), 
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
axis(side = 1, at = seq(2000,year,2), labels = NA)
axis(side = 2, at = seq(0,200,50))
legend("topleft", c("AIR","BSW","WSU"), 
       fill = c("lightgreen","cadetblue1","darksalmon"), 
       border = NA, bty = "n", inset = c(0,0), horiz = F)

## Disturbed areas
plot(bwp$year, bwp$swe_mean, type="n", xlim = c(2000,year), ylim = c(0,200), 
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
axis(side = 1, at = seq(2000,year,2), labels = NA)
axis(side = 2, at = seq(0,200,50))
legend("topleft", c("BWP","BFR","PFR"), 
       fill = c("lightgreen","darksalmon","cadetblue1"), 
       border = NA, bty = "n", inset = c(0,0), horiz = F)

## Exposed areas
plot(tun$year, tun$swe_mean, type="n", xlim = c(2000,year), ylim = c(0,200), 
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
axis(side = 1, at = seq(2000,year,2), labels = seq(2000,year,2))
axis(side = 2, at = seq(0,200,50))
legend("topleft", c("TUN","FEN","PPA","PPD"), 
       fill = c("lightgreen","cadetblue1","darksalmon","mediumorchid1"), 
       border = NA, bty = "n", inset = c(0,0), horiz = F)
mtext(side = 1, "Year", line = 1.5, cex = 0.75, outer = T)
mtext(side = 2, "SWE (mm)", line = 1.5, cex = 0.75, outer = T)
dev.off()

# ______________________________________

## HTC ----
jpeg(sprintf("~/Desktop/Workspace/Earthwatch/Churchill/figures/snow_htc_%s.jpg", year),
     height = 9, width = 5, units = "in", res = 150)
par(mfrow = c(4,1), mar = c(1,1,1,1), oma = c(3,3,0,0))
## TIS
plot(tis$year, tis$htc_mean, type='n', xlim = c(2000,year), ylim = c(0,1), axes = F, xlab = "", ylab = "")
polygon(c(tis$year, rev(tis$year)), c(tis$htc_c95, rev(tis$htc_c5)), col = alpha("lightgreen",0.5), border = NA)
lines(tis$year, tis$htc_mean, col = alpha("green4",0.6), lwd = 2)
abline(coef(tis_htc), col = alpha("green4",0.5), lty = 3, lwd = 2)
box()
axis(side = 1, at = seq(2000,year,2), labels = NA)
axis(side = 2, at = seq(0,1,0.2))
legend("bottomleft", "TIS", fill = "lightgreen", border = NA, bty = "n", inset = c(0,0))

## Treed areas
plot(air$year, air$htc_mean, type="n", xlim = c(2000,year), ylim = c(0,1), 
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
axis(side = 1, at = seq(2000,year,2), labels = NA)
axis(side = 2, at = seq(0,1,0.2))
legend("topleft", c("AIR","BSW","WSU"), 
       fill = c("lightgreen","cadetblue1","darksalmon"), 
       border = NA, bty = "n", inset = c(0,0), horiz = F)

## Disturbed areas
plot(bwp$year, bwp$htc_mean, type="n", xlim = c(2000,year), ylim = c(0,5), 
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
axis(side = 1, at = seq(2000,year,2), labels = NA)
axis(side = 2, at = seq(0,5,1))
legend("topleft", c("BWP","BFR","PFR"), 
       fill = c("lightgreen","darksalmon","cadetblue1"), 
       border = NA, bty = "n", inset = c(0,0), horiz = F)

## Exposed areas
plot(tun$year, tun$htc_mean, type="n", xlim = c(2000,year), ylim = c(0,12), 
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
axis(side = 1, at = seq(2000,year,2), labels = seq(2000,year,2))
axis(side = 2, at = seq(0,12,2))
legend("topleft", c("TUN","FEN","PPA","PPD"), 
       fill = c("lightgreen","cadetblue1","darksalmon","mediumorchid1"), 
       border = NA, bty = "n", inset = c(0,0), horiz = F)
mtext(side = 1, "Year", line = 1.5, cex = 0.75, outer = T)
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=1.5, cex = 0.75, outer = T)
dev.off()

