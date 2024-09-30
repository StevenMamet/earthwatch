library(dplyr)
library(lubridate)
library(pscl)
library(lmtest)
library(scales)
library(tidyverse)
library(wesanderson)

rm(list = ls())

setwd("~/Desktop/Workspace/")

# Constants ----
year <- 2024

# Functions ----
poly_na <- function(df, color) {
  # This function will plot polygons for each segment of a 
  # dateset containing NAs. The default approach would draw gibberish
  # polygons.
  enc <- rle(!is.na(df$mean))
  endIdxs <- cumsum(enc$lengths)
  for (i in 1:length(enc$lengths)) {
    if (enc$values[i]) {
      endIdx <- endIdxs[i]
      startIdx <- endIdx - enc$lengths[i] + 1
      
      subc5 <- df$c5[startIdx:endIdx]
      subc95 <- df$c95[startIdx:endIdx]
      subyear <- df$year[startIdx:endIdx]
      
      x <- c(subyear, rev(subyear))
      y <- c(subc5, rev(subc95))
      
      polygon(
        x = x,
        y = y,
        col = scales::alpha(color,0.6),
        border = NA
      )
    }
  }
}

#_______________________________----
# Microclimate ----
ch <- read.csv("./Earthwatch/Churchill/data/microclimate_20000101_20221010_filled.csv", header = T)[-1]

# Format date, add in cool/warm seasons, season_year
ch <- ch %>% 
  mutate(Date = ymd(Date), 
         season_year = ifelse(month(Date) == 10 | month(Date) == 11 | month(Date) == 12,
                              year(Date) + 1, year(Date)),
         season = case_when(month(Date) %in% c(10,11,12,1,2,3,4,5) ~ "cool",
                            month(Date) %in% c(6, 7, 8, 9) ~ "warm",
                            T ~ NA_character_
         ))

ch_temps_season <- ch %>% 
  group_by(season_year, season) %>% 
  summarise(across(ends_with(c("airp150","bsw150","tis150","wsu150","mlk150","fen150","bfr150","pfr150",
                               "bwp150","ppa150","ppd150","tun150","rlk150",
                               "airp0","bsw0","tis0","wsu0","mlk0","fen0","bfr0","pfr0",
                               "bwp0","ppa0","ppd0","tun0","rlk0",
                               "neg80")), ~ mean(.x, na.rm = TRUE)))

ch_temps_w <- subset(ch_temps_season, season == "warm")
ch_temps_c <- subset(ch_temps_season, season == "cool")
matplot(ch_temps_w[,-c(1,2)], type = "l")
matplot(ch_temps_c[,-c(1,2)], type = "l")

## Air temperature regressions
summary(tun150w <- lm(tun150 ~ season_year, data = ch_temps_w[-c(1,23),]))     # ns
summary(ppa150w <- lm(ppa150 ~ season_year, data = ch_temps_w[-c(1,23),]))     # ns
summary(ppd150w <- lm(ppd150 ~ season_year, data = ch_temps_w[-c(1,23),]))     # ns
summary(fen150w <- lm(fen150 ~ season_year, data = ch_temps_w[-c(1,23),]))     # ns
summary(tis150w <- lm(tis150 ~ season_year, data = ch_temps_w[-c(1,23),]))     # ns
summary(airp150w <- lm(airp150 ~ season_year, data = ch_temps_w[-c(1,23),]))   # ns
summary(bsw150w <- lm(bsw150 ~ season_year, data = ch_temps_w[-c(1,23),]))     # ns
summary(wsu150w <- lm(wsu150 ~ season_year, data = ch_temps_w[-c(1,23),]))     # ns
summary(bfr150w <- lm(bfr150 ~ season_year, data = ch_temps_w[-c(1,23),]))     # ns
summary(pfr150w <- lm(pfr150 ~ season_year, data = ch_temps_w[-c(1,23),]))     # ns
summary(bwp150w <- lm(bwp150 ~ season_year, data = ch_temps_w[-c(1,23),]))     # SIG

summary(tun150c <- lm(tun150 ~ season_year, data = ch_temps_c[-c(1,22),]))     # ns
summary(ppa150c <- lm(ppa150 ~ season_year, data = ch_temps_c[-c(1,22),]))     # ns
summary(ppd150c <- lm(ppd150 ~ season_year, data = ch_temps_c[-c(1,22),]))     # ns
summary(fen150c <- lm(fen150 ~ season_year, data = ch_temps_c[-c(1,22),]))     # ns
summary(tis150c <- lm(tis150 ~ season_year, data = ch_temps_c[-c(1,22),]))     # ns
summary(airp150c <- lm(airp150 ~ season_year, data = ch_temps_c[-c(1,22),]))   # ns
summary(bsw150c <- lm(bsw150 ~ season_year, data = ch_temps_c[-c(1,22),]))     # ns
summary(wsu150c <- lm(wsu150 ~ season_year, data = ch_temps_c[-c(1,22),]))     # ns
summary(bfr150c <- lm(bfr150 ~ season_year, data = ch_temps_c[-c(1,22),]))     # ns
summary(pfr150c <- lm(pfr150 ~ season_year, data = ch_temps_c[-c(1,22),]))     # ns
summary(bwp150c <- lm(bwp150 ~ season_year, data = ch_temps_c[-c(1,22),]))     # ns

## Ground surface regressions
summary(tun0w <- lm(tun0 ~ season_year, data = ch_temps_w[-c(1,23),]))     # ns
summary(ppa0w <- lm(ppa0 ~ season_year, data = ch_temps_w[-c(1,23),]))     # ns
summary(ppd0w <- lm(ppd0 ~ season_year, data = ch_temps_w[-c(1,23),]))     # SIG
summary(fen0w <- lm(fen0 ~ season_year, data = ch_temps_w[-c(1,23),]))     # ns
summary(tis0w <- lm(tis0 ~ season_year, data = ch_temps_w[-c(1,23),]))     # ns
summary(airp0w <- lm(airp0 ~ season_year, data = ch_temps_w[-c(1,23),]))   # ns
summary(bsw0w <- lm(bsw0 ~ season_year, data = ch_temps_w[-c(1,23),]))     # ns
summary(wsu0w <- lm(wsu0 ~ season_year, data = ch_temps_w[-c(1,23),]))     # SIG
summary(bfr0w <- lm(bfr0 ~ season_year, data = ch_temps_w[-c(1,23),]))     # SIG
summary(pfr0w <- lm(pfr0 ~ season_year, data = ch_temps_w[-c(1,23),]))     # SIG
summary(bwp0w <- lm(bwp0 ~ season_year, data = ch_temps_w[-c(1,23),]))     # ns

summary(tun0c <- lm(tun0 ~ season_year, data = ch_temps_c[-c(1,22),]))     # SIG
summary(ppa0c <- lm(ppa0 ~ season_year, data = ch_temps_c[-c(1,22),]))     # SIG
summary(ppd0c <- lm(ppd0 ~ season_year, data = ch_temps_c[-c(1,22),]))     # SIG
summary(fen0c <- lm(fen0 ~ season_year, data = ch_temps_c[-c(1,22),]))     # SIG
summary(tis0c <- lm(tis0 ~ season_year, data = ch_temps_c[-c(1,22),]))     # SIG
summary(airp0c <- lm(airp0 ~ season_year, data = ch_temps_c[-c(1,22),]))   # SIG
summary(bsw0c <- lm(bsw0 ~ season_year, data = ch_temps_c[-c(1,22),]))     # ns
summary(wsu0c <- lm(wsu0 ~ season_year, data = ch_temps_c[-c(1,22),]))     # ns
summary(bfr0c <- lm(bfr0 ~ season_year, data = ch_temps_c[-c(1,22),]))     # SIG
summary(pfr0c <- lm(pfr0 ~ season_year, data = ch_temps_c[-c(1,22),]))     # SIG
summary(bwp0c <- lm(bwp0 ~ season_year, data = ch_temps_c[-c(1,22),]))     # ns

## Permafrost regressions
summary(tunneg80w <- lm(tunneg80 ~ season_year, data = ch_temps_w[-c(1,23),]))     # ns
summary(ppaneg80w <- lm(ppaneg80 ~ season_year, data = ch_temps_w[-c(1,23),]))     # SIG
summary(ppdneg80w <- lm(ppdneg80 ~ season_year, data = ch_temps_w[-c(1,23),]))     # ns
summary(fenneg80w <- lm(fenneg80 ~ season_year, data = ch_temps_w[-c(1,23),]))     # SIG
summary(tisneg80w <- lm(tisneg80 ~ season_year, data = ch_temps_w[-c(1,23),]))     # ns
summary(airpneg80w <- lm(airpneg80 ~ season_year, data = ch_temps_w[-c(1,23),]))   # ns
summary(bswneg80w <- lm(bswneg80 ~ season_year, data = ch_temps_w[-c(1,23),]))     # ns
summary(wsuneg80w <- lm(wsuneg80 ~ season_year, data = ch_temps_w[-c(1,23),]))     # SIG
summary(bfrneg80w <- lm(bfrneg80 ~ season_year, data = ch_temps_w[-c(1,23),]))     # ns
summary(pfrneg80w <- lm(pfrneg80 ~ season_year, data = ch_temps_w[-c(1,23),]))     # SIG
summary(bwpneg80w <- lm(bwpneg80 ~ season_year, data = ch_temps_w[-c(1,23),]))     # SIG

summary(tunneg80c <- lm(tunneg80 ~ season_year, data = ch_temps_c[-c(1,22),]))     # ns
summary(ppaneg80c <- lm(ppaneg80 ~ season_year, data = ch_temps_c[-c(1,22),]))     # SIG
summary(ppdneg80c <- lm(ppdneg80 ~ season_year, data = ch_temps_c[-c(1,22),]))     # ns
summary(fenneg80c <- lm(fenneg80 ~ season_year, data = ch_temps_c[-c(1,22),]))     # SIG
summary(tisneg80c <- lm(tisneg80 ~ season_year, data = ch_temps_c[-c(1,22),]))     # SIG
summary(airpneg80c <- lm(airpneg80 ~ season_year, data = ch_temps_c[-c(1,22),]))   # ns
summary(bswneg80c <- lm(bswneg80 ~ season_year, data = ch_temps_c[-c(1,22),]))     # ns
summary(wsuneg80c <- lm(wsuneg80 ~ season_year, data = ch_temps_c[-c(1,22),]))     # ns
summary(bfrneg80c <- lm(bfrneg80 ~ season_year, data = ch_temps_c[-c(1,22),]))     # SIG
summary(pfrneg80c <- lm(pfrneg80 ~ season_year, data = ch_temps_c[-c(1,22),]))     # SIG
summary(bwpneg80c <- lm(bwpneg80 ~ season_year, data = ch_temps_c[-c(1,22),]))     # SIG

## Export at 6 x 3.5

treed_cols <- rep(wes_palette("BottleRocket2")[4], 4)
disturbed_cols <- rep(wes_palette("FantasticFox1")[4], 3)
fen_cols <- wes_palette("FantasticFox1")[2]
treeless_cols <- rep(wes_palette("FantasticFox1")[3], 3)
col_pal <- c(treed_cols, disturbed_cols, fen_cols, treeless_cols)
col_pal2 <- c(treed_cols[1], disturbed_cols[1], fen_cols[1], treeless_cols[1])

# Warm
jpeg("~/Desktop/Workspace/Earthwatch/Churchill/figures/warmT_2022.jpg", width = 5, height = 7, units = "in", res = 300)
par(xpd = F)
par(mar = c(2,2,1,1), oma = c(2,2,0,0))
par(mfrow = c(3,1))
## Air T
matplot(ch_temps_w$season_year[-c(1,23)], ch_temps_w[-c(1,23),c(3:6,8:14)], 
        type = "l", col = col_pal, 
        lty = 1, ylab = "", xlab = "", ylim = c(6,15))
mtext("Air (°C)", side = 2, line = 2.5, cex = 0.7)
abline(coef(bwp150w), lty=2, col = wes_palette("FantasticFox1")[4])
## Grd T
matplot(ch_temps_w$season_year[-c(1,23)], ch_temps_w[-c(1,23),c(16:19,21:27)], 
        type = "l", col = col_pal, 
        lty = 1, ylab = "", xlab = "", ylim = c(5,14))
mtext("Ground surface (°C)", side = 2, line = 2.5, cex = 0.7)
abline(coef(ppd0w), lty=2, col = wes_palette("FantasticFox1")[3])
abline(coef(wsu0w), lty=2, col = wes_palette("BottleRocket2")[4])
abline(coef(bfr0w), lty=2, col = wes_palette("BottleRocket2")[4])
abline(coef(pfr0w), lty=2, col = wes_palette("BottleRocket2")[4])
## -80 T
matplot(ch_temps_w$season_year[-c(1,23)], ch_temps_w[-c(1,23),c(28:31,33:39)], type = "l", 
        col = col_pal, 
        lty = 1, ylab = "", xlab = "", ylim = c(-2,12))
mtext("Subsurface (°C)", side = 2, line = 2.5, cex = 0.7)
mtext("Year", side = 1, line = 2.5, cex = 0.7)
abline(coef(ppaneg80w), lty=2, col = wes_palette("FantasticFox1")[3])
abline(coef(fenneg80w), lty=2, col = wes_palette("FantasticFox1")[2])
abline(coef(wsuneg80w), lty=2, col = wes_palette("BottleRocket2")[4])
abline(coef(pfrneg80w), lty=2, col = wes_palette("FantasticFox1")[4])
abline(coef(bwpneg80w), lty=2, col = wes_palette("FantasticFox1")[4])

par(xpd = NA)
legend(2004,12, c("Treed","Fen","Disturbed","Treeless"), 
       col = col_pal2, 
       horiz = T, lty = 1, cex = 1, bty = "n", y.intersp = 1, text.width = 2)
dev.off()

# Cool
jpeg("~/Desktop/Workspace/Earthwatch/Churchill/figures/coolT_2022.jpg", width = 5, height = 7, units = "in", res = 300)
par(xpd = F)
par(mar = c(2,2,1,1), oma = c(2,2,0,0))
par(mfrow = c(3,1))
## Air T
matplot(ch_temps_c$season_year[-c(1,23,24)], ch_temps_c[-c(1,23,24),c(3:6,8:14)], 
        type = "l", col = col_pal, 
        lty = 1, ylab = "", xlab = "", ylim = c(-16.5,-9))
mtext("Air (°C)", side = 2, line = 2.5, cex = 0.7)
## Grd T
matplot(ch_temps_c$season_year[-c(1,23,24)], ch_temps_c[-c(1,23,24),c(16:19,21:27)], 
        type = "l", col = col_pal, 
        lty = 1, ylab = "", xlab = "", ylim = c(-16,1))
mtext("Ground surface (°C)", side = 2, line = 2.5, cex = 0.7)
abline(coef(tun0c), lty=2, col = wes_palette("FantasticFox1")[3])
abline(coef(ppa0c), lty=2, col = wes_palette("FantasticFox1")[3])
abline(coef(ppd0c), lty=2, col = wes_palette("FantasticFox1")[3])
abline(coef(tis0c), lty=2, col = wes_palette("BottleRocket2")[4])
abline(coef(airp0c), lty=2, col = wes_palette("BottleRocket2")[4])
abline(coef(bfr0c), lty=2, col = wes_palette("BottleRocket2")[4])
abline(coef(pfr0c), lty=2, col = wes_palette("BottleRocket2")[4])
abline(coef(fen0c), lty=2, col = wes_palette("FantasticFox1")[2])
## -80 T
matplot(ch_temps_c$season_year[-c(1,23,24)], ch_temps_c[-c(1,23,24),c(28:31,33:39)], type = "l", 
        col = col_pal, 
        lty = 1, ylab = "", xlab = "", ylim = c(-9,4))
mtext("Subsurface (°C)", side = 2, line = 2.5, cex = 0.7)
mtext("Year", side = 1, line = 2.5, cex = 0.7)
abline(coef(ppaneg80c), lty=2, col = wes_palette("FantasticFox1")[3])
abline(coef(fenneg80c), lty=2, col = wes_palette("FantasticFox1")[2])
abline(coef(tisneg80c), lty=2, col = wes_palette("BottleRocket2")[4])
abline(coef(bfrneg80c), lty=2, col = wes_palette("FantasticFox1")[4])
abline(coef(pfrneg80c), lty=2, col = wes_palette("FantasticFox1")[4])
abline(coef(bwpneg80c), lty=2, col = wes_palette("FantasticFox1")[4])

par(xpd = NA)
legend(2004,4, c("Treed","Fen","Disturbed","Treeless"), 
       col = col_pal2, 
       horiz = T, lty = 1, cex = 1, bty = "n", y.intersp = 1, text.width = 2)
dev.off()

#_______________________________----
# Thaw depths ----
thaw_ch <- read.csv(file = "~/Desktop/Workspace/Earthwatch/Churchill/data/ch_thaw_depths.csv", header = TRUE)
thaw_ch$ci <- thaw_ch$se*qt(0.975, thaw_ch$n-1)
thaw_ch$c5 <- thaw_ch$mean - thaw_ch$ci
thaw_ch$c95 <- thaw_ch$mean + thaw_ch$ci

air <- subset(thaw_ch, site == "AIR")
blk <- subset(thaw_ch, site == "BLK")
ppa <- subset(thaw_ch, site == "PPA")
ppd <- subset(thaw_ch, site == "PPD")

air_lm <- lm(air$mean~air$year)
blk_lm <- lm(blk$mean~blk$year)
ppa_lm <- lm(ppa$mean~ppa$year)
ppd_lm <- lm(ppd$mean~ppd$year)

summary(air_lm) # ns
summary(blk_lm) # SIG
summary(ppa_lm) # SIG
summary(ppd_lm) # SIG

blk_slope <- round(coef(blk_lm)[[2]], 3)
ppa_slope <- round(coef(ppa_lm)[[2]], 3)
ppd_slope <- round(coef(ppd_lm)[[2]], 3)

jpeg(sprintf("./Earthwatch/Churchill/figures/thaw_depth_%s.jpeg", year), width = 6, height = 5, units = "in", res = 300)
par(mar = c(4,4,2,1))
par(xpd = FALSE)

# PPA
plot(ppa$year, ppa$mean, type='n', xlim = c(2002,year), ylim = rev(c(35,130)), xaxt='n',yaxt = "n", ann=FALSE)# yaxs = "i", xaxs = "i", xlab = "", ylab = "")
poly_na(ppa, "olivedrab1")
points(ppa[c(1,3,c(nrow(ppa):nrow(ppa)-1)),1], ppa[c(1,3,c(nrow(ppa):nrow(ppa)-1)),4], col = alpha("olivedrab1",0.6), pch = 15)
lines(ppa$year, ppa$mean, type = "l", lwd=2, col = "olivedrab4")
# legend("topleft", "PPA (17 m.a.s.l.)", bty = "n", inset = c(0,0.05))
abline(coef(ppa_lm),lty=2, col = "olivedrab4")

# PPD
poly_na(ppd, "darkorchid1")
lines(ppd$year, ppd$mean, type = "l", lwd=2, col = "darkorchid4")
points(ppd[c(1,3),1], ppd[c(1,3),4], col = alpha("darkorchid4",0.6), pch = 15)
# legend("topleft", "PPD (17 m.a.s.l.)", bty = "n", inset = c(0,0.05))
abline(coef(ppd_lm),lty=2, col = "darkorchid4")

# BLK
poly_na(blk, "firebrick1")
lines(blk$year, blk$mean, type = "l", lwd=2, col = "firebrick4")
# points(blk[c(nrow(blk)-1),1], blk[c(nrow(blk)-1),4], col = alpha("firebrick4",0.6), pch = 15)
abline(coef(blk_lm),lty=2, col = "firebrick4")

# AIR
poly_na(air, "darkslategray1")
lines(air$year, air$mean, type = "l", lwd=2, col = "darkslategray4")

# Regression slopes
legend("bottomleft", legend=bquote("PPA slope = " ~ .(-ppa_slope) ~ "cm yr"^-1), bty = "n", inset = c(0.0,0.12), text.col = "olivedrab4")
legend("bottomleft", legend=bquote("PPD slope = " ~ .(-ppd_slope) ~ "cm yr"^-1), bty = "n", inset = c(0.0,0.06), text.col = "darkorchid4")
legend("bottomleft", legend=bquote("BLK slope = " ~ .(-blk_slope) ~ "cm yr"^-1), bty = "n", inset = c(0.0,0.0), text.col = "firebrick4")

# Axes
axis(1, at = seq(2002,year,1), labels = NA)
axis(1, at = seq(2002,year,2), labels = seq(2002,year,2))
axis(2, at = rev(seq(40,130,10)), labels = NA)
axis(2, at = rev(seq(40,120,20)), labels = rev(seq(40,120,20)), tick = FALSE)
mtext(side = 1, "Year", line = 2.5)
mtext(side = 2, "Thaw depth (cm)", line = 2.5)

# Legend
par(xpd = NA)
legend(2002,22, c("AIR","PPA","PPD","BLK"), col = c("darkslategray1","olivedrab1", "darkorchid1","firebrick1"), horiz = T, 
       bty = "n", pch = 15, pt.cex = c(1,1), text.width = 2, y.intersp = 1, inset = c(0.05,-0.02))
dev.off()

#_______________________________----
# GTREE exclosures ----
ch <- read.csv(file = "~/Desktop/Workspace/Earthwatch/Churchill/data/gtree_ch_exclosures.csv", header = TRUE)

ch$treat <- as.factor(ch$treat)
ch$scarif <- as.factor(ch$scarif)
ch$seeded <- as.factor(ch$seeded)
ch$tag <- factor(ch$tag)

ch.tun <- droplevels(subset(ch, site == "TUN" & seeded == 1))
ch.tis <- droplevels(subset(ch, site == "TIS" & seeded == 1))
ch.rid <- droplevels(subset(ch, site == "RID" & seeded == 1))
ch.wsu <- droplevels(subset(ch, site == "WSU" & seeded == 1))

ch.wsu.larch.ex1 <- subset(ch.wsu, species == "ll" & exclosure == "yes" & sow.year == 2016)
ch.wsu.larch.no1 <- subset(ch.wsu, species == "ll" & exclosure == "no" & sow.year == 2016)
ch.tun.larch.ex1 <- subset(ch.tun, species == "ll" & exclosure == "yes" & sow.year == 2016)
ch.tun.larch.no1 <- subset(ch.tun, species == "ll" & exclosure == "no" & sow.year == 2016)
ch.tis.larch.ex1 <- subset(ch.tis, species == "ll" & exclosure == "yes" & sow.year == 2016)
ch.tis.larch.no1 <- subset(ch.tis, species == "ll" & exclosure == "no" & sow.year == 2016)
ch.rid.larch.ex1 <- subset(ch.rid, species == "ll" & exclosure == "yes" & sow.year == 2016)
ch.rid.larch.no1 <- subset(ch.rid, species == "ll" & exclosure == "no" & sow.year == 2016)

ch.wsu.wspruce.ex1 <- subset(ch.wsu, species == "ws" & exclosure == "yes" & sow.year == 2016)
ch.wsu.wspruce.no1 <- subset(ch.wsu, species == "ws" & exclosure == "no" & sow.year == 2016)
ch.tun.wspruce.ex1 <- subset(ch.tun, species == "ws" & exclosure == "yes" & sow.year == 2016)
ch.tun.wspruce.no1 <- subset(ch.tun, species == "ws" & exclosure == "no" & sow.year == 2016)
ch.tis.wspruce.ex1 <- subset(ch.tis, species == "ws" & exclosure == "yes" & sow.year == 2016)
ch.tis.wspruce.no1 <- subset(ch.tis, species == "ws" & exclosure == "no" & sow.year == 2016)
ch.rid.wspruce.ex1 <- subset(ch.rid, species == "ws" & exclosure == "yes" & sow.year == 2016)
ch.rid.wspruce.no1 <- subset(ch.rid, species == "ws" & exclosure == "no" & sow.year == 2016)

ch.wsu.bspruce.ex1 <- subset(ch.wsu, species == "bs" & exclosure == "yes" & sow.year == 2016)
ch.wsu.bspruce.no1 <- subset(ch.wsu, species == "bs" & exclosure == "no" & sow.year == 2016)
ch.tun.bspruce.ex1 <- subset(ch.tun, species == "bs" & exclosure == "yes" & sow.year == 2016)
ch.tun.bspruce.no1 <- subset(ch.tun, species == "bs" & exclosure == "no" & sow.year == 2016)
ch.tis.bspruce.ex1 <- subset(ch.tis, species == "bs" & exclosure == "yes" & sow.year == 2016)
ch.tis.bspruce.no1 <- subset(ch.tis, species == "bs" & exclosure == "no" & sow.year == 2016)
ch.rid.bspruce.ex1 <- subset(ch.rid, species == "bs" & exclosure == "yes" & sow.year == 2016)
ch.rid.bspruce.no1 <- subset(ch.rid, species == "bs" & exclosure == "no" & sow.year == 2016)


ch.wsu.larch.ex2 <- subset(ch.wsu, species == "ll" & exclosure == "yes" & sow.year == 2017)
ch.wsu.larch.no2 <- subset(ch.wsu, species == "ll" & exclosure == "no" & sow.year == 2017)
ch.tun.larch.ex2 <- subset(ch.tun, species == "ll" & exclosure == "yes" & sow.year == 2017)
ch.tun.larch.no2 <- subset(ch.tun, species == "ll" & exclosure == "no" & sow.year == 2017)
ch.tis.larch.ex2 <- subset(ch.tis, species == "ll" & exclosure == "yes" & sow.year == 2017)
ch.tis.larch.no2 <- subset(ch.tis, species == "ll" & exclosure == "no" & sow.year == 2017)
ch.rid.larch.ex2 <- subset(ch.rid, species == "ll" & exclosure == "yes" & sow.year == 2017)
ch.rid.larch.no2 <- subset(ch.rid, species == "ll" & exclosure == "no" & sow.year == 2017)

ch.wsu.wspruce.ex2 <- subset(ch.wsu, species == "ws" & exclosure == "yes" & sow.year == 2017)
ch.wsu.wspruce.no2 <- subset(ch.wsu, species == "ws" & exclosure == "no" & sow.year == 2017)
ch.tun.wspruce.ex2 <- subset(ch.tun, species == "ws" & exclosure == "yes" & sow.year == 2017)
ch.tun.wspruce.no2 <- subset(ch.tun, species == "ws" & exclosure == "no" & sow.year == 2017)
ch.tis.wspruce.ex2 <- subset(ch.tis, species == "ws" & exclosure == "yes" & sow.year == 2017)
ch.tis.wspruce.no2 <- subset(ch.tis, species == "ws" & exclosure == "no" & sow.year == 2017)
ch.rid.wspruce.ex2 <- subset(ch.rid, species == "ws" & exclosure == "yes" & sow.year == 2017)
ch.rid.wspruce.no2 <- subset(ch.rid, species == "ws" & exclosure == "no" & sow.year == 2017)

ch.wsu.bspruce.ex2 <- subset(ch.wsu, species == "bs" & exclosure == "yes" & sow.year == 2017)
ch.wsu.bspruce.no2 <- subset(ch.wsu, species == "bs" & exclosure == "no" & sow.year == 2017)
ch.tun.bspruce.ex2 <- subset(ch.tun, species == "bs" & exclosure == "yes" & sow.year == 2017)
ch.tun.bspruce.no2 <- subset(ch.tun, species == "bs" & exclosure == "no" & sow.year == 2017)
ch.tis.bspruce.ex2 <- subset(ch.tis, species == "bs" & exclosure == "yes" & sow.year == 2017)
ch.tis.bspruce.no2 <- subset(ch.tis, species == "bs" & exclosure == "no" & sow.year == 2017)
ch.rid.bspruce.ex2 <- subset(ch.rid, species == "bs" & exclosure == "yes" & sow.year == 2017)
ch.rid.bspruce.no2 <- subset(ch.rid, species == "bs" & exclosure == "no" & sow.year == 2017)

ch.wsu.larch.ex3 <- subset(ch.wsu, species == "ll" & exclosure == "yes" & sow.year == 2018)
ch.wsu.larch.no3 <- subset(ch.wsu, species == "ll" & exclosure == "no" & sow.year == 2018)
ch.tun.larch.ex3 <- subset(ch.tun, species == "ll" & exclosure == "yes" & sow.year == 2018)
ch.tun.larch.no3 <- subset(ch.tun, species == "ll" & exclosure == "no" & sow.year == 2018)
ch.tis.larch.ex3 <- subset(ch.tis, species == "ll" & exclosure == "yes" & sow.year == 2018)
ch.tis.larch.no3 <- subset(ch.tis, species == "ll" & exclosure == "no" & sow.year == 2018)
ch.rid.larch.ex3 <- subset(ch.rid, species == "ll" & exclosure == "yes" & sow.year == 2018)
ch.rid.larch.no3 <- subset(ch.rid, species == "ll" & exclosure == "no" & sow.year == 2018)

ch.wsu.wspruce.ex3 <- subset(ch.wsu, species == "ws" & exclosure == "yes" & sow.year == 2018)
ch.wsu.wspruce.no3 <- subset(ch.wsu, species == "ws" & exclosure == "no" & sow.year == 2018)
ch.tun.wspruce.ex3 <- subset(ch.tun, species == "ws" & exclosure == "yes" & sow.year == 2018)
ch.tun.wspruce.no3 <- subset(ch.tun, species == "ws" & exclosure == "no" & sow.year == 2018)
ch.tis.wspruce.ex3 <- subset(ch.tis, species == "ws" & exclosure == "yes" & sow.year == 2018)
ch.tis.wspruce.no3 <- subset(ch.tis, species == "ws" & exclosure == "no" & sow.year == 2018)
ch.rid.wspruce.ex3 <- subset(ch.rid, species == "ws" & exclosure == "yes" & sow.year == 2018)
ch.rid.wspruce.no3 <- subset(ch.rid, species == "ws" & exclosure == "no" & sow.year == 2018)

ch.wsu.bspruce.ex3 <- subset(ch.wsu, species == "bs" & exclosure == "yes" & sow.year == 2018)
ch.wsu.bspruce.no3 <- subset(ch.wsu, species == "bs" & exclosure == "no" & sow.year == 2018)
ch.tun.bspruce.ex3 <- subset(ch.tun, species == "bs" & exclosure == "yes" & sow.year == 2018)
ch.tun.bspruce.no3 <- subset(ch.tun, species == "bs" & exclosure == "no" & sow.year == 2018)
ch.tis.bspruce.ex3 <- subset(ch.tis, species == "bs" & exclosure == "yes" & sow.year == 2018)
ch.tis.bspruce.no3 <- subset(ch.tis, species == "bs" & exclosure == "no" & sow.year == 2018)
ch.rid.bspruce.ex3 <- subset(ch.rid, species == "bs" & exclosure == "yes" & sow.year == 2018)
ch.rid.bspruce.no3 <- subset(ch.rid, species == "bs" & exclosure == "no" & sow.year == 2018)

x1a <- factor(ch.tun.larch.ex1[ch.tun.larch.ex1$sow.year==2016,"treat"], levels = c("2","4"))
x1b <- factor(ch.tun.larch.no1[ch.tun.larch.no1$sow.year==2016,"treat"], levels = c("2","4"))
x2a <- factor(ch.tis.larch.ex1[ch.tis.larch.ex1$sow.year==2016,"treat"], levels = c("2","4"))
x2b <- factor(ch.tis.larch.no1[ch.tis.larch.no1$sow.year==2016,"treat"], levels = c("2","4"))
x3a <- factor(ch.rid.larch.ex1[ch.rid.larch.ex1$sow.year==2016,"treat"], levels = c("2","4"))
x3b <- factor(ch.rid.larch.no1[ch.rid.larch.no1$sow.year==2016,"treat"], levels = c("2","4"))
x4a <- factor(ch.wsu.larch.ex1[ch.wsu.larch.ex1$sow.year==2016,"treat"], levels = c("2","4"))
x4b <- factor(ch.wsu.larch.no1[ch.wsu.larch.no1$sow.year==2016,"treat"], levels = c("2","4"))

x5a <- factor(ch.tun.wspruce.ex1[ch.tun.wspruce.ex1$sow.year==2016,"treat"], levels = c("2","4"))
x5b <- factor(ch.tun.wspruce.no1[ch.tun.wspruce.no1$sow.year==2016,"treat"], levels = c("2","4"))
x6a <- factor(ch.tis.wspruce.ex1[ch.tis.wspruce.ex1$sow.year==2016,"treat"], levels = c("2","4"))
x6b <- factor(ch.tis.wspruce.no1[ch.tis.wspruce.no1$sow.year==2016,"treat"], levels = c("2","4"))
x7a <- factor(ch.rid.wspruce.ex1[ch.rid.wspruce.ex1$sow.year==2016,"treat"], levels = c("2","4"))
x7b <- factor(ch.rid.wspruce.no1[ch.rid.wspruce.no1$sow.year==2016,"treat"], levels = c("2","4"))
x8a <- factor(ch.wsu.wspruce.ex1[ch.wsu.wspruce.ex1$sow.year==2016,"treat"], levels = c("2","4"))
x8b <- factor(ch.wsu.wspruce.no1[ch.wsu.wspruce.no1$sow.year==2016,"treat"], levels = c("2","4"))

x9a <- factor(ch.tun.bspruce.ex1[ch.tun.bspruce.ex1$sow.year==2016,"treat"], levels = c("2","4"))
x9b <- factor(ch.tun.bspruce.no1[ch.tun.bspruce.no1$sow.year==2016,"treat"], levels = c("2","4"))
x10a <- factor(ch.tis.bspruce.ex1[ch.tis.bspruce.ex1$sow.year==2016,"treat"], levels = c("2","4"))
x10b <- factor(ch.tis.bspruce.no1[ch.tis.bspruce.no1$sow.year==2016,"treat"], levels = c("2","4"))
x11a <- factor(ch.rid.bspruce.ex1[ch.rid.bspruce.ex1$sow.year==2016,"treat"], levels = c("2","4"))
x11b <- factor(ch.rid.bspruce.no1[ch.rid.bspruce.no1$sow.year==2016,"treat"], levels = c("2","4"))
x12a <- factor(ch.wsu.bspruce.ex1[ch.wsu.bspruce.ex1$sow.year==2016,"treat"], levels = c("2","4"))
x12b <- factor(ch.wsu.bspruce.no1[ch.wsu.bspruce.no1$sow.year==2016,"treat"], levels = c("2","4"))



y1a <- factor(ch.tun.larch.ex2[ch.tun.larch.ex2$sow.year==2017,"treat"], levels = c("2","4"))
y1b <- factor(ch.tun.larch.no2[ch.tun.larch.no2$sow.year==2017,"treat"], levels = c("2","4"))
y2a <- factor(ch.tis.larch.ex2[ch.tis.larch.ex2$sow.year==2017,"treat"], levels = c("2","4"))
y2b <- factor(ch.tis.larch.no2[ch.tis.larch.no2$sow.year==2017,"treat"], levels = c("2","4"))
y3a <- factor(ch.rid.larch.ex2[ch.rid.larch.ex2$sow.year==2017,"treat"], levels = c("2","4"))
y3b <- factor(ch.rid.larch.no2[ch.rid.larch.no2$sow.year==2017,"treat"], levels = c("2","4"))
y4a <- factor(ch.wsu.larch.ex2[ch.wsu.larch.ex2$sow.year==2017,"treat"], levels = c("2","4"))
y4b <- factor(ch.wsu.larch.no2[ch.wsu.larch.no2$sow.year==2017,"treat"], levels = c("2","4"))

y5a <- factor(ch.tun.wspruce.ex2[ch.tun.wspruce.ex2$sow.year==2017,"treat"], levels = c("2","4"))
y5b <- factor(ch.tun.wspruce.no2[ch.tun.wspruce.no2$sow.year==2017,"treat"], levels = c("2","4"))
y6a <- factor(ch.tis.wspruce.ex2[ch.tis.wspruce.ex2$sow.year==2017,"treat"], levels = c("2","4"))
y6b <- factor(ch.tis.wspruce.no2[ch.tis.wspruce.no2$sow.year==2017,"treat"], levels = c("2","4"))
y7a <- factor(ch.rid.wspruce.ex2[ch.rid.wspruce.ex2$sow.year==2017,"treat"], levels = c("2","4"))
y7b <- factor(ch.rid.wspruce.no2[ch.rid.wspruce.no2$sow.year==2017,"treat"], levels = c("2","4"))
y8a <- factor(ch.wsu.wspruce.ex2[ch.wsu.wspruce.ex2$sow.year==2017,"treat"], levels = c("2","4"))
y8b <- factor(ch.wsu.wspruce.no2[ch.wsu.wspruce.no2$sow.year==2017,"treat"], levels = c("2","4"))

y9a <- factor(ch.tun.bspruce.ex2[ch.tun.bspruce.ex2$sow.year==2017,"treat"], levels = c("2","4"))
y9b <- factor(ch.tun.bspruce.no2[ch.tun.bspruce.no2$sow.year==2017,"treat"], levels = c("2","4"))
y10a <- factor(ch.tis.bspruce.ex2[ch.tis.bspruce.ex2$sow.year==2017,"treat"], levels = c("2","4"))
y10b <- factor(ch.tis.bspruce.no2[ch.tis.bspruce.no2$sow.year==2017,"treat"], levels = c("2","4"))
y11a <- factor(ch.rid.bspruce.ex2[ch.rid.bspruce.ex2$sow.year==2017,"treat"], levels = c("2","4"))
y11b <- factor(ch.rid.bspruce.no2[ch.rid.bspruce.no2$sow.year==2017,"treat"], levels = c("2","4"))
y12a <- factor(ch.wsu.bspruce.ex2[ch.wsu.bspruce.ex2$sow.year==2017,"treat"], levels = c("2","4"))
y12b <- factor(ch.wsu.bspruce.no2[ch.wsu.bspruce.no2$sow.year==2017,"treat"], levels = c("2","4"))

z1a <- factor(ch.tun.larch.ex3[ch.tun.larch.ex3$sow.year==2018,"treat"], levels = c("2","4"))
z1b <- factor(ch.tun.larch.no3[ch.tun.larch.no3$sow.year==2018,"treat"], levels = c("2","4"))
z2a <- factor(ch.tis.larch.ex3[ch.tis.larch.ex3$sow.year==2018,"treat"], levels = c("2","4"))
z2b <- factor(ch.tis.larch.no3[ch.tis.larch.no3$sow.year==2018,"treat"], levels = c("2","4"))
z3a <- factor(ch.rid.larch.ex3[ch.rid.larch.ex3$sow.year==2018,"treat"], levels = c("2","4"))
z3b <- factor(ch.rid.larch.no3[ch.rid.larch.no3$sow.year==2018,"treat"], levels = c("2","4"))
z4a <- factor(ch.wsu.larch.ex3[ch.wsu.larch.ex3$sow.year==2018,"treat"], levels = c("2","4"))
z4b <- factor(ch.wsu.larch.no3[ch.wsu.larch.no3$sow.year==2018,"treat"], levels = c("2","4"))

z5a <- factor(ch.tun.wspruce.ex3[ch.tun.wspruce.ex3$sow.year==2018,"treat"], levels = c("2","4"))
z5b <- factor(ch.tun.wspruce.no3[ch.tun.wspruce.no3$sow.year==2018,"treat"], levels = c("2","4"))
z6a <- factor(ch.tis.wspruce.ex3[ch.tis.wspruce.ex3$sow.year==2018,"treat"], levels = c("2","4"))
z6b <- factor(ch.tis.wspruce.no3[ch.tis.wspruce.no3$sow.year==2018,"treat"], levels = c("2","4"))
z7a <- factor(ch.rid.wspruce.ex3[ch.rid.wspruce.ex3$sow.year==2018,"treat"], levels = c("2","4"))
z7b <- factor(ch.rid.wspruce.no3[ch.rid.wspruce.no3$sow.year==2018,"treat"], levels = c("2","4"))
z8a <- factor(ch.wsu.wspruce.ex3[ch.wsu.wspruce.ex3$sow.year==2018,"treat"], levels = c("2","4"))
z8b <- factor(ch.wsu.wspruce.no3[ch.wsu.wspruce.no3$sow.year==2018,"treat"], levels = c("2","4"))

z9a <- factor(ch.tun.bspruce.ex3[ch.tun.bspruce.ex3$sow.year==2018,"treat"], levels = c("2","4"))
z9b <- factor(ch.tun.bspruce.no3[ch.tun.bspruce.no3$sow.year==2018,"treat"], levels = c("2","4"))
z10a <- factor(ch.tis.bspruce.ex3[ch.tis.bspruce.ex3$sow.year==2018,"treat"], levels = c("2","4"))
z10b <- factor(ch.tis.bspruce.no3[ch.tis.bspruce.no3$sow.year==2018,"treat"], levels = c("2","4"))
z11a <- factor(ch.rid.bspruce.ex3[ch.rid.bspruce.ex3$sow.year==2018,"treat"], levels = c("2","4"))
z11b <- factor(ch.rid.bspruce.no3[ch.rid.bspruce.no3$sow.year==2018,"treat"], levels = c("2","4"))
z12a <- factor(ch.wsu.bspruce.ex3[ch.wsu.bspruce.ex3$sow.year==2018,"treat"], levels = c("2","4"))
z12b <- factor(ch.wsu.bspruce.no3[ch.wsu.bspruce.no3$sow.year==2018,"treat"], levels = c("2","4"))


#__________

ch_wsu_larch_ex1 <- subset(ch.wsu, species == "ll" & exclosure == "yes" & sow.year >= 2016)
ch_wsu_larch_no1 <- subset(ch.wsu, species == "ll" & exclosure == "no" & sow.year >= 2016)
ch_tun_larch_ex1 <- subset(ch.tun, species == "ll" & exclosure == "yes" & sow.year >= 2016)
ch_tun_larch_no1 <- subset(ch.tun, species == "ll" & exclosure == "no" & sow.year >= 2016)
ch_tis_larch_ex1 <- subset(ch.tis, species == "ll" & exclosure == "yes" & sow.year >= 2016)
ch_tis_larch_no1 <- subset(ch.tis, species == "ll" & exclosure == "no" & sow.year >= 2016)
ch_rid_larch_ex1 <- subset(ch.rid, species == "ll" & exclosure == "yes" & sow.year >= 2016)
ch_rid_larch_no1 <- subset(ch.rid, species == "ll" & exclosure == "no" & sow.year >= 2016)

ch_wsu_wspruce_ex1 <- subset(ch.wsu, species == "ws" & exclosure == "yes" & sow.year >= 2016)
ch_wsu_wspruce_no1 <- subset(ch.wsu, species == "ws" & exclosure == "no" & sow.year >= 2016)
ch_tun_wspruce_ex1 <- subset(ch.tun, species == "ws" & exclosure == "yes" & sow.year >= 2016)
ch_tun_wspruce_no1 <- subset(ch.tun, species == "ws" & exclosure == "no" & sow.year >= 2016)
ch_tis_wspruce_ex1 <- subset(ch.tis, species == "ws" & exclosure == "yes" & sow.year >= 2016)
ch_tis_wspruce_no1 <- subset(ch.tis, species == "ws" & exclosure == "no" & sow.year >= 2016)
ch_rid_wspruce_ex1 <- subset(ch.rid, species == "ws" & exclosure == "yes" & sow.year >= 2016)
ch_rid_wspruce_no1 <- subset(ch.rid, species == "ws" & exclosure == "no" & sow.year >= 2016)

ch_wsu_bspruce_ex1 <- subset(ch.wsu, species == "bs" & exclosure == "yes" & sow.year >= 2016)
ch_wsu_bspruce_no1 <- subset(ch.wsu, species == "bs" & exclosure == "no" & sow.year >= 2016)
ch_tun_bspruce_ex1 <- subset(ch.tun, species == "bs" & exclosure == "yes" & sow.year >= 2016)
ch_tun_bspruce_no1 <- subset(ch.tun, species == "bs" & exclosure == "no" & sow.year >= 2016)
ch_tis_bspruce_ex1 <- subset(ch.tis, species == "bs" & exclosure == "yes" & sow.year >= 2016)
ch_tis_bspruce_no1 <- subset(ch.tis, species == "bs" & exclosure == "no" & sow.year >= 2016)
ch_rid_bspruce_ex1 <- subset(ch.rid, species == "bs" & exclosure == "yes" & sow.year >= 2016)
ch_rid_bspruce_no1 <- subset(ch.rid, species == "bs" & exclosure == "no" & sow.year >= 2016)

full_x1a <- factor(ch_tun_larch_ex1[ch_tun_larch_ex1$sow.year >= 2016,"treat"], levels = c("2","4"))
full_x1b <- factor(ch_tun_larch_no1[ch_tun_larch_no1$sow.year >= 2016,"treat"], levels = c("2","4"))
full_x2a <- factor(ch_tis_larch_ex1[ch_tis_larch_ex1$sow.year >= 2016,"treat"], levels = c("2","4"))
full_x2b <- factor(ch_tis_larch_no1[ch_tis_larch_no1$sow.year >= 2016,"treat"], levels = c("2","4"))
full_x3a <- factor(ch_rid_larch_ex1[ch_rid_larch_ex1$sow.year >= 2016,"treat"], levels = c("2","4"))
full_x3b <- factor(ch_rid_larch_no1[ch_rid_larch_no1$sow.year >= 2016,"treat"], levels = c("2","4"))
full_x4a <- factor(ch_wsu_larch_ex1[ch_wsu_larch_ex1$sow.year >= 2016,"treat"], levels = c("2","4"))
full_x4b <- factor(ch_wsu_larch_no1[ch_wsu_larch_no1$sow.year >= 2016,"treat"], levels = c("2","4"))

##**************************
# Set up some plotting constants
x_lim = c(0.4,4.5)
y_lim = c(0,1.1)

#_______________________----
# Proportion white spruce germination cage/uncaged ----
jpeg(sprintf("~/Desktop/Workspace/Earthwatch/Churchill/figures/wspruce_germination_%s.jpg", year), width = 5, height = 7, units = "in", res = 300)
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(0.5, 2, 1, 1), oma = c(2,1,0,0))

## TUN - White spruce out of exclosures
boxplot(ch_tun_wspruce_no1$germ.prop.via ~ full_x1b, at = c(1,3), xlim = x_lim, ylim = y_lim, 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(ch_tun_wspruce_no1$germ.prop.via ~ full_x1b, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## TUN - White spruce in exclosures
boxplot(ch_tun_wspruce_ex1$germ.prop.via ~ full_x1a, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(ch_tun_wspruce_ex1$germ.prop.via ~ full_x1a, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
legend("topleft", "(a) TUN", bty = "n", inset = c(0,0))
legend("topright", c("Uncaged","Caged"), pt.bg = c("red", "blue"), col = "black",
       bty = "n", pch = 22, pt.cex = c(1,1), text.width = 1, 
       y.intersp = 0.7, inset = c(0.1,0.01), horiz = F)

## TIS - White spruce out of exclosures
boxplot(ch_tis_wspruce_no1$germ.prop.via ~ full_x1b, at = c(1,3), xlim = x_lim, ylim = y_lim, 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(ch_tis_wspruce_no1$germ.prop.via ~ full_x1b, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## TIS - White spruce in exclosures
boxplot(ch_tis_wspruce_ex1$germ.prop.via ~ full_x1a, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(ch_tis_wspruce_ex1$germ.prop.via ~ full_x1a, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
legend("topleft", "(b) TIS", bty = "n", inset = c(0,0))

## RID - White spruce out of exclosures
boxplot(ch_rid_wspruce_no1$germ.prop.via ~ full_x1b, at = c(1,3), xlim = x_lim, ylim = y_lim, 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(ch_rid_wspruce_no1$germ.prop.via ~ full_x1b, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## RID - White spruce in exclosures
boxplot(ch_rid_wspruce_ex1$germ.prop.via ~ full_x1a, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(ch_rid_wspruce_ex1$germ.prop.via ~ full_x1a, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
legend("topleft", "(c) RID", bty = "n", inset = c(0,0))

## WSU - White spruce out of exclosures
boxplot(ch_wsu_wspruce_no1$germ.prop.via ~ full_x1b, at = c(1,3), xlim = x_lim, ylim = y_lim, 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(ch_wsu_wspruce_no1$germ.prop.via ~ full_x1b, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## WSU - White spruce in exclosures
boxplot(ch_wsu_wspruce_ex1$germ.prop.via ~ full_x1a, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(ch_wsu_wspruce_ex1$germ.prop.via ~ full_x1a, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
legend("topleft", "(d) WSU", bty = "n", inset = c(0,0))

axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
axis(1, at = c(1.5,3.5), labels = c("Vegetated","Scarified"), tick = TRUE)
mtext(side = 2, "Germination proportion of viable", outer = TRUE)
dev.off()

#_______________________----
# Proportion black spruce germination cage/uncaged ----
jpeg(sprintf("~/Desktop/Workspace/Earthwatch/Churchill/figures/bspruce_germination_%s.jpg", year), width = 5, height = 7, units = "in", res = 300)
par(mfrow = c(3, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(0.5, 2, 1, 1), oma = c(2,1,0,0))

## TUN - Black spruce out of exclosures
boxplot(ch_tun_bspruce_no1$germ.prop.via ~ full_x1b, at = c(1,3), xlim = x_lim, ylim = y_lim, 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(ch_tun_bspruce_no1$germ.prop.via ~ full_x1b, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## TUN - Black spruce in exclosures
boxplot(ch_tun_bspruce_ex1$germ.prop.via ~ full_x1a, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(ch_tun_bspruce_ex1$germ.prop.via ~ full_x1a, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
legend("topleft", "(a) TUN", bty = "n", inset = c(0,0))
legend("topright", c("Uncaged","Caged"), pt.bg = c("red", "blue"), col = "black",
       bty = "n", pch = 22, pt.cex = c(1,1), text.width = 1, 
       y.intersp = 0.7, inset = c(0.1,0.01), horiz = F)

# ## TIS - Black spruce out of exclosures
# boxplot(ch_tis_bspruce_no1$germ.prop.via ~ full_x1b, at = c(1,3), xlim = x_lim, ylim = y_lim, 
#         xaxt = "n", yaxt = "n", col = "red")
# stripchart(ch_tis_bspruce_no1$germ.prop.via ~ full_x1b, at = c(1,3), vertical = TRUE, method = "jitter", 
#            add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
# ## TIS - Black spruce in exclosures
# boxplot(ch_tis_bspruce_ex1$germ.prop.via ~ full_x1a, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
# stripchart(ch_tis_bspruce_ex1$germ.prop.via ~ full_x1a, at = c(2,4), vertical = TRUE, method = "jitter", 
#            add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
# axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
# legend("topleft", "(b) TIS", bty = "n", inset = c(0,0))

## RID - Black spruce out of exclosures
boxplot(ch_rid_bspruce_no1$germ.prop.via ~ full_x1b, at = c(1,3), xlim = x_lim, ylim = y_lim, 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(ch_rid_bspruce_no1$germ.prop.via ~ full_x1b, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## RID - Black spruce in exclosures
boxplot(ch_rid_bspruce_ex1$germ.prop.via ~ full_x1a, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(ch_rid_bspruce_ex1$germ.prop.via ~ full_x1a, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
legend("topleft", "(b) RID", bty = "n", inset = c(0,0))

## WSU - Black spruce out of exclosures
boxplot(ch_wsu_bspruce_no1$germ.prop.via ~ full_x1b, at = c(1,3), xlim = x_lim, ylim = y_lim, 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(ch_wsu_bspruce_no1$germ.prop.via ~ full_x1b, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## WSU - Black spruce in exclosures
boxplot(ch_wsu_bspruce_ex1$germ.prop.via ~ full_x1a, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(ch_wsu_bspruce_ex1$germ.prop.via ~ full_x1a, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
legend("topleft", "(c) WSU", bty = "n", inset = c(0,0))

axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
axis(1, at = c(1.5,3.5), labels = c("Vegetated","Scarified"), tick = TRUE)
mtext(side = 2, "Germination proportion of viable", outer = TRUE)
dev.off()

#_______________________----
# Proportion larch germination cage/uncaged ----
jpeg(sprintf("~/Desktop/Workspace/Earthwatch/Churchill/figures/larch_germination_%s.jpg", year), width = 5, height = 7, units = "in", res = 300)
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(0.5, 2, 1, 1), oma = c(2,1,0,0))

## TUN - Larch out of exclosures
boxplot(ch_tun_larch_no1$germ.prop.via ~ full_x1b, at = c(1,3), xlim = x_lim, ylim = y_lim, 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(ch_tun_larch_no1$germ.prop.via ~ full_x1b, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## TUN - Larch in exclosures
boxplot(ch_tun_larch_ex1$germ.prop.via ~ full_x1a, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(ch_tun_larch_ex1$germ.prop.via ~ full_x1a, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
legend("topleft", "(a) TUN", bty = "n", inset = c(0,0))
legend("topright", c("Uncaged","Caged"), pt.bg = c("red", "blue"), col = "black",
       bty = "n", pch = 22, pt.cex = c(1,1), text.width = 1, 
       y.intersp = 0.7, inset = c(0.1,0.01), horiz = F)

## TIS - Larch out of exclosures
boxplot(ch_tis_larch_no1$germ.prop.via ~ full_x1b, at = c(1,3), xlim = x_lim, ylim = y_lim, 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(ch_tis_larch_no1$germ.prop.via ~ full_x1b, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## TIS - Larch in exclosures
boxplot(ch_tis_larch_ex1$germ.prop.via ~ full_x1a, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(ch_tis_larch_ex1$germ.prop.via ~ full_x1a, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
legend("topleft", "(b) TIS", bty = "n", inset = c(0,0))

## RID - Larch out of exclosures
boxplot(ch_rid_larch_no1$germ.prop.via ~ full_x1b, at = c(1,3), xlim = x_lim, ylim = y_lim, 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(ch_rid_larch_no1$germ.prop.via ~ full_x1b, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## RID - Larch in exclosures
boxplot(ch_rid_larch_ex1$germ.prop.via ~ full_x1a, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(ch_rid_larch_ex1$germ.prop.via ~ full_x1a, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
legend("topleft", "(c) RID", bty = "n", inset = c(0,0))

## WSU - Larch out of exclosures
boxplot(ch_wsu_larch_no1$germ.prop.via ~ full_x1b, at = c(1,3), xlim = x_lim, ylim = y_lim, 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(ch_wsu_larch_no1$germ.prop.via ~ full_x1b, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## WSU - Larch in exclosures
boxplot(ch_wsu_larch_ex1$germ.prop.via ~ full_x1a, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(ch_wsu_larch_ex1$germ.prop.via ~ full_x1a, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
legend("topleft", "(d) WSU", bty = "n", inset = c(0,0))

axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
axis(1, at = c(1.5,3.5), labels = c("Vegetated","Scarified"), tick = TRUE)
mtext(side = 2, "Germination proportion of viable", outer = TRUE)
dev.off()








# Proportion larch germination each year in and out of cages - Export at 5 x 7
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(0, 2, 1, 1), oma = c(4,2,0.5,1))

## TUN
boxplot(ch.tun.larch.no1$germ.prop.via ~ x1b, at = c(1,9), xlim = c(0,16), ylim = c(0,1), axes = F, xlab = "", ylab = "", col = c("darkorange","yellow"))
boxplot(ch.tun.larch.no2$germ.prop.via ~ y1b, at = c(2,10), xaxt = "n", yaxt = "n", add = TRUE,col = c("darkorange","yellow"))
boxplot(ch.tun.larch.no3$germ.prop.via ~ z1b, at = c(3,11), xaxt = "n", yaxt = "n", add = TRUE,col = c("darkorange","yellow"))
boxplot(ch.tun.larch.ex1$germ.prop.via ~ x1a, at = c(5,13), xaxt = "n", yaxt = "n", add = TRUE,col = c("darkorange4","yellow4"))
boxplot(ch.tun.larch.ex2$germ.prop.via ~ y1a, at = c(6,14), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
boxplot(ch.tun.larch.ex3$germ.prop.via ~ z1a, at = c(7,15), xaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
axis(1, at = c(2,6,10,14), labels = c("","","",""), tick = TRUE)
legend("topleft", "(a) TUN - tundra", bty = "n", inset = c(0.0,-0.1))
# legend("topright", c("Uncaged","Caged"), fill=c("red","blue"), horiz=FALSE, bty = "n", inset = c(0.1,0))

## TIS
boxplot(ch.tis.larch.no1$germ.prop.via ~ x2b, at = c(1,9), xlim = c(0,16), ylim = c(0,1), axes = F, xlab = "", ylab = "", col = c("darkorange","yellow"))
boxplot(ch.tis.larch.no2$germ.prop.via ~ y2b, at = c(2,10), xaxt = "n", yaxt = "n", add = TRUE,col = c("darkorange","yellow"))
boxplot(ch.tis.larch.no3$germ.prop.via ~ z2b, at = c(3,11), xaxt = "n", yaxt = "n", add = TRUE,col = c("darkorange","yellow"))
boxplot(ch.tis.larch.ex1$germ.prop.via ~ x2a, at = c(5,13), xaxt = "n", yaxt = "n", add = TRUE,col = c("darkorange4","yellow4"))
boxplot(ch.tis.larch.ex2$germ.prop.via ~ y2a, at = c(6,14), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
boxplot(ch.tis.larch.ex3$germ.prop.via ~ z2a, at = c(7,15), xaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
axis(1, at = c(2,6,10,14), labels = c("","","",""), tick = TRUE)
legend("topleft", "(b) TIS - tree island", bty = "n", inset = c(0.0,-0.1))

## RID
boxplot(ch.rid.larch.no1$germ.prop.via ~ x3b, at = c(1,9), xlim = c(0,16), ylim = c(0,1), axes = F, xlab = "", ylab = "", col = c("darkorange","yellow"))
boxplot(ch.rid.larch.no2$germ.prop.via ~ y3b, at = c(2,10), xaxt = "n", yaxt = "n", add = T, col = c("darkorange","yellow"))
boxplot(ch.rid.larch.no3$germ.prop.via ~ z3b, at = c(3,11), xaxt = "n", yaxt = "n", add = T, col = c("darkorange","yellow"))
boxplot(ch.rid.larch.ex1$germ.prop.via ~ x3a, at = c(5,13), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
boxplot(ch.rid.larch.ex2$germ.prop.via ~ y3a, at = c(6,14), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
boxplot(ch.rid.larch.ex3$germ.prop.via ~ z3a, at = c(7,15), xaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
axis(1, at = c(2,6,10,14), labels = c("","","",""), tick = TRUE)
legend("topleft", "(c) RID - treeline", bty = "n", inset = c(0.0,-0.1))

## WSU
boxplot(ch.wsu.larch.no1$germ.prop.via ~ x4b, at = c(1,9), xlim = c(0,16), ylim = c(0,1), axes = F, xlab = "", ylab = "", col = c("darkorange","yellow"))
boxplot(ch.wsu.larch.no2$germ.prop.via ~ y4b, at = c(2,10), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
boxplot(ch.wsu.larch.no3$germ.prop.via ~ z4b, at = c(3,11), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
boxplot(ch.wsu.larch.ex1$germ.prop.via ~ x4a, at = c(5,13), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
boxplot(ch.wsu.larch.ex2$germ.prop.via ~ y4a, at = c(6,14), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
boxplot(ch.wsu.larch.ex3$germ.prop.via ~ z4a, at = c(7,15), xaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
axis(1, at = c(2,6,10,14), labels = c("","","",""), tick = TRUE)
legend("topleft", "(d) WSU - forest", bty = "n", inset = c(0.0,-0.1))
axis(1, at = c(4,12), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1)
axis(1, at = c(2,6,10,14), labels = c("Uncaged","Caged","Uncaged","Caged"), tick = FALSE)
mtext(side = 2, "Germination proportion of viable", outer = TRUE)

# par(xpd = NA)
# rect(0.5,-0.55,4.4,-0.45, col = 'orange', border = NA)
# rect(4.6,-0.55,8.5,-0.45, col = 'green', border = NA)

# Proportion white spruce germination each year in and out of cages - Export at 5 x 7
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(0, 2, 1, 1), oma = c(4,2,0.5,1))

## TUN
boxplot(ch.tun.wspruce.no1$germ.prop.via ~ x1b, at = c(1,9), xlim = c(0,16), ylim = c(0,1), axes = F, xlab = "", ylab = "", col = c("darkorange","yellow"))
boxplot(ch.tun.wspruce.no2$germ.prop.via ~ y1b, at = c(2,10), xaxt = "n", yaxt = "n", add = TRUE,col = c("darkorange","yellow"))
boxplot(ch.tun.wspruce.no3$germ.prop.via ~ z1b, at = c(3,11), xaxt = "n", yaxt = "n", add = TRUE,col = c("darkorange","yellow"))
boxplot(ch.tun.wspruce.ex1$germ.prop.via ~ x1a, at = c(5,13), xaxt = "n", yaxt = "n", add = TRUE,col = c("darkorange4","yellow4"))
boxplot(ch.tun.wspruce.ex2$germ.prop.via ~ y1a, at = c(6,14), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
boxplot(ch.tun.wspruce.ex3$germ.prop.via ~ z1a, at = c(7,15), xaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
axis(1, at = c(2,6,10,14), labels = c("","","",""), tick = TRUE)
legend("topleft", "(a) TUN - tundra", bty = "n", inset = c(0.0,-0.1))
# legend("topright", c("Uncaged","Caged"), fill=c("red","blue"), horiz=FALSE, bty = "n", inset = c(0.1,0))

## TIS
boxplot(ch.tis.wspruce.no1$germ.prop.via ~ x2b, at = c(1,9), xlim = c(0,16), ylim = c(0,1), axes = F, xlab = "", ylab = "", col = c("darkorange","yellow"))
boxplot(ch.tis.wspruce.no2$germ.prop.via ~ y2b, at = c(2,10), xaxt = "n", yaxt = "n", add = TRUE,col = c("darkorange","yellow"))
boxplot(ch.tis.wspruce.no3$germ.prop.via ~ z2b, at = c(3,11), xaxt = "n", yaxt = "n", add = TRUE,col = c("darkorange","yellow"))
boxplot(ch.tis.wspruce.ex1$germ.prop.via ~ x2a, at = c(5,13), xaxt = "n", yaxt = "n", add = TRUE,col = c("darkorange4","yellow4"))
boxplot(ch.tis.wspruce.ex2$germ.prop.via ~ y2a, at = c(6,14), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
boxplot(ch.tis.wspruce.ex3$germ.prop.via ~ z2a, at = c(7,15), xaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
axis(1, at = c(2,6,10,14), labels = c("","","",""), tick = TRUE)
legend("topleft", "(b) TIS - tree island", bty = "n", inset = c(0.0,-0.1))

## RID
boxplot(ch.rid.wspruce.no1$germ.prop.via ~ x3b, at = c(1,9), xlim = c(0,16), ylim = c(0,1), axes = F, xlab = "", ylab = "", col = c("darkorange","yellow"))
boxplot(ch.rid.wspruce.no2$germ.prop.via ~ y3b, at = c(2,10), xaxt = "n", yaxt = "n", add = T, col = c("darkorange","yellow"))
boxplot(ch.rid.wspruce.no3$germ.prop.via ~ z3b, at = c(3,11), xaxt = "n", yaxt = "n", add = T, col = c("darkorange","yellow"))
boxplot(ch.rid.wspruce.ex1$germ.prop.via ~ x3a, at = c(5,13), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
boxplot(ch.rid.wspruce.ex2$germ.prop.via ~ y3a, at = c(6,14), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
boxplot(ch.rid.wspruce.ex3$germ.prop.via ~ z3a, at = c(7,15), xaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
axis(1, at = c(2,6,10,14), labels = c("","","",""), tick = TRUE)
legend("topleft", "(c) RID - treeline", bty = "n", inset = c(0.0,-0.1))

## WSU
boxplot(ch.wsu.wspruce.no1$germ.prop.via ~ x4b, at = c(1,9), xlim = c(0,16), ylim = c(0,1), axes = F, xlab = "", ylab = "", col = c("darkorange","yellow"))
boxplot(ch.wsu.wspruce.no2$germ.prop.via ~ y4b, at = c(2,10), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
boxplot(ch.wsu.wspruce.no3$germ.prop.via ~ z4b, at = c(3,11), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
boxplot(ch.wsu.wspruce.ex1$germ.prop.via ~ x4a, at = c(5,13), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
boxplot(ch.wsu.wspruce.ex2$germ.prop.via ~ y4a, at = c(6,14), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
boxplot(ch.wsu.wspruce.ex3$germ.prop.via ~ z4a, at = c(7,15), xaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
axis(1, at = c(2,6,10,14), labels = c("","","",""), tick = TRUE)
legend("topleft", "(d) WSU - forest", bty = "n", inset = c(0.0,-0.1))
axis(1, at = c(4,12), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1)
axis(1, at = c(2,6,10,14), labels = c("Uncaged","Caged","Uncaged","Caged"), tick = FALSE)
mtext(side = 2, "Germination proportion of viable", outer = TRUE)

# par(xpd = NA)
# rect(0.5,-0.55,4.4,-0.45, col = 'orange', border = NA)
# rect(4.6,-0.55,8.5,-0.45, col = 'green', border = NA)

# Proportion black spruce germination each year in and out of cages - Export at 5 x 7
par(mfrow = c(3, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(0, 2, 1, 1), oma = c(4,2,0.5,1))

## TUN
boxplot(ch.tun.bspruce.no1$germ.prop.via ~ x1b, at = c(1,9), xlim = c(0,16), ylim = c(0,1), axes = F, xlab = "", ylab = "", col = c("darkorange","yellow"))
boxplot(ch.tun.bspruce.no2$germ.prop.via ~ y1b, at = c(2,10), xaxt = "n", yaxt = "n", add = TRUE,col = c("darkorange","yellow"))
boxplot(ch.tun.bspruce.no3$germ.prop.via ~ z1b, at = c(3,11), xaxt = "n", yaxt = "n", add = TRUE,col = c("darkorange","yellow"))
boxplot(ch.tun.bspruce.ex1$germ.prop.via ~ x1a, at = c(5,13), xaxt = "n", yaxt = "n", add = TRUE,col = c("darkorange4","yellow4"))
boxplot(ch.tun.bspruce.ex2$germ.prop.via ~ y1a, at = c(6,14), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
boxplot(ch.tun.bspruce.ex3$germ.prop.via ~ z1a, at = c(7,15), xaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
axis(1, at = c(2,6,10,14), labels = c("","","",""), tick = TRUE)
legend("topleft", "(a) TUN - tundra", bty = "n", inset = c(0.0,-0.1))
# legend("topright", c("Uncaged","Caged"), fill=c("red","blue"), horiz=FALSE, bty = "n", inset = c(0.1,0))

## RID
boxplot(ch.rid.bspruce.no1$germ.prop.via ~ x3b, at = c(1,9), xlim = c(0,16), ylim = c(0,1), axes = F, xlab = "", ylab = "", col = c("darkorange","yellow"))
boxplot(ch.rid.bspruce.no2$germ.prop.via ~ y3b, at = c(2,10), xaxt = "n", yaxt = "n", add = T, col = c("darkorange","yellow"))
boxplot(ch.rid.bspruce.no3$germ.prop.via ~ z3b, at = c(3,11), xaxt = "n", yaxt = "n", add = T, col = c("darkorange","yellow"))
boxplot(ch.rid.bspruce.ex1$germ.prop.via ~ x3a, at = c(5,13), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
boxplot(ch.rid.bspruce.ex2$germ.prop.via ~ y3a, at = c(6,14), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
boxplot(ch.rid.bspruce.ex3$germ.prop.via ~ z3a, at = c(7,15), xaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
axis(1, at = c(2,6,10,14), labels = c("","","",""), tick = TRUE)
legend("topleft", "(c) RID - treeline", bty = "n", inset = c(0.0,-0.1))

## WSU
boxplot(ch.wsu.bspruce.no1$germ.prop.via ~ x4b, at = c(1,9), xlim = c(0,16), ylim = c(0,1), axes = F, xlab = "", ylab = "", col = c("darkorange","yellow"))
boxplot(ch.wsu.bspruce.no2$germ.prop.via ~ y4b, at = c(2,10), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
boxplot(ch.wsu.bspruce.no3$germ.prop.via ~ z4b, at = c(3,11), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
boxplot(ch.wsu.bspruce.ex1$germ.prop.via ~ x4a, at = c(5,13), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
boxplot(ch.wsu.bspruce.ex2$germ.prop.via ~ y4a, at = c(6,14), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
boxplot(ch.wsu.bspruce.ex3$germ.prop.via ~ z4a, at = c(7,15), xaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
axis(1, at = c(2,6,10,14), labels = c("","","",""), tick = TRUE)
legend("topleft", "(d) WSU - forest", bty = "n", inset = c(0.0,-0.1))
axis(1, at = c(4,12), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1)
axis(1, at = c(2,6,10,14), labels = c("Uncaged","Caged","Uncaged","Caged"), tick = FALSE)
mtext(side = 2, "Germination proportion of viable", outer = TRUE)

# par(xpd = NA)
# rect(0.5,-0.55,4.4,-0.45, col = 'orange', border = NA)
# rect(4.6,-0.55,8.5,-0.45, col = 'green', border = NA)



ch.wsu.larch.ex <- subset(ch.wsu, species == "ll" & exclosure == "yes" & sow.year != 2018)
ch.wsu.larch.no <- subset(ch.wsu, species == "ll" & exclosure == "no"& sow.year != 2018)
ch.tun.larch.ex <- subset(ch.tun, species == "ll" & exclosure == "yes")
ch.tun.larch.no <- subset(ch.tun, species == "ll" & exclosure == "no")
ch.tis.larch.ex <- subset(ch.tis, species == "ll" & exclosure == "yes")
ch.tis.larch.no <- subset(ch.tis, species == "ll" & exclosure == "no")
ch.rid.larch.ex <- subset(ch.rid, species == "ll" & exclosure == "yes")
ch.rid.larch.no <- subset(ch.rid, species == "ll" & exclosure == "no")

ch.wsu.wspruce.ex <- subset(ch.wsu, species == "ws" & exclosure == "yes"& sow.year != 2018)
ch.wsu.wspruce.no <- subset(ch.wsu, species == "ws" & exclosure == "no"& sow.year != 2018)
ch.tun.wspruce.ex <- subset(ch.tun, species == "ws" & exclosure == "yes")
ch.tun.wspruce.no <- subset(ch.tun, species == "ws" & exclosure == "no")
ch.tis.wspruce.ex <- subset(ch.tis, species == "ws" & exclosure == "yes")
ch.tis.wspruce.no <- subset(ch.tis, species == "ws" & exclosure == "no")
ch.rid.wspruce.ex <- subset(ch.rid, species == "ws" & exclosure == "yes")
ch.rid.wspruce.no <- subset(ch.rid, species == "ws" & exclosure == "no")

ch.wsu.bspruce.ex <- subset(ch.wsu, species == "bs" & exclosure == "yes"& sow.year != 2018)
ch.wsu.bspruce.no <- subset(ch.wsu, species == "bs" & exclosure == "no"& sow.year != 2018)
ch.tun.bspruce.ex <- subset(ch.tun, species == "bs" & exclosure == "yes")
ch.tun.bspruce.no <- subset(ch.tun, species == "bs" & exclosure == "no")
ch.tis.bspruce.ex <- subset(ch.tis, species == "bs" & exclosure == "yes")
ch.tis.bspruce.no <- subset(ch.tis, species == "bs" & exclosure == "no")
ch.rid.bspruce.ex <- subset(ch.rid, species == "bs" & exclosure == "yes")
ch.rid.bspruce.no <- subset(ch.rid, species == "bs" & exclosure == "no")

x10a <- factor(ch.tun.larch.ex[,"treat"], levels = c("2","4"))
x10b <- factor(ch.tun.larch.no[,"treat"], levels = c("2","4"))
x20a <- factor(ch.tis.larch.ex[,"treat"], levels = c("2","4"))
x20b <- factor(ch.tis.larch.no[,"treat"], levels = c("2","4"))
x30a <- factor(ch.rid.larch.ex[,"treat"], levels = c("2","4"))
x30b <- factor(ch.rid.larch.no[,"treat"], levels = c("2","4"))
x40a <- factor(ch.wsu.larch.ex[,"treat"], levels = c("2","4"))
x40b <- factor(ch.wsu.larch.no[,"treat"], levels = c("2","4"))

x50a <- factor(ch.tun.wspruce.ex1[,"treat"], levels = c("2","4"))
x50b <- factor(ch.tun.wspruce.no1[,"treat"], levels = c("2","4"))
x60a <- factor(ch.tis.wspruce.ex1[,"treat"], levels = c("2","4"))
x60b <- factor(ch.tis.wspruce.no1[,"treat"], levels = c("2","4"))
x70a <- factor(ch.rid.wspruce.ex1[,"treat"], levels = c("2","4"))
x70b <- factor(ch.rid.wspruce.no1[,"treat"], levels = c("2","4"))
x80a <- factor(ch.wsu.wspruce.ex1[,"treat"], levels = c("2","4"))
x80b <- factor(ch.wsu.wspruce.no1[,"treat"], levels = c("2","4"))

x90a <- factor(ch.tun.bspruce.ex[,"treat"], levels = c("2","4"))
x90b <- factor(ch.tun.bspruce.no[,"treat"], levels = c("2","4"))
x100a <- factor(ch.tis.bspruce.ex[,"treat"], levels = c("2","4"))
x100b <- factor(ch.tis.bspruce.no[,"treat"], levels = c("2","4"))
x110a <- factor(ch.rid.bspruce.ex[,"treat"], levels = c("2","4"))
x110b <- factor(ch.rid.bspruce.no[,"treat"], levels = c("2","4"))
x120a <- factor(ch.wsu.bspruce.ex[,"treat"], levels = c("2","4"))
x120b <- factor(ch.wsu.bspruce.no[,"treat"], levels = c("2","4"))





# Proportion larch germination each year in and out of cages - Export at 5 x 7
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(0, 2, 1, 1), oma = c(4,2,0.5,1))

## TUN
boxplot(ch.tun.larch.no$germ.prop.via ~ x10b, at = c(1,4), xlim = c(0,6), ylim = c(0,1), axes = F, xlab = "", ylab = "", col = c("darkorange","yellow"))
boxplot(ch.tun.larch.ex$germ.prop.via ~ x10a, at = c(2,5), xaxt = "n", add = TRUE,col = c("darkorange4","yellow4"))
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(a) TUN - tundra", bty = "n", inset = c(0.0,-0.1))
# legend("topright", c("Uncaged","Caged"), fill=c("red","blue"), horiz=FALSE, bty = "n", inset = c(0.1,0))

## TIS
boxplot(ch.tis.larch.no$germ.prop.via ~ x20b, at = c(1,4), xlim = c(0,6), ylim = c(0,1), axes = F, xlab = "", ylab = "", col = c("darkorange","yellow"))
boxplot(ch.tis.larch.ex$germ.prop.via ~ x20a, at = c(2,5), xaxt = "n", add = TRUE,col = c("darkorange4","yellow4"))
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(b) TIS - tree island", bty = "n", inset = c(0.0,-0.1))

## RID
boxplot(ch.rid.larch.no$germ.prop.via ~ x30b, at = c(1,4), xlim = c(0,6), ylim = c(0,1), axes = F, xlab = "", ylab = "", col = c("darkorange","yellow"))
boxplot(ch.rid.larch.ex$germ.prop.via ~ x30a, at = c(2,5), xaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(c) RID - treeline", bty = "n", inset = c(0.0,-0.1))

## WSU
boxplot(ch.wsu.larch.no$germ.prop.via ~ x40b, at = c(1,4), xlim = c(0,6), ylim = c(0,1), axes = F, xlab = "", ylab = "", col = c("darkorange","yellow"))
boxplot(ch.wsu.larch.ex$germ.prop.via ~ x40a, at = c(2,5), xaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(d) WSU - forest", bty = "n", inset = c(0.0,-0.1))
axis(1, at = c(1.5,4.5), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1)
axis(1, at = c(1,2,4,5), labels = c("U","C","U","C"), tick = FALSE)
mtext(side = 2, "Germination proportion of viable", outer = TRUE)

# Proportion white spruce germination each year in and out of cages - Export at 5 x 7
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(0, 2, 1, 1), oma = c(4,2,0.5,1))

## TUN
boxplot(ch.tun.wspruce.no$germ.prop.via ~ x10b, at = c(1,4), xlim = c(0,6), ylim = c(0,1), axes = F, xlab = "", ylab = "", col = c("darkorange","yellow"))
boxplot(ch.tun.wspruce.ex$germ.prop.via ~ x10a, at = c(2,5), xaxt = "n", add = TRUE,col = c("darkorange4","yellow4"))
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(a) TUN - tundra", bty = "n", inset = c(0.0,-0.1))
# legend("topright", c("Uncaged","Caged"), fill=c("red","blue"), horiz=FALSE, bty = "n", inset = c(0.1,0))

## TIS
boxplot(ch.tis.wspruce.no$germ.prop.via ~ x20b, at = c(1,4), xlim = c(0,6), ylim = c(0,1), axes = F, xlab = "", ylab = "", col = c("darkorange","yellow"))
boxplot(ch.tis.wspruce.ex$germ.prop.via ~ x20a, at = c(2,5), xaxt = "n", add = TRUE,col = c("darkorange4","yellow4"))
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(b) TIS - tree island", bty = "n", inset = c(0.0,-0.1))

## RID
boxplot(ch.rid.wspruce.no$germ.prop.via ~ x30b, at = c(1,4), xlim = c(0,6), ylim = c(0,1), axes = F, xlab = "", ylab = "", col = c("darkorange","yellow"))
boxplot(ch.rid.wspruce.ex$germ.prop.via ~ x30a, at = c(2,5), xaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(c) RID - treeline", bty = "n", inset = c(0.0,-0.1))

## WSU
boxplot(ch.wsu.wspruce.no$germ.prop.via ~ x40b, at = c(1,4), xlim = c(0,6), ylim = c(0,1), axes = F, xlab = "", ylab = "", col = c("darkorange","yellow"))
boxplot(ch.wsu.wspruce.ex$germ.prop.via ~ x40a, at = c(2,5), xaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(d) WSU - forest", bty = "n", inset = c(0.0,-0.1))
axis(1, at = c(1.5,4.5), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1)
axis(1, at = c(1,2,4,5), labels = c("U","C","U","C"), tick = FALSE)
mtext(side = 2, "Germination proportion of viable", outer = TRUE)

# par(xpd = NA)
# rect(0.5,-0.55,4.4,-0.45, col = 'orange', border = NA)
# rect(4.6,-0.55,8.5,-0.45, col = 'green', border = NA)

# Proportion black spruce germination each year in and out of cages - Export at 5 x 5.75
par(mfrow = c(3, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(0, 2, 1, 1), oma = c(4,2,0.5,1))

## TUN
boxplot(ch.tun.bspruce.no$germ.prop.via ~ x10b, at = c(1,4), xlim = c(0,6), ylim = c(0,1), axes = F, xlab = "", ylab = "", col = c("darkorange","yellow"))
boxplot(ch.tun.bspruce.ex$germ.prop.via ~ x10a, at = c(2,5), xaxt = "n", add = TRUE,col = c("darkorange4","yellow4"))
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(a) TUN - tundra", bty = "n", inset = c(0.0,-0.1))
# legend("topright", c("Uncaged","Caged"), fill=c("red","blue"), horiz=FALSE, bty = "n", inset = c(0.1,0))

## RID
boxplot(ch.rid.bspruce.no$germ.prop.via ~ x30b, at = c(1,4), xlim = c(0,6), ylim = c(0,1), axes = F, xlab = "", ylab = "", col = c("darkorange","yellow"))
boxplot(ch.rid.bspruce.ex$germ.prop.via ~ x30a, at = c(2,5), xaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(b) RID - treeline", bty = "n", inset = c(0.0,-0.1))

## WSU
boxplot(ch.wsu.bspruce.no$germ.prop.via ~ x40b, at = c(1,4), xlim = c(0,6), ylim = c(0,1), axes = F, xlab = "", ylab = "", col = c("darkorange","yellow"))
boxplot(ch.wsu.bspruce.ex$germ.prop.via ~ x40a, at = c(2,5), xaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(c) WSU - forest", bty = "n", inset = c(0.0,-0.1))
axis(1, at = c(1.5,4.5), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1)
axis(1, at = c(1,2,4,5), labels = c("U","C","U","C"), tick = FALSE)
mtext(side = 2, "Germination proportion of viable", outer = TRUE)

##**************************
# Make combined Churchill and NWT GTREE plots
ch <- read.csv(file = "~/Desktop/Workspace/Earthwatch/Churchill/data/gtree_ch_exclosures.csv", header = TRUE)
mm <- read.csv(file = "~/Desktop/Workspace/Earthwatch/Macpass/data/gtree_mm_exclosures.csv", header = TRUE)

ch$treat <- as.factor(ch$treat)
ch$scarif <- as.factor(ch$scarif)
ch$seeded <- as.factor(ch$seeded)
ch$tag <- factor(ch$tag)

mm$treat <- as.factor(mm$treat)
mm$scarif <- as.factor(mm$scarif)
mm$seeded <- as.factor(mm$seeded)
mm$tag <- factor(mm$plot)

ch.tun <- droplevels(subset(ch, site == "TUN" & seeded == 1))
ch.tis <- droplevels(subset(ch, site == "TIS" & seeded == 1))
ch.rid <- droplevels(subset(ch, site == "RID" & seeded == 1))
ch.wsu <- droplevels(subset(ch, site == "WSU" & seeded == 1))
ch.tun.pg <- droplevels(subset(ch.tun, species == "ws"))
ch.tis.pg <- droplevels(subset(ch.tis, species == "ws"))
ch.rid.pg <- droplevels(subset(ch.rid, species == "ws"))
ch.wsu.pg <- droplevels(subset(ch.wsu, species == "ws"))

mm.nalp <- droplevels(subset(mm, site == "nalp" & seeded == 1))
mm.salp <- droplevels(subset(mm, site == "salp" & seeded == 1))
mm.scut <- droplevels(subset(mm, site == "scut" & seeded == 1))
mm.sshr <- droplevels(subset(mm, site == "sshr" & seeded == 1))
mm.nalp.pg <- droplevels(subset(mm.nalp, species == "ws"))
mm.salp.pg <- droplevels(subset(mm.salp, species == "ws"))
mm.scut.pg <- droplevels(subset(mm.scut, species == "ws"))
mm.sshr.pg <- droplevels(subset(mm.sshr, species == "ws"))

col_pal <- wes_palette("Darjeeling1")
jpeg("~/Desktop/Workspace/Earthwatch/Churchill/figures/GTREE_mm_ch.jpg",
     height = 7, width = 6, res = 300, units = "in")
par(mfcol = c(4, 2))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(0.5, 1.25, 1, 1), oma = c(2,2,0,0))

boxplot(germ.0 ~ treatment*exclosure, data = ch.tun.pg, 
        xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
        ylim = c(0,20), col = col_pal[c(3,5)])
axis(1, at = c(1.5, 3.5), labels = c("",""), tick = TRUE)
axis(2, cex.axis = 0.8)
legend("topleft", "(a) TUN", bty = "n", inset = c(0,0), cex = 0.8)
legend("topright", legend = c("Seeded", "Seeded and scarified"), 
       fill = col_pal[c(3,5)], bty = "n", cex = 0.8)

boxplot(germ.0 ~ treatment*exclosure, data = ch.tis.pg, 
        xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
        ylim = c(0,20), col = col_pal[c(3,5)])
axis(1, at = c(1.5, 3.5), labels = c("",""), tick = TRUE)
axis(2, cex.axis = 0.8)
legend("topleft", "(b) TIS", bty = "n", inset = c(0,0), cex = 0.8)

boxplot(germ.0 ~ treatment*exclosure, data = ch.rid.pg, 
        xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
        ylim = c(0,20), col = col_pal[c(3,5)])
axis(1, at = c(1.5, 3.5), labels = c("",""), tick = TRUE)
axis(2, cex.axis = 0.8)
legend("topleft", "(c) RID", bty = "n", inset = c(0,0), cex = 0.8)

boxplot(germ.0 ~ treatment*exclosure, data = ch.wsu.pg, 
        xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
        ylim = c(0,20), col = col_pal[c(3,5)])
axis(1, at = c(1.5, 3.5), labels = c("No cage","Cage"), tick = TRUE, cex.axis = 0.8)
axis(2, cex.axis = 0.8)
legend("topleft", "(d) WSU", bty = "n", inset = c(0,0), cex = 0.8)

mtext("Germinants per seeds planted", side = 2, outer = T, line = 1, cex = 0.8)

boxplot(germ.0 ~ treatment*exclosure, data = ch.tun.pg, 
        xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
        ylim = c(0,20), col = col_pal[c(3,5)])
axis(1, at = c(1.5, 3.5), labels = c("",""), tick = TRUE)
axis(2, cex.axis = 0.8)
legend("topleft", "(e) Nalp", bty = "n", inset = c(0,0), cex = 0.8)

boxplot(germ.0 ~ treatment*exclosure, data = ch.tis.pg, 
        xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
        ylim = c(0,20), col = col_pal[c(3,5)])
axis(1, at = c(1.5, 3.5), labels = c("",""), tick = TRUE)
axis(2, cex.axis = 0.8)
legend("topleft", "(f) Salp", bty = "n", inset = c(0,0), cex = 0.8)

boxplot(germ.0 ~ treatment*exclosure, data = ch.rid.pg, 
        xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
        ylim = c(0,20), col = col_pal[c(3,5)])
axis(1, at = c(1.5, 3.5), labels = c("",""), tick = TRUE)
axis(2, cex.axis = 0.8)
legend("topleft", "(g) Scut", bty = "n", inset = c(0,0), cex = 0.8)

boxplot(germ.0 ~ treatment*exclosure, data = ch.wsu.pg, 
        xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
        ylim = c(0,20), col = col_pal[c(3,5)])
axis(1, at = c(1.5, 3.5), labels = c("No cage","Cage"), tick = TRUE, cex.axis = 0.8)
axis(2, cex.axis = 0.8)
legend("topleft", "(h) Sshr", bty = "n", inset = c(0,0), cex = 0.8)
dev.off()
