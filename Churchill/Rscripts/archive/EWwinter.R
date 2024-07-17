library(multcomp) # Simultaneous Inference in General Parametric Models
# Produce letters from Tukey's HSD and plot above boxplot
library(vegan)
library(cluster)
library(gclus) # Used for plotting clusters
library(car) # Used for determining variance inflation factors
library(labdsv) # PCA
library(psych) # Used to run t-tests
library(dplyr) # Use to subset by rows
library(scales)

rm(list = ls())

## ******************************************
## IPY

# Plot the data and plot with 95% CI

ipy.snow <- read.csv("~/Dropbox/Desktop/Workspace/Earthwatch/ipy.snow.csv", header = TRUE)
snow.depth <- aggregate(depth ~ site1 + year, data = ipy.snow, mean)
snow.density <- aggregate(density ~ site1 + year, data = ipy.snow, mean)
snow.swe <- aggregate(swe ~ site1 + year, data = ipy.snow, mean)
snow.htc <- aggregate(htc ~ site1 + year, data = ipy.snow, mean)
snow.n <- aggregate(depth ~ site1 + year, data = ipy.snow, length)
snow.depth.sd <- aggregate(depth ~ site1 + year, data = ipy.snow, sd)
snow.density.sd <- aggregate(density ~ site1 + year, data = ipy.snow, sd)
snow.swe.sd <- aggregate(swe ~ site1 + year, data = ipy.snow, sd)
snow.htc.sd <- aggregate(htc ~ site1 + year, data = ipy.snow, sd)

snow2 <- data.frame(cbind(snow.depth, snow.density$density, 
                          snow.swe$swe, snow.htc$htc, 
                          snow.n$depth, 
                          snow.depth.sd$depth,
                          snow.density.sd$density, 
                          snow.swe.sd$swe,
                          snow.htc.sd$htc))
names(snow2) <- c("site","year","depth","density","swe","htc","n","depth.sd",
                  "density.sd","swe.sd","htc.sd")

snow2$depth.se <- snow2$depth.sd / sqrt(snow2$n)
snow2$density.se <- snow2$density.sd / sqrt(snow2$n)
snow2$swe.se <- snow2$swe.sd / sqrt(snow2$n)
snow2$htc.se <- snow2$htc.sd / sqrt(snow2$n)

snow2$depth.ci <- snow2$depth.se*qt(0.975, snow2$n-1)
snow2$depth.c5 <- snow2$depth - snow2$depth.ci
snow2$depth.c95 <- snow2$depth + snow2$depth.ci

snow2$density.ci <- snow2$density.se*qt(0.975, snow2$n-1)
snow2$density.c5 <- snow2$density - snow2$density.ci
snow2$density.c95 <- snow2$density + snow2$density.ci

snow2$swe.ci <- snow2$swe.se*qt(0.975, snow2$n-1)
snow2$swe.c5 <- snow2$swe - snow2$swe.ci
snow2$swe.c95 <- snow2$swe + snow2$swe.ci

snow2$htc.ci <- snow2$htc.se*qt(0.975, snow2$n-1)
snow2$htc.c5 <- snow2$htc - snow2$htc.ci
snow2$htc.c95 <- snow2$htc + snow2$htc.ci

# Forest
rok.f <- subset(snow2, site == "ROKF")
blk.f <- subset(snow2, site == "BLKF")
ra2.f <- subset(snow2, site == "RA2F")

# Ecotone
rok.e <- subset(snow2, site == "ROKE")
blk.e <- subset(snow2, site == "BLKE")
ra2.e <- subset(snow2, site == "RA2E")

# Tundra
rok.t <- subset(snow2, site == "ROKT")
blk.t <- subset(snow2, site == "BLKT")
ra2.t <- subset(snow2, site == "RA2T")



####################################
####################################


par(mfrow = c(3,1), mar = c(1,1,1,1), oma = c(3,3,0,0))

## Forest
# ROKF
plot(rok.f$year, rok.f$depth, type='n', xlim = c(2007,2020), 
     ylim = c(40,135), axes = F, xlab = "", ylab = "")
lines(rok.f$year, rok.f$depth.c5, col = alpha('lightgreen', 0.5), xlim = c(2007,2020), 
      ylim = c(40,135), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(rok.f$year, rok.f$depth.c95, col = alpha('lightgreen', 0.5), xlim = c(2007,2020), 
      ylim = c(40,135), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rok.f$year, rev(rok.f$year)), c(rok.f$depth.c95, rev(rok.f$depth.c5)), 
        col = alpha("lightgreen", 0.5), border = NA)
par(new=T)
plot(rok.f$year, rok.f$depth, xlim = c(2007,2020), 
     ylim = c(40,135), axes = F, type = "l", lwd=2, 
     col = alpha("green4", 0.5), xlab = "", ylab = "")
# BLKF
lines(blk.f$year, blk.f$depth.c5, col = alpha('cadetblue1', 0.5), xlim = c(2007,2020), 
      ylim = c(40,135), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(blk.f$year, blk.f$depth.c95, col = alpha('cadetblue1', 0.5), xlim = c(2007,2020), 
      ylim = c(40,135), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(blk.f$year, rev(blk.f$year)), c(blk.f$depth.c95, rev(blk.f$depth.c5)), 
        col = alpha("cadetblue1", 0.5), border = NA)
par(new=T)
plot(blk.f$year, blk.f$depth, xlim = c(2007,2020), 
     ylim = c(40,135), axes = F, type = "l", lwd=2, 
     col = alpha("deepskyblue1", 0.5), xlab = "", ylab = "")
# RA2F
lines(ra2.f$year, ra2.f$depth.c5, col = alpha('darksalmon', 0.5), xlim = c(2007,2020), 
      ylim = c(40,135), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(ra2.f$year, ra2.f$depth.c95, col = alpha('darksalmon', 0.5), xlim = c(2007,2020), 
      ylim = c(40,135), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(ra2.f$year, rev(ra2.f$year)), c(ra2.f$depth.c95, rev(ra2.f$depth.c5)), 
        col = alpha("darksalmon", 0.5), border = NA)
par(new=T)
plot(ra2.f$year, ra2.f$depth, xlim = c(2007,2020), 
     ylim = c(40,135), axes = F, type = "l", lwd=2, 
     col = alpha("darkorange3", 0.5), xlab = "", ylab = "")
box()
axis(side = 1, at = seq(2000,2040,2), labels = NA)
axis(side = 2, at = seq(0,200,50), seq(0,200,50))
legend("topleft", "Forest", bty = "n")
legend("topright", c("ROK","BLK","RA2"), 
       fill = c("lightgreen","cadetblue1","darksalmon"), 
       border = NA, bty = "n", inset = c(0,-0.1), horiz = T)


## Ecotone
# ROKF
plot(rok.e$year, rok.e$depth, type='n', xlim = c(2007,2020), 
     ylim = c(20,135), axes = F, xlab = "", ylab = "")
lines(rok.e$year, rok.e$depth.c5, col = alpha('lightgreen', 0.5), xlim = c(2007,2020), 
      ylim = c(20,135), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(rok.e$year, rok.e$depth.c95, col = alpha('lightgreen', 0.5), xlim = c(2007,2020), 
      ylim = c(20,135), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rok.e$year, rev(rok.e$year)), c(rok.e$depth.c95, rev(rok.e$depth.c5)), 
        col = alpha("lightgreen", 0.5), border = NA)
par(new=T)
plot(rok.e$year, rok.e$depth, xlim = c(2007,2020), 
     ylim = c(20,135), axes = F, type = "l", lwd=2, 
     col = alpha("green4", 0.5), xlab = "", ylab = "")
# BLKF
lines(blk.e$year, blk.e$depth.c5, col = alpha('cadetblue1', 0.5), xlim = c(2007,2020), 
      ylim = c(20,135), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(blk.e$year, blk.e$depth.c95, col = alpha('cadetblue1', 0.5), xlim = c(2007,2020), 
      ylim = c(20,135), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(blk.e$year, rev(blk.e$year)), c(blk.e$depth.c95, rev(blk.e$depth.c5)), 
        col = alpha("cadetblue1", 0.5), border = NA)
par(new=T)
plot(blk.e$year, blk.e$depth, xlim = c(2007,2020), 
     ylim = c(20,135), axes = F, type = "l", lwd=2, 
     col = alpha("deepskyblue1", 0.5), xlab = "", ylab = "")
# RA2F
lines(ra2.e$year, ra2.e$depth.c5, col = alpha('darksalmon', 0.5), xlim = c(2007,2020), 
      ylim = c(20,135), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(ra2.e$year, ra2.e$depth.c95, col = alpha('darksalmon', 0.5), xlim = c(2007,2020), 
      ylim = c(20,135), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(ra2.e$year, rev(ra2.e$year)), c(ra2.e$depth.c95, rev(ra2.e$depth.c5)), 
        col = alpha("darksalmon", 0.5), border = NA)
par(new=T)
plot(ra2.e$year, ra2.e$depth, xlim = c(2007,2020), 
     ylim = c(20,135), axes = F, type = "l", lwd=2, 
     col = alpha("darkorange3", 0.5), xlab = "", ylab = "")
box()
axis(side = 1, at = seq(2000,2020,2), labels = NA)
axis(side = 2, at = seq(0,200,50), seq(0,200,50))
legend("topleft", "Ecotone", bty = "n")

## Tundra
# ROKF
plot(rok.t$year, rok.t$depth, type='n', xlim = c(2007,2020), 
     ylim = c(0,50), axes = F, xlab = "", ylab = "")
lines(rok.t$year, rok.t$depth.c5, col = alpha('lightgreen', 0.5), xlim = c(2007,2020), 
      ylim = c(0,50), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(rok.t$year, rok.t$depth.c95, col = alpha('lightgreen', 0.5), xlim = c(2007,2020), 
      ylim = c(0,50), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rok.t$year, rev(rok.t$year)), c(rok.t$depth.c95, rev(rok.t$depth.c5)), 
        col = alpha("lightgreen", 0.5), border = NA)
par(new=T)
plot(rok.t$year, rok.t$depth, xlim = c(2007,2020), 
     ylim = c(0,50), axes = F, type = "l", lwd=2, 
     col = alpha("green4", 0.5), xlab = "", ylab = "")
# BLKF
lines(blk.t$year, blk.t$depth.c5, col = alpha('cadetblue1', 0.5), xlim = c(2007,2020), 
      ylim = c(0,50), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(blk.t$year, blk.t$depth.c95, col = alpha('cadetblue1', 0.5), xlim = c(2007,2020), 
      ylim = c(0,50), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(blk.t$year, rev(blk.t$year)), c(blk.t$depth.c95, rev(blk.t$depth.c5)), 
        col = alpha("cadetblue1", 0.5), border = NA)
par(new=T)
plot(blk.t$year, blk.t$depth, xlim = c(2007,2020), 
     ylim = c(0,50), axes = F, type = "l", lwd=2, 
     col = alpha("deepskyblue1", 0.5), xlab = "", ylab = "")
# RA2F
lines(ra2.t$year, ra2.t$depth.c5, col = alpha('darksalmon', 0.5), xlim = c(2007,2020), 
      ylim = c(0,50), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(ra2.t$year, ra2.t$depth.c95, col = alpha('darksalmon', 0.5), xlim = c(2007,2020), 
      ylim = c(0,50), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(ra2.t$year, rev(ra2.t$year)), c(ra2.t$depth.c95, rev(ra2.t$depth.c5)), 
        col = alpha("darksalmon", 0.5), border = NA)
par(new=T)
plot(ra2.t$year, ra2.t$depth, xlim = c(2007,2020), 
     ylim = c(0,50), axes = F, type = "l", lwd=2, 
     col = alpha("darkorange3", 0.5), xlab = "", ylab = "")
box()
axis(side = 1, at = seq(2000,2020,2), labels = seq(2000,2020,2))
axis(side = 2, at = seq(0,50,10), seq(0,50,10))
legend("topleft", "Tundra", bty = "n")


mtext(side = 1, "Year", line = 1.5, cex = 0.75, outer = T)
mtext(side = 2, "Depth (cm)", line = 1.5, cex = 0.75, outer = T)







## ******************************************
## Tree islands

# Plot the data and plot with 95% CI

ti.snow <- read.csv("~/Desktop/Workspace/Earthwatch/treeisland.snow.csv", header = TRUE)
snow.depth <- aggregate(depth ~ site2 + year, data = ti.snow, mean)
snow.density <- aggregate(density ~ site2 + year, data = ti.snow, mean)
snow.swe <- aggregate(swe ~ site2 + year, data = ti.snow, mean)
snow.htc <- aggregate(htc ~ site2 + year, data = ti.snow, mean)
snow.n <- aggregate(depth ~ site2 + year, data = ti.snow, length)
snow.depth.sd <- aggregate(depth ~ site2 + year, data = ti.snow, sd)
snow.density.sd <- aggregate(density ~ site2 + year, data = ti.snow, sd)
snow.swe.sd <- aggregate(swe ~ site2 + year, data = ti.snow, sd)
snow.htc.sd <- aggregate(htc ~ site2 + year, data = ti.snow, sd)

snow2 <- data.frame(cbind(snow.depth, snow.density$density, 
                          snow.swe$swe, snow.htc$htc, 
                          snow.n$depth, 
                          snow.depth.sd$depth,
                          snow.density.sd$density, 
                          snow.swe.sd$swe,
                          snow.htc.sd$htc))
names(snow2) <- c("site","year","depth","density","swe","htc","n","depth.sd",
                  "density.sd","swe.sd","htc.sd")

snow2$depth.se <- snow2$depth.sd / sqrt(snow2$n)
snow2$density.se <- snow2$density.sd / sqrt(snow2$n)
snow2$swe.se <- snow2$swe.sd / sqrt(snow2$n)
snow2$htc.se <- snow2$htc.sd / sqrt(snow2$n)

snow2$depth.ci <- snow2$depth.se*qt(0.975, snow2$n-1)
snow2$depth.c5 <- snow2$depth - snow2$depth.ci
snow2$depth.c95 <- snow2$depth + snow2$depth.ci

snow2$density.ci <- snow2$density.se*qt(0.975, snow2$n-1)
snow2$density.c5 <- snow2$density - snow2$density.ci
snow2$density.c95 <- snow2$density + snow2$density.ci

snow2$swe.ci <- snow2$swe.se*qt(0.975, snow2$n-1)
snow2$swe.c5 <- snow2$swe - snow2$swe.ci
snow2$swe.c95 <- snow2$swe + snow2$swe.ci

snow2$htc.ci <- snow2$htc.se*qt(0.975, snow2$n-1)
snow2$htc.c5 <- snow2$htc - snow2$htc.ci
snow2$htc.c95 <- snow2$htc + snow2$htc.ci

# Leeward
tis.l <- subset(snow2, site == "TISl")
t10.l <- subset(snow2, site == "T10l")
t11.l <- subset(snow2, site == "T11l")
t12.l <- subset(snow2, site == "T12l")
t13.l <- subset(snow2, site == "T13l")

# Windward
tis.w <- subset(snow2, site == "TISw")
t10.w <- subset(snow2, site == "T10w")
t11.w <- subset(snow2, site == "T11w")
t12.w <- subset(snow2, site == "T12w")
t13.w <- subset(snow2, site == "T13w")

# Deflation
tis.d <- subset(snow2, site == "TISd")
t10.d <- subset(snow2, site == "T10d")
t11.d <- subset(snow2, site == "T11d")
t12.d <- subset(snow2, site == "T12d")
t13.d <- subset(snow2, site == "T13d")

# Centre
tis.c <- subset(snow2, site == "TISc")
t10.c <- subset(snow2, site == "T10c")
t11.c <- subset(snow2, site == "T11c")
t12.c <- subset(snow2, site == "T12c")
t13.c <- subset(snow2, site == "T13c")

## ******************************
## Depth
# Leeward
summary(lm(tis.l[,3] ~ tis.l$year)) # n.s.
summary(lm(t10.l[,3] ~ t10.l$year)) # n.s.
summary(lm(t11.l[,3] ~ t11.l$year)) # n.s.
summary(lm(t12.l[,3] ~ t12.l$year)) # n.s.
summary(lm(t13.l[,3] ~ t13.l$year)) # b1 = 8.737, p = 0.0221*
# Windward
summary(lm(tis.w[,3] ~ tis.w$year)) # n.s.
summary(lm(t10.w[,3] ~ t10.w$year)) # n.s.
summary(lm(t11.w[,3] ~ t11.w$year)) # n.s.
summary(lm(t12.w[,3] ~ t12.w$year)) # b1 = 4.5183, p = 0.00326
summary(lm(t13.w[,3] ~ t13.w$year)) # b1 = 5.689, p = 0.0339
# Deflation
summary(lm(tis.d[,3] ~ tis.d$year)) # n.s.
summary(lm(t10.d[,3] ~ t10.d$year)) # n.s.
summary(lm(t11.d[,3] ~ t11.d$year)) # n.s.
summary(lm(t12.d[,3] ~ t12.d$year)) # b1 = 3.193, p = 0.0264
summary(lm(t13.d[,3] ~ t13.d$year)) # n.s.
# Centre
summary(lm(tis.c[,3] ~ tis.c$year)) # n.s.
summary(lm(t10.c[,3] ~ t10.c$year)) # b1 = 2.443, p = 0.0226
summary(lm(t11.c[,3] ~ t11.c$year)) # b1 = 5.999, p = 0.0079
summary(lm(t12.c[,3] ~ t12.c$year)) # n.s.
summary(lm(t13.c[,3] ~ t13.c$year)) # n.s.

## Density
# Leeward
summary(lm(tis.l[,4] ~ tis.l$year)) # n.s.
summary(lm(t10.l[,4] ~ t10.l$year)) # n.s.
summary(lm(t11.l[,4] ~ t11.l$year)) # n.s.
summary(lm(t12.l[,4] ~ t12.l$year)) # n.s.
summary(lm(t13.l[,4] ~ t13.l$year)) # n.s.
# Windward
summary(lm(tis.w[,4] ~ tis.w$year)) # n.s.
summary(lm(t10.w[,4] ~ t10.w$year)) # n.s.
summary(lm(t11.w[,4] ~ t11.w$year)) # n.s.
summary(lm(t12.w[,4] ~ t12.w$year)) # n.s.
summary(lm(t13.w[,4] ~ t13.w$year)) # n.s.
# Deflation
summary(lm(tis.d[,4] ~ tis.d$year)) # n.s.
summary(lm(t10.d[,4] ~ t10.d$year)) # n.s.
summary(lm(t11.d[,4] ~ t11.d$year)) # n.s.
summary(lm(t12.d[,4] ~ t12.d$year)) # n.s.
summary(lm(t13.d[,4] ~ t13.d$year)) # n.s.
# Centre
summary(lm(tis.c[,4] ~ tis.c$year)) # n.s.
summary(lm(t10.c[,4] ~ t10.c$year)) # n.s.
summary(lm(t11.c[,4] ~ t11.c$year)) # n.s.
summary(lm(t12.c[,4] ~ t12.c$year)) # n.s.
summary(lm(t13.c[,4] ~ t13.c$year)) # n.s.

## SWE
# Leeward
summary(lm(tis.l[,5] ~ tis.l$year)) # n.s.
summary(lm(t10.l[,5] ~ t10.l$year)) # n.s.
summary(lm(t11.l[,5] ~ t11.l$year)) # n.s.
summary(lm(t12.l[,5] ~ t12.l$year)) # n.s.
summary(lm(t13.l[,5] ~ t13.l$year)) # n.s.
# Windward
summary(lm(tis.w[,5] ~ tis.w$year)) # n.s.
summary(lm(t10.w[,5] ~ t10.w$year)) # n.s.
summary(lm(t11.w[,5] ~ t11.w$year)) # n.s.
summary(lm(t12.w[,5] ~ t12.w$year)) # b1 = 13.716, P = 0.0229
summary(lm(t13.w[,5] ~ t13.w$year)) # b1 = 14.125, P = 0.00679
# Deflation
summary(lm(tis.d[,5] ~ tis.d$year)) # n.s.
summary(lm(t10.d[,5] ~ t10.d$year)) # n.s.
summary(lm(t11.d[,5] ~ t11.d$year)) # n.s.
summary(lm(t12.d[,5] ~ t12.d$year)) # b1 = 9.504, P = 0.0361
summary(lm(t13.d[,5] ~ t13.d$year)) # n.s.
# Centre
summary(lm(tis.c[,5] ~ tis.c$year)) # n.s.
summary(lm(t10.c[,5] ~ t10.c$year)) # n.s.
summary(lm(t11.c[,5] ~ t11.c$year)) # b1 = 22.768, p = 0.0116
summary(lm(t12.c[,5] ~ t12.c$year)) # n.s.
summary(lm(t13.c[,5] ~ t13.c$year)) # n.s.

## HTC
# Leeward
summary(lm(tis.l[,6] ~ tis.l$year)) # n.s.
summary(lm(t10.l[,6] ~ t10.l$year)) # n.s.
summary(lm(t11.l[,6] ~ t11.l$year)) # n.s.
summary(lm(t12.l[,6] ~ t12.l$year)) # n.s.
summary(lm(t13.l[,6] ~ t13.l$year)) # b1 = -0.03622, P = 0.0122
# Windward
summary(lm(tis.w[,6] ~ tis.w$year)) # n.s.
summary(lm(t10.w[,6] ~ t10.w$year)) # n.s.
summary(lm(t11.w[,6] ~ t11.w$year)) # n.s.
summary(lm(t12.w[,6] ~ t12.w$year)) # n.s.
summary(lm(t13.w[,6] ~ t13.w$year)) # n.s.
# Deflation
summary(lm(tis.d[,6] ~ tis.d$year)) # n.s.
summary(lm(t10.d[,6] ~ t10.d$year)) # n.s.
summary(lm(t11.d[,6] ~ t11.d$year)) # n.s.
summary(lm(t12.d[,6] ~ t12.d$year)) # n.s.
summary(lm(t13.d[,6] ~ t13.d$year)) # n.s.
# Centre
summary(lm(tis.c[,6] ~ tis.c$year)) # n.s.
summary(lm(t10.c[,6] ~ t10.c$year)) # b1 = -0.03515, p = 0.0216
summary(lm(t11.c[,6] ~ t11.c$year)) # b1 = -0.015896, p = 0.00208
summary(lm(t12.c[,6] ~ t12.c$year)) # n.s.
summary(lm(t13.c[,6] ~ t13.c$year)) # n.s.


####################################
####################################


par(mfrow = c(4,1), mar = c(1,1,1,1), oma = c(3,3,0,0))

## Windward
# TISw
plot(tis.w$year, tis.w$depth, type='n', xlim = c(2012,2020), 
     ylim = c(0,150), axes = F, xlab = "", ylab = "")
lines(tis.w$year, tis.w$depth.c5, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      ylim = c(0,150), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(tis.w$year, tis.w$depth.c95, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      ylim = c(0,150), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(tis.w$year, rev(tis.w$year)), c(tis.w$depth.c95, rev(tis.w$depth.c5)), 
        col = alpha("lightgreen", 0.5), border = NA)
par(new=T)
plot(tis.w$year, tis.w$depth, xlim = c(2012,2020), 
     ylim = c(0,150), axes = F, type = "l", lwd=2, 
     col = alpha("green4", 0.5), xlab = "", ylab = "")
# T10w
lines(t10.w$year, t10.w$depth.c5, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,150), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t10.w$year, t10.w$depth.c95, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,150), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t10.w$year, rev(t10.w$year)), c(t10.w$depth.c95, rev(t10.w$depth.c5)), 
        col = alpha("cadetblue1", 0.5), border = NA)
par(new=T)
plot(t10.w$year, t10.w$depth, xlim = c(2012,2020), 
     ylim = c(0,150), axes = F, type = "l", lwd=2, 
     col = alpha("deepskyblue1", 0.5), xlab = "", ylab = "")
# T11w
lines(t11.w$year, t11.w$depth.c5, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(0,150), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t11.w$year, t11.w$depth.c95, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(0,150), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t11.w$year, rev(t11.w$year)), c(t11.w$depth.c95, rev(t11.w$depth.c5)), 
        col = alpha("darksalmon", 0.5), border = NA)
par(new=T)
plot(t11.w$year, t11.w$depth, xlim = c(2012,2020), 
     ylim = c(0,150), axes = F, type = "l", lwd=2, 
     col = alpha("darkorange3", 0.5), xlab = "", ylab = "")
# T12w
lines(t12.w$year, t12.w$depth.c5, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,150), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t12.w$year, t12.w$depth.c95, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,150), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t12.w$year, rev(t12.w$year)), c(t12.w$depth.c95, rev(t12.w$depth.c5)), 
        col = alpha("mediumorchid1", 0.5), border = NA)
par(new=T)
plot(t12.w$year, t12.w$depth, xlim = c(2012,2020), 
     ylim = c(0,150), axes = F, type = "l", lwd=2, 
     col = alpha("mediumorchid4", 0.5), xlab = "", ylab = "")
abline(coef(lm(t12.w$depth ~ t12.w$year)), col = "mediumorchid4", lty = 3, lwd = 2)
# T13w
lines(t13.w$year, t13.w$depth.c5, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      ylim = c(0,150), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t13.w$year, t13.w$depth.c95, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      ylim = c(0,150), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t13.w$year, rev(t13.w$year)), c(t13.w$depth.c95, rev(t13.w$depth.c5)), 
        col = alpha("yellow", 0.5), border = NA)
par(new=T)
plot(t13.w$year, t13.w$depth, xlim = c(2012,2020), 
     ylim = c(0,150), axes = F, type = "l", lwd=2, 
     col = alpha("yellow3", 0.5), xlab = "", ylab = "")
abline(coef(lm(t13.w$depth ~ t13.w$year)), col = "yellow3", lty = 3, lwd = 2)
box()
axis(side = 1, at = seq(2000,2020,2), labels = NA)
axis(side = 2, at = seq(0,200,50), seq(0,200,50))
legend("topleft", "Windward", bty = "n")
legend("topright", c("TIS","T10","T11","T12","T13"), 
       fill = c("lightgreen","cadetblue1","darksalmon","mediumorchid1","yellow"), 
       border = NA, bty = "n", inset = c(0.05,0.01), x.intersp = 0.75, text.width = 0.5, horiz = T)


## Leeward
# TIS
plot(tis.l$year, tis.l$depth, type='n', xlim = c(2012,2020), 
     ylim = c(0,300), axes = F, xlab = "", ylab = "")
lines(tis.l$year, tis.l$depth.c5, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      ylim = c(0,300), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(tis.l$year, tis.l$depth.c95, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      ylim = c(0,300), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(tis.l$year, rev(tis.l$year)), c(tis.l$depth.c95, rev(tis.l$depth.c5)), 
        col = alpha("lightgreen", 0.5), border = NA)
par(new=T)
plot(tis.l$year, tis.l$depth, xlim = c(2012,2020), 
     ylim = c(0,300), axes = F, type = "l", lwd=2, 
     col = alpha("green4", 0.5), xlab = "", ylab = "")
# T10
lines(t10.l$year, t10.l$depth.c5, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,300), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t10.l$year, t10.l$depth.c95, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,300), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t10.l$year, rev(t10.l$year)), c(t10.l$depth.c95, rev(t10.l$depth.c5)), 
        col = alpha("cadetblue1", 0.5), border = NA)
par(new=T)
plot(t10.l$year, t10.l$depth, xlim = c(2012,2020), 
     ylim = c(0,300), axes = F, type = "l", lwd=2, 
     col = alpha("deepskyblue1", 0.5), xlab = "", ylab = "")
# T11
lines(t11.l$year, t11.l$depth.c5, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(0,300), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t11.l$year, t11.l$depth.c95, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(0,300), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t11.l$year, rev(t11.l$year)), c(t11.l$depth.c95, rev(t11.l$depth.c5)), 
        col = alpha("darksalmon", 0.5), border = NA)
par(new=T)
plot(t11.l$year, t11.l$depth, xlim = c(2012,2020), 
     ylim = c(0,300), axes = F, type = "l", lwd=2, 
     col = alpha("darkorange3", 0.5), xlab = "", ylab = "")
# T12
lines(t12.l$year, t12.l$depth.c5, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,300), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t12.l$year, t12.l$depth.c95, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,300), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t12.l$year, rev(t12.l$year)), c(t12.l$depth.c95, rev(t12.l$depth.c5)), 
        col = alpha("mediumorchid1", 0.5), border = NA)
par(new=T)
plot(t12.l$year, t12.l$depth, xlim = c(2012,2020), 
     ylim = c(0,300), axes = F, type = "l", lwd=2, 
     col = alpha("mediumorchid4", 0.5), xlab = "", ylab = "")
# T13
lines(t13.l$year, t13.l$depth.c5, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      ylim = c(0,300), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t13.l$year, t13.l$depth.c95, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      ylim = c(0,300), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t13.l$year, rev(t13.l$year)), c(t13.l$depth.c95, rev(t13.l$depth.c5)), 
        col = alpha("yellow", 0.5), border = NA)
par(new=T)
plot(t13.l$year, t13.l$depth, xlim = c(2012,2020), 
     ylim = c(0,300), axes = F, type = "l", lwd=2, 
     col = alpha("yellow3", 0.5), xlab = "", ylab = "")
abline(coef(lm(t13.l$depth ~ t13.l$year)), col = "yellow3", lty = 3, lwd = 2)
box()
axis(side = 1, at = seq(2012,2020,2), labels = NA)
axis(side = 2, at = seq(0,300,50), seq(0,300,50))
legend("topleft", "Leeward", bty = "n")

## Deflation
# TIS
plot(tis.d$year, tis.d$depth, type='n', xlim = c(2012,2020), 
     ylim = c(0,250), axes = F, xlab = "", ylab = "")
lines(tis.d$year, tis.d$depth.c5, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      ylim = c(0,250), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(tis.d$year, tis.d$depth.c95, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      ylim = c(0,250), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(tis.d$year, rev(tis.d$year)), c(tis.d$depth.c95, rev(tis.d$depth.c5)), 
        col = alpha("lightgreen", 0.5), border = NA)
par(new=T)
plot(tis.d$year, tis.d$depth, xlim = c(2012,2020), 
     ylim = c(0,250), axes = F, type = "l", lwd=2, 
     col = alpha("green4", 0.5), xlab = "", ylab = "")
# T10
lines(t10.d$year, t10.d$depth.c5, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,250), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t10.d$year, t10.d$depth.c95, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,250), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t10.d$year, rev(t10.d$year)), c(t10.d$depth.c95, rev(t10.d$depth.c5)), 
        col = alpha("cadetblue1", 0.5), border = NA)
par(new=T)
plot(t10.d$year, t10.d$depth, xlim = c(2012,2020), 
     ylim = c(0,250), axes = F, type = "l", lwd=2, 
     col = alpha("deepskyblue1", 0.5), xlab = "", ylab = "")
# T11
lines(t11.d$year, t11.d$depth.c5, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(0,250), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t11.d$year, t11.d$depth.c95, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(0,250), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t11.d$year, rev(t11.d$year)), c(t11.d$depth.c95, rev(t11.d$depth.c5)), 
        col = alpha("darksalmon", 0.5), border = NA)
par(new=T)
plot(t11.d$year, t11.d$depth, xlim = c(2012,2020), 
     ylim = c(0,250), axes = F, type = "l", lwd=2, 
     col = alpha("darkorange3", 0.5), xlab = "", ylab = "")
# T12
lines(t12.d$year, t12.d$depth.c5, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,250), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t12.d$year, t12.d$depth.c95, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,250), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t12.d$year, rev(t12.d$year)), c(t12.d$depth.c95, rev(t12.d$depth.c5)), 
        col = alpha("mediumorchid1", 0.5), border = NA)
par(new=T)
plot(t12.d$year, t12.d$depth, xlim = c(2012,2020), 
     ylim = c(0,250), axes = F, type = "l", lwd=2, 
     col = alpha("mediumorchid4", 0.5), xlab = "", ylab = "")
abline(coef(lm(t12.d$depth ~ t12.d$year)), col = "mediumorchid4", lty = 3, lwd = 2)
# T13
lines(t13.d$year, t13.d$depth.c5, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      ylim = c(0,250), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t13.d$year, t13.d$depth.c95, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      ylim = c(0,250), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t13.d$year, rev(t13.d$year)), c(t13.d$depth.c95, rev(t13.d$depth.c5)), 
        col = alpha("yellow", 0.5), border = NA)
par(new=T)
plot(t13.d$year, t13.d$depth, xlim = c(2012,2020), 
     ylim = c(0,250), axes = F, type = "l", lwd=2, 
     col = alpha("yellow3", 0.5), xlab = "", ylab = "")
box()
axis(side = 1, at = seq(2012,2020,2), labels = NA)
axis(side = 2, at = seq(0,250,50), seq(0,250,50))
legend("topleft", "Deflation", bty = "n")

## Centre
# TIS
plot(tis.c$year, tis.c$depth, type='n', xlim = c(2012,2020), 
     ylim = c(0,250), axes = F, xlab = "", ylab = "")
lines(tis.c$year, tis.c$depth.c5, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      ylim = c(0,250), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(tis.c$year, tis.c$depth.c95, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      ylim = c(0,250), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(tis.c$year, rev(tis.c$year)), c(tis.c$depth.c95, rev(tis.c$depth.c5)), 
        col = alpha("lightgreen", 0.5), border = NA)
par(new=T)
plot(tis.c$year, tis.c$depth, xlim = c(2012,2020), 
     ylim = c(0,250), axes = F, type = "l", lwd=2, 
     col = alpha("green4", 0.5), xlab = "", ylab = "")
# T10
lines(t10.c$year, t10.c$depth.c5, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,250), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t10.c$year, t10.c$depth.c95, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,250), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t10.c$year, rev(t10.c$year)), c(t10.c$depth.c95, rev(t10.c$depth.c5)), 
        col = alpha("cadetblue1", 0.5), border = NA)
par(new=T)
plot(t10.c$year, t10.c$depth, xlim = c(2012,2020), 
     ylim = c(0,250), axes = F, type = "l", lwd=2, 
     col = alpha("deepskyblue1", 0.5), xlab = "", ylab = "")
abline(coef(lm(t10.c$depth ~ t10.c$year)), col = "deepskyblue3", lty = 3, lwd = 2)
# T11
lines(t11.c$year, t11.c$depth.c5, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(0,250), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t11.c$year, t11.c$depth.c95, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(0,250), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t11.c$year, rev(t11.c$year)), c(t11.c$depth.c95, rev(t11.c$depth.c5)), 
        col = alpha("darksalmon", 0.5), border = NA)
par(new=T)
plot(t11.c$year, t11.c$depth, xlim = c(2012,2020), 
     ylim = c(0,250), axes = F, type = "l", lwd=2, 
     col = alpha("darkorange3", 0.5), xlab = "", ylab = "")
abline(coef(lm(t11.c$depth ~ t11.c$year)), col = "darkorange3", lty = 3, lwd = 2)
# T12
lines(t12.c$year, t12.c$depth.c5, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,250), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t12.c$year, t12.c$depth.c95, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,250), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t12.c$year, rev(t12.c$year)), c(t12.c$depth.c95, rev(t12.c$depth.c5)), 
        col = alpha("mediumorchid1", 0.5), border = NA)
par(new=T)
plot(t12.c$year, t12.c$depth, xlim = c(2012,2020), 
     ylim = c(0,250), axes = F, type = "l", lwd=2, 
     col = alpha("mediumorchid4", 0.5), xlab = "", ylab = "")
# T13
lines(t13.c$year, t13.c$depth.c5, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      ylim = c(0,250), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t13.c$year, t13.c$depth.c95, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      ylim = c(0,250), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t13.c$year, rev(t13.c$year)), c(t13.c$depth.c95, rev(t13.c$depth.c5)), 
        col = alpha("yellow", 0.5), border = NA)
par(new=T)
plot(t13.c$year, t13.c$depth, xlim = c(2012,2020), 
     ylim = c(0,250), axes = F, type = "l", lwd=2, 
     col = alpha("yellow3", 0.5), xlab = "", ylab = "")
box()
axis(side = 1, at = seq(2012,2020,2), labels = seq(2012,2020,2))
axis(side = 2, at = seq(0,250,50), seq(0,250,50))
legend("topleft", "Centre", bty = "n")

mtext(side = 1, "Year", line = 1.5, cex = 0.75, outer = T)
mtext(side = 2, "Depth (cm)", line = 1.5, cex = 0.75, outer = T)







####################################
####################################


par(mfrow = c(4,1), mar = c(1,1,1,1), oma = c(3,3,0,0))

## Windward
# TISw
plot(tis.w$year, tis.w$density, type='n', xlim = c(2012,2020), 
     ylim = c(100,500), axes = F, xlab = "", ylab = "")
lines(tis.w$year, tis.w$density.c5, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(tis.w$year, tis.w$density.c95, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(tis.w$year, rev(tis.w$year)), c(tis.w$density.c95, rev(tis.w$density.c5)), 
        col = alpha("lightgreen", 0.5), border = NA)
par(new=T)
plot(tis.w$year, tis.w$density, xlim = c(2012,2020), 
     ylim = c(100,500), axes = F, type = "l", lwd=2, 
     col = alpha("green4", 0.5), xlab = "", ylab = "")
# T10w
lines(t10.w$year, t10.w$density.c5, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t10.w$year, t10.w$density.c95, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t10.w$year, rev(t10.w$year)), c(t10.w$density.c95, rev(t10.w$density.c5)), 
        col = alpha("cadetblue1", 0.5), border = NA)
par(new=T)
plot(t10.w$year, t10.w$density, xlim = c(2012,2020), 
     ylim = c(100,500), axes = F, type = "l", lwd=2, 
     col = alpha("deepskyblue1", 0.5), xlab = "", ylab = "")
# T11w
lines(t11.w$year, t11.w$density.c5, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t11.w$year, t11.w$density.c95, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t11.w$year, rev(t11.w$year)), c(t11.w$density.c95, rev(t11.w$density.c5)), 
        col = alpha("darksalmon", 0.5), border = NA)
par(new=T)
plot(t11.w$year, t11.w$density, xlim = c(2012,2020), 
     ylim = c(100,500), axes = F, type = "l", lwd=2, 
     col = alpha("darkorange3", 0.5), xlab = "", ylab = "")
# T12w
lines(t12.w$year, t12.w$density.c5, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t12.w$year, t12.w$density.c95, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t12.w$year, rev(t12.w$year)), c(t12.w$density.c95, rev(t12.w$density.c5)), 
        col = alpha("mediumorchid1", 0.5), border = NA)
par(new=T)
plot(t12.w$year, t12.w$density, xlim = c(2012,2020), 
     ylim = c(100,500), axes = F, type = "l", lwd=2, 
     col = alpha("mediumorchid4", 0.5), xlab = "", ylab = "")
# abline(coef(lm(t12.w$density ~ t12.w$year)), col = "mediumorchid4", lty = 3, lwd = 2)
# T13w
lines(t13.w$year, t13.w$density.c5, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t13.w$year, t13.w$density.c95, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t13.w$year, rev(t13.w$year)), c(t13.w$density.c95, rev(t13.w$density.c5)), 
        col = alpha("yellow", 0.5), border = NA)
par(new=T)
plot(t13.w$year, t13.w$density, xlim = c(2012,2020), 
     ylim = c(100,500), axes = F, type = "l", lwd=2, 
     col = alpha("yellow3", 0.5), xlab = "", ylab = "")
# abline(coef(lm(t13.w$density ~ t13.w$year)), col = "yellow3", lty = 3, lwd = 2)
box()
axis(side = 1, at = seq(2000,2020,2), labels = NA)
axis(side = 2, at = seq(0,500,100), seq(0,500,100))
legend("topleft", "Windward", bty = "n")
legend("topright", c("TIS","T10","T11","T12","T13"), 
       fill = c("lightgreen","cadetblue1","darksalmon","mediumorchid1","yellow"), 
       border = NA, bty = "n", inset = c(0.05,0.01), x.intersp = 0.75, text.width = 0.5, horiz = T)


## Leeward
# TIS
plot(tis.l$year, tis.l$density, type='n', xlim = c(2012,2020), 
     ylim = c(100,500), axes = F, xlab = "", ylab = "")
lines(tis.l$year, tis.l$density.c5, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(tis.l$year, tis.l$density.c95, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(tis.l$year, rev(tis.l$year)), c(tis.l$density.c95, rev(tis.l$density.c5)), 
        col = alpha("lightgreen", 0.5), border = NA)
par(new=T)
plot(tis.l$year, tis.l$density, xlim = c(2012,2020), 
     ylim = c(100,500), axes = F, type = "l", lwd=2, 
     col = alpha("green4", 0.5), xlab = "", ylab = "")
# T10
lines(t10.l$year, t10.l$density.c5, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t10.l$year, t10.l$density.c95, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t10.l$year, rev(t10.l$year)), c(t10.l$density.c95, rev(t10.l$density.c5)), 
        col = alpha("cadetblue1", 0.5), border = NA)
par(new=T)
plot(t10.l$year, t10.l$density, xlim = c(2012,2020), 
     ylim = c(100,500), axes = F, type = "l", lwd=2, 
     col = alpha("deepskyblue1", 0.5), xlab = "", ylab = "")
# T11
lines(t11.l$year, t11.l$density.c5, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t11.l$year, t11.l$density.c95, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t11.l$year, rev(t11.l$year)), c(t11.l$density.c95, rev(t11.l$density.c5)), 
        col = alpha("darksalmon", 0.5), border = NA)
par(new=T)
plot(t11.l$year, t11.l$density, xlim = c(2012,2020), 
     ylim = c(100,500), axes = F, type = "l", lwd=2, 
     col = alpha("darkorange3", 0.5), xlab = "", ylab = "")
# T12
lines(t12.l$year, t12.l$density.c5, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t12.l$year, t12.l$density.c95, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t12.l$year, rev(t12.l$year)), c(t12.l$density.c95, rev(t12.l$density.c5)), 
        col = alpha("mediumorchid1", 0.5), border = NA)
par(new=T)
plot(t12.l$year, t12.l$density, xlim = c(2012,2020), 
     ylim = c(100,500), axes = F, type = "l", lwd=2, 
     col = alpha("mediumorchid4", 0.5), xlab = "", ylab = "")
# T13
lines(t13.l$year, t13.l$density.c5, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t13.l$year, t13.l$density.c95, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t13.l$year, rev(t13.l$year)), c(t13.l$density.c95, rev(t13.l$density.c5)), 
        col = alpha("yellow", 0.5), border = NA)
par(new=T)
plot(t13.l$year, t13.l$density, xlim = c(2012,2020), 
     ylim = c(100,500), axes = F, type = "l", lwd=2, 
     col = alpha("yellow3", 0.5), xlab = "", ylab = "")
# abline(coef(lm(t13.l$density ~ t13.l$year)), col = "yellow3", lty = 3, lwd = 2)
box()
axis(side = 1, at = seq(2012,2020,2), labels = NA)
axis(side = 2, at = seq(0,500,100), seq(0,500,100))
legend("bottomleft", "Leeward", bty = "n")

## Deflation
# TIS
plot(tis.d$year, tis.d$density, type='n', xlim = c(2012,2020), 
     ylim = c(100,500), axes = F, xlab = "", ylab = "")
lines(tis.d$year, tis.d$density.c5, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(tis.d$year, tis.d$density.c95, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(tis.d$year, rev(tis.d$year)), c(tis.d$density.c95, rev(tis.d$density.c5)), 
        col = alpha("lightgreen", 0.5), border = NA)
par(new=T)
plot(tis.d$year, tis.d$density, xlim = c(2012,2020), 
     ylim = c(100,500), axes = F, type = "l", lwd=2, 
     col = alpha("green4", 0.5), xlab = "", ylab = "")
# T10
lines(t10.d$year, t10.d$density.c5, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t10.d$year, t10.d$density.c95, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t10.d$year, rev(t10.d$year)), c(t10.d$density.c95, rev(t10.d$density.c5)), 
        col = alpha("cadetblue1", 0.5), border = NA)
par(new=T)
plot(t10.d$year, t10.d$density, xlim = c(2012,2020), 
     ylim = c(100,500), axes = F, type = "l", lwd=2, 
     col = alpha("deepskyblue1", 0.5), xlab = "", ylab = "")
# T11
lines(t11.d$year, t11.d$density.c5, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t11.d$year, t11.d$density.c95, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t11.d$year, rev(t11.d$year)), c(t11.d$density.c95, rev(t11.d$density.c5)), 
        col = alpha("darksalmon", 0.5), border = NA)
par(new=T)
plot(t11.d$year, t11.d$density, xlim = c(2012,2020), 
     ylim = c(100,500), axes = F, type = "l", lwd=2, 
     col = alpha("darkorange3", 0.5), xlab = "", ylab = "")
# T12
lines(t12.d$year, t12.d$density.c5, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t12.d$year, t12.d$density.c95, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t12.d$year, rev(t12.d$year)), c(t12.d$density.c95, rev(t12.d$density.c5)), 
        col = alpha("mediumorchid1", 0.5), border = NA)
par(new=T)
plot(t12.d$year, t12.d$density, xlim = c(2012,2020), 
     ylim = c(100,500), axes = F, type = "l", lwd=2, 
     col = alpha("mediumorchid4", 0.5), xlab = "", ylab = "")
# abline(coef(lm(t12.d$density ~ t12.d$year)), col = "mediumorchid4", lty = 3, lwd = 2)
# T13
lines(t13.d$year, t13.d$density.c5, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t13.d$year, t13.d$density.c95, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t13.d$year, rev(t13.d$year)), c(t13.d$density.c95, rev(t13.d$density.c5)), 
        col = alpha("yellow", 0.5), border = NA)
par(new=T)
plot(t13.d$year, t13.d$density, xlim = c(2012,2020), 
     ylim = c(100,500), axes = F, type = "l", lwd=2, 
     col = alpha("yellow3", 0.5), xlab = "", ylab = "")
box()
axis(side = 1, at = seq(2012,2020,2), labels = NA)
axis(side = 2, at = seq(0,500,100), seq(0,500,100))
legend("topleft", "Deflation", bty = "n")

## Centre
# TIS
plot(tis.c$year, tis.c$density, type='n', xlim = c(2012,2020), 
     ylim = c(100,500), axes = F, xlab = "", ylab = "")
lines(tis.c$year, tis.c$density.c5, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(tis.c$year, tis.c$density.c95, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(tis.c$year, rev(tis.c$year)), c(tis.c$density.c95, rev(tis.c$density.c5)), 
        col = alpha("lightgreen", 0.5), border = NA)
par(new=T)
plot(tis.c$year, tis.c$density, xlim = c(2012,2020), 
     ylim = c(100,500), axes = F, type = "l", lwd=2, 
     col = alpha("green4", 0.5), xlab = "", ylab = "")
# T10
lines(t10.c$year, t10.c$density.c5, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t10.c$year, t10.c$density.c95, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t10.c$year, rev(t10.c$year)), c(t10.c$density.c95, rev(t10.c$density.c5)), 
        col = alpha("cadetblue1", 0.5), border = NA)
par(new=T)
plot(t10.c$year, t10.c$density, xlim = c(2012,2020), 
     ylim = c(100,500), axes = F, type = "l", lwd=2, 
     col = alpha("deepskyblue1", 0.5), xlab = "", ylab = "")
# abline(coef(lm(t10.c$density ~ t10.c$year)), col = "deepskyblue3", lty = 3, lwd = 2)
# T11
lines(t11.c$year, t11.c$density.c5, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t11.c$year, t11.c$density.c95, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t11.c$year, rev(t11.c$year)), c(t11.c$density.c95, rev(t11.c$density.c5)), 
        col = alpha("darksalmon", 0.5), border = NA)
par(new=T)
plot(t11.c$year, t11.c$density, xlim = c(2012,2020), 
     ylim = c(100,500), axes = F, type = "l", lwd=2, 
     col = alpha("darkorange3", 0.5), xlab = "", ylab = "")
# abline(coef(lm(t11.c$density ~ t11.c$year)), col = "darkorange3", lty = 3, lwd = 2)
# T12
lines(t12.c$year, t12.c$density.c5, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t12.c$year, t12.c$density.c95, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t12.c$year, rev(t12.c$year)), c(t12.c$density.c95, rev(t12.c$density.c5)), 
        col = alpha("mediumorchid1", 0.5), border = NA)
par(new=T)
plot(t12.c$year, t12.c$density, xlim = c(2012,2020), 
     ylim = c(100,500), axes = F, type = "l", lwd=2, 
     col = alpha("mediumorchid4", 0.5), xlab = "", ylab = "")
# T13
lines(t13.c$year, t13.c$density.c5, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t13.c$year, t13.c$density.c95, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      ylim = c(100,500), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t13.c$year, rev(t13.c$year)), c(t13.c$density.c95, rev(t13.c$density.c5)), 
        col = alpha("yellow", 0.5), border = NA)
par(new=T)
plot(t13.c$year, t13.c$density, xlim = c(2012,2020), 
     ylim = c(100,500), axes = F, type = "l", lwd=2, 
     col = alpha("yellow3", 0.5), xlab = "", ylab = "")
box()
axis(side = 1, at = seq(2012,2020,2), labels = seq(2012,2020,2))
axis(side = 2, at = seq(0,500,100), seq(0,500,100))
legend("topleft", "Centre", bty = "n")

mtext(side = 1, "Year", line = 1.5, cex = 0.75, outer = T)
mtext(side = 2, expression(paste("Density (kg m"^"-3",")")), line = 1.5, cex = 0.75, outer = T)




####################################
####################################


par(mfrow = c(4,1), mar = c(1,1,1,1), oma = c(3,3,0,0))

## Windward
# TISw
plot(tis.w$year, tis.w$swe, type='n', xlim = c(2012,2020), 
     ylim = c(0,1000), axes = F, xlab = "", ylab = "")
lines(tis.w$year, tis.w$swe.c5, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(tis.w$year, tis.w$swe.c95, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(tis.w$year, rev(tis.w$year)), c(tis.w$swe.c95, rev(tis.w$swe.c5)), 
        col = alpha("lightgreen", 0.5), border = NA)
par(new=T)
plot(tis.w$year, tis.w$swe, xlim = c(2012,2020), 
     ylim = c(0,1000), axes = F, type = "l", lwd=2, 
     col = alpha("green4", 0.5), xlab = "", ylab = "")
# T10w
lines(t10.w$year, t10.w$swe.c5, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t10.w$year, t10.w$swe.c95, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t10.w$year, rev(t10.w$year)), c(t10.w$swe.c95, rev(t10.w$swe.c5)), 
        col = alpha("cadetblue1", 0.5), border = NA)
par(new=T)
plot(t10.w$year, t10.w$swe, xlim = c(2012,2020), 
     ylim = c(0,1000), axes = F, type = "l", lwd=2, 
     col = alpha("deepskyblue1", 0.5), xlab = "", ylab = "")
# T11w
lines(t11.w$year, t11.w$swe.c5, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t11.w$year, t11.w$swe.c95, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t11.w$year, rev(t11.w$year)), c(t11.w$swe.c95, rev(t11.w$swe.c5)), 
        col = alpha("darksalmon", 0.5), border = NA)
par(new=T)
plot(t11.w$year, t11.w$swe, xlim = c(2012,2020), 
     ylim = c(0,1000), axes = F, type = "l", lwd=2, 
     col = alpha("darkorange3", 0.5), xlab = "", ylab = "")
# T12w
lines(t12.w$year, t12.w$swe.c5, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t12.w$year, t12.w$swe.c95, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t12.w$year, rev(t12.w$year)), c(t12.w$swe.c95, rev(t12.w$swe.c5)), 
        col = alpha("mediumorchid1", 0.5), border = NA)
par(new=T)
plot(t12.w$year, t12.w$swe, xlim = c(2012,2020), 
     ylim = c(0,1000), axes = F, type = "l", lwd=2, 
     col = alpha("mediumorchid4", 0.5), xlab = "", ylab = "")
abline(coef(lm(t12.w$swe ~ t12.w$year)), col = "mediumorchid4", lty = 3, lwd = 2)
# T13w
lines(t13.w$year, t13.w$swe.c5, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t13.w$year, t13.w$swe.c95, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t13.w$year, rev(t13.w$year)), c(t13.w$swe.c95, rev(t13.w$swe.c5)), 
        col = alpha("yellow", 0.5), border = NA)
par(new=T)
plot(t13.w$year, t13.w$swe, xlim = c(2012,2020), 
     ylim = c(0,1000), axes = F, type = "l", lwd=2, 
     col = alpha("yellow3", 0.5), xlab = "", ylab = "")
abline(coef(lm(t13.w$swe ~ t13.w$year)), col = "yellow3", lty = 3, lwd = 2)
box()
axis(side = 1, at = seq(2000,2020,2), labels = NA)
axis(side = 2, at = seq(0,1000,250), seq(0,1000,250))
legend("topleft", "Windward", bty = "n")
legend("topright", c("TIS","T10","T11","T12","T13"), 
       fill = c("lightgreen","cadetblue1","darksalmon","mediumorchid1","yellow"), 
       border = NA, bty = "n", inset = c(0.05,0.01), x.intersp = 0.75, text.width = 0.5, horiz = T)


## Leeward
# TIS
plot(tis.l$year, tis.l$swe, type='n', xlim = c(2012,2020), 
     ylim = c(0,1000), axes = F, xlab = "", ylab = "")
lines(tis.l$year, tis.l$swe.c5, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(tis.l$year, tis.l$swe.c95, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(tis.l$year, rev(tis.l$year)), c(tis.l$swe.c95, rev(tis.l$swe.c5)), 
        col = alpha("lightgreen", 0.5), border = NA)
par(new=T)
plot(tis.l$year, tis.l$swe, xlim = c(2012,2020), 
     ylim = c(0,1000), axes = F, type = "l", lwd=2, 
     col = alpha("green4", 0.5), xlab = "", ylab = "")
# T10
lines(t10.l$year, t10.l$swe.c5, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t10.l$year, t10.l$swe.c95, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t10.l$year, rev(t10.l$year)), c(t10.l$swe.c95, rev(t10.l$swe.c5)), 
        col = alpha("cadetblue1", 0.5), border = NA)
par(new=T)
plot(t10.l$year, t10.l$swe, xlim = c(2012,2020), 
     ylim = c(0,1000), axes = F, type = "l", lwd=2, 
     col = alpha("deepskyblue1", 0.5), xlab = "", ylab = "")
# T11
lines(t11.l$year, t11.l$swe.c5, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t11.l$year, t11.l$swe.c95, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t11.l$year, rev(t11.l$year)), c(t11.l$swe.c95, rev(t11.l$swe.c5)), 
        col = alpha("darksalmon", 0.5), border = NA)
par(new=T)
plot(t11.l$year, t11.l$swe, xlim = c(2012,2020), 
     ylim = c(0,1000), axes = F, type = "l", lwd=2, 
     col = alpha("darkorange3", 0.5), xlab = "", ylab = "")
# T12
lines(t12.l$year, t12.l$swe.c5, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t12.l$year, t12.l$swe.c95, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t12.l$year, rev(t12.l$year)), c(t12.l$swe.c95, rev(t12.l$swe.c5)), 
        col = alpha("mediumorchid1", 0.5), border = NA)
par(new=T)
plot(t12.l$year, t12.l$swe, xlim = c(2012,2020), 
     ylim = c(0,1000), axes = F, type = "l", lwd=2, 
     col = alpha("mediumorchid4", 0.5), xlab = "", ylab = "")
# T13
lines(t13.l$year, t13.l$swe.c5, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t13.l$year, t13.l$swe.c95, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t13.l$year, rev(t13.l$year)), c(t13.l$swe.c95, rev(t13.l$swe.c5)), 
        col = alpha("yellow", 0.5), border = NA)
par(new=T)
plot(t13.l$year, t13.l$swe, xlim = c(2012,2020), 
     ylim = c(0,1000), axes = F, type = "l", lwd=2, 
     col = alpha("yellow3", 0.5), xlab = "", ylab = "")
# abline(coef(lm(t13.l$swe ~ t13.l$year)), col = "yellow3", lty = 3, lwd = 2)
box()
axis(side = 1, at = seq(2012,2020,2), labels = NA)
axis(side = 2, at = seq(0,1000,250), seq(0,1000,250))
legend("bottomleft", "Leeward", bty = "n")

## Deflation
# TIS
plot(tis.d$year, tis.d$swe, type='n', xlim = c(2012,2020), 
     ylim = c(0,1000), axes = F, xlab = "", ylab = "")
lines(tis.d$year, tis.d$swe.c5, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(tis.d$year, tis.d$swe.c95, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(tis.d$year, rev(tis.d$year)), c(tis.d$swe.c95, rev(tis.d$swe.c5)), 
        col = alpha("lightgreen", 0.5), border = NA)
par(new=T)
plot(tis.d$year, tis.d$swe, xlim = c(2012,2020), 
     ylim = c(0,1000), axes = F, type = "l", lwd=2, 
     col = alpha("green4", 0.5), xlab = "", ylab = "")
# T10
lines(t10.d$year, t10.d$swe.c5, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t10.d$year, t10.d$swe.c95, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t10.d$year, rev(t10.d$year)), c(t10.d$swe.c95, rev(t10.d$swe.c5)), 
        col = alpha("cadetblue1", 0.5), border = NA)
par(new=T)
plot(t10.d$year, t10.d$swe, xlim = c(2012,2020), 
     ylim = c(0,1000), axes = F, type = "l", lwd=2, 
     col = alpha("deepskyblue1", 0.5), xlab = "", ylab = "")
# T11
lines(t11.d$year, t11.d$swe.c5, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t11.d$year, t11.d$swe.c95, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t11.d$year, rev(t11.d$year)), c(t11.d$swe.c95, rev(t11.d$swe.c5)), 
        col = alpha("darksalmon", 0.5), border = NA)
par(new=T)
plot(t11.d$year, t11.d$swe, xlim = c(2012,2020), 
     ylim = c(0,1000), axes = F, type = "l", lwd=2, 
     col = alpha("darkorange3", 0.5), xlab = "", ylab = "")
# T12
lines(t12.d$year, t12.d$swe.c5, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t12.d$year, t12.d$swe.c95, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t12.d$year, rev(t12.d$year)), c(t12.d$swe.c95, rev(t12.d$swe.c5)), 
        col = alpha("mediumorchid1", 0.5), border = NA)
par(new=T)
plot(t12.d$year, t12.d$swe, xlim = c(2012,2020), 
     ylim = c(0,1000), axes = F, type = "l", lwd=2, 
     col = alpha("mediumorchid4", 0.5), xlab = "", ylab = "")
abline(coef(lm(t12.d$swe ~ t12.d$year)), col = "mediumorchid4", lty = 3, lwd = 2)
# T13
lines(t13.d$year, t13.d$swe.c5, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t13.d$year, t13.d$swe.c95, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t13.d$year, rev(t13.d$year)), c(t13.d$swe.c95, rev(t13.d$swe.c5)), 
        col = alpha("yellow", 0.5), border = NA)
par(new=T)
plot(t13.d$year, t13.d$swe, xlim = c(2012,2020), 
     ylim = c(0,1000), axes = F, type = "l", lwd=2, 
     col = alpha("yellow3", 0.5), xlab = "", ylab = "")
box()
axis(side = 1, at = seq(2012,2020,2), labels = NA)
axis(side = 2, at = seq(0,1000,250), seq(0,1000,250))
legend("topleft", "Deflation", bty = "n")

## Centre
# TIS
plot(tis.c$year, tis.c$swe, type='n', xlim = c(2012,2020), 
     ylim = c(0,1000), axes = F, xlab = "", ylab = "")
lines(tis.c$year, tis.c$swe.c5, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(tis.c$year, tis.c$swe.c95, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(tis.c$year, rev(tis.c$year)), c(tis.c$swe.c95, rev(tis.c$swe.c5)), 
        col = alpha("lightgreen", 0.5), border = NA)
par(new=T)
plot(tis.c$year, tis.c$swe, xlim = c(2012,2020), 
     ylim = c(0,1000), axes = F, type = "l", lwd=2, 
     col = alpha("green4", 0.5), xlab = "", ylab = "")
# T10
lines(t10.c$year, t10.c$swe.c5, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t10.c$year, t10.c$swe.c95, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t10.c$year, rev(t10.c$year)), c(t10.c$swe.c95, rev(t10.c$swe.c5)), 
        col = alpha("cadetblue1", 0.5), border = NA)
par(new=T)
plot(t10.c$year, t10.c$swe, xlim = c(2012,2020), 
     ylim = c(0,1000), axes = F, type = "l", lwd=2, 
     col = alpha("deepskyblue1", 0.5), xlab = "", ylab = "")
# abline(coef(lm(t10.c$swe ~ t10.c$year)), col = "deepskyblue3", lty = 3, lwd = 2)
# T11
lines(t11.c$year, t11.c$swe.c5, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t11.c$year, t11.c$swe.c95, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t11.c$year, rev(t11.c$year)), c(t11.c$swe.c95, rev(t11.c$swe.c5)), 
        col = alpha("darksalmon", 0.5), border = NA)
par(new=T)
plot(t11.c$year, t11.c$swe, xlim = c(2012,2020), 
     ylim = c(0,1000), axes = F, type = "l", lwd=2, 
     col = alpha("darkorange3", 0.5), xlab = "", ylab = "")
abline(coef(lm(t11.c$swe ~ t11.c$year)), col = "darkorange3", lty = 3, lwd = 2)
# T12
lines(t12.c$year, t12.c$swe.c5, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t12.c$year, t12.c$swe.c95, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t12.c$year, rev(t12.c$year)), c(t12.c$swe.c95, rev(t12.c$swe.c5)), 
        col = alpha("mediumorchid1", 0.5), border = NA)
par(new=T)
plot(t12.c$year, t12.c$swe, xlim = c(2012,2020), 
     ylim = c(0,1000), axes = F, type = "l", lwd=2, 
     col = alpha("mediumorchid4", 0.5), xlab = "", ylab = "")
# T13
lines(t13.c$year, t13.c$swe.c5, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t13.c$year, t13.c$swe.c95, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      ylim = c(0,1000), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t13.c$year, rev(t13.c$year)), c(t13.c$swe.c95, rev(t13.c$swe.c5)), 
        col = alpha("yellow", 0.5), border = NA)
par(new=T)
plot(t13.c$year, t13.c$swe, xlim = c(2012,2020), 
     ylim = c(0,1000), axes = F, type = "l", lwd=2, 
     col = alpha("yellow3", 0.5), xlab = "", ylab = "")
box()
axis(side = 1, at = seq(2012,2020,2), labels = seq(2012,2020,2))
axis(side = 2, at = seq(0,1000,250), seq(0,1000,250))
legend("topleft", "Centre", bty = "n")

mtext(side = 1, "Year", line = 1.5, cex = 0.75, outer = T)
mtext(side = 2, "SWE (mm)", line = 1.5, cex = 0.75, outer = T)







####################################
####################################


par(mfrow = c(4,1), mar = c(1,1,1,1), oma = c(3,3,0,0))

## Windward
# TISw
plot(tis.w$year, tis.w$htc, type='n', xlim = c(2012,2020), 
     ylim = c(0,2.5), axes = F, xlab = "", ylab = "")
lines(tis.w$year, tis.w$htc.c5, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(tis.w$year, tis.w$htc.c95, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(tis.w$year, rev(tis.w$year)), c(tis.w$htc.c95, rev(tis.w$htc.c5)), 
        col = alpha("lightgreen", 0.5), border = NA)
par(new=T)
plot(tis.w$year, tis.w$htc, xlim = c(2012,2020), 
     ylim = c(0,2.5), axes = F, type = "l", lwd=2, 
     col = alpha("green4", 0.5), xlab = "", ylab = "")
# T10w
lines(t10.w$year, t10.w$htc.c5, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t10.w$year, t10.w$htc.c95, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t10.w$year, rev(t10.w$year)), c(t10.w$htc.c95, rev(t10.w$htc.c5)), 
        col = alpha("cadetblue1", 0.5), border = NA)
par(new=T)
plot(t10.w$year, t10.w$htc, xlim = c(2012,2020), 
     ylim = c(0,2.5), axes = F, type = "l", lwd=2, 
     col = alpha("deepskyblue1", 0.5), xlab = "", ylab = "")
# T11w
lines(t11.w$year, t11.w$htc.c5, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(0,10), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t11.w$year, t11.w$htc.c95, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      ylim = c(0,10), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t11.w$year, rev(t11.w$year)), c(t11.w$htc.c95, rev(t11.w$htc.c5)), 
        col = alpha("darksalmon", 0.5), border = NA)
par(new=T)
plot(t11.w$year, t11.w$htc, xlim = c(2012,2020), 
     ylim = c(0,2.5), axes = F, type = "l", lwd=2, 
     col = alpha("darkorange3", 0.5), xlab = "", ylab = "")
# T12w
lines(t12.w$year, t12.w$htc.c5, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t12.w$year, t12.w$htc.c95, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t12.w$year, rev(t12.w$year)), c(t12.w$htc.c95, rev(t12.w$htc.c5)), 
        col = alpha("mediumorchid1", 0.5), border = NA)
par(new=T)
plot(t12.w$year, t12.w$htc, xlim = c(2012,2020), 
     ylim = c(0,2.5), axes = F, type = "l", lwd=2, 
     col = alpha("mediumorchid4", 0.5), xlab = "", ylab = "")
# abline(coef(lm(t12.w$htc ~ t12.w$year)), col = "mediumorchid4", lty = 3, lwd = 2)
# T13w
lines(t13.w$year, t13.w$htc.c5, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t13.w$year, t13.w$htc.c95, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t13.w$year, rev(t13.w$year)), c(t13.w$htc.c95, rev(t13.w$htc.c5)), 
        col = alpha("yellow", 0.5), border = NA)
par(new=T)
plot(t13.w$year, t13.w$htc, xlim = c(2012,2020), 
     ylim = c(0,2.5), axes = F, type = "l", lwd=2, 
     col = alpha("yellow3", 0.5), xlab = "", ylab = "")
# abline(coef(lm(t13.w$htc ~ t13.w$year)), col = "yellow3", lty = 3, lwd = 2)
box()
axis(side = 1, at = seq(2000,2020,2), labels = NA)
axis(side = 2, at = seq(0,2.5,0.5), seq(0,2.5,0.5))
legend("topleft", "Windward", bty = "n")
legend("topright", c("TIS","T10","T11","T12","T13"), 
       fill = c("lightgreen","cadetblue1","darksalmon","mediumorchid1","yellow"), 
       border = NA, bty = "n", inset = c(0.05,0.01), x.intersp = 0.75, text.width = 0.5, horiz = T)


## Leeward
# TIS
plot(tis.l$year, tis.l$htc, type='n', xlim = c(2012,2020), 
     ylim = c(0,0.5), axes = F, xlab = "", ylab = "")
lines(tis.l$year, tis.l$htc.c5, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(tis.l$year, tis.l$htc.c95, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(tis.l$year, rev(tis.l$year)), c(tis.l$htc.c95, rev(tis.l$htc.c5)), 
        col = alpha("lightgreen", 0.5), border = NA)
par(new=T)
plot(tis.l$year, tis.l$htc, xlim = c(2012,2020), 
     ylim = c(0,0.5), axes = F, type = "l", lwd=2, 
     col = alpha("green4", 0.5), xlab = "", ylab = "")
# T10
lines(t10.l$year, t10.l$htc.c5, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t10.l$year, t10.l$htc.c95, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t10.l$year, rev(t10.l$year)), c(t10.l$htc.c95, rev(t10.l$htc.c5)), 
        col = alpha("cadetblue1", 0.5), border = NA)
par(new=T)
plot(t10.l$year, t10.l$htc, xlim = c(2012,2020), 
     ylim = c(0,0.5), axes = F, type = "l", lwd=2, 
     col = alpha("deepskyblue1", 0.5), xlab = "", ylab = "")
# T11
lines(t11.l$year, t11.l$htc.c5, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t11.l$year, t11.l$htc.c95, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t11.l$year, rev(t11.l$year)), c(t11.l$htc.c95, rev(t11.l$htc.c5)), 
        col = alpha("darksalmon", 0.5), border = NA)
par(new=T)
plot(t11.l$year, t11.l$htc, xlim = c(2012,2020), 
     ylim = c(0,0.5), axes = F, type = "l", lwd=2, 
     col = alpha("darkorange3", 0.5), xlab = "", ylab = "")
# T12
lines(t12.l$year, t12.l$htc.c5, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t12.l$year, t12.l$htc.c95, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t12.l$year, rev(t12.l$year)), c(t12.l$htc.c95, rev(t12.l$htc.c5)), 
        col = alpha("mediumorchid1", 0.5), border = NA)
par(new=T)
plot(t12.l$year, t12.l$htc, xlim = c(2012,2020), 
     ylim = c(0,0.5), axes = F, type = "l", lwd=2, 
     col = alpha("mediumorchid4", 0.5), xlab = "", ylab = "")
# T13
lines(t13.l$year, t13.l$htc.c5, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t13.l$year, t13.l$htc.c95, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t13.l$year, rev(t13.l$year)), c(t13.l$htc.c95, rev(t13.l$htc.c5)), 
        col = alpha("yellow", 0.5), border = NA)
par(new=T)
plot(t13.l$year, t13.l$htc, xlim = c(2012,2020), 
     ylim = c(0,0.5), axes = F, type = "l", lwd=2, 
     col = alpha("yellow3", 0.5), xlab = "", ylab = "")
abline(coef(lm(t13.l$htc ~ t13.l$year)), col = "yellow3", lty = 3, lwd = 2)
box()
axis(side = 1, at = seq(2012,2020,2), labels = NA)
axis(side = 2, at = seq(0,0.5,0.1), seq(0,0.5,0.1))
legend("bottomleft", "Leeward", bty = "n")

## Deflation
# TIS
plot(tis.d$year, tis.d$htc, type='n', xlim = c(2012,2020), 
     ylim = c(0,2.5), axes = F, xlab = "", ylab = "")
lines(tis.d$year, tis.d$htc.c5, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(tis.d$year, tis.d$htc.c95, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(tis.d$year, rev(tis.d$year)), c(tis.d$htc.c95, rev(tis.d$htc.c5)), 
        col = alpha("lightgreen", 0.5), border = NA)
par(new=T)
plot(tis.d$year, tis.d$htc, xlim = c(2012,2020), 
     ylim = c(0,2.5), axes = F, type = "l", lwd=2, 
     col = alpha("green4", 0.5), xlab = "", ylab = "")
# T10
lines(t10.d$year, t10.d$htc.c5, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t10.d$year, t10.d$htc.c95, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t10.d$year, rev(t10.d$year)), c(t10.d$htc.c95, rev(t10.d$htc.c5)), 
        col = alpha("cadetblue1", 0.5), border = NA)
par(new=T)
plot(t10.d$year, t10.d$htc, xlim = c(2012,2020), 
     ylim = c(0,2.5), axes = F, type = "l", lwd=2, 
     col = alpha("deepskyblue1", 0.5), xlab = "", ylab = "")
# T11
lines(t11.d$year, t11.d$htc.c5, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t11.d$year, t11.d$htc.c95, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t11.d$year, rev(t11.d$year)), c(t11.d$htc.c95, rev(t11.d$htc.c5)), 
        col = alpha("darksalmon", 0.5), border = NA)
par(new=T)
plot(t11.d$year, t11.d$htc, xlim = c(2012,2020), 
     ylim = c(0,2.5), axes = F, type = "l", lwd=2, 
     col = alpha("darkorange3", 0.5), xlab = "", ylab = "")
# T12
lines(t12.d$year, t12.d$htc.c5, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t12.d$year, t12.d$htc.c95, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t12.d$year, rev(t12.d$year)), c(t12.d$htc.c95, rev(t12.d$htc.c5)), 
        col = alpha("mediumorchid1", 0.5), border = NA)
par(new=T)
plot(t12.d$year, t12.d$htc, xlim = c(2012,2020), 
     ylim = c(0,2.5), axes = F, type = "l", lwd=2, 
     col = alpha("mediumorchid4", 0.5), xlab = "", ylab = "")
# abline(coef(lm(t12.d$htc ~ t12.d$year)), col = "mediumorchid4", lty = 3, lwd = 2)
# T13
lines(t13.d$year, t13.d$htc.c5, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t13.d$year, t13.d$htc.c95, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t13.d$year, rev(t13.d$year)), c(t13.d$htc.c95, rev(t13.d$htc.c5)), 
        col = alpha("yellow", 0.5), border = NA)
par(new=T)
plot(t13.d$year, t13.d$htc, xlim = c(2012,2020), 
     ylim = c(0,2.5), axes = F, type = "l", lwd=2, 
     col = alpha("yellow3", 0.5), xlab = "", ylab = "")
box()
axis(side = 1, at = seq(2012,2020,2), labels = NA)
axis(side = 2, at = seq(0,2.5,0.5), seq(0,2.5,0.5))
legend("topleft", "Deflation", bty = "n")

## Centre
# TIS
plot(tis.c$year, tis.c$htc, type='n', xlim = c(2012,2020), 
     ylim = c(0,0.5), axes = F, xlab = "", ylab = "")
lines(tis.c$year, tis.c$htc.c5, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(tis.c$year, tis.c$htc.c95, col = alpha('lightgreen', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(tis.c$year, rev(tis.c$year)), c(tis.c$htc.c95, rev(tis.c$htc.c5)), 
        col = alpha("lightgreen", 0.5), border = NA)
par(new=T)
plot(tis.c$year, tis.c$htc, xlim = c(2012,2020), 
     ylim = c(0,0.5), axes = F, type = "l", lwd=2, 
     col = alpha("green4", 0.5), xlab = "", ylab = "")
# T10
lines(t10.c$year, t10.c$htc.c5, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t10.c$year, t10.c$htc.c95, col = alpha('cadetblue1', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t10.c$year, rev(t10.c$year)), c(t10.c$htc.c95, rev(t10.c$htc.c5)), 
        col = alpha("cadetblue1", 0.5), border = NA)
par(new=T)
plot(t10.c$year, t10.c$htc, xlim = c(2012,2020), 
     ylim = c(0,0.5), axes = F, type = "l", lwd=2, 
     col = alpha("deepskyblue1", 0.5), xlab = "", ylab = "")
abline(coef(lm(t10.c$htc ~ t10.c$year)), col = "deepskyblue3", lty = 3, lwd = 2)
# T11
lines(t11.c$year, t11.c$htc.c5, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t11.c$year, t11.c$htc.c95, col = alpha('darksalmon', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t11.c$year, rev(t11.c$year)), c(t11.c$htc.c95, rev(t11.c$htc.c5)), 
        col = alpha("darksalmon", 0.5), border = NA)
par(new=T)
plot(t11.c$year, t11.c$htc, xlim = c(2012,2020), 
     ylim = c(0,0.5), axes = F, type = "l", lwd=2, 
     col = alpha("darkorange3", 0.5), xlab = "", ylab = "")
abline(coef(lm(t11.c$htc ~ t11.c$year)), col = "darkorange3", lty = 3, lwd = 2)
# T12
lines(t12.c$year, t12.c$htc.c5, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t12.c$year, t12.c$htc.c95, col = alpha('mediumorchid1', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t12.c$year, rev(t12.c$year)), c(t12.c$htc.c95, rev(t12.c$htc.c5)), 
        col = alpha("mediumorchid1", 0.5), border = NA)
par(new=T)
plot(t12.c$year, t12.c$htc, xlim = c(2012,2020), 
     ylim = c(0,0.5), axes = F, type = "l", lwd=2, 
     col = alpha("mediumorchid4", 0.5), xlab = "", ylab = "")
# T13
lines(t13.c$year, t13.c$htc.c5, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(t13.c$year, t13.c$htc.c95, col = alpha('yellow', 0.5), xlim = c(2012,2020), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(t13.c$year, rev(t13.c$year)), c(t13.c$htc.c95, rev(t13.c$htc.c5)), 
        col = alpha("yellow", 0.5), border = NA)
par(new=T)
plot(t13.c$year, t13.c$htc, xlim = c(2012,2020), 
     ylim = c(0,0.5), axes = F, type = "l", lwd=2, 
     col = alpha("yellow3", 0.5), xlab = "", ylab = "")
box()
axis(side = 1, at = seq(2012,2020,2), labels = seq(2012,2020,2))
axis(side = 2, at = seq(0,0.5,0.1), seq(0,0.5,0.1))
legend("topleft", "Centre", bty = "n")

mtext(side = 1, "Year", line = 1.5, cex = 0.75, outer = T)
mtext(side = 2, expression(paste("HTC (W m"^"-2"," K"^"-1",")")), line = 1.5, cex = 0.75, outer = T)





## ***************************
## LTEMS
## Subset the data and plot with 95% CI

snow <- read.csv("~/Desktop/Workspace/Earthwatch/ltems_snow.csv", header = TRUE)
snow.depth <- aggregate(depth ~ site + date, data = snow, mean)
snow.density <- aggregate(density ~ site + date, data = snow, mean)
snow.swe <- aggregate(swe ~ site + date, data = snow, mean)
snow.htc <- aggregate(htc ~ site + date, data = snow, mean)
snow.n <- aggregate(depth ~ site + date, data = snow, length)
snow.depth.sd <- aggregate(depth ~ site + date, data = snow, sd)
snow.density.sd <- aggregate(density ~ site + date, data = snow, sd)
snow.swe.sd <- aggregate(swe ~ site + date, data = snow, sd)
snow.htc.sd <- aggregate(htc ~ site + date, data = snow, sd)

snow2 <- data.frame(cbind(snow.depth, snow.density$density, 
                       snow.swe$swe, snow.htc$htc, 
                       snow.n$depth, 
                       snow.depth.sd$depth,
                       snow.density.sd$density, 
                       snow.swe.sd$swe,
                       snow.htc.sd$htc))
names(snow2) <- c("site","year","depth","density","swe","htc","n","depth.sd",
                  "density.sd","swe.sd","htc.sd")

snow2$depth.se <- snow2$depth.sd / sqrt(snow2$n)
snow2$density.se <- snow2$density.sd / sqrt(snow2$n)
snow2$swe.se <- snow2$swe.sd / sqrt(snow2$n)
snow2$htc.se <- snow2$htc.sd / sqrt(snow2$n)

snow2$depth.ci <- snow2$depth.se*qt(0.975, snow2$n-1)
snow2$depth.c5 <- snow2$depth - snow2$depth.ci
snow2$depth.c95 <- snow2$depth + snow2$depth.ci

snow2$density.ci <- snow2$density.se*qt(0.975, snow2$n-1)
snow2$density.c5 <- snow2$density - snow2$density.ci
snow2$density.c95 <- snow2$density + snow2$density.ci

snow2$swe.ci <- snow2$swe.se*qt(0.975, snow2$n-1)
snow2$swe.c5 <- snow2$swe - snow2$swe.ci
snow2$swe.c95 <- snow2$swe + snow2$swe.ci

snow2$htc.ci <- snow2$htc.se*qt(0.975, snow2$n-1)
snow2$htc.c5 <- snow2$htc - snow2$htc.ci
snow2$htc.c95 <- snow2$htc + snow2$htc.ci

# Tree island
tis <- subset(snow2, site == "TIS")

# Treed areas
air <- subset(snow2, site == "AIR")
bsw <- subset(snow2, site == "BSW")
wsu <- subset(snow2, site == "WSU")

# Disturbed
bfr <- subset(snow2, site == "BFR")
pfr <- subset(snow2, site == "PFR") 
bwp <- subset(snow2, site == "BWP")

# Exposed
fen <- subset(snow2, site == "FEN")
ppa <- subset(snow2, site == "PPA")
ppd <- subset(snow2, site == "PPD")
tun <- subset(snow2, site == "TUN")

# Depth
summary(lm(tis[,3] ~ tis$year)) # n.s.
summary(lm(air[,3] ~ air$year)) # n.s.
summary(lm(bsw[,3] ~ bsw$year)) # n.s.
summary(lm(wsu[,3] ~ wsu$year)) # n.s.
summary(lm(bfr[,3] ~ bfr$year)) # n.s.
summary(lm(pfr[,3] ~ pfr$year)) # n.s.
summary(lm(bwp[,3] ~ bwp$year)) # b1 = 0.7711, p = 0.02926
summary(lm(fen[,3] ~ fen$year)) # n.s.
summary(lm(ppa[,3] ~ ppa$year)) # n.s.
summary(lm(ppd[,3] ~ ppd$year)) # n.s.
summary(lm(tun[,3] ~ tun$year)) # n.s.

# Density
summary(lm(tis[,4] ~ tis$year)) # n.s.
summary(lm(air[,4] ~ air$year)) # b1 = -3.176, p = 0.02237
summary(lm(bsw[,4] ~ bsw$year)) # n.s.
summary(lm(wsu[,4] ~ wsu$year)) # n.s.
summary(lm(bfr[,4] ~ bfr$year)) # b1 = -2.2783, p = 0.02067
summary(lm(pfr[,4] ~ pfr$year)) # n.s.
summary(lm(bwp[,4] ~ bwp$year)) # b1 = -2.0582, p = 0.03812
summary(lm(fen[,4] ~ fen$year)) # n.s.
summary(lm(ppa[,4] ~ ppa$year)) # n.s.
summary(lm(ppd[,4] ~ ppd$year)) # n.s.
summary(lm(tun[,4] ~ tun$year)) # b1 = -3.711, p = 0.0269

# SWE
summary(lm(tis[,5] ~ tis$year)) # n.s.
summary(lm(air[,5] ~ air$year)) # n.s.
summary(lm(bsw[,5] ~ bsw$year)) # n.s.
summary(lm(wsu[,5] ~ wsu$year)) # n.s.
summary(lm(bfr[,5] ~ bfr$year)) # b1 = -1.6168, p = 0.03756
summary(lm(pfr[,5] ~ pfr$year)) # n.s.
summary(lm(bwp[,5] ~ bwp$year)) # n.s.
summary(lm(fen[,5] ~ fen$year)) # n.s.
summary(lm(ppa[,5] ~ ppa$year)) # n.s.
summary(lm(ppd[,5] ~ ppd$year)) # n.s.
summary(lm(tun[,5] ~ tun$year)) # n.s.

# HTC
summary(lm(tis[,6] ~ tis$year)) # n.s.
summary(lm(air[,6] ~ air$year)) # n.s.
summary(lm(bsw[,6] ~ bsw$year)) # n.s.
summary(lm(wsu[,6] ~ wsu$year)) # n.s.
summary(lm(bfr[,6] ~ bfr$year)) # n.s.
summary(lm(pfr[,6] ~ pfr$year)) # n.s.
summary(lm(bwp[,6] ~ bwp$year)) # b1 = -0.06648, p = 0.0192
summary(lm(fen[,6] ~ fen$year)) # n.s.
summary(lm(ppa[,6] ~ ppa$year)) # n.s.
summary(lm(ppd[,6] ~ ppd$year)) # n.s.
summary(lm(tun[,6] ~ tun$year)) # n.s.

####################################
####################################

## Depth
par(mfrow = c(4,1), mar = c(1,1,1,1), oma = c(3,3,0,0))
# TIS
plot(tis$year, tis$depth, type='n', xlim = c(2000,2020), 
     ylim = c(40,200), axes = F, xlab = "", ylab = "")
lines(tis$year, tis$depth.c5, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(40,200), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(tis$year, tis$depth.c95, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(40,200), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(tis$year, rev(tis$year)), c(tis$depth.c95, rev(tis$depth.c5)), 
        col = "lightgreen", border = NA)
par(new=T)
plot(tis$year, tis$depth, xlim = c(2000,2020), 
     ylim = c(40,200), axes = F, type = "l", lwd=2, 
     col = "green4", xlab = "", ylab = "")
box()
axis(side = 1, at = seq(2000,2020,2), labels = NA)
axis(side = 2, at = seq(0,200,50), seq(0,200,50))
legend("bottomleft", "TIS", fill = "lightgreen", border = NA, bty = "n", inset = c(0,-0.1))
# legend("bottomleft", "", col = "green4", lwd = 2, border = NA, bty = "n", inset = c(-0.025,-0.01))

# Treed areas
plot(air$year, air$depth, type='n', xlim = c(2000,2020), ylim = c(40,100), 
     axes = F, xlab = "", ylab = "")
# AIR
lines(air$year, air$depth.c5, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(40,100), xlab = "", ylab = "")
lines(air$year, air$depth.c95, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(40,100), xlab = "", ylab = "")
polygon(c(air$year, rev(air$year)), c(air$depth.c95, rev(air$depth.c5)), 
        col = "lightgreen", border = NA)
par(new=T)
plot(air$year, air$depth, xlim = c(2000,2020), ylim = c(40,100), 
     axes = F, type = "l", lwd=2, col = "green4", xlab = "", ylab = "")
# BSW
par(new = T)
lines(bsw$year, bsw$depth.c5, col = 'cadetblue1', xlim = c(2000,2020), 
      ylim = c(40,100), xlab = "", ylab = "")
lines(bsw$year, bsw$depth.c95, col = 'cadetblue1', xlim = c(2000,2020), 
      ylim = c(40,100), xlab = "", ylab = "")
polygon(c(bsw$year, rev(bsw$year)), c(bsw$depth.c95, rev(bsw$depth.c5)), 
        col = "cadetblue1", border = NA)
par(new=T)
plot(bsw$year, bsw$depth, xlim = c(2000,2020), ylim = c(40,100), 
     axes = F, type = "l", lwd=2, col = "deepskyblue1", xlab = "", ylab = "")
# WSU
par(new = T)
lines(wsu$year, wsu$depth.c5, col = 'darksalmon', xlim = c(2000,2020), ylim = c(40,100), 
      xlab = "", ylab = "")
lines(wsu$year, wsu$depth.c95, col = 'darksalmon', xlim = c(2000,2020), ylim = c(40,100), 
      xlab = "", ylab = "")
polygon(c(wsu$year, rev(wsu$year)), c(wsu$depth.c95, rev(wsu$depth.c5)), 
        col = "darksalmon", border = NA)
par(new=T)
plot(wsu$year, wsu$depth, xlim = c(2000,2020), ylim = c(40,100), 
     axes = F, type = "l", lwd=2, col = "darkorange3", xlab = "", ylab = "")
box()
axis(side = 1, at = seq(2000,2020,2), labels = NA)
axis(side = 2, at = seq(0,100,25), seq(0,100,25))
legend("topleft", c("AIR","BSW","WSU"), 
       fill = c("lightgreen","cadetblue1","darksalmon"), 
       border = NA, bty = "n", inset = c(0,-0.1), horiz = T)
# legend("topleft", c("","",""), col = c("green4","deepskyblue1","darkorange3"), 
#        lwd = c(2,2,2), border = NA, bty = "n", x.intersp = c(2.4,2.4),
#        inset = c(-0.025,-0.01), horiz = T)


# Disturbed areas
plot(bwp$year, bwp$depth, type='n', xlim = c(2000,2020), ylim = c(0,100), 
     axes = F, xlab = "", ylab = "")
# BWP
lines(bwp$year, bwp$depth.c5, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(0,100), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(bwp$year, bwp$depth.c95, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(0,100), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(bwp$year, rev(bwp$year)), c(bwp$depth.c95, rev(bwp$depth.c5)), 
        col = "lightgreen", border = NA)
par(new=T)
plot(bwp$year, bwp$depth, xlim = c(2000,2020), ylim = c(0,100), 
     axes = F, type = "l", lwd=2, col = "green4", xlab = "", ylab = "")
abline(coef(lm(bwp[,3] ~ bwp$year)), col = "green4", lty = 3, lwd = 2)
# PFR
par(new = T)
lines(pfr$year, pfr$depth.c5, col = 'cadetblue1', xlim = c(2000,2020), ylim = c(40,100), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(pfr$year, pfr$depth.c95, col = 'cadetblue1', xlim = c(2000,2020), ylim = c(40,100), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(pfr$year, rev(pfr$year)), c(pfr$depth.c95, rev(pfr$depth.c5)), 
        col = "cadetblue1", border = NA)
par(new=T)
plot(pfr$year, pfr$depth, xlim = c(2000,2020), ylim = c(0,100), 
     axes = F, type = "l", lwd=2, col = "deepskyblue1", xlab = "", ylab = "")
# BFR
par(new = T)
lines(bfr$year, bfr$depth.c5, col = 'darksalmon', xlim = c(2000,2020), 
      ylim = c(0,100), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(bfr$year, bfr$depth.c95, col = 'darksalmon', xlim = c(2000,2020), 
      ylim = c(0,100), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(bfr$year, rev(bfr$year)), c(bfr$depth.c95, rev(bfr$depth.c5)), 
        col = "darksalmon", border = NA)
par(new=T)
plot(bfr$year, bfr$depth, xlim = c(2000,2020), ylim = c(0,100), 
     axes = F, type = "l", lwd=2, col = "darkorange3", xlab = "", ylab = "")
box()
axis(side = 1, at = seq(2000,2020,2), labels = NA)
axis(side = 2, at = seq(0,200,50), seq(0,200,50))
legend("topleft", c("BWP","BFR","PFR"), 
       fill = c("lightgreen","darksalmon","cadetblue1"), 
       border = NA, bty = "n", inset = c(0,-0.1), horiz = T)

# Exposed areas
plot(tun$year, tun$depth, type='n', xlim = c(2000,2020), ylim = c(0,50), 
     axes = F, xlab = "", ylab = "")
# PPD
par(new = T)
lines(ppd$year, ppd$depth.c5, col = 'mediumorchid1', xlim = c(2000,2020), 
      ylim = c(0,50), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(ppd$year, ppd$depth.c95, col = 'mediumorchid1', xlim = c(2000,2020), 
      ylim = c(0,50), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(ppd$year, rev(ppd$year)), c(ppd$depth.c95, rev(ppd$depth.c5)), 
        col = "mediumorchid1", border = NA)
par(new=T)
plot(ppd$year, ppd$depth, xlim = c(2000,2020), ylim = c(0,50), 
     axes = F, type = "l", lwd=2, col = "mediumorchid4", xlab = "", ylab = "")
# PPA
par(new = T)
lines(ppa$year, ppa$depth.c5, col = 'darksalmon', xlim = c(2000,2020), 
      ylim = c(0,50), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(ppa$year, ppa$depth.c95, col = 'darksalmon', xlim = c(2000,2020), 
      ylim = c(0,50), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(ppa$year, rev(ppa$year)), c(ppa$depth.c95, rev(ppa$depth.c5)), 
        col = "darksalmon", border = NA)
par(new=T)
plot(ppa$year, ppa$depth, xlim = c(2000,2020), ylim = c(0,50), 
     axes = F, type = "l", lwd=2, col = "darkorange3", xlab = "", ylab = "")
# TUN
lines(tun$year, tun$depth.c5, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(0,50), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(tun$year, tun$depth.c95, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(0,50), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(tun$year, rev(tun$year)), c(tun$depth.c95, rev(tun$depth.c5)), 
        col = "lightgreen", border = NA)
par(new=T)
plot(tun$year, tun$depth, xlim = c(2000,2020), ylim = c(0,50), 
     axes = F, type = "l", lwd=2, col = "green4", xlab = "", ylab = "")
# FEN
par(new = T)
lines(fen$year, fen$depth.c5, col = 'cadetblue1', xlim = c(2000,2020), ylim = c(0,50), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(fen$year, fen$depth.c95, col = 'cadetblue1', xlim = c(2000,2020), ylim = c(0,50), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(fen$year, rev(fen$year)), c(fen$depth.c95, rev(fen$depth.c5)), 
        col = "cadetblue1", border = NA)
par(new=T)
plot(fen$year, fen$depth, xlim = c(2000,2020), ylim = c(0,50), 
     axes = F, type = "l", lwd=2, col = "deepskyblue1", xlab = "", ylab = "")
box()
axis(side = 1, at = seq(2000,2020,2), labels = seq(2000,2020,2))
axis(side = 2, at = seq(0,50,10), seq(0,50,10))
legend("topleft", c("TUN","FEN","PPA","PPD"), 
       fill = c("lightgreen","cadetblue1","darksalmon","mediumorchid1"), 
       border = NA, bty = "n", inset = c(0,-0.1), horiz = T)
# legend("topleft", c("","","",""), col = c("green4","deepskyblue1","darkorange3","mediumorchid4"), 
#        lwd = c(2,2,2,2), border = NA, bty = "n", x.intersp = c(4.8,4.4,4.2,4.2),
#        inset = c(-0.025,-0.05), horiz = T)
mtext(side = 1, "Year", line = 1.5, cex = 0.75, outer = T)
mtext(side = 2, "Depth (cm)", line = 1.5, cex = 0.75, outer = T)

####################################
####################################

## Density
par(mfrow = c(4,1), mar = c(1,1,1,1), oma = c(3,3,0,0))
# TIS
plot(tis$year, tis$density, type='n', xlim = c(2000,2020), 
     ylim = c(100,400), axes = F, xlab = "", ylab = "")
lines(tis$year, tis$density.c5, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(100,400), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(tis$year, tis$density.c95, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(100,400), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(tis$year, rev(tis$year)), c(tis$density.c95, rev(tis$density.c5)), 
        col = "lightgreen", border = NA)
par(new=T)
plot(tis$year, tis$density, xlim = c(2000,2020), 
     ylim = c(100,400), axes = F, type = "l", lwd=2, 
     col = "green4", xlab = "", ylab = "")
box()
axis(side = 1, at = seq(2000,2020,2), labels = NA)
axis(side = 2, at = seq(0,500,100), seq(0,500,100))
legend("bottomleft", "TIS", fill = "lightgreen", border = NA, bty = "n", inset = c(0,-0.1))

# Treed areas
plot(air$year, air$density, type='n', xlim = c(2000,2020), ylim = c(100,400), 
     axes = F, xlab = "", ylab = "")
# AIR
lines(air$year, air$density.c5, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(100,400), xlab = "", ylab = "")
lines(air$year, air$density.c95, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(100,400), xlab = "", ylab = "")
polygon(c(air$year, rev(air$year)), c(air$density.c95, rev(air$density.c5)), 
        col = "lightgreen", border = NA)
par(new=T)
plot(air$year, air$density, xlim = c(2000,2020), ylim = c(100,400), 
     axes = F, type = "l", lwd=2, col = "green4", xlab = "", ylab = "")
abline(coef(lm(air[,4] ~ air$year)), col = "green4", lty = 3, lwd = 2)
# BSW
par(new = T)
lines(bsw$year, bsw$density.c5, col = 'cadetblue1', xlim = c(2000,2020), 
      ylim = c(100,400), xlab = "", ylab = "")
lines(bsw$year, bsw$density.c95, col = 'cadetblue1', xlim = c(2000,2020), 
      ylim = c(100,400), xlab = "", ylab = "")
polygon(c(bsw$year, rev(bsw$year)), c(bsw$density.c95, rev(bsw$density.c5)), 
        col = "cadetblue1", border = NA)
par(new=T)
plot(bsw$year, bsw$density, xlim = c(2000,2020), ylim = c(100,400), 
     axes = F, type = "l", lwd=2, col = "deepskyblue1", xlab = "", ylab = "")
# WSU
par(new = T)
lines(wsu$year, wsu$density.c5, col = 'darksalmon', xlim = c(2000,2020), ylim = c(100,400), 
      xlab = "", ylab = "")
lines(wsu$year, wsu$density.c95, col = 'darksalmon', xlim = c(2000,2020), ylim = c(100,400), 
      xlab = "", ylab = "")
polygon(c(wsu$year, rev(wsu$year)), c(wsu$density.c95, rev(wsu$density.c5)), 
        col = "darksalmon", border = NA)
par(new=T)
plot(wsu$year, wsu$density, xlim = c(2000,2020), ylim = c(100,400), 
     axes = F, type = "l", lwd=2, col = "darkorange3", xlab = "", ylab = "")
box()
axis(side = 1, at = seq(2000,2020,2), labels = NA)
axis(side = 2, at = seq(0,500,100), seq(0,500,100))
legend("topleft", c("AIR","BSW","WSU"), 
       fill = c("lightgreen","cadetblue1","darksalmon"), 
       border = NA, bty = "n", inset = c(0,-0.1), horiz = T)

# Disturbed areas
plot(bwp$year, bwp$density, type='n', xlim = c(2000,2020), ylim = c(100,400), 
     axes = F, xlab = "", ylab = "")
# BWP
lines(bwp$year, bwp$density.c5, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(100,400), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(bwp$year, bwp$density.c95, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(100,400), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(bwp$year, rev(bwp$year)), c(bwp$density.c95, rev(bwp$density.c5)), 
        col = "lightgreen", border = NA)
par(new=T)
plot(bwp$year, bwp$density, xlim = c(2000,2020), ylim = c(100,400), 
     axes = F, type = "l", lwd=2, col = "green4", xlab = "", ylab = "")
abline(coef(lm(bwp[,4] ~ bwp$year)), col = "green4", lty = 3, lwd = 2)
# PFR
par(new = T)
lines(pfr$year, pfr$density.c5, col = 'cadetblue1', xlim = c(2000,2020), ylim = c(100,400), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(pfr$year, pfr$density.c95, col = 'cadetblue1', xlim = c(2000,2020), ylim = c(100,400), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(pfr$year, rev(pfr$year)), c(pfr$density.c95, rev(pfr$density.c5)), 
        col = "cadetblue1", border = NA)
par(new=T)
plot(pfr$year, pfr$density, xlim = c(2000,2020), ylim = c(100,400), 
     axes = F, type = "l", lwd=2, col = "deepskyblue1", xlab = "", ylab = "")
# abline(coef(lm(pfr[,4] ~ pfr$year)), col = "deepskyblue1", lty = 3, lwd = 2)
# BFR
par(new = T)
lines(bfr$year, bfr$density.c5, col = 'darksalmon', xlim = c(2000,2020), 
      ylim = c(100,400), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(bfr$year, bfr$density.c95, col = 'darksalmon', xlim = c(2000,2020), 
      ylim = c(100,400), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(bfr$year, rev(bfr$year)), c(bfr$density.c95, rev(bfr$density.c5)), 
        col = "darksalmon", border = NA)
par(new=T)
plot(bfr$year, bfr$density, xlim = c(2000,2020), ylim = c(100,400), 
     axes = F, type = "l", lwd=2, col = "darkorange3", xlab = "", ylab = "")
abline(coef(lm(bfr[,4] ~ bfr$year)), col = "darkorange3", lty = 3, lwd = 2)
box()
axis(side = 1, at = seq(2000,2020,2), labels = NA)
axis(side = 2, at = seq(0,500,100), seq(0,500,100))
legend("topleft", c("BWP","BFR","PFR"), 
       fill = c("lightgreen","darksalmon","cadetblue1"), 
       border = NA, bty = "n", inset = c(0,-0.1), horiz = T)

# Exposed areas
plot(tun$year, tun$density, type='n', xlim = c(2000,2020), ylim = c(100,400), 
     axes = F, xlab = "", ylab = "")
# PPD
par(new = T)
lines(ppd$year, ppd$density.c5, col = 'mediumorchid1', xlim = c(2000,2020), 
      ylim = c(100,400), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(ppd$year, ppd$density.c95, col = 'mediumorchid1', xlim = c(2000,2020), 
      ylim = c(100,400), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(ppd$year, rev(ppd$year)), c(ppd$density.c95, rev(ppd$density.c5)), 
        col = "mediumorchid1", border = NA)
par(new=T)
plot(ppd$year, ppd$density, xlim = c(2000,2020), ylim = c(100,400), 
     axes = F, type = "l", lwd=2, col = "mediumorchid4", xlab = "", ylab = "")
# PPA
par(new = T)
lines(ppa$year, ppa$density.c5, col = 'darksalmon', xlim = c(2000,2020), 
      ylim = c(100,400), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(ppa$year, ppa$density.c95, col = 'darksalmon', xlim = c(2000,2020), 
      ylim = c(100,400), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(ppa$year, rev(ppa$year)), c(ppa$density.c95, rev(ppa$density.c5)), 
        col = "darksalmon", border = NA)
par(new=T)
plot(ppa$year, ppa$density, xlim = c(2000,2020), ylim = c(100,400), 
     axes = F, type = "l", lwd=2, col = "darkorange3", xlab = "", ylab = "")
# TUN
lines(tun$year, tun$density.c5, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(100,400), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(tun$year, tun$density.c95, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(100,400), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(tun$year, rev(tun$year)), c(tun$density.c95, rev(tun$density.c5)), 
        col = "lightgreen", border = NA)
par(new=T)
plot(tun$year, tun$density, xlim = c(2000,2020), ylim = c(100,400), 
     axes = F, type = "l", lwd=2, col = "green4", xlab = "", ylab = "")
abline(coef(lm(tun[,4] ~ tun$year)), col = "green4", lty = 3, lwd = 2)
# FEN
par(new = T)
lines(fen$year, fen$density.c5, col = 'cadetblue1', xlim = c(2000,2020), ylim = c(100,400), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(fen$year, fen$density.c95, col = 'cadetblue1', xlim = c(2000,2020), ylim = c(100,400), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(fen$year, rev(fen$year)), c(fen$density.c95, rev(fen$density.c5)), 
        col = "cadetblue1", border = NA)
par(new=T)
plot(fen$year, fen$density, xlim = c(2000,2020), ylim = c(100,400), 
     axes = F, type = "l", lwd=2, col = "deepskyblue1", xlab = "", ylab = "")
box()
axis(side = 1, at = seq(2000,2020,2), labels = seq(2000,2020,2))
axis(side = 2, at = seq(0,500,100), seq(0,500,100))
legend("topleft", c("TUN","FEN","PPA","PPD"), 
       fill = c("lightgreen","cadetblue1","darksalmon","mediumorchid1"), 
       border = NA, bty = "n", inset = c(0,-0.1), horiz = T)

mtext(side = 1, "Year", line = 1.5, cex = 0.75, outer = T)
# mtext(side = 2, "Density (cm)", line = 1.5, cex = 0.75, outer = T)
mtext(expression(paste("Density (kg m"^"-3",")")), side=2, line=1.5, cex = 0.75, outer = T)

####################################
####################################

## SWE
par(mfrow = c(4,1), mar = c(1,1,1,1), oma = c(3,3,0,0))
# TIS
plot(tis$year, tis$swe, type='n', xlim = c(2000,2020), 
     ylim = c(0,800), axes = F, xlab = "", ylab = "")
lines(tis$year, tis$swe.c5, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(0,800), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(tis$year, tis$swe.c95, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(0,800), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(tis$year, rev(tis$year)), c(tis$swe.c95, rev(tis$swe.c5)), 
        col = "lightgreen", border = NA)
par(new=T)
plot(tis$year, tis$swe, xlim = c(2000,2020), 
     ylim = c(0,800), axes = F, type = "l", lwd=2, 
     col = "green4", xlab = "", ylab = "")
box()
axis(side = 1, at = seq(2000,2020,2), labels = NA)
axis(side = 2, at = seq(0,800,200), seq(0,800,200))
legend("bottomleft", "TIS", fill = "lightgreen", border = NA, bty = "n", inset = c(0,-0.1))

# Treed areas
plot(air$year, air$swe, type='n', xlim = c(2000,2020), ylim = c(0,200), 
     axes = F, xlab = "", ylab = "")
# AIR
lines(air$year, air$swe.c5, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(0,200), xlab = "", ylab = "")
lines(air$year, air$swe.c95, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(0,200), xlab = "", ylab = "")
polygon(c(air$year, rev(air$year)), c(air$swe.c95, rev(air$swe.c5)), 
        col = "lightgreen", border = NA)
par(new=T)
plot(air$year, air$swe, xlim = c(2000,2020), ylim = c(0,200), 
     axes = F, type = "l", lwd=2, col = "green4", xlab = "", ylab = "")
# abline(coef(lm(air[,5] ~ air$year)), col = "green4", lty = 3, lwd = 2)
# BSW
par(new = T)
lines(bsw$year, bsw$swe.c5, col = 'cadetblue1', xlim = c(2000,2020), 
      ylim = c(0,200), xlab = "", ylab = "")
lines(bsw$year, bsw$swe.c95, col = 'cadetblue1', xlim = c(2000,2020), 
      ylim = c(0,200), xlab = "", ylab = "")
polygon(c(bsw$year, rev(bsw$year)), c(bsw$swe.c95, rev(bsw$swe.c5)), 
        col = "cadetblue1", border = NA)
par(new=T)
plot(bsw$year, bsw$swe, xlim = c(2000,2020), ylim = c(0,200), 
     axes = F, type = "l", lwd=2, col = "deepskyblue1", xlab = "", ylab = "")
# WSU
par(new = T)
lines(wsu$year, wsu$swe.c5, col = 'darksalmon', xlim = c(2000,2020), ylim = c(0,200), 
      xlab = "", ylab = "")
lines(wsu$year, wsu$swe.c95, col = 'darksalmon', xlim = c(2000,2020), ylim = c(0,200), 
      xlab = "", ylab = "")
polygon(c(wsu$year, rev(wsu$year)), c(wsu$swe.c95, rev(wsu$swe.c5)), 
        col = "darksalmon", border = NA)
par(new=T)
plot(wsu$year, wsu$swe, xlim = c(2000,2020), ylim = c(0,200), 
     axes = F, type = "l", lwd=2, col = "darkorange3", xlab = "", ylab = "")
box()
axis(side = 1, at = seq(2000,2020,2), labels = NA)
axis(side = 2, at = seq(0,200,50), seq(0,200,50))
legend("topleft", c("AIR","BSW","WSU"), 
       fill = c("lightgreen","cadetblue1","darksalmon"), 
       border = NA, bty = "n", inset = c(0,-0.1), horiz = T)


# Disturbed areas
plot(bwp$year, bwp$swe, type='n', xlim = c(2000,2020), ylim = c(0,200), 
     axes = F, xlab = "", ylab = "")
# BWP
lines(bwp$year, bwp$swe.c5, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(0,200), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(bwp$year, bwp$swe.c95, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(0,200), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(bwp$year, rev(bwp$year)), c(bwp$swe.c95, rev(bwp$swe.c5)), 
        col = "lightgreen", border = NA)
par(new=T)
plot(bwp$year, bwp$swe, xlim = c(2000,2020), ylim = c(0,200), 
     axes = F, type = "l", lwd=2, col = "green4", xlab = "", ylab = "")
# PFR
par(new = T)
lines(pfr$year, pfr$swe.c5, col = 'cadetblue1', xlim = c(2000,2020), ylim = c(40,200), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(pfr$year, pfr$swe.c95, col = 'cadetblue1', xlim = c(2000,2020), ylim = c(40,200), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(pfr$year, rev(pfr$year)), c(pfr$swe.c95, rev(pfr$swe.c5)), 
        col = "cadetblue1", border = NA)
par(new=T)
plot(pfr$year, pfr$swe, xlim = c(2000,2020), ylim = c(0,200), 
     axes = F, type = "l", lwd=2, col = "deepskyblue1", xlab = "", ylab = "")
# BFR
par(new = T)
lines(bfr$year, bfr$swe.c5, col = 'darksalmon', xlim = c(2000,2020), 
      ylim = c(0,200), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(bfr$year, bfr$swe.c95, col = 'darksalmon', xlim = c(2000,2020), 
      ylim = c(0,200), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(bfr$year, rev(bfr$year)), c(bfr$swe.c95, rev(bfr$swe.c5)), 
        col = "darksalmon", border = NA)
par(new=T)
plot(bfr$year, bfr$swe, xlim = c(2000,2020), ylim = c(0,200), 
     axes = F, type = "l", lwd=2, col = "darkorange3", xlab = "", ylab = "")
# abline(coef(lm(bfr[,5] ~ bfr$year)), col = "darkorange3", lty = 3, lwd = 2)
box()
axis(side = 1, at = seq(2000,2020,2), labels = NA)
axis(side = 2, at = seq(0,200,50), seq(0,200,50))
legend("topleft", c("BWP","BFR","PFR"), 
       fill = c("lightgreen","darksalmon","cadetblue1"), 
       border = NA, bty = "n", inset = c(0,-0.1), horiz = T)

# Exposed areas
plot(tun$year, tun$swe, type='n', xlim = c(2000,2020), ylim = c(0,200), 
     axes = F, xlab = "", ylab = "")
# PPD
par(new = T)
lines(ppd$year, ppd$swe.c5, col = 'mediumorchid1', xlim = c(2000,2020), 
      ylim = c(0,200), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(ppd$year, ppd$swe.c95, col = 'mediumorchid1', xlim = c(2000,2020), 
      ylim = c(0,200), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(ppd$year, rev(ppd$year)), c(ppd$swe.c95, rev(ppd$swe.c5)), 
        col = "mediumorchid1", border = NA)
par(new=T)
plot(ppd$year, ppd$swe, xlim = c(2000,2020), ylim = c(0,200), 
     axes = F, type = "l", lwd=2, col = "mediumorchid4", xlab = "", ylab = "")
# PPA
par(new = T)
lines(ppa$year, ppa$swe.c5, col = 'darksalmon', xlim = c(2000,2020), 
      ylim = c(0,200), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(ppa$year, ppa$swe.c95, col = 'darksalmon', xlim = c(2000,2020), 
      ylim = c(0,200), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(ppa$year, rev(ppa$year)), c(ppa$swe.c95, rev(ppa$swe.c5)), 
        col = "darksalmon", border = NA)
par(new=T)
plot(ppa$year, ppa$swe, xlim = c(2000,2020), ylim = c(0,200), 
     axes = F, type = "l", lwd=2, col = "darkorange3", xlab = "", ylab = "")
# TUN
lines(tun$year, tun$swe.c5, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(0,200), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(tun$year, tun$swe.c95, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(0,200), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(tun$year, rev(tun$year)), c(tun$swe.c95, rev(tun$swe.c5)), 
        col = "lightgreen", border = NA)
par(new=T)
plot(tun$year, tun$swe, xlim = c(2000,2020), ylim = c(0,200), 
     axes = F, type = "l", lwd=2, col = "green4", xlab = "", ylab = "")
# FEN
par(new = T)
lines(fen$year, fen$swe.c5, col = 'cadetblue1', xlim = c(2000,2020), ylim = c(0,200), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(fen$year, fen$swe.c95, col = 'cadetblue1', xlim = c(2000,2020), ylim = c(0,200), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(fen$year, rev(fen$year)), c(fen$swe.c95, rev(fen$swe.c5)), 
        col = "cadetblue1", border = NA)
par(new=T)
plot(fen$year, fen$swe, xlim = c(2000,2020), ylim = c(0,200), 
     axes = F, type = "l", lwd=2, col = "deepskyblue1", xlab = "", ylab = "")
box()
axis(side = 1, at = seq(2000,2020,2), labels = seq(2000,2020,2))
axis(side = 2, at = seq(0,200,50), seq(0,200,50))
legend("topleft", c("TUN","FEN","PPA","PPD"), 
       fill = c("lightgreen","cadetblue1","darksalmon","mediumorchid1"), 
       border = NA, bty = "n", inset = c(0,-0.1), horiz = T)
mtext(side = 1, "Year", line = 1.5, cex = 0.75, outer = T)
mtext(side = 2, "SWE (mm)", line = 1.5, cex = 0.75, outer = T)




####################################
####################################

## HTC
par(mfrow = c(4,1), mar = c(1,1,1,1), oma = c(3,3,0,0))
# TIS
plot(tis$year, tis$htc, type='n', xlim = c(2000,2020), 
     ylim = c(0,1), axes = F, xlab = "", ylab = "")
lines(tis$year, tis$htc.c5, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(0,1), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(tis$year, tis$htc.c95, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(0,1), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(tis$year, rev(tis$year)), c(tis$htc.c95, rev(tis$htc.c5)), 
        col = "lightgreen", border = NA)
par(new=T)
plot(tis$year, tis$htc, xlim = c(2000,2020), 
     ylim = c(0,1), axes = F, type = "l", lwd=2, 
     col = "green4", xlab = "", ylab = "")
abline(coef(lm(tis[,6] ~ tis$year)), col = "green4", lty = 3, lwd = 2)
box()
axis(side = 1, at = seq(2000,2020,2), labels = NA)
axis(side = 2, at = seq(0,1,0.5), seq(0,1,0.5))
legend("bottomleft", "TIS", fill = "lightgreen", border = NA, bty = "n", inset = c(0,-0.1))


# Treed areas
plot(air$year, air$htc, type='n', xlim = c(2000,2020), ylim = c(0,0.5), 
     axes = F, xlab = "", ylab = "")
# AIR
lines(air$year, air$htc.c5, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(0,0.5), xlab = "", ylab = "")
lines(air$year, air$htc.c95, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(0,0.5), xlab = "", ylab = "")
polygon(c(air$year, rev(air$year)), c(air$htc.c95, rev(air$htc.c5)), 
        col = "lightgreen", border = NA)
par(new=T)
plot(air$year, air$htc, xlim = c(2000,2020), ylim = c(0,0.5), 
     axes = F, type = "l", lwd=2, col = "green4", xlab = "", ylab = "")
# BSW
par(new = T)
lines(bsw$year, bsw$htc.c5, col = 'cadetblue1', xlim = c(2000,2020), 
      ylim = c(0,0.5), xlab = "", ylab = "")
lines(bsw$year, bsw$htc.c95, col = 'cadetblue1', xlim = c(2000,2020), 
      ylim = c(0,0.5), xlab = "", ylab = "")
polygon(c(bsw$year, rev(bsw$year)), c(bsw$htc.c95, rev(bsw$htc.c5)), 
        col = "cadetblue1", border = NA)
par(new=T)
plot(bsw$year, bsw$htc, xlim = c(2000,2020), ylim = c(0,0.5), 
     axes = F, type = "l", lwd=2, col = "deepskyblue1", xlab = "", ylab = "")
# WSU
par(new = T)
lines(wsu$year, wsu$htc.c5, col = 'darksalmon', xlim = c(2000,2020), ylim = c(0,0.5), 
      xlab = "", ylab = "")
lines(wsu$year, wsu$htc.c95, col = 'darksalmon', xlim = c(2000,2020), ylim = c(0,0.5), 
      xlab = "", ylab = "")
polygon(c(wsu$year, rev(wsu$year)), c(wsu$htc.c95, rev(wsu$htc.c5)), 
        col = "darksalmon", border = NA)
par(new=T)
plot(wsu$year, wsu$htc, xlim = c(2000,2020), ylim = c(0,0.5), 
     axes = F, type = "l", lwd=2, col = "darkorange3", xlab = "", ylab = "")
box()
axis(side = 1, at = seq(2000,2020,2), labels = NA)
axis(side = 2, at = seq(0,1,0.1), seq(0,1,0.1))
legend("topleft", c("AIR","BSW","WSU"), 
       fill = c("lightgreen","cadetblue1","darksalmon"), 
       border = NA, bty = "n", inset = c(0,-0.1), horiz = T)


# Disturbed areas
plot(bwp$year, bwp$htc, type='n', xlim = c(2000,2020), ylim = c(0,5), 
     axes = F, xlab = "", ylab = "")
# BWP
lines(bwp$year, bwp$htc.c5, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(0,5), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(bwp$year, bwp$htc.c95, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(0,5), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(bwp$year, rev(bwp$year)), c(bwp$htc.c95, rev(bwp$htc.c5)), 
        col = "lightgreen", border = NA)
par(new=T)
plot(bwp$year, bwp$htc, xlim = c(2000,2020), ylim = c(0,5), 
     axes = F, type = "l", lwd=2, col = "green4", xlab = "", ylab = "")
abline(coef(lm(bwp[,6] ~ bwp$year)), col = "green4", lty = 3, lwd = 2)
# PFR
par(new = T)
lines(pfr$year, pfr$htc.c5, col = 'cadetblue1', xlim = c(2000,2020), ylim = c(0,5), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(pfr$year, pfr$htc.c95, col = 'cadetblue1', xlim = c(2000,2020), ylim = c(0,5), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(pfr$year, rev(pfr$year)), c(pfr$htc.c95, rev(pfr$htc.c5)), 
        col = "cadetblue1", border = NA)
par(new=T)
plot(pfr$year, pfr$htc, xlim = c(2000,2020), ylim = c(0,5), 
     axes = F, type = "l", lwd=2, col = "deepskyblue1", xlab = "", ylab = "")
# BFR
par(new = T)
lines(bfr$year, bfr$htc.c5, col = 'darksalmon', xlim = c(2000,2020), 
      ylim = c(0,5), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(bfr$year, bfr$htc.c95, col = 'darksalmon', xlim = c(2000,2020), 
      ylim = c(0,5), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(bfr$year, rev(bfr$year)), c(bfr$htc.c95, rev(bfr$htc.c5)), 
        col = "darksalmon", border = NA)
par(new=T)
plot(bfr$year, bfr$htc, xlim = c(2000,2020), ylim = c(0,5), 
     axes = F, type = "l", lwd=2, col = "darkorange3", xlab = "", ylab = "")
box()
axis(side = 1, at = seq(2000,2020,2), labels = NA)
axis(side = 2, at = seq(0,5,1), seq(0,5,1))
legend("topleft", c("BWP","BFR","PFR"), 
       fill = c("lightgreen","darksalmon","cadetblue1"), 
       border = NA, bty = "n", inset = c(0,-0.1), horiz = T)

# Exposed areas
plot(tun$year, tun$htc, type='n', xlim = c(2000,2020), ylim = c(0,12), 
     axes = F, xlab = "", ylab = "")
# PPD
par(new = T)
lines(ppd$year, ppd$htc.c5, col = 'mediumorchid1', xlim = c(2000,2020), 
      ylim = c(0,12), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(ppd$year, ppd$htc.c95, col = 'mediumorchid1', xlim = c(2000,2020), 
      ylim = c(0,12), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(ppd$year, rev(ppd$year)), c(ppd$htc.c95, rev(ppd$htc.c5)), 
        col = "mediumorchid1", border = NA)
par(new=T)
plot(ppd$year, ppd$htc, xlim = c(2000,2020), ylim = c(0,12), 
     axes = F, type = "l", lwd=2, col = "mediumorchid4", xlab = "", ylab = "")
# PPA
par(new = T)
lines(ppa$year, ppa$htc.c5, col = 'darksalmon', xlim = c(2000,2020), 
      ylim = c(0,12), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(ppa$year, ppa$htc.c95, col = 'darksalmon', xlim = c(2000,2020), 
      ylim = c(0,12), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(ppa$year, rev(ppa$year)), c(ppa$htc.c95, rev(ppa$htc.c5)), 
        col = "darksalmon", border = NA)
par(new=T)
plot(ppa$year, ppa$htc, xlim = c(2000,2020), ylim = c(0,12), 
     axes = F, type = "l", lwd=2, col = "darkorange3", xlab = "", ylab = "")
# TUN
lines(tun$year, tun$htc.c5, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(0,12), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(tun$year, tun$htc.c95, col = 'lightgreen', xlim = c(2000,2020), 
      ylim = c(0,12), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(tun$year, rev(tun$year)), c(tun$htc.c95, rev(tun$htc.c5)), 
        col = "lightgreen", border = NA)
par(new=T)
plot(tun$year, tun$htc, xlim = c(2000,2020), ylim = c(0,12), 
     axes = F, type = "l", lwd=2, col = "green4", xlab = "", ylab = "")
# FEN
par(new = T)
lines(fen$year, fen$htc.c5, col = 'cadetblue1', xlim = c(2000,2020), ylim = c(0,12), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(fen$year, fen$htc.c95, col = 'cadetblue1', xlim = c(2000,2020), ylim = c(0,12), 
      yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(fen$year, rev(fen$year)), c(fen$htc.c95, rev(fen$htc.c5)), 
        col = "cadetblue1", border = NA)
par(new=T)
plot(fen$year, fen$htc, xlim = c(2000,2020), ylim = c(0,12), 
     axes = F, type = "l", lwd=2, col = "deepskyblue1", xlab = "", ylab = "")
box()
axis(side = 1, at = seq(2000,2020,2), labels = seq(2000,2020,2))
axis(side = 2, at = seq(0,12,3), seq(0,12,3))
legend("topleft", c("TUN","FEN","PPA","PPD"), 
       fill = c("lightgreen","cadetblue1","darksalmon","mediumorchid1"), 
       border = NA, bty = "n", inset = c(0,-0.1), horiz = T)

mtext(side = 1, "Year", line = 1.5, cex = 0.75, outer = T)
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=1.5, cex = 0.75, outer = T)

















############################

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






snow.cores <- read.csv(file = "~/Desktop/Workspace/EW/SnowCores2015.2.csv", header = TRUE)

snow.sub <- snow.cores[(snow.cores$site %in% c("WSU", "TIS", "TUN")), ]


# Export at 6 x 7.5
par(ps = 12, cex = 1, cex.axis = 1)
par(mfrow = c(3, 1))
par(mar = c(1, 5, 0.5, 1) , oma = c(2, 2, 0.5, 2))

colors = c(rep("darkgreen",13),rep("green",13),rep("yellow",13))
x1 <- factor(snow.cores$site, levels = c("WSU", "TIS", "TUN"))
boxplot(depth ~ year*x1, data = snow.cores, xaxt = 'n', ylim = c(0,300), col = colors, ylab = "Depth (cm)")
axis(side=1, at = 1:39, labels=FALSE)
legend("topleft", "Spruce", bty = "n", text.font = 2, inset = c(0,-0.15))
boxplot(density ~ year*x1, data = snow.cores, xaxt = 'n', ylim = c(0,600), col = colors, ylab = expression("Density" ~ (kg ~ m^{-3})))
axis(side=1, at = 1:39, labels=FALSE)
boxplot(htc ~ year*x1, data = snow.cores, xaxt = 'n', ylim = c(0,30), col = colors, ylab = expression("HTC" ~ (W ~ m^{-2} ~ K^{-1})))
axis(side=1, at = 1:39, labels=FALSE)


par(cex = 0.75)
par(mfrow = c(2, 2))
par(mar = c(2, 5, 0.5, 1) , oma = c(2, 10, 0.5, 10))
boxplot(depth ~ year + zone, data = snow.cores.all[snow.cores.all$site == "ROK", c("year","zone", "depth","density","swe", "htc")]
        , col = c("green", "blue", "purple", "yellow"), ylim = c(0,175), ylab = "Depth (cm)")
boxplot(density ~ year + zone, data = snow.cores.all[snow.cores.all$site == "ROK", c("year","zone", "depth","density","swe", "htc")]
        , col = c("green", "blue", "purple", "yellow"), ylim = c(0,500), ylab = expression("Density" ~ (kg ~ m^{-3})))
boxplot(swe ~ year + zone, data = snow.cores.all[snow.cores.all$site == "ROK", c("year","zone", "depth","density","swe", "htc")]
        , col = c("green", "blue", "purple", "yellow"), ylim = c(0,500), ylab = "SWE (mm)")
boxplot(htc ~ year + zone, data = snow.cores.all[snow.cores.all$site == "ROK", c("year","zone", "depth","density","swe", "htc")]
        , col = c("green", "blue", "purple", "yellow"), ylim = c(0,15), ylab = expression("HTC" ~ (W ~ m^{-2} ~ K^{-1})))

par(cex = 0.75)
par(mfrow = c(2, 2))
par(mar = c(5, 5, 0.5, 1) , oma = c(2, 10, 0.5, 10))
boxplot(depth ~ year + zone, data = snow.cores.all[snow.cores.all$site == "RA2", c("year","zone", "depth","density","swe", "htc")]
        , col = c("green", "blue", "purple", "yellow"), ylim = c(0,175), ylab = "Depth (cm)")
boxplot(density ~ year + zone, data = snow.cores.all[snow.cores.all$site == "RA2", c("year","zone", "depth","density","swe", "htc")]
        , col = c("green", "blue", "purple", "yellow"), ylim = c(0,500), ylab = expression("Density" ~ (kg ~ m^{-3})))
boxplot(swe ~ year + zone, data = snow.cores.all[snow.cores.all$site == "RA2", c("year","zone", "depth","density","swe", "htc")]
        , col = c("green", "blue", "purple", "yellow"), ylim = c(0,500), ylab = "SWE (mm)")
boxplot(htc ~ year + zone, data = snow.cores.all[snow.cores.all$site == "RA2", c("year","zone", "depth","density","swe", "htc")]
        , col = c("green", "blue", "purple", "yellow"), ylim = c(0,15), ylab = expression("HTC" ~ (W ~ m^{-2} ~ K^{-1})))

par(cex = 0.75)
par(mfrow = c(2, 2))
par(mar = c(5, 5, 0.5, 1) , oma = c(2, 10, 0.5, 10))
boxplot(depth ~ year + zone, data = snow.cores.all[snow.cores.all$site == "RID", c("year","zone", "depth","density","swe", "htc")]
        , col = c("green", "blue", "purple", "yellow"), ylim = c(0,175), ylab = "Depth (cm)")
boxplot(density ~ year + zone, data = snow.cores.all[snow.cores.all$site == "RID", c("year","zone", "depth","density","swe", "htc")]
        , col = c("green", "blue", "purple", "yellow"), ylim = c(0,500), ylab = expression("Density" ~ (kg ~ m^{-3})))
boxplot(swe ~ year + zone, data = snow.cores.all[snow.cores.all$site == "RID", c("year","zone", "depth","density","swe", "htc")]
        , col = c("green", "blue", "purple", "yellow"), ylim = c(0,500), ylab = "SWE (mm)")
boxplot(htc ~ year + zone, data = snow.cores.all[snow.cores.all$site == "RID", c("year","zone", "depth","density","swe", "htc")]
        , col = c("green", "blue", "purple", "yellow"), ylim = c(0,15), ylab = expression("HTC" ~ (W ~ m^{-2} ~ K^{-1})))

########

par(cex = 0.75)
par(mfrow = c(2, 2))
par(mar = c(5, 5, 0.5, 1) , oma = c(2, 10, 0.5, 10))
boxplot(depth ~ date + pit, data = snow.cores, ylim = c(0,300), 
        col = c("darkgreen", "green", "yellow"), ylab = "Depth (cm)")
boxplot(density ~ date + pit, data = snow.cores, ylim = c(0,500), 
        col = c("darkgreen", "green", "yellow"), ylab = expression("Density" ~ (kg ~ m^{-3})))
boxplot(swe ~ date + type, data = snow.cores, ylim = c(0,600), 
        col = c("darkgreen", "green", "yellow"), ylab = "SWE (mm)")
boxplot(htc ~ date + type, data = snow.cores, ylim = c(0, 17.5), 
        col = c("darkgreen", "green", "yellow"), ylab = expression("HTC" ~ (W ~ m^{-2} ~ K^{-1})))



boxplot(htc ~ date, data = snow.cores[snow.cores$site == "TUN",c("date","htc")], ylim = c(0,25))
str(snow.cores$date)


