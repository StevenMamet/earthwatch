library(multcomp) # Simultaneous Inference in General Parametric Models 
library(vegan)
library(cluster)
library(gclus) # Used for plotting clusters
library(car) # Used for determining variance inflation factors
library(labdsv) # PCA
library(scales) # For adjusting transparency
library(zoo) # Interpolating snow data

rm(list = ls())

# Plot data for tree islands with any trends present
island.snow <- read.csv(file = "~/Desktop/Workspace/Earthwatch/treeisland.snow.csv", header = TRUE)
names(island.snow)

island.mean <- aggregate(island.snow[,9:12], list(as.factor(island.snow$year), as.factor(island.snow$site)), mean)
names(island.mean) <- c("year", "site", "depth", "density", "swe", "htc")
island.mean$year <- as.numeric(as.character(island.mean$year))

tis.snow <- subset(island.mean, site == "TIS")
t10.snow <- subset(island.mean, site == "T10")
t11.snow <- subset(island.mean, site == "T11")
t12.snow <- subset(island.mean, site == "T12")
t13.snow <- subset(island.mean, site == "T13")

# Add the missing year of 2016
t10.snow <- rbind(t10.snow[c(1:4),], c("2016","T10",NA,NA,NA,NA), t10.snow[c(5:6),])
t10.snow$year <- as.integer(t10.snow$year)
t10.snow$depth <- as.numeric(t10.snow$depth)
t10.snow$density <- as.numeric(t10.snow$density)
t10.snow$swe <- as.numeric(t10.snow$swe)
t10.snow$htc <- as.numeric(t10.snow$htc)

t11.snow <- rbind(t11.snow[c(1:4),], c("2016","T11",NA,NA,NA,NA), t11.snow[c(5:6),])
t11.snow$year <- as.integer(t11.snow$year)
t11.snow$depth <- as.numeric(t11.snow$depth)
t11.snow$density <- as.numeric(t11.snow$density)
t11.snow$swe <- as.numeric(t11.snow$swe)
t11.snow$htc <- as.numeric(t11.snow$htc)

t12.snow <- rbind(t12.snow[c(1:4),], c("2016","T12",NA,NA,NA,NA), t12.snow[c(5:6),])
t12.snow$year <- as.integer(t12.snow$year)
t12.snow$depth <- as.numeric(t12.snow$depth)
t12.snow$density <- as.numeric(t12.snow$density)
t12.snow$swe <- as.numeric(t12.snow$swe)
t12.snow$htc <- as.numeric(t12.snow$htc)

t13.snow <- rbind(t13.snow[c(1:4),], c("2016","T13",NA,NA,NA,NA), t13.snow[c(5:6),])
t13.snow$year <- as.integer(t13.snow$year)
t13.snow$depth <- as.numeric(t13.snow$depth)
t13.snow$density <- as.numeric(t13.snow$density)
t13.snow$swe <- as.numeric(t13.snow$swe)
t13.snow$htc <- as.numeric(t13.snow$htc)

# Depth
summary(lm(tis.snow$depth ~ tis.snow$year)) # P = 0.04636
summary(lm(t10.snow$depth ~ t10.snow$year)) # NS
summary(lm(t11.snow$depth ~ t11.snow$year)) # NS
summary(lm(t12.snow$depth ~ t12.snow$year)) # NS
summary(lm(t13.snow$depth ~ t13.snow$year)) # P = 0.04431
# Density
summary(lm(tis.snow$density ~ tis.snow$year)) # NS
summary(lm(t10.snow$density ~ t10.snow$year)) # NS
summary(lm(t11.snow$density ~ t11.snow$year)) # NS
summary(lm(t12.snow$density ~ t12.snow$year)) # NS
summary(lm(t13.snow$density ~ t13.snow$year)) # P = 0.01841
# SWE
summary(lm(tis.snow$swe ~ tis.snow$year)) # NS
summary(lm(t10.snow$swe ~ t10.snow$year)) # NS
summary(lm(t11.snow$swe ~ t11.snow$year)) # NS
summary(lm(t12.snow$swe ~ t12.snow$year)) # NS
summary(lm(t13.snow$swe ~ t13.snow$year)) # NS
# HTC
summary(lm(tis.snow$htc ~ tis.snow$year)) # P = 0.04301
summary(lm(t10.snow$htc ~ t10.snow$year)) # NS
summary(lm(t11.snow$htc ~ t11.snow$year)) # NS
summary(lm(t12.snow$htc ~ t12.snow$year)) # P = 0.03705
summary(lm(t13.snow$htc ~ t13.snow$year)) # NS

## Fill 2016
# Depth
t13.depth.lm <- lm(t13.snow$depth ~ tis.snow$depth)
summary(t13.depth.lm)

t13.snow2 <- t13.snow
t13.snow2$depth[is.na(t13.snow2$depth)] <- coef(summary(t13.depth.lm))[[1]] + 
                                         coef(summary(t13.depth.lm))[[2]]*tis.snow$depth[tis.snow$year == 2016]-26


t10.depth.lm <- lm(t10.snow$depth ~ tis.snow$depth + t13.snow2$depth)
t11.depth.lm <- lm(t11.snow$depth ~ tis.snow$depth + t13.snow2$depth)
t12.depth.lm <- lm(t12.snow$depth ~ tis.snow$depth + t13.snow2$depth)
summary(t10.depth.lm)
summary(t11.depth.lm)
summary(t12.depth.lm)

t10.snow2 <- t10.snow
t10.snow2$depth[is.na(t10.snow2$depth)] <- 
  coef(summary(t10.depth.lm))[[1]] + 
  coef(summary(t10.depth.lm))[[2]]*tis.snow$depth[tis.snow$year == 2016] +
  coef(summary(t10.depth.lm))[[3]]*t13.snow2$depth[t13.snow2$year == 2016]

t11.snow2 <- t11.snow
t11.snow2$depth[is.na(t11.snow2$depth)] <- 
  coef(summary(t11.depth.lm))[[1]] + 
  coef(summary(t11.depth.lm))[[2]]*tis.snow$depth[tis.snow$year == 2016] +
  coef(summary(t11.depth.lm))[[3]]*t13.snow2$depth[t13.snow2$year == 2016]

t12.snow2 <- t12.snow
t12.snow2$depth[is.na(t12.snow2$depth)] <- 
  coef(summary(t12.depth.lm))[[1]] + 
  coef(summary(t12.depth.lm))[[2]]*tis.snow$depth[tis.snow$year == 2016] +
  coef(summary(t12.depth.lm))[[3]]*t13.snow2$depth[t13.snow2$year == 2016]

# Density
t10.density.lm <- lm(t10.snow$density ~ tis.snow$density)
t11.density.lm <- lm(t11.snow$density ~ tis.snow$density)
t12.density.lm <- lm(t12.snow$density ~ tis.snow$density)
t13.density.lm <- lm(t13.snow$density ~ tis.snow$density)
summary(t10.density.lm)
summary(t11.density.lm)
summary(t12.density.lm)
summary(t13.density.lm)

t11.snow2$density[is.na(t11.snow2$density)] <- coef(summary(t11.density.lm))[[1]] + 
  coef(summary(t11.density.lm))[[2]]*tis.snow$density[tis.snow$year == 2016]

t12.snow2$density[is.na(t12.snow2$density)] <- coef(summary(t12.density.lm))[[1]] + 
  coef(summary(t12.density.lm))[[2]]*tis.snow$density[tis.snow$year == 2016]


t10.density.lm <- lm(t10.snow$density ~ tis.snow$density + t11.snow2$density + t12.snow2$density)
t13.density.lm <- lm(t13.snow$density ~ tis.snow$density + t11.snow2$density + t12.snow2$density)
summary(t10.density.lm)
summary(t13.density.lm)

t10.snow2$density[is.na(t10.snow2$density)] <- 
  coef(summary(t10.density.lm))[[1]] + 
  coef(summary(t10.density.lm))[[2]]*tis.snow$density[tis.snow$year == 2016] +
  coef(summary(t10.density.lm))[[3]]*t11.snow2$density[t11.snow2$year == 2016] +
  coef(summary(t10.density.lm))[[4]]*t12.snow2$density[t12.snow2$year == 2016]

t13.snow2$density[is.na(t13.snow2$density)] <- 
  coef(summary(t13.density.lm))[[1]] + 
  coef(summary(t13.density.lm))[[2]]*tis.snow$density[tis.snow$year == 2016] +
  coef(summary(t13.density.lm))[[3]]*t11.snow2$density[t11.snow2$year == 2016] +
  coef(summary(t13.density.lm))[[4]]*t12.snow2$density[t12.snow2$year == 2016]

# SWE
t10.swe.lm <- lm(t10.snow$swe ~ t10.snow2$depth)
t11.swe.lm <- lm(t11.snow$swe ~ t11.snow2$depth)
t12.swe.lm <- lm(t12.snow$swe ~ t12.snow2$depth)
t13.swe.lm <- lm(t13.snow$swe ~ t13.snow2$depth)
summary(t10.swe.lm)
summary(t11.swe.lm)
summary(t12.swe.lm)
summary(t13.swe.lm)

t10.snow2$swe[is.na(t10.snow2$swe)] <- coef(summary(t10.swe.lm))[[1]] + 
  coef(summary(t10.swe.lm))[[2]]*t10.snow2$depth[t10.snow2$year == 2016]

t11.snow2$swe[is.na(t11.snow2$swe)] <- coef(summary(t11.swe.lm))[[1]] + 
  coef(summary(t11.swe.lm))[[2]]*t11.snow2$depth[t11.snow2$year == 2016]

t12.snow2$swe[is.na(t12.snow2$swe)] <- coef(summary(t12.swe.lm))[[1]] + 
  coef(summary(t12.swe.lm))[[2]]*t12.snow2$depth[t12.snow2$year == 2016]

t13.snow2$swe[is.na(t13.snow2$swe)] <- coef(summary(t13.swe.lm))[[1]] + 
  coef(summary(t13.swe.lm))[[2]]*t13.snow2$depth[t13.snow2$year == 2016]

# HTC
t10.htc.lm <- lm(t10.snow$htc ~ t10.snow2$depth + t10.snow2$density)
t11.htc.lm <- lm(t11.snow$htc ~ t11.snow2$depth + t11.snow2$density)
t12.htc.lm <- lm(t12.snow$htc ~ t12.snow2$depth + t12.snow2$density)
t13.htc.lm <- lm(t13.snow$htc ~ t13.snow2$depth + t13.snow2$density)
summary(t10.htc.lm)
summary(t11.htc.lm)
summary(t12.htc.lm)
summary(t13.htc.lm)

t10.snow2$htc[is.na(t10.snow2$htc)] <- coef(summary(t10.htc.lm))[[1]] + 
  coef(summary(t10.htc.lm))[[2]]*t10.snow2$depth[t10.snow2$year == 2016] +
  coef(summary(t10.htc.lm))[[3]]*t10.snow2$density[t10.snow2$year == 2016]

t11.snow2$htc[is.na(t11.snow2$htc)] <- coef(summary(t11.htc.lm))[[1]] + 
  coef(summary(t11.htc.lm))[[2]]*t11.snow2$depth[t11.snow2$year == 2016] +
  coef(summary(t11.htc.lm))[[3]]*t11.snow2$density[t11.snow2$year == 2016]

t12.snow2$htc[is.na(t12.snow2$htc)] <- coef(summary(t12.htc.lm))[[1]] + 
  coef(summary(t12.htc.lm))[[2]]*t12.snow2$depth[t12.snow2$year == 2016] +
  coef(summary(t12.htc.lm))[[3]]*t12.snow2$density[t12.snow2$year == 2016]

t13.snow2$htc[is.na(t13.snow2$htc)] <- coef(summary(t13.htc.lm))[[1]] + 
  coef(summary(t13.htc.lm))[[2]]*t13.snow2$depth[t13.snow2$year == 2016] +
  coef(summary(t13.htc.lm))[[3]]*t13.snow2$density[t13.snow2$year == 2016]

# Depth
summary(lm(tis.snow$depth ~ tis.snow$year))   # P = 0.04636
summary(lm(t10.snow2$depth ~ t10.snow2$year)) # NS
summary(lm(t11.snow2$depth ~ t11.snow2$year)) # NS
summary(lm(t12.snow2$depth ~ t12.snow2$year)) # NS
summary(lm(t13.snow2$depth ~ t13.snow2$year)) # P = 0.0518

# Density
summary(lm(tis.snow$density ~ tis.snow$year))   # NS
summary(lm(t10.snow2$density ~ t10.snow2$year)) # P = 0.0289
summary(lm(t11.snow2$density ~ t11.snow2$year)) # NS
summary(lm(t12.snow2$density ~ t12.snow2$year)) # NS
summary(lm(t13.snow2$density ~ t13.snow2$year)) # P = 0.0201

# SWE
summary(lm(tis.snow$swe ~ tis.snow$year))   # P = 0.0638
summary(lm(t10.snow2$swe ~ t10.snow2$year)) # NS
summary(lm(t11.snow2$swe ~ t11.snow2$year)) # NS
summary(lm(t12.snow2$swe ~ t12.snow2$year)) # NS
summary(lm(t13.snow2$swe ~ t13.snow2$year)) # NS

# HTC
summary(lm(tis.snow$htc ~ tis.snow$year))   # P = 0.0430
summary(lm(t10.snow2$htc ~ t10.snow2$year)) # NS
summary(lm(t11.snow2$htc ~ t11.snow2$year)) # NS
summary(lm(t12.snow2$htc ~ t12.snow2$year)) # P = 0.0419
summary(lm(t13.snow2$htc ~ t13.snow2$year)) # P = 0.0728


## Export at 5 x 4
# Plot a 4-panel figure of the tree island snow
par(mar = c(2,2,0,0), oma = c(2,2,1,1))
par(mfrow = c(4,1))
# Plot the depth
plot(tis.snow$depth ~ tis.snow$year, type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(30,180), axes = FALSE, lwd = 2, col = "#c16131")
# abline(coef(lm(tis.snow$depth ~ tis.snow$year)), col = "#c16131", lty = 2, lwd = 2)
par(new=T)
plot(t10.snow2$depth ~ t10.snow2$year, type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(30,180), axes = FALSE, lwd = 2, col = "#974dcd")
par(new=T)
plot(t11.snow2$depth ~ t11.snow2$year, type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(30,180), axes = FALSE, lwd = 2, col = "#668c41")
par(new=T)
plot(t12.snow2$depth ~ t12.snow2$year, type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(30,180), axes = FALSE, lwd = 2, col = "#756db1")
par(new=T)
plot(t13.snow2$depth ~ t13.snow2$year, type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(30,180), axes = FALSE, lwd = 2, col = "#ba4c73")
# abline(coef(lm(t13.snow2$depth ~ t13.snow2$year)), col = "#ba4c73", lty = 2, lwd = 2)
box()
axis(side = 1, at = seq(2012,2018), labels = NA)
axis(side = 2, at = seq(40,180,30), seq(40,180,30))
mtext(side = 2, "Depth (cm)", line = 2.5, cex = 0.75)
legend("topright", c("TIS","T10","T11","T12","T13"), col = c("#c16131","#974dcd","#668c41","#756db1","#ba4c73"), 
       lwd = c(2,2,2,2), border = NA, bty = "n", y.intersp = 0.5, inset = c(0,-0.02))
rect(2015.5,0,2016.5,200, col = alpha("gray", 0.3), border = NA)
text(2016,165,"El Niño")

# Plot the density
plot(tis.snow$density ~ tis.snow$year, type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(225,375), axes = FALSE, lwd = 2, col = "#c16131")
par(new=T)
plot(t10.snow2$density ~ t10.snow2$year, type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(225,375), axes = FALSE, lwd = 2, col = "#974dcd")
par(new=T)
plot(t11.snow2$density ~ t11.snow2$year, type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(225,375), axes = FALSE, lwd = 2, col = "#668c41")
par(new=T)
plot(t12.snow2$density ~ t12.snow2$year, type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(225,375), axes = FALSE, lwd = 2, col = "#756db1")
par(new=T)
plot(t13.snow2$density ~ t13.snow2$year, type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(225,375), axes = FALSE, lwd = 2, col = "#ba4c73")
# abline(coef(lm(t13.snow$density ~ t13.snow$year)), col = "#ba4c73", lty = 3, lwd = 2)
box()
axis(side = 1, at = seq(2012,2018), labels = NA)
axis(side = 2, at = seq(200,400,50), seq(200,400,50))
rect(2015.5,0,2016.5,500, col = alpha("gray", 0.3), border = NA)
mtext(expression(paste("Density (kg m"^"-3",")")), side=2, line=2.2, cex = 0.75)

# Plot the swe
plot(tis.snow$swe ~ tis.snow$year, type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(100,600), axes = FALSE, lwd = 2, col = "#c16131")
par(new=T)
plot(t10.snow2$swe ~ t10.snow2$year, type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(100,600), axes = FALSE, lwd = 2, col = "#974dcd")
par(new=T)
plot(t11.snow2$swe ~ t11.snow2$year, type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(100,600), axes = FALSE, lwd = 2, col = "#668c41")
par(new=T)
plot(t12.snow2$swe ~ t12.snow2$year, type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(100,600), axes = FALSE, lwd = 2, col = "#756db1")
par(new=T)
plot(t13.snow2$swe ~ t13.snow2$year, type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(100,600), axes = FALSE, lwd = 2, col = "#ba4c73")
box()
axis(side = 1, at = seq(2012,2018), labels = NA)
axis(side = 2, at = seq(100,600,100), seq(100,600,100))
rect(2015.5,0,2016.5,1000, col = alpha("gray", 0.3), border = NA)
mtext("SWE (mm)", side=2, line=2.5, cex = 0.75)

# Plot the htc
plot(tis.snow$htc ~ tis.snow$year, type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(0.1,5.1), axes = FALSE, lwd = 2, col = "#c16131")
par(new=T)
plot(t10.snow2$htc ~ t10.snow2$year, type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(0.1,5.1), axes = FALSE, lwd = 2, col = "#974dcd")
par(new=T)
plot(t11.snow2$htc ~ t11.snow2$year, type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(0.1,5.1), axes = FALSE, lwd = 2, col = "#668c41")
par(new=T)
plot(t12.snow2$htc ~ t12.snow2$year, type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(0.1,5.1), axes = FALSE, lwd = 2, col = "#756db1")
par(new=T)
plot(t13.snow2$htc ~ t13.snow2$year, type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(0.1,5.1), axes = FALSE, lwd = 2, col = "#ba4c73")
box()
axis(side = 1, at = seq(2012,2018), labels = seq(2012,2018))
axis(side = 2, at = seq(0,5,1), seq(0,5,1))
mtext(side = 1, "Year", line = 0.5, outer = T, cex = 0.75)
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2.2, cex = 0.75)
rect(2015.5,0,2016.5,200, col = alpha("gray", 0.3), border = NA)




# Plot microclimate data with any trends present
micro <- read.csv(file = "~/Desktop/Workspace/Earthwatch/microclimate2018.csv", header = TRUE)
names(micro)

summary(lm(micro[,2] ~ micro$Row.Labels)) # airp0
summary(lm(micro[,3] ~ micro$Row.Labels)) # airp15
summary(lm(micro[,4] ~ micro$Row.Labels)) # airp8, b = -0.039, p = 0.02783
summary(lm(micro[,5] ~ micro$Row.Labels)) # bsw0
summary(lm(micro[,6] ~ micro$Row.Labels)) # bsw15
summary(lm(micro[,7] ~ micro$Row.Labels)) # bsw8
summary(lm(micro[,8] ~ micro$Row.Labels)) # tis0
summary(lm(micro[,9] ~ micro$Row.Labels)) # tis15
summary(lm(micro[,10] ~ micro$Row.Labels)) # tis8
summary(lm(micro[,11] ~ micro$Row.Labels)) # wsu0, b = -0.11255, p = 0.002758
summary(lm(micro[,12] ~ micro$Row.Labels)) # wsu15
summary(lm(micro[,13] ~ micro$Row.Labels)) # wsu8
summary(lm(micro[,14] ~ micro$Row.Labels)) # fen0
summary(lm(micro[,15] ~ micro$Row.Labels)) # fen15
summary(lm(micro[,16] ~ micro$Row.Labels)) # fen8
summary(lm(micro[,17] ~ micro$Row.Labels)) # bfr0
summary(lm(micro[,18] ~ micro$Row.Labels)) # bfr15
summary(lm(micro[,19] ~ micro$Row.Labels)) # bfr8
summary(lm(micro[,20] ~ micro$Row.Labels)) # pfr0
summary(lm(micro[,21] ~ micro$Row.Labels)) # pfr15
summary(lm(micro[,22] ~ micro$Row.Labels)) # pfr8, b = 0.07290, p = 0.02865
summary(lm(micro[,23] ~ micro$Row.Labels)) # bwp0
summary(lm(micro[,24] ~ micro$Row.Labels)) # bwp15
summary(lm(micro[,25] ~ micro$Row.Labels)) # bwp8, b = 0.09901, p = 0.003156
summary(lm(micro[,26] ~ micro$Row.Labels)) # ppac0
summary(lm(micro[,27] ~ micro$Row.Labels)) # ppac15
summary(lm(micro[,28] ~ micro$Row.Labels)) # ppac8
summary(lm(micro[,29] ~ micro$Row.Labels)) # ppdc0
summary(lm(micro[,30] ~ micro$Row.Labels)) # ppdc15
summary(lm(micro[,31] ~ micro$Row.Labels)) # ppdc8
summary(lm(micro[,32] ~ micro$Row.Labels)) # tun0
summary(lm(micro[,33] ~ micro$Row.Labels)) # tun15
summary(lm(micro[,34] ~ micro$Row.Labels)) # tun8

# Plot snow core data
snow.all <- read.csv(file = "~/Desktop/Workspace/Earthwatch/snow2018.csv", header = TRUE)

# Depth
summary(lm(snow.all[c(1:16),3] ~ snow.all[c(1:16),2])) # AIRf
summary(lm(snow.all[c(1:16),4] ~ snow.all[c(1:16),2])) # AIRp
summary(lm(snow.all[c(1:16),5] ~ snow.all[c(1:16),2])) # BFR
summary(lm(snow.all[c(1:16),6] ~ snow.all[c(1:16),2])) # BSW
summary(lm(snow.all[c(1:16),7] ~ snow.all[c(1:16),2])) # BWP
summary(lm(snow.all[c(1:16),8] ~ snow.all[c(1:16),2])) # FEN
summary(lm(snow.all[c(1:16),9] ~ snow.all[c(1:16),2])) # PFR
summary(lm(snow.all[c(1:16),10] ~ snow.all[c(1:16),2])) # PPAc, p = 0.0721
summary(lm(snow.all[c(1:16),11] ~ snow.all[c(1:16),2])) # PPAw
summary(lm(snow.all[c(1:16),12] ~ snow.all[c(1:16),2])) # PPDc
summary(lm(snow.all[c(1:16),13] ~ snow.all[c(1:16),2])) # PPDw
summary(lm(snow.all[c(1:16),14] ~ snow.all[c(1:16),2])) # TIS, p = 0.05934
summary(lm(snow.all[c(1:16),15] ~ snow.all[c(1:16),2])) # TUN
summary(lm(snow.all[c(1:16),16] ~ snow.all[c(1:16),2])) # WSU

# Density
summary(lm(snow.all[c(17:32),3] ~ snow.all[c(17:32),2])) # AIRf, p = 0.05177
summary(lm(snow.all[c(17:32),4] ~ snow.all[c(17:32),2])) # AIRp, p = 0.02325
summary(lm(snow.all[c(17:32),5] ~ snow.all[c(17:32),2])) # BFR, p = 0.04251
summary(lm(snow.all[c(17:32),6] ~ snow.all[c(17:32),2])) # BSW
summary(lm(snow.all[c(17:32),7] ~ snow.all[c(17:32),2])) # BWP
summary(lm(snow.all[c(17:32),8] ~ snow.all[c(17:32),2])) # FEN
summary(lm(snow.all[c(17:32),9] ~ snow.all[c(17:32),2])) # PFR, p = 0.03294
summary(lm(snow.all[c(17:32),10] ~ snow.all[c(17:32),2])) # PPAc
summary(lm(snow.all[c(17:32),11] ~ snow.all[c(17:32),2])) # PPAw
summary(lm(snow.all[c(17:32),12] ~ snow.all[c(17:32),2])) # PPDc
summary(lm(snow.all[c(17:32),13] ~ snow.all[c(17:32),2])) # PPDw
summary(lm(snow.all[c(17:32),14] ~ snow.all[c(17:32),2])) # TIS
summary(lm(snow.all[c(17:32),15] ~ snow.all[c(17:32),2])) # TUN
summary(lm(snow.all[c(17:32),16] ~ snow.all[c(17:32),2])) # WSU

# HTC
summary(lm(snow.all[c(33:48),3] ~ snow.all[c(33:48),2])) # AIRf
summary(lm(snow.all[c(33:48),4] ~ snow.all[c(33:48),2])) # AIRp
summary(lm(snow.all[c(33:48),5] ~ snow.all[c(33:48),2])) # BFR
summary(lm(snow.all[c(33:48),6] ~ snow.all[c(33:48),2])) # BSW
summary(lm(snow.all[c(33:48),7] ~ snow.all[c(33:48),2])) # BWP
summary(lm(snow.all[c(33:48),8] ~ snow.all[c(33:48),2])) # FEN
summary(lm(snow.all[c(33:48),9] ~ snow.all[c(33:48),2])) # PFR
summary(lm(snow.all[c(33:48),10] ~ snow.all[c(33:48),2])) # PPAc, p = 0.05863
summary(lm(snow.all[c(33:48),11] ~ snow.all[c(33:48),2])) # PPAw
summary(lm(snow.all[c(33:48),12] ~ snow.all[c(33:48),2])) # PPDc
summary(lm(snow.all[c(33:48),13] ~ snow.all[c(33:48),2])) # PPDw
summary(lm(snow.all[c(33:48),14] ~ snow.all[c(33:48),2])) # TIS, p = 0.01843
summary(lm(snow.all[c(33:48),15] ~ snow.all[c(33:48),2])) # TUN
summary(lm(snow.all[c(33:48),16] ~ snow.all[c(33:48),2])) # WSU

# Plot the depth for the treed areas
plot(snow.all[c(1:16),3] ~ snow.all[c(1:16),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(40,180), axes = FALSE, col = "green1")
par(new=T)
plot(snow.all[c(1:16),4] ~ snow.all[c(1:16),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(40,180), axes = FALSE, col = "green1", lty = 2)
par(new=T)
plot(snow.all[c(1:16),5] ~ snow.all[c(1:16),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(40,180), axes = FALSE, col = "orange1")
par(new=T)
plot(snow.all[c(1:16),6] ~ snow.all[c(1:16),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(40,180), axes = FALSE, col = "darkgreen")
par(new=T)
plot(snow.all[c(1:16),9] ~ snow.all[c(1:16),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
     ylim = c(40,180), axes = FALSE, col = "orange1", lty = 2)
par(new=T)
plot(snow.all[c(1:16),14] ~ snow.all[c(1:16),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(40,180), axes = FALSE, col = "royalblue1")
par(new=T)
plot(snow.all[c(1:16),16] ~ snow.all[c(1:16),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(40,180), axes = FALSE, col = "darkgreen", lty = 2)
abline(coef(lm(snow.all[c(1:16),14] ~ snow.all[c(1:16),2])), col = "royalblue1", lty = 3, lwd = 2)
box()
axis(side = 1, at = seq(2003,2018), labels = seq(2003,2018))
axis(side = 2, at = seq(40,180,30), seq(20,220,50))
mtext(side = 2, "Depth (cm)", line = 2.5)
par(xpd = NA)
legend(2002, 212, c("AIRf","AIRp","PFR","BFR"), col = c("green1","green1","orange1","orange1"), 
       lty = c(1,2,2,1),border = NA, bty = "n", horiz = TRUE)
legend(2002, 204, c("BSW","WSU","TIS"), col = c("darkgreen","darkgreen","royalblue1"), 
       lty = c(1,2,2),border = NA, bty = "n", horiz = TRUE)

# Density
plot(snow.all[c(17:32),3] ~ snow.all[c(17:32),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(140,400), axes = FALSE, col = "green1")
par(new=T)
plot(snow.all[c(17:32),4] ~ snow.all[c(17:32),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(140,400), axes = FALSE, col = "green1", lty = 2)
par(new=T)
plot(snow.all[c(17:32),5] ~ snow.all[c(17:32),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(140,400), axes = FALSE, col = "orange1")
par(new=T)
plot(snow.all[c(17:32),6] ~ snow.all[c(17:32),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(140,400), axes = FALSE, col = "darkgreen")
par(new=T)
plot(snow.all[c(17:32),9] ~ snow.all[c(17:32),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
     ylim = c(140,400), axes = FALSE, col = "orange1", lty = 2)
par(new=T)
plot(snow.all[c(17:32),14] ~ snow.all[c(17:32),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(140,400), axes = FALSE, col = "royalblue1")
par(new=T)
plot(snow.all[c(17:32),16] ~ snow.all[c(17:32),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(140,400), axes = FALSE, col = "darkgreen", lty = 2)
abline(coef(lm(snow.all[c(17:32),3] ~ snow.all[c(17:32),2])), col = "green1", lty = 3, lwd = 2)
abline(coef(lm(snow.all[c(17:32),4] ~ snow.all[c(17:32),2])), col = "green1", lty = 4, lwd = 2)
abline(coef(lm(snow.all[c(17:32),5] ~ snow.all[c(17:32),2])), col = "orange1", lty = 3, lwd = 2)
abline(coef(lm(snow.all[c(17:32),9] ~ snow.all[c(17:32),2])), col = "orange1", lty = 4, lwd = 2)
box()
axis(side = 1, at = seq(2003,2018), labels = seq(2003,2018))
axis(side = 2, at = seq(100,400,50), seq(100,400,50))
mtext(expression(paste("Density (kg m"^"-3",")")), side=2, line=2.5)
par(xpd = NA)
legend(2002, 460, c("AIRf","AIRp","PFR","BFR"), col = c("green1","green1","orange1","orange1"), 
       lty = c(1,2,2,1),border = NA, bty = "n", horiz = TRUE)
legend(2002, 442, c("BSW","WSU","TIS"), col = c("darkgreen","darkgreen","royalblue1"), 
       lty = c(1,2,2),border = NA, bty = "n", horiz = TRUE)

# HTC
plot(snow.all[c(33:48),3] ~ snow.all[c(33:48),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(0.1,0.65), axes = FALSE, col = "green1")
par(new=T)
plot(snow.all[c(33:48),4] ~ snow.all[c(33:48),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(0.1,0.65), axes = FALSE, col = "green1", lty = 2)
par(new=T)
plot(snow.all[c(33:48),5] ~ snow.all[c(33:48),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(0.1,0.65), axes = FALSE, col = "orange1")
par(new=T)
plot(snow.all[c(33:48),6] ~ snow.all[c(33:48),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(0.1,0.65), axes = FALSE, col = "darkgreen")
par(new=T)
plot(snow.all[c(33:48),9] ~ snow.all[c(33:48),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
     ylim = c(0.1,0.65), axes = FALSE, col = "orange1", lty = 2)
par(new=T)
plot(snow.all[c(33:48),14] ~ snow.all[c(33:48),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(0.1,0.65), axes = FALSE, col = "royalblue1")
par(new=T)
plot(snow.all[c(33:48),16] ~ snow.all[c(33:48),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(0.1,0.65), axes = FALSE, col = "darkgreen", lty = 2)
abline(coef(lm(snow.all[c(33:48),14] ~ snow.all[c(33:48),2])), col = "royalblue1", lty = 3, lwd = 2)
box()
axis(side = 1, at = seq(2003,2018), labels = seq(2003,2018))
axis(side = 2, at = seq(0.1,0.6,0.1), seq(0.1,0.6,0.1))
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2.5)
par(xpd = NA)
legend(2002, 0.78, c("AIRf","AIRp","PFR","BFR"), col = c("green1","green1","orange1","orange1"), 
       lty = c(1,2,2,1),border = NA, bty = "n", horiz = TRUE)
legend(2002, 0.74, c("BSW","WSU","TIS"), col = c("darkgreen","darkgreen","royalblue1"), 
       lty = c(1,2,2),border = NA, bty = "n", horiz = TRUE)

#############
# Plot the depth for the wind swept areas
# BWP
plot(snow.all[c(1:16),7] ~ snow.all[c(1:16),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(0,60), axes = FALSE, col = "darkgoldenrod4")
# FEN
par(new=T)
plot(snow.all[c(1:16),8] ~ snow.all[c(1:16),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(0,60), axes = FALSE, col = "red")
# PPAc
par(new=T)
plot(snow.all[c(1:16),10] ~ snow.all[c(1:16),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(0,60), axes = FALSE, col = "dodgerblue2")
abline(coef(lm(snow.all[c(1:16),10] ~ snow.all[c(1:16),2])), col = "dodgerblue2", lty = 3, lwd = 2)
# PPAw
par(new=T)
plot(snow.all[c(1:16),11] ~ snow.all[c(1:16),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(0,60), axes = FALSE, col = "dodgerblue2", lty = 2)
# PPDc
par(new=T)
plot(snow.all[c(1:16),12] ~ snow.all[c(1:16),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
     ylim = c(0,60), axes = FALSE, col = "cyan3")
# PPDw
par(new=T)
plot(snow.all[c(1:16),13] ~ snow.all[c(1:16),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(0,60), axes = FALSE, col = "cyan3", lty = 2)
# TUN
par(new=T)
plot(snow.all[c(1:16),15] ~ snow.all[c(1:16),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(0,60), axes = FALSE, col = "deeppink2")
box()
axis(side = 1, at = seq(2003,2018), labels = seq(2003,2018))
axis(side = 2, at = seq(0,60,15), seq(0,60,15))
mtext(side = 2, "Depth (cm)", line = 2.5)
par(xpd = NA)
legend(2002, 75, c("PPAc","PPAw","PPDc","PPDw"), col = c("dodgerblue2","dodgerblue2","cyan3","cyan3"), 
       lty = c(1,2,1,2),border = NA, bty = "n", horiz = TRUE)
legend(2002, 70, c("BWP","FEN","TUN"), col = c("darkgoldenrod4","red","deeppink2"), 
       lty = c(1,2,2),border = NA, bty = "n", horiz = TRUE)

# Plot the density for the wind swept areas
# BWP
plot(snow.all[c(17:32),7] ~ snow.all[c(17:32),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(100,300), axes = FALSE, col = "darkgoldenrod4")
# FEN
par(new=T)
plot(snow.all[c(17:32),8] ~ snow.all[c(17:32),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(100,300), axes = FALSE, col = "red")
# PPAc
par(new=T)
plot(snow.all[c(17:32),10] ~ snow.all[c(17:32),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(100,300), axes = FALSE, col = "dodgerblue2")
# PPAw
par(new=T)
plot(snow.all[c(17:32),11] ~ snow.all[c(17:32),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(100,300), axes = FALSE, col = "dodgerblue2", lty = 2)
# PPDc
par(new=T)
plot(snow.all[c(17:32),12] ~ snow.all[c(17:32),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
     ylim = c(100,300), axes = FALSE, col = "cyan3")
# PPDw
par(new=T)
plot(snow.all[c(17:32),13] ~ snow.all[c(17:32),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(100,300), axes = FALSE, col = "cyan3", lty = 2)
# TUN
par(new=T)
plot(snow.all[c(17:32),15] ~ snow.all[c(17:32),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(100,300), axes = FALSE, col = "deeppink2")
box()
axis(side = 1, at = seq(2003,2018), labels = seq(2003,2018))
axis(side = 2, at = seq(100,300,40), seq(100,300,40))
mtext(expression(paste("Density (kg m"^"-3",")")), side=2, line=2.5)
par(xpd = NA)
legend(2002, 345, c("PPAc","PPAw","PPDc","PPDw"), col = c("dodgerblue2","dodgerblue2","cyan3","cyan3"), 
       lty = c(1,2,1,2),border = NA, bty = "n", horiz = TRUE)
legend(2002, 331, c("BWP","FEN","TUN"), col = c("darkgoldenrod4","red","deeppink2"), 
       lty = c(1,2,2),border = NA, bty = "n", horiz = TRUE)

# Plot the HTC for the wind swept areas
# BWP
plot(snow.all[c(33:48),7] ~ snow.all[c(33:48),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(0,15), axes = FALSE, col = "darkgoldenrod4")
# FEN
par(new=T)
plot(snow.all[c(33:48),8] ~ snow.all[c(33:48),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(0,15), axes = FALSE, col = "red")
# PPAc
par(new=T)
plot(snow.all[c(33:48),10] ~ snow.all[c(33:48),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(0,15), axes = FALSE, col = "dodgerblue2")
abline(coef(lm(snow.all[c(33:48),10] ~ snow.all[c(33:48),2])), col = "dodgerblue2", lty = 3, lwd = 2)
# PPAw
par(new=T)
plot(snow.all[c(33:48),11] ~ snow.all[c(33:48),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(0,15), axes = FALSE, col = "dodgerblue2", lty = 2)
# PPDc
par(new=T)
plot(snow.all[c(33:48),12] ~ snow.all[c(33:48),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
     ylim = c(0,15), axes = FALSE, col = "cyan3")
# PPDw
par(new=T)
plot(snow.all[c(33:48),13] ~ snow.all[c(33:48),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(0,15), axes = FALSE, col = "cyan3", lty = 2)
# TUN
par(new=T)
plot(snow.all[c(33:48),15] ~ snow.all[c(33:48),2], type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(0,15), axes = FALSE, col = "deeppink2")
box()
axis(side = 1, at = seq(2003,2018), labels = seq(2003,2018))
axis(side = 2, at = seq(0,15,3), seq(0,15,3))
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2.5)
par(xpd = NA)
legend(2002, 18.5, c("PPAc","PPAw","PPDc","PPDw"), col = c("dodgerblue2","dodgerblue2","cyan3","cyan3"), 
       lty = c(1,2,1,2),border = NA, bty = "n", horiz = TRUE)
legend(2002, 17.5, c("BWP","FEN","TUN"), col = c("darkgoldenrod4","red","deeppink2"), 
       lty = c(1,2,2),border = NA, bty = "n", horiz = TRUE)

## Plotting the snow data from cameras and the field

cam <- read.csv("~/Desktop/Workspace/Earthwatch/ch_trail_cam.csv")
bp <- barplot(cam$tis.cam, col = alpha("darkgreen", 0.5), ylim = c(0, 2.7), 
              border = NA, xaxt = "n", yaxt = "n", xlab = "", ylab = "")

# PPA snow sampling 2015, 2016, 2017, 2018
bp[which(cam$date == "2015-02-18"),] # 145, 173.5
bp[which(cam$date == "2016-02-10"),] # 502, 601.9
bp[which(cam$date == "2017-02-24"),] # 882, 1057.9
bp[which(cam$date == "2018-02-16"),] # 1239, 1486.3

# PPD snow sampling 2015, 2016, 2017, 2018
bp[which(cam$date == "2015-02-12"),] # 139, 166.3
bp[which(cam$date == "2016-02-12"),] # 504, 604.3
bp[which(cam$date == "2017-03-01"),] # 887, 1063.9
bp[which(cam$date == "2018-02-19"),] # 1242, 1489.9

# TIS snow sampling 2015, 2016, 2017, 2018
bp[which(cam$date == "2015-02-14"),] # 141, 168.7
bp[which(cam$date == "2016-02-16"),] # 508, 609.1
bp[which(cam$date == "2017-02-26"),] # 884, 1060.3
bp[which(cam$date == "2018-02-17"),] # NA

# TUN snow sampling 2015, 2016, 2017, 2018
bp[which(cam$date == "2015-02-12"),] # 139, 166.3
bp[which(cam$date == "2016-02-14"),] # 506, 606.7
bp[which(cam$date == "2017-02-25"),] # 883, 1059.1
bp[which(cam$date == "2018-02-17"),] # NA

# WSU snow sampling 2015, 2016, 2017, 2018
bp[which(cam$date == "2015-02-19"),] # 146, 174.7
bp[which(cam$date == "2016-02-17"),] # 509, 610.3
bp[which(cam$date == "2017-02-27"),] # 885, 1061.5
bp[which(cam$date == "2018-02-17"),] # NA

# MLK snow sampling 2015, 2016, 2017, 2018
bp[which(cam$date == "2015-02-22"),] # 149, 178.3
bp[which(cam$date == "2016-02-25"),] # 517, 619.9
bp[which(cam$date == "2017-02-28"),] # 886, 1062.7
bp[which(cam$date == "2018-02-17"),] # NA

# RLK snow sampling 2015, 2016, 2017, 2018
bp[which(cam$date == "2015-03-10"),] # 165, 197.5
bp[which(cam$date == "2016-02-21"),] # 513, 615.1
bp[which(cam$date == "2017-02-20"),] # 878, 1053.1
bp[which(cam$date == "2018-02-17"),] # NA

# Years
bp[which(cam$date == "2015-01-01"),] # 97, 115.9
bp[which(cam$date == "2016-01-01"),] # 462, 553.9
bp[which(cam$date == "2017-01-01"),] # 828, 993.1
bp[which(cam$date == "2018-01-01"),] # 1193, 1431.1

# Plot the data

# Interpolating the missing values
# TIS
tis.1 <- data.frame(x = c(cam$id), y = c(cam$tis.cam))
tis.approx <- as.data.frame(na.approx(tis.1))
plot(tis.approx, type = "l")
cam2 <- cam
cam2$tis.cam[is.na(cam2$tis.cam)] <- tis.approx$y[match(tis.approx$x[is.na(cam2$tis.cam)],tis.approx$x)]
cam2$tis.cam[c(1:261)] <- 0
# RTI
rti.1 <- data.frame(x = c(cam$id), y = c(cam$rti.cam))
rti.approx <- as.data.frame(na.approx(rti.1))
plot(rti.approx, type = "l")
cam2$rti.cam[is.na(cam2$rti.cam)] <- rti.approx$y[match(rti.approx$x[is.na(cam2$rti.cam)],rti.approx$x)]
cam2$rti.cam[c(1:261,738:1239)] <- 0
# WSU
wsu.1 <- data.frame(x = c(cam$id), y = c(cam$wsu.cam))
wsu.approx <- as.data.frame(na.approx(wsu.1))
plot(wsu.approx, type = "l")
cam2$wsu.cam[is.na(cam2$wsu.cam)] <- wsu.approx$y[match(wsu.approx$x[is.na(cam2$wsu.cam)],wsu.approx$x)]
# MLK
mlk.1 <- data.frame(x = c(cam$id), y = c(cam$mlk.cam))
mlk.approx <- as.data.frame(na.approx(mlk.1))
plot(mlk.approx, type = "l")
cam2$mlk.cam[is.na(cam2$mlk.cam)] <- mlk.approx$y[match(mlk.approx$x[is.na(cam2$mlk.cam)],mlk.approx$x)]
# TUN
tun.1 <- data.frame(x = c(cam$id), y = c(cam$tun.cam))
tun.approx <- as.data.frame(na.approx(tun.1))
plot(tun.approx, type = "l")
cam2$tun.cam[is.na(cam2$tun.cam)] <- tun.approx$y[match(tun.approx$x[is.na(cam2$tun.cam)],tun.approx$x)]
# PPA
ppa.1 <- data.frame(x = c(cam$id), y = c(cam$ppa.cam))
ppa.approx <- as.data.frame(na.approx(ppa.1))
plot(ppa.approx, type = "l")
cam2$ppa.cam[is.na(cam2$ppa.cam)] <- ppa.approx$y[match(ppa.approx$x[is.na(cam2$ppa.cam)],ppa.approx$x)]
# RPP
rpp.1 <- data.frame(x = c(cam$id), y = c(cam$rpp.cam))
rpp.approx <- as.data.frame(na.approx(rpp.1))
plot(rpp.approx, type = "l")
cam2$rpp.cam[is.na(cam2$rpp.cam)] <- rpp.approx$y[match(rpp.approx$x[is.na(cam2$rpp.cam)],rpp.approx$x)]


# Forest-tundra
bp <- barplot(cam2$tis.cam, col = alpha("darkgreen", 0.5), ylim = c(0, 2.7), 
              border = NA, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
barplot(cam2$rti.cam, col = alpha("green", 0.5), border = NA, 
        add = TRUE, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
barplot(cam2$wsu.cam, col = alpha("orange", 0.5), border = NA, 
        add = TRUE, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
barplot(cam2$mlk.cam, col = alpha("red", 0.5), border = NA, 
        add = TRUE, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
barplot(cam2$tun.cam, col = alpha("blue", 0.5), border = NA, add = TRUE)
points(c(174.7, 610.3, 1061.5), c(0.581, 0.542, 0.881), col = "darkorange", bg = "orange", pch = 21)
points(c(168.7, 609.1, 1060.3), c(2.4025, 1.70, 2.212), col = "darkgreen", bg = "darkgreen", pch = 21)
points(c(197.5, 615.1, 1053.1), c(1.3096, 0.7348, 0.5218), col = "darkgreen", bg = "green", pch = 21)
points(c(166.3, 606.7, 1059.1), c(0.067, 0.079, 0.278), col = "darkblue", bg = "blue", pch = 21)
points(c(178.3, 619.9, 1062.7), c(0.638, 0.4609, 0.6248), col = "darkred", bg = "red", pch = 21)
box()
axis(side = 1, at = c(115.9,553.9,993.1,1431.1), labels = c(2015,2016,2017,2018))
axis(side = 1, at = c(115.9+219,553.9+219,993.1+219), labels = NA)
mtext(side = 2, "Depth (m)", line = 2.5)
par(xpd = NA)
legend(-50, 3, c("MLK","WSU","RTI","TIS","TUN"), fill = alpha(c("red","orange","green","darkgreen","blue"),0.5), border = NA, bty = "n", horiz = TRUE)
legend(-170, 3.2, "Forest-tundra", bty = "n", text.font = 2)

# Permafrost
bp <- barplot(cam2$rpp.cam, col = alpha("darkblue", 0.5), ylim = c(0, 2.7), 
              border = NA, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
barplot(cam2$ppa.cam, col = alpha("orange", 0.5), border = NA, 
        add = TRUE, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
barplot(cam2$ppd.cam, col = alpha("darkorchid2", 0.5), border = NA, add = TRUE)
points(c(173.5, 601.9, 1057.9, 1486.3), c(0.0727, 0.1266, 0.0651, 0.0946), col = "darkorange", bg = "orange", pch = 21)
points(c(168.7, 609.1, 1060.3, 1489.9), c(0.07004, 0.0665, 0.0474, 0.04623), col = "darkorchid2", bg = "darkorchid2", pch = 21)
points(c(197.5, 615.1, 1053.1), c(0.067, 0.079, 0.278), col = "darkblue", bg = "blue", pch = 21)
abline(h = 0.065, col = alpha("darkorange",0.8), lty = 2)
abline(h = 0.061, col = alpha("darkorchid2",0.8), lty = 2)
abline(h = 0.230, col = alpha("blue",0.6), lty = 2)
box()
axis(side = 1, at = c(115.9,553.9,993.1,1431.1), labels = c(2015,2016,2017,2018))
axis(side = 1, at = c(115.9+219,553.9+219,993.1+219), labels = NA)
mtext(side = 2, "Depth (m)", line = 2.5)
par(xpd = NA)
legend(-100, 3, c("RPP","PPA","PPD"), fill = alpha(c("darkblue","orange","darkorchid2"),0.5), border = NA, bty = "n", horiz = TRUE)
legend(750, 3, "Field data", pch = 16, col = alpha("gray70",0.8), border = "pink", bty = "n", horiz = TRUE, pt.cex = 1.5)
legend(1150, 3, "Mean", lty = 2, col = "gray", bty = "n", horiz = TRUE, pt.cex = 1.5)
legend(-170, 3.15, "Permafrost", bty = "n", text.font = 2)

# PPD 2017, 4.474
# PPD 2016, 6.65
# PPD 2015, 7.004

0.0727
0.1266
0.0651

bp[which(cam$date == "2015-02-18"),] # 145, 173.5
bp[which(cam$date == "2016-02-10"),] # 502, 601.9
bp[which(cam$date == "2017-02-24"),] # 882, 1057.9
bp[which(cam$date == "2018-02-17"),] # NA


## Plot both Churchill and Mackenzies temperatures for EW report 2016
temp.all <- read.csv(file = "~/Desktop/Workspace/EW/ch.mm.csv", header = TRUE)

## 
summary(lm(airp15~year, data = temp.all)) # Not sig
summary(lm(bsw15~year, data = temp.all)) # Not sig
summary(lm(tis15~year, data = temp.all)) # Not sig
summary(lm(wsu15~year, data = temp.all)) # Not sig
summary(lm(fen15~year, data = temp.all)) # Not sig
summary(lm(bfr15~year, data = temp.all)) # Not sig
summary(lm(bsw15~year, data = temp.all)) # Not sig
summary(lm(pfr15~year, data = temp.all)) # Not sig
summary(lm(ppac15~year, data = temp.all)) # Not sig
summary(lm(ppdc15~year, data = temp.all)) # Not sig

summary(lm(airp0~year, data = temp.all)) # Not sig
summary(lm(bsw0~year, data = temp.all)) # Not sig
summary(lm(tis0~year, data = temp.all)) # Not sig
summary(lm(wsu0~year, data = temp.all)) # Sig
summary(lm(fen0~year, data = temp.all)) # Not sig
summary(lm(bfr0~year, data = temp.all)) # Not sig
summary(lm(bsw0~year, data = temp.all)) # Not sig
summary(lm(pfr0~year, data = temp.all)) # Not sig
summary(lm(ppac0~year, data = temp.all)) # Not sig
summary(lm(ppdc0~year, data = temp.all)) # Not sig

summary(lm(airp8~year, data = temp.all)) # Not sig
summary(lm(bsw8~year, data = temp.all)) # Not sig
summary(lm(tis8~year, data = temp.all)) # Not sig
summary(lm(wsu8~year, data = temp.all)) # Not sig
summary(lm(fen8~year, data = temp.all)) # Not sig
summary(lm(bfr8~year, data = temp.all)) # Not sig
summary(lm(bsw8~year, data = temp.all)) # Not sig
summary(lm(pfr8~year, data = temp.all)) # Not sig
summary(lm(ppac8~year, data = temp.all)) # Not sig
summary(lm(ppdc8~year, data = temp.all)) # Not sig

summary(lm(hf15~year, data = temp.all)) # Not sig
summary(lm(bp15~year, data = temp.all)) # Not sig
summary(lm(d215~year, data = temp.all)) # Not sig
summary(lm(d615~year, data = temp.all)) # Sig
summary(lm(gf15~year, data = temp.all)) # Not sig

summary(lm(hf0~year, data = temp.all)) # Not sig
summary(lm(bp0~year, data = temp.all)) # Not sig
summary(lm(d20~year, data = temp.all)) # Not sig
summary(lm(d60~year, data = temp.all)) # Not sig
summary(lm(gf0~year, data = temp.all)) # Sig

summary(lm(hf150~year, data = temp.all)) # Not sig
summary(lm(bp150~year, data = temp.all)) # Not sig
summary(lm(d2150~year, data = temp.all)) # Not sig
summary(lm(d6150~year, data = temp.all)) # Not sig
summary(lm(gf150~year, data = temp.all)) # Sig

par(mfrow = c(3, 2))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 9 pts
par(mar = c(0.5, 0.5, 0, 0), oma = c(3,3,1,1))
# Export at 3.40 x 4.27 for 2-panel

## Panel A - Churchill air temperatures
plot(airp15~year, type = "l", lty = 1, ylim = c(-10,0), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "forestgreen",data = temp.all)
par(new=T)
plot(bsw15~year, type = "l", lty = 2, ylim = c(-10,0), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "forestgreen",data = temp.all)
par(new=T)
plot(tis15~year, type = "l", lty = 3, ylim = c(-10,0), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "forestgreen",data = temp.all)
par(new=T)
plot(wsu15~year, type = "l", lty = 4, ylim = c(-10,0), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "forestgreen",data = temp.all)
par(new=T)
plot(fen15~year, type = "l", lty = 2, ylim = c(-10,0), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "darkorange2",data = temp.all)
par(new=T)
plot(bfr15~year, type = "l", lty = 1, ylim = c(-10,0), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "blueviolet",data = temp.all)
par(new=T)
plot(pfr15~year, type = "l", lty = 2, ylim = c(-10,0), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "blueviolet",data = temp.all)
par(new=T)
plot(ppac15~year, type = "l", lty = 1, ylim = c(-10,0), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "dodgerblue1",data = temp.all)
par(new=T)
plot(ppdc15~year, type = "l", lty = 2, ylim = c(-10,0), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "dodgerblue1",data = temp.all)
par(new=T)
plot(tun15~year, type = "l", lty = 3, ylim = c(-10,0), xlim = c(1990,2016), xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = "dodgerblue1",data = temp.all)
axis(side = 1, at = seq(1990,2015,5), labels = NA, lwd = 1, tck = -0.03)
axis(side = 2, at = seq(-10,0,2), labels = NA, lwd = 1, tck = -0.03)
axis(side = 2, at = seq(-10,0,2), labels = seq(-10,0,2), line = -0.5, lwd = 1, tick = FALSE)
axis(side = 2, at = seq(-10,0,2), labels = seq(-10,0,2), line = -0.5, lwd = 1, tick = FALSE)

## Panel B - Mac Pass air temperatures
plot(hf15~year, type = "l", lty = 1, ylim = c(-10,0), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "darkred",data = temp.all)
par(new=T)
plot(bp15~year, type = "l", lty = 1, ylim = c(-10,0), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "firebrick1",data = temp.all)
par(new=T)
plot(d215~year, type = "l", lty = 1, ylim = c(-10,0), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "orange",data = temp.all)
par(new=T)
plot(d615~year, type = "l", lty = 1, ylim = c(-10,0), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "yellow",data = temp.all)
par(new=T)
plot(gf15~year, type = "l", lty = 1, ylim = c(-10,0), xlim = c(1990,2016), xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = "green",data = temp.all)
axis(side = 1, at = seq(1990,2015,5), labels = NA, lwd = 1, tck = -0.03)
axis(side = 2, at = seq(-10,0,2), labels = NA, lwd = 1, tck = -0.03)

## Panel C - Churchill ground surface temperatures
plot(airp0~year, type = "l", lty = 1, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "forestgreen",data = temp.all)
par(new=T)
plot(bsw0~year, type = "l", lty = 2, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "forestgreen",data = temp.all)
par(new=T)
plot(tis0~year, type = "l", lty = 3, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "forestgreen",data = temp.all)
par(new=T)
plot(wsu0~year, type = "l", lty = 4, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "forestgreen",data = temp.all)
par(new=T)
plot(fen0~year, type = "l", lty = 2, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "darkorange2",data = temp.all)
par(new=T)
plot(bfr0~year, type = "l", lty = 1, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "blueviolet",data = temp.all)
par(new=T)
plot(pfr0~year, type = "l", lty = 2, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "blueviolet",data = temp.all)
par(new=T)
plot(ppac0~year, type = "l", lty = 1, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "dodgerblue1",data = temp.all)
par(new=T)
plot(ppdc0~year, type = "l", lty = 2, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "dodgerblue1",data = temp.all)
par(new=T)
plot(tun0~year, type = "l", lty = 3, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = "dodgerblue1",data = temp.all)
mtext("Temperature °C", side = 2, line = 1.5)#, outer = TRUE)
axis(side = 1, at = seq(1990,2015,5), labels = NA, lwd = 1, tck = -0.03)
axis(side = 2, at = seq(-8,4,2), labels = NA, lwd = 1, tck = -0.03)
axis(side = 2, at = seq(-8,4,2), labels = seq(-8,4,2), line = -0.5, lwd = 1, tick = FALSE)

## Panel D - Mac Pass ground surface temperatures
plot(hf0~year, type = "l", lty = 1, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "darkred",data = temp.all)
par(new=T)
plot(bp0~year, type = "l", lty = 1, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "firebrick1",data = temp.all)
par(new=T)
plot(d20~year, type = "l", lty = 1, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "orange",data = temp.all)
par(new=T)
plot(d60~year, type = "l", lty = 1, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "yellow",data = temp.all)
par(new=T)
plot(gf0~year, type = "l", lty = 1, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = "green",data = temp.all)
axis(side = 1, at = seq(1990,2015,5), labels = NA, lwd = 1, tck = -0.03)
axis(side = 2, at = seq(-8,4,2), labels = NA, lwd = 1, tck = -0.03)

## Panel E - Churchill subsurface temperatures
plot(airp8~year, type = "l", lty = 1, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "forestgreen",data = temp.all)
par(new=T)
plot(bsw8~year, type = "l", lty = 2, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "forestgreen",data = temp.all)
par(new=T)
plot(tis8~year, type = "l", lty = 3, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "forestgreen",data = temp.all)
par(new=T)
plot(wsu8~year, type = "l", lty = 4, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "forestgreen",data = temp.all)
par(new=T)
plot(fen8~year, type = "l", lty = 2, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "darkorange2",data = temp.all)
par(new=T)
plot(bfr8~year, type = "l", lty = 1, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "blueviolet",data = temp.all)
par(new=T)
plot(pfr8~year, type = "l", lty = 2, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "blueviolet",data = temp.all)
par(new=T)
plot(ppac8~year, type = "l", lty = 1, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "dodgerblue1",data = temp.all)
par(new=T)
plot(ppdc8~year, type = "l", lty = 2, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "dodgerblue1",data = temp.all)
par(new=T)
plot(tun8~year, type = "l", lty = 3, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = "dodgerblue1",data = temp.all)
mtext("Year", side = 1, line = 1.5)
axis(side = 1, at = seq(1990,2015,5), labels = NA, lwd = 1, tck = -0.03)
axis(side = 1, at = seq(1990,2015,5), labels = seq(1990,2015,5), line = -0.5, lwd = 1, tick = FALSE)
axis(side = 2, at = seq(-8,4,2), labels = NA, lwd = 1, tck = -0.03)
axis(side = 2, at = seq(-8,4,2), labels = seq(-8,4,2), line = -0.5, lwd = 1, tick = FALSE)

## Panel F - Mac Pass subsurface temperatures
plot(hf150~year, type = "l", lty = 1, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "darkred",data = temp.all)
par(new=T)
plot(bp150~year, type = "l", lty = 1, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "firebrick1",data = temp.all)
par(new=T)
plot(d2150~year, type = "l", lty = 1, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "orange",data = temp.all)
par(new=T)
plot(d6150~year, type = "l", lty = 1, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", axes = FALSE, col = "yellow",data = temp.all)
par(new=T)
plot(gf150~year, type = "l", lty = 1, ylim = c(-8,4), xlim = c(1990,2016), xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = "green",data = temp.all)
mtext("Year", side = 1, line = 1.5)
axis(side = 1, at = seq(1990,2015,5), labels = NA, lwd = 1, tck = -0.03)
axis(side = 1, at = seq(1990,2015,5), labels = seq(1990,2015,5), line = -0.5, lwd = 1, tick = FALSE)
axis(side = 2, at = seq(-8,4,2), labels = NA, lwd = 1, tck = -0.03)


#### Plot just the Churchill temperatures

par(mfrow = c(3, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 9 pts
par(mar = c(0.5, 0.5, 0.1, 0.1), oma = c(3,3,1,1))
# Export at 3.40 x 4.27 for 2-panel

## Panel A - Churchill air temperatures
plot(airp15~year, type = "l", lty = 1, ylim = c(-10,0), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "forestgreen",data = temp.all)
par(new=T)
plot(bsw15~year, type = "l", lty = 2, ylim = c(-10,0), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "forestgreen",data = temp.all)
par(new=T)
plot(tis15~year, type = "l", lty = 3, ylim = c(-10,0), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "forestgreen",data = temp.all)
par(new=T)
plot(wsu15~year, type = "l", lty = 4, ylim = c(-10,0), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "forestgreen",data = temp.all)
par(new=T)
plot(fen15~year, type = "l", lty = 2, ylim = c(-10,0), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "darkorange2",data = temp.all)
par(new=T)
plot(bfr15~year, type = "l", lty = 1, ylim = c(-10,0), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "blueviolet",data = temp.all)
par(new=T)
plot(pfr15~year, type = "l", lty = 2, ylim = c(-10,0), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "blueviolet",data = temp.all)
par(new=T)
plot(ppac15~year, type = "l", lty = 1, ylim = c(-10,0), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "dodgerblue1",data = temp.all)
par(new=T)
plot(ppdc15~year, type = "l", lty = 2, ylim = c(-10,0), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "dodgerblue1",data = temp.all)
par(new=T)
plot(tun15~year, type = "l", lty = 3, ylim = c(-10,0), xlim = c(2000,2016), xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = "dodgerblue1",data = temp.all)
axis(side = 1, at = seq(2000,2016,2), labels = NA, lwd = 1, tck = -0.03)
axis(side = 2, at = seq(-10,0,2), labels = NA, lwd = 1, tck = -0.03)
axis(side = 2, at = seq(-10,0,2), labels = seq(-10,0,2), line = -0.5, lwd = 1, tick = FALSE)
axis(side = 2, at = seq(-10,0,2), labels = seq(-10,0,2), line = -0.5, lwd = 1, tick = FALSE)
legend("topleft", "(a) Air (+150 cm)", bty = "n", inset = c(-0.05,-0.2))

## Panel C - Churchill ground surface temperatures
plot(airp0~year, type = "l", lty = 1, ylim = c(-8,4), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "forestgreen",data = temp.all)
par(new=T)
plot(bsw0~year, type = "l", lty = 2, ylim = c(-8,4), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "forestgreen",data = temp.all)
par(new=T)
plot(tis0~year, type = "l", lty = 3, ylim = c(-8,4), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "forestgreen",data = temp.all)
par(new=T)
plot(wsu0~year, type = "l", lty = 4, ylim = c(-8,4), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "forestgreen",data = temp.all)
abline(lm(wsu0~year, data = temp.all), col = "forestgreen")
par(new=T)
plot(fen0~year, type = "l", lty = 2, ylim = c(-8,4), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "darkorange2",data = temp.all)
par(new=T)
plot(bfr0~year, type = "l", lty = 1, ylim = c(-8,4), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "blueviolet",data = temp.all)
par(new=T)
plot(pfr0~year, type = "l", lty = 2, ylim = c(-8,4), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "blueviolet",data = temp.all)
par(new=T)
plot(ppac0~year, type = "l", lty = 1, ylim = c(-8,4), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "dodgerblue1",data = temp.all)
par(new=T)
plot(ppdc0~year, type = "l", lty = 2, ylim = c(-8,4), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "dodgerblue1",data = temp.all)
par(new=T)
plot(tun0~year, type = "l", lty = 3, ylim = c(-8,4), xlim = c(2000,2016), xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = "dodgerblue1",data = temp.all)
mtext("Temperature °C", side = 2, line = 1.5)#, outer = TRUE)
axis(side = 1, at = seq(2000,2016,2), labels = NA, lwd = 1, tck = -0.03)
axis(side = 2, at = seq(-8,4,2), labels = NA, lwd = 1, tck = -0.03)
axis(side = 2, at = seq(-8,4,2), labels = seq(-8,4,2), line = -0.5, lwd = 1, tick = FALSE)
legend("topleft", "(b) Ground (0 cm)", bty = "n", inset = c(-0.05,-0.2))

## Panel E - Churchill subsurface temperatures
plot(airp8~year, type = "l", lty = 1, ylim = c(-8,4), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "forestgreen",data = temp.all)
par(new=T)
plot(bsw8~year, type = "l", lty = 2, ylim = c(-8,4), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "forestgreen",data = temp.all)
par(new=T)
plot(tis8~year, type = "l", lty = 3, ylim = c(-8,4), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "forestgreen",data = temp.all)
par(new=T)
plot(wsu8~year, type = "l", lty = 4, ylim = c(-8,4), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "forestgreen",data = temp.all)
par(new=T)
plot(fen8~year, type = "l", lty = 2, ylim = c(-8,4), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "darkorange2",data = temp.all)
par(new=T)
plot(bfr8~year, type = "l", lty = 1, ylim = c(-8,4), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "blueviolet",data = temp.all)
par(new=T)
plot(pfr8~year, type = "l", lty = 2, ylim = c(-8,4), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "blueviolet",data = temp.all)
par(new=T)
plot(ppac8~year, type = "l", lty = 1, ylim = c(-8,4), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "dodgerblue1",data = temp.all)
par(new=T)
plot(ppdc8~year, type = "l", lty = 2, ylim = c(-8,4), xlim = c(2000,2016), xlab = "", ylab = "", axes = FALSE, col = "dodgerblue1",data = temp.all)
par(new=T)
plot(tun8~year, type = "l", lty = 3, ylim = c(-8,4), xlim = c(2000,2016), xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = "dodgerblue1",data = temp.all)
mtext("Year", side = 1, line = 1.5)
axis(side = 1, at = seq(2000,2016,2), labels = NA, lwd = 1, tck = -0.03)
axis(side = 1, at = seq(2000,2016,2), labels = seq(2000,2015,5), line = -0.5, lwd = 1, tick = FALSE)
axis(side = 2, at = seq(-8,4,2), labels = NA, lwd = 1, tck = -0.03)
axis(side = 2, at = seq(-8,4,2), labels = seq(-8,4,2), line = -0.5, lwd = 1, tick = FALSE)
legend("topleft", "(c) Subsurface (-80 cm)", bty = "n", inset = c(-0.05,-0.2))







thaw.mm <- read.csv(file = "~/Desktop/Workspace/Earthwatch/thaw.mm.csv", header = TRUE)
thaw.mm$ci <- thaw.mm$se*qt(0.975, thaw.mm$n-1)
thaw.mm$c5 <- thaw.mm$mean - thaw.mm$ci
thaw.mm$c95 <- thaw.mm$mean + thaw.mm$ci

thaw.ch <- read.csv(file = "~/Desktop/Workspace/Earthwatch/thaw.ch.csv", header = TRUE)
thaw.ch$ci <- thaw.ch$se*qt(0.975, thaw.ch$n-1)
thaw.ch$c5 <- thaw.ch$mean - thaw.ch$ci
thaw.ch$c95 <- thaw.ch$mean + thaw.ch$ci

d2 <- subset(thaw.mm, site == "D2")
d6 <- subset(thaw.mm, site == "D6")
beaver <- subset(thaw.mm, site == "Beaver")
hare <- subset(thaw.mm, site == "Hare")
goose <- subset(thaw.mm, site == "Goose")
snow <- subset(thaw.mm, site == "Snow")
porsild <- subset(thaw.mm, site == "Porsild")
pipeline <- subset(thaw.mm, site == "Pipeline")
pp.control.p <- subset(thaw.mm, site == "PP.Control.P")
pp.pipeline.p <- subset(thaw.mm, site == "PP.Pipeline.P")
pp.track.t <- subset(thaw.mm, site == "PP.Track.T")
pp.control.t <- subset(thaw.mm, site == "PP.Control.T")

ppa <- subset(thaw.ch, site == "PPA")
ppd <- subset(thaw.ch, site == "PPD")
air <- subset(thaw.ch, site == "AIR")
blk <- subset(thaw.ch, site == "BLK")

temp.mm <- read.csv(file = "~/Desktop/Workspace/MacPass/master.csv", header = TRUE)
temp.mm$air.ci <- temp.mm$air.se*qt(0.975, 11)
temp.mm$ground.ci <- temp.mm$ground.se*qt(0.975, 11)
temp.mm$p.150.ci <- temp.mm$p.150.se*qt(0.975, 11)
temp.mm$air.c5 <- temp.mm$air - temp.mm$air.ci
temp.mm$ground.c5 <- temp.mm$ground - temp.mm$ground.ci
temp.mm$p.150.c5 <- temp.mm$p.150 - temp.mm$p.150.ci
temp.mm$air.c95 <- temp.mm$air + temp.mm$air.ci
temp.mm$ground.c95 <- temp.mm$ground + temp.mm$ground.ci
temp.mm$p.150.c95 <- temp.mm$p.150 + temp.mm$p.150.ci


d2.1 <- subset(temp.mm, site == "D2")
d6.1 <- subset(temp.mm, site == "D6")
beaver.1 <- subset(temp.mm, site == "BP")
hare.1 <- subset(temp.mm, site == "HF")
goose.1 <- subset(temp.mm, site == "GF")

## Regressions for gap filling the met station data

bp.air <- read.csv(file = "~/Desktop/Workspace/MacPass/BP.csv", header = TRUE)

## Using air temperature to predict ground surface temperatures
bp.air.lm <- lm(bp.air[bp.air$Month==1, 6] ~ bp.air[bp.air$Month==1, 5]) # Jan
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==2, 6] ~ bp.air[bp.air$Month==2, 5]) # Feb
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==3, 6] ~ bp.air[bp.air$Month==3, 5]) # Mar
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==4, 6] ~ bp.air[bp.air$Month==4, 5]) # Apr
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==5, 6] ~ bp.air[bp.air$Month==5, 5]) # May
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==6, 6] ~ bp.air[bp.air$Month==6, 5]) # Jun
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==7, 6] ~ bp.air[bp.air$Month==7, 5]) # Jul
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==8, 6] ~ bp.air[bp.air$Month==8, 5]) # Aug
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==9, 6] ~ bp.air[bp.air$Month==9, 5]) # Sep
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==10, 6] ~ bp.air[bp.air$Month==10, 5]) # Oct
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==11, 6] ~ bp.air[bp.air$Month==11, 5]) # Nov
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==12, 6] ~ bp.air[bp.air$Month==12, 5]) # Dec
summary(bp.air.lm)

## Using -150 temperature to predict -215 temperatures
bp.air.lm <- lm(bp.air[bp.air$Month==1, 10] ~ bp.air[bp.air$Month==1, 9]) # Jan
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==2, 10] ~ bp.air[bp.air$Month==2, 9]) # Feb
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==3, 10] ~ bp.air[bp.air$Month==3, 9]) # Mar
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==4, 10] ~ bp.air[bp.air$Month==4, 9]) # Apr
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==5, 10] ~ bp.air[bp.air$Month==5, 9]) # May
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==6, 10] ~ bp.air[bp.air$Month==6, 9]) # Jun
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==7, 10] ~ bp.air[bp.air$Month==7, 9]) # Jul
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==8, 10] ~ bp.air[bp.air$Month==8, 9]) # Aug
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==9, 10] ~ bp.air[bp.air$Month==9, 9]) # Sep
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==10, 10] ~ bp.air[bp.air$Month==10, 9]) # Oct
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==11, 10] ~ bp.air[bp.air$Month==11, 9]) # Nov
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==12, 10] ~ bp.air[bp.air$Month==12, 9]) # Dec
summary(bp.air.lm)

## Using -215 temperature to predict -385 temperatures
bp.air.lm <- lm(bp.air[bp.air$Month==1, 11] ~ bp.air[bp.air$Month==1, 10]) # Jan
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==2, 11] ~ bp.air[bp.air$Month==2, 10]) # Feb
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==3, 11] ~ bp.air[bp.air$Month==3, 10]) # Mar
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==4, 11] ~ bp.air[bp.air$Month==4, 10]) # Apr
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==5, 11] ~ bp.air[bp.air$Month==5, 10]) # May
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==6, 11] ~ bp.air[bp.air$Month==6, 10]) # Jun
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==7, 11] ~ bp.air[bp.air$Month==7, 10]) # Jul
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==8, 11] ~ bp.air[bp.air$Month==8, 10]) # Aug
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==9, 11] ~ bp.air[bp.air$Month==9, 10]) # Sep
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==10, 11] ~ bp.air[bp.air$Month==10, 10]) # Oct
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==11, 11] ~ bp.air[bp.air$Month==11, 10]) # Nov
summary(bp.air.lm)
bp.air.lm <- lm(bp.air[bp.air$Month==12, 11] ~ bp.air[bp.air$Month==12, 10]) # Dec
summary(bp.air.lm)

###############
## Dale Creek 2

d2.air <- read.csv(file = "~/Desktop/Workspace/MacPass/D2.csv", header = TRUE, skip = 2)
d2.air <- d2.air[-10095,]

# Predicting air temperatures as a function of BP air temperatures
d2.1 <- lm(d2.air[d2.air$Month==1, 5] ~ bp.air[bp.air$Month==1, 5]) # Jan
summary(d2.1)
d2.2 <- lm(d2.air[d2.air$Month==2, 5] ~ bp.air[bp.air$Month==2, 5]) # Feb
summary(d2.2)
d2.3 <- lm(d2.air[d2.air$Month==3, 5] ~ bp.air[bp.air$Month==3, 5]) # Feb
summary(d2.3)
d2.4 <- lm(d2.air[d2.air$Month==4, 5] ~ bp.air[bp.air$Month==4, 5]) # Feb
summary(d2.4)
d2.5 <- lm(d2.air[d2.air$Month==5, 5] ~ bp.air[bp.air$Month==5, 5]) # Feb
summary(d2.5)
d2.6 <- lm(d2.air[d2.air$Month==6, 5] ~ bp.air[bp.air$Month==6, 5]) # Feb
summary(d2.6)
d2.7 <- lm(d2.air[d2.air$Month==7, 5] ~ bp.air[bp.air$Month==7, 5]) # Feb
summary(d2.7)
d2.8 <- lm(d2.air[d2.air$Month==8, 5] ~ bp.air[bp.air$Month==8, 5]) # Feb
summary(d2.8)
d2.9 <- lm(d2.air[d2.air$Month==9, 5] ~ bp.air[bp.air$Month==9, 5]) # Feb
summary(d2.9)
d2.10 <- lm(d2.air[d2.air$Month==10, 5] ~ bp.air[bp.air$Month==10, 5]) # Feb
summary(d2.10)
d2.11 <- lm(d2.air[d2.air$Month==11, 5] ~ bp.air[bp.air$Month==11, 5]) # Feb
summary(d2.11)
d2.12 <- lm(d2.air[d2.air$Month==12, 5] ~ bp.air[bp.air$Month==12, 5]) # Feb
summary(d2.12)

d2.airT <- data.frame(rbind(coef(d2.1),coef(d2.2),coef(d2.3),coef(d2.4),coef(d2.5),coef(d2.6),
                              coef(d2.7),coef(d2.8),coef(d2.9),coef(d2.10),coef(d2.11),coef(d2.12)))
names(d2.airT) <- c("b0","bp.airT")
rownames(d2.airT) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(d2.airT, "~/Desktop/Workspace/MacPass/d2.airT.csv")

# Predicting ground temperatures as a function of -5, -2.5, 150 temperatures
d2.1 <- lm(d2.air[d2.air$Month==1, 6] ~ d2.air[d2.air$Month==1, 8]+d2.air[d2.air$Month==1, 7]+d2.air[d2.air$Month==1, 5]) # Jan
summary(d2.1)
d2.2 <- lm(d2.air[d2.air$Month==2, 6] ~ d2.air[d2.air$Month==2, 8]+d2.air[d2.air$Month==2, 7]+d2.air[d2.air$Month==2, 5]) # Feb
summary(d2.2)
d2.3 <- lm(d2.air[d2.air$Month==3, 6] ~ d2.air[d2.air$Month==3, 8]+d2.air[d2.air$Month==3, 7]+d2.air[d2.air$Month==3, 5]) # Mar
summary(d2.3)
d2.4 <- lm(d2.air[d2.air$Month==4, 6] ~ d2.air[d2.air$Month==4, 8]+d2.air[d2.air$Month==4, 7]+d2.air[d2.air$Month==4, 5]) # Apr
summary(d2.4)
d2.5 <- lm(d2.air[d2.air$Month==5, 6] ~ d2.air[d2.air$Month==5, 8]+d2.air[d2.air$Month==5, 7]+d2.air[d2.air$Month==5, 5]) # May
summary(d2.5)
d2.6 <- lm(d2.air[d2.air$Month==6, 6] ~ d2.air[d2.air$Month==6, 8]+d2.air[d2.air$Month==6, 7]+d2.air[d2.air$Month==6, 5]) # Jun
summary(d2.6)
d2.7 <- lm(d2.air[d2.air$Month==7, 6] ~ d2.air[d2.air$Month==7, 8]+d2.air[d2.air$Month==7, 7]+d2.air[d2.air$Month==7, 5]) # Jul
summary(d2.7)
d2.8 <- lm(d2.air[d2.air$Month==8, 6] ~ d2.air[d2.air$Month==8, 8]+d2.air[d2.air$Month==8, 7]+d2.air[d2.air$Month==8, 5]) # Aug
summary(d2.8)
d2.9 <- lm(d2.air[d2.air$Month==9, 6] ~ d2.air[d2.air$Month==9, 8]+d2.air[d2.air$Month==9, 7]+d2.air[d2.air$Month==9, 5]) # Sep
summary(d2.9)
d2.10 <- lm(d2.air[d2.air$Month==10, 6] ~ d2.air[d2.air$Month==10, 8]+d2.air[d2.air$Month==10, 7]+d2.air[d2.air$Month==10, 5]) # Oct
summary(d2.10)
d2.11 <- lm(d2.air[d2.air$Month==11, 6] ~ d2.air[d2.air$Month==11, 8]+d2.air[d2.air$Month==11, 7]+d2.air[d2.air$Month==11, 5]) # Nov
summary(d2.11)
d2.12 <- lm(d2.air[d2.air$Month==12, 6] ~ d2.air[d2.air$Month==12, 8]+d2.air[d2.air$Month==12, 7]+d2.air[d2.air$Month==12, 5]) # Dec
summary(d2.12)

d2.ground <- data.frame(rbind(coef(d2.1),coef(d2.2),coef(d2.3),coef(d2.4),coef(d2.5),coef(d2.6),
                              coef(d2.7),coef(d2.8),coef(d2.9),coef(d2.10),coef(d2.11),coef(d2.12)))
names(d2.ground) <- c("b0","neg5","neg2.5","airT")
rownames(d2.ground) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(d2.ground, "d2.ground.csv")

# Predicting -150 temperatures as a function of -5, -2.5, 0, and 150 temperatures
d2.1 <- lm(d2.air[d2.air$Month==1, 9] ~ d2.air[d2.air$Month==1, 8]+d2.air[d2.air$Month==1, 7]+d2.air[d2.air$Month==1, 6]+d2.air[d2.air$Month==1, 5]) # Jan
summary(d2.1)
d2.2 <- lm(d2.air[d2.air$Month==2, 9] ~ d2.air[d2.air$Month==2, 8]+d2.air[d2.air$Month==2, 7]+d2.air[d2.air$Month==2, 6]+d2.air[d2.air$Month==2, 5]) # Jan
summary(d2.2)
d2.3 <- lm(d2.air[d2.air$Month==3, 9] ~ d2.air[d2.air$Month==3, 8]+d2.air[d2.air$Month==3, 7]+d2.air[d2.air$Month==3, 6]+d2.air[d2.air$Month==3, 5]) # Jan
summary(d2.3)
d2.4 <- lm(d2.air[d2.air$Month==4, 9] ~ d2.air[d2.air$Month==4, 8]+d2.air[d2.air$Month==4, 7]+d2.air[d2.air$Month==4, 6]+d2.air[d2.air$Month==4, 5]) # Jan
summary(d2.4)
d2.5 <- lm(d2.air[d2.air$Month==5, 9] ~ d2.air[d2.air$Month==5, 8]+d2.air[d2.air$Month==5, 7]+d2.air[d2.air$Month==5, 6]+d2.air[d2.air$Month==5, 5]) # Jan
summary(d2.5)
d2.6 <- lm(d2.air[d2.air$Month==6, 9] ~ d2.air[d2.air$Month==6, 8]+d2.air[d2.air$Month==6, 7]+d2.air[d2.air$Month==6, 6]+d2.air[d2.air$Month==6, 5]) # Jan
summary(d2.6)
d2.7 <- lm(d2.air[d2.air$Month==7, 9] ~ d2.air[d2.air$Month==7, 8]+d2.air[d2.air$Month==7, 7]+d2.air[d2.air$Month==7, 6]+d2.air[d2.air$Month==7, 5]) # Jan
summary(d2.7)
d2.8 <- lm(d2.air[d2.air$Month==8, 9] ~ d2.air[d2.air$Month==8, 8]+d2.air[d2.air$Month==8, 7]+d2.air[d2.air$Month==8, 6]+d2.air[d2.air$Month==8, 5]) # Jan
summary(d2.8)
d2.9 <- lm(d2.air[d2.air$Month==9, 9] ~ d2.air[d2.air$Month==9, 8]+d2.air[d2.air$Month==9, 7]+d2.air[d2.air$Month==9, 6]+d2.air[d2.air$Month==9, 5]) # Jan
summary(d2.9)
d2.10 <- lm(d2.air[d2.air$Month==10, 9] ~ d2.air[d2.air$Month==10, 8]+d2.air[d2.air$Month==10, 7]+d2.air[d2.air$Month==10, 6]+d2.air[d2.air$Month==10, 5]) # Jan
summary(d2.10)
d2.11 <- lm(d2.air[d2.air$Month==11, 9] ~ d2.air[d2.air$Month==11, 8]+d2.air[d2.air$Month==11, 7]+d2.air[d2.air$Month==11, 6]+d2.air[d2.air$Month==11, 5]) # Jan
summary(d2.11)
d2.12 <- lm(d2.air[d2.air$Month==12, 9] ~ d2.air[d2.air$Month==12, 8]+d2.air[d2.air$Month==12, 7]+d2.air[d2.air$Month==12, 6]+d2.air[d2.air$Month==12, 5]) # Jan
summary(d2.12)

d2.neg150 <- data.frame(rbind(coef(d2.1),coef(d2.2),coef(d2.3),coef(d2.4),coef(d2.5),coef(d2.6),
                              coef(d2.7),coef(d2.8),coef(d2.9),coef(d2.10),coef(d2.11),coef(d2.12)))
names(d2.neg150) <- c("b0","neg5","neg2.5","surfT","airT")
rownames(d2.neg150) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

### Dale Creek 6
d6.air <- read.csv(file = "~/Desktop/Workspace/MacPass/D6.csv", header = TRUE)

## Using D2 air temperatures to predict D6 air temperatures
d2.air2 <- d2.air[-c(1:1826),]

d6.1 <- lm(d6.air[d6.air$Month==1, 5] ~ d2.air2[d2.air2$Month==1, 5]) # Jan
summary(d6.1)
d6.2 <- lm(d6.air[d6.air$Month==2, 5] ~ d2.air2[d2.air2$Month==2, 5]) # Jan
summary(d6.2)
d6.3 <- lm(d6.air[d6.air$Month==3, 5] ~ d2.air2[d2.air2$Month==3, 5]) # Jan
summary(d6.3)
d6.4 <- lm(d6.air[d6.air$Month==4, 5] ~ d2.air2[d2.air2$Month==4, 5]) # Jan
summary(d6.4)
d6.5 <- lm(d6.air[d6.air$Month==5, 5] ~ d2.air2[d2.air2$Month==5, 5]) # Jan
summary(d6.5)
d6.6 <- lm(d6.air[d6.air$Month==6, 5] ~ d2.air2[d2.air2$Month==6, 5]) # Jan
summary(d6.6)
d6.7 <- lm(d6.air[d6.air$Month==7, 5] ~ d2.air2[d2.air2$Month==7, 5]) # Jan
summary(d6.7)
d6.8 <- lm(d6.air[d6.air$Month==8, 5] ~ d2.air2[d2.air2$Month==8, 5]) # Jan
summary(d6.8)
d6.9 <- lm(d6.air[d6.air$Month==9, 5] ~ d2.air2[d2.air2$Month==9, 5]) # Jan
summary(d6.9)
d6.10 <- lm(d6.air[d6.air$Month==10, 5] ~ d2.air2[d2.air2$Month==10, 5]) # Jan
summary(d6.10)
d6.11 <- lm(d6.air[d6.air$Month==11, 5] ~ d2.air2[d2.air2$Month==11, 5]) # Jan
summary(d6.11)
d6.12 <- lm(d6.air[d6.air$Month==12, 5] ~ d2.air2[d2.air2$Month==12, 5]) # Jan
summary(d6.12)

d6.airT <- data.frame(rbind(coef(d6.1),coef(d6.2),coef(d6.3),coef(d6.4),coef(d6.5),coef(d6.6),
                              coef(d6.7),coef(d6.8),coef(d6.9),coef(d6.10),coef(d6.11),coef(d6.12)))
names(d6.airT) <- c("b0","d2.airT")
rownames(d6.airT) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(d6.airT, "~/Desktop/Workspace/MacPass/d6.airT.csv")

## Using surface temperature to predict -2.9 temperature
d6.air.lm <- lm(d6.air[d6.air$Month==1, 7] ~ d6.air[d6.air$Month==1, 6]) # Jan
summary(d6.air.lm)
d6.air.lm <- lm(d6.air[d6.air$Month==2, 7] ~ d6.air[d6.air$Month==2, 6]) # Feb
summary(d6.air.lm)
d6.air.lm <- lm(d6.air[d6.air$Month==3, 7] ~ d6.air[d6.air$Month==3, 6]) # Mar
summary(d6.air.lm)
d6.air.lm <- lm(d6.air[d6.air$Month==4, 7] ~ d6.air[d6.air$Month==4, 6]) # Apr
summary(d6.air.lm)
d6.air.lm <- lm(d6.air[d6.air$Month==5, 7] ~ d6.air[d6.air$Month==5, 6]) # May
summary(d6.air.lm)
d6.air.lm <- lm(d6.air[d6.air$Month==6, 7] ~ d6.air[d6.air$Month==6, 6]) # Jun
summary(d6.air.lm)
d6.air.lm <- lm(d6.air[d6.air$Month==7, 7] ~ d6.air[d6.air$Month==7, 6]) # Jul
summary(d6.air.lm)
d6.air.lm <- lm(d6.air[d6.air$Month==8, 7] ~ d6.air[d6.air$Month==8, 6]) # Aug
summary(d6.air.lm)
d6.air.lm <- lm(d6.air[d6.air$Month==9, 7] ~ d6.air[d6.air$Month==9, 6]) # Sep
summary(d6.air.lm)
d6.air.lm <- lm(d6.air[d6.air$Month==10, 7] ~ d6.air[d6.air$Month==10, 6]) # Oct
summary(d6.air.lm)
d6.air.lm <- lm(d6.air[d6.air$Month==11, 7] ~ d6.air[d6.air$Month==11, 6]) # Nov
summary(d6.air.lm)
d6.air.lm <- lm(d6.air[d6.air$Month==12, 7] ~ d6.air[d6.air$Month==12, 6]) # Dec
summary(d6.air.lm)

## Using -2.5 temperature to predict -5 temperature
d6.air.lm <- lm(d6.air[d6.air$Month==1, 8] ~ d6.air[d6.air$Month==1, 7]) # Jan
summary(d6.air.lm)
d6.air.lm <- lm(d6.air[d6.air$Month==2, 8] ~ d6.air[d6.air$Month==2, 7]) # Feb
summary(d6.air.lm)
d6.air.lm <- lm(d6.air[d6.air$Month==3, 8] ~ d6.air[d6.air$Month==3, 7]) # Mar
summary(d6.air.lm)
d6.air.lm <- lm(d6.air[d6.air$Month==4, 8] ~ d6.air[d6.air$Month==4, 7]) # Apr
summary(d6.air.lm)
d6.air.lm <- lm(d6.air[d6.air$Month==5, 8] ~ d6.air[d6.air$Month==5, 7]) # May
summary(d6.air.lm)
d6.air.lm <- lm(d6.air[d6.air$Month==6, 8] ~ d6.air[d6.air$Month==6, 7]) # Jun
summary(d6.air.lm)
d6.air.lm <- lm(d6.air[d6.air$Month==7, 8] ~ d6.air[d6.air$Month==7, 7]) # Jul
summary(d6.air.lm)
d6.air.lm <- lm(d6.air[d6.air$Month==8, 8] ~ d6.air[d6.air$Month==8, 7]) # Aug
summary(d6.air.lm)
d6.air.lm <- lm(d6.air[d6.air$Month==9, 8] ~ d6.air[d6.air$Month==9, 7]) # Sep
summary(d6.air.lm)
d6.air.lm <- lm(d6.air[d6.air$Month==10, 8] ~ d6.air[d6.air$Month==10, 7]) # Oct
summary(d6.air.lm)
d6.air.lm <- lm(d6.air[d6.air$Month==11, 8] ~ d6.air[d6.air$Month==11, 7]) # Nov
summary(d6.air.lm)
d6.air.lm <- lm(d6.air[d6.air$Month==12, 8] ~ d6.air[d6.air$Month==12, 7]) # Dec
summary(d6.air.lm)

## Using 0 temperature from D2 and D6 air temps to predict 0 temperature
d6.1 <- lm(d6.air[d6.air$Month==1, 6] ~ d2.air2[d2.air2$Month==1, 6] + d6.air[d6.air$Month==1, 5]) # Jan
summary(d6.1)
d6.2 <- lm(d6.air[d6.air$Month==2, 6] ~ d2.air2[d2.air2$Month==2, 6] + d6.air[d6.air$Month==2, 5]) # Feb
summary(d6.2)
d6.3 <- lm(d6.air[d6.air$Month==3, 6] ~ d2.air2[d2.air2$Month==3, 6] + d6.air[d6.air$Month==3, 5]) # Mar
summary(d6.3)
d6.4 <- lm(d6.air[d6.air$Month==4, 6] ~ d2.air2[d2.air2$Month==4, 6] + d6.air[d6.air$Month==4, 5]) # Apr
summary(d6.4)
d6.5 <- lm(d6.air[d6.air$Month==5, 6] ~ d2.air2[d2.air2$Month==5, 6] + d6.air[d6.air$Month==5, 5]) # May
summary(d6.5)
d6.6 <- lm(d6.air[d6.air$Month==6, 6] ~ d2.air2[d2.air2$Month==6, 6] + d6.air[d6.air$Month==6, 5]) # Jun
summary(d6.6)
d6.7 <- lm(d6.air[d6.air$Month==7, 6] ~ d2.air2[d2.air2$Month==7, 6] + d6.air[d6.air$Month==7, 5]) # Jul
summary(d6.7)
d6.8 <- lm(d6.air[d6.air$Month==8, 6] ~ d2.air2[d2.air2$Month==8, 6] + d6.air[d6.air$Month==8, 5]) # Aug
summary(d6.8)
d6.9 <- lm(d6.air[d6.air$Month==9, 6] ~ d2.air2[d2.air2$Month==9, 6] + d6.air[d6.air$Month==9, 5]) # Sep
summary(d6.9)
d6.10 <- lm(d6.air[d6.air$Month==10, 6] ~ d2.air2[d2.air2$Month==10, 6] + d6.air[d6.air$Month==10, 5]) # Oct
summary(d6.10)
d6.11 <- lm(d6.air[d6.air$Month==11, 6] ~ d2.air2[d2.air2$Month==11, 6] + d6.air[d6.air$Month==11, 5]) # Nov
summary(d6.11)
d6.12 <- lm(d6.air[d6.air$Month==12, 6] ~ d2.air2[d2.air2$Month==12, 6] + d6.air[d6.air$Month==12, 5]) # Dec
summary(d6.12)

d6.surfT <- data.frame(rbind(coef(d6.1),coef(d6.2),coef(d6.3),coef(d6.4),coef(d6.5),coef(d6.6),
                            coef(d6.7),coef(d6.8),coef(d6.9),coef(d6.10),coef(d6.11),coef(d6.12)))
names(d6.surfT) <- c("b0","d2.surfT","D6.airT")
rownames(d6.surfT) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(d6.surfT, "~/Desktop/Workspace/MacPass/d6.surfT.csv")

## Using -150 temperature from D2, D6 -5, -2.5, and surfT to predict -150 at D6
d6.1 <- lm(d6.air[d6.air$Month==1, 9] ~ d2.air2[d2.air2$Month==1, 9] + d6.air[d6.air$Month==1, 8] + d6.air[d6.air$Month==1, 7] + d6.air[d6.air$Month==1, 6]) # Jan
summary(d6.1)
d6.2 <- lm(d6.air[d6.air$Month==2, 9] ~ d2.air2[d2.air2$Month==2, 9] + d6.air[d6.air$Month==2, 8] + d6.air[d6.air$Month==2, 7] + d6.air[d6.air$Month==2, 6]) # Jan
summary(d6.2)
d6.3 <- lm(d6.air[d6.air$Month==3, 9] ~ d2.air2[d2.air2$Month==3, 9] + d6.air[d6.air$Month==3, 8] + d6.air[d6.air$Month==3, 7] + d6.air[d6.air$Month==3, 6]) # Jan
summary(d6.3)
d6.4 <- lm(d6.air[d6.air$Month==4, 9] ~ d2.air2[d2.air2$Month==4, 9] + d6.air[d6.air$Month==4, 8] + d6.air[d6.air$Month==4, 7] + d6.air[d6.air$Month==4, 6]) # Jan
summary(d6.4)
d6.5 <- lm(d6.air[d6.air$Month==5, 9] ~ d2.air2[d2.air2$Month==5, 9] + d6.air[d6.air$Month==5, 8] + d6.air[d6.air$Month==5, 7] + d6.air[d6.air$Month==5, 6]) # Jan
summary(d6.5)
d6.6 <- lm(d6.air[d6.air$Month==6, 9] ~ d2.air2[d2.air2$Month==6, 9] + d6.air[d6.air$Month==6, 8] + d6.air[d6.air$Month==6, 7] + d6.air[d6.air$Month==6, 6]) # Jan
summary(d6.6)
d6.7 <- lm(d6.air[d6.air$Month==7, 9] ~ d2.air2[d2.air2$Month==7, 9] + d6.air[d6.air$Month==7, 8] + d6.air[d6.air$Month==7, 7] + d6.air[d6.air$Month==7, 6]) # Jan
summary(d6.7)
d6.8 <- lm(d6.air[d6.air$Month==8, 9] ~ d2.air2[d2.air2$Month==8, 9] + d6.air[d6.air$Month==8, 8] + d6.air[d6.air$Month==8, 7] + d6.air[d6.air$Month==8, 6]) # Jan
summary(d6.8)
d6.9 <- lm(d6.air[d6.air$Month==9, 9] ~ d2.air2[d2.air2$Month==9, 9] + d6.air[d6.air$Month==9, 8] + d6.air[d6.air$Month==9, 7] + d6.air[d6.air$Month==9, 6]) # Jan
summary(d6.9)
d6.10 <- lm(d6.air[d6.air$Month==10, 9] ~ d2.air2[d2.air2$Month==10, 9] + d6.air[d6.air$Month==10, 8] + d6.air[d6.air$Month==10, 7] + d6.air[d6.air$Month==10, 6]) # Jan
summary(d6.10)
d6.11 <- lm(d6.air[d6.air$Month==11, 9] ~ d2.air2[d2.air2$Month==11, 9] + d6.air[d6.air$Month==11, 8] + d6.air[d6.air$Month==11, 7] + d6.air[d6.air$Month==11, 6]) # Jan
summary(d6.11)
d6.12 <- lm(d6.air[d6.air$Month==12, 9] ~ d2.air2[d2.air2$Month==12, 9] + d6.air[d6.air$Month==12, 8] + d6.air[d6.air$Month==12, 7] + d6.air[d6.air$Month==12, 6]) # Jan
summary(d6.12)

d6.neg150 <- data.frame(rbind(coef(d6.1),coef(d6.2),coef(d6.3),coef(d6.4),coef(d6.5),coef(d6.6),
                             coef(d6.7),coef(d6.8),coef(d6.9),coef(d6.10),coef(d6.11),coef(d6.12)))
names(d6.neg150) <- c("b0","d2.neg150","D6.neg5","D6.neg2p5","D6.surfT")
rownames(d6.neg150) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(d6.neg150, "~/Desktop/Workspace/MacPass/d6.neg150.csv")

## Using -150 temperature from D2 to predict -150 at D6
d6.1 <- lm(d6.air[d6.air$Month==1, 9] ~ d2.air2[d2.air2$Month==1, 9]) # Jan
summary(d6.1)
d6.2 <- lm(d6.air[d6.air$Month==2, 9] ~ d2.air2[d2.air2$Month==2, 9]) # Jan
summary(d6.2)
d6.3 <- lm(d6.air[d6.air$Month==3, 9] ~ d2.air2[d2.air2$Month==3, 9]) # Jan
summary(d6.3)
d6.4 <- lm(d6.air[d6.air$Month==4, 9] ~ d2.air2[d2.air2$Month==4, 9]) # Jan
summary(d6.4)
d6.5 <- lm(d6.air[d6.air$Month==5, 9] ~ d2.air2[d2.air2$Month==5, 9]) # Jan
summary(d6.5)
d6.6 <- lm(d6.air[d6.air$Month==6, 9] ~ d2.air2[d2.air2$Month==6, 9]) # Jan
summary(d6.6)
d6.7 <- lm(d6.air[d6.air$Month==7, 9] ~ d2.air2[d2.air2$Month==7, 9]) # Jan
summary(d6.7)
d6.8 <- lm(d6.air[d6.air$Month==8, 9] ~ d2.air2[d2.air2$Month==8, 9]) # Jan
summary(d6.8)
d6.9 <- lm(d6.air[d6.air$Month==9, 9] ~ d2.air2[d2.air2$Month==9, 9]) # Jan
summary(d6.9)
d6.10 <- lm(d6.air[d6.air$Month==10, 9] ~ d2.air2[d2.air2$Month==10, 9]) # Jan
summary(d6.10)
d6.11 <- lm(d6.air[d6.air$Month==11, 9] ~ d2.air2[d2.air2$Month==11, 9]) # Jan
summary(d6.11)
d6.12 <- lm(d6.air[d6.air$Month==12, 9] ~ d2.air2[d2.air2$Month==12, 9]) # Jan
summary(d6.12)

d6.neg150 <- data.frame(rbind(coef(d6.1),coef(d6.2),coef(d6.3),coef(d6.4),coef(d6.5),coef(d6.6),
                              coef(d6.7),coef(d6.8),coef(d6.9),coef(d6.10),coef(d6.11),coef(d6.12)))
names(d6.neg150) <- c("b0","d2.neg150")
rownames(d6.neg150) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(d6.neg150, "~/Desktop/Workspace/MacPass/d6.neg150.csv")

##### Hare Foot
hf.air <- read.csv(file = "~/Desktop/Workspace/MacPass/HF.csv", header = TRUE)
hf.air <- hf.air[-c(10095,10096),]

# Predicting -150 temperatures using BP -150 temperatures, HF -5, -2.5, 0, and air temperatures
hf.1 <- lm(hf.air[hf.air$Month==1, 9] ~ bp.air[bp.air$Month==1, 12] + hf.air[hf.air$Month==1, 8] + hf.air[hf.air$Month==1, 7] + hf.air[hf.air$Month==1, 6] + hf.air[hf.air$Month==1, 5]) # Jan
summary(hf.1)
hf.2 <- lm(hf.air[hf.air$Month==2, 9] ~ bp.air[bp.air$Month==2, 12] + hf.air[hf.air$Month==2, 8] + hf.air[hf.air$Month==2, 7] + hf.air[hf.air$Month==2, 6] + hf.air[hf.air$Month==2, 5]) # Jan
summary(hf.2)
hf.3 <- lm(hf.air[hf.air$Month==3, 9] ~ bp.air[bp.air$Month==3, 12] + hf.air[hf.air$Month==3, 8] + hf.air[hf.air$Month==3, 7] + hf.air[hf.air$Month==3, 6] + hf.air[hf.air$Month==3, 5]) # Jan
summary(hf.3)
hf.4 <- lm(hf.air[hf.air$Month==4, 9] ~ bp.air[bp.air$Month==4, 12] + hf.air[hf.air$Month==4, 8] + hf.air[hf.air$Month==4, 7] + hf.air[hf.air$Month==4, 6] + hf.air[hf.air$Month==4, 5]) # Jan
summary(hf.4)
hf.5 <- lm(hf.air[hf.air$Month==5, 9] ~ bp.air[bp.air$Month==5, 12] + hf.air[hf.air$Month==5, 8] + hf.air[hf.air$Month==5, 7] + hf.air[hf.air$Month==5, 6] + hf.air[hf.air$Month==5, 5]) # Jan
summary(hf.5)
hf.6 <- lm(hf.air[hf.air$Month==6, 9] ~ bp.air[bp.air$Month==6, 12] + hf.air[hf.air$Month==6, 8] + hf.air[hf.air$Month==6, 7] + hf.air[hf.air$Month==6, 6] + hf.air[hf.air$Month==6, 5]) # Jan
summary(hf.6)
hf.7 <- lm(hf.air[hf.air$Month==7, 9] ~ bp.air[bp.air$Month==7, 12] + hf.air[hf.air$Month==7, 8] + hf.air[hf.air$Month==7, 7] + hf.air[hf.air$Month==7, 6] + hf.air[hf.air$Month==7, 5]) # Jan
summary(hf.7)
hf.8 <- lm(hf.air[hf.air$Month==8, 9] ~ bp.air[bp.air$Month==8, 12] + hf.air[hf.air$Month==8, 8] + hf.air[hf.air$Month==8, 7] + hf.air[hf.air$Month==8, 6] + hf.air[hf.air$Month==8, 5]) # Jan
summary(hf.8)
hf.9 <- lm(hf.air[hf.air$Month==9, 9] ~ bp.air[bp.air$Month==9, 12] + hf.air[hf.air$Month==9, 8] + hf.air[hf.air$Month==9, 7] + hf.air[hf.air$Month==9, 6] + hf.air[hf.air$Month==9, 5]) # Jan
summary(hf.9)
hf.10 <- lm(hf.air[hf.air$Month==10, 9] ~ bp.air[bp.air$Month==10, 12] + hf.air[hf.air$Month==10, 8] + hf.air[hf.air$Month==10, 7] + hf.air[hf.air$Month==10, 6] + hf.air[hf.air$Month==10, 5]) # Jan
summary(hf.10)
hf.11 <- lm(hf.air[hf.air$Month==11, 9] ~ bp.air[bp.air$Month==11, 12] + hf.air[hf.air$Month==11, 8] + hf.air[hf.air$Month==11, 7] + hf.air[hf.air$Month==11, 6] + hf.air[hf.air$Month==11, 5]) # Jan
summary(hf.11)
hf.12 <- lm(hf.air[hf.air$Month==12, 9] ~ bp.air[bp.air$Month==12, 12] + hf.air[hf.air$Month==12, 8] + hf.air[hf.air$Month==12, 7] + hf.air[hf.air$Month==12, 6] + hf.air[hf.air$Month==12, 5]) # Jan
summary(hf.12)

hf.neg150 <- data.frame(rbind(coef(hf.1),coef(hf.2),coef(hf.3),coef(hf.4),coef(hf.5),coef(hf.6),
                            coef(hf.7),coef(hf.8),coef(hf.9),coef(hf.10),coef(hf.11),coef(hf.12)))
names(hf.neg150) <- c("b0","bp.neg150","hf.neg5","hf.neg2.5","hf.surfT","hf.air")
rownames(hf.neg150) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(hf.neg150, "~/Desktop/Workspace/MacPass/hf.neg150.csv")


### Goose Flats
gf.air <- read.csv(file = "~/Desktop/Workspace/MacPass/GF.csv", header = TRUE, skip = 2)
hf.air2 <- hf.air[-c(10093:10094),]

gf.1 <- lm(gf.air[gf.air$Month==1, 5] ~ hf.air2[hf.air2$Month==1, 5]) # Jan
summary(gf.1)
gf.2 <- lm(gf.air[gf.air$Month==2, 5] ~ hf.air2[hf.air2$Month==2, 5]) # Feb
summary(gf.2)
gf.3 <- lm(gf.air[gf.air$Month==3, 5] ~ hf.air2[hf.air2$Month==3, 5]) # Mar
summary(gf.3)
gf.4 <- lm(gf.air[gf.air$Month==4, 5] ~ hf.air2[hf.air2$Month==4, 5]) # Apr
summary(gf.4)
gf.5 <- lm(gf.air[gf.air$Month==5, 5] ~ hf.air2[hf.air2$Month==5, 5]) # May
summary(gf.5)
gf.6 <- lm(gf.air[gf.air$Month==6, 5] ~ hf.air2[hf.air2$Month==6, 5]) # Jun
summary(gf.6)
gf.7 <- lm(gf.air[gf.air$Month==7, 5] ~ hf.air2[hf.air2$Month==7, 5]) # Jul
summary(gf.7)
gf.8 <- lm(gf.air[gf.air$Month==8, 5] ~ hf.air2[hf.air2$Month==8, 5]) # Aug
summary(gf.8)
gf.9 <- lm(gf.air[gf.air$Month==9, 5] ~ hf.air2[hf.air2$Month==9, 5]) # Sep
summary(gf.9)
gf.10 <- lm(gf.air[gf.air$Month==10, 5] ~ hf.air2[hf.air2$Month==10, 5]) # Oct
summary(gf.10)
gf.11 <- lm(gf.air[gf.air$Month==11, 5] ~ hf.air2[hf.air2$Month==11, 5]) # Nov
summary(gf.11)
gf.12 <- lm(gf.air[gf.air$Month==12, 5] ~ hf.air2[hf.air2$Month==12, 5]) # Dec
summary(gf.12)

gf.airT <- data.frame(rbind(coef(gf.1),coef(gf.2),coef(gf.3),coef(gf.4),coef(gf.5),coef(gf.6),
                              coef(gf.7),coef(gf.8),coef(gf.9),coef(gf.10),coef(gf.11),coef(gf.12)))
names(gf.airT) <- c("b0","hf.airT")
rownames(gf.airT) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(gf.airT, "~/Desktop/Workspace/MacPass/gf.airT.csv")

# Using GF air temperatures to get surface temperature
gf.1 <- lm(gf.air[gf.air$Month==1, 6] ~ gf.air[gf.air$Month==1, 5]) # Jan
summary(gf.1)
gf.2 <- lm(gf.air[gf.air$Month==2, 6] ~ gf.air[gf.air$Month==2, 5]) # Jan
summary(gf.2)
gf.3 <- lm(gf.air[gf.air$Month==3, 6] ~ gf.air[gf.air$Month==3, 5]) # Jan
summary(gf.3)
gf.4 <- lm(gf.air[gf.air$Month==4, 6] ~ gf.air[gf.air$Month==4, 5]) # Jan
summary(gf.4)
gf.5 <- lm(gf.air[gf.air$Month==5, 6] ~ gf.air[gf.air$Month==5, 5]) # Jan
summary(gf.5)
gf.6 <- lm(gf.air[gf.air$Month==6, 6] ~ gf.air[gf.air$Month==6, 5]) # Jan
summary(gf.6)
gf.7 <- lm(gf.air[gf.air$Month==7, 6] ~ gf.air[gf.air$Month==7, 5]) # Jan
summary(gf.7)
gf.8 <- lm(gf.air[gf.air$Month==8, 6] ~ gf.air[gf.air$Month==8, 5]) # Jan
summary(gf.8)
gf.9 <- lm(gf.air[gf.air$Month==9, 6] ~ gf.air[gf.air$Month==9, 5]) # Jan
summary(gf.9)
gf.10 <- lm(gf.air[gf.air$Month==10, 6] ~ gf.air[gf.air$Month==10, 5]) # Jan
summary(gf.10)
gf.11 <- lm(gf.air[gf.air$Month==11, 6] ~ gf.air[gf.air$Month==11, 5]) # Jan
summary(gf.11)
gf.12 <- lm(gf.air[gf.air$Month==12, 6] ~ gf.air[gf.air$Month==12, 5]) # Jan
summary(gf.12)

gf.surfT <- data.frame(rbind(coef(gf.1),coef(gf.2),coef(gf.3),coef(gf.4),coef(gf.5),coef(gf.6),
                            coef(gf.7),coef(gf.8),coef(gf.9),coef(gf.10),coef(gf.11),coef(gf.12)))
names(gf.surfT) <- c("b0","gf.airT")
rownames(gf.surfT) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(gf.surfT, "~/Desktop/Workspace/MacPass/gf.surfT.csv")

# Using GF surface temperatures and HF -150 to predict -150 at GF
gf.1 <- lm(gf.air[gf.air$Month==1, 9] ~ gf.air[gf.air$Month==1, 6] + hf.air2[hf.air2$Month==1, 9]) # Jan
summary(gf.1)
gf.2 <- lm(gf.air[gf.air$Month==2, 9] ~ gf.air[gf.air$Month==2, 6] + hf.air2[hf.air2$Month==2, 9]) # Jan
summary(gf.2)
gf.3 <- lm(gf.air[gf.air$Month==3, 9] ~ gf.air[gf.air$Month==3, 6] + hf.air2[hf.air2$Month==3, 9]) # Jan
summary(gf.3)
gf.4 <- lm(gf.air[gf.air$Month==4, 9] ~ gf.air[gf.air$Month==4, 6] + hf.air2[hf.air2$Month==4, 9]) # Jan
summary(gf.4)
gf.5 <- lm(gf.air[gf.air$Month==5, 9] ~ gf.air[gf.air$Month==5, 6] + hf.air2[hf.air2$Month==5, 9]) # Jan
summary(gf.5)
gf.6 <- lm(gf.air[gf.air$Month==6, 9] ~ gf.air[gf.air$Month==6, 6] + hf.air2[hf.air2$Month==6, 9]) # Jan
summary(gf.6)
gf.7 <- lm(gf.air[gf.air$Month==7, 9] ~ gf.air[gf.air$Month==7, 6] + hf.air2[hf.air2$Month==7, 9]) # Jan
summary(gf.7)
gf.8 <- lm(gf.air[gf.air$Month==8, 9] ~ gf.air[gf.air$Month==8, 6] + hf.air2[hf.air2$Month==8, 9]) # Jan
summary(gf.8)
gf.9 <- lm(gf.air[gf.air$Month==9, 9] ~ gf.air[gf.air$Month==9, 6] + hf.air2[hf.air2$Month==9, 9]) # Jan
summary(gf.9)
gf.10 <- lm(gf.air[gf.air$Month==10, 9] ~ gf.air[gf.air$Month==10, 6] + hf.air2[hf.air2$Month==10, 9]) # Jan
summary(gf.10)
gf.11 <- lm(gf.air[gf.air$Month==11, 9] ~ gf.air[gf.air$Month==11, 6] + hf.air2[hf.air2$Month==11, 9]) # Jan
summary(gf.11)
gf.12 <- lm(gf.air[gf.air$Month==12, 9] ~ gf.air[gf.air$Month==12, 6] + hf.air2[hf.air2$Month==12, 9]) # Jan
summary(gf.12)

gf.neg150 <- data.frame(rbind(coef(gf.1),coef(gf.2),coef(gf.3),coef(gf.4),coef(gf.5),coef(gf.6),
                             coef(gf.7),coef(gf.8),coef(gf.9),coef(gf.10),coef(gf.11),coef(gf.12)))
names(gf.neg150) <- c("b0","gf.surfT","hf.neg150")
rownames(gf.neg150) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(gf.neg150, "~/Desktop/Workspace/MacPass/gf.neg150.csv")

# Plotting temperature records for NWT report
## Export at 5 x 7

par(mfrow = c(3, 1))
par(cex = 0.75)
par(mar = c(3, 3, 0, 0), oma = c(2, 2, 2, 2)+0.2)

plot(goose.1$year, goose.1$air, type='o', xlim = c(1990,2015), ylim = c(-10,-2), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 3, axes = FALSE)
par(new=T)
plot(d2.1$year, d2.1$air, type='o', xlim = c(1990,2015), ylim = c(-10,-2), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 4, axes = FALSE)
par(new=T)
plot(d6.1$year, d6.1$air, type='o', xlim = c(1990,2015), ylim = c(-10,-2), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "orange", axes = FALSE)
par(new=T)
plot(beaver.1$year, beaver.1$air, type='o', xlim = c(1990,2015), ylim = c(-10,-2), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "magenta3", axes = FALSE)
par(new=T)
plot(hare.1$year, hare.1$air, type='o', xlim = c(1990,2015), ylim = c(-10,-2), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 2)
legend("topleft", "(a) Air (+150 cm)", bty = "n", inset = c(-0.02,-0.05))

plot(goose.1$year, goose.1$ground, type='o', xlim = c(1990,2015), ylim = c(-7,3), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 3, axes = FALSE)
par(new=T)
plot(d2.1$year, d2.1$ground, type='o', xlim = c(1990,2015), ylim = c(-7,3), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 4, axes = FALSE)
par(new=T)
plot(d6.1$year, d6.1$ground, type='o', xlim = c(1990,2015), ylim = c(-7,3), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "orange", axes = FALSE)
par(new=T)
plot(beaver.1$year, beaver.1$ground, type='o', xlim = c(1990,2015), ylim = c(-7,3), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "magenta3", axes = FALSE)
par(new=T)
plot(hare.1$year, hare.1$ground, type='o', xlim = c(1990,2015), ylim = c(-7,3), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 2)
legend("topleft", "(b) Ground surface (0 cm)", bty = "n", inset = c(-0.02,-0.05))

plot(goose.1$year, goose.1$p.150, type='o', xlim = c(1990,2015), ylim = c(-4,1), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 3, axes = FALSE)
par(new=T)
plot(d2.1$year, d2.1$p.150, type='o', xlim = c(1990,2015), ylim = c(-4,1), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 4, axes = FALSE)
par(new=T)
plot(d6.1$year, d6.1$p.150, type='o', xlim = c(1990,2015), ylim = c(-4,1), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "orange", axes = FALSE)
par(new=T)
plot(beaver.1$year, beaver.1$p.150, type='o', xlim = c(1990,2015), ylim = c(-4,1), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "magenta3", axes = FALSE)
par(new=T)
plot(hare.1$year, hare.1$p.150, type='o', xlim = c(1990,2015), ylim = c(-4,1), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 2)
legend("topleft", "(c) Subsurface (-150 cm)", bty = "n", inset = c(-0.02,-0.05))

mtext("Temperature (°C)", side=2, outer=TRUE, cex = 0.75, adj=0.5)
mtext("Year", side=1, outer=TRUE, cex=0.75, adj=0.5)

# Regressions for temperatures plotted above

## Air
goose.air.lm <- lm(goose.1$air~goose.1$year)
d2.air.lm <- lm(d2.1$air~d2.1$year)
d6.air.lm <- lm(d6.1$air~d6.1$year)
beaver.air.lm <- lm(beaver.1$air~beaver.1$year)
hare.air.lm <- lm(hare.1$air~hare.1$year)
summary(goose.air.lm) # --
summary(d2.air.lm) # --
summary(d6.air.lm) # --
summary(beaver.air.lm) # --
summary(hare.air.lm) # --

## Ground
goose.ground.lm <- lm(goose.1$ground~goose.1$year)
d2.ground.lm <- lm(d2.1$ground~d2.1$year)
d6.ground.lm <- lm(d6.1$ground~d6.1$year)
beaver.ground.lm <- lm(beaver.1$ground~beaver.1$year)
hare.ground.lm <- lm(hare.1$ground~hare.1$year)
summary(goose.ground.lm) # Sig
summary(d2.ground.lm) # --
summary(d6.ground.lm) # --
summary(beaver.ground.lm) # --
summary(hare.ground.lm) # --

## p.150
goose.p.150.lm <- lm(goose.1$p.150~goose.1$year)
d2.p.150.lm <- lm(d2.1$p.150~d2.1$year)
d6.p.150.lm <- lm(d6.1$p.150~d6.1$year)
beaver.p.150.lm <- lm(beaver.1$p.150~beaver.1$year)
hare.p.150.lm <- lm(hare.1$p.150~hare.1$year)
summary(goose.p.150.lm) # Sig
summary(d2.p.150.lm) # --
summary(d6.p.150.lm) # --
summary(beaver.p.150.lm) # --
summary(hare.p.150.lm) # --

##

##******************************##
## Plotting Churchill thaw data ##
##******************************##

ppa.lm <- lm(ppa$mean~ppa$year)
ppd.lm <- lm(ppd$mean~ppd$year)
air.lm <- lm(air$mean~air$year)
blk.lm <- lm(blk$mean~blk$year)
summary(ppa.lm)
summary(ppd.lm)
summary(air.lm)

# Export at 7 x 5
par(mfrow = c(2, 2))
par(cex = 0.75)
par(mar = c(3, 3, 0, 0), oma = c(2, 2, 2, 2)+0.2)

plot(ppa$year, ppa$mean, type='n', xlim = c(2000,2018), ylim = rev(c(20,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(ppa$year, ppa$c5, col = 'darkslategray1', xlim = c(2000,2018), ylim = rev(c(20,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(ppa$year, ppa$c95, col = 'darkslategray1', xlim = c(2000,2018), ylim = rev(c(20,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(ppa$year, rev(ppa$year)), c(ppa$c95, rev(ppa$c5)), col = "darkslategray1", border = NA)
par(new=T)
plot(ppa$year, ppa$mean, xlim = c(2000,2018), ylim = rev(c(20,100)), yaxs = "i", xaxs = "i",  xlab = "", ylab = "", type = "p", lwd=2, col = "darkslategray4")
par(new=T)
plot(ppa$year, ppa$mean, xlim = c(2000,2018), ylim = rev(c(20,100)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "darkslategray4", xlab = "Year", ylab = "Thaw depth (cm)")
#legend("topleft", "(a) PPA", bty = "n", inset = c(-0.08,-0.15))
abline(coef(ppa.lm),lty=2)

plot(ppd$year, ppd$mean, type='n', xlim = c(2000,2018), ylim = rev(c(20,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(ppd$year, ppd$c5, col = 'darkslategray1', xlim = c(2000,2018), ylim = rev(c(20,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(ppd$year, ppd$c95, col = 'darkslategray1', xlim = c(2000,2018), ylim = rev(c(20,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(ppd$year, rev(ppd$year)), c(ppd$c95, rev(ppd$c5)), col = "darkslategray1", border = NA)
par(new=T)
plot(ppd$year, ppd$mean, xlim = c(2000,2018), ylim = rev(c(20,100)), yaxs = "i", xaxs = "i",  xlab = "", ylab = "", type = "p", lwd=2, col = "darkslategray4")
par(new=T)
plot(ppd$year, ppd$mean, xlim = c(2000,2018), ylim = rev(c(20,100)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "darkslategray4", xlab = "Year", ylab = "Thaw depth (cm)")
#abline(coef(ppd.lm),lty=2)

plot(air$year, air$mean, type='n', xlim = c(2000,2018), ylim = rev(c(20,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(air$year, air$c5, col = 'darkslategray1', xlim = c(2000,2018), ylim = rev(c(20,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(air$year, air$c95, col = 'darkslategray1', xlim = c(2000,2018), ylim = rev(c(20,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(air$year, rev(air$year)), c(air$c95, rev(air$c5)), col = "darkslategray1", border = NA)
par(new=T)
plot(air$year, air$mean, xlim = c(2000,2018), ylim = rev(c(20,100)), yaxs = "i", xaxs = "i",  xlab = "", ylab = "", type = "p", lwd=2, col = "darkslategray4")
par(new=T)
plot(air$year, air$mean, xlim = c(2000,2018), ylim = rev(c(20,100)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "darkslategray4", xlab = "Year", ylab = "Thaw depth (cm)")
#abline(coef(air.lm),lty=2)

plot(blk$year, blk$mean, type='n', xlim = c(2000,2018), ylim = rev(c(20,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(blk$year, blk$c5, col = 'darkslategray1', xlim = c(2000,2018), ylim = rev(c(20,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(blk$year, blk$c95, col = 'darkslategray1', xlim = c(2000,2018), ylim = rev(c(20,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(blk$year, rev(blk$year)), c(blk$c95, rev(blk$c5)), col = "darkslategray1", border = NA)
par(new=T)
plot(blk$year, blk$mean, xlim = c(2000,2018), ylim = rev(c(20,100)), yaxs = "i", xaxs = "i",  xlab = "", ylab = "", type = "p", lwd=2, col = "darkslategray4")
par(new=T)
plot(blk$year, blk$mean, xlim = c(2000,2018), ylim = rev(c(20,100)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "darkslategray4", xlab = "Year", ylab = "Thaw depth (cm)")
#abline(coef(blk.lm),lty=2)

##******************************##
## Plotting Mac Pass thaw data ##
##******************************##


# Export at 7 x 7
par(mfrow = c(4, 2))
par(cex = 0.75)
par(mar = c(3, 3, 0, 0), oma = c(2, 2, 2, 2)+0.2)

plot(snow$year, snow$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(snow$year, snow$c5, col = 'darkslategray1', xlim = c(1990,2020), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(snow$year, snow$c95, col = 'darkslategray1', xlim = c(1990,2020), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(snow$year, rev(snow$year)), c(snow$c95, rev(snow$c5)), col = "darkslategray1", border = NA)
par(new=T)
plot(snow$year, snow$mean, xlim = c(1990,2020), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "darkslategray4", xlab = "Year", ylab = "Thaw depth (cm)")
legend("topleft", "(a) SF (1660 m.a.s.l.)", bty = "n", inset = c(-0.08,-0.15))
abline(coef(snow.lm),lty=2)

plot(pipeline$year, pipeline$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(pipeline$year, pipeline$c5, col = 'firebrick1', xlim = c(1990,2020), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(pipeline$year, pipeline$c95, col = 'firebrick1', xlim = c(1990,2020), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(pipeline$year, rev(pipeline$year)), c(pipeline$c95, rev(pipeline$c5)), col = "firebrick1", border = NA)
par(new=T)
plot(pipeline$year, pipeline$mean, xlim = c(1990,2020), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "firebrick4", xlab = "Year", ylab = "Thaw depth (cm)")
legend("topleft", "(b) PP (1623 m.a.s.l.)", bty = "n", inset = c(-0.08,-0.15))

plot(goose$year, goose$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(goose$year, goose$c5, col = 'olivedrab1', xlim = c(1990,2020), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(goose$year, goose$c95, col = 'olivedrab1', xlim = c(1990,2020), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(goose$year, rev(goose$year)), c(goose$c95, rev(goose$c5)), col = "olivedrab1", border = NA)
par(new=T)
plot(goose$year, goose$mean, xlim = c(1990,2020), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", type = "l", lwd=2,  col = "darkgreen", xlab = "Year", ylab = "Thaw depth (cm)")
legend("topleft", "(c) GF (1621 m.a.s.l.)", bty = "n", inset = c(-0.08,-0.15))
abline(coef(goose.lm),lty=2)

plot(d2$year, d2$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(d2$year, d2$c5, col = 'lightskyblue', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(d2$year, d2$c95, col = 'lightskyblue', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(d2$year, rev(d2$year)), c(d2$c95, rev(d2$c5)), col = "lightskyblue", border = NA)
par(new=T)
plot(d2$year, d2$mean, xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "blue", xlab = "Year", ylab = "Thaw depth (cm)")
legend("topleft", "(d) D2 (1477 m.a.s.l.)", bty = "n", inset = c(-0.08,-0.15))
abline(coef(d2.lm),lty=2)

plot(d6$year, d6$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(d6$year, d6$c5, col = 'orange1', xlim = c(1990,2020), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(d6$year, d6$c95, col = 'orange1', xlim = c(1990,2020), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(d6$year, rev(d6$year)), c(d6$c95, rev(d6$c5)), col = "orange1", border = NA)
par(new=T)
plot(d6$year, d6$mean, xlim = c(1990,2020), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "orangered2", xlab = "Year", ylab = "Thaw depth (cm)")
legend("topleft", "(e) D6 (1473 m.a.s.l.)", bty = "n", inset = c(-0.08,-0.15))

plot(porsild$year, porsild$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(porsild$year, porsild$c5, col = 'darkorchid1', xlim = c(1990,2020), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(porsild$year, porsild$c95, col = 'darkorchid1', xlim = c(1990,2020), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(porsild$year, rev(porsild$year)), c(porsild$c95, rev(porsild$c5)), col = "darkorchid1", border = NA)
par(new=T)
plot(porsild$year, porsild$mean, xlim = c(1990,2020), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", lwd=2, type = "l", col = "darkorchid4", xlab = "Year", ylab = "Thaw depth (cm)")
legend("topleft", "(f) PF (1380 m.a.s.l.)", bty = "n", inset = c(-0.08,-0.15))
abline(coef(porsild.lm),lty=2)

plot(beaver$year, beaver$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(beaver$year, beaver$c5, col = 'gold', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(beaver$year, beaver$c95, col = 'gold', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(beaver$year, rev(beaver$year)), c(beaver$c95, rev(beaver$c5)), col = "gold", border = NA)
par(new=T)
plot(beaver$year, beaver$mean, xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", lwd=2, type = "l", col = "goldenrod3",xlab = "Year", ylab = "Thaw depth (cm)")
legend("topleft", "(g) BP (1272 m.a.s.l.)", bty = "n", inset = c(-0.08,-0.15))
abline(coef(beaver.lm),lty=2)

plot(hare$year, hare$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(hare$year, hare$c5, col = 'pink', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(hare$year, hare$c95, col = 'pink', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(hare$year, rev(hare$year)), c(hare$c95, rev(hare$c5)), col = "pink", border = NA)
par(new=T)
plot(hare$year, hare$mean, xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", lwd=2, type = "l", col = "red", xlab = "Year", ylab = "Thaw depth (cm)")
legend("topleft", "(h) HF (1260 m.a.s.l.)", bty = "n", inset = c(-0.08,-0.15))

mtext("Thaw depth (cm)", side=2, outer=TRUE, cex = 0.75, adj=0.5)
mtext("Year", side=1, outer=TRUE, cex=0.75, adj=0.5)

##

par(mfrow = c(3, 1))
par(cex = 0.75)
par(mar = c(3, 3, 0, 0), oma = c(2, 2, 2, 2)+0.2)

plot(pp.control.p$year, pp.control.p$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(pp.control.p$year, pp.control.p$c5, col = 'firebrick1', xlim = c(1990,2020), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(pp.control.p$year, pp.control.p$c95, col = 'firebrick1', xlim = c(1990,2020), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(pp.control.p$year, rev(pp.control.p$year)), c(pp.control.p$c95, rev(pp.control.p$c5)), col = "firebrick1", border = NA)
par(new=T)
plot(pp.control.p$year, pp.control.p$mean, xlim = c(1990,2020), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "firebrick4", xlab = "Year", ylab = "Thaw depth (cm)")
abline(h = mean(pp.control.p$mean), lty = 2, lwd = 2)
legend("topleft", "PP: pipeline-control (1623 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))

plot(pp.pipeline.p$year, pp.pipeline.p$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(pp.pipeline.p$year, pp.pipeline.p$c5, col = 'firebrick1', xlim = c(1990,2020), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(pp.pipeline.p$year, pp.pipeline.p$c95, col = 'firebrick1', xlim = c(1990,2020), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(pp.pipeline.p$year, rev(pp.pipeline.p$year)), c(pp.pipeline.p$c95, rev(pp.pipeline.p$c5)), col = "firebrick1", border = NA)
par(new=T)
plot(pp.pipeline.p$year, pp.pipeline.p$mean, xlim = c(1990,2020), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "firebrick4", xlab = "Year", ylab = "Thaw depth (cm)")
abline(h = mean(pp.pipeline.p$mean), lty = 2, lwd = 2)
legend("topleft", "PP: pipeline (1623 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))

plot(pp.track.t$year, pp.track.t$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(pp.track.t$year, pp.track.t$c5, col = 'firebrick1', xlim = c(1990,2020), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(pp.track.t$year, pp.track.t$c95, col = 'firebrick1', xlim = c(1990,2020), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(pp.track.t$year, rev(pp.track.t$year)), c(pp.track.t$c95, rev(pp.track.t$c5)), col = "firebrick1", border = NA)
par(new=T)
plot(pp.track.t$year, pp.track.t$mean, xlim = c(1990,2020), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "firebrick4", xlab = "Year", ylab = "Thaw depth (cm)")
abline(h = mean(pp.track.t$mean), lty = 2, lwd = 2)
legend("topleft", "PP: track (1623 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))

mtext("Thaw depth (cm)", side=2, outer=TRUE, cex = 0.75, adj=0.5)
mtext("Year", side=1, outer=TRUE, cex=0.75, adj=0.5)

snow.lm <- lm(snow$mean~snow$year)
pipeline.lm <- lm(pipeline$mean~pipeline$year)
goose.lm <- lm(goose$mean~goose$year)
d2.lm <- lm(d2$mean~d2$year)
d6.lm <- lm(d6$mean~d6$year)
porsild.lm <- lm(porsild$mean~porsild$year)
beaver.lm <- lm(beaver$mean~beaver$year)
hare.lm <- lm(hare$mean~hare$year)
pp.track.t.lm <- lm(pp.track.t$mean ~ pp.track.t$year)
pp.pipeline.p.lm <- lm(pp.pipeline.p$mean ~ pp.pipeline.p$year)
pp.control.p.lm <- lm(pp.control.p$mean ~ pp.control.p$year)

summary(snow.lm) # Sig
# summary(pipeline.lm) # --
summary(goose.lm) # Sig
summary(d2.lm) # Sig
summary(d6.lm) # --
summary(porsild.lm) # Sig
summary(beaver.lm) # Sig
summary(hare.lm) # --
summary(pp.track.t.lm) # --
summary(pp.pipeline.p.lm) # Sig
summary(pp.control.p.lm) # --

goose.1.lm <- lm(goose.1$mean~goose.1$year)
d2.1.lm <- lm(d2.1$mean~d2.1$year)
d6.1.lm <- lm(d6.1$mean~d6.1$year)
beaver.lm <- lm(beaver$mean~beaver$year)
hare.1.lm <- lm(hare.1$mean~hare.1$year)
summary(goose.1.lm)
summary(d2.1.lm)
summary(d6.1.lm)
summary(beaver.lm)
summary(hare.1.lm)


## Plotting just the 5 sites with met and thaw data
# Export at 3.5 x 5
par(mfrow = c(5, 1))
par(cex = 0.5)
par(mar = c(3, 3, 0, 0), oma = c(2, 2, 2, 2)+0.2)

plot(hare$year, hare$mean, type='n', xlim = c(1990,2014), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(hare$year, hare$c5, col = 'lightgrey', xlim = c(1990,2014), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(hare$year, hare$c95, col = 'lightgrey', xlim = c(1990,2014), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(hare$year, rev(hare$year)), c(hare$c95, rev(hare$c5)), col = "lightgrey", border = NA)
par(new=T)
plot(hare$year, hare$mean, xlim = c(1990,2014), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", lwd=2, type = "l", col = "black", xlab = "Year", ylab = "Thaw depth (cm)")
legend("topleft", "(a) HF (1260 m.a.s.l.)", bty = "n", inset = c(0,-0.05))

plot(beaver$year, beaver$mean, type='n', xlim = c(1990,2014), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(beaver$year, beaver$c5, col = 'lightgrey', xlim = c(1990,2014), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(beaver$year, beaver$c95, col = 'lightgrey', xlim = c(1990,2014), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(beaver$year, rev(beaver$year)), c(beaver$c95, rev(beaver$c5)), col = "lightgrey", border = NA)
par(new=T)
plot(beaver$year, beaver$mean, xlim = c(1990,2014), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", lwd=2, type = "l", col = "black")
legend("topleft", "(b) BP (1272 m.a.s.l.)", bty = "n", inset = c(0,-0.05))

plot(d2$year, d2$mean, type='n', xlim = c(1990,2014), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(d2$year, d2$c5, col = 'lightgrey', xlim = c(1990,2014), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(d2$year, d2$c95, col = 'lightgrey', xlim = c(1990,2014), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(d2$year, rev(d2$year)), c(d2$c95, rev(d2$c5)), col = "lightgrey", border = NA)
par(new=T)
plot(d2$year, d2$mean, xlim = c(1990,2014), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "black", xlab = "Year", ylab = "Thaw depth (cm)")
legend("topleft", "(c) D2 (1477 m.a.s.l.)", bty = "n", inset = c(0,-0.05))
abline(coef(d2.lm),lty=2)


plot(d6$year, d6$mean, type='n', xlim = c(1990,2014), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(d6$year, d6$c5, col = 'lightgrey', xlim = c(1990,2014), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(d6$year, d6$c95, col = 'lightgrey', xlim = c(1990,2014), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(d6$year, rev(d6$year)), c(d6$c95, rev(d6$c5)), col = "lightgrey", border = NA)
par(new=T)
plot(d6$year, d6$mean, xlim = c(1990,2014), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "black", xlab = "Year", ylab = "Thaw depth (cm)")
legend("topleft", "(d) D6 (1473 m.a.s.l.)", bty = "n", inset = c(0,-0.05))

plot(goose$year, goose$mean, type='n', xlim = c(1990,2014), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(goose$year, goose$c5, col = 'lightgrey', xlim = c(1990,2014), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(goose$year, goose$c95, col = 'lightgrey', xlim = c(1990,2014), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(goose$year, rev(goose$year)), c(goose$c95, rev(goose$c5)), col = "lightgrey", border = NA)
par(new=T)
plot(goose$year, goose$mean, xlim = c(1990,2014), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", type = "l", lwd=2,  col = "black", xlab = "Year", ylab = "Thaw depth (cm)")
legend("topleft", "(e) GF (1621 m.a.s.l.)", bty = "n", inset = c(0,-0.05))
abline(coef(goose.lm),lty=2)

mtext("Thaw depth (cm)", side=2, outer=TRUE, cex = 0.5, adj=0.5)
mtext("Year", side=1, outer=TRUE, cex=0.5, adj=0.5)

# Air temperatures

## Plotting just the 5 sites with met and thaw data
# Export at 3.5 x 5
par(mfrow = c(5, 1))
par(cex = 0.5)
par(mar = c(3, 3, 0, 0), oma = c(2, 2, 2, 2)+0.2)

plot(hare.1$year, hare.1$air, type='o', xlim = c(1990,2014), ylim = c(-10,-4), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 2)
par(new=T)
plot(goose.1$year, goose.1$air, type='o', xlim = c(1990,2014), ylim = c(-10,-4), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 3)
par(new=T)
plot(d2.1$year, d2.1$air, type='o', xlim = c(1990,2014), ylim = c(-10,-4), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 3)
legend("topleft", "(a) HF (1260 m.a.s.l.)", bty = "n", inset = c(0,-0.05))

# Plotting temperature data for EW summary 2015
###

# Export at 6 x 8

par(mfrow = c(3, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(3, 3, 0, 0), oma = c(2, 2, 2, 2)+0.2)

plot(goose.1$year, goose.1$air, type='o', xlim = c(1990,2015), ylim = c(-10,-4), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 3)
par(new=T)
plot(d2.1$year, d2.1$air, type='o', xlim = c(1990,2015), ylim = c(-10,-4), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 4)
par(new=T)
plot(d6.1$year, d6.1$air, type='o', xlim = c(1990,2015), ylim = c(-10,-4), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "orange")
par(new=T)
plot(beaver.1$year, beaver.1$air, type='o', xlim = c(1990,2015), ylim = c(-10,-4), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "goldenrod3")
par(new=T)
plot(hare.1$year, hare.1$air, type='o', xlim = c(1990,2015), ylim = c(-10,-4), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 2)

plot(goose.1$year, goose.1$ground, type='o', xlim = c(1990,2015), ylim = c(-8,2), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 3)
par(new=T)
plot(d2.1$year, d2.1$ground, type='o', xlim = c(1990,2015), ylim = c(-8,2), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 4)
par(new=T)
plot(d6.1$year, d6.1$ground, type='o', xlim = c(1990,2015), ylim = c(-8,2), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "orange")
par(new=T)
plot(beaver.1$year, beaver.1$ground, type='o', xlim = c(1990,2015), ylim = c(-8,2), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "goldenrod3")
par(new=T)
plot(hare.1$year, hare.1$ground, type='o', xlim = c(1990,2015), ylim = c(-8,2), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 2)

plot(goose.1$year, goose.1$p.150, type='o', xlim = c(1990,2015), ylim = c(-4,0), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 3)
par(new=T)
plot(d2.1$year, d2.1$p.150, type='o', xlim = c(1990,2015), ylim = c(-4,0), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 4)
par(new=T)
plot(d6.1$year, d6.1$p.150, type='o', xlim = c(1990,2015), ylim = c(-4,0), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "orange")
par(new=T)
plot(beaver.1$year, beaver.1$p.150, type='o', xlim = c(1990,2015), ylim = c(-4,0), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "magenta")
par(new=T)
plot(hare.1$year, hare.1$p.150, type='o', xlim = c(1990,2015), ylim = c(-4,0), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 2)


mtext("Temperature (°C)", side=2, outer=TRUE, adj=0.5)
mtext("Year", side=1, outer=TRUE, adj=0.5)




par(new=T)
plot(goose.1$year, goose.1$air, type='o', xlim = c(1990,2015), ylim = c(-10,-4), yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 3)
legend("topleft", "(a) HF (1260 m.a.s.l.)", bty = "n", inset = c(0,-0.05))


plot(beaver.1$year, beaver.1$air, type='n', xlim = c(1990,2014), ylim = c(-5,5), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(beaver.1$year, beaver.1$c5, col = 'lightgrey', xlim = c(1990,2014), ylim = c(-5,5), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(beaver.1$year, beaver.1$c95, col = 'lightgrey', xlim = c(1990,2014), ylim = c(-5,5), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(beaver.1$year, rev(beaver.1$year)), c(beaver.1$c95, rev(beaver.1$c5)), col = "lightgrey", border = NA)
par(new=T)
plot(beaver.1$year, beaver.1$air, xlim = c(1990,2014), ylim = c(-5,5), yaxs = "i", xaxs = "i", lwd=2, type = "l")
legend("topleft", "(b) BP (1272 m.a.s.l.)", bty = "n", inset = c(0,-0.05))

plot(d2.1$year, d2.1$air, type='n', xlim = c(1990,2014), ylim = c(-5,5), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(d2.1$year, d2.1$c5, col = 'lightgrey', xlim = c(1990,2014), ylim = c(-5,5), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(d2.1$year, d2.1$c95, col = 'lightgrey', xlim = c(1990,2014), ylim = c(-5,5), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(d2.1$year, rev(d2.1$year)), c(d2.1$c95, rev(d2.1$c5)), col = "lightgrey", border = NA)
par(new=T)
plot(d2.1$year, d2.1$air, xlim = c(1990,2014), ylim = c(-5,5), yaxs = "i", xaxs = "i", lwd=2, type = "l", col = "black")
legend("topleft", "(c) D2 (1475 m.a.s.l.)", bty = "n", inset = c(0,-0.05))


plot(d6.1$year, d6.1$air, type='n', xlim = c(1990,2014), ylim = c(-5,5), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(d6.1$year, d6.1$c5, col = 'lightgrey', xlim = c(1990,2014), ylim = c(-5,5), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(d6.1$year, d6.1$c95, col = 'lightgrey', xlim = c(1990,2014), ylim = c(-5,5), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(d6.1$year, rev(d6.1$year)), c(d6.1$c95, rev(d6.1$c5)), col = "lightgrey", border = NA)
par(new=T)
plot(d6.1$year, d6.1$air, xlim = c(1990,2014), ylim = c(-5,5), yaxs = "i", xaxs = "i", lwd=2, type = "l", col = "black")
legend("topleft", "(d) D6 (1475 m.a.s.l.)", bty = "n", inset = c(0,-0.05))

plot(goose.1$year, goose.1$air, type='n', xlim = c(1990,2020), ylim = c(-5,5), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(goose.1$year, goose.1$c5, col = 'lightgrey', xlim = c(1990,2020), ylim = c(-5,5), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(goose.1$year, goose.1$c95, col = 'lightgrey', xlim = c(1990,2020), ylim = c(-5,5), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(goose.1$year, rev(goose.1$year)), c(goose.1$c95, rev(goose.1$c5)), col = "lightgrey", border = NA)
par(new=T)
plot(goose.1$year, goose.1$air, xlim = c(1990,2020), ylim = c(-5,5), yaxs = "i", xaxs = "i", xlab = "Year", ylab = "Air temperature (°C)", lwd=2, type = "l", col = "black")
legend("topleft", "GF (1621 m.a.s.l.)", bty = "n")#, inset = c(0,-0.05))

mtext("Standardized air temperature (°C)", side=2, outer=TRUE, cex = 0.5, adj=0.5)
mtext("Year", side=1, outer=TRUE, cex=0.5, adj=0.5)


needles.all <- read.csv(file = "~/Desktop/Workspace/EW/needles.csv", header = TRUE)
n.2008 <- subset(needles.all, year == 2008)
n.2009 <- subset(needles.all, year == 2009)
n.2010 <- subset(needles.all, year == 2010)
n.2013 <- subset(needles.all, year == 2013)
n.2014 <- subset(needles.all, year == 2014)
n.2015 <- subset(needles.all, year == 2015)


############################################################
############# Hierarchical Clustering Analysis #############
############################################################

dist.needles <- dist(needles.all[11:13],method="euclidean")/12.804207
hc.needles <- hclust(dist.needles,method="ward.D2")
dist.2008 <- dist(n.2008[11:13],method = "euclidean")/2.26804
hc.2008 <- hclust(dist.2008,method="ward.D2")
dist.2009 <- dist(n.2009[11:13],method = "euclidean")/3.233536
hc.2009 <- hclust(dist.2009,method="ward.D2")
dist.2010 <- dist(n.2010[11:13],method = "euclidean")/3.139475
hc.2010 <- hclust(dist.2010,method="ward.D2")
dist.2013 <- dist(n.2013[11:13],method = "euclidean")/10.17424
hc.2013 <- hclust(dist.2013,method="ward.D2")
dist.2014 <- dist(n.2014[11:13],method = "euclidean")/8.751599
hc.2014 <- hclust(dist.2014,method="ward.D2")
######################################################################
# Optimal number of clusters according to Mantel statistic (Pearson) #
######################################################################

# Function to compute a binary distance matrix from groups
grpdist <- function(X)
{
  require(cluster)
  gr <- as.data.frame(as.factor(X))
  distgr <- daisy(gr, "gower")
  distgr
}

# Run based on the Ward clustering

kt.needles <- data.frame(k=1:nrow(needles.all[11:13]), r=0)
pb <- txtProgressBar(min = 0, max = (nrow(needles.all[11:13])-1), style = 3)
for (i in 2:(nrow(needles.all[11:13])-1)) {
  Sys.sleep(0.1)
    setTxtProgressBar(pb, i)
  gr.needles <- cutree(hc.needles, i)
  distgr.needles <- grpdist(gr.needles)
  mt.needles <- cor(dist.needles, distgr.needles, method="pearson")
  kt.needles[i,2] <- mt.needles
}
kt.needles
kneedles.best <- which.max(kt.needles$r)
plot(kt.needles$k, kt.needles$r, type="h", xlim = c(0,20), main="Mantel-optimal number of clusters - Ward", 
     xlab="k (number of groups)", ylab="Pearson's correlation")
axis(1, kneedles.best, paste("optimum", kneedles.best, sep="\n"), col="red", font=2,
     col.axis="red")
points(kneedles.best, max(kt.needles$r), pch=16, col="red", cex=1.5)
cat("", "Mantel-optimal number of clusters k =", kneedles.best, "\n", 
    "with a matrix linear correlation of", max(kt.needles$r), "\n")


kt.2008 <- data.frame(k=1:nrow(n.2008[11:13]), r=0)
pb <- txtProgressBar(min = 0, max = (nrow(n.2008[11:13])-1), style = 3)
for (i in 2:(nrow(n.2008[11:13])-1)) {
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
  gr.2008 <- cutree(hc.2008, i)
  distgr.2008 <- grpdist(gr.2008)
  mt.2008 <- cor(dist.2008, distgr.2008, method="pearson")
  kt.2008[i,2] <- mt.2008
}
kt.2008
k2008.best <- which.max(kt.2008$r)
plot(kt.2008$k, kt.2008$r, type="h", xlim = c(0,20), main="Mantel-optimal number of clusters - Ward", 
     xlab="k (number of groups)", ylab="Pearson's correlation")
axis(1, k2008.best, paste("optimum", k2008.best, sep="\n"), col="red", font=2,
     col.axis="red")
points(k2008.best, max(kt.2008$r), pch=16, col="red", cex=1.5)
cat("", "Mantel-optimal number of clusters k =", k2008.best, "\n", 
    "with a matrix linear correlation of", max(kt.2008$r), "\n")


kt.2009 <- data.frame(k=1:nrow(n.2009[11:13]), r=0)
pb <- txtProgressBar(min = 0, max = (nrow(n.2009[11:13])-1), style = 3)
for (i in 2:(nrow(n.2009[11:13])-1)) {
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
  gr.2009 <- cutree(hc.2009, i)
  distgr.2009 <- grpdist(gr.2009)
  mt.2009 <- cor(dist.2009, distgr.2009, method="pearson")
  kt.2009[i,2] <- mt.2009
}
kt.2009
k2009.best <- which.max(kt.2009$r)
plot(kt.2009$k, kt.2009$r, type="h", xlim = c(0,20), main="Mantel-optimal number of clusters - Ward", 
     xlab="k (number of groups)", ylab="Pearson's correlation")
axis(1, k2009.best, paste("optimum", k2009.best, sep="\n"), col="red", font=2,
     col.axis="red")
points(k2009.best, max(kt.2009$r), pch=16, col="red", cex=1.5)
cat("", "Mantel-optimal number of clusters k =", k2009.best, "\n", 
    "with a matrix linear correlation of", max(kt.2009$r), "\n")

kt.2010 <- data.frame(k=1:nrow(n.2010[11:13]), r=0)
pb <- txtProgressBar(min = 0, max = (nrow(n.2010[11:13])-1), style = 3)
for (i in 2:(nrow(n.2010[11:13])-1)) {
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
  gr.2010 <- cutree(hc.2010, i)
  distgr.2010 <- grpdist(gr.2010)
  mt.2010 <- cor(dist.2010, distgr.2010, method="pearson")
  kt.2010[i,2] <- mt.2010
}
kt.2010
k2010.best <- which.max(kt.2010$r)
plot(kt.2010$k, kt.2010$r, type="h", xlim = c(0,20), main="Mantel-optimal number of clusters - Ward", 
     xlab="k (number of groups)", ylab="Pearson's correlation")
axis(1, k2010.best, paste("optimum", k2010.best, sep="\n"), col="red", font=2,
     col.axis="red")
points(k2010.best, max(kt.2010$r), pch=16, col="red", cex=1.5)
cat("", "Mantel-optimal number of clusters k =", k2010.best, "\n", 
    "with a matrix linear correlation of", max(kt.2010$r), "\n")

kt.2013 <- data.frame(k=1:nrow(n.2013[11:13]), r=0)
pb <- txtProgressBar(min = 0, max = (nrow(n.2013[11:13])-1), style = 3)
for (i in 2:(nrow(n.2013[11:13])-1)) {
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
  gr.2013 <- cutree(hc.2013, i)
  distgr.2013 <- grpdist(gr.2013)
  mt.2013 <- cor(dist.2013, distgr.2013, method="pearson")
  kt.2013[i,2] <- mt.2013
}
kt.2013
k2013.best <- which.max(kt.2013$r)
plot(kt.2013$k, kt.2013$r, type="h", xlim = c(0,20), main="Mantel-optimal number of clusters - Ward", 
     xlab="k (number of groups)", ylab="Pearson's correlation")
axis(1, k2013.best, paste("optimum", k2013.best, sep="\n"), col="red", font=2,
     col.axis="red")
points(k2013.best, max(kt.2013$r), pch=16, col="red", cex=1.5)
cat("", "Mantel-optimal number of clusters k =", k2013.best, "\n", 
    "with a matrix linear correlation of", max(kt.2013$r), "\n")

kt.2014 <- data.frame(k=1:nrow(n.2014[11:13]), r=0)
pb <- txtProgressBar(min = 0, max = (nrow(n.2014[11:13])-1), style = 3)
for (i in 2:(nrow(n.2014[11:13])-1)) {
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
  gr.2014 <- cutree(hc.2014, i)
  distgr.2014 <- grpdist(gr.2014)
  mt.2014 <- cor(dist.2014, distgr.2014, method="pearson")
  kt.2014[i,2] <- mt.2014
}
kt.2014
k2014.best <- which.max(kt.2014$r)
plot(kt.2014$k, kt.2014$r, type="h", xlim = c(0,20), main="Mantel-optimal number of clusters - Ward", 
     xlab="k (number of groups)", ylab="Pearson's correlation")
axis(1, k2014.best, paste("optimum", k2014.best, sep="\n"), col="red", font=2,
     col.axis="red")
points(k2014.best, max(kt.2014$r), pch=16, col="red", cex=1.5)
cat("", "Mantel-optimal number of clusters k =", k2014.best, "\n", 
    "with a matrix linear correlation of", max(kt.2014$r), "\n")


# Silhouette plot of the final partition
# **************************************

# Choose the number of clusters
k <- 4
# Silhouette plot
cutg.needles <- cutree(hc.needles, k=k)
sil.needles <- silhouette(cutg.needles, dist.needles)
rownames(sil.needles) <- row.names(needles.all)
plot(sil.needles, main="Silhouette plot - Chord - Ward", 
     cex.names=0.8, col=2:(k+1), nmax=100)

# Choose the number of clusters
k <- 3
# Silhouette plot
cutg.2008 <- cutree(hc.2008, k=k)
sil.2008 <- silhouette(cutg.2008, dist.2008)
rownames(sil.2008) <- row.names(n.2008)
plot(sil.2008, main="Silhouette plot - Chord - Ward", 
     cex.names=0.8, col=2:(k+1), nmax=100)

# Final dendrogram with the selected groups
# *****************************************

chwo.needles <- reorder.hclust(hc.needles, dist.needles)
plot(chwo.needles,hang = -1, axes = FALSE, labels = FALSE, main = NULL, ylab = "", xlab = "")
axis(side = 2, at = seq(0, 100, 20), labels = FALSE, lwd = 1)
mtext(seq(0, 100, 20), side = 2, at = seq(0, 100, 20), line = 1, las = 0)
mtext(side=2, "Information Remaining (%)", adj=1, line=2.2)
rect.hclust(chwo.needles, k=4, border= c("red","green","blue","purple","orange"))

chwo.2008 <- reorder.hclust(hc.2008, dist.2008)
plot(chwo.2008,hang = -1, axes = FALSE, labels = FALSE, main = NULL, ylab = "", xlab = "")
axis(side = 2, at = seq(0, 100, 20), labels = FALSE, lwd = 1)
mtext(seq(0, 100, 20), side = 2, at = seq(0, 100, 20), line = 1, las = 0)
mtext(side=2, "Information Remaining (%)", adj=1, line=2.2)
rect.hclust(chwo.2008, k=4, border= c("red","green","blue","purple","orange"))

chwo.needles <- reorder.hclust(hc.needles, dist.needles)
plot(chwo.needles,hang = -1, axes = FALSE, labels = FALSE, main = NULL, ylab = "", xlab = "")
axis(side = 2, at = seq(0, 100, 20), labels = FALSE, lwd = 1)
mtext(seq(0, 100, 20), side = 2, at = seq(0, 100, 20), line = 1, las = 0)
mtext(side=2, "Information Remaining (%)", adj=1, line=2.2)
rect.hclust(chwo.needles, k=4, border= c("red","green","blue","purple","orange"))

needles.ct <-as.data.frame(cutree(chwo.needles,k=4))

needles.all$group<-needles.ct[,1]

write.table(needles.all, file = "~/Desktop/Workspace/Earthwatch/needles.all2.csv", sep=",")

##############################################################################
## Stepwise regression of age versus stand/environmental/climatic variables ##
##############################################################################

needles.mod0 <- rda(needles.all[11:13] ~ 1, needles.all[c(1,3,4,5,6,8,9,15,16,17,18,19)])  # Model with intercept only
needles.mod1 <- rda(needles.all[11:13] ~ ., needles.all[c(1,3,4,5,6,8,9,15,16,17,18,19)])  # Model with all explanatory variables
vif(needles.mod1)
needles.step <- ordiR2step(needles.mod0, scope = formula(needles.mod1), direction = "both", R2scope = TRUE, 
                      Pin = 0.05, perm.max = 999)
summary(needles.step)
vif(needles.step)
needles.step$anova
RsquareAdj(needles.step)$r.squared
RsquareAdj(needles.step)$adj.r.squared

n2008.mod0 <- rda(n.2008[11:13] ~ 1, n.2008[c(3,5,6,8,9,15,16,17,18,19)])  # Model with intercept only
n2008.mod1 <- rda(n.2008[11:13] ~ ., n.2008[c(3,5,6,8,9,15,16,17,18,19)])  # Model with all explanatory variables
vif(n2008.mod1)
n2008.step <- ordiR2step(n2008.mod0, scope = formula(n2008.mod1), direction = "both", R2scope = TRUE, 
              Pin = 0.05, perm.max = 999)
summary(n2008.step)
vif(n2008.step)
n2008.step$anova
RsquareAdj(n2008.step)$r.squared
RsquareAdj(n2008.step)$adj.r.squared

n2009.mod0 <- rda(n.2009[11:13] ~ 1, n.2009[c(3,5,6,8,9,15,16,17,18,19)])  # Model with intercept only
n2009.mod1 <- rda(n.2009[11:13] ~ ., n.2009[c(3,5,6,8,9,15,16,17,18,19)])  # Model with all explanatory variables
vif(n2009.mod1)
n2009.step <- ordiR2step(n2009.mod0, scope = formula(n2009.mod1), direction = "both", R2scope = TRUE, 
                         Pin = 0.05, perm.max = 999)
summary(n2009.step)
vif(n2009.step)
n2009.step$anova
RsquareAdj(n2009.step)$r.squared
RsquareAdj(n2009.step)$adj.r.squared

n2010.mod0 <- rda(n.2010[11:13] ~ 1, n.2010[c(3,5,6,8,9,15,16,17,18,19)])  # Model with intercept only
n2010.mod1 <- rda(n.2010[11:13] ~ ., n.2010[c(3,5,6,8,9,15,16,17,18,19)])  # Model with all explanatory variables
vif(n2010.mod1)
n2010.step <- ordiR2step(n2010.mod0, scope = formula(n2010.mod1), direction = "both", R2scope = TRUE, 
                         Pin = 1, perm.max = 999)
summary(n2010.step)
vif(n2010.step)
n2010.step$anova
RsquareAdj(n2010.step)$r.squared
RsquareAdj(n2010.step)$adj.r.squared

n2013.mod0 <- rda(n.2013[11:13] ~ 1, n.2013[c(3,5,6,8,9,15,16,17,18,19)])  # Model with intercept only
n2013.mod1 <- rda(n.2013[11:13] ~ ., n.2013[c(3,5,6,8,9,15,16,17,18,19)])  # Model with all explanatory variables
vif(n2013.mod1)
n2013.step <- ordiR2step(n2013.mod0, scope = formula(n2013.mod1), direction = "both", R2scope = TRUE, 
                         Pin = 0.05, perm.max = 999)
summary(n2013.step)
vif(n2013.step)
n2013.step$anova
RsquareAdj(n2013.step)$r.squared
RsquareAdj(n2013.step)$adj.r.squared

n2014.mod0 <- rda(n.2014[11:13] ~ 1, n.2014[c(3,5,6,8,9,15,16,17,18,19)])  # Model with intercept only
n2014.mod1 <- rda(n.2014[11:13] ~ ., n.2014[c(3,5,6,8,9,15,16,17,18,19)])  # Model with all explanatory variables
vif(n2014.mod1)
n2014.step <- ordiR2step(n2014.mod0, scope = formula(n2014.mod1), direction = "both", R2scope = TRUE, 
                         Pin = 0.05, perm.max = 999)
summary(n2014.step)
vif(n2014.step)
n2014.step$anova
RsquareAdj(n2014.step)$r.squared
RsquareAdj(n2014.step)$adj.r.squared

#######################################
## PCA based on stepwise regressions ##
#######################################
needles.pca <- pca(needles.all[11:13],display="site")
plot(needles.pca)
ordiellipse(needles.pca, needles.all$group,conf=0.99)




needles.pca <- pca(needles.all[11:13],cor=TRUE, dim = min(nrow(needles.all[11:13]),
     ncol(needles.all[11:13])))
summary(needles.pca)
needles.load<-loadings.pca(needles.pca, dim = min(nrow(needles.all[11:13]),ncol(needles.all[11:13])))
vpch<-c(17,21,22,23)
graph <- plot(x=needles.pca$scores[,1], y=needles.pca$scores[,2], display = "sites",xlab="", ylab="", 
     cex.lab=1.2, cex.main=1.5)#, pch=vpch[needles.all$group], col = vpch[needles.all$group],
     cex = 0.75)
#surf(needles.pca,needles.all$swe)
abline(h=0,lty=3, col = "gray48")
abline(v=0,lty=3, col = "gray48")
mtext("PC2 (32.9%)", side=2, line=2, cex=1, outer=FALSE)
mtext("PC1 (43.4%)", side=1, line=2, cex=1, outer=FALSE)
needles.vec<-envfit(needles.pca$scores, needles.all[c("year","swe")], na.rm=T)
needles.vec
plot(needles.vec, add=T, col="blue", cex=0.9)


par(mfrow = c(3, 2))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(2, 1, 1.75, 3), oma = c(1,2,1,1))

n2008.pca <- pca(n.2008[11:13],cor=TRUE, dim = min(nrow(n.2008[11:13]), ncol(n.2008[11:13])))
summary(n2008.pca)
n2008.load<-loadings.pca(n2008.pca, dim = min(nrow(n.2008[11:13]),ncol(n.2008[11:13])))
vpch<-c(17,21,22,23)
graph <- plot(x=n2008.pca$scores[,1], y=n2008.pca$scores[,2], xlab="", ylab="", 
              cex.lab=1.2, cex.main=1.5)#, pch=vpch[n2008.all$group], col = vpch[n2008.all$group],
#cex = 0.75)
surf(n2008.pca,n.2008$swe)
abline(h=0,lty=3, col = "gray48")
abline(v=0,lty=3, col = "gray48")
mtext("PC2 (27.0%)", side=2, line=2, cex=1, outer=FALSE)
mtext("PC1 (54.2%)", side=1, line=2, cex=1, outer=FALSE)
n2008.vec<-envfit(n2008.pca$scores, n.2008[c("elev","swe","dist")], na.rm=T)
n2008.vec
plot(n2008.vec, add=T, col="blue", cex=0.9)

n2009.pca <- pca(n.2009[11:13],cor=TRUE, dim = min(nrow(n.2009[11:13]),
                                                   ncol(n.2009[11:13])))
summary(n2009.pca)
n2009.load<-loadings.pca(n2009.pca, dim = min(nrow(n.2009[11:13]),ncol(n.2009[11:13])))
vpch<-c(17,21,22,23)
graph <- plot(x=n2009.pca$scores[,1], y=n2009.pca$scores[,2], xlab="", ylab="", 
              cex.lab=1.2, cex.main=1.5)#, pch=vpch[n2009.all$group], col = vpch[n2009.all$group],
#cex = 0.75)
surf(n2009.pca,n.2009$swe)
abline(h=0,lty=3, col = "gray48")
abline(v=0,lty=3, col = "gray48")
mtext("PC2 (33.3%)", side=2, line=2, cex=1, outer=FALSE)
mtext("PC1 (37.7%)", side=1, line=2, cex=1, outer=FALSE)
n2009.vec<-envfit(n2009.pca$scores, n.2009[c("dist","density","cover","lon")], na.rm=T)
n2009.vec
plot(n2009.vec, add=T, col="blue", cex=0.9)

n2010.pca <- pca(n.2010[11:13],cor=TRUE, dim = min(nrow(n.2010[11:13]),ncol(n.2010[11:13])))
summary(n2010.pca)
n2010.load<-loadings.pca(n2010.pca, dim = min(nrow(n.2010[11:13]),ncol(n.2010[11:13])))
vpch<-c(17,21,22,23)
graph <- plot(x=n2010.pca$scores[,1], y=n2010.pca$scores[,2], xlab="", ylab="", 
              cex.lab=1.2, cex.main=1.5)#, pch=vpch[n2010.all$group], col = vpch[n2010.all$group],
#cex = 0.75)
surf(n2010.pca,n.2010$swe)
abline(h=0,lty=3, col = "gray48")
abline(v=0,lty=3, col = "gray48")
mtext("PC2 (32.7%)", side=2, line=2, cex=1, outer=FALSE)
mtext("PC1 (37.8%)", side=1, line=2, cex=1, outer=FALSE)
n2010.vec<-envfit(n2010.pca$scores, n.2010[c("density","dist")], na.rm=T)
n2010.vec
plot(n2010.vec, add=T, col="blue", cex=0.9)

n2013.pca <- pca(n.2013[11:13],cor=TRUE, dim = min(nrow(n.2013[11:13]),ncol(n.2013[11:13])))
summary(n2013.pca)
n2013.load<-loadings.pca(n2013.pca, dim = min(nrow(n.2013[11:13]),ncol(n.2013[11:13])))
vpch<-c(17,21,22,23)
graph <- plot(x=n2013.pca$scores[,1], y=n2013.pca$scores[,2], xlab="", ylab="", 
              cex.lab=1.2, cex.main=1.5)#, pch=vpch[n2013.all$group], col = vpch[n2013.all$group],
#cex = 0.75)
surf(n2013.pca,n.2013$swe)
abline(h=0,lty=3, col = "gray48")
abline(v=0,lty=3, col = "gray48")
mtext("PC2 (33.2%)", side=2, line=2, cex=1, outer=FALSE)
mtext("PC1 (43.2%)", side=1, line=2, cex=1, outer=FALSE)
n2013.vec<-envfit(n2013.pca$scores, n.2013[c("lat","lon","swe","htc","dist")], na.rm=T)
n2013.vec
plot(n2013.vec, add=T, col="blue", cex=0.9)


n2014.pca <- pca(n.2014[11:13],cor=TRUE, dim = min(nrow(n.2014[11:13]),ncol(n.2014[11:13])))
summary(n2014.pca)
n2014.load<-loadings.pca(n2014.pca, dim = min(nrow(n.2014[11:13]),ncol(n.2014[11:13])))
vpch<-c(17,21,22,23)
graph <- plot(x=n2014.pca$scores[,1], y=n2014.pca$scores[,2], xlab="", ylab="", 
              cex.lab=1.2, cex.main=1.5)#, pch=vpch[n2014.all$group], col = vpch[n2014.all$group],
#cex = 0.75)
surf(n2014.pca,n.2014$swe)
abline(h=0,lty=3, col = "gray48")
abline(v=0,lty=3, col = "gray48")
mtext("PC2 (33.2%)", side=2, line=2, cex=1, outer=FALSE)
mtext("PC1 (48.8%)", side=1, line=2, cex=1, outer=FALSE)
n2014.vec<-envfit(n2014.pca$scores, n.2014[c("lat","lon","swe","cover")], na.rm=T)
n2014.vec
plot(n2014.vec, add=T, col="blue", cex=0.9)






#### Plotting needle data

needles.2013 <- read.csv(file = "~/Desktop/Workspace/Earthwatch/needles2013.csv", header = TRUE)
needles.all <- read.csv(file = "~/Desktop/Workspace/EW/needles.csv", header = TRUE)
summary(needles.all)
boxplot(gmin~group*zone, data = needles.all, ylim = c(0,0.0002))
hist(needles.all$group)
aov.all <- aov(gmin ~ year*zone, data = needles.all)
summary(aov.all)
TukeyHSD(aov.all)
plot(gmin~water, data = needles.all)
abline(glm(gmin~water, data = needles.all))
summary(glm(gmin~water*zone, data = needles.all))

aov.parks <- aov(gmin ~ zone.aspect, data = needles.2013)
summary(aov.parks)
TukeyHSD(aov.parks)

needles.2013$gmin <- needles.2013$gmin*100000

### Export as 12 x 16
par(ps = 12, cex = 1, cex.axis = 1)
par(mfrow = c(2, 1))
par(mar = c(5, 5, 0.5, 1) , oma = c(2, 10, 0.5, 10))
x1 <- factor(needles.2013$zone.aspect, levels = c("Forest.L", "Ecotone.L", "Tree island.L",
                                           "Forest.W", "Ecotone.W", "Tree island.W"))
bxp1 <- boxplot(needles.2013$gmin ~ x1, xaxt = "n", xlab = "Zone", ylim = c(0,11),
                col = c("darkgreen", "green", "yellow"), 
                ylab = expression(italic("g")[min] ~ (m ~ s^{-1} ~ 10^{-5})))
abline(v = 3.5)
axis(1, at = 1:6, labels = c("Forest", "Ecotone", "Island",
                             "Forest", "Ecotone", "Island"))
text(x = c(2,5), y = c(10.5,10.5), label=c("Leeward","Windward"))

x2 <- factor(needles.2013$zone.height, levels = c("Forest.C", "Forest.A", "Forest.S",
                                                  "Ecotone.C", "Ecotone.A", "Ecotone.S",
                                                  "Tree island.C","Tree island.A","Tree island.S"))
bxp2 <- boxplot(needles.2013$gmin ~ x2, xaxt = "n", xlab = "Height", ylim = c(0,11),
                col = c("darkgreen", "green", "yellow"), 
                ylab = expression(italic("g")[min] ~ (m ~ s^{-1} ~ 10^{-5})))
abline(v = 3.5)
abline(v = 6.5)
axis(1, at = 1:9, labels = c("Crown", "Abrasion", "Snow",
                             "Crown", "Abrasion", "Snow",
                             "Crown", "Abrasion", "Snow"))
text(x = c(2,5,8), y = c(10.5,10.5,10.5), label=c("Forest","Ecotone","Island"))

#**************************************#
#### Plotting snow data ####
#**************************************#


snow.cores.all <- read.csv(file = "~/Desktop/Workspace/EW/SnowCores2015.csv", header = TRUE)
park.snow <- read.csv(file = "~/Desktop/Workspace/EW/ParkSnow.csv", header = TRUE)
snow.ti <- read.csv(file = "~/Desktop/Workspace/EW/SnowCoresTI.csv", header = TRUE)
snow.ipy <- read.csv(file = "~/Desktop/Workspace/EW/SnowCoresIPY.csv", header = TRUE)

ti.2015 <- subset(snow.ti, year == "2015") 
ti.2014 <- subset(snow.ti, year == "2014") 
ti.2013 <- subset(snow.ti, year == "2013") 
ti.2012 <- subset(snow.ti, year == "2012") 
t10 <- subset(ti.2015, site == "T10")
t11 <- subset(ti.2015, site == "T11")
t12 <- subset(ti.2015, site == "T12")
t13 <- subset(ti.2015, site == "T13")


ipy.2015 <- subset(snow.ipy, year == "2015") 
ipy.2014 <- subset(snow.ipy, year == "2014") 
ipy.2013 <- subset(snow.ipy, year == "2013") 
ipy.2009 <- subset(snow.ipy, year == "2009") 
ipy.2008 <- subset(snow.ipy, year == "2008") 

ipy.ram <- subset(ipy.2015, site == "RAM")
ipy.ra2 <- subset(ipy.2015, site == "RA2")
ipy.rid <- subset(ipy.2015, site == "RID")
ipy.rok <- subset(ipy.2015, site == "ROK")
ipy.lin <- subset(ipy.2015, site == "LIN")
ipy.blk <- subset(ipy.2015, site == "BLK")

snow.ram <- subset(snow.ipy, site == "RAM")
snow.ra2 <- subset(snow.ipy, site == "RA2")
snow.rid <- subset(snow.ipy, site == "RID")
snow.rok <- subset(snow.ipy, site == "ROK")
snow.lin <- subset(snow.ipy, site == "LIN")
snow.blk <- subset(snow.ipy, site == "BLK")

par(mfrow = c(2, 2))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(2, 1, 1.75, 3), oma = c(1,2,1,1))
# Export at 10 x 8

aov.depth.ram <- aov(depth ~ zone, data = ipy.ram)
summary(aov.depth.ram)
TukeyHSD(aov.depth.ram)
depth.ram.pairs <- glht(aov.depth.ram, linfct = mcp(zone = "Tukey"))
depth.ram.cld <- cld(depth.ram.pairs)
depth.ram.letters <- depth.ram.cld$mcletters$Letters

x1 <- factor(ipy.ram$zone, levels = c("F","E","T"))
boxplot(depth ~ x1, data = ipy.ram,
                    col = c("darkgreen","lightgreen","royalblue2"),
                    ylim = c(0,500), ylab = '')
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
depth.y <- t(aggregate(ipy.ram$depth, by=list(x1), FUN=max)[2])
text(x=c(1,2,3),y=depth.y+30,c("a","b","c"))

aov.density.ram <- aov(density ~ zone, data = ipy.ram)
summary(aov.density.ram)
TukeyHSD(aov.density.ram)
density.ram.pairs <- glht(aov.density.ram, linfct = mcp(zone = "Tukey"))
density.ram.cld <- cld(density.ram.pairs)
density.ram.letters <- density.ram.cld$mcletters$Letters

x1 <- factor(ipy.ram$zone, levels = c("F","E","T"))
boxplot(density ~ x1, data = ipy.ram,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,600), ylab = '')
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
density.y <- t(aggregate(ipy.ram$density, by=list(x1), FUN=max)[2])
text(x=c(1,2,3),y=density.y+30,c("a","b","b"))

aov.swe.ram <- aov(swe ~ zone, data = ipy.ram)
summary(aov.swe.ram)
TukeyHSD(aov.swe.ram)
swe.ram.pairs <- glht(aov.swe.ram, linfct = mcp(zone = "Tukey"))
swe.ram.cld <- cld(swe.ram.pairs)
swe.ram.letters <- swe.ram.cld$mcletters$Letters

x1 <- factor(ipy.ram$zone, levels = c("F","E","T"))
boxplot(swe ~ x1, data = ipy.ram,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,1800), ylab = '')
mtext(expression(paste("swe (mm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
swe.y <- t(aggregate(ipy.ram$swe, by=list(x1), FUN=max)[2])
text(x=c(1,2,3),y=swe.y+80,c("a","b","c"))


aov.htc.ram <- aov(htc ~ zone, data = ipy.ram)
summary(aov.htc.ram)
TukeyHSD(aov.htc.ram)
htc.ram.pairs <- glht(aov.htc.ram, linfct = mcp(zone = "Tukey"))
htc.ram.cld <- cld(htc.ram.pairs)
htc.ram.letters <- htc.ram.cld$mcletters$Letters

x1 <- factor(ipy.ram$zone, levels = c("F","E","T"))
boxplot(htc ~ x1, data = ipy.ram,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,8), ylab = '')
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
htc.y <- t(aggregate(ipy.ram$htc, by=list(x1), FUN=max)[2])
text(x=c(1,2,3),y=htc.y+0.3,c("a","a","b"))

########################

par(mfrow = c(2, 2))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(2, 1, 1.75, 3), oma = c(1,2,1,1))
# Export at 10 x 8

aov.depth.ra2 <- aov(depth ~ zone, data = ipy.ra2)
summary(aov.depth.ra2)
TukeyHSD(aov.depth.ra2)
depth.ra2.pairs <- glht(aov.depth.ra2, linfct = mcp(zone = "Tukey"))
depth.ra2.cld <- cld(depth.ra2.pairs)
depth.ra2.letters <- depth.ra2.cld$mcletters$Letters

x1 <- factor(ipy.ra2$zone, levels = c("F","E","T"))
boxplot(depth ~ x1, data = ipy.ra2,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,500), ylab = '')
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
depth.y <- t(aggregate(ipy.ra2$depth, by=list(x1), FUN=max)[2])
text(x=c(1,2,3),y=depth.y+30,c("a","b","c"))

aov.density.ra2 <- aov(density ~ zone, data = ipy.ra2)
summary(aov.density.ra2)
TukeyHSD(aov.density.ra2)
density.ra2.pairs <- glht(aov.density.ra2, linfct = mcp(zone = "Tukey"))
density.ra2.cld <- cld(density.ra2.pairs)
density.ra2.letters <- density.ra2.cld$mcletters$Letters

x1 <- factor(ipy.ra2$zone, levels = c("F","E","T"))
boxplot(density ~ x1, data = ipy.ra2,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,600), ylab = '')
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
density.y <- t(aggregate(ipy.ra2$density, by=list(x1), FUN=max)[2])
text(x=c(1,2,3),y=density.y+30,c("a","a","a"))

aov.swe.ra2 <- aov(swe ~ zone, data = ipy.ra2)
summary(aov.swe.ra2)
TukeyHSD(aov.swe.ra2)
swe.ra2.pairs <- glht(aov.swe.ra2, linfct = mcp(zone = "Tukey"))
swe.ra2.cld <- cld(swe.ra2.pairs)
swe.ra2.letters <- swe.ra2.cld$mcletters$Letters

x1 <- factor(ipy.ra2$zone, levels = c("F","E","T"))
boxplot(swe ~ x1, data = ipy.ra2,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,1800), ylab = '')
mtext(expression(paste("swe (mm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
swe.y <- t(aggregate(ipy.ra2$swe, by=list(x1), FUN=max)[2])
text(x=c(1,2,3),y=swe.y+80,c("a","b","c"))


aov.htc.ra2 <- aov(htc ~ zone, data = ipy.ra2)
summary(aov.htc.ra2)
TukeyHSD(aov.htc.ra2)
htc.ra2.pairs <- glht(aov.htc.ra2, linfct = mcp(zone = "Tukey"))
htc.ra2.cld <- cld(htc.ra2.pairs)
htc.ra2.letters <- htc.ra2.cld$mcletters$Letters

x1 <- factor(ipy.ra2$zone, levels = c("F","E","T"))
boxplot(htc ~ x1, data = ipy.ra2,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,8), ylab = '')
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
htc.y <- t(aggregate(ipy.ra2$htc, by=list(x1), FUN=max)[2])
text(x=c(1,2,3),y=htc.y+0.3,c("a","a","b"))

########################

par(mfrow = c(2, 2))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(2, 1, 1.75, 3), oma = c(1,2,1,1))
# Export at 10 x 8

aov.depth.rid <- aov(depth ~ zone, data = ipy.rid)
summary(aov.depth.rid)
TukeyHSD(aov.depth.rid)
depth.rid.pairs <- glht(aov.depth.rid, linfct = mcp(zone = "Tukey"))
depth.rid.cld <- cld(depth.rid.pairs)
depth.rid.letters <- depth.rid.cld$mcletters$Letters

x1 <- factor(ipy.rid$zone, levels = c("F","E","T"))
boxplot(depth ~ x1, data = ipy.rid,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,500), ylab = '')
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
depth.y <- t(aggregate(ipy.rid$depth, by=list(x1), FUN=max)[2])
text(x=c(1,2,3),y=depth.y+30,c("a","b","c"))

aov.density.rid <- aov(density ~ zone, data = ipy.rid)
summary(aov.density.rid)
TukeyHSD(aov.density.rid)
density.rid.pairs <- glht(aov.density.rid, linfct = mcp(zone = "Tukey"))
density.rid.cld <- cld(density.rid.pairs)
density.rid.letters <- density.rid.cld$mcletters$Letters

x1 <- factor(ipy.rid$zone, levels = c("F","E","T"))
boxplot(density ~ x1, data = ipy.rid,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,600), ylab = '')
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
density.y <- t(aggregate(ipy.rid$density, by=list(x1), FUN=max)[2])
text(x=c(1,2,3),y=density.y+30,c("a","b","b"))

aov.swe.rid <- aov(swe ~ zone, data = ipy.rid)
summary(aov.swe.rid)
TukeyHSD(aov.swe.rid)
swe.rid.pairs <- glht(aov.swe.rid, linfct = mcp(zone = "Tukey"))
swe.rid.cld <- cld(swe.rid.pairs)
swe.rid.letters <- swe.rid.cld$mcletters$Letters

x1 <- factor(ipy.rid$zone, levels = c("F","E","T"))
boxplot(swe ~ x1, data = ipy.rid,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,1800), ylab = '')
mtext(expression(paste("swe (mm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
swe.y <- t(aggregate(ipy.rid$swe, by=list(x1), FUN=max)[2])
text(x=c(1,2,3),y=swe.y+80,c("a","b","c"))


aov.htc.rid <- aov(htc ~ zone, data = ipy.rid)
summary(aov.htc.rid)
TukeyHSD(aov.htc.rid)
htc.rid.pairs <- glht(aov.htc.rid, linfct = mcp(zone = "Tukey"))
htc.rid.cld <- cld(htc.rid.pairs)
htc.rid.letters <- htc.rid.cld$mcletters$Letters

x1 <- factor(ipy.rid$zone, levels = c("F","E","T"))
boxplot(htc ~ x1, data = ipy.rid,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,8), ylab = '')
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
htc.y <- t(aggregate(ipy.rid$htc, by=list(x1), FUN=max)[2])
text(x=c(1,2,3),y=htc.y+0.3,c("a","a","b"))

########################

par(mfrow = c(2, 2))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(2, 1, 1.75, 3), oma = c(1,2,1,1))
# Export at 10 x 8

aov.depth.rok <- aov(depth ~ zone, data = ipy.rok)
summary(aov.depth.rok)
TukeyHSD(aov.depth.rok)
depth.rok.pairs <- glht(aov.depth.rok, linfct = mcp(zone = "Tukey"))
depth.rok.cld <- cld(depth.rok.pairs)
depth.rok.letters <- depth.rok.cld$mcletters$Letters

x1 <- factor(ipy.rok$zone, levels = c("F","E","T"))
boxplot(depth ~ x1, data = ipy.rok,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,500), ylab = '')
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
depth.y <- t(aggregate(ipy.rok$depth, by=list(x1), FUN=max)[2])
text(x=c(1,2,3),y=depth.y+30,c("a","b","c"))

aov.density.rok <- aov(density ~ zone, data = ipy.rok)
summary(aov.density.rok)
TukeyHSD(aov.density.rok)
density.rok.pairs <- glht(aov.density.rok, linfct = mcp(zone = "Tukey"))
density.rok.cld <- cld(density.rok.pairs)
density.rok.letters <- density.rok.cld$mcletters$Letters

x1 <- factor(ipy.rok$zone, levels = c("F","E","T"))
boxplot(density ~ x1, data = ipy.rok,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,600), ylab = '')
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
density.y <- t(aggregate(ipy.rok$density, by=list(x1), FUN=max)[2])
text(x=c(1,2,3),y=density.y+30,c("a","b","c"))

aov.swe.rok <- aov(swe ~ zone, data = ipy.rok)
summary(aov.swe.rok)
TukeyHSD(aov.swe.rok)
swe.rok.pairs <- glht(aov.swe.rok, linfct = mcp(zone = "Tukey"))
swe.rok.cld <- cld(swe.rok.pairs)
swe.rok.letters <- swe.rok.cld$mcletters$Letters

x1 <- factor(ipy.rok$zone, levels = c("F","E","T"))
boxplot(swe ~ x1, data = ipy.rok,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,1800), ylab = '')
mtext(expression(paste("swe (mm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
swe.y <- t(aggregate(ipy.rok$swe, by=list(x1), FUN=max)[2])
text(x=c(1,2,3),y=swe.y+80,c("a","b","c"))


aov.htc.rok <- aov(htc ~ zone, data = ipy.rok)
summary(aov.htc.rok)
TukeyHSD(aov.htc.rok)
htc.rok.pairs <- glht(aov.htc.rok, linfct = mcp(zone = "Tukey"))
htc.rok.cld <- cld(htc.rok.pairs)
htc.rok.letters <- htc.rok.cld$mcletters$Letters

x1 <- factor(ipy.rok$zone, levels = c("F","E","T"))
boxplot(htc ~ x1, data = ipy.rok,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,8), ylab = '')
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
htc.y <- t(aggregate(ipy.rok$htc, by=list(x1), FUN=max)[2])
text(x=c(1,2,3),y=htc.y+0.3,c("a","a","b"))

########################

par(mfrow = c(2, 2))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(2, 1, 1.75, 3), oma = c(1,2,1,1))
# Export at 10 x 8

aov.depth.lin <- aov(depth ~ zone, data = ipy.lin)
summary(aov.depth.lin)
TukeyHSD(aov.depth.lin)
depth.lin.pairs <- glht(aov.depth.lin, linfct = mcp(zone = "Tukey"))
depth.lin.cld <- cld(depth.lin.pairs)
depth.lin.letters <- depth.lin.cld$mcletters$Letters

x1 <- factor(ipy.lin$zone, levels = c("F","E","T"))
boxplot(depth ~ x1, data = ipy.lin,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,500), ylab = '')
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
depth.y <- t(aggregate(ipy.lin$depth, by=list(x1), FUN=max)[2])
text(x=c(1,2,3),y=depth.y+30,c("a","a","c"))

aov.density.lin <- aov(density ~ zone, data = ipy.lin)
summary(aov.density.lin)
TukeyHSD(aov.density.lin)
density.lin.pairs <- glht(aov.density.lin, linfct = mcp(zone = "Tukey"))
density.lin.cld <- cld(density.lin.pairs)
density.lin.letters <- density.lin.cld$mcletters$Letters

x1 <- factor(ipy.lin$zone, levels = c("F","E","T"))
boxplot(density ~ x1, data = ipy.lin,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,600), ylab = '')
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
density.y <- t(aggregate(ipy.lin$density, by=list(x1), FUN=max)[2])
text(x=c(1,2,3),y=density.y+30,c("a","b","b"))

aov.swe.lin <- aov(swe ~ zone, data = ipy.lin)
summary(aov.swe.lin)
TukeyHSD(aov.swe.lin)
swe.lin.pairs <- glht(aov.swe.lin, linfct = mcp(zone = "Tukey"))
swe.lin.cld <- cld(swe.lin.pairs)
swe.lin.letters <- swe.lin.cld$mcletters$Letters

x1 <- factor(ipy.lin$zone, levels = c("F","E","T"))
boxplot(swe ~ x1, data = ipy.lin,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,1800), ylab = '')
mtext(expression(paste("swe (mm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
swe.y <- t(aggregate(ipy.lin$swe, by=list(x1), FUN=max)[2])
text(x=c(1,2,3),y=swe.y+80,c("a","a","b"))


aov.htc.lin <- aov(htc ~ zone, data = ipy.lin)
summary(aov.htc.lin)
TukeyHSD(aov.htc.lin)
htc.lin.pairs <- glht(aov.htc.lin, linfct = mcp(zone = "Tukey"))
htc.lin.cld <- cld(htc.lin.pairs)
htc.lin.letters <- htc.lin.cld$mcletters$Letters

x1 <- factor(ipy.lin$zone, levels = c("F","E","T"))
boxplot(htc ~ x1, data = ipy.lin,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,8), ylab = '')
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
htc.y <- t(aggregate(ipy.lin$htc, by=list(x1), FUN=max)[2])
text(x=c(1,2,3),y=htc.y+0.3,c("a","a","b"))

########################

par(mfrow = c(2, 2))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(2, 1, 1.75, 3), oma = c(1,2,1,1))
# Export at 10 x 8

aov.depth.blk <- aov(depth ~ zone, data = ipy.blk)
summary(aov.depth.blk)
TukeyHSD(aov.depth.blk)
depth.blk.pairs <- glht(aov.depth.blk, linfct = mcp(zone = "Tukey"))
depth.blk.cld <- cld(depth.blk.pairs)
depth.blk.letters <- depth.blk.cld$mcletters$Letters

x1 <- factor(ipy.blk$zone, levels = c("F","E","T"))
boxplot(depth ~ x1, data = ipy.blk,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,500), ylab = '')
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
depth.y <- t(aggregate(ipy.blk$depth, by=list(x1), FUN=max)[2])
text(x=c(1,2,3),y=depth.y+30,c("a","a","b"))

aov.density.blk <- aov(density ~ zone, data = ipy.blk)
summary(aov.density.blk)
TukeyHSD(aov.density.blk)
density.blk.pairs <- glht(aov.density.blk, linfct = mcp(zone = "Tukey"))
density.blk.cld <- cld(density.blk.pairs)
density.blk.letters <- density.blk.cld$mcletters$Letters

x1 <- factor(ipy.blk$zone, levels = c("F","E","T"))
boxplot(density ~ x1, data = ipy.blk,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,600), ylab = '')
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
density.y <- t(aggregate(ipy.blk$density, by=list(x1), FUN=max)[2])
text(x=c(1,2,3),y=density.y+30,c("ab","b","a"))

aov.swe.blk <- aov(swe ~ zone, data = ipy.blk)
summary(aov.swe.blk)
TukeyHSD(aov.swe.blk)
swe.blk.pairs <- glht(aov.swe.blk, linfct = mcp(zone = "Tukey"))
swe.blk.cld <- cld(swe.blk.pairs)
swe.blk.letters <- swe.blk.cld$mcletters$Letters

x1 <- factor(ipy.blk$zone, levels = c("F","E","T"))
boxplot(swe ~ x1, data = ipy.blk,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,1800), ylab = '')
mtext(expression(paste("swe (mm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
swe.y <- t(aggregate(ipy.blk$swe, by=list(x1), FUN=max)[2])
text(x=c(1,2,3),y=swe.y+80,c("a","a","b"))


aov.htc.blk <- aov(htc ~ zone, data = ipy.blk)
summary(aov.htc.blk)
TukeyHSD(aov.htc.blk)
htc.blk.pairs <- glht(aov.htc.blk, linfct = mcp(zone = "Tukey"))
htc.blk.cld <- cld(htc.blk.pairs)
htc.blk.letters <- htc.blk.cld$mcletters$Letters

x1 <- factor(ipy.blk$zone, levels = c("F","E","T"))
boxplot(htc ~ x1, data = ipy.blk,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,8), ylab = '')
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
htc.y <- t(aggregate(ipy.blk$htc, by=list(x1), FUN=max)[2])
text(x=c(1,2,3),y=htc.y+0.3,c("a","a","b"))

##################

par(mfrow = c(3, 2))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(2, 1, 1.75, 3), oma = c(1,2,1,1))
# Export at 10 x 8

aov.depth.ram <- aov(depth ~ zone*year, data = snow.ram)
summary(aov.depth.ram)
TukeyHSD(aov.depth.ram)

boxplot(depth ~ zone.year, data = snow.ram,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,500), ylab = '')
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=450,"RAM",pos=4, font = 2)

aov.depth.ra2 <- aov(depth ~ zone*year, data = snow.ra2)
summary(aov.depth.ra2)
TukeyHSD(aov.depth.ra2)

boxplot(depth ~ zone.year, data = snow.ra2,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,500), ylab = '')
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=450,"RA2",pos=4, font = 2)

aov.depth.rid <- aov(depth ~ zone*year, data = snow.rid)
summary(aov.depth.rid)
TukeyHSD(aov.depth.rid)

boxplot(depth ~ zone.year, data = snow.rid,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,500), ylab = '')
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=450,"RID",pos=4, font = 2)

aov.depth.rok <- aov(depth ~ zone*year, data = snow.rok)
summary(aov.depth.rok)
TukeyHSD(aov.depth.rok)

boxplot(depth ~ zone.year, data = snow.rok,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,500), ylab = '')
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=450,"ROK",pos=4, font = 2)

aov.depth.lin <- aov(depth ~ zone*year, data = snow.lin)
summary(aov.depth.lin)
TukeyHSD(aov.depth.lin)

boxplot(depth ~ zone.year, data = snow.lin,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,500), ylab = '')
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=450,"LIN",pos=4, font = 2)

aov.depth.blk <- aov(depth ~ zone*year, data = snow.blk)
summary(aov.depth.blk)
TukeyHSD(aov.depth.blk)

boxplot(depth ~ zone.year, data = snow.blk,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,500), ylab = '')
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=450,"BLK",pos=4, font = 2)

##########

par(mfrow = c(3, 2))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(2, 1, 1.75, 3), oma = c(1,2,1,1))
# Export at 10 x 8

aov.density.ram <- aov(density ~ zone*year, data = snow.ram)
summary(aov.density.ram)
TukeyHSD(aov.density.ram)

boxplot(density ~ zone.year, data = snow.ram,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,600), ylab = '')
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=550,"RAM",pos=4, font = 2)

aov.density.ra2 <- aov(density ~ zone*year, data = snow.ra2)
summary(aov.density.ra2)
TukeyHSD(aov.density.ra2)

boxplot(density ~ zone.year, data = snow.ra2,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,600), ylab = '')
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=550,"RA2",pos=4, font = 2)

aov.density.rid <- aov(density ~ zone*year, data = snow.rid)
summary(aov.density.rid)
TukeyHSD(aov.density.rid)

boxplot(density ~ zone.year, data = snow.rid,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,600), ylab = '')
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=550,"RID",pos=4, font = 2)

aov.density.rok <- aov(density ~ zone*year, data = snow.rok)
summary(aov.density.rok)
TukeyHSD(aov.density.rok)

boxplot(density ~ zone.year, data = snow.rok,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,600), ylab = '')
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=550,"ROK",pos=4, font = 2)

aov.density.lin <- aov(density ~ zone*year, data = snow.lin)
summary(aov.density.lin)
TukeyHSD(aov.density.lin)

boxplot(density ~ zone.year, data = snow.lin,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,600), ylab = '')
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=550,"LIN",pos=4, font = 2)

aov.density.blk <- aov(density ~ zone*year, data = snow.blk)
summary(aov.density.blk)
TukeyHSD(aov.density.blk)

boxplot(density ~ zone.year, data = snow.blk,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,600), ylab = '')
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=550,"BLK",pos=4, font = 2)

##########

par(mfrow = c(3, 2))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(2, 1, 1.75, 3), oma = c(1,2,1,1))
# Export at 10 x 8

aov.swe.ram <- aov(swe ~ zone*year, data = snow.ram)
summary(aov.swe.ram)
TukeyHSD(aov.swe.ram)

boxplot(swe ~ zone.year, data = snow.ram,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,1800), ylab = '')
mtext(expression(paste("SWE (mm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=1700,"RAM",pos=4, font = 2)

aov.swe.ra2 <- aov(swe ~ zone*year, data = snow.ra2)
summary(aov.swe.ra2)
TukeyHSD(aov.swe.ra2)

boxplot(swe ~ zone.year, data = snow.ra2,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,1800), ylab = '')
mtext(expression(paste("SWE (mm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=1700,"RA2",pos=4, font = 2)

aov.swe.rid <- aov(swe ~ zone*year, data = snow.rid)
summary(aov.swe.rid)
TukeyHSD(aov.swe.rid)

boxplot(swe ~ zone.year, data = snow.rid,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,1800), ylab = '')
mtext(expression(paste("SWE (mm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=1700,"RID",pos=4, font = 2)

aov.swe.rok <- aov(swe ~ zone*year, data = snow.rok)
summary(aov.swe.rok)
TukeyHSD(aov.swe.rok)

boxplot(swe ~ zone.year, data = snow.rok,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,1800), ylab = '')
mtext(expression(paste("SWE (mm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=1700,"ROK",pos=4, font = 2)

aov.swe.lin <- aov(swe ~ zone*year, data = snow.lin)
summary(aov.swe.lin)
TukeyHSD(aov.swe.lin)

boxplot(swe ~ zone.year, data = snow.lin,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,1800), ylab = '')
mtext(expression(paste("SWE (mm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=1700,"LIN",pos=4, font = 2)

aov.swe.blk <- aov(swe ~ zone*year, data = snow.blk)
summary(aov.swe.blk)
TukeyHSD(aov.swe.blk)

boxplot(swe ~ zone.year, data = snow.blk,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,1800), ylab = '')
mtext(expression(paste("SWE (mm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=1700,"BLK",pos=4, font = 2)

##########

par(mfrow = c(3, 2))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(2, 1, 1.75, 3), oma = c(1,2,1,1))
# Export at 10 x 8

aov.htc.ram <- aov(htc ~ zone*year, data = snow.ram)
summary(aov.htc.ram)
TukeyHSD(aov.htc.ram)

boxplot(htc ~ zone.year, data = snow.ram,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,8), ylab = '')
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=7,"RAM",pos=4, font = 2)

aov.htc.ra2 <- aov(htc ~ zone*year, data = snow.ra2)
summary(aov.htc.ra2)
TukeyHSD(aov.htc.ra2)

boxplot(htc ~ zone.year, data = snow.ra2,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,8), ylab = '')
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=7,"RA2",pos=4, font = 2)

aov.htc.rid <- aov(htc ~ zone*year, data = snow.rid)
summary(aov.htc.rid)
TukeyHSD(aov.htc.rid)

boxplot(htc ~ zone.year, data = snow.rid,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,8), ylab = '')
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=7,"RID",pos=4, font = 2)

aov.htc.rok <- aov(htc ~ zone*year, data = snow.rok)
summary(aov.htc.rok)
TukeyHSD(aov.htc.rok)

boxplot(htc ~ zone.year, data = snow.rok,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,8), ylab = '')
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=7,"ROK",pos=4, font = 2)

aov.htc.lin <- aov(htc ~ zone*year, data = snow.lin)
summary(aov.htc.lin)
TukeyHSD(aov.htc.lin)

boxplot(htc ~ zone.year, data = snow.lin,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,8), ylab = '')
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=7,"LIN",pos=4, font = 2)

aov.htc.blk <- aov(htc ~ zone*year, data = snow.blk)
summary(aov.htc.blk)
TukeyHSD(aov.htc.blk)

boxplot(htc ~ zone.year, data = snow.blk,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,8), ylab = '')
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=7,"BLK",pos=4, font = 2)

######################################

par(mfrow = c(2, 2))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(2, 1, 1.75, 3), oma = c(1,2,1,1))
# Export at 10 x 8

aov.depth <- aov(depth ~ zone.year, data = snow.ipy)
summary(aov.depth)
TukeyHSD(aov.depth)
#depth.pairs <- glht(aov.depth, linfct = mcp(zone.year = "Tukey"))
#depth.cld <- cld(depth.pairs)
#depth.letters <- depth.cld$mcletters$Letters

boxplot(depth ~ zone.year, data = snow.ipy,
        col = c("darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","yellow","yellow",
                "yellow","yellow","yellow","cyan","cyan","cyan","cyan","cyan"),
        ylim = c(0,600), ylab = '')
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=550,"Depth",pos=4, font = 2)
#depth.y <- t(aggregate(snow.ipy$depth, by=list(snow.ipy$zone.year), FUN=max)[2])
#text(x=seq(1,15,by=1),depth.y+30, depth.letters)

boxplot(density ~ zone.year, data = snow.ipy,
        col = c("darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","yellow","yellow",
                "yellow","yellow","yellow","cyan","cyan","cyan","cyan","cyan"),
        ylim = c(0,600), ylab = '')
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=550,"Density",pos=4, font = 2)

boxplot(swe ~ zone.year, data = snow.ipy,
        col = c("darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","yellow","yellow",
                "yellow","yellow","yellow","cyan","cyan","cyan","cyan","cyan"),
        ylim = c(0,1800), ylab = '')
mtext(expression(paste("SWE (mm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=1650,"SWE",pos=4, font = 2)

boxplot(htc ~ zone.year, data = snow.ipy,
        col = c("darkgreen","darkgreen","darkgreen","darkgreen","darkgreen","yellow","yellow",
                "yellow","yellow","yellow","cyan","cyan","cyan","cyan","cyan"),
        ylim = c(0,8), ylab = '')
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(x=1,y=7,"HTC",pos=4, font = 2)












              ######################
              #### Tree Islands ####
              ######################

par(mfrow = c(2, 2))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(2, 1, 1.75, 3), oma = c(1,2,1,1))
# Export at 10 x 8


boxplot(depth ~ zone, data = t10,
        col = c("royalblue4","lightblue1","royalblue2","slateblue"),
        ylim = c(0,300), ylab = '')
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(1, 275,"T10")

boxplot(depth ~ zone, data = t11,
        col = c("royalblue4","lightblue1","royalblue2","slateblue"),
        ylim = c(0,300), ylab = '')
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(1, 275,"T11")

boxplot(depth ~ zone, data = t12,
        col = c("royalblue4","lightblue1","royalblue2","slateblue"),
        ylim = c(0,300), ylab = '')
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(1, 275,"T12")

boxplot(depth ~ zone, data = t13,
        col = c("royalblue4","lightblue1","royalblue2","slateblue"),
        ylim = c(0,300), ylab = '')
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(1, 275,"T13")

aov.t10 <- aov(depth ~ zone, data = t10)
summary(aov.t10)
TukeyHSD(aov.t10)

aov.t11 <- aov(depth ~ zone, data = t11)
summary(aov.t11)
TukeyHSD(aov.t11)

aov.t12 <- aov(depth ~ zone, data = t12)
summary(aov.t12)
TukeyHSD(aov.t12)

aov.t13 <- aov(depth ~ zone, data = t13)
summary(aov.t13)
TukeyHSD(aov.t13)


par(mfrow = c(2, 2))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(2, 1, 1.75, 3), oma = c(1,2,1,1))
# Export at 10 x 8


boxplot(density ~ zone, data = t10,
        col = c("royalblue4","lightblue1","royalblue2","slateblue"),
        ylim = c(100,600), ylab = '')
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(1, 550,"T10")

boxplot(density ~ zone, data = t11,
        col = c("royalblue4","lightblue1","royalblue2","slateblue"),
        ylim = c(100,600), ylab = '')
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(1, 550,"T11")

boxplot(density ~ zone, data = t12,
        col = c("royalblue4","lightblue1","royalblue2","slateblue"),
        ylim = c(100,600), ylab = '')
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(1, 550,"T12")

boxplot(density ~ zone, data = t13,
        col = c("royalblue4","lightblue1","royalblue2","slateblue"),
        ylim = c(100,600), ylab = '')
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(1, 550,"T13")

aov.t10 <- aov(density ~ zone, data = t10)
summary(aov.t10)
TukeyHSD(aov.t10)

aov.t11 <- aov(density ~ zone, data = t11)
summary(aov.t11)
TukeyHSD(aov.t11)

aov.t12 <- aov(density ~ zone, data = t12)
summary(aov.t12)
TukeyHSD(aov.t12)

aov.t13 <- aov(density ~ zone, data = t13)
summary(aov.t13)
TukeyHSD(aov.t13)

par(mfrow = c(2, 2))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(2, 1, 1.75, 3), oma = c(1,2,1,1))
# Export at 10 x 8


boxplot(swe ~ zone, data = t10,
        col = c("royalblue4","lightblue1","royalblue2","slateblue"),
        ylim = c(0,1000), ylab = '')
mtext(expression(paste("SWE (mm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(1, 900,"T10")

boxplot(swe ~ zone, data = t11,
        col = c("royalblue4","lightblue1","royalblue2","slateblue"),
        ylim = c(0,1000), ylab = '')
mtext(expression(paste("SWE (mm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(1, 900,"T11")

boxplot(swe ~ zone, data = t12,
        col = c("royalblue4","lightblue1","royalblue2","slateblue"),
        ylim = c(0,1000), ylab = '')
mtext(expression(paste("SWE (mm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(1, 900,"T12")

boxplot(swe ~ zone, data = t13,
        col = c("royalblue4","lightblue1","royalblue2","slateblue"),
        ylim = c(0,1000), ylab = '')
mtext(expression(paste("SWE (mm)")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(1, 900,"T13")

aov.t10 <- aov(swe ~ zone, data = t10)
summary(aov.t10)
TukeyHSD(aov.t10)

aov.t11 <- aov(swe ~ zone, data = t11)
summary(aov.t11)
TukeyHSD(aov.t11)

aov.t12 <- aov(swe ~ zone, data = t12)
summary(aov.t12)
TukeyHSD(aov.t12)

aov.t13 <- aov(swe ~ zone, data = t13)
summary(aov.t13)
TukeyHSD(aov.t13)


par(mfrow = c(2, 2))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(2, 1, 1.75, 3), oma = c(1,2,1,1))
# Export at 10 x 8


boxplot(htc ~ zone, data = t10,
        col = c("royalblue4","lightblue1","royalblue2","slateblue"),
        ylim = c(0,10), ylab = '')
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(1, 9,"T10")

boxplot(htc ~ zone, data = t11,
        col = c("royalblue4","lightblue1","royalblue2","slateblue"),
        ylim = c(0,10), ylab = '')
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(1, 9,"T11")

boxplot(htc ~ zone, data = t12,
        col = c("royalblue4","lightblue1","royalblue2","slateblue"),
        ylim = c(0,10), ylab = '')
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(1, 9,"T12")

boxplot(htc ~ zone, data = t13,
        col = c("royalblue4","lightblue1","royalblue2","slateblue"),
        ylim = c(0,10), ylab = '')
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
text(1, 9,"T13")

aov.t10 <- aov(htc ~ zone, data = t10)
summary(aov.t10)
TukeyHSD(aov.t10)

aov.t11 <- aov(htc ~ zone, data = t11)
summary(aov.t11)
TukeyHSD(aov.t11)

aov.t12 <- aov(htc ~ zone, data = t12)
summary(aov.t12)
TukeyHSD(aov.t12)

aov.t13 <- aov(htc ~ zone, data = t13)
summary(aov.t13)
TukeyHSD(aov.t13)


par(mfrow = c(2, 2))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(2, 1, 1.75, 3), oma = c(1,2,1,1))
# Export at 10 x 8

snow.ti2 <- snow.ti
snow.ti2$year <- as.factor(snow.ti2$year)
aov.depth <- aov(depth ~ year, data = snow.ti2)
summary(aov.depth)
TukeyHSD(aov.depth)
depth.pairs <- glht(aov.depth, linfct = mcp(year = "Tukey"))
depth.cld <- cld(depth.pairs)
depth.letters <- depth.cld$mcletters$Letters

aov.density <- aov(density ~ year, data = snow.ti2)
summary(aov.density)
TukeyHSD(aov.density)
density.pairs <- glht(aov.density, linfct = mcp(year = "Tukey"))
density.cld <- cld(density.pairs)
density.letters <- density.cld$mcletters$Letters

aov.swe <- aov(swe ~ year, data = snow.ti2)
summary(aov.swe)
TukeyHSD(aov.swe)
swe.pairs <- glht(aov.swe, linfct = mcp(year = "Tukey"))
swe.cld <- cld(swe.pairs)
swe.letters <- swe.cld$mcletters$Letters

aov.htc <- aov(htc ~ year, data = snow.ti2)
summary(aov.htc)
TukeyHSD(aov.htc)
htc.pairs <- glht(aov.htc, linfct = mcp(year = "Tukey"))
htc.cld <- cld(htc.pairs)
htc.letters <- htc.cld$mcletters$Letters

depth.bp <- boxplot(depth ~ year, data = snow.ti,
        col = c("royalblue4","lightblue1","royalblue2","slateblue"),
        ylim = c(0,400), ylab = '')
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
depth.y <- t(aggregate(snow.ti$depth, by=list(snow.ti$year), FUN=max)[2])
depth.x <- c(1,2,3,4)
text(x=depth.x,y=depth.y*1.10,depth.letters)

boxplot(density ~ year, data = snow.ti,
        col = c("royalblue4","lightblue1","royalblue2","slateblue"),
        ylim = c(0,700), ylab = '')
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
density.y <- t(aggregate(snow.ti$density, by=list(snow.ti$year), FUN=max)[2])
density.x <- c(1,2,3,4)
text(x=density.x,y=density.y*1.10,density.letters)

boxplot(swe ~ year, data = snow.ti,
        col = c("royalblue4","lightblue1","royalblue2","slateblue"),
        ylim = c(0,1400), ylab = '')
mtext(expression(paste("SWE (mm)")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
swe.y <- t(aggregate(snow.ti$swe, by=list(snow.ti$year), FUN=max)[2])
swe.x <- c(1,2,3,4)
text(x=swe.x,y=swe.y*1.10,swe.letters)

boxplot(htc ~ year, data = snow.ti,
        col = c("royalblue4","lightblue1","royalblue2","slateblue"),
        ylim = c(0,10), ylab = '')
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
htc.y <- t(aggregate(snow.ti$htc, by=list(snow.ti$year), FUN=max)[2])
htc.x <- c(1,2,3,4)
text(x=htc.x,y=htc.y*1.10,htc.letters)











boxplot(depth ~ site*zone, data = ti.2015,
        col = c("darkgreen","green","yellow"),
        ylim = c(0,50), ylab = "Depth (cm)")

aov.depth <- aov(depth ~ year, data = snow.cores.all)
summary(aov.depth)
TukeyHSD(aov.depth)
depth.pairs <- glht(aov.depth, linfct = mcp(year = "Tukey"))
depth.cld <- cld(depth.pairs)
depth.letters <- depth.cld$mcletters$Letters

aov.density <- aov(density ~ zone.year, data = snow.cores.all)
summary(aov.density)
TukeyHSD(aov.density)
density.pairs <- glht(aov.density, linfct = mcp(zone.year = "Tukey"))
density.cld <- cld(density.pairs)
density.letters <- density.cld$mcletters$Letters

aov.swe <- aov(swe ~ zone.year, data = snow.cores.all)
summary(aov.swe)
TukeyHSD(aov.swe)
swe.pairs <- glht(aov.swe, linfct = mcp(zone.year = "Tukey"))
swe.cld <- cld(swe.pairs)
swe.letters <- swe.cld$mcletters$Letters
min(snow.cores.all[snow.cores.all[,"zone.year"]=="2014.aF" , "htc" ]) 

aov.htc <- aov(htc ~ zone.year, data = snow.cores.all)
summary(aov.htc)
TukeyHSD(aov.htc)
htc.pairs <- glht(aov.htc, linfct = mcp(zone.year = "Tukey"))
htc.cld <- cld(htc.pairs)
htc.letters <- htc.cld$mcletters$Letters








### Exported as 8 x 12

par(ps = 12, cex = 1, cex.axis = 1)
par(mfrow = c(2, 2))
par(mar = c(5, 5, 0.5, 1) , oma = c(2, 10, 0.5, 10))

boxplot(depth ~ year, data = snow.cores.all, 
        col = c("darkgreen","green","yellow"),
        ylim = c(0,300), ylab = "Depth (cm)")
#text(x = c(1:12), y = c(244,126,30,196,148,40,149,120,35,260,195,13)+10, 
     #label=depth.cld$mcletters$Letters)

boxplot(density ~ year, data = snow.cores.all, 
        col = c("darkgreen","green","yellow"),
        ylim = c(0,700), ylab = expression("Density" ~ (kg ~ m^{-3})))
#text(x = c(1:12), y = c(405,517,590,418,401,414,399,372,484,514,485,364)+20, 
     #label=density.cld$mcletters$Letters)

boxplot(swe ~ year, data = snow.cores.all, log = "y",
        col = c("darkgreen","green","yellow"),
        ylab = "SWE (mm)")
#text(x = c(1:12), y = c(901,353,90,677,546,134,465,386,78,1335,837,22)+140, 
     #label=swe.cld$mcletters$Letters)

boxplot(htc ~ year, data = snow.cores.all, log = "y",
        col = c("darkgreen","green","yellow"),
        ylab = expression("HTC" ~ (W ~ m^{-2} ~ K^{-1})))
#text(x = c(1:12), y = c(1.3,6.6,9.0,0.8,0.9,6.5,0.9,1.5,7.4,1.3,1.3,10)+4, 
 #    label=htc.cld$mcletters$Letters)






par(cex = 0.75)
par(mfrow = c(2, 2))
par(mar = c(5, 5, 0.5, 1) , oma = c(2, 10, 0.5, 10))
boxplot(depth ~ year + site, data = snow.cores.all[snow.cores.all$site == "ROK", c("year","zone", "depth","density","swe", "htc")]
        , col = c("green", "blue", "purple", "yellow"), ylim = c(0,175), ylab = "Depth (cm)")
boxplot(density ~ year + zone, data = snow.cores.all[snow.cores.all$site == "ROK", c("year","zone", "depth","density","swe", "htc")]
        , col = c("green", "blue", "purple", "yellow"), ylim = c(0,500), ylab = expression("Density" ~ (kg ~ m^{-3})))
boxplot(swe ~ year + zone, data = snow.cores.all[snow.cores.all$site == "ROK", c("year","zone", "depth","density","swe", "htc")]
        , col = c("green", "blue", "purple", "yellow"), ylim = c(0,500), ylab = "SWE (mm)")
boxplot(htc ~ year + zone, data = snow.cores.all[snow.cores.all$site == "ROK", c("year","zone", "depth","density","swe", "htc")]
        , col = c("green", "blue", "purple", "yellow"), ylim = c(0,15), ylab = expression("HTC" ~ (W ~ m^{-2} ~ K^{-1})))

boxplot(depth ~ year + site, data = snow.cores.all)
boxplot(density ~ year + site, data = snow.cores.all)
boxplot(swe ~ year + site, data = snow.cores.all, ylim = c(0,2000))
boxplot(htc ~ year + site, data = snow.cores.all, ylim = c(0,20))

par(mfrow = c(2, 2))
par(ps = 11, cex = 1, cex.axis = 1) # Sets the font size to 11 pts
par(mar = c(2, 4, 0.5, 2), oma = c(2, 2, 0.5, 2))

boxplot(depth ~ site, data = snow.cores.all[snow.cores.all$year == "2015", c("site", "depth","density","swe", "htc")]
        , ylim = c(0,250), ylab = "Depth (cm)")
boxplot(density ~ site, data = snow.cores.all[snow.cores.all$year == "2015", c("site", "depth","density","swe", "htc")]
        , ylim = c(0,750), ylab = expression("Density" ~ (kg ~ m^{-3})))
boxplot(swe ~ site, data = snow.cores.all[snow.cores.all$year == "2015", c("site", "depth","density","swe", "htc")]
        , ylim = c(0,1200), ylab = "SWE (mm)")
boxplot(htc ~ site, data = snow.cores.all[snow.cores.all$year == "2015", c("site", "depth","density","swe", "htc")]
        , ylim = c(0,20), ylab = expression("HTC" ~ (W ~ m^{-2} ~ K^{-1})))


## Analyze treeling data from 2014

treelings <- read.csv(file = "~/Desktop/Workspace/Earthwatch/treelings.csv", header = TRUE)

par(mfrow = c(3, 1))
par(ps = 11, cex = 1, cex.axis = 1) # Sets the font size to 11 pts
par(mar = c(2, 4, 0.5, 2), oma = c(2, 2, 0.5, 2))

boxplot(tpg ~ type, data = treelings)
text(1, 70, "White spruce", font = 2)
boxplot(tpm ~ type, data = treelings, ylim = c(0,20), ylab = "No. of seedlings")
text(1, 17.5, "Black spruce", font = 2)
boxplot(tll ~ type, data = treelings, ylim = c(0,20), xlab = "Site")
text(1, 17.5, "Tamarack", font = 2)

## Analyze needle data from 2013-2014

needles <- read.csv(file = "~/Desktop/Workspace/Earthwatch/needles.csv", header = TRUE)

par(mfrow = c(2, 1))
par(ps = 11, cex = 1, cex.axis = 1) # Sets the font size to 11 pts
par(mar = c(2, 4, 0.5, 2), oma = c(2, 2, 0.5, 2))
boxplot(gmin ~ site, data = needles[needles$year == 2014, c("site", "zone", "tree", "aspect", 
        "height", "viability", "gmin")], ylim = c(0,0.0002), 
        ylab = expression(italic("g")[min] ~ (m ~ s^{-1} ~ 10^{-5})))
boxplot(gmin ~ site, data = needles[needles$year == 2013, c("site", "zone", "tree", "aspect", 
        "height", "viability", "gmin")], ylim = c(0,0.0002), xlab = "Site",
        ylab = expression(italic("g")[min] ~ (m ~ s^{-1} ~ 10^{-5})))

## Export at 7 x 7


gmin.2014 <- needles[needles$year == 2014, c("site", "zone", "tree", "aspect", "height", "viability", "gmin")]
gmin.2013 <- needles[needles$year == 2013, c("site", "zone", "tree", "aspect", "height", "viability", "gmin")]
zone.2014 <- factor(gmin.2014$zone, levels = c("Forest", "Ecotone", "Tree island"))
zone.2013 <- factor(gmin.2013$zone, levels = c("Forest", "Ecotone", "Tree island"))

par(mfrow = c(2, 1))
par(ps = 16, cex = 1, cex.axis = 1) # Sets the font size to 11 pts
par(mar = c(2, 5, 0.5, 2), oma = c(2, 2, 0.5, 2))
boxplot(gmin.2014$gmin ~ zone.2014, ylim = c(0,0.00015), col = c("darkgreen", "green", "yellow"),
        ylab = expression(italic("g")[min] ~ (m ~ s^{-1} ~ 10^{-5})))
text(0.7, 0.00014, label = 2014, font = 2)
boxplot(gmin.2013$gmin ~ zone.2013, ylim = c(0,0.00015), col = c("darkgreen", "green", "yellow"),
        ylab = expression(italic("g")[min] ~ (m ~ s^{-1} ~ 10^{-5})))
text(0.7, 0.00014, label = 2013, font = 2)




boxplot(gmin ~ zone, data = needles, ylim = c(0,0.0002))


########

## Analyzing the G-TREE data in Mac Pass in 2014

gtree.mm <- read.csv(file = "~/Desktop/Workspace/Earthwatch/gtree.csv", header = TRUE)

aov.treat <- aov(germinated ~ treatment, data = gtree.mm)
summary(aov.treat)
TukeyHSD(aov.treat)

aov.germin <- aov(survived ~ treatment, data = gtree.mm)
summary(aov.germin)
TukeyHSD(aov.germin)

