library(multcomp) # Simultaneous Inference in General Parametric Models 
library(vegan)
library(cluster)
library(gclus) # Used for plotting clusters
library(car) # Used for determining variance inflation factors
library(labdsv) # PCA

rm(list = ls())

needles.all <- read.csv(file = "~/Desktop/Workspace/EW/needles.csv", header = TRUE)
n.2008 <- subset(needles.all, year == 2008)
n.2009 <- subset(needles.all, year == 2009)
n.2010 <- subset(needles.all, year == 2010)
n.2013 <- subset(needles.all, year == 2013)
n.2014 <- subset(needles.all, year == 2014)
n.2015 <- subset(needles.all, year == 2015)
needles.all$year <- as.factor(needles.all$year)

tree.island <- subset(needles.all, zone == "Tree island")
forest <- subset(needles.all, zone == "Forest")


par(mfrow = c(3, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 11 pts
par(mar = c(1, 2, 0.5, 1), oma = c(2, 1.5, 0.5, 1))
# x1 <- factor(needles.all$zone, levels = c("Forest","Ecotone","Tree island", "Tundra", "Burn"))
x1 <- factor(n.2013$zone, levels = c("Forest","Ecotone","Tree island", "Tundra", "Burn"))
x2 <- factor(n.2014$zone, levels = c("Forest","Ecotone","Tree island", "Tundra", "Burn"))
x3 <- factor(n.2015$zone, levels = c("Forest","Ecotone","Tree island", "Tundra", "Burn"))
# boxplot(gmin ~ year, data = needles.all, ylim = c(0,0.00018))
boxplot(gmin ~ x1, data = n.2013, ylim = c(0,0.00018), xaxt = "n")
legend("topleft", "2013", bty = "n", inset = c(-0.05,-0.2))
boxplot(gmin ~ x2, data = n.2014, ylim = c(0,0.00018), xaxt = "n")
legend("topleft", "2014", bty = "n", inset = c(-0.05,-0.2))
boxplot(gmin ~ x3, data = n.2015, ylim = c(0,0.00018))
legend("topleft", "2015", bty = "n", inset = c(-0.05,-0.2))
mtext(expression(italic("g")[min] ~ (m ~ s^{-1} ~ 10^{-5})), side = 2, outer = T, line = 0)

needles.micro <- read.csv(file = "~/Desktop/Workspace/EW/needles.microclimate.csv", header = TRUE)
boxplot(jjas ~ year, data = needles.micro)

par(mfrow = c(2, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 11 pts
par(mar = c(2, 4, 0.5, 2), oma = c(2, 3, 0.5, 2))
boxplot(gmin ~ year, data = needles.all, ylim = c(0,0.00018), ylab = expression(italic("g")[min] ~ (m ~ s^{-1} ~ 10^{-5})))
boxplot(jjas ~ year, data = needles.micro, ylab = "Temperature (°C)")
mtext("Year", side = 1, line = 2)


boxplot(gmin ~ year, data = tree.island, ylim = c(0,0.00018))
boxplot(gmin ~ year, data = forest, ylim = c(0,0.00018))
