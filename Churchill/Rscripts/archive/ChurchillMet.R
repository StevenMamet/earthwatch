library(multcomp) # Simultaneous Inference in General Parametric Models 
library(vegan)
library(cluster)
library(gclus) # Used for plotting clusters
library(car) # Used for determining variance inflation factors
library(labdsv) # PCA

rm(list = ls())

## November 2016 - regressions for the parks presentation in December

temps <- read.csv(file = "~/Desktop/Workspace/EW/CH.WNP.Temps.201611.csv", header = TRUE)

summary(lm(rlk8~Row.Labels, data = temps)) # slope = 0.01043
summary(lm(mlk80~Row.Labels, data = temps)) # slope = -0.03931
summary(lm(ppac8~Row.Labels, data = temps)) # slope = 0.03977
summary(lm(ppdc8~Row.Labels, data = temps)) # slope = 0.01268
summary(lm(tun8~Row.Labels, data = temps)) # slope = 0.02115

summary(lm(rlk15~Row.Labels, data = temps[7:17,])) # slope = 0.01963
summary(lm(mlk15~Row.Labels, data = temps[7:17,])) # slope = -0.1303
summary(lm(ppac15~Row.Labels, data = temps[7:17,])) # slope = 0.06949
summary(lm(ppdc15~Row.Labels, data = temps[7:17,])) # slope = 0.07689
summary(lm(tun15~Row.Labels, data = temps[7:17,])) # slope = 0.02683

plot(tun15~Row.Labels, type = "l", ylim = c(-8,-2), data = temps)
par(new=T)
plot(ppac15~Row.Labels, type = "l", col = "blue", ylim = c(-8,-2), data = temps)

abline(coef(lm(ppac15~Row.Labels, data = temps)), col = "blue")
abline(coef(lm(tun15~Row.Labels, data = temps)), col = "black")

## Regressions for gap filling the met station data

mlk <- read.csv(file = "~/Desktop/Workspace/EW/MaryLakeMet.csv", header = TRUE)
rlk <- read.csv(file = "~/Desktop/Workspace/EW/RobergeLakeMet.csv", header = TRUE)
ch.temp <- read.csv(file = "~/Desktop/Workspace/EW/ch_microclimate.csv", header = TRUE)

## See if there are significant trends in the weather data (March 2017)

temps.2017 <- read.csv(file = "~/Desktop/Workspace/EW/ch.temps.2017.csv", header = TRUE)

airp15.lm <- lm(temps.2017$airp15 ~ temps.2017$year)
bsw15.lm <- lm(temps.2017$bsw15 ~ temps.2017$year)
tis15.lm <- lm(temps.2017$tis15 ~ temps.2017$year)
wsu15.lm <- lm(temps.2017$wsu15 ~ temps.2017$year)
fen15.lm <- lm(temps.2017$fen15 ~ temps.2017$year)
bfr15.lm <- lm(temps.2017$bfr15 ~ temps.2017$year)
pfr15.lm <- lm(temps.2017$pfr15 ~ temps.2017$year)
bwp15.lm <- lm(temps.2017$bwp15 ~ temps.2017$year)
ppac15.lm <- lm(temps.2017$ppac15 ~ temps.2017$year)
ppdc15.lm <- lm(temps.2017$ppdc15 ~ temps.2017$year)
tun15.lm <- lm(temps.2017$tun15 ~ temps.2017$year)
summary(airp15.lm)
summary(bsw15.lm)
summary(tis15.lm)
summary(wsu15.lm)
summary(fen15.lm)
summary(bfr15.lm)
summary(pfr15.lm)
summary(bwp15.lm)
summary(ppac15.lm)
summary(ppdc15.lm)
summary(tun15.lm)

airp0.lm <- lm(temps.2017$airp0 ~ temps.2017$year)
bsw0.lm <- lm(temps.2017$bsw0 ~ temps.2017$year)
tis0.lm <- lm(temps.2017$tis0 ~ temps.2017$year)
wsu0.lm <- lm(temps.2017$wsu0 ~ temps.2017$year)
fen0.lm <- lm(temps.2017$fen0 ~ temps.2017$year)
bfr0.lm <- lm(temps.2017$bfr0 ~ temps.2017$year)
pfr0.lm <- lm(temps.2017$pfr0 ~ temps.2017$year)
bwp0.lm <- lm(temps.2017$bwp0 ~ temps.2017$year)
ppac0.lm <- lm(temps.2017$ppac0 ~ temps.2017$year)
ppdc0.lm <- lm(temps.2017$ppdc0 ~ temps.2017$year)
tun0.lm <- lm(temps.2017$tun0 ~ temps.2017$year)
summary(airp0.lm)
summary(bsw0.lm)
summary(tis0.lm)
summary(wsu0.lm) # SIG b1 = -0.116, p = 0.005
summary(fen0.lm)
summary(bfr0.lm)
summary(pfr0.lm)
summary(bwp0.lm)
summary(ppac0.lm)
summary(ppdc0.lm)
summary(tun0.lm)

airp8.lm <- lm(temps.2017$airp8 ~ temps.2017$year)
bsw8.lm <- lm(temps.2017$bsw8 ~ temps.2017$year)
tis8.lm <- lm(temps.2017$tis8 ~ temps.2017$year)
wsu8.lm <- lm(temps.2017$wsu8 ~ temps.2017$year)
fen8.lm <- lm(temps.2017$fen8 ~ temps.2017$year)
bfr8.lm <- lm(temps.2017$bfr8 ~ temps.2017$year)
pfr8.lm <- lm(temps.2017$pfr8 ~ temps.2017$year)
bwp8.lm <- lm(temps.2017$bwp8 ~ temps.2017$year)
ppac8.lm <- lm(temps.2017$ppac8 ~ temps.2017$year)
ppdc8.lm <- lm(temps.2017$ppdc8 ~ temps.2017$year)
tun8.lm <- lm(temps.2017$tun8 ~ temps.2017$year)
summary(airp8.lm) # SIG, b1 = -0.045, P = 0.031
summary(bsw8.lm)
summary(tis8.lm)
summary(wsu8.lm)
summary(fen8.lm)
summary(bfr8.lm)
summary(pfr8.lm)
summary(bwp8.lm) # SIG, b1 = 0.086, P = 0.015
summary(ppac8.lm)
summary(ppdc8.lm)
summary(tun8.lm)

## Snow data 2017

snow.2017 <- read.csv(file = "~/Desktop/Workspace/EW/snow.2017.csv", header = TRUE)

par(mfrow = c(3, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(2, 2, 1.75, 1), oma = c(1,2,1,1))
# Export at 6 x 8

# AIR
boxplot(snow.2017[snow.2017$site == "AIR","depth"] ~ snow.2017[snow.2017$site == "AIR","date"])
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
boxplot(snow.2017[snow.2017$site == "AIR","density"] ~ snow.2017[snow.2017$site == "AIR","date"])
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
boxplot(log(snow.2017[snow.2017$site == "AIR","htc"]) ~ snow.2017[snow.2017$site == "AIR","date"])
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)

# BSW
boxplot(snow.2017[snow.2017$site == "BSW","depth"] ~ snow.2017[snow.2017$site == "BSW","date"])
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
boxplot(snow.2017[snow.2017$site == "BSW","density"] ~ snow.2017[snow.2017$site == "BSW","date"])
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
boxplot(log(snow.2017[snow.2017$site == "BSW","htc"]) ~ snow.2017[snow.2017$site == "BSW","date"])
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)

# WSU
boxplot(snow.2017[snow.2017$site == "WSU","depth"] ~ snow.2017[snow.2017$site == "WSU","date"])
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
boxplot(snow.2017[snow.2017$site == "WSU","density"] ~ snow.2017[snow.2017$site == "WSU","date"])
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
boxplot(log(snow.2017[snow.2017$site == "WSU","htc"]) ~ snow.2017[snow.2017$site == "WSU","date"])
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)

# TIS
boxplot(snow.2017[snow.2017$site == "TIS","depth"] ~ snow.2017[snow.2017$site == "TIS","date"])
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
boxplot(snow.2017[snow.2017$site == "TIS","density"] ~ snow.2017[snow.2017$site == "TIS","date"])
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
boxplot(log(snow.2017[snow.2017$site == "TIS","htc"]) ~ snow.2017[snow.2017$site == "TIS","date"])
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)

# FEN
boxplot(snow.2017[snow.2017$site == "FEN","depth"] ~ snow.2017[snow.2017$site == "FEN","date"])
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
boxplot(snow.2017[snow.2017$site == "FEN","density"] ~ snow.2017[snow.2017$site == "FEN","date"])
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
boxplot(log(snow.2017[snow.2017$site == "FEN","htc"]) ~ snow.2017[snow.2017$site == "FEN","date"])
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)

# BFR
boxplot(snow.2017[snow.2017$site == "BFR","depth"] ~ snow.2017[snow.2017$site == "BFR","date"])
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
boxplot(snow.2017[snow.2017$site == "BFR","density"] ~ snow.2017[snow.2017$site == "BFR","date"])
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
boxplot(log(snow.2017[snow.2017$site == "BFR","htc"]) ~ snow.2017[snow.2017$site == "BFR","date"])
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)

# PFR
boxplot(snow.2017[snow.2017$site == "PFR","depth"] ~ snow.2017[snow.2017$site == "PFR","date"])
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
boxplot(snow.2017[snow.2017$site == "PFR","density"] ~ snow.2017[snow.2017$site == "PFR","date"])
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
boxplot(log(snow.2017[snow.2017$site == "PFR","htc"]) ~ snow.2017[snow.2017$site == "PFR","date"])
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)

# PPA
boxplot(snow.2017[snow.2017$site == "PPA","depth"] ~ snow.2017[snow.2017$site == "PPA","date"])
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
boxplot(snow.2017[snow.2017$site == "PPA","density"] ~ snow.2017[snow.2017$site == "PPA","date"])
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
boxplot(log(snow.2017[snow.2017$site == "PPA","htc"]) ~ snow.2017[snow.2017$site == "PPA","date"])
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)

# PPD
boxplot(snow.2017[snow.2017$site == "PPD","depth"] ~ snow.2017[snow.2017$site == "PPD","date"])
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
boxplot(snow.2017[snow.2017$site == "PPD","density"] ~ snow.2017[snow.2017$site == "PPD","date"])
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
boxplot(log(snow.2017[snow.2017$site == "PPD","htc"]) ~ snow.2017[snow.2017$site == "PPD","date"])
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)

# TUN
boxplot(snow.2017[snow.2017$site == "TUN","depth"] ~ snow.2017[snow.2017$site == "TUN","date"])
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
boxplot(snow.2017[snow.2017$site == "TUN","density"] ~ snow.2017[snow.2017$site == "TUN","date"])
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
boxplot(log(snow.2017[snow.2017$site == "TUN","htc"]) ~ snow.2017[snow.2017$site == "TUN","date"])
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)


# BWP
boxplot(snow.2017[snow.2017$site == "BWP","depth"] ~ snow.2017[snow.2017$site == "BWP","date"])
mtext(expression(paste("Depth (cm)")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
boxplot(snow.2017[snow.2017$site == "BWP","density"] ~ snow.2017[snow.2017$site == "BWP","date"])
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)
boxplot(log(snow.2017[snow.2017$site == "BWP","htc"]) ~ snow.2017[snow.2017$site == "BWP","date"])
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2)
mtext(expression(paste("Year")), side=1, line=2)




# Regress Mary Lake air temperatures against TUN air temperatures
mlk.lm <- lm(mlk[mlk$month==1, 6] ~ mlk[mlk$month==1, 9]) # Jan
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==2, 6] ~ mlk[mlk$month==2, 9]) # Feb
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==3, 6] ~ mlk[mlk$month==3, 9]) # Mar
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==4, 6] ~ mlk[mlk$month==4, 9]) # Apr
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==5, 6] ~ mlk[mlk$month==5, 9]) # May
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==6, 6] ~ mlk[mlk$month==6, 9]) # Jun
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==7, 6] ~ mlk[mlk$month==7, 9]) # Jul
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==8, 6] ~ mlk[mlk$month==8, 9]) # Aug
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==9, 6] ~ mlk[mlk$month==9, 9]) # Sep
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==10, 6] ~ mlk[mlk$month==10, 9]) # Oct
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==11, 6] ~ mlk[mlk$month==11, 9]) # Nov
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==12, 6] ~ mlk[mlk$month==12, 9]) # Dec
summary(mlk.lm)

# Regress Mary Lake ground temperatures against gap-filled MLK air temperatures
mlk.lm <- lm(mlk[mlk$month==1, 7] ~ mlk[mlk$month==1, 6]) # Jan
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==2, 7] ~ mlk[mlk$month==2, 6]) # Feb
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==3, 7] ~ mlk[mlk$month==3, 6]) # Mar
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==4, 7] ~ mlk[mlk$month==4, 6]) # Apr
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==5, 7] ~ mlk[mlk$month==5, 6]) # May
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==6, 7] ~ mlk[mlk$month==6, 6]) # Jun
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==7, 7] ~ mlk[mlk$month==7, 6]) # Jul
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==8, 7] ~ mlk[mlk$month==8, 6]) # Aug
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==9, 7] ~ mlk[mlk$month==9, 6]) # Sep
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==10, 7] ~ mlk[mlk$month==10, 6]) # Oct
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==11, 7] ~ mlk[mlk$month==11, 6]) # Nov
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==12, 7] ~ mlk[mlk$month==12, 6]) # Dec
summary(mlk.lm)

# Regress Mary Lake -80cm temperatures against gap-filled MLK ground temperatures
mlk.lm <- lm(mlk[mlk$month==1, 8] ~ mlk[mlk$month==1, 7]) # Jan
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==2, 8] ~ mlk[mlk$month==2, 7]) # Feb
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==3, 8] ~ mlk[mlk$month==3, 7]) # Mar
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==4, 8] ~ mlk[mlk$month==4, 7]) # Apr
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==5, 8] ~ mlk[mlk$month==5, 7]) # May
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==6, 8] ~ mlk[mlk$month==6, 7]) # Jun
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==7, 8] ~ mlk[mlk$month==7, 7]) # Jul
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==8, 8] ~ mlk[mlk$month==8, 7]) # Aug
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==9, 8] ~ mlk[mlk$month==9, 7]) # Sep
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==10, 8] ~ mlk[mlk$month==10, 7]) # Oct
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==11, 8] ~ mlk[mlk$month==11, 7]) # Nov
summary(mlk.lm)
mlk.lm <- lm(mlk[mlk$month==12, 8] ~ mlk[mlk$month==12, 7]) # Dec
summary(mlk.lm)

##################
##################

# Regress Roberge Lake air temperatures against TUN air temperatures
rlk.lm <- lm(rlk[rlk$month==1, 6] ~ rlk[rlk$month==1, 9]) # Jan
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==2, 6] ~ rlk[rlk$month==2, 9]) # Feb
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==3, 6] ~ rlk[rlk$month==3, 9]) # Mar
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==4, 6] ~ rlk[rlk$month==4, 9]) # Apr
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==5, 6] ~ rlk[rlk$month==5, 9]) # May
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==6, 6] ~ rlk[rlk$month==6, 9]) # Jun
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==7, 6] ~ rlk[rlk$month==7, 9]) # Jul
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==8, 6] ~ rlk[rlk$month==8, 9]) # Aug
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==9, 6] ~ rlk[rlk$month==9, 9]) # Sep
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==10, 6] ~ rlk[rlk$month==10, 9]) # Oct
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==11, 6] ~ rlk[rlk$month==11, 9]) # Nov
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==12, 6] ~ rlk[rlk$month==12, 9]) # Dec
summary(rlk.lm)

# Regress Roberge Lake ground temperatures against gap-filled rlk air temperatures
rlk.lm <- lm(rlk[rlk$month==1, 7] ~ rlk[rlk$month==1, 6]) # Jan
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==2, 7] ~ rlk[rlk$month==2, 6]) # Feb
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==3, 7] ~ rlk[rlk$month==3, 6]) # Mar
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==4, 7] ~ rlk[rlk$month==4, 6]) # Apr
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==5, 7] ~ rlk[rlk$month==5, 6]) # May
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==6, 7] ~ rlk[rlk$month==6, 6]) # Jun
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==7, 7] ~ rlk[rlk$month==7, 6]) # Jul
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==8, 7] ~ rlk[rlk$month==8, 6]) # Aug
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==9, 7] ~ rlk[rlk$month==9, 6]) # Sep
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==10, 7] ~ rlk[rlk$month==10, 6]) # Oct
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==11, 7] ~ rlk[rlk$month==11, 6]) # Nov
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==12, 7] ~ rlk[rlk$month==12, 6]) # Dec
summary(rlk.lm)

# Regress Roberge Lake -80cm temperatures against PPA -80cm temperatures
rlk.lm <- lm(rlk[rlk$month==1, 8] ~ rlk[rlk$month==1, 11]) # Jan
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==2, 8] ~ rlk[rlk$month==2, 11]) # Feb
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==3, 8] ~ rlk[rlk$month==3, 11]) # Mar
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==4, 8] ~ rlk[rlk$month==4, 11]) # Apr
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==5, 8] ~ rlk[rlk$month==5, 11]) # May
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==6, 8] ~ rlk[rlk$month==6, 11]) # Jun
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==7, 8] ~ rlk[rlk$month==7, 11]) # Jul
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==8, 8] ~ rlk[rlk$month==8, 11]) # Aug
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==9, 8] ~ rlk[rlk$month==9, 11]) # Sep
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==10, 8] ~ rlk[rlk$month==10, 11]) # Oct
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==11, 8] ~ rlk[rlk$month==11, 11]) # Nov
summary(rlk.lm)
rlk.lm <- lm(rlk[rlk$month==12, 8] ~ rlk[rlk$month==12, 11]) # Dec
summary(rlk.lm)

# Regress AIR air temperatures against TUN air temperatures
air.lm <- lm(ch.temp[ch.temp$month==1, 8] ~ ch.temp[ch.temp$month==1, 40]) # Jan
summary(air.lm)
air.lm <- lm(ch.temp[ch.temp$month==2, 8] ~ ch.temp[ch.temp$month==2, 40]) # Feb
summary(air.lm)
air.lm <- lm(ch.temp[ch.temp$month==3, 8] ~ ch.temp[ch.temp$month==3, 40]) # Mar
summary(air.lm)
air.lm <- lm(ch.temp[ch.temp$month==4, 8] ~ ch.temp[ch.temp$month==4, 40]) # Apr
summary(air.lm)
air.lm <- lm(ch.temp[ch.temp$month==5, 8] ~ ch.temp[ch.temp$month==5, 40]) # May
summary(air.lm)
air.lm <- lm(ch.temp[ch.temp$month==6, 8] ~ ch.temp[ch.temp$month==6, 40]) # Jun
summary(air.lm)
air.lm <- lm(ch.temp[ch.temp$month==7, 8] ~ ch.temp[ch.temp$month==7, 40]) # Jul
summary(air.lm)
air.lm <- lm(ch.temp[ch.temp$month==8, 8] ~ ch.temp[ch.temp$month==8, 40]) # Aug
summary(air.lm)
air.lm <- lm(ch.temp[ch.temp$month==9, 8] ~ ch.temp[ch.temp$month==9, 40]) # Sep
summary(air.lm)
air.lm <- lm(ch.temp[ch.temp$month==10, 8] ~ ch.temp[ch.temp$month==10, 40]) # Oct
summary(air.lm)
air.lm <- lm(ch.temp[ch.temp$month==11, 8] ~ ch.temp[ch.temp$month==11, 40]) # Nov
summary(air.lm)
air.lm <- lm(ch.temp[ch.temp$month==12, 8] ~ ch.temp[ch.temp$month==12, 40]) # Dec
summary(air.lm)

# Regress AIR ground surface temperatures against AIR air temperatures
air.lm <- lm(ch.temp[ch.temp$month==1, 7] ~ ch.temp[ch.temp$month==1, 8]) # Jan
summary(air.lm)
air.lm <- lm(ch.temp[ch.temp$month==2, 7] ~ ch.temp[ch.temp$month==2, 8]) # Feb
summary(air.lm)
air.lm <- lm(ch.temp[ch.temp$month==3, 7] ~ ch.temp[ch.temp$month==3, 8]) # Mar
summary(air.lm)
air.lm <- lm(ch.temp[ch.temp$month==4, 7] ~ ch.temp[ch.temp$month==4, 8]) # Apr
summary(air.lm)
air.lm <- lm(ch.temp[ch.temp$month==5, 7] ~ ch.temp[ch.temp$month==5, 8]) # May
summary(air.lm)
air.lm <- lm(ch.temp[ch.temp$month==6, 7] ~ ch.temp[ch.temp$month==6, 8]) # Jun
summary(air.lm)
air.lm <- lm(ch.temp[ch.temp$month==7, 7] ~ ch.temp[ch.temp$month==7, 8]) # Jul
summary(air.lm)
air.lm <- lm(ch.temp[ch.temp$month==8, 7] ~ ch.temp[ch.temp$month==8, 8]) # Aug
summary(air.lm)
air.lm <- lm(ch.temp[ch.temp$month==9, 7] ~ ch.temp[ch.temp$month==9, 8]) # Sep
summary(air.lm)
air.lm <- lm(ch.temp[ch.temp$month==10, 7] ~ ch.temp[ch.temp$month==10, 8]) # Oct
summary(air.lm)
air.lm <- lm(ch.temp[ch.temp$month==11, 7] ~ ch.temp[ch.temp$month==11, 8]) # Nov
summary(air.lm)
air.lm <- lm(ch.temp[ch.temp$month==12, 7] ~ ch.temp[ch.temp$month==12, 8]) # Dec
summary(air.lm)

# Regress AIR -80 temperatures against AIR and PPA -80 temperatures
air.lm <- lm(ch.temp[ch.temp$month==6, 9] ~ ch.temp[ch.temp$month==6, 33]) # Jun
summary(air.lm)
air.lm <- lm(ch.temp[ch.temp$month==9, 9] ~ ch.temp[ch.temp$month==9, 7]) # Sep
summary(air.lm)

# Regress TIS -80 temperatures against TIS ground surface temperatures
tis.lm <- lm(ch.temp[ch.temp$month==1, 15] ~ ch.temp[ch.temp$month==1, 13]) # Jan
summary(tis.lm)
tis.lm <- lm(ch.temp[ch.temp$month==2, 15] ~ ch.temp[ch.temp$month==2, 13]) # Feb
summary(tis.lm)
tis.lm <- lm(ch.temp[ch.temp$month==3, 15] ~ ch.temp[ch.temp$month==3, 13]) # Mar
summary(tis.lm)
tis.lm <- lm(ch.temp[ch.temp$month==4, 15] ~ ch.temp[ch.temp$month==4, 13]) # Apr
summary(tis.lm)
tis.lm <- lm(ch.temp[ch.temp$month==5, 15] ~ ch.temp[ch.temp$month==5, 13]) # May
summary(tis.lm)
tis.lm <- lm(ch.temp[ch.temp$month==6, 15] ~ ch.temp[ch.temp$month==6, 13]) # Jun
summary(tis.lm)
tis.lm <- lm(ch.temp[ch.temp$month==7, 15] ~ ch.temp[ch.temp$month==7, 13]) # Jul
summary(tis.lm)
tis.lm <- lm(ch.temp[ch.temp$month==8, 15] ~ ch.temp[ch.temp$month==8, 13]) # Aug
summary(tis.lm)
tis.lm <- lm(ch.temp[ch.temp$month==9, 15] ~ ch.temp[ch.temp$month==9, 13]) # Sep
summary(tis.lm)
tis.lm <- lm(ch.temp[ch.temp$month==10, 15] ~ ch.temp[ch.temp$month==10, 13]) # Oct
summary(tis.lm)
tis.lm <- lm(ch.temp[ch.temp$month==11, 15] ~ ch.temp[ch.temp$month==11, 13]) # Nov
summary(tis.lm)
tis.lm <- lm(ch.temp[ch.temp$month==12, 15] ~ ch.temp[ch.temp$month==12, 13]) # Dec
summary(tis.lm)

# Regress FEN air temperatures against TUN air temperatures
fen.lm <- lm(ch.temp[ch.temp$month==1, 20] ~ ch.temp[ch.temp$month==1, 40]) # Jan
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==2, 20] ~ ch.temp[ch.temp$month==2, 40]) # Feb
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==3, 20] ~ ch.temp[ch.temp$month==3, 40]) # Mar
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==4, 20] ~ ch.temp[ch.temp$month==4, 40]) # Apr
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==5, 20] ~ ch.temp[ch.temp$month==5, 40]) # May
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==6, 20] ~ ch.temp[ch.temp$month==6, 40]) # Jun
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==7, 20] ~ ch.temp[ch.temp$month==7, 40]) # Jul
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==8, 20] ~ ch.temp[ch.temp$month==8, 40]) # Aug
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==9, 20] ~ ch.temp[ch.temp$month==9, 40]) # Sep
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==10, 20] ~ ch.temp[ch.temp$month==10, 40]) # Oct
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==11, 20] ~ ch.temp[ch.temp$month==11, 40]) # Nov
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==12, 20] ~ ch.temp[ch.temp$month==12, 40]) # Dec
summary(fen.lm)




# Regress FEN ground surface temperatures against FEN air temperatures
fen.lm <- lm(ch.temp[ch.temp$month==1, 19] ~ ch.temp[ch.temp$month==1, 20]) # Jan
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==2, 19] ~ ch.temp[ch.temp$month==2, 20]) # Feb
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==3, 19] ~ ch.temp[ch.temp$month==3, 20]) # Mar
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==4, 19] ~ ch.temp[ch.temp$month==4, 20]) # Apr
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==5, 19] ~ ch.temp[ch.temp$month==5, 20]) # May
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==6, 19] ~ ch.temp[ch.temp$month==6, 20]) # Jun
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==7, 19] ~ ch.temp[ch.temp$month==7, 20]) # Jul
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==8, 19] ~ ch.temp[ch.temp$month==8, 20]) # Aug
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==9, 19] ~ ch.temp[ch.temp$month==9, 20]) # Sep
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==10, 19] ~ ch.temp[ch.temp$month==10, 20]) # Oct
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==11, 19] ~ ch.temp[ch.temp$month==11, 20]) # Nov
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==12, 19] ~ ch.temp[ch.temp$month==12, 20]) # Dec
summary(fen.lm)

# Regress FEN -80 temperatures against FEN ground surface temperatures
fen.lm <- lm(ch.temp[ch.temp$month==1, 21] ~ ch.temp[ch.temp$month==1, 19]) # Jan
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==2, 21] ~ ch.temp[ch.temp$month==2, 19]) # Feb
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==3, 21] ~ ch.temp[ch.temp$month==3, 19]) # Mar
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==4, 21] ~ ch.temp[ch.temp$month==4, 19]) # Apr
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==5, 21] ~ ch.temp[ch.temp$month==5, 19]) # May
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==6, 21] ~ ch.temp[ch.temp$month==6, 19]) # Jun
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==7, 21] ~ ch.temp[ch.temp$month==7, 19]) # Jul
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==8, 21] ~ ch.temp[ch.temp$month==8, 19]) # Aug
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==9, 21] ~ ch.temp[ch.temp$month==9, 19]) # Sep
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==10, 21] ~ ch.temp[ch.temp$month==10, 19]) # Oct
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==11, 21] ~ ch.temp[ch.temp$month==11, 19]) # Nov
summary(fen.lm)
fen.lm <- lm(ch.temp[ch.temp$month==12, 21] ~ ch.temp[ch.temp$month==12, 19]) # Dec
summary(fen.lm)

# Regress BFR air temperatures against BSW air temperatures
bfr.lm <- lm(ch.temp[ch.temp$month==1, 23] ~ ch.temp[ch.temp$month==1, 11]) # Jan
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==2, 23] ~ ch.temp[ch.temp$month==2, 11]) # Feb
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==3, 23] ~ ch.temp[ch.temp$month==3, 11]) # Mar
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==4, 23] ~ ch.temp[ch.temp$month==4, 11]) # Apr
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==5, 23] ~ ch.temp[ch.temp$month==5, 11]) # May
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==6, 23] ~ ch.temp[ch.temp$month==6, 11]) # Jun
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==7, 23] ~ ch.temp[ch.temp$month==7, 11]) # Jul
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==8, 23] ~ ch.temp[ch.temp$month==8, 11]) # Aug
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==9, 23] ~ ch.temp[ch.temp$month==9, 11]) # Sep
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==10, 23] ~ ch.temp[ch.temp$month==10, 11]) # Oct
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==11, 23] ~ ch.temp[ch.temp$month==11, 11]) # Nov
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==12, 23] ~ ch.temp[ch.temp$month==12, 11]) # Dec
summary(bfr.lm)

# Regress BFR ground surface temperatures against BFR air temperatures
bfr.lm <- lm(ch.temp[ch.temp$month==1, 22] ~ ch.temp[ch.temp$month==1, 23]) # Jan
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==2, 22] ~ ch.temp[ch.temp$month==2, 23]) # Feb
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==3, 22] ~ ch.temp[ch.temp$month==3, 23]) # Mar
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==4, 22] ~ ch.temp[ch.temp$month==4, 23]) # Apr
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==5, 22] ~ ch.temp[ch.temp$month==5, 23]) # May
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==6, 22] ~ ch.temp[ch.temp$month==6, 23]) # Jun
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==7, 22] ~ ch.temp[ch.temp$month==7, 23]) # Jul
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==8, 22] ~ ch.temp[ch.temp$month==8, 23]) # Aug
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==9, 22] ~ ch.temp[ch.temp$month==9, 23]) # Sep
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==10, 22] ~ ch.temp[ch.temp$month==10, 23]) # Oct
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==11, 22] ~ ch.temp[ch.temp$month==11, 23]) # Nov
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==12, 22] ~ ch.temp[ch.temp$month==12, 23]) # Dec
summary(bfr.lm)

# Regress BFR ground surface temperatures against BFR air temperatures
bfr.lm <- lm(ch.temp[ch.temp$month==1, 24] ~ ch.temp[ch.temp$month==1, 22]) # Jan
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==2, 24] ~ ch.temp[ch.temp$month==2, 22]) # Feb
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==3, 24] ~ ch.temp[ch.temp$month==3, 22]) # Mar
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==4, 24] ~ ch.temp[ch.temp$month==4, 22]) # Apr
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==5, 24] ~ ch.temp[ch.temp$month==5, 22]) # May
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==6, 24] ~ ch.temp[ch.temp$month==6, 22]) # Jun
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==7, 24] ~ ch.temp[ch.temp$month==7, 22]) # Jul
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==8, 24] ~ ch.temp[ch.temp$month==8, 22]) # Aug
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==9, 24] ~ ch.temp[ch.temp$month==9, 22]) # Sep
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==10, 24] ~ ch.temp[ch.temp$month==10, 22]) # Oct
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==11, 24] ~ ch.temp[ch.temp$month==11, 22]) # Nov
summary(bfr.lm)
bfr.lm <- lm(ch.temp[ch.temp$month==12, 24] ~ ch.temp[ch.temp$month==12, 22]) # Dec
summary(bfr.lm)

# Regress ppd ground surface temperatures against ppd air temperatures
ppd.lm <- lm(ch.temp[ch.temp$month==1, 36] ~ ch.temp[ch.temp$month==1, 32]) # Jan
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==2, 36] ~ ch.temp[ch.temp$month==2, 32]) # Feb
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==3, 36] ~ ch.temp[ch.temp$month==3, 32]) # Mar
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==4, 36] ~ ch.temp[ch.temp$month==4, 32]) # Apr
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==5, 36] ~ ch.temp[ch.temp$month==5, 32]) # May
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==6, 36] ~ ch.temp[ch.temp$month==6, 32]) # Jun
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==7, 36] ~ ch.temp[ch.temp$month==7, 32]) # Jul
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==8, 36] ~ ch.temp[ch.temp$month==8, 32]) # Aug
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==9, 36] ~ ch.temp[ch.temp$month==9, 32]) # Sep
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==10, 36] ~ ch.temp[ch.temp$month==10, 32]) # Oct
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==11, 36] ~ ch.temp[ch.temp$month==11, 32]) # Nov
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==12, 36] ~ ch.temp[ch.temp$month==12, 32]) # Dec
summary(ppd.lm)

# Regress ppd ground surface temperatures against ppd air temperatures
ppd.lm <- lm(ch.temp[ch.temp$month==1, 35] ~ ch.temp[ch.temp$month==1, 36]) # Jan
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==2, 35] ~ ch.temp[ch.temp$month==2, 36]) # Feb
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==3, 35] ~ ch.temp[ch.temp$month==3, 36]) # Mar
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==4, 35] ~ ch.temp[ch.temp$month==4, 36]) # Apr
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==5, 35] ~ ch.temp[ch.temp$month==5, 36]) # May
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==6, 35] ~ ch.temp[ch.temp$month==6, 36]) # Jun
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==7, 35] ~ ch.temp[ch.temp$month==7, 36]) # Jul
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==8, 35] ~ ch.temp[ch.temp$month==8, 36]) # Aug
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==9, 35] ~ ch.temp[ch.temp$month==9, 36]) # Sep
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==10, 35] ~ ch.temp[ch.temp$month==10, 36]) # Oct
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==11, 35] ~ ch.temp[ch.temp$month==11, 36]) # Nov
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==12, 35] ~ ch.temp[ch.temp$month==12, 36]) # Dec
summary(ppd.lm)

# Regress ppd -80 temperatures against ppa -80 temperatures
ppd.lm <- lm(ch.temp[ch.temp$month==1, 37] ~ ch.temp[ch.temp$month==1, 33]) # Jan
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==2, 37] ~ ch.temp[ch.temp$month==2, 33]) # Feb
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==3, 37] ~ ch.temp[ch.temp$month==3, 33]) # Mar
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==4, 37] ~ ch.temp[ch.temp$month==4, 33]) # Apr
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==5, 37] ~ ch.temp[ch.temp$month==5, 33]) # May
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==6, 37] ~ ch.temp[ch.temp$month==6, 33]) # Jun
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==7, 37] ~ ch.temp[ch.temp$month==7, 33]) # Jul
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==8, 37] ~ ch.temp[ch.temp$month==8, 33]) # Aug
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==9, 37] ~ ch.temp[ch.temp$month==9, 33]) # Sep
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==10, 37] ~ ch.temp[ch.temp$month==10, 33]) # Oct
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==11, 37] ~ ch.temp[ch.temp$month==11, 33]) # Nov
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==12, 37] ~ ch.temp[ch.temp$month==12, 33]) # Dec
summary(ppd.lm)

# Regress ppd wedge 0 temperatures against ppd centre 0 temperatures
ppd.lm <- lm(ch.temp[ch.temp$month==1, 38] ~ ch.temp[ch.temp$month==1, 34]) # Jan
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==2, 38] ~ ch.temp[ch.temp$month==2, 34]) # Feb
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==3, 38] ~ ch.temp[ch.temp$month==3, 34]) # Mar
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==4, 38] ~ ch.temp[ch.temp$month==4, 34]) # Apr
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==5, 38] ~ ch.temp[ch.temp$month==5, 34]) # May
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==6, 38] ~ ch.temp[ch.temp$month==6, 34]) # Jun
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==7, 38] ~ ch.temp[ch.temp$month==7, 34]) # Jul
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==8, 38] ~ ch.temp[ch.temp$month==8, 34]) # Aug
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==9, 38] ~ ch.temp[ch.temp$month==9, 34]) # Sep
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==10, 38] ~ ch.temp[ch.temp$month==10, 34]) # Oct
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==11, 38] ~ ch.temp[ch.temp$month==11, 34]) # Nov
summary(ppd.lm)
ppd.lm <- lm(ch.temp[ch.temp$month==12, 38] ~ ch.temp[ch.temp$month==12, 34]) # Dec
summary(ppd.lm)

# Regress tun -300 temperatures against tun centre -80 temperatures
tun.lm <- lm(ch.temp[ch.temp$month==1, 42] ~ ch.temp[ch.temp$month==1, 41]) # Jan
summary(tun.lm)
tun.lm <- lm(ch.temp[ch.temp$month==2, 42] ~ ch.temp[ch.temp$month==2, 41]) # Feb
summary(tun.lm)
tun.lm <- lm(ch.temp[ch.temp$month==3, 42] ~ ch.temp[ch.temp$month==3, 41]) # Mar
summary(tun.lm)
tun.lm <- lm(ch.temp[ch.temp$month==4, 42] ~ ch.temp[ch.temp$month==4, 41]) # Apr
summary(tun.lm)
tun.lm <- lm(ch.temp[ch.temp$month==5, 42] ~ ch.temp[ch.temp$month==5, 41]) # May
summary(tun.lm)
tun.lm <- lm(ch.temp[ch.temp$month==6, 42] ~ ch.temp[ch.temp$month==6, 41]) # Jun
summary(tun.lm)
tun.lm <- lm(ch.temp[ch.temp$month==7, 42] ~ ch.temp[ch.temp$month==7, 41]) # Jul
summary(tun.lm)
tun.lm <- lm(ch.temp[ch.temp$month==8, 42] ~ ch.temp[ch.temp$month==8, 41]) # Aug
summary(tun.lm)
tun.lm <- lm(ch.temp[ch.temp$month==9, 42] ~ ch.temp[ch.temp$month==9, 41]) # Sep
summary(tun.lm)
tun.lm <- lm(ch.temp[ch.temp$month==10, 42] ~ ch.temp[ch.temp$month==10, 41]) # Oct
summary(tun.lm)
tun.lm <- lm(ch.temp[ch.temp$month==11, 42] ~ ch.temp[ch.temp$month==11, 41]) # Nov
summary(tun.lm)
tun.lm <- lm(ch.temp[ch.temp$month==12, 42] ~ ch.temp[ch.temp$month==12, 41]) # Dec
summary(tun.lm)

# Regress TUN air temperatures against BWP air temperatures
tun.lm1 <- lm(ch.temp[ch.temp$month==1, 43] ~ ch.temp[ch.temp$month==1, 21]) # Jan
summary(tun.lm1)
tun.lm2 <- lm(ch.temp[ch.temp$month==2, 43] ~ ch.temp[ch.temp$month==2, 21]) # Feb
summary(tun.lm2)
tun.lm3 <- lm(ch.temp[ch.temp$month==3, 43] ~ ch.temp[ch.temp$month==3, 21]) # Mar
summary(tun.lm3)
tun.lm4 <- lm(ch.temp[ch.temp$month==4, 43] ~ ch.temp[ch.temp$month==4, 21]) # Apr
summary(tun.lm4)
tun.lm5 <- lm(ch.temp[ch.temp$month==5, 43] ~ ch.temp[ch.temp$month==5, 21]) # May
summary(tun.lm5)
tun.lm6 <- lm(ch.temp[ch.temp$month==6, 43] ~ ch.temp[ch.temp$month==6, 21]) # Jun
summary(tun.lm6)
tun.lm7 <- lm(ch.temp[ch.temp$month==7, 43] ~ ch.temp[ch.temp$month==7, 21]) # Jul
summary(tun.lm7)
tun.lm8 <- lm(ch.temp[ch.temp$month==8, 43] ~ ch.temp[ch.temp$month==8, 21]) # Aug
summary(tun.lm8)
tun.lm9 <- lm(ch.temp[ch.temp$month==9, 43] ~ ch.temp[ch.temp$month==9, 21]) # Sep
summary(tun.lm9)
tun.lm10 <- lm(ch.temp[ch.temp$month==10, 43] ~ ch.temp[ch.temp$month==10, 21]) # Oct
summary(tun.lm10)
tun.lm11 <- lm(ch.temp[ch.temp$month==11, 43] ~ ch.temp[ch.temp$month==11, 21]) # Nov
summary(tun.lm11)
tun.lm12 <- lm(ch.temp[ch.temp$month==12, 43] ~ ch.temp[ch.temp$month==12, 21]) # Dec
summary(tun.lm12)

shit <- cbind(coef(tun.lm1),coef(tun.lm2),coef(tun.lm3),coef(tun.lm4),coef(tun.lm5),coef(tun.lm6),
        coef(tun.lm7),coef(tun.lm8),coef(tun.lm9),coef(tun.lm10),coef(tun.lm11),coef(tun.lm12))

# Regress TUN ground surface temperatures against TUN air temperatures
tun.lm1 <- lm(ch.temp[ch.temp$month==1, 42] ~ ch.temp[ch.temp$month==1, 43]) # Jan
summary(tun.lm1)
tun.lm2 <- lm(ch.temp[ch.temp$month==2, 42] ~ ch.temp[ch.temp$month==2, 43]) # Feb
summary(tun.lm2)
tun.lm3 <- lm(ch.temp[ch.temp$month==3, 42] ~ ch.temp[ch.temp$month==3, 43]) # Mar
summary(tun.lm3)
tun.lm4 <- lm(ch.temp[ch.temp$month==4, 42] ~ ch.temp[ch.temp$month==4, 43]) # Apr
summary(tun.lm4)
tun.lm5 <- lm(ch.temp[ch.temp$month==5, 42] ~ ch.temp[ch.temp$month==5, 43]) # May
summary(tun.lm5)
tun.lm6 <- lm(ch.temp[ch.temp$month==6, 42] ~ ch.temp[ch.temp$month==6, 43]) # Jun
summary(tun.lm6)
tun.lm7 <- lm(ch.temp[ch.temp$month==7, 42] ~ ch.temp[ch.temp$month==7, 43]) # Jul
summary(tun.lm7)
tun.lm8 <- lm(ch.temp[ch.temp$month==8, 42] ~ ch.temp[ch.temp$month==8, 43]) # Aug
summary(tun.lm8)
tun.lm9 <- lm(ch.temp[ch.temp$month==9, 42] ~ ch.temp[ch.temp$month==9, 43]) # Sep
summary(tun.lm9)
tun.lm10 <- lm(ch.temp[ch.temp$month==10, 42] ~ ch.temp[ch.temp$month==10, 43]) # Oct
summary(tun.lm10)
tun.lm11 <- lm(ch.temp[ch.temp$month==11, 42] ~ ch.temp[ch.temp$month==11, 43]) # Nov
summary(tun.lm11)
tun.lm12 <- lm(ch.temp[ch.temp$month==12, 42] ~ ch.temp[ch.temp$month==12, 43]) # Dec
summary(tun.lm12)

shit <- cbind(coef(tun.lm1),coef(tun.lm2),coef(tun.lm3),coef(tun.lm4),coef(tun.lm5),coef(tun.lm6),
              coef(tun.lm7),coef(tun.lm8),coef(tun.lm9),coef(tun.lm10),coef(tun.lm11),coef(tun.lm12))

# Regress TUN subsurface temperatures against BWP subsurface temperatures
tun.lm1 <- lm(ch.temp[ch.temp$month==1, 44] ~ ch.temp[ch.temp$month==1, 33]) # Jan
summary(tun.lm1)
tun.lm2 <- lm(ch.temp[ch.temp$month==2, 44] ~ ch.temp[ch.temp$month==2, 33]) # Feb
summary(tun.lm2)
tun.lm3 <- lm(ch.temp[ch.temp$month==3, 44] ~ ch.temp[ch.temp$month==3, 33]) # Mar
summary(tun.lm3)
tun.lm4 <- lm(ch.temp[ch.temp$month==4, 44] ~ ch.temp[ch.temp$month==4, 33]) # Apr
summary(tun.lm4)
tun.lm5 <- lm(ch.temp[ch.temp$month==5, 44] ~ ch.temp[ch.temp$month==5, 33]) # May
summary(tun.lm5)
tun.lm6 <- lm(ch.temp[ch.temp$month==6, 44] ~ ch.temp[ch.temp$month==6, 33]) # Jun
summary(tun.lm6)
tun.lm7 <- lm(ch.temp[ch.temp$month==7, 44] ~ ch.temp[ch.temp$month==7, 33]) # Jul
summary(tun.lm7)
tun.lm8 <- lm(ch.temp[ch.temp$month==8, 44] ~ ch.temp[ch.temp$month==8, 33]) # Aug
summary(tun.lm8)
tun.lm9 <- lm(ch.temp[ch.temp$month==9, 44] ~ ch.temp[ch.temp$month==9, 33]) # Sep
summary(tun.lm9)
tun.lm10 <- lm(ch.temp[ch.temp$month==10, 44] ~ ch.temp[ch.temp$month==10, 33]) # Oct
summary(tun.lm10)
tun.lm11 <- lm(ch.temp[ch.temp$month==11, 44] ~ ch.temp[ch.temp$month==11, 33]) # Nov
summary(tun.lm11)
tun.lm12 <- lm(ch.temp[ch.temp$month==12, 44] ~ ch.temp[ch.temp$month==12, 33]) # Dec
summary(tun.lm12)

shit <- cbind(coef(tun.lm1),coef(tun.lm2),coef(tun.lm3),coef(tun.lm4),coef(tun.lm5),coef(tun.lm6),
              coef(tun.lm7),coef(tun.lm8),coef(tun.lm9),coef(tun.lm10),coef(tun.lm11),coef(tun.lm12))

# Regress BWP subsurface temperatures against TUN subsurface temperatures
bwp.lm1 <- lm(ch.temp[ch.temp$month==1, 33] ~ ch.temp[ch.temp$month==1, 44]) # Jan
summary(bwp.lm1)
bwp.lm2 <- lm(ch.temp[ch.temp$month==2, 33] ~ ch.temp[ch.temp$month==2, 44]) # Feb
summary(bwp.lm2)
bwp.lm3 <- lm(ch.temp[ch.temp$month==3, 33] ~ ch.temp[ch.temp$month==3, 44]) # Mar
summary(bwp.lm3)
bwp.lm4 <- lm(ch.temp[ch.temp$month==4, 33] ~ ch.temp[ch.temp$month==4, 44]) # Apr
summary(bwp.lm4)
bwp.lm5 <- lm(ch.temp[ch.temp$month==5, 33] ~ ch.temp[ch.temp$month==5, 44]) # May
summary(bwp.lm5)
bwp.lm6 <- lm(ch.temp[ch.temp$month==6, 33] ~ ch.temp[ch.temp$month==6, 44]) # Jun
summary(bwp.lm6)
bwp.lm7 <- lm(ch.temp[ch.temp$month==7, 33] ~ ch.temp[ch.temp$month==7, 44]) # Jul
summary(bwp.lm7)
bwp.lm8 <- lm(ch.temp[ch.temp$month==8, 33] ~ ch.temp[ch.temp$month==8, 44]) # Aug
summary(bwp.lm8)
bwp.lm9 <- lm(ch.temp[ch.temp$month==9, 33] ~ ch.temp[ch.temp$month==9, 44]) # Sep
summary(bwp.lm9)
bwp.lm10 <- lm(ch.temp[ch.temp$month==10, 33] ~ ch.temp[ch.temp$month==10, 44]) # Oct
summary(bwp.lm10)
bwp.lm11 <- lm(ch.temp[ch.temp$month==11, 33] ~ ch.temp[ch.temp$month==11, 44]) # Nov
summary(bwp.lm11)
bwp.lm12 <- lm(ch.temp[ch.temp$month==12, 33] ~ ch.temp[ch.temp$month==12, 44]) # Dec
summary(bwp.lm12)

shit <- cbind(coef(bwp.lm1),coef(bwp.lm2),coef(bwp.lm3),coef(bwp.lm4),coef(bwp.lm5),coef(bwp.lm6),
              coef(bwp.lm7),coef(bwp.lm8),coef(bwp.lm9),coef(bwp.lm10),coef(bwp.lm11),coef(bwp.lm12))

# Regress BWP air temperatures against TUN air temperatures
bwp.lm1 <- lm(ch.temp[ch.temp$month==1, 32] ~ ch.temp[ch.temp$month==1, 43]) # Jan
summary(bwp.lm1)
bwp.lm2 <- lm(ch.temp[ch.temp$month==2, 32] ~ ch.temp[ch.temp$month==2, 43]) # Feb
summary(bwp.lm2)
bwp.lm3 <- lm(ch.temp[ch.temp$month==3, 32] ~ ch.temp[ch.temp$month==3, 43]) # Mar
summary(bwp.lm3)
bwp.lm4 <- lm(ch.temp[ch.temp$month==4, 32] ~ ch.temp[ch.temp$month==4, 43]) # Apr
summary(bwp.lm4)
bwp.lm5 <- lm(ch.temp[ch.temp$month==5, 32] ~ ch.temp[ch.temp$month==5, 43]) # May
summary(bwp.lm5)
bwp.lm6 <- lm(ch.temp[ch.temp$month==6, 32] ~ ch.temp[ch.temp$month==6, 43]) # Jun
summary(bwp.lm6)
bwp.lm7 <- lm(ch.temp[ch.temp$month==7, 32] ~ ch.temp[ch.temp$month==7, 43]) # Jul
summary(bwp.lm7)
bwp.lm8 <- lm(ch.temp[ch.temp$month==8, 32] ~ ch.temp[ch.temp$month==8, 43]) # Aug
summary(bwp.lm8)
bwp.lm9 <- lm(ch.temp[ch.temp$month==9, 32] ~ ch.temp[ch.temp$month==9, 43]) # Sep
summary(bwp.lm9)
bwp.lm10 <- lm(ch.temp[ch.temp$month==10, 32] ~ ch.temp[ch.temp$month==10, 43]) # Oct
summary(bwp.lm10)
bwp.lm11 <- lm(ch.temp[ch.temp$month==11, 32] ~ ch.temp[ch.temp$month==11, 43]) # Nov
summary(bwp.lm11)
bwp.lm12 <- lm(ch.temp[ch.temp$month==12, 32] ~ ch.temp[ch.temp$month==12, 43]) # Dec
summary(bwp.lm12)

shit <- cbind(coef(bwp.lm1),coef(bwp.lm2),coef(bwp.lm3),coef(bwp.lm4),coef(bwp.lm5),coef(bwp.lm6),
              coef(bwp.lm7),coef(bwp.lm8),coef(bwp.lm9),coef(bwp.lm10),coef(bwp.lm11),coef(bwp.lm12))

# Regress BWP ground temperatures against TUN ground temperatures
bwp.lm1 <- lm(ch.temp[ch.temp$month==1, 31] ~ ch.temp[ch.temp$month==1, 42]) # Jan
summary(bwp.lm1)
bwp.lm2 <- lm(ch.temp[ch.temp$month==2, 31] ~ ch.temp[ch.temp$month==2, 42]) # Feb
summary(bwp.lm2)
bwp.lm3 <- lm(ch.temp[ch.temp$month==3, 31] ~ ch.temp[ch.temp$month==3, 42]) # Mar
summary(bwp.lm3)
bwp.lm4 <- lm(ch.temp[ch.temp$month==4, 31] ~ ch.temp[ch.temp$month==4, 42]) # Apr
summary(bwp.lm4)
bwp.lm5 <- lm(ch.temp[ch.temp$month==5, 31] ~ ch.temp[ch.temp$month==5, 42]) # May
summary(bwp.lm5)
bwp.lm6 <- lm(ch.temp[ch.temp$month==6, 31] ~ ch.temp[ch.temp$month==6, 42]) # Jun
summary(bwp.lm6)
bwp.lm7 <- lm(ch.temp[ch.temp$month==7, 31] ~ ch.temp[ch.temp$month==7, 42]) # Jul
summary(bwp.lm7)
bwp.lm8 <- lm(ch.temp[ch.temp$month==8, 31] ~ ch.temp[ch.temp$month==8, 42]) # Aug
summary(bwp.lm8)
bwp.lm9 <- lm(ch.temp[ch.temp$month==9, 31] ~ ch.temp[ch.temp$month==9, 42]) # Sep
summary(bwp.lm9)
bwp.lm10 <- lm(ch.temp[ch.temp$month==10, 31] ~ ch.temp[ch.temp$month==10, 42]) # Oct
summary(bwp.lm10)
bwp.lm11 <- lm(ch.temp[ch.temp$month==11, 31] ~ ch.temp[ch.temp$month==11, 42]) # Nov
summary(bwp.lm11)
bwp.lm12 <- lm(ch.temp[ch.temp$month==12, 31] ~ ch.temp[ch.temp$month==12, 42]) # Dec
summary(bwp.lm12)

shit <- cbind(coef(bwp.lm1),coef(bwp.lm2),coef(bwp.lm3),coef(bwp.lm4),coef(bwp.lm5),coef(bwp.lm6),
              coef(bwp.lm7),coef(bwp.lm8),coef(bwp.lm9),coef(bwp.lm10),coef(bwp.lm11),coef(bwp.lm12))

# Regress TIS subsurface temperatures against TUN subsurface temperatures (only for months 7-10)
bwp.lm1 <- lm(ch.temp[ch.temp$month==1, 16] ~ ch.temp[ch.temp$month==1, 44]) # Jan
summary(bwp.lm1)
bwp.lm2 <- lm(ch.temp[ch.temp$month==2, 16] ~ ch.temp[ch.temp$month==2, 44]) # Feb
summary(bwp.lm2)
bwp.lm3 <- lm(ch.temp[ch.temp$month==3, 16] ~ ch.temp[ch.temp$month==3, 44]) # Mar
summary(bwp.lm3)
bwp.lm4 <- lm(ch.temp[ch.temp$month==4, 16] ~ ch.temp[ch.temp$month==4, 44]) # Apr
summary(bwp.lm4)
bwp.lm5 <- lm(ch.temp[ch.temp$month==5, 16] ~ ch.temp[ch.temp$month==5, 44]) # May
summary(bwp.lm5)
bwp.lm6 <- lm(ch.temp[ch.temp$month==6, 16] ~ ch.temp[ch.temp$month==6, 44]) # Jun
summary(bwp.lm6)
bwp.lm7 <- lm(ch.temp[ch.temp$month==7, 16] ~ ch.temp[ch.temp$month==7, 44]) # Jul
summary(bwp.lm7)
bwp.lm8 <- lm(ch.temp[ch.temp$month==8, 16] ~ ch.temp[ch.temp$month==8, 44]) # Aug
summary(bwp.lm8)
bwp.lm9 <- lm(ch.temp[ch.temp$month==9, 16] ~ ch.temp[ch.temp$month==9, 44]) # Sep
summary(bwp.lm9)
bwp.lm10 <- lm(ch.temp[ch.temp$month==10, 16] ~ ch.temp[ch.temp$month==10, 44]) # Oct
summary(bwp.lm10)
bwp.lm11 <- lm(ch.temp[ch.temp$month==11, 16] ~ ch.temp[ch.temp$month==11, 44]) # Nov
summary(bwp.lm11)
bwp.lm12 <- lm(ch.temp[ch.temp$month==12, 16] ~ ch.temp[ch.temp$month==12, 44]) # Dec
summary(bwp.lm12)

shit <- cbind(coef(bwp.lm1),coef(bwp.lm2),coef(bwp.lm3),coef(bwp.lm4),coef(bwp.lm5),coef(bwp.lm6),
              coef(bwp.lm7),coef(bwp.lm8),coef(bwp.lm9),coef(bwp.lm10),coef(bwp.lm11),coef(bwp.lm12))

# Regress TIS subsurface temperatures against WSU subsurface temperatures.
bwp.lm1 <- lm(ch.temp[ch.temp$month==1, 16] ~ ch.temp[ch.temp$month==1, 19]) # Jan
summary(bwp.lm1)
bwp.lm2 <- lm(ch.temp[ch.temp$month==2, 16] ~ ch.temp[ch.temp$month==2, 19]) # Feb
summary(bwp.lm2)
bwp.lm3 <- lm(ch.temp[ch.temp$month==3, 16] ~ ch.temp[ch.temp$month==3, 19]) # Mar
summary(bwp.lm3)
bwp.lm4 <- lm(ch.temp[ch.temp$month==4, 16] ~ ch.temp[ch.temp$month==4, 19]) # Apr
summary(bwp.lm4)
bwp.lm5 <- lm(ch.temp[ch.temp$month==5, 16] ~ ch.temp[ch.temp$month==5, 19]) # May
summary(bwp.lm5)
bwp.lm6 <- lm(ch.temp[ch.temp$month==6, 16] ~ ch.temp[ch.temp$month==6, 19]) # Jun
summary(bwp.lm6)
bwp.lm7 <- lm(ch.temp[ch.temp$month==7, 16] ~ ch.temp[ch.temp$month==7, 19]) # Jul
summary(bwp.lm7)
bwp.lm8 <- lm(ch.temp[ch.temp$month==8, 16] ~ ch.temp[ch.temp$month==8, 19]) # Aug
summary(bwp.lm8)
bwp.lm9 <- lm(ch.temp[ch.temp$month==9, 16] ~ ch.temp[ch.temp$month==9, 19]) # Sep
summary(bwp.lm9)
bwp.lm10 <- lm(ch.temp[ch.temp$month==10, 16] ~ ch.temp[ch.temp$month==10, 19]) # Oct
summary(bwp.lm10)
bwp.lm11 <- lm(ch.temp[ch.temp$month==11, 16] ~ ch.temp[ch.temp$month==11, 19]) # Nov
summary(bwp.lm11)
bwp.lm12 <- lm(ch.temp[ch.temp$month==12, 16] ~ ch.temp[ch.temp$month==12, 19]) # Dec
summary(bwp.lm12)

shit <- cbind(coef(bwp.lm1),coef(bwp.lm2),coef(bwp.lm3),coef(bwp.lm4),coef(bwp.lm5),coef(bwp.lm6),
              coef(bwp.lm7),coef(bwp.lm8),coef(bwp.lm9),coef(bwp.lm10),coef(bwp.lm11),coef(bwp.lm12))

# Regress TIS air temperatures against WSU air temperatures.
bwp.lm1 <- lm(ch.temp[ch.temp$month==1, 15] ~ ch.temp[ch.temp$month==1, 43]) # Jan
summary(bwp.lm1)
bwp.lm2 <- lm(ch.temp[ch.temp$month==2, 15] ~ ch.temp[ch.temp$month==2, 43]) # Feb
summary(bwp.lm2)
bwp.lm3 <- lm(ch.temp[ch.temp$month==3, 15] ~ ch.temp[ch.temp$month==3, 43]) # Mar
summary(bwp.lm3)
bwp.lm4 <- lm(ch.temp[ch.temp$month==4, 15] ~ ch.temp[ch.temp$month==4, 43]) # Apr
summary(bwp.lm4)
bwp.lm5 <- lm(ch.temp[ch.temp$month==5, 15] ~ ch.temp[ch.temp$month==5, 43]) # May
summary(bwp.lm5)
bwp.lm6 <- lm(ch.temp[ch.temp$month==6, 15] ~ ch.temp[ch.temp$month==6, 43]) # Jun
summary(bwp.lm6)
bwp.lm7 <- lm(ch.temp[ch.temp$month==7, 15] ~ ch.temp[ch.temp$month==7, 43]) # Jul
summary(bwp.lm7)
bwp.lm8 <- lm(ch.temp[ch.temp$month==8, 15] ~ ch.temp[ch.temp$month==8, 43]) # Aug
summary(bwp.lm8)
bwp.lm9 <- lm(ch.temp[ch.temp$month==9, 15] ~ ch.temp[ch.temp$month==9, 43]) # Sep
summary(bwp.lm9)
bwp.lm10 <- lm(ch.temp[ch.temp$month==10, 15] ~ ch.temp[ch.temp$month==10, 43]) # Oct
summary(bwp.lm10)
bwp.lm11 <- lm(ch.temp[ch.temp$month==11, 15] ~ ch.temp[ch.temp$month==11, 43]) # Nov
summary(bwp.lm11)
bwp.lm12 <- lm(ch.temp[ch.temp$month==12, 15] ~ ch.temp[ch.temp$month==12, 43]) # Dec
summary(bwp.lm12)

shit <- cbind(coef(bwp.lm1),coef(bwp.lm2),coef(bwp.lm3),coef(bwp.lm4),coef(bwp.lm5),coef(bwp.lm6),
              coef(bwp.lm7),coef(bwp.lm8),coef(bwp.lm9),coef(bwp.lm10),coef(bwp.lm11),coef(bwp.lm12))

# Regress AIR air temperatures against WSU air temperatures.
air.lm1 <- lm(ch.temp[ch.temp$month==1, 9] ~ ch.temp[ch.temp$month==1, 18]) # Jan
summary(air.lm1)
air.lm2 <- lm(ch.temp[ch.temp$month==2, 9] ~ ch.temp[ch.temp$month==2, 18]) # Feb
summary(air.lm2)
air.lm3 <- lm(ch.temp[ch.temp$month==3, 9] ~ ch.temp[ch.temp$month==3, 18]) # Mar
summary(air.lm3)
air.lm4 <- lm(ch.temp[ch.temp$month==4, 9] ~ ch.temp[ch.temp$month==4, 18]) # Apr
summary(air.lm4)
air.lm5 <- lm(ch.temp[ch.temp$month==5, 9] ~ ch.temp[ch.temp$month==5, 18]) # May
summary(air.lm5)
air.lm6 <- lm(ch.temp[ch.temp$month==6, 9] ~ ch.temp[ch.temp$month==6, 18]) # Jun
summary(air.lm6)
air.lm7 <- lm(ch.temp[ch.temp$month==7, 9] ~ ch.temp[ch.temp$month==7, 18]) # Jul
summary(air.lm7)
air.lm8 <- lm(ch.temp[ch.temp$month==8, 9] ~ ch.temp[ch.temp$month==8, 18]) # Aug
summary(air.lm8)
air.lm9 <- lm(ch.temp[ch.temp$month==9, 9] ~ ch.temp[ch.temp$month==9, 18]) # Sep
summary(air.lm9)
air.lm10 <- lm(ch.temp[ch.temp$month==10, 9] ~ ch.temp[ch.temp$month==10, 18]) # Oct
summary(air.lm10)
air.lm11 <- lm(ch.temp[ch.temp$month==11, 9] ~ ch.temp[ch.temp$month==11, 18]) # Nov
summary(air.lm11)
air.lm12 <- lm(ch.temp[ch.temp$month==12, 9] ~ ch.temp[ch.temp$month==12, 18]) # Dec
summary(air.lm12)

shit <- cbind(coef(air.lm1),coef(air.lm2),coef(air.lm3),coef(air.lm4),coef(air.lm5),coef(air.lm6),
              coef(air.lm7),coef(air.lm8),coef(air.lm9),coef(air.lm10),coef(air.lm11),coef(air.lm12))

# Regress AIR ground temperatures against WSU ground temperatures.
air.lm1 <- lm(ch.temp[ch.temp$month==1, 8] ~ ch.temp[ch.temp$month==1, 17]) # Jan
summary(air.lm1)
air.lm2 <- lm(ch.temp[ch.temp$month==2, 8] ~ ch.temp[ch.temp$month==2, 17]) # Feb
summary(air.lm2)
air.lm3 <- lm(ch.temp[ch.temp$month==3, 8] ~ ch.temp[ch.temp$month==3, 17]) # Mar
summary(air.lm3)
air.lm4 <- lm(ch.temp[ch.temp$month==4, 8] ~ ch.temp[ch.temp$month==4, 17]) # Apr
summary(air.lm4)
air.lm5 <- lm(ch.temp[ch.temp$month==5, 8] ~ ch.temp[ch.temp$month==5, 17]) # May
summary(air.lm5)
air.lm6 <- lm(ch.temp[ch.temp$month==6, 8] ~ ch.temp[ch.temp$month==6, 17]) # Jun
summary(air.lm6)
air.lm7 <- lm(ch.temp[ch.temp$month==7, 8] ~ ch.temp[ch.temp$month==7, 17]) # Jul
summary(air.lm7)
air.lm8 <- lm(ch.temp[ch.temp$month==8, 8] ~ ch.temp[ch.temp$month==8, 17]) # Aug
summary(air.lm8)
air.lm8 <- lm(ch.temp[ch.temp$month==9, 8] ~ ch.temp[ch.temp$month==9, 17]) # Sep
summary(air.lm8)
air.lm10 <- lm(ch.temp[ch.temp$month==10, 8] ~ ch.temp[ch.temp$month==10, 17]) # Oct
summary(air.lm10)
air.lm11 <- lm(ch.temp[ch.temp$month==11, 8] ~ ch.temp[ch.temp$month==11, 17]) # Nov
summary(air.lm11)
air.lm12 <- lm(ch.temp[ch.temp$month==12, 8] ~ ch.temp[ch.temp$month==12, 17]) # Dec
summary(air.lm12)

shit <- cbind(coef(air.lm1),coef(air.lm2),coef(air.lm3),coef(air.lm4),coef(air.lm5),coef(air.lm6),
              coef(air.lm7),coef(air.lm8),coef(air.lm9),coef(air.lm10),coef(air.lm11),coef(air.lm12))

# Regress AIR -80 temperatures against BSW and WSU -80 temperatures.
air.lm1 <- lm(ch.temp[ch.temp$month==1, 10] ~ ch.temp[ch.temp$month==1, 13] + ch.temp[ch.temp$month==1, 19]) # Jan
summary(air.lm1)
air.lm2 <- lm(ch.temp[ch.temp$month==2, 10] ~ ch.temp[ch.temp$month==2, 13] + ch.temp[ch.temp$month==2, 19]) # Feb
summary(air.lm2)
air.lm3 <- lm(ch.temp[ch.temp$month==3, 10] ~ ch.temp[ch.temp$month==3, 13] + ch.temp[ch.temp$month==3, 19]) # Mar
summary(air.lm3)
air.lm4 <- lm(ch.temp[ch.temp$month==4, 10] ~ ch.temp[ch.temp$month==4, 13] + ch.temp[ch.temp$month==4, 19]) # Apr
summary(air.lm4)
air.lm5 <- lm(ch.temp[ch.temp$month==5, 10] ~ ch.temp[ch.temp$month==5, 13] + ch.temp[ch.temp$month==5, 19]) # May
summary(air.lm5)
air.lm6 <- lm(ch.temp[ch.temp$month==6, 10] ~ ch.temp[ch.temp$month==6, 13] + ch.temp[ch.temp$month==6, 19]) # Jun
summary(air.lm6)
air.lm7 <- lm(ch.temp[ch.temp$month==7, 10] ~ ch.temp[ch.temp$month==7, 13] + ch.temp[ch.temp$month==7, 19]) # Jul
summary(air.lm7)
air.lm8 <- lm(ch.temp[ch.temp$month==8, 10] ~ ch.temp[ch.temp$month==8, 13] + ch.temp[ch.temp$month==8, 19]) # Aug
summary(air.lm8)
air.lm9 <- lm(ch.temp[ch.temp$month==9, 10] ~ ch.temp[ch.temp$month==9, 13] + ch.temp[ch.temp$month==9, 19]) # Sep
summary(air.lm9)
air.lm10 <- lm(ch.temp[ch.temp$month==10, 10] ~ ch.temp[ch.temp$month==10, 13] + ch.temp[ch.temp$month==10, 19]) # Oct
summary(air.lm10)
air.lm11 <- lm(ch.temp[ch.temp$month==11, 10] ~ ch.temp[ch.temp$month==11, 13] + ch.temp[ch.temp$month==11, 19]) # Nov
summary(air.lm11)
air.lm12 <- lm(ch.temp[ch.temp$month==12, 10] ~ ch.temp[ch.temp$month==12, 13] + ch.temp[ch.temp$month==12, 19]) # Dec
summary(air.lm12)

shit <- cbind(coef(air.lm1),coef(air.lm2),coef(air.lm3),coef(air.lm4),coef(air.lm5),coef(air.lm6),
              coef(air.lm7),coef(air.lm8),coef(air.lm9),coef(air.lm10),coef(air.lm11),coef(air.lm12))

# Regress PPD air temperatures against WSU air temperatures.
ppd.lm1 <- lm(ch.temp[ch.temp$month==1, 35] ~ ch.temp[ch.temp$month==1, 18]) # Jan
summary(ppd.lm1)
ppd.lm2 <- lm(ch.temp[ch.temp$month==2, 35] ~ ch.temp[ch.temp$month==2, 18]) # Feb
summary(ppd.lm2)
ppd.lm3 <- lm(ch.temp[ch.temp$month==3, 35] ~ ch.temp[ch.temp$month==3, 18]) # Mar
summary(ppd.lm3)
ppd.lm4 <- lm(ch.temp[ch.temp$month==4, 35] ~ ch.temp[ch.temp$month==4, 18]) # Apr
summary(ppd.lm4)
ppd.lm5 <- lm(ch.temp[ch.temp$month==5, 35] ~ ch.temp[ch.temp$month==5, 18]) # May
summary(ppd.lm5)
ppd.lm6 <- lm(ch.temp[ch.temp$month==6, 35] ~ ch.temp[ch.temp$month==6, 18]) # Jun
summary(ppd.lm6)
ppd.lm7 <- lm(ch.temp[ch.temp$month==7, 35] ~ ch.temp[ch.temp$month==7, 18]) # Jul
summary(ppd.lm7)
ppd.lm8 <- lm(ch.temp[ch.temp$month==8, 35] ~ ch.temp[ch.temp$month==8, 18]) # Aug
summary(ppd.lm8)
ppd.lm9 <- lm(ch.temp[ch.temp$month==9, 35] ~ ch.temp[ch.temp$month==9, 18]) # Sep
summary(ppd.lm9)
ppd.lm10 <- lm(ch.temp[ch.temp$month==10, 35] ~ ch.temp[ch.temp$month==10, 18]) # Oct
summary(ppd.lm10)
ppd.lm11 <- lm(ch.temp[ch.temp$month==11, 35] ~ ch.temp[ch.temp$month==11, 18]) # Nov
summary(ppd.lm11)
ppd.lm12 <- lm(ch.temp[ch.temp$month==12, 35] ~ ch.temp[ch.temp$month==12, 18]) # Dec
summary(ppd.lm12)

shit <- cbind(coef(ppd.lm1),coef(ppd.lm2),coef(ppd.lm3),coef(ppd.lm4),coef(ppd.lm5),coef(ppd.lm6),
              coef(ppd.lm7),coef(ppd.lm8),coef(ppd.lm9),coef(ppd.lm10),coef(ppd.lm11),coef(ppd.lm12))

# Regress PPD ground temperatures against TUN ground temperatures.
ppd.lm1 <- lm(ch.temp[ch.temp$month==1, 38] ~ ch.temp[ch.temp$month==1, 42]) # Jan
summary(ppd.lm1)
ppd.lm2 <- lm(ch.temp[ch.temp$month==2, 38] ~ ch.temp[ch.temp$month==2, 42]) # Feb
summary(ppd.lm2)
ppd.lm3 <- lm(ch.temp[ch.temp$month==3, 38] ~ ch.temp[ch.temp$month==3, 42]) # Mar
summary(ppd.lm3)
ppd.lm4 <- lm(ch.temp[ch.temp$month==4, 38] ~ ch.temp[ch.temp$month==4, 42]) # Apr
summary(ppd.lm4)
ppd.lm5 <- lm(ch.temp[ch.temp$month==5, 38] ~ ch.temp[ch.temp$month==5, 42]) # May
summary(ppd.lm5)
ppd.lm6 <- lm(ch.temp[ch.temp$month==6, 38] ~ ch.temp[ch.temp$month==6, 42]) # Jun
summary(ppd.lm6)
ppd.lm7 <- lm(ch.temp[ch.temp$month==7, 38] ~ ch.temp[ch.temp$month==7, 42]) # Jul
summary(ppd.lm7)
ppd.lm8 <- lm(ch.temp[ch.temp$month==8, 38] ~ ch.temp[ch.temp$month==8, 42]) # Aug
summary(ppd.lm8)
ppd.lm9 <- lm(ch.temp[ch.temp$month==9, 38] ~ ch.temp[ch.temp$month==9, 42]) # Sep
summary(ppd.lm9)
ppd.lm10 <- lm(ch.temp[ch.temp$month==10, 38] ~ ch.temp[ch.temp$month==10, 42]) # Oct
summary(ppd.lm10)
ppd.lm11 <- lm(ch.temp[ch.temp$month==11, 38] ~ ch.temp[ch.temp$month==11, 42]) # Nov
summary(ppd.lm11)
ppd.lm12 <- lm(ch.temp[ch.temp$month==12, 38] ~ ch.temp[ch.temp$month==12, 42]) # Dec
summary(ppd.lm12)

shit <- cbind(coef(ppd.lm1),coef(ppd.lm2),coef(ppd.lm3),coef(ppd.lm4),coef(ppd.lm5),coef(ppd.lm6),
              coef(ppd.lm7),coef(ppd.lm8),coef(ppd.lm9),coef(ppd.lm10),coef(ppd.lm11),coef(ppd.lm12))

# Regress PPD -80 temperatures against TUN -80 temperatures.
ppd.lm1 <- lm(ch.temp[ch.temp$month==1, 40] ~ ch.temp[ch.temp$month==1, 44]) # Jan
summary(ppd.lm1)
ppd.lm2 <- lm(ch.temp[ch.temp$month==2, 40] ~ ch.temp[ch.temp$month==2, 44]) # Feb
summary(ppd.lm2)
ppd.lm3 <- lm(ch.temp[ch.temp$month==3, 40] ~ ch.temp[ch.temp$month==3, 44]) # Mar
summary(ppd.lm3)
ppd.lm4 <- lm(ch.temp[ch.temp$month==4, 40] ~ ch.temp[ch.temp$month==4, 44]) # Apr
summary(ppd.lm4)
ppd.lm5 <- lm(ch.temp[ch.temp$month==5, 40] ~ ch.temp[ch.temp$month==5, 44]) # May
summary(ppd.lm5)
ppd.lm6 <- lm(ch.temp[ch.temp$month==6, 40] ~ ch.temp[ch.temp$month==6, 44]) # Jun
summary(ppd.lm6)
ppd.lm7 <- lm(ch.temp[ch.temp$month==7, 40] ~ ch.temp[ch.temp$month==7, 44]) # Jul
summary(ppd.lm7)
ppd.lm8 <- lm(ch.temp[ch.temp$month==8, 40] ~ ch.temp[ch.temp$month==8, 44]) # Aug
summary(ppd.lm8)
ppd.lm9 <- lm(ch.temp[ch.temp$month==9, 40] ~ ch.temp[ch.temp$month==9, 44]) # Sep
summary(ppd.lm9)
ppd.lm10 <- lm(ch.temp[ch.temp$month==10, 40] ~ ch.temp[ch.temp$month==10, 44]) # Oct
summary(ppd.lm10)
ppd.lm11 <- lm(ch.temp[ch.temp$month==11, 40] ~ ch.temp[ch.temp$month==11, 44]) # Nov
summary(ppd.lm11)
ppd.lm12 <- lm(ch.temp[ch.temp$month==12, 40] ~ ch.temp[ch.temp$month==12, 44]) # Dec
summary(ppd.lm12)

shit <- cbind(coef(ppd.lm1),coef(ppd.lm2),coef(ppd.lm3),coef(ppd.lm4),coef(ppd.lm5),coef(ppd.lm6),
              coef(ppd.lm7),coef(ppd.lm8),coef(ppd.lm9),coef(ppd.lm10),coef(ppd.lm11),coef(ppd.lm12))

# Regress PPA air temperatures against PPD air temperatures.
ppa.lm1 <- lm(ch.temp[ch.temp$month==1, 35] ~ ch.temp[ch.temp$month==1, 39]) # Jan
summary(ppa.lm1)
ppa.lm2 <- lm(ch.temp[ch.temp$month==2, 35] ~ ch.temp[ch.temp$month==2, 39]) # Feb
summary(ppa.lm2)
ppa.lm3 <- lm(ch.temp[ch.temp$month==3, 35] ~ ch.temp[ch.temp$month==3, 39]) # Mar
summary(ppa.lm3)
ppa.lm4 <- lm(ch.temp[ch.temp$month==4, 35] ~ ch.temp[ch.temp$month==4, 39]) # Apr
summary(ppa.lm4)
ppa.lm5 <- lm(ch.temp[ch.temp$month==5, 35] ~ ch.temp[ch.temp$month==5, 39]) # May
summary(ppa.lm5)
ppa.lm6 <- lm(ch.temp[ch.temp$month==6, 35] ~ ch.temp[ch.temp$month==6, 39]) # Jun
summary(ppa.lm6)
ppa.lm7 <- lm(ch.temp[ch.temp$month==7, 35] ~ ch.temp[ch.temp$month==7, 39]) # Jul
summary(ppa.lm7)
ppa.lm8 <- lm(ch.temp[ch.temp$month==8, 35] ~ ch.temp[ch.temp$month==8, 39]) # Aug
summary(ppa.lm8)
ppa.lm9 <- lm(ch.temp[ch.temp$month==9, 35] ~ ch.temp[ch.temp$month==9, 39]) # Sep
summary(ppa.lm9)
ppa.lm10 <- lm(ch.temp[ch.temp$month==10, 35] ~ ch.temp[ch.temp$month==10, 39]) # Oct
summary(ppa.lm10)
ppa.lm11 <- lm(ch.temp[ch.temp$month==11, 35] ~ ch.temp[ch.temp$month==11, 39]) # Nov
summary(ppa.lm11)
ppa.lm12 <- lm(ch.temp[ch.temp$month==12, 35] ~ ch.temp[ch.temp$month==12, 39]) # Dec
summary(ppa.lm12)

shit <- cbind(coef(ppa.lm1),coef(ppa.lm2),coef(ppa.lm3),coef(ppa.lm4),coef(ppa.lm5),coef(ppa.lm6),
              coef(ppa.lm7),coef(ppa.lm8),coef(ppa.lm9),coef(ppa.lm10),coef(ppa.lm11),coef(ppa.lm12))

# Regress PPA ground surface temperatures against PPD ground surface temperatures.
ppa.lm1 <- lm(ch.temp[ch.temp$month==1, 34] ~ ch.temp[ch.temp$month==1, 38]) # Jan
summary(ppa.lm1)
ppa.lm2 <- lm(ch.temp[ch.temp$month==2, 34] ~ ch.temp[ch.temp$month==2, 38]) # Feb
summary(ppa.lm2)
ppa.lm3 <- lm(ch.temp[ch.temp$month==3, 34] ~ ch.temp[ch.temp$month==3, 38]) # Mar
summary(ppa.lm3)
ppa.lm4 <- lm(ch.temp[ch.temp$month==4, 34] ~ ch.temp[ch.temp$month==4, 38]) # Apr
summary(ppa.lm4)
ppa.lm5 <- lm(ch.temp[ch.temp$month==5, 34] ~ ch.temp[ch.temp$month==5, 38]) # May
summary(ppa.lm5)
ppa.lm6 <- lm(ch.temp[ch.temp$month==6, 34] ~ ch.temp[ch.temp$month==6, 38]) # Jun
summary(ppa.lm6)
ppa.lm7 <- lm(ch.temp[ch.temp$month==7, 34] ~ ch.temp[ch.temp$month==7, 38]) # Jul
summary(ppa.lm7)
ppa.lm8 <- lm(ch.temp[ch.temp$month==8, 34] ~ ch.temp[ch.temp$month==8, 38]) # Aug
summary(ppa.lm8)
ppa.lm9 <- lm(ch.temp[ch.temp$month==9, 34] ~ ch.temp[ch.temp$month==9, 38]) # Sep
summary(ppa.lm9)
ppa.lm10 <- lm(ch.temp[ch.temp$month==10, 34] ~ ch.temp[ch.temp$month==10, 38]) # Oct
summary(ppa.lm10)
ppa.lm11 <- lm(ch.temp[ch.temp$month==11, 34] ~ ch.temp[ch.temp$month==11, 38]) # Nov
summary(ppa.lm11)
ppa.lm12 <- lm(ch.temp[ch.temp$month==12, 34] ~ ch.temp[ch.temp$month==12, 38]) # Dec
summary(ppa.lm12)

shit <- cbind(coef(ppa.lm1),coef(ppa.lm2),coef(ppa.lm3),coef(ppa.lm4),coef(ppa.lm5),coef(ppa.lm6),
              coef(ppa.lm7),coef(ppa.lm8),coef(ppa.lm9),coef(ppa.lm10),coef(ppa.lm11),coef(ppa.lm12))

# Regress PPA subsurface temperatures against PPD and TUN subsurface temperatures.
ppa.lm1 <- lm(ch.temp[ch.temp$month==1, 36] ~ ch.temp[ch.temp$month==1, 40] + ch.temp[ch.temp$month==1, 44]) # Jan
summary(ppa.lm1)
ppa.lm2 <- lm(ch.temp[ch.temp$month==2, 36] ~ ch.temp[ch.temp$month==2, 40] + ch.temp[ch.temp$month==2, 44]) # Feb
summary(ppa.lm2)
ppa.lm3 <- lm(ch.temp[ch.temp$month==3, 36] ~ ch.temp[ch.temp$month==3, 40] + ch.temp[ch.temp$month==3, 44]) # Mar
summary(ppa.lm3)
ppa.lm4 <- lm(ch.temp[ch.temp$month==4, 36] ~ ch.temp[ch.temp$month==4, 40] + ch.temp[ch.temp$month==4, 44]) # Apr
summary(ppa.lm4)
ppa.lm5 <- lm(ch.temp[ch.temp$month==5, 36] ~ ch.temp[ch.temp$month==5, 40] + ch.temp[ch.temp$month==5, 44]) # May
summary(ppa.lm5)
ppa.lm6 <- lm(ch.temp[ch.temp$month==6, 36] ~ ch.temp[ch.temp$month==6, 40] + ch.temp[ch.temp$month==6, 44]) # Jun
summary(ppa.lm6)
ppa.lm7 <- lm(ch.temp[ch.temp$month==7, 36] ~ ch.temp[ch.temp$month==7, 40] + ch.temp[ch.temp$month==7, 44]) # Jul
summary(ppa.lm7)
ppa.lm8 <- lm(ch.temp[ch.temp$month==8, 36] ~ ch.temp[ch.temp$month==8, 40] + ch.temp[ch.temp$month==8, 44]) # Aug
summary(ppa.lm8)
ppa.lm9 <- lm(ch.temp[ch.temp$month==9, 36] ~ ch.temp[ch.temp$month==9, 40] + ch.temp[ch.temp$month==9, 44]) # Sep
summary(ppa.lm9)
ppa.lm10 <- lm(ch.temp[ch.temp$month==10, 36] ~ ch.temp[ch.temp$month==10, 40] + ch.temp[ch.temp$month==10, 44]) # Oct
summary(ppa.lm10)
ppa.lm11 <- lm(ch.temp[ch.temp$month==11, 36] ~ ch.temp[ch.temp$month==11, 40] + ch.temp[ch.temp$month==11, 44]) # Nov
summary(ppa.lm11)
ppa.lm12 <- lm(ch.temp[ch.temp$month==12, 36] ~ ch.temp[ch.temp$month==12, 40] + ch.temp[ch.temp$month==12, 44]) # Dec
summary(ppa.lm12)

shit <- cbind(coef(ppa.lm1),coef(ppa.lm2),coef(ppa.lm3),coef(ppa.lm4),coef(ppa.lm5),coef(ppa.lm6),
              coef(ppa.lm7),coef(ppa.lm8),coef(ppa.lm9),coef(ppa.lm10),coef(ppa.lm11),coef(ppa.lm12))

# Regress BFR air temperatures against WSU air temperatures.
bfr.lm1 <- lm(ch.temp[ch.temp$month==1, 26] ~ ch.temp[ch.temp$month==1, 18]) # Jan
summary(bfr.lm1)
bfr.lm2 <- lm(ch.temp[ch.temp$month==2, 26] ~ ch.temp[ch.temp$month==2, 18]) # Feb
summary(bfr.lm2)
bfr.lm3 <- lm(ch.temp[ch.temp$month==3, 26] ~ ch.temp[ch.temp$month==3, 18]) # Mar
summary(bfr.lm3)
bfr.lm4 <- lm(ch.temp[ch.temp$month==4, 26] ~ ch.temp[ch.temp$month==4, 18]) # Apr
summary(bfr.lm4)
bfr.lm5 <- lm(ch.temp[ch.temp$month==5, 26] ~ ch.temp[ch.temp$month==5, 18]) # May
summary(bfr.lm5)
bfr.lm6 <- lm(ch.temp[ch.temp$month==6, 26] ~ ch.temp[ch.temp$month==6, 18]) # Jun
summary(bfr.lm6)
bfr.lm7 <- lm(ch.temp[ch.temp$month==7, 26] ~ ch.temp[ch.temp$month==7, 18]) # Jul
summary(bfr.lm7)
bfr.lm8 <- lm(ch.temp[ch.temp$month==8, 26] ~ ch.temp[ch.temp$month==8, 18]) # Aug
summary(bfr.lm8)
bfr.lm9 <- lm(ch.temp[ch.temp$month==9, 26] ~ ch.temp[ch.temp$month==9, 18]) # Sep
summary(bfr.lm9)
bfr.lm10 <- lm(ch.temp[ch.temp$month==10, 26] ~ ch.temp[ch.temp$month==10, 18]) # Oct
summary(bfr.lm10)
bfr.lm11 <- lm(ch.temp[ch.temp$month==11, 26] ~ ch.temp[ch.temp$month==11, 18]) # Nov
summary(bfr.lm11)
bfr.lm12 <- lm(ch.temp[ch.temp$month==12, 26] ~ ch.temp[ch.temp$month==12, 18]) # Dec
summary(bfr.lm12)

shit <- cbind(coef(bfr.lm1),coef(bfr.lm2),coef(bfr.lm3),coef(bfr.lm4),coef(bfr.lm5),coef(bfr.lm6),
              coef(bfr.lm7),coef(bfr.lm8),coef(bfr.lm9),coef(bfr.lm10),coef(bfr.lm11),coef(bfr.lm12))

# Regress BFR ground surface temperatures against WSU + BWP ground surface temperatures.
bfr.lm1 <- lm(ch.temp[ch.temp$month==1, 25] ~ ch.temp[ch.temp$month==1, 17] + ch.temp[ch.temp$month==1, 31]) # Jan
summary(bfr.lm1)
bfr.lm2 <- lm(ch.temp[ch.temp$month==2, 25] ~ ch.temp[ch.temp$month==2, 17] + ch.temp[ch.temp$month==2, 31]) # Feb
summary(bfr.lm2)
bfr.lm3 <- lm(ch.temp[ch.temp$month==3, 25] ~ ch.temp[ch.temp$month==3, 17] + ch.temp[ch.temp$month==3, 31]) # Mar
summary(bfr.lm3)
bfr.lm4 <- lm(ch.temp[ch.temp$month==4, 25] ~ ch.temp[ch.temp$month==4, 17] + ch.temp[ch.temp$month==4, 31]) # Apr
summary(bfr.lm4)
bfr.lm5 <- lm(ch.temp[ch.temp$month==5, 25] ~ ch.temp[ch.temp$month==5, 17] + ch.temp[ch.temp$month==5, 31]) # May
summary(bfr.lm5)
bfr.lm6 <- lm(ch.temp[ch.temp$month==6, 25] ~ ch.temp[ch.temp$month==6, 17] + ch.temp[ch.temp$month==6, 31]) # Jun
summary(bfr.lm6)
bfr.lm7 <- lm(ch.temp[ch.temp$month==7, 25] ~ ch.temp[ch.temp$month==7, 17] + ch.temp[ch.temp$month==7, 31]) # Jul
summary(bfr.lm7)
bfr.lm8 <- lm(ch.temp[ch.temp$month==8, 25] ~ ch.temp[ch.temp$month==8, 17] + ch.temp[ch.temp$month==8, 31]) # Aug
summary(bfr.lm8)
bfr.lm9 <- lm(ch.temp[ch.temp$month==9, 25] ~ ch.temp[ch.temp$month==9, 17] + ch.temp[ch.temp$month==9, 31]) # Sep
summary(bfr.lm9)
bfr.lm10 <- lm(ch.temp[ch.temp$month==10, 25] ~ ch.temp[ch.temp$month==10, 17] + ch.temp[ch.temp$month==10, 31]) # Oct
summary(bfr.lm10)
bfr.lm11 <- lm(ch.temp[ch.temp$month==11, 25] ~ ch.temp[ch.temp$month==11, 17] + ch.temp[ch.temp$month==11, 31]) # Nov
summary(bfr.lm11)
bfr.lm12 <- lm(ch.temp[ch.temp$month==12, 25] ~ ch.temp[ch.temp$month==12, 17] + ch.temp[ch.temp$month==12, 31]) # Dec
summary(bfr.lm12)

shit <- cbind(coef(bfr.lm1),coef(bfr.lm2),coef(bfr.lm3),coef(bfr.lm4),coef(bfr.lm5),coef(bfr.lm6),
              coef(bfr.lm7),coef(bfr.lm8),coef(bfr.lm9),coef(bfr.lm10),coef(bfr.lm11),coef(bfr.lm12))

# Regress BFR subsurface temperatures against WSU subsurface temperatures.
bfr.lm1 <- lm(ch.temp[ch.temp$month==1, 27] ~ ch.temp[ch.temp$month==1, 19]) # Jan
summary(bfr.lm1)
bfr.lm2 <- lm(ch.temp[ch.temp$month==2, 27] ~ ch.temp[ch.temp$month==2, 19]) # Feb
summary(bfr.lm2)
bfr.lm3 <- lm(ch.temp[ch.temp$month==3, 27] ~ ch.temp[ch.temp$month==3, 19]) # Mar
summary(bfr.lm3)
bfr.lm4 <- lm(ch.temp[ch.temp$month==4, 27] ~ ch.temp[ch.temp$month==4, 19]) # Apr
summary(bfr.lm4)
bfr.lm5 <- lm(ch.temp[ch.temp$month==5, 27] ~ ch.temp[ch.temp$month==5, 19]) # May
summary(bfr.lm5)
bfr.lm6 <- lm(ch.temp[ch.temp$month==6, 27] ~ ch.temp[ch.temp$month==6, 19]) # Jun
summary(bfr.lm6)
bfr.lm7 <- lm(ch.temp[ch.temp$month==7, 27] ~ ch.temp[ch.temp$month==7, 19]) # Jul
summary(bfr.lm7)
bfr.lm8 <- lm(ch.temp[ch.temp$month==8, 27] ~ ch.temp[ch.temp$month==8, 19]) # Aug
summary(bfr.lm8)
bfr.lm9 <- lm(ch.temp[ch.temp$month==9, 27] ~ ch.temp[ch.temp$month==9, 19]) # Sep
summary(bfr.lm9)
bfr.lm10 <- lm(ch.temp[ch.temp$month==10, 27] ~ ch.temp[ch.temp$month==10, 19]) # Oct
summary(bfr.lm10)
bfr.lm11 <- lm(ch.temp[ch.temp$month==11, 27] ~ ch.temp[ch.temp$month==11, 19]) # Nov
summary(bfr.lm11)
bfr.lm12 <- lm(ch.temp[ch.temp$month==12, 27] ~ ch.temp[ch.temp$month==12, 19]) # Dec
summary(bfr.lm12)

shit <- cbind(coef(bfr.lm1),coef(bfr.lm2),coef(bfr.lm3),coef(bfr.lm4),coef(bfr.lm5),coef(bfr.lm6),
              coef(bfr.lm7),coef(bfr.lm8),coef(bfr.lm9),coef(bfr.lm10),coef(bfr.lm11),coef(bfr.lm12))

# Regress PFR air temperatures against BSW + BFR air temperatures.
pfr.lm1 <- lm(ch.temp[ch.temp$month==1, 29] ~ ch.temp[ch.temp$month==1, 12] + ch.temp[ch.temp$month==1, 26]) # Jan
summary(pfr.lm1)
pfr.lm2 <- lm(ch.temp[ch.temp$month==2, 29] ~ ch.temp[ch.temp$month==2, 12] + ch.temp[ch.temp$month==2, 26]) # Feb
summary(pfr.lm2)
pfr.lm3 <- lm(ch.temp[ch.temp$month==3, 29] ~ ch.temp[ch.temp$month==3, 12] + ch.temp[ch.temp$month==3, 26]) # Mar
summary(pfr.lm3)
pfr.lm4 <- lm(ch.temp[ch.temp$month==4, 29] ~ ch.temp[ch.temp$month==4, 12] + ch.temp[ch.temp$month==4, 26]) # Apr
summary(pfr.lm4)
pfr.lm5 <- lm(ch.temp[ch.temp$month==5, 29] ~ ch.temp[ch.temp$month==5, 12] + ch.temp[ch.temp$month==5, 26]) # May
summary(pfr.lm5)
pfr.lm6 <- lm(ch.temp[ch.temp$month==6, 29] ~ ch.temp[ch.temp$month==6, 12] + ch.temp[ch.temp$month==6, 26]) # Jun
summary(pfr.lm6)
pfr.lm7 <- lm(ch.temp[ch.temp$month==7, 29] ~ ch.temp[ch.temp$month==7, 12] + ch.temp[ch.temp$month==7, 26]) # Jul
summary(pfr.lm7)
pfr.lm8 <- lm(ch.temp[ch.temp$month==8, 29] ~ ch.temp[ch.temp$month==8, 12] + ch.temp[ch.temp$month==8, 26]) # Aug
summary(pfr.lm8)
pfr.lm9 <- lm(ch.temp[ch.temp$month==9, 29] ~ ch.temp[ch.temp$month==9, 12] + ch.temp[ch.temp$month==9, 26]) # Sep
summary(pfr.lm9)
pfr.lm10 <- lm(ch.temp[ch.temp$month==10, 29] ~ ch.temp[ch.temp$month==10, 12] + ch.temp[ch.temp$month==10, 26]) # Oct
summary(pfr.lm10)
pfr.lm11 <- lm(ch.temp[ch.temp$month==11, 29] ~ ch.temp[ch.temp$month==11, 12] + ch.temp[ch.temp$month==11, 26]) # Nov
summary(pfr.lm11)
pfr.lm12 <- lm(ch.temp[ch.temp$month==12, 29] ~ ch.temp[ch.temp$month==12, 12] + ch.temp[ch.temp$month==12, 26]) # Dec
summary(pfr.lm12)

shit <- cbind(coef(pfr.lm1),coef(pfr.lm2),coef(pfr.lm3),coef(pfr.lm4),coef(pfr.lm5),coef(pfr.lm6),
              coef(pfr.lm7),coef(pfr.lm8),coef(pfr.lm9),coef(pfr.lm10),coef(pfr.lm11),coef(pfr.lm12))

# Regress PFR ground surface temperatures against BFR  ground surface temperatures.
pfr.lm1 <- lm(ch.temp[ch.temp$month==1, 28] ~ ch.temp[ch.temp$month==1, 25]) # Jan
summary(pfr.lm1)
pfr.lm2 <- lm(ch.temp[ch.temp$month==2, 28] ~ ch.temp[ch.temp$month==2, 25]) # Feb
summary(pfr.lm2)
pfr.lm3 <- lm(ch.temp[ch.temp$month==3, 28] ~ ch.temp[ch.temp$month==3, 25]) # Mar
summary(pfr.lm3)
pfr.lm4 <- lm(ch.temp[ch.temp$month==4, 28] ~ ch.temp[ch.temp$month==4, 25]) # Apr
summary(pfr.lm4)
pfr.lm5 <- lm(ch.temp[ch.temp$month==5, 28] ~ ch.temp[ch.temp$month==5, 25]) # May
summary(pfr.lm5)
pfr.lm6 <- lm(ch.temp[ch.temp$month==6, 28] ~ ch.temp[ch.temp$month==6, 25]) # Jun
summary(pfr.lm6)
pfr.lm7 <- lm(ch.temp[ch.temp$month==7, 28] ~ ch.temp[ch.temp$month==7, 25]) # Jul
summary(pfr.lm7)
pfr.lm8 <- lm(ch.temp[ch.temp$month==8, 28] ~ ch.temp[ch.temp$month==8, 25]) # Aug
summary(pfr.lm8)
pfr.lm9 <- lm(ch.temp[ch.temp$month==9, 28] ~ ch.temp[ch.temp$month==9, 25]) # Sep
summary(pfr.lm9)
pfr.lm10 <- lm(ch.temp[ch.temp$month==10, 28] ~ ch.temp[ch.temp$month==10, 25]) # Oct
summary(pfr.lm10)
pfr.lm11 <- lm(ch.temp[ch.temp$month==11, 28] ~ ch.temp[ch.temp$month==11, 25]) # Nov
summary(pfr.lm11)
pfr.lm12 <- lm(ch.temp[ch.temp$month==12, 28] ~ ch.temp[ch.temp$month==12, 25]) # Dec
summary(pfr.lm12)

shit <- cbind(coef(pfr.lm1),coef(pfr.lm2),coef(pfr.lm3),coef(pfr.lm4),coef(pfr.lm5),coef(pfr.lm6),
              coef(pfr.lm7),coef(pfr.lm8),coef(pfr.lm9),coef(pfr.lm10),coef(pfr.lm11),coef(pfr.lm12))

# Regress PFR ground surface temperatures against BFR  ground surface temperatures.
pfr.lm1 <- lm(ch.temp[ch.temp$month==1, 30] ~ ch.temp[ch.temp$month==1, 27]) # Jan
summary(pfr.lm1)
pfr.lm2 <- lm(ch.temp[ch.temp$month==2, 30] ~ ch.temp[ch.temp$month==2, 27]) # Feb
summary(pfr.lm2)
pfr.lm3 <- lm(ch.temp[ch.temp$month==3, 30] ~ ch.temp[ch.temp$month==3, 27]) # Mar
summary(pfr.lm3)
pfr.lm4 <- lm(ch.temp[ch.temp$month==4, 30] ~ ch.temp[ch.temp$month==4, 27]) # Apr
summary(pfr.lm4)
pfr.lm5 <- lm(ch.temp[ch.temp$month==5, 30] ~ ch.temp[ch.temp$month==5, 27]) # May
summary(pfr.lm5)
pfr.lm6 <- lm(ch.temp[ch.temp$month==6, 30] ~ ch.temp[ch.temp$month==6, 27]) # Jun
summary(pfr.lm6)
pfr.lm7 <- lm(ch.temp[ch.temp$month==7, 30] ~ ch.temp[ch.temp$month==7, 27]) # Jul
summary(pfr.lm7)
pfr.lm8 <- lm(ch.temp[ch.temp$month==8, 30] ~ ch.temp[ch.temp$month==8, 27]) # Aug
summary(pfr.lm8)
pfr.lm9 <- lm(ch.temp[ch.temp$month==9, 30] ~ ch.temp[ch.temp$month==9, 27]) # Sep
summary(pfr.lm9)
pfr.lm10 <- lm(ch.temp[ch.temp$month==10, 30] ~ ch.temp[ch.temp$month==10, 27]) # Oct
summary(pfr.lm10)
pfr.lm11 <- lm(ch.temp[ch.temp$month==11, 30] ~ ch.temp[ch.temp$month==11, 27]) # Nov
summary(pfr.lm11)
pfr.lm12 <- lm(ch.temp[ch.temp$month==12, 30] ~ ch.temp[ch.temp$month==12, 27]) # Dec
summary(pfr.lm12)

shit <- cbind(coef(pfr.lm1),coef(pfr.lm2),coef(pfr.lm3),coef(pfr.lm4),coef(pfr.lm5),coef(pfr.lm6),
              coef(pfr.lm7),coef(pfr.lm8),coef(pfr.lm9),coef(pfr.lm10),coef(pfr.lm11),coef(pfr.lm12))
