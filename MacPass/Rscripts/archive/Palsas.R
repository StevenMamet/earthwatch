library(lme4)
library(arm) # Convenience displays with glm and glmer
library(plm) # To use 'phtest' to see if random effects are necessary
library(MuMIn) # For calculating pseudo R2
library(car) # Use for VIF
library(randomForest) # Random forest analyses
library(vegan) # RDA analyses
library(labdsv) # PCA
library(nlme) # gls 
library(multcomp) # Simultaneous Inference in General Parametric Models
# Produce letters from Tukey's HSD and plot above boxplot


setwd("~/Desktop/Workspace/MacPass")

###

rm(list = ls())

###################
### Lapse rates ###
###################

# These are the palsa area data up to 2016
lapse<-read.csv('lapse.rates.csv',skip=0)

## Export at 5 x 4
par(mfrow = c(2, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(1, 3, 0, 0), oma = c(2, 2, 2, 2)+0.2)

plot(lapse$day, lapse$HF, type='l', lwd = 2, ylim = c(-30,20), col = "red", axes = FALSE)
par(new=T)
plot(lapse$day, lapse$GF, type='l', lwd = 1.5, ylim = c(-30,20), axes = FALSE)
legend("topleft", "a) Air temperature", bty = "n", inset = c(-0.01,-0.1))
box()
axis(side = 2, at = seq(-30,20,10), labels = NA, cex = 0.5, lwd = 1, tck = -0.05)
axis(side = 2, at = seq(-30,20,10), labels = seq(-30,20,10), line = -0.5,tick = FALSE)
axis(side = 1, at = seq(1, 365, 25), labels = NA, cex = 0.5, lwd = 1, tck = -0.05)
mtext("Temperature (°C)", side=2, adj=0.5, line = 1.75)
legend("bottom", c("HF","GF"), 
       lwd=c(1.5,2),
       col=c("red","black"), horiz = TRUE, bty = "n") 

plot(lapse$day, lapse$lapse, type='l', lwd = 1.5, ylim = c(-1,1.2), axes = FALSE)
legend("topleft", "b) Lapse rates", bty = "n", inset = c(0.15,-0.1))
abline(h = 0, lty = 2)
box()
axis(side = 2, at = seq(-1,1.2,0.5), labels = NA, cex = 0.5, lwd = 1, tck = -0.05)
axis(side = 2, at = seq(-1,1.2,0.5), labels = seq(-1,1,0.5), line = -0.5, tick = FALSE)
axis(side = 1, at = seq(1, 365, 25), labels = NA, cex = 0.5, lwd = 1, tck = -0.05)
axis(side = 1, at = seq(1, 365, 50), labels = c(1,50,100,150,200,250,300,350), line = -0.5, tick = FALSE)
mtext("Lapse rate (°C/100 m)", side=2, adj=0.5, line = 1.75)
mtext("Julian day", side=1, adj=0.5, line = 1.75)



###################
### Shape Index ###
###################

# These are the palsa area data up to 2016
shape<-read.csv('ShapeIndex.csv',skip=0)

# Eyeball the trends at each site
with(shape, scatter.smooth(HF ~ year, span=7/8, las=1, xlim = c(1938,2018), ylim = c(1.2,1.5), yaxs = "i", xaxs = "i", xlab = "", ylab = ""))
with(shape, scatter.smooth(BP ~ year, span=7/8, las=1, xlim = c(1938,2018), ylim = c(1.2,1.5), yaxs = "i", xaxs = "i", xlab = "", ylab = ""))
with(shape, scatter.smooth(D6 ~ year, span=7/8, las=1, xlim = c(1938,2018), ylim = c(1.3,2.25), yaxs = "i", xaxs = "i", xlab = "", ylab = ""))
with(shape, scatter.smooth(D2 ~ year, span=7/8, las=1, xlim = c(1938,2018), ylim = c(0.915,0.945), yaxs = "i", xaxs = "i", xlab = "", ylab = ""))
with(shape, scatter.smooth(GF ~ year, span=7/8, las=1, xlim = c(1938,2018), ylim = c(0.9,2.5), yaxs = "i", xaxs = "i", xlab = "", ylab = ""))

# Now to plot the shape index curves all fancy-like
# Export at 3.5 x 5


par(mfrow = c(5, 1))
par(cex = 0.5)
par(mar = c(3, 3, 0, 0), oma = c(2, 2, 2, 2)+0.2)

with(shape, scatter.smooth(HF ~ year, span=7/8,  xlim = c(1938,2018), ylim = c(1.2,1.5), yaxs = "i", xaxs = "i", xlab = "", ylab = ""))
legend("topleft", "(a) HF (1260 m.a.s.l.)", bty = "n", inset = c(0,-0.19))

with(shape, scatter.smooth(BP ~ year, span=7/8,  xlim = c(1938,2018), ylim = c(1.2,1.5), yaxs = "i", xaxs = "i", xlab = "", ylab = ""))
legend("topleft", "(b) BP (1272 m.a.s.l.)", bty = "n", inset = c(0,-0.19))

with(shape, scatter.smooth(D6 ~ year, span=7/8,  xlim = c(1938,2018), ylim = c(1.3,2.25), yaxs = "i", xaxs = "i", xlab = "", ylab = ""))
legend("topleft", "(c) D6 (1473 m.a.s.l.)", bty = "n", inset = c(0,-0.19))

with(shape[-38,], scatter.smooth(D2 ~ year, span=7/8,  xlim = c(1938,2018), ylim = c(0.925,0.95), yaxs = "i", xaxs = "i", xlab = "", ylab = ""))
par(new = T)
# plot(shape[38,],  xlim = c(1938,2018), ylim = c(0.9,2.5), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
legend("topleft", "(d) D2 (1477 m.a.s.l.)", bty = "n", inset = c(0,-0.19))

with(shape, scatter.smooth(GF ~ year, span=7/8,  xlim = c(1938,2018), ylim = c(0.9,2.5), yaxs = "i", xaxs = "i", xlab = "", ylab = ""))
legend("topleft", "(e) GF (1621 m.a.s.l.)", bty = "n", inset = c(0,-0.19))

mtext("Shape index", side=2, outer=TRUE, cex = 0.5, adj=0.5)
mtext("Year", side=1, outer=TRUE, cex=0.5, adj=0.5)


###################
### Palsa areas ###
###################

# These are the palsa area data up to 2016
areas<-read.csv('PalsaAreas.csv',skip=0)

# Convert the areas from m2 to hectares
areas$HF <- areas$HF/10000
areas$BP <- areas$BP/10000
areas$D6 <- areas$D6/10000
areas$D2 <- areas$D2/10000
areas$GF <- areas$GF/10000

## The areas range from 0.044 to 1.2112 ha over the record

# Eyeball the trends at each site
with(areas, scatter.smooth(HF ~ year, span=7/8, pch=16, cex=.6, las=1))
with(areas, scatter.smooth(BP ~ year, span=7/8, pch=16, cex=.6, las=1))
with(areas, scatter.smooth(D6 ~ year, span=7/8, pch=16, cex=.6, las=1))#, ylim = c(-10, 10)))
with(areas, scatter.smooth(D2 ~ year, span=7/8, pch=16, cex=.6, las=1))
with(areas, scatter.smooth(GF ~ year, span=7/8, pch=16, cex=.6, las=1))

# Based on the eyeballing above, fit a non-linear function
hf.out <-  lm(HF ~ year + I(log(year)), data = areas)
bp.out <-  lm(BP ~ year + I(log(year)), data = areas)
d6.out <-  lm(D6 ~ year + I(year^-2), data = areas)
d2.out <-  lm(D2 ~ year + I(year^-2), data = areas)
gf.out <-  lm(GF ~ year + I(year^-2), data = areas)

# hf.out <- lm(HF ~ log(year), data = areas)
# bp.out <- lm(BP ~ log(year), data = areas)
# d6.out <- lm(D6 ~ sqrt(year), data = areas)
# d2.out <- lm(D2 ~ sqrt(year), data = areas)
# gf.out <- lm(GF ~ sqrt(year), data = areas)

# If the coefficients are significant, you've nailed it
summary(hf.out) # R2 = 0.9996, F2,2 = 2585, P = 0.0003868
summary(bp.out) # R2 = 0.9998, F2,2 = 5438, P = 0.0001839
summary(d6.out) # R2 = 0.999, F2,2 = 963,  P = 0.001071
summary(d2.out) # R2 = 0.9901, F2,2 = 100.4, P = 0.009864
summary(gf.out) # R2 = 0.9773, F2,3 = 64.57, P = 0.00342
# 
# summary(hf.out) # R2 = 0.974, F1,3 = 110.4, P = 0.00184
# summary(bp.out) # R2 = 0.951, F1,3 = 59.18, P = 0.00456
# summary(d6.out) # R2 = 0.910, F1,3 = 30.34, P = 0.01178
# summary(d2.out) # R2 = 0.970, F1,3 = 95.65, P = 0.002272
# summary(gf.out) # R2 = 0.894, F1,4 = 33.80, P = 0.004358

# There's a rounding error when I get the coefficients from above, so need to get all the decimal places below
coef(hf.out)
coef(bp.out)
coef(d6.out)
coef(d2.out)
coef(gf.out)

# Add the fitted curve to the scatter plot
curve(-1297.3510962 - 0.1034332*x + 197.9226065*(log(x)), add=T, col="red") # HF
curve(-2382.157085 - 0.187717*x + 362.852054*(log(x)), add=T, col="red") # BP
curve(1110.921291 + 0.551193*x - 49.488872*(sqrt(x)) , add=T, col="red") # D6
curve(271.8596019 + 0.1325181*x - 12.0027313*(sqrt(x)) , add=T, col="red") # D2
curve(3677.497833 + 1.827379*x - 163.952388*(sqrt(x)) , add=T, col="red") # GF

# curve(53.271923 - 6.987048*(log(x)), add=T, col="red") # HF
# curve(69.220937 - 9.054912*(log(x)), add=T, col="red") # BP
# curve(17.0202919 - 0.3777627*(sqrt(x)) , add=T, col="red") # D6
# curve(8.8633355 - 0.1954121*(sqrt(x)) , add=T, col="red") # D2
# curve(59.463616 - 1.325985*(sqrt(x)) , add=T, col="red") # GF

# Now to plot the decay curves all fancy-like
# Export at 3.5 x 5

par(mfrow = c(5, 1))
par(cex = 0.5)
par(mar = c(3, 3, 0, 0), oma = c(2, 2, 2, 2)+0.2)

plot(areas$year, areas$HF, type='n', xlim = c(1938,2018), ylim = c(0,0.4), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
points(areas$year, areas$HF, col = 'black', xlim = c(1938,2018), ylim = c(0,0.4), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
curve(-1297.3510962 - 0.1034332*x + 197.9226065*(log(x)), add=T, col="black", lty = 1) # HF
legend("topright", "(a) HF (1260 m.a.s.l.)", bty = "n", inset = c(0.23,-0.19))

plot(areas$year, areas$BP, type='n', xlim = c(1938,2018), ylim = c(0.2,0.7), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
points(areas$year, areas$BP, col = 'black', xlim = c(1938,2018), ylim = c(0.2,0.7), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
curve(-2382.157085 - 0.187717*x + 362.852054*(log(x)), add=T, col="black", lty = 1) # BP
legend("topright", "(b) BP (1272 m.a.s.l.)", bty = "n", inset = c(0.23,-0.19))

plot(areas$year, areas$D6, type='n', xlim = c(1938,2018), ylim = c(0,0.5), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
points(areas$year, areas$D6, col = 'black', xlim = c(1938,2018), ylim = c(0,0.5), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
curve(-2.655744e+02 + 8.785359e-02*x + 3.598614e+08*(x^-2), add=T, col="black", lty = 1) # D6
legend("topright", "(c) D6 (1473 m.a.s.l.)", bty = "n", inset = c(0.23,-0.19))

plot(areas$year, areas$D2, type='n', xlim = c(1938,2018), ylim = c(0.06,0.3), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
points(areas$year, areas$D2, col = 'black', xlim = c(1938,2018), ylim = c(0.06,0.3), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
curve(-6.211051e+01 + 2.018395e-02*x + 8.744027e+07*(x^-2), add=T, col="black", lty = 1) # D2
legend("topright", "(d) D2 (1477 m.a.s.l.)", bty = "n", inset = c(0.23,-0.19))

plot(areas$year, areas$GF, type='n', xlim = c(1938,2018), ylim = c(0,1.4), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
points(areas$year, areas$GF, col = 'black', xlim = c(1938,2018), ylim = c(0,1.4), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
curve(-8.817683e+02 + 2.920596e-01*x + 1.190934e+09*(x^-2), add=T, col="black", lty = 1) # GF
legend("topright", "(e) GF (1621 m.a.s.l.)", bty = "n", inset = c(0.23,-0.19))

mtext("Palsa area (ha)", side=2, outer=TRUE, cex = 0.5, adj=0.5)
mtext("Year", side=1, outer=TRUE, cex=0.5, adj=0.5)


plot(areas$HF~shape$HF)
abline(lm(areas$HF~shape$HF))
plot(areas$BP~shape$BP)
plot(areas$D6~shape$D6)
plot(areas$D2~shape$D2)
plot(areas$GF~shape$GF)

summary(lm(areas$HF~shape$HF))

  cor.test(areas$HF, shape$HF)
cor.test(areas$BP, shape$BP)
cor.test(areas$D6, shape$D6)
cor.test(areas$D2, shape$D2)
cor.test(areas$GF, shape$GF)


####################
### Microclimate ###
####################

temps<-read.csv('MM_temps_2015.csv',skip=0)

par(mfrow = c(2, 2))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(0.5, 0.5, 0.5, 0.5), oma = c(3.5,3,1,1.5))
# Export at 3.40 x 4.27 for 2-panel

## Panel A
plot(temps$cru.warm ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 1, type = "l",
     xlim = c(1989,2016), ylim = c(3,12), lty = 2, axes = FALSE)
par(new=T)
plot(temps$ec.warm ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 1, type = "l",
     xlim = c(1989,2016), ylim = c(3,12), lty = 3, axes = FALSE)
par(new=T)
plot(temps$hf.warm ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "darkred", 
     type = "l", xlim = c(1989,2016), ylim = c(3,12), axes = FALSE)
par(new=T)
plot(temps$bp.warm ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "red", 
     type = "l", xlim = c(1989,2016), ylim = c(3,12), axes = FALSE)
par(new=T)
plot(temps$d2.warm ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "yellow2", 
     type = "l", xlim = c(1989,2016), ylim = c(3,12), axes = FALSE)
par(new=T)
plot(temps$d6.warm ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "darkorange1", 
     type = "l", xlim = c(1989,2016), ylim = c(3,12), axes = FALSE)
par(new=T)
plot(temps$gf.warm ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "darkblue", 
     type = "l", xlim = c(1989,2016), ylim = c(3,12), axes = FALSE)
box()
axis(side = 2, at = seq(4,12,2), labels = NA, lwd = 1, tck = -0.05)
axis(side = 2, at = seq(4,12,2), labels = seq(4,12,2), line = -0.5, tick = FALSE)
axis(side = 1, at = seq(1990, 2015, 5), labels = NA, lwd = 1, tck = -0.05)
legend("topleft", "a) June-September", bty = "n", inset = c(-0.1,-0.1))

## Panel C
plot(temps$pre.warm ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 1, type = "l",
     xlim = c(1989,2016), ylim = c(190,275), lty = 2, axes = FALSE)
box()
axis(side = 4, at = seq(200,260,20), labels = NA, lwd = 1, tck = -0.05)
# text(par("usr")[2] + 1.5, seq(200,260,20), labels = seq(200,260,20), srt = -90, adj = c(0.5,0.5), xpd = TRUE)
axis(side = 1, at = seq(1990, 2015, 5), labels = NA, lwd = 1, tck = -0.05)
legend("topleft", "c) June-September", bty = "n", inset = c(-0.1,-0.1))

## Panel B
plot(temps$cru.cold ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 1, type = "l",
     xlim = c(1989,2016), ylim = c(-17,-10), lty = 2, axes = FALSE)
par(new=T)
plot(temps$ec.cold ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 1, type = "l",
     xlim = c(1989,2016), ylim = c(-17,-10), lty = 3, axes = FALSE)
par(new=T)
plot(temps$hf.cold ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "darkred", 
     type = "l", xlim = c(1989,2016), ylim = c(-17,-10), axes = FALSE)
par(new=T)
plot(temps$bp.cold ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "red", 
     type = "l", xlim = c(1989,2016), ylim = c(-17,-10), axes = FALSE)
par(new=T)
plot(temps$d2.cold ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "yellow2", 
     type = "l", xlim = c(1989,2016), ylim = c(-17,-10), axes = FALSE)
par(new=T)
plot(temps$d6.cold ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "darkorange1", 
     type = "l", xlim = c(1989,2016), ylim = c(-17,-10), axes = FALSE)
par(new=T)
plot(temps$gf.cold ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "darkblue", 
     type = "l", xlim = c(1989,2016), ylim = c(-17,-10), axes = FALSE)
box()
axis(side = 2, at = seq(-16,-10,2), labels = NA, lwd = 1, tck = -0.05)
axis(side = 2, at = seq(-16,-10,2), labels = seq(-16,-10,2), line = -0.5, tick = FALSE)
axis(side = 1, at = seq(1990, 2015, 5), labels = NA, lwd = 1, tck = -0.05)
axis(side = 1, at = seq(1990, 2015, 5), labels = seq(1990, 2015, 5), line = -0.5, tick = FALSE)
legend("topleft", "b) October-April", bty = "n", inset = c(-0.1,-0.1))

## Panel D
plot(temps$pre.cold ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = 1, type = "l",
     xlim = c(1989,2016), ylim = c(190,275), lty = 2, axes = FALSE)
box()
axis(side = 4, at = seq(200,260,20), labels = NA, lwd = 1, tck = -0.05)
# text(par("usr")[2] + 1.5, seq(200,260,20), labels = seq(200,260,20), srt = -90, adj = c(0.5,0.5), xpd = TRUE)
axis(side = 1, at = seq(1990, 2015, 5), labels = NA, lwd = 1, tck = -0.05)
axis(side = 1, at = seq(1990, 2015, 5), labels = seq(1990, 2015, 5), line = -0.5, tick = FALSE)
legend("topleft", "d) October-April", bty = "n", inset = c(-0.1,-0.1))

mtext("Temperature (°C)", side = 2, line = 1.1, outer = TRUE)
mtext("Year", side = 1, line = 1.1, outer = TRUE)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", c("CRU","EC","HF","BP","D2","D6","GF"), lty=c(2,3,1,1,1,1,1), 
       lwd=c(2.5,2.5,2.5,2.5,2.5,2.5,2.5),
       col=c("black","black","darkred","red","yellow2","darkorange1","darkblue"), horiz = TRUE,
       xpd = TRUE, bty = "n") 


##### Plot the ground temperatures

par(mfrow = c(2, 2))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 9 pts
par(mar = c(0.5, 0.5, 0.5, 0.5), oma = c(3.5,3,1,1.5))
# Export at 3.40 x 4.27 for 2-panel

## Panel A
plot(temps$hf.wst.0 ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "darkred", 
     type = "l", xlim = c(1989,2016), ylim = c(2.7,12), axes = FALSE)
par(new=T)
plot(temps$bp.wst.0 ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "red", 
     type = "l", xlim = c(1989,2016), ylim = c(2.7,12), axes = FALSE)
par(new=T)
plot(temps$d2.wst.0 ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "yellow2", 
     type = "l", xlim = c(1989,2016), ylim = c(2.7,12), axes = FALSE)
par(new=T)
plot(temps$d6.wst.0 ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "darkorange1", 
     type = "l", xlim = c(1989,2016), ylim = c(2.7,12), axes = FALSE)
abline(coef(lm(temps[c(91:114),35]~temps[c(91:114),1])), col = "darkorange1", lty = 2, lwd = 2)
par(new=T)
plot(temps$gf.wst.0 ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "darkblue", 
     type = "l", xlim = c(1989,2016), ylim = c(2.7,12), axes = FALSE)
abline(coef(lm(temps[c(91:114),33]~temps[c(91:114),1])), col = "darkblue", lty = 2, lwd = 2)
box()
axis(side = 2, at = seq(4,12,2), labels = NA, lwd = 1, tck = -0.05)
axis(side = 2, at = seq(4,12,2), labels = seq(4,12,2), line = -0.5, tick = FALSE)
axis(side = 1, at = seq(1990, 2015, 5), labels = NA, lwd = 1, tck = -0.05)
legend("topleft", "a) June-September", bty = "n", inset = c(-0.1,-0.1))

## Panel C
plot(temps$hf.wst.150 ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "darkred", 
     type = "l", xlim = c(1989,2016), ylim = c(-1.75,0.5), axes = FALSE)
abline(coef(lm(temps[c(91:114),42]~temps[c(91:114),1])), col = "darkred", lty = 2, lwd = 2)
par(new=T)
plot(temps$bp.wst.150 ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "red", 
     type = "l", xlim = c(1989,2016), ylim = c(-1.75,0.5), axes = FALSE)
abline(coef(lm(temps[c(91:114),41]~temps[c(91:114),1])), col = "red", lty = 2, lwd = 2)
par(new=T)
plot(temps$d2.wst.150 ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "yellow2", 
     type = "l", xlim = c(1989,2016), ylim = c(-1.75,0.5), axes = FALSE)
abline(coef(lm(temps[c(91:114),44]~temps[c(91:114),1])), col = "yellow2", lty = 2, lwd = 2)
par(new=T)
plot(temps$d6.wst.150 ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "darkorange1", 
     type = "l", xlim = c(1989,2016), ylim = c(-1.75,0.5), axes = FALSE)
par(new=T)
plot(temps$gf.wst.150 ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "darkblue", 
     type = "l", xlim = c(1989,2016), ylim = c(-1.75,0.5), axes = FALSE)
abline(coef(lm(temps[c(91:114),43]~temps[c(91:114),1])), col = "darkblue", lty = 2, lwd = 2)
box()
axis(side = 4, at = seq(-2,1,0.5), labels = NA, lwd = 1, tck = -0.05)
axis(side = 1, at = seq(1990, 2015, 5), labels = NA, lwd = 1, tck = -0.05)
legend("topleft", "c) June-September", bty = "n", inset = c(-0.1,-0.1))

## Panel B
plot(temps$hf.cst.0 ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "darkred", 
     type = "l", xlim = c(1989,2016), ylim = c(-14.5,0), axes = FALSE)
abline(coef(lm(temps[c(91:114),27]~temps[c(91:114),1])), col = "darkred", lty = 2, lwd = 2)
par(new=T)
plot(temps$bp.cst.0 ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "red", 
     type = "l", xlim = c(1989,2016), ylim = c(-14.5,0), axes = FALSE)
par(new=T)
plot(temps$d2.cst.0 ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "yellow2", 
     type = "l", xlim = c(1989,2016), ylim = c(-14.5,0), axes = FALSE)
par(new=T)
plot(temps$d6.cst.0 ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "darkorange1", 
     type = "l", xlim = c(1989,2016), ylim = c(-14.5,0), axes = FALSE)
par(new=T)
plot(temps$gf.cst.0 ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "darkblue", 
     type = "l", xlim = c(1989,2016), ylim = c(-14.5,0), axes = FALSE)
abline(coef(lm(temps[c(91:114),28]~temps[c(91:114),1])), col = "darkblue", lty = 2, lwd = 2)
box()
axis(side = 2, at = seq(-15,0,3), labels = NA, lwd = 1, tck = -0.05)
axis(side = 2, at = seq(-15,0,3), labels = seq(-15,0,3), line = -0.5, tick = FALSE)
axis(side = 1, at = seq(1990, 2015, 5), labels = NA, lwd = 1, tck = -0.05)
axis(side = 1, at = seq(1990, 2015, 5), labels = seq(1990, 2015, 5), line = -0.5, tick = FALSE)
legend("topleft", "b) October-April", bty = "n", inset = c(-0.1,-0.1))

## Panel D
plot(temps$hf.cst.150 ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "darkred", 
     type = "l", xlim = c(1989,2016), ylim = c(-4,1), axes = FALSE)
abline(coef(lm(temps[c(91:114),37]~temps[c(91:114),1])), col = "darkred", lty = 2, lwd = 2)
par(new=T)
plot(temps$bp.cst.150 ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "red", 
     type = "l", xlim = c(1989,2016), ylim = c(-4,1), axes = FALSE)
par(new=T)
plot(temps$d2.cst.150 ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "yellow2", 
     type = "l", xlim = c(1989,2016), ylim = c(-4,1), axes = FALSE)
abline(coef(lm(temps[c(91:114),39]~temps[c(91:114),1])), col = "yellow2", lty = 2, lwd = 2)
par(new=T)
plot(temps$d6.cst.150 ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "darkorange1", 
     type = "l", xlim = c(1989,2016), ylim = c(-4,1), axes = FALSE)
par(new=T)
plot(temps$gf.cst.150 ~ temps$Temperature, yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "darkblue", 
     type = "l", xlim = c(1989,2016), ylim = c(-4,1), axes = FALSE)
abline(coef(lm(temps[c(91:114),38]~temps[c(91:114),1])), col = "darkblue", lty = 2, lwd = 2)
box()
axis(side = 4, at = seq(-4,0,1), labels = NA, lwd = 1, tck = -0.05)
# text(par("usr")[2] + 1.5, seq(200,260,20), labels = seq(200,260,20), srt = -90, adj = c(0.5,0.5), xpd = TRUE)
axis(side = 1, at = seq(1990, 2015, 5), labels = NA, lwd = 1, tck = -0.05)
axis(side = 1, at = seq(1990, 2015, 5), labels = seq(1990, 2015, 5), line = -0.5, tick = FALSE)
legend("topleft", "d) October-April", bty = "n", inset = c(-0.1,-0.1))

mtext("Temperature (°C)", side = 2, line = 1.1, outer = TRUE)
mtext("Year", side = 1, line = 1.1, outer = TRUE)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", c("HF","BP","D2","D6","GF"), lty=c(1,1,1,1,1), 
       lwd=c(2.5,2.5,2.5,2.5,2.5),
       col=c("darkred","red","yellow2","darkorange1","darkblue"), horiz = TRUE,
       xpd = TRUE, bty = "n") 


anova(lm(Temperature~Meth,temps))

## Cold temperature season
# Air:
summary(lm(temps[c(99:114),9]~temps[c(99:114),1])) # CU: Not sig
summary(lm(temps[c(99:114),10]~temps[c(99:114),1])) # EC: Not sig
summary(lm(temps[c(91:114),12]~temps[c(91:114),1])) # HF: Not sig
summary(lm(temps[c(91:114),11]~temps[c(91:114),1])) # BP: Not sig
summary(lm(temps[c(91:114),13]~temps[c(91:114),1])) # D2: Not sig
summary(lm(temps[c(97:114),14]~temps[c(97:114),1])) # D6: Not sig
summary(lm(temps[c(91:114),15]~temps[c(91:114),1])) # GF: Not sig
# 0:
summary(lm(temps[c(91:114),27]~temps[c(91:114),1])) # HF: SIG  (F1,22 = 8.150, P = 0.009)
summary(lm(temps[c(91:114),26]~temps[c(91:114),1])) # BP: Not sig
summary(lm(temps[c(91:114),29]~temps[c(91:114),1])) # D2: Not sig
summary(lm(temps[c(97:114),30]~temps[c(97:114),1])) # D6: Not sig
summary(lm(temps[c(91:114),28]~temps[c(91:114),1])) # GF: SIG  (F1,22 = 9.197, P = 0.006)
# -150:
summary(lm(temps[c(91:114),37]~temps[c(91:114),1])) # HF: SIG (F1,22 = 15.640, P < 0.001)
summary(lm(temps[c(91:114),36]~temps[c(91:114),1])) # BP: Not sig
summary(lm(temps[c(91:114),39]~temps[c(91:114),1])) # D2: SIG  (F1,22 = 8.801, P = 0.007)
summary(lm(temps[c(97:114),40]~temps[c(97:114),1])) # D6: Not sig
summary(lm(temps[c(91:114),38]~temps[c(91:114),1])) # GF: SIG  (F1,22 = 17.18, P < 0.001)

## Warm temperature season
# Air:
summary(lm(temps[c(99:114),16]~temps[c(99:114),1])) # CU: Not sig
summary(lm(temps[c(99:114),17]~temps[c(99:114),1])) # EC: Not sig
summary(lm(temps[c(91:114),19]~temps[c(91:114),1])) # HF: Not sig
summary(lm(temps[c(91:114),18]~temps[c(91:114),1])) # BP: SIG (F1,22=6.69,P=0.017)
summary(lm(temps[c(91:114),20]~temps[c(91:114),1])) # D2: Not sig
summary(lm(temps[c(97:114),21]~temps[c(97:114),1])) # D6: Not sig
summary(lm(temps[c(91:114),22]~temps[c(91:114),1])) # GF: Not sig
# 0:
summary(lm(temps[c(91:114),32]~temps[c(91:114),1])) # HF: Not sig
summary(lm(temps[c(91:114),31]~temps[c(91:114),1])) # BP: Not sig
summary(lm(temps[c(91:114),34]~temps[c(91:114),1])) # D2: Not sig
summary(lm(temps[c(97:114),35]~temps[c(97:114),1])) # D6: SIG (F1,16=5.28,P=0.035)
summary(lm(temps[c(91:114),33]~temps[c(91:114),1])) # GF: SIG (F1,22=19.12,P<0.001)
# -150:
summary(lm(temps[c(91:114),42]~temps[c(91:114),1])) # HF: SIG (F1,22=30.52,P<0.001)
summary(lm(temps[c(91:114),41]~temps[c(91:114),1])) # BP: SIG (F1,22=10.75,P=0.003)
summary(lm(temps[c(91:114),44]~temps[c(91:114),1])) # D2: SIG (F1,22=16.60,P<0.001)
summary(lm(temps[c(97:114),45]~temps[c(97:114),1])) # D6: Not sig
summary(lm(temps[c(91:114),43]~temps[c(91:114),1])) # GF: SIG (F1,22=27.78,P<0.001)

# Precipitation
summary(lm(temps[c(99:114),24]~temps[c(99:114),1])) # EC: Not sig
summary(lm(temps[c(91:114),25]~temps[c(91:114),1])) # HF: Not sig

## Annual temperature
summary(lm(temps[c(99:114),2]~temps[c(99:114),1])) # CU: Not sig
summary(lm(temps[c(99:114),3]~temps[c(99:114),1])) # EC: Not sig
summary(lm(temps[c(94:114),4]~temps[c(94:114),1])) # HF: Not sig
summary(lm(temps[c(94:114),5]~temps[c(94:114),1])) # BP: Not sig
summary(lm(temps[c(94:114),6]~temps[c(94:114),1])) # D2: Not sig
summary(lm(temps[c(97:114),7]~temps[c(97:114),1])) # D6: Not sig
summary(lm(temps[c(94:114),8]~temps[c(94:114),1])) # GF: Not sig

plot(temps[c(95:114),8] ~ temps[c(95:114),1], yaxs = "i", xaxs = "i", xlab = "", ylab = "", col = "darkorange1", 
     type = "l", xlim = c(1989,2016), ylim = c(-10,-4))
box()

BP<-read.csv('BP.csv',skip=2)
HF<-read.csv('HF.csv',skip=2)
GF<-read.csv('GF.csv',skip=2)
D2<-read.csv('D2.csv',skip=2)
D6<-read.csv('D6.csv',skip=2)

BP$Season<-NA
HF$Season<-NA
GF$Season<-NA
D2$Season<-NA
D6$Season<-NA

BP$Season[BP$Month%in%c(5,6,7,8,9)]<-'WST'
BP$Season[BP$Month%in%c(1,2,3,4,10,11,12)]<-'CST'
HF$Season[HF$Month%in%c(5,6,7,8,9)]<-'WST'
HF$Season[HF$Month%in%c(1,2,3,4,10,11,12)]<-'CST'
GF$Season[GF$Month%in%c(5,6,7,8,9)]<-'WST'
GF$Season[GF$Month%in%c(1,2,3,4,10,11,12)]<-'CST'
D2$Season[D2$Month%in%c(5,6,7,8,9)]<-'WST'
D2$Season[D2$Month%in%c(1,2,3,4,10,11,12)]<-'CST'
D6$Season[D6$Month%in%c(5,6,7,8,9)]<-'WST'
D6$Season[D6$Month%in%c(1,2,3,4,10,11,12)]<-'CST'

BP$Season<-as.factor(BP$Season)
HF$Season<-as.factor(HF$Season)
GF$Season<-as.factor(GF$Season)
D2$Season<-as.factor(D2$Season)
D6$Season<-as.factor(D6$Season)

### Plotting the thaw depth data for figure 5

thaw.mm <- read.csv(file = "~/Desktop/Workspace/Earthwatch/thaw.mm.csv", header = TRUE)
thaw.mm$ci <- thaw.mm$se*qt(0.975, thaw.mm$n-1)
thaw.mm$c5 <- thaw.mm$mean - thaw.mm$ci
thaw.mm$c95 <- thaw.mm$mean + thaw.mm$ci

d2 <- subset(thaw.mm, site == "D2")
d6 <- subset(thaw.mm, site == "D6")
beaver <- subset(thaw.mm, site == "Beaver")
hare <- subset(thaw.mm, site == "Hare")
goose <- subset(thaw.mm, site == "Goose")

goose.lm <- lm(goose$mean~goose$year)
d2.lm <- lm(d2$mean~d2$year)
d6.lm <- lm(d6$mean~d6$year)
beaver.lm <- lm(beaver$mean~beaver$year)
hare.lm <- lm(hare$mean~hare$year)
summary(goose.lm) # Sig (b1 = 1.743, F1,24 = 20.69, P = 0.0001309)
summary(d2.lm) # Sig (b1 = 0.8194, F1,24 = 59.3, P < 0.001)
summary(d6.lm) # --
summary(beaver.lm) # Sig (b1 = 0.3915, F1,24 = 6.635, P = 0.01659)
summary(hare.lm) # --

hf.nls <- lm(hare$mean ~ hare$year + I(hare$year^-2) + log(hare$year))
bp.nls <- lm(beaver$mean ~ beaver$year + I(beaver$year^2) + I(beaver$year^-2) + I(log(beaver$year)))
d6.nls <- lm(d6$mean ~ d6$year + I(d6$year^2) + I(d6$year^-2) + I(log(d6$year)))
d2.nls <- lm(d2$mean ~ d2$year + I(d2$year^2) + I(d2$year^-2) + I(log(d2$year)))
gf.nls <- lm(goose$mean ~ goose$year + I(goose$year^2) + I(goose$year^-2) + I(log(goose$year)))

summary(hf.nls)
summary(bp.nls)
summary(d6.nls)
summary(d2.nls)
summary(gf.nls)

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

plot(d6$year, d6$mean, type='n', xlim = c(1990,2014), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(d6$year, d6$c5, col = 'lightgrey', xlim = c(1990,2014), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(d6$year, d6$c95, col = 'lightgrey', xlim = c(1990,2014), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(d6$year, rev(d6$year)), c(d6$c95, rev(d6$c5)), col = "lightgrey", border = NA)
par(new=T)
plot(d6$year, d6$mean, xlim = c(1990,2014), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "black", xlab = "Year", ylab = "Thaw depth (cm)")
legend("topleft", "(d) D6 (1473 m.a.s.l.)", bty = "n", inset = c(0,-0.05))

plot(d2$year, d2$mean, type='n', xlim = c(1990,2014), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(d2$year, d2$c5, col = 'lightgrey', xlim = c(1990,2014), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(d2$year, d2$c95, col = 'lightgrey', xlim = c(1990,2014), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(d2$year, rev(d2$year)), c(d2$c95, rev(d2$c5)), col = "lightgrey", border = NA)
par(new=T)
plot(d2$year, d2$mean, xlim = c(1990,2014), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "black", xlab = "Year", ylab = "Thaw depth (cm)")
legend("topleft", "(c) D2 (1477 m.a.s.l.)", bty = "n", inset = c(0,-0.05))
abline(coef(d2.lm),lty=2)

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

### Regression data

# logit.mp <- read.csv('master.csv',header = T) # Thaw is univariate
logit.mp <- read.csv('master2.csv',header = T) # Thaw is multivariate - 1997 to 2014 (separate lines)
logit.mp <- read.csv('8Nov2016.thaw.csv',header = T)
# logit.mp <- read.csv('master4.csv',header = T) # Thaw is multivariate (separate lines) and I've subset to
# 1997 to 2014 - the data are normalized
# logit.mp <- read.csv('master5.csv',header = T) # Thaw is multivariate (separate lines) and I've subset to
# 1997 to 2014 - the data are not normalized

# November 8, 2016 - redoing the analyses because Sharon says so
logit.mp2 <- decostand(logit.mp[c(13:20)], "standardize")
apply(logit.mp2, 2, mean)  # means = 0
apply(logit.mp2, 2, sd)  	# standard deviations = 1
logit.mp2 <- cbind(year = logit.mp$year,site = logit.mp$site,line = logit.mp$line,
                   thaw = logit.mp$thaw, logit.mp2)

hf <- subset(logit.mp2, site == "HF")
bp <- subset(logit.mp2, site == "BP")
d6 <- subset(logit.mp2, site == "D6")
d2 <- subset(logit.mp2, site == "D2")
gf <- subset(logit.mp2, site == "GF")

with(hf, scatter.smooth(thaw ~ year, span=7/8, las=1, yaxs = "i", xaxs = "i", xlab = "", ylab = ""))
with(bp, scatter.smooth(thaw ~ year, span=7/8, las=1, yaxs = "i", xaxs = "i", xlab = "", ylab = ""))
with(d6, scatter.smooth(thaw ~ year, span=7/8, las=1, yaxs = "i", xaxs = "i", xlab = "", ylab = ""))
with(d2, scatter.smooth(thaw ~ year, span=7/8, las=1, yaxs = "i", xaxs = "i", xlab = "", ylab = ""))
with(gf, scatter.smooth(thaw ~ year, span=7/8, las=1, yaxs = "i", xaxs = "i", xlab = "", ylab = ""))

with(hf, scatter.smooth(thaw ~ TI_0, span=7/8, las=1, yaxs = "i", xaxs = "i", xlab = "", ylab = ""))
with(bp, scatter.smooth(thaw ~ TI_0, span=7/8, las=1, yaxs = "i", xaxs = "i", xlab = "", ylab = ""))
with(d6, scatter.smooth(thaw ~ TI_0, span=7/8, las=1, yaxs = "i", xaxs = "i", xlab = "", ylab = ""))
with(d2, scatter.smooth(thaw ~ TI_0, span=7/8, las=1, yaxs = "i", xaxs = "i", xlab = "", ylab = ""))
with(gf, scatter.smooth(thaw ~ TI_0, span=7/8, las=1, yaxs = "i", xaxs = "i", xlab = "", ylab = ""))

summary(lm(log(thaw) ~ year + I(year^2), data = logit.mp2[logit.mp2$site=='HF',])) # Non-linear
summary(lm(log(thaw) ~ year + I(year^2), data = logit.mp2[logit.mp2$site=='BP',])) # Non-linear
summary(lm(log(thaw) ~ year + I(year^2), data = logit.mp2[logit.mp2$site=='D6',]))
summary(lm(log(thaw) ~ year + I(year^2), data = logit.mp2[logit.mp2$site=='D2',]))
summary(lm(log(thaw) ~ year + I(year^2), data = logit.mp2[logit.mp2$site=='GF',]))

summary(lm(log(thaw) ~ TI_0 + I(TI_0^2), data = logit.mp2[logit.mp2$site=='HF',]))
summary(lm(log(thaw) ~ TI_0 + I(TI_0^2), data = logit.mp2[logit.mp2$site=='BP',])) # Very non-linear
summary(lm(log(thaw) ~ TI_0 + I(TI_0^2), data = logit.mp2[logit.mp2$site=='D6',]))
summary(lm(log(thaw) ~ TI_0 + I(TI_0^2), data = logit.mp2[logit.mp2$site=='D2',]))
summary(lm(log(thaw) ~ TI_0 + I(TI_0^2), data = logit.mp2[logit.mp2$site=='GF',]))

summary(lm(log(thaw) ~ FI_0 + I(FI_0^2), data = logit.mp2[logit.mp2$site=='HF',]))
summary(lm(log(thaw) ~ FI_0 + I(FI_0^2), data = logit.mp2[logit.mp2$site=='BP',]))
summary(lm(log(thaw) ~ FI_0 + I(FI_0^2), data = logit.mp2[logit.mp2$site=='D6',]))
summary(lm(log(thaw) ~ FI_0 + I(FI_0^2), data = logit.mp2[logit.mp2$site=='D2',]))
summary(lm(log(thaw) ~ FI_0 + I(FI_0^2), data = logit.mp2[logit.mp2$site=='GF',])) # Non-linear

summary(lm(log(thaw) ~ WSP_CRU + I(WSP_CRU^2), data = logit.mp2[logit.mp2$site=='HF',]))
summary(lm(log(thaw) ~ WSP_CRU + I(WSP_CRU^2), data = logit.mp2[logit.mp2$site=='BP',]))
summary(lm(log(thaw) ~ WSP_CRU + I(WSP_CRU^2), data = logit.mp2[logit.mp2$site=='D6',]))
summary(lm(log(thaw) ~ WSP_CRU + I(WSP_CRU^2), data = logit.mp2[logit.mp2$site=='D2',]))
summary(lm(log(thaw) ~ WSP_CRU + I(WSP_CRU^2), data = logit.mp2[logit.mp2$site=='GF',])) # non-linear

summary(lm(log(thaw) ~ CSP_CRU + I(CSP_CRU^2), data = logit.mp2[logit.mp2$site=='HF',]))
summary(lm(log(thaw) ~ CSP_CRU + I(CSP_CRU^2), data = logit.mp2[logit.mp2$site=='BP',])) 
summary(lm(log(thaw) ~ CSP_CRU + I(CSP_CRU^2), data = logit.mp2[logit.mp2$site=='D6',]))
summary(lm(log(thaw) ~ CSP_CRU + I(CSP_CRU^2), data = logit.mp2[logit.mp2$site=='D2',])) # Non-linear
summary(lm(log(thaw) ~ CSP_CRU + I(CSP_CRU^2), data = logit.mp2[logit.mp2$site=='GF',]))

summary(lm(log(thaw) ~ late.TDD + I(late.TDD^2), data = logit.mp2[logit.mp2$site=='HF',]))
summary(lm(log(thaw) ~ late.TDD + I(late.TDD^2), data = logit.mp2[logit.mp2$site=='BP',])) # Non-linear
summary(lm(log(thaw) ~ late.TDD + I(late.TDD^2), data = logit.mp2[logit.mp2$site=='D6',]))
summary(lm(log(thaw) ~ late.TDD + I(late.TDD^2), data = logit.mp2[logit.mp2$site=='D2',])) 
summary(lm(log(thaw) ~ late.TDD + I(late.TDD^2), data = logit.mp2[logit.mp2$site=='GF',]))

summary(lm(log(thaw) ~ late.FDD + I(late.FDD^2), data = logit.mp2[logit.mp2$site=='HF',]))
summary(lm(log(thaw) ~ late.FDD + I(late.FDD^2), data = logit.mp2[logit.mp2$site=='BP',])) # Non-linear
summary(lm(log(thaw) ~ late.FDD + I(late.FDD^2), data = logit.mp2[logit.mp2$site=='D6',]))
summary(lm(log(thaw) ~ late.FDD + I(late.FDD^2), data = logit.mp2[logit.mp2$site=='D2',])) 
summary(lm(log(thaw) ~ late.FDD + I(late.FDD^2), data = logit.mp2[logit.mp2$site=='GF',]))




# Compare the temperature data among sites

## Export at 7 x 6
par(mfrow = c(3, 2))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(1, 2, 0.5, 1), oma = c(2,2,1,1))

x1 <- factor(logit.mp$site, levels = c("HF","BP","D6","D2","GF"))
bp1 <- boxplot(WST150 ~ x1, ylim = c(3,10), xaxt = "n", data = logit.mp)
axis(side = 1, labels = FALSE, tick = TRUE)
legend("topleft", "a) Warm season air temperatures", bty = "n", inset = c(-0.1,-0.2))
for.y <- t(aggregate(logit.mp$WST150, by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:5),y=for.y[1:5]+0.5,c("a","a","b","c","d"))

x1 <- factor(logit.mp$site, levels = c("HF","BP","D6","D2","GF"))
bp1 <- boxplot(CST150 ~ x1, ylim = c(-17,-8), xaxt = "n", data = logit.mp)
axis(side = 1, labels = FALSE, tick = TRUE)
legend("topleft", "d) Cold season air temperatures", bty = "n", inset = c(-0.1,-0.2))
for.y <- t(aggregate(logit.mp$CST150, by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:5),y=for.y[1:5]+0.75,c("a","a","b","b","ab"))

x1 <- factor(logit.mp$site, levels = c("HF","BP","D6","D2","GF"))
bp1 <- boxplot(WST0 ~ x1, ylim = c(3.8,11), xaxt = "n", data = logit.mp)
axis(side = 1, labels = FALSE, tick = TRUE)
legend("topleft", "b) Warm season ground surface temperatures", bty = "n", inset = c(-0.1,-0.2))
for.y <- t(aggregate(logit.mp$WST0, by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:5),y=for.y[1:5]+0.75,c("a","a","b","b","c"))

x1 <- factor(logit.mp$site, levels = c("HF","BP","D6","D2","GF"))
bp1 <- boxplot(CST0 ~ x1, ylim = c(-11,2), xaxt = "n", data = logit.mp)
axis(side = 1, labels = FALSE, tick = TRUE)
legend("topleft", "e) Cold season ground surface temperatures", bty = "n", inset = c(-0.1,-0.2))
for.y <- t(aggregate(logit.mp$CST0, by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:5),y=for.y[1:5]+0.9,c("c","b","a","c","d"))

x1 <- factor(logit.mp$site, levels = c("HF","BP","D6","D2","GF"))
bp1 <- boxplot(WST_150 ~ x1, ylim = c(-1.9,1), data = logit.mp)
abline(h = 0, lty = 2)
legend("topleft", "c) Warm season subsurface temperatures", bty = "n", inset = c(-0.1,-0.2))
for.y <- t(aggregate(logit.mp$WST_150, by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:5),y=for.y[1:5]+0.25,c("b","c","d","a","b"))

x1 <- factor(logit.mp$site, levels = c("HF","BP","D6","D2","GF"))
bp1 <- boxplot(CST_150 ~ x1, ylim = c(-4.5,2), data = logit.mp)
abline(h = 0, lty = 2)
legend("topleft", "f) Cold season subsurface temperatures", bty = "n", inset = c(-0.1,-0.2))
for.y <- t(aggregate(logit.mp$CST_150, by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:5),y=for.y[1:5]+0.6,c("b","d","e","a","c"))

mtext(side=2, "Temperature (°C)", adj=0.5, line=0.5, outer = TRUE)
mtext(side=1, "Site", adj=0.5, line=0.5, outer = TRUE)

########################################
# Compare the n-factors among sites

## Export at 7 x 4
par(mfrow = c(2, 2))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(1, 2, 0.5, 1), oma = c(2,2,1,1))

x1 <- factor(logit.mp$site, levels = c("HF","BP","D6","D2","GF"))
bp1 <- boxplot(TI_0 ~ x1, ylim = c(0,2.5), xaxt = "n", data = logit.mp)
abline(h = 1, lty = 2)
axis(side = 1, labels = FALSE, tick = TRUE)
legend("topleft", "a)", bty = "n", inset = c(-0.1,-0.1))
for.y <- t(aggregate(logit.mp$TI_0, by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:5),y=for.y[1:5]+0.3,c("b","ab","a","b","ab"))

x1 <- factor(logit.mp$site, levels = c("HF","BP","D6","D2","GF"))
bp1 <- boxplot(FI_0 ~ x1, ylim = c(0,2.5), xaxt = "n", data = logit.mp)
abline(h = 1, lty = 2)
axis(side = 1, labels = FALSE, tick = TRUE)
legend("topleft", "c)", bty = "n", inset = c(-0.1,-0.1))
for.y <- t(aggregate(logit.mp$FI_0, by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:5),y=for.y[1:5]+0.3,c("b","b","a","ab","b"))

x1 <- factor(logit.mp$site, levels = c("HF","BP","D6","D2","GF"))
bp1 <- boxplot(TI150 ~ x1, ylim = c(0,1), data = logit.mp)
legend("topleft", "b)", bty = "n", inset = c(-0.1,-0.1))
for.y <- t(aggregate(logit.mp$TI150, by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:5),y=for.y[1:5]+0.1,c("a","a","b","a","a"))

x1 <- factor(logit.mp$site, levels = c("HF","BP","D6","D2","GF"))
bp1 <- boxplot(FI150 ~ x1, ylim = c(0,1), data = logit.mp)
legend("topleft", "d)", bty = "n", inset = c(-0.1,-0.1))
for.y <- t(aggregate(logit.mp$FI150, by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:5),y=for.y[1:5]+0.1,c("bc","a","ab","c","ab"))

mtext(expression(italic("n") ~ "-factor"), side=2, adj=0.5, line=0.5, outer = TRUE)
mtext(side=1, "Site", adj=0.5, line=0.5, outer = TRUE)

# Plot the thaw depth as a function of site 

par(mfrow = c(1, 1))
par(ps = 10) # Sets the font size to 12 pts
par(mar = c(2, 2, 2, 0))

x1 <- factor(logit.mp$site, levels = c("HF","BP","D6","D2","GF"))
bp1 <- boxplot(thaw ~ x1, ylim = c(40,170), data = logit.mp)
# legend("topleft", "Thaw depth (cm)", bty = "n")#, inset = c(-0.1,-0.1))
for.y <- t(aggregate(logit.mp$thaw, by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:5),y=for.y[1:5]+12,c("a","a","b","a","b"))
mtext("Thaw depth (cm)", side=2, adj=0.5, line=2)
mtext(side=1, "Site", adj=0.5, line=2)


########################################
##### Linear mixed effect modeling #####
########################################


shapiro.test(logit.mp2$thaw)
hist(logit.mp2$thaw)

boxplot(thaw ~ site,data = logit.mp2)
boxplot(thaw ~ as.numeric(year), data = logit.mp)

# plot(thaw ~ as.numeric(year), ylim = c(120,40), data = logit.mp)
# lines(lowess(logit.mp$year,logit.mp$thaw, f = .00000000001), col = 3, lwd = 5, ylim = c(120,40))
# reg1 <- lm(logit.mp$thaw ~ logit.mp$year)
# abline(reg1, lty = 2, lwd = 2)
# summary(reg1)

## Check for autocorrelation in the data

m0.hf <- gls(thaw ~ year, data = logit.mp2[logit.mp2$site=='HF',c(1:12)])
E.hf <- residuals(m0.hf, type = "normalized")
acf(E.hf, na.action = na.pass, main = "Auto-correlation plot for residuals")

m0.bp <- gls(thaw ~ year, data = logit.mp2[logit.mp2$site=='BP',c(1:12)])
E.bp <- residuals(m0.bp, type = "normalized")
acf(E.bp, na.action = na.pass, main = "Auto-correlation plot for residuals")

m0.d6 <- gls(thaw ~ year, data = logit.mp2[logit.mp2$site=='D6',c(1:12)])
E.d6 <- residuals(m0.d6, type = "normalized")
acf(E.d6, na.action = na.pass, main = "Auto-correlation plot for residuals")

m0.d2 <- gls(thaw ~ year, data = logit.mp2[logit.mp2$site=='D2',c(1:12)])
E.d2 <- residuals(m0.d2, type = "normalized")
acf(E.d2, na.action = na.pass, main = "Auto-correlation plot for residuals")

m0.gf <- gls(thaw ~ year, data = logit.mp2[logit.mp2$site=='GF',c(1:12)])
E.gf <- residuals(m0.gf, type = "normalized")
acf(E.gf, na.action = na.pass, main = "Auto-correlation plot for residuals")

## All data show significant autocorrelation, so we need to account for this in our model

# Mixed-effect modeling of thaw depth
# ***********************************

### Regional model

logit.reg <- read.csv('master.csv', header = T)
logit.reg2 <- decostand(logit.reg[,c(11,12,16,27:32)], method = "standardize")
logit.reg2 <- cbind(year = logit.reg$year,site = logit.reg$site, logit.reg2)


thaw.reg1 <- lme(thaw ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.tdd + total.tdd + late.fdd + total.fdd, 
                method = "ML", random = ~ 1 | site, correlation=corARMA(form = ~ year | site, p=3, q=0),
                data = logit.reg2) # AIC = -91.81561
thaw.reg2 <- lme(thaw ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.tdd + total.tdd + late.fdd + total.fdd, 
                 method = "ML", random = ~ 1 | site, correlation=corARMA(form = ~ year | site, p=2, q=0),
                 data = logit.reg2) # AIC = -91.81561
thaw.reg3 <- lme(thaw ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.tdd + total.tdd + late.fdd + total.fdd, 
                 method = "ML", random = ~ 1 | site, correlation=corARMA(form = ~ year | site, p=1, q=0),
                 data = logit.reg2) # AIC = -91.81561
thaw.reg4 <- lme(thaw ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.tdd + total.tdd + late.fdd + total.fdd, 
                 method = "ML", random = ~ 1 | site, correlation=corARMA(form = ~ year | site, p=0, q=0),
                 data = logit.reg2) # Doesn't work
thaw.reg5 <- lme(thaw ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.tdd + total.tdd + late.fdd + total.fdd, 
                 method = "ML", random = ~ 1 | site, correlation=corARMA(form = ~ year | site, p=0, q=1),
                 data = logit.reg2) # AIC = -91.81561
thaw.reg6 <- lme(thaw ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.tdd + total.tdd + late.fdd + total.fdd, 
                 method = "ML", random = ~ 1 | site, correlation=corARMA(form = ~ year | site, p=0, q=2),
                 data = logit.reg2) # AIC = -91.81561
thaw.reg7 <- lme(thaw ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.tdd + total.tdd + late.fdd + total.fdd, 
                 method = "ML", random = ~ 1 | site, correlation=corARMA(form = ~ year | site, p=0, q=3),
                 data = logit.reg2) # AIC = -91.81561
thaw.reg8 <- lme(thaw ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.tdd + total.tdd + late.fdd + total.fdd, 
                 method = "ML", random = ~ 1 | site, correlation=corARMA(form = ~ year | site, p=1, q=1),
                 data = logit.reg2) # AIC = -91.81561
thaw.reg9 <- lme(thaw ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.tdd + total.tdd + late.fdd + total.fdd, 
                 method = "ML", random = ~ 1 | site, correlation=corARMA(form = ~ year | site, p=1, q=2),
                 data = logit.reg2) # AIC = -91.81561
thaw.reg10 <- lme(thaw ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.tdd + total.tdd + late.fdd + total.fdd, 
                 method = "ML", random = ~ 1 | site, correlation=corARMA(form = ~ year | site, p=1, q=3),
                 data = logit.reg2) # AIC = -91.81561
thaw.reg11 <- lme(thaw ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.tdd + total.tdd + late.fdd + total.fdd, 
                 method = "ML", random = ~ 1 | site, correlation=corARMA(form = ~ year | site, p=2, q=1),
                 data = logit.reg2) # AIC = -91.81561
thaw.reg12 <- lme(thaw ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.tdd + total.tdd + late.fdd + total.fdd, 
                 method = "ML", random = ~ 1 | site, correlation=corARMA(form = ~ year | site, p=2, q=2),
                 data = logit.reg2) # AIC = -91.81561
thaw.reg13 <- lme(thaw ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.tdd + total.tdd + late.fdd + total.fdd, 
                  method = "ML", random = ~ 1 | site, correlation=corARMA(form = ~ year | site, p=2, q=3),
                  data = logit.reg2) # AIC = -91.81561
thaw.reg14 <- lme(thaw ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.tdd + total.tdd + late.fdd + total.fdd, 
                 method = "ML", random = ~ 1 | site, correlation=corARMA(form = ~ year | site, p=3, q=1),
                 data = logit.reg2) # AIC = -91.81561
thaw.reg15 <- lme(thaw ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.tdd + total.tdd + late.fdd + total.fdd, 
                 method = "ML", random = ~ 1 | site, correlation=corARMA(form = ~ year | site, p=3, q=2),
                 data = logit.reg2) # AIC = -91.81561
thaw.reg16 <- lme(thaw ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.tdd + total.tdd + late.fdd + total.fdd, 
                 method = "ML", random = ~ 1 | site, correlation=corARMA(form = ~ year | site, p=3, q=3),
                 data = logit.reg2) # AIC = -91.81561

AIC(thaw.reg1) # AIC = 136.9503
AIC(thaw.reg2) # AIC = 140.014
AIC(thaw.reg3) # AIC = 142.8936
AIC(thaw.reg4) # AIC = NA
AIC(thaw.reg5) # AIC = 153.0946
AIC(thaw.reg6) # AIC = 150.7462
AIC(thaw.reg7) # AIC = 145.313
AIC(thaw.reg8) # AIC = 135.527
AIC(thaw.reg9) # AIC = 136.9169
AIC(thaw.reg10) # AIC = 138.5388
AIC(thaw.reg11) # AIC = 137.046
AIC(thaw.reg12) # AIC = 138.3497
AIC(thaw.reg13) # AIC = NA
AIC(thaw.reg14) # AIC = 138.6581
AIC(thaw.reg15) # AIC = NA
AIC(thaw.reg16) # AIC = NA

## The p = 3, q = 0 model is the best-ish. Now to determine most useful predictors.

thaw.full <- lme(thaw ~  TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.tdd + total.tdd + late.fdd + total.fdd, 
                method = "ML", random = ~ 1 | site, data = logit.reg2, 
                correlation=corARMA(form = ~ year | site, p=1, q=1)) # AIC = 136.9503
thaw.TI_0 <- update(thaw.full, .~. -TI_0) # AIC = 135.0145
thaw.FI_0 <- update(thaw.full, .~. -FI_0) # AIC = 134.9850
thaw.WSP_CRU <- update(thaw.full, .~. -WSP_CRU) # AIC = 135.1762
thaw.CSP_CRU <- update(thaw.full, .~. -CSP_CRU) # AIC = 136.1034
thaw.late.tdd <- update(thaw.full, .~. -late.tdd) # AIC = 139.4597
thaw.total.tdd <- update(thaw.full, .~. -total.tdd) # AIC = 135.970
thaw.late.fdd <- update(thaw.full, .~. -late.fdd) # AIC = 136.9268
thaw.total.fdd <- update(thaw.full, .~. -total.fdd) # AIC = 137.5676

anova(thaw.full, thaw.TI_0) # --
anova(thaw.full, thaw.FI_0) # --
anova(thaw.full, thaw.WSP_CRU) # --
anova(thaw.full, thaw.CSP_CRU) # --
anova(thaw.full, thaw.late.tdd) # SIG
anova(thaw.full, thaw.total.tdd) # --
anova(thaw.full, thaw.late.fdd) # SIG
anova(thaw.full, thaw.total.fdd) #SIG

# Final model fit with REML
thaw.fin <- lme(thaw ~ late.tdd + late.fdd, method = "REML", random = ~ 1 | site, 
                data = logit.reg2, correlation=corARMA(form = ~ year | site, p=1, q=1)) # AIC = 136.7348
summary(thaw.fin)
vif(thaw.fin)
r.squaredGLMM(thaw.fin)
r.squaredLR(thaw.fin, null.RE = TRUE)
plot(thaw.fin,resid(.,type="p")~fitted(.)|site)
qqnorm(thaw.fin,~resid(.)|site)

###### Site models

# BP
thaw.bp1 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
               method = "ML", random = ~ 1 | line, 
               data = logit.mp2[logit.mp2$site=='BP',c(1:12)],
               correlation=corARMA(form = ~ year | line, p=0, q=0))
thaw.bp2 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
               method = "ML", random = ~ 1 | line, 
               data = logit.mp2[logit.mp2$site=='BP',c(1:12)],
               correlation=corARMA(form = ~ year | line, p=1, q=0))
thaw.bp3 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
               method = "ML", random = ~ 1 | line, 
               data = logit.mp2[logit.mp2$site=='BP',c(1:12)],
               correlation=corARMA(form = ~ year | line, p=2, q=0))
thaw.bp4 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
               method = "ML", random = ~ 1 | line, 
               data = logit.mp2[logit.mp2$site=='BP',c(1:12)],
               correlation=corARMA(form = ~ year | line, p=3, q=0))
thaw.bp5 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
               method = "ML", random = ~ 1 | line, 
               data = logit.mp2[logit.mp2$site=='BP',c(1:12)],
               correlation=corARMA(form = ~ year | line, p=0, q=1))
thaw.bp6 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
               method = "ML", random = ~ 1 | line, 
               data = logit.mp2[logit.mp2$site=='BP',c(1:12)],
               correlation=corARMA(form = ~ year | line, p=1, q=1))
thaw.bp7 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
               method = "ML", random = ~ 1 | line, 
               data = logit.mp2[logit.mp2$site=='BP',c(1:12)],
               correlation=corARMA(form = ~ year | line, p=2, q=1))
thaw.bp8 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
               method = "ML", random = ~ 1 | line, 
               data = logit.mp2[logit.mp2$site=='BP',c(1:12)],
               correlation=corARMA(form = ~ year | line, p=3, q=1))
thaw.bp9 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
               method = "ML", random = ~ 1 | line, 
               data = logit.mp2[logit.mp2$site=='BP',c(1:12)],
               correlation=corARMA(form = ~ year | line, p=0, q=2))
thaw.bp10 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
               method = "ML", random = ~ 1 | line, 
               data = logit.mp2[logit.mp2$site=='BP',c(1:12)],
               correlation=corARMA(form = ~ year | line, p=1, q=2))
thaw.bp11 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
               method = "ML", random = ~ 1 | line, 
               data = logit.mp2[logit.mp2$site=='BP',c(1:12)],
               correlation=corARMA(form = ~ year | line, p=2, q=2))
thaw.bp12 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
               method = "ML", random = ~ 1 | line, 
               data = logit.mp2[logit.mp2$site=='BP',c(1:12)],
               correlation=corARMA(form = ~ year | line, p=3, q=2))
thaw.bp13 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
               method = "ML", random = ~ 1 | line, 
               data = logit.mp2[logit.mp2$site=='BP',c(1:12)],
               correlation=corARMA(form = ~ year | line, p=0, q=3))
thaw.bp14 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
               method = "ML", random = ~ 1 | line, 
               data = logit.mp2[logit.mp2$site=='BP',c(1:12)],
               correlation=corARMA(form = ~ year | line, p=1, q=3))
thaw.bp15 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
               method = "ML", random = ~ 1 | line, 
               data = logit.mp2[logit.mp2$site=='BP',c(1:12)],
               correlation=corARMA(form = ~ year | line, p=2, q=3))
thaw.bp16 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
               method = "ML", random = ~ 1 | line, 
               data = logit.mp2[logit.mp2$site=='BP',c(1:12)],
               correlation=corARMA(form = ~ year | line, p=3, q=3))

AIC(thaw.bp1) # NA
AIC(thaw.bp2) # -145.2979
AIC(thaw.bp3) # -154.8507 (p = 2, q = 0)
AIC(thaw.bp4) # -153.2163
AIC(thaw.bp5) # -121.3253 
AIC(thaw.bp6) # -154.7384
AIC(thaw.bp7) # -153.4447
AIC(thaw.bp8) # -151.4503
AIC(thaw.bp9) # -133.7068
AIC(thaw.bp10) # -153.4857
AIC(thaw.bp11) # -151.9064
AIC(thaw.bp12) # -149.9101
AIC(thaw.bp13) # -134.7955
AIC(thaw.bp14) # -151.643
AIC(thaw.bp15) # -149.7047
AIC(thaw.bp16) # NA

thaw.bp.TI_0 <- update(thaw.bp5, .~. -TI_0) 
thaw.bp.FI_0 <- update(thaw.bp5, .~. -FI_0) 
thaw.bp.WSP_CRU <- update(thaw.bp5, .~. -WSP_CRU) 
thaw.bp.CSP_CRU <- update(thaw.bp5, .~. -CSP_CRU) 
thaw.bp.late.tdd <- update(thaw.bp5, .~. -late.TDD) 
thaw.bp.late.fdd <- update(thaw.bp5, .~. -late.FDD) 

# HF
thaw.hf1 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='HF',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=0, q=0))
thaw.hf2 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='HF',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=1, q=0))
thaw.hf3 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='HF',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=2, q=0))
thaw.hf4 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='HF',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=3, q=0))
thaw.hf5 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='HF',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=0, q=1))
thaw.hf6 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='HF',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=1, q=1))
thaw.hf7 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='HF',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=2, q=1))
thaw.hf8 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='HF',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=3, q=1))
thaw.hf9 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='HF',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=0, q=2))
thaw.hf10 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='HF',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=1, q=2))
thaw.hf11 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='HF',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=2, q=2))
thaw.hf12 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='HF',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=3, q=2))
thaw.hf13 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='HF',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=0, q=3))
thaw.hf14 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='HF',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=1, q=3))
thaw.hf15 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='HF',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=2, q=3))
thaw.hf16 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='HF',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=3, q=3))

AIC(thaw.hf1) # NA
AIC(thaw.hf2) # -53.90864
AIC(thaw.hf3) # -52.24108
AIC(thaw.hf4) # -57.68474
AIC(thaw.hf5) # -53.85105
AIC(thaw.hf6) # -54.01704
AIC(thaw.hf7) # -52.24182
AIC(thaw.hf8) # -57.56093
AIC(thaw.hf9) # -52.60119
AIC(thaw.hf10) # NA
AIC(thaw.hf11) # -53.29475
AIC(thaw.hf12) # -55.69256
AIC(thaw.hf13) # -57.92211 (p = 0, q = 3)
AIC(thaw.hf14) # NA
AIC(thaw.hf15) # NA
AIC(thaw.hf16) # -53.77296

thaw.hf.TI_0 <- update(thaw.hf4, .~. -TI_0) 
thaw.hf.FI_0 <- update(thaw.hf4, .~. -FI_0) 
thaw.hf.WSP_CRU <- update(thaw.hf4, .~. -WSP_CRU) 
thaw.hf.CSP_CRU <- update(thaw.hf4, .~. -CSP_CRU) 
thaw.hf.late.tdd <- update(thaw.hf4, .~. -late.TDD) 
thaw.hf.late.fdd <- update(thaw.hf4, .~. -late.FDD) 

# D2
thaw.d21 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='D2',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=0, q=0))
thaw.d22 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='D2',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=1, q=0))
thaw.d23 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='D2',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=2, q=0))
thaw.d24 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='D2',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=3, q=0))
thaw.d25 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='D2',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=0, q=1))
thaw.d26 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='D2',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=1, q=1))
thaw.d27 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='D2',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=2, q=1))
thaw.d28 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='D2',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=3, q=1))
thaw.d29 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='D2',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=0, q=2))
thaw.d210 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='D2',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=1, q=2))
thaw.d211 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='D2',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=2, q=2))
thaw.d212 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='D2',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=3, q=2))
thaw.d213 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='D2',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=0, q=3))
thaw.d214 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='D2',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=1, q=3))
thaw.d215 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='D2',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=2, q=3))
thaw.d216 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='D2',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=3, q=3))

AIC(thaw.d21) # NA
AIC(thaw.d22) # -57.60233
AIC(thaw.d23) # -63.60334
AIC(thaw.d24) # -74.88116
AIC(thaw.d25) # -39.51055
AIC(thaw.d26) # -69.17681
AIC(thaw.d27) # -67.34864
AIC(thaw.d28) # -73.89681
AIC(thaw.d29) # -38.77637
AIC(thaw.d210) # 67.71415
AIC(thaw.d211) # -69.63084
AIC(thaw.d212) # -72.72868
AIC(thaw.d213) # -56.62187 
AIC(thaw.d214) # -74.58744
AIC(thaw.d215) # NA
AIC(thaw.d216) # -71.53706

thaw.d2.TI_0 <- update(thaw.d24, .~. -TI_0) 
thaw.d2.FI_0 <- update(thaw.d24, .~. -FI_0) 
thaw.d2.WSP_CRU <- update(thaw.d24, .~. -WSP_CRU) 
thaw.d2.CSP_CRU <- update(thaw.d24, .~. -CSP_CRU) 
thaw.d2.late.tdd <- update(thaw.d24, .~. -late.TDD) 
thaw.d2.late.fdd <- update(thaw.d24, .~. -late.FDD) 

# D6
thaw.d61 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='D6',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=0, q=0))
thaw.d62 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='D6',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=1, q=0))
thaw.d63 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='D6',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=2, q=0))
thaw.d64 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='D6',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=3, q=0))
thaw.d65 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='D6',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=0, q=1))
thaw.d66 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='D6',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=1, q=1))
thaw.d67 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='D6',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=2, q=1))
thaw.d68 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='D6',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=3, q=1))
thaw.d69 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='D6',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=0, q=2))
thaw.d610 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='D6',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=1, q=2))
thaw.d611 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='D6',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=2, q=2))
thaw.d612 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='D6',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=3, q=2))
thaw.d613 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='D6',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=0, q=3))
thaw.d614 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='D6',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=1, q=3))
thaw.d615 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='D6',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=2, q=3))
thaw.d616 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='D6',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=3, q=3))

AIC(thaw.d61) # NA
AIC(thaw.d62) # -39.40493
AIC(thaw.d63) # -39.5216
AIC(thaw.d64) # -47.40457
AIC(thaw.d65) # -38.2426
AIC(thaw.d66) # -44.3685
AIC(thaw.d67) # -42.70552
AIC(thaw.d68) # -46.38806
AIC(thaw.d69) # -36.75275
AIC(thaw.d610) # -40.55579
AIC(thaw.d611) # -42.71363
AIC(thaw.d612) # -45.71423
AIC(thaw.d613) # -40.7466 
AIC(thaw.d614) # NA
AIC(thaw.d615) # NA
AIC(thaw.d616) # NA

thaw.d6.TI_0 <- update(thaw.d64, .~. -TI_0) 
thaw.d6.FI_0 <- update(thaw.d64, .~. -FI_0) 
thaw.d6.WSP_CRU <- update(thaw.d64, .~. -WSP_CRU) 
thaw.d6.CSP_CRU <- update(thaw.d64, .~. -CSP_CRU) 
thaw.d6.late.tdd <- update(thaw.d64, .~. -late.TDD) 
thaw.d6.late.fdd <- update(thaw.d64, .~. -late.FDD) 

# GF
thaw.gf1 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='GF',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=0, q=0))
thaw.gf2 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='GF',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=1, q=0))
thaw.gf3 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='GF',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=2, q=0))
thaw.gf4 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='GF',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=3, q=0))
thaw.gf5 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='GF',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=0, q=1))
thaw.gf6 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='GF',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=1, q=1))
thaw.gf7 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='GF',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=2, q=1))
thaw.gf8 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='GF',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=3, q=1))
thaw.gf9 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                method = "ML", random = ~ 1 | line, 
                data = logit.mp2[logit.mp2$site=='GF',c(1:12)],
                correlation=corARMA(form = ~ year | line, p=0, q=2))
thaw.gf10 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='GF',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=1, q=2))
thaw.gf11 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='GF',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=2, q=2))
thaw.gf12 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='GF',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=3, q=2))
thaw.gf13 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='GF',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=0, q=3))
thaw.gf14 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='GF',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=1, q=3))
thaw.gf15 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='GF',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=2, q=3))
thaw.gf16 <- lme(log(thaw) ~ TI_0 + FI_0 + WSP_CRU + CSP_CRU + late.TDD + late.FDD, 
                 method = "ML", random = ~ 1 | line, 
                 data = logit.mp2[logit.mp2$site=='GF',c(1:12)],
                 correlation=corARMA(form = ~ year | line, p=3, q=3))

AIC(thaw.gf1) # NA
AIC(thaw.gf2) # -9.260962
AIC(thaw.gf3) # -9.33839
AIC(thaw.gf4) # -7.391672
AIC(thaw.gf5) # -4.841282
AIC(thaw.gf6) # -9.257183
AIC(thaw.gf7) # -7.528994
AIC(thaw.gf8) # -5.545626
AIC(thaw.gf9) # -8.380851
AIC(thaw.gf10) # -7.257299
AIC(thaw.gf11) # NA
AIC(thaw.gf12) # -3.832111
AIC(thaw.gf13) # -6.52495
AIC(thaw.gf14) # NA
AIC(thaw.gf15) # -4.062944
AIC(thaw.gf16) # NA

thaw.gf.TI_0 <- update(thaw.gf3, .~. -TI_0) 
thaw.gf.FI_0 <- update(thaw.gf3, .~. -FI_0) 
thaw.gf.WSP_CRU <- update(thaw.gf3, .~. -WSP_CRU) 
thaw.gf.CSP_CRU <- update(thaw.gf3, .~. -CSP_CRU) 
thaw.gf.late.tdd <- update(thaw.gf3, .~. -late.TDD) 
thaw.gf.late.fdd <- update(thaw.gf3, .~. -late.FDD) 

## A bunch of ANOVAs to test the effects of term elimination
# BP
summary(thaw.bp5) # AIC = -121.3253
anova(thaw.bp5, thaw.bp.TI_0) # Not sig
anova(thaw.bp5, thaw.bp.FI_0) # Not sig
anova(thaw.bp5, thaw.bp.WSP_CRU) # Not sig
anova(thaw.bp5, thaw.bp.CSP_CRU) # Not sig
anova(thaw.bp5, thaw.bp.late.tdd) # SIG
anova(thaw.bp5, thaw.bp.late.fdd) # Not sig

# HF
summary(thaw.hf4) # AIC = -57.68474
anova(thaw.hf4, thaw.hf.TI_0) # Not sig
anova(thaw.hf4, thaw.hf.FI_0) # SIG
anova(thaw.hf4, thaw.hf.WSP_CRU) # Not sig
anova(thaw.hf4, thaw.hf.CSP_CRU) # Not sig
anova(thaw.hf4, thaw.hf.late.tdd) # SIG
anova(thaw.hf4, thaw.hf.late.fdd) # SIG

# D2
summary(thaw.d24) # AIC = -74.88116
anova(thaw.d24, thaw.d2.TI_0) # SIG
anova(thaw.d24, thaw.d2.FI_0) # Not sig
anova(thaw.d24, thaw.d2.WSP_CRU) # Not sig
anova(thaw.d24, thaw.d2.CSP_CRU) # Not sig
anova(thaw.d24, thaw.d2.late.tdd) # Not sig
anova(thaw.d24, thaw.d2.late.fdd) # Not sig

# D6
summary(thaw.d64) # AIC = -47.40457
anova(thaw.d64, thaw.d6.TI_0) # Not sig
anova(thaw.d64, thaw.d6.FI_0) # Not sig
anova(thaw.d64, thaw.d6.WSP_CRU) # Not sig
anova(thaw.d64, thaw.d6.CSP_CRU) # Not sig
anova(thaw.d64, thaw.d6.late.tdd) # Not sig
anova(thaw.d64, thaw.d6.late.fdd) # "Sig" p = 0.0883

# GF
summary(thaw.gf3) # AIC = -9.257183
anova(thaw.gf3, thaw.gf.TI_0) # Not sig
anova(thaw.gf3, thaw.gf.FI_0) # SIG
anova(thaw.gf3, thaw.gf.WSP_CRU) # Not sig
anova(thaw.gf3, thaw.gf.CSP_CRU) # Not sig
anova(thaw.gf3, thaw.gf.late.tdd) # SIG
anova(thaw.gf3, thaw.gf.late.fdd) # Not sig

# Final models

thaw.final.hf <- lme(thaw ~ FI_0 + late.TDD + late.FDD + WSP_CRU, method = "REML", random = ~ 1 | line, 
                     data = logit.mp2[logit.mp2$site=='HF',c(1:12)],
                     correlation=corARMA(form = ~ year | line, p=3, q=0))
summary(thaw.final.hf)

thaw.final.bp <- lme(thaw ~ late.TDD, method = "REML", random = ~ 1 | line, 
               data = logit.mp2[logit.mp2$site=='BP',c(1:12)],
               correlation=corARMA(form = ~ year | line, p=0, q=1))
summary(thaw.final.bp)

## Nothing was significant for D6
thaw.final.d6 <- lme(thaw ~ late.FDD, method = "REML", random = ~ 1 | line, 
                     data = logit.mp2[logit.mp2$site=='D6',c(1:12)],
                     correlation=corARMA(form = ~ year | line, p=3, q=2))
summary(thaw.final.d6)

thaw.final.d2 <- lme(thaw ~ TI_0, method = "REML", random = ~ 1 | line, 
                     data = logit.mp2[logit.mp2$site=='D2',c(1:12)],
                     correlation=corARMA(form = ~ year | line, p=3, q=0))
summary(thaw.final.d2)

thaw.final.gf <- lme(thaw ~ FI_0 + late.TDD, method = "REML", random = ~ 1 | line, 
               data = logit.mp2[logit.mp2$site=='GF',c(1:12)],
               correlation=corARMA(form = ~ year | line, p=2, q=0))
summary(thaw.final.gf)

summary(thaw.final.hf)
summary(thaw.final.bp)
summary(thaw.final.d6)
summary(thaw.final.d2)
summary(thaw.final.gf)

vif(thaw.final.hf)
vif(thaw.final.bp)
vif(thaw.final.d6)
vif(thaw.final.d2)
vif(thaw.final.gf)

cor(cbind(logit.mp2[logit.mp2$site=='BP',"thaw"]), cbind(fitted(thaw.final.bp, level = 0:1)[,1]))^2
cor(cbind(logit.mp2[logit.mp2$site=='HF',"thaw"]), cbind(fitted(thaw.final.hf, level = 0:1)[,1]))^2
cor(cbind(logit.mp2[logit.mp2$site=='GF',"thaw"]), cbind(fitted(thaw.final.gf, level = 0:1)[,1]))^2
cor(cbind(logit.mp2[logit.mp2$site=='D2',"thaw"]), cbind(fitted(thaw.final.d2, level = 0:1)[,1]))^2
cor(cbind(logit.mp2[logit.mp2$site=='D6',"thaw"]), cbind(fitted(thaw.final.d6, level = 0:1)[,1]))^2

r.squaredGLMM(thaw.final.hf)
r.squaredGLMM(thaw.final.bp)
r.squaredGLMM(thaw.final.d6)
r.squaredGLMM(thaw.final.d2)
r.squaredGLMM(thaw.final.gf)

r.squaredLR(thaw.final.hf, null.RE = TRUE)
r.squaredLR(thaw.final.bp, null.RE = TRUE)
r.squaredLR(thaw.final.d6, null.RE = TRUE)
r.squaredLR(thaw.final.d2, null.RE = TRUE)
r.squaredLR(thaw.final.gf, null.RE = TRUE)


plot(thaw.final.bp,resid(.,type="p")~fitted(.)|line)
plot(thaw.final.hf,resid(.,type="p")~fitted(.)|line)
plot(thaw.final.gf,resid(.,type="p")~fitted(.)|line)
plot(thaw.final.d2,resid(.,type="p")~fitted(.)|line)
plot(thaw.final.d6,resid(.,type="p")~fitted(.)|line)

qqnorm(thaw.final.bp,~resid(.)|line)
qqnorm(thaw.final.hf,~resid(.)|line)
qqnorm(thaw.final.gf,~resid(.)|line)
qqnorm(thaw.final.d2,~resid(.)|line)
qqnorm(thaw.final.d6,~resid(.)|line)



# shapiro.test(resid(lm(thaw ~ site, data = logit.mp2))) # P = 0.667 (normal)
# bartlett.test(sqrt(1/logit.mp2$thaw), as.factor(logit.mp2$site)) # P = 0.479 (homogeneous variance)
# 
# summary(lm(thaw ~ year, data = logit.mp[logit.mp$site=='HF',]))
# summary(lm(thaw ~ year, data = logit.mp[logit.mp$site=='BP',]))
# summary(lm(thaw ~ year, data = logit.mp[logit.mp$site=='D6',]))
# summary(lm(thaw ~ year, data = logit.mp[logit.mp$site=='D2',]))
# summary(lm(thaw ~ year, data = logit.mp[logit.mp$site=='GF',]))
# 
# plot(rev(thaw) ~ year, type = "l", ylim = c(80,0), data = logit.mp[logit.mp$site=='BP',])
# 
# # ANOVA for thaw
# thaw.aov <- aov(lm(thaw ~ site, data = logit.mp))
# summary(thaw.aov)
# TukeyHSD(thaw.aov)
# thaw.pairs <- glht(thaw.aov, linfct = mcp(site = "Tukey"))
# thaw.cld <- cld(thaw.pairs)
# thaw.letters <- thaw.cld$mcletters$Letters
# 
# logit.mp2[logit.mp2$site=='BP',c(1:28)]
# 
# # ANOVA for warm and cold season temperatures
# 
# WST150.aov <- aov(lm(WST150 ~ site, data = logit.mp2))
# summary(WST150.aov)
# TukeyHSD(WST150.aov)
# WST150.pairs <- glht(WST150.aov, linfct = mcp(site = "Tukey"))
# WST150.cld <- cld(WST150.pairs)
# WST150.letters <- WST150.cld$mcletters$Letters
# 
# WST0.aov <- aov(lm(WST0 ~ site, data = logit.mp2))
# summary(WST0.aov)
# TukeyHSD(WST0.aov)
# WST0.pairs <- glht(WST0.aov, linfct = mcp(site = "Tukey"))
# WST0.cld <- cld(WST0.pairs)
# WST0.letters <- WST0.cld$mcletters$Letters
# 
# WST_150.aov <- aov(lm(WST_150 ~ site, data = logit.mp2))
# summary(WST_150.aov)
# TukeyHSD(WST_150.aov)
# WST_150.pairs <- glht(WST_150.aov, linfct = mcp(site = "Tukey"))
# WST_150.cld <- cld(WST_150.pairs)
# WST_150.letters <- WST_150.cld$mcletters$Letters
# 
# CST150.aov <- aov(lm(CST150 ~ site, data = logit.mp2))
# summary(CST150.aov)
# TukeyHSD(CST150.aov)
# CST150.pairs <- glht(CST150.aov, linfct = mcp(site = "Tukey"))
# CST150.cld <- cld(CST150.pairs)
# CST150.letters <- CST150.cld$mcletters$Letters
# 
# CST0.aov <- aov(lm(CST0 ~ site, data = logit.mp2))
# summary(CST0.aov)
# TukeyHSD(CST0.aov)
# CST0.pairs <- glht(CST0.aov, linfct = mcp(site = "Tukey"))
# CST0.cld <- cld(CST0.pairs)
# CST0.letters <- CST0.cld$mcletters$Letters
# 
# CST_150.aov <- aov(lm(CST_150 ~ site, data = logit.mp2))
# summary(CST_150.aov)
# TukeyHSD(CST_150.aov)
# CST_150.pairs <- glht(CST_150.aov, linfct = mcp(site = "Tukey"))
# CST_150.cld <- cld(CST_150.pairs)
# CST_150.letters <- CST_150.cld$mcletters$Letters
# 
# # ANOVA for thaw factors
# 
# TI0.aov <- aov(lm(TI_0 ~ site, data = logit.mp))
# summary(TI0.aov)
# TukeyHSD(TI0.aov)
# TI0.pairs <- glht(TI0.aov, linfct = mcp(site = "Tukey"))
# TI0.cld <- cld(TI0.pairs)
# TI0.letters <- TI0.cld$mcletters$Letters
# 
# TI150.aov <- aov(lm(TI150 ~ site, data = logit.mp))
# summary(TI150.aov)
# TukeyHSD(TI150.aov)
# TI150.pairs <- glht(TI150.aov, linfct = mcp(site = "Tukey"))
# TI150.cld <- cld(TI150.pairs)
# TI150.letters <- TI150.cld$mcletters$Letters
# 
# FI0.aov <- aov(lm(FI_0 ~ site, data = logit.mp))
# summary(FI0.aov)
# TukeyHSD(FI0.aov)
# FI0.pairs <- glht(FI0.aov, linfct = mcp(site = "Tukey"))
# FI0.cld <- cld(FI0.pairs)
# FI0.letters <- FI0.cld$mcletters$Letters
# 
# FI150.aov <- aov(lm(FI150 ~ site, data = logit.mp))
# summary(FI150.aov)
# TukeyHSD(FI150.aov)
# FI150.pairs <- glht(FI150.aov, linfct = mcp(site = "Tukey"))
# FI150.cld <- cld(FI150.pairs)
# FI150.letters <- FI150.cld$mcletters$Letters