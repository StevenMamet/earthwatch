library(multcomp) # Simultaneous Inference in General Parametric Models 
library(vegan)
library(cluster)
library(gclus) # Used for plotting clusters
library(car) # Used for determining variance inflation factors
library(labdsv) # PCA

rm(list = ls())

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

##******************************##
## Plotting Mac Pass thaw data ##
##******************************##


# Export at 7 x 7
# Singles at 6 x 4
par(mfrow = c(4, 2))
par(cex = 0.75)
par(mar = c(3, 3, 0, 0), oma = c(2, 2, 2, 2)+0.2)

plot(snow$year, snow$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(snow$year, snow$c5, col = 'darkslategray1', xlim = c(1990,2020), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(snow$year, snow$c95, col = 'darkslategray1', xlim = c(1990,2020), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(snow$year, rev(snow$year)), c(snow$c95, rev(snow$c5)), col = "darkslategray1", border = NA)
par(new=T)
plot(snow$year, snow$mean, xlim = c(1990,2020), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "darkslategray4", xlab = "Year", ylab = "Thaw depth (cm)")
legend("topleft", "SF (1660 m.a.s.l.)", bty = "n", inset = c(-0.001,0))
abline(coef(snow.lm),lty=2)

plot(goose$year, goose$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(goose$year, goose$c5, col = 'olivedrab1', xlim = c(1990,2020), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(goose$year, goose$c95, col = 'olivedrab1', xlim = c(1990,2020), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(goose$year, rev(goose$year)), c(goose$c95, rev(goose$c5)), col = "olivedrab1", border = NA)
par(new=T)
plot(goose$year, goose$mean, xlim = c(1990,2020), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", type = "l", lwd=2,  col = "darkgreen", xlab = "Year", ylab = "Thaw depth (cm)")
legend("topleft", "GF (1621 m.a.s.l.)", bty = "n", inset = c(-0.001,0))
abline(coef(goose.lm),lty=2)

plot(d2$year, d2$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(d2$year, d2$c5, col = 'lightskyblue', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(d2$year, d2$c95, col = 'lightskyblue', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(d2$year, rev(d2$year)), c(d2$c95, rev(d2$c5)), col = "lightskyblue", border = NA)
par(new=T)
plot(d2$year, d2$mean, xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "blue", xlab = "Year", ylab = "Thaw depth (cm)")
legend("topleft", "D2 (1477 m.a.s.l.)", bty = "n", inset = c(-0.001,0))
abline(coef(d2.lm),lty=2)

plot(d6$year, d6$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(d6$year, d6$c5, col = 'orange1', xlim = c(1990,2020), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(d6$year, d6$c95, col = 'orange1', xlim = c(1990,2020), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(d6$year, rev(d6$year)), c(d6$c95, rev(d6$c5)), col = "orange1", border = NA)
par(new=T)
plot(d6$year, d6$mean, xlim = c(1990,2020), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "orangered2", xlab = "Year", ylab = "Thaw depth (cm)")
legend("topleft", "D6 (1473 m.a.s.l.)", bty = "n", inset = c(-0.001,0))

plot(porsild$year, porsild$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(porsild$year, porsild$c5, col = 'darkorchid1', xlim = c(1990,2020), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(porsild$year, porsild$c95, col = 'darkorchid1', xlim = c(1990,2020), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(porsild$year, rev(porsild$year)), c(porsild$c95, rev(porsild$c5)), col = "darkorchid1", border = NA)
par(new=T)
plot(porsild$year, porsild$mean, xlim = c(1990,2020), ylim = rev(c(0,80)), yaxs = "i", xaxs = "i", lwd=2, type = "l", col = "darkorchid4", xlab = "Year", ylab = "Thaw depth (cm)")
legend("topleft", "PF (1380 m.a.s.l.)", bty = "n", inset = c(-0.001,0))

plot(beaver$year, beaver$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(beaver$year, beaver$c5, col = 'gold', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(beaver$year, beaver$c95, col = 'gold', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(beaver$year, rev(beaver$year)), c(beaver$c95, rev(beaver$c5)), col = "gold", border = NA)
par(new=T)
plot(beaver$year, beaver$mean, xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", lwd=2, type = "l", col = "goldenrod3",xlab = "Year", ylab = "Thaw depth (cm)")
legend("topleft", "BP (1272 m.a.s.l.)", bty = "n", inset = c(-0.001,0))
abline(coef(beaver.lm),lty=2)

plot(hare$year, hare$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(hare$year, hare$c5, col = 'pink', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(hare$year, hare$c95, col = 'pink', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(hare$year, rev(hare$year)), c(hare$c95, rev(hare$c5)), col = "pink", border = NA)
par(new=T)
plot(hare$year, hare$mean, xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", lwd=2, type = "l", col = "red", xlab = "Year", ylab = "Thaw depth (cm)")
legend("topleft", "HF (1260 m.a.s.l.)", bty = "n", inset = c(-0.001,0))
abline(coef(hare.lm),lty=2)

mtext("Thaw depth (cm)", side=2, outer=TRUE, cex = 0.75, adj=0.5)
mtext("Year", side=1, outer=TRUE, cex=0.75, adj=0.5)


snow.lm <- lm(snow$mean~snow$year)
goose.lm <- lm(goose$mean~goose$year)
d2.lm <- lm(d2$mean~d2$year)
d6.lm <- lm(d6$mean~d6$year)
porsild.lm <- lm(porsild$mean~porsild$year)
beaver.lm <- lm(beaver$mean~beaver$year)
hare.lm <- lm(hare$mean~hare$year)
summary(snow.lm) # Sig (0.617)
summary(goose.lm) # Sig (1.7645)
summary(d2.lm) # Sig (0.8836)
summary(d6.lm) # --
summary(porsild.lm) # --
summary(beaver.lm) # Sig (0.4355)
summary(hare.lm) # Sig (0.3559)

#*************************************************************#
#### Churchill ####
#*************************************************************#

ppa <- subset(thaw.ch, site == "PPA")
ppd <- subset(thaw.ch, site == "PPD")
air <- subset(thaw.ch, site == "AIR")

par(mfrow = c(2, 1))
par(cex = 0.75)
par(mar = c(3, 3, 0, 0), oma = c(2, 2, 2, 2)+0.2)

plot(ppa$year, ppa$mean, type='n', xlim = c(2006,2017), ylim = rev(c(30,60)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(ppa$year, ppa$c5, col = 'plum2', xlim = c(2006,2017), ylim = rev(c(30,60)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(ppa$year, ppa$c95, col = 'plum2', xlim = c(2006,2017), ylim = rev(c(30,60)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(ppa$year, rev(ppa$year)), c(ppa$c95, rev(ppa$c5)), col = "plum2", border = NA)
par(new=T)
plot(ppa$year, ppa$mean, xlim = c(2006,2017), ylim = rev(c(30,60)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "darkslategray4", xlab = "Year", ylab = "Thaw depth (cm)")
# legend("topleft", "PPA", bty = "n", inset = c(-0.001,0))
abline(coef(ppa.lm),lty=2, col = "plum2")
par(new=T)
plot(ppd$year, ppd$mean, type='n', xlim = c(2006,2017), ylim = rev(c(30,60)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(ppd$year, ppd$c5, col = 'darkslategray1', xlim = c(2006,2017), ylim = rev(c(30,60)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(ppd$year, ppd$c95, col = 'darkslategray1', xlim = c(2006,2017), ylim = rev(c(30,60)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(ppd$year, rev(ppd$year)), c(ppd$c95, rev(ppd$c5)), col = "darkslategray1", border = NA)
par(new=T)
plot(ppd$year, ppd$mean, xlim = c(2006,2017), ylim = rev(c(30,60)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "darkslategray4", xlab = "Year", ylab = "Thaw depth (cm)")
# legend("topleft", "PPD", bty = "n", inset = c(-0.001,0))
# abline(coef(ppd.lm),lty=2, col = "darkslategray1")


mtext("Thaw depth (cm)", side=2, outer=TRUE, cex = 0.75, adj=0.5)
mtext("Year", side=1, outer=TRUE, cex=0.75, adj=0.5)


ppa.lm <- lm(ppa$mean~ppa$year)
ppd.lm <- lm(ppd$mean~ppd$year)
air.lm <- lm(air$mean~air$year)
summary(ppa.lm) # Sig (0.5136)
summary(ppd.lm) # Sig (0.5632)
summary(air.lm) # Sig (-1.3727)
