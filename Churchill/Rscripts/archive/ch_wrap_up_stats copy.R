rm(list=ls())

##******************************##
## Plotting Churchill thaw data ##
##******************************##
thaw.ch <- read.csv(file = "~/Desktop/Workspace/Earthwatch/thaw.ch.csv", header = TRUE)
thaw.ch$ci <- thaw.ch$se*qt(0.975, thaw.ch$n-1)
thaw.ch$c5 <- thaw.ch$mean - thaw.ch$ci
thaw.ch$c95 <- thaw.ch$mean + thaw.ch$ci

ppa <- subset(thaw.ch, site == "PPA")
ppd <- subset(thaw.ch, site == "PPD")
air <- subset(thaw.ch, site == "AIR")
blk <- subset(thaw.ch, site == "BLK")

ppa.lm <- lm(ppa$mean~ppa$year)
ppd.lm <- lm(ppd$mean~ppd$year)
air.lm <- lm(air$mean~air$year)
blk.lm <- lm(blk$mean~blk$year)
summary(ppa.lm)
summary(ppd.lm)
summary(air.lm)
summary(blk.lm)

# Export at 7 x 5
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,1))
plot(ppa$year, ppa$mean, type='n', xlim = c(2000,2018), ylim = rev(c(30,60)), yaxs = "i", xaxs = "i", xlab = "", ylab = "", xaxt = "n")
lines(ppa$year, ppa$c5, col = 'darkslategray1', xlim = c(2000,2018), ylim = rev(c(30,60)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(ppa$year, ppa$c95, col = 'darkslategray1', xlim = c(2000,2018), ylim = rev(c(30,60)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(ppa$year, rev(ppa$year)), c(ppa$c95, rev(ppa$c5)), col = "darkslategray1", border = NA)
par(new=T)
plot(ppa$year, ppa$mean, xlim = c(2000,2018), ylim = rev(c(30,60)), yaxs = "i", xaxs = "i",  xlab = "", ylab = "", xaxt = "n", type = "p", lwd=2, col = "darkslategray4")
par(new=T)
plot(ppa$year, ppa$mean, xlim = c(2000,2018), ylim = rev(c(30,60)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "darkslategray4", yaxs = "i", xaxs = "i", xlab = "", ylab = "", xaxt = "n")
legend("bottomleft", "(a) PPA", bty = "n", inset = c(0,0))
abline(coef(ppa.lm),lty=2)
axis(1, at = seq(2000,2018,2), labels = NA, tick = TRUE)

plot(ppd$year, ppd$mean, type='n', xlim = c(2000,2018), ylim = rev(c(30,60)), yaxs = "i", xaxs = "i", xlab = "", ylab = "", xaxt = "n")
lines(ppd$year, ppd$c5, col = 'darkslategray1', xlim = c(2000,2018), ylim = rev(c(30,60)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(ppd$year, ppd$c95, col = 'darkslategray1', xlim = c(2000,2018), ylim = rev(c(30,60)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(ppd$year, rev(ppd$year)), c(ppd$c95, rev(ppd$c5)), col = "darkslategray1", border = NA)
par(new=T)
plot(ppd$year, ppd$mean, xlim = c(2000,2018), ylim = rev(c(30,60)), yaxs = "i", xaxs = "i",  xlab = "", ylab = "", xaxt = "n", type = "p", lwd=2, col = "darkslategray4")
par(new=T)
plot(ppd$year, ppd$mean, xlim = c(2000,2018), ylim = rev(c(30,60)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "darkslategray4", yaxs = "i", xaxs = "i", xlab = "", ylab = "", xaxt = "n")
legend("bottomleft", "(b) PPD", bty = "n", inset = c(0,0))
abline(coef(ppd.lm),lty=2)
axis(1, at = seq(2000,2018,2), labels = NA, tick = TRUE)

plot(air$year, air$mean, type='n', xlim = c(2000,2018), ylim = rev(c(30,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "", xaxt = "n")
lines(air$year, air$c5, col = 'darkslategray1', xlim = c(2000,2018), ylim = rev(c(30,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(air$year, air$c95, col = 'darkslategray1', xlim = c(2000,2018), ylim = rev(c(30,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(air$year, rev(air$year)), c(air$c95, rev(air$c5)), col = "darkslategray1", border = NA)
par(new=T)
plot(air$year, air$mean, xlim = c(2000,2018), ylim = rev(c(30,100)), yaxs = "i", xaxs = "i",  xlab = "", ylab = "", xaxt = "n", type = "p", lwd=2, col = "darkslategray4")
par(new=T)
plot(air$year, air$mean, xlim = c(2000,2018), ylim = rev(c(30,100)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "darkslategray4", yaxs = "i", xaxs = "i", xlab = "", ylab = "", xaxt = "n")
legend("bottomleft", "(c) AIR", bty = "n", inset = c(0,0))
abline(coef(air.lm),lty=2)
axis(1, at = seq(2000,2018,2), labels = NA, tick = TRUE)

plot(blk$year, blk$mean, type='n', xlim = c(2000,2018), ylim = rev(c(30,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "", xaxt = "n")
lines(blk$year, blk$c5, col = 'darkslategray1', xlim = c(2000,2018), ylim = rev(c(30,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(blk$year, blk$c95, col = 'darkslategray1', xlim = c(2000,2018), ylim = rev(c(30,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(blk$year, rev(blk$year)), c(blk$c95, rev(blk$c5)), col = "darkslategray1", border = NA)
par(new=T)
plot(blk$year, blk$mean, xlim = c(2000,2018), ylim = rev(c(30,100)), yaxs = "i", xaxs = "i",  xlab = "", ylab = "", xaxt = "n", type = "p", lwd=2, col = "darkslategray4")
par(new=T)
plot(blk$year, blk$mean, xlim = c(2000,2018), ylim = rev(c(30,100)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "darkslategray4", yaxs = "i", xaxs = "i", xlab = "", ylab = "", xaxt = "n")
legend("bottomleft", "(d) BLK", bty = "n", inset = c(0,0))
# abline(coef(blk.lm),lty=2)
axis(1, at = seq(2000,2018,2), labels = NA, tick = TRUE)

#abline(coef(blk.lm),lty=2)
axis(1, at = seq(2000,2018,2), labels = seq(2000,2018,2), tick = TRUE)
mtext(side = 2, "Thaw depth (cm)", outer = TRUE)
mtext(side = 1, "Year", outer = TRUE, line = 2)



