library(vegan)
library(MASS) # Use for glm.nb
library(car) # Use to get VIF
library(pscl) # Zero inflated models
library(lmtest) # Use to compare ZIP and ZINB models
library(visreg)

rm(list = ls())

## Read in the GTREE data for Churchill, Mac Pass, and Wolf Creek
plot.wc <- read.csv("~/Desktop/Workspace/WolfCreek/plot.csv") # Wolf Creek
plot.ch <- read.csv("~/Desktop/Workspace/gtree/gtree_ch.csv") # Churchill
plot.mm <- read.csv("~/Desktop/Workspace/gtree/gtree_mm.csv") # Mac Pass

plot(table(plot.wc$germ.0.area), ylab = "",xlab = "") # Have a look at the zero-inflation
mtext(side=2, "Frequency", adj=0.5, line=2.5)
mtext(side=1, "Number of seedlings", adj=0.5, line=2.5)

# Need to adjust the data for different numbers of seeds planted each year (100 in some, 50 in others),
# and make them count data so we can analyze them using hurdle models
temp.wc <- plot.wc$germ.prop.via*100                                  # Convert the 0-1 data to 0-100
plot.wc$response <- as.integer(round(temp.wc,0))                      # Make an integer for the zero-inflated modelling
plot(table(plot.wc$response))                                         # Have a look at the zero-inflation
tmp <- plot.wc[,c(25:34,39)]
tmp2 <- decostand(tmp, "range")                                       # Standardize the explanatory variables to 0-1
gtree <- cbind(plot.wc[c(1:3,6)], plot.wc[42], tmp2)                  # Bind the data together
names(gtree) <- c(names(plot.wc[c(1:3,6)]), "response", names(tmp2))  # Rename the columns
gtree$aspect <- factor(c(rep(c("north","south"), each=240), 
                         rep("north", 240)))                          # Add aspect factors

panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}

pairs(gtree[,c(5,16:17)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

####### WOLF CREEK #######

##########################
## Pre-amble: Compare the different sites for each species

# Pine
# There were virtually no natural seedlings so only focus on the seeded treatments
gtree.wc <- subset(gtree, treatment == "seeded" | treatment == "seeded.scarified")

## Plot only the seeded treatments
wc.pine <- subset(gtree.wc, species == "lp")
wc.pine.nalp.13 <- subset(wc.pine, site == "Nalp" & sow.year == "2013")
wc.pine.nalp.14 <- subset(wc.pine, site == "Nalp" & sow.year == "2014")
wc.pine.ncut.13 <- subset(wc.pine, site == "Ncut" & sow.year == "2013")
wc.pine.ncut.14 <- subset(wc.pine, site == "Ncut" & sow.year == "2014")
wc.pine.nshr.13 <- subset(wc.pine, site == "Nshr" & sow.year == "2013")
wc.pine.nshr.14 <- subset(wc.pine, site == "Nshr" & sow.year == "2014")
wc.pine.nfor.13 <- subset(wc.pine, site == "Nfor" & sow.year == "2013")
wc.pine.nfor.14 <- subset(wc.pine, site == "Nfor" & sow.year == "2014")
wc.pine.salp.14 <- subset(wc.pine, site == "Salp" & sow.year == "2014")
wc.pine.scut.14 <- subset(wc.pine, site == "Scut" & sow.year == "2014")
wc.pine.sshr.14 <- subset(wc.pine, site == "Sshr" & sow.year == "2014")
wc.pine.sfor.14 <- subset(wc.pine, site == "Sfor" & sow.year == "2014")

wc.spruce <- subset(gtree.wc, species == "ws")
wc.spruce.nalp.13 <- subset(wc.spruce, site == "Nalp" & sow.year == "2013")
wc.spruce.nalp.14 <- subset(wc.spruce, site == "Nalp" & sow.year == "2014")
wc.spruce.ncut.13 <- subset(wc.spruce, site == "Ncut" & sow.year == "2013")
wc.spruce.ncut.14 <- subset(wc.spruce, site == "Ncut" & sow.year == "2014")
wc.spruce.nshr.13 <- subset(wc.spruce, site == "Nshr" & sow.year == "2013")
wc.spruce.nshr.14 <- subset(wc.spruce, site == "Nshr" & sow.year == "2014")
wc.spruce.nfor.13 <- subset(wc.spruce, site == "Nfor" & sow.year == "2013")
wc.spruce.nfor.14 <- subset(wc.spruce, site == "Nfor" & sow.year == "2014")
wc.spruce.salp.14 <- subset(wc.spruce, site == "Salp" & sow.year == "2014")
wc.spruce.scut.14 <- subset(wc.spruce, site == "Scut" & sow.year == "2014")
wc.spruce.sshr.14 <- subset(wc.spruce, site == "Sshr" & sow.year == "2014")
wc.spruce.sfor.14 <- subset(wc.spruce, site == "Sfor" & sow.year == "2014")

wc.fir <- subset(gtree.wc, species == "sf")
wc.fir.nalp.13 <- subset(wc.fir, site == "Nalp" & sow.year == "2013")
wc.fir.nalp.14 <- subset(wc.fir, site == "Nalp" & sow.year == "2014")
wc.fir.ncut.13 <- subset(wc.fir, site == "Ncut" & sow.year == "2013")
wc.fir.ncut.14 <- subset(wc.fir, site == "Ncut" & sow.year == "2014")
wc.fir.nshr.13 <- subset(wc.fir, site == "Nshr" & sow.year == "2013")
wc.fir.nshr.14 <- subset(wc.fir, site == "Nshr" & sow.year == "2014")
wc.fir.nfor.13 <- subset(wc.fir, site == "Nfor" & sow.year == "2013")
wc.fir.nfor.14 <- subset(wc.fir, site == "Nfor" & sow.year == "2014")
wc.fir.salp.14 <- subset(wc.fir, site == "Salp" & sow.year == "2014")
wc.fir.scut.14 <- subset(wc.fir, site == "Scut" & sow.year == "2014")
wc.fir.sshr.14 <- subset(wc.fir, site == "Sshr" & sow.year == "2014")
wc.fir.sfor.14 <- subset(wc.fir, site == "Sfor" & sow.year == "2014")

pine.nalp.2013.x1a <- factor(as.integer(wc.pine.nalp.13$treatment), levels = c( "3","4"))
pine.nalp.2014.x1a <- factor(as.integer(wc.pine.nalp.14$treatment), levels = c( "3","4"))
pine.ncut.2013.x1a <- factor(as.integer(wc.pine.ncut.13$treatment), levels = c( "3","4"))
pine.ncut.2014.x1a <- factor(as.integer(wc.pine.ncut.14$treatment), levels = c( "3","4"))
pine.nshr.2013.x1a <- factor(as.integer(wc.pine.nshr.13$treatment), levels = c( "3","4"))
pine.nshr.2014.x1a <- factor(as.integer(wc.pine.nshr.14$treatment), levels = c( "3","4"))
pine.nfor.2013.x1a <- factor(as.integer(wc.pine.nfor.13$treatment), levels = c( "3","4"))
pine.nfor.2014.x1a <- factor(as.integer(wc.pine.nfor.14$treatment), levels = c( "3","4"))
pine.salp.2014.x1a <- factor(as.integer(wc.pine.salp.14$treatment), levels = c( "3","4"))
pine.scut.2014.x1a <- factor(as.integer(wc.pine.scut.14$treatment), levels = c( "3","4"))
pine.sshr.2014.x1a <- factor(as.integer(wc.pine.sshr.14$treatment), levels = c( "3","4"))
pine.sfor.2014.x1a <- factor(as.integer(wc.pine.sfor.14$treatment), levels = c( "3","4"))

spruce.nalp.2013.x1a <- factor(as.integer(wc.spruce.nalp.13$treatment), levels = c( "3","4"))
spruce.nalp.2014.x1a <- factor(as.integer(wc.spruce.nalp.14$treatment), levels = c( "3","4"))
spruce.ncut.2013.x1a <- factor(as.integer(wc.spruce.ncut.13$treatment), levels = c( "3","4"))
spruce.ncut.2014.x1a <- factor(as.integer(wc.spruce.ncut.14$treatment), levels = c( "3","4"))
spruce.nshr.2013.x1a <- factor(as.integer(wc.spruce.nshr.13$treatment), levels = c( "3","4"))
spruce.nshr.2014.x1a <- factor(as.integer(wc.spruce.nshr.14$treatment), levels = c( "3","4"))
spruce.nfor.2013.x1a <- factor(as.integer(wc.spruce.nfor.13$treatment), levels = c( "3","4"))
spruce.nfor.2014.x1a <- factor(as.integer(wc.spruce.nfor.14$treatment), levels = c( "3","4"))
spruce.salp.2014.x1a <- factor(as.integer(wc.spruce.salp.14$treatment), levels = c( "3","4"))
spruce.scut.2014.x1a <- factor(as.integer(wc.spruce.scut.14$treatment), levels = c( "3","4"))
spruce.sshr.2014.x1a <- factor(as.integer(wc.spruce.sshr.14$treatment), levels = c( "3","4"))
spruce.sfor.2014.x1a <- factor(as.integer(wc.spruce.sfor.14$treatment), levels = c( "3","4"))

fir.nalp.2013.x1a <- factor(as.integer(wc.fir.nalp.13$treatment), levels = c( "3","4"))
fir.nalp.2014.x1a <- factor(as.integer(wc.fir.nalp.14$treatment), levels = c( "3","4"))
fir.ncut.2013.x1a <- factor(as.integer(wc.fir.ncut.13$treatment), levels = c( "3","4"))
fir.ncut.2014.x1a <- factor(as.integer(wc.fir.ncut.14$treatment), levels = c( "3","4"))
fir.nshr.2013.x1a <- factor(as.integer(wc.fir.nshr.13$treatment), levels = c( "3","4"))
fir.nshr.2014.x1a <- factor(as.integer(wc.fir.nshr.14$treatment), levels = c( "3","4"))
fir.nfor.2013.x1a <- factor(as.integer(wc.fir.nfor.13$treatment), levels = c( "3","4"))
fir.nfor.2014.x1a <- factor(as.integer(wc.fir.nfor.14$treatment), levels = c( "3","4"))
fir.salp.2014.x1a <- factor(as.integer(wc.fir.salp.14$treatment), levels = c( "3","4"))
fir.scut.2014.x1a <- factor(as.integer(wc.fir.scut.14$treatment), levels = c( "3","4"))
fir.sshr.2014.x1a <- factor(as.integer(wc.fir.sshr.14$treatment), levels = c( "3","4"))
fir.sfor.2014.x1a <- factor(as.integer(wc.fir.sfor.14$treatment), levels = c( "3","4"))

par(mfrow = c(4, 1)) # Export at 3 x 7
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,0.1))

## Pine

## North-facing
# Nalp
boxplot(wc.pine.nalp.13$response ~ pine.nalp.2013.x1a, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(wc.pine.nalp.14$response ~ pine.nalp.2014.x1a, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(wc.pine.nalp.13$response ~ pine.nalp.2013.x1a, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(wc.pine.nalp.14$response ~ pine.nalp.2014.x1a, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(a) NF alpine", bty = "n", inset = c(0.005,-0.1))

# Ncut
boxplot(wc.pine.ncut.13$response ~ pine.ncut.2013.x1a, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(wc.pine.ncut.14$response ~ pine.ncut.2014.x1a, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(wc.pine.ncut.13$response ~ pine.ncut.2013.x1a, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(wc.pine.ncut.14$response ~ pine.ncut.2014.x1a, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(b) NF shrub - cut", bty = "n", inset = c(0.005,-0.1))

# Nshr
boxplot(wc.pine.nshr.13$response ~ pine.nshr.2013.x1a, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(wc.pine.nshr.14$response ~ pine.nshr.2014.x1a, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(wc.pine.nshr.13$response ~ pine.nshr.2013.x1a, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(wc.pine.nshr.14$response ~ pine.nshr.2014.x1a, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(c) NF shrub", bty = "n", inset = c(0.005,-0.1))

# Nfor
boxplot(wc.pine.nfor.13$response ~ pine.nfor.2013.x1a, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(wc.pine.nfor.14$response ~ pine.nfor.2014.x1a, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(wc.pine.nfor.13$response ~ pine.nfor.2013.x1a, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(wc.pine.nfor.14$response ~ pine.nfor.2014.x1a, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(d) NF forest", bty = "n", inset = c(0.005,-0.1))
axis(1, at = c(1,2,4,5), labels = c("14","15","14","15"), tick = TRUE)
axis(1, at = c(1.5,4.5), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1.25)
axis(1, at = 3, labels = "Seeded", tick = FALSE, line = 2.85)

mtext(side = 2, "Number of seedlings", outer = TRUE)
par(xpd = NA)
rect(0.5,-70,5.5,-64, col = 'blue', border = NA)

## South-facing
par(mfrow = c(4, 1)) # Export at 3 x 7
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,0.1))

# Salp
boxplot(wc.pine.salp.14$response ~ pine.salp.2014.x1a, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
stripchart(wc.pine.salp.14$response ~ pine.salp.2014.x1a, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(a) SF alpine", bty = "n", inset = c(0.005,-0.1))

# Scut
boxplot(wc.pine.scut.14$response ~ pine.scut.2014.x1a, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
stripchart(wc.pine.scut.14$response ~ pine.scut.2014.x1a, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(b) SF shrub - cut", bty = "n", inset = c(0.005,-0.1))

# Sshr
boxplot(wc.pine.sshr.14$response ~ pine.sshr.2014.x1a, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
stripchart(wc.pine.sshr.14$response ~ pine.sshr.2014.x1a, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(c) SF shrub", bty = "n", inset = c(0.005,-0.1))

# Nfor
boxplot(wc.pine.sfor.14$response ~ pine.sfor.2014.x1a, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
stripchart(wc.pine.sfor.14$response ~ pine.sfor.2014.x1a, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(d) SF forest", bty = "n", inset = c(0.005,-0.1))
axis(1, at = c(1,2,4,5), labels = c("14","15","14","15"), tick = TRUE)
axis(1, at = c(1.5,4.5), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1.25)
axis(1, at = 3, labels = "Seeded", tick = FALSE, line = 2.85)

mtext(side = 2, "Number of seedlings", outer = TRUE)
par(xpd = NA)
rect(0.5,-70,5.5,-64, col = 'blue', border = NA)


## Spruce

## North-facing
par(mfrow = c(4, 1)) # Export at 3 x 7
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,0.1))

# Nalp
boxplot(wc.spruce.nalp.13$response ~ spruce.nalp.2013.x1a, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(wc.spruce.nalp.14$response ~ spruce.nalp.2014.x1a, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(wc.spruce.nalp.13$response ~ spruce.nalp.2013.x1a, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(wc.spruce.nalp.14$response ~ spruce.nalp.2014.x1a, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(a) NF alpine", bty = "n", inset = c(0.005,-0.1))

# Ncut
boxplot(wc.spruce.ncut.13$response ~ spruce.ncut.2013.x1a, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(wc.spruce.ncut.14$response ~ spruce.ncut.2014.x1a, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(wc.spruce.ncut.13$response ~ spruce.ncut.2013.x1a, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(wc.spruce.ncut.14$response ~ spruce.ncut.2014.x1a, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(b) NF shrub - cut", bty = "n", inset = c(0.005,-0.1))

# Nshr
boxplot(wc.spruce.nshr.13$response ~ spruce.nshr.2013.x1a, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(wc.spruce.nshr.14$response ~ spruce.nshr.2014.x1a, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(wc.spruce.nshr.13$response ~ spruce.nshr.2013.x1a, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(wc.spruce.nshr.14$response ~ spruce.nshr.2014.x1a, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(c) NF shrub", bty = "n", inset = c(0.005,-0.1))

# Nfor
boxplot(wc.spruce.nfor.13$response ~ spruce.nfor.2013.x1a, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(wc.spruce.nfor.14$response ~ spruce.nfor.2014.x1a, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(wc.spruce.nfor.13$response ~ spruce.nfor.2013.x1a, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(wc.spruce.nfor.14$response ~ spruce.nfor.2014.x1a, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(d) NF forest", bty = "n", inset = c(0.005,-0.1))
axis(1, at = c(1,2,4,5), labels = c("14","15","14","15"), tick = TRUE)
axis(1, at = c(1.5,4.5), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1.25)
axis(1, at = 3, labels = "Seeded", tick = FALSE, line = 2.85)

mtext(side = 2, "Number of seedlings", outer = TRUE)
par(xpd = NA)
rect(0.5,-70,5.5,-64, col = 'blue', border = NA)

## South-facing
par(mfrow = c(4, 1)) # Export at 3 x 7
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,0.1))

# Salp
boxplot(wc.spruce.salp.14$response ~ spruce.salp.2014.x1a, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
stripchart(wc.spruce.salp.14$response ~ spruce.salp.2014.x1a, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(a) SF alpine", bty = "n", inset = c(0.005,-0.1))

# Scut
boxplot(wc.spruce.scut.14$response ~ spruce.scut.2014.x1a, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
stripchart(wc.spruce.scut.14$response ~ spruce.scut.2014.x1a, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(b) SF shrub - cut", bty = "n", inset = c(0.005,-0.1))

# Sshr
boxplot(wc.spruce.sshr.14$response ~ spruce.sshr.2014.x1a, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
stripchart(wc.spruce.sshr.14$response ~ spruce.sshr.2014.x1a, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(c) SF shrub", bty = "n", inset = c(0.005,-0.1))

# Sfor
boxplot(wc.spruce.sfor.14$response ~ spruce.sfor.2014.x1a, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
stripchart(wc.spruce.sfor.14$response ~ spruce.sfor.2014.x1a, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(d) SF forest", bty = "n", inset = c(0.005,-0.1))
axis(1, at = c(1,2,4,5), labels = c("14","15","14","15"), tick = TRUE)
axis(1, at = c(1.5,4.5), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1.25)
axis(1, at = 3, labels = "Seeded", tick = FALSE, line = 2.85)

mtext(side = 2, "Number of seedlings", outer = TRUE)
par(xpd = NA)
rect(0.5,-70,5.5,-64, col = 'blue', border = NA)


## Fir

## North-facing
par(mfrow = c(4, 1)) # Export at 3 x 7
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,0.1))

# Nalp
boxplot(wc.fir.nalp.13$response ~ fir.nalp.2013.x1a, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(wc.fir.nalp.14$response ~ fir.nalp.2014.x1a, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(wc.fir.nalp.13$response ~ fir.nalp.2013.x1a, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(wc.fir.nalp.14$response ~ fir.nalp.2014.x1a, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(a) NF alpine", bty = "n", inset = c(0.005,-0.1))

# Ncut
boxplot(wc.fir.ncut.13$response ~ fir.ncut.2013.x1a, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(wc.fir.ncut.14$response ~ fir.ncut.2014.x1a, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(wc.fir.ncut.13$response ~ fir.ncut.2013.x1a, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(wc.fir.ncut.14$response ~ fir.ncut.2014.x1a, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(b) NF shrub - cut", bty = "n", inset = c(0.005,-0.1))

# Nshr
boxplot(wc.fir.nshr.13$response ~ fir.nshr.2013.x1a, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(wc.fir.nshr.14$response ~ fir.nshr.2014.x1a, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(wc.fir.nshr.13$response ~ fir.nshr.2013.x1a, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(wc.fir.nshr.14$response ~ fir.nshr.2014.x1a, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(c) NF shrub", bty = "n", inset = c(0.005,-0.1))

# Nfor
boxplot(wc.fir.nfor.13$response ~ fir.nfor.2013.x1a, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(wc.fir.nfor.14$response ~ fir.nfor.2014.x1a, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(wc.fir.nfor.13$response ~ fir.nfor.2013.x1a, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(wc.fir.nfor.14$response ~ fir.nfor.2014.x1a, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(d) NF forest", bty = "n", inset = c(0.005,-0.1))
axis(1, at = c(1,2,4,5), labels = c("14","15","14","15"), tick = TRUE)
axis(1, at = c(1.5,4.5), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1.25)
axis(1, at = 3, labels = "Seeded", tick = FALSE, line = 2.85)

mtext(side = 2, "Number of seedlings", outer = TRUE)
par(xpd = NA)
rect(0.5,-70,5.5,-64, col = 'blue', border = NA)

## South-facing
par(mfrow = c(4, 1)) # Export at 3 x 7
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,0.1))

# Salp
boxplot(wc.fir.salp.14$response ~ fir.salp.2014.x1a, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
stripchart(wc.fir.salp.14$response ~ fir.salp.2014.x1a, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(a) SF alpine", bty = "n", inset = c(0.005,-0.1))

# Scut
boxplot(wc.fir.scut.14$response ~ fir.scut.2014.x1a, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
stripchart(wc.fir.scut.14$response ~ fir.scut.2014.x1a, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(b) SF shrub - cut", bty = "n", inset = c(0.005,-0.1))

# Sshr
boxplot(wc.fir.sshr.14$response ~ fir.sshr.2014.x1a, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
stripchart(wc.fir.sshr.14$response ~ fir.sshr.2014.x1a, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(c) SF shrub", bty = "n", inset = c(0.005,-0.1))

# Nfor
boxplot(wc.fir.sfor.14$response ~ fir.sfor.2014.x1a, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
stripchart(wc.fir.sfor.14$response ~ fir.sfor.2014.x1a, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(d) SF forest", bty = "n", inset = c(0.005,-0.1))
axis(1, at = c(1,2,4,5), labels = c("14","15","14","15"), tick = TRUE)
axis(1, at = c(1.5,4.5), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1.25)
axis(1, at = 3, labels = "Seeded", tick = FALSE, line = 2.85)

mtext(side = 2, "Number of seedlings", outer = TRUE)
par(xpd = NA)
rect(0.5,-70,5.5,-64, col = 'blue', border = NA)


################################
################################
################################
################################

## Mac pass

# Need to adjust the data for different numbers of seeds planted each year (100 in some, 50 in others),
# and make them count data so we can analyze them using hurdle models
temp.mm <- plot.mm$germ.prop.via*100                                  # Convert the 0-1 data to 0-100
plot.mm$response <- as.integer(round(temp.mm,0))                      # Make an integer for the zero-inflated modelling
plot(table(plot.mm$response))                                         # Have a look at the zero-inflation
tmp <- plot.mm[,c(24:31)]
tmp2 <- decostand(tmp, "range")                                       # Standardize the explanatory variables to 0-1
gtree <- cbind(plot.mm[c(1:4,6,22,23)], plot.mm[33], tmp2)                  # Bind the data together
names(gtree) <- c(names(plot.mm[c(1:4,6,22,23)]), "response", names(tmp2))  # Rename the columns
gtree$aspect <- factor(c(rep("north", 20), 
                         rep("south", 60),
                         rep("north", 20), 
                         rep("south", 60),
                         rep("north", 20), 
                         rep("south", 60)))                          # Add aspect factors
gtree$sow.year <- factor(gtree$sow.year)

##########################
## Pre-amble: Compare the different sites for each species
# There were virtually no natural seedlings so only focus on the seeded treatments
gtree.mm <- subset(gtree, treatment == "seeded" | treatment == "seeded.scarified")

# Fir
mm.fir <- subset(gtree.mm, species == "fir")

mm.fir.nalp.14 <- subset(mm.fir, site == "nalp" & sow.year == "2014")
mm.fir.nalp.15 <- subset(mm.fir, site == "nalp" & sow.year == "2015")
mm.fir.salp.13 <- subset(mm.fir, site == "salp" & sow.year == "2013")
mm.fir.salp.14 <- subset(mm.fir, site == "salp" & sow.year == "2014")
mm.fir.salp.15 <- subset(mm.fir, site == "salp" & sow.year == "2015")
mm.fir.scut.13 <- subset(mm.fir, site == "scut" & sow.year == "2013")
mm.fir.scut.14 <- subset(mm.fir, site == "scut" & sow.year == "2014")
mm.fir.scut.15 <- subset(mm.fir, site == "scut" & sow.year == "2015")
mm.fir.sshr.13 <- subset(mm.fir, site == "sshr" & sow.year == "2013")
mm.fir.sshr.14 <- subset(mm.fir, site == "sshr" & sow.year == "2014")
mm.fir.sshr.15 <- subset(mm.fir, site == "sshr" & sow.year == "2015")

fir.nalp.2014.x1a <- factor(as.integer(mm.fir.nalp.14$treatment), levels = c("3", "4"))
fir.nalp.2015.x1a <- factor(as.integer(mm.fir.nalp.15$treatment), levels = c("3", "4"))
fir.salp.2013.x1a <- factor(as.integer(mm.fir.salp.13$treatment), levels = c("3", "4"))
fir.salp.2014.x1a <- factor(as.integer(mm.fir.salp.14$treatment), levels = c("3", "4"))
fir.salp.2015.x1a <- factor(as.integer(mm.fir.salp.15$treatment), levels = c("3", "4"))
fir.scut.2013.x1a <- factor(as.integer(mm.fir.scut.13$treatment), levels = c("3", "4"))
fir.scut.2014.x1a <- factor(as.integer(mm.fir.scut.14$treatment), levels = c("3", "4"))
fir.scut.2015.x1a <- factor(as.integer(mm.fir.scut.15$treatment), levels = c("3", "4"))
fir.sshr.2013.x1a <- factor(as.integer(mm.fir.sshr.13$treatment), levels = c("3", "4"))
fir.sshr.2014.x1a <- factor(as.integer(mm.fir.sshr.14$treatment), levels = c("3", "4"))
fir.sshr.2015.x1a <- factor(as.integer(mm.fir.sshr.15$treatment), levels = c("3", "4"))

par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,0.1))

# Nalp
boxplot(mm.fir.nalp.14$response ~ fir.nalp.2014.x1a, at = c(2,6), xlim = c(0,8), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(mm.fir.nalp.15$response ~ fir.nalp.2015.x1a, at = c(3,7), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(mm.fir.nalp.14$response ~ fir.nalp.2014.x1a, at = c(2,6), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(mm.fir.nalp.15$response ~ fir.nalp.2015.x1a, at = c(3,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,3,5,6,7), labels = c("","","","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(a) NF alpine", bty = "n", inset = c(0.005,-0.1))

# Salp
boxplot(mm.fir.salp.13$response ~ fir.salp.2013.x1a, at = c(1,5), xlim = c(0,8), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(mm.fir.salp.14$response ~ fir.salp.2014.x1a, at = c(2,6), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
boxplot(mm.fir.salp.15$response ~ fir.salp.2015.x1a, at = c(3,7), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(mm.fir.salp.13$response ~ fir.salp.2013.x1a, at = c(1,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(mm.fir.salp.14$response ~ fir.salp.2014.x1a, at = c(2,6), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(mm.fir.salp.15$response ~ fir.salp.2015.x1a, at = c(3,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,3,5,6,7), labels = c("","","","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(b) SF alpine", bty = "n", inset = c(0.005,-0.1))

# Scut
boxplot(mm.fir.scut.13$response ~ fir.scut.2013.x1a, at = c(1,5), xlim = c(0,8), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("white","white","darkorange","yellow"))
boxplot(mm.fir.scut.14$response ~ fir.scut.2014.x1a, at = c(2,6), xaxt = "n", yaxt = "n", add = TRUE, col = c("white","white","darkorange","yellow"))
boxplot(mm.fir.scut.15$response ~ fir.scut.2015.x1a, at = c(3,7), xaxt = "n", yaxt = "n", add = TRUE, col = c("white","white","darkorange","yellow"))
stripchart(mm.fir.scut.13$response ~ fir.scut.2013.x1a, at = c(1,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(mm.fir.scut.14$response ~ fir.scut.2014.x1a, at = c(2,6), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(mm.fir.scut.15$response ~ fir.scut.2015.x1a, at = c(3,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,3,5,6,7), labels = c("","","","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(c) SF shrub - cut", bty = "n", inset = c(0.005,-0.1))

# Sshr
boxplot(mm.fir.sshr.13$response ~ fir.sshr.2013.x1a, at = c(1,5), xlim = c(0,8), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(mm.fir.sshr.14$response ~ fir.sshr.2014.x1a, at = c(2,6), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
boxplot(mm.fir.sshr.15$response ~ fir.sshr.2015.x1a, at = c(3,7), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(mm.fir.sshr.13$response ~ fir.sshr.2013.x1a, at = c(1,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(mm.fir.sshr.14$response ~ fir.sshr.2014.x1a, at = c(2,6), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(mm.fir.sshr.15$response ~ fir.sshr.2015.x1a, at = c(3,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
legend("topleft", "(d) SF shrub", bty = "n", inset = c(0.005,-0.1))

axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,3,5,6,7), labels = c("14","15","16","14","15","16"), tick = TRUE)
axis(1, at = c(2,6), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1.25)
axis(1, at = 4, labels = "Seeded", tick = FALSE, line = 2.85)

mtext(side = 2, "Number of seedlings", outer = TRUE)
par(xpd = NA)
rect(0.5,-70,7.5,-64, col = 'blue', border = NA)


# Spruce
mm.spruce <- subset(gtree.mm, species == "spruce")

mm.spruce.nalp.14 <- subset(mm.spruce, site == "nalp" & sow.year == "2014")
mm.spruce.nalp.15 <- subset(mm.spruce, site == "nalp" & sow.year == "2015")
mm.spruce.salp.13 <- subset(mm.spruce, site == "salp" & sow.year == "2013")
mm.spruce.salp.14 <- subset(mm.spruce, site == "salp" & sow.year == "2014")
mm.spruce.salp.15 <- subset(mm.spruce, site == "salp" & sow.year == "2015")
mm.spruce.scut.13 <- subset(mm.spruce, site == "scut" & sow.year == "2013")
mm.spruce.scut.14 <- subset(mm.spruce, site == "scut" & sow.year == "2014")
mm.spruce.scut.15 <- subset(mm.spruce, site == "scut" & sow.year == "2015")
mm.spruce.sshr.13 <- subset(mm.spruce, site == "sshr" & sow.year == "2013")
mm.spruce.sshr.14 <- subset(mm.spruce, site == "sshr" & sow.year == "2014")
mm.spruce.sshr.15 <- subset(mm.spruce, site == "sshr" & sow.year == "2015")

spruce.nalp.2014.x1a <- factor(as.integer(mm.spruce.nalp.14$treatment), levels = c("3", "4"))
spruce.nalp.2015.x1a <- factor(as.integer(mm.spruce.nalp.15$treatment), levels = c("3", "4"))
spruce.salp.2013.x1a <- factor(as.integer(mm.spruce.salp.13$treatment), levels = c("3", "4"))
spruce.salp.2014.x1a <- factor(as.integer(mm.spruce.salp.14$treatment), levels = c("3", "4"))
spruce.salp.2015.x1a <- factor(as.integer(mm.spruce.salp.15$treatment), levels = c("3", "4"))
spruce.scut.2013.x1a <- factor(as.integer(mm.spruce.scut.13$treatment), levels = c("3", "4"))
spruce.scut.2014.x1a <- factor(as.integer(mm.spruce.scut.14$treatment), levels = c("3", "4"))
spruce.scut.2015.x1a <- factor(as.integer(mm.spruce.scut.15$treatment), levels = c("3", "4"))
spruce.sshr.2013.x1a <- factor(as.integer(mm.spruce.sshr.13$treatment), levels = c("3", "4"))
spruce.sshr.2014.x1a <- factor(as.integer(mm.spruce.sshr.14$treatment), levels = c("3", "4"))
spruce.sshr.2015.x1a <- factor(as.integer(mm.spruce.sshr.15$treatment), levels = c("3", "4"))

par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,0.1))

# Nalp
boxplot(mm.spruce.nalp.14$response ~ spruce.nalp.2014.x1a, at = c(2,6), xlim = c(0,8), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(mm.spruce.nalp.15$response ~ spruce.nalp.2015.x1a, at = c(3,7), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(mm.spruce.nalp.14$response ~ spruce.nalp.2014.x1a, at = c(2,6), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(mm.spruce.nalp.15$response ~ spruce.nalp.2015.x1a, at = c(3,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,3,5,6,7), labels = c("","","","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(a) NF alpine", bty = "n", inset = c(0.005,-0.1))

# Salp
boxplot(mm.spruce.salp.13$response ~ spruce.salp.2013.x1a, at = c(1,5), xlim = c(0,8), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(mm.spruce.salp.14$response ~ spruce.salp.2014.x1a, at = c(2,6), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
boxplot(mm.spruce.salp.15$response ~ spruce.salp.2015.x1a, at = c(3,7), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(mm.spruce.salp.13$response ~ spruce.salp.2013.x1a, at = c(1,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(mm.spruce.salp.14$response ~ spruce.salp.2014.x1a, at = c(2,6), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(mm.spruce.salp.15$response ~ spruce.salp.2015.x1a, at = c(3,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,3,5,6,7), labels = c("","","","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(b) SF alpine", bty = "n", inset = c(0.005,-0.1))

# Scut
boxplot(mm.spruce.scut.13$response ~ spruce.scut.2013.x1a, at = c(1,5), xlim = c(0,8), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("white","white","darkorange","yellow"))
boxplot(mm.spruce.scut.14$response ~ spruce.scut.2014.x1a, at = c(2,6), xaxt = "n", yaxt = "n", add = TRUE, col = c("white","white","darkorange","yellow"))
boxplot(mm.spruce.scut.15$response ~ spruce.scut.2015.x1a, at = c(3,7), xaxt = "n", yaxt = "n", add = TRUE, col = c("white","white","darkorange","yellow"))
stripchart(mm.spruce.scut.13$response ~ spruce.scut.2013.x1a, at = c(1,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(mm.spruce.scut.14$response ~ spruce.scut.2014.x1a, at = c(2,6), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(mm.spruce.scut.15$response ~ spruce.scut.2015.x1a, at = c(3,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,3,5,6,7), labels = c("","","","","",""), tick = TRUE)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
legend("topleft", "(c) SF shrub - cut", bty = "n", inset = c(0.005,-0.1))

# Sshr
boxplot(mm.spruce.sshr.13$response ~ spruce.sshr.2013.x1a, at = c(1,5), xlim = c(0,8), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(mm.spruce.sshr.14$response ~ spruce.sshr.2014.x1a, at = c(2,6), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
boxplot(mm.spruce.sshr.15$response ~ spruce.sshr.2015.x1a, at = c(3,7), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(mm.spruce.sshr.13$response ~ spruce.sshr.2013.x1a, at = c(1,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(mm.spruce.sshr.14$response ~ spruce.sshr.2014.x1a, at = c(2,6), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(mm.spruce.sshr.15$response ~ spruce.sshr.2015.x1a, at = c(3,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
legend("topleft", "(d) SF shrub", bty = "n", inset = c(0.005,-0.1))

axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,3,5,6,7), labels = c("14","15","16","14","15","16"), tick = TRUE)
axis(1, at = c(2,6), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1.25)
axis(1, at = 4, labels = "Seeded", tick = FALSE, line = 2.85)

mtext(side = 2, "Number of seedlings", outer = TRUE)
par(xpd = NA)
rect(0.5,-70,7.5,-64, col = 'blue', border = NA)


################################
################################
################################
################################

# Need to adjust the data for different numbers of seeds planted each year (100 in some, 50 in others),
# and make them count data so we can analyze them using hurdle models
temp.ch <- plot.ch$germ.prop.via*100                                  # Convert the 0-1 data to 0-100
plot.ch$response <- as.integer(round(temp.ch,0))                      # Make an integer for the zero-inflated modelling
plot(table(plot.ch$response))                                         # Have a look at the zero-inflation
tmp <- plot.ch[,c(23:32)]
tmp2 <- decostand(tmp, "range")                                       # Standardize the explanatory variables to 0-1
gtree <- cbind(plot.ch[c(1:7)], plot.ch[34], tmp2)                  # Bind the data together
names(gtree) <- c(names(plot.ch[c(1:7)]), "response", names(tmp2))  # Rename the columns
gtree$sow.year <- factor(gtree$sow.year)

##########################
## Pre-amble: Compare the different sites for each species
# There were virtually no natural seedlings so only focus on the seeded treatments
gtree.ch <- subset(gtree, treatment == "seeded" | treatment == "seeded.scarified")

## White spruce
ch.wspruce <- subset(gtree.ch, species == "ws")

ch.wspruce.tun.13 <- subset(ch.wspruce, site == "TUN" & sow.year == "2013")
ch.wspruce.tun.14 <- subset(ch.wspruce, site == "TUN" & sow.year == "2014")
ch.wspruce.tun.15 <- subset(ch.wspruce, site == "TUN" & sow.year == "2015")
ch.wspruce.tis.13 <- subset(ch.wspruce, site == "TIS" & sow.year == "2013")
ch.wspruce.tis.14 <- subset(ch.wspruce, site == "TIS" & sow.year == "2014")
ch.wspruce.tis.15 <- subset(ch.wspruce, site == "TIS" & sow.year == "2015")
ch.wspruce.rid.13 <- subset(ch.wspruce, site == "RID" & sow.year == "2013")
ch.wspruce.rid.14 <- subset(ch.wspruce, site == "RID" & sow.year == "2014")
ch.wspruce.rid.15 <- subset(ch.wspruce, site == "RID" & sow.year == "2015")
ch.wspruce.wsu.13 <- subset(ch.wspruce, site == "WSU" & sow.year == "2013")
ch.wspruce.wsu.14 <- subset(ch.wspruce, site == "WSU" & sow.year == "2014")
ch.wspruce.wsu.15 <- subset(ch.wspruce, site == "WSU" & sow.year == "2015")

wspruce.tun.2013.x1a <- factor(as.integer(ch.wspruce.tun.13$treatment), levels = c("3","4"))
wspruce.tun.2014.x1a <- factor(as.integer(ch.wspruce.tun.14$treatment), levels = c("3","4"))
wspruce.tun.2015.x1a <- factor(as.integer(ch.wspruce.tun.15$treatment), levels = c("3","4"))
wspruce.tis.2013.x1a <- factor(as.integer(ch.wspruce.tis.13$treatment), levels = c("3","4"))
wspruce.tis.2014.x1a <- factor(as.integer(ch.wspruce.tis.14$treatment), levels = c("3","4"))
wspruce.tis.2015.x1a <- factor(as.integer(ch.wspruce.tis.15$treatment), levels = c("3","4"))
wspruce.rid.2013.x1a <- factor(as.integer(ch.wspruce.rid.13$treatment), levels = c("3","4"))
wspruce.rid.2014.x1a <- factor(as.integer(ch.wspruce.rid.14$treatment), levels = c("3","4"))
wspruce.rid.2015.x1a <- factor(as.integer(ch.wspruce.rid.15$treatment), levels = c("3","4"))
wspruce.wsu.2013.x1a <- factor(as.integer(ch.wspruce.wsu.13$treatment), levels = c("3","4"))
wspruce.wsu.2014.x1a <- factor(as.integer(ch.wspruce.wsu.14$treatment), levels = c("3","4"))
wspruce.wsu.2015.x1a <- factor(as.integer(ch.wspruce.wsu.15$treatment), levels = c("3","4"))


par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,0.1))

# Tundra
boxplot(ch.wspruce.tun.13$response ~ wspruce.tun.2013.x1a, at = c(1,5), xlim = c(0,8), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.wspruce.tun.14$response ~ wspruce.tun.2014.x1a, at = c(2,6), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
boxplot(ch.wspruce.tun.15$response ~ wspruce.tun.2015.x1a, at = c(3,7), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(ch.wspruce.tun.13$response ~ wspruce.tun.2013.x1a, at = c(1,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.wspruce.tun.14$response ~ wspruce.tun.2014.x1a, at = c(2,6), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.wspruce.tun.15$response ~ wspruce.tun.2015.x1a, at = c(3,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,3,5,6,7), labels = c("","","","","",""), tick = TRUE)
legend("topleft", "(a) Tundra", bty = "n", inset = c(0.005,-0.1))

# Tree island
boxplot(ch.wspruce.tis.13$response ~ wspruce.tis.2013.x1a, at = c(1,5), xlim = c(0,8), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.wspruce.tis.14$response ~ wspruce.tis.2014.x1a, at = c(2,6), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
boxplot(ch.wspruce.tis.15$response ~ wspruce.tis.2015.x1a, at = c(3,7), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(ch.wspruce.tis.13$response ~ wspruce.tis.2013.x1a, at = c(1,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.wspruce.tis.14$response ~ wspruce.tis.2014.x1a, at = c(2,6), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.wspruce.tis.15$response ~ wspruce.tis.2015.x1a, at = c(3,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,3,5,6,7), labels = c("","","","","",""), tick = TRUE)
legend("topleft", "(b) Tree island", bty = "n", inset = c(0.005,-0.1))

# Treeline
boxplot(ch.wspruce.rid.13$response ~ wspruce.rid.2013.x1a, at = c(1,5), xlim = c(0,8), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.wspruce.rid.14$response ~ wspruce.rid.2014.x1a, at = c(2,6), xaxt = "n", yaxt = "n", add = TRUE,col = c("darkorange","yellow"))
boxplot(ch.wspruce.rid.15$response ~ wspruce.rid.2015.x1a, at = c(3,7), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(ch.wspruce.rid.13$response ~ wspruce.rid.2013.x1a, at = c(1,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.wspruce.rid.14$response ~ wspruce.rid.2014.x1a, at = c(2,6), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.wspruce.rid.15$response ~ wspruce.rid.2015.x1a, at = c(3,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,3,5,6,7), labels = c("","","","","",""), tick = TRUE)
legend("topleft", "(c) Treeline", bty = "n", inset = c(0.005,-0.1))

# Forest
boxplot(ch.wspruce.wsu.13$response ~ wspruce.wsu.2013.x1a, at = c(1,5), xlim = c(0,8), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.wspruce.wsu.14$response ~ wspruce.wsu.2014.x1a, at = c(2,6), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
boxplot(ch.wspruce.wsu.15$response ~ wspruce.wsu.2015.x1a, at = c(3,7), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(ch.wspruce.wsu.13$response ~ wspruce.wsu.2013.x1a, at = c(1,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.wspruce.wsu.14$response ~ wspruce.wsu.2014.x1a, at = c(2,6), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.wspruce.wsu.15$response ~ wspruce.wsu.2015.x1a, at = c(3,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
legend("topleft", "(d) Forest", bty = "n", inset = c(0.005,-0.1))

axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,3,5,6,7), labels = c("14","15","16","14","15","16"), tick = TRUE)
axis(1, at = c(2,6), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1.25)
axis(1, at = 4, labels = "Seeded", tick = FALSE, line = 2.85)
mtext(side = 2, "Number of seedlings", outer = TRUE)

par(xpd = NA)
rect(0.5,-70,7.5,-64, col = 'blue', border = NA)

## Black spruce
ch.bspruce <- subset(gtree.ch, species == "bs")

ch.bspruce.tun.13 <- subset(ch.bspruce, site == "TUN" & sow.year == "2013")
ch.bspruce.tun.14 <- subset(ch.bspruce, site == "TUN" & sow.year == "2014")
ch.bspruce.tun.15 <- subset(ch.bspruce, site == "TUN" & sow.year == "2015")
ch.bspruce.rid.13 <- subset(ch.bspruce, site == "RID" & sow.year == "2013")
ch.bspruce.rid.14 <- subset(ch.bspruce, site == "RID" & sow.year == "2014")
ch.bspruce.rid.15 <- subset(ch.bspruce, site == "RID" & sow.year == "2015")
ch.bspruce.wsu.13 <- subset(ch.bspruce, site == "WSU" & sow.year == "2013")
ch.bspruce.wsu.14 <- subset(ch.bspruce, site == "WSU" & sow.year == "2014")
ch.bspruce.wsu.15 <- subset(ch.bspruce, site == "WSU" & sow.year == "2015")

bspruce.tun.2013.x1a <- factor(as.integer(ch.bspruce.tun.13$treatment), levels = c("3","4"))
bspruce.tun.2014.x1a <- factor(as.integer(ch.bspruce.tun.14$treatment), levels = c("3","4"))
bspruce.tun.2015.x1a <- factor(as.integer(ch.bspruce.tun.15$treatment), levels = c("3","4"))
bspruce.rid.2013.x1a <- factor(as.integer(ch.bspruce.rid.13$treatment), levels = c("3","4"))
bspruce.rid.2014.x1a <- factor(as.integer(ch.bspruce.rid.14$treatment), levels = c("3","4"))
bspruce.rid.2015.x1a <- factor(as.integer(ch.bspruce.rid.15$treatment), levels = c("3","4"))
bspruce.wsu.2013.x1a <- factor(as.integer(ch.bspruce.wsu.13$treatment), levels = c("3","4"))
bspruce.wsu.2014.x1a <- factor(as.integer(ch.bspruce.wsu.14$treatment), levels = c("3","4"))
bspruce.wsu.2015.x1a <- factor(as.integer(ch.bspruce.wsu.15$treatment), levels = c("3","4"))


par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,0.1))

# Tundra
boxplot(ch.bspruce.tun.13$response ~ bspruce.tun.2013.x1a, at = c(1,5), xlim = c(0,8), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.bspruce.tun.14$response ~ bspruce.tun.2014.x1a, at = c(2,6), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
boxplot(ch.bspruce.tun.15$response ~ bspruce.tun.2015.x1a, at = c(3,7), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(ch.bspruce.tun.13$response ~ bspruce.tun.2013.x1a, at = c(1,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.bspruce.tun.14$response ~ bspruce.tun.2014.x1a, at = c(2,6), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.bspruce.tun.15$response ~ bspruce.tun.2015.x1a, at = c(3,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,3,5,6,7), labels = c("","","","","",""), tick = TRUE)
legend("topleft", "(a) Tundra", bty = "n", inset = c(0.005,-0.1))

# Treeline
boxplot(ch.bspruce.rid.13$response ~ bspruce.rid.2013.x1a, at = c(1,5), xlim = c(0,8), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.bspruce.rid.14$response ~ bspruce.rid.2014.x1a, at = c(2,6), xaxt = "n", yaxt = "n", add = TRUE,col = c("darkorange","yellow"))
boxplot(ch.bspruce.rid.15$response ~ bspruce.rid.2015.x1a, at = c(3,7), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(ch.bspruce.rid.13$response ~ bspruce.rid.2013.x1a, at = c(1,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.bspruce.rid.14$response ~ bspruce.rid.2014.x1a, at = c(2,6), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.bspruce.rid.15$response ~ bspruce.rid.2015.x1a, at = c(3,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,3,5,6,7), labels = c("","","","","",""), tick = TRUE)
legend("topleft", "(b) Treeline", bty = "n", inset = c(0.005,-0.1))
mtext(side = 2, "Number of seedlings", line = 2)

# Forest
boxplot(ch.bspruce.wsu.13$response ~ bspruce.wsu.2013.x1a, at = c(1,5), xlim = c(0,8), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.bspruce.wsu.14$response ~ bspruce.wsu.2014.x1a, at = c(2,6), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
boxplot(ch.bspruce.wsu.15$response ~ bspruce.wsu.2015.x1a, at = c(3,7), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(ch.bspruce.wsu.13$response ~ bspruce.wsu.2013.x1a, at = c(1,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.bspruce.wsu.14$response ~ bspruce.wsu.2014.x1a, at = c(2,6), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.bspruce.wsu.15$response ~ bspruce.wsu.2015.x1a, at = c(3,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
legend("topleft", "(c) Forest", bty = "n", inset = c(0.005,-0.1))

axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,3,5,6,7), labels = c("14","15","16","14","15","16"), tick = TRUE)
axis(1, at = c(2,6), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1.25)
axis(1, at = 4, labels = "Seeded", tick = FALSE, line = 2.85)

par(xpd = NA)
rect(0.5,-70,7.5,-64, col = 'blue', border = NA)


## White spruce
ch.larch <- subset(gtree.ch, species == "ll")

ch.larch.tun.13 <- subset(ch.larch, site == "TUN" & sow.year == "2013")
ch.larch.tun.14 <- subset(ch.larch, site == "TUN" & sow.year == "2014")
ch.larch.tun.15 <- subset(ch.larch, site == "TUN" & sow.year == "2015")
ch.larch.tis.13 <- subset(ch.larch, site == "TIS" & sow.year == "2013")
ch.larch.tis.14 <- subset(ch.larch, site == "TIS" & sow.year == "2014")
ch.larch.tis.15 <- subset(ch.larch, site == "TIS" & sow.year == "2015")
ch.larch.rid.13 <- subset(ch.larch, site == "RID" & sow.year == "2013")
ch.larch.rid.14 <- subset(ch.larch, site == "RID" & sow.year == "2014")
ch.larch.rid.15 <- subset(ch.larch, site == "RID" & sow.year == "2015")
ch.larch.wsu.13 <- subset(ch.larch, site == "WSU" & sow.year == "2013")
ch.larch.wsu.14 <- subset(ch.larch, site == "WSU" & sow.year == "2014")
ch.larch.wsu.15 <- subset(ch.larch, site == "WSU" & sow.year == "2015")

larch.tun.2013.x1a <- factor(as.integer(ch.larch.tun.13$treatment), levels = c("3","4"))
larch.tun.2014.x1a <- factor(as.integer(ch.larch.tun.14$treatment), levels = c("3","4"))
larch.tun.2015.x1a <- factor(as.integer(ch.larch.tun.15$treatment), levels = c("3","4"))
larch.tis.2013.x1a <- factor(as.integer(ch.larch.tis.13$treatment), levels = c("3","4"))
larch.tis.2014.x1a <- factor(as.integer(ch.larch.tis.14$treatment), levels = c("3","4"))
larch.tis.2015.x1a <- factor(as.integer(ch.larch.tis.15$treatment), levels = c("3","4"))
larch.rid.2013.x1a <- factor(as.integer(ch.larch.rid.13$treatment), levels = c("3","4"))
larch.rid.2014.x1a <- factor(as.integer(ch.larch.rid.14$treatment), levels = c("3","4"))
larch.rid.2015.x1a <- factor(as.integer(ch.larch.rid.15$treatment), levels = c("3","4"))
larch.wsu.2013.x1a <- factor(as.integer(ch.larch.wsu.13$treatment), levels = c("3","4"))
larch.wsu.2014.x1a <- factor(as.integer(ch.larch.wsu.14$treatment), levels = c("3","4"))
larch.wsu.2015.x1a <- factor(as.integer(ch.larch.wsu.15$treatment), levels = c("3","4"))


par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,0.1))

# Tundra
boxplot(ch.larch.tun.13$response ~ larch.tun.2013.x1a, at = c(1,5), xlim = c(0,8), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.larch.tun.14$response ~ larch.tun.2014.x1a, at = c(2,6), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
boxplot(ch.larch.tun.15$response ~ larch.tun.2015.x1a, at = c(3,7), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(ch.larch.tun.13$response ~ larch.tun.2013.x1a, at = c(1,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.larch.tun.14$response ~ larch.tun.2014.x1a, at = c(2,6), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.larch.tun.15$response ~ larch.tun.2015.x1a, at = c(3,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,3,5,6,7), labels = c("","","","","",""), tick = TRUE)
legend("topleft", "(a) Tundra", bty = "n", inset = c(0.005,-0.1))

# Tree island
boxplot(ch.larch.tis.13$response ~ larch.tis.2013.x1a, at = c(1,5), xlim = c(0,8), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.larch.tis.14$response ~ larch.tis.2014.x1a, at = c(2,6), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
boxplot(ch.larch.tis.15$response ~ larch.tis.2015.x1a, at = c(3,7), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(ch.larch.tis.13$response ~ larch.tis.2013.x1a, at = c(1,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.larch.tis.14$response ~ larch.tis.2014.x1a, at = c(2,6), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.larch.tis.15$response ~ larch.tis.2015.x1a, at = c(3,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,3,5,6,7), labels = c("","","","","",""), tick = TRUE)
legend("topleft", "(b) Tree island", bty = "n", inset = c(0.005,-0.1))

# Treeline
boxplot(ch.larch.rid.13$response ~ larch.rid.2013.x1a, at = c(1,5), xlim = c(0,8), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.larch.rid.14$response ~ larch.rid.2014.x1a, at = c(2,6), xaxt = "n", yaxt = "n", add = TRUE,col = c("darkorange","yellow"))
boxplot(ch.larch.rid.15$response ~ larch.rid.2015.x1a, at = c(3,7), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(ch.larch.rid.13$response ~ larch.rid.2013.x1a, at = c(1,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.larch.rid.14$response ~ larch.rid.2014.x1a, at = c(2,6), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.larch.rid.15$response ~ larch.rid.2015.x1a, at = c(3,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,3,5,6,7), labels = c("","","","","",""), tick = TRUE)
legend("topleft", "(c) Treeline", bty = "n", inset = c(0.005,-0.1))

# Forest
boxplot(ch.larch.wsu.13$response ~ larch.wsu.2013.x1a, at = c(1,5), xlim = c(0,8), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.larch.wsu.14$response ~ larch.wsu.2014.x1a, at = c(2,6), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
boxplot(ch.larch.wsu.15$response ~ larch.wsu.2015.x1a, at = c(3,7), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(ch.larch.wsu.13$response ~ larch.wsu.2013.x1a, at = c(1,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.larch.wsu.14$response ~ larch.wsu.2014.x1a, at = c(2,6), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.larch.wsu.15$response ~ larch.wsu.2015.x1a, at = c(3,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
legend("topleft", "(d) Forest", bty = "n", inset = c(0.005,-0.1))

axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,3,5,6,7), labels = c("14","15","16","14","15","16"), tick = TRUE)
axis(1, at = c(2,6), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1.25)
axis(1, at = 4, labels = "Seeded", tick = FALSE, line = 2.85)
mtext(side = 2, "Number of seedlings", outer = TRUE)

par(xpd = NA)
rect(0.5,-70,7.5,-64, col = 'blue', border = NA)




###############################
###############################
###############################
###############################

#### Plot the exclosure and non-exclosure data

# Mac pass

plot.mm.ex <- read.csv("~/Desktop/Workspace/gtree/gtree_mm_exclosures.csv") # Wolf Creek

# Need to adjust the data for different numbers of seeds planted each year (100 in some, 50 in others),
# and make them count data so we can analyze them using hurdle models
temp.mm.ex <- plot.mm.ex$germ.prop.via*100                                  # Convert the 0-1 data to 0-100
plot.mm.ex$response <- as.integer(round(temp.mm.ex,0))                      # Make an integer for the zero-inflated modelling
plot(table(plot.mm.ex$response))                                         # Have a look at the zero-inflation
gtree <- cbind(plot.mm.ex[c(1:5,7)], plot.mm.ex[19])                  # Bind the data together
names(gtree) <- c(names(plot.mm.ex[c(1:5,7)]), "response")  # Rename the columns

gtree.mm.ex <- subset(gtree, treatment == "seeded" | treatment == "seeded.scarified")
gtree.mm.ex$response[gtree.mm.ex$response > 100] <- 100

mm.fir.ex <- subset(gtree.mm.ex, species == "fir")
mm.spruce.ex <- subset(gtree.mm.ex, species == "spruce")
mm.fir.ex.nalp <- subset(mm.fir.ex, site == "nalp")
mm.fir.ex.salp <- subset(mm.fir.ex, site == "salp")
mm.fir.ex.scut <- subset(mm.fir.ex, site == "scut")
mm.fir.ex.sshr <- subset(mm.fir.ex, site == "sshr")
mm.spruce.ex.nalp <- subset(mm.spruce.ex, site == "nalp")
mm.spruce.ex.salp <- subset(mm.spruce.ex, site == "salp")
mm.spruce.ex.scut <- subset(mm.spruce.ex, site == "scut")
mm.spruce.ex.sshr <- subset(mm.spruce.ex, site == "sshr")

mm.fir.nalp.n <- subset(mm.fir.ex.nalp, exclosure == "no")
mm.fir.nalp.y <- subset(mm.fir.ex.nalp, exclosure == "yes")
mm.fir.salp.n <- subset(mm.fir.ex.salp, exclosure == "no")
mm.fir.salp.y <- subset(mm.fir.ex.salp, exclosure == "yes")
mm.fir.scut.n <- subset(mm.fir.ex.scut, exclosure == "no")
mm.fir.scut.y <- subset(mm.fir.ex.scut, exclosure == "yes")
mm.fir.sshr.n <- subset(mm.fir.ex.sshr, exclosure == "no")
mm.fir.sshr.y <- subset(mm.fir.ex.sshr, exclosure == "yes")

mm.x1 <- factor(as.integer(mm.fir.nalp.n$treatment), levels = c("3","4"))
mm.x2 <- factor(as.integer(mm.fir.nalp.y$treatment), levels = c("3","4"))
mm.x3 <- factor(as.integer(mm.fir.salp.n$treatment), levels = c("3","4"))
mm.x4 <- factor(as.integer(mm.fir.salp.y$treatment), levels = c("3","4"))
mm.x5 <- factor(as.integer(mm.fir.scut.n$treatment), levels = c("3","4"))
mm.x6 <- factor(as.integer(mm.fir.scut.y$treatment), levels = c("3","4"))
mm.x7 <- factor(as.integer(mm.fir.sshr.n$treatment), levels = c("3","4"))
mm.x8 <- factor(as.integer(mm.fir.sshr.y$treatment), levels = c("3","4"))

mm.spruce.nalp.n <- subset(mm.spruce.ex.nalp, exclosure == "no")
mm.spruce.nalp.y <- subset(mm.spruce.ex.nalp, exclosure == "yes")
mm.spruce.salp.n <- subset(mm.spruce.ex.salp, exclosure == "no")
mm.spruce.salp.y <- subset(mm.spruce.ex.salp, exclosure == "yes")
mm.spruce.scut.n <- subset(mm.spruce.ex.scut, exclosure == "no")
mm.spruce.scut.y <- subset(mm.spruce.ex.scut, exclosure == "yes")
mm.spruce.sshr.n <- subset(mm.spruce.ex.sshr, exclosure == "no")
mm.spruce.sshr.y <- subset(mm.spruce.ex.sshr, exclosure == "yes")

mm.x1 <- factor(as.integer(mm.spruce.nalp.n$treatment), levels = c("3","4"))
mm.x2 <- factor(as.integer(mm.spruce.nalp.y$treatment), levels = c("3","4"))
mm.x3 <- factor(as.integer(mm.spruce.salp.n$treatment), levels = c("3","4"))
mm.x4 <- factor(as.integer(mm.spruce.salp.y$treatment), levels = c("3","4"))
mm.x5 <- factor(as.integer(mm.spruce.scut.n$treatment), levels = c("3","4"))
mm.x6 <- factor(as.integer(mm.spruce.scut.y$treatment), levels = c("3","4"))
mm.x7 <- factor(as.integer(mm.spruce.sshr.n$treatment), levels = c("3","4"))
mm.x8 <- factor(as.integer(mm.spruce.sshr.y$treatment), levels = c("3","4"))


## Fir
par(mfrow = c(4, 1)) # Export at 3 x 7
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,0.1))

# Nalp
boxplot(mm.fir.nalp.n$response ~ mm.x1, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n",  yaxt = "n", col = c("darkorange","yellow"))
boxplot(mm.fir.nalp.y$response ~ mm.x2, at = c(2,5), xlim = c(0,6), ylim = c(0,100), add = TRUE, xaxt = "n", yaxt = "n", col = c("darkorange4","yellow4"))
stripchart(mm.fir.nalp.n$response ~ mm.x1, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
stripchart(mm.fir.nalp.y$response ~ mm.x2, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(a) NF alpine", bty = "n", inset = c(0.005,-0.1))

# Salp
boxplot(mm.fir.salp.n$response ~ mm.x3, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(mm.fir.salp.y$response ~ mm.x4, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
stripchart(mm.fir.salp.n$response ~ mm.x3, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
stripchart(mm.fir.salp.y$response ~ mm.x4, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(b) SF alpine", bty = "n", inset = c(0.005,-0.1))

# Scut
boxplot(mm.fir.scut.n$response ~ mm.x5, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(mm.fir.scut.y$response ~ mm.x6, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
stripchart(mm.fir.scut.n$response ~ mm.x5, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
stripchart(mm.fir.scut.y$response ~ mm.x6, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(c) SF shrub - cut", bty = "n", inset = c(0.005,-0.1))

# Sshr
boxplot(mm.fir.sshr.n$response ~ mm.x7, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(mm.fir.sshr.y$response ~ mm.x8, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
stripchart(mm.fir.sshr.n$response ~ mm.x7, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
stripchart(mm.fir.sshr.y$response ~ mm.x8, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
legend("topleft", "(d) SF shrub", bty = "n", inset = c(0.005,-0.1))
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,4,5), labels = c("Ctl","Exc","Ctl","Exc"), tick = TRUE)
axis(1, at = c(1.5,4.5), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1.25)
axis(1, at = 3, labels = "Seeded", tick = FALSE, line = 2.85)
mtext(side = 2, "Number of seedlings", outer = TRUE)

par(xpd = NA)
rect(0.4,-72,5.7,-65, col = 'blue', border = NA)




## Spruce
par(mfrow = c(4, 1)) # Export at 4 x 7
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,0.1))

# Nalp
boxplot(mm.spruce.nalp.n$response ~ mm.x1, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(mm.spruce.nalp.y$response ~ mm.x2, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
stripchart(mm.spruce.nalp.n$response ~ mm.x1, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
stripchart(mm.spruce.nalp.y$response ~ mm.x2, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(a) NF alpine", bty = "n", inset = c(0.005,-0.1))

# Salp
boxplot(mm.spruce.salp.n$response ~ mm.x3, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(mm.spruce.salp.y$response ~ mm.x4, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
stripchart(mm.spruce.salp.n$response ~ mm.x3, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
stripchart(mm.spruce.salp.y$response ~ mm.x4, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(b) SF alpine", bty = "n", inset = c(0.005,-0.1))

# Scut
boxplot(mm.spruce.scut.n$response ~ mm.x5, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(mm.spruce.scut.y$response ~ mm.x6, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
stripchart(mm.spruce.scut.n$response ~ mm.x5, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
stripchart(mm.spruce.scut.y$response ~ mm.x6, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(c) SF shrub - cut", bty = "n", inset = c(0.005,-0.1))

# Sshr
boxplot(mm.spruce.sshr.n$response ~ mm.x7, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(mm.spruce.sshr.y$response ~ mm.x8, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
stripchart(mm.spruce.sshr.n$response ~ mm.x7, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
stripchart(mm.spruce.sshr.y$response ~ mm.x8, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,4,5), labels = c("Ctl","Exc","Ctl","Exc"), tick = TRUE)
axis(1, at = c(1.5,4.5), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1.25)
axis(1, at = 3, labels = "Seeded", tick = FALSE, line = 2.85)
legend("topleft", "(d) SF shrub", bty = "n", inset = c(0.005,-0.1))
mtext(side = 2, "Number of seedlings", outer = TRUE)

par(xpd = NA)
rect(0.4,-72,5.7,-65, col = 'blue', border = NA)



## Churchill

plot.ch.ex <- read.csv("~/Desktop/Workspace/gtree/gtree_ch_exclosures.csv")

# Need to adjust the data for different numbers of seeds planted each year (100 in some, 50 in others),
# and make them count data so we can analyze them using hurdle models
temp.ch.ex <- plot.ch.ex$germ.prop.via*100                                  # Convert the 0-1 data to 0-100
plot.ch.ex$response <- as.integer(round(temp.ch.ex,0))                      # Make an integer for the zero-inflated modelling
plot(table(plot.ch.ex$response))                                         # Have a look at the zero-inflation
gtree <- cbind(plot.ch.ex[c(1:5,7)], plot.ch.ex[27])                  # Bind the data together
names(gtree) <- c(names(plot.ch.ex[c(1:5,7)]), "response")  # Rename the columns

gtree.ch.ex <- subset(gtree, treatment == "seeded" | treatment == "seeded.scarified")
gtree.ch.ex$response[gtree.ch.ex$response > 100] <- 100

ch.larch.ex <- subset(gtree.ch.ex, species == "ll")
ch.white.ex <- subset(gtree.ch.ex, species == "ws")
ch.black.ex <- subset(gtree.ch.ex, species == "bs")

# Larch
ch.ll.ex.tun.n <- subset(ch.larch.ex, site == "TUN" & exclosure == "no")
ch.ll.ex.tun.y <- subset(ch.larch.ex, site == "TUN" & exclosure == "yes")
ch.ll.ex.tis.n <- subset(ch.larch.ex, site == "TIS" & exclosure == "no")
ch.ll.ex.tis.y <- subset(ch.larch.ex, site == "TIS" & exclosure == "yes")
ch.ll.ex.rid.n <- subset(ch.larch.ex, site == "RID" & exclosure == "no")
ch.ll.ex.rid.y <- subset(ch.larch.ex, site == "RID" & exclosure == "yes")
ch.ll.ex.wsu.n <- subset(ch.larch.ex, site == "WSU" & exclosure == "no")
ch.ll.ex.wsu.y <- subset(ch.larch.ex, site == "WSU" & exclosure == "yes")

# White spruce
ch.ws.ex.tun.n <- subset(ch.white.ex, site == "TUN" & exclosure == "no")
ch.ws.ex.tun.y <- subset(ch.white.ex, site == "TUN" & exclosure == "yes")
ch.ws.ex.tis.n <- subset(ch.white.ex, site == "TIS" & exclosure == "no")
ch.ws.ex.tis.y <- subset(ch.white.ex, site == "TIS" & exclosure == "yes")
ch.ws.ex.rid.n <- subset(ch.white.ex, site == "RID" & exclosure == "no")
ch.ws.ex.rid.y <- subset(ch.white.ex, site == "RID" & exclosure == "yes")
ch.ws.ex.wsu.n <- subset(ch.white.ex, site == "WSU" & exclosure == "no")
ch.ws.ex.wsu.y <- subset(ch.white.ex, site == "WSU" & exclosure == "yes")

# Black spruce
ch.bs.ex.tun.n <- subset(ch.black.ex, site == "TUN" & exclosure == "no")
ch.bs.ex.tun.y <- subset(ch.black.ex, site == "TUN" & exclosure == "yes")
ch.bs.ex.rid.n <- subset(ch.black.ex, site == "RID" & exclosure == "no")
ch.bs.ex.rid.y <- subset(ch.black.ex, site == "RID" & exclosure == "yes")
ch.bs.ex.wsu.n <- subset(ch.black.ex, site == "WSU" & exclosure == "no")
ch.bs.ex.wsu.y <- subset(ch.black.ex, site == "WSU" & exclosure == "yes")

ch.x1 <- factor(as.integer(ch.ll.ex.tun.n$treatment), levels = c("3","4"))
ch.x2 <- factor(as.integer(ch.ll.ex.tun.y$treatment), levels = c("3","4"))
ch.x3 <- factor(as.integer(ch.ll.ex.tis.n$treatment), levels = c("3","4"))
ch.x4 <- factor(as.integer(ch.ll.ex.tis.y$treatment), levels = c("3","4"))
ch.x5 <- factor(as.integer(ch.ll.ex.rid.n$treatment), levels = c("3","4"))
ch.x6 <- factor(as.integer(ch.ll.ex.rid.y$treatment), levels = c("3","4"))
ch.x7 <- factor(as.integer(ch.ll.ex.wsu.n$treatment), levels = c("3","4"))
ch.x8 <- factor(as.integer(ch.ll.ex.wsu.y$treatment), levels = c("3","4"))

ch.x9 <- factor(as.integer(ch.ws.ex.tun.n$treatment), levels = c("3","4"))
ch.x10 <- factor(as.integer(ch.ws.ex.tun.y$treatment), levels = c("3","4"))
ch.x11 <- factor(as.integer(ch.ws.ex.tis.n$treatment), levels = c("3","4"))
ch.x12 <- factor(as.integer(ch.ws.ex.tis.y$treatment), levels = c("3","4"))
ch.x13 <- factor(as.integer(ch.ws.ex.rid.n$treatment), levels = c("3","4"))
ch.x14 <- factor(as.integer(ch.ws.ex.rid.y$treatment), levels = c("3","4"))
ch.x15 <- factor(as.integer(ch.ws.ex.wsu.n$treatment), levels = c("3","4"))
ch.x16 <- factor(as.integer(ch.ws.ex.wsu.y$treatment), levels = c("3","4"))

ch.x17 <- factor(as.integer(ch.bs.ex.tun.n$treatment), levels = c("3","4"))
ch.x18 <- factor(as.integer(ch.bs.ex.tun.y$treatment), levels = c("3","4"))
ch.x19 <- factor(as.integer(ch.bs.ex.rid.n$treatment), levels = c("3","4"))
ch.x20 <- factor(as.integer(ch.bs.ex.rid.y$treatment), levels = c("3","4"))
ch.x21 <- factor(as.integer(ch.bs.ex.wsu.n$treatment), levels = c("3","4"))
ch.x22 <- factor(as.integer(ch.bs.ex.wsu.y$treatment), levels = c("3","4"))

## Larch
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,0.1))

# Tundra
boxplot(ch.ll.ex.tun.n$response ~ ch.x1, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.ll.ex.tun.y$response ~ ch.x2, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
stripchart(ch.ll.ex.tun.n$response ~ ch.x1, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
stripchart(ch.ll.ex.tun.y$response ~ ch.x2, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(a) Tundra", bty = "n", inset = c(0.005,-0.1))

# Tree island
boxplot(ch.ll.ex.tis.n$response ~ ch.x3, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.ll.ex.tis.y$response ~ ch.x4, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
stripchart(ch.ll.ex.tis.n$response ~ ch.x3, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
stripchart(ch.ll.ex.tis.y$response ~ ch.x4, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(b) Tree island", bty = "n", inset = c(0.005,-0.1))

# Treeline
boxplot(ch.ll.ex.rid.n$response ~ ch.x5, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.ll.ex.rid.y$response ~ ch.x6, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
stripchart(ch.ll.ex.rid.n$response ~ ch.x5, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
stripchart(ch.ll.ex.rid.y$response ~ ch.x6, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(c) Treeline", bty = "n", inset = c(0.005,-0.1))

# Forest
boxplot(ch.ll.ex.wsu.n$response ~ ch.x7, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.ll.ex.wsu.y$response ~ ch.x8, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
stripchart(ch.ll.ex.wsu.n$response ~ ch.x7, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
stripchart(ch.ll.ex.wsu.y$response ~ ch.x8, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,4,5), labels = c("Ctl","Exc","Ctl","Exc"), tick = TRUE)
axis(1, at = c(1.5,4.5), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1.25)
axis(1, at = 3, labels = "Seeded", tick = FALSE, line = 2.85)
legend("topleft", "(d) Forest", bty = "n", inset = c(0.005,-0.1))
mtext(side = 2, "Number of seedlings", outer = TRUE)
par(xpd = NA)
rect(0.4,-72,5.7,-65, col = 'blue', border = NA)


## White spruce
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,0.1))

# Tundra
boxplot(ch.ws.ex.tun.n$response ~ ch.x9, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.ws.ex.tun.y$response ~ ch.x10, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
stripchart(ch.ws.ex.tun.n$response ~ ch.x9, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
stripchart(ch.ws.ex.tun.y$response ~ ch.x10, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(a) Tundra", bty = "n", inset = c(0.005,-0.1))

# Tree island
boxplot(ch.ws.ex.tis.n$response ~ ch.x11, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.ws.ex.tis.y$response ~ ch.x12, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
stripchart(ch.ws.ex.tis.n$response ~ ch.x11, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
stripchart(ch.ws.ex.tis.y$response ~ ch.x12, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(b) Tree island", bty = "n", inset = c(0.005,-0.1))

# Treeline
boxplot(ch.ws.ex.rid.n$response ~ ch.x13, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.ws.ex.rid.y$response ~ ch.x14, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
stripchart(ch.ws.ex.rid.n$response ~ ch.x13, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
stripchart(ch.ws.ex.rid.y$response ~ ch.x14, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(c) Treeline", bty = "n", inset = c(0.005,-0.1))

# Forest
boxplot(ch.ws.ex.wsu.n$response ~ ch.x15, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.ws.ex.wsu.y$response ~ ch.x16, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
stripchart(ch.ws.ex.wsu.n$response ~ ch.x15, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
stripchart(ch.ws.ex.wsu.y$response ~ ch.x16, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
legend("topleft", "(d) Forest", bty = "n", inset = c(0.005,-0.1))
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,4,5), labels = c("Ctl","Exc","Ctl","Exc"), tick = TRUE)
axis(1, at = c(1.5,4.5), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1.25)
axis(1, at = 3, labels = "Seeded", tick = FALSE, line = 2.85)
legend("topleft", "(d) Forest", bty = "n", inset = c(0.005,-0.1))
mtext(side = 2, "Number of seedlings", outer = TRUE)
par(xpd = NA)
rect(0.4,-72,5.7,-65, col = 'blue', border = NA)


## Black spruce
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,0.1))

# Tundra
boxplot(ch.bs.ex.tun.n$response ~ ch.x17, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.bs.ex.tun.y$response ~ ch.x18, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
stripchart(ch.bs.ex.tun.n$response ~ ch.x17, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
stripchart(ch.bs.ex.tun.y$response ~ ch.x18, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(a) Tundra", bty = "n", inset = c(0.005,-0.1))

# Treeline
boxplot(ch.bs.ex.rid.n$response ~ ch.x19, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.bs.ex.rid.y$response ~ ch.x20, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
stripchart(ch.bs.ex.rid.n$response ~ ch.x19, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
stripchart(ch.bs.ex.rid.y$response ~ ch.x20, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(b) Treeline", bty = "n", inset = c(0.005,-0.1))

# Forest
boxplot(ch.bs.ex.wsu.n$response ~ ch.x21, at = c(1,4), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.bs.ex.wsu.y$response ~ ch.x22, at = c(2,5), xlim = c(0,6), ylim = c(0,100), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange4","yellow4"))
stripchart(ch.bs.ex.wsu.n$response ~ ch.x21, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
stripchart(ch.bs.ex.wsu.y$response ~ ch.x22, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.6)
legend("topleft", "(c) Forest", bty = "n", inset = c(0.005,-0.1))
axis(2, at = c(0,25,50,75,100), labels = c(0,25,50,75,100), tick = TRUE)
axis(1, at = c(1,2,4,5), labels = c("Ctl","Exc","Ctl","Exc"), tick = TRUE)
axis(1, at = c(1.5,4.5), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1.25)
axis(1, at = 3, labels = "Seeded", tick = FALSE, line = 2.85)
mtext(side = 2, "Number of seedlings", outer = TRUE)

par(xpd = NA)
rect(0.4,-72,5.7,-65, col = 'blue', border = NA)


####################################
####################################
####################################
####################################

#### Plot the survival proportions

plot.mm <- read.csv("~/Desktop/Workspace/gtree/gtree_mm.csv") # Mac Pass

## Prepare the dataframes

# Mac pass
mm.surv1 <- cbind(plot.mm[c(19,1:4,6)])                  # Bind the data together
names(mm.surv1) <- c(names(plot.mm[c(19,1:4,6)]))  # Rename the columns
mm.surv1$aspect <- factor(c(rep("north", 20), 
                            rep("south", 60),
                            rep("north", 20), 
                            rep("south", 60),
                            rep("north", 20), 
                            rep("south", 60)))                          # Add aspect factors

mm.surv2 <- cbind(plot.mm[c(20,1:4,6)])                  # Bind the data together
names(mm.surv2) <- c(names(plot.mm[c(20,1:4,6)]))  # Rename the columns
mm.surv2$aspect <- factor(c(rep("north", 20), 
                            rep("south", 60),
                            rep("north", 20), 
                            rep("south", 60),
                            rep("north", 20), 
                            rep("south", 60)))                          # Add aspect factors

mm.surv1 <- subset(mm.surv1, treatment == "seeded" | treatment == "seeded.scarified")
mm.surv2 <- subset(mm.surv2, treatment == "seeded" | treatment == "seeded.scarified")

mm.surv1.sf <- subset(mm.surv1, species == "fir")
mm.surv1.ws <- subset(mm.surv1, species == "spruce")
mm.surv2.sf <- subset(mm.surv2, species == "fir")
mm.surv2.ws <- subset(mm.surv2, species == "spruce")

mm.surv1.sf.nalp <- subset(mm.surv1, species == "fir" & site == "nalp")
mm.surv1.sf.salp <- subset(mm.surv1, species == "fir" & site == "salp")
mm.surv1.sf.scut <- subset(mm.surv1, species == "fir" & site == "scut")
mm.surv1.sf.sshr <- subset(mm.surv1, species == "fir" & site == "sshr")
mm.surv2.sf.nalp <- subset(mm.surv2, species == "fir" & site == "nalp")
mm.surv2.sf.salp <- subset(mm.surv2, species == "fir" & site == "salp")
mm.surv2.sf.scut <- subset(mm.surv2, species == "fir" & site == "scut")
mm.surv2.sf.sshr <- subset(mm.surv2, species == "fir" & site == "sshr")


mm.surv1.ws.nalp <- subset(mm.surv1, species == "spruce" & site == "nalp")
mm.surv1.ws.salp <- subset(mm.surv1, species == "spruce" & site == "salp")
mm.surv1.ws.scut <- subset(mm.surv1, species == "spruce" & site == "scut")
mm.surv1.ws.sshr <- subset(mm.surv1, species == "spruce" & site == "sshr")
mm.surv2.ws.nalp <- subset(mm.surv2, species == "spruce" & site == "nalp")
mm.surv2.ws.salp <- subset(mm.surv2, species == "spruce" & site == "salp")
mm.surv2.ws.scut <- subset(mm.surv2, species == "spruce" & site == "scut")
mm.surv2.ws.sshr <- subset(mm.surv2, species == "spruce" & site == "sshr")

## Mac Pass

mm.surv1.x1 <- factor(as.integer(mm.surv1.sf.nalp$treatment), levels = c("3","4"))
mm.surv2.x1 <- factor(as.integer(mm.surv2.sf.nalp$treatment), levels = c("3","4"))
mm.surv1.x2 <- factor(as.integer(mm.surv1.sf.salp$treatment), levels = c("3","4"))
mm.surv2.x2 <- factor(as.integer(mm.surv2.sf.salp$treatment), levels = c("3","4"))
mm.surv1.x3 <- factor(as.integer(mm.surv1.sf.scut$treatment), levels = c("3","4"))
mm.surv2.x3 <- factor(as.integer(mm.surv2.sf.scut$treatment), levels = c("3","4"))
mm.surv1.x4 <- factor(as.integer(mm.surv1.sf.sshr$treatment), levels = c("3","4"))
mm.surv2.x4 <- factor(as.integer(mm.surv2.sf.sshr$treatment), levels = c("3","4"))

mm.surv1.ws.x1 <- factor(as.integer(mm.surv1.ws.nalp$treatment), levels = c("3","4"))
mm.surv2.ws.x1 <- factor(as.integer(mm.surv2.ws.nalp$treatment), levels = c("3","4"))
mm.surv1.ws.x2 <- factor(as.integer(mm.surv1.ws.salp$treatment), levels = c("3","4"))
mm.surv2.ws.x2 <- factor(as.integer(mm.surv2.ws.salp$treatment), levels = c("3","4"))
mm.surv1.ws.x3 <- factor(as.integer(mm.surv1.ws.scut$treatment), levels = c("3","4"))
mm.surv2.ws.x3 <- factor(as.integer(mm.surv2.ws.scut$treatment), levels = c("3","4"))
mm.surv1.ws.x4 <- factor(as.integer(mm.surv1.ws.sshr$treatment), levels = c("3","4"))
mm.surv2.ws.x4 <- factor(as.integer(mm.surv2.ws.sshr$treatment), levels = c("3","4"))



### Fir
par(mfrow = c(4, 1)) # Export at 4 x 7
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,0.1))

# Nalp
boxplot(mm.surv1.sf.nalp$surv.1 ~ mm.surv1.x1, at = c(1,4), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(mm.surv2.sf.nalp$surv.2 ~ mm.surv2.x1, at = c(2,5), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(mm.surv1.sf.nalp$surv.1 ~ mm.surv1.x1, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(mm.surv2.sf.nalp$surv.2 ~ mm.surv2.x1, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1), tick = TRUE)
legend("topleft", "(a) NF alpine", bty = "n", inset = c(0.005,-0.2))

# Salp
boxplot(mm.surv1.sf.salp$surv.1 ~ mm.surv1.x2, at = c(1,4), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(mm.surv2.sf.salp$surv.2 ~ mm.surv2.x2, at = c(2,5), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(mm.surv1.sf.salp$surv.1 ~ mm.surv1.x2, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(mm.surv2.sf.salp$surv.2 ~ mm.surv2.x2, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1), tick = TRUE)
legend("topleft", "(b) SF alpine", bty = "n", inset = c(0.005,-0.2))

# Scut
boxplot(mm.surv1.sf.scut$surv.1 ~ mm.surv1.x3, at = c(1,4), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(mm.surv2.sf.scut$surv.2 ~ mm.surv2.x3, at = c(2,5), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(mm.surv1.sf.scut$surv.1 ~ mm.surv1.x3, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(mm.surv2.sf.scut$surv.2 ~ mm.surv2.x3, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1), tick = TRUE)
legend("topleft", "(c) SF shrub - cut", bty = "n", inset = c(0.005,-0.2))

# Sshr
boxplot(mm.surv1.sf.sshr$surv.1 ~ mm.surv1.x4, at = c(1,4), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(mm.surv2.sf.sshr$surv.2 ~ mm.surv2.x4, at = c(2,5), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(mm.surv1.sf.sshr$surv.1 ~ mm.surv1.x4, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(mm.surv2.sf.sshr$surv.2 ~ mm.surv2.x4, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
legend("topleft", "(d) SF shrub", bty = "n", inset = c(0.005,-0.2))
axis(1, at = c(1,2,4,5), labels = c("Y1","Y2","Y1","Y2"), tick = TRUE)
axis(1, at = c(1.5,4.5), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1.25)
axis(1, at = 3, labels = "Seeded", tick = FALSE, line = 2.85)
axis(2, at = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1), tick = TRUE)
mtext(side = 2, "Survival proportion", outer = TRUE) #, adj = 0.6
par(xpd = NA)
rect(0.4,-.85,5.7,-.78, col = 'blue', border = NA)


### Spruce
par(mfrow = c(4, 1)) # Export at 3 x 7
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,0.1))

# Nalp
boxplot(mm.surv1.ws.nalp$surv.1 ~ mm.surv1.ws.x1, at = c(1,4), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(mm.surv2.ws.nalp$surv.2 ~ mm.surv2.ws.x1, at = c(2,5), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(mm.surv1.ws.nalp$surv.1 ~ mm.surv1.ws.x1, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(mm.surv2.ws.nalp$surv.2 ~ mm.surv2.ws.x1, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1), tick = TRUE)
legend("topleft", "(a) NF alpine", bty = "n", inset = c(0.005,-0.2))

# Salp
boxplot(mm.surv1.ws.salp$surv.1 ~ mm.surv1.ws.x2, at = c(1,4), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(mm.surv2.ws.salp$surv.2 ~ mm.surv2.ws.x2, at = c(2,5), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(mm.surv1.ws.salp$surv.1 ~ mm.surv1.ws.x2, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(mm.surv2.ws.salp$surv.2 ~ mm.surv2.ws.x2, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1), tick = TRUE)
legend("topleft", "(b) SF alpine", bty = "n", inset = c(0.005,-0.2))

# Scut
boxplot(mm.surv1.ws.scut$surv.1 ~ mm.surv1.ws.x3, at = c(1,4), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(mm.surv2.ws.scut$surv.2 ~ mm.surv2.ws.x3, at = c(2,5), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(mm.surv1.ws.scut$surv.1 ~ mm.surv1.ws.x3, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(mm.surv2.ws.scut$surv.2 ~ mm.surv2.ws.x3, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1), tick = TRUE)
legend("topleft", "(c) SF shrub - cut", bty = "n", inset = c(0.005,-0.2))

# Sshr
boxplot(mm.surv1.ws.sshr$surv.1 ~ mm.surv1.ws.x4, at = c(1,4), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(mm.surv2.ws.sshr$surv.2 ~ mm.surv2.ws.x4, at = c(2,5), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(mm.surv1.ws.sshr$surv.1 ~ mm.surv1.ws.x4, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(mm.surv2.ws.sshr$surv.2 ~ mm.surv2.ws.x4, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
legend("topleft", "(d) SF shrub", bty = "n", inset = c(0.005,-0.2))
axis(1, at = c(1,2,4,5), labels = c("Y1","Y2","Y1","Y2"), tick = TRUE)
axis(1, at = c(1.5,4.5), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1.25)
axis(1, at = 3, labels = "Seeded", tick = FALSE, line = 2.85)
axis(2, at = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1), tick = TRUE)
mtext(side = 2, "Survival proportion", outer = TRUE, adj = 0.6)
par(xpd = NA)
rect(0.4,-.85,5.7,-.78, col = 'blue', border = NA)


## Churchill

# Churchill

plot.ch <- read.csv("~/Desktop/Workspace/gtree/gtree_ch.csv") # Churchill

ch.surv1 <- cbind(plot.ch[c(20,2:5,7)])                  # Bind the data together
names(ch.surv1) <- c(names(plot.ch[c(20,2:5,7)]))  # Rename the columns

ch.surv2 <- cbind(plot.ch[c(21,2:5,7)])                  # Bind the data together
names(ch.surv2) <- c(names(plot.ch[c(21,2:5,7)]))  # Rename the columns

ch.surv1 <- subset(ch.surv1, treatment == "seeded" | treatment == "seeded.scarified")
ch.surv2 <- subset(ch.surv2, treatment == "seeded" | treatment == "seeded.scarified")

ch.surv1.ws <- subset(ch.surv1, species == "ws")
ch.surv1.bs <- subset(ch.surv1, species == "bs")
ch.surv2.ws <- subset(ch.surv2, species == "ws")
ch.surv2.bs <- subset(ch.surv2, species == "bs")

ch.surv1.ws.tun <- subset(ch.surv1, species == "ws" & site == "TUN")
ch.surv1.ws.tis <- subset(ch.surv1, species == "ws" & site == "TIS")
ch.surv1.ws.rid <- subset(ch.surv1, species == "ws" & site == "RID")
ch.surv1.ws.wsu <- subset(ch.surv1, species == "ws" & site == "WSU")
ch.surv2.ws.tun <- subset(ch.surv2, species == "ws" & site == "TUN")
ch.surv2.ws.tis <- subset(ch.surv2, species == "ws" & site == "TIS")
ch.surv2.ws.rid <- subset(ch.surv2, species == "ws" & site == "RID")
ch.surv2.ws.wsu <- subset(ch.surv2, species == "ws" & site == "WSU")

ch.surv1.bs.tun <- subset(ch.surv1, species == "bs" & site == "TUN")
ch.surv1.bs.rid <- subset(ch.surv1, species == "bs" & site == "RID")
ch.surv1.bs.wsu <- subset(ch.surv1, species == "bs" & site == "WSU")
ch.surv2.bs.tun <- subset(ch.surv2, species == "bs" & site == "TUN")
ch.surv2.bs.rid <- subset(ch.surv2, species == "bs" & site == "RID")
ch.surv2.bs.wsu <- subset(ch.surv2, species == "bs" & site == "WSU")

ch.surv1.ws.x1 <- factor(as.integer(ch.surv1.ws.tun$treatment), levels = c("3","4"))
ch.surv2.ws.x1 <- factor(as.integer(ch.surv2.ws.tun$treatment), levels = c("3","4"))
ch.surv1.ws.x2 <- factor(as.integer(ch.surv1.ws.tis$treatment), levels = c("3","4"))
ch.surv2.ws.x2 <- factor(as.integer(ch.surv2.ws.tis$treatment), levels = c("3","4"))
ch.surv1.ws.x3 <- factor(as.integer(ch.surv1.ws.rid$treatment), levels = c("3","4"))
ch.surv2.ws.x3 <- factor(as.integer(ch.surv2.ws.rid$treatment), levels = c("3","4"))
ch.surv1.ws.x4 <- factor(as.integer(ch.surv1.ws.wsu$treatment), levels = c("3","4"))
ch.surv2.ws.x4 <- factor(as.integer(ch.surv2.ws.wsu$treatment), levels = c("3","4"))

ch.surv1.bs.x1 <- factor(as.integer(ch.surv1.bs.tun$treatment), levels = c("3","4"))
ch.surv2.bs.x1 <- factor(as.integer(ch.surv2.bs.tun$treatment), levels = c("3","4"))
ch.surv1.bs.x3 <- factor(as.integer(ch.surv1.bs.rid$treatment), levels = c("3","4"))
ch.surv2.bs.x3 <- factor(as.integer(ch.surv2.bs.rid$treatment), levels = c("3","4"))
ch.surv1.bs.x4 <- factor(as.integer(ch.surv1.bs.wsu$treatment), levels = c("3","4"))
ch.surv2.bs.x4 <- factor(as.integer(ch.surv2.bs.wsu$treatment), levels = c("3","4"))


### White spruce
par(mfrow = c(4, 1)) # Export at 4 x 7
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,0.1))

# TUN
boxplot(ch.surv1.ws.tun$surv.1 ~ ch.surv1.ws.x1, at = c(1,4), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.surv2.ws.tun$surv.2 ~ ch.surv2.ws.x1, at = c(2,5), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(ch.surv1.ws.tun$surv.1 ~ ch.surv1.ws.x1, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.surv2.ws.tun$surv.2 ~ ch.surv2.ws.x1, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1), tick = TRUE)
legend("topleft", "(a) Tundra", bty = "n", inset = c(0.005,-0.1))

# TIS
boxplot(ch.surv1.ws.tis$surv.1 ~ ch.surv1.ws.x2, at = c(1,4), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.surv2.ws.tis$surv.2 ~ ch.surv2.ws.x2, at = c(2,5), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(ch.surv1.ws.tis$surv.1 ~ ch.surv1.ws.x2, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.surv2.ws.tis$surv.2 ~ ch.surv2.ws.x2, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1), tick = TRUE)
legend("top", "(b) Tree island", bty = "n", inset = c(0.005,-0.1))

# RID
boxplot(ch.surv1.ws.rid$surv.1 ~ ch.surv1.ws.x3, at = c(1,4), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.surv2.ws.rid$surv.2 ~ ch.surv2.ws.x3, at = c(2,5), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(ch.surv1.ws.rid$surv.1 ~ ch.surv1.ws.x3, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.surv2.ws.rid$surv.2 ~ ch.surv2.ws.x3, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1), tick = TRUE)
legend("topleft", "(c) Treeline", bty = "n", inset = c(0.005,-0.1))

# WSU
boxplot(ch.surv1.ws.wsu$surv.1 ~ ch.surv1.ws.x4, at = c(1,4), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.surv2.ws.wsu$surv.2 ~ ch.surv2.ws.x4, at = c(2,5), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(ch.surv1.ws.wsu$surv.1 ~ ch.surv1.ws.x4, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.surv2.ws.wsu$surv.2 ~ ch.surv2.ws.x4, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
legend("topleft", "(d) Forest", bty = "n", inset = c(0.005,-0.1))
axis(1, at = c(1,2,4,5), labels = c("Y1","Y2","Y1","Y2"), tick = TRUE)
axis(1, at = c(1.5,4.5), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1.25)
axis(1, at = 3, labels = "Seeded", tick = FALSE, line = 2.85)
axis(2, at = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1), tick = TRUE)
mtext(side = 2, "Survival proportion", outer = TRUE)
par(xpd = NA)
rect(0.4,-.85,5.7,-.78, col = 'blue', border = NA)

### Black spruce
par(mfrow = c(4, 1)) # Export at 4 x 7
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,0.1))

# TUN
boxplot(ch.surv1.bs.tun$surv.1 ~ ch.surv1.bs.x1, at = c(1,4), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.surv2.bs.tun$surv.2 ~ ch.surv2.bs.x1, at = c(2,5), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(ch.surv1.bs.tun$surv.1 ~ ch.surv1.bs.x1, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.surv2.bs.tun$surv.2 ~ ch.surv2.bs.x1, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1), tick = TRUE)
legend("topleft", "(a) Tundra", bty = "n", inset = c(0.005,-0.1))

# RID
boxplot(ch.surv1.bs.rid$surv.1 ~ ch.surv1.bs.x3, at = c(1,4), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.surv2.bs.rid$surv.2 ~ ch.surv2.bs.x3, at = c(2,5), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(ch.surv1.bs.rid$surv.1 ~ ch.surv1.bs.x3, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.surv2.bs.rid$surv.2 ~ ch.surv2.bs.x3, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
axis(1, at = c(1,2,4,5), labels = c("","","",""), tick = TRUE)
axis(2, at = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1), tick = TRUE)
legend("topleft", "(b) Treeline", bty = "n", inset = c(0.005,0.1))

# WSU
boxplot(ch.surv1.bs.wsu$surv.1 ~ ch.surv1.bs.x4, at = c(1,4), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", col = c("darkorange","yellow"))
boxplot(ch.surv2.bs.wsu$surv.2 ~ ch.surv2.bs.x4, at = c(2,5), xlim = c(0,6), ylim = c(0,1.2), xaxt = "n", yaxt = "n", add = TRUE, col = c("darkorange","yellow"))
stripchart(ch.surv1.bs.wsu$surv.1 ~ ch.surv1.bs.x4, at = c(1,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
stripchart(ch.surv2.bs.wsu$surv.2 ~ ch.surv2.bs.x4, at = c(2,5), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "grey50", pch = 21, cex = 0.5)
legend("topleft", "(c) Forest", bty = "n", inset = c(0.005,-0.1))
axis(1, at = c(1,2,4,5), labels = c("Y1","Y2","Y1","Y2"), tick = TRUE)
axis(1, at = c(1.5,4.5), labels = c("Vegetated","Scarified"), tick = FALSE, line = 1.25)
axis(1, at = 3, labels = "Seeded", tick = FALSE, line = 2.85)
axis(2, at = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1), tick = TRUE)
mtext(side = 2, "Survival proportion", outer = TRUE, adj = 0.6)
par(xpd = NA)
rect(0.4,-.85,5.7,-.78, col = 'blue', border = NA)
