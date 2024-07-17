library(vegan)
library(scales)

rm(list = ls())

needles <- read.csv("~/Desktop/Workspace/Desiccation/Needles_2013_2018.csv", row.names = 1)

needles2 <- droplevels(needles[complete.cases(needles),c(1:5,18:23)])

needles.env <- needles2[,c(1:5)]
needles.env$Tree <- as.factor(needles.env$Tree)
needles.mat <- decostand(needles2[,c(6:11)],"standardize")
rda1 <- rda(needles.mat ~ ., needles.env)

rda1.summary <- summary(rda1)
rda1.anova <- anova(rda1, by = "terms")
vif.cca(rda1)
rda1.model_variance <- anova(rda1, test = "F")
rda1.r2adj <- RsquareAdj(rda1)$adj.r.squared

rda1.scrs <- scores(rda1, display = c("sites","species"), scaling = 1)
rda1.xlim <- with(rda1.scrs, range(sites[,1]))
rda1.ylim <- with(rda1.scrs, range(sites[,2]))

plot.new()
plot.window(xlim = rda1.xlim, ylim = rda1.ylim, asp = 1)
abline(h = 0, lty = "dotted")
abline(v = 0, lty = "dotted")
colvec1 <- c(rainbow(9))
vpch <- c(15:18)
with(needles.env, 
     points(rda1.scrs$sites, 
            pch = vpch[needles.env$Zone], 
            col = alpha(colvec1[needles.env$Site], 0.5), 
            cex = 1))
axis(side = 1, cex = 0.8)
axis(side = 2, cex = 0.8)
box()
legend("bottomleft", c(levels(needles.env$Site)),
       pch = 15, pt.cex = 1, text.col = colvec1, 
       text.font = 2, col = "white", ncol = 1, bty = "n", inset = c(0.1,0.1), adj = 1, y.intersp = 0.8, cex = 0.8)


legend("topleft", expression(bold("A")), bty = "n", inset = c(0,0.3), cex = 1.2)





# Sum the total variance
d.mvar <- sum(d.pcx$CA$eig)

# Calculate the PC1 and PC2 variance
PC1 <- paste("PC1: ", round(sum(d.pcx$CA$eig[1])/d.mvar, 3))
PC2 <- paste("PC2: ", round(sum(d.pcx$CA$eig[2])/d.mvar, 3))

# Extract the plotting coordinates for the biplot
can.scrs <- scores(d.pcx, display = c("sites","species"), scaling = 1)
can.xlim <- with(can.scrs, range(sites[,1]))
can.ylim <- with(can.scrs, range(sites[,2]))

# Multivariate analyses
can.asv.root <- subset(can.asv.3, RootSoil == "R")
can.asv.soil <- subset(can.asv.3, RootSoil == "S")

can.asv.root.euc <- vegdist(can.asv.root[,c(20:10885)], "euclidean")
can.asv.soil.euc <- vegdist(can.asv.soil[,c(20:10885)], "euclidean")

## ANOSIM
# Root and soil subsets
root.anos <- anosim(can.asv.root.euc, can.asv.root$Year, permutations=999, parallel = parallel::detectCores()-1)
soil.anos <- anosim(can.asv.soil.euc, can.asv.soil$Year, permutations=999, parallel = parallel::detectCores()-1)

# PERMANOVA
# Root and soil subsets
root.pman <- adonis(can.asv.root.euc ~ can.asv.root$Year*can.asv.root$Site, permutations=999, parallel = parallel::detectCores()-1)
soil.pman <- adonis(can.asv.soil.euc ~ can.asv.soil$Year*can.asv.soil$Site, permutations=999, parallel = parallel::detectCores()-1)
# Full dataset
can.asv.euc <- vegdist(can.asv.3[,c(20:10885)], "euclidean")
year.pman <- adonis(can.asv.euc ~ can.asv.3$Year, permutations=999, parallel = parallel::detectCores()-1)
roso.pman <- adonis(can.asv.euc ~ can.asv.3$RootSoil, permutations=999, parallel = parallel::detectCores())
siye.pman <- adonis(can.asv.euc ~ can.asv.3$Site*can.asv.3$Year, permutations=999, parallel = parallel::detectCores())

# RDA
# Root and soil subsets
root.rda <- rda(can.asv.root[,c(20:10885)] ~ Year*Site, data = can.asv.root)
root.rda.summary <- summary(root.rda)
root.rda.anova <- anova(root.rda, by = "terms")
root.rda.model_variance <- anova(root.rda, test = "F")
root.rda.r2adj <- RsquareAdj(root.rda)$adj.r.squared

soil.rda <- rda(can.asv.soil[,c(20:10885)] ~ Year*Site, data = can.asv.soil)
soil.rda.summary <- summary(soil.rda)
soil.rda.anova <- anova(soil.rda, by = "terms")
soil.rda.model_variance <- anova(soil.rda, test = "F")
soil.rda.r2adj <- RsquareAdj(soil.rda)$adj.r.squared



# Export at 9 x 3.5
par(mfrow = c(1,3))
par(mar = c(2,2,0,0), oma = c(1.5,1.5,2,1))
# par(xpd = FALSE)

# First biplot: sites are pch, color is year
plot.new()
plot.window(xlim = can.xlim, ylim = can.ylim, asp = 1)
abline(h = 0, lty = "dotted")
abline(v = 0, lty = "dotted")
colvec1 <- c("#7e48c9","#cbb652")
vpch <- c(15:17)
vcex <- c(0.75,1.5)
with(can.asv.3, 
     points(can.scrs$sites, 
            pch = vpch[can.asv.3$Site], 
            col = alpha(colvec1[factor(can.asv.3$Year)], 0.5), 
            cex = 1))
axis(side = 1, cex = 0.8)
axis(side = 2, cex = 0.8)
box()
legend("topright", c("2016","2017"),
       pch = 15, pt.cex = 1, text.col = colvec1, 
       text.font = 2, col = "white", ncol = 1, bty = "n", inset = c(-0.11,0.3), adj = 1, y.intersp = 0.8, cex = 0.8)
legend("topleft", expression(bold("A")), bty = "n", inset = c(0,0.3), cex = 1.2)
