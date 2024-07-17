library(multcomp) # Simultaneous Inference in General Parametric Models
                  # Produce letters from Tukey's HSD and plot above boxplot
library(vegan)
library(cluster)
library(gclus) # Used for plotting clusters
library(car) # Used for determining variance inflation factors
library(labdsv) # PCA
library(psych) # Used to run t-tests
library(tidyverse)

rm(list = ls()) # Clears the workspace

park.snow <- read.csv(file = "~/Desktop/Workspace/Earthwatch/ParkSnow.csv", header = TRUE)
# park.snow <- read.csv(file = "~/Desktop/Workspace/EW/ParkSnow.OBRdelete.csv", header = TRUE)
# park.needles <- read.csv(file = "~/Desktop/Workspace/EW/ParkBranches.csv", header = TRUE)
# park.needles$year <- as.factor(park.needles$year)
# park.snow$year <- as.factor(park.snow$year)
park.micro <- read.csv(file = "~/Desktop/Workspace/Earthwatch/ParkMicroclimate.csv", header = TRUE)

## All sites for annual report 2017
aov.bbr <- aov(swe ~ year, data = park.snow[park.snow$site=="BBR",])
summary(aov.bbr)
TukeyHSD(aov.bbr)
bbr.pairs <- glht(aov.bbr, linfct = mcp(year = "Tukey"))
bbr.cld <- cld(bbr.pairs)
bbr.letters <- bbr.cld$mcletters$Letters

############

plot(sort(park.snow$density))
park.snow <- park.snow[!(park.snow$density > 500),]
plot(sort(park.snow$density))

plot(sort(park.snow$htc))
park.snow <- park.snow[!(park.snow$htc > 10),]
plot(sort(park.snow$htc))

park.snow$type <- factor(park.snow$type, levels = c("island","forest","shrub","fen","beach","wedge","polygon"))
snow.2020 <- subset(park.snow, year == "2020")
snow.2020 <- snow.2020 %>%
  group_by(type) %>%
  summarise(swe.mean = mean(swe), depth.mean = mean(depth), density.mean = mean(density), htc.mean = mean(htc),
            swe.sd = sd(swe, na.rm = T), depth.sd = sd(depth, na.rm = T), density.sd = sd(density, na.rm = T), htc.sd = sd(htc, na.rm = T),
            swe.n = length(swe), depth.n = length(depth), density.n = length(density), htc.n = length(htc))
snow.2020$swe.se <- snow.2020$swe.sd / sqrt(snow.2020$swe.n)
snow.2020$depth.se <- snow.2020$depth.sd / sqrt(snow.2020$depth.n)
snow.2020$density.se <- snow.2020$density.sd / sqrt(snow.2020$density.n)
snow.2020$htc.se <- snow.2020$htc.sd / sqrt(snow.2020$htc.n)

snow.2020$swe.ci <- qnorm(0.975) * snow.2020$swe.se
snow.2020$depth.ci <- qnorm(0.975) * snow.2020$depth.se
snow.2020$density.ci <- qnorm(0.975) * snow.2020$density.se
snow.2020$htc.ci <- qnorm(0.975) * snow.2020$htc.se

snow.mean <- na.omit(park.snow) %>%
  group_by(type) %>%
  summarise(swe.mean = mean(swe), depth.mean = mean(depth), density.mean = mean(density), htc.mean = mean(htc),
            swe.sd = sd(swe, na.rm = T), depth.sd = sd(depth, na.rm = T), density.sd = sd(density, na.rm = T), htc.sd = sd(htc, na.rm = T),
            swe.n = length(swe), depth.n = length(depth), density.n = length(density), htc.n = length(htc))
snow.mean$swe.se <- snow.mean$swe.sd / sqrt(snow.mean$swe.n)
snow.mean$depth.se <- snow.mean$depth.sd / sqrt(snow.mean$depth.n)
snow.mean$density.se <- snow.mean$density.sd / sqrt(snow.mean$density.n)
snow.mean$htc.se <- snow.mean$htc.sd / sqrt(snow.mean$htc.n)
snow.mean$swe.ci <- qnorm(0.975) * snow.mean$swe.se
snow.mean$depth.ci <- qnorm(0.975) * snow.mean$depth.se
snow.mean$density.ci <- qnorm(0.975) * snow.mean$density.se
snow.mean$htc.ci <- qnorm(0.975) * snow.mean$htc.se

plot(swe ~ type, park.snow)
points(snow.2020$type, snow.2020$swe.mean, pch = 15)
arrows(c(1:7), snow.2020$swe.mean - snow.2020$swe.ci, c(1:7), snow.2020$swe.mean + snow.2020$swe.ci, length = 0, angle = 90, lwd = 3)

depth.p <- boxplot(depth ~ type, park.snow)
points(snow.mean$type, snow.mean$depth.mean, pch = "---")
points(snow.2020$type, snow.2020$depth.mean, pch = 15)
arrows(c(1:7), snow.2020$depth.mean - snow.2020$depth.ci, c(1:7), snow.2020$depth.mean + snow.2020$depth.ci, length = 0, angle = 90, lwd = 3)
arrows(c(1:7), snow.mean$depth.mean - snow.mean$depth.ci, c(1:7), snow.mean$depth.mean + snow.mean$depth.ci, length = 0, angle = 90, lwd = 3)

cbind.data.frame(snow.mean$type, ((snow.2020$depth.mean - snow.mean$depth.mean) / snow.mean$depth.mean)*100)


plot(density ~ type, park.snow)
points(snow.2020$type, snow.2020$density.mean, pch = 15)
arrows(c(1:7), snow.2020$density.mean - snow.2020$density.ci, c(1:7), snow.2020$density.mean + snow.2020$density.ci, length = 0, angle = 90, lwd = 3)

plot(htc ~ type, log = "y", park.snow)
points(snow.2020$type, snow.2020$htc.mean, pch = 15)
arrows(c(1:7), snow.2020$htc.mean - snow.2020$htc.ci, c(1:7), snow.2020$htc.mean + snow.2020$htc.ci, length = 0, angle = 90, lwd = 3)

# Plot all snow data for 2006-2017
# Export at 8 x 6
par(ps = 12, cex = 1, cex.axis = 1)
par(mfrow = c(2, 1))
par(mar = c(0, 2, 1, 1), oma = c(8,2,1,1))

# Set the colors for the various groups in the boxplots
colors <- c(rep('darkgreen',10),rep('forestgreen',11),rep('green3',12),rep('lightgreen',11),rep('yellow',10),
            rep('orange',11),rep('red',10))

# Plot the first boxplot and color rectangles (SWE)
bp1 <- boxplot(swe ~ plotting, data = park.snow, outline=FALSE, ylim = c(0,550), col = colors, xaxt = "n")
points(bp1$group, bp1$out, type = "p", pch=1, cex = 0.75)
rect(1,-14,10.5,-6, col = 'darkgreen', border = NA)
rect(10.5,-14,21.5,-6, col = 'forestgreen', border = NA)
rect(21.5,-14,33.5,-6, col = 'green3', border = NA)
rect(33.5,-14,44.5,-6, col = 'lightgreen', border = NA)
rect(44.5,-14,54.5,-6, col = 'yellow', border = NA)
rect(54.5,-14,65.5,-6, col = 'orange', border = NA)
rect(65.5,-14,75.5,-6, col = 'red', border = NA)
axis(1, at = c(1,3,5,7,9,11,13,15,16,18,20,22,24,26,28,30,32,35,37,39,41,43,45,47,49,51,53,56,58,60,62,64,66,68,70,72,74),labels = NA, tick = TRUE)
axis(1, at = c(2,4,6,8,10,12,14,17,19,21,23,25,27,29,31,33,34,36,38,40,42,44,46,48,50,52,54,55,57,59,61,63,65,67,69,71,73,75),labels = NA, tick = TRUE, tck = -0.025)
mtext(side=2, "SWE (mm) ",adj=0.5, line=2.5)
legend("topright", "Snow Water Equivalent", bty = "n")

# Plot the first boxplot and color rectangles (HTC)
bp2 <- boxplot(htc ~ plotting, data = park.snow, outline=FALSE, ylim = c(0,16), col = colors, xaxt = "n")
points(bp2$group, bp2$out, type = "p", pch=1, cex = 0.75)
rect(1,-0.45,10.5,-0.25, col = 'darkgreen', border = NA)
rect(10.5,-0.45,21.5,-0.25, col = 'forestgreen', border = NA)
rect(21.5,-0.45,33.5,-0.25, col = 'green3', border = NA)
rect(33.5,-0.45,44.5,-0.25, col = 'lightgreen', border = NA)
rect(44.5,-0.45,54.5,-0.25, col = 'yellow', border = NA)
rect(54.5,-0.45,65.5,-0.25, col = 'orange', border = NA)
rect(65.5,-0.45,75.5,-0.25, col = 'red', border = NA)
# axis(1, at = 1:75, labels = NA, tick = TRUE)
axis(1, at = c(1,3,5,7,9,11,13,15,16,18,20,22,24,26,28,30,32,35,37,39,41,43,45,47,49,51,53,56,58,60,62,64,66,68,70,72,74),labels = NA, tick = TRUE)
axis(1, at = c(2,4,6,8,10,12,14,17,19,21,23,25,27,29,31,33,34,36,38,40,42,44,46,48,50,52,54,55,57,59,61,63,65,67,69,71,73,75),labels = NA, tick = TRUE, tck = -0.025)
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2.5)
legend("topleft", "Heat Transfer Coefficient", bty = "n")
axis(1, at = c(1,5,9,16,20,24,28,32,35,39,43,45,49,53,56,60,64,66,70,74), labels = c(8,12,16,12,16, 8,12,16,8,12,16,8,12,16,8,12,16,8,12,16), tick = FALSE, line = -0.25)
axis(1, at = c(3,7,18,22,26,30,37,41,47,51,58,62,68,72), labels = c(10,14,14, 6,10,14,10,14,10,14,10,14,10,14), tick = FALSE, tck = -0.025, line = 0.5)
mtext(side = 1, "Year", line = 2.6)

# Now plot the colored bar at the bottom with labels
par(xpd = NA)
rect(1,-8,10.5,-9, col = 'darkgreen', border = NA)
rect(10.5,-8,21.5,-9, col = 'forestgreen', border = NA)
rect(21.5,-8,33.5,-9, col = 'green3', border = NA)
rect(33.5,-8,44.5,-9, col = 'lightgreen', border = NA)
rect(44.5,-8,54.5,-9, col = 'yellow', border = NA)
rect(54.5,-8,65.5,-9, col = 'orange', border = NA)
rect(65.5,-8,75.5,-9, col = 'red', border = NA)
text(6,-10,"Tree")
text(6,-11.5,"island")
text(16,-10,"Shrub")
text(27,-10,"Forest")
text(39,-10,"Fen")
text(49.5,-10,"Ice")
text(49.5,-11.5,"wedge")
text(60,-10,"Beach")
text(60,-11.5,"ridge")
text(71,-10,"Polygonal")
text(71,-11.5,"peat plateau")

#######


# Check to see how wind-exposure and year interact to determine SWE

snow <- subset(park.snow, wind != "other")
droplevels(snow$wind)
wind.lm <- glm(swe ~ year*wind, data = snow)
summary(wind.lm)
mean(coef(wind.lm)[14:23])
shit <- alias(wind.lm)[2]
site.means <- aggregate( swe~year+site, snow, mean )






###### Look at trends in met data for Roberge and Mary

ecT <- read.csv("~/Desktop/Workspace/Earthwatch/EC_Churchill_MeanT.csv", header = T)

ec.mean <- ecT %>% 
  group_by(year, month, day) %>%
  summarise(mean = mean(mean, na.rm = F))

ec.mean <- ec.mean[!(ec.mean$year < 2006),]
park.micro

park.mean <- cbind(aggregate(R150~year.1, park.micro, mean), 
                   aggregate(M150~year.1, park.micro, mean)[2])

mean(c(park.mean[c(1:10),2], park.mean[c(1:10),3])) # -4.8631 mean annual
mean(c(park.mean[c(10:11),2], park.mean[c(10:11),3])) # -3.562275 mean annual

rlk.150.mean <- aggregate( R150~year.1+season.2, park.micro, mean)
rlk.0.mean <- aggregate( R0~year.1+season.2, park.micro, mean)
rlk.80.mean <- aggregate( R80~year.1+season.2, park.micro, mean)
rlk.150.min <- aggregate( R150~year.1+season.2, park.micro, min)
rlk.0.min <- aggregate( R0~year.1+season.2, park.micro, min)
rlk.80.min <- aggregate( R80~year.1+season.2, park.micro, min)
rlk.150.max <- aggregate( R150~year.1+season.2, park.micro, max)
rlk.0.max <- aggregate( R0~year.1+season.2, park.micro, max)
rlk.80.max <- aggregate( R80~year.1+season.2, park.micro, max)

mlk.150.mean <- aggregate( M150~year.1+season.2, park.micro, mean)
mlk.0.mean <- aggregate( M0~year.1+season.2, park.micro, mean)
mlk.80.mean <- aggregate( M80~year.1+season.2, park.micro, mean)
mlk.150.min <- aggregate( M150~year.1+season.2, park.micro, min)
mlk.0.min <- aggregate( M0~year.1+season.2, park.micro, min)
mlk.80.min <- aggregate( M80~year.1+season.2, park.micro, min)
mlk.150.max <- aggregate( M150~year.1+season.2, park.micro, max)
mlk.0.max <- aggregate( M0~year.1+season.2, park.micro, max)
mlk.80.max <- aggregate( M80~year.1+season.2, park.micro, max)

# There are no trends in annual values, so I've split the data into cool (Oct-Apr) and
# warm (May-Sep)

wnp.mean.w <- cbind(rlk.150.mean[c(12:22),c(1,3)], rlk.0.mean[c(12:22),3], rlk.80.mean[c(12:22),3], 
                    mlk.150.mean[c(12:22),3], mlk.0.mean[c(12:22),3], mlk.80.mean[c(12:22),3])
names(wnp.mean.w) <- c("year","r150meanw","r0meanw","r80meanw","m150meanw","m0meanw","m80meanw")
wnp.mean.c <- cbind(rlk.150.mean[c(1:11),c(1,3)], rlk.0.mean[c(1:11),3], rlk.80.mean[c(1:11),3], 
                    mlk.150.mean[c(1:11),3], mlk.0.mean[c(1:11),3], mlk.80.mean[c(1:11),3])
names(wnp.mean.c) <- c("year","r150meanc","r0meanc","r80meanc","m150meanc","m0meanc","m80meanc")

wnp.min.w <- cbind(rlk.150.min[c(12:22),c(1,3)], rlk.0.min[c(12:22),3], rlk.80.min[c(12:22),3], 
                    mlk.150.min[c(12:22),3], mlk.0.min[c(12:22),3], mlk.80.min[c(12:22),3])
names(wnp.min.w) <- c("year","r150minw","r0minw","r80minw","m150minw","m0minw","m80minw")
wnp.min.c <- cbind(rlk.150.min[c(1:11),c(1,3)], rlk.0.min[c(1:11),3], rlk.80.min[c(1:11),3], 
                    mlk.150.min[c(1:11),3], mlk.0.min[c(1:11),3], mlk.80.min[c(1:11),3])
names(wnp.min.c) <- c("year","r150minc","r0minc","r80minc","m150minc","m0minc","m80minc")

wnp.max.w <- cbind(rlk.150.max[c(12:22),c(1,3)], rlk.0.max[c(12:22),3], rlk.80.max[c(12:22),3], 
                    mlk.150.max[c(12:22),3], mlk.0.max[c(12:22),3], mlk.80.max[c(12:22),3])
names(wnp.max.w) <- c("year","r150maxw","r0maxw","r80maxw","m150maxw","m0maxw","m80maxw")
wnp.max.c <- cbind(rlk.150.max[c(1:11),c(1,3)], rlk.0.max[c(1:11),3], rlk.80.max[c(1:11),3], 
                    mlk.150.max[c(1:11),3], mlk.0.max[c(1:11),3], mlk.80.max[c(1:11),3])
names(wnp.max.c) <- c("year","r150maxc","r0maxc","r80maxc","m150maxc","m0maxc","m80maxc")

wnp.met <- cbind(wnp.mean.w, wnp.mean.c[,c(2:7)], wnp.min.w[,c(2:7)], wnp.min.c[,c(2:7)],
                 wnp.max.w[,c(2:7)], wnp.max.c[,c(2:7)])

matplot(wnp.mean.w$year, wnp.mean.w[,c(2:7)], type = "l")
matplot(wnp.mean.c$year, wnp.mean.c[,c(2:7)], type = "l")

matplot(wnp.min.w$year, wnp.min.w[,c(2:7)], type = "l")
matplot(wnp.min.c$year, wnp.min.c[,c(2:7)], type = "l")

matplot(wnp.max.w$year, wnp.max.w[,c(2:7)], type = "l")
matplot(wnp.max.c$year, wnp.max.c[,c(2:7)], type = "l")



summary(lm(R150 ~ year.1, data = wnp.mean))


A <- wnp.met[,c(2:37)]
B <- wnp.met[,1]
output = matrix(nrow=ncol(A), ncol=3) # Here is the table the loop will output.pu to - 3 columns b/c 3 variables
count = 1 # Start at the beginning eh?
for( i in 1:ncol(A) ){
  # zinb.pu <- glm.nb(A[,i] ~ B) # Negbin regression
  met.lm <- glm(A[,i] ~ B, family = gaussian) # Normal regression
  output[count,] = c(colnames(A)[i],coef(summary(met.lm))[2,1],coef(summary(met.lm))[2,4]) # Add column names, slope estimate, and p-values
  count = count + 1
}
colnames(output) <- c("Metric","Slope","Pvalue")
output <- as.data.frame(output)
colnames(output) <- c("Metric","Slope","Pvalue")
output$Slope <- as.numeric(as.character(output$Slope))
output$Pvalue <- as.numeric(as.character(output$Pvalue))
# output.sig <- subset(output, Pvalue < 0.05)

matplot(wnp.met$year, wnp.met[,c(2,5,8,11)], type = "l", col = c(rep("red",2), rep("blue",2)),
        lty = c(1,2,1,2))
matplot(wnp.met$year, wnp.met[,c(2,5,8,11)], type = "l", col = c(rep("red",2), rep("blue",2)),
        lty = c(1,2,1,2))


#################################################################
#################################################################
#################################################################

# Explore the parks needle data

bp1 <- boxplot(gmin ~ year*zone, data = park.needles, ylim = c(0,0.00012))#, outline=FALSE, xaxt = "n")

needle.mean <- aggregate(park.needles$gmin, list(park.needles$year), mean, na.rm = TRUE)
needle.mean$Group.1 <- as.numeric(as.character(needle.mean$Group.1))
plot(park.mean$year.1, park.mean$M150, type = "l", xlim = c(2006,2017))
par(new = T)
plot(needle.mean$Group.1, needle.mean$x, type = "l", xlim = c(2007,2018), ylim = c(0.00001,0.00006), col = "blue", xaxt = "n")

plot(park.mean[c(7:10),2], needle.mean$x, type = "p")

matplot(wnp.mean.w[c(8:11),1], wnp.mean.w[c(8:11),c(2,5)], type = "l", col = c("blue","red"))

#################################################################
#################################################################
#################################################################



bp1 <- boxplot(gmin ~ year*site, ylim = c(0,0.00015), col = c(rep("green",3),rep("blue",3),rep("yellow",3),
                                                              rep("orange",3),rep("darkorchid",3)),
                                                              data = park.needles)


bp1 <- boxplot(100-viability ~ year, data = park.needles)

aggregate(100-park.needles$viability, list(park.needles$year), mean, na.rm = TRUE)


points(bp1$group, bp1$out, type = "p", pch=1, cex = 0.75)
axis(1, at = 1:68, labels = NA, tick = TRUE)
mtext(side=2, "SWE (mm) ",adj=0.5, line=2.5)

#####################
#******Level 1******#
#####################

# Subset by landscape type
beach <- subset(park.snow, type == "beach") 
fen <- subset(park.snow, type == "fen")
forest <- subset(park.snow, type == "forest")
# coastal.forest <- subset(park.snow, type == "coastal.forest")
# wetland.forest <- subset(park.snow, type == "wetland.forest")
island <- subset(park.snow, type == "island") 
polygon <- subset(park.snow, type == "polygon")
shrub <- subset(park.snow, type == "shrub") 
wedge <- subset(park.snow, type == "wedge") 

# Run t-tests comparing 2006-2010 to 2011-2015, when there are enough data available
# with(beach, t.test(beach$depth[beach$period == "A"], beach$depth[beach$period == "B"]))
# with(beach, wilcox.test(depth ~ period))

with(beach, t.test(depth ~ period), paired = T) # p-value < 2.2e-16***
with(beach, t.test(density ~ period), paired = T) # p-value = 5.786e-10***
with(beach, t.test(swe ~ period), paired = T) # p-value < 2.2e-16***
with(beach, t.test(htc ~ period), paired = T) # p-value < 2.2e-16***

with(fen, t.test(depth ~ period), paired = T) # p-value = 1.807e-14***
with(fen, t.test(density ~ period), paired = T) # p-value = 0.7715
with(fen, t.test(swe ~ period), paired = T) # p-value = 6.558e-12***
with(fen, t.test(htc ~ period), paired = T) # p-value = 8.802e-10***

with(coastal.forest, t.test(depth ~ period), paired = T) # p-value = 8.263e-06***
with(coastal.forest, t.test(density ~ period), paired = T) # p-value = 0.0001518***
with(coastal.forest, t.test(swe ~ period), paired = T) # p-value = 2.259e-07***
with(coastal.forest, t.test(htc ~ period), paired = T) # p-value = 0.1683

with(wetland.forest, t.test(depth ~ period), paired = T) # p-value = 0.2898
with(wetland.forest, t.test(density ~ period), paired = T) # p-value = 1.637e-05***
with(wetland.forest, t.test(swe ~ period), paired = T) # p-value = 0.0007409***
with(wetland.forest, t.test(htc ~ period), paired = T) # p-value = 0.0001087***

with(island, t.test(depth ~ period), paired = T) # p-value = 7.853e-05***
with(island, t.test(density ~ period), paired = T) # p-value = 0.02468*
with(island, t.test(swe ~ period), paired = T) # p-value = 0.0001148***
with(island, t.test(htc ~ period), paired = T) # p-value = 0.6759

with(polygon, t.test(depth ~ period), paired = T) # p-value = 2.056e-08***
with(polygon, t.test(density ~ period), paired = T) # p-value = = 2.699e-05***
with(polygon, t.test(swe ~ period), paired = T) # p-value = 7.662e-10***
with(polygon, t.test(htc ~ period), paired = T) # p-value = 0.005405**

with(shrub, t.test(depth ~ period), paired = T) # p-value < 2.2e-16***
with(shrub, t.test(density ~ period), paired = T) # p-value = 3.155e-07***
with(shrub, t.test(swe ~ period), paired = T) # p-value < 2.2e-16***
with(shrub, t.test(htc ~ period), paired = T) # p-value = 0.04621*

with(wedge, t.test(depth ~ period), paired = T) # p-value = 0.0008406***
with(wedge, t.test(density ~ period), paired = T) # p-value = 0.5263
with(wedge, t.test(swe ~ period), paired = T) # p-value = 2.885e-05***
with(wedge, t.test(htc ~ period), paired = T) # p-value = 0.1696

# Calculate means and standard deviations for each landscape type
beach.depth.avg <- aggregate(beach$depth, by = list(beach$year), FUN = mean, na.rm=TRUE, na.action=NULL)
beach.depth.sd <- aggregate(beach$depth, by = list(beach$year), FUN = sd, na.rm=TRUE)
beach.swe.avg <- aggregate(beach$swe, by = list(beach$year), FUN = mean, na.rm=TRUE, na.action=NULL)
beach.swe.sd <- aggregate(beach$swe, by = list(beach$year), FUN = sd, na.rm=TRUE)
beach.density.avg <- aggregate(beach$density, by = list(beach$year), FUN = mean, na.rm=TRUE, na.action=NULL)
beach.density.sd <- aggregate(beach$density, by = list(beach$year), FUN = sd, na.rm=TRUE)
beach.htc.avg <- aggregate(beach$htc, by = list(beach$year), FUN = mean, na.rm=TRUE, na.action=NULL)
beach.htc.sd <- aggregate(beach$htc, by = list(beach$year), FUN = sd, na.rm=TRUE)

fen.depth.avg <- aggregate(fen$depth, by = list(fen$year), FUN = mean, na.rm=TRUE, na.action=NULL)
fen.depth.sd <- aggregate(fen$depth, by = list(fen$year), FUN = sd, na.rm=TRUE)
fen.swe.avg <- aggregate(fen$swe, by = list(fen$year), FUN = mean, na.rm=TRUE, na.action=NULL)
fen.swe.sd <- aggregate(fen$swe, by = list(fen$year), FUN = sd, na.rm=TRUE)
fen.density.avg <- aggregate(fen$density, by = list(fen$year), FUN = mean, na.rm=TRUE, na.action=NULL)
fen.density.sd <- aggregate(fen$density, by = list(fen$year), FUN = sd, na.rm=TRUE)
fen.htc.avg <- aggregate(fen$htc, by = list(fen$year), FUN = mean, na.rm=TRUE, na.action=NULL)
fen.htc.sd <- aggregate(fen$htc, by = list(fen$year), FUN = sd, na.rm=TRUE)

# coastal.forest.depth.avg <- aggregate(coastal.forest$depth, by = list(coastal.forest$year), FUN = mean, na.rm=TRUE, na.action=NULL)
# coastal.forest.depth.sd <- aggregate(coastal.forest$depth, by = list(coastal.forest$year), FUN = sd, na.rm=TRUE)
# coastal.forest.swe.avg <- aggregate(coastal.forest$swe, by = list(coastal.forest$year), FUN = mean, na.rm=TRUE, na.action=NULL)
# coastal.forest.swe.sd <- aggregate(coastal.forest$swe, by = list(coastal.forest$year), FUN = sd, na.rm=TRUE)
# coastal.forest.density.avg <- aggregate(coastal.forest$density, by = list(coastal.forest$year), FUN = mean, na.rm=TRUE, na.action=NULL)
# coastal.forest.density.sd <- aggregate(coastal.forest$density, by = list(coastal.forest$year), FUN = sd, na.rm=TRUE)
# coastal.forest.htc.avg <- aggregate(coastal.forest$htc, by = list(coastal.forest$year), FUN = mean, na.rm=TRUE, na.action=NULL)
# coastal.forest.htc.sd <- aggregate(coastal.forest$htc, by = list(coastal.forest$year), FUN = sd, na.rm=TRUE)
# 
# wetland.forest.depth.avg <- aggregate(wetland.forest$depth, by = list(wetland.forest$year), FUN = mean, na.rm=TRUE, na.action=NULL)
# wetland.forest.depth.sd <- aggregate(wetland.forest$depth, by = list(wetland.forest$year), FUN = sd, na.rm=TRUE)
# wetland.forest.swe.avg <- aggregate(wetland.forest$swe, by = list(wetland.forest$year), FUN = mean, na.rm=TRUE, na.action=NULL)
# wetland.forest.swe.sd <- aggregate(wetland.forest$swe, by = list(wetland.forest$year), FUN = sd, na.rm=TRUE)
# wetland.forest.density.avg <- aggregate(wetland.forest$density, by = list(wetland.forest$year), FUN = mean, na.rm=TRUE, na.action=NULL)
# wetland.forest.density.sd <- aggregate(wetland.forest$density, by = list(wetland.forest$year), FUN = sd, na.rm=TRUE)
# wetland.forest.htc.avg <- aggregate(wetland.forest$htc, by = list(wetland.forest$year), FUN = mean, na.rm=TRUE, na.action=NULL)
# wetland.forest.htc.sd <- aggregate(wetland.forest$htc, by = list(wetland.forest$year), FUN = sd, na.rm=TRUE)

island.depth.avg <- aggregate(island$depth, by = list(island$year), FUN = mean, na.rm=TRUE, na.action=NULL)
island.depth.sd <- aggregate(island$depth, by = list(island$year), FUN = sd, na.rm=TRUE)
island.swe.avg <- aggregate(island$swe, by = list(island$year), FUN = mean, na.rm=TRUE, na.action=NULL)
island.swe.sd <- aggregate(island$swe, by = list(island$year), FUN = sd, na.rm=TRUE)
island.density.avg <- aggregate(island$density, by = list(island$year), FUN = mean, na.rm=TRUE, na.action=NULL)
island.density.sd <- aggregate(island$density, by = list(island$year), FUN = sd, na.rm=TRUE)
island.htc.avg <- aggregate(island$htc, by = list(island$year), FUN = mean, na.rm=TRUE, na.action=NULL)
island.htc.sd <- aggregate(island$htc, by = list(island$year), FUN = sd, na.rm=TRUE)

polygon.depth.avg <- aggregate(polygon$depth, by = list(polygon$year), FUN = mean, na.rm=TRUE, na.action=NULL)
polygon.depth.sd <- aggregate(polygon$depth, by = list(polygon$year), FUN = sd, na.rm=TRUE)
polygon.swe.avg <- aggregate(polygon$swe, by = list(polygon$year), FUN = mean, na.rm=TRUE, na.action=NULL)
polygon.swe.sd <- aggregate(polygon$swe, by = list(polygon$year), FUN = sd, na.rm=TRUE)
polygon.density.avg <- aggregate(polygon$density, by = list(polygon$year), FUN = mean, na.rm=TRUE, na.action=NULL)
polygon.density.sd <- aggregate(polygon$density, by = list(polygon$year), FUN = sd, na.rm=TRUE)
polygon.htc.avg <- aggregate(polygon$htc, by = list(polygon$year), FUN = mean, na.rm=TRUE, na.action=NULL)
polygon.htc.sd <- aggregate(polygon$htc, by = list(polygon$year), FUN = sd, na.rm=TRUE)

shrub.depth.avg <- aggregate(shrub$depth, by = list(shrub$year), FUN = mean, na.rm=TRUE, na.action=NULL)
shrub.depth.sd <- aggregate(shrub$depth, by = list(shrub$year), FUN = sd, na.rm=TRUE)
shrub.swe.avg <- aggregate(shrub$swe, by = list(shrub$year), FUN = mean, na.rm=TRUE, na.action=NULL)
shrub.swe.sd <- aggregate(shrub$swe, by = list(shrub$year), FUN = sd, na.rm=TRUE)
shrub.density.avg <- aggregate(shrub$density, by = list(shrub$year), FUN = mean, na.rm=TRUE, na.action=NULL)
shrub.density.sd <- aggregate(shrub$density, by = list(shrub$year), FUN = sd, na.rm=TRUE)
shrub.htc.avg <- aggregate(shrub$htc, by = list(shrub$year), FUN = mean, na.rm=TRUE, na.action=NULL)
shrub.htc.sd <- aggregate(shrub$htc, by = list(shrub$year), FUN = sd, na.rm=TRUE)

wedge.depth.avg <- aggregate(wedge$depth, by = list(wedge$year), FUN = mean, na.rm=TRUE, na.action=NULL)
wedge.depth.sd <- aggregate(wedge$depth, by = list(wedge$year), FUN = sd, na.rm=TRUE)
wedge.swe.avg <- aggregate(wedge$swe, by = list(wedge$year), FUN = mean, na.rm=TRUE, na.action=NULL)
wedge.swe.sd <- aggregate(wedge$swe, by = list(wedge$year), FUN = sd, na.rm=TRUE)
wedge.density.avg <- aggregate(wedge$density, by = list(wedge$year), FUN = mean, na.rm=TRUE, na.action=NULL)
wedge.density.sd <- aggregate(wedge$density, by = list(wedge$year), FUN = sd, na.rm=TRUE)
wedge.htc.avg <- aggregate(wedge$htc, by = list(wedge$year), FUN = mean, na.rm=TRUE, na.action=NULL)
wedge.htc.sd <- aggregate(wedge$htc, by = list(wedge$year), FUN = sd, na.rm=TRUE)

## Plotting each snowpack metric versus time for each landscape type (beach, polygon, wedge, shrub, forest,
## tree island, fen)

# BEACH
# Export at 7 x 7
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(0, 2, 1, 3), oma = c(4,2,1,1))

plot(beach.depth.avg$x ~ beach.depth.avg$Group.1, type = "p", xaxt = "n", ylim = c(0,60), ylab = "",xlab = "",pch=19)
beach.depth.glm <- glm(beach.depth.avg$x ~ beach.depth.avg$Group.1, data = beach.depth.avg, family = gaussian)
summary(beach.depth.glm) # Slope = -3.047, P = 0.0488
# abline(coef(beach.depth.glm), lty = 2)
arrows(c(2007:2016),beach.depth.avg[1:10,2], c(2007:2016), beach.depth.avg[1:10,2]+beach.depth.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),beach.depth.avg[1:10,2], c(2007:2016), beach.depth.avg[1:10,2]-beach.depth.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE)   
mtext(side=2, "Depth (cm)", adj=0.5, line=2.5)

plot(beach.swe.avg$x ~ beach.swe.avg$Group.1, type = "p",xaxt = "n", ylim = c(0,180), ylab = "",xlab = "",pch=19)
beach.swe.glm <- glm(beach.swe.avg$x ~ beach.swe.avg$Group.1, data = beach.swe.avg, family = gaussian)
summary(beach.swe.glm) # Slope = -10.048, P = 0.0682
# abline(coef(beach.swe.glm), lty = 2)
arrows(c(2007:2016),beach.swe.avg[1:10,2], c(2007:2016), beach.swe.avg[1:10,2]+beach.swe.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),beach.swe.avg[1:10,2], c(2007:2016), beach.swe.avg[1:10,2]-beach.swe.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE)
mtext(side=2, "SWE (mm)", adj=0.5, line=2.5)

plot(beach.density.avg$x ~ beach.density.avg$Group.1, type = "p",xaxt = "n", ylim = c(160,400), ylab = "",xlab = "",pch=19)
beach.density.glm <- glm(beach.density.avg$x ~ beach.density.avg$Group.1, data = beach.density.avg, family = gaussian)
summary(beach.density.glm) # Slope = -6.332, P = 0.0352*
abline(coef(beach.density.glm), lty = 2)
arrows(c(2007:2016),beach.density.avg[1:10,2], c(2007:2016), beach.density.avg[1:10,2]+beach.density.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),beach.density.avg[1:10,2], c(2007:2016), beach.density.avg[1:10,2]-beach.density.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE) 
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)

plot(beach.htc.avg$x ~ beach.htc.avg$Group.1, type = "p",xaxt = "n", ylim = c(0,5), ylab = "",xlab = "",pch=19)
beach.htc.glm <- glm(beach.htc.avg$x ~ beach.htc.avg$Group.1, data = beach.htc.avg, family = gaussian)
summary(beach.htc.glm) # Slope = 0.2447, P = 0.102
# abline(coef(beach.htc.glm), lty = 2)
arrows(c(2007:2016),beach.htc.avg[1:10,2], c(2007:2016), beach.htc.avg[1:10,2]+beach.htc.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),beach.htc.avg[1:10,2], c(2007:2016), beach.htc.avg[1:10,2]-beach.htc.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = c(2007:2016), tick = TRUE)   
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
mtext(side=1, "Year", adj=0.5, line=2.5)

# FEN
# Export at 7 x 7
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(0, 2, 1, 3), oma = c(4,2,1,1))

plot(fen.depth.avg$x ~ fen.depth.avg$Group.1, type = "p",xaxt = "n", ylim = c(0,90), ylab = "",xlab = "",pch=19)
fen.depth.glm <- glm(fen.depth.avg$x ~ fen.depth.avg$Group.1, data = fen.depth.avg, family = gaussian)
summary(fen.depth.glm) # Slope = -3.220, P = 0.121
# abline(coef(fen.depth.glm), lty = 2)
arrows(c(2007:2016),fen.depth.avg[1:10,2], c(2007:2016), fen.depth.avg[1:10,2]+fen.depth.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),fen.depth.avg[1:10,2], c(2007:2016), fen.depth.avg[1:10,2]-fen.depth.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE)   
mtext(side=2, "Depth (cm)", adj=0.5, line=2.5)

plot(fen.swe.avg$x ~ fen.swe.avg$Group.1, type = "p",xaxt = "n", ylim = c(0,230), ylab = "",xlab = "",pch=19)
fen.swe.glm <- glm(fen.swe.avg$x ~ fen.swe.avg$Group.1, data = fen.swe.avg, family = gaussian)
summary(fen.swe.glm) # Slope = -10.499, P = 0.0967
# abline(coef(fen.swe.glm), lty = 2)
arrows(c(2007:2016),fen.swe.avg[1:10,2], c(2007:2016), fen.swe.avg[1:10,2]+fen.swe.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),fen.swe.avg[1:10,2], c(2007:2016), fen.swe.avg[1:10,2]-fen.swe.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE)    
mtext(side=2, "SWE (mm)", adj=0.5, line=2.5)

plot(fen.density.avg$x ~ fen.density.avg$Group.1, type = "p",xaxt = "n", ylim = c(140,400), ylab = "",xlab = "",pch=19)
fen.density.glm <- glm(fen.density.avg$x ~ fen.density.avg$Group.1, data = fen.density.avg, family = gaussian)
summary(fen.density.glm) # Slope = 0.1587, P = 0.974
# abline(coef(fen.density.glm), lty = 2)
arrows(c(2007:2016),fen.density.avg[1:10,2], c(2007:2016), fen.density.avg[1:10,2]+fen.density.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),fen.density.avg[1:10,2], c(2007:2016), fen.density.avg[1:10,2]-fen.density.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE)     
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)

plot(fen.htc.avg$x ~ fen.htc.avg$Group.1, type = "p",xaxt = "n", ylim = c(0,5), ylab = "",xlab = "",pch=19)
fen.htc.glm <- glm(fen.htc.avg$x ~ fen.htc.avg$Group.1, data = fen.htc.avg, family = gaussian)
summary(fen.htc.glm) # Slope = 0.1748, P = 0.284
# abline(coef(fen.htc.glm), lty = 2)
arrows(c(2007:2016),fen.htc.avg[1:10,2], c(2007:2016), fen.htc.avg[1:10,2]+fen.htc.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),fen.htc.avg[1:10,2], c(2007:2016), fen.htc.avg[1:10,2]-fen.htc.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = c(2007:2016), tick = TRUE)
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
mtext(side=1, "Year", adj=0.5, line=2.5)

# # COASTAL FOREST
# # Export at 7 x 7
# par(mfrow = c(4, 1))
# par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
# par(mar = c(0, 2, 1, 3), oma = c(4,2,1,1))

# plot(coastal.forest.depth.avg$x ~ coastal.forest.depth.avg$Group.1, type = "p",xaxt = "n", ylim = c(40,120), ylab = "",xlab = "",pch=19)
# coastal.forest.depth.glm <- glm(coastal.forest.depth.avg$x ~ coastal.forest.depth.avg$Group.1, data = coastal.forest.depth.avg, family = gaussian)
# summary(coastal.forest.depth.glm) # Slope = 1.153, P = 0.390
# # abline(coef(coastal.forest.depth.glm), lty = 2)
# arrows(c(2007:2016),coastal.forest.depth.avg[1:10,2], c(2007:2016), coastal.forest.depth.avg[1:10,2]+coastal.forest.depth.sd[1:10,2], length=0.05, angle=90, code=3)
# arrows(c(2007:2016),coastal.forest.depth.avg[1:10,2], c(2007:2016), coastal.forest.depth.avg[1:10,2]-coastal.forest.depth.sd[1:10,2], length=0.05, angle=90, code=3)
# axis(1, at = c(2007:2016), labels = NA, tick = TRUE)
# mtext(side=2, "Depth (cm)", adj=0.5, line=2.5)
# 
# plot(coastal.forest.swe.avg$x ~ coastal.forest.swe.avg$Group.1, type = "p",xaxt = "n", ylim = c(50,330), ylab = "",xlab = "",pch=19)
# coastal.forest.swe.glm <- glm(coastal.forest.swe.avg$x ~ coastal.forest.swe.avg$Group.1, data = coastal.forest.swe.avg, family = gaussian)
# summary(coastal.forest.swe.glm) # Slope = 3.957, P = 0.455
# # abline(coef(coastal.forest.swe.glm), lty = 2)
# arrows(c(2007:2016),coastal.forest.swe.avg[1:10,2], c(2007:2016), coastal.forest.swe.avg[1:10,2]+coastal.forest.swe.sd[1:10,2], length=0.05, angle=90, code=3)
# arrows(c(2007:2016),coastal.forest.swe.avg[1:10,2], c(2007:2016), coastal.forest.swe.avg[1:10,2]-coastal.forest.swe.sd[1:10,2], length=0.05, angle=90, code=3)
# axis(1, at = c(2007:2016), labels = NA, tick = TRUE)
# mtext(side=2, "SWE (mm)", adj=0.5, line=2.5)
# 
# plot(coastal.forest.density.avg$x ~ coastal.forest.density.avg$Group.1, type = "p",xaxt = "n", ylim = c(150,280), ylab = "",xlab = "",pch=19)
# coastal.forest.density.glm <- glm(coastal.forest.density.avg$x ~ coastal.forest.density.avg$Group.1, data = coastal.forest.density.avg, family = gaussian)
# summary(coastal.forest.density.glm) # Slope = 1.113, P = 0.739
# # abline(coef(coastal.forest.density.glm), lty = 2)
# arrows(c(2007:2016),coastal.forest.density.avg[1:10,2], c(2007:2016), coastal.forest.density.avg[1:10,2]+coastal.forest.density.sd[1:10,2], length=0.05, angle=90, code=3)
# arrows(c(2007:2016),coastal.forest.density.avg[1:10,2], c(2007:2016), coastal.forest.density.avg[1:10,2]-coastal.forest.density.sd[1:10,2], length=0.05, angle=90, code=3)
# axis(1, at = c(2007:2016), labels = NA, tick = TRUE)
# mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)
# 
# plot(coastal.forest.htc.avg$x ~ coastal.forest.htc.avg$Group.1, type = "p",xaxt = "n", ylim = c(0.1,0.3), ylab = "",xlab = "",pch=19)
# coastal.forest.htc.glm <- glm(coastal.forest.htc.avg$x ~ coastal.forest.htc.avg$Group.1, data = coastal.forest.htc.avg, family = gaussian)
# summary(coastal.forest.htc.glm) # Slope = -1.013e-05, P = 0.998
# # abline(coef(coastal.forest.htc.glm), lty = 2)
# arrows(c(2007:2016),coastal.forest.htc.avg[1:10,2], c(2007:2016), coastal.forest.htc.avg[1:10,2]+coastal.forest.htc.sd[1:10,2], length=0.05, angle=90, code=3)
# arrows(c(2007:2016),coastal.forest.htc.avg[1:10,2], c(2007:2016), coastal.forest.htc.avg[1:10,2]-coastal.forest.htc.sd[1:10,2], length=0.05, angle=90, code=3)
# axis(1, at = c(2007:2016), labels = c(2007:2016), tick = TRUE)
# mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
# mtext(side=1, "Year", adj=0.5, line=2.5)

# # WETLAND FOREST
# # Export at 7 x 7
# par(mfrow = c(4, 1))
# par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
# par(mar = c(0, 2, 1, 3), oma = c(4,2,1,1))
# 
# plot(wetland.forest.depth.avg$x ~ wetland.forest.depth.avg$Group.1, type = "p",xaxt = "n", ylim = c(30,100), ylab = "",xlab = "",pch=19)
# wetland.forest.depth.glm <- glm(wetland.forest.depth.avg$x ~ wetland.forest.depth.avg$Group.1, data = wetland.forest.depth.avg, family = gaussian)
# summary(wetland.forest.depth.glm) # Slope = 0.2728, P = 0.815
# # abline(coef(wetland.forest.depth.glm), lty = 2)
# arrows(c(2006:2016),wetland.forest.depth.avg[1:10,2], c(2006:2016), wetland.forest.depth.avg[1:10,2]+wetland.forest.depth.sd[1:10,2], length=0.05, angle=90, code=3)
# arrows(c(2006:2016),wetland.forest.depth.avg[1:10,2], c(2006:2016), wetland.forest.depth.avg[1:10,2]-wetland.forest.depth.sd[1:10,2], length=0.05, angle=90, code=3)
# axis(1, at = c(2006:2016), labels = NA, tick = TRUE)
# mtext(side=2, "Depth (cm)", adj=0.5, line=2.5)
# 
# plot(wetland.forest.swe.avg$x ~ wetland.forest.swe.avg$Group.1, type = "p",xaxt = "n", ylim = c(60,275), ylab = "",xlab = "",pch=19)
# wetland.forest.swe.glm <- glm(wetland.forest.swe.avg$x ~ wetland.forest.swe.avg$Group.1, data = wetland.forest.swe.avg, family = gaussian)
# summary(wetland.forest.swe.glm) # Slope = -2.571, P = 0.540
# # abline(coef(wetland.forest.swe.glm), lty = 2)
# arrows(c(2006:2016),wetland.forest.swe.avg[1:10,2], c(2006:2016), wetland.forest.swe.avg[1:10,2]+wetland.forest.swe.sd[1:10,2], length=0.05, angle=90, code=3)
# arrows(c(2006:2016),wetland.forest.swe.avg[1:10,2], c(2006:2016), wetland.forest.swe.avg[1:10,2]-wetland.forest.swe.sd[1:10,2], length=0.05, angle=90, code=3)
# axis(1, at = c(2006:2016), labels = NA, tick = TRUE)
# mtext(side=2, "SWE (mm)", adj=0.5, line=2.5)
# 
# plot(wetland.forest.density.avg$x ~ wetland.forest.density.avg$Group.1, type = "p",xaxt = "n", ylim = c(150,300), ylab = "",xlab = "",pch=19)
# wetland.forest.density.glm <- glm(wetland.forest.density.avg$x ~ wetland.forest.density.avg$Group.1, data = wetland.forest.density.avg, family = gaussian)
# summary(wetland.forest.density.glm) # Slope = -4.119, P = 0.173
# # abline(coef(wetland.forest.density.glm), lty = 2)
# arrows(c(2006:2016),wetland.forest.density.avg[1:10,2], c(2006:2016), wetland.forest.density.avg[1:10,2]+wetland.forest.density.sd[1:10,2], length=0.05, angle=90, code=3)
# arrows(c(2006:2016),wetland.forest.density.avg[1:10,2], c(2006:2016), wetland.forest.density.avg[1:10,2]-wetland.forest.density.sd[1:10,2], length=0.05, angle=90, code=3)
# axis(1, at = c(2006:2016), labels = NA, tick = TRUE)
# mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)
# 
# plot(wetland.forest.htc.avg$x ~ wetland.forest.htc.avg$Group.1, type = "p",xaxt = "n", ylim = c(0.1,0.4), ylab = "",xlab = "",pch=19)
# wetland.forest.htc.glm <- glm(wetland.forest.htc.avg$x ~ wetland.forest.htc.avg$Group.1, data = wetland.forest.htc.avg, family = gaussian)
# summary(wetland.forest.htc.glm) # Slope = -0.00967, P = 0.0731
# # abline(coef(wetland.forest.htc.glm), lty = 2)
# arrows(c(2006:2016),wetland.forest.htc.avg[1:10,2], c(2006:2016), wetland.forest.htc.avg[1:10,2]+wetland.forest.htc.sd[1:10,2], length=0.05, angle=90, code=3)
# arrows(c(2006:2016),wetland.forest.htc.avg[1:10,2], c(2006:2016), wetland.forest.htc.avg[1:10,2]-wetland.forest.htc.sd[1:10,2], length=0.05, angle=90, code=3)
# axis(1, at = c(2006:2016), labels = c(2006:2016), tick = TRUE)
# mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
# mtext(side=1, "Year", adj=0.5, line=2.5)
# 

# TREE ISLANDS
# Export at 7 x 7
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(0, 2, 1, 3), oma = c(4,2,1,1))

plot(island.depth.avg$x ~ island.depth.avg$Group.1, type = "p",xaxt = "n", ylim = c(60,170), ylab = "",xlab = "",pch=19)
island.depth.glm <- glm(island.depth.avg$x ~ island.depth.avg$Group.1, data = island.depth.avg, family = gaussian)
summary(island.depth.glm) # Slope = 4.907, P = 0.102
# abline(coef(island.depth.glm), lty = 2)
arrows(c(2008:2016),island.depth.avg[1:9,2], c(2008:2016), island.depth.avg[1:9,2]+island.depth.sd[1:9,2], length=0.05, angle=90, code=3)
arrows(c(2008:2016),island.depth.avg[1:9,2], c(2008:2016), island.depth.avg[1:9,2]-island.depth.sd[1:9,2], length=0.05, angle=90, code=3)
axis(1, at = c(2008:2016), labels = NA, tick = TRUE)
mtext(side=2, "Depth (cm)", adj=0.5, line=2.5)

plot(island.swe.avg$x ~ island.swe.avg$Group.1, type = "p",xaxt = "n", ylim = c(100,500), ylab = "",xlab = "",pch=19)
island.swe.glm <- glm(island.swe.avg$x ~ island.swe.avg$Group.1, data = island.swe.avg, family = gaussian)
summary(island.swe.glm) # Slope = 18.762, P = 0.0892
# abline(coef(island.swe.glm), lty = 2)
arrows(c(2008:2016),island.swe.avg[1:9,2], c(2008:2016), island.swe.avg[1:9,2]+island.swe.sd[1:9,2], length=0.05, angle=90, code=3)
arrows(c(2008:2016),island.swe.avg[1:9,2], c(2008:2016), island.swe.avg[1:9,2]-island.swe.sd[1:9,2], length=0.05, angle=90, code=3)
axis(1, at = c(2008:2016), labels = NA, tick = TRUE)
mtext(side=2, "SWE (mm)", adj=0.5, line=2.5)

plot(island.density.avg$x ~ island.density.avg$Group.1, type = "p",xaxt = "n", ylim = c(160,360), ylab = "",xlab = "",pch=19)
island.density.glm <- glm(island.density.avg$x ~ island.density.avg$Group.1, data = island.density.avg, family = gaussian)
summary(island.density.glm) # Slope = 5.754, P = 0.203
# abline(coef(island.density.glm), lty = 2)
arrows(c(2008:2016),island.density.avg[1:9,2], c(2008:2016), island.density.avg[1:9,2]+island.density.sd[1:9,2], length=0.05, angle=90, code=3)
arrows(c(2008:2016),island.density.avg[1:9,2], c(2008:2016), island.density.avg[1:9,2]-island.density.sd[1:9,2], length=0.05, angle=90, code=3)
axis(1, at = c(2008:2016), labels = NA, tick = TRUE)
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)

plot(island.htc.avg$x ~ island.htc.avg$Group.1, type = "p",xaxt = "n", ylim = c(0,0.5), ylab = "",xlab = "",pch=19)
island.htc.glm <- glm(island.htc.avg$x ~ island.htc.avg$Group.1, data = island.htc.avg, family = gaussian)
summary(island.htc.glm) # Slope = 0.0005203, P = 0.934
# abline(coef(island.htc.glm), lty = 2)
arrows(c(2008:2016),island.htc.avg[1:9,2], c(2008:2016), island.htc.avg[1:9,2]+island.htc.sd[1:9,2], length=0.05, angle=90, code=3)
arrows(c(2008:2016),island.htc.avg[1:9,2], c(2008:2016), island.htc.avg[1:9,2]-island.htc.sd[1:9,2], length=0.05, angle=90, code=3)
axis(1, at = c(2008:2016), labels = c(2008:2016), tick = TRUE)
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
mtext(side=1, "Year", adj=0.5, line=2.5)

# POLYGON CENTERS
# Export at 7 x 7
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(0, 2, 1, 3), oma = c(4,2,1,1))

plot(polygon.depth.avg$x ~ polygon.depth.avg$Group.1, type = "p",xaxt = "n", ylim = c(0,25), ylab = "",xlab = "",pch=19)
polygon.depth.glm <- glm(polygon.depth.avg$x ~ polygon.depth.avg$Group.1, data = polygon.depth.avg, family = gaussian)
summary(polygon.depth.glm) # Slope = -0.563, P = 0.446
# abline(coef(polygon.depth.glm), lty = 2)
arrows(c(2008:2016),polygon.depth.avg[1:9,2], c(2008:2016), polygon.depth.avg[1:9,2]+polygon.depth.sd[1:9,2], length=0.05, angle=90, code=3)
arrows(c(2008:2016),polygon.depth.avg[1:9,2], c(2008:2016), polygon.depth.avg[1:9,2]-polygon.depth.sd[1:9,2], length=0.05, angle=90, code=3)
axis(1, at = c(2008:2016), labels = NA, tick = TRUE) 
mtext(side=2, "Depth (cm)", adj=0.5, line=2.5)

plot(polygon.swe.avg$x ~ polygon.swe.avg$Group.1, type = "p",xaxt = "n", ylim = c(0,70), ylab = "",xlab = "",pch=19)
polygon.swe.glm <- glm(polygon.swe.avg$x ~ polygon.swe.avg$Group.1, data = polygon.swe.avg, family = gaussian)
summary(polygon.swe.glm) # Slope = -2.273, P = 0.249
# abline(coef(polygon.swe.glm), lty = 2)
arrows(c(2008:2016),polygon.swe.avg[1:9,2], c(2008:2016), polygon.swe.avg[1:9,2]+polygon.swe.sd[1:9,2], length=0.05, angle=90, code=3)
arrows(c(2008:2016),polygon.swe.avg[1:9,2], c(2008:2016), polygon.swe.avg[1:9,2]-polygon.swe.sd[1:9,2], length=0.05, angle=90, code=3)
axis(1, at = c(2008:2016), labels = NA, tick = TRUE) 
mtext(side=2, "SWE (mm)", adj=0.5, line=2.5)

plot(polygon.density.avg$x ~ polygon.density.avg$Group.1, type = "p",xaxt = "n", ylim = c(60,300), ylab = "",xlab = "",pch=19)
polygon.density.glm <- glm(polygon.density.avg$x ~ polygon.density.avg$Group.1, data = polygon.density.avg, family = gaussian)
summary(polygon.density.glm) # Slope = -7.393, P = 0.323
# abline(coef(polygon.density.glm), lty = 2)
arrows(c(2008:2016),polygon.density.avg[1:9,2], c(2008:2016), polygon.density.avg[1:9,2]+polygon.density.sd[1:9,2], length=0.05, angle=90, code=3)
arrows(c(2008:2016),polygon.density.avg[1:9,2], c(2008:2016), polygon.density.avg[1:9,2]-polygon.density.sd[1:9,2], length=0.05, angle=90, code=3)
axis(1, at = c(2008:2016), labels = NA, tick = TRUE)
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)

plot(polygon.htc.avg$x ~ polygon.htc.avg$Group.1, type = "p",xaxt = "n", ylim = c(0,8), ylab = "",xlab = "",pch=19)
polygon.htc.glm <- glm(polygon.htc.avg$x ~ polygon.htc.avg$Group.1, data = polygon.htc.avg, family = gaussian)
summary(polygon.htc.glm) # Slope = -0.04089, P = 0.796
# abline(coef(polygon.htc.glm), lty = 2)
arrows(c(2008:2016),polygon.htc.avg[1:9,2], c(2008:2016), polygon.htc.avg[1:9,2]+polygon.htc.sd[1:9,2], length=0.05, angle=90, code=3)
arrows(c(2008:2016),polygon.htc.avg[1:9,2], c(2008:2016), polygon.htc.avg[1:9,2]-polygon.htc.sd[1:9,2], length=0.05, angle=90, code=3)
axis(1, at = c(2008:2016), labels = c(2008:2016), tick = TRUE)
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
mtext(side=1, "Year", adj=0.5, line=2.5)

# ICE WEDGES
# Export at 7 x 7
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(0, 2, 1, 3), oma = c(4,2,1,1))

plot(wedge.depth.avg$x ~ wedge.depth.avg$Group.1, type = "p",xaxt = "n", ylim = c(0,60), ylab = "",xlab = "",pch=19)
wedge.depth.glm <- glm(wedge.depth.avg$x ~ wedge.depth.avg$Group.1, data = wedge.depth.avg, family = gaussian)
summary(wedge.depth.glm) # Slope = 2.724, P = 0.225
# abline(coef(wedge.depth.glm), lty = 2)
arrows(c(2008:2016),wedge.depth.avg[1:9,2], c(2008:2016), wedge.depth.avg[1:9,2]+wedge.depth.sd[1:9,2], length=0.05, angle=90, code=3)
arrows(c(2008:2016),wedge.depth.avg[1:9,2], c(2008:2016), wedge.depth.avg[1:9,2]-wedge.depth.sd[1:9,2], length=0.05, angle=90, code=3)
axis(1, at = c(2008:2016), labels = NA, tick = TRUE)
mtext(side=2, "Depth (cm)", adj=0.5, line=2.5)

plot(wedge.swe.avg$x ~ wedge.swe.avg$Group.1, type = "p",xaxt = "n", ylim = c(20,140), ylab = "",xlab = "",pch=19)
wedge.swe.glm <- glm(wedge.swe.avg$x ~ wedge.swe.avg$Group.1, data = wedge.swe.avg, family = gaussian)
summary(wedge.swe.glm) # Slope = 5.394, P = 0.240
# abline(coef(wedge.swe.glm), lty = 2)
arrows(c(2008:2016),wedge.swe.avg[1:9,2], c(2008:2016), wedge.swe.avg[1:9,2]+wedge.swe.sd[1:9,2], length=0.05, angle=90, code=3)
arrows(c(2008:2016),wedge.swe.avg[1:9,2], c(2008:2016), wedge.swe.avg[1:9,2]-wedge.swe.sd[1:9,2], length=0.05, angle=90, code=3)
axis(1, at = c(2008:2016), labels = NA, tick = TRUE)
mtext(side=2, "SWE (mm)", adj=0.5, line=2.5)

plot(wedge.density.avg$x ~ wedge.density.avg$Group.1, type = "p",xaxt = "n", ylim = c(140,360), ylab = "",xlab = "",pch=19)
wedge.density.glm <- glm(wedge.density.avg$x ~ wedge.density.avg$Group.1, data = wedge.density.avg, family = gaussian)
summary(wedge.density.glm) # Slope = -5.291, P = 0.645
# abline(coef(wedge.density.glm), lty = 2)
arrows(c(2008:2016),wedge.density.avg[1:9,2], c(2008:2016), wedge.density.avg[1:9,2]+wedge.density.sd[1:9,2], length=0.05, angle=90, code=3)
arrows(c(2008:2016),wedge.density.avg[1:9,2], c(2008:2016), wedge.density.avg[1:9,2]-wedge.density.sd[1:9,2], length=0.05, angle=90, code=3)
axis(1, at = c(2008:2016), labels = NA, tick = TRUE)
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)

plot(wedge.htc.avg$x ~ wedge.htc.avg$Group.1, type = "p",xaxt = "n", ylim = c(0,5), ylab = "",xlab = "",pch=19)
wedge.htc.glm <- glm(wedge.htc.avg$x ~ wedge.htc.avg$Group.1, data = wedge.htc.avg, family = gaussian)
summary(wedge.htc.glm) # Slope = -0.2582, P = 0.149
# abline(coef(wedge.htc.glm), lty = 2)
arrows(c(2008:2016),wedge.htc.avg[1:9,2], c(2008:2016), wedge.htc.avg[1:9,2]+wedge.htc.sd[1:9,2], length=0.05, angle=90, code=3)
arrows(c(2008:2016),wedge.htc.avg[1:9,2], c(2008:2016), wedge.htc.avg[1:9,2]-wedge.htc.sd[1:9,2], length=0.05, angle=90, code=3)
axis(1, at = c(2008:2016), labels = c(2008:2016), tick = TRUE)
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
mtext(side=1, "Year", adj=0.5, line=2.5)

# SHRUBS
# Export at 7 x 7
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(0, 2, 1, 3), oma = c(4,2,1,1))

plot(shrub.depth.avg$x ~ shrub.depth.avg$Group.1, type = "p",xaxt = "n", ylim = c(30,110), ylab = "",xlab = "",pch=19)
shrub.depth.glm <- glm(shrub.depth.avg$x ~ shrub.depth.avg$Group.1, data = shrub.depth.avg, family = gaussian)
summary(shrub.depth.glm) # Slope = 4.655, P = 0.141
# abline(coef(shrub.depth.glm), lty = 2)
arrows(c(2007:2016),shrub.depth.avg[1:10,2], c(2007:2016), shrub.depth.avg[1:10,2]+shrub.depth.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),shrub.depth.avg[1:10,2], c(2007:2016), shrub.depth.avg[1:10,2]-shrub.depth.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE)
mtext(side=2, "Depth (cm)", adj=0.5, line=2.5)

plot(shrub.swe.avg$x ~ shrub.swe.avg$Group.1, type = "p",xaxt = "n", ylim = c(20,340), ylab = "",xlab = "",pch=19)
shrub.swe.glm <- glm(shrub.swe.avg$x ~ shrub.swe.avg$Group.1, data = shrub.swe.avg, family = gaussian)
summary(shrub.swe.glm) # Slope = 14.693, P = 0.127
# abline(coef(shrub.swe.glm), lty = 2)
arrows(c(2007:2016),shrub.swe.avg[1:10,2], c(2007:2016), shrub.swe.avg[1:10,2]+shrub.swe.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),shrub.swe.avg[1:10,2], c(2007:2016), shrub.swe.avg[1:10,2]-shrub.swe.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE)
mtext(side=2, "SWE (mm)", adj=0.5, line=2.5)

plot(shrub.density.avg$x ~ shrub.density.avg$Group.1, type = "p",xaxt = "n", ylim = c(120,360), ylab = "",xlab = "",pch=19)
shrub.density.glm <- glm(shrub.density.avg$x ~ shrub.density.avg$Group.1, data = shrub.density.avg, family = gaussian)
summary(shrub.density.glm) # Slope = 4.215, P = 0.517
# abline(coef(shrub.density.glm), lty = 2)
arrows(c(2007:2016),shrub.density.avg[1:10,2], c(2007:2016), shrub.density.avg[1:10,2]+shrub.density.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),shrub.density.avg[1:10,2], c(2007:2016), shrub.density.avg[1:10,2]-shrub.density.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE)
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)

plot(shrub.htc.avg$x ~ shrub.htc.avg$Group.1, type = "p",xaxt = "n", ylim = c(0,0.6), ylab = "",xlab = "",pch=19)
shrub.htc.glm <- glm(shrub.htc.avg$x ~ shrub.htc.avg$Group.1, data = shrub.htc.avg, family = gaussian)
summary(shrub.htc.glm) # Slope = -0.02403, P = 0.202
# abline(coef(shrub.htc.glm), lty = 2)
arrows(c(2007:2016),shrub.htc.avg[1:10,2], c(2007:2016), shrub.htc.avg[1:10,2]+shrub.htc.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),shrub.htc.avg[1:10,2], c(2007:2016), shrub.htc.avg[1:10,2]-shrub.htc.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = c(2007:2016), tick = TRUE)
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
mtext(side=1, "Year", adj=0.5, line=2.5)

#####################
#******Level 2******#
#####################

# Subset by site
bcr <- subset(park.snow, location == "Broad: Coastal Beach Ridge") 
bcf <- subset(park.snow, location == "Broad: Sedge Fen")
brf <- subset(park.snow, location == "Broad: Riparian Forest")
bsf <- subset(park.snow, location == "Broad: Spruce Forest")
bsh <- subset(park.snow, location == "Broad: Shrub")
ocr <- subset(park.snow, location == "Owl: Coastal Beach Ridge")
osh <- subset(park.snow, location == "Owl: Coastal Fen")
osf <- subset(park.snow, location == "Owl: Spruce Forest")
riw <- subset(park.snow, location == "Roberge Lake: Peat Plateau - Ice Wedge")
rpp <- subset(park.snow, location == "Roberge Lake: Peat Plateau - Polygon Center")
rti <- subset(park.snow, location == "Roberge Lake: Tree Island")
mlk <- subset(park.snow, location == "Mary Lake: Forest")

aov.bbr <- aov(swe ~ year, data = park.snow[park.snow$site=="BBR",])
summary(aov.bbr)
TukeyHSD(aov.bbr)
bbr.pairs <- glht(aov.bbr, linfct = mcp(year = "Tukey"))
bbr.cld <- cld(bbr.pairs)
bbr.letters <- bbr.cld$mcletters$Letters

x1 <- factor(park.snow$year, levels = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016"))
bp1 <- boxplot(swe ~ year*site, data = park.snow, col = "darkgreen")
# mtext(expression(italic("g")[min] ~ (m ~ s^{-1} ~ 10^{-5})),side=2, adj=0.5, line=2.5)
# mtext(side=1,"Year", adj=0.5, line=2.5)
text(0.5,0.00018,"(a) Forest", adj = c(0,0))
for.y <- t(aggregate(forest2$gmin, by=list(x1), FUN=max, na.rm=TRUE, na.action=NULL)[2])
text(x=c(1:3),y=for.y[1:3]+0.00002,for.letters[1:3])
text(6,0.00008,"a")
text(7,0.0001827,"b")





# Run t-tests comparing 2006-2010 to 2011-2015, when there are enough data available

with(bcr, t.test(depth ~ period), paired = T) # p-value < 2.2e-16***
with(bcr, t.test(density ~ period), paired = T) # p-value < 2.2e-16***
with(bcr, t.test(swe ~ period), paired = T) # p-value < 2.2e-16***
with(bcr, t.test(htc ~ period), paired = T) # p-value = 0.004369**

with(bcf, t.test(depth ~ period), paired = T) # p-value = 1.807e-14***
with(bcf, t.test(density ~ period), paired = T) # p-value = 0.7715
with(bcf, t.test(swe ~ period), paired = T) # p-value = 6.558e-12***
with(bcf, t.test(htc ~ period), paired = T) # p-value = 8.802e-10***

with(brf, t.test(depth ~ period), paired = T) # p-value < 2.2e-16***
with(brf, t.test(density ~ period), paired = T) # p-value = 1.694e-07***
with(brf, t.test(swe ~ period), paired = T) # p-value < 2.2e-16***
with(brf, t.test(htc ~ period), paired = T) # p-value = 0.1395

## Broad River shrub only has data from 2012 onwards - can run t-test
# with(bsh, t.test(depth ~ period), paired = T) 
# with(bsh, t.test(density ~ period), paired = T) 
# with(bsh, t.test(swe ~ period), paired = T) 
# with(bsh, t.test(htc ~ period), paired = T) 

with(bsf, t.test(depth ~ period), paired = T) # p-value = 7.838e-07***
with(bsf, t.test(density ~ period), paired = T) # p-value = 0.001266**
with(bsf, t.test(swe ~ period), paired = T) # p-value = 6.021e-08***
with(bsf, t.test(htc ~ period), paired = T) # p-value = 0.3335

with(ocr, t.test(depth ~ period), paired = T) # p-value < 2.2e-16***
with(ocr, t.test(density ~ period), paired = T) # p-value = 0.8323
with(ocr, t.test(swe ~ period), paired = T) # p-value < 2.2e-16***
with(ocr, t.test(htc ~ period), paired = T) # p-value < 2.2e-16***

with(osh, t.test(depth ~ period), paired = T) # p-value = 7.388e-13***
with(osh, t.test(density ~ period), paired = T) # p-value = 0.9797
with(osh, t.test(swe ~ period), paired = T) # p-value = 6.99e-07***
with(osh, t.test(htc ~ period), paired = T) # p-value = 0.003927***

with(osf, t.test(depth ~ period), paired = T) # p-value = 0.002862***
with(osf, t.test(density ~ period), paired = T) # p-value = 0.497
with(osf, t.test(swe ~ period), paired = T) # p-value = 0.1016
with(osf, t.test(htc ~ period), paired = T) # p-value = 0.002875***

with(riw, t.test(depth ~ period), paired = T) # p-value = 0.0008406***
with(riw, t.test(density ~ period), paired = T) # p-value = 0.5263
with(riw, t.test(swe ~ period), paired = T) # p-value = 2.885e-05***
with(riw, t.test(htc ~ period), paired = T) # p-value = 0.1696

with(rpp, t.test(depth ~ period), paired = T) # p-value = 2.056e-08***
with(rpp, t.test(density ~ period), paired = T) # p-value = 2.699e-05***
with(rpp, t.test(swe ~ period), paired = T) # p-value = 7.662e-10***
with(rpp, t.test(htc ~ period), paired = T) # p-value = 0.005405**

with(rti, t.test(depth ~ period), paired = T) # p-value = 7.853e-05***
with(rti, t.test(density ~ period), paired = T) # p-value = 0.02468*
with(rti, t.test(swe ~ period), paired = T) # p-value = 0.0001148***
with(rti, t.test(htc ~ period), paired = T) # p-value = 0.6759

with(mlk, t.test(depth ~ period), paired = T) # p-value = 0.2898
with(mlk, t.test(density ~ period), paired = T) # p-value = 1.637e-05***
with(mlk, t.test(swe ~ period), paired = T) # p-value = 0.0007409***
with(mlk, t.test(htc ~ period), paired = T) # p-value = 0.0001087***

# Calculate means and standard deviations for each group
bcr.depth.avg <- aggregate(bcr$depth, by = list(bcr$year), FUN = mean, na.rm=TRUE, na.action=NULL)
bcr.depth.sd <- aggregate(bcr$depth, by = list(bcr$year), FUN = sd, na.rm=TRUE)
bcr.swe.avg <- aggregate(bcr$swe, by = list(bcr$year), FUN = mean, na.rm=TRUE, na.action=NULL)
bcr.swe.sd <- aggregate(bcr$swe, by = list(bcr$year), FUN = sd, na.rm=TRUE)
bcr.density.avg <- aggregate(bcr$density, by = list(bcr$year), FUN = mean, na.rm=TRUE, na.action=NULL)
bcr.density.sd <- aggregate(bcr$density, by = list(bcr$year), FUN = sd, na.rm=TRUE)
bcr.htc.avg <- aggregate(bcr$htc, by = list(bcr$year), FUN = mean, na.rm=TRUE, na.action=NULL)
bcr.htc.sd <- aggregate(bcr$htc, by = list(bcr$year), FUN = sd, na.rm=TRUE)

bcf.depth.avg <- aggregate(bcf$depth, by = list(bcf$year), FUN = mean, na.rm=TRUE, na.action=NULL)
bcf.depth.sd <- aggregate(bcf$depth, by = list(bcf$year), FUN = sd, na.rm=TRUE)
bcf.swe.avg <- aggregate(bcf$swe, by = list(bcf$year), FUN = mean, na.rm=TRUE, na.action=NULL)
bcf.swe.sd <- aggregate(bcf$swe, by = list(bcf$year), FUN = sd, na.rm=TRUE)
bcf.density.avg <- aggregate(bcf$density, by = list(bcf$year), FUN = mean, na.rm=TRUE, na.action=NULL)
bcf.density.sd <- aggregate(bcf$density, by = list(bcf$year), FUN = sd, na.rm=TRUE)
bcf.htc.avg <- aggregate(bcf$htc, by = list(bcf$year), FUN = mean, na.rm=TRUE, na.action=NULL)
bcf.htc.sd <- aggregate(bcf$htc, by = list(bcf$year), FUN = sd, na.rm=TRUE)

brf.depth.avg <- aggregate(brf$depth, by = list(brf$year), FUN = mean, na.rm=TRUE, na.action=NULL)
brf.depth.sd <- aggregate(brf$depth, by = list(brf$year), FUN = sd, na.rm=TRUE)
brf.swe.avg <- aggregate(brf$swe, by = list(brf$year), FUN = mean, na.rm=TRUE, na.action=NULL)
brf.swe.sd <- aggregate(brf$swe, by = list(brf$year), FUN = sd, na.rm=TRUE)
brf.density.avg <- aggregate(brf$density, by = list(brf$year), FUN = mean, na.rm=TRUE, na.action=NULL)
brf.density.sd <- aggregate(brf$density, by = list(brf$year), FUN = sd, na.rm=TRUE)
brf.htc.avg <- aggregate(brf$htc, by = list(brf$year), FUN = mean, na.rm=TRUE, na.action=NULL)
brf.htc.sd <- aggregate(brf$htc, by = list(brf$year), FUN = sd, na.rm=TRUE)

bsh.depth.avg <- aggregate(bsh$depth, by = list(bsh$year), FUN = mean, na.rm=TRUE, na.action=NULL)
bsh.depth.sd <- aggregate(bsh$depth, by = list(bsh$year), FUN = sd, na.rm=TRUE)
bsh.swe.avg <- aggregate(bsh$swe, by = list(bsh$year), FUN = mean, na.rm=TRUE, na.action=NULL)
bsh.swe.sd <- aggregate(bsh$swe, by = list(bsh$year), FUN = sd, na.rm=TRUE)
bsh.density.avg <- aggregate(bsh$density, by = list(bsh$year), FUN = mean, na.rm=TRUE, na.action=NULL)
bsh.density.sd <- aggregate(bsh$density, by = list(bsh$year), FUN = sd, na.rm=TRUE)
bsh.htc.avg <- aggregate(bsh$htc, by = list(bsh$year), FUN = mean, na.rm=TRUE, na.action=NULL)
bsh.htc.sd <- aggregate(bsh$htc, by = list(bsh$year), FUN = sd, na.rm=TRUE)

bsf.depth.avg <- aggregate(bsf$depth, by = list(bsf$year), FUN = mean, na.rm=TRUE, na.action=NULL)
bsf.depth.sd <- aggregate(bsf$depth, by = list(bsf$year), FUN = sd, na.rm=TRUE)
bsf.swe.avg <- aggregate(bsf$swe, by = list(bsf$year), FUN = mean, na.rm=TRUE, na.action=NULL)
bsf.swe.sd <- aggregate(bsf$swe, by = list(bsf$year), FUN = sd, na.rm=TRUE)
bsf.density.avg <- aggregate(bsf$density, by = list(bsf$year), FUN = mean, na.rm=TRUE, na.action=NULL)
bsf.density.sd <- aggregate(bsf$density, by = list(bsf$year), FUN = sd, na.rm=TRUE)
bsf.htc.avg <- aggregate(bsf$htc, by = list(bsf$year), FUN = mean, na.rm=TRUE, na.action=NULL)
bsf.htc.sd <- aggregate(bsf$htc, by = list(bsf$year), FUN = sd, na.rm=TRUE)

ocr.depth.avg <- aggregate(ocr$depth, by = list(ocr$year), FUN = mean, na.rm=TRUE, na.action=NULL)
ocr.depth.sd <- aggregate(ocr$depth, by = list(ocr$year), FUN = sd, na.rm=TRUE)
ocr.swe.avg <- aggregate(ocr$swe, by = list(ocr$year), FUN = mean, na.rm=TRUE, na.action=NULL)
ocr.swe.sd <- aggregate(ocr$swe, by = list(ocr$year), FUN = sd, na.rm=TRUE)
ocr.density.avg <- aggregate(ocr$density, by = list(ocr$year), FUN = mean, na.rm=TRUE, na.action=NULL)
ocr.density.sd <- aggregate(ocr$density, by = list(ocr$year), FUN = sd, na.rm=TRUE)
ocr.htc.avg <- aggregate(ocr$htc, by = list(ocr$year), FUN = mean, na.rm=TRUE, na.action=NULL)
ocr.htc.sd <- aggregate(ocr$htc, by = list(ocr$year), FUN = sd, na.rm=TRUE)

osh.depth.avg <- aggregate(osh$depth, by = list(osh$year), FUN = mean, na.rm=TRUE, na.action=NULL)
osh.depth.sd <- aggregate(osh$depth, by = list(osh$year), FUN = sd, na.rm=TRUE)
osh.swe.avg <- aggregate(osh$swe, by = list(osh$year), FUN = mean, na.rm=TRUE, na.action=NULL)
osh.swe.sd <- aggregate(osh$swe, by = list(osh$year), FUN = sd, na.rm=TRUE)
osh.density.avg <- aggregate(osh$density, by = list(osh$year), FUN = mean, na.rm=TRUE, na.action=NULL)
osh.density.sd <- aggregate(osh$density, by = list(osh$year), FUN = sd, na.rm=TRUE)
osh.htc.avg <- aggregate(osh$htc, by = list(osh$year), FUN = mean, na.rm=TRUE, na.action=NULL)
osh.htc.sd <- aggregate(osh$htc, by = list(osh$year), FUN = sd, na.rm=TRUE)

osf.depth.avg <- aggregate(osf$depth, by = list(osf$year), FUN = mean, na.rm=TRUE, na.action=NULL)
osf.depth.sd <- aggregate(osf$depth, by = list(osf$year), FUN = sd, na.rm=TRUE)
osf.swe.avg <- aggregate(osf$swe, by = list(osf$year), FUN = mean, na.rm=TRUE, na.action=NULL)
osf.swe.sd <- aggregate(osf$swe, by = list(osf$year), FUN = sd, na.rm=TRUE)
osf.density.avg <- aggregate(osf$density, by = list(osf$year), FUN = mean, na.rm=TRUE, na.action=NULL)
osf.density.sd <- aggregate(osf$density, by = list(osf$year), FUN = sd, na.rm=TRUE)
osf.htc.avg <- aggregate(osf$htc, by = list(osf$year), FUN = mean, na.rm=TRUE, na.action=NULL)
osf.htc.sd <- aggregate(osf$htc, by = list(osf$year), FUN = sd, na.rm=TRUE)

riw.depth.avg <- aggregate(riw$depth, by = list(riw$year), FUN = mean, na.rm=TRUE, na.action=NULL)
riw.depth.sd <- aggregate(riw$depth, by = list(riw$year), FUN = sd, na.rm=TRUE)
riw.swe.avg <- aggregate(riw$swe, by = list(riw$year), FUN = mean, na.rm=TRUE, na.action=NULL)
riw.swe.sd <- aggregate(riw$swe, by = list(riw$year), FUN = sd, na.rm=TRUE)
riw.density.avg <- aggregate(riw$density, by = list(riw$year), FUN = mean, na.rm=TRUE, na.action=NULL)
riw.density.sd <- aggregate(riw$density, by = list(riw$year), FUN = sd, na.rm=TRUE)
riw.htc.avg <- aggregate(riw$htc, by = list(riw$year), FUN = mean, na.rm=TRUE, na.action=NULL)
riw.htc.sd <- aggregate(riw$htc, by = list(riw$year), FUN = sd, na.rm=TRUE)

rpp.depth.avg <- aggregate(rpp$depth, by = list(rpp$year), FUN = mean, na.rm=TRUE, na.action=NULL)
rpp.depth.sd <- aggregate(rpp$depth, by = list(rpp$year), FUN = sd, na.rm=TRUE)
rpp.swe.avg <- aggregate(rpp$swe, by = list(rpp$year), FUN = mean, na.rm=TRUE, na.action=NULL)
rpp.swe.sd <- aggregate(rpp$swe, by = list(rpp$year), FUN = sd, na.rm=TRUE)
rpp.density.avg <- aggregate(rpp$density, by = list(rpp$year), FUN = mean, na.rm=TRUE, na.action=NULL)
rpp.density.sd <- aggregate(rpp$density, by = list(rpp$year), FUN = sd, na.rm=TRUE)
rpp.htc.avg <- aggregate(rpp$htc, by = list(rpp$year), FUN = mean, na.rm=TRUE, na.action=NULL)
rpp.htc.sd <- aggregate(rpp$htc, by = list(rpp$year), FUN = sd, na.rm=TRUE)

rti.depth.avg <- aggregate(rti$depth, by = list(rti$year), FUN = mean, na.rm=TRUE, na.action=NULL)
rti.depth.sd <- aggregate(rti$depth, by = list(rti$year), FUN = sd, na.rm=TRUE)
rti.swe.avg <- aggregate(rti$swe, by = list(rti$year), FUN = mean, na.rm=TRUE, na.action=NULL)
rti.swe.sd <- aggregate(rti$swe, by = list(rti$year), FUN = sd, na.rm=TRUE)
rti.density.avg <- aggregate(rti$density, by = list(rti$year), FUN = mean, na.rm=TRUE, na.action=NULL)
rti.density.sd <- aggregate(rti$density, by = list(rti$year), FUN = sd, na.rm=TRUE)
rti.htc.avg <- aggregate(rti$htc, by = list(rti$year), FUN = mean, na.rm=TRUE, na.action=NULL)
rti.htc.sd <- aggregate(rti$htc, by = list(rti$year), FUN = sd, na.rm=TRUE)

mlk.depth.avg <- aggregate(mlk$depth, by = list(mlk$year), FUN = mean, na.rm=TRUE, na.action=NULL)
mlk.depth.sd <- aggregate(mlk$depth, by = list(mlk$year), FUN = sd, na.rm=TRUE)
mlk.swe.avg <- aggregate(mlk$swe, by = list(mlk$year), FUN = mean, na.rm=TRUE, na.action=NULL)
mlk.swe.sd <- aggregate(mlk$swe, by = list(mlk$year), FUN = sd, na.rm=TRUE)
mlk.density.avg <- aggregate(mlk$density, by = list(mlk$year), FUN = mean, na.rm=TRUE, na.action=NULL)
mlk.density.sd <- aggregate(mlk$density, by = list(mlk$year), FUN = sd, na.rm=TRUE)
mlk.htc.avg <- aggregate(mlk$htc, by = list(mlk$year), FUN = mean, na.rm=TRUE, na.action=NULL)
mlk.htc.sd <- aggregate(mlk$htc, by = list(mlk$year), FUN = sd, na.rm=TRUE)

## Plot the four snow metrics versus time for each site 
# Broad River: Coastal Beach Ridge
# Export at 7 x 7
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(0, 2, 1, 3), oma = c(4,2,1,1))

plot(bcr.depth.avg$x ~ bcr.depth.avg$Group.1, type = "p", xaxt = "n", ylim = c(0,40), ylab = "",xlab = "",pch=19)
bcr.depth.glm <- glm(bcr.depth.avg$x ~ bcr.depth.avg$Group.1, data = bcr.depth.avg, family = gaussian)
summary(bcr.depth.glm) # Slope = -1.2188, P = 0.0971
# abline(coef(bcr.depth.glm), lty = 2)
arrows(c(2007:2016),bcr.depth.avg[1:10,2], c(2007:2016), bcr.depth.avg[1:10,2]+bcr.depth.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),bcr.depth.avg[1:10,2], c(2007:2016), bcr.depth.avg[1:10,2]-bcr.depth.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE)   
mtext(side=2, "Depth (cm)", adj=0.5, line=2.5)

plot(bcr.swe.avg$x ~ bcr.swe.avg$Group.1, type = "p",xaxt = "n", ylim = c(0,120), ylab = "",xlab = "",pch=19)
bcr.swe.glm <- glm(bcr.swe.avg$x ~ bcr.swe.avg$Group.1, data = bcr.swe.avg, family = gaussian)
summary(bcr.swe.glm) # Slope = -4.475, P = 0.0493*
abline(coef(bcr.swe.glm), lty = 2)
arrows(c(2007:2016),bcr.swe.avg[1:10,2], c(2007:2016), bcr.swe.avg[1:10,2]+bcr.swe.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),bcr.swe.avg[1:10,2], c(2007:2016), bcr.swe.avg[1:10,2]-bcr.swe.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE)
mtext(side=2, "SWE (mm)", adj=0.5, line=2.5)

plot(bcr.density.avg$x ~ bcr.density.avg$Group.1, type = "p",xaxt = "n", ylim = c(160,400), ylab = "",xlab = "",pch=19)
bcr.density.glm <- glm(bcr.density.avg$x ~ bcr.density.avg$Group.1, data = bcr.density.avg, family = gaussian)
summary(bcr.density.glm) # Slope = -11.075, P = 0.00610**
abline(coef(bcr.density.glm), lty = 2)
arrows(c(2007:2016),bcr.density.avg[1:10,2], c(2007:2016), bcr.density.avg[1:10,2]+bcr.density.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),bcr.density.avg[1:10,2], c(2007:2016), bcr.density.avg[1:10,2]-bcr.density.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE) 
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)

plot(bcr.htc.avg$x ~ bcr.htc.avg$Group.1, type = "p",xaxt = "n", ylim = c(0,5), ylab = "",xlab = "",pch=19)
bcr.htc.glm <- glm(bcr.htc.avg$x ~ bcr.htc.avg$Group.1, data = bcr.htc.avg, family = gaussian)
summary(bcr.htc.glm) # Slope = 0.03228, P = 0.753
# abline(coef(bcr.htc.glm), lty = 2)
arrows(c(2007:2016),bcr.htc.avg[1:10,2], c(2007:2016), bcr.htc.avg[1:10,2]+bcr.htc.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),bcr.htc.avg[1:10,2], c(2007:2016), bcr.htc.avg[1:10,2]-bcr.htc.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = c(2007:2016), tick = TRUE)   
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
mtext(side=1, "Year", adj=0.5, line=2.5)

# Broad River: Coastal Fen
# Export at 7 x 7
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(0, 2, 1, 3), oma = c(4,2,1,1))

plot(bcf.depth.avg$x ~ bcf.depth.avg$Group.1, type = "p", xaxt = "n", ylim = c(0,60), ylab = "",xlab = "",pch=19)
bcf.depth.glm <- glm(bcf.depth.avg$x ~ bcf.depth.avg$Group.1, data = bcf.depth.avg, family = gaussian)
summary(bcf.depth.glm) # Slope = -3.277, P = 0.0439*
abline(coef(bcf.depth.glm), lty = 2)
arrows(c(2007:2016),bcf.depth.avg[1:10,2], c(2007:2016), bcf.depth.avg[1:10,2]+bcf.depth.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),bcf.depth.avg[1:10,2], c(2007:2016), bcf.depth.avg[1:10,2]-bcf.depth.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE)   
mtext(side=2, "Depth (cm)", adj=0.5, line=2.5)

plot(bcf.swe.avg$x ~ bcf.swe.avg$Group.1, type = "p",xaxt = "n", ylim = c(0,200), ylab = "",xlab = "",pch=19)
bcf.swe.glm <- glm(bcf.swe.avg$x ~ bcf.swe.avg$Group.1, data = bcf.swe.avg, family = gaussian)
summary(bcf.swe.glm) # Slope = -10.866, P = 0.0300*
abline(coef(bcf.swe.glm), lty = 2)
arrows(c(2007:2016),bcf.swe.avg[1:10,2], c(2007:2016), bcf.swe.avg[1:10,2]+bcf.swe.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),bcf.swe.avg[1:10,2], c(2007:2016), bcf.swe.avg[1:10,2]-bcf.swe.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE)
mtext(side=2, "SWE (mm)", adj=0.5, line=2.5)

plot(bcf.density.avg$x ~ bcf.density.avg$Group.1, type = "p",xaxt = "n", ylim = c(200,400), ylab = "",xlab = "",pch=19)
bcf.density.glm <- glm(bcf.density.avg$x ~ bcf.density.avg$Group.1, data = bcf.density.avg, family = gaussian)
summary(bcf.density.glm) # Slope = -2.270, P = 0.568
# abline(coef(bcf.density.glm), lty = 2)
arrows(c(2007:2016),bcf.density.avg[1:10,2], c(2007:2016), bcf.density.avg[1:10,2]+bcf.density.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),bcf.density.avg[1:10,2], c(2007:2016), bcf.density.avg[1:10,2]-bcf.density.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE) 
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)

plot(bcf.htc.avg$x ~ bcf.htc.avg$Group.1, type = "p",xaxt = "n", ylim = c(0,4), ylab = "",xlab = "",pch=19)
bcf.htc.glm <- glm(bcf.htc.avg$x ~ bcf.htc.avg$Group.1, data = bcf.htc.avg, family = gaussian)
summary(bcf.htc.glm) # Slope = 0.1337, P = 0.274
# abline(coef(bcf.htc.glm), lty = 2)
arrows(c(2007:2016),bcf.htc.avg[1:10,2], c(2007:2016), bcf.htc.avg[1:10,2]+bcf.htc.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),bcf.htc.avg[1:10,2], c(2007:2016), bcf.htc.avg[1:10,2]-bcf.htc.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = c(2007:2016), tick = TRUE)   
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
mtext(side=1, "Year", adj=0.5, line=2.5)

# Broad River: Riparian Forest
# Export at 7 x 7
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(0, 2, 1, 3), oma = c(4,2,1,1))

plot(brf.depth.avg$x ~ brf.depth.avg$Group.1, type = "p", xaxt = "n", ylim = c(40,150), ylab = "",xlab = "",pch=19)
brf.depth.glm <- glm(brf.depth.avg$x ~ brf.depth.avg$Group.1, data = brf.depth.avg, family = gaussian)
summary(brf.depth.glm) # Slope = 0.6353, P = 0.882
# abline(coef(brf.depth.glm), lty = 2)
arrows(c(2007:2016),brf.depth.avg[1:10,2], c(2007:2016), brf.depth.avg[1:10,2]+brf.depth.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),brf.depth.avg[1:10,2], c(2007:2016), brf.depth.avg[1:10,2]-brf.depth.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE)   
mtext(side=2, "Depth (cm)", adj=0.5, line=2.5)

plot(brf.swe.avg$x ~ brf.swe.avg$Group.1, type = "p",xaxt = "n", ylim = c(60,430), ylab = "",xlab = "",pch=19)
brf.swe.glm <- glm(brf.swe.avg$x ~ brf.swe.avg$Group.1, data = brf.swe.avg, family = gaussian)
summary(brf.swe.glm) # Slope = 0.9399, P = 0.951
# abline(coef(brf.swe.glm), lty = 2)
arrows(c(2007:2016),brf.swe.avg[1:10,2], c(2007:2016), brf.swe.avg[1:10,2]+brf.swe.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),brf.swe.avg[1:10,2], c(2007:2016), brf.swe.avg[1:10,2]-brf.swe.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE)
mtext(side=2, "SWE (mm)", adj=0.5, line=2.5)

plot(brf.density.avg$x ~ brf.density.avg$Group.1, type = "p",xaxt = "n", ylim = c(130,320), ylab = "",xlab = "",pch=19)
brf.density.glm <- glm(brf.density.avg$x ~ brf.density.avg$Group.1, data = brf.density.avg, family = gaussian)
summary(brf.density.glm) # Slope = -3.429, P = 0.612
# abline(coef(brf.density.glm), lty = 2)
arrows(c(2007:2016),brf.density.avg[1:10,2], c(2007:2016), brf.density.avg[1:10,2]+brf.density.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),brf.density.avg[1:10,2], c(2007:2016), brf.density.avg[1:10,2]-brf.density.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE) 
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)

plot(brf.htc.avg$x ~ brf.htc.avg$Group.1, type = "p",xaxt = "n", ylim = c(0.05,0.35), ylab = "",xlab = "",pch=19)
brf.htc.glm <- glm(brf.htc.avg$x ~ brf.htc.avg$Group.1, data = brf.htc.avg, family = gaussian)
summary(brf.htc.glm) # Slope = -0.006754, P = 0.166
# abline(coef(brf.htc.glm), lty = 2)
arrows(c(2007:2016),brf.htc.avg[1:10,2], c(2007:2016), brf.htc.avg[1:10,2]+brf.htc.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),brf.htc.avg[1:10,2], c(2007:2016), brf.htc.avg[1:10,2]-brf.htc.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = c(2007:2016), tick = TRUE)   
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
mtext(side=1, "Year", adj=0.5, line=2.5)

# Broad River: Shrub Fen
# Export at 7 x 7
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(0, 2, 1, 3), oma = c(4,2,1,1))

plot(bsh.depth.avg$x ~ bsh.depth.avg$Group.1, type = "p", xaxt = "n", ylim = c(60,110), ylab = "",xlab = "",pch=19)
bsh.depth.glm <- glm(bsh.depth.avg$x ~ bsh.depth.avg$Group.1, data = bsh.depth.avg, family = gaussian)
summary(bsh.depth.glm) # Slope = 0.3515, P = 0.935
# abline(coef(bsh.depth.glm), lty = 2)
arrows(c(2007:2016),bsh.depth.avg[1:10,2], c(2007:2016), bsh.depth.avg[1:10,2]+bsh.depth.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),bsh.depth.avg[1:10,2], c(2007:2016), bsh.depth.avg[1:10,2]-bsh.depth.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE)   
mtext(side=2, "Depth (cm)", adj=0.5, line=2.5)

plot(bsh.swe.avg$x ~ bsh.swe.avg$Group.1, type = "p",xaxt = "n", ylim = c(100,350), ylab = "",xlab = "",pch=19)
bsh.swe.glm <- glm(bsh.swe.avg$x ~ bsh.swe.avg$Group.1, data = bsh.swe.avg, family = gaussian)
summary(bsh.swe.glm) # Slope = -15.74, P = 0.391
# abline(coef(bsh.swe.glm), lty = 2)
arrows(c(2007:2016),bsh.swe.avg[1:10,2], c(2007:2016), bsh.swe.avg[1:10,2]+bsh.swe.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),bsh.swe.avg[1:10,2], c(2007:2016), bsh.swe.avg[1:10,2]-bsh.swe.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE)
mtext(side=2, "SWE (mm)", adj=0.5, line=2.5)

plot(bsh.density.avg$x ~ bsh.density.avg$Group.1, type = "p",xaxt = "n", ylim = c(150,380), ylab = "",xlab = "",pch=19)
bsh.density.glm <- glm(bsh.density.avg$x ~ bsh.density.avg$Group.1, data = bsh.density.avg, family = gaussian)
summary(bsh.density.glm) # Slope = -22.48, P = 0.171
# abline(coef(bsh.density.glm), lty = 2)
arrows(c(2007:2016),bsh.density.avg[1:10,2], c(2007:2016), bsh.density.avg[1:10,2]+bsh.density.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),bsh.density.avg[1:10,2], c(2007:2016), bsh.density.avg[1:10,2]-bsh.density.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE) 
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)

plot(bsh.htc.avg$x ~ bsh.htc.avg$Group.1, type = "p",xaxt = "n", ylim = c(0.1,0.55), ylab = "",xlab = "",pch=19)
bsh.htc.glm <- glm(bsh.htc.avg$x ~ bsh.htc.avg$Group.1, data = bsh.htc.avg, family = gaussian)
summary(bsh.htc.glm) # Slope = -0.04735, P = 0.216
# abline(coef(bsh.htc.glm), lty = 2)
arrows(c(2007:2016),bsh.htc.avg[1:10,2], c(2007:2016), bsh.htc.avg[1:10,2]+bsh.htc.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),bsh.htc.avg[1:10,2], c(2007:2016), bsh.htc.avg[1:10,2]-bsh.htc.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = c(2007:2016), tick = TRUE)   
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
mtext(side=1, "Year", adj=0.5, line=2.5)

# Broad River: Spruce Forest
# Export at 7 x 7
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(0, 2, 1, 3), oma = c(4,2,1,1))

plot(bsf.depth.avg$x ~ bsf.depth.avg$Group.1, type = "p", xaxt = "n", ylim = c(40,80), ylab = "",xlab = "",pch=19)
bsf.depth.glm <- glm(bsf.depth.avg$x ~ bsf.depth.avg$Group.1, data = bsf.depth.avg, family = gaussian)
summary(bsf.depth.glm) # Slope = 0.06494, P = 0.966
# abline(coef(bsf.depth.glm), lty = 2)
arrows(c(2007:2016),bsf.depth.avg[1:10,2], c(2007:2016), bsf.depth.avg[1:10,2]+bsf.depth.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),bsf.depth.avg[1:10,2], c(2007:2016), bsf.depth.avg[1:10,2]-bsf.depth.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE)   
mtext(side=2, "Depth (cm)", adj=0.5, line=2.5)

plot(bsf.swe.avg$x ~ bsf.swe.avg$Group.1, type = "p",xaxt = "n", ylim = c(60,180), ylab = "",xlab = "",pch=19)
bsf.swe.glm <- glm(bsf.swe.avg$x ~ bsf.swe.avg$Group.1, data = bsf.swe.avg, family = gaussian)
summary(bsf.swe.glm) # Slope = -0.9304, P = 0.869
# abline(coef(bsf.swe.glm), lty = 2)
arrows(c(2007:2016),bsf.swe.avg[1:10,2], c(2007:2016), bsf.swe.avg[1:10,2]+bsf.swe.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),bsf.swe.avg[1:10,2], c(2007:2016), bsf.swe.avg[1:10,2]-bsf.swe.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE)
mtext(side=2, "SWE (mm)", adj=0.5, line=2.5)

plot(bsf.density.avg$x ~ bsf.density.avg$Group.1, type = "p",xaxt = "n", ylim = c(120,250), ylab = "",xlab = "",pch=19)
bsf.density.glm <- glm(bsf.density.avg$x ~ bsf.density.avg$Group.1, data = bsf.density.avg, family = gaussian)
summary(bsf.density.glm) # Slope = -2.506, P = 0.628
# abline(coef(bsf.density.glm), lty = 2)
arrows(c(2007:2016),bsf.density.avg[1:10,2], c(2007:2016), bsf.density.avg[1:10,2]+bsf.density.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),bsf.density.avg[1:10,2], c(2007:2016), bsf.density.avg[1:10,2]-bsf.density.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE) 
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)

plot(bsf.htc.avg$x ~ bsf.htc.avg$Group.1, type = "p",xaxt = "n", ylim = c(0.09,0.3), ylab = "",xlab = "",pch=19)
bsf.htc.glm <- glm(bsf.htc.avg$x ~ bsf.htc.avg$Group.1, data = bsf.htc.avg, family = gaussian)
summary(bsf.htc.glm) # Slope = -0.004730, P = 0.485
# abline(coef(bsf.htc.glm), lty = 2)
arrows(c(2007:2016),bsf.htc.avg[1:10,2], c(2007:2016), bsf.htc.avg[1:10,2]+bsf.htc.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),bsf.htc.avg[1:10,2], c(2007:2016), bsf.htc.avg[1:10,2]-bsf.htc.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = c(2007:2016), tick = TRUE)   
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
mtext(side=1, "Year", adj=0.5, line=2.5)

# Mary Lake: Forest
# Export at 7 x 7
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(0, 2, 1, 3), oma = c(4,2,1,1))

plot(mlk.depth.avg$x ~ mlk.depth.avg$Group.1, type = "p", xaxt = "n", ylim = c(30,100), ylab = "",xlab = "",pch=19)
mlk.depth.glm <- glm(mlk.depth.avg$x ~ mlk.depth.avg$Group.1, data = mlk.depth.avg, family = gaussian)
summary(mlk.depth.glm) # Slope = 0.2728, P = 0.815
# abline(coef(mlk.depth.glm), lty = 2)
arrows(c(2006:2016),mlk.depth.avg[1:11,2], c(2006:2016), mlk.depth.avg[1:11,2]+mlk.depth.sd[1:11,2], length=0.05, angle=90, code=3)
arrows(c(2006:2016),mlk.depth.avg[1:11,2], c(2006:2016), mlk.depth.avg[1:11,2]-mlk.depth.sd[1:11,2], length=0.05, angle=90, code=3)
axis(1, at = c(2006:2016), labels = NA, tick = TRUE)   
mtext(side=2, "Depth (cm)", adj=0.5, line=2.5)

plot(mlk.swe.avg$x ~ mlk.swe.avg$Group.1, type = "p",xaxt = "n", ylim = c(60,280), ylab = "",xlab = "",pch=19)
mlk.swe.glm <- glm(mlk.swe.avg$x ~ mlk.swe.avg$Group.1, data = mlk.swe.avg, family = gaussian)
summary(mlk.swe.glm) # Slope = -2.571, P = 0.540
# abline(coef(mlk.swe.glm), lty = 2)
arrows(c(2006:2016),mlk.swe.avg[1:11,2], c(2006:2016), mlk.swe.avg[1:11,2]+mlk.swe.sd[1:11,2], length=0.05, angle=90, code=3)
arrows(c(2006:2016),mlk.swe.avg[1:11,2], c(2006:2016), mlk.swe.avg[1:11,2]-mlk.swe.sd[1:11,2], length=0.05, angle=90, code=3)
axis(1, at = c(2006:2016), labels = NA, tick = TRUE)
mtext(side=2, "SWE (mm)", adj=0.5, line=2.5)

plot(mlk.density.avg$x ~ mlk.density.avg$Group.1, type = "p",xaxt = "n", ylim = c(150,300), ylab = "",xlab = "",pch=19)
mlk.density.glm <- glm(mlk.density.avg$x ~ mlk.density.avg$Group.1, data = mlk.density.avg, family = gaussian)
summary(mlk.density.glm) # Slope = -4.119, P = 0.173
# abline(coef(mlk.density.glm), lty = 2)
arrows(c(2006:2016),mlk.density.avg[1:11,2], c(2006:2016), mlk.density.avg[1:11,2]+mlk.density.sd[1:11,2], length=0.05, angle=90, code=3)
arrows(c(2006:2016),mlk.density.avg[1:11,2], c(2006:2016), mlk.density.avg[1:11,2]-mlk.density.sd[1:11,2], length=0.05, angle=90, code=3)
axis(1, at = c(2006:2016), labels = NA, tick = TRUE) 
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)

plot(mlk.htc.avg$x ~ mlk.htc.avg$Group.1, type = "p",xaxt = "n", ylim = c(0.1,0.35), ylab = "",xlab = "",pch=19)
mlk.htc.glm <- glm(mlk.htc.avg$x ~ mlk.htc.avg$Group.1, data = mlk.htc.avg, family = gaussian)
summary(mlk.htc.glm) # Slope = -0.009670, P = 0.0731
# abline(coef(mlk.htc.glm), lty = 2)
arrows(c(2006:2016),mlk.htc.avg[1:11,2], c(2006:2016), mlk.htc.avg[1:11,2]+mlk.htc.sd[1:11,2], length=0.05, angle=90, code=3)
arrows(c(2006:2016),mlk.htc.avg[1:11,2], c(2006:2016), mlk.htc.avg[1:11,2]-mlk.htc.sd[1:11,2], length=0.05, angle=90, code=3)
axis(1, at = c(2006:2016), labels = c(2006:2016), tick = TRUE)   
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
mtext(side=1, "Year", adj=0.5, line=2.5)

# Owl River: Coastal Beach Ridge
# Export at 7 x 7
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(0, 2, 1, 3), oma = c(4,2,1,1))

plot(ocr.depth.avg$x ~ ocr.depth.avg$Group.1, type = "p", xaxt = "n", ylim = c(0,60), ylab = "",xlab = "",pch=19)
ocr.depth.glm <- glm(ocr.depth.avg$x ~ ocr.depth.avg$Group.1, data = ocr.depth.avg, family = gaussian)
summary(ocr.depth.glm) # Slope = -4.490, P = 0.0725
# abline(coef(ocr.depth.glm), lty = 2)
arrows(c(2007:2016),ocr.depth.avg[1:10,2], c(2007:2016), ocr.depth.avg[1:10,2]+ocr.depth.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),ocr.depth.avg[1:10,2], c(2007:2016), ocr.depth.avg[1:10,2]-ocr.depth.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE)   
mtext(side=2, "Depth (cm)", adj=0.5, line=2.5)

plot(ocr.swe.avg$x ~ ocr.swe.avg$Group.1, type = "p",xaxt = "n", ylim = c(0,180), ylab = "",xlab = "",pch=19)
ocr.swe.glm <- glm(ocr.swe.avg$x ~ ocr.swe.avg$Group.1, data = ocr.swe.avg, family = gaussian)
summary(ocr.swe.glm) # Slope = -12.383, P = 0.0948
# abline(coef(ocr.swe.glm), lty = 2)
arrows(c(2007:2016),ocr.swe.avg[1:10,2], c(2007:2016), ocr.swe.avg[1:10,2]+ocr.swe.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),ocr.swe.avg[1:10,2], c(2007:2016), ocr.swe.avg[1:10,2]-ocr.swe.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE)
mtext(side=2, "SWE (mm)", adj=0.5, line=2.5)

plot(ocr.density.avg$x ~ ocr.density.avg$Group.1, type = "p",xaxt = "n", ylim = c(180,420), ylab = "",xlab = "",pch=19)
ocr.density.glm <- glm(ocr.density.avg$x ~ ocr.density.avg$Group.1, data = ocr.density.avg, family = gaussian)
summary(ocr.density.glm) # Slope = 0.2863, P = 0.945
# abline(coef(ocr.density.glm), lty = 2)
arrows(c(2007:2016),ocr.density.avg[1:10,2], c(2007:2016), ocr.density.avg[1:10,2]+ocr.density.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),ocr.density.avg[1:10,2], c(2007:2016), ocr.density.avg[1:10,2]-ocr.density.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE) 
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)

plot(ocr.htc.avg$x ~ ocr.htc.avg$Group.1, type = "p",xaxt = "n", ylim = c(0.1,10), ylab = "",xlab = "",pch=19)
ocr.htc.glm <- glm(ocr.htc.avg$x ~ ocr.htc.avg$Group.1, data = ocr.htc.avg, family = gaussian)
summary(ocr.htc.glm) # Slope = 0.4025, P = 0.0364*
abline(coef(ocr.htc.glm), lty = 2)
arrows(c(2007:2016),ocr.htc.avg[1:10,2], c(2007:2016), ocr.htc.avg[1:10,2]+ocr.htc.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),ocr.htc.avg[1:10,2], c(2007:2016), ocr.htc.avg[1:10,2]-ocr.htc.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = c(2007:2016), tick = TRUE)   
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
mtext(side=1, "Year", adj=0.5, line=2.5)

# Owl River: Shrub Fen
# Export at 7 x 7
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(0, 2, 1, 3), oma = c(4,2,1,1))

plot(osh.depth.avg$x ~ osh.depth.avg$Group.1, type = "p", xaxt = "n", ylim = c(20,110), ylab = "",xlab = "",pch=19)
osh.depth.glm <- glm(osh.depth.avg$x ~ osh.depth.avg$Group.1, data = osh.depth.avg, family = gaussian)
summary(osh.depth.glm) # Slope = 2.481, P = 0.477
# abline(coef(osh.depth.glm), lty = 2)
arrows(c(2007:2016),osh.depth.avg[1:10,2], c(2007:2016), osh.depth.avg[1:10,2]+osh.depth.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),osh.depth.avg[1:10,2], c(2007:2016), osh.depth.avg[1:10,2]-osh.depth.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE)   
mtext(side=2, "Depth (cm)", adj=0.5, line=2.5)

plot(osh.swe.avg$x ~ osh.swe.avg$Group.1, type = "p",xaxt = "n", ylim = c(40,260), ylab = "",xlab = "",pch=19)
osh.swe.glm <- glm(osh.swe.avg$x ~ osh.swe.avg$Group.1, data = osh.swe.avg, family = gaussian)
summary(osh.swe.glm) # Slope = 3.891, P = 0.657
# abline(coef(osh.swe.glm), lty = 2)
arrows(c(2007:2016),osh.swe.avg[1:10,2], c(2007:2016), osh.swe.avg[1:10,2]+osh.swe.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),osh.swe.avg[1:10,2], c(2007:2016), osh.swe.avg[1:10,2]-osh.swe.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE)
mtext(side=2, "SWE (mm)", adj=0.5, line=2.5)

plot(osh.density.avg$x ~ osh.density.avg$Group.1, type = "p",xaxt = "n", ylim = c(70,300), ylab = "",xlab = "",pch=19)
osh.density.glm <- glm(osh.density.avg$x ~ osh.density.avg$Group.1, data = osh.density.avg, family = gaussian)
summary(osh.density.glm) # Slope = -3.289, P = 0.506
# abline(coef(osh.density.glm), lty = 2)
arrows(c(2007:2016),osh.density.avg[1:10,2], c(2007:2016), osh.density.avg[1:10,2]+osh.density.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),osh.density.avg[1:10,2], c(2007:2016), osh.density.avg[1:10,2]-osh.density.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE) 
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)

plot(osh.htc.avg$x ~ osh.htc.avg$Group.1, type = "p",xaxt = "n", ylim = c(0.06,0.8), ylab = "",xlab = "",pch=19)
osh.htc.glm <- glm(osh.htc.avg$x ~ osh.htc.avg$Group.1, data = osh.htc.avg, family = gaussian)
summary(osh.htc.glm) # Slope = -0.03050, P = 0.119
# abline(coef(osh.htc.glm), lty = 2)
arrows(c(2007:2016),osh.htc.avg[1:10,2], c(2007:2016), osh.htc.avg[1:10,2]+osh.htc.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),osh.htc.avg[1:10,2], c(2007:2016), osh.htc.avg[1:10,2]-osh.htc.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = c(2007:2016), tick = TRUE)   
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
mtext(side=1, "Year", adj=0.5, line=2.5)

# Owl River: Spruce Forest
# Export at 7 x 7
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(0, 2, 1, 3), oma = c(4,2,1,1))

plot(osf.depth.avg$x ~ osf.depth.avg$Group.1, type = "p", xaxt = "n", ylim = c(20,80), ylab = "",xlab = "",pch=19)
osf.depth.glm <- glm(osf.depth.avg$x ~ osf.depth.avg$Group.1, data = osf.depth.avg, family = gaussian)
summary(osf.depth.glm) # Slope = -1.062, P = 0.366
# abline(coef(osf.depth.glm), lty = 2)
arrows(c(2007:2016),osf.depth.avg[1:10,2], c(2007:2016), osf.depth.avg[1:10,2]+osf.depth.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),osf.depth.avg[1:10,2], c(2007:2016), osf.depth.avg[1:10,2]-osf.depth.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE)   
mtext(side=2, "Depth (cm)", adj=0.5, line=2.5)

plot(osf.swe.avg$x ~ osf.swe.avg$Group.1, type = "p",xaxt = "n", ylim = c(40,170), ylab = "",xlab = "",pch=19)
osf.swe.glm <- glm(osf.swe.avg$x ~ osf.swe.avg$Group.1, data = osf.swe.avg, family = gaussian)
summary(osf.swe.glm) # Slope = -2.154, P = 0.568
# abline(coef(osf.swe.glm), lty = 2)
arrows(c(2007:2016),osf.swe.avg[1:10,2], c(2007:2016), osf.swe.avg[1:10,2]+osf.swe.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),osf.swe.avg[1:10,2], c(2007:2016), osf.swe.avg[1:10,2]-osf.swe.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE)
mtext(side=2, "SWE (mm)", adj=0.5, line=2.5)

plot(osf.density.avg$x ~ osf.density.avg$Group.1, type = "p",xaxt = "n", ylim = c(120,250), ylab = "",xlab = "",pch=19)
osf.density.glm <- glm(osf.density.avg$x ~ osf.density.avg$Group.1, data = osf.density.avg, family = gaussian)
summary(osf.density.glm) # Slope = -0.07398, P = 0.982
# abline(coef(osf.density.glm), lty = 2)
arrows(c(2007:2016),osf.density.avg[1:10,2], c(2007:2016), osf.density.avg[1:10,2]+osf.density.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),osf.density.avg[1:10,2], c(2007:2016), osf.density.avg[1:10,2]-osf.density.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = NA, tick = TRUE) 
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)

plot(osf.htc.avg$x ~ osf.htc.avg$Group.1, type = "p",xaxt = "n", ylim = c(0.10,0.31), ylab = "",xlab = "",pch=19)
osf.htc.glm <- glm(osf.htc.avg$x ~ osf.htc.avg$Group.1, data = osf.htc.avg, family = gaussian)
summary(osf.htc.glm) # Slope = 0.003534, P = 0.420
# abline(coef(osf.htc.glm), lty = 2)
arrows(c(2007:2016),osf.htc.avg[1:10,2], c(2007:2016), osf.htc.avg[1:10,2]+osf.htc.sd[1:10,2], length=0.05, angle=90, code=3)
arrows(c(2007:2016),osf.htc.avg[1:10,2], c(2007:2016), osf.htc.avg[1:10,2]-osf.htc.sd[1:10,2], length=0.05, angle=90, code=3)
axis(1, at = c(2007:2016), labels = c(2007:2016), tick = TRUE)   
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
mtext(side=1, "Year", adj=0.5, line=2.5)

# Roberge Lake: Polygonal Peat Plateau - Ice Wedge
# Export at 7 x 7
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(0, 2, 1, 3), oma = c(4,2,1,1))

plot(riw.depth.avg$x ~ riw.depth.avg$Group.1, type = "p", xaxt = "n", ylim = c(0,50), ylab = "",xlab = "",pch=19)
riw.depth.glm <- glm(riw.depth.avg$x ~ riw.depth.avg$Group.1, data = riw.depth.avg, family = gaussian)
summary(riw.depth.glm) # Slope = 2.724, P = 0.225
# abline(coef(riw.depth.glm), lty = 2)
arrows(c(2009:2015),riw.depth.avg[1:7,2], c(2009:2015), riw.depth.avg[1:7,2]+riw.depth.sd[1:7,2], length=0.05, angle=90, code=3)
arrows(c(2009:2015),riw.depth.avg[1:7,2], c(2009:2015), riw.depth.avg[1:7,2]-riw.depth.sd[1:7,2], length=0.05, angle=90, code=3)
axis(1, at = c(2009:2015), labels = NA, tick = TRUE)   
mtext(side=2, "Depth (cm)", adj=0.5, line=2.5)

plot(riw.swe.avg$x ~ riw.swe.avg$Group.1, type = "p",xaxt = "n", ylim = c(20,120), ylab = "",xlab = "",pch=19)
riw.swe.glm <- glm(riw.swe.avg$x ~ riw.swe.avg$Group.1, data = riw.swe.avg, family = gaussian)
summary(riw.swe.glm) # Slope = 5.364, P = 0.242
# abline(coef(riw.swe.glm), lty = 2)
arrows(c(2009:2015),riw.swe.avg[1:7,2], c(2009:2015), riw.swe.avg[1:7,2]+riw.swe.sd[1:7,2], length=0.05, angle=90, code=3)
arrows(c(2009:2015),riw.swe.avg[1:7,2], c(2009:2015), riw.swe.avg[1:7,2]-riw.swe.sd[1:7,2], length=0.05, angle=90, code=3)
axis(1, at = c(2009:2015), labels = NA, tick = TRUE)
mtext(side=2, "SWE (mm)", adj=0.5, line=2.5)

plot(riw.density.avg$x ~ riw.density.avg$Group.1, type = "p",xaxt = "n", ylim = c(140,340), ylab = "",xlab = "",pch=19)
riw.density.glm <- glm(riw.density.avg$x ~ riw.density.avg$Group.1, data = riw.density.avg, family = gaussian)
summary(riw.density.glm) # Slope = -5.284, P = 0.645
# abline(coef(riw.density.glm), lty = 2)
arrows(c(2009:2015),riw.density.avg[1:7,2], c(2009:2015), riw.density.avg[1:7,2]+riw.density.sd[1:7,2], length=0.05, angle=90, code=3)
arrows(c(2009:2015),riw.density.avg[1:7,2], c(2009:2015), riw.density.avg[1:7,2]-riw.density.sd[1:7,2], length=0.05, angle=90, code=3)
axis(1, at = c(2009:2015), labels = NA, tick = TRUE) 
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)

plot(riw.htc.avg$x ~ riw.htc.avg$Group.1, type = "p",xaxt = "n", ylim = c(0,5), ylab = "",xlab = "",pch=19)
riw.htc.glm <- glm(riw.htc.avg$x ~ riw.htc.avg$Group.1, data = riw.htc.avg, family = gaussian)
summary(riw.htc.glm) # Slope = -0.2582, P = 0.149
# abline(coef(riw.htc.glm), lty = 2)
arrows(c(2009:2015),riw.htc.avg[1:7,2], c(2009:2015), riw.htc.avg[1:7,2]+riw.htc.sd[1:7,2], length=0.05, angle=90, code=3)
arrows(c(2009:2015),riw.htc.avg[1:7,2], c(2009:2015), riw.htc.avg[1:7,2]-riw.htc.sd[1:7,2], length=0.05, angle=90, code=3)
axis(1, at = c(2009:2015), labels = c(2009:2015), tick = TRUE)   
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
mtext(side=1, "Year", adj=0.5, line=2.5)

# Roberge Lake: Polygonal Peat Plateau - Polygon Center
# Export at 7 x 7
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(0, 2, 1, 3), oma = c(4,2,1,1))

plot(rpp.depth.avg$x ~ rpp.depth.avg$Group.1, type = "p", xaxt = "n", ylim = c(0,25), ylab = "",xlab = "",pch=19)
rpp.depth.glm <- glm(rpp.depth.avg$x ~ rpp.depth.avg$Group.1, data = rpp.depth.avg, family = gaussian)
summary(rpp.depth.glm) # Slope = -0.5630, P = 0.446
# abline(coef(rpp.depth.glm), lty = 2)
arrows(c(2008:2015),rpp.depth.avg[1:8,2], c(2008:2015), rpp.depth.avg[1:8,2]+rpp.depth.sd[1:8,2], length=0.05, angle=90, code=3)
arrows(c(2008:2015),rpp.depth.avg[1:8,2], c(2008:2015), rpp.depth.avg[1:8,2]-rpp.depth.sd[1:8,2], length=0.05, angle=90, code=3)
axis(1, at = c(2008:2015), labels = NA, tick = TRUE)   
mtext(side=2, "Depth (cm)", adj=0.5, line=2.5)

plot(rpp.swe.avg$x ~ rpp.swe.avg$Group.1, type = "p",xaxt = "n", ylim = c(0,70), ylab = "",xlab = "",pch=19)
rpp.swe.glm <- glm(rpp.swe.avg$x ~ rpp.swe.avg$Group.1, data = rpp.swe.avg, family = gaussian)
summary(rpp.swe.glm) # Slope = -2.273, P = 0.249
# abline(coef(rpp.swe.glm), lty = 2)
arrows(c(2008:2015),rpp.swe.avg[1:8,2], c(2008:2015), rpp.swe.avg[1:8,2]+rpp.swe.sd[1:8,2], length=0.05, angle=90, code=3)
arrows(c(2008:2015),rpp.swe.avg[1:8,2], c(2008:2015), rpp.swe.avg[1:8,2]-rpp.swe.sd[1:8,2], length=0.05, angle=90, code=3)
axis(1, at = c(2008:2015), labels = NA, tick = TRUE)
mtext(side=2, "SWE (mm)", adj=0.5, line=2.5)

plot(rpp.density.avg$x ~ rpp.density.avg$Group.1, type = "p",xaxt = "n", ylim = c(60,320), ylab = "",xlab = "",pch=19)
rpp.density.glm <- glm(rpp.density.avg$x ~ rpp.density.avg$Group.1, data = rpp.density.avg, family = gaussian)
summary(rpp.density.glm) # Slope = -7.390, P = 0.323
# abline(coef(rpp.density.glm), lty = 2)
arrows(c(2008:2015),rpp.density.avg[1:8,2], c(2008:2015), rpp.density.avg[1:8,2]+rpp.density.sd[1:8,2], length=0.05, angle=90, code=3)
arrows(c(2008:2015),rpp.density.avg[1:8,2], c(2008:2015), rpp.density.avg[1:8,2]-rpp.density.sd[1:8,2], length=0.05, angle=90, code=3)
axis(1, at = c(2008:2015), labels = NA, tick = TRUE) 
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)

plot(rpp.htc.avg$x ~ rpp.htc.avg$Group.1, type = "p",xaxt = "n", ylim = c(0,4.2), ylab = "",xlab = "",pch=19)
rpp.htc.glm <- glm(rpp.htc.avg$x ~ rpp.htc.avg$Group.1, data = rpp.htc.avg, family = gaussian)
summary(rpp.htc.glm) # Slope = -0.04087, P = 0.796
# abline(coef(rpp.htc.glm), lty = 2)
arrows(c(2008:2015),rpp.htc.avg[1:8,2], c(2008:2015), rpp.htc.avg[1:8,2]+rpp.htc.sd[1:8,2], length=0.05, angle=90, code=3)
arrows(c(2008:2015),rpp.htc.avg[1:8,2], c(2008:2015), rpp.htc.avg[1:8,2]-rpp.htc.sd[1:8,2], length=0.05, angle=90, code=3)
axis(1, at = c(2008:2015), labels = c(2008:2015), tick = TRUE)   
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
mtext(side=1, "Year", adj=0.5, line=2.5)

# Roberge Lake: Tree Island
# Export at 7 x 7
par(mfrow = c(4, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 12 pts
par(mar = c(0, 2, 1, 3), oma = c(4,2,1,1))

plot(rti.depth.avg$x ~ rti.depth.avg$Group.1, type = "p", xaxt = "n", ylim = c(60,160), ylab = "",xlab = "",pch=19)
rti.depth.glm <- glm(rti.depth.avg$x ~ rti.depth.avg$Group.1, data = rti.depth.avg, family = gaussian)
summary(rti.depth.glm) # Slope = 4.907, P = 0.102
# abline(coef(rti.depth.glm), lty = 2)
arrows(c(2008:2015),rti.depth.avg[1:8,2], c(2008:2015), rti.depth.avg[1:8,2]+rti.depth.sd[1:8,2], length=0.05, angle=90, code=3)
arrows(c(2008:2015),rti.depth.avg[1:8,2], c(2008:2015), rti.depth.avg[1:8,2]-rti.depth.sd[1:8,2], length=0.05, angle=90, code=3)
axis(1, at = c(2008:2015), labels = NA, tick = TRUE)   
mtext(side=2, "Depth (cm)", adj=0.5, line=2.5)

plot(rti.swe.avg$x ~ rti.swe.avg$Group.1, type = "p",xaxt = "n", ylim = c(120,500), ylab = "",xlab = "",pch=19)
rti.swe.glm <- glm(rti.swe.avg$x ~ rti.swe.avg$Group.1, data = rti.swe.avg, family = gaussian)
summary(rti.swe.glm) # Slope = 18.762, P = 0.0892
# abline(coef(rti.swe.glm), lty = 2)
arrows(c(2008:2015),rti.swe.avg[1:8,2], c(2008:2015), rti.swe.avg[1:8,2]+rti.swe.sd[1:8,2], length=0.05, angle=90, code=3)
arrows(c(2008:2015),rti.swe.avg[1:8,2], c(2008:2015), rti.swe.avg[1:8,2]-rti.swe.sd[1:8,2], length=0.05, angle=90, code=3)
axis(1, at = c(2008:2015), labels = NA, tick = TRUE)
mtext(side=2, "SWE (mm)", adj=0.5, line=2.5)

plot(rti.density.avg$x ~ rti.density.avg$Group.1, type = "p", xaxt = "n", ylim = c(190,340), ylab = "",xlab = "",pch=19)
rti.density.glm <- glm(rti.density.avg$x ~ rti.density.avg$Group.1, data = rti.density.avg, family = gaussian)
summary(rti.density.glm) # Slope = 5.754, P = 0.203
# abline(coef(rti.density.glm), lty = 2)
arrows(c(2008:2015),rti.density.avg[1:8,2], c(2008:2015), rti.density.avg[1:8,2]+rti.density.sd[1:8,2], length=0.05, angle=90, code=3)
arrows(c(2008:2015),rti.density.avg[1:8,2], c(2008:2015), rti.density.avg[1:8,2]-rti.density.sd[1:8,2], length=0.05, angle=90, code=3)
axis(1, at = c(2008:2015), labels = NA, tick = TRUE) 
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, adj=0.5, line=2.5)

plot(rti.htc.avg$x ~ rti.htc.avg$Group.1, type = "p", xaxt = "n", ylim = c(0.08,0.4), ylab = "",xlab = "",pch=19)
rti.htc.glm <- glm(rti.htc.avg$x ~ rti.htc.avg$Group.1, data = rti.htc.avg, family = gaussian)
summary(rti.htc.glm) # Slope = 0.0005203, P = 0.934
# abline(coef(rti.htc.glm), lty = 2)
arrows(c(2008:2015),rti.htc.avg[1:8,2], c(2008:2015), rti.htc.avg[1:8,2]+rti.htc.sd[1:8,2], length=0.05, angle=90, code=3)
arrows(c(2008:2015),rti.htc.avg[1:8,2], c(2008:2015), rti.htc.avg[1:8,2]-rti.htc.sd[1:8,2], length=0.05, angle=90, code=3)
axis(1, at = c(2008:2015), labels = c(2008:2015), tick = TRUE)   
mtext(expression(paste("HTC (W m"^"-2","K"^"-1",")")), side=2, adj=0.5, line=2.5)
mtext(side=1, "Year", adj=0.5, line=2.5)

## Other code not used for this round
################################################################################################
################################################################################################
################################################################################################
################################################################################################


## Run glms for each snowpack metric versus time for each landscape type
beach.depth <- glm(beach$depth ~ beach$year, data = beach, family = gaussian)
beach.density <- glm(beach$density ~ beach$year, data = beach, family = gaussian)
beach.swe <- glm(beach$swe ~ beach$year, data = beach, family = gaussian)
beach.htc <- glm(beach$htc ~ beach$year, data = beach, family = gaussian)
fen.depth <- glm(fen$depth ~ fen$year, data = fen, family = gaussian)
fen.density <- glm(fen$density ~ fen$year, data = fen, family = gaussian)
fen.swe <- glm(fen$swe ~ fen$year, data = fen, family = gaussian)
fen.htc <- glm(fen$htc ~ fen$year, data = fen, family = gaussian)
forest.depth <- glm(forest$depth ~ forest$year, data = forest, family = gaussian)
forest.density <- glm(forest$density ~ forest$year, data = forest, family = gaussian)
forest.swe <- glm(forest$swe ~ forest$year, data = forest, family = gaussian)
forest.htc <- glm(forest$htc ~ forest$year, data = forest, family = gaussian)
island.depth <- glm(island$depth ~ island$year, data = island, family = gaussian)
island.density <- glm(island$density ~ island$year, data = island, family = gaussian)
island.swe <- glm(island$swe ~ island$year, data = island, family = gaussian)
island.htc <- glm(island$htc ~ island$year, data = island, family = gaussian)
shrub.depth <- glm(shrub$depth ~ shrub$year, data = shrub, family = gaussian)
shrub.density <- glm(shrub$density ~ shrub$year, data = shrub, family = gaussian)
shrub.swe <- glm(shrub$swe ~ shrub$year, data = shrub, family = gaussian)
shrub.htc <- glm(shrub$htc ~ shrub$year, data = shrub, family = gaussian)
polygon.depth <- glm(polygon$depth ~ polygon$year, data = polygon, family = gaussian)
polygon.density <- glm(polygon$density ~ polygon$year, data = polygon, family = gaussian)
polygon.swe <- glm(polygon$swe ~ polygon$year, data = polygon, family = gaussian)
polygon.htc <- glm(polygon$htc ~ polygon$year, data = polygon, family = gaussian)
wedge.depth <- glm(wedge$depth ~ wedge$year, data = wedge, family = gaussian)
wedge.density <- glm(wedge$density ~ wedge$year, data = wedge, family = gaussian)
wedge.swe <- glm(wedge$swe ~ wedge$year, data = wedge, family = gaussian)
wedge.htc <- glm(wedge$htc ~ wedge$year, data = wedge, family = gaussian)

summary(beach.depth) # slope = -3.1543, P < 0.0001***
summary(beach.density) # slope = -5.8302
summary(beach.swe)
summary(beach.htc)

summary(fen.depth)
summary(fen.density)
summary(fen.swe)
summary(fen.htc)

summary(forest.depth)
summary(forest.density)
summary(forest.swe)
summary(forest.htc)

summary(island.depth)
summary(island.density)
summary(island.swe)
summary(island.htc)

summary(shrub.depth)
summary(shrub.density)
summary(shrub.swe)
summary(shrub.htc)

summary(polygon.depth)
summary(polygon.density)
summary(polygon.swe)
summary(polygon.htc)

summary(beach.depth)
summary(beach.density)
summary(beach.swe)
summary(beach.htc)

## Calculate 95% confidence intervals 
beach.depth.ci <- confint(beach.depth, level=0.95)
beach.htc.ci <- confint(beach.htc, level=0.95)
beach.depth.cf <- coef(beach.depth)
beach.htc.cf <- coef(beach.htc)
fen.depth.ci <- confint(fen.depth, level=0.95)
fen.htc.ci <- confint(fen.htc, level=0.95)
fen.depth.cf <- coef(fen.depth)
fen.htc.cf <- coef(fen.htc)
forest.depth.ci <- confint(forest.depth, level=0.95)
forest.htc.ci <- confint(forest.htc, level=0.95)
forest.depth.cf <- coef(forest.depth)
forest.htc.cf <- coef(forest.htc)
island.depth.ci <- confint(island.depth, level=0.95)
island.htc.ci <- confint(island.htc, level=0.95)
island.depth.cf <- coef(island.depth)
island.htc.cf <- coef(island.htc)
shrub.depth.ci <- confint(shrub.depth, level=0.95)
shrub.htc.ci <- confint(shrub.htc, level=0.95)
shrub.depth.cf <- coef(shrub.depth)
shrub.htc.cf <- coef(shrub.htc)
polygon.depth.ci <- confint(polygon.depth, level=0.95)
polygon.htc.ci <- confint(polygon.htc, level=0.95)
polygon.depth.cf <- coef(polygon.depth)
polygon.htc.cf <- coef(polygon.htc)

wedge.depth.ci <- confint(wedge.depth, level=0.95)
wedge.htc.ci <- confint(wedge.htc, level=0.95)
wedge.depth.cf <- coef(wedge.depth)
wedge.htc.cf <- coef(wedge.htc)


plot(depth.plot[1:7,2], type = "p", ylim = c(-3,10), xaxt = "n", ylab = "",xlab = "",pch=19)
axis(1, at = c(1:7), labels = c("","","","","","",""), tick = TRUE)   
mtext(side=2, "Parametre estimate", adj=0.5, line=2.5)
abline(h=0, lty = 2)
arrows(c(1:7),depth.plot[1:7,2], c(1:7),depth.plot[1:7,3], length=0.05, angle=90, code=3)
legend("topleft", "Snow depth", bty = "n", inset = c(-0.05,-0.05))


depth.plot <- t(data.frame(forest.d = forest.depth.cf, island.d = island.depth.cf, shrub.d = 
                             shrub.depth.cf, fen.d = fen.depth.cf, wedge.d = wedge.depth.cf, 
                           polygon.d = polygon.depth.cf, beach.d = beach.depth.cf, 
                           forest.ci = forest.depth.ci, island.ci = island.depth.ci, shrub.ci = 
                             shrub.depth.ci, fen.ci = fen.depth.ci, wedge.ci = wedge.depth.ci, 
                           polygon.ci = polygon.depth.ci, beach.ci = beach.depth.ci))







# write.csv(depth.plot, file = "~/Desktop/Workspace/EW/depth.plot.csv")
depth.plot <- read.csv(file = "~/Desktop/Workspace/EW/depth.plot.csv", header = TRUE)

htc.plot <- t(data.frame(forest.h = forest.htc.cf, island.h = island.htc.cf, shrub.h = 
                           shrub.htc.cf, fen.h = fen.htc.cf, wedge.h = wedge.htc.cf, 
                         polygon.h = polygon.htc.cf, beach.h = beach.htc.cf, 
                         forest.ci = forest.htc.ci, island.ci = island.htc.ci, shrub.ci = 
                           shrub.htc.ci, fen.ci = fen.htc.ci, wedge.ci = wedge.htc.ci, 
                         polygon.ci = polygon.htc.ci, beach.ci = beach.htc.ci))

# write.csv(htc.plot, file = "~/Desktop/Workspace/EW/htc.plot.csv")
htc.plot <- read.csv(file = "~/Desktop/Workspace/EW/htc.plot.csv", header = TRUE)


par(mfrow = c(2, 1))
par(ps = 12, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(0.25, 1, 1.75, 3), oma = c(3,3,1,1))

plot(depth.plot[1:7,2], type = "p", ylim = c(-3,10), xaxt = "n", ylab = "",xlab = "",pch=19)
axis(1, at = c(1:7), labels = c("","","","","","",""), tick = TRUE)   
mtext(side=2, "Parametre estimate", adj=0.5, line=2.5)
abline(h=0, lty = 2)
arrows(c(1:7),depth.plot[1:7,3], c(1:7),depth.plot[1:7,4], length=0.05, angle=90, code=3)
legend("topleft", "Snow depth", bty = "n", inset = c(-0.05,-0.05))

plot(htc.plot[1:7,2], type = "p", xaxt = "n", ylim = c(-0.2,0.2),ylab = "",xlab = "",pch=19)
axis(1, at = c(1:7), labels = c("forest","island","shrub","fen","wedge","polygon","beach"), tick = TRUE)   
mtext(side=2, "Parametre estimate", adj=0.5, line=2.5)
abline(h=0, lty = 2)
arrows(c(1:7),htc.plot[1:7,3], c(1:7),htc.plot[1:7,4], length=0.05, angle=90, code=3)
legend("topleft", "Snow HTC", bty = "n", inset = c(-0.05,-0.05))



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
boxplot(density ~ x1, data = ipy.ram,
        col = c("darkgreen","lightgreen","royalblue2"),
        ylim = c(0,600), ylab = '')
mtext(expression(paste("Density (kg m"^"-2",")")), side=2, line=2)
mtext(expression(paste("Zone")), side=1, line=2)
density.y <- t(aggregate(ipy.ram$density, by=list(x1), FUN=max)[2])
text(x=c(1,2,3),y=density.y+30,c("a","b","b"))



