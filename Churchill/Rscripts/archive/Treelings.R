library(plyr)

rm(list = ls())

treelings <- read.csv("~/Dropbox/Desktop/Workspace/Churchill/Treelings.csv", header = T)
str(treelings)

count.agg <- count(treelings, c("Year", "Site", "Species","Vitality2"))
count.agg2 <- count.agg[complete.cases(count.agg),]

count.air <- subset(count.agg2, Site == c("AIRF","AIRP"))
count.bfr <- subset(count.agg2, Site == c("BFR")) # No seedlings
count.pab <- subset(count.agg2, Site == c("PAB"))
count.bsw <- subset(count.agg2, Site == c("BSW"))
count.bwp <- subset(count.agg2, Site == c("BWP"))
count.fen <- subset(count.agg2, Site == c("FEN"))
count.ppa <- subset(count.agg2, Site == c("PPA")) # No seedlings
count.ppd <- subset(count.agg2, Site == c("PPD")) # No seedlings
count.tun <- subset(count.agg2, Site == c("TUN"))
count.wsu <- subset(count.agg2, Site == c("WSU"))

count.air$density <- (count.air$freq / (18*2))*10000
count.pab$density <- (count.pab$freq / (16*2))*10000
count.bsw$density <- (count.bsw$freq / (16*2))*10000
count.bwp$density <- (count.bwp$freq / (16*2))*10000
count.fen$density <- (count.fen$freq / (17*2))*10000
count.tun$density <- (count.tun$freq / (16*2))*10000
count.wsu$density <- (count.wsu$freq / (18*2))*10000

density.df <- rbind.data.frame(count.air,
                               count.pab,
                               count.bsw,
                               count.bwp,
                               count.fen,
                               count.tun,
                               count.wsu)

density.agg <- aggregate(density ~ Year + Species + Vitality2, data = density.df, function(x) c(mean = mean(x), sd = sd(x)))

density.agg.live <- subset(density.agg, Vitality2 == "Live")
density.agg.dead <- subset(density.agg, Vitality2 == "Dead")
density.agg.live.pg <- subset(density.agg.live, Species == "PG")
density.agg.live.pm <- subset(density.agg.live, Species == "PM")
density.agg.live.ll <- subset(density.agg.live, Species == "LL")
density.agg.dead.pg <- subset(density.agg.dead, Species == "PG")
density.agg.dead.pm <- subset(density.agg.dead, Species == "PM")
density.agg.dead.ll <- subset(density.agg.dead, Species == "LL")

## White spruce
density.agg.live.pg2 <- cbind.data.frame(density.agg.live.pg[,c(1:3)], 
                                         density = density.agg.live.pg$density[,1])
density.agg.dead.pg2 <- cbind.data.frame(density.agg.dead.pg[,c(1:3)], 
                                         density = density.agg.dead.pg$density[,1])
density.agg.live.pg.tmp <- cbind.data.frame(Year = 2018, Species = "PG", 
                                            Vitality2 = "Live", density = NA)
density.agg.dead.pg.tmp <- cbind.data.frame(Year = c(2018,2019), Species = rep("PG",2), 
                                            Vitality2 = rep("Dead",2), density = c(NA,NA))
density.agg.live.pg3 <- rbind(density.agg.live.pg2[c(1:4),], density.agg.live.pg.tmp, density.agg.live.pg2[5,])
density.agg.dead.pg3 <- rbind(density.agg.dead.pg2, density.agg.dead.pg.tmp)
net.density.pg <- density.agg.live.pg3$density - density.agg.dead.pg3$density
density.agg.net.pg3 <- cbind.data.frame(Year = seq(2014,2019,1), density = net.density.pg)


## Black spruce
density.agg.live.pm2 <- cbind.data.frame(density.agg.live.pm[,c(1:3)], 
                                         density = density.agg.live.pm$density[,1])
density.agg.live.pm.tmp1 <- cbind.data.frame(Year = 2016, Species = "PM", 
                                             Vitality2 = "Live", density = NA)
density.agg.live.pm.tmp2 <- cbind.data.frame(Year = 2018, Species = "PM", 
                                            Vitality2 = "Live", density = NA)
density.agg.live.pm3 <- rbind(density.agg.live.pm2[c(1:2),], density.agg.live.pm.tmp1, 
                              density.agg.live.pm2[3,], density.agg.live.pm.tmp2, density.agg.live.pm2[4,])



## Larch
density.agg.live.ll2 <- cbind.data.frame(density.agg.live.ll[,c(1:3)], 
                                         density = density.agg.live.ll$density[,1])
density.agg.dead.ll2 <- cbind.data.frame(density.agg.dead.ll[,c(1:3)], 
                                         density = density.agg.dead.ll$density[,1])
density.agg.live.ll.tmp <- cbind.data.frame(Year = 2018, Species = "LL", 
                                            Vitality2 = "Live", density = NA)
density.agg.dead.ll.tmp <- cbind.data.frame(Year = 2018, Species = "LL", 
                                            Vitality2 = "Dead", density = NA)
density.agg.live.ll3 <- rbind(density.agg.live.ll2[c(1:4),], density.agg.live.ll.tmp, density.agg.live.ll2[5,])
density.agg.dead.ll3 <- rbind(density.agg.dead.ll2[c(1:4),], density.agg.dead.ll.tmp, density.agg.dead.ll2[5,])
net.density.ll <- density.agg.live.ll3$density - density.agg.dead.ll3$density
density.agg.net.ll3 <- cbind.data.frame(Year = seq(2014,2019,1), density = net.density.ll)


## Plotting

# White spruce
# Export at 5 x 4
range(density.agg.live.pg3$density, na.rm = T)
par(mar = c(3.75,3.75,0.75,0.75))
plot(density.agg.live.pg3$density ~ density.agg.live.pg3$Year, type = "b", col = "green4", ylim = c(0,40000), 
     axes = F, xlab = "", ylab = "")
par(new = T)
plot(density.agg.dead.pg3$density ~ density.agg.dead.pg3$Year, type = "b", col = "goldenrod", ylim = c(0,40000), 
     axes = F, xlab = "", ylab = "")
par(new = T)
plot(density.agg.net.pg3$density ~ density.agg.net.pg3$Year, type = "b", col = "blue4", ylim = c(0,40000), 
     axes = F, xlab = "", ylab = "")
box()
axis(1, seq(2014,2019,1))
axis(2, seq(0,40000,10000))
mtext(side = 1, "Year", line = 2.2)
mtext(side = 2, expression(paste("Treeling density (ha"^"-1",")")), line = 2.2)

# Black spruce
# Export at 5 x 4
range(density.agg.live.pm3$density, na.rm = T)
par(mar = c(3.75,3.75,0.75,0.75))
plot(density.agg.live.pm3$density ~ density.agg.live.pm3$Year, type = "b", col = "green4", ylim = c(0,4000), 
     axes = F, xlab = "", ylab = "")
box()
axis(1, seq(2014,2019,1))
axis(2, seq(0,4000,1000))
mtext(side = 1, "Year", line = 2.2)
mtext(side = 2, expression(paste("Treeling density (ha"^"-1",")")), line = 2.2)

# Larch
# Export at 5 x 4
range(density.agg.live.ll3$density, na.rm = T)
par(mar = c(3.75,3.75,0.75,0.75))
plot(density.agg.live.ll3$density ~ density.agg.live.ll3$Year, type = "b", col = "green4", ylim = c(0,40000), 
     axes = F, xlab = "", ylab = "")
par(new = T)
plot(density.agg.dead.ll3$density ~ density.agg.dead.ll3$Year, type = "b", col = "goldenrod", ylim = c(0,40000), 
     axes = F, xlab = "", ylab = "")
par(new = T)
plot(density.agg.net.ll3$density ~ density.agg.net.ll3$Year, type = "b", col = "blue4", ylim = c(0,40000), 
     axes = F, xlab = "", ylab = "")
box()
axis(1, seq(2014,2019,1))
axis(2, seq(0,40000,10000))
mtext(side = 1, "Year", line = 2.2)
mtext(side = 2, expression(paste("Treeling density (ha"^"-1",")")), line = 2.2)




## Number of plots at each site
count.agg.tmp <- count(treelings, c("Site","Plot"))
nrow(subset(count.agg.tmp, Site == c("AIRF","AIRP"))) # 18
nrow(subset(count.agg.tmp, Site == c("BFR"))) # No seedlings
nrow(subset(count.agg.tmp, Site == c("PAB"))) # 16
nrow(subset(count.agg.tmp, Site == c("BSW"))) # 16
nrow(subset(count.agg.tmp, Site == c("BWP"))) # 16
nrow(subset(count.agg.tmp, Site == c("FEN"))) # 17
nrow(subset(count.agg.tmp, Site == c("PPA"))) # No seedlings
nrow(subset(count.agg.tmp, Site == c("PPD"))) # No seedlings
nrow(subset(count.agg.tmp, Site == c("TUN"))) # 16
nrow(subset(count.agg.tmp, Site == c("WSU"))) # 18




