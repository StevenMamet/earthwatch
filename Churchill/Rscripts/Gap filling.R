library(dplyr)

rm(list=ls())

microclim <- read.csv(file = "~/Desktop/Workspace/Earthwatch/microclimate.csv", header = TRUE)

# BSW soil based on WSU soilace
bsw.1.soilT <- lm(microclim[microclim$month==1, 15] ~ microclim[microclim$month==1, 21]) # Jan
summary(bsw.1.soilT)
bsw.2.soilT <- lm(microclim[microclim$month==2, 15] ~ microclim[microclim$month==2, 21]) # Feb
summary(bsw.2.soilT)
bsw.3.soilT <- lm(microclim[microclim$month==3, 15] ~ microclim[microclim$month==3, 21]) # Mar
summary(bsw.3.soilT)
bsw.4.soilT <- lm(microclim[microclim$month==4, 15] ~ microclim[microclim$month==4, 21]) # Apr
summary(bsw.4.soilT)
bsw.5.soilT <- lm(microclim[microclim$month==5, 15] ~ microclim[microclim$month==5, 21]) # May
summary(bsw.5.soilT)
bsw.6.soilT <- lm(microclim[microclim$month==6, 15] ~ microclim[microclim$month==6, 21]) # Jun
summary(bsw.6.soilT)
bsw.7.soilT <- lm(microclim[microclim$month==7, 15] ~ microclim[microclim$month==7, 21]) # Jul
summary(bsw.7.soilT)
bsw.8.soilT <- lm(microclim[microclim$month==8, 15] ~ microclim[microclim$month==8, 21]) # Aug
summary(bsw.8.soilT)
bsw.9.soilT <- lm(microclim[microclim$month==9, 15] ~ microclim[microclim$month==9, 21]) # Sep
summary(bsw.9.soilT)
bsw.10.soilT <- lm(microclim[microclim$month==10, 15] ~ microclim[microclim$month==10, 21]) # Oct
summary(bsw.10.soilT)
bsw.11.soilT <- lm(microclim[microclim$month==11, 15] ~ microclim[microclim$month==11, 21]) # Nov
summary(bsw.11.soilT)
bsw.12.soilT <- lm(microclim[microclim$month==12, 15] ~ microclim[microclim$month==12, 21]) # Dec
summary(bsw.12.soilT)

bsw.soilT <- data.frame(rbind(coef(bsw.1.soilT),coef(bsw.2.soilT),coef(bsw.3.soilT),coef(bsw.4.soilT),coef(bsw.5.soilT),coef(bsw.6.soilT),
                              coef(bsw.7.soilT),coef(bsw.8.soilT),coef(bsw.9.soilT),coef(bsw.10.soilT),coef(bsw.11.soilT),coef(bsw.12.soilT)))
names(bsw.soilT) <- c("b0","wsu.soilT")
rownames(bsw.soilT) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(bsw.soilT, "~/Desktop/Workspace/Earthwatch/bsw.soilT.csv")

# BSW surface based on WSU surface
bsw.1.surfT <- lm(microclim[microclim$month==1, 13] ~ microclim[microclim$month==1, 19]) # Jan
summary(bsw.1.surfT)
bsw.2.surfT <- lm(microclim[microclim$month==2, 13] ~ microclim[microclim$month==2, 19]) # Feb
summary(bsw.2.surfT)
bsw.3.surfT <- lm(microclim[microclim$month==3, 13] ~ microclim[microclim$month==3, 19]) # Mar
summary(bsw.3.surfT)
bsw.4.surfT <- lm(microclim[microclim$month==4, 13] ~ microclim[microclim$month==4, 19]) # Apr
summary(bsw.4.surfT)
bsw.5.surfT <- lm(microclim[microclim$month==5, 13] ~ microclim[microclim$month==5, 19]) # May
summary(bsw.5.surfT)
bsw.6.surfT <- lm(microclim[microclim$month==6, 13] ~ microclim[microclim$month==6, 19]) # Jun
summary(bsw.6.surfT)
bsw.7.surfT <- lm(microclim[microclim$month==7, 13] ~ microclim[microclim$month==7, 19]) # Jul
summary(bsw.7.surfT)
bsw.8.surfT <- lm(microclim[microclim$month==8, 13] ~ microclim[microclim$month==8, 19]) # Aug
summary(bsw.8.surfT)
bsw.9.surfT <- lm(microclim[microclim$month==9, 13] ~ microclim[microclim$month==9, 19]) # Sep
summary(bsw.9.surfT)
bsw.10.surfT <- lm(microclim[microclim$month==10, 13] ~ microclim[microclim$month==10, 19]) # Oct
summary(bsw.10.surfT)
bsw.11.surfT <- lm(microclim[microclim$month==11, 13] ~ microclim[microclim$month==11, 19]) # Nov
summary(bsw.11.surfT)
bsw.12.surfT <- lm(microclim[microclim$month==12, 13] ~ microclim[microclim$month==12, 19]) # Dec
summary(bsw.12.surfT)

bsw.surfT <- data.frame(rbind(coef(bsw.1.surfT),coef(bsw.2.surfT),coef(bsw.3.surfT),coef(bsw.4.surfT),coef(bsw.5.surfT),coef(bsw.6.surfT),
                             coef(bsw.7.surfT),coef(bsw.8.surfT),coef(bsw.9.surfT),coef(bsw.10.surfT),coef(bsw.11.surfT),coef(bsw.12.surfT)))
names(bsw.surfT) <- c("b0","wsu.surfT")
rownames(bsw.surfT) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(bsw.surfT, "~/Desktop/Workspace/Earthwatch/bsw.surfT.csv")

# BSW air based on WSU air
bsw.1.airT <- lm(microclim[microclim$month==1, 14] ~ microclim[microclim$month==1, 20]) # Jan
summary(bsw.1.airT)
bsw.2.airT <- lm(microclim[microclim$month==2, 14] ~ microclim[microclim$month==2, 20]) # Feb
summary(bsw.2.airT)
bsw.3.airT <- lm(microclim[microclim$month==3, 14] ~ microclim[microclim$month==3, 20]) # Mar
summary(bsw.3.airT)
bsw.4.airT <- lm(microclim[microclim$month==4, 14] ~ microclim[microclim$month==4, 20]) # Apr
summary(bsw.4.airT)
bsw.5.airT <- lm(microclim[microclim$month==5, 14] ~ microclim[microclim$month==5, 20]) # May
summary(bsw.5.airT)
bsw.6.airT <- lm(microclim[microclim$month==6, 14] ~ microclim[microclim$month==6, 20]) # Jun
summary(bsw.6.airT)
bsw.7.airT <- lm(microclim[microclim$month==7, 14] ~ microclim[microclim$month==7, 20]) # Jul
summary(bsw.7.airT)
bsw.8.airT <- lm(microclim[microclim$month==8, 14] ~ microclim[microclim$month==8, 20]) # Aug
summary(bsw.8.airT)
bsw.9.airT <- lm(microclim[microclim$month==9, 14] ~ microclim[microclim$month==9, 20]) # Sep
summary(bsw.9.airT)
bsw.10.airT <- lm(microclim[microclim$month==10, 14] ~ microclim[microclim$month==10, 20]) # Oct
summary(bsw.10.airT)
bsw.11.airT <- lm(microclim[microclim$month==11, 14] ~ microclim[microclim$month==11, 20]) # Nov
summary(bsw.11.airT)
bsw.12.airT <- lm(microclim[microclim$month==12, 14] ~ microclim[microclim$month==12, 20]) # Dec
summary(bsw.12.airT)

bsw.airT <- data.frame(rbind(coef(bsw.1.airT),coef(bsw.2.airT),coef(bsw.3.airT),coef(bsw.4.airT),coef(bsw.5.airT),coef(bsw.6.airT),
                              coef(bsw.7.airT),coef(bsw.8.airT),coef(bsw.9.airT),coef(bsw.10.airT),coef(bsw.11.airT),coef(bsw.12.airT)))
names(bsw.airT) <- c("b0","wsu.airT")
rownames(bsw.airT) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(bsw.airT, "~/Desktop/Workspace/Earthwatch/bsw.airT.csv")

# PPA soil based on PP
ppa.1.soil <- lm(microclim[microclim$month==1, 38] ~ microclim[microclim$month==1, 42] + microclim[microclim$month==1, 46]) # Jan
summary(ppa.1.soil)
ppa.2.soil <- lm(microclim[microclim$month==2, 38] ~ microclim[microclim$month==2, 42] + microclim[microclim$month==2, 46]) # Feb
summary(ppa.2.soil)
ppa.3.soil <- lm(microclim[microclim$month==3, 38] ~ microclim[microclim$month==3, 42] + microclim[microclim$month==3, 46]) # Mar
summary(ppa.3.soil)
ppa.4.soil <- lm(microclim[microclim$month==4, 38] ~ microclim[microclim$month==4, 42] + microclim[microclim$month==4, 46]) # Apr
summary(ppa.4.soil)
ppa.5.soil <- lm(microclim[microclim$month==5, 38] ~ microclim[microclim$month==5, 42] + microclim[microclim$month==5, 46]) # May
summary(ppa.5.soil)
ppa.6.soil <- lm(microclim[microclim$month==6, 38] ~ microclim[microclim$month==6, 42] + microclim[microclim$month==6, 46]) # Jun
summary(ppa.6.soil)
ppa.7.soil <- lm(microclim[microclim$month==7, 38] ~ microclim[microclim$month==7, 42] + microclim[microclim$month==7, 46]) # Jul
summary(ppa.7.soil)
ppa.8.soil <- lm(microclim[microclim$month==8, 38] ~ microclim[microclim$month==8, 42] + microclim[microclim$month==8, 46]) # Aug
summary(ppa.8.soil)
ppa.9.soil <- lm(microclim[microclim$month==9, 38] ~ microclim[microclim$month==9, 42] + microclim[microclim$month==9, 46]) # Sep
summary(ppa.9.soil)
ppa.10.soil <- lm(microclim[microclim$month==10, 38] ~ microclim[microclim$month==10, 42] + microclim[microclim$month==10, 46]) # Oct
summary(ppa.10.soil)
ppa.11.soil <- lm(microclim[microclim$month==11, 38] ~ microclim[microclim$month==11, 42] + microclim[microclim$month==11, 46]) # Nov
summary(ppa.11.soil)
ppa.12.soil <- lm(microclim[microclim$month==12, 38] ~ microclim[microclim$month==12, 42] + microclim[microclim$month==12, 46]) # Dec
summary(ppa.12.soil)

ppa.soilT <- data.frame(rbind(coef(ppa.1.soil),coef(ppa.2.soil),coef(ppa.3.soil),coef(ppa.4.soil),coef(ppa.5.soil),coef(ppa.6.soil),
                              coef(ppa.7.soil),coef(ppa.8.soil),coef(ppa.9.soil),coef(ppa.10.soil),coef(ppa.11.soil),coef(ppa.12.soil)))
names(ppa.soilT) <- c("b0","ppd.soilT","tun.soilT")
rownames(ppa.soilT) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(ppa.soilT, "~/Desktop/Workspace/Earthwatch/ppa.soilT.csv")

# PPA surf based on TUN surf
ppa.1.surf <- lm(microclim[microclim$month==1, 36] ~ microclim[microclim$month==1, 37]) # Jan
summary(ppa.1.surf)
ppa.2.surf <- lm(microclim[microclim$month==2, 36] ~ microclim[microclim$month==2, 37]) # Feb
summary(ppa.2.surf)
ppa.3.surf <- lm(microclim[microclim$month==3, 36] ~ microclim[microclim$month==3, 37]) # Mar
summary(ppa.3.surf)
ppa.4.surf <- lm(microclim[microclim$month==4, 36] ~ microclim[microclim$month==4, 37]) # Apr
summary(ppa.4.surf)
ppa.5.surf <- lm(microclim[microclim$month==5, 36] ~ microclim[microclim$month==5, 37]) # May
summary(ppa.5.surf)
ppa.6.surf <- lm(microclim[microclim$month==6, 36] ~ microclim[microclim$month==6, 37]) # Jun
summary(ppa.6.surf)
ppa.7.surf <- lm(microclim[microclim$month==7, 36] ~ microclim[microclim$month==7, 37]) # Jul
summary(ppa.7.surf)
ppa.8.surf <- lm(microclim[microclim$month==8, 36] ~ microclim[microclim$month==8, 37]) # Aug
summary(ppa.8.surf)
ppa.9.surf <- lm(microclim[microclim$month==9, 36] ~ microclim[microclim$month==9, 37]) # Sep
summary(ppa.9.surf)
ppa.10.surf <- lm(microclim[microclim$month==10, 36] ~ microclim[microclim$month==10, 37]) # Oct
summary(ppa.10.surf)
ppa.11.surf <- lm(microclim[microclim$month==11, 36] ~ microclim[microclim$month==11, 37]) # Nov
summary(ppa.11.surf)
ppa.12.surf <- lm(microclim[microclim$month==12, 36] ~ microclim[microclim$month==12, 37]) # Dec
summary(ppa.12.surf)

ppa.surfT <- data.frame(rbind(coef(ppa.1.surf),coef(ppa.2.surf),coef(ppa.3.surf),coef(ppa.4.surf),coef(ppa.5.surf),coef(ppa.6.surf),
                              coef(ppa.7.surf),coef(ppa.8.surf),coef(ppa.9.surf),coef(ppa.10.surf),coef(ppa.11.surf),coef(ppa.12.surf)))
names(ppa.surfT) <- c("b0","ppa.airT")
rownames(ppa.surfT) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(ppa.surfT, "~/Desktop/Workspace/Earthwatch/ppa.surfT.csv")

# TUN based on PPD
tun.1.surf <- lm(microclim[microclim$month==1, 44] ~ microclim[microclim$month==1, 45]) # Jan
summary(tun.1.surf)
tun.2.surf <- lm(microclim[microclim$month==2, 44] ~ microclim[microclim$month==2, 45]) # Feb
summary(tun.2.surf)
tun.3.surf <- lm(microclim[microclim$month==3, 44] ~ microclim[microclim$month==3, 45]) # Mar
summary(tun.3.surf)
tun.4.surf <- lm(microclim[microclim$month==4, 44] ~ microclim[microclim$month==4, 45]) # Apr
summary(tun.4.surf)
tun.5.surf <- lm(microclim[microclim$month==5, 44] ~ microclim[microclim$month==5, 45]) # May
summary(tun.5.surf)
tun.6.surf <- lm(microclim[microclim$month==6, 44] ~ microclim[microclim$month==6, 45]) # Jun
summary(tun.6.surf)
tun.7.surf <- lm(microclim[microclim$month==7, 44] ~ microclim[microclim$month==7, 45]) # Jul
summary(tun.7.surf)
tun.8.surf <- lm(microclim[microclim$month==8, 44] ~ microclim[microclim$month==8, 45]) # Aug
summary(tun.8.surf)
tun.9.surf <- lm(microclim[microclim$month==9, 44] ~ microclim[microclim$month==9, 45]) # Sep
summary(tun.9.surf)
tun.10.surf <- lm(microclim[microclim$month==10, 44] ~ microclim[microclim$month==10, 45]) # Oct
summary(tun.10.surf)
tun.11.surf <- lm(microclim[microclim$month==11, 44] ~ microclim[microclim$month==11, 45]) # Nov
summary(tun.11.surf)
tun.12.surf <- lm(microclim[microclim$month==12, 44] ~ microclim[microclim$month==12, 45]) # Dec
summary(tun.12.surf)

tun.surfT <- data.frame(rbind(coef(tun.1.surf),coef(tun.2.surf),coef(tun.3.surf),coef(tun.4.surf),coef(tun.5.surf),coef(tun.6.surf),
                             coef(tun.7.surf),coef(tun.8.surf),coef(tun.9.surf),coef(tun.10.surf),coef(tun.11.surf),coef(tun.12.surf)))
names(tun.surfT) <- c("b0","tun.surfT")
rownames(tun.surfT) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(tun.surfT, "~/Desktop/Workspace/Earthwatch/tun.surfT.csv")

# Looked at lagged correlations between air temperature and temps at -80 cm
ccf(microclim[microclim$month==1, 46], microclim[microclim$month==1, 45], na.action = na.exclude)
ccf(microclim[microclim$month==1, 47], microclim[microclim$month==1, 45], na.action = na.exclude)
# The correlation is greatest at a lag of 0

# TUN based on PPD
tun.1.soil <- lm(microclim[microclim$month==1, 46] ~ microclim[microclim$month==1, 45] + microclim[microclim$month==1, 42] + microclim[microclim$month==1, 35]) # Jan
summary(tun.1.soil)
tun.2.soil <- lm(microclim[microclim$month==2, 46] ~  microclim[microclim$month==2, 45] + microclim[microclim$month==2, 42] + microclim[microclim$month==2, 35]) # Feb
summary(tun.2.soil)
tun.3.soil <- lm(microclim[microclim$month==3, 46] ~  microclim[microclim$month==3, 45] + microclim[microclim$month==3, 42] + microclim[microclim$month==3, 35]) # Mar
summary(tun.3.soil)
tun.4.soil <- lm(microclim[microclim$month==4, 46] ~  microclim[microclim$month==4, 45] + microclim[microclim$month==4, 42] + microclim[microclim$month==4, 35]) # Apr
summary(tun.4.soil)
tun.5.soil <- lm(microclim[microclim$month==5, 46] ~  microclim[microclim$month==5, 45] + microclim[microclim$month==5, 42] + microclim[microclim$month==5, 35]) # May
summary(tun.5.soil)
tun.6.soil <- lm(microclim[microclim$month==6, 46] ~  microclim[microclim$month==6, 45] + microclim[microclim$month==6, 42] + microclim[microclim$month==6, 35]) # Jun
summary(tun.6.soil)
tun.7.soil <- lm(microclim[microclim$month==7, 46] ~  microclim[microclim$month==7, 45] + microclim[microclim$month==7, 42] + microclim[microclim$month==7, 35]) # Jul
summary(tun.7.soil)
tun.8.soil <- lm(microclim[microclim$month==8, 46] ~  microclim[microclim$month==8, 45] + microclim[microclim$month==8, 42] + microclim[microclim$month==8, 35]) # Aug
summary(tun.8.soil)
tun.9.soil <- lm(microclim[microclim$month==9, 46] ~  microclim[microclim$month==9, 45] + microclim[microclim$month==9, 42] + microclim[microclim$month==9, 35]) # Sep
summary(tun.9.soil)
tun.10.soil <- lm(microclim[microclim$month==10, 46] ~  microclim[microclim$month==10, 45] + microclim[microclim$month==10, 42] + microclim[microclim$month==10, 35]) # Oct
summary(tun.10.soil)
tun.11.soil <- lm(microclim[microclim$month==11, 46] ~  microclim[microclim$month==11, 45] + microclim[microclim$month==11, 42] + microclim[microclim$month==11, 35]) # Nov
summary(tun.11.soil)
tun.12.soil <- lm(microclim[microclim$month==12, 46] ~  microclim[microclim$month==12, 45] + microclim[microclim$month==12, 42] + microclim[microclim$month==12, 35]) # Dec
summary(tun.12.soil)

tun.soilT <- data.frame(rbind(coef(tun.1.soil),coef(tun.2.soil),coef(tun.3.soil),coef(tun.4.soil),coef(tun.5.soil),coef(tun.6.soil),
                             coef(tun.7.soil),coef(tun.8.soil),coef(tun.9.soil),coef(tun.10.soil),coef(tun.11.soil),coef(tun.12.soil)))
names(tun.soilT) <- c("b0","tun.soilT","ppd.soil","bwp.soil")
rownames(tun.soilT) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(tun.soilT, "~/Desktop/Workspace/Earthwatch/tun.soilT.csv")

# TUN based on PPD
tun.1.air <- lm(microclim[microclim$month==1, 45] ~ microclim[microclim$month==1, 41]) # Jan
summary(tun.1.air)
tun.2.air <- lm(microclim[microclim$month==2, 45] ~ microclim[microclim$month==2, 41]) # Feb
summary(tun.2.air)
tun.3.air <- lm(microclim[microclim$month==3, 45] ~ microclim[microclim$month==3, 41]) # Mar
summary(tun.3.air)
tun.4.air <- lm(microclim[microclim$month==4, 45] ~ microclim[microclim$month==4, 41]) # Apr
summary(tun.4.air)
tun.5.air <- lm(microclim[microclim$month==5, 45] ~ microclim[microclim$month==5, 41]) # May
summary(tun.5.air)
tun.6.air <- lm(microclim[microclim$month==6, 45] ~ microclim[microclim$month==6, 41]) # Jun
summary(tun.6.air)
tun.7.air <- lm(microclim[microclim$month==7, 45] ~ microclim[microclim$month==7, 41]) # Jul
summary(tun.7.air)
tun.8.air <- lm(microclim[microclim$month==8, 45] ~ microclim[microclim$month==8, 41]) # Aug
summary(tun.8.air)
tun.9.air <- lm(microclim[microclim$month==9, 45] ~ microclim[microclim$month==9, 41]) # Sep
summary(tun.9.air)
tun.10.air <- lm(microclim[microclim$month==10, 45] ~ microclim[microclim$month==10, 41]) # Oct
summary(tun.10.air)
tun.11.air <- lm(microclim[microclim$month==11, 45] ~ microclim[microclim$month==11, 41]) # Nov
summary(tun.11.air)
tun.12.air <- lm(microclim[microclim$month==12, 45] ~ microclim[microclim$month==12, 41]) # Dec
summary(tun.12.air)

tun.airT <- data.frame(rbind(coef(tun.1.air),coef(tun.2.air),coef(tun.3.air),coef(tun.4.air),coef(tun.5.air),coef(tun.6.air),
                            coef(tun.7.air),coef(tun.8.air),coef(tun.9.air),coef(tun.10.air),coef(tun.11.air),coef(tun.12.air)))
names(tun.airT) <- c("b0","tun.airT")
rownames(tun.airT) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(tun.airT, "~/Desktop/Workspace/Earthwatch/tun.airT.csv")

## EC based on TUN
ec.1.air <- lm(microclim[microclim$month==1, 49] ~ microclim[microclim$month==1, 45]) # Jan
summary(ec.1.air)
ec.2.air <- lm(microclim[microclim$month==2, 49] ~ microclim[microclim$month==2, 45]) # Feb
summary(ec.2.air)
ec.3.air <- lm(microclim[microclim$month==3, 49] ~ microclim[microclim$month==3, 45]) # Mar
summary(ec.3.air)
ec.4.air <- lm(microclim[microclim$month==4, 49] ~ microclim[microclim$month==4, 45]) # Apr
summary(ec.4.air)
ec.5.air <- lm(microclim[microclim$month==5, 49] ~ microclim[microclim$month==5, 45]) # May
summary(ec.5.air)
ec.6.air <- lm(microclim[microclim$month==6, 49] ~ microclim[microclim$month==6, 45]) # Jun
summary(ec.6.air)
ec.7.air <- lm(microclim[microclim$month==7, 49] ~ microclim[microclim$month==7, 45]) # Jul
summary(ec.7.air)
ec.8.air <- lm(microclim[microclim$month==8, 49] ~ microclim[microclim$month==8, 45]) # Aug
summary(ec.8.air)
ec.9.air <- lm(microclim[microclim$month==9, 49] ~ microclim[microclim$month==9, 45]) # Sep
summary(ec.9.air)
ec.10.air <- lm(microclim[microclim$month==10, 49] ~ microclim[microclim$month==10, 45]) # Oct
summary(ec.10.air)
ec.11.air <- lm(microclim[microclim$month==11, 49] ~ microclim[microclim$month==11, 45]) # Nov
summary(ec.11.air)
ec.12.air <- lm(microclim[microclim$month==12, 49] ~ microclim[microclim$month==12, 45]) # Dec
summary(ec.12.air)

ec.airT <- data.frame(rbind(coef(ec.1.air),coef(ec.2.air),coef(ec.3.air),coef(ec.4.air),coef(ec.5.air),coef(ec.6.air),
                             coef(ec.7.air),coef(ec.8.air),coef(ec.9.air),coef(ec.10.air),coef(ec.11.air),coef(ec.12.air)))
names(ec.airT) <- c("b0","ec.airT")
rownames(ec.airT) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(ec.airT, "~/Desktop/Workspace/Earthwatch/ec.airT.csv")

## EC based on BWP
ec.1.air <- lm(microclim[microclim$month==1, 49] ~ microclim[microclim$month==1, 34]) # Jan
summary(ec.1.air)
ec.2.air <- lm(microclim[microclim$month==2, 49] ~ microclim[microclim$month==2, 34]) # Feb
summary(ec.2.air)
ec.3.air <- lm(microclim[microclim$month==3, 49] ~ microclim[microclim$month==3, 34]) # Mar
summary(ec.3.air)
ec.4.air <- lm(microclim[microclim$month==4, 49] ~ microclim[microclim$month==4, 34]) # Apr
summary(ec.4.air)
ec.5.air <- lm(microclim[microclim$month==5, 49] ~ microclim[microclim$month==5, 34]) # May
summary(ec.5.air)
ec.6.air <- lm(microclim[microclim$month==6, 49] ~ microclim[microclim$month==6, 34]) # Jun
summary(ec.6.air)
ec.7.air <- lm(microclim[microclim$month==7, 49] ~ microclim[microclim$month==7, 34]) # Jul
summary(ec.7.air)
ec.8.air <- lm(microclim[microclim$month==8, 49] ~ microclim[microclim$month==8, 34]) # Aug
summary(ec.8.air)
ec.9.air <- lm(microclim[microclim$month==9, 49] ~ microclim[microclim$month==9, 34]) # Sep
summary(ec.9.air)
ec.10.air <- lm(microclim[microclim$month==10, 49] ~ microclim[microclim$month==10, 34]) # Oct
summary(ec.10.air)
ec.11.air <- lm(microclim[microclim$month==11, 49] ~ microclim[microclim$month==11, 34]) # Nov
summary(ec.11.air)
ec.12.air <- lm(microclim[microclim$month==12, 49] ~ microclim[microclim$month==12, 34]) # Dec
summary(ec.12.air)

ec.airT <- data.frame(rbind(coef(ec.1.air),coef(ec.2.air),coef(ec.3.air),coef(ec.4.air),coef(ec.5.air),coef(ec.6.air),
                            coef(ec.7.air),coef(ec.8.air),coef(ec.9.air),coef(ec.10.air),coef(ec.11.air),coef(ec.12.air)))
names(ec.airT) <- c("b0","ec.airT")
rownames(ec.airT) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(ec.airT, "~/Desktop/Workspace/Earthwatch/ec.airT2.csv")

### PPA based on EC
ppa.1.air <- lm(microclim[microclim$month==1, 37] ~ microclim[microclim$month==1, 49]) # Jan
summary(ppa.1.air)
ppa.2.air <- lm(microclim[microclim$month==2, 37] ~ microclim[microclim$month==2, 49]) # Jan
summary(ppa.2.air)
ppa.3.air <- lm(microclim[microclim$month==3, 37] ~ microclim[microclim$month==3, 49]) # Jan
summary(ppa.3.air)
ppa.4.air <- lm(microclim[microclim$month==4, 37] ~ microclim[microclim$month==4, 49]) # Jan
summary(ppa.4.air)
ppa.5.air <- lm(microclim[microclim$month==5, 37] ~ microclim[microclim$month==5, 49]) # Jan
summary(ppa.5.air)
ppa.6.air <- lm(microclim[microclim$month==6, 37] ~ microclim[microclim$month==6, 49]) # Jan
summary(ppa.6.air)
ppa.7.air <- lm(microclim[microclim$month==7, 37] ~ microclim[microclim$month==7, 49]) # Jan
summary(ppa.7.air)
ppa.8.air <- lm(microclim[microclim$month==8, 37] ~ microclim[microclim$month==8, 49]) # Jan
summary(ppa.8.air)
ppa.9.air <- lm(microclim[microclim$month==9, 37] ~ microclim[microclim$month==9, 49]) # Jan
summary(ppa.9.air)
ppa.10.air <- lm(microclim[microclim$month==10, 37] ~ microclim[microclim$month==10, 49]) # Jan
summary(ppa.10.air)
ppa.11.air <- lm(microclim[microclim$month==11, 37] ~ microclim[microclim$month==11, 49]) # Jan
summary(ppa.11.air)
ppa.12.air <- lm(microclim[microclim$month==12, 37] ~ microclim[microclim$month==12, 49]) # Jan
summary(ppa.12.air)

ppa.airT <- data.frame(rbind(coef(ppa.1.air),coef(ppa.2.air),coef(ppa.3.air),coef(ppa.4.air),coef(ppa.5.air),coef(ppa.6.air),
                             coef(ppa.7.air),coef(ppa.8.air),coef(ppa.9.air),coef(ppa.10.air),coef(ppa.11.air),coef(ppa.12.air)))
names(ppa.airT) <- c("b0","ppa.airT")
rownames(ppa.airT) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(ppa.airT, "~/Desktop/Workspace/Earthwatch/ppa.airT.csv")


### BWP surface based on BWP air
bwp.1.surf <- lm(microclim[microclim$month==1, 33] ~ microclim[microclim$month==1, 34]) # Jan
summary(bwp.1.surf)
bwp.2.surf <- lm(microclim[microclim$month==2, 33] ~ microclim[microclim$month==2, 34]) # Jan
summary(bwp.2.surf)
bwp.3.surf <- lm(microclim[microclim$month==3, 33] ~ microclim[microclim$month==3, 34]) # Jan
summary(bwp.3.surf)
bwp.4.surf <- lm(microclim[microclim$month==4, 33] ~ microclim[microclim$month==4, 34]) # Jan
summary(bwp.4.surf)
bwp.5.surf <- lm(microclim[microclim$month==5, 33] ~ microclim[microclim$month==5, 34]) # Jan
summary(bwp.5.surf)
bwp.6.surf <- lm(microclim[microclim$month==6, 33] ~ microclim[microclim$month==6, 34]) # Jan
summary(bwp.6.surf)
bwp.7.surf <- lm(microclim[microclim$month==7, 33] ~ microclim[microclim$month==7, 34]) # Jan
summary(bwp.7.surf)
bwp.8.surf <- lm(microclim[microclim$month==8, 33] ~ microclim[microclim$month==8, 34]) # Jan
summary(bwp.8.surf)
bwp.9.surf <- lm(microclim[microclim$month==9, 33] ~ microclim[microclim$month==9, 34]) # Jan
summary(bwp.9.surf)
bwp.10.surf <- lm(microclim[microclim$month==10, 33] ~ microclim[microclim$month==10, 34]) # Jan
summary(bwp.10.surf)
bwp.11.surf <- lm(microclim[microclim$month==11, 33] ~ microclim[microclim$month==11, 34]) # Jan
summary(bwp.11.surf)
bwp.12.surf <- lm(microclim[microclim$month==12, 33] ~ microclim[microclim$month==12, 34]) # Jan
summary(bwp.12.surf)

bwp.surfT <- data.frame(rbind(coef(bwp.1.surf),coef(bwp.2.surf),coef(bwp.3.surf),coef(bwp.4.surf),coef(bwp.5.surf),coef(bwp.6.surf),
                             coef(bwp.7.surf),coef(bwp.8.surf),coef(bwp.9.surf),coef(bwp.10.surf),coef(bwp.11.surf),coef(bwp.12.surf)))
names(bwp.surfT) <- c("b0","bwp.surfT")
rownames(bwp.surfT) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(bwp.surfT, "~/Desktop/Workspace/Earthwatch/bwp.surfT.csv")

### Tundra based on WSU
wsu.tun <- read.csv(file = "~/Desktop/Workspace/Earthwatch/WSU.TUN.csv", header = TRUE)

# Air
tun.1.air <- lm(wsu.tun[wsu.tun$Month==1, 5] ~ wsu.tun[wsu.tun$Month==1, 4]) # Jan
summary(tun.1.air)
tun.2.air <- lm(wsu.tun[wsu.tun$Month==2, 5] ~ wsu.tun[wsu.tun$Month==2, 4]) # Feb
summary(tun.2.air)
tun.3.air <- lm(wsu.tun[wsu.tun$Month==3, 5] ~ wsu.tun[wsu.tun$Month==3, 4]) # Mar
summary(tun.3.air)
tun.4.air <- lm(wsu.tun[wsu.tun$Month==4, 5] ~ wsu.tun[wsu.tun$Month==4, 4]) # Apr
summary(tun.4.air)
tun.5.air <- lm(wsu.tun[wsu.tun$Month==5, 5] ~ wsu.tun[wsu.tun$Month==5, 4]) # May
summary(tun.5.air)
tun.6.air <- lm(wsu.tun[wsu.tun$Month==6, 5] ~ wsu.tun[wsu.tun$Month==6, 4]) # Jun
summary(tun.6.air)
tun.7.air <- lm(wsu.tun[wsu.tun$Month==7, 5] ~ wsu.tun[wsu.tun$Month==7, 4]) # Jul
summary(tun.7.air)
tun.8.air <- lm(wsu.tun[wsu.tun$Month==8, 5] ~ wsu.tun[wsu.tun$Month==8, 4]) # Aug
summary(tun.8.air)
tun.9.air <- lm(wsu.tun[wsu.tun$Month==9, 5] ~ wsu.tun[wsu.tun$Month==9, 4]) # Sep
summary(tun.9.air)
tun.10.air <- lm(wsu.tun[wsu.tun$Month==10, 5] ~ wsu.tun[wsu.tun$Month==10, 4]) # Oct
summary(tun.10.air)
tun.11.air <- lm(wsu.tun[wsu.tun$Month==11, 5] ~ wsu.tun[wsu.tun$Month==11, 4]) # Nov
summary(tun.11.air)
tun.12.air <- lm(wsu.tun[wsu.tun$Month==12, 5] ~ wsu.tun[wsu.tun$Month==12, 4]) # Dec
summary(tun.12.air)

# Ground
tun.1.grd <- lm(wsu.tun[wsu.tun$Month==1, 6] ~ wsu.tun[wsu.tun$Month==1, 5]) # Jan
summary(tun.1.grd)
tun.2.grd <- lm(wsu.tun[wsu.tun$Month==2, 6] ~ wsu.tun[wsu.tun$Month==2, 5]) # Feb
summary(tun.2.grd)
tun.3.grd <- lm(wsu.tun[wsu.tun$Month==3, 6] ~ wsu.tun[wsu.tun$Month==3, 5]) # Mar
summary(tun.3.grd)
tun.4.grd <- lm(wsu.tun[wsu.tun$Month==4, 6] ~ wsu.tun[wsu.tun$Month==4, 5]) # Apr
summary(tun.4.grd)
tun.5.grd <- lm(wsu.tun[wsu.tun$Month==5, 6] ~ wsu.tun[wsu.tun$Month==5, 5]) # May
summary(tun.5.grd)
tun.6.grd <- lm(wsu.tun[wsu.tun$Month==6, 6] ~ wsu.tun[wsu.tun$Month==6, 5]) # Jun
summary(tun.6.grd)
tun.7.grd <- lm(wsu.tun[wsu.tun$Month==7, 6] ~ wsu.tun[wsu.tun$Month==7, 5]) # Jul
summary(tun.7.grd)
tun.8.grd <- lm(wsu.tun[wsu.tun$Month==8, 6] ~ wsu.tun[wsu.tun$Month==8, 5]) # Aug
summary(tun.8.grd)
tun.9.grd <- lm(wsu.tun[wsu.tun$Month==9, 6] ~ wsu.tun[wsu.tun$Month==9, 5]) # Sep
summary(tun.9.grd)
tun.10.grd <- lm(wsu.tun[wsu.tun$Month==10, 6] ~ wsu.tun[wsu.tun$Month==10, 5]) # Oct
summary(tun.10.grd)
tun.11.grd <- lm(wsu.tun[wsu.tun$Month==11, 6] ~ wsu.tun[wsu.tun$Month==11, 5]) # Nov
summary(tun.11.grd)
tun.12.grd <- lm(wsu.tun[wsu.tun$Month==12, 6] ~ wsu.tun[wsu.tun$Month==12, 5]) # Dec
summary(tun.12.grd)

# Soil
tun.1.soil <- lm(wsu.tun[wsu.tun$Month==1, 7] ~ wsu.tun[wsu.tun$Month==1, 6]) # Jan
summary(tun.1.soil)
tun.2.soil <- lm(wsu.tun[wsu.tun$Month==2, 7] ~ wsu.tun[wsu.tun$Month==2, 6]) # Feb
summary(tun.2.soil)
tun.3.soil <- lm(wsu.tun[wsu.tun$Month==3, 7] ~ wsu.tun[wsu.tun$Month==3, 6]) # Mar
summary(tun.3.soil)
tun.4.soil <- lm(wsu.tun[wsu.tun$Month==4, 7] ~ wsu.tun[wsu.tun$Month==4, 6]) # Apr
summary(tun.4.soil)
tun.5.soil <- lm(wsu.tun[wsu.tun$Month==5, 7] ~ wsu.tun[wsu.tun$Month==5, 6]) # May
summary(tun.5.soil)
tun.6.soil <- lm(wsu.tun[wsu.tun$Month==6, 7] ~ wsu.tun[wsu.tun$Month==6, 6]) # Jun
summary(tun.6.soil)
tun.7.soil <- lm(wsu.tun[wsu.tun$Month==7, 7] ~ wsu.tun[wsu.tun$Month==7, 6]) # Jul
summary(tun.7.soil)
tun.8.soil <- lm(wsu.tun[wsu.tun$Month==8, 7] ~ wsu.tun[wsu.tun$Month==8, 6]) # Aug
summary(tun.8.soil)
tun.9.soil <- lm(wsu.tun[wsu.tun$Month==9, 7] ~ wsu.tun[wsu.tun$Month==9, 6]) # Sep
summary(tun.9.soil)
tun.10.soil <- lm(wsu.tun[wsu.tun$Month==10, 7] ~ wsu.tun[wsu.tun$Month==10, 6]) # Oct
summary(tun.10.soil)
tun.11.soil <- lm(wsu.tun[wsu.tun$Month==11, 7] ~ wsu.tun[wsu.tun$Month==11, 6]) # Nov
summary(tun.11.soil)
tun.12.soil <- lm(wsu.tun[wsu.tun$Month==12, 7] ~ wsu.tun[wsu.tun$Month==12, 6]) # Dec
summary(tun.12.soil)

tun.airT <- data.frame(rbind(coef(tun.1.air),coef(tun.2.air),coef(tun.3.air),coef(tun.4.air),coef(tun.5.air),coef(tun.6.air),
                            coef(tun.7.air),coef(tun.8.air),coef(tun.9.air),coef(tun.10.air),coef(tun.11.air),coef(tun.12.air)))
names(tun.airT) <- c("b0","wsu.airT")
rownames(tun.airT) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(tun.airT, "~/Desktop/Workspace/Earthwatch/tun.airT.csv")

tun.grdT <- data.frame(rbind(coef(tun.1.grd),coef(tun.2.grd),coef(tun.3.grd),coef(tun.4.grd),coef(tun.5.grd),coef(tun.6.grd),
                             coef(tun.7.grd),coef(tun.8.grd),coef(tun.9.grd),coef(tun.10.grd),coef(tun.11.grd),coef(tun.12.grd)))
names(tun.grdT) <- c("b0","tun.airT")
rownames(tun.grdT) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(tun.grdT, "~/Desktop/Workspace/Earthwatch/tun.grdT.csv")

tun.soilT <- data.frame(rbind(coef(tun.1.soil),coef(tun.2.soil),coef(tun.3.soil),coef(tun.4.soil),coef(tun.5.soil),coef(tun.6.soil),
                             coef(tun.7.soil),coef(tun.8.soil),coef(tun.9.soil),coef(tun.10.soil),coef(tun.11.soil),coef(tun.12.soil)))
names(tun.soilT) <- c("b0","tun.airT")
rownames(tun.soilT) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(tun.soilT, "~/Desktop/Workspace/Earthwatch/tun.soilT.csv")





### RID based on WSU
rid.wsu <- read.csv(file = "~/Desktop/Workspace/Earthwatch/rid.soil.csv", header = TRUE)

# Soil
rid.1.soil <- lm(rid.wsu[rid.wsu$Month==1, 16] ~ rid.wsu[rid.wsu$Month==1, 15]) # Jan
summary(rid.1.soil)
rid.2.soil <- lm(rid.wsu[rid.wsu$Month==2, 16] ~ rid.wsu[rid.wsu$Month==2, 15]) # Feb
summary(rid.2.soil)
rid.3.soil <- lm(rid.wsu[rid.wsu$Month==3, 16] ~ rid.wsu[rid.wsu$Month==3, 15]) # Mar
summary(rid.3.soil)
rid.4.soil <- lm(rid.wsu[rid.wsu$Month==4, 16] ~ rid.wsu[rid.wsu$Month==4, 15]) # Apr
summary(rid.4.soil)
rid.5.soil <- lm(rid.wsu[rid.wsu$Month==5, 16] ~ rid.wsu[rid.wsu$Month==5, 15]) # May
summary(rid.5.soil)
rid.6.soil <- lm(rid.wsu[rid.wsu$Month==6, 16] ~ rid.wsu[rid.wsu$Month==6, 15]) # Jun
summary(rid.6.soil)
rid.7.soil <- lm(rid.wsu[rid.wsu$Month==7, 16] ~ rid.wsu[rid.wsu$Month==7, 15]) # Jul
summary(rid.7.soil)
rid.8.soil <- lm(rid.wsu[rid.wsu$Month==8, 16] ~ rid.wsu[rid.wsu$Month==8, 15]) # Aug
summary(rid.8.soil)
rid.9.soil <- lm(rid.wsu[rid.wsu$Month==9, 16] ~ rid.wsu[rid.wsu$Month==9, 15]) # Sep
summary(rid.9.soil)
rid.10.soil <- lm(rid.wsu[rid.wsu$Month==10, 16] ~ rid.wsu[rid.wsu$Month==10, 15]) # Oct
summary(rid.10.soil)
rid.11.soil <- lm(rid.wsu[rid.wsu$Month==11, 16] ~ rid.wsu[rid.wsu$Month==11, 15]) # Nov
summary(rid.11.soil)
rid.12.soil <- lm(rid.wsu[rid.wsu$Month==12, 16] ~ rid.wsu[rid.wsu$Month==12, 15]) # Dec
summary(rid.12.soil)

rid.soilT <- data.frame(rbind(coef(rid.1.soil),coef(rid.2.soil),coef(rid.3.soil),coef(rid.4.soil),coef(rid.5.soil),coef(rid.6.soil),
                              coef(rid.7.soil),coef(rid.8.soil),coef(rid.9.soil),coef(rid.10.soil),coef(rid.11.soil),coef(rid.12.soil)))
names(rid.soilT) <- c("b0","wsu.soilT")
rownames(rid.soilT) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(rid.soilT, "~/Desktop/Workspace/Earthwatch/rid.soilT.csv")

################################################################################################################################################################################
################################################################################################################################################################################
################################################################################################################################################################################
################################################################################################################################################################################

## Mac Pass

### BP soil surface based on HF soil surface
macpass <- read.csv(file = "~/Desktop/Workspace/Earthwatch/microclimate.mm.csv", header = TRUE)

# Ground surface temperature (0 cm)
bp.1.soil <- lm(macpass[macpass$Month==1, 6] ~ macpass[macpass$Month==1, 9]) # Jan
summary(bp.1.soil)
bp.2.soil <- lm(macpass[macpass$Month==2, 6] ~ macpass[macpass$Month==2, 9]) # Feb
summary(bp.2.soil)
bp.3.soil <- lm(macpass[macpass$Month==3, 6] ~ macpass[macpass$Month==3, 9]) # Mar
summary(bp.3.soil)
bp.4.soil <- lm(macpass[macpass$Month==4, 6] ~ macpass[macpass$Month==4, 9]) # Apr
summary(bp.4.soil)
bp.5.soil <- lm(macpass[macpass$Month==5, 6] ~ macpass[macpass$Month==5, 9]) # May
summary(bp.5.soil)
bp.6.soil <- lm(macpass[macpass$Month==6, 6] ~ macpass[macpass$Month==6, 9]) # Jun
summary(bp.6.soil)
bp.7.soil <- lm(macpass[macpass$Month==7, 6] ~ macpass[macpass$Month==7, 9]) # Jul
summary(bp.7.soil)
bp.8.soil <- lm(macpass[macpass$Month==8, 6] ~ macpass[macpass$Month==8, 9]) # Aug
summary(bp.8.soil)
bp.9.soil <- lm(macpass[macpass$Month==9, 6] ~ macpass[macpass$Month==9, 9]) # Sep
summary(bp.9.soil)
bp.10.soil <- lm(macpass[macpass$Month==10, 6] ~ macpass[macpass$Month==10, 9]) # Oct
summary(bp.10.soil)
bp.11.soil <- lm(macpass[macpass$Month==11, 6] ~ macpass[macpass$Month==11, 9]) # Nov
summary(bp.11.soil)
bp.12.soil <- lm(macpass[macpass$Month==12, 6] ~ macpass[macpass$Month==12, 9]) # Dec
summary(bp.12.soil)

bp.soilT <- data.frame(rbind(coef(bp.1.soil),coef(bp.2.soil),coef(bp.3.soil),coef(bp.4.soil),coef(bp.5.soil),coef(bp.6.soil),
                             coef(bp.7.soil),coef(bp.8.soil),coef(bp.9.soil),coef(bp.10.soil),coef(bp.11.soil),coef(bp.12.soil)))
names(bp.soilT) <- c("b0","hf.grdT")
rownames(bp.soilT) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(bp.soilT, "~/Desktop/bp.soilT.csv")

### HF permafrost based on BP soil surface
hf.1.permafrost <- lm(macpass[macpass$Month==1, 10] 
                      ~ macpass[macpass$Month==1, 7] + 
                        macpass[macpass$Month==1, 13] + 
                        macpass[macpass$Month==1, 16]) # Jan
summary(hf.1.permafrost)
hf.2.permafrost <- lm(macpass[macpass$Month==2, 10]
                      ~ macpass[macpass$Month==2, 7] + 
                        macpass[macpass$Month==2, 13] + 
                        macpass[macpass$Month==2, 16]) # Feb
summary(hf.2.permafrost)
hf.3.permafrost <- lm(macpass[macpass$Month==3, 10]
                      ~ macpass[macpass$Month==3, 7] + 
                        macpass[macpass$Month==3, 13] + 
                        macpass[macpass$Month==3, 16]) # Mar
summary(hf.3.permafrost)
hf.4.permafrost <- lm(macpass[macpass$Month==4, 10]
                      ~ macpass[macpass$Month==4, 7] + 
                        macpass[macpass$Month==4, 13] + 
                        macpass[macpass$Month==4, 16]) # Apr
summary(hf.4.permafrost)
hf.5.permafrost <- lm(macpass[macpass$Month==5, 10]
                      ~ macpass[macpass$Month==5, 7] + 
                        macpass[macpass$Month==5, 13] + 
                        macpass[macpass$Month==5, 16]) # May
summary(hf.5.permafrost)
hf.6.permafrost <- lm(macpass[macpass$Month==6, 10]
                      ~ macpass[macpass$Month==6, 7] + 
                        macpass[macpass$Month==6, 13] + 
                        macpass[macpass$Month==6, 16]) # Jun
summary(hf.6.permafrost)
hf.7.permafrost <- lm(macpass[macpass$Month==7, 10]
                      ~ macpass[macpass$Month==7, 7] + 
                        macpass[macpass$Month==7, 13] + 
                        macpass[macpass$Month==7, 16]) # Jul
summary(hf.7.permafrost)
hf.8.permafrost <- lm(macpass[macpass$Month==8, 10]
                      ~ macpass[macpass$Month==8, 7] + 
                        macpass[macpass$Month==8, 13] + 
                        macpass[macpass$Month==8, 16]) # Aug
summary(hf.8.permafrost)
hf.9.permafrost <- lm(macpass[macpass$Month==9, 10]
                      ~ macpass[macpass$Month==9, 7] + 
                        macpass[macpass$Month==9, 13] + 
                        macpass[macpass$Month==9, 16]) # Sep
summary(hf.9.permafrost)
hf.10.permafrost <- lm(macpass[macpass$Month==10, 10]
                       ~ macpass[macpass$Month==10, 7] + 
                         macpass[macpass$Month==10, 13] + 
                         macpass[macpass$Month==10, 16]) # Oct
summary(hf.10.permafrost)
hf.11.permafrost <- lm(macpass[macpass$Month==11, 10]
                       ~ macpass[macpass$Month==11, 7] + 
                         macpass[macpass$Month==11, 13] + 
                         macpass[macpass$Month==11, 16]) # Nov
summary(hf.11.permafrost)
hf.12.permafrost <- lm(macpass[macpass$Month==12, 10]
                       ~ macpass[macpass$Month==12, 7] + 
                         macpass[macpass$Month==12, 13] + 
                         macpass[macpass$Month==12, 16]) # Dec
summary(hf.12.permafrost)

hf.permafrostT <- data.frame(rbind(coef(hf.1.permafrost),coef(hf.2.permafrost),coef(hf.3.permafrost),coef(hf.4.permafrost),coef(hf.5.permafrost),coef(hf.6.permafrost),
                             coef(hf.7.permafrost),coef(hf.8.permafrost),coef(hf.9.permafrost),coef(hf.10.permafrost),coef(hf.11.permafrost),coef(hf.12.permafrost)))
names(hf.permafrostT) <- c("b0","bp.prmT","d2.prmT","d6.prmT")
rownames(hf.permafrostT) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(hf.permafrostT, "~/Desktop/hf.permafrostT.csv")


### GF based on HF
macpass <- read.csv(file = "~/Desktop/Workspace/Earthwatch/microclimate.mm.csv", header = TRUE)

# Ground subsruface temperature (-5 cm)
gf.1.soil <- lm(macpass[macpass$Month==1, 8] ~ macpass[macpass$Month==1, 7]) # Jan
summary(gf.1.soil)
gf.2.soil <- lm(macpass[macpass$Month==2, 8] ~ macpass[macpass$Month==2, 7]) # Feb
summary(gf.2.soil)
gf.3.soil <- lm(macpass[macpass$Month==3, 8] ~ macpass[macpass$Month==3, 7]) # Mar
summary(gf.3.soil)
gf.4.soil <- lm(macpass[macpass$Month==4, 8] ~ macpass[macpass$Month==4, 7]) # Apr
summary(gf.4.soil)
gf.5.soil <- lm(macpass[macpass$Month==5, 8] ~ macpass[macpass$Month==5, 7]) # May
summary(gf.5.soil)
gf.6.soil <- lm(macpass[macpass$Month==6, 8] ~ macpass[macpass$Month==6, 7]) # Jun
summary(gf.6.soil)
gf.7.soil <- lm(macpass[macpass$Month==7, 8] ~ macpass[macpass$Month==7, 7]) # Jul
summary(gf.7.soil)
gf.8.soil <- lm(macpass[macpass$Month==8, 8] ~ macpass[macpass$Month==8, 7]) # Aug
summary(gf.8.soil)
gf.9.soil <- lm(macpass[macpass$Month==9, 8] ~ macpass[macpass$Month==9, 7]) # Sep
summary(gf.9.soil)
gf.10.soil <- lm(macpass[macpass$Month==10, 8] ~ macpass[macpass$Month==10, 7]) # Oct
summary(gf.10.soil)
gf.11.soil <- lm(macpass[macpass$Month==11, 8] ~ macpass[macpass$Month==11, 7]) # Nov
summary(gf.11.soil)
gf.12.soil <- lm(macpass[macpass$Month==12, 8] ~ macpass[macpass$Month==12, 7]) # Dec
summary(gf.12.soil)

gf.soilT <- data.frame(rbind(coef(gf.1.soil),coef(gf.2.soil),coef(gf.3.soil),coef(gf.4.soil),coef(gf.5.soil),coef(gf.6.soil),
                              coef(gf.7.soil),coef(gf.8.soil),coef(gf.9.soil),coef(gf.10.soil),coef(gf.11.soil),coef(gf.12.soil)))
names(gf.soilT) <- c("b0","gf.grdT")
rownames(gf.soilT) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(gf.soilT, "~/Desktop/Workspace/Earthwatch/gf.soilT.csv")


# rm(list = ls())

### Filling GTREE temps
macpass <- read.csv(file = "~/Desktop/Workspace/Earthwatch/macpass_gtree_temps.csv", header = TRUE)
macpass2 <- macpass

##********************************
### bwc_sca_cage
## January
# Construct linear model based on non-NA pairs
bwc_sca_cage.1 <- macpass %>% filter(!is.na(bwc_sca_cage) & Month == 1)
bwc_sca_cage.1.fit <- lm(bwc_sca_cage ~ gf.soil, data = bwc_sca_cage.1)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_cage.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bwc_sca_cage = ifelse(is.na(bwc_sca_cage), pred, bwc_sca_cage))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
bwc_sca_cage.2 <- macpass %>% filter(!is.na(bwc_sca_cage) & Month == 2)
bwc_sca_cage.2.fit <- lm(bwc_sca_cage ~ gf.soil, data = bwc_sca_cage.2)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_cage.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(bwc_sca_cage = ifelse(is.na(bwc_sca_cage), pred, bwc_sca_cage))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
bwc_sca_cage.3 <- macpass %>% filter(!is.na(bwc_sca_cage) & Month == 3)
bwc_sca_cage.3.fit <- lm(bwc_sca_cage ~ gf.soil, data = bwc_sca_cage.3)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_cage.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(bwc_sca_cage = ifelse(is.na(bwc_sca_cage), pred, bwc_sca_cage))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
bwc_sca_cage.4 <- macpass %>% filter(!is.na(bwc_sca_cage) & Month == 4)
bwc_sca_cage.4.fit <- lm(bwc_sca_cage ~ gf.soil, data = bwc_sca_cage.4)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_cage.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(bwc_sca_cage = ifelse(is.na(bwc_sca_cage), pred, bwc_sca_cage))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
bwc_sca_cage.5 <- macpass %>% filter(!is.na(bwc_sca_cage) & Month == 5)
bwc_sca_cage.5.fit <- lm(bwc_sca_cage ~ gf.soil, data = bwc_sca_cage.5)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_cage.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(bwc_sca_cage = ifelse(is.na(bwc_sca_cage), pred, bwc_sca_cage))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
bwc_sca_cage.6 <- macpass %>% filter(!is.na(bwc_sca_cage) & Month == 6)
bwc_sca_cage.6.fit <- lm(bwc_sca_cage ~ gf.soil, data = bwc_sca_cage.6)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_cage.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(bwc_sca_cage = ifelse(is.na(bwc_sca_cage), pred, bwc_sca_cage))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
bwc_sca_cage.7 <- macpass %>% filter(!is.na(bwc_sca_cage) & Month == 7)
bwc_sca_cage.7.fit <- lm(bwc_sca_cage ~ gf.soil, data = bwc_sca_cage.7)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_cage.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(bwc_sca_cage = ifelse(is.na(bwc_sca_cage), pred, bwc_sca_cage))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
bwc_sca_cage.8 <- macpass %>% filter(!is.na(bwc_sca_cage) & Month == 8)
bwc_sca_cage.8.fit <- lm(bwc_sca_cage ~ gf.soil, data = bwc_sca_cage.8)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_cage.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(bwc_sca_cage = ifelse(is.na(bwc_sca_cage), pred, bwc_sca_cage))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
bwc_sca_cage.9 <- macpass %>% filter(!is.na(bwc_sca_cage) & Month == 9)
bwc_sca_cage.9.fit <- lm(bwc_sca_cage ~ gf.soil, data = bwc_sca_cage.9)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_cage.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(bwc_sca_cage = ifelse(is.na(bwc_sca_cage), pred, bwc_sca_cage))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
bwc_sca_cage.10 <- macpass %>% filter(!is.na(bwc_sca_cage) & Month == 10)
bwc_sca_cage.10.fit <- lm(bwc_sca_cage ~ gf.soil, data = bwc_sca_cage.10)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_cage.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(bwc_sca_cage = ifelse(is.na(bwc_sca_cage), pred, bwc_sca_cage))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
bwc_sca_cage.11 <- macpass %>% filter(!is.na(bwc_sca_cage) & Month == 11)
bwc_sca_cage.11.fit <- lm(bwc_sca_cage ~ gf.soil, data = bwc_sca_cage.11)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_cage.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(bwc_sca_cage = ifelse(is.na(bwc_sca_cage), pred, bwc_sca_cage))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
bwc_sca_cage.12 <- macpass %>% filter(!is.na(bwc_sca_cage) & Month == 12)
bwc_sca_cage.12.fit <- lm(bwc_sca_cage ~ gf.soil, data = bwc_sca_cage.12)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_cage.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(bwc_sca_cage = ifelse(is.na(bwc_sca_cage), pred, bwc_sca_cage))
macpass %>% as.data.frame()



##********************************
### bwc_sca_no
## January
# Construct linear model based on non-NA pairs
bwc_sca_no.1 <- macpass %>% filter(!is.na(bwc_sca_no) & Month == 1)
bwc_sca_no.1.fit <- lm(bwc_sca_no ~ gf.soil, data = bwc_sca_no.1)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_no.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bwc_sca_no = ifelse(is.na(bwc_sca_no), pred, bwc_sca_no))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
bwc_sca_no.2 <- macpass %>% filter(!is.na(bwc_sca_no) & Month == 2)
bwc_sca_no.2.fit <- lm(bwc_sca_no ~ gf.soil, data = bwc_sca_no.2)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_no.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(bwc_sca_no = ifelse(is.na(bwc_sca_no), pred, bwc_sca_no))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
bwc_sca_no.3 <- macpass %>% filter(!is.na(bwc_sca_no) & Month == 3)
bwc_sca_no.3.fit <- lm(bwc_sca_no ~ gf.soil, data = bwc_sca_no.3)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_no.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(bwc_sca_no = ifelse(is.na(bwc_sca_no), pred, bwc_sca_no))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
bwc_sca_no.4 <- macpass %>% filter(!is.na(bwc_sca_no) & Month == 4)
bwc_sca_no.4.fit <- lm(bwc_sca_no ~ gf.soil, data = bwc_sca_no.4)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_no.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(bwc_sca_no = ifelse(is.na(bwc_sca_no), pred, bwc_sca_no))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
bwc_sca_no.5 <- macpass %>% filter(!is.na(bwc_sca_no) & Month == 5)
bwc_sca_no.5.fit <- lm(bwc_sca_no ~ gf.soil, data = bwc_sca_no.5)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_no.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(bwc_sca_no = ifelse(is.na(bwc_sca_no), pred, bwc_sca_no))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
bwc_sca_no.6 <- macpass %>% filter(!is.na(bwc_sca_no) & Month == 6)
bwc_sca_no.6.fit <- lm(bwc_sca_no ~ gf.soil, data = bwc_sca_no.6)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_no.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(bwc_sca_no = ifelse(is.na(bwc_sca_no), pred, bwc_sca_no))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
bwc_sca_no.7 <- macpass %>% filter(!is.na(bwc_sca_no) & Month == 7)
bwc_sca_no.7.fit <- lm(bwc_sca_no ~ gf.soil, data = bwc_sca_no.7)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_no.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(bwc_sca_no = ifelse(is.na(bwc_sca_no), pred, bwc_sca_no))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
bwc_sca_no.8 <- macpass %>% filter(!is.na(bwc_sca_no) & Month == 8)
bwc_sca_no.8.fit <- lm(bwc_sca_no ~ gf.soil, data = bwc_sca_no.8)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_no.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(bwc_sca_no = ifelse(is.na(bwc_sca_no), pred, bwc_sca_no))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
bwc_sca_no.9 <- macpass %>% filter(!is.na(bwc_sca_no) & Month == 9)
bwc_sca_no.9.fit <- lm(bwc_sca_no ~ gf.soil, data = bwc_sca_no.9)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_no.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(bwc_sca_no = ifelse(is.na(bwc_sca_no), pred, bwc_sca_no))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
bwc_sca_no.10 <- macpass %>% filter(!is.na(bwc_sca_no) & Month == 10)
bwc_sca_no.10.fit <- lm(bwc_sca_no ~ gf.soil, data = bwc_sca_no.10)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_no.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(bwc_sca_no = ifelse(is.na(bwc_sca_no), pred, bwc_sca_no))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
bwc_sca_no.11 <- macpass %>% filter(!is.na(bwc_sca_no) & Month == 11)
bwc_sca_no.11.fit <- lm(bwc_sca_no ~ gf.soil, data = bwc_sca_no.11)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_no.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(bwc_sca_no = ifelse(is.na(bwc_sca_no), pred, bwc_sca_no))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
bwc_sca_no.12 <- macpass %>% filter(!is.na(bwc_sca_no) & Month == 12)
bwc_sca_no.12.fit <- lm(bwc_sca_no ~ gf.soil, data = bwc_sca_no.12)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_no.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(bwc_sca_no = ifelse(is.na(bwc_sca_no), pred, bwc_sca_no))
macpass %>% as.data.frame()





##********************************
### bwc_veg_cage
## January
# Construct linear model based on non-NA pairs
bwc_veg_cage.1 <- macpass %>% filter(!is.na(bwc_veg_cage) & Month == 1)
bwc_veg_cage.1.fit <- lm(bwc_veg_cage ~ gf.soil, data = bwc_veg_cage.1)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_cage.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bwc_veg_cage = ifelse(is.na(bwc_veg_cage), pred, bwc_veg_cage))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
bwc_veg_cage.2 <- macpass %>% filter(!is.na(bwc_veg_cage) & Month == 2)
bwc_veg_cage.2.fit <- lm(bwc_veg_cage ~ gf.soil, data = bwc_veg_cage.2)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_cage.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(bwc_veg_cage = ifelse(is.na(bwc_veg_cage), pred, bwc_veg_cage))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
bwc_veg_cage.3 <- macpass %>% filter(!is.na(bwc_veg_cage) & Month == 3)
bwc_veg_cage.3.fit <- lm(bwc_veg_cage ~ gf.soil, data = bwc_veg_cage.3)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_cage.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(bwc_veg_cage = ifelse(is.na(bwc_veg_cage), pred, bwc_veg_cage))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
bwc_veg_cage.4 <- macpass %>% filter(!is.na(bwc_veg_cage) & Month == 4)
bwc_veg_cage.4.fit <- lm(bwc_veg_cage ~ gf.soil, data = bwc_veg_cage.4)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_cage.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(bwc_veg_cage = ifelse(is.na(bwc_veg_cage), pred, bwc_veg_cage))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
bwc_veg_cage.5 <- macpass %>% filter(!is.na(bwc_veg_cage) & Month == 5)
bwc_veg_cage.5.fit <- lm(bwc_veg_cage ~ gf.soil, data = bwc_veg_cage.5)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_cage.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(bwc_veg_cage = ifelse(is.na(bwc_veg_cage), pred, bwc_veg_cage))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
bwc_veg_cage.6 <- macpass %>% filter(!is.na(bwc_veg_cage) & Month == 6)
bwc_veg_cage.6.fit <- lm(bwc_veg_cage ~ gf.soil, data = bwc_veg_cage.6)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_cage.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(bwc_veg_cage = ifelse(is.na(bwc_veg_cage), pred, bwc_veg_cage))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
bwc_veg_cage.7 <- macpass %>% filter(!is.na(bwc_veg_cage) & Month == 7)
bwc_veg_cage.7.fit <- lm(bwc_veg_cage ~ gf.soil, data = bwc_veg_cage.7)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_cage.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(bwc_veg_cage = ifelse(is.na(bwc_veg_cage), pred, bwc_veg_cage))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
bwc_veg_cage.8 <- macpass %>% filter(!is.na(bwc_veg_cage) & Month == 8)
bwc_veg_cage.8.fit <- lm(bwc_veg_cage ~ gf.soil, data = bwc_veg_cage.8)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_cage.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(bwc_veg_cage = ifelse(is.na(bwc_veg_cage), pred, bwc_veg_cage))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
bwc_veg_cage.9 <- macpass %>% filter(!is.na(bwc_veg_cage) & Month == 9)
bwc_veg_cage.9.fit <- lm(bwc_veg_cage ~ gf.soil, data = bwc_veg_cage.9)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_cage.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(bwc_veg_cage = ifelse(is.na(bwc_veg_cage), pred, bwc_veg_cage))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
bwc_veg_cage.10 <- macpass %>% filter(!is.na(bwc_veg_cage) & Month == 10)
bwc_veg_cage.10.fit <- lm(bwc_veg_cage ~ gf.soil, data = bwc_veg_cage.10)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_cage.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(bwc_veg_cage = ifelse(is.na(bwc_veg_cage), pred, bwc_veg_cage))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
bwc_veg_cage.11 <- macpass %>% filter(!is.na(bwc_veg_cage) & Month == 11)
bwc_veg_cage.11.fit <- lm(bwc_veg_cage ~ gf.soil, data = bwc_veg_cage.11)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_cage.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(bwc_veg_cage = ifelse(is.na(bwc_veg_cage), pred, bwc_veg_cage))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
bwc_veg_cage.12 <- macpass %>% filter(!is.na(bwc_veg_cage) & Month == 12)
bwc_veg_cage.12.fit <- lm(bwc_veg_cage ~ gf.soil, data = bwc_veg_cage.12)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_cage.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(bwc_veg_cage = ifelse(is.na(bwc_veg_cage), pred, bwc_veg_cage))
macpass %>% as.data.frame()


##********************************
### bwc_veg_no
## January
# Construct linear model based on non-NA pairs
bwc_veg_no.1 <- macpass %>% filter(!is.na(bwc_veg_no) & Month == 1)
bwc_veg_no.1.fit <- lm(bwc_veg_no ~ gf.soil, data = bwc_veg_no.1)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_no.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bwc_veg_no = ifelse(is.na(bwc_veg_no), pred, bwc_veg_no))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
bwc_veg_no.2 <- macpass %>% filter(!is.na(bwc_veg_no) & Month == 2)
bwc_veg_no.2.fit <- lm(bwc_veg_no ~ gf.soil, data = bwc_veg_no.2)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_no.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(bwc_veg_no = ifelse(is.na(bwc_veg_no), pred, bwc_veg_no))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
bwc_veg_no.3 <- macpass %>% filter(!is.na(bwc_veg_no) & Month == 3)
bwc_veg_no.3.fit <- lm(bwc_veg_no ~ gf.soil, data = bwc_veg_no.3)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_no.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(bwc_veg_no = ifelse(is.na(bwc_veg_no), pred, bwc_veg_no))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
bwc_veg_no.4 <- macpass %>% filter(!is.na(bwc_veg_no) & Month == 4)
bwc_veg_no.4.fit <- lm(bwc_veg_no ~ gf.soil, data = bwc_veg_no.4)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_no.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(bwc_veg_no = ifelse(is.na(bwc_veg_no), pred, bwc_veg_no))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
bwc_veg_no.5 <- macpass %>% filter(!is.na(bwc_veg_no) & Month == 5)
bwc_veg_no.5.fit <- lm(bwc_veg_no ~ gf.soil, data = bwc_veg_no.5)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_no.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(bwc_veg_no = ifelse(is.na(bwc_veg_no), pred, bwc_veg_no))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
bwc_veg_no.6 <- macpass %>% filter(!is.na(bwc_veg_no) & Month == 6)
bwc_veg_no.6.fit <- lm(bwc_veg_no ~ gf.soil, data = bwc_veg_no.6)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_no.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(bwc_veg_no = ifelse(is.na(bwc_veg_no), pred, bwc_veg_no))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
bwc_veg_no.7 <- macpass %>% filter(!is.na(bwc_veg_no) & Month == 7)
bwc_veg_no.7.fit <- lm(bwc_veg_no ~ gf.soil, data = bwc_veg_no.7)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_no.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(bwc_veg_no = ifelse(is.na(bwc_veg_no), pred, bwc_veg_no))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
bwc_veg_no.8 <- macpass %>% filter(!is.na(bwc_veg_no) & Month == 8)
bwc_veg_no.8.fit <- lm(bwc_veg_no ~ gf.soil, data = bwc_veg_no.8)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_no.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(bwc_veg_no = ifelse(is.na(bwc_veg_no), pred, bwc_veg_no))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
bwc_veg_no.9 <- macpass %>% filter(!is.na(bwc_veg_no) & Month == 9)
bwc_veg_no.9.fit <- lm(bwc_veg_no ~ gf.soil, data = bwc_veg_no.9)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_no.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(bwc_veg_no = ifelse(is.na(bwc_veg_no), pred, bwc_veg_no))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
bwc_veg_no.10 <- macpass %>% filter(!is.na(bwc_veg_no) & Month == 10)
bwc_veg_no.10.fit <- lm(bwc_veg_no ~ gf.soil, data = bwc_veg_no.10)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_no.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(bwc_veg_no = ifelse(is.na(bwc_veg_no), pred, bwc_veg_no))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
bwc_veg_no.11 <- macpass %>% filter(!is.na(bwc_veg_no) & Month == 11)
bwc_veg_no.11.fit <- lm(bwc_veg_no ~ gf.soil, data = bwc_veg_no.11)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_no.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(bwc_veg_no = ifelse(is.na(bwc_veg_no), pred, bwc_veg_no))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
bwc_veg_no.12 <- macpass %>% filter(!is.na(bwc_veg_no) & Month == 12)
bwc_veg_no.12.fit <- lm(bwc_veg_no ~ gf.soil, data = bwc_veg_no.12)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_no.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(bwc_veg_no = ifelse(is.na(bwc_veg_no), pred, bwc_veg_no))
macpass %>% as.data.frame()

############################################################
############################################################
############################################################
############################################################

##********************************
### bws_sca_cage
## January
# Construct linear model based on non-NA pairs
bws_sca_cage.1 <- macpass %>% filter(!is.na(bws_sca_cage) & Month == 1)
bws_sca_cage.1.fit <- lm(bws_sca_cage ~ gf.soil, data = bws_sca_cage.1)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_cage.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bws_sca_cage = ifelse(is.na(bws_sca_cage), pred, bws_sca_cage))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
bws_sca_cage.2 <- macpass %>% filter(!is.na(bws_sca_cage) & Month == 2)
bws_sca_cage.2.fit <- lm(bws_sca_cage ~ gf.soil, data = bws_sca_cage.2)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_cage.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(bws_sca_cage = ifelse(is.na(bws_sca_cage), pred, bws_sca_cage))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
bws_sca_cage.3 <- macpass %>% filter(!is.na(bws_sca_cage) & Month == 3)
bws_sca_cage.3.fit <- lm(bws_sca_cage ~ gf.soil, data = bws_sca_cage.3)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_cage.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(bws_sca_cage = ifelse(is.na(bws_sca_cage), pred, bws_sca_cage))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
bws_sca_cage.4 <- macpass %>% filter(!is.na(bws_sca_cage) & Month == 4)
bws_sca_cage.4.fit <- lm(bws_sca_cage ~ gf.soil, data = bws_sca_cage.4)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_cage.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(bws_sca_cage = ifelse(is.na(bws_sca_cage), pred, bws_sca_cage))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
bws_sca_cage.5 <- macpass %>% filter(!is.na(bws_sca_cage) & Month == 5)
bws_sca_cage.5.fit <- lm(bws_sca_cage ~ gf.soil, data = bws_sca_cage.5)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_cage.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(bws_sca_cage = ifelse(is.na(bws_sca_cage), pred, bws_sca_cage))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
bws_sca_cage.6 <- macpass %>% filter(!is.na(bws_sca_cage) & Month == 6)
bws_sca_cage.6.fit <- lm(bws_sca_cage ~ gf.soil, data = bws_sca_cage.6)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_cage.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(bws_sca_cage = ifelse(is.na(bws_sca_cage), pred, bws_sca_cage))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
bws_sca_cage.7 <- macpass %>% filter(!is.na(bws_sca_cage) & Month == 7)
bws_sca_cage.7.fit <- lm(bws_sca_cage ~ gf.soil, data = bws_sca_cage.7)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_cage.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(bws_sca_cage = ifelse(is.na(bws_sca_cage), pred, bws_sca_cage))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
bws_sca_cage.8 <- macpass %>% filter(!is.na(bws_sca_cage) & Month == 8)
bws_sca_cage.8.fit <- lm(bws_sca_cage ~ gf.soil, data = bws_sca_cage.8)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_cage.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(bws_sca_cage = ifelse(is.na(bws_sca_cage), pred, bws_sca_cage))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
bws_sca_cage.9 <- macpass %>% filter(!is.na(bws_sca_cage) & Month == 9)
bws_sca_cage.9.fit <- lm(bws_sca_cage ~ gf.soil, data = bws_sca_cage.9)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_cage.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(bws_sca_cage = ifelse(is.na(bws_sca_cage), pred, bws_sca_cage))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
bws_sca_cage.10 <- macpass %>% filter(!is.na(bws_sca_cage) & Month == 10)
bws_sca_cage.10.fit <- lm(bws_sca_cage ~ gf.soil, data = bws_sca_cage.10)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_cage.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(bws_sca_cage = ifelse(is.na(bws_sca_cage), pred, bws_sca_cage))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
bws_sca_cage.11 <- macpass %>% filter(!is.na(bws_sca_cage) & Month == 11)
bws_sca_cage.11.fit <- lm(bws_sca_cage ~ gf.soil, data = bws_sca_cage.11)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_cage.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(bws_sca_cage = ifelse(is.na(bws_sca_cage), pred, bws_sca_cage))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
bws_sca_cage.12 <- macpass %>% filter(!is.na(bws_sca_cage) & Month == 12)
bws_sca_cage.12.fit <- lm(bws_sca_cage ~ gf.soil, data = bws_sca_cage.12)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_cage.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(bws_sca_cage = ifelse(is.na(bws_sca_cage), pred, bws_sca_cage))
macpass %>% as.data.frame()



##********************************
### bws_sca_no
## January
# Construct linear model based on non-NA pairs
bws_sca_no.1 <- macpass %>% filter(!is.na(bws_sca_no) & Month == 1)
bws_sca_no.1.fit <- lm(bws_sca_no ~ gf.soil, data = bws_sca_no.1)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_no.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bws_sca_no = ifelse(is.na(bws_sca_no), pred, bws_sca_no))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
bws_sca_no.2 <- macpass %>% filter(!is.na(bws_sca_no) & Month == 2)
bws_sca_no.2.fit <- lm(bws_sca_no ~ gf.soil, data = bws_sca_no.2)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_no.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(bws_sca_no = ifelse(is.na(bws_sca_no), pred, bws_sca_no))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
bws_sca_no.3 <- macpass %>% filter(!is.na(bws_sca_no) & Month == 3)
bws_sca_no.3.fit <- lm(bws_sca_no ~ gf.soil, data = bws_sca_no.3)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_no.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(bws_sca_no = ifelse(is.na(bws_sca_no), pred, bws_sca_no))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
bws_sca_no.4 <- macpass %>% filter(!is.na(bws_sca_no) & Month == 4)
bws_sca_no.4.fit <- lm(bws_sca_no ~ gf.soil, data = bws_sca_no.4)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_no.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(bws_sca_no = ifelse(is.na(bws_sca_no), pred, bws_sca_no))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
bws_sca_no.5 <- macpass %>% filter(!is.na(bws_sca_no) & Month == 5)
bws_sca_no.5.fit <- lm(bws_sca_no ~ gf.soil, data = bws_sca_no.5)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_no.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(bws_sca_no = ifelse(is.na(bws_sca_no), pred, bws_sca_no))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
bws_sca_no.6 <- macpass %>% filter(!is.na(bws_sca_no) & Month == 6)
bws_sca_no.6.fit <- lm(bws_sca_no ~ gf.soil, data = bws_sca_no.6)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_no.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(bws_sca_no = ifelse(is.na(bws_sca_no), pred, bws_sca_no))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
bws_sca_no.7 <- macpass %>% filter(!is.na(bws_sca_no) & Month == 7)
bws_sca_no.7.fit <- lm(bws_sca_no ~ gf.soil, data = bws_sca_no.7)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_no.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(bws_sca_no = ifelse(is.na(bws_sca_no), pred, bws_sca_no))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
bws_sca_no.8 <- macpass %>% filter(!is.na(bws_sca_no) & Month == 8)
bws_sca_no.8.fit <- lm(bws_sca_no ~ gf.soil, data = bws_sca_no.8)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_no.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(bws_sca_no = ifelse(is.na(bws_sca_no), pred, bws_sca_no))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
bws_sca_no.9 <- macpass %>% filter(!is.na(bws_sca_no) & Month == 9)
bws_sca_no.9.fit <- lm(bws_sca_no ~ gf.soil, data = bws_sca_no.9)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_no.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(bws_sca_no = ifelse(is.na(bws_sca_no), pred, bws_sca_no))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
bws_sca_no.10 <- macpass %>% filter(!is.na(bws_sca_no) & Month == 10)
bws_sca_no.10.fit <- lm(bws_sca_no ~ gf.soil, data = bws_sca_no.10)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_no.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(bws_sca_no = ifelse(is.na(bws_sca_no), pred, bws_sca_no))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
bws_sca_no.11 <- macpass %>% filter(!is.na(bws_sca_no) & Month == 11)
bws_sca_no.11.fit <- lm(bws_sca_no ~ gf.soil, data = bws_sca_no.11)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_no.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(bws_sca_no = ifelse(is.na(bws_sca_no), pred, bws_sca_no))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
bws_sca_no.12 <- macpass %>% filter(!is.na(bws_sca_no) & Month == 12)
bws_sca_no.12.fit <- lm(bws_sca_no ~ gf.soil, data = bws_sca_no.12)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_no.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(bws_sca_no = ifelse(is.na(bws_sca_no), pred, bws_sca_no))
macpass %>% as.data.frame()





##********************************
### bws_veg_cage
## January
# Construct linear model based on non-NA pairs
bws_veg_cage.1 <- macpass %>% filter(!is.na(bws_veg_cage) & Month == 1)
bws_veg_cage.1.fit <- lm(bws_veg_cage ~ gf.soil, data = bws_veg_cage.1)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_cage.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bws_veg_cage = ifelse(is.na(bws_veg_cage), pred, bws_veg_cage))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
bws_veg_cage.2 <- macpass %>% filter(!is.na(bws_veg_cage) & Month == 2)
bws_veg_cage.2.fit <- lm(bws_veg_cage ~ gf.soil, data = bws_veg_cage.2)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_cage.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(bws_veg_cage = ifelse(is.na(bws_veg_cage), pred, bws_veg_cage))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
bws_veg_cage.3 <- macpass %>% filter(!is.na(bws_veg_cage) & Month == 3)
bws_veg_cage.3.fit <- lm(bws_veg_cage ~ gf.soil, data = bws_veg_cage.3)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_cage.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(bws_veg_cage = ifelse(is.na(bws_veg_cage), pred, bws_veg_cage))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
bws_veg_cage.4 <- macpass %>% filter(!is.na(bws_veg_cage) & Month == 4)
bws_veg_cage.4.fit <- lm(bws_veg_cage ~ gf.soil, data = bws_veg_cage.4)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_cage.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(bws_veg_cage = ifelse(is.na(bws_veg_cage), pred, bws_veg_cage))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
bws_veg_cage.5 <- macpass %>% filter(!is.na(bws_veg_cage) & Month == 5)
bws_veg_cage.5.fit <- lm(bws_veg_cage ~ gf.soil, data = bws_veg_cage.5)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_cage.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(bws_veg_cage = ifelse(is.na(bws_veg_cage), pred, bws_veg_cage))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
bws_veg_cage.6 <- macpass %>% filter(!is.na(bws_veg_cage) & Month == 6)
bws_veg_cage.6.fit <- lm(bws_veg_cage ~ gf.soil, data = bws_veg_cage.6)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_cage.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(bws_veg_cage = ifelse(is.na(bws_veg_cage), pred, bws_veg_cage))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
bws_veg_cage.7 <- macpass %>% filter(!is.na(bws_veg_cage) & Month == 7)
bws_veg_cage.7.fit <- lm(bws_veg_cage ~ gf.soil, data = bws_veg_cage.7)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_cage.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(bws_veg_cage = ifelse(is.na(bws_veg_cage), pred, bws_veg_cage))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
bws_veg_cage.8 <- macpass %>% filter(!is.na(bws_veg_cage) & Month == 8)
bws_veg_cage.8.fit <- lm(bws_veg_cage ~ gf.soil, data = bws_veg_cage.8)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_cage.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(bws_veg_cage = ifelse(is.na(bws_veg_cage), pred, bws_veg_cage))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
bws_veg_cage.9 <- macpass %>% filter(!is.na(bws_veg_cage) & Month == 9)
bws_veg_cage.9.fit <- lm(bws_veg_cage ~ gf.soil, data = bws_veg_cage.9)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_cage.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(bws_veg_cage = ifelse(is.na(bws_veg_cage), pred, bws_veg_cage))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
bws_veg_cage.10 <- macpass %>% filter(!is.na(bws_veg_cage) & Month == 10)
bws_veg_cage.10.fit <- lm(bws_veg_cage ~ gf.soil, data = bws_veg_cage.10)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_cage.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(bws_veg_cage = ifelse(is.na(bws_veg_cage), pred, bws_veg_cage))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
bws_veg_cage.11 <- macpass %>% filter(!is.na(bws_veg_cage) & Month == 11)
bws_veg_cage.11.fit <- lm(bws_veg_cage ~ gf.soil, data = bws_veg_cage.11)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_cage.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(bws_veg_cage = ifelse(is.na(bws_veg_cage), pred, bws_veg_cage))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
bws_veg_cage.12 <- macpass %>% filter(!is.na(bws_veg_cage) & Month == 12)
bws_veg_cage.12.fit <- lm(bws_veg_cage ~ gf.soil, data = bws_veg_cage.12)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_cage.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(bws_veg_cage = ifelse(is.na(bws_veg_cage), pred, bws_veg_cage))
macpass %>% as.data.frame()


##********************************
### bws_veg_no
## January
# Construct linear model based on non-NA pairs
bws_veg_no.1 <- macpass %>% filter(!is.na(bws_veg_no) & Month == 1)
bws_veg_no.1.fit <- lm(bws_veg_no ~ gf.soil, data = bws_veg_no.1)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_no.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bws_veg_no = ifelse(is.na(bws_veg_no), pred, bws_veg_no))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
bws_veg_no.2 <- macpass %>% filter(!is.na(bws_veg_no) & Month == 2)
bws_veg_no.2.fit <- lm(bws_veg_no ~ gf.soil, data = bws_veg_no.2)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_no.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(bws_veg_no = ifelse(is.na(bws_veg_no), pred, bws_veg_no))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
bws_veg_no.3 <- macpass %>% filter(!is.na(bws_veg_no) & Month == 3)
bws_veg_no.3.fit <- lm(bws_veg_no ~ gf.soil, data = bws_veg_no.3)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_no.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(bws_veg_no = ifelse(is.na(bws_veg_no), pred, bws_veg_no))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
bws_veg_no.4 <- macpass %>% filter(!is.na(bws_veg_no) & Month == 4)
bws_veg_no.4.fit <- lm(bws_veg_no ~ gf.soil, data = bws_veg_no.4)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_no.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(bws_veg_no = ifelse(is.na(bws_veg_no), pred, bws_veg_no))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
bws_veg_no.5 <- macpass %>% filter(!is.na(bws_veg_no) & Month == 5)
bws_veg_no.5.fit <- lm(bws_veg_no ~ gf.soil, data = bws_veg_no.5)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_no.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(bws_veg_no = ifelse(is.na(bws_veg_no), pred, bws_veg_no))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
bws_veg_no.6 <- macpass %>% filter(!is.na(bws_veg_no) & Month == 6)
bws_veg_no.6.fit <- lm(bws_veg_no ~ gf.soil, data = bws_veg_no.6)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_no.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(bws_veg_no = ifelse(is.na(bws_veg_no), pred, bws_veg_no))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
bws_veg_no.7 <- macpass %>% filter(!is.na(bws_veg_no) & Month == 7)
bws_veg_no.7.fit <- lm(bws_veg_no ~ gf.soil, data = bws_veg_no.7)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_no.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(bws_veg_no = ifelse(is.na(bws_veg_no), pred, bws_veg_no))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
bws_veg_no.8 <- macpass %>% filter(!is.na(bws_veg_no) & Month == 8)
bws_veg_no.8.fit <- lm(bws_veg_no ~ gf.soil, data = bws_veg_no.8)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_no.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(bws_veg_no = ifelse(is.na(bws_veg_no), pred, bws_veg_no))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
bws_veg_no.9 <- macpass %>% filter(!is.na(bws_veg_no) & Month == 9)
bws_veg_no.9.fit <- lm(bws_veg_no ~ gf.soil, data = bws_veg_no.9)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_no.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(bws_veg_no = ifelse(is.na(bws_veg_no), pred, bws_veg_no))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
bws_veg_no.10 <- macpass %>% filter(!is.na(bws_veg_no) & Month == 10)
bws_veg_no.10.fit <- lm(bws_veg_no ~ gf.soil, data = bws_veg_no.10)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_no.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(bws_veg_no = ifelse(is.na(bws_veg_no), pred, bws_veg_no))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
bws_veg_no.11 <- macpass %>% filter(!is.na(bws_veg_no) & Month == 11)
bws_veg_no.11.fit <- lm(bws_veg_no ~ gf.soil, data = bws_veg_no.11)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_no.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(bws_veg_no = ifelse(is.na(bws_veg_no), pred, bws_veg_no))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
bws_veg_no.12 <- macpass %>% filter(!is.na(bws_veg_no) & Month == 12)
bws_veg_no.12.fit <- lm(bws_veg_no ~ gf.soil, data = bws_veg_no.12)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_no.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(bws_veg_no = ifelse(is.na(bws_veg_no), pred, bws_veg_no))
macpass %>% as.data.frame()


############################################################
############################################################
############################################################
############################################################

##********************************
### dln_sca_cage
## January
# Construct linear model based on non-NA pairs
dln_sca_cage.1 <- macpass %>% filter(!is.na(dln_sca_cage) & Month == 1)
dln_sca_cage.1.fit <- lm(dln_sca_cage ~ gf.soil, data = dln_sca_cage.1)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_cage.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(dln_sca_cage = ifelse(is.na(dln_sca_cage), pred, dln_sca_cage))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
dln_sca_cage.2 <- macpass %>% filter(!is.na(dln_sca_cage) & Month == 2)
dln_sca_cage.2.fit <- lm(dln_sca_cage ~ gf.soil, data = dln_sca_cage.2)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_cage.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(dln_sca_cage = ifelse(is.na(dln_sca_cage), pred, dln_sca_cage))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
dln_sca_cage.3 <- macpass %>% filter(!is.na(dln_sca_cage) & Month == 3)
dln_sca_cage.3.fit <- lm(dln_sca_cage ~ gf.soil, data = dln_sca_cage.3)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_cage.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(dln_sca_cage = ifelse(is.na(dln_sca_cage), pred, dln_sca_cage))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
dln_sca_cage.4 <- macpass %>% filter(!is.na(dln_sca_cage) & Month == 4)
dln_sca_cage.4.fit <- lm(dln_sca_cage ~ gf.soil, data = dln_sca_cage.4)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_cage.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(dln_sca_cage = ifelse(is.na(dln_sca_cage), pred, dln_sca_cage))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
dln_sca_cage.5 <- macpass %>% filter(!is.na(dln_sca_cage) & Month == 5)
dln_sca_cage.5.fit <- lm(dln_sca_cage ~ gf.soil, data = dln_sca_cage.5)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_cage.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(dln_sca_cage = ifelse(is.na(dln_sca_cage), pred, dln_sca_cage))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
dln_sca_cage.6 <- macpass %>% filter(!is.na(dln_sca_cage) & Month == 6)
dln_sca_cage.6.fit <- lm(dln_sca_cage ~ gf.soil, data = dln_sca_cage.6)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_cage.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(dln_sca_cage = ifelse(is.na(dln_sca_cage), pred, dln_sca_cage))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
dln_sca_cage.7 <- macpass %>% filter(!is.na(dln_sca_cage) & Month == 7)
dln_sca_cage.7.fit <- lm(dln_sca_cage ~ gf.soil, data = dln_sca_cage.7)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_cage.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(dln_sca_cage = ifelse(is.na(dln_sca_cage), pred, dln_sca_cage))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
dln_sca_cage.8 <- macpass %>% filter(!is.na(dln_sca_cage) & Month == 8)
dln_sca_cage.8.fit <- lm(dln_sca_cage ~ gf.soil, data = dln_sca_cage.8)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_cage.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(dln_sca_cage = ifelse(is.na(dln_sca_cage), pred, dln_sca_cage))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
dln_sca_cage.9 <- macpass %>% filter(!is.na(dln_sca_cage) & Month == 9)
dln_sca_cage.9.fit <- lm(dln_sca_cage ~ gf.soil, data = dln_sca_cage.9)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_cage.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(dln_sca_cage = ifelse(is.na(dln_sca_cage), pred, dln_sca_cage))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
dln_sca_cage.10 <- macpass %>% filter(!is.na(dln_sca_cage) & Month == 10)
dln_sca_cage.10.fit <- lm(dln_sca_cage ~ gf.soil, data = dln_sca_cage.10)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_cage.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(dln_sca_cage = ifelse(is.na(dln_sca_cage), pred, dln_sca_cage))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
dln_sca_cage.11 <- macpass %>% filter(!is.na(dln_sca_cage) & Month == 11)
dln_sca_cage.11.fit <- lm(dln_sca_cage ~ gf.soil, data = dln_sca_cage.11)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_cage.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(dln_sca_cage = ifelse(is.na(dln_sca_cage), pred, dln_sca_cage))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
dln_sca_cage.12 <- macpass %>% filter(!is.na(dln_sca_cage) & Month == 12)
dln_sca_cage.12.fit <- lm(dln_sca_cage ~ gf.soil, data = dln_sca_cage.12)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_cage.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(dln_sca_cage = ifelse(is.na(dln_sca_cage), pred, dln_sca_cage))
macpass %>% as.data.frame()



##********************************
### dln_sca_no
## January
# Construct linear model based on non-NA pairs
dln_sca_no.1 <- macpass %>% filter(!is.na(dln_sca_no) & Month == 1)
dln_sca_no.1.fit <- lm(dln_sca_no ~ gf.soil, data = dln_sca_no.1)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_no.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(dln_sca_no = ifelse(is.na(dln_sca_no), pred, dln_sca_no))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
dln_sca_no.2 <- macpass %>% filter(!is.na(dln_sca_no) & Month == 2)
dln_sca_no.2.fit <- lm(dln_sca_no ~ gf.soil, data = dln_sca_no.2)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_no.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(dln_sca_no = ifelse(is.na(dln_sca_no), pred, dln_sca_no))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
dln_sca_no.3 <- macpass %>% filter(!is.na(dln_sca_no) & Month == 3)
dln_sca_no.3.fit <- lm(dln_sca_no ~ gf.soil, data = dln_sca_no.3)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_no.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(dln_sca_no = ifelse(is.na(dln_sca_no), pred, dln_sca_no))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
dln_sca_no.4 <- macpass %>% filter(!is.na(dln_sca_no) & Month == 4)
dln_sca_no.4.fit <- lm(dln_sca_no ~ gf.soil, data = dln_sca_no.4)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_no.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(dln_sca_no = ifelse(is.na(dln_sca_no), pred, dln_sca_no))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
dln_sca_no.5 <- macpass %>% filter(!is.na(dln_sca_no) & Month == 5)
dln_sca_no.5.fit <- lm(dln_sca_no ~ gf.soil, data = dln_sca_no.5)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_no.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(dln_sca_no = ifelse(is.na(dln_sca_no), pred, dln_sca_no))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
dln_sca_no.6 <- macpass %>% filter(!is.na(dln_sca_no) & Month == 6)
dln_sca_no.6.fit <- lm(dln_sca_no ~ gf.soil, data = dln_sca_no.6)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_no.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(dln_sca_no = ifelse(is.na(dln_sca_no), pred, dln_sca_no))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
dln_sca_no.7 <- macpass %>% filter(!is.na(dln_sca_no) & Month == 7)
dln_sca_no.7.fit <- lm(dln_sca_no ~ gf.soil, data = dln_sca_no.7)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_no.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(dln_sca_no = ifelse(is.na(dln_sca_no), pred, dln_sca_no))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
dln_sca_no.8 <- macpass %>% filter(!is.na(dln_sca_no) & Month == 8)
dln_sca_no.8.fit <- lm(dln_sca_no ~ gf.soil, data = dln_sca_no.8)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_no.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(dln_sca_no = ifelse(is.na(dln_sca_no), pred, dln_sca_no))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
dln_sca_no.9 <- macpass %>% filter(!is.na(dln_sca_no) & Month == 9)
dln_sca_no.9.fit <- lm(dln_sca_no ~ gf.soil, data = dln_sca_no.9)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_no.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(dln_sca_no = ifelse(is.na(dln_sca_no), pred, dln_sca_no))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
dln_sca_no.10 <- macpass %>% filter(!is.na(dln_sca_no) & Month == 10)
dln_sca_no.10.fit <- lm(dln_sca_no ~ gf.soil, data = dln_sca_no.10)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_no.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(dln_sca_no = ifelse(is.na(dln_sca_no), pred, dln_sca_no))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
dln_sca_no.11 <- macpass %>% filter(!is.na(dln_sca_no) & Month == 11)
dln_sca_no.11.fit <- lm(dln_sca_no ~ gf.soil, data = dln_sca_no.11)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_no.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(dln_sca_no = ifelse(is.na(dln_sca_no), pred, dln_sca_no))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
dln_sca_no.12 <- macpass %>% filter(!is.na(dln_sca_no) & Month == 12)
dln_sca_no.12.fit <- lm(dln_sca_no ~ gf.soil, data = dln_sca_no.12)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_no.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(dln_sca_no = ifelse(is.na(dln_sca_no), pred, dln_sca_no))
macpass %>% as.data.frame()





##********************************
### dln_veg_cage
## January
# Construct linear model based on non-NA pairs
dln_veg_cage.1 <- macpass %>% filter(!is.na(dln_veg_cage) & Month == 1)
dln_veg_cage.1.fit <- lm(dln_veg_cage ~ gf.soil, data = dln_veg_cage.1)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_cage.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(dln_veg_cage = ifelse(is.na(dln_veg_cage), pred, dln_veg_cage))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
dln_veg_cage.2 <- macpass %>% filter(!is.na(dln_veg_cage) & Month == 2)
dln_veg_cage.2.fit <- lm(dln_veg_cage ~ gf.soil, data = dln_veg_cage.2)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_cage.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(dln_veg_cage = ifelse(is.na(dln_veg_cage), pred, dln_veg_cage))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
dln_veg_cage.3 <- macpass %>% filter(!is.na(dln_veg_cage) & Month == 3)
dln_veg_cage.3.fit <- lm(dln_veg_cage ~ gf.soil, data = dln_veg_cage.3)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_cage.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(dln_veg_cage = ifelse(is.na(dln_veg_cage), pred, dln_veg_cage))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
dln_veg_cage.4 <- macpass %>% filter(!is.na(dln_veg_cage) & Month == 4)
dln_veg_cage.4.fit <- lm(dln_veg_cage ~ gf.soil, data = dln_veg_cage.4)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_cage.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(dln_veg_cage = ifelse(is.na(dln_veg_cage), pred, dln_veg_cage))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
dln_veg_cage.5 <- macpass %>% filter(!is.na(dln_veg_cage) & Month == 5)
dln_veg_cage.5.fit <- lm(dln_veg_cage ~ gf.soil, data = dln_veg_cage.5)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_cage.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(dln_veg_cage = ifelse(is.na(dln_veg_cage), pred, dln_veg_cage))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
dln_veg_cage.6 <- macpass %>% filter(!is.na(dln_veg_cage) & Month == 6)
dln_veg_cage.6.fit <- lm(dln_veg_cage ~ gf.soil, data = dln_veg_cage.6)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_cage.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(dln_veg_cage = ifelse(is.na(dln_veg_cage), pred, dln_veg_cage))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
dln_veg_cage.7 <- macpass %>% filter(!is.na(dln_veg_cage) & Month == 7)
dln_veg_cage.7.fit <- lm(dln_veg_cage ~ gf.soil, data = dln_veg_cage.7)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_cage.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(dln_veg_cage = ifelse(is.na(dln_veg_cage), pred, dln_veg_cage))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
dln_veg_cage.8 <- macpass %>% filter(!is.na(dln_veg_cage) & Month == 8)
dln_veg_cage.8.fit <- lm(dln_veg_cage ~ gf.soil, data = dln_veg_cage.8)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_cage.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(dln_veg_cage = ifelse(is.na(dln_veg_cage), pred, dln_veg_cage))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
dln_veg_cage.9 <- macpass %>% filter(!is.na(dln_veg_cage) & Month == 9)
dln_veg_cage.9.fit <- lm(dln_veg_cage ~ gf.soil, data = dln_veg_cage.9)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_cage.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(dln_veg_cage = ifelse(is.na(dln_veg_cage), pred, dln_veg_cage))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
dln_veg_cage.10 <- macpass %>% filter(!is.na(dln_veg_cage) & Month == 10)
dln_veg_cage.10.fit <- lm(dln_veg_cage ~ gf.soil, data = dln_veg_cage.10)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_cage.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(dln_veg_cage = ifelse(is.na(dln_veg_cage), pred, dln_veg_cage))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
dln_veg_cage.11 <- macpass %>% filter(!is.na(dln_veg_cage) & Month == 11)
dln_veg_cage.11.fit <- lm(dln_veg_cage ~ gf.soil, data = dln_veg_cage.11)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_cage.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(dln_veg_cage = ifelse(is.na(dln_veg_cage), pred, dln_veg_cage))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
dln_veg_cage.12 <- macpass %>% filter(!is.na(dln_veg_cage) & Month == 12)
dln_veg_cage.12.fit <- lm(dln_veg_cage ~ gf.soil, data = dln_veg_cage.12)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_cage.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(dln_veg_cage = ifelse(is.na(dln_veg_cage), pred, dln_veg_cage))
macpass %>% as.data.frame()


##********************************
### dln_veg_no
## January
# Construct linear model based on non-NA pairs
dln_veg_no.1 <- macpass %>% filter(!is.na(dln_veg_no) & Month == 1)
dln_veg_no.1.fit <- lm(dln_veg_no ~ gf.soil, data = dln_veg_no.1)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_no.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(dln_veg_no = ifelse(is.na(dln_veg_no), pred, dln_veg_no))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
dln_veg_no.2 <- macpass %>% filter(!is.na(dln_veg_no) & Month == 2)
dln_veg_no.2.fit <- lm(dln_veg_no ~ gf.soil, data = dln_veg_no.2)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_no.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(dln_veg_no = ifelse(is.na(dln_veg_no), pred, dln_veg_no))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
dln_veg_no.3 <- macpass %>% filter(!is.na(dln_veg_no) & Month == 3)
dln_veg_no.3.fit <- lm(dln_veg_no ~ gf.soil, data = dln_veg_no.3)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_no.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(dln_veg_no = ifelse(is.na(dln_veg_no), pred, dln_veg_no))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
dln_veg_no.4 <- macpass %>% filter(!is.na(dln_veg_no) & Month == 4)
dln_veg_no.4.fit <- lm(dln_veg_no ~ gf.soil, data = dln_veg_no.4)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_no.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(dln_veg_no = ifelse(is.na(dln_veg_no), pred, dln_veg_no))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
dln_veg_no.5 <- macpass %>% filter(!is.na(dln_veg_no) & Month == 5)
dln_veg_no.5.fit <- lm(dln_veg_no ~ gf.soil, data = dln_veg_no.5)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_no.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(dln_veg_no = ifelse(is.na(dln_veg_no), pred, dln_veg_no))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
dln_veg_no.6 <- macpass %>% filter(!is.na(dln_veg_no) & Month == 6)
dln_veg_no.6.fit <- lm(dln_veg_no ~ gf.soil, data = dln_veg_no.6)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_no.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(dln_veg_no = ifelse(is.na(dln_veg_no), pred, dln_veg_no))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
dln_veg_no.7 <- macpass %>% filter(!is.na(dln_veg_no) & Month == 7)
dln_veg_no.7.fit <- lm(dln_veg_no ~ gf.soil, data = dln_veg_no.7)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_no.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(dln_veg_no = ifelse(is.na(dln_veg_no), pred, dln_veg_no))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
dln_veg_no.8 <- macpass %>% filter(!is.na(dln_veg_no) & Month == 8)
dln_veg_no.8.fit <- lm(dln_veg_no ~ gf.soil, data = dln_veg_no.8)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_no.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(dln_veg_no = ifelse(is.na(dln_veg_no), pred, dln_veg_no))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
dln_veg_no.9 <- macpass %>% filter(!is.na(dln_veg_no) & Month == 9)
dln_veg_no.9.fit <- lm(dln_veg_no ~ gf.soil, data = dln_veg_no.9)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_no.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(dln_veg_no = ifelse(is.na(dln_veg_no), pred, dln_veg_no))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
dln_veg_no.10 <- macpass %>% filter(!is.na(dln_veg_no) & Month == 10)
dln_veg_no.10.fit <- lm(dln_veg_no ~ gf.soil, data = dln_veg_no.10)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_no.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(dln_veg_no = ifelse(is.na(dln_veg_no), pred, dln_veg_no))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
dln_veg_no.11 <- macpass %>% filter(!is.na(dln_veg_no) & Month == 11)
dln_veg_no.11.fit <- lm(dln_veg_no ~ gf.soil, data = dln_veg_no.11)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_no.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(dln_veg_no = ifelse(is.na(dln_veg_no), pred, dln_veg_no))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
dln_veg_no.12 <- macpass %>% filter(!is.na(dln_veg_no) & Month == 12)
dln_veg_no.12.fit <- lm(dln_veg_no ~ gf.soil, data = dln_veg_no.12)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_no.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(dln_veg_no = ifelse(is.na(dln_veg_no), pred, dln_veg_no))
macpass %>% as.data.frame()



############################################################
############################################################
############################################################
############################################################

##********************************
### dls_sca_cage
## January
# Construct linear model based on non-NA pairs
dls_sca_cage.1 <- macpass %>% filter(!is.na(dls_sca_cage) & Month == 1)
dls_sca_cage.1.fit <- lm(dls_sca_cage ~ gf.soil, data = dls_sca_cage.1)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_cage.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(dls_sca_cage = ifelse(is.na(dls_sca_cage), pred, dls_sca_cage))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
dls_sca_cage.2 <- macpass %>% filter(!is.na(dls_sca_cage) & Month == 2)
dls_sca_cage.2.fit <- lm(dls_sca_cage ~ gf.soil, data = dls_sca_cage.2)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_cage.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(dls_sca_cage = ifelse(is.na(dls_sca_cage), pred, dls_sca_cage))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
dls_sca_cage.3 <- macpass %>% filter(!is.na(dls_sca_cage) & Month == 3)
dls_sca_cage.3.fit <- lm(dls_sca_cage ~ gf.soil, data = dls_sca_cage.3)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_cage.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(dls_sca_cage = ifelse(is.na(dls_sca_cage), pred, dls_sca_cage))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
dls_sca_cage.4 <- macpass %>% filter(!is.na(dls_sca_cage) & Month == 4)
dls_sca_cage.4.fit <- lm(dls_sca_cage ~ gf.soil, data = dls_sca_cage.4)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_cage.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(dls_sca_cage = ifelse(is.na(dls_sca_cage), pred, dls_sca_cage))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
dls_sca_cage.5 <- macpass %>% filter(!is.na(dls_sca_cage) & Month == 5)
dls_sca_cage.5.fit <- lm(dls_sca_cage ~ gf.soil, data = dls_sca_cage.5)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_cage.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(dls_sca_cage = ifelse(is.na(dls_sca_cage), pred, dls_sca_cage))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
dls_sca_cage.6 <- macpass %>% filter(!is.na(dls_sca_cage) & Month == 6)
dls_sca_cage.6.fit <- lm(dls_sca_cage ~ gf.soil, data = dls_sca_cage.6)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_cage.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(dls_sca_cage = ifelse(is.na(dls_sca_cage), pred, dls_sca_cage))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
dls_sca_cage.7 <- macpass %>% filter(!is.na(dls_sca_cage) & Month == 7)
dls_sca_cage.7.fit <- lm(dls_sca_cage ~ gf.soil, data = dls_sca_cage.7)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_cage.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(dls_sca_cage = ifelse(is.na(dls_sca_cage), pred, dls_sca_cage))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
dls_sca_cage.8 <- macpass %>% filter(!is.na(dls_sca_cage) & Month == 8)
dls_sca_cage.8.fit <- lm(dls_sca_cage ~ gf.soil, data = dls_sca_cage.8)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_cage.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(dls_sca_cage = ifelse(is.na(dls_sca_cage), pred, dls_sca_cage))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
dls_sca_cage.9 <- macpass %>% filter(!is.na(dls_sca_cage) & Month == 9)
dls_sca_cage.9.fit <- lm(dls_sca_cage ~ gf.soil, data = dls_sca_cage.9)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_cage.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(dls_sca_cage = ifelse(is.na(dls_sca_cage), pred, dls_sca_cage))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
dls_sca_cage.10 <- macpass %>% filter(!is.na(dls_sca_cage) & Month == 10)
dls_sca_cage.10.fit <- lm(dls_sca_cage ~ gf.soil, data = dls_sca_cage.10)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_cage.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(dls_sca_cage = ifelse(is.na(dls_sca_cage), pred, dls_sca_cage))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
dls_sca_cage.11 <- macpass %>% filter(!is.na(dls_sca_cage) & Month == 11)
dls_sca_cage.11.fit <- lm(dls_sca_cage ~ gf.soil, data = dls_sca_cage.11)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_cage.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(dls_sca_cage = ifelse(is.na(dls_sca_cage), pred, dls_sca_cage))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
dls_sca_cage.12 <- macpass %>% filter(!is.na(dls_sca_cage) & Month == 12)
dls_sca_cage.12.fit <- lm(dls_sca_cage ~ gf.soil, data = dls_sca_cage.12)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_cage.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(dls_sca_cage = ifelse(is.na(dls_sca_cage), pred, dls_sca_cage))
macpass %>% as.data.frame()



##********************************
### dls_sca_no
## January
# Construct linear model based on non-NA pairs
dls_sca_no.1 <- macpass %>% filter(!is.na(dls_sca_no) & Month == 1)
dls_sca_no.1.fit <- lm(dls_sca_no ~ gf.soil, data = dls_sca_no.1)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_no.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(dls_sca_no = ifelse(is.na(dls_sca_no), pred, dls_sca_no))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
dls_sca_no.2 <- macpass %>% filter(!is.na(dls_sca_no) & Month == 2)
dls_sca_no.2.fit <- lm(dls_sca_no ~ gf.soil, data = dls_sca_no.2)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_no.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(dls_sca_no = ifelse(is.na(dls_sca_no), pred, dls_sca_no))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
dls_sca_no.3 <- macpass %>% filter(!is.na(dls_sca_no) & Month == 3)
dls_sca_no.3.fit <- lm(dls_sca_no ~ gf.soil, data = dls_sca_no.3)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_no.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(dls_sca_no = ifelse(is.na(dls_sca_no), pred, dls_sca_no))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
dls_sca_no.4 <- macpass %>% filter(!is.na(dls_sca_no) & Month == 4)
dls_sca_no.4.fit <- lm(dls_sca_no ~ gf.soil, data = dls_sca_no.4)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_no.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(dls_sca_no = ifelse(is.na(dls_sca_no), pred, dls_sca_no))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
dls_sca_no.5 <- macpass %>% filter(!is.na(dls_sca_no) & Month == 5)
dls_sca_no.5.fit <- lm(dls_sca_no ~ gf.soil, data = dls_sca_no.5)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_no.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(dls_sca_no = ifelse(is.na(dls_sca_no), pred, dls_sca_no))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
dls_sca_no.6 <- macpass %>% filter(!is.na(dls_sca_no) & Month == 6)
dls_sca_no.6.fit <- lm(dls_sca_no ~ gf.soil, data = dls_sca_no.6)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_no.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(dls_sca_no = ifelse(is.na(dls_sca_no), pred, dls_sca_no))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
dls_sca_no.7 <- macpass %>% filter(!is.na(dls_sca_no) & Month == 7)
dls_sca_no.7.fit <- lm(dls_sca_no ~ gf.soil, data = dls_sca_no.7)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_no.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(dls_sca_no = ifelse(is.na(dls_sca_no), pred, dls_sca_no))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
dls_sca_no.8 <- macpass %>% filter(!is.na(dls_sca_no) & Month == 8)
dls_sca_no.8.fit <- lm(dls_sca_no ~ gf.soil, data = dls_sca_no.8)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_no.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(dls_sca_no = ifelse(is.na(dls_sca_no), pred, dls_sca_no))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
dls_sca_no.9 <- macpass %>% filter(!is.na(dls_sca_no) & Month == 9)
dls_sca_no.9.fit <- lm(dls_sca_no ~ gf.soil, data = dls_sca_no.9)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_no.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(dls_sca_no = ifelse(is.na(dls_sca_no), pred, dls_sca_no))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
dls_sca_no.10 <- macpass %>% filter(!is.na(dls_sca_no) & Month == 10)
dls_sca_no.10.fit <- lm(dls_sca_no ~ gf.soil, data = dls_sca_no.10)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_no.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(dls_sca_no = ifelse(is.na(dls_sca_no), pred, dls_sca_no))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
dls_sca_no.11 <- macpass %>% filter(!is.na(dls_sca_no) & Month == 11)
dls_sca_no.11.fit <- lm(dls_sca_no ~ gf.soil, data = dls_sca_no.11)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_no.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(dls_sca_no = ifelse(is.na(dls_sca_no), pred, dls_sca_no))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
dls_sca_no.12 <- macpass %>% filter(!is.na(dls_sca_no) & Month == 12)
dls_sca_no.12.fit <- lm(dls_sca_no ~ gf.soil, data = dls_sca_no.12)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_no.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(dls_sca_no = ifelse(is.na(dls_sca_no), pred, dls_sca_no))
macpass %>% as.data.frame()





##********************************
### dls_veg_cage
## January
# Construct linear model based on non-NA pairs
dls_veg_cage.1 <- macpass %>% filter(!is.na(dls_veg_cage) & Month == 1)
dls_veg_cage.1.fit <- lm(dls_veg_cage ~ gf.soil, data = dls_veg_cage.1)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_cage.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(dls_veg_cage = ifelse(is.na(dls_veg_cage), pred, dls_veg_cage))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
dls_veg_cage.2 <- macpass %>% filter(!is.na(dls_veg_cage) & Month == 2)
dls_veg_cage.2.fit <- lm(dls_veg_cage ~ gf.soil, data = dls_veg_cage.2)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_cage.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(dls_veg_cage = ifelse(is.na(dls_veg_cage), pred, dls_veg_cage))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
dls_veg_cage.3 <- macpass %>% filter(!is.na(dls_veg_cage) & Month == 3)
dls_veg_cage.3.fit <- lm(dls_veg_cage ~ gf.soil, data = dls_veg_cage.3)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_cage.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(dls_veg_cage = ifelse(is.na(dls_veg_cage), pred, dls_veg_cage))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
dls_veg_cage.4 <- macpass %>% filter(!is.na(dls_veg_cage) & Month == 4)
dls_veg_cage.4.fit <- lm(dls_veg_cage ~ gf.soil, data = dls_veg_cage.4)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_cage.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(dls_veg_cage = ifelse(is.na(dls_veg_cage), pred, dls_veg_cage))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
dls_veg_cage.5 <- macpass %>% filter(!is.na(dls_veg_cage) & Month == 5)
dls_veg_cage.5.fit <- lm(dls_veg_cage ~ gf.soil, data = dls_veg_cage.5)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_cage.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(dls_veg_cage = ifelse(is.na(dls_veg_cage), pred, dls_veg_cage))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
dls_veg_cage.6 <- macpass %>% filter(!is.na(dls_veg_cage) & Month == 6)
dls_veg_cage.6.fit <- lm(dls_veg_cage ~ gf.soil, data = dls_veg_cage.6)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_cage.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(dls_veg_cage = ifelse(is.na(dls_veg_cage), pred, dls_veg_cage))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
dls_veg_cage.7 <- macpass %>% filter(!is.na(dls_veg_cage) & Month == 7)
dls_veg_cage.7.fit <- lm(dls_veg_cage ~ gf.soil, data = dls_veg_cage.7)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_cage.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(dls_veg_cage = ifelse(is.na(dls_veg_cage), pred, dls_veg_cage))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
dls_veg_cage.8 <- macpass %>% filter(!is.na(dls_veg_cage) & Month == 8)
dls_veg_cage.8.fit <- lm(dls_veg_cage ~ gf.soil, data = dls_veg_cage.8)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_cage.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(dls_veg_cage = ifelse(is.na(dls_veg_cage), pred, dls_veg_cage))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
dls_veg_cage.9 <- macpass %>% filter(!is.na(dls_veg_cage) & Month == 9)
dls_veg_cage.9.fit <- lm(dls_veg_cage ~ gf.soil, data = dls_veg_cage.9)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_cage.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(dls_veg_cage = ifelse(is.na(dls_veg_cage), pred, dls_veg_cage))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
dls_veg_cage.10 <- macpass %>% filter(!is.na(dls_veg_cage) & Month == 10)
dls_veg_cage.10.fit <- lm(dls_veg_cage ~ gf.soil, data = dls_veg_cage.10)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_cage.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(dls_veg_cage = ifelse(is.na(dls_veg_cage), pred, dls_veg_cage))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
dls_veg_cage.11 <- macpass %>% filter(!is.na(dls_veg_cage) & Month == 11)
dls_veg_cage.11.fit <- lm(dls_veg_cage ~ gf.soil, data = dls_veg_cage.11)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_cage.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(dls_veg_cage = ifelse(is.na(dls_veg_cage), pred, dls_veg_cage))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
dls_veg_cage.12 <- macpass %>% filter(!is.na(dls_veg_cage) & Month == 12)
dls_veg_cage.12.fit <- lm(dls_veg_cage ~ gf.soil, data = dls_veg_cage.12)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_cage.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(dls_veg_cage = ifelse(is.na(dls_veg_cage), pred, dls_veg_cage))
macpass %>% as.data.frame()


##********************************
### dls_veg_no
## January
# Construct linear model based on non-NA pairs
dls_veg_no.1 <- macpass %>% filter(!is.na(dls_veg_no) & Month == 1)
dls_veg_no.1.fit <- lm(dls_veg_no ~ gf.soil, data = dls_veg_no.1)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_no.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(dls_veg_no = ifelse(is.na(dls_veg_no), pred, dls_veg_no))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
dls_veg_no.2 <- macpass %>% filter(!is.na(dls_veg_no) & Month == 2)
dls_veg_no.2.fit <- lm(dls_veg_no ~ gf.soil, data = dls_veg_no.2)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_no.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(dls_veg_no = ifelse(is.na(dls_veg_no), pred, dls_veg_no))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
dls_veg_no.3 <- macpass %>% filter(!is.na(dls_veg_no) & Month == 3)
dls_veg_no.3.fit <- lm(dls_veg_no ~ gf.soil, data = dls_veg_no.3)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_no.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(dls_veg_no = ifelse(is.na(dls_veg_no), pred, dls_veg_no))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
dls_veg_no.4 <- macpass %>% filter(!is.na(dls_veg_no) & Month == 4)
dls_veg_no.4.fit <- lm(dls_veg_no ~ gf.soil, data = dls_veg_no.4)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_no.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(dls_veg_no = ifelse(is.na(dls_veg_no), pred, dls_veg_no))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
dls_veg_no.5 <- macpass %>% filter(!is.na(dls_veg_no) & Month == 5)
dls_veg_no.5.fit <- lm(dls_veg_no ~ gf.soil, data = dls_veg_no.5)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_no.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(dls_veg_no = ifelse(is.na(dls_veg_no), pred, dls_veg_no))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
dls_veg_no.6 <- macpass %>% filter(!is.na(dls_veg_no) & Month == 6)
dls_veg_no.6.fit <- lm(dls_veg_no ~ gf.soil, data = dls_veg_no.6)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_no.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(dls_veg_no = ifelse(is.na(dls_veg_no), pred, dls_veg_no))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
dls_veg_no.7 <- macpass %>% filter(!is.na(dls_veg_no) & Month == 7)
dls_veg_no.7.fit <- lm(dls_veg_no ~ gf.soil, data = dls_veg_no.7)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_no.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(dls_veg_no = ifelse(is.na(dls_veg_no), pred, dls_veg_no))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
dls_veg_no.8 <- macpass %>% filter(!is.na(dls_veg_no) & Month == 8)
dls_veg_no.8.fit <- lm(dls_veg_no ~ gf.soil, data = dls_veg_no.8)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_no.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(dls_veg_no = ifelse(is.na(dls_veg_no), pred, dls_veg_no))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
dls_veg_no.9 <- macpass %>% filter(!is.na(dls_veg_no) & Month == 9)
dls_veg_no.9.fit <- lm(dls_veg_no ~ gf.soil, data = dls_veg_no.9)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_no.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(dls_veg_no = ifelse(is.na(dls_veg_no), pred, dls_veg_no))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
dls_veg_no.10 <- macpass %>% filter(!is.na(dls_veg_no) & Month == 10)
dls_veg_no.10.fit <- lm(dls_veg_no ~ gf.soil, data = dls_veg_no.10)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_no.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(dls_veg_no = ifelse(is.na(dls_veg_no), pred, dls_veg_no))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
dls_veg_no.11 <- macpass %>% filter(!is.na(dls_veg_no) & Month == 11)
dls_veg_no.11.fit <- lm(dls_veg_no ~ gf.soil, data = dls_veg_no.11)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_no.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(dls_veg_no = ifelse(is.na(dls_veg_no), pred, dls_veg_no))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
dls_veg_no.12 <- macpass %>% filter(!is.na(dls_veg_no) & Month == 12)
dls_veg_no.12.fit <- lm(dls_veg_no ~ gf.soil, data = dls_veg_no.12)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_no.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(dls_veg_no = ifelse(is.na(dls_veg_no), pred, dls_veg_no))
macpass %>% as.data.frame()

plot(macpass$bwc_sca_cage, type = "l", col = "blue")
lines(macpass2$bwc_sca_cage)

