rm(list = ls())

panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y, use="complete.obs")) 
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

### Microclimate

# Contains PPA, PPD, and AIR to be filled

pp <- read.csv(file = "~/Desktop/Workspace/Churchill/ppa_ppd_micro.csv", header = TRUE)

# PPA
plot(pp$ppa.wwBg.5, type = "l")
plot(pp$ppa.wwBg.10, type = "l")
plot(pp[,46], type = "l")
plot(pp[,47], type = "l")
plot(pp[,48], type = "l")
plot(pp[,49], type = "l")
plot(pp[,50], type = "l")
plot(pp[,51], type = "l")
matplot(pp[,c(6:10)], type = "l")
matplot(pp[,c(11:15)], type = "l")
matplot(pp[,c(16:20)], type = "l")
matplot(pp[,c(21:25)], type = "l")
matplot(pp[,c(26:30)], type = "l")
matplot(pp[,c(31:35)], type = "l")
matplot(pp[,c(36:40)], type = "l")
matplot(pp[,c(41:45)], type = "l")
matplot(pp[,c(46:50)], type = "l")
# All columns are aligned correctly

# PPD
plot(pp[,64], type = "l")
plot(pp[,65], type = "l")
plot(pp[,66], type = "l")
plot(pp[,67], type = "l")
plot(pp[,68], type = "l")
plot(pp[,69], type = "l")
plot(pp[,85], type = "l")
plot(pp[,86], type = "l")
plot(pp[,87], type = "l")
plot(pp[,88], type = "l")
plot(pp[,89], type = "l")
plot(pp[,90], type = "l")
plot(pp[,91], type = "l")
plot(pp[,92], type = "l")
plot(pp[,93], type = "l")
plot(pp[,94], type = "l")
matplot(pp[,c(52:56)], type = "l")
matplot(pp[,c(57:63)], type = "l")
matplot(pp[,c(64:70)], type = "l")
matplot(pp[,c(71:77)], type = "l")
matplot(pp[,c(78:84)], type = "l")
matplot(pp[,c(85:91)], type = "l")
matplot(pp[,c(92:94)], type = "l")

# pairs(pp[,c(52,96)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)


## Only fill 5-10,23,24,50,51
# PPA air temmperatures based on WSU
ppa.1.airT <- lm(pp[pp$month==1, 52] ~ pp[pp$month==1, 96]) # Jan
ppa.2.airT <- lm(pp[pp$month==2, 52] ~ pp[pp$month==2, 96]) # Feb
ppa.3.airT <- lm(pp[pp$month==3, 52] ~ pp[pp$month==3, 96]) # Mar
ppa.4.airT <- lm(pp[pp$month==4, 52] ~ pp[pp$month==4, 96]) # Apr
ppa.5.airT <- lm(pp[pp$month==5, 52] ~ pp[pp$month==5, 96]) # May
ppa.6.airT <- lm(pp[pp$month==6, 52] ~ pp[pp$month==6, 96]) # Jun
ppa.7.airT <- lm(pp[pp$month==7, 52] ~ pp[pp$month==7, 96]) # Jul
ppa.8.airT <- lm(pp[pp$month==8, 52] ~ pp[pp$month==8, 96]) # Aug
ppa.9.airT <- lm(pp[pp$month==9, 52] ~ pp[pp$month==9, 96]) # Sep
ppa.10.airT <- lm(pp[pp$month==10, 52] ~ pp[pp$month==10, 96]) # Oct
ppa.11.airT <- lm(pp[pp$month==11, 52] ~ pp[pp$month==11, 96]) # Nov
ppa.12.airT <- lm(pp[pp$month==12, 52] ~ pp[pp$month==12, 96]) # Dec
summary(ppa.1.airT)
summary(ppa.2.airT)
summary(ppa.3.airT)
summary(ppa.4.airT)
summary(ppa.5.airT)
summary(ppa.6.airT)
summary(ppa.7.airT)
summary(ppa.8.airT)
summary(ppa.9.airT)
summary(ppa.10.airT)
summary(ppa.11.airT)
summary(ppa.12.airT)

# Bind the coefficients together
ppa.airT <- data.frame(rbind(coef(ppa.1.airT),coef(ppa.2.airT),coef(ppa.3.airT),coef(ppa.4.airT),coef(ppa.5.airT),coef(ppa.6.airT),
                             coef(ppa.7.airT),coef(ppa.8.airT),coef(ppa.9.airT),coef(ppa.10.airT),coef(ppa.11.airT),coef(ppa.12.airT)))
names(ppa.airT) <- c("b0","wsu.airT")
rownames(ppa.airT) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace AIR air T NAs with calculated air temperatures
pp2 <- pp
pp2$ppa.Tair <- ifelse(pp2$month == 1 & is.na(pp2$ppa.Tair), (ppa.airT[1,1] + ppa.airT[1,2]*pp2$wsu.Temp.150cm), pp2$ppa.Tair)
pp2$ppa.Tair <- ifelse(pp2$month == 2 & is.na(pp2$ppa.Tair), (ppa.airT[2,1] + ppa.airT[2,2]*pp2$wsu.Temp.150cm), pp2$ppa.Tair)
pp2$ppa.Tair <- ifelse(pp2$month == 3 & is.na(pp2$ppa.Tair), (ppa.airT[3,1] + ppa.airT[3,2]*pp2$wsu.Temp.150cm), pp2$ppa.Tair)
pp2$ppa.Tair <- ifelse(pp2$month == 4 & is.na(pp2$ppa.Tair), (ppa.airT[4,1] + ppa.airT[4,2]*pp2$wsu.Temp.150cm), pp2$ppa.Tair)
pp2$ppa.Tair <- ifelse(pp2$month == 5 & is.na(pp2$ppa.Tair), (ppa.airT[5,1] + ppa.airT[5,2]*pp2$wsu.Temp.150cm), pp2$ppa.Tair)
pp2$ppa.Tair <- ifelse(pp2$month == 6 & is.na(pp2$ppa.Tair), (ppa.airT[6,1] + ppa.airT[6,2]*pp2$wsu.Temp.150cm), pp2$ppa.Tair)
pp2$ppa.Tair <- ifelse(pp2$month == 7 & is.na(pp2$ppa.Tair), (ppa.airT[7,1] + ppa.airT[7,2]*pp2$wsu.Temp.150cm), pp2$ppa.Tair)
pp2$ppa.Tair <- ifelse(pp2$month == 8 & is.na(pp2$ppa.Tair), (ppa.airT[8,1] + ppa.airT[8,2]*pp2$wsu.Temp.150cm), pp2$ppa.Tair)
pp2$ppa.Tair <- ifelse(pp2$month == 9 & is.na(pp2$ppa.Tair), (ppa.airT[9,1] + ppa.airT[9,2]*pp2$wsu.Temp.150cm), pp2$ppa.Tair)
pp2$ppa.Tair <- ifelse(pp2$month == 10 & is.na(pp2$ppa.Tair), (ppa.airT[10,1] + ppa.airT[10,2]*pp2$wsu.Temp.150cm), pp2$ppa.Tair)
pp2$ppa.Tair <- ifelse(pp2$month == 11 & is.na(pp2$ppa.Tair), (ppa.airT[11,1] + ppa.airT[11,2]*pp2$wsu.Temp.150cm), pp2$ppa.Tair)
pp2$ppa.Tair <- ifelse(pp2$month == 12 & is.na(pp2$ppa.Tair), (ppa.airT[12,1] + ppa.airT[12,2]*pp2$wsu.Temp.150cm), pp2$ppa.Tair)

plot(pp2$ppa.Tair, type = "l", col = "red")
lines(pp$ppa.Tair, col = "blue")

# pairs(pp[,c(100,5,6)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col5 <- lm(pp2[pp2$month==1, 5] ~ pp2[pp2$month==1, 52] + pp2[pp2$month==1, 99]) # Jan
ppa.2.col5 <- lm(pp2[pp2$month==2, 5] ~ pp2[pp2$month==2, 52] + pp2[pp2$month==2, 99]) # Feb
ppa.3.col5 <- lm(pp2[pp2$month==3, 5] ~ pp2[pp2$month==3, 52] + pp2[pp2$month==3, 99]) # Mar
ppa.4.col5 <- lm(pp2[pp2$month==4, 5] ~ pp2[pp2$month==4, 52] + pp2[pp2$month==4, 99]) # Apr
ppa.5.col5 <- lm(pp2[pp2$month==5, 5] ~ pp2[pp2$month==5, 52] + pp2[pp2$month==5, 99]) # May
ppa.6.col5 <- lm(pp2[pp2$month==6, 5] ~ pp2[pp2$month==6, 52] + pp2[pp2$month==6, 99]) # Jun
ppa.7.col5 <- lm(pp2[pp2$month==7, 5] ~ pp2[pp2$month==7, 52] + pp2[pp2$month==7, 99]) # Jul
ppa.8.col5 <- lm(pp2[pp2$month==8, 5] ~ pp2[pp2$month==8, 52] + pp2[pp2$month==8, 99]) # Aug
ppa.9.col5 <- lm(pp2[pp2$month==9, 5] ~ pp2[pp2$month==9, 52] + pp2[pp2$month==9, 99]) # Sep
ppa.10.col5 <- lm(pp2[pp2$month==10, 5] ~ pp2[pp2$month==10, 52] + pp2[pp2$month==10, 99]) # Oct
ppa.11.col5 <- lm(pp2[pp2$month==11, 5] ~ pp2[pp2$month==11, 52] + pp2[pp2$month==11, 99]) # Nov
ppa.12.col5 <- lm(pp2[pp2$month==12, 5] ~ pp2[pp2$month==12, 52] + pp2[pp2$month==12, 99]) # Dec
summary(ppa.1.col5)
summary(ppa.2.col5)
summary(ppa.3.col5)
summary(ppa.4.col5)
summary(ppa.5.col5)
summary(ppa.6.col5)
summary(ppa.7.col5)
summary(ppa.8.col5)
summary(ppa.9.col5)
summary(ppa.10.col5)
summary(ppa.11.col5)
summary(ppa.12.col5)

# Bind the coefficients together
ppa.col5 <- data.frame(rbind(coef(ppa.1.col5),coef(ppa.2.col5),coef(ppa.3.col5),coef(ppa.4.col5),coef(ppa.5.col5),coef(ppa.6.col5),
                             coef(ppa.7.col5),coef(ppa.8.col5),coef(ppa.9.col5),coef(ppa.10.col5),coef(ppa.11.col5),coef(ppa.12.col5)))
ppa.col5 <- cbind(ppa.col5, rbind(summary(ppa.1.col5)$adj.r.squared, summary(ppa.2.col5)$adj.r.squared, summary(ppa.3.col5)$adj.r.squared,
                                  summary(ppa.4.col5)$adj.r.squared, summary(ppa.5.col5)$adj.r.squared, summary(ppa.6.col5)$adj.r.squared,
                                  summary(ppa.7.col5)$adj.r.squared, summary(ppa.8.col5)$adj.r.squared, summary(ppa.9.col5)$adj.r.squared,
                                  summary(ppa.10.col5)$adj.r.squared, summary(ppa.11.col5)$adj.r.squared, summary(ppa.12.col5)$adj.r.squared))
names(ppa.col5) <- c("b0","ppa.airT","wsu.n15","r2adj")
rownames(ppa.col5) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.wwBg.5 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.wwBg.5), (ppa.col5[1,1] + ppa.col5[1,2]*pp2$wsu.Temp..15cm
                                                              + ppa.col5[1,3]*pp2$wsu.Temp..15cm), pp2$ppa.wwBg.5)
pp2$ppa.wwBg.5 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.wwBg.5), (ppa.col5[2,1] + ppa.col5[2,2]*pp2$wsu.Temp..15cm
                                                              + ppa.col5[2,3]*pp2$wsu.Temp..15cm), pp2$ppa.wwBg.5)
pp2$ppa.wwBg.5 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.wwBg.5), (ppa.col5[3,1] + ppa.col5[3,2]*pp2$wsu.Temp..15cm
                                                              + ppa.col5[3,3]*pp2$wsu.Temp..15cm), pp2$ppa.wwBg.5)
pp2$ppa.wwBg.5 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.wwBg.5), (ppa.col5[4,1] + ppa.col5[4,2]*pp2$wsu.Temp..15cm
                                                              + ppa.col5[4,3]*pp2$wsu.Temp..15cm), pp2$ppa.wwBg.5)
pp2$ppa.wwBg.5 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.wwBg.5), (ppa.col5[5,1] + ppa.col5[5,2]*pp2$wsu.Temp..15cm
                                                              + ppa.col5[5,3]*pp2$wsu.Temp..15cm), pp2$ppa.wwBg.5)
pp2$ppa.wwBg.5 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.wwBg.5), (ppa.col5[6,1] + ppa.col5[6,2]*pp2$wsu.Temp..15cm
                                                              + ppa.col5[6,3]*pp2$wsu.Temp..15cm), pp2$ppa.wwBg.5)
pp2$ppa.wwBg.5 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.wwBg.5), (ppa.col5[7,1] + ppa.col5[7,2]*pp2$wsu.Temp..15cm
                                                              + ppa.col5[7,3]*pp2$wsu.Temp..15cm), pp2$ppa.wwBg.5)
pp2$ppa.wwBg.5 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.wwBg.5), (ppa.col5[8,1] + ppa.col5[8,2]*pp2$wsu.Temp..15cm
                                                              + ppa.col5[8,3]*pp2$wsu.Temp..15cm), pp2$ppa.wwBg.5)
pp2$ppa.wwBg.5 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.wwBg.5), (ppa.col5[9,1] + ppa.col5[9,2]*pp2$wsu.Temp..15cm
                                                              + ppa.col5[9,3]*pp2$wsu.Temp..15cm), pp2$ppa.wwBg.5)
pp2$ppa.wwBg.5 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.wwBg.5), (ppa.col5[10,1] + ppa.col5[10,2]*pp2$wsu.Temp..15cm
                                                               + ppa.col5[10,3]*pp2$wsu.Temp..15cm), pp2$ppa.wwBg.5)
pp2$ppa.wwBg.5 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.wwBg.5), (ppa.col5[11,1] + ppa.col5[11,2]*pp2$wsu.Temp..15cm
                                                               + ppa.col5[11,3]*pp2$wsu.Temp..15cm), pp2$ppa.wwBg.5)
pp2$ppa.wwBg.5 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.wwBg.5), (ppa.col5[12,1] + ppa.col5[12,2]*pp2$wsu.Temp..15cm
                                                               + ppa.col5[12,3]*pp2$wsu.Temp..15cm), pp2$ppa.wwBg.5)

plot(pp2$ppa.wwBg.5, type = "l", col = "red")
lines(pp$ppa.wwBg.5, col = "blue")


################
# pairs(pp[,c(6,5)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col6 <- lm(pp2[pp2$month==1, 6] ~ pp2[pp2$month==1, 5]) # Jan
ppa.2.col6 <- lm(pp2[pp2$month==2, 6] ~ pp2[pp2$month==2, 5]) # Feb
ppa.3.col6 <- lm(pp2[pp2$month==3, 6] ~ pp2[pp2$month==3, 5]) # Mar
ppa.4.col6 <- lm(pp2[pp2$month==4, 6] ~ pp2[pp2$month==4, 5]) # Apr
ppa.5.col6 <- lm(pp2[pp2$month==5, 6] ~ pp2[pp2$month==5, 5]) # May
ppa.6.col6 <- lm(pp2[pp2$month==6, 6] ~ pp2[pp2$month==6, 5]) # Jun
ppa.7.col6 <- lm(pp2[pp2$month==7, 6] ~ pp2[pp2$month==7, 5]) # Jul
ppa.8.col6 <- lm(pp2[pp2$month==8, 6] ~ pp2[pp2$month==8, 5]) # Aug
ppa.9.col6 <- lm(pp2[pp2$month==9, 6] ~ pp2[pp2$month==9, 5]) # Sep
ppa.10.col6 <- lm(pp2[pp2$month==10, 6] ~ pp2[pp2$month==10, 5]) # Oct
ppa.11.col6 <- lm(pp2[pp2$month==11, 6] ~ pp2[pp2$month==11, 5]) # Nov
ppa.12.col6 <- lm(pp2[pp2$month==12, 6] ~ pp2[pp2$month==12, 5]) # Dec
summary(ppa.1.col6)
summary(ppa.2.col6)
summary(ppa.3.col6)
summary(ppa.4.col6)
summary(ppa.5.col6)
summary(ppa.6.col6)
summary(ppa.7.col6)
summary(ppa.8.col6)
summary(ppa.9.col6)
summary(ppa.10.col6)
summary(ppa.11.col6)
summary(ppa.12.col6)

# Bind the coefficients together
ppa.col6 <- data.frame(rbind(coef(ppa.1.col6),coef(ppa.2.col6),coef(ppa.3.col6),coef(ppa.4.col6),coef(ppa.5.col6),coef(ppa.6.col6),
                             coef(ppa.7.col6),coef(ppa.8.col6),coef(ppa.9.col6),coef(ppa.10.col6),coef(ppa.11.col6),coef(ppa.12.col6)))
ppa.col6 <- cbind(ppa.col6, rbind(summary(ppa.1.col6)$adj.r.squared, summary(ppa.2.col6)$adj.r.squared, summary(ppa.3.col6)$adj.r.squared,
                                  summary(ppa.4.col6)$adj.r.squared, summary(ppa.5.col6)$adj.r.squared, summary(ppa.6.col6)$adj.r.squared,
                                  summary(ppa.7.col6)$adj.r.squared, summary(ppa.8.col6)$adj.r.squared, summary(ppa.9.col6)$adj.r.squared,
                                  summary(ppa.10.col6)$adj.r.squared, summary(ppa.11.col6)$adj.r.squared, summary(ppa.12.col6)$adj.r.squared))
names(ppa.col6) <- c("b0","ppa.col6","r2adj")
rownames(ppa.col6) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.wwBg.10 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.wwBg.10), (ppa.col6[1,1] + ppa.col6[1,2]*pp2$ppa.wwBg.5), pp2$ppa.wwBg.10)
pp2$ppa.wwBg.10 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.wwBg.10), (ppa.col6[2,1] + ppa.col6[2,2]*pp2$ppa.wwBg.5), pp2$ppa.wwBg.10)
pp2$ppa.wwBg.10 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.wwBg.10), (ppa.col6[3,1] + ppa.col6[3,2]*pp2$ppa.wwBg.5), pp2$ppa.wwBg.10)
pp2$ppa.wwBg.10 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.wwBg.10), (ppa.col6[4,1] + ppa.col6[4,2]*pp2$ppa.wwBg.5), pp2$ppa.wwBg.10)
pp2$ppa.wwBg.10 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.wwBg.10), (ppa.col6[5,1] + ppa.col6[5,2]*pp2$ppa.wwBg.5), pp2$ppa.wwBg.10)
pp2$ppa.wwBg.10 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.wwBg.10), (ppa.col6[6,1] + ppa.col6[6,2]*pp2$ppa.wwBg.5), pp2$ppa.wwBg.10)
pp2$ppa.wwBg.10 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.wwBg.10), (ppa.col6[7,1] + ppa.col6[7,2]*pp2$ppa.wwBg.5), pp2$ppa.wwBg.10)
pp2$ppa.wwBg.10 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.wwBg.10), (ppa.col6[8,1] + ppa.col6[8,2]*pp2$ppa.wwBg.5), pp2$ppa.wwBg.10)
pp2$ppa.wwBg.10 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.wwBg.10), (ppa.col6[9,1] + ppa.col6[9,2]*pp2$ppa.wwBg.5), pp2$ppa.wwBg.10)
pp2$ppa.wwBg.10 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.wwBg.10), (ppa.col6[10,1] + ppa.col6[10,2]*pp2$ppa.wwBg.5), pp2$ppa.wwBg.10)
pp2$ppa.wwBg.10 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.wwBg.10), (ppa.col6[11,1] + ppa.col6[11,2]*pp2$ppa.wwBg.5), pp2$ppa.wwBg.10)
pp2$ppa.wwBg.10 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.wwBg.10), (ppa.col6[12,1] + ppa.col6[12,2]*pp2$ppa.wwBg.5), pp2$ppa.wwBg.10)

plot(pp2$ppa.wwBg.10, type = "l", col = "red")
lines(pp$ppa.wwBg.10, col = "blue")


################################################
################################################
# pairs(pp[,c(7,52,5)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col7 <- lm(pp2[pp2$month==1, 7] ~ pp2[pp2$month==1, 52]) # Jan
ppa.2.col7 <- lm(pp2[pp2$month==2, 7] ~ pp2[pp2$month==2, 52]) # Feb
ppa.3.col7 <- lm(pp2[pp2$month==3, 7] ~ pp2[pp2$month==3, 52]) # Mar
ppa.4.col7 <- lm(pp2[pp2$month==4, 7] ~ pp2[pp2$month==4, 52]) # Apr
ppa.5.col7 <- lm(pp2[pp2$month==5, 7] ~ pp2[pp2$month==5, 52]) # May
ppa.6.col7 <- lm(pp2[pp2$month==6, 7] ~ pp2[pp2$month==6, 52]) # Jun
ppa.7.col7 <- lm(pp2[pp2$month==7, 7] ~ pp2[pp2$month==7, 52]) # Jul
ppa.8.col7 <- lm(pp2[pp2$month==8, 7] ~ pp2[pp2$month==8, 52]) # Aug
ppa.9.col7 <- lm(pp2[pp2$month==9, 7] ~ pp2[pp2$month==9, 52]) # Sep
ppa.10.col7 <- lm(pp2[pp2$month==10, 7] ~ pp2[pp2$month==10, 52]) # Oct
ppa.11.col7 <- lm(pp2[pp2$month==11, 7] ~ pp2[pp2$month==11, 52]) # Nov
ppa.12.col7 <- lm(pp2[pp2$month==12, 7] ~ pp2[pp2$month==12, 52]) # Dec
summary(ppa.1.col7)
summary(ppa.2.col7)
summary(ppa.3.col7)
summary(ppa.4.col7)
summary(ppa.5.col7)
summary(ppa.6.col7)
summary(ppa.7.col7)
summary(ppa.8.col7)
summary(ppa.9.col7)
summary(ppa.10.col7)
summary(ppa.11.col7)
summary(ppa.12.col7)

# Bind the coefficients together
ppa.col7 <- data.frame(rbind(coef(ppa.1.col7),coef(ppa.2.col7),coef(ppa.3.col7),coef(ppa.4.col7),coef(ppa.5.col7),coef(ppa.6.col7),
                             coef(ppa.7.col7),coef(ppa.8.col7),coef(ppa.9.col7),coef(ppa.10.col7),coef(ppa.11.col7),coef(ppa.12.col7)))
ppa.col7 <- cbind(ppa.col7, rbind(summary(ppa.1.col7)$adj.r.squared, summary(ppa.2.col7)$adj.r.squared, summary(ppa.3.col7)$adj.r.squared,
                                  summary(ppa.4.col7)$adj.r.squared, summary(ppa.5.col7)$adj.r.squared, summary(ppa.6.col7)$adj.r.squared,
                                  summary(ppa.7.col7)$adj.r.squared, summary(ppa.8.col7)$adj.r.squared, summary(ppa.9.col7)$adj.r.squared,
                                  summary(ppa.10.col7)$adj.r.squared, summary(ppa.11.col7)$adj.r.squared, summary(ppa.12.col7)$adj.r.squared))
names(ppa.col7) <- c("b0","ppa.col7","r2adj")
rownames(ppa.col7) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.wwBg.5.1 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.wwBg.5.1), (ppa.col7[1,1] + ppa.col7[1,2]*pp2$ppa.Tair), pp2$ppa.wwBg.5.1)
pp2$ppa.wwBg.5.1 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.wwBg.5.1), (ppa.col7[2,1] + ppa.col7[2,2]*pp2$ppa.Tair), pp2$ppa.wwBg.5.1)
pp2$ppa.wwBg.5.1 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.wwBg.5.1), (ppa.col7[3,1] + ppa.col7[3,2]*pp2$ppa.Tair), pp2$ppa.wwBg.5.1)
pp2$ppa.wwBg.5.1 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.wwBg.5.1), (ppa.col7[4,1] + ppa.col7[4,2]*pp2$ppa.Tair), pp2$ppa.wwBg.5.1)
pp2$ppa.wwBg.5.1 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.wwBg.5.1), (ppa.col7[5,1] + ppa.col7[5,2]*pp2$ppa.Tair), pp2$ppa.wwBg.5.1)
pp2$ppa.wwBg.5.1 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.wwBg.5.1), (ppa.col7[6,1] + ppa.col7[6,2]*pp2$ppa.Tair), pp2$ppa.wwBg.5.1)
pp2$ppa.wwBg.5.1 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.wwBg.5.1), (ppa.col7[7,1] + ppa.col7[7,2]*pp2$ppa.Tair), pp2$ppa.wwBg.5.1)
pp2$ppa.wwBg.5.1 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.wwBg.5.1), (ppa.col7[8,1] + ppa.col7[8,2]*pp2$ppa.Tair), pp2$ppa.wwBg.5.1)
pp2$ppa.wwBg.5.1 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.wwBg.5.1), (ppa.col7[9,1] + ppa.col7[9,2]*pp2$ppa.Tair), pp2$ppa.wwBg.5.1)
pp2$ppa.wwBg.5.1 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.wwBg.5.1), (ppa.col7[10,1] + ppa.col7[10,2]*pp2$ppa.Tair), pp2$ppa.wwBg.5.1)
pp2$ppa.wwBg.5.1 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.wwBg.5.1), (ppa.col7[11,1] + ppa.col7[11,2]*pp2$ppa.Tair), pp2$ppa.wwBg.5.1)
pp2$ppa.wwBg.5.1 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.wwBg.5.1), (ppa.col7[12,1] + ppa.col7[12,2]*pp2$ppa.Tair), pp2$ppa.wwBg.5.1)

plot(pp2$ppa.wwBg.5.1, type = "l", col = "red")
lines(pp$ppa.wwBg.5.1, col = "blue")



################################################
################################################
# pairs(pp[,c(8,5,6,7,52)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col8 <- lm(pp2[pp2$month==1, 8] ~ pp2[pp2$month==1, 7] + pp2[pp2$month==1, 52]) # Jan
ppa.2.col8 <- lm(pp2[pp2$month==2, 8] ~ pp2[pp2$month==2, 7] + pp2[pp2$month==2, 52]) # Feb
ppa.3.col8 <- lm(pp2[pp2$month==3, 8] ~ pp2[pp2$month==3, 7] + pp2[pp2$month==3, 52]) # Mar
ppa.4.col8 <- lm(pp2[pp2$month==4, 8] ~ pp2[pp2$month==4, 7] + pp2[pp2$month==4, 52]) # Apr
ppa.5.col8 <- lm(pp2[pp2$month==5, 8] ~ pp2[pp2$month==5, 7] + pp2[pp2$month==5, 52]) # May
ppa.6.col8 <- lm(pp2[pp2$month==6, 8] ~ pp2[pp2$month==6, 7] + pp2[pp2$month==6, 52]) # Jun
ppa.7.col8 <- lm(pp2[pp2$month==7, 8] ~ pp2[pp2$month==7, 7] + pp2[pp2$month==7, 52]) # Jul
ppa.8.col8 <- lm(pp2[pp2$month==8, 8] ~ pp2[pp2$month==8, 7] + pp2[pp2$month==8, 52]) # Aug
ppa.9.col8 <- lm(pp2[pp2$month==9, 8] ~ pp2[pp2$month==9, 7] + pp2[pp2$month==9, 52]) # Sep
ppa.10.col8 <- lm(pp2[pp2$month==10, 8] ~ pp2[pp2$month==10, 7] + pp2[pp2$month==10, 52]) # Oct
ppa.11.col8 <- lm(pp2[pp2$month==11, 8] ~ pp2[pp2$month==11, 7] + pp2[pp2$month==11, 52]) # Nov
ppa.12.col8 <- lm(pp2[pp2$month==12, 8] ~ pp2[pp2$month==12, 7] + pp2[pp2$month==12, 52]) # Dec
summary(ppa.1.col8)
summary(ppa.2.col8)
summary(ppa.3.col8)
summary(ppa.4.col8)
summary(ppa.5.col8)
summary(ppa.6.col8)
summary(ppa.7.col8)
summary(ppa.8.col8)
summary(ppa.9.col8)
summary(ppa.10.col8)
summary(ppa.11.col8)
summary(ppa.12.col8)

# Bind the coefficients together
ppa.col8 <- data.frame(rbind(coef(ppa.1.col8),coef(ppa.2.col8),coef(ppa.3.col8),coef(ppa.4.col8),coef(ppa.5.col8),coef(ppa.6.col8),
                             coef(ppa.7.col8),coef(ppa.8.col8),coef(ppa.9.col8),coef(ppa.10.col8),coef(ppa.11.col8),coef(ppa.12.col8)))
ppa.col8 <- cbind(ppa.col8, rbind(summary(ppa.1.col8)$adj.r.squared, summary(ppa.2.col8)$adj.r.squared, summary(ppa.3.col8)$adj.r.squared,
                                  summary(ppa.4.col8)$adj.r.squared, summary(ppa.5.col8)$adj.r.squared, summary(ppa.6.col8)$adj.r.squared,
                                  summary(ppa.7.col8)$adj.r.squared, summary(ppa.8.col8)$adj.r.squared, summary(ppa.9.col8)$adj.r.squared,
                                  summary(ppa.10.col8)$adj.r.squared, summary(ppa.11.col8)$adj.r.squared, summary(ppa.12.col8)$adj.r.squared))
names(ppa.col8) <- c("b0","ppa.col7","ppa.col52","r2adj")
rownames(ppa.col8) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.cLdLi.15 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.cLdLi.15), (ppa.col8[1,1] + ppa.col8[1,3]*pp2$ppa.Tair
                                                                      + ppa.col8[1,2]*pp2$ppa.wwBg.5.1), pp2$ppa.cLdLi.15)
pp2$ppa.cLdLi.15 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.cLdLi.15), (ppa.col8[2,1] + ppa.col8[2,3]*pp2$ppa.Tair
                                                                      + ppa.col8[2,2]*pp2$ppa.wwBg.5.1), pp2$ppa.cLdLi.15)
pp2$ppa.cLdLi.15 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.cLdLi.15), (ppa.col8[3,1] + ppa.col8[3,3]*pp2$ppa.Tair
                                                                      + ppa.col8[3,2]*pp2$ppa.wwBg.5.1), pp2$ppa.cLdLi.15)
pp2$ppa.cLdLi.15 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.cLdLi.15), (ppa.col8[4,1] + ppa.col8[4,3]*pp2$ppa.Tair
                                                                      + ppa.col8[4,2]*pp2$ppa.wwBg.5.1), pp2$ppa.cLdLi.15)
pp2$ppa.cLdLi.15 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.cLdLi.15), (ppa.col8[5,1] + ppa.col8[5,3]*pp2$ppa.Tair
                                                                      + ppa.col8[5,2]*pp2$ppa.wwBg.5.1), pp2$ppa.cLdLi.15)
pp2$ppa.cLdLi.15 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.cLdLi.15), (ppa.col8[6,1] + ppa.col8[6,3]*pp2$ppa.Tair
                                                                      + ppa.col8[6,2]*pp2$ppa.wwBg.5.1), pp2$ppa.cLdLi.15)
pp2$ppa.cLdLi.15 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.cLdLi.15), (ppa.col8[7,1] + ppa.col8[7,3]*pp2$ppa.Tair
                                                                      + ppa.col8[7,2]*pp2$ppa.wwBg.5.1), pp2$ppa.cLdLi.15)
pp2$ppa.cLdLi.15 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.cLdLi.15), (ppa.col8[8,1] + ppa.col8[8,3]*pp2$ppa.Tair
                                                                      + ppa.col8[8,2]*pp2$ppa.wwBg.5.1), pp2$ppa.cLdLi.15)
pp2$ppa.cLdLi.15 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.cLdLi.15), (ppa.col8[9,1] + ppa.col8[9,3]*pp2$ppa.Tair
                                                                      + ppa.col8[9,2]*pp2$ppa.wwBg.5.1), pp2$ppa.cLdLi.15)
pp2$ppa.cLdLi.15 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.cLdLi.15), (ppa.col8[10,1] + ppa.col8[10,3]*pp2$ppa.Tair
                                                                       + ppa.col8[10,2]*pp2$ppa.wwBg.5.1), pp2$ppa.cLdLi.15)
pp2$ppa.cLdLi.15 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.cLdLi.15), (ppa.col8[11,1] + ppa.col8[11,3]*pp2$ppa.Tair
                                                                       + ppa.col8[11,2]*pp2$ppa.wwBg.5.1), pp2$ppa.cLdLi.15)
pp2$ppa.cLdLi.15 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.cLdLi.15), (ppa.col8[12,1] + ppa.col8[12,3]*pp2$ppa.Tair
                                                                       + ppa.col8[12,2]*pp2$ppa.wwBg.5.1), pp2$ppa.cLdLi.15)

plot(pp2$ppa.cLdLi.15, type = "l", col = "red")
lines(pp$ppa.cLdLi.15, col = "blue")




################################################
################################################
# pairs(pp[,c(9,8,52)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col9 <- lm(pp2[pp2$month==1, 9] ~ pp2[pp2$month==1, 8] + pp2[pp2$month==1, 52]) # Jan
ppa.2.col9 <- lm(pp2[pp2$month==2, 9] ~ pp2[pp2$month==2, 8] + pp2[pp2$month==2, 52]) # Feb
ppa.3.col9 <- lm(pp2[pp2$month==3, 9] ~ pp2[pp2$month==3, 8] + pp2[pp2$month==3, 52]) # Mar
ppa.4.col9 <- lm(pp2[pp2$month==4, 9] ~ pp2[pp2$month==4, 8] + pp2[pp2$month==4, 52]) # Apr
ppa.5.col9 <- lm(pp2[pp2$month==5, 9] ~ pp2[pp2$month==5, 8] + pp2[pp2$month==5, 52]) # May
ppa.6.col9 <- lm(pp2[pp2$month==6, 9] ~ pp2[pp2$month==6, 8] + pp2[pp2$month==6, 52]) # Jun
ppa.7.col9 <- lm(pp2[pp2$month==7, 9] ~ pp2[pp2$month==7, 8] + pp2[pp2$month==7, 52]) # Jul
ppa.8.col9 <- lm(pp2[pp2$month==8, 9] ~ pp2[pp2$month==8, 8] + pp2[pp2$month==8, 52]) # Aug
ppa.9.col9 <- lm(pp2[pp2$month==9, 9] ~ pp2[pp2$month==9, 8] + pp2[pp2$month==9, 52]) # Sep
ppa.10.col9 <- lm(pp2[pp2$month==10, 9] ~ pp2[pp2$month==10, 8] + pp2[pp2$month==10, 52]) # Oct
ppa.11.col9 <- lm(pp2[pp2$month==11, 9] ~ pp2[pp2$month==11, 8] + pp2[pp2$month==11, 52]) # Nov
ppa.12.col9 <- lm(pp2[pp2$month==12, 9] ~ pp2[pp2$month==12, 8] + pp2[pp2$month==12, 52]) # Dec
summary(ppa.1.col9)
summary(ppa.2.col9)
summary(ppa.3.col9)
summary(ppa.4.col9)
summary(ppa.5.col9)
summary(ppa.6.col9)
summary(ppa.7.col9)
summary(ppa.8.col9)
summary(ppa.9.col9)
summary(ppa.10.col9)
summary(ppa.11.col9)
summary(ppa.12.col9)

# Bind the coefficients together
ppa.col9 <- data.frame(rbind(coef(ppa.1.col9),coef(ppa.2.col9),coef(ppa.3.col9),coef(ppa.4.col9),coef(ppa.5.col9),coef(ppa.6.col9),
                             coef(ppa.7.col9),coef(ppa.8.col9),coef(ppa.9.col9),coef(ppa.10.col9),coef(ppa.11.col9),coef(ppa.12.col9)))
ppa.col9 <- cbind(ppa.col9, rbind(summary(ppa.1.col9)$adj.r.squared, summary(ppa.2.col9)$adj.r.squared, summary(ppa.3.col9)$adj.r.squared,
                                  summary(ppa.4.col9)$adj.r.squared, summary(ppa.5.col9)$adj.r.squared, summary(ppa.6.col9)$adj.r.squared,
                                  summary(ppa.7.col9)$adj.r.squared, summary(ppa.8.col9)$adj.r.squared, summary(ppa.9.col9)$adj.r.squared,
                                  summary(ppa.10.col9)$adj.r.squared, summary(ppa.11.col9)$adj.r.squared, summary(ppa.12.col9)$adj.r.squared))
names(ppa.col9) <- c("b0","ppa.col8","ppa.col52","r2adj")
rownames(ppa.col9) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.cLdLi.10 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.cLdLi.10), (ppa.col9[1,1] + ppa.col9[1,3]*pp2$ppa.Tair
                                                                      + ppa.col9[1,2]*pp2$ppa.cLdLi.15), pp2$ppa.cLdLi.10)
pp2$ppa.cLdLi.10 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.cLdLi.10), (ppa.col9[2,1] + ppa.col9[2,3]*pp2$ppa.Tair
                                                                      + ppa.col9[2,2]*pp2$ppa.cLdLi.15), pp2$ppa.cLdLi.10)
pp2$ppa.cLdLi.10 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.cLdLi.10), (ppa.col9[3,1] + ppa.col9[3,3]*pp2$ppa.Tair
                                                                      + ppa.col9[3,2]*pp2$ppa.cLdLi.15), pp2$ppa.cLdLi.10)
pp2$ppa.cLdLi.10 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.cLdLi.10), (ppa.col9[4,1] + ppa.col9[4,3]*pp2$ppa.Tair
                                                                      + ppa.col9[4,2]*pp2$ppa.cLdLi.15), pp2$ppa.cLdLi.10)
pp2$ppa.cLdLi.10 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.cLdLi.10), (ppa.col9[5,1] + ppa.col9[5,3]*pp2$ppa.Tair
                                                                      + ppa.col9[5,2]*pp2$ppa.cLdLi.15), pp2$ppa.cLdLi.10)
pp2$ppa.cLdLi.10 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.cLdLi.10), (ppa.col9[6,1] + ppa.col9[6,3]*pp2$ppa.Tair
                                                                      + ppa.col9[6,2]*pp2$ppa.cLdLi.15), pp2$ppa.cLdLi.10)
pp2$ppa.cLdLi.10 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.cLdLi.10), (ppa.col9[7,1] + ppa.col9[7,3]*pp2$ppa.Tair
                                                                      + ppa.col9[7,2]*pp2$ppa.cLdLi.15), pp2$ppa.cLdLi.10)
pp2$ppa.cLdLi.10 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.cLdLi.10), (ppa.col9[8,1] + ppa.col9[8,3]*pp2$ppa.Tair
                                                                      + ppa.col9[8,2]*pp2$ppa.cLdLi.15), pp2$ppa.cLdLi.10)
pp2$ppa.cLdLi.10 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.cLdLi.10), (ppa.col9[9,1] + ppa.col9[9,3]*pp2$ppa.Tair
                                                                      + ppa.col9[9,2]*pp2$ppa.cLdLi.15), pp2$ppa.cLdLi.10)
pp2$ppa.cLdLi.10 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.cLdLi.10), (ppa.col9[10,1] + ppa.col9[10,3]*pp2$ppa.Tair
                                                                       + ppa.col9[10,2]*pp2$ppa.cLdLi.15), pp2$ppa.cLdLi.10)
pp2$ppa.cLdLi.10 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.cLdLi.10), (ppa.col9[11,1] + ppa.col9[11,3]*pp2$ppa.Tair
                                                                       + ppa.col9[11,2]*pp2$ppa.cLdLi.15), pp2$ppa.cLdLi.10)
pp2$ppa.cLdLi.10 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.cLdLi.10), (ppa.col9[12,1] + ppa.col9[12,3]*pp2$ppa.Tair
                                                                       + ppa.col9[12,2]*pp2$ppa.cLdLi.15), pp2$ppa.cLdLi.10)

plot(pp2$ppa.cLdLi.10, type = "l", col = "red")
lines(pp$ppa.cLdLi.10, col = "blue")





################################################
################################################
# pairs(pp[,c(10,8,52)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col10 <- lm(pp2[pp2$month==1, 10] ~ pp2[pp2$month==1, 8] + pp2[pp2$month==1, 52]) # Jan
ppa.2.col10 <- lm(pp2[pp2$month==2, 10] ~ pp2[pp2$month==2, 8] + pp2[pp2$month==2, 52]) # Feb
ppa.3.col10 <- lm(pp2[pp2$month==3, 10] ~ pp2[pp2$month==3, 8] + pp2[pp2$month==3, 52]) # Mar
ppa.4.col10 <- lm(pp2[pp2$month==4, 10] ~ pp2[pp2$month==4, 8] + pp2[pp2$month==4, 52]) # Apr
ppa.5.col10 <- lm(pp2[pp2$month==5, 10] ~ pp2[pp2$month==5, 8] + pp2[pp2$month==5, 52]) # May
ppa.6.col10 <- lm(pp2[pp2$month==6, 10] ~ pp2[pp2$month==6, 8] + pp2[pp2$month==6, 52]) # Jun
ppa.7.col10 <- lm(pp2[pp2$month==7, 10] ~ pp2[pp2$month==7, 8] + pp2[pp2$month==7, 52]) # Jul
ppa.8.col10 <- lm(pp2[pp2$month==8, 10] ~ pp2[pp2$month==8, 8] + pp2[pp2$month==8, 52]) # Aug
ppa.9.col10 <- lm(pp2[pp2$month==9, 10] ~ pp2[pp2$month==9, 8] + pp2[pp2$month==9, 52]) # Sep
ppa.10.col10 <- lm(pp2[pp2$month==10, 10] ~ pp2[pp2$month==10, 8] + pp2[pp2$month==10, 52]) # Oct
ppa.11.col10 <- lm(pp2[pp2$month==11, 10] ~ pp2[pp2$month==11, 8] + pp2[pp2$month==11, 52]) # Nov
ppa.12.col10 <- lm(pp2[pp2$month==12, 10] ~ pp2[pp2$month==12, 8] + pp2[pp2$month==12, 52]) # Dec
summary(ppa.1.col10)
summary(ppa.2.col10)
summary(ppa.3.col10)
summary(ppa.4.col10)
summary(ppa.5.col10)
summary(ppa.6.col10)
summary(ppa.7.col10)
summary(ppa.8.col10)
summary(ppa.9.col10)
summary(ppa.10.col10)
summary(ppa.11.col10)
summary(ppa.12.col10)

# Bind the coefficients together
ppa.col10 <- data.frame(rbind(coef(ppa.1.col10),coef(ppa.2.col10),coef(ppa.3.col10),coef(ppa.4.col10),coef(ppa.5.col10),coef(ppa.6.col10),
                             coef(ppa.7.col10),coef(ppa.8.col10),coef(ppa.9.col10),coef(ppa.10.col10),coef(ppa.11.col10),coef(ppa.12.col10)))
ppa.col10 <- cbind(ppa.col10, rbind(summary(ppa.1.col10)$adj.r.squared, summary(ppa.2.col10)$adj.r.squared, summary(ppa.3.col10)$adj.r.squared,
                                  summary(ppa.4.col10)$adj.r.squared, summary(ppa.5.col10)$adj.r.squared, summary(ppa.6.col10)$adj.r.squared,
                                  summary(ppa.7.col10)$adj.r.squared, summary(ppa.8.col10)$adj.r.squared, summary(ppa.9.col10)$adj.r.squared,
                                  summary(ppa.10.col10)$adj.r.squared, summary(ppa.11.col10)$adj.r.squared, summary(ppa.12.col10)$adj.r.squared))
names(ppa.col10) <- c("b0","ppa.col8","ppa.col52","r2adj")
rownames(ppa.col10) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.cLdLi.20 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.cLdLi.20), (ppa.col10[1,1] + ppa.col10[1,3]*pp2$ppa.Tair
                                                                      + ppa.col10[1,2]*pp2$ppa.cLdLi.15), pp2$ppa.cLdLi.20)
pp2$ppa.cLdLi.20 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.cLdLi.20), (ppa.col10[2,1] + ppa.col10[2,3]*pp2$ppa.Tair
                                                                      + ppa.col10[2,2]*pp2$ppa.cLdLi.15), pp2$ppa.cLdLi.20)
pp2$ppa.cLdLi.20 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.cLdLi.20), (ppa.col10[3,1] + ppa.col10[3,3]*pp2$ppa.Tair
                                                                      + ppa.col10[3,2]*pp2$ppa.cLdLi.15), pp2$ppa.cLdLi.20)
pp2$ppa.cLdLi.20 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.cLdLi.20), (ppa.col10[4,1] + ppa.col10[4,3]*pp2$ppa.Tair
                                                                      + ppa.col10[4,2]*pp2$ppa.cLdLi.15), pp2$ppa.cLdLi.20)
pp2$ppa.cLdLi.20 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.cLdLi.20), (ppa.col10[5,1] + ppa.col10[5,3]*pp2$ppa.Tair
                                                                      + ppa.col10[5,2]*pp2$ppa.cLdLi.15), pp2$ppa.cLdLi.20)
pp2$ppa.cLdLi.20 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.cLdLi.20), (ppa.col10[6,1] + ppa.col10[6,3]*pp2$ppa.Tair
                                                                      + ppa.col10[6,2]*pp2$ppa.cLdLi.15), pp2$ppa.cLdLi.20)
pp2$ppa.cLdLi.20 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.cLdLi.20), (ppa.col10[7,1] + ppa.col10[7,3]*pp2$ppa.Tair
                                                                      + ppa.col10[7,2]*pp2$ppa.cLdLi.15), pp2$ppa.cLdLi.20)
pp2$ppa.cLdLi.20 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.cLdLi.20), (ppa.col10[8,1] + ppa.col10[8,3]*pp2$ppa.Tair
                                                                      + ppa.col10[8,2]*pp2$ppa.cLdLi.15), pp2$ppa.cLdLi.20)
pp2$ppa.cLdLi.20 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.cLdLi.20), (ppa.col10[9,1] + ppa.col10[9,3]*pp2$ppa.Tair
                                                                      + ppa.col10[9,2]*pp2$ppa.cLdLi.15), pp2$ppa.cLdLi.20)
pp2$ppa.cLdLi.20 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.cLdLi.20), (ppa.col10[10,1] + ppa.col10[10,3]*pp2$ppa.Tair
                                                                       + ppa.col10[10,2]*pp2$ppa.cLdLi.15), pp2$ppa.cLdLi.20)
pp2$ppa.cLdLi.20 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.cLdLi.20), (ppa.col10[11,1] + ppa.col10[11,3]*pp2$ppa.Tair
                                                                       + ppa.col10[11,2]*pp2$ppa.cLdLi.15), pp2$ppa.cLdLi.20)
pp2$ppa.cLdLi.20 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.cLdLi.20), (ppa.col10[12,1] + ppa.col10[12,3]*pp2$ppa.Tair
                                                                       + ppa.col10[12,2]*pp2$ppa.cLdLi.15), pp2$ppa.cLdLi.20)

plot(pp2$ppa.cLdLi.20, type = "l", col = "red")
lines(pp$ppa.cLdLi.20, col = "blue")




################################################
################################################
# pairs(pp[,c(23,8,9,10,52)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col23 <- lm(pp2[pp2$month==1, 23] ~ pp2[pp2$month==1, 8] + pp2[pp2$month==1, 10]) # Jan
ppa.2.col23 <- lm(pp2[pp2$month==2, 23] ~ pp2[pp2$month==2, 8] + pp2[pp2$month==2, 10]) # Feb
ppa.3.col23 <- lm(pp2[pp2$month==3, 23] ~ pp2[pp2$month==3, 8] + pp2[pp2$month==3, 10]) # Mar
ppa.4.col23 <- lm(pp2[pp2$month==4, 23] ~ pp2[pp2$month==4, 8] + pp2[pp2$month==4, 10]) # Apr
ppa.5.col23 <- lm(pp2[pp2$month==5, 23] ~ pp2[pp2$month==5, 8] + pp2[pp2$month==5, 10]) # May
ppa.6.col23 <- lm(pp2[pp2$month==6, 23] ~ pp2[pp2$month==6, 8] + pp2[pp2$month==6, 10]) # Jun
ppa.7.col23 <- lm(pp2[pp2$month==7, 23] ~ pp2[pp2$month==7, 8] + pp2[pp2$month==7, 10]) # Jul
ppa.8.col23 <- lm(pp2[pp2$month==8, 23] ~ pp2[pp2$month==8, 8] + pp2[pp2$month==8, 10]) # Aug
ppa.9.col23 <- lm(pp2[pp2$month==9, 23] ~ pp2[pp2$month==9, 8] + pp2[pp2$month==9, 10]) # Sep
ppa.10.col23 <- lm(pp2[pp2$month==10, 23] ~ pp2[pp2$month==10, 8] + pp2[pp2$month==10, 10]) # Oct
ppa.11.col23 <- lm(pp2[pp2$month==11, 23] ~ pp2[pp2$month==11, 8] + pp2[pp2$month==11, 10]) # Nov
ppa.12.col23 <- lm(pp2[pp2$month==12, 23] ~ pp2[pp2$month==12, 8] + pp2[pp2$month==12, 10]) # Dec
summary(ppa.1.col23)
summary(ppa.2.col23)
summary(ppa.3.col23)
summary(ppa.4.col23)
summary(ppa.5.col23)
summary(ppa.6.col23)
summary(ppa.7.col23)
summary(ppa.8.col23)
summary(ppa.9.col23)
summary(ppa.10.col23)
summary(ppa.11.col23)
summary(ppa.12.col23)

# Bind the coefficients together
ppa.col23 <- data.frame(rbind(coef(ppa.1.col23),coef(ppa.2.col23),coef(ppa.3.col23),coef(ppa.4.col23),coef(ppa.5.col23),coef(ppa.6.col23),
                              coef(ppa.7.col23),coef(ppa.8.col23),coef(ppa.9.col23),coef(ppa.10.col23),coef(ppa.11.col23),coef(ppa.12.col23)))
ppa.col23 <- cbind(ppa.col23, rbind(summary(ppa.1.col23)$adj.r.squared, summary(ppa.2.col23)$adj.r.squared, summary(ppa.3.col23)$adj.r.squared,
                                    summary(ppa.4.col23)$adj.r.squared, summary(ppa.5.col23)$adj.r.squared, summary(ppa.6.col23)$adj.r.squared,
                                    summary(ppa.7.col23)$adj.r.squared, summary(ppa.8.col23)$adj.r.squared, summary(ppa.9.col23)$adj.r.squared,
                                    summary(ppa.10.col23)$adj.r.squared, summary(ppa.11.col23)$adj.r.squared, summary(ppa.12.col23)$adj.r.squared))
names(ppa.col23) <- c("b0","ppa.col8","ppa.col10","r2adj")
rownames(ppa.col23) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.wdMo.40 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.wdMo.40), (ppa.col23[1,1] + ppa.col23[1,3]*pp2$ppa.cLdLi.20
                                                                      + ppa.col23[1,2]*pp2$ppa.cLdLi.15), pp2$ppa.wdMo.40)
pp2$ppa.wdMo.40 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.wdMo.40), (ppa.col23[2,1] + ppa.col23[2,3]*pp2$ppa.cLdLi.20
                                                                      + ppa.col23[2,2]*pp2$ppa.cLdLi.15), pp2$ppa.wdMo.40)
pp2$ppa.wdMo.40 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.wdMo.40), (ppa.col23[3,1] + ppa.col23[3,3]*pp2$ppa.cLdLi.20
                                                                      + ppa.col23[3,2]*pp2$ppa.cLdLi.15), pp2$ppa.wdMo.40)
pp2$ppa.wdMo.40 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.wdMo.40), (ppa.col23[4,1] + ppa.col23[4,3]*pp2$ppa.cLdLi.20
                                                                      + ppa.col23[4,2]*pp2$ppa.cLdLi.15), pp2$ppa.wdMo.40)
pp2$ppa.wdMo.40 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.wdMo.40), (ppa.col23[5,1] + ppa.col23[5,3]*pp2$ppa.cLdLi.20
                                                                      + ppa.col23[5,2]*pp2$ppa.cLdLi.15), pp2$ppa.wdMo.40)
pp2$ppa.wdMo.40 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.wdMo.40), (ppa.col23[6,1] + ppa.col23[6,3]*pp2$ppa.cLdLi.20
                                                                      + ppa.col23[6,2]*pp2$ppa.cLdLi.15), pp2$ppa.wdMo.40)
pp2$ppa.wdMo.40 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.wdMo.40), (ppa.col23[7,1] + ppa.col23[7,3]*pp2$ppa.cLdLi.20
                                                                      + ppa.col23[7,2]*pp2$ppa.cLdLi.15), pp2$ppa.wdMo.40)
pp2$ppa.wdMo.40 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.wdMo.40), (ppa.col23[8,1] + ppa.col23[8,3]*pp2$ppa.cLdLi.20
                                                                      + ppa.col23[8,2]*pp2$ppa.cLdLi.15), pp2$ppa.wdMo.40)
pp2$ppa.wdMo.40 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.wdMo.40), (ppa.col23[9,1] + ppa.col23[9,3]*pp2$ppa.cLdLi.20
                                                                      + ppa.col23[9,2]*pp2$ppa.cLdLi.15), pp2$ppa.wdMo.40)
pp2$ppa.wdMo.40 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.wdMo.40), (ppa.col23[10,1] + ppa.col23[10,3]*pp2$ppa.cLdLi.20
                                                                       + ppa.col23[10,2]*pp2$ppa.cLdLi.15), pp2$ppa.wdMo.40)
pp2$ppa.wdMo.40 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.wdMo.40), (ppa.col23[11,1] + ppa.col23[11,3]*pp2$ppa.cLdLi.20
                                                                       + ppa.col23[11,2]*pp2$ppa.cLdLi.15), pp2$ppa.wdMo.40)
pp2$ppa.wdMo.40 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.wdMo.40), (ppa.col23[12,1] + ppa.col23[12,3]*pp2$ppa.cLdLi.20
                                                                       + ppa.col23[12,2]*pp2$ppa.cLdLi.15), pp2$ppa.wdMo.40)

plot(pp2$ppa.wdMo.40, type = "l", col = "red")
lines(pp$ppa.wdMo.40, col = "blue")




################################################
################################################
# pairs(pp[,c(24,8,10,23,52)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col24 <- lm(pp2[pp2$month==1, 24] ~ pp2[pp2$month==1, 23]) # Jan
ppa.2.col24 <- lm(pp2[pp2$month==2, 24] ~ pp2[pp2$month==2, 23]) # Feb
ppa.3.col24 <- lm(pp2[pp2$month==3, 24] ~ pp2[pp2$month==3, 23]) # Mar
ppa.4.col24 <- lm(pp2[pp2$month==4, 24] ~ pp2[pp2$month==4, 23]) # Apr
ppa.5.col24 <- lm(pp2[pp2$month==5, 24] ~ pp2[pp2$month==5, 23]) # May
ppa.6.col24 <- lm(pp2[pp2$month==6, 24] ~ pp2[pp2$month==6, 23]) # Jun
ppa.7.col24 <- lm(pp2[pp2$month==7, 24] ~ pp2[pp2$month==7, 23]) # Jul
ppa.8.col24 <- lm(pp2[pp2$month==8, 24] ~ pp2[pp2$month==8, 23]) # Aug
ppa.9.col24 <- lm(pp2[pp2$month==9, 24] ~ pp2[pp2$month==9, 23]) # Sep
ppa.10.col24 <- lm(pp2[pp2$month==10, 24] ~ pp2[pp2$month==10, 23]) # Oct
ppa.11.col24 <- lm(pp2[pp2$month==11, 24] ~ pp2[pp2$month==11, 23]) # Nov
ppa.12.col24 <- lm(pp2[pp2$month==12, 24] ~ pp2[pp2$month==12, 23]) # Dec
summary(ppa.1.col24)
summary(ppa.2.col24)
summary(ppa.3.col24)
summary(ppa.4.col24)
summary(ppa.5.col24)
summary(ppa.6.col24)
summary(ppa.7.col24)
summary(ppa.8.col24)
summary(ppa.9.col24)
summary(ppa.10.col24)
summary(ppa.11.col24)
summary(ppa.12.col24)

# Bind the coefficients together
ppa.col24 <- data.frame(rbind(coef(ppa.1.col24),coef(ppa.2.col24),coef(ppa.3.col24),coef(ppa.4.col24),coef(ppa.5.col24),coef(ppa.6.col24),
                              coef(ppa.7.col24),coef(ppa.8.col24),coef(ppa.9.col24),coef(ppa.10.col24),coef(ppa.11.col24),coef(ppa.12.col24)))
ppa.col24 <- cbind(ppa.col24, rbind(summary(ppa.1.col24)$adj.r.squared, summary(ppa.2.col24)$adj.r.squared, summary(ppa.3.col24)$adj.r.squared,
                                    summary(ppa.4.col24)$adj.r.squared, summary(ppa.5.col24)$adj.r.squared, summary(ppa.6.col24)$adj.r.squared,
                                    summary(ppa.7.col24)$adj.r.squared, summary(ppa.8.col24)$adj.r.squared, summary(ppa.9.col24)$adj.r.squared,
                                    summary(ppa.10.col24)$adj.r.squared, summary(ppa.11.col24)$adj.r.squared, summary(ppa.12.col24)$adj.r.squared))
names(ppa.col24) <- c("b0","ppa.col23","r2adj")
rownames(ppa.col24) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.wdMo.80 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.wdMo.80), (ppa.col24[1,1] + ppa.col24[1,2]*pp2$ppa.wdMo.40), pp2$ppa.wdMo.80)
pp2$ppa.wdMo.80 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.wdMo.80), (ppa.col24[2,1] + ppa.col24[2,2]*pp2$ppa.wdMo.40), pp2$ppa.wdMo.80)
pp2$ppa.wdMo.80 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.wdMo.80), (ppa.col24[3,1] + ppa.col24[3,2]*pp2$ppa.wdMo.40), pp2$ppa.wdMo.80)
pp2$ppa.wdMo.80 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.wdMo.80), (ppa.col24[4,1] + ppa.col24[4,2]*pp2$ppa.wdMo.40), pp2$ppa.wdMo.80)
pp2$ppa.wdMo.80 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.wdMo.80), (ppa.col24[5,1] + ppa.col24[5,2]*pp2$ppa.wdMo.40), pp2$ppa.wdMo.80)
pp2$ppa.wdMo.80 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.wdMo.80), (ppa.col24[6,1] + ppa.col24[6,2]*pp2$ppa.wdMo.40), pp2$ppa.wdMo.80)
pp2$ppa.wdMo.80 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.wdMo.80), (ppa.col24[7,1] + ppa.col24[7,2]*pp2$ppa.wdMo.40), pp2$ppa.wdMo.80)
pp2$ppa.wdMo.80 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.wdMo.80), (ppa.col24[8,1] + ppa.col24[8,2]*pp2$ppa.wdMo.40), pp2$ppa.wdMo.80)
pp2$ppa.wdMo.80 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.wdMo.80), (ppa.col24[9,1] + ppa.col24[9,2]*pp2$ppa.wdMo.40), pp2$ppa.wdMo.80)
pp2$ppa.wdMo.80 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.wdMo.80), (ppa.col24[10,1] + ppa.col24[10,2]*pp2$ppa.wdMo.40), pp2$ppa.wdMo.80)
pp2$ppa.wdMo.80 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.wdMo.80), (ppa.col24[11,1] + ppa.col24[11,2]*pp2$ppa.wdMo.40), pp2$ppa.wdMo.80)
pp2$ppa.wdMo.80 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.wdMo.80), (ppa.col24[12,1] + ppa.col24[12,2]*pp2$ppa.wdMo.40), pp2$ppa.wdMo.80)

plot(pp2$ppa.wdMo.80, type = "l", col = "red")
lines(pp$ppa.wdMo.80, col = "blue")





################################################
################################################
# pairs(pp[,c(50,8,9,10,23,24)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col50 <- lm(pp2[pp2$month==1, 50] ~ pp2[pp2$month==1, 10] + pp2[pp2$month==1, 23]) # Jan
ppa.2.col50 <- lm(pp2[pp2$month==2, 50] ~ pp2[pp2$month==2, 10] + pp2[pp2$month==2, 23]) # Feb
ppa.3.col50 <- lm(pp2[pp2$month==3, 50] ~ pp2[pp2$month==3, 10] + pp2[pp2$month==3, 23]) # Mar
ppa.4.col50 <- lm(pp2[pp2$month==4, 50] ~ pp2[pp2$month==4, 10] + pp2[pp2$month==4, 23]) # Apr
ppa.5.col50 <- lm(pp2[pp2$month==5, 50] ~ pp2[pp2$month==5, 10] + pp2[pp2$month==5, 23]) # May
ppa.6.col50 <- lm(pp2[pp2$month==6, 50] ~ pp2[pp2$month==6, 10] + pp2[pp2$month==6, 23]) # Jun
ppa.7.col50 <- lm(pp2[pp2$month==7, 50] ~ pp2[pp2$month==7, 10] + pp2[pp2$month==7, 23]) # Jul
ppa.8.col50 <- lm(pp2[pp2$month==8, 50] ~ pp2[pp2$month==8, 10] + pp2[pp2$month==8, 23]) # Aug
ppa.9.col50 <- lm(pp2[pp2$month==9, 50] ~ pp2[pp2$month==9, 10] + pp2[pp2$month==9, 23]) # Sep
ppa.10.col50 <- lm(pp2[pp2$month==10, 50] ~ pp2[pp2$month==10, 10] + pp2[pp2$month==10, 23]) # Oct
ppa.11.col50 <- lm(pp2[pp2$month==11, 50] ~ pp2[pp2$month==11, 10] + pp2[pp2$month==11, 23]) # Nov
ppa.12.col50 <- lm(pp2[pp2$month==12, 50] ~ pp2[pp2$month==12, 10] + pp2[pp2$month==12, 23]) # Dec
summary(ppa.1.col50)
summary(ppa.2.col50)
summary(ppa.3.col50)
summary(ppa.4.col50)
summary(ppa.5.col50)
summary(ppa.6.col50)
summary(ppa.7.col50)
summary(ppa.8.col50)
summary(ppa.9.col50)
summary(ppa.10.col50)
summary(ppa.11.col50)
summary(ppa.12.col50)

# Bind the coefficients together
ppa.col50 <- data.frame(rbind(coef(ppa.1.col50),coef(ppa.2.col50),coef(ppa.3.col50),coef(ppa.4.col50),coef(ppa.5.col50),coef(ppa.6.col50),
                              coef(ppa.7.col50),coef(ppa.8.col50),coef(ppa.9.col50),coef(ppa.10.col50),coef(ppa.11.col50),coef(ppa.12.col50)))
ppa.col50 <- cbind(ppa.col50, rbind(summary(ppa.1.col50)$adj.r.squared, summary(ppa.2.col50)$adj.r.squared, summary(ppa.3.col50)$adj.r.squared,
                                    summary(ppa.4.col50)$adj.r.squared, summary(ppa.5.col50)$adj.r.squared, summary(ppa.6.col50)$adj.r.squared,
                                    summary(ppa.7.col50)$adj.r.squared, summary(ppa.8.col50)$adj.r.squared, summary(ppa.9.col50)$adj.r.squared,
                                    summary(ppa.10.col50)$adj.r.squared, summary(ppa.11.col50)$adj.r.squared, summary(ppa.12.col50)$adj.r.squared))
names(ppa.col50) <- c("b0","ppa.col10",",ppa.col23","r2adj")
rownames(ppa.col50) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.cLdLi.40 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.cLdLi.40), (ppa.col50[1,1] + ppa.col50[1,2]*pp2$ppa.cLdLi.20
                                                                      + ppa.col50[1,3]*pp2$ppa.wdMo.40), pp2$ppa.cLdLi.40)
pp2$ppa.cLdLi.40 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.cLdLi.40), (ppa.col50[2,1] + ppa.col50[2,2]*pp2$ppa.cLdLi.20
                                                                      + ppa.col50[2,3]*pp2$ppa.wdMo.40), pp2$ppa.cLdLi.40)
pp2$ppa.cLdLi.40 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.cLdLi.40), (ppa.col50[3,1] + ppa.col50[3,2]*pp2$ppa.cLdLi.20
                                                                      + ppa.col50[3,3]*pp2$ppa.wdMo.40), pp2$ppa.cLdLi.40)
pp2$ppa.cLdLi.40 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.cLdLi.40), (ppa.col50[4,1] + ppa.col50[4,2]*pp2$ppa.cLdLi.20
                                                                      + ppa.col50[4,3]*pp2$ppa.wdMo.40), pp2$ppa.cLdLi.40)
pp2$ppa.cLdLi.40 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.cLdLi.40), (ppa.col50[5,1] + ppa.col50[5,2]*pp2$ppa.cLdLi.20
                                                                      + ppa.col50[5,3]*pp2$ppa.wdMo.40), pp2$ppa.cLdLi.40)
pp2$ppa.cLdLi.40 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.cLdLi.40), (ppa.col50[6,1] + ppa.col50[6,2]*pp2$ppa.cLdLi.20
                                                                      + ppa.col50[6,3]*pp2$ppa.wdMo.40), pp2$ppa.cLdLi.40)
pp2$ppa.cLdLi.40 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.cLdLi.40), (ppa.col50[7,1] + ppa.col50[7,2]*pp2$ppa.cLdLi.20
                                                                      + ppa.col50[7,3]*pp2$ppa.wdMo.40), pp2$ppa.cLdLi.40)
pp2$ppa.cLdLi.40 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.cLdLi.40), (ppa.col50[8,1] + ppa.col50[8,2]*pp2$ppa.cLdLi.20
                                                                      + ppa.col50[8,3]*pp2$ppa.wdMo.40), pp2$ppa.cLdLi.40)
pp2$ppa.cLdLi.40 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.cLdLi.40), (ppa.col50[9,1] + ppa.col50[9,2]*pp2$ppa.cLdLi.20
                                                                      + ppa.col50[9,3]*pp2$ppa.wdMo.40), pp2$ppa.cLdLi.40)
pp2$ppa.cLdLi.40 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.cLdLi.40), (ppa.col50[10,1] + ppa.col50[10,2]*pp2$ppa.cLdLi.20
                                                                       + ppa.col50[10,3]*pp2$ppa.wdMo.40), pp2$ppa.cLdLi.40)
pp2$ppa.cLdLi.40 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.cLdLi.40), (ppa.col50[11,1] + ppa.col50[11,2]*pp2$ppa.cLdLi.20
                                                                       + ppa.col50[11,3]*pp2$ppa.wdMo.40), pp2$ppa.cLdLi.40)
pp2$ppa.cLdLi.40 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.cLdLi.40), (ppa.col50[12,1] + ppa.col50[12,2]*pp2$ppa.cLdLi.20
                                                                       + ppa.col50[12,3]*pp2$ppa.wdMo.40), pp2$ppa.cLdLi.40)

plot(pp2$ppa.cLdLi.40, type = "l", col = "red")
lines(pp$ppa.cLdLi.40, col = "blue")





################################################
################################################
# pairs(pp[,c(51,8,9,10,23,24,50)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col51 <- lm(pp2[pp2$month==1, 51] ~ pp2[pp2$month==1, 24] + pp2[pp2$month==1, 50]) # Jan
ppa.2.col51 <- lm(pp2[pp2$month==2, 51] ~ pp2[pp2$month==2, 24] + pp2[pp2$month==2, 50]) # Feb
ppa.3.col51 <- lm(pp2[pp2$month==3, 51] ~ pp2[pp2$month==3, 24] + pp2[pp2$month==3, 50]) # Mar
ppa.4.col51 <- lm(pp2[pp2$month==4, 51] ~ pp2[pp2$month==4, 24] + pp2[pp2$month==4, 50]) # Apr
ppa.5.col51 <- lm(pp2[pp2$month==5, 51] ~ pp2[pp2$month==5, 24] + pp2[pp2$month==5, 50]) # May
ppa.6.col51 <- lm(pp2[pp2$month==6, 51] ~ pp2[pp2$month==6, 24] + pp2[pp2$month==6, 50]) # Jun
ppa.7.col51 <- lm(pp2[pp2$month==7, 51] ~ pp2[pp2$month==7, 24] + pp2[pp2$month==7, 50]) # Jul
ppa.8.col51 <- lm(pp2[pp2$month==8, 51] ~ pp2[pp2$month==8, 24] + pp2[pp2$month==8, 50]) # Aug
ppa.9.col51 <- lm(pp2[pp2$month==9, 51] ~ pp2[pp2$month==9, 24] + pp2[pp2$month==9, 50]) # Sep
ppa.10.col51 <- lm(pp2[pp2$month==10, 51] ~ pp2[pp2$month==10, 24] + pp2[pp2$month==10, 50]) # Oct
ppa.11.col51 <- lm(pp2[pp2$month==11, 51] ~ pp2[pp2$month==11, 24] + pp2[pp2$month==11, 50]) # Nov
ppa.12.col51 <- lm(pp2[pp2$month==12, 51] ~ pp2[pp2$month==12, 24] + pp2[pp2$month==12, 50]) # Dec
summary(ppa.1.col51)
summary(ppa.2.col51)
summary(ppa.3.col51)
summary(ppa.4.col51)
summary(ppa.5.col51)
summary(ppa.6.col51)
summary(ppa.7.col51)
summary(ppa.8.col51)
summary(ppa.9.col51)
summary(ppa.10.col51)
summary(ppa.11.col51)
summary(ppa.12.col51)

# Bind the coefficients together
ppa.col51 <- data.frame(rbind(coef(ppa.1.col51),coef(ppa.2.col51),coef(ppa.3.col51),coef(ppa.4.col51),coef(ppa.5.col51),coef(ppa.6.col51),
                              coef(ppa.7.col51),coef(ppa.8.col51),coef(ppa.9.col51),coef(ppa.10.col51),coef(ppa.11.col51),coef(ppa.12.col51)))
ppa.col51 <- cbind(ppa.col51, rbind(summary(ppa.1.col51)$adj.r.squared, summary(ppa.2.col51)$adj.r.squared, summary(ppa.3.col51)$adj.r.squared,
                                    summary(ppa.4.col51)$adj.r.squared, summary(ppa.5.col51)$adj.r.squared, summary(ppa.6.col51)$adj.r.squared,
                                    summary(ppa.7.col51)$adj.r.squared, summary(ppa.8.col51)$adj.r.squared, summary(ppa.9.col51)$adj.r.squared,
                                    summary(ppa.10.col51)$adj.r.squared, summary(ppa.11.col51)$adj.r.squared, summary(ppa.12.col51)$adj.r.squared))
names(ppa.col51) <- c("b0","ppa.col24",",ppa.col50","r2adj")
rownames(ppa.col51) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.cLdLi.80 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.cLdLi.80), (ppa.col51[1,1] + ppa.col51[1,2]*pp2$ppa.wdMo.80
                                                                      + ppa.col51[1,3]*pp2$ppa.cLdLi.40), pp2$ppa.cLdLi.80)
pp2$ppa.cLdLi.80 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.cLdLi.80), (ppa.col51[2,1] + ppa.col51[2,2]*pp2$ppa.wdMo.80
                                                                      + ppa.col51[2,3]*pp2$ppa.cLdLi.40), pp2$ppa.cLdLi.80)
pp2$ppa.cLdLi.80 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.cLdLi.80), (ppa.col51[3,1] + ppa.col51[3,2]*pp2$ppa.wdMo.80
                                                                      + ppa.col51[3,3]*pp2$ppa.cLdLi.40), pp2$ppa.cLdLi.80)
pp2$ppa.cLdLi.80 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.cLdLi.80), (ppa.col51[4,1] + ppa.col51[4,2]*pp2$ppa.wdMo.80
                                                                      + ppa.col51[4,3]*pp2$ppa.cLdLi.40), pp2$ppa.cLdLi.80)
pp2$ppa.cLdLi.80 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.cLdLi.80), (ppa.col51[5,1] + ppa.col51[5,2]*pp2$ppa.wdMo.80
                                                                      + ppa.col51[5,3]*pp2$ppa.cLdLi.40), pp2$ppa.cLdLi.80)
pp2$ppa.cLdLi.80 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.cLdLi.80), (ppa.col51[6,1] + ppa.col51[6,2]*pp2$ppa.wdMo.80
                                                                      + ppa.col51[6,3]*pp2$ppa.cLdLi.40), pp2$ppa.cLdLi.80)
pp2$ppa.cLdLi.80 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.cLdLi.80), (ppa.col51[7,1] + ppa.col51[7,2]*pp2$ppa.wdMo.80
                                                                      + ppa.col51[7,3]*pp2$ppa.cLdLi.40), pp2$ppa.cLdLi.80)
pp2$ppa.cLdLi.80 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.cLdLi.80), (ppa.col51[8,1] + ppa.col51[8,2]*pp2$ppa.wdMo.80
                                                                      + ppa.col51[8,3]*pp2$ppa.cLdLi.40), pp2$ppa.cLdLi.80)
pp2$ppa.cLdLi.80 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.cLdLi.80), (ppa.col51[9,1] + ppa.col51[9,2]*pp2$ppa.wdMo.80
                                                                      + ppa.col51[9,3]*pp2$ppa.cLdLi.40), pp2$ppa.cLdLi.80)
pp2$ppa.cLdLi.80 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.cLdLi.80), (ppa.col51[10,1] + ppa.col51[10,2]*pp2$ppa.wdMo.80
                                                                       + ppa.col51[10,3]*pp2$ppa.cLdLi.40), pp2$ppa.cLdLi.80)
pp2$ppa.cLdLi.80 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.cLdLi.80), (ppa.col51[11,1] + ppa.col51[11,2]*pp2$ppa.wdMo.80
                                                                       + ppa.col51[11,3]*pp2$ppa.cLdLi.40), pp2$ppa.cLdLi.80)
pp2$ppa.cLdLi.80 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.cLdLi.80), (ppa.col51[12,1] + ppa.col51[12,2]*pp2$ppa.wdMo.80
                                                                       + ppa.col51[12,3]*pp2$ppa.cLdLi.40), pp2$ppa.cLdLi.80)

plot(pp2$ppa.cLdLi.80, type = "l", col = "red")
lines(pp$ppa.cLdLi.80, col = "blue")




################################################
################################################
# pairs(pp[,c(45,5,7,52)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col45 <- lm(pp2[pp2$month==1, 45] ~ pp2[pp2$month==1, 7] + pp2[pp2$month==1, 52]) # Jan
ppa.2.col45 <- lm(pp2[pp2$month==2, 45] ~ pp2[pp2$month==2, 7] + pp2[pp2$month==2, 52]) # Feb
ppa.3.col45 <- lm(pp2[pp2$month==3, 45] ~ pp2[pp2$month==3, 7] + pp2[pp2$month==3, 52]) # Mar
ppa.4.col45 <- lm(pp2[pp2$month==4, 45] ~ pp2[pp2$month==4, 7] + pp2[pp2$month==4, 52]) # Apr
ppa.5.col45 <- lm(pp2[pp2$month==5, 45] ~ pp2[pp2$month==5, 7] + pp2[pp2$month==5, 52]) # May
ppa.6.col45 <- lm(pp2[pp2$month==6, 45] ~ pp2[pp2$month==6, 7] + pp2[pp2$month==6, 52]) # Jun
ppa.7.col45 <- lm(pp2[pp2$month==7, 45] ~ pp2[pp2$month==7, 7] + pp2[pp2$month==7, 52]) # Jul
ppa.8.col45 <- lm(pp2[pp2$month==8, 45] ~ pp2[pp2$month==8, 7] + pp2[pp2$month==8, 52]) # Aug
ppa.9.col45 <- lm(pp2[pp2$month==9, 45] ~ pp2[pp2$month==9, 7] + pp2[pp2$month==9, 52]) # Sep
ppa.10.col45 <- lm(pp2[pp2$month==10, 45] ~ pp2[pp2$month==10, 7] + pp2[pp2$month==10, 52]) # Oct
ppa.11.col45 <- lm(pp2[pp2$month==11, 45] ~ pp2[pp2$month==11, 7] + pp2[pp2$month==11, 52]) # Nov
ppa.12.col45 <- lm(pp2[pp2$month==12, 45] ~ pp2[pp2$month==12, 7] + pp2[pp2$month==12, 52]) # Dec
summary(ppa.1.col45)
summary(ppa.2.col45)
summary(ppa.3.col45)
summary(ppa.4.col45)
summary(ppa.5.col45)
summary(ppa.6.col45)
summary(ppa.7.col45)
summary(ppa.8.col45)
summary(ppa.9.col45)
summary(ppa.10.col45)
summary(ppa.11.col45)
summary(ppa.12.col45)

# Bind the coefficients together
ppa.col45 <- data.frame(rbind(coef(ppa.1.col45),coef(ppa.2.col45),coef(ppa.3.col45),coef(ppa.4.col45),coef(ppa.5.col45),coef(ppa.6.col45),
                              coef(ppa.7.col45),coef(ppa.8.col45),coef(ppa.9.col45),coef(ppa.10.col45),coef(ppa.11.col45),coef(ppa.12.col45)))
ppa.col45 <- cbind(ppa.col45, rbind(summary(ppa.1.col45)$adj.r.squared, summary(ppa.2.col45)$adj.r.squared, summary(ppa.3.col45)$adj.r.squared,
                                    summary(ppa.4.col45)$adj.r.squared, summary(ppa.5.col45)$adj.r.squared, summary(ppa.6.col45)$adj.r.squared,
                                    summary(ppa.7.col45)$adj.r.squared, summary(ppa.8.col45)$adj.r.squared, summary(ppa.9.col45)$adj.r.squared,
                                    summary(ppa.10.col45)$adj.r.squared, summary(ppa.11.col45)$adj.r.squared, summary(ppa.12.col45)$adj.r.squared))
names(ppa.col45) <- c("b0","ppa.col7","ppa.col52","r2adj")
rownames(ppa.col45) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.cLi.0 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.cLi.0), (ppa.col45[1,1] + ppa.col45[1,2]*pp2$ppa.wwBg.5.1
                                                                      + ppa.col45[1,3]*pp2$ppa.Tair), pp2$ppa.cLi.0)
pp2$ppa.cLi.0 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.cLi.0), (ppa.col45[2,1] + ppa.col45[2,2]*pp2$ppa.wwBg.5.1
                                                                      + ppa.col45[2,3]*pp2$ppa.Tair), pp2$ppa.cLi.0)
pp2$ppa.cLi.0 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.cLi.0), (ppa.col45[3,1] + ppa.col45[3,2]*pp2$ppa.wwBg.5.1
                                                                      + ppa.col45[3,3]*pp2$ppa.Tair), pp2$ppa.cLi.0)
pp2$ppa.cLi.0 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.cLi.0), (ppa.col45[4,1] + ppa.col45[4,2]*pp2$ppa.wwBg.5.1
                                                                      + ppa.col45[4,3]*pp2$ppa.Tair), pp2$ppa.cLi.0)
pp2$ppa.cLi.0 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.cLi.0), (ppa.col45[5,1] + ppa.col45[5,2]*pp2$ppa.wwBg.5.1
                                                                      + ppa.col45[5,3]*pp2$ppa.Tair), pp2$ppa.cLi.0)
pp2$ppa.cLi.0 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.cLi.0), (ppa.col45[6,1] + ppa.col45[6,2]*pp2$ppa.wwBg.5.1
                                                                      + ppa.col45[6,3]*pp2$ppa.Tair), pp2$ppa.cLi.0)
pp2$ppa.cLi.0 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.cLi.0), (ppa.col45[7,1] + ppa.col45[7,2]*pp2$ppa.wwBg.5.1
                                                                      + ppa.col45[7,3]*pp2$ppa.Tair), pp2$ppa.cLi.0)
pp2$ppa.cLi.0 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.cLi.0), (ppa.col45[8,1] + ppa.col45[8,2]*pp2$ppa.wwBg.5.1
                                                                      + ppa.col45[8,3]*pp2$ppa.Tair), pp2$ppa.cLi.0)
pp2$ppa.cLi.0 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.cLi.0), (ppa.col45[9,1] + ppa.col45[9,2]*pp2$ppa.wwBg.5.1
                                                                      + ppa.col45[9,3]*pp2$ppa.Tair), pp2$ppa.cLi.0)
pp2$ppa.cLi.0 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.cLi.0), (ppa.col45[10,1] + ppa.col45[10,2]*pp2$ppa.wwBg.5.1
                                                                       + ppa.col45[10,3]*pp2$ppa.Tair), pp2$ppa.cLi.0)
pp2$ppa.cLi.0 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.cLi.0), (ppa.col45[11,1] + ppa.col45[11,2]*pp2$ppa.wwBg.5.1
                                                                       + ppa.col45[11,3]*pp2$ppa.Tair), pp2$ppa.cLi.0)
pp2$ppa.cLi.0 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.cLi.0), (ppa.col45[12,1] + ppa.col45[12,2]*pp2$ppa.wwBg.5.1
                                                                       + ppa.col45[12,3]*pp2$ppa.Tair), pp2$ppa.cLi.0)

plot(pp2$ppa.cLi.0, type = "l", col = "red")
lines(pp$ppa.cLi.0, col = "blue")






################################################
################################################
# pairs(pp[,c(37,7,52)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col37 <- lm(pp2[pp2$month==1, 37] ~ pp2[pp2$month==1, 7] + pp2[pp2$month==1, 52]) # Jan
ppa.2.col37 <- lm(pp2[pp2$month==2, 37] ~ pp2[pp2$month==2, 7] + pp2[pp2$month==2, 52]) # Feb
ppa.3.col37 <- lm(pp2[pp2$month==3, 37] ~ pp2[pp2$month==3, 7] + pp2[pp2$month==3, 52]) # Mar
ppa.4.col37 <- lm(pp2[pp2$month==4, 37] ~ pp2[pp2$month==4, 7] + pp2[pp2$month==4, 52]) # Apr
ppa.5.col37 <- lm(pp2[pp2$month==5, 37] ~ pp2[pp2$month==5, 7] + pp2[pp2$month==5, 52]) # May
ppa.6.col37 <- lm(pp2[pp2$month==6, 37] ~ pp2[pp2$month==6, 7] + pp2[pp2$month==6, 52]) # Jun
ppa.7.col37 <- lm(pp2[pp2$month==7, 37] ~ pp2[pp2$month==7, 7] + pp2[pp2$month==7, 52]) # Jul
ppa.8.col37 <- lm(pp2[pp2$month==8, 37] ~ pp2[pp2$month==8, 7] + pp2[pp2$month==8, 52]) # Aug
ppa.9.col37 <- lm(pp2[pp2$month==9, 37] ~ pp2[pp2$month==9, 7] + pp2[pp2$month==9, 52]) # Sep
ppa.10.col37 <- lm(pp2[pp2$month==10, 37] ~ pp2[pp2$month==10, 7] + pp2[pp2$month==10, 52]) # Oct
ppa.11.col37 <- lm(pp2[pp2$month==11, 37] ~ pp2[pp2$month==11, 7] + pp2[pp2$month==11, 52]) # Nov
ppa.12.col37 <- lm(pp2[pp2$month==12, 37] ~ pp2[pp2$month==12, 7] + pp2[pp2$month==12, 52]) # Dec
summary(ppa.1.col37)
summary(ppa.2.col37)
summary(ppa.3.col37)
summary(ppa.4.col37)
summary(ppa.5.col37)
summary(ppa.6.col37)
summary(ppa.7.col37)
summary(ppa.8.col37)
summary(ppa.9.col37)
summary(ppa.10.col37)
summary(ppa.11.col37)
summary(ppa.12.col37)

# Bind the coefficients together
ppa.col37 <- data.frame(rbind(coef(ppa.1.col37),coef(ppa.2.col37),coef(ppa.3.col37),coef(ppa.4.col37),coef(ppa.5.col37),coef(ppa.6.col37),
                              coef(ppa.7.col37),coef(ppa.8.col37),coef(ppa.9.col37),coef(ppa.10.col37),coef(ppa.11.col37),coef(ppa.12.col37)))
ppa.col37 <- cbind(ppa.col37, rbind(summary(ppa.1.col37)$adj.r.squared, summary(ppa.2.col37)$adj.r.squared, summary(ppa.3.col37)$adj.r.squared,
                                    summary(ppa.4.col37)$adj.r.squared, summary(ppa.5.col37)$adj.r.squared, summary(ppa.6.col37)$adj.r.squared,
                                    summary(ppa.7.col37)$adj.r.squared, summary(ppa.8.col37)$adj.r.squared, summary(ppa.9.col37)$adj.r.squared,
                                    summary(ppa.10.col37)$adj.r.squared, summary(ppa.11.col37)$adj.r.squared, summary(ppa.12.col37)$adj.r.squared))
names(ppa.col37) <- c("b0","ppa.col7","ppa.col52","r2adj")
rownames(ppa.col37) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.cLd.0 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.cLd.0), (ppa.col37[1,1] + ppa.col37[1,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col37[1,3]*pp2$ppa.Tair), pp2$ppa.cLd.0)
pp2$ppa.cLd.0 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.cLd.0), (ppa.col37[2,1] + ppa.col37[2,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col37[2,3]*pp2$ppa.Tair), pp2$ppa.cLd.0)
pp2$ppa.cLd.0 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.cLd.0), (ppa.col37[3,1] + ppa.col37[3,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col37[3,3]*pp2$ppa.Tair), pp2$ppa.cLd.0)
pp2$ppa.cLd.0 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.cLd.0), (ppa.col37[4,1] + ppa.col37[4,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col37[4,3]*pp2$ppa.Tair), pp2$ppa.cLd.0)
pp2$ppa.cLd.0 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.cLd.0), (ppa.col37[5,1] + ppa.col37[5,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col37[5,3]*pp2$ppa.Tair), pp2$ppa.cLd.0)
pp2$ppa.cLd.0 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.cLd.0), (ppa.col37[6,1] + ppa.col37[6,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col37[6,3]*pp2$ppa.Tair), pp2$ppa.cLd.0)
pp2$ppa.cLd.0 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.cLd.0), (ppa.col37[7,1] + ppa.col37[7,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col37[7,3]*pp2$ppa.Tair), pp2$ppa.cLd.0)
pp2$ppa.cLd.0 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.cLd.0), (ppa.col37[8,1] + ppa.col37[8,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col37[8,3]*pp2$ppa.Tair), pp2$ppa.cLd.0)
pp2$ppa.cLd.0 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.cLd.0), (ppa.col37[9,1] + ppa.col37[9,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col37[9,3]*pp2$ppa.Tair), pp2$ppa.cLd.0)
pp2$ppa.cLd.0 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.cLd.0), (ppa.col37[10,1] + ppa.col37[10,2]*pp2$ppa.wwBg.5.1
                                                                 + ppa.col37[10,3]*pp2$ppa.Tair), pp2$ppa.cLd.0)
pp2$ppa.cLd.0 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.cLd.0), (ppa.col37[11,1] + ppa.col37[11,2]*pp2$ppa.wwBg.5.1
                                                                 + ppa.col37[11,3]*pp2$ppa.Tair), pp2$ppa.cLd.0)
pp2$ppa.cLd.0 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.cLd.0), (ppa.col37[12,1] + ppa.col37[12,2]*pp2$ppa.wwBg.5.1
                                                                 + ppa.col37[12,3]*pp2$ppa.Tair), pp2$ppa.cLd.0)

plot(pp2$ppa.cLd.0, type = "l", col = "red")
lines(pp$ppa.cLd.0, col = "blue")







################################################
################################################
# pairs(pp[,c(44,7,37,45,52)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col44 <- lm(pp2[pp2$month==1, 44] ~ pp2[pp2$month==1, 7] + pp2[pp2$month==1, 52]) # Jan
ppa.2.col44 <- lm(pp2[pp2$month==2, 44] ~ pp2[pp2$month==2, 7] + pp2[pp2$month==2, 52]) # Feb
ppa.3.col44 <- lm(pp2[pp2$month==3, 44] ~ pp2[pp2$month==3, 7] + pp2[pp2$month==3, 52]) # Mar
ppa.4.col44 <- lm(pp2[pp2$month==4, 44] ~ pp2[pp2$month==4, 7] + pp2[pp2$month==4, 52]) # Apr
ppa.5.col44 <- lm(pp2[pp2$month==5, 44] ~ pp2[pp2$month==5, 7] + pp2[pp2$month==5, 52]) # May
ppa.6.col44 <- lm(pp2[pp2$month==6, 44] ~ pp2[pp2$month==6, 7] + pp2[pp2$month==6, 52]) # Jun
ppa.7.col44 <- lm(pp2[pp2$month==7, 44] ~ pp2[pp2$month==7, 7] + pp2[pp2$month==7, 52]) # Jul
ppa.8.col44 <- lm(pp2[pp2$month==8, 44] ~ pp2[pp2$month==8, 7] + pp2[pp2$month==8, 52]) # Aug
ppa.9.col44 <- lm(pp2[pp2$month==9, 44] ~ pp2[pp2$month==9, 7] + pp2[pp2$month==9, 52]) # Sep
ppa.10.col44 <- lm(pp2[pp2$month==10, 44] ~ pp2[pp2$month==10, 7] + pp2[pp2$month==10, 52]) # Oct
ppa.11.col44 <- lm(pp2[pp2$month==11, 44] ~ pp2[pp2$month==11, 7] + pp2[pp2$month==11, 52]) # Nov
ppa.12.col44 <- lm(pp2[pp2$month==12, 44] ~ pp2[pp2$month==12, 7] + pp2[pp2$month==12, 52]) # Dec
summary(ppa.1.col44)
summary(ppa.2.col44)
summary(ppa.3.col44)
summary(ppa.4.col44)
summary(ppa.5.col44)
summary(ppa.6.col44)
summary(ppa.7.col44)
summary(ppa.8.col44)
summary(ppa.9.col44)
summary(ppa.10.col44)
summary(ppa.11.col44)
summary(ppa.12.col44)

# Bind the coefficients together
ppa.col44 <- data.frame(rbind(coef(ppa.1.col44),coef(ppa.2.col44),coef(ppa.3.col44),coef(ppa.4.col44),coef(ppa.5.col44),coef(ppa.6.col44),
                              coef(ppa.7.col44),coef(ppa.8.col44),coef(ppa.9.col44),coef(ppa.10.col44),coef(ppa.11.col44),coef(ppa.12.col44)))
ppa.col44 <- cbind(ppa.col44, rbind(summary(ppa.1.col44)$adj.r.squared, summary(ppa.2.col44)$adj.r.squared, summary(ppa.3.col44)$adj.r.squared,
                                    summary(ppa.4.col44)$adj.r.squared, summary(ppa.5.col44)$adj.r.squared, summary(ppa.6.col44)$adj.r.squared,
                                    summary(ppa.7.col44)$adj.r.squared, summary(ppa.8.col44)$adj.r.squared, summary(ppa.9.col44)$adj.r.squared,
                                    summary(ppa.10.col44)$adj.r.squared, summary(ppa.11.col44)$adj.r.squared, summary(ppa.12.col44)$adj.r.squared))
names(ppa.col44) <- c("b0","ppa.col7","ppa.col52","r2adj")
rownames(ppa.col44) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.cLd.5 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.cLd.5), (ppa.col44[1,1] + ppa.col44[1,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col44[1,3]*pp2$ppa.Tair), pp2$ppa.cLd.5)
pp2$ppa.cLd.5 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.cLd.5), (ppa.col44[2,1] + ppa.col44[2,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col44[2,3]*pp2$ppa.Tair), pp2$ppa.cLd.5)
pp2$ppa.cLd.5 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.cLd.5), (ppa.col44[3,1] + ppa.col44[3,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col44[3,3]*pp2$ppa.Tair), pp2$ppa.cLd.5)
pp2$ppa.cLd.5 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.cLd.5), (ppa.col44[4,1] + ppa.col44[4,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col44[4,3]*pp2$ppa.Tair), pp2$ppa.cLd.5)
pp2$ppa.cLd.5 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.cLd.5), (ppa.col44[5,1] + ppa.col44[5,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col44[5,3]*pp2$ppa.Tair), pp2$ppa.cLd.5)
pp2$ppa.cLd.5 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.cLd.5), (ppa.col44[6,1] + ppa.col44[6,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col44[6,3]*pp2$ppa.Tair), pp2$ppa.cLd.5)
pp2$ppa.cLd.5 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.cLd.5), (ppa.col44[7,1] + ppa.col44[7,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col44[7,3]*pp2$ppa.Tair), pp2$ppa.cLd.5)
pp2$ppa.cLd.5 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.cLd.5), (ppa.col44[8,1] + ppa.col44[8,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col44[8,3]*pp2$ppa.Tair), pp2$ppa.cLd.5)
pp2$ppa.cLd.5 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.cLd.5), (ppa.col44[9,1] + ppa.col44[9,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col44[9,3]*pp2$ppa.Tair), pp2$ppa.cLd.5)
pp2$ppa.cLd.5 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.cLd.5), (ppa.col44[10,1] + ppa.col44[10,2]*pp2$ppa.wwBg.5.1
                                                                 + ppa.col44[10,3]*pp2$ppa.Tair), pp2$ppa.cLd.5)
pp2$ppa.cLd.5 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.cLd.5), (ppa.col44[11,1] + ppa.col44[11,2]*pp2$ppa.wwBg.5.1
                                                                 + ppa.col44[11,3]*pp2$ppa.Tair), pp2$ppa.cLd.5)
pp2$ppa.cLd.5 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.cLd.5), (ppa.col44[12,1] + ppa.col44[12,2]*pp2$ppa.wwBg.5.1
                                                                 + ppa.col44[12,3]*pp2$ppa.Tair), pp2$ppa.cLd.5)

plot(pp2$ppa.cLd.5, type = "l", col = "red")
lines(pp$ppa.cLd.5, col = "blue")




################################################
################################################
# pairs(pp[,c(43,7,37,44,52)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col43 <- lm(pp2[pp2$month==1, 43] ~ pp2[pp2$month==1, 7] + pp2[pp2$month==1, 52]) # Jan
ppa.2.col43 <- lm(pp2[pp2$month==2, 43] ~ pp2[pp2$month==2, 7] + pp2[pp2$month==2, 52]) # Feb
ppa.3.col43 <- lm(pp2[pp2$month==3, 43] ~ pp2[pp2$month==3, 7] + pp2[pp2$month==3, 52]) # Mar
ppa.4.col43 <- lm(pp2[pp2$month==4, 43] ~ pp2[pp2$month==4, 7] + pp2[pp2$month==4, 52]) # Apr
ppa.5.col43 <- lm(pp2[pp2$month==5, 43] ~ pp2[pp2$month==5, 7] + pp2[pp2$month==5, 52]) # May
ppa.6.col43 <- lm(pp2[pp2$month==6, 43] ~ pp2[pp2$month==6, 7] + pp2[pp2$month==6, 52]) # Jun
ppa.7.col43 <- lm(pp2[pp2$month==7, 43] ~ pp2[pp2$month==7, 7] + pp2[pp2$month==7, 52]) # Jul
ppa.8.col43 <- lm(pp2[pp2$month==8, 43] ~ pp2[pp2$month==8, 7] + pp2[pp2$month==8, 52]) # Aug
ppa.9.col43 <- lm(pp2[pp2$month==9, 43] ~ pp2[pp2$month==9, 7] + pp2[pp2$month==9, 52]) # Sep
ppa.10.col43 <- lm(pp2[pp2$month==10, 43] ~ pp2[pp2$month==10, 7] + pp2[pp2$month==10, 52]) # Oct
ppa.11.col43 <- lm(pp2[pp2$month==11, 43] ~ pp2[pp2$month==11, 7] + pp2[pp2$month==11, 52]) # Nov
ppa.12.col43 <- lm(pp2[pp2$month==12, 43] ~ pp2[pp2$month==12, 7] + pp2[pp2$month==12, 52]) # Dec
summary(ppa.1.col43)
summary(ppa.2.col43)
summary(ppa.3.col43)
summary(ppa.4.col43)
summary(ppa.5.col43)
summary(ppa.6.col43)
summary(ppa.7.col43)
summary(ppa.8.col43)
summary(ppa.9.col43)
summary(ppa.10.col43)
summary(ppa.11.col43)
summary(ppa.12.col43)

# Bind the coefficients together
ppa.col43 <- data.frame(rbind(coef(ppa.1.col43),coef(ppa.2.col43),coef(ppa.3.col43),coef(ppa.4.col43),coef(ppa.5.col43),coef(ppa.6.col43),
                              coef(ppa.7.col43),coef(ppa.8.col43),coef(ppa.9.col43),coef(ppa.10.col43),coef(ppa.11.col43),coef(ppa.12.col43)))
ppa.col43 <- cbind(ppa.col43, rbind(summary(ppa.1.col43)$adj.r.squared, summary(ppa.2.col43)$adj.r.squared, summary(ppa.3.col43)$adj.r.squared,
                                    summary(ppa.4.col43)$adj.r.squared, summary(ppa.5.col43)$adj.r.squared, summary(ppa.6.col43)$adj.r.squared,
                                    summary(ppa.7.col43)$adj.r.squared, summary(ppa.8.col43)$adj.r.squared, summary(ppa.9.col43)$adj.r.squared,
                                    summary(ppa.10.col43)$adj.r.squared, summary(ppa.11.col43)$adj.r.squared, summary(ppa.12.col43)$adj.r.squared))
names(ppa.col43) <- c("b0","ppa.col7","ppa.col52","r2adj")
rownames(ppa.col43) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.cLd.0.1 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.cLd.0.1), (ppa.col43[1,1] + ppa.col43[1,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col43[1,3]*pp2$ppa.Tair), pp2$ppa.cLd.0.1)
pp2$ppa.cLd.0.1 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.cLd.0.1), (ppa.col43[2,1] + ppa.col43[2,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col43[2,3]*pp2$ppa.Tair), pp2$ppa.cLd.0.1)
pp2$ppa.cLd.0.1 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.cLd.0.1), (ppa.col43[3,1] + ppa.col43[3,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col43[3,3]*pp2$ppa.Tair), pp2$ppa.cLd.0.1)
pp2$ppa.cLd.0.1 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.cLd.0.1), (ppa.col43[4,1] + ppa.col43[4,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col43[4,3]*pp2$ppa.Tair), pp2$ppa.cLd.0.1)
pp2$ppa.cLd.0.1 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.cLd.0.1), (ppa.col43[5,1] + ppa.col43[5,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col43[5,3]*pp2$ppa.Tair), pp2$ppa.cLd.0.1)
pp2$ppa.cLd.0.1 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.cLd.0.1), (ppa.col43[6,1] + ppa.col43[6,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col43[6,3]*pp2$ppa.Tair), pp2$ppa.cLd.0.1)
pp2$ppa.cLd.0.1 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.cLd.0.1), (ppa.col43[7,1] + ppa.col43[7,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col43[7,3]*pp2$ppa.Tair), pp2$ppa.cLd.0.1)
pp2$ppa.cLd.0.1 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.cLd.0.1), (ppa.col43[8,1] + ppa.col43[8,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col43[8,3]*pp2$ppa.Tair), pp2$ppa.cLd.0.1)
pp2$ppa.cLd.0.1 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.cLd.0.1), (ppa.col43[9,1] + ppa.col43[9,2]*pp2$ppa.wwBg.5.1
                                                                + ppa.col43[9,3]*pp2$ppa.Tair), pp2$ppa.cLd.0.1)
pp2$ppa.cLd.0.1 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.cLd.0.1), (ppa.col43[10,1] + ppa.col43[10,2]*pp2$ppa.wwBg.5.1
                                                                 + ppa.col43[10,3]*pp2$ppa.Tair), pp2$ppa.cLd.0.1)
pp2$ppa.cLd.0.1 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.cLd.0.1), (ppa.col43[11,1] + ppa.col43[11,2]*pp2$ppa.wwBg.5.1
                                                                 + ppa.col43[11,3]*pp2$ppa.Tair), pp2$ppa.cLd.0.1)
pp2$ppa.cLd.0.1 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.cLd.0.1), (ppa.col43[12,1] + ppa.col43[12,2]*pp2$ppa.wwBg.5.1
                                                                 + ppa.col43[12,3]*pp2$ppa.Tair), pp2$ppa.cLd.0.1)

plot(pp2$ppa.cLd.0.1, type = "l", col = "red")
lines(pp$ppa.cLd.0.1, col = "blue")




################################################
################################################
# pairs(pp[,c(31,7,10,44,52)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col31 <- lm(pp2[pp2$month==1, 31] ~ pp2[pp2$month==1, 10] + pp2[pp2$month==1, 44]) # Jan
ppa.2.col31 <- lm(pp2[pp2$month==2, 31] ~ pp2[pp2$month==2, 10] + pp2[pp2$month==2, 44]) # Feb
ppa.3.col31 <- lm(pp2[pp2$month==3, 31] ~ pp2[pp2$month==3, 10] + pp2[pp2$month==3, 44]) # Mar
ppa.4.col31 <- lm(pp2[pp2$month==4, 31] ~ pp2[pp2$month==4, 10] + pp2[pp2$month==4, 44]) # Apr
ppa.5.col31 <- lm(pp2[pp2$month==5, 31] ~ pp2[pp2$month==5, 10] + pp2[pp2$month==5, 44]) # May
ppa.6.col31 <- lm(pp2[pp2$month==6, 31] ~ pp2[pp2$month==6, 10] + pp2[pp2$month==6, 44]) # Jun
ppa.7.col31 <- lm(pp2[pp2$month==7, 31] ~ pp2[pp2$month==7, 10] + pp2[pp2$month==7, 44]) # Jul
ppa.8.col31 <- lm(pp2[pp2$month==8, 31] ~ pp2[pp2$month==8, 10] + pp2[pp2$month==8, 44]) # Aug
ppa.9.col31 <- lm(pp2[pp2$month==9, 31] ~ pp2[pp2$month==9, 10] + pp2[pp2$month==9, 44]) # Sep
ppa.10.col31 <- lm(pp2[pp2$month==10, 31] ~ pp2[pp2$month==10, 10] + pp2[pp2$month==10, 44]) # Oct
ppa.11.col31 <- lm(pp2[pp2$month==11, 31] ~ pp2[pp2$month==11, 10] + pp2[pp2$month==11, 44]) # Nov
ppa.12.col31 <- lm(pp2[pp2$month==12, 31] ~ pp2[pp2$month==12, 10] + pp2[pp2$month==12, 44]) # Dec
summary(ppa.1.col31)
summary(ppa.2.col31)
summary(ppa.3.col31)
summary(ppa.4.col31)
summary(ppa.5.col31)
summary(ppa.6.col31)
summary(ppa.7.col31)
summary(ppa.8.col31)
summary(ppa.9.col31)
summary(ppa.10.col31)
summary(ppa.11.col31)
summary(ppa.12.col31)

# Bind the coefficients together
ppa.col31 <- data.frame(rbind(coef(ppa.1.col31),coef(ppa.2.col31),coef(ppa.3.col31),coef(ppa.4.col31),coef(ppa.5.col31),coef(ppa.6.col31),
                              coef(ppa.7.col31),coef(ppa.8.col31),coef(ppa.9.col31),coef(ppa.10.col31),coef(ppa.11.col31),coef(ppa.12.col31)))
ppa.col31 <- cbind(ppa.col31, rbind(summary(ppa.1.col31)$adj.r.squared, summary(ppa.2.col31)$adj.r.squared, summary(ppa.3.col31)$adj.r.squared,
                                    summary(ppa.4.col31)$adj.r.squared, summary(ppa.5.col31)$adj.r.squared, summary(ppa.6.col31)$adj.r.squared,
                                    summary(ppa.7.col31)$adj.r.squared, summary(ppa.8.col31)$adj.r.squared, summary(ppa.9.col31)$adj.r.squared,
                                    summary(ppa.10.col31)$adj.r.squared, summary(ppa.11.col31)$adj.r.squared, summary(ppa.12.col31)$adj.r.squared))
names(ppa.col31) <- c("b0","ppa.col10","ppa.col44","r2adj")
rownames(ppa.col31) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.cLd.20 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.cLd.20), (ppa.col31[1,1] + ppa.col31[1,2]*pp2$ppa.cLdLi.20
                                                                    + ppa.col31[1,3]*pp2$ppa.cLd.5), pp2$ppa.cLd.20)
pp2$ppa.cLd.20 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.cLd.20), (ppa.col31[2,1] + ppa.col31[2,2]*pp2$ppa.cLdLi.20
                                                                    + ppa.col31[2,3]*pp2$ppa.cLd.5), pp2$ppa.cLd.20)
pp2$ppa.cLd.20 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.cLd.20), (ppa.col31[3,1] + ppa.col31[3,2]*pp2$ppa.cLdLi.20
                                                                    + ppa.col31[3,3]*pp2$ppa.cLd.5), pp2$ppa.cLd.20)
pp2$ppa.cLd.20 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.cLd.20), (ppa.col31[4,1] + ppa.col31[4,2]*pp2$ppa.cLdLi.20
                                                                    + ppa.col31[4,3]*pp2$ppa.cLd.5), pp2$ppa.cLd.20)
pp2$ppa.cLd.20 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.cLd.20), (ppa.col31[5,1] + ppa.col31[5,2]*pp2$ppa.cLdLi.20
                                                                    + ppa.col31[5,3]*pp2$ppa.cLd.5), pp2$ppa.cLd.20)
pp2$ppa.cLd.20 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.cLd.20), (ppa.col31[6,1] + ppa.col31[6,2]*pp2$ppa.cLdLi.20
                                                                    + ppa.col31[6,3]*pp2$ppa.cLd.5), pp2$ppa.cLd.20)
pp2$ppa.cLd.20 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.cLd.20), (ppa.col31[7,1] + ppa.col31[7,2]*pp2$ppa.cLdLi.20
                                                                    + ppa.col31[7,3]*pp2$ppa.cLd.5), pp2$ppa.cLd.20)
pp2$ppa.cLd.20 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.cLd.20), (ppa.col31[8,1] + ppa.col31[8,2]*pp2$ppa.cLdLi.20
                                                                    + ppa.col31[8,3]*pp2$ppa.cLd.5), pp2$ppa.cLd.20)
pp2$ppa.cLd.20 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.cLd.20), (ppa.col31[9,1] + ppa.col31[9,2]*pp2$ppa.cLdLi.20
                                                                    + ppa.col31[9,3]*pp2$ppa.cLd.5), pp2$ppa.cLd.20)
pp2$ppa.cLd.20 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.cLd.20), (ppa.col31[10,1] + ppa.col31[10,2]*pp2$ppa.cLdLi.20
                                                                     + ppa.col31[10,3]*pp2$ppa.cLd.5), pp2$ppa.cLd.20)
pp2$ppa.cLd.20 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.cLd.20), (ppa.col31[11,1] + ppa.col31[11,2]*pp2$ppa.cLdLi.20
                                                                     + ppa.col31[11,3]*pp2$ppa.cLd.5), pp2$ppa.cLd.20)
pp2$ppa.cLd.20 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.cLd.20), (ppa.col31[12,1] + ppa.col31[12,2]*pp2$ppa.cLdLi.20
                                                                     + ppa.col31[12,3]*pp2$ppa.cLd.5), pp2$ppa.cLd.20)

plot(pp2$ppa.cLd.20, type = "l", col = "red")
lines(pp$ppa.cLd.20, col = "blue")





################################################
################################################
# pairs(pp[,c(32,10,31,44,52)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col32 <- lm(pp2[pp2$month==1, 32] ~ pp2[pp2$month==1, 10] + pp2[pp2$month==1, 31]) # Jan
ppa.2.col32 <- lm(pp2[pp2$month==2, 32] ~ pp2[pp2$month==2, 10] + pp2[pp2$month==2, 31]) # Feb
ppa.3.col32 <- lm(pp2[pp2$month==3, 32] ~ pp2[pp2$month==3, 10] + pp2[pp2$month==3, 31]) # Mar
ppa.4.col32 <- lm(pp2[pp2$month==4, 32] ~ pp2[pp2$month==4, 10] + pp2[pp2$month==4, 31]) # Apr
ppa.5.col32 <- lm(pp2[pp2$month==5, 32] ~ pp2[pp2$month==5, 10] + pp2[pp2$month==5, 31]) # May
ppa.6.col32 <- lm(pp2[pp2$month==6, 32] ~ pp2[pp2$month==6, 10] + pp2[pp2$month==6, 31]) # Jun
ppa.7.col32 <- lm(pp2[pp2$month==7, 32] ~ pp2[pp2$month==7, 10] + pp2[pp2$month==7, 31]) # Jul
ppa.8.col32 <- lm(pp2[pp2$month==8, 32] ~ pp2[pp2$month==8, 10] + pp2[pp2$month==8, 31]) # Aug
ppa.9.col32 <- lm(pp2[pp2$month==9, 32] ~ pp2[pp2$month==9, 10] + pp2[pp2$month==9, 31]) # Sep
ppa.10.col32 <- lm(pp2[pp2$month==10, 32] ~ pp2[pp2$month==10, 10] + pp2[pp2$month==10, 31]) # Oct
ppa.11.col32 <- lm(pp2[pp2$month==11, 32] ~ pp2[pp2$month==11, 10] + pp2[pp2$month==11, 31]) # Nov
ppa.12.col32 <- lm(pp2[pp2$month==12, 32] ~ pp2[pp2$month==12, 10] + pp2[pp2$month==12, 31]) # Dec
summary(ppa.1.col32)
summary(ppa.2.col32)
summary(ppa.3.col32)
summary(ppa.4.col32)
summary(ppa.5.col32)
summary(ppa.6.col32)
summary(ppa.7.col32)
summary(ppa.8.col32)
summary(ppa.9.col32)
summary(ppa.10.col32)
summary(ppa.11.col32)
summary(ppa.12.col32)

# Bind the coefficients together
ppa.col32 <- data.frame(rbind(coef(ppa.1.col32),coef(ppa.2.col32),coef(ppa.3.col32),coef(ppa.4.col32),coef(ppa.5.col32),coef(ppa.6.col32),
                              coef(ppa.7.col32),coef(ppa.8.col32),coef(ppa.9.col32),coef(ppa.10.col32),coef(ppa.11.col32),coef(ppa.12.col32)))
ppa.col32 <- cbind(ppa.col32, rbind(summary(ppa.1.col32)$adj.r.squared, summary(ppa.2.col32)$adj.r.squared, summary(ppa.3.col32)$adj.r.squared,
                                    summary(ppa.4.col32)$adj.r.squared, summary(ppa.5.col32)$adj.r.squared, summary(ppa.6.col32)$adj.r.squared,
                                    summary(ppa.7.col32)$adj.r.squared, summary(ppa.8.col32)$adj.r.squared, summary(ppa.9.col32)$adj.r.squared,
                                    summary(ppa.10.col32)$adj.r.squared, summary(ppa.11.col32)$adj.r.squared, summary(ppa.12.col32)$adj.r.squared))
names(ppa.col32) <- c("b0","ppa.col10","ppa.col31","r2adj")
rownames(ppa.col32) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.cLd.25 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.cLd.25), (ppa.col32[1,1] + ppa.col32[1,2]*pp2$ppa.cLdLi.20
                                                                  + ppa.col32[1,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.25)
pp2$ppa.cLd.25 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.cLd.25), (ppa.col32[2,1] + ppa.col32[2,2]*pp2$ppa.cLdLi.20
                                                                  + ppa.col32[2,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.25)
pp2$ppa.cLd.25 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.cLd.25), (ppa.col32[3,1] + ppa.col32[3,2]*pp2$ppa.cLdLi.20
                                                                  + ppa.col32[3,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.25)
pp2$ppa.cLd.25 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.cLd.25), (ppa.col32[4,1] + ppa.col32[4,2]*pp2$ppa.cLdLi.20
                                                                  + ppa.col32[4,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.25)
pp2$ppa.cLd.25 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.cLd.25), (ppa.col32[5,1] + ppa.col32[5,2]*pp2$ppa.cLdLi.20
                                                                  + ppa.col32[5,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.25)
pp2$ppa.cLd.25 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.cLd.25), (ppa.col32[6,1] + ppa.col32[6,2]*pp2$ppa.cLdLi.20
                                                                  + ppa.col32[6,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.25)
pp2$ppa.cLd.25 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.cLd.25), (ppa.col32[7,1] + ppa.col32[7,2]*pp2$ppa.cLdLi.20
                                                                  + ppa.col32[7,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.25)
pp2$ppa.cLd.25 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.cLd.25), (ppa.col32[8,1] + ppa.col32[8,2]*pp2$ppa.cLdLi.20
                                                                  + ppa.col32[8,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.25)
pp2$ppa.cLd.25 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.cLd.25), (ppa.col32[9,1] + ppa.col32[9,2]*pp2$ppa.cLdLi.20
                                                                  + ppa.col32[9,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.25)
pp2$ppa.cLd.25 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.cLd.25), (ppa.col32[10,1] + ppa.col32[10,2]*pp2$ppa.cLdLi.20
                                                                   + ppa.col32[10,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.25)
pp2$ppa.cLd.25 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.cLd.25), (ppa.col32[11,1] + ppa.col32[11,2]*pp2$ppa.cLdLi.20
                                                                   + ppa.col32[11,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.25)
pp2$ppa.cLd.25 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.cLd.25), (ppa.col32[12,1] + ppa.col32[12,2]*pp2$ppa.cLdLi.20
                                                                   + ppa.col32[12,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.25)

plot(pp2$ppa.cLd.25, type = "l", col = "red")
lines(pp$ppa.cLd.25, col = "blue")





################################################
################################################
# pairs(pp[,c(38,9,10,31)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col38 <- lm(pp2[pp2$month==1, 38] ~ pp2[pp2$month==1, 10] + pp2[pp2$month==1, 31]) # Jan
ppa.2.col38 <- lm(pp2[pp2$month==2, 38] ~ pp2[pp2$month==2, 10] + pp2[pp2$month==2, 31]) # Feb
ppa.3.col38 <- lm(pp2[pp2$month==3, 38] ~ pp2[pp2$month==3, 10] + pp2[pp2$month==3, 31]) # Mar
ppa.4.col38 <- lm(pp2[pp2$month==4, 38] ~ pp2[pp2$month==4, 10] + pp2[pp2$month==4, 31]) # Apr
ppa.5.col38 <- lm(pp2[pp2$month==5, 38] ~ pp2[pp2$month==5, 10] + pp2[pp2$month==5, 31]) # May
ppa.6.col38 <- lm(pp2[pp2$month==6, 38] ~ pp2[pp2$month==6, 10] + pp2[pp2$month==6, 31]) # Jun
ppa.7.col38 <- lm(pp2[pp2$month==7, 38] ~ pp2[pp2$month==7, 10] + pp2[pp2$month==7, 31]) # Jul
ppa.8.col38 <- lm(pp2[pp2$month==8, 38] ~ pp2[pp2$month==8, 10] + pp2[pp2$month==8, 31]) # Aug
ppa.9.col38 <- lm(pp2[pp2$month==9, 38] ~ pp2[pp2$month==9, 10] + pp2[pp2$month==9, 31]) # Sep
ppa.10.col38 <- lm(pp2[pp2$month==10, 38] ~ pp2[pp2$month==10, 10] + pp2[pp2$month==10, 31]) # Oct
ppa.11.col38 <- lm(pp2[pp2$month==11, 38] ~ pp2[pp2$month==11, 10] + pp2[pp2$month==11, 31]) # Nov
ppa.12.col38 <- lm(pp2[pp2$month==12, 38] ~ pp2[pp2$month==12, 10] + pp2[pp2$month==12, 31]) # Dec
summary(ppa.1.col38)
summary(ppa.2.col38)
summary(ppa.3.col38)
summary(ppa.4.col38)
summary(ppa.5.col38)
summary(ppa.6.col38)
summary(ppa.7.col38)
summary(ppa.8.col38)
summary(ppa.9.col38)
summary(ppa.10.col38)
summary(ppa.11.col38)
summary(ppa.12.col38)

# Bind the coefficients together
ppa.col38 <- data.frame(rbind(coef(ppa.1.col38),coef(ppa.2.col38),coef(ppa.3.col38),coef(ppa.4.col38),coef(ppa.5.col38),coef(ppa.6.col38),
                              coef(ppa.7.col38),coef(ppa.8.col38),coef(ppa.9.col38),coef(ppa.10.col38),coef(ppa.11.col38),coef(ppa.12.col38)))
ppa.col38 <- cbind(ppa.col38, rbind(summary(ppa.1.col38)$adj.r.squared, summary(ppa.2.col38)$adj.r.squared, summary(ppa.3.col38)$adj.r.squared,
                                    summary(ppa.4.col38)$adj.r.squared, summary(ppa.5.col38)$adj.r.squared, summary(ppa.6.col38)$adj.r.squared,
                                    summary(ppa.7.col38)$adj.r.squared, summary(ppa.8.col38)$adj.r.squared, summary(ppa.9.col38)$adj.r.squared,
                                    summary(ppa.10.col38)$adj.r.squared, summary(ppa.11.col38)$adj.r.squared, summary(ppa.12.col38)$adj.r.squared))
names(ppa.col38) <- c("b0","ppa.col10","ppa.col31","r2adj")
rownames(ppa.col38) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.cLd.10 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.cLd.10), (ppa.col38[1,1] + ppa.col38[1,2]*pp2$ppa.cLdLi.20
                                                                  + ppa.col38[1,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.cLd.10), (ppa.col38[2,1] + ppa.col38[2,2]*pp2$ppa.cLdLi.20
                                                                  + ppa.col38[2,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.cLd.10), (ppa.col38[3,1] + ppa.col38[3,2]*pp2$ppa.cLdLi.20
                                                                  + ppa.col38[3,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.cLd.10), (ppa.col38[4,1] + ppa.col38[4,2]*pp2$ppa.cLdLi.20
                                                                  + ppa.col38[4,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.cLd.10), (ppa.col38[5,1] + ppa.col38[5,2]*pp2$ppa.cLdLi.20
                                                                  + ppa.col38[5,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.cLd.10), (ppa.col38[6,1] + ppa.col38[6,2]*pp2$ppa.cLdLi.20
                                                                  + ppa.col38[6,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.cLd.10), (ppa.col38[7,1] + ppa.col38[7,2]*pp2$ppa.cLdLi.20
                                                                  + ppa.col38[7,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.cLd.10), (ppa.col38[8,1] + ppa.col38[8,2]*pp2$ppa.cLdLi.20
                                                                  + ppa.col38[8,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.cLd.10), (ppa.col38[9,1] + ppa.col38[9,2]*pp2$ppa.cLdLi.20
                                                                  + ppa.col38[9,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.cLd.10), (ppa.col38[10,1] + ppa.col38[10,2]*pp2$ppa.cLdLi.20
                                                                   + ppa.col38[10,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.cLd.10), (ppa.col38[11,1] + ppa.col38[11,2]*pp2$ppa.cLdLi.20
                                                                   + ppa.col38[11,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.cLd.10), (ppa.col38[12,1] + ppa.col38[12,2]*pp2$ppa.cLdLi.20
                                                                   + ppa.col38[12,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.10)

plot(pp2$ppa.cLd.10, type = "l", col = "red")
lines(pp$ppa.cLd.10, col = "blue")




################################################
################################################
# pairs(pp[,c(40,9,13,31)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col40 <- lm(pp2[pp2$month==1, 40] ~ pp2[pp2$month==1, 9] + pp2[pp2$month==1, 13]) # Jan
ppa.2.col40 <- lm(pp2[pp2$month==2, 40] ~ pp2[pp2$month==2, 9] + pp2[pp2$month==2, 13]) # Feb
ppa.3.col40 <- lm(pp2[pp2$month==3, 40] ~ pp2[pp2$month==3, 9] + pp2[pp2$month==3, 13]) # Mar
ppa.4.col40 <- lm(pp2[pp2$month==4, 40] ~ pp2[pp2$month==4, 9] + pp2[pp2$month==4, 13]) # Apr
ppa.5.col40 <- lm(pp2[pp2$month==5, 40] ~ pp2[pp2$month==5, 9] + pp2[pp2$month==5, 13]) # May
ppa.6.col40 <- lm(pp2[pp2$month==6, 40] ~ pp2[pp2$month==6, 9] + pp2[pp2$month==6, 13]) # Jun
ppa.7.col40 <- lm(pp2[pp2$month==7, 40] ~ pp2[pp2$month==7, 9] + pp2[pp2$month==7, 13]) # Jul
ppa.8.col40 <- lm(pp2[pp2$month==8, 40] ~ pp2[pp2$month==8, 9] + pp2[pp2$month==8, 13]) # Aug
ppa.9.col40 <- lm(pp2[pp2$month==9, 40] ~ pp2[pp2$month==9, 9] + pp2[pp2$month==9, 13]) # Sep
ppa.10.col40 <- lm(pp2[pp2$month==10, 40] ~ pp2[pp2$month==10, 9] + pp2[pp2$month==10, 13]) # Oct
ppa.11.col40 <- lm(pp2[pp2$month==11, 40] ~ pp2[pp2$month==11, 9] + pp2[pp2$month==11, 13]) # Nov
ppa.12.col40 <- lm(pp2[pp2$month==12, 40] ~ pp2[pp2$month==12, 9] + pp2[pp2$month==12, 13]) # Dec
summary(ppa.1.col40)
summary(ppa.2.col40)
summary(ppa.3.col40)
summary(ppa.4.col40)
summary(ppa.5.col40)
summary(ppa.6.col40)
summary(ppa.7.col40)
summary(ppa.8.col40)
summary(ppa.9.col40)
summary(ppa.10.col40)
summary(ppa.11.col40)
summary(ppa.12.col40)

# Bind the coefficients together
ppa.col40 <- data.frame(rbind(coef(ppa.1.col40),coef(ppa.2.col40),coef(ppa.3.col40),coef(ppa.4.col40),coef(ppa.5.col40),coef(ppa.6.col40),
                              coef(ppa.7.col40),coef(ppa.8.col40),coef(ppa.9.col40),coef(ppa.10.col40),coef(ppa.11.col40),coef(ppa.12.col40)))
ppa.col40 <- cbind(ppa.col40, rbind(summary(ppa.1.col40)$adj.r.squared, summary(ppa.2.col40)$adj.r.squared, summary(ppa.3.col40)$adj.r.squared,
                                    summary(ppa.4.col40)$adj.r.squared, summary(ppa.5.col40)$adj.r.squared, summary(ppa.6.col40)$adj.r.squared,
                                    summary(ppa.7.col40)$adj.r.squared, summary(ppa.8.col40)$adj.r.squared, summary(ppa.9.col40)$adj.r.squared,
                                    summary(ppa.10.col40)$adj.r.squared, summary(ppa.11.col40)$adj.r.squared, summary(ppa.12.col40)$adj.r.squared))
names(ppa.col40) <- c("b0","ppa.col9","ppa.col13","r2adj")
rownames(ppa.col40) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.cLd.10 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.cLd.10), (ppa.col40[1,1] + ppa.col40[1,2]*pp2$ppa.cLdLi.10
                                                                  + ppa.col40[1,3]*pp2$ppa.cLdLi.5), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.cLd.10), (ppa.col40[2,1] + ppa.col40[2,2]*pp2$ppa.cLdLi.10
                                                                  + ppa.col40[2,3]*pp2$ppa.cLdLi.5), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.cLd.10), (ppa.col40[3,1] + ppa.col40[3,2]*pp2$ppa.cLdLi.10
                                                                  + ppa.col40[3,3]*pp2$ppa.cLdLi.5), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.cLd.10), (ppa.col40[4,1] + ppa.col40[4,2]*pp2$ppa.cLdLi.10
                                                                  + ppa.col40[4,3]*pp2$ppa.cLdLi.5), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.cLd.10), (ppa.col40[5,1] + ppa.col40[5,2]*pp2$ppa.cLdLi.10
                                                                  + ppa.col40[5,3]*pp2$ppa.cLdLi.5), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.cLd.10), (ppa.col40[6,1] + ppa.col40[6,2]*pp2$ppa.cLdLi.10
                                                                  + ppa.col40[6,3]*pp2$ppa.cLdLi.5), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.cLd.10), (ppa.col40[7,1] + ppa.col40[7,2]*pp2$ppa.cLdLi.10
                                                                  + ppa.col40[7,3]*pp2$ppa.cLdLi.5), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.cLd.10), (ppa.col40[8,1] + ppa.col40[8,2]*pp2$ppa.cLdLi.10
                                                                  + ppa.col40[8,3]*pp2$ppa.cLdLi.5), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.cLd.10), (ppa.col40[9,1] + ppa.col40[9,2]*pp2$ppa.cLdLi.10
                                                                  + ppa.col40[9,3]*pp2$ppa.cLdLi.5), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.cLd.10), (ppa.col40[10,1] + ppa.col40[10,2]*pp2$ppa.cLdLi.10
                                                                   + ppa.col40[10,3]*pp2$ppa.cLdLi.5), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.cLd.10), (ppa.col40[11,1] + ppa.col40[11,2]*pp2$ppa.cLdLi.10
                                                                   + ppa.col40[11,3]*pp2$ppa.cLdLi.5), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.cLd.10), (ppa.col40[12,1] + ppa.col40[12,2]*pp2$ppa.cLdLi.10
                                                                   + ppa.col40[12,3]*pp2$ppa.cLdLi.5), pp2$ppa.cLd.10)

plot(pp2$ppa.cLd.10, type = "l", col = "red")
lines(pp$ppa.cLd.10, col = "blue")



################################################
################################################
# pairs(pp[,c(41,9,13,31)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col41 <- lm(pp2[pp2$month==1, 41] ~ pp2[pp2$month==1, 9] + pp2[pp2$month==1, 31]) # Jan
ppa.2.col41 <- lm(pp2[pp2$month==2, 41] ~ pp2[pp2$month==2, 9] + pp2[pp2$month==2, 31]) # Feb
ppa.3.col41 <- lm(pp2[pp2$month==3, 41] ~ pp2[pp2$month==3, 9] + pp2[pp2$month==3, 31]) # Mar
ppa.4.col41 <- lm(pp2[pp2$month==4, 41] ~ pp2[pp2$month==4, 9] + pp2[pp2$month==4, 31]) # Apr
ppa.5.col41 <- lm(pp2[pp2$month==5, 41] ~ pp2[pp2$month==5, 9] + pp2[pp2$month==5, 31]) # May
ppa.6.col41 <- lm(pp2[pp2$month==6, 41] ~ pp2[pp2$month==6, 9] + pp2[pp2$month==6, 31]) # Jun
ppa.7.col41 <- lm(pp2[pp2$month==7, 41] ~ pp2[pp2$month==7, 9] + pp2[pp2$month==7, 31]) # Jul
ppa.8.col41 <- lm(pp2[pp2$month==8, 41] ~ pp2[pp2$month==8, 9] + pp2[pp2$month==8, 31]) # Aug
ppa.9.col41 <- lm(pp2[pp2$month==9, 41] ~ pp2[pp2$month==9, 9] + pp2[pp2$month==9, 31]) # Sep
ppa.10.col41 <- lm(pp2[pp2$month==10, 41] ~ pp2[pp2$month==10, 9] + pp2[pp2$month==10, 31]) # Oct
ppa.11.col41 <- lm(pp2[pp2$month==11, 41] ~ pp2[pp2$month==11, 9] + pp2[pp2$month==11, 31]) # Nov
ppa.12.col41 <- lm(pp2[pp2$month==12, 41] ~ pp2[pp2$month==12, 9] + pp2[pp2$month==12, 31]) # Dec
summary(ppa.1.col41)
summary(ppa.2.col41)
summary(ppa.3.col41)
summary(ppa.4.col41)
summary(ppa.5.col41)
summary(ppa.6.col41)
summary(ppa.7.col41)
summary(ppa.8.col41)
summary(ppa.9.col41)
summary(ppa.10.col41)
summary(ppa.11.col41)
summary(ppa.12.col41)

# Bind the coefficients together
ppa.col41 <- data.frame(rbind(coef(ppa.1.col41),coef(ppa.2.col41),coef(ppa.3.col41),coef(ppa.4.col41),coef(ppa.5.col41),coef(ppa.6.col41),
                              coef(ppa.7.col41),coef(ppa.8.col41),coef(ppa.9.col41),coef(ppa.10.col41),coef(ppa.11.col41),coef(ppa.12.col41)))
ppa.col41 <- cbind(ppa.col41, rbind(summary(ppa.1.col41)$adj.r.squared, summary(ppa.2.col41)$adj.r.squared, summary(ppa.3.col41)$adj.r.squared,
                                    summary(ppa.4.col41)$adj.r.squared, summary(ppa.5.col41)$adj.r.squared, summary(ppa.6.col41)$adj.r.squared,
                                    summary(ppa.7.col41)$adj.r.squared, summary(ppa.8.col41)$adj.r.squared, summary(ppa.9.col41)$adj.r.squared,
                                    summary(ppa.10.col41)$adj.r.squared, summary(ppa.11.col41)$adj.r.squared, summary(ppa.12.col41)$adj.r.squared))
names(ppa.col41) <- c("b0","ppa.col9","ppa.col31","r2adj")
rownames(ppa.col41) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.cLd.10 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.cLd.10), (ppa.col41[1,1] + ppa.col41[1,2]*pp2$ppa.cLdLi.10
                                                                  + ppa.col41[1,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.cLd.10), (ppa.col41[2,1] + ppa.col41[2,2]*pp2$ppa.cLdLi.10
                                                                  + ppa.col41[2,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.cLd.10), (ppa.col41[3,1] + ppa.col41[3,2]*pp2$ppa.cLdLi.10
                                                                  + ppa.col41[3,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.cLd.10), (ppa.col41[4,1] + ppa.col41[4,2]*pp2$ppa.cLdLi.10
                                                                  + ppa.col41[4,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.cLd.10), (ppa.col41[5,1] + ppa.col41[5,2]*pp2$ppa.cLdLi.10
                                                                  + ppa.col41[5,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.cLd.10), (ppa.col41[6,1] + ppa.col41[6,2]*pp2$ppa.cLdLi.10
                                                                  + ppa.col41[6,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.cLd.10), (ppa.col41[7,1] + ppa.col41[7,2]*pp2$ppa.cLdLi.10
                                                                  + ppa.col41[7,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.cLd.10), (ppa.col41[8,1] + ppa.col41[8,2]*pp2$ppa.cLdLi.10
                                                                  + ppa.col41[8,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.cLd.10), (ppa.col41[9,1] + ppa.col41[9,2]*pp2$ppa.cLdLi.10
                                                                  + ppa.col41[9,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.cLd.10), (ppa.col41[10,1] + ppa.col41[10,2]*pp2$ppa.cLdLi.10
                                                                   + ppa.col41[10,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.cLd.10), (ppa.col41[11,1] + ppa.col41[11,2]*pp2$ppa.cLdLi.10
                                                                   + ppa.col41[11,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.10)
pp2$ppa.cLd.10 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.cLd.10), (ppa.col41[12,1] + ppa.col41[12,2]*pp2$ppa.cLdLi.10
                                                                   + ppa.col41[12,3]*pp2$ppa.cLd.20), pp2$ppa.cLd.10)

plot(pp2$ppa.cLd.10, type = "l", col = "red")
lines(pp$ppa.cLd.10, col = "blue")




################################################
################################################
# pairs(pp[,c(13,44,52)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col13 <- lm(pp2[pp2$month==1, 13] ~ pp2[pp2$month==1, 44] + pp2[pp2$month==1, 52]) # Jan
ppa.2.col13 <- lm(pp2[pp2$month==2, 13] ~ pp2[pp2$month==2, 44] + pp2[pp2$month==2, 52]) # Feb
ppa.3.col13 <- lm(pp2[pp2$month==3, 13] ~ pp2[pp2$month==3, 44] + pp2[pp2$month==3, 52]) # Mar
ppa.4.col13 <- lm(pp2[pp2$month==4, 13] ~ pp2[pp2$month==4, 44] + pp2[pp2$month==4, 52]) # Apr
ppa.5.col13 <- lm(pp2[pp2$month==5, 13] ~ pp2[pp2$month==5, 44] + pp2[pp2$month==5, 52]) # May
ppa.6.col13 <- lm(pp2[pp2$month==6, 13] ~ pp2[pp2$month==6, 44] + pp2[pp2$month==6, 52]) # Jun
ppa.7.col13 <- lm(pp2[pp2$month==7, 13] ~ pp2[pp2$month==7, 44] + pp2[pp2$month==7, 52]) # Jul
ppa.8.col13 <- lm(pp2[pp2$month==8, 13] ~ pp2[pp2$month==8, 44] + pp2[pp2$month==8, 52]) # Aug
ppa.9.col13 <- lm(pp2[pp2$month==9, 13] ~ pp2[pp2$month==9, 44] + pp2[pp2$month==9, 52]) # Sep
ppa.10.col13 <- lm(pp2[pp2$month==10, 13] ~ pp2[pp2$month==10, 44] + pp2[pp2$month==10, 52]) # Oct
ppa.11.col13 <- lm(pp2[pp2$month==11, 13] ~ pp2[pp2$month==11, 44] + pp2[pp2$month==11, 52]) # Nov
ppa.12.col13 <- lm(pp2[pp2$month==12, 13] ~ pp2[pp2$month==12, 44] + pp2[pp2$month==12, 52]) # Dec
summary(ppa.1.col13)
summary(ppa.2.col13)
summary(ppa.3.col13)
summary(ppa.4.col13)
summary(ppa.5.col13)
summary(ppa.6.col13)
summary(ppa.7.col13)
summary(ppa.8.col13)
summary(ppa.9.col13)
summary(ppa.10.col13)
summary(ppa.11.col13)
summary(ppa.12.col13)

# Bind the coefficients together
ppa.col13 <- data.frame(rbind(coef(ppa.1.col13),coef(ppa.2.col13),coef(ppa.3.col13),coef(ppa.4.col13),coef(ppa.5.col13),coef(ppa.6.col13),
                              coef(ppa.7.col13),coef(ppa.8.col13),coef(ppa.9.col13),coef(ppa.10.col13),coef(ppa.11.col13),coef(ppa.12.col13)))
ppa.col13 <- cbind(ppa.col13, rbind(summary(ppa.1.col13)$adj.r.squared, summary(ppa.2.col13)$adj.r.squared, summary(ppa.3.col13)$adj.r.squared,
                                    summary(ppa.4.col13)$adj.r.squared, summary(ppa.5.col13)$adj.r.squared, summary(ppa.6.col13)$adj.r.squared,
                                    summary(ppa.7.col13)$adj.r.squared, summary(ppa.8.col13)$adj.r.squared, summary(ppa.9.col13)$adj.r.squared,
                                    summary(ppa.10.col13)$adj.r.squared, summary(ppa.11.col13)$adj.r.squared, summary(ppa.12.col13)$adj.r.squared))
names(ppa.col13) <- c("b0","ppa.col44","ppa.col52","r2adj")
rownames(ppa.col13) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.cLdLi.5 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.cLdLi.5), (ppa.col13[1,1] + ppa.col13[1,2]*pp2$ppa.cLd.5
                                                                  + ppa.col13[1,3]*pp2$ppa.Tair), pp2$ppa.cLdLi.5)
pp2$ppa.cLdLi.5 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.cLdLi.5), (ppa.col13[2,1] + ppa.col13[2,2]*pp2$ppa.cLd.5
                                                                  + ppa.col13[2,3]*pp2$ppa.Tair), pp2$ppa.cLdLi.5)
pp2$ppa.cLdLi.5 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.cLdLi.5), (ppa.col13[3,1] + ppa.col13[3,2]*pp2$ppa.cLd.5
                                                                  + ppa.col13[3,3]*pp2$ppa.Tair), pp2$ppa.cLdLi.5)
pp2$ppa.cLdLi.5 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.cLdLi.5), (ppa.col13[4,1] + ppa.col13[4,2]*pp2$ppa.cLd.5
                                                                  + ppa.col13[4,3]*pp2$ppa.Tair), pp2$ppa.cLdLi.5)
pp2$ppa.cLdLi.5 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.cLdLi.5), (ppa.col13[5,1] + ppa.col13[5,2]*pp2$ppa.cLd.5
                                                                  + ppa.col13[5,3]*pp2$ppa.Tair), pp2$ppa.cLdLi.5)
pp2$ppa.cLdLi.5 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.cLdLi.5), (ppa.col13[6,1] + ppa.col13[6,2]*pp2$ppa.cLd.5
                                                                  + ppa.col13[6,3]*pp2$ppa.Tair), pp2$ppa.cLdLi.5)
pp2$ppa.cLdLi.5 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.cLdLi.5), (ppa.col13[7,1] + ppa.col13[7,2]*pp2$ppa.cLd.5
                                                                  + ppa.col13[7,3]*pp2$ppa.Tair), pp2$ppa.cLdLi.5)
pp2$ppa.cLdLi.5 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.cLdLi.5), (ppa.col13[8,1] + ppa.col13[8,2]*pp2$ppa.cLd.5
                                                                  + ppa.col13[8,3]*pp2$ppa.Tair), pp2$ppa.cLdLi.5)
pp2$ppa.cLdLi.5 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.cLdLi.5), (ppa.col13[9,1] + ppa.col13[9,2]*pp2$ppa.cLd.5
                                                                  + ppa.col13[9,3]*pp2$ppa.Tair), pp2$ppa.cLdLi.5)
pp2$ppa.cLdLi.5 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.cLdLi.5), (ppa.col13[10,1] + ppa.col13[10,2]*pp2$ppa.cLd.5
                                                                   + ppa.col13[10,3]*pp2$ppa.Tair), pp2$ppa.cLdLi.5)
pp2$ppa.cLdLi.5 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.cLdLi.5), (ppa.col13[11,1] + ppa.col13[11,2]*pp2$ppa.cLd.5
                                                                   + ppa.col13[11,3]*pp2$ppa.Tair), pp2$ppa.cLdLi.5)
pp2$ppa.cLdLi.5 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.cLdLi.5), (ppa.col13[12,1] + ppa.col13[12,2]*pp2$ppa.cLd.5
                                                                   + ppa.col13[12,3]*pp2$ppa.Tair), pp2$ppa.cLdLi.5)

plot(pp2$ppa.cLdLi.5, type = "l", col = "red")
lines(pp$ppa.cLdLi.5, col = "blue")




################################################
################################################
# pairs(pp[,c(36,8,10)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col36 <- lm(pp2[pp2$month==1, 36] ~ pp2[pp2$month==1, 8] + pp2[pp2$month==1, 10]) # Jan
ppa.2.col36 <- lm(pp2[pp2$month==2, 36] ~ pp2[pp2$month==2, 8] + pp2[pp2$month==2, 10]) # Feb
ppa.3.col36 <- lm(pp2[pp2$month==3, 36] ~ pp2[pp2$month==3, 8] + pp2[pp2$month==3, 10]) # Mar
ppa.4.col36 <- lm(pp2[pp2$month==4, 36] ~ pp2[pp2$month==4, 8] + pp2[pp2$month==4, 10]) # Apr
ppa.5.col36 <- lm(pp2[pp2$month==5, 36] ~ pp2[pp2$month==5, 8] + pp2[pp2$month==5, 10]) # May
ppa.6.col36 <- lm(pp2[pp2$month==6, 36] ~ pp2[pp2$month==6, 8] + pp2[pp2$month==6, 10]) # Jun
ppa.7.col36 <- lm(pp2[pp2$month==7, 36] ~ pp2[pp2$month==7, 8] + pp2[pp2$month==7, 10]) # Jul
ppa.8.col36 <- lm(pp2[pp2$month==8, 36] ~ pp2[pp2$month==8, 8] + pp2[pp2$month==8, 10]) # Aug
ppa.9.col36 <- lm(pp2[pp2$month==9, 36] ~ pp2[pp2$month==9, 8] + pp2[pp2$month==9, 10]) # Sep
ppa.10.col36 <- lm(pp2[pp2$month==10, 36] ~ pp2[pp2$month==10, 8] + pp2[pp2$month==10, 10]) # Oct
ppa.11.col36 <- lm(pp2[pp2$month==11, 36] ~ pp2[pp2$month==11, 8] + pp2[pp2$month==11, 10]) # Nov
ppa.12.col36 <- lm(pp2[pp2$month==12, 36] ~ pp2[pp2$month==12, 8] + pp2[pp2$month==12, 10]) # Dec
summary(ppa.1.col36)
summary(ppa.2.col36)
summary(ppa.3.col36)
summary(ppa.4.col36)
summary(ppa.5.col36)
summary(ppa.6.col36)
summary(ppa.7.col36)
summary(ppa.8.col36)
summary(ppa.9.col36)
summary(ppa.10.col36)
summary(ppa.11.col36)
summary(ppa.12.col36)

# Bind the coefficients together
ppa.col36 <- data.frame(rbind(coef(ppa.1.col36),coef(ppa.2.col36),coef(ppa.3.col36),coef(ppa.4.col36),coef(ppa.5.col36),coef(ppa.6.col36),
                              coef(ppa.7.col36),coef(ppa.8.col36),coef(ppa.9.col36),coef(ppa.10.col36),coef(ppa.11.col36),coef(ppa.12.col36)))
ppa.col36 <- cbind(ppa.col36, rbind(summary(ppa.1.col36)$adj.r.squared, summary(ppa.2.col36)$adj.r.squared, summary(ppa.3.col36)$adj.r.squared,
                                    summary(ppa.4.col36)$adj.r.squared, summary(ppa.5.col36)$adj.r.squared, summary(ppa.6.col36)$adj.r.squared,
                                    summary(ppa.7.col36)$adj.r.squared, summary(ppa.8.col36)$adj.r.squared, summary(ppa.9.col36)$adj.r.squared,
                                    summary(ppa.10.col36)$adj.r.squared, summary(ppa.11.col36)$adj.r.squared, summary(ppa.12.col36)$adj.r.squared))
names(ppa.col36) <- c("b0","ppa.col8","ppa.col10","r2adj")
rownames(ppa.col36) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.cLd.15 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.cLd.15), (ppa.col36[1,1] + ppa.col36[1,2]*pp2$ppa.cLdLi.15
                                                                    + ppa.col36[1,3]*pp2$ppa.cLdLi.20), pp2$ppa.cLd.15)
pp2$ppa.cLd.15 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.cLd.15), (ppa.col36[2,1] + ppa.col36[2,2]*pp2$ppa.cLdLi.15
                                                                    + ppa.col36[2,3]*pp2$ppa.cLdLi.20), pp2$ppa.cLd.15)
pp2$ppa.cLd.15 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.cLd.15), (ppa.col36[3,1] + ppa.col36[3,2]*pp2$ppa.cLdLi.15
                                                                    + ppa.col36[3,3]*pp2$ppa.cLdLi.20), pp2$ppa.cLd.15)
pp2$ppa.cLd.15 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.cLd.15), (ppa.col36[4,1] + ppa.col36[4,2]*pp2$ppa.cLdLi.15
                                                                    + ppa.col36[4,3]*pp2$ppa.cLdLi.20), pp2$ppa.cLd.15)
pp2$ppa.cLd.15 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.cLd.15), (ppa.col36[5,1] + ppa.col36[5,2]*pp2$ppa.cLdLi.15
                                                                    + ppa.col36[5,3]*pp2$ppa.cLdLi.20), pp2$ppa.cLd.15)
pp2$ppa.cLd.15 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.cLd.15), (ppa.col36[6,1] + ppa.col36[6,2]*pp2$ppa.cLdLi.15
                                                                    + ppa.col36[6,3]*pp2$ppa.cLdLi.20), pp2$ppa.cLd.15)
pp2$ppa.cLd.15 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.cLd.15), (ppa.col36[7,1] + ppa.col36[7,2]*pp2$ppa.cLdLi.15
                                                                    + ppa.col36[7,3]*pp2$ppa.cLdLi.20), pp2$ppa.cLd.15)
pp2$ppa.cLd.15 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.cLd.15), (ppa.col36[8,1] + ppa.col36[8,2]*pp2$ppa.cLdLi.15
                                                                    + ppa.col36[8,3]*pp2$ppa.cLdLi.20), pp2$ppa.cLd.15)
pp2$ppa.cLd.15 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.cLd.15), (ppa.col36[9,1] + ppa.col36[9,2]*pp2$ppa.cLdLi.15
                                                                    + ppa.col36[9,3]*pp2$ppa.cLdLi.20), pp2$ppa.cLd.15)
pp2$ppa.cLd.15 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.cLd.15), (ppa.col36[10,1] + ppa.col36[10,2]*pp2$ppa.cLdLi.15
                                                                     + ppa.col36[10,3]*pp2$ppa.cLdLi.20), pp2$ppa.cLd.15)
pp2$ppa.cLd.15 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.cLd.15), (ppa.col36[11,1] + ppa.col36[11,2]*pp2$ppa.cLdLi.15
                                                                     + ppa.col36[11,3]*pp2$ppa.cLdLi.20), pp2$ppa.cLd.15)
pp2$ppa.cLd.15 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.cLd.15), (ppa.col36[12,1] + ppa.col36[12,2]*pp2$ppa.cLdLi.15
                                                                     + ppa.col36[12,3]*pp2$ppa.cLdLi.20), pp2$ppa.cLd.15)

plot(pp2$ppa.cLd.15, type = "l", col = "red")
lines(pp$ppa.cLd.15, col = "blue")



################################################
################################################
# pairs(pp[,c(42,31,38)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col42 <- lm(pp2[pp2$month==1, 42] ~ pp2[pp2$month==1, 31] + pp2[pp2$month==1, 38]) # Jan
ppa.2.col42 <- lm(pp2[pp2$month==2, 42] ~ pp2[pp2$month==2, 31] + pp2[pp2$month==2, 38]) # Feb
ppa.3.col42 <- lm(pp2[pp2$month==3, 42] ~ pp2[pp2$month==3, 31] + pp2[pp2$month==3, 38]) # Mar
ppa.4.col42 <- lm(pp2[pp2$month==4, 42] ~ pp2[pp2$month==4, 31] + pp2[pp2$month==4, 38]) # Apr
ppa.5.col42 <- lm(pp2[pp2$month==5, 42] ~ pp2[pp2$month==5, 31] + pp2[pp2$month==5, 38]) # May
ppa.6.col42 <- lm(pp2[pp2$month==6, 42] ~ pp2[pp2$month==6, 31] + pp2[pp2$month==6, 38]) # Jun
ppa.7.col42 <- lm(pp2[pp2$month==7, 42] ~ pp2[pp2$month==7, 31] + pp2[pp2$month==7, 38]) # Jul
ppa.8.col42 <- lm(pp2[pp2$month==8, 42] ~ pp2[pp2$month==8, 31] + pp2[pp2$month==8, 38]) # Aug
ppa.9.col42 <- lm(pp2[pp2$month==9, 42] ~ pp2[pp2$month==9, 31] + pp2[pp2$month==9, 38]) # Sep
ppa.10.col42 <- lm(pp2[pp2$month==10, 42] ~ pp2[pp2$month==10, 31] + pp2[pp2$month==10, 38]) # Oct
ppa.11.col42 <- lm(pp2[pp2$month==11, 42] ~ pp2[pp2$month==11, 31] + pp2[pp2$month==11, 38]) # Nov
ppa.12.col42 <- lm(pp2[pp2$month==12, 42] ~ pp2[pp2$month==12, 31] + pp2[pp2$month==12, 38]) # Dec
summary(ppa.1.col42)
summary(ppa.2.col42)
summary(ppa.3.col42)
summary(ppa.4.col42)
summary(ppa.5.col42)
summary(ppa.6.col42)
summary(ppa.7.col42)
summary(ppa.8.col42)
summary(ppa.9.col42)
summary(ppa.10.col42)
summary(ppa.11.col42)
summary(ppa.12.col42)

# Bind the coefficients together
ppa.col42 <- data.frame(rbind(coef(ppa.1.col42),coef(ppa.2.col42),coef(ppa.3.col42),coef(ppa.4.col42),coef(ppa.5.col42),coef(ppa.6.col42),
                              coef(ppa.7.col42),coef(ppa.8.col42),coef(ppa.9.col42),coef(ppa.10.col42),coef(ppa.11.col42),coef(ppa.12.col42)))
ppa.col42 <- cbind(ppa.col42, rbind(summary(ppa.1.col42)$adj.r.squared, summary(ppa.2.col42)$adj.r.squared, summary(ppa.3.col42)$adj.r.squared,
                                    summary(ppa.4.col42)$adj.r.squared, summary(ppa.5.col42)$adj.r.squared, summary(ppa.6.col42)$adj.r.squared,
                                    summary(ppa.7.col42)$adj.r.squared, summary(ppa.8.col42)$adj.r.squared, summary(ppa.9.col42)$adj.r.squared,
                                    summary(ppa.10.col42)$adj.r.squared, summary(ppa.11.col42)$adj.r.squared, summary(ppa.12.col42)$adj.r.squared))
names(ppa.col42) <- c("b0","ppa.col31","ppa.col38","r2adj")
rownames(ppa.col42) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.cLd.10.1 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.cLd.10.1), (ppa.col42[1,1] + ppa.col42[1,2]*pp2$ppa.cLd.20
                                                                  + ppa.col42[1,3]*pp2$ppa.cLd.10), pp2$ppa.cLd.10.1)
pp2$ppa.cLd.10.1 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.cLd.10.1), (ppa.col42[2,1] + ppa.col42[2,2]*pp2$ppa.cLd.20
                                                                  + ppa.col42[2,3]*pp2$ppa.cLd.10), pp2$ppa.cLd.10.1)
pp2$ppa.cLd.10.1 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.cLd.10.1), (ppa.col42[3,1] + ppa.col42[3,2]*pp2$ppa.cLd.20
                                                                  + ppa.col42[3,3]*pp2$ppa.cLd.10), pp2$ppa.cLd.10.1)
pp2$ppa.cLd.10.1 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.cLd.10.1), (ppa.col42[4,1] + ppa.col42[4,2]*pp2$ppa.cLd.20
                                                                  + ppa.col42[4,3]*pp2$ppa.cLd.10), pp2$ppa.cLd.10.1)
pp2$ppa.cLd.10.1 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.cLd.10.1), (ppa.col42[5,1] + ppa.col42[5,2]*pp2$ppa.cLd.20
                                                                  + ppa.col42[5,3]*pp2$ppa.cLd.10), pp2$ppa.cLd.10.1)
pp2$ppa.cLd.10.1 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.cLd.10.1), (ppa.col42[6,1] + ppa.col42[6,2]*pp2$ppa.cLd.20
                                                                  + ppa.col42[6,3]*pp2$ppa.cLd.10), pp2$ppa.cLd.10.1)
pp2$ppa.cLd.10.1 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.cLd.10.1), (ppa.col42[7,1] + ppa.col42[7,2]*pp2$ppa.cLd.20
                                                                  + ppa.col42[7,3]*pp2$ppa.cLd.10), pp2$ppa.cLd.10.1)
pp2$ppa.cLd.10.1 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.cLd.10.1), (ppa.col42[8,1] + ppa.col42[8,2]*pp2$ppa.cLd.20
                                                                  + ppa.col42[8,3]*pp2$ppa.cLd.10), pp2$ppa.cLd.10.1)
pp2$ppa.cLd.10.1 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.cLd.10.1), (ppa.col42[9,1] + ppa.col42[9,2]*pp2$ppa.cLd.20
                                                                  + ppa.col42[9,3]*pp2$ppa.cLd.10), pp2$ppa.cLd.10.1)
pp2$ppa.cLd.10.1 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.cLd.10.1), (ppa.col42[10,1] + ppa.col42[10,2]*pp2$ppa.cLd.20
                                                                   + ppa.col42[10,3]*pp2$ppa.cLd.10), pp2$ppa.cLd.10.1)
pp2$ppa.cLd.10.1 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.cLd.10.1), (ppa.col42[11,1] + ppa.col42[11,2]*pp2$ppa.cLd.20
                                                                   + ppa.col42[11,3]*pp2$ppa.cLd.10), pp2$ppa.cLd.10.1)
pp2$ppa.cLd.10.1 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.cLd.10.1), (ppa.col42[12,1] + ppa.col42[12,2]*pp2$ppa.cLd.20
                                                                   + ppa.col42[12,3]*pp2$ppa.cLd.10), pp2$ppa.cLd.10.1)

plot(pp2$ppa.cLd.10.1, type = "l", col = "red")
lines(pp$ppa.cLd.10.1, col = "blue")


################################################
################################################
# pairs(pp[,c(48,44,52)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col48 <- lm(pp2[pp2$month==1, 48] ~ pp2[pp2$month==1, 44] + pp2[pp2$month==1, 52]) # Jan
ppa.2.col48 <- lm(pp2[pp2$month==2, 48] ~ pp2[pp2$month==2, 44] + pp2[pp2$month==2, 52]) # Feb
ppa.3.col48 <- lm(pp2[pp2$month==3, 48] ~ pp2[pp2$month==3, 44] + pp2[pp2$month==3, 52]) # Mar
ppa.4.col48 <- lm(pp2[pp2$month==4, 48] ~ pp2[pp2$month==4, 44] + pp2[pp2$month==4, 52]) # Apr
ppa.5.col48 <- lm(pp2[pp2$month==5, 48] ~ pp2[pp2$month==5, 44] + pp2[pp2$month==5, 52]) # May
ppa.6.col48 <- lm(pp2[pp2$month==6, 48] ~ pp2[pp2$month==6, 44] + pp2[pp2$month==6, 52]) # Jun
ppa.7.col48 <- lm(pp2[pp2$month==7, 48] ~ pp2[pp2$month==7, 44] + pp2[pp2$month==7, 52]) # Jul
ppa.8.col48 <- lm(pp2[pp2$month==8, 48] ~ pp2[pp2$month==8, 44] + pp2[pp2$month==8, 52]) # Aug
ppa.9.col48 <- lm(pp2[pp2$month==9, 48] ~ pp2[pp2$month==9, 44] + pp2[pp2$month==9, 52]) # Sep
ppa.10.col48 <- lm(pp2[pp2$month==10, 48] ~ pp2[pp2$month==10, 44] + pp2[pp2$month==10, 52]) # Oct
ppa.11.col48 <- lm(pp2[pp2$month==11, 48] ~ pp2[pp2$month==11, 44] + pp2[pp2$month==11, 52]) # Nov
ppa.12.col48 <- lm(pp2[pp2$month==12, 48] ~ pp2[pp2$month==12, 44] + pp2[pp2$month==12, 52]) # Dec
summary(ppa.1.col48)
summary(ppa.2.col48)
summary(ppa.3.col48)
summary(ppa.4.col48)
summary(ppa.5.col48)
summary(ppa.6.col48)
summary(ppa.7.col48)
summary(ppa.8.col48)
summary(ppa.9.col48)
summary(ppa.10.col48)
summary(ppa.11.col48)
summary(ppa.12.col48)

# Bind the coefficients together
ppa.col48 <- data.frame(rbind(coef(ppa.1.col48),coef(ppa.2.col48),coef(ppa.3.col48),coef(ppa.4.col48),coef(ppa.5.col48),coef(ppa.6.col48),
                              coef(ppa.7.col48),coef(ppa.8.col48),coef(ppa.9.col48),coef(ppa.10.col48),coef(ppa.11.col48),coef(ppa.12.col48)))
ppa.col48 <- cbind(ppa.col48, rbind(summary(ppa.1.col48)$adj.r.squared, summary(ppa.2.col48)$adj.r.squared, summary(ppa.3.col48)$adj.r.squared,
                                    summary(ppa.4.col48)$adj.r.squared, summary(ppa.5.col48)$adj.r.squared, summary(ppa.6.col48)$adj.r.squared,
                                    summary(ppa.7.col48)$adj.r.squared, summary(ppa.8.col48)$adj.r.squared, summary(ppa.9.col48)$adj.r.squared,
                                    summary(ppa.10.col48)$adj.r.squared, summary(ppa.11.col48)$adj.r.squared, summary(ppa.12.col48)$adj.r.squared))
names(ppa.col48) <- c("b0","ppa.col44","ppa.col52","r2adj")
rownames(ppa.col48) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.cLd.5.1 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.cLd.5.1), (ppa.col48[1,1] + ppa.col48[1,2]*pp2$ppa.cLd.5
                                                                      + ppa.col48[1,3]*pp2$ppa.Tair), pp2$ppa.cLd.5.1)
pp2$ppa.cLd.5.1 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.cLd.5.1), (ppa.col48[2,1] + ppa.col48[2,2]*pp2$ppa.cLd.5
                                                                      + ppa.col48[2,3]*pp2$ppa.Tair), pp2$ppa.cLd.5.1)
pp2$ppa.cLd.5.1 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.cLd.5.1), (ppa.col48[3,1] + ppa.col48[3,2]*pp2$ppa.cLd.5
                                                                      + ppa.col48[3,3]*pp2$ppa.Tair), pp2$ppa.cLd.5.1)
pp2$ppa.cLd.5.1 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.cLd.5.1), (ppa.col48[4,1] + ppa.col48[4,2]*pp2$ppa.cLd.5
                                                                      + ppa.col48[4,3]*pp2$ppa.Tair), pp2$ppa.cLd.5.1)
pp2$ppa.cLd.5.1 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.cLd.5.1), (ppa.col48[5,1] + ppa.col48[5,2]*pp2$ppa.cLd.5
                                                                      + ppa.col48[5,3]*pp2$ppa.Tair), pp2$ppa.cLd.5.1)
pp2$ppa.cLd.5.1 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.cLd.5.1), (ppa.col48[6,1] + ppa.col48[6,2]*pp2$ppa.cLd.5
                                                                      + ppa.col48[6,3]*pp2$ppa.Tair), pp2$ppa.cLd.5.1)
pp2$ppa.cLd.5.1 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.cLd.5.1), (ppa.col48[7,1] + ppa.col48[7,2]*pp2$ppa.cLd.5
                                                                      + ppa.col48[7,3]*pp2$ppa.Tair), pp2$ppa.cLd.5.1)
pp2$ppa.cLd.5.1 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.cLd.5.1), (ppa.col48[8,1] + ppa.col48[8,2]*pp2$ppa.cLd.5
                                                                      + ppa.col48[8,3]*pp2$ppa.Tair), pp2$ppa.cLd.5.1)
pp2$ppa.cLd.5.1 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.cLd.5.1), (ppa.col48[9,1] + ppa.col48[9,2]*pp2$ppa.cLd.5
                                                                      + ppa.col48[9,3]*pp2$ppa.Tair), pp2$ppa.cLd.5.1)
pp2$ppa.cLd.5.1 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.cLd.5.1), (ppa.col48[10,1] + ppa.col48[10,2]*pp2$ppa.cLd.5
                                                                       + ppa.col48[10,3]*pp2$ppa.Tair), pp2$ppa.cLd.5.1)
pp2$ppa.cLd.5.1 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.cLd.5.1), (ppa.col48[11,1] + ppa.col48[11,2]*pp2$ppa.cLd.5
                                                                       + ppa.col48[11,3]*pp2$ppa.Tair), pp2$ppa.cLd.5.1)
pp2$ppa.cLd.5.1 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.cLd.5.1), (ppa.col48[12,1] + ppa.col48[12,2]*pp2$ppa.cLd.5
                                                                       + ppa.col48[12,3]*pp2$ppa.Tair), pp2$ppa.cLd.5.1)

plot(pp2$ppa.cLd.5.1, type = "l", col = "red")
lines(pp$ppa.cLd.5.1, col = "blue")



################################################
################################################
# pairs(pp[,c(46,44,48)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col46 <- lm(pp2[pp2$month==1, 46] ~ pp2[pp2$month==1, 44] + pp2[pp2$month==1, 48]) # Jan
ppa.2.col46 <- lm(pp2[pp2$month==2, 46] ~ pp2[pp2$month==2, 44] + pp2[pp2$month==2, 48]) # Feb
ppa.3.col46 <- lm(pp2[pp2$month==3, 46] ~ pp2[pp2$month==3, 44] + pp2[pp2$month==3, 48]) # Mar
ppa.4.col46 <- lm(pp2[pp2$month==4, 46] ~ pp2[pp2$month==4, 44] + pp2[pp2$month==4, 48]) # Apr
ppa.5.col46 <- lm(pp2[pp2$month==5, 46] ~ pp2[pp2$month==5, 44] + pp2[pp2$month==5, 48]) # May
ppa.6.col46 <- lm(pp2[pp2$month==6, 46] ~ pp2[pp2$month==6, 44] + pp2[pp2$month==6, 48]) # Jun
ppa.7.col46 <- lm(pp2[pp2$month==7, 46] ~ pp2[pp2$month==7, 44] + pp2[pp2$month==7, 48]) # Jul
ppa.8.col46 <- lm(pp2[pp2$month==8, 46] ~ pp2[pp2$month==8, 44] + pp2[pp2$month==8, 48]) # Aug
ppa.9.col46 <- lm(pp2[pp2$month==9, 46] ~ pp2[pp2$month==9, 44] + pp2[pp2$month==9, 48]) # Sep
ppa.10.col46 <- lm(pp2[pp2$month==10, 46] ~ pp2[pp2$month==10, 44] + pp2[pp2$month==10, 48]) # Oct
ppa.11.col46 <- lm(pp2[pp2$month==11, 46] ~ pp2[pp2$month==11, 44] + pp2[pp2$month==11, 48]) # Nov
ppa.12.col46 <- lm(pp2[pp2$month==12, 46] ~ pp2[pp2$month==12, 44] + pp2[pp2$month==12, 48]) # Dec
summary(ppa.1.col46)
summary(ppa.2.col46)
summary(ppa.3.col46)
summary(ppa.4.col46)
summary(ppa.5.col46)
summary(ppa.6.col46)
summary(ppa.7.col46)
summary(ppa.8.col46)
summary(ppa.9.col46)
summary(ppa.10.col46)
summary(ppa.11.col46)
summary(ppa.12.col46)

# Bind the coefficients together
ppa.col46 <- data.frame(rbind(coef(ppa.1.col46),coef(ppa.2.col46),coef(ppa.3.col46),coef(ppa.4.col46),coef(ppa.5.col46),coef(ppa.6.col46),
                              coef(ppa.7.col46),coef(ppa.8.col46),coef(ppa.9.col46),coef(ppa.10.col46),coef(ppa.11.col46),coef(ppa.12.col46)))
ppa.col46 <- cbind(ppa.col46, rbind(summary(ppa.1.col46)$adj.r.squared, summary(ppa.2.col46)$adj.r.squared, summary(ppa.3.col46)$adj.r.squared,
                                    summary(ppa.4.col46)$adj.r.squared, summary(ppa.5.col46)$adj.r.squared, summary(ppa.6.col46)$adj.r.squared,
                                    summary(ppa.7.col46)$adj.r.squared, summary(ppa.8.col46)$adj.r.squared, summary(ppa.9.col46)$adj.r.squared,
                                    summary(ppa.10.col46)$adj.r.squared, summary(ppa.11.col46)$adj.r.squared, summary(ppa.12.col46)$adj.r.squared))
names(ppa.col46) <- c("b0","ppa.col44","ppa.col48","r2adj")
rownames(ppa.col46) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.cLi.10.1 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.cLi.10.1), (ppa.col46[1,1] + ppa.col46[1,2]*pp2$ppa.cLd.5
                                                                    + ppa.col46[1,3]*pp2$ppa.cLd.5.1), pp2$ppa.cLi.10.1)
pp2$ppa.cLi.10.1 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.cLi.10.1), (ppa.col46[2,1] + ppa.col46[2,2]*pp2$ppa.cLd.5
                                                                    + ppa.col46[2,3]*pp2$ppa.cLd.5.1), pp2$ppa.cLi.10.1)
pp2$ppa.cLi.10.1 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.cLi.10.1), (ppa.col46[3,1] + ppa.col46[3,2]*pp2$ppa.cLd.5
                                                                    + ppa.col46[3,3]*pp2$ppa.cLd.5.1), pp2$ppa.cLi.10.1)
pp2$ppa.cLi.10.1 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.cLi.10.1), (ppa.col46[4,1] + ppa.col46[4,2]*pp2$ppa.cLd.5
                                                                    + ppa.col46[4,3]*pp2$ppa.cLd.5.1), pp2$ppa.cLi.10.1)
pp2$ppa.cLi.10.1 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.cLi.10.1), (ppa.col46[5,1] + ppa.col46[5,2]*pp2$ppa.cLd.5
                                                                    + ppa.col46[5,3]*pp2$ppa.cLd.5.1), pp2$ppa.cLi.10.1)
pp2$ppa.cLi.10.1 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.cLi.10.1), (ppa.col46[6,1] + ppa.col46[6,2]*pp2$ppa.cLd.5
                                                                    + ppa.col46[6,3]*pp2$ppa.cLd.5.1), pp2$ppa.cLi.10.1)
pp2$ppa.cLi.10.1 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.cLi.10.1), (ppa.col46[7,1] + ppa.col46[7,2]*pp2$ppa.cLd.5
                                                                    + ppa.col46[7,3]*pp2$ppa.cLd.5.1), pp2$ppa.cLi.10.1)
pp2$ppa.cLi.10.1 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.cLi.10.1), (ppa.col46[8,1] + ppa.col46[8,2]*pp2$ppa.cLd.5
                                                                    + ppa.col46[8,3]*pp2$ppa.cLd.5.1), pp2$ppa.cLi.10.1)
pp2$ppa.cLi.10.1 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.cLi.10.1), (ppa.col46[9,1] + ppa.col46[9,2]*pp2$ppa.cLd.5
                                                                    + ppa.col46[9,3]*pp2$ppa.cLd.5.1), pp2$ppa.cLi.10.1)
pp2$ppa.cLi.10.1 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.cLi.10.1), (ppa.col46[10,1] + ppa.col46[10,2]*pp2$ppa.cLd.5
                                                                     + ppa.col46[10,3]*pp2$ppa.cLd.5.1), pp2$ppa.cLi.10.1)
pp2$ppa.cLi.10.1 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.cLi.10.1), (ppa.col46[11,1] + ppa.col46[11,2]*pp2$ppa.cLd.5
                                                                     + ppa.col46[11,3]*pp2$ppa.cLd.5.1), pp2$ppa.cLi.10.1)
pp2$ppa.cLi.10.1 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.cLi.10.1), (ppa.col46[12,1] + ppa.col46[12,2]*pp2$ppa.cLd.5
                                                                     + ppa.col46[12,3]*pp2$ppa.cLd.5.1), pp2$ppa.cLi.10.1)

plot(pp2$ppa.cLi.10.1, type = "l", col = "red")
lines(pp$ppa.cLi.10.1, col = "blue")





################################################
################################################
# pairs(pp[,c(47,44,48)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col47 <- lm(pp2[pp2$month==1, 47] ~ pp2[pp2$month==1, 44] + pp2[pp2$month==1, 48]) # Jan
ppa.2.col47 <- lm(pp2[pp2$month==2, 47] ~ pp2[pp2$month==2, 44] + pp2[pp2$month==2, 48]) # Feb
ppa.3.col47 <- lm(pp2[pp2$month==3, 47] ~ pp2[pp2$month==3, 44] + pp2[pp2$month==3, 48]) # Mar
ppa.4.col47 <- lm(pp2[pp2$month==4, 47] ~ pp2[pp2$month==4, 44] + pp2[pp2$month==4, 48]) # Apr
ppa.5.col47 <- lm(pp2[pp2$month==5, 47] ~ pp2[pp2$month==5, 44] + pp2[pp2$month==5, 48]) # May
ppa.6.col47 <- lm(pp2[pp2$month==6, 47] ~ pp2[pp2$month==6, 44] + pp2[pp2$month==6, 48]) # Jun
ppa.7.col47 <- lm(pp2[pp2$month==7, 47] ~ pp2[pp2$month==7, 44] + pp2[pp2$month==7, 48]) # Jul
ppa.8.col47 <- lm(pp2[pp2$month==8, 47] ~ pp2[pp2$month==8, 44] + pp2[pp2$month==8, 48]) # Aug
ppa.9.col47 <- lm(pp2[pp2$month==9, 47] ~ pp2[pp2$month==9, 44] + pp2[pp2$month==9, 48]) # Sep
ppa.10.col47 <- lm(pp2[pp2$month==10, 47] ~ pp2[pp2$month==10, 44] + pp2[pp2$month==10, 48]) # Oct
ppa.11.col47 <- lm(pp2[pp2$month==11, 47] ~ pp2[pp2$month==11, 44] + pp2[pp2$month==11, 48]) # Nov
ppa.12.col47 <- lm(pp2[pp2$month==12, 47] ~ pp2[pp2$month==12, 44] + pp2[pp2$month==12, 48]) # Dec
summary(ppa.1.col47)
summary(ppa.2.col47)
summary(ppa.3.col47)
summary(ppa.4.col47)
summary(ppa.5.col47)
summary(ppa.6.col47)
summary(ppa.7.col47)
summary(ppa.8.col47)
summary(ppa.9.col47)
summary(ppa.10.col47)
summary(ppa.11.col47)
summary(ppa.12.col47)

# Bind the coefficients together
ppa.col47 <- data.frame(rbind(coef(ppa.1.col47),coef(ppa.2.col47),coef(ppa.3.col47),coef(ppa.4.col47),coef(ppa.5.col47),coef(ppa.6.col47),
                              coef(ppa.7.col47),coef(ppa.8.col47),coef(ppa.9.col47),coef(ppa.10.col47),coef(ppa.11.col47),coef(ppa.12.col47)))
ppa.col47 <- cbind(ppa.col47, rbind(summary(ppa.1.col47)$adj.r.squared, summary(ppa.2.col47)$adj.r.squared, summary(ppa.3.col47)$adj.r.squared,
                                    summary(ppa.4.col47)$adj.r.squared, summary(ppa.5.col47)$adj.r.squared, summary(ppa.6.col47)$adj.r.squared,
                                    summary(ppa.7.col47)$adj.r.squared, summary(ppa.8.col47)$adj.r.squared, summary(ppa.9.col47)$adj.r.squared,
                                    summary(ppa.10.col47)$adj.r.squared, summary(ppa.11.col47)$adj.r.squared, summary(ppa.12.col47)$adj.r.squared))
names(ppa.col47) <- c("b0","ppa.col44","ppa.col48","r2adj")
rownames(ppa.col47) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.cLi.5.1 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.cLi.5.1), (ppa.col47[1,1] + ppa.col47[1,2]*pp2$ppa.cLd.5
                                                                      + ppa.col47[1,3]*pp2$ppa.cLd.5.1), pp2$ppa.cLi.5.1)
pp2$ppa.cLi.5.1 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.cLi.5.1), (ppa.col47[2,1] + ppa.col47[2,2]*pp2$ppa.cLd.5
                                                                      + ppa.col47[2,3]*pp2$ppa.cLd.5.1), pp2$ppa.cLi.5.1)
pp2$ppa.cLi.5.1 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.cLi.5.1), (ppa.col47[3,1] + ppa.col47[3,2]*pp2$ppa.cLd.5
                                                                      + ppa.col47[3,3]*pp2$ppa.cLd.5.1), pp2$ppa.cLi.5.1)
pp2$ppa.cLi.5.1 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.cLi.5.1), (ppa.col47[4,1] + ppa.col47[4,2]*pp2$ppa.cLd.5
                                                                      + ppa.col47[4,3]*pp2$ppa.cLd.5.1), pp2$ppa.cLi.5.1)
pp2$ppa.cLi.5.1 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.cLi.5.1), (ppa.col47[5,1] + ppa.col47[5,2]*pp2$ppa.cLd.5
                                                                      + ppa.col47[5,3]*pp2$ppa.cLd.5.1), pp2$ppa.cLi.5.1)
pp2$ppa.cLi.5.1 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.cLi.5.1), (ppa.col47[6,1] + ppa.col47[6,2]*pp2$ppa.cLd.5
                                                                      + ppa.col47[6,3]*pp2$ppa.cLd.5.1), pp2$ppa.cLi.5.1)
pp2$ppa.cLi.5.1 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.cLi.5.1), (ppa.col47[7,1] + ppa.col47[7,2]*pp2$ppa.cLd.5
                                                                      + ppa.col47[7,3]*pp2$ppa.cLd.5.1), pp2$ppa.cLi.5.1)
pp2$ppa.cLi.5.1 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.cLi.5.1), (ppa.col47[8,1] + ppa.col47[8,2]*pp2$ppa.cLd.5
                                                                      + ppa.col47[8,3]*pp2$ppa.cLd.5.1), pp2$ppa.cLi.5.1)
pp2$ppa.cLi.5.1 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.cLi.5.1), (ppa.col47[9,1] + ppa.col47[9,2]*pp2$ppa.cLd.5
                                                                      + ppa.col47[9,3]*pp2$ppa.cLd.5.1), pp2$ppa.cLi.5.1)
pp2$ppa.cLi.5.1 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.cLi.5.1), (ppa.col47[10,1] + ppa.col47[10,2]*pp2$ppa.cLd.5
                                                                       + ppa.col47[10,3]*pp2$ppa.cLd.5.1), pp2$ppa.cLi.5.1)
pp2$ppa.cLi.5.1 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.cLi.5.1), (ppa.col47[11,1] + ppa.col47[11,2]*pp2$ppa.cLd.5
                                                                       + ppa.col47[11,3]*pp2$ppa.cLd.5.1), pp2$ppa.cLi.5.1)
pp2$ppa.cLi.5.1 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.cLi.5.1), (ppa.col47[12,1] + ppa.col47[12,2]*pp2$ppa.cLd.5
                                                                       + ppa.col47[12,3]*pp2$ppa.cLd.5.1), pp2$ppa.cLi.5.1)

plot(pp2$ppa.cLi.5.1, type = "l", col = "red")
lines(pp$ppa.cLi.5.1, col = "blue")



################################################
################################################
# pairs(pp[,c(26,5,6,7)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col26 <- lm(pp2[pp2$month==1, 26] ~ pp2[pp2$month==1, 5] + pp2[pp2$month==1, 7]) # Jan
ppa.2.col26 <- lm(pp2[pp2$month==2, 26] ~ pp2[pp2$month==2, 5] + pp2[pp2$month==2, 7]) # Feb
ppa.3.col26 <- lm(pp2[pp2$month==3, 26] ~ pp2[pp2$month==3, 5] + pp2[pp2$month==3, 7]) # Mar
ppa.4.col26 <- lm(pp2[pp2$month==4, 26] ~ pp2[pp2$month==4, 5] + pp2[pp2$month==4, 7]) # Apr
ppa.5.col26 <- lm(pp2[pp2$month==5, 26] ~ pp2[pp2$month==5, 5] + pp2[pp2$month==5, 7]) # May
ppa.6.col26 <- lm(pp2[pp2$month==6, 26] ~ pp2[pp2$month==6, 5] + pp2[pp2$month==6, 7]) # Jun
ppa.7.col26 <- lm(pp2[pp2$month==7, 26] ~ pp2[pp2$month==7, 5] + pp2[pp2$month==7, 7]) # Jul
ppa.8.col26 <- lm(pp2[pp2$month==8, 26] ~ pp2[pp2$month==8, 5] + pp2[pp2$month==8, 7]) # Aug
ppa.9.col26 <- lm(pp2[pp2$month==9, 26] ~ pp2[pp2$month==9, 5] + pp2[pp2$month==9, 7]) # Sep
ppa.10.col26 <- lm(pp2[pp2$month==10, 26] ~ pp2[pp2$month==10, 5] + pp2[pp2$month==10, 7]) # Oct
ppa.11.col26 <- lm(pp2[pp2$month==11, 26] ~ pp2[pp2$month==11, 5] + pp2[pp2$month==11, 7]) # Nov
ppa.12.col26 <- lm(pp2[pp2$month==12, 26] ~ pp2[pp2$month==12, 5] + pp2[pp2$month==12, 7]) # Dec
summary(ppa.1.col26)
summary(ppa.2.col26)
summary(ppa.3.col26)
summary(ppa.4.col26)
summary(ppa.5.col26)
summary(ppa.6.col26)
summary(ppa.7.col26)
summary(ppa.8.col26)
summary(ppa.9.col26)
summary(ppa.10.col26)
summary(ppa.11.col26)
summary(ppa.12.col26)

# Bind the coefficients together
ppa.col26 <- data.frame(rbind(coef(ppa.1.col26),coef(ppa.2.col26),coef(ppa.3.col26),coef(ppa.4.col26),coef(ppa.5.col26),coef(ppa.6.col26),
                              coef(ppa.7.col26),coef(ppa.8.col26),coef(ppa.9.col26),coef(ppa.10.col26),coef(ppa.11.col26),coef(ppa.12.col26)))
ppa.col26 <- cbind(ppa.col26, rbind(summary(ppa.1.col26)$adj.r.squared, summary(ppa.2.col26)$adj.r.squared, summary(ppa.3.col26)$adj.r.squared,
                                    summary(ppa.4.col26)$adj.r.squared, summary(ppa.5.col26)$adj.r.squared, summary(ppa.6.col26)$adj.r.squared,
                                    summary(ppa.7.col26)$adj.r.squared, summary(ppa.8.col26)$adj.r.squared, summary(ppa.9.col26)$adj.r.squared,
                                    summary(ppa.10.col26)$adj.r.squared, summary(ppa.11.col26)$adj.r.squared, summary(ppa.12.col26)$adj.r.squared))
names(ppa.col26) <- c("b0","ppa.col5","ppa.col7","r2adj")
rownames(ppa.col26) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.wdMo.10 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.wdMo.10), (ppa.col26[1,1] + ppa.col26[1,2]*pp2$ppa.wwBg.5
                                                                    + ppa.col26[1,3]*pp2$ppa.wwBg.5.1), pp2$ppa.wdMo.10)
pp2$ppa.wdMo.10 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.wdMo.10), (ppa.col26[2,1] + ppa.col26[2,2]*pp2$ppa.wwBg.5
                                                                    + ppa.col26[2,3]*pp2$ppa.wwBg.5.1), pp2$ppa.wdMo.10)
pp2$ppa.wdMo.10 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.wdMo.10), (ppa.col26[3,1] + ppa.col26[3,2]*pp2$ppa.wwBg.5
                                                                    + ppa.col26[3,3]*pp2$ppa.wwBg.5.1), pp2$ppa.wdMo.10)
pp2$ppa.wdMo.10 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.wdMo.10), (ppa.col26[4,1] + ppa.col26[4,2]*pp2$ppa.wwBg.5
                                                                    + ppa.col26[4,3]*pp2$ppa.wwBg.5.1), pp2$ppa.wdMo.10)
pp2$ppa.wdMo.10 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.wdMo.10), (ppa.col26[5,1] + ppa.col26[5,2]*pp2$ppa.wwBg.5
                                                                    + ppa.col26[5,3]*pp2$ppa.wwBg.5.1), pp2$ppa.wdMo.10)
pp2$ppa.wdMo.10 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.wdMo.10), (ppa.col26[6,1] + ppa.col26[6,2]*pp2$ppa.wwBg.5
                                                                    + ppa.col26[6,3]*pp2$ppa.wwBg.5.1), pp2$ppa.wdMo.10)
pp2$ppa.wdMo.10 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.wdMo.10), (ppa.col26[7,1] + ppa.col26[7,2]*pp2$ppa.wwBg.5
                                                                    + ppa.col26[7,3]*pp2$ppa.wwBg.5.1), pp2$ppa.wdMo.10)
pp2$ppa.wdMo.10 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.wdMo.10), (ppa.col26[8,1] + ppa.col26[8,2]*pp2$ppa.wwBg.5
                                                                    + ppa.col26[8,3]*pp2$ppa.wwBg.5.1), pp2$ppa.wdMo.10)
pp2$ppa.wdMo.10 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.wdMo.10), (ppa.col26[9,1] + ppa.col26[9,2]*pp2$ppa.wwBg.5
                                                                    + ppa.col26[9,3]*pp2$ppa.wwBg.5.1), pp2$ppa.wdMo.10)
pp2$ppa.wdMo.10 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.wdMo.10), (ppa.col26[10,1] + ppa.col26[10,2]*pp2$ppa.wwBg.5
                                                                     + ppa.col26[10,3]*pp2$ppa.wwBg.5.1), pp2$ppa.wdMo.10)
pp2$ppa.wdMo.10 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.wdMo.10), (ppa.col26[11,1] + ppa.col26[11,2]*pp2$ppa.wwBg.5
                                                                     + ppa.col26[11,3]*pp2$ppa.wwBg.5.1), pp2$ppa.wdMo.10)
pp2$ppa.wdMo.10 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.wdMo.10), (ppa.col26[12,1] + ppa.col26[12,2]*pp2$ppa.wwBg.5
                                                                     + ppa.col26[12,3]*pp2$ppa.wwBg.5.1), pp2$ppa.wdMo.10)

plot(pp2$ppa.wdMo.10, type = "l", col = "red")
lines(pp$ppa.wdMo.10, col = "blue")



################################################
################################################
# pairs(pp[,c(28,6,26)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col28 <- lm(pp2[pp2$month==1, 28] ~ pp2[pp2$month==1, 6] + pp2[pp2$month==1, 26]) # Jan
ppa.2.col28 <- lm(pp2[pp2$month==2, 28] ~ pp2[pp2$month==2, 6] + pp2[pp2$month==2, 26]) # Feb
ppa.3.col28 <- lm(pp2[pp2$month==3, 28] ~ pp2[pp2$month==3, 6] + pp2[pp2$month==3, 26]) # Mar
ppa.4.col28 <- lm(pp2[pp2$month==4, 28] ~ pp2[pp2$month==4, 6] + pp2[pp2$month==4, 26]) # Apr
ppa.5.col28 <- lm(pp2[pp2$month==5, 28] ~ pp2[pp2$month==5, 6] + pp2[pp2$month==5, 26]) # May
ppa.6.col28 <- lm(pp2[pp2$month==6, 28] ~ pp2[pp2$month==6, 6] + pp2[pp2$month==6, 26]) # Jun
ppa.7.col28 <- lm(pp2[pp2$month==7, 28] ~ pp2[pp2$month==7, 6] + pp2[pp2$month==7, 26]) # Jul
ppa.8.col28 <- lm(pp2[pp2$month==8, 28] ~ pp2[pp2$month==8, 6] + pp2[pp2$month==8, 26]) # Aug
ppa.9.col28 <- lm(pp2[pp2$month==9, 28] ~ pp2[pp2$month==9, 6] + pp2[pp2$month==9, 26]) # Sep
ppa.10.col28 <- lm(pp2[pp2$month==10, 28] ~ pp2[pp2$month==10, 6] + pp2[pp2$month==10, 26]) # Oct
ppa.11.col28 <- lm(pp2[pp2$month==11, 28] ~ pp2[pp2$month==11, 6] + pp2[pp2$month==11, 26]) # Nov
ppa.12.col28 <- lm(pp2[pp2$month==12, 28] ~ pp2[pp2$month==12, 6] + pp2[pp2$month==12, 26]) # Dec
summary(ppa.1.col28)
summary(ppa.2.col28)
summary(ppa.3.col28)
summary(ppa.4.col28)
summary(ppa.5.col28)
summary(ppa.6.col28)
summary(ppa.7.col28)
summary(ppa.8.col28)
summary(ppa.9.col28)
summary(ppa.10.col28)
summary(ppa.11.col28)
summary(ppa.12.col28)

# Bind the coefficients together
ppa.col28 <- data.frame(rbind(coef(ppa.1.col28),coef(ppa.2.col28),coef(ppa.3.col28),coef(ppa.4.col28),coef(ppa.5.col28),coef(ppa.6.col28),
                              coef(ppa.7.col28),coef(ppa.8.col28),coef(ppa.9.col28),coef(ppa.10.col28),coef(ppa.11.col28),coef(ppa.12.col28)))
ppa.col28 <- cbind(ppa.col28, rbind(summary(ppa.1.col28)$adj.r.squared, summary(ppa.2.col28)$adj.r.squared, summary(ppa.3.col28)$adj.r.squared,
                                    summary(ppa.4.col28)$adj.r.squared, summary(ppa.5.col28)$adj.r.squared, summary(ppa.6.col28)$adj.r.squared,
                                    summary(ppa.7.col28)$adj.r.squared, summary(ppa.8.col28)$adj.r.squared, summary(ppa.9.col28)$adj.r.squared,
                                    summary(ppa.10.col28)$adj.r.squared, summary(ppa.11.col28)$adj.r.squared, summary(ppa.12.col28)$adj.r.squared))
names(ppa.col28) <- c("b0","ppa.col6","ppa.col26","r2adj")
rownames(ppa.col28) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.wdMo.15 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.wdMo.15), (ppa.col28[1,1] + ppa.col28[1,2]*pp2$ppa.wwBg.10
                                                                    + ppa.col28[1,3]*pp2$ppa.wdMo.10), pp2$ppa.wdMo.15)
pp2$ppa.wdMo.15 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.wdMo.15), (ppa.col28[2,1] + ppa.col28[2,2]*pp2$ppa.wwBg.10
                                                                    + ppa.col28[2,3]*pp2$ppa.wdMo.10), pp2$ppa.wdMo.15)
pp2$ppa.wdMo.15 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.wdMo.15), (ppa.col28[3,1] + ppa.col28[3,2]*pp2$ppa.wwBg.10
                                                                    + ppa.col28[3,3]*pp2$ppa.wdMo.10), pp2$ppa.wdMo.15)
pp2$ppa.wdMo.15 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.wdMo.15), (ppa.col28[4,1] + ppa.col28[4,2]*pp2$ppa.wwBg.10
                                                                    + ppa.col28[4,3]*pp2$ppa.wdMo.10), pp2$ppa.wdMo.15)
pp2$ppa.wdMo.15 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.wdMo.15), (ppa.col28[5,1] + ppa.col28[5,2]*pp2$ppa.wwBg.10
                                                                    + ppa.col28[5,3]*pp2$ppa.wdMo.10), pp2$ppa.wdMo.15)
pp2$ppa.wdMo.15 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.wdMo.15), (ppa.col28[6,1] + ppa.col28[6,2]*pp2$ppa.wwBg.10
                                                                    + ppa.col28[6,3]*pp2$ppa.wdMo.10), pp2$ppa.wdMo.15)
pp2$ppa.wdMo.15 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.wdMo.15), (ppa.col28[7,1] + ppa.col28[7,2]*pp2$ppa.wwBg.10
                                                                    + ppa.col28[7,3]*pp2$ppa.wdMo.10), pp2$ppa.wdMo.15)
pp2$ppa.wdMo.15 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.wdMo.15), (ppa.col28[8,1] + ppa.col28[8,2]*pp2$ppa.wwBg.10
                                                                    + ppa.col28[8,3]*pp2$ppa.wdMo.10), pp2$ppa.wdMo.15)
pp2$ppa.wdMo.15 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.wdMo.15), (ppa.col28[9,1] + ppa.col28[9,2]*pp2$ppa.wwBg.10
                                                                    + ppa.col28[9,3]*pp2$ppa.wdMo.10), pp2$ppa.wdMo.15)
pp2$ppa.wdMo.15 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.wdMo.15), (ppa.col28[10,1] + ppa.col28[10,2]*pp2$ppa.wwBg.10
                                                                     + ppa.col28[10,3]*pp2$ppa.wdMo.10), pp2$ppa.wdMo.15)
pp2$ppa.wdMo.15 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.wdMo.15), (ppa.col28[11,1] + ppa.col28[11,2]*pp2$ppa.wwBg.10
                                                                     + ppa.col28[11,3]*pp2$ppa.wdMo.10), pp2$ppa.wdMo.15)
pp2$ppa.wdMo.15 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.wdMo.15), (ppa.col28[12,1] + ppa.col28[12,2]*pp2$ppa.wwBg.10
                                                                     + ppa.col28[12,3]*pp2$ppa.wdMo.10), pp2$ppa.wdMo.15)

plot(pp2$ppa.wdMo.15, type = "l", col = "red")
lines(pp$ppa.wdMo.15, col = "blue")

################################################
################################################
# pairs(pp[,c(25,5,6)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col25 <- lm(pp2[pp2$month==1, 25] ~ pp2[pp2$month==1, 5] + pp2[pp2$month==1, 6]) # Jan
ppa.2.col25 <- lm(pp2[pp2$month==2, 25] ~ pp2[pp2$month==2, 5] + pp2[pp2$month==2, 6]) # Feb
ppa.3.col25 <- lm(pp2[pp2$month==3, 25] ~ pp2[pp2$month==3, 5] + pp2[pp2$month==3, 6]) # Mar
ppa.4.col25 <- lm(pp2[pp2$month==4, 25] ~ pp2[pp2$month==4, 5] + pp2[pp2$month==4, 6]) # Apr
ppa.5.col25 <- lm(pp2[pp2$month==5, 25] ~ pp2[pp2$month==5, 5] + pp2[pp2$month==5, 6]) # May
ppa.6.col25 <- lm(pp2[pp2$month==6, 25] ~ pp2[pp2$month==6, 5] + pp2[pp2$month==6, 6]) # Jun
ppa.7.col25 <- lm(pp2[pp2$month==7, 25] ~ pp2[pp2$month==7, 5] + pp2[pp2$month==7, 6]) # Jul
ppa.8.col25 <- lm(pp2[pp2$month==8, 25] ~ pp2[pp2$month==8, 5] + pp2[pp2$month==8, 6]) # Aug
ppa.9.col25 <- lm(pp2[pp2$month==9, 25] ~ pp2[pp2$month==9, 5] + pp2[pp2$month==9, 6]) # Sep
ppa.10.col25 <- lm(pp2[pp2$month==10, 25] ~ pp2[pp2$month==10, 5] + pp2[pp2$month==10, 6]) # Oct
ppa.11.col25 <- lm(pp2[pp2$month==11, 25] ~ pp2[pp2$month==11, 5] + pp2[pp2$month==11, 6]) # Nov
ppa.12.col25 <- lm(pp2[pp2$month==12, 25] ~ pp2[pp2$month==12, 5] + pp2[pp2$month==12, 6]) # Dec
summary(ppa.1.col25)
summary(ppa.2.col25)
summary(ppa.3.col25)
summary(ppa.4.col25)
summary(ppa.5.col25)
summary(ppa.6.col25)
summary(ppa.7.col25)
summary(ppa.8.col25)
summary(ppa.9.col25)
summary(ppa.10.col25)
summary(ppa.11.col25)
summary(ppa.12.col25)

# Bind the coefficients together
ppa.col25 <- data.frame(rbind(coef(ppa.1.col25),coef(ppa.2.col25),coef(ppa.3.col25),coef(ppa.4.col25),coef(ppa.5.col25),coef(ppa.6.col25),
                              coef(ppa.7.col25),coef(ppa.8.col25),coef(ppa.9.col25),coef(ppa.10.col25),coef(ppa.11.col25),coef(ppa.12.col25)))
ppa.col25 <- cbind(ppa.col25, rbind(summary(ppa.1.col25)$adj.r.squared, summary(ppa.2.col25)$adj.r.squared, summary(ppa.3.col25)$adj.r.squared,
                                    summary(ppa.4.col25)$adj.r.squared, summary(ppa.5.col25)$adj.r.squared, summary(ppa.6.col25)$adj.r.squared,
                                    summary(ppa.7.col25)$adj.r.squared, summary(ppa.8.col25)$adj.r.squared, summary(ppa.9.col25)$adj.r.squared,
                                    summary(ppa.10.col25)$adj.r.squared, summary(ppa.11.col25)$adj.r.squared, summary(ppa.12.col25)$adj.r.squared))
names(ppa.col25) <- c("b0","ppa.col5","ppa.col6","r2adj")
rownames(ppa.col25) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.wdMo.5 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.wdMo.5), (ppa.col25[1,1] + ppa.col25[1,2]*pp2$ppa.wwBg.5
                                                                    + ppa.col25[1,3]*pp2$ppa.wwBg.10), pp2$ppa.wdMo.5)
pp2$ppa.wdMo.5 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.wdMo.5), (ppa.col25[2,1] + ppa.col25[2,2]*pp2$ppa.wwBg.5
                                                                    + ppa.col25[2,3]*pp2$ppa.wwBg.10), pp2$ppa.wdMo.5)
pp2$ppa.wdMo.5 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.wdMo.5), (ppa.col25[3,1] + ppa.col25[3,2]*pp2$ppa.wwBg.5
                                                                    + ppa.col25[3,3]*pp2$ppa.wwBg.10), pp2$ppa.wdMo.5)
pp2$ppa.wdMo.5 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.wdMo.5), (ppa.col25[4,1] + ppa.col25[4,2]*pp2$ppa.wwBg.5
                                                                    + ppa.col25[4,3]*pp2$ppa.wwBg.10), pp2$ppa.wdMo.5)
pp2$ppa.wdMo.5 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.wdMo.5), (ppa.col25[5,1] + ppa.col25[5,2]*pp2$ppa.wwBg.5
                                                                    + ppa.col25[5,3]*pp2$ppa.wwBg.10), pp2$ppa.wdMo.5)
pp2$ppa.wdMo.5 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.wdMo.5), (ppa.col25[6,1] + ppa.col25[6,2]*pp2$ppa.wwBg.5
                                                                    + ppa.col25[6,3]*pp2$ppa.wwBg.10), pp2$ppa.wdMo.5)
pp2$ppa.wdMo.5 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.wdMo.5), (ppa.col25[7,1] + ppa.col25[7,2]*pp2$ppa.wwBg.5
                                                                    + ppa.col25[7,3]*pp2$ppa.wwBg.10), pp2$ppa.wdMo.5)
pp2$ppa.wdMo.5 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.wdMo.5), (ppa.col25[8,1] + ppa.col25[8,2]*pp2$ppa.wwBg.5
                                                                    + ppa.col25[8,3]*pp2$ppa.wwBg.10), pp2$ppa.wdMo.5)
pp2$ppa.wdMo.5 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.wdMo.5), (ppa.col25[9,1] + ppa.col25[9,2]*pp2$ppa.wwBg.5
                                                                    + ppa.col25[9,3]*pp2$ppa.wwBg.10), pp2$ppa.wdMo.5)
pp2$ppa.wdMo.5 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.wdMo.5), (ppa.col25[10,1] + ppa.col25[10,2]*pp2$ppa.wwBg.5
                                                                     + ppa.col25[10,3]*pp2$ppa.wwBg.10), pp2$ppa.wdMo.5)
pp2$ppa.wdMo.5 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.wdMo.5), (ppa.col25[11,1] + ppa.col25[11,2]*pp2$ppa.wwBg.5
                                                                     + ppa.col25[11,3]*pp2$ppa.wwBg.10), pp2$ppa.wdMo.5)
pp2$ppa.wdMo.5 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.wdMo.5), (ppa.col25[12,1] + ppa.col25[12,2]*pp2$ppa.wwBg.5
                                                                     + ppa.col25[12,3]*pp2$ppa.wwBg.10), pp2$ppa.wdMo.5)

plot(pp2$ppa.wdMo.5, type = "l", col = "red")
lines(pp$ppa.wdMo.5, col = "blue")



################################################
################################################
# pairs(pp[,c(29,5,7,25)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPA column 5 temperatures based on PPA air + wsu 15 cm
ppa.1.col29 <- lm(pp2[pp2$month==1, 29] ~ pp2[pp2$month==1, 5] + pp2[pp2$month==1, 7]) # Jan
ppa.2.col29 <- lm(pp2[pp2$month==2, 29] ~ pp2[pp2$month==2, 5] + pp2[pp2$month==2, 7]) # Feb
ppa.3.col29 <- lm(pp2[pp2$month==3, 29] ~ pp2[pp2$month==3, 5] + pp2[pp2$month==3, 7]) # Mar
ppa.4.col29 <- lm(pp2[pp2$month==4, 29] ~ pp2[pp2$month==4, 5] + pp2[pp2$month==4, 7]) # Apr
ppa.5.col29 <- lm(pp2[pp2$month==5, 29] ~ pp2[pp2$month==5, 5] + pp2[pp2$month==5, 7]) # May
ppa.6.col29 <- lm(pp2[pp2$month==6, 29] ~ pp2[pp2$month==6, 5] + pp2[pp2$month==6, 7]) # Jun
ppa.7.col29 <- lm(pp2[pp2$month==7, 29] ~ pp2[pp2$month==7, 5] + pp2[pp2$month==7, 7]) # Jul
ppa.8.col29 <- lm(pp2[pp2$month==8, 29] ~ pp2[pp2$month==8, 5] + pp2[pp2$month==8, 7]) # Aug
ppa.9.col29 <- lm(pp2[pp2$month==9, 29] ~ pp2[pp2$month==9, 5] + pp2[pp2$month==9, 7]) # Sep
ppa.10.col29 <- lm(pp2[pp2$month==10, 29] ~ pp2[pp2$month==10, 5] + pp2[pp2$month==10, 7]) # Oct
ppa.11.col29 <- lm(pp2[pp2$month==11, 29] ~ pp2[pp2$month==11, 5] + pp2[pp2$month==11, 7]) # Nov
ppa.12.col29 <- lm(pp2[pp2$month==12, 29] ~ pp2[pp2$month==12, 5] + pp2[pp2$month==12, 7]) # Dec
summary(ppa.1.col29)
summary(ppa.2.col29)
summary(ppa.3.col29)
summary(ppa.4.col29)
summary(ppa.5.col29)
summary(ppa.6.col29)
summary(ppa.7.col29)
summary(ppa.8.col29)
summary(ppa.9.col29)
summary(ppa.10.col29)
summary(ppa.11.col29)
summary(ppa.12.col29)

# Bind the coefficients together
ppa.col29 <- data.frame(rbind(coef(ppa.1.col29),coef(ppa.2.col29),coef(ppa.3.col29),coef(ppa.4.col29),coef(ppa.5.col29),coef(ppa.6.col29),
                              coef(ppa.7.col29),coef(ppa.8.col29),coef(ppa.9.col29),coef(ppa.10.col29),coef(ppa.11.col29),coef(ppa.12.col29)))
ppa.col29 <- cbind(ppa.col29, rbind(summary(ppa.1.col29)$adj.r.squared, summary(ppa.2.col29)$adj.r.squared, summary(ppa.3.col29)$adj.r.squared,
                                    summary(ppa.4.col29)$adj.r.squared, summary(ppa.5.col29)$adj.r.squared, summary(ppa.6.col29)$adj.r.squared,
                                    summary(ppa.7.col29)$adj.r.squared, summary(ppa.8.col29)$adj.r.squared, summary(ppa.9.col29)$adj.r.squared,
                                    summary(ppa.10.col29)$adj.r.squared, summary(ppa.11.col29)$adj.r.squared, summary(ppa.12.col29)$adj.r.squared))
names(ppa.col29) <- c("b0","ppa.col5","ppa.col7","r2adj")
rownames(ppa.col29) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPA ppa.wwBg.5 NAs
pp2$ppa.wdMo.5.1 <- ifelse(pp2$month == 1 & is.na(pp2$ppa.wdMo.5.1), (ppa.col29[1,1] + ppa.col29[1,2]*pp2$ppa.wwBg.5
                                                                  + ppa.col29[1,3]*pp2$ppa.wwBg.5.1), pp2$ppa.wdMo.5.1)
pp2$ppa.wdMo.5.1 <- ifelse(pp2$month == 2 & is.na(pp2$ppa.wdMo.5.1), (ppa.col29[2,1] + ppa.col29[2,2]*pp2$ppa.wwBg.5
                                                                  + ppa.col29[2,3]*pp2$ppa.wwBg.5.1), pp2$ppa.wdMo.5.1)
pp2$ppa.wdMo.5.1 <- ifelse(pp2$month == 3 & is.na(pp2$ppa.wdMo.5.1), (ppa.col29[3,1] + ppa.col29[3,2]*pp2$ppa.wwBg.5
                                                                  + ppa.col29[3,3]*pp2$ppa.wwBg.5.1), pp2$ppa.wdMo.5.1)
pp2$ppa.wdMo.5.1 <- ifelse(pp2$month == 4 & is.na(pp2$ppa.wdMo.5.1), (ppa.col29[4,1] + ppa.col29[4,2]*pp2$ppa.wwBg.5
                                                                  + ppa.col29[4,3]*pp2$ppa.wwBg.5.1), pp2$ppa.wdMo.5.1)
pp2$ppa.wdMo.5.1 <- ifelse(pp2$month == 5 & is.na(pp2$ppa.wdMo.5.1), (ppa.col29[5,1] + ppa.col29[5,2]*pp2$ppa.wwBg.5
                                                                  + ppa.col29[5,3]*pp2$ppa.wwBg.5.1), pp2$ppa.wdMo.5.1)
pp2$ppa.wdMo.5.1 <- ifelse(pp2$month == 6 & is.na(pp2$ppa.wdMo.5.1), (ppa.col29[6,1] + ppa.col29[6,2]*pp2$ppa.wwBg.5
                                                                  + ppa.col29[6,3]*pp2$ppa.wwBg.5.1), pp2$ppa.wdMo.5.1)
pp2$ppa.wdMo.5.1 <- ifelse(pp2$month == 7 & is.na(pp2$ppa.wdMo.5.1), (ppa.col29[7,1] + ppa.col29[7,2]*pp2$ppa.wwBg.5
                                                                  + ppa.col29[7,3]*pp2$ppa.wwBg.5.1), pp2$ppa.wdMo.5.1)
pp2$ppa.wdMo.5.1 <- ifelse(pp2$month == 8 & is.na(pp2$ppa.wdMo.5.1), (ppa.col29[8,1] + ppa.col29[8,2]*pp2$ppa.wwBg.5
                                                                  + ppa.col29[8,3]*pp2$ppa.wwBg.5.1), pp2$ppa.wdMo.5.1)
pp2$ppa.wdMo.5.1 <- ifelse(pp2$month == 9 & is.na(pp2$ppa.wdMo.5.1), (ppa.col29[9,1] + ppa.col29[9,2]*pp2$ppa.wwBg.5
                                                                  + ppa.col29[9,3]*pp2$ppa.wwBg.5.1), pp2$ppa.wdMo.5.1)
pp2$ppa.wdMo.5.1 <- ifelse(pp2$month == 10 & is.na(pp2$ppa.wdMo.5.1), (ppa.col29[10,1] + ppa.col29[10,2]*pp2$ppa.wwBg.5
                                                                   + ppa.col29[10,3]*pp2$ppa.wwBg.5.1), pp2$ppa.wdMo.5.1)
pp2$ppa.wdMo.5.1 <- ifelse(pp2$month == 11 & is.na(pp2$ppa.wdMo.5.1), (ppa.col29[11,1] + ppa.col29[11,2]*pp2$ppa.wwBg.5
                                                                   + ppa.col29[11,3]*pp2$ppa.wwBg.5.1), pp2$ppa.wdMo.5.1)
pp2$ppa.wdMo.5.1 <- ifelse(pp2$month == 12 & is.na(pp2$ppa.wdMo.5.1), (ppa.col29[12,1] + ppa.col29[12,2]*pp2$ppa.wwBg.5
                                                                   + ppa.col29[12,3]*pp2$ppa.wwBg.5.1), pp2$ppa.wdMo.5.1)

plot(pp2$ppa.wdMo.5.1, type = "l", col = "red")
lines(pp$ppa.wdMo.5.1, col = "blue")


##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################

## PPD

################################################
################################################
# pairs(pp[,c(95,52,96)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPD air temperatures based on PPA and WSU
ppd.1.col95 <- lm(pp2[pp2$month==1, 95] ~ pp2[pp2$month==1, 52] + pp2[pp2$month==1, 96]) # Jan
ppd.2.col95 <- lm(pp2[pp2$month==2, 95] ~ pp2[pp2$month==2, 52] + pp2[pp2$month==2, 96]) # Feb
ppd.3.col95 <- lm(pp2[pp2$month==3, 95] ~ pp2[pp2$month==3, 52] + pp2[pp2$month==3, 96]) # Mar
ppd.4.col95 <- lm(pp2[pp2$month==4, 95] ~ pp2[pp2$month==4, 52] + pp2[pp2$month==4, 96]) # Apr
ppd.5.col95 <- lm(pp2[pp2$month==5, 95] ~ pp2[pp2$month==5, 52] + pp2[pp2$month==5, 96]) # May
ppd.6.col95 <- lm(pp2[pp2$month==6, 95] ~ pp2[pp2$month==6, 52] + pp2[pp2$month==6, 96]) # Jun
ppd.7.col95 <- lm(pp2[pp2$month==7, 95] ~ pp2[pp2$month==7, 52] + pp2[pp2$month==7, 96]) # Jul
ppd.8.col95 <- lm(pp2[pp2$month==8, 95] ~ pp2[pp2$month==8, 52] + pp2[pp2$month==8, 96]) # Aug
ppd.9.col95 <- lm(pp2[pp2$month==9, 95] ~ pp2[pp2$month==9, 52] + pp2[pp2$month==9, 96]) # Sep
ppd.10.col95 <- lm(pp2[pp2$month==10, 95] ~ pp2[pp2$month==10, 52] + pp2[pp2$month==10, 96]) # Oct
ppd.11.col95 <- lm(pp2[pp2$month==11, 95] ~ pp2[pp2$month==11, 52] + pp2[pp2$month==11, 96]) # Nov
ppd.12.col95 <- lm(pp2[pp2$month==12, 95] ~ pp2[pp2$month==12, 52] + pp2[pp2$month==12, 96]) # Dec
summary(ppd.1.col95)
summary(ppd.2.col95)
summary(ppd.3.col95)
summary(ppd.4.col95)
summary(ppd.5.col95)
summary(ppd.6.col95)
summary(ppd.7.col95)
summary(ppd.8.col95)
summary(ppd.9.col95)
summary(ppd.10.col95)
summary(ppd.11.col95)
summary(ppd.12.col95)

# Bind the coefficients together
ppd.col95 <- data.frame(rbind(coef(ppd.1.col95),coef(ppd.2.col95),coef(ppd.3.col95),coef(ppd.4.col95),coef(ppd.5.col95),coef(ppd.6.col95),
                              coef(ppd.7.col95),coef(ppd.8.col95),coef(ppd.9.col95),coef(ppd.10.col95),coef(ppd.11.col95),coef(ppd.12.col95)))
ppd.col95 <- cbind(ppd.col95, rbind(summary(ppd.1.col95)$adj.r.squared, summary(ppd.2.col95)$adj.r.squared, summary(ppd.3.col95)$adj.r.squared,
                                    summary(ppd.4.col95)$adj.r.squared, summary(ppd.5.col95)$adj.r.squared, summary(ppd.6.col95)$adj.r.squared,
                                    summary(ppd.7.col95)$adj.r.squared, summary(ppd.8.col95)$adj.r.squared, summary(ppd.9.col95)$adj.r.squared,
                                    summary(ppd.10.col95)$adj.r.squared, summary(ppd.11.col95)$adj.r.squared, summary(ppd.12.col95)$adj.r.squared))
names(ppd.col95) <- c("b0","ppd.col52","ppd.col96","r2adj")
rownames(ppd.col95) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPD ppd.wwBg.5 NAs
pp2$ppd.Temp.150.cm <- ifelse(pp2$month == 1 & is.na(pp2$ppd.Temp.150.cm), (ppd.col95[1,1] + ppd.col95[1,2]*pp2$ppa.Tair
                                                                      + ppd.col95[1,3]*pp2$wsu.Temp.150cm), pp2$ppd.Temp.150.cm)
pp2$ppd.Temp.150.cm <- ifelse(pp2$month == 2 & is.na(pp2$ppd.Temp.150.cm), (ppd.col95[2,1] + ppd.col95[2,2]*pp2$ppa.Tair
                                                                      + ppd.col95[2,3]*pp2$wsu.Temp.150cm), pp2$ppd.Temp.150.cm)
pp2$ppd.Temp.150.cm <- ifelse(pp2$month == 3 & is.na(pp2$ppd.Temp.150.cm), (ppd.col95[3,1] + ppd.col95[3,2]*pp2$ppa.Tair
                                                                      + ppd.col95[3,3]*pp2$wsu.Temp.150cm), pp2$ppd.Temp.150.cm)
pp2$ppd.Temp.150.cm <- ifelse(pp2$month == 4 & is.na(pp2$ppd.Temp.150.cm), (ppd.col95[4,1] + ppd.col95[4,2]*pp2$ppa.Tair
                                                                      + ppd.col95[4,3]*pp2$wsu.Temp.150cm), pp2$ppd.Temp.150.cm)
pp2$ppd.Temp.150.cm <- ifelse(pp2$month == 5 & is.na(pp2$ppd.Temp.150.cm), (ppd.col95[5,1] + ppd.col95[5,2]*pp2$ppa.Tair
                                                                      + ppd.col95[5,3]*pp2$wsu.Temp.150cm), pp2$ppd.Temp.150.cm)
pp2$ppd.Temp.150.cm <- ifelse(pp2$month == 6 & is.na(pp2$ppd.Temp.150.cm), (ppd.col95[6,1] + ppd.col95[6,2]*pp2$ppa.Tair
                                                                      + ppd.col95[6,3]*pp2$wsu.Temp.150cm), pp2$ppd.Temp.150.cm)
pp2$ppd.Temp.150.cm <- ifelse(pp2$month == 7 & is.na(pp2$ppd.Temp.150.cm), (ppd.col95[7,1] + ppd.col95[7,2]*pp2$ppa.Tair
                                                                      + ppd.col95[7,3]*pp2$wsu.Temp.150cm), pp2$ppd.Temp.150.cm)
pp2$ppd.Temp.150.cm <- ifelse(pp2$month == 8 & is.na(pp2$ppd.Temp.150.cm), (ppd.col95[8,1] + ppd.col95[8,2]*pp2$ppa.Tair
                                                                      + ppd.col95[8,3]*pp2$wsu.Temp.150cm), pp2$ppd.Temp.150.cm)
pp2$ppd.Temp.150.cm <- ifelse(pp2$month == 9 & is.na(pp2$ppd.Temp.150.cm), (ppd.col95[9,1] + ppd.col95[9,2]*pp2$ppa.Tair
                                                                      + ppd.col95[9,3]*pp2$wsu.Temp.150cm), pp2$ppd.Temp.150.cm)
pp2$ppd.Temp.150.cm <- ifelse(pp2$month == 10 & is.na(pp2$ppd.Temp.150.cm), (ppd.col95[10,1] + ppd.col95[10,2]*pp2$ppa.Tair
                                                                       + ppd.col95[10,3]*pp2$wsu.Temp.150cm), pp2$ppd.Temp.150.cm)
pp2$ppd.Temp.150.cm <- ifelse(pp2$month == 11 & is.na(pp2$ppd.Temp.150.cm), (ppd.col95[11,1] + ppd.col95[11,2]*pp2$ppa.Tair
                                                                       + ppd.col95[11,3]*pp2$wsu.Temp.150cm), pp2$ppd.Temp.150.cm)
pp2$ppd.Temp.150.cm <- ifelse(pp2$month == 12 & is.na(pp2$ppd.Temp.150.cm), (ppd.col95[12,1] + ppd.col95[12,2]*pp2$ppa.Tair
                                                                       + ppd.col95[12,3]*pp2$wsu.Temp.150cm), pp2$ppd.Temp.150.cm)

plot(pp2$ppd.Temp.150.cm, type = "l", col = "red")
lines(pp$ppd.Temp.150.cm, col = "blue")




################################################
################################################
# pairs(pp[,c(54,43,95)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPD air temperatures based on PPA and WSU
ppd.1.col54 <- lm(pp2[pp2$month==1, 54] ~ pp2[pp2$month==1, 43] + pp2[pp2$month==1, 95]) # Jan
ppd.2.col54 <- lm(pp2[pp2$month==2, 54] ~ pp2[pp2$month==2, 43] + pp2[pp2$month==2, 95]) # Feb
ppd.3.col54 <- lm(pp2[pp2$month==3, 54] ~ pp2[pp2$month==3, 43] + pp2[pp2$month==3, 95]) # Mar
ppd.4.col54 <- lm(pp2[pp2$month==4, 54] ~ pp2[pp2$month==4, 43] + pp2[pp2$month==4, 95]) # Apr
ppd.5.col54 <- lm(pp2[pp2$month==5, 54] ~ pp2[pp2$month==5, 43] + pp2[pp2$month==5, 95]) # May
ppd.6.col54 <- lm(pp2[pp2$month==6, 54] ~ pp2[pp2$month==6, 43] + pp2[pp2$month==6, 95]) # Jun
ppd.7.col54 <- lm(pp2[pp2$month==7, 54] ~ pp2[pp2$month==7, 43] + pp2[pp2$month==7, 95]) # Jul
ppd.8.col54 <- lm(pp2[pp2$month==8, 54] ~ pp2[pp2$month==8, 43] + pp2[pp2$month==8, 95]) # Aug
ppd.9.col54 <- lm(pp2[pp2$month==9, 54] ~ pp2[pp2$month==9, 43] + pp2[pp2$month==9, 95]) # Sep
ppd.10.col54 <- lm(pp2[pp2$month==10, 54] ~ pp2[pp2$month==10, 43] + pp2[pp2$month==10, 95]) # Oct
ppd.11.col54 <- lm(pp2[pp2$month==11, 54] ~ pp2[pp2$month==11, 43] + pp2[pp2$month==11, 95]) # Nov
ppd.12.col54 <- lm(pp2[pp2$month==12, 54] ~ pp2[pp2$month==12, 43] + pp2[pp2$month==12, 95]) # Dec
summary(ppd.1.col54)
summary(ppd.2.col54)
summary(ppd.3.col54)
summary(ppd.4.col54)
summary(ppd.5.col54)
summary(ppd.6.col54)
summary(ppd.7.col54)
summary(ppd.8.col54)
summary(ppd.9.col54)
summary(ppd.10.col54)
summary(ppd.11.col54)
summary(ppd.12.col54)

# Bind the coefficients together
ppd.col54 <- data.frame(rbind(coef(ppd.1.col54),coef(ppd.2.col54),coef(ppd.3.col54),coef(ppd.4.col54),coef(ppd.5.col54),coef(ppd.6.col54),
                              coef(ppd.7.col54),coef(ppd.8.col54),coef(ppd.9.col54),coef(ppd.10.col54),coef(ppd.11.col54),coef(ppd.12.col54)))
ppd.col54 <- cbind(ppd.col54, rbind(summary(ppd.1.col54)$adj.r.squared, summary(ppd.2.col54)$adj.r.squared, summary(ppd.3.col54)$adj.r.squared,
                                    summary(ppd.4.col54)$adj.r.squared, summary(ppd.5.col54)$adj.r.squared, summary(ppd.6.col54)$adj.r.squared,
                                    summary(ppd.7.col54)$adj.r.squared, summary(ppd.8.col54)$adj.r.squared, summary(ppd.9.col54)$adj.r.squared,
                                    summary(ppd.10.col54)$adj.r.squared, summary(ppd.11.col54)$adj.r.squared, summary(ppd.12.col54)$adj.r.squared))
names(ppd.col54) <- c("b0","ppd.col43","ppd.col95","r2adj")
rownames(ppd.col54) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPD ppd.wwBg.5 NAs
pp2$ppd.Mast0.cm <- ifelse(pp2$month == 1 & is.na(pp2$ppd.Mast0.cm), (ppd.col54[1,1] + ppd.col54[1,2]*pp2$ppa.cLd.0.1
                                                                            + ppd.col54[1,3]*pp2$ppd.Temp.150.cm), pp2$ppd.Mast0.cm)
pp2$ppd.Mast0.cm <- ifelse(pp2$month == 2 & is.na(pp2$ppd.Mast0.cm), (ppd.col54[2,1] + ppd.col54[2,2]*pp2$ppa.cLd.0.1
                                                                            + ppd.col54[2,3]*pp2$ppd.Temp.150.cm), pp2$ppd.Mast0.cm)
pp2$ppd.Mast0.cm <- ifelse(pp2$month == 3 & is.na(pp2$ppd.Mast0.cm), (ppd.col54[3,1] + ppd.col54[3,2]*pp2$ppa.cLd.0.1
                                                                            + ppd.col54[3,3]*pp2$ppd.Temp.150.cm), pp2$ppd.Mast0.cm)
pp2$ppd.Mast0.cm <- ifelse(pp2$month == 4 & is.na(pp2$ppd.Mast0.cm), (ppd.col54[4,1] + ppd.col54[4,2]*pp2$ppa.cLd.0.1
                                                                            + ppd.col54[4,3]*pp2$ppd.Temp.150.cm), pp2$ppd.Mast0.cm)
pp2$ppd.Mast0.cm <- ifelse(pp2$month == 5 & is.na(pp2$ppd.Mast0.cm), (ppd.col54[5,1] + ppd.col54[5,2]*pp2$ppa.cLd.0.1
                                                                            + ppd.col54[5,3]*pp2$ppd.Temp.150.cm), pp2$ppd.Mast0.cm)
pp2$ppd.Mast0.cm <- ifelse(pp2$month == 6 & is.na(pp2$ppd.Mast0.cm), (ppd.col54[6,1] + ppd.col54[6,2]*pp2$ppa.cLd.0.1
                                                                            + ppd.col54[6,3]*pp2$ppd.Temp.150.cm), pp2$ppd.Mast0.cm)
pp2$ppd.Mast0.cm <- ifelse(pp2$month == 7 & is.na(pp2$ppd.Mast0.cm), (ppd.col54[7,1] + ppd.col54[7,2]*pp2$ppa.cLd.0.1
                                                                            + ppd.col54[7,3]*pp2$ppd.Temp.150.cm), pp2$ppd.Mast0.cm)
pp2$ppd.Mast0.cm <- ifelse(pp2$month == 8 & is.na(pp2$ppd.Mast0.cm), (ppd.col54[8,1] + ppd.col54[8,2]*pp2$ppa.cLd.0.1
                                                                            + ppd.col54[8,3]*pp2$ppd.Temp.150.cm), pp2$ppd.Mast0.cm)
pp2$ppd.Mast0.cm <- ifelse(pp2$month == 9 & is.na(pp2$ppd.Mast0.cm), (ppd.col54[9,1] + ppd.col54[9,2]*pp2$ppa.cLd.0.1
                                                                            + ppd.col54[9,3]*pp2$ppd.Temp.150.cm), pp2$ppd.Mast0.cm)
pp2$ppd.Mast0.cm <- ifelse(pp2$month == 10 & is.na(pp2$ppd.Mast0.cm), (ppd.col54[10,1] + ppd.col54[10,2]*pp2$ppa.cLd.0.1
                                                                             + ppd.col54[10,3]*pp2$ppd.Temp.150.cm), pp2$ppd.Mast0.cm)
pp2$ppd.Mast0.cm <- ifelse(pp2$month == 11 & is.na(pp2$ppd.Mast0.cm), (ppd.col54[11,1] + ppd.col54[11,2]*pp2$ppa.cLd.0.1
                                                                             + ppd.col54[11,3]*pp2$ppd.Temp.150.cm), pp2$ppd.Mast0.cm)
pp2$ppd.Mast0.cm <- ifelse(pp2$month == 12 & is.na(pp2$ppd.Mast0.cm), (ppd.col54[12,1] + ppd.col54[12,2]*pp2$ppa.cLd.0.1
                                                                             + ppd.col54[12,3]*pp2$ppd.Temp.150.cm), pp2$ppd.Mast0.cm)

plot(pp2$ppd.Mast0.cm, type = "l", col = "red")
lines(pp$ppd.Mast0.cm, col = "blue")


################################################
################################################
# pairs(pp[,c(55,54,95)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPD air temperatures based on PPA and WSU
ppd.1.col55 <- lm(pp2[pp2$month==1, 55] ~ pp2[pp2$month==1, 54] + pp2[pp2$month==1, 95]) # Jan
ppd.2.col55 <- lm(pp2[pp2$month==2, 55] ~ pp2[pp2$month==2, 54] + pp2[pp2$month==2, 95]) # Feb
ppd.3.col55 <- lm(pp2[pp2$month==3, 55] ~ pp2[pp2$month==3, 54] + pp2[pp2$month==3, 95]) # Mar
ppd.4.col55 <- lm(pp2[pp2$month==4, 55] ~ pp2[pp2$month==4, 54] + pp2[pp2$month==4, 95]) # Apr
ppd.5.col55 <- lm(pp2[pp2$month==5, 55] ~ pp2[pp2$month==5, 54] + pp2[pp2$month==5, 95]) # May
ppd.6.col55 <- lm(pp2[pp2$month==6, 55] ~ pp2[pp2$month==6, 54] + pp2[pp2$month==6, 95]) # Jun
ppd.7.col55 <- lm(pp2[pp2$month==7, 55] ~ pp2[pp2$month==7, 54] + pp2[pp2$month==7, 95]) # Jul
ppd.8.col55 <- lm(pp2[pp2$month==8, 55] ~ pp2[pp2$month==8, 54] + pp2[pp2$month==8, 95]) # Aug
ppd.9.col55 <- lm(pp2[pp2$month==9, 55] ~ pp2[pp2$month==9, 54] + pp2[pp2$month==9, 95]) # Sep
ppd.10.col55 <- lm(pp2[pp2$month==10, 55] ~ pp2[pp2$month==10, 54] + pp2[pp2$month==10, 95]) # Oct
ppd.11.col55 <- lm(pp2[pp2$month==11, 55] ~ pp2[pp2$month==11, 54] + pp2[pp2$month==11, 95]) # Nov
ppd.12.col55 <- lm(pp2[pp2$month==12, 55] ~ pp2[pp2$month==12, 54] + pp2[pp2$month==12, 95]) # Dec
summary(ppd.1.col55)
summary(ppd.2.col55)
summary(ppd.3.col55)
summary(ppd.4.col55)
summary(ppd.5.col55)
summary(ppd.6.col55)
summary(ppd.7.col55)
summary(ppd.8.col55)
summary(ppd.9.col55)
summary(ppd.10.col55)
summary(ppd.11.col55)
summary(ppd.12.col55)

# Bind the coefficients together
ppd.col55 <- data.frame(rbind(coef(ppd.1.col55),coef(ppd.2.col55),coef(ppd.3.col55),coef(ppd.4.col55),coef(ppd.5.col55),coef(ppd.6.col55),
                              coef(ppd.7.col55),coef(ppd.8.col55),coef(ppd.9.col55),coef(ppd.10.col55),coef(ppd.11.col55),coef(ppd.12.col55)))
ppd.col55 <- cbind(ppd.col55, rbind(summary(ppd.1.col55)$adj.r.squared, summary(ppd.2.col55)$adj.r.squared, summary(ppd.3.col55)$adj.r.squared,
                                    summary(ppd.4.col55)$adj.r.squared, summary(ppd.5.col55)$adj.r.squared, summary(ppd.6.col55)$adj.r.squared,
                                    summary(ppd.7.col55)$adj.r.squared, summary(ppd.8.col55)$adj.r.squared, summary(ppd.9.col55)$adj.r.squared,
                                    summary(ppd.10.col55)$adj.r.squared, summary(ppd.11.col55)$adj.r.squared, summary(ppd.12.col55)$adj.r.squared))
names(ppd.col55) <- c("b0","ppd.col54","ppd.col95","r2adj")
rownames(ppd.col55) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPD ppd.wwBg.5 NAs
pp2$ppd.Mast.5.cm.1 <- ifelse(pp2$month == 1 & is.na(pp2$ppd.Mast.5.cm.1), (ppd.col55[1,1] + ppd.col55[1,2]*pp2$ppd.Mast0.cm
                                                                      + ppd.col55[1,3]*pp2$ppd.Temp.150.cm), pp2$ppd.Mast.5.cm.1)
pp2$ppd.Mast.5.cm.1 <- ifelse(pp2$month == 2 & is.na(pp2$ppd.Mast.5.cm.1), (ppd.col55[2,1] + ppd.col55[2,2]*pp2$ppd.Mast0.cm
                                                                      + ppd.col55[2,3]*pp2$ppd.Temp.150.cm), pp2$ppd.Mast.5.cm.1)
pp2$ppd.Mast.5.cm.1 <- ifelse(pp2$month == 3 & is.na(pp2$ppd.Mast.5.cm.1), (ppd.col55[3,1] + ppd.col55[3,2]*pp2$ppd.Mast0.cm
                                                                      + ppd.col55[3,3]*pp2$ppd.Temp.150.cm), pp2$ppd.Mast.5.cm.1)
pp2$ppd.Mast.5.cm.1 <- ifelse(pp2$month == 4 & is.na(pp2$ppd.Mast.5.cm.1), (ppd.col55[4,1] + ppd.col55[4,2]*pp2$ppd.Mast0.cm
                                                                      + ppd.col55[4,3]*pp2$ppd.Temp.150.cm), pp2$ppd.Mast.5.cm.1)
pp2$ppd.Mast.5.cm.1 <- ifelse(pp2$month == 5 & is.na(pp2$ppd.Mast.5.cm.1), (ppd.col55[5,1] + ppd.col55[5,2]*pp2$ppd.Mast0.cm
                                                                      + ppd.col55[5,3]*pp2$ppd.Temp.150.cm), pp2$ppd.Mast.5.cm.1)
pp2$ppd.Mast.5.cm.1 <- ifelse(pp2$month == 6 & is.na(pp2$ppd.Mast.5.cm.1), (ppd.col55[6,1] + ppd.col55[6,2]*pp2$ppd.Mast0.cm
                                                                      + ppd.col55[6,3]*pp2$ppd.Temp.150.cm), pp2$ppd.Mast.5.cm.1)
pp2$ppd.Mast.5.cm.1 <- ifelse(pp2$month == 7 & is.na(pp2$ppd.Mast.5.cm.1), (ppd.col55[7,1] + ppd.col55[7,2]*pp2$ppd.Mast0.cm
                                                                      + ppd.col55[7,3]*pp2$ppd.Temp.150.cm), pp2$ppd.Mast.5.cm.1)
pp2$ppd.Mast.5.cm.1 <- ifelse(pp2$month == 8 & is.na(pp2$ppd.Mast.5.cm.1), (ppd.col55[8,1] + ppd.col55[8,2]*pp2$ppd.Mast0.cm
                                                                      + ppd.col55[8,3]*pp2$ppd.Temp.150.cm), pp2$ppd.Mast.5.cm.1)
pp2$ppd.Mast.5.cm.1 <- ifelse(pp2$month == 9 & is.na(pp2$ppd.Mast.5.cm.1), (ppd.col55[9,1] + ppd.col55[9,2]*pp2$ppd.Mast0.cm
                                                                      + ppd.col55[9,3]*pp2$ppd.Temp.150.cm), pp2$ppd.Mast.5.cm.1)
pp2$ppd.Mast.5.cm.1 <- ifelse(pp2$month == 10 & is.na(pp2$ppd.Mast.5.cm.1), (ppd.col55[10,1] + ppd.col55[10,2]*pp2$ppd.Mast0.cm
                                                                       + ppd.col55[10,3]*pp2$ppd.Temp.150.cm), pp2$ppd.Mast.5.cm.1)
pp2$ppd.Mast.5.cm.1 <- ifelse(pp2$month == 11 & is.na(pp2$ppd.Mast.5.cm.1), (ppd.col55[11,1] + ppd.col55[11,2]*pp2$ppd.Mast0.cm
                                                                       + ppd.col55[11,3]*pp2$ppd.Temp.150.cm), pp2$ppd.Mast.5.cm.1)
pp2$ppd.Mast.5.cm.1 <- ifelse(pp2$month == 12 & is.na(pp2$ppd.Mast.5.cm.1), (ppd.col55[12,1] + ppd.col55[12,2]*pp2$ppd.Mast0.cm
                                                                       + ppd.col55[12,3]*pp2$ppd.Temp.150.cm), pp2$ppd.Mast.5.cm.1)

plot(pp2$ppd.Mast.5.cm.1, type = "l", col = "red")
lines(pp$ppd.Mast.5.cm.1, col = "blue")

################################################
################################################
# pairs(pp[,c(56,42,55)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPD air temperatures based on PPA and WSU
ppd.1.col56 <- lm(pp2[pp2$month==1, 56] ~ pp2[pp2$month==1, 42] + pp2[pp2$month==1, 55]) # Jan
ppd.2.col56 <- lm(pp2[pp2$month==2, 56] ~ pp2[pp2$month==2, 42] + pp2[pp2$month==2, 55]) # Feb
ppd.3.col56 <- lm(pp2[pp2$month==3, 56] ~ pp2[pp2$month==3, 42] + pp2[pp2$month==3, 55]) # Mar
ppd.4.col56 <- lm(pp2[pp2$month==4, 56] ~ pp2[pp2$month==4, 42] + pp2[pp2$month==4, 55]) # Apr
ppd.5.col56 <- lm(pp2[pp2$month==5, 56] ~ pp2[pp2$month==5, 42] + pp2[pp2$month==5, 55]) # May
ppd.6.col56 <- lm(pp2[pp2$month==6, 56] ~ pp2[pp2$month==6, 42] + pp2[pp2$month==6, 55]) # Jun
ppd.7.col56 <- lm(pp2[pp2$month==7, 56] ~ pp2[pp2$month==7, 42] + pp2[pp2$month==7, 55]) # Jul
ppd.8.col56 <- lm(pp2[pp2$month==8, 56] ~ pp2[pp2$month==8, 42] + pp2[pp2$month==8, 55]) # Aug
ppd.9.col56 <- lm(pp2[pp2$month==9, 56] ~ pp2[pp2$month==9, 42] + pp2[pp2$month==9, 55]) # Sep
ppd.10.col56 <- lm(pp2[pp2$month==10, 56] ~ pp2[pp2$month==10, 42] + pp2[pp2$month==10, 55]) # Oct
ppd.11.col56 <- lm(pp2[pp2$month==11, 56] ~ pp2[pp2$month==11, 42] + pp2[pp2$month==11, 55]) # Nov
ppd.12.col56 <- lm(pp2[pp2$month==12, 56] ~ pp2[pp2$month==12, 42] + pp2[pp2$month==12, 55]) # Dec
summary(ppd.1.col56)
summary(ppd.2.col56)
summary(ppd.3.col56)
summary(ppd.4.col56)
summary(ppd.5.col56)
summary(ppd.6.col56)
summary(ppd.7.col56)
summary(ppd.8.col56)
summary(ppd.9.col56)
summary(ppd.10.col56)
summary(ppd.11.col56)
summary(ppd.12.col56)

# Bind the coefficients together
ppd.col56 <- data.frame(rbind(coef(ppd.1.col56),coef(ppd.2.col56),coef(ppd.3.col56),coef(ppd.4.col56),coef(ppd.5.col56),coef(ppd.6.col56),
                              coef(ppd.7.col56),coef(ppd.8.col56),coef(ppd.9.col56),coef(ppd.10.col56),coef(ppd.11.col56),coef(ppd.12.col56)))
ppd.col56 <- cbind(ppd.col56, rbind(summary(ppd.1.col56)$adj.r.squared, summary(ppd.2.col56)$adj.r.squared, summary(ppd.3.col56)$adj.r.squared,
                                    summary(ppd.4.col56)$adj.r.squared, summary(ppd.5.col56)$adj.r.squared, summary(ppd.6.col56)$adj.r.squared,
                                    summary(ppd.7.col56)$adj.r.squared, summary(ppd.8.col56)$adj.r.squared, summary(ppd.9.col56)$adj.r.squared,
                                    summary(ppd.10.col56)$adj.r.squared, summary(ppd.11.col56)$adj.r.squared, summary(ppd.12.col56)$adj.r.squared))
names(ppd.col56) <- c("b0","ppd.col42","ppd.col55","r2adj")
rownames(ppd.col56) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPD ppd.wwBg.5 NAs
pp2$ppd.Mast.10.cm <- ifelse(pp2$month == 1 & is.na(pp2$ppd.Mast.10.cm), (ppd.col56[1,1] + ppd.col56[1,2]*pp2$ppa.cLd.10.1
                                                                            + ppd.col56[1,3]*pp2$ppd.Mast.5.cm.1), pp2$ppd.Mast.10.cm)
pp2$ppd.Mast.10.cm <- ifelse(pp2$month == 2 & is.na(pp2$ppd.Mast.10.cm), (ppd.col56[2,1] + ppd.col56[2,2]*pp2$ppa.cLd.10.1
                                                                            + ppd.col56[2,3]*pp2$ppd.Mast.5.cm.1), pp2$ppd.Mast.10.cm)
pp2$ppd.Mast.10.cm <- ifelse(pp2$month == 3 & is.na(pp2$ppd.Mast.10.cm), (ppd.col56[3,1] + ppd.col56[3,2]*pp2$ppa.cLd.10.1
                                                                            + ppd.col56[3,3]*pp2$ppd.Mast.5.cm.1), pp2$ppd.Mast.10.cm)
pp2$ppd.Mast.10.cm <- ifelse(pp2$month == 4 & is.na(pp2$ppd.Mast.10.cm), (ppd.col56[4,1] + ppd.col56[4,2]*pp2$ppa.cLd.10.1
                                                                            + ppd.col56[4,3]*pp2$ppd.Mast.5.cm.1), pp2$ppd.Mast.10.cm)
pp2$ppd.Mast.10.cm <- ifelse(pp2$month == 5 & is.na(pp2$ppd.Mast.10.cm), (ppd.col56[5,1] + ppd.col56[5,2]*pp2$ppa.cLd.10.1
                                                                            + ppd.col56[5,3]*pp2$ppd.Mast.5.cm.1), pp2$ppd.Mast.10.cm)
pp2$ppd.Mast.10.cm <- ifelse(pp2$month == 6 & is.na(pp2$ppd.Mast.10.cm), (ppd.col56[6,1] + ppd.col56[6,2]*pp2$ppa.cLd.10.1
                                                                            + ppd.col56[6,3]*pp2$ppd.Mast.5.cm.1), pp2$ppd.Mast.10.cm)
pp2$ppd.Mast.10.cm <- ifelse(pp2$month == 7 & is.na(pp2$ppd.Mast.10.cm), (ppd.col56[7,1] + ppd.col56[7,2]*pp2$ppa.cLd.10.1
                                                                            + ppd.col56[7,3]*pp2$ppd.Mast.5.cm.1), pp2$ppd.Mast.10.cm)
pp2$ppd.Mast.10.cm <- ifelse(pp2$month == 8 & is.na(pp2$ppd.Mast.10.cm), (ppd.col56[8,1] + ppd.col56[8,2]*pp2$ppa.cLd.10.1
                                                                            + ppd.col56[8,3]*pp2$ppd.Mast.5.cm.1), pp2$ppd.Mast.10.cm)
pp2$ppd.Mast.10.cm <- ifelse(pp2$month == 9 & is.na(pp2$ppd.Mast.10.cm), (ppd.col56[9,1] + ppd.col56[9,2]*pp2$ppa.cLd.10.1
                                                                            + ppd.col56[9,3]*pp2$ppd.Mast.5.cm.1), pp2$ppd.Mast.10.cm)
pp2$ppd.Mast.10.cm <- ifelse(pp2$month == 10 & is.na(pp2$ppd.Mast.10.cm), (ppd.col56[10,1] + ppd.col56[10,2]*pp2$ppa.cLd.10.1
                                                                             + ppd.col56[10,3]*pp2$ppd.Mast.5.cm.1), pp2$ppd.Mast.10.cm)
pp2$ppd.Mast.10.cm <- ifelse(pp2$month == 11 & is.na(pp2$ppd.Mast.10.cm), (ppd.col56[11,1] + ppd.col56[11,2]*pp2$ppa.cLd.10.1
                                                                             + ppd.col56[11,3]*pp2$ppd.Mast.5.cm.1), pp2$ppd.Mast.10.cm)
pp2$ppd.Mast.10.cm <- ifelse(pp2$month == 12 & is.na(pp2$ppd.Mast.10.cm), (ppd.col56[12,1] + ppd.col56[12,2]*pp2$ppa.cLd.10.1
                                                                             + ppd.col56[12,3]*pp2$ppd.Mast.5.cm.1), pp2$ppd.Mast.10.cm)

plot(pp2$ppd.Mast.10.cm, type = "l", col = "red")
lines(pp$ppd.Mast.10.cm, col = "blue")


################################################
################################################
# pairs(pp[,c(57,36,55)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPD air temperatures based on PPA and WSU
ppd.1.col57 <- lm(pp2[pp2$month==1, 56] ~ pp2[pp2$month==1, 36] + pp2[pp2$month==1, 55]) # Jan
ppd.2.col57 <- lm(pp2[pp2$month==2, 56] ~ pp2[pp2$month==2, 36] + pp2[pp2$month==2, 55]) # Feb
ppd.3.col57 <- lm(pp2[pp2$month==3, 56] ~ pp2[pp2$month==3, 36] + pp2[pp2$month==3, 55]) # Mar
ppd.4.col57 <- lm(pp2[pp2$month==4, 56] ~ pp2[pp2$month==4, 36] + pp2[pp2$month==4, 55]) # Apr
ppd.5.col57 <- lm(pp2[pp2$month==5, 56] ~ pp2[pp2$month==5, 36] + pp2[pp2$month==5, 55]) # May
ppd.6.col57 <- lm(pp2[pp2$month==6, 56] ~ pp2[pp2$month==6, 36] + pp2[pp2$month==6, 55]) # Jun
ppd.7.col57 <- lm(pp2[pp2$month==7, 56] ~ pp2[pp2$month==7, 36] + pp2[pp2$month==7, 55]) # Jul
ppd.8.col57 <- lm(pp2[pp2$month==8, 56] ~ pp2[pp2$month==8, 36] + pp2[pp2$month==8, 55]) # Aug
ppd.9.col57 <- lm(pp2[pp2$month==9, 56] ~ pp2[pp2$month==9, 36] + pp2[pp2$month==9, 55]) # Sep
ppd.10.col57 <- lm(pp2[pp2$month==10, 56] ~ pp2[pp2$month==10, 36] + pp2[pp2$month==10, 55]) # Oct
ppd.11.col57 <- lm(pp2[pp2$month==11, 56] ~ pp2[pp2$month==11, 36] + pp2[pp2$month==11, 55]) # Nov
ppd.12.col57 <- lm(pp2[pp2$month==12, 56] ~ pp2[pp2$month==12, 36] + pp2[pp2$month==12, 55]) # Dec
summary(ppd.1.col57)
summary(ppd.2.col57)
summary(ppd.3.col57)
summary(ppd.4.col57)
summary(ppd.5.col57)
summary(ppd.6.col57)
summary(ppd.7.col57)
summary(ppd.8.col57)
summary(ppd.9.col57)
summary(ppd.10.col57)
summary(ppd.11.col57)
summary(ppd.12.col57)

# Bind the coefficients together
ppd.col57 <- data.frame(rbind(coef(ppd.1.col57),coef(ppd.2.col57),coef(ppd.3.col57),coef(ppd.4.col57),coef(ppd.5.col57),coef(ppd.6.col57),
                              coef(ppd.7.col57),coef(ppd.8.col57),coef(ppd.9.col57),coef(ppd.10.col57),coef(ppd.11.col57),coef(ppd.12.col57)))
ppd.col57 <- cbind(ppd.col57, rbind(summary(ppd.1.col57)$adj.r.squared, summary(ppd.2.col57)$adj.r.squared, summary(ppd.3.col57)$adj.r.squared,
                                    summary(ppd.4.col57)$adj.r.squared, summary(ppd.5.col57)$adj.r.squared, summary(ppd.6.col57)$adj.r.squared,
                                    summary(ppd.7.col57)$adj.r.squared, summary(ppd.8.col57)$adj.r.squared, summary(ppd.9.col57)$adj.r.squared,
                                    summary(ppd.10.col57)$adj.r.squared, summary(ppd.11.col57)$adj.r.squared, summary(ppd.12.col57)$adj.r.squared))
names(ppd.col57) <- c("b0","ppd.col36","ppd.col55","r2adj")
rownames(ppd.col57) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPD ppd.wwBg.5 NAs
pp2$ppd.Mast.15.cm <- ifelse(pp2$month == 1 & is.na(pp2$ppd.Mast.15.cm), (ppd.col57[1,1] + ppd.col57[1,2]*pp2$ppa.cLd.15
                                                                          + ppd.col57[1,3]*pp2$ppd.Mast.5.cm.1), pp2$ppd.Mast.15.cm)
pp2$ppd.Mast.15.cm <- ifelse(pp2$month == 2 & is.na(pp2$ppd.Mast.15.cm), (ppd.col57[2,1] + ppd.col57[2,2]*pp2$ppa.cLd.15
                                                                          + ppd.col57[2,3]*pp2$ppd.Mast.5.cm.1), pp2$ppd.Mast.15.cm)
pp2$ppd.Mast.15.cm <- ifelse(pp2$month == 3 & is.na(pp2$ppd.Mast.15.cm), (ppd.col57[3,1] + ppd.col57[3,2]*pp2$ppa.cLd.15
                                                                          + ppd.col57[3,3]*pp2$ppd.Mast.5.cm.1), pp2$ppd.Mast.15.cm)
pp2$ppd.Mast.15.cm <- ifelse(pp2$month == 4 & is.na(pp2$ppd.Mast.15.cm), (ppd.col57[4,1] + ppd.col57[4,2]*pp2$ppa.cLd.15
                                                                          + ppd.col57[4,3]*pp2$ppd.Mast.5.cm.1), pp2$ppd.Mast.15.cm)
pp2$ppd.Mast.15.cm <- ifelse(pp2$month == 5 & is.na(pp2$ppd.Mast.15.cm), (ppd.col57[5,1] + ppd.col57[5,2]*pp2$ppa.cLd.15
                                                                          + ppd.col57[5,3]*pp2$ppd.Mast.5.cm.1), pp2$ppd.Mast.15.cm)
pp2$ppd.Mast.15.cm <- ifelse(pp2$month == 6 & is.na(pp2$ppd.Mast.15.cm), (ppd.col57[6,1] + ppd.col57[6,2]*pp2$ppa.cLd.15
                                                                          + ppd.col57[6,3]*pp2$ppd.Mast.5.cm.1), pp2$ppd.Mast.15.cm)
pp2$ppd.Mast.15.cm <- ifelse(pp2$month == 7 & is.na(pp2$ppd.Mast.15.cm), (ppd.col57[7,1] + ppd.col57[7,2]*pp2$ppa.cLd.15
                                                                          + ppd.col57[7,3]*pp2$ppd.Mast.5.cm.1), pp2$ppd.Mast.15.cm)
pp2$ppd.Mast.15.cm <- ifelse(pp2$month == 8 & is.na(pp2$ppd.Mast.15.cm), (ppd.col57[8,1] + ppd.col57[8,2]*pp2$ppa.cLd.15
                                                                          + ppd.col57[8,3]*pp2$ppd.Mast.5.cm.1), pp2$ppd.Mast.15.cm)
pp2$ppd.Mast.15.cm <- ifelse(pp2$month == 9 & is.na(pp2$ppd.Mast.15.cm), (ppd.col57[9,1] + ppd.col57[9,2]*pp2$ppa.cLd.15
                                                                          + ppd.col57[9,3]*pp2$ppd.Mast.5.cm.1), pp2$ppd.Mast.15.cm)
pp2$ppd.Mast.15.cm <- ifelse(pp2$month == 10 & is.na(pp2$ppd.Mast.15.cm), (ppd.col57[10,1] + ppd.col57[10,2]*pp2$ppa.cLd.15
                                                                           + ppd.col57[10,3]*pp2$ppd.Mast.5.cm.1), pp2$ppd.Mast.15.cm)
pp2$ppd.Mast.15.cm <- ifelse(pp2$month == 11 & is.na(pp2$ppd.Mast.15.cm), (ppd.col57[11,1] + ppd.col57[11,2]*pp2$ppa.cLd.15
                                                                           + ppd.col57[11,3]*pp2$ppd.Mast.5.cm.1), pp2$ppd.Mast.15.cm)
pp2$ppd.Mast.15.cm <- ifelse(pp2$month == 12 & is.na(pp2$ppd.Mast.15.cm), (ppd.col57[12,1] + ppd.col57[12,2]*pp2$ppa.cLd.15
                                                                           + ppd.col57[12,3]*pp2$ppd.Mast.5.cm.1), pp2$ppd.Mast.15.cm)

plot(pp2$ppd.Mast.15.cm, type = "l", col = "red")
lines(pp$ppd.Mast.15.cm, col = "blue")


################################################
################################################
# pairs(pp[,c(94,56,57)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPD air temperatures based on PPA and WSU
ppd.1.col94 <- lm(pp2[pp2$month==1, 94] ~ pp2[pp2$month==1, 56] + pp2[pp2$month==1, 57]) # Jan
ppd.2.col94 <- lm(pp2[pp2$month==2, 94] ~ pp2[pp2$month==2, 56] + pp2[pp2$month==2, 57]) # Feb
ppd.3.col94 <- lm(pp2[pp2$month==3, 94] ~ pp2[pp2$month==3, 56] + pp2[pp2$month==3, 57]) # Mar
ppd.4.col94 <- lm(pp2[pp2$month==4, 94] ~ pp2[pp2$month==4, 56] + pp2[pp2$month==4, 57]) # Apr
ppd.5.col94 <- lm(pp2[pp2$month==5, 94] ~ pp2[pp2$month==5, 56] + pp2[pp2$month==5, 57]) # May
ppd.6.col94 <- lm(pp2[pp2$month==6, 94] ~ pp2[pp2$month==6, 56] + pp2[pp2$month==6, 57]) # Jun
ppd.7.col94 <- lm(pp2[pp2$month==7, 94] ~ pp2[pp2$month==7, 56] + pp2[pp2$month==7, 57]) # Jul
ppd.8.col94 <- lm(pp2[pp2$month==8, 94] ~ pp2[pp2$month==8, 56] + pp2[pp2$month==8, 57]) # Aug
ppd.9.col94 <- lm(pp2[pp2$month==9, 94] ~ pp2[pp2$month==9, 56] + pp2[pp2$month==9, 57]) # Sep
ppd.10.col94 <- lm(pp2[pp2$month==10, 94] ~ pp2[pp2$month==10, 56] + pp2[pp2$month==10, 57]) # Oct
ppd.11.col94 <- lm(pp2[pp2$month==11, 94] ~ pp2[pp2$month==11, 56] + pp2[pp2$month==11, 57]) # Nov
ppd.12.col94 <- lm(pp2[pp2$month==12, 94] ~ pp2[pp2$month==12, 56] + pp2[pp2$month==12, 57]) # Dec
summary(ppd.1.col94)
summary(ppd.2.col94)
summary(ppd.3.col94)
summary(ppd.4.col94)
summary(ppd.5.col94)
summary(ppd.6.col94)
summary(ppd.7.col94)
summary(ppd.8.col94)
summary(ppd.9.col94)
summary(ppd.10.col94)
summary(ppd.11.col94)
summary(ppd.12.col94)

# Bind the coefficients together
ppd.col94 <- data.frame(rbind(coef(ppd.1.col94),coef(ppd.2.col94),coef(ppd.3.col94),coef(ppd.4.col94),coef(ppd.5.col94),coef(ppd.6.col94),
                              coef(ppd.7.col94),coef(ppd.8.col94),coef(ppd.9.col94),coef(ppd.10.col94),coef(ppd.11.col94),coef(ppd.12.col94)))
ppd.col94 <- cbind(ppd.col94, rbind(summary(ppd.1.col94)$adj.r.squared, summary(ppd.2.col94)$adj.r.squared, summary(ppd.3.col94)$adj.r.squared,
                                    summary(ppd.4.col94)$adj.r.squared, summary(ppd.5.col94)$adj.r.squared, summary(ppd.6.col94)$adj.r.squared,
                                    summary(ppd.7.col94)$adj.r.squared, summary(ppd.8.col94)$adj.r.squared, summary(ppd.9.col94)$adj.r.squared,
                                    summary(ppd.10.col94)$adj.r.squared, summary(ppd.11.col94)$adj.r.squared, summary(ppd.12.col94)$adj.r.squared))
names(ppd.col94) <- c("b0","ppd.col56","ppd.col57","r2adj")
rownames(ppd.col94) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPD ppd.wwBg.5 NAs
pp2$ppd..20.cmBlank <- ifelse(pp2$month == 1 & is.na(pp2$ppd..20.cmBlank), (ppd.col94[1,1] + ppd.col94[1,2]*pp2$ppd.Mast.10.cm
                                                                          + ppd.col94[1,3]*pp2$ppd.Mast.15.cm), pp2$ppd..20.cmBlank)
pp2$ppd..20.cmBlank <- ifelse(pp2$month == 2 & is.na(pp2$ppd..20.cmBlank), (ppd.col94[2,1] + ppd.col94[2,2]*pp2$ppd.Mast.10.cm
                                                                          + ppd.col94[2,3]*pp2$ppd.Mast.15.cm), pp2$ppd..20.cmBlank)
pp2$ppd..20.cmBlank <- ifelse(pp2$month == 3 & is.na(pp2$ppd..20.cmBlank), (ppd.col94[3,1] + ppd.col94[3,2]*pp2$ppd.Mast.10.cm
                                                                          + ppd.col94[3,3]*pp2$ppd.Mast.15.cm), pp2$ppd..20.cmBlank)
pp2$ppd..20.cmBlank <- ifelse(pp2$month == 4 & is.na(pp2$ppd..20.cmBlank), (ppd.col94[4,1] + ppd.col94[4,2]*pp2$ppd.Mast.10.cm
                                                                          + ppd.col94[4,3]*pp2$ppd.Mast.15.cm), pp2$ppd..20.cmBlank)
pp2$ppd..20.cmBlank <- ifelse(pp2$month == 5 & is.na(pp2$ppd..20.cmBlank), (ppd.col94[5,1] + ppd.col94[5,2]*pp2$ppd.Mast.10.cm
                                                                          + ppd.col94[5,3]*pp2$ppd.Mast.15.cm), pp2$ppd..20.cmBlank)
pp2$ppd..20.cmBlank <- ifelse(pp2$month == 6 & is.na(pp2$ppd..20.cmBlank), (ppd.col94[6,1] + ppd.col94[6,2]*pp2$ppd.Mast.10.cm
                                                                          + ppd.col94[6,3]*pp2$ppd.Mast.15.cm), pp2$ppd..20.cmBlank)
pp2$ppd..20.cmBlank <- ifelse(pp2$month == 7 & is.na(pp2$ppd..20.cmBlank), (ppd.col94[7,1] + ppd.col94[7,2]*pp2$ppd.Mast.10.cm
                                                                          + ppd.col94[7,3]*pp2$ppd.Mast.15.cm), pp2$ppd..20.cmBlank)
pp2$ppd..20.cmBlank <- ifelse(pp2$month == 8 & is.na(pp2$ppd..20.cmBlank), (ppd.col94[8,1] + ppd.col94[8,2]*pp2$ppd.Mast.10.cm
                                                                          + ppd.col94[8,3]*pp2$ppd.Mast.15.cm), pp2$ppd..20.cmBlank)
pp2$ppd..20.cmBlank <- ifelse(pp2$month == 9 & is.na(pp2$ppd..20.cmBlank), (ppd.col94[9,1] + ppd.col94[9,2]*pp2$ppd.Mast.10.cm
                                                                          + ppd.col94[9,3]*pp2$ppd.Mast.15.cm), pp2$ppd..20.cmBlank)
pp2$ppd..20.cmBlank <- ifelse(pp2$month == 10 & is.na(pp2$ppd..20.cmBlank), (ppd.col94[10,1] + ppd.col94[10,2]*pp2$ppd.Mast.10.cm
                                                                           + ppd.col94[10,3]*pp2$ppd.Mast.15.cm), pp2$ppd..20.cmBlank)
pp2$ppd..20.cmBlank <- ifelse(pp2$month == 11 & is.na(pp2$ppd..20.cmBlank), (ppd.col94[11,1] + ppd.col94[11,2]*pp2$ppd.Mast.10.cm
                                                                           + ppd.col94[11,3]*pp2$ppd.Mast.15.cm), pp2$ppd..20.cmBlank)
pp2$ppd..20.cmBlank <- ifelse(pp2$month == 12 & is.na(pp2$ppd..20.cmBlank), (ppd.col94[12,1] + ppd.col94[12,2]*pp2$ppd.Mast.10.cm
                                                                           + ppd.col94[12,3]*pp2$ppd.Mast.15.cm), pp2$ppd..20.cmBlank)

plot(pp2$ppd..20.cmBlank, type = "l", col = "red")
lines(pp$ppd..20.cmBlank, col = "blue")


################################################
################################################
# pairs(pp[,c(93,50,94)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPD air temperatures based on PPA and WSU
ppd.1.col93 <- lm(pp2[pp2$month==1, 93] ~ pp2[pp2$month==1, 50] + pp2[pp2$month==1, 94]) # Jan
ppd.2.col93 <- lm(pp2[pp2$month==2, 93] ~ pp2[pp2$month==2, 50] + pp2[pp2$month==2, 94]) # Feb
ppd.3.col93 <- lm(pp2[pp2$month==3, 93] ~ pp2[pp2$month==3, 50] + pp2[pp2$month==3, 94]) # Mar
ppd.4.col93 <- lm(pp2[pp2$month==4, 93] ~ pp2[pp2$month==4, 50] + pp2[pp2$month==4, 94]) # Apr
ppd.5.col93 <- lm(pp2[pp2$month==5, 93] ~ pp2[pp2$month==5, 50] + pp2[pp2$month==5, 94]) # May
ppd.6.col93 <- lm(pp2[pp2$month==6, 93] ~ pp2[pp2$month==6, 50] + pp2[pp2$month==6, 94]) # Jun
ppd.7.col93 <- lm(pp2[pp2$month==7, 93] ~ pp2[pp2$month==7, 50] + pp2[pp2$month==7, 94]) # Jul
ppd.8.col93 <- lm(pp2[pp2$month==8, 93] ~ pp2[pp2$month==8, 50] + pp2[pp2$month==8, 94]) # Aug
ppd.9.col93 <- lm(pp2[pp2$month==9, 93] ~ pp2[pp2$month==9, 50] + pp2[pp2$month==9, 94]) # Sep
ppd.10.col93 <- lm(pp2[pp2$month==10, 93] ~ pp2[pp2$month==10, 50] + pp2[pp2$month==10, 94]) # Oct
ppd.11.col93 <- lm(pp2[pp2$month==11, 93] ~ pp2[pp2$month==11, 50] + pp2[pp2$month==11, 94]) # Nov
ppd.12.col93 <- lm(pp2[pp2$month==12, 93] ~ pp2[pp2$month==12, 50] + pp2[pp2$month==12, 94]) # Dec
summary(ppd.1.col93)
summary(ppd.2.col93)
summary(ppd.3.col93)
summary(ppd.4.col93)
summary(ppd.5.col93)
summary(ppd.6.col93)
summary(ppd.7.col93)
summary(ppd.8.col93)
summary(ppd.9.col93)
summary(ppd.10.col93)
summary(ppd.11.col93)
summary(ppd.12.col93)

# Bind the coefficients together
ppd.col93 <- data.frame(rbind(coef(ppd.1.col93),coef(ppd.2.col93),coef(ppd.3.col93),coef(ppd.4.col93),coef(ppd.5.col93),coef(ppd.6.col93),
                              coef(ppd.7.col93),coef(ppd.8.col93),coef(ppd.9.col93),coef(ppd.10.col93),coef(ppd.11.col93),coef(ppd.12.col93)))
ppd.col93 <- cbind(ppd.col93, rbind(summary(ppd.1.col93)$adj.r.squared, summary(ppd.2.col93)$adj.r.squared, summary(ppd.3.col93)$adj.r.squared,
                                    summary(ppd.4.col93)$adj.r.squared, summary(ppd.5.col93)$adj.r.squared, summary(ppd.6.col93)$adj.r.squared,
                                    summary(ppd.7.col93)$adj.r.squared, summary(ppd.8.col93)$adj.r.squared, summary(ppd.9.col93)$adj.r.squared,
                                    summary(ppd.10.col93)$adj.r.squared, summary(ppd.11.col93)$adj.r.squared, summary(ppd.12.col93)$adj.r.squared))
names(ppd.col93) <- c("b0","ppd.col50","ppd.col94","r2adj")
rownames(ppd.col93) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPD ppd.wwBg.5 NAs
pp2$ppd..40.cmBlank <- ifelse(pp2$month == 1 & is.na(pp2$ppd..40.cmBlank), (ppd.col93[1,1] + ppd.col93[1,2]*pp2$ppa.cLdLi.40
                                                                            + ppd.col93[1,3]*pp2$ppd..20.cmBlank), pp2$ppd..40.cmBlank)
pp2$ppd..40.cmBlank <- ifelse(pp2$month == 2 & is.na(pp2$ppd..40.cmBlank), (ppd.col93[2,1] + ppd.col93[2,2]*pp2$ppa.cLdLi.40
                                                                            + ppd.col93[2,3]*pp2$ppd..20.cmBlank), pp2$ppd..40.cmBlank)
pp2$ppd..40.cmBlank <- ifelse(pp2$month == 3 & is.na(pp2$ppd..40.cmBlank), (ppd.col93[3,1] + ppd.col93[3,2]*pp2$ppa.cLdLi.40
                                                                            + ppd.col93[3,3]*pp2$ppd..20.cmBlank), pp2$ppd..40.cmBlank)
pp2$ppd..40.cmBlank <- ifelse(pp2$month == 4 & is.na(pp2$ppd..40.cmBlank), (ppd.col93[4,1] + ppd.col93[4,2]*pp2$ppa.cLdLi.40
                                                                            + ppd.col93[4,3]*pp2$ppd..20.cmBlank), pp2$ppd..40.cmBlank)
pp2$ppd..40.cmBlank <- ifelse(pp2$month == 5 & is.na(pp2$ppd..40.cmBlank), (ppd.col93[5,1] + ppd.col93[5,2]*pp2$ppa.cLdLi.40
                                                                            + ppd.col93[5,3]*pp2$ppd..20.cmBlank), pp2$ppd..40.cmBlank)
pp2$ppd..40.cmBlank <- ifelse(pp2$month == 6 & is.na(pp2$ppd..40.cmBlank), (ppd.col93[6,1] + ppd.col93[6,2]*pp2$ppa.cLdLi.40
                                                                            + ppd.col93[6,3]*pp2$ppd..20.cmBlank), pp2$ppd..40.cmBlank)
pp2$ppd..40.cmBlank <- ifelse(pp2$month == 7 & is.na(pp2$ppd..40.cmBlank), (ppd.col93[7,1] + ppd.col93[7,2]*pp2$ppa.cLdLi.40
                                                                            + ppd.col93[7,3]*pp2$ppd..20.cmBlank), pp2$ppd..40.cmBlank)
pp2$ppd..40.cmBlank <- ifelse(pp2$month == 8 & is.na(pp2$ppd..40.cmBlank), (ppd.col93[8,1] + ppd.col93[8,2]*pp2$ppa.cLdLi.40
                                                                            + ppd.col93[8,3]*pp2$ppd..20.cmBlank), pp2$ppd..40.cmBlank)
pp2$ppd..40.cmBlank <- ifelse(pp2$month == 9 & is.na(pp2$ppd..40.cmBlank), (ppd.col93[9,1] + ppd.col93[9,2]*pp2$ppa.cLdLi.40
                                                                            + ppd.col93[9,3]*pp2$ppd..20.cmBlank), pp2$ppd..40.cmBlank)
pp2$ppd..40.cmBlank <- ifelse(pp2$month == 10 & is.na(pp2$ppd..40.cmBlank), (ppd.col93[10,1] + ppd.col93[10,2]*pp2$ppa.cLdLi.40
                                                                             + ppd.col93[10,3]*pp2$ppd..20.cmBlank), pp2$ppd..40.cmBlank)
pp2$ppd..40.cmBlank <- ifelse(pp2$month == 11 & is.na(pp2$ppd..40.cmBlank), (ppd.col93[11,1] + ppd.col93[11,2]*pp2$ppa.cLdLi.40
                                                                             + ppd.col93[11,3]*pp2$ppd..20.cmBlank), pp2$ppd..40.cmBlank)
pp2$ppd..40.cmBlank <- ifelse(pp2$month == 12 & is.na(pp2$ppd..40.cmBlank), (ppd.col93[12,1] + ppd.col93[12,2]*pp2$ppa.cLdLi.40
                                                                             + ppd.col93[12,3]*pp2$ppd..20.cmBlank), pp2$ppd..40.cmBlank)

plot(pp2$ppd..40.cmBlank, type = "l", col = "red")
lines(pp$ppd..40.cmBlank, col = "blue")

################################################
################################################
# pairs(pp[,c(92,51,93)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPD air temperatures based on PPA and WSU
ppd.1.col92 <- lm(pp2[pp2$month==1, 92] ~ pp2[pp2$month==1, 51] + pp2[pp2$month==1, 93]) # Jan
ppd.2.col92 <- lm(pp2[pp2$month==2, 92] ~ pp2[pp2$month==2, 51] + pp2[pp2$month==2, 93]) # Feb
ppd.3.col92 <- lm(pp2[pp2$month==3, 92] ~ pp2[pp2$month==3, 51] + pp2[pp2$month==3, 93]) # Mar
ppd.4.col92 <- lm(pp2[pp2$month==4, 92] ~ pp2[pp2$month==4, 51] + pp2[pp2$month==4, 93]) # Apr
ppd.5.col92 <- lm(pp2[pp2$month==5, 92] ~ pp2[pp2$month==5, 51] + pp2[pp2$month==5, 93]) # May
ppd.6.col92 <- lm(pp2[pp2$month==6, 92] ~ pp2[pp2$month==6, 51] + pp2[pp2$month==6, 93]) # Jun
ppd.7.col92 <- lm(pp2[pp2$month==7, 92] ~ pp2[pp2$month==7, 51] + pp2[pp2$month==7, 93]) # Jul
ppd.8.col92 <- lm(pp2[pp2$month==8, 92] ~ pp2[pp2$month==8, 51] + pp2[pp2$month==8, 93]) # Aug
ppd.9.col92 <- lm(pp2[pp2$month==9, 92] ~ pp2[pp2$month==9, 51] + pp2[pp2$month==9, 93]) # Sep
ppd.10.col92 <- lm(pp2[pp2$month==10, 92] ~ pp2[pp2$month==10, 51] + pp2[pp2$month==10, 93]) # Oct
ppd.11.col92 <- lm(pp2[pp2$month==11, 92] ~ pp2[pp2$month==11, 51] + pp2[pp2$month==11, 93]) # Nov
ppd.12.col92 <- lm(pp2[pp2$month==12, 92] ~ pp2[pp2$month==12, 51] + pp2[pp2$month==12, 93]) # Dec
summary(ppd.1.col92)
summary(ppd.2.col92)
summary(ppd.3.col92)
summary(ppd.4.col92)
summary(ppd.5.col92)
summary(ppd.6.col92)
summary(ppd.7.col92)
summary(ppd.8.col92)
summary(ppd.9.col92)
summary(ppd.10.col92)
summary(ppd.11.col92)
summary(ppd.12.col92)

# Bind the coefficients together
ppd.col92 <- data.frame(rbind(coef(ppd.1.col92),coef(ppd.2.col92),coef(ppd.3.col92),coef(ppd.4.col92),coef(ppd.5.col92),coef(ppd.6.col92),
                              coef(ppd.7.col92),coef(ppd.8.col92),coef(ppd.9.col92),coef(ppd.10.col92),coef(ppd.11.col92),coef(ppd.12.col92)))
ppd.col92 <- cbind(ppd.col92, rbind(summary(ppd.1.col92)$adj.r.squared, summary(ppd.2.col92)$adj.r.squared, summary(ppd.3.col92)$adj.r.squared,
                                    summary(ppd.4.col92)$adj.r.squared, summary(ppd.5.col92)$adj.r.squared, summary(ppd.6.col92)$adj.r.squared,
                                    summary(ppd.7.col92)$adj.r.squared, summary(ppd.8.col92)$adj.r.squared, summary(ppd.9.col92)$adj.r.squared,
                                    summary(ppd.10.col92)$adj.r.squared, summary(ppd.11.col92)$adj.r.squared, summary(ppd.12.col92)$adj.r.squared))
names(ppd.col92) <- c("b0","ppd.col51","ppd.col93","r2adj")
rownames(ppd.col92) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPD ppd.wwBg.5 NAs
pp2$ppd..80.cmBlank <- ifelse(pp2$month == 1 & is.na(pp2$ppd..80.cmBlank), (ppd.col92[1,1] + ppd.col92[1,2]*pp2$ppa.cLdLi.80
                                                                            + ppd.col92[1,3]*pp2$ppd..40.cmBlank), pp2$ppd..80.cmBlank)
pp2$ppd..80.cmBlank <- ifelse(pp2$month == 2 & is.na(pp2$ppd..80.cmBlank), (ppd.col92[2,1] + ppd.col92[2,2]*pp2$ppa.cLdLi.80
                                                                            + ppd.col92[2,3]*pp2$ppd..40.cmBlank), pp2$ppd..80.cmBlank)
pp2$ppd..80.cmBlank <- ifelse(pp2$month == 3 & is.na(pp2$ppd..80.cmBlank), (ppd.col92[3,1] + ppd.col92[3,2]*pp2$ppa.cLdLi.80
                                                                            + ppd.col92[3,3]*pp2$ppd..40.cmBlank), pp2$ppd..80.cmBlank)
pp2$ppd..80.cmBlank <- ifelse(pp2$month == 4 & is.na(pp2$ppd..80.cmBlank), (ppd.col92[4,1] + ppd.col92[4,2]*pp2$ppa.cLdLi.80
                                                                            + ppd.col92[4,3]*pp2$ppd..40.cmBlank), pp2$ppd..80.cmBlank)
pp2$ppd..80.cmBlank <- ifelse(pp2$month == 5 & is.na(pp2$ppd..80.cmBlank), (ppd.col92[5,1] + ppd.col92[5,2]*pp2$ppa.cLdLi.80
                                                                            + ppd.col92[5,3]*pp2$ppd..40.cmBlank), pp2$ppd..80.cmBlank)
pp2$ppd..80.cmBlank <- ifelse(pp2$month == 6 & is.na(pp2$ppd..80.cmBlank), (ppd.col92[6,1] + ppd.col92[6,2]*pp2$ppa.cLdLi.80
                                                                            + ppd.col92[6,3]*pp2$ppd..40.cmBlank), pp2$ppd..80.cmBlank)
pp2$ppd..80.cmBlank <- ifelse(pp2$month == 7 & is.na(pp2$ppd..80.cmBlank), (ppd.col92[7,1] + ppd.col92[7,2]*pp2$ppa.cLdLi.80
                                                                            + ppd.col92[7,3]*pp2$ppd..40.cmBlank), pp2$ppd..80.cmBlank)
pp2$ppd..80.cmBlank <- ifelse(pp2$month == 8 & is.na(pp2$ppd..80.cmBlank), (ppd.col92[8,1] + ppd.col92[8,2]*pp2$ppa.cLdLi.80
                                                                            + ppd.col92[8,3]*pp2$ppd..40.cmBlank), pp2$ppd..80.cmBlank)
pp2$ppd..80.cmBlank <- ifelse(pp2$month == 9 & is.na(pp2$ppd..80.cmBlank), (ppd.col92[9,1] + ppd.col92[9,2]*pp2$ppa.cLdLi.80
                                                                            + ppd.col92[9,3]*pp2$ppd..40.cmBlank), pp2$ppd..80.cmBlank)
pp2$ppd..80.cmBlank <- ifelse(pp2$month == 10 & is.na(pp2$ppd..80.cmBlank), (ppd.col92[10,1] + ppd.col92[10,2]*pp2$ppa.cLdLi.80
                                                                             + ppd.col92[10,3]*pp2$ppd..40.cmBlank), pp2$ppd..80.cmBlank)
pp2$ppd..80.cmBlank <- ifelse(pp2$month == 11 & is.na(pp2$ppd..80.cmBlank), (ppd.col92[11,1] + ppd.col92[11,2]*pp2$ppa.cLdLi.80
                                                                             + ppd.col92[11,3]*pp2$ppd..40.cmBlank), pp2$ppd..80.cmBlank)
pp2$ppd..80.cmBlank <- ifelse(pp2$month == 12 & is.na(pp2$ppd..80.cmBlank), (ppd.col92[12,1] + ppd.col92[12,2]*pp2$ppa.cLdLi.80
                                                                             + ppd.col92[12,3]*pp2$ppd..40.cmBlank), pp2$ppd..80.cmBlank)

plot(pp2$ppd..80.cmBlank, type = "l", col = "red")
lines(pp$ppd..80.cmBlank, col = "blue")
abline(h = 0)



######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################

################################################
################################################
# pairs(pp[,c(107,52,95,96)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPD air temperatures based on PPA and WSU
air.1.col107 <- lm(pp2[pp2$month==1, 107] ~ pp2[pp2$month==1, 52] + pp2[pp2$month==1, 96]) # Jan
air.2.col107 <- lm(pp2[pp2$month==2, 107] ~ pp2[pp2$month==2, 52] + pp2[pp2$month==2, 96]) # Feb
air.3.col107 <- lm(pp2[pp2$month==3, 107] ~ pp2[pp2$month==3, 52] + pp2[pp2$month==3, 96]) # Mar
air.4.col107 <- lm(pp2[pp2$month==4, 107] ~ pp2[pp2$month==4, 52] + pp2[pp2$month==4, 96]) # Apr
air.5.col107 <- lm(pp2[pp2$month==5, 107] ~ pp2[pp2$month==5, 52] + pp2[pp2$month==5, 96]) # May
air.6.col107 <- lm(pp2[pp2$month==6, 107] ~ pp2[pp2$month==6, 52] + pp2[pp2$month==6, 96]) # Jun
air.7.col107 <- lm(pp2[pp2$month==7, 107] ~ pp2[pp2$month==7, 52] + pp2[pp2$month==7, 96]) # Jul
air.8.col107 <- lm(pp2[pp2$month==8, 107] ~ pp2[pp2$month==8, 52] + pp2[pp2$month==8, 96]) # Aug
air.9.col107 <- lm(pp2[pp2$month==9, 107] ~ pp2[pp2$month==9, 52] + pp2[pp2$month==9, 96]) # Sep
air.10.col107 <- lm(pp2[pp2$month==10, 107] ~ pp2[pp2$month==10, 52] + pp2[pp2$month==10, 96]) # Oct
air.11.col107 <- lm(pp2[pp2$month==11, 107] ~ pp2[pp2$month==11, 52] + pp2[pp2$month==11, 96]) # Nov
air.12.col107 <- lm(pp2[pp2$month==12, 107] ~ pp2[pp2$month==12, 52] + pp2[pp2$month==12, 96]) # Dec
summary(air.1.col107)
summary(air.2.col107)
summary(air.3.col107)
summary(air.4.col107)
summary(air.5.col107)
summary(air.6.col107)
summary(air.7.col107)
summary(air.8.col107)
summary(air.9.col107)
summary(air.10.col107)
summary(air.11.col107)
summary(air.12.col107)

# Bind the coefficients together
air.col107 <- data.frame(rbind(coef(air.1.col107),coef(air.2.col107),coef(air.3.col107),coef(air.4.col107),coef(air.5.col107),coef(air.6.col107),
                              coef(air.7.col107),coef(air.8.col107),coef(air.9.col107),coef(air.10.col107),coef(air.11.col107),coef(air.12.col107)))
air.col107 <- cbind(air.col107, rbind(summary(air.1.col107)$adj.r.squared, summary(air.2.col107)$adj.r.squared, summary(air.3.col107)$adj.r.squared,
                                    summary(air.4.col107)$adj.r.squared, summary(air.5.col107)$adj.r.squared, summary(air.6.col107)$adj.r.squared,
                                    summary(air.7.col107)$adj.r.squared, summary(air.8.col107)$adj.r.squared, summary(air.9.col107)$adj.r.squared,
                                    summary(air.10.col107)$adj.r.squared, summary(air.11.col107)$adj.r.squared, summary(air.12.col107)$adj.r.squared))
names(air.col107) <- c("b0","air.col52","air.col96","r2adj")
rownames(air.col107) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPD air.wwBg.5 NAs
pp2$air.pp.15 <- ifelse(pp2$month == 1 & is.na(pp2$air.pp.15), (air.col107[1,1] + air.col107[1,2]*pp2$ppa.Tair
                                                                            + air.col107[1,3]*pp2$wsu.Temp.150cm), pp2$air.pp.15)
pp2$air.pp.15 <- ifelse(pp2$month == 2 & is.na(pp2$air.pp.15), (air.col107[2,1] + air.col107[2,2]*pp2$ppa.Tair
                                                                            + air.col107[2,3]*pp2$wsu.Temp.150cm), pp2$air.pp.15)
pp2$air.pp.15 <- ifelse(pp2$month == 3 & is.na(pp2$air.pp.15), (air.col107[3,1] + air.col107[3,2]*pp2$ppa.Tair
                                                                            + air.col107[3,3]*pp2$wsu.Temp.150cm), pp2$air.pp.15)
pp2$air.pp.15 <- ifelse(pp2$month == 4 & is.na(pp2$air.pp.15), (air.col107[4,1] + air.col107[4,2]*pp2$ppa.Tair
                                                                            + air.col107[4,3]*pp2$wsu.Temp.150cm), pp2$air.pp.15)
pp2$air.pp.15 <- ifelse(pp2$month == 5 & is.na(pp2$air.pp.15), (air.col107[5,1] + air.col107[5,2]*pp2$ppa.Tair
                                                                            + air.col107[5,3]*pp2$wsu.Temp.150cm), pp2$air.pp.15)
pp2$air.pp.15 <- ifelse(pp2$month == 6 & is.na(pp2$air.pp.15), (air.col107[6,1] + air.col107[6,2]*pp2$ppa.Tair
                                                                            + air.col107[6,3]*pp2$wsu.Temp.150cm), pp2$air.pp.15)
pp2$air.pp.15 <- ifelse(pp2$month == 7 & is.na(pp2$air.pp.15), (air.col107[7,1] + air.col107[7,2]*pp2$ppa.Tair
                                                                            + air.col107[7,3]*pp2$wsu.Temp.150cm), pp2$air.pp.15)
pp2$air.pp.15 <- ifelse(pp2$month == 8 & is.na(pp2$air.pp.15), (air.col107[8,1] + air.col107[8,2]*pp2$ppa.Tair
                                                                            + air.col107[8,3]*pp2$wsu.Temp.150cm), pp2$air.pp.15)
pp2$air.pp.15 <- ifelse(pp2$month == 9 & is.na(pp2$air.pp.15), (air.col107[9,1] + air.col107[9,2]*pp2$ppa.Tair
                                                                            + air.col107[9,3]*pp2$wsu.Temp.150cm), pp2$air.pp.15)
pp2$air.pp.15 <- ifelse(pp2$month == 10 & is.na(pp2$air.pp.15), (air.col107[10,1] + air.col107[10,2]*pp2$ppa.Tair
                                                                             + air.col107[10,3]*pp2$wsu.Temp.150cm), pp2$air.pp.15)
pp2$air.pp.15 <- ifelse(pp2$month == 11 & is.na(pp2$air.pp.15), (air.col107[11,1] + air.col107[11,2]*pp2$ppa.Tair
                                                                             + air.col107[11,3]*pp2$wsu.Temp.150cm), pp2$air.pp.15)
pp2$air.pp.15 <- ifelse(pp2$month == 12 & is.na(pp2$air.pp.15), (air.col107[12,1] + air.col107[12,2]*pp2$ppa.Tair
                                                                             + air.col107[12,3]*pp2$wsu.Temp.150cm), pp2$air.pp.15)

plot(pp2$air.pp.15, type = "l", col = "red")
lines(pp$air.pp.15, col = "blue")




################################################
################################################
# pairs(pp[,c(108,97,107)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPD air temperatures based on PPA and WSU
air.1.col108 <- lm(pp2[pp2$month==1, 108] ~ pp2[pp2$month==1, 97] + pp2[pp2$month==1, 107]) # Jan
air.2.col108 <- lm(pp2[pp2$month==2, 108] ~ pp2[pp2$month==2, 97] + pp2[pp2$month==2, 107]) # Feb
air.3.col108 <- lm(pp2[pp2$month==3, 108] ~ pp2[pp2$month==3, 97] + pp2[pp2$month==3, 107]) # Mar
air.4.col108 <- lm(pp2[pp2$month==4, 108] ~ pp2[pp2$month==4, 97] + pp2[pp2$month==4, 107]) # Apr
air.5.col108 <- lm(pp2[pp2$month==5, 108] ~ pp2[pp2$month==5, 97] + pp2[pp2$month==5, 107]) # May
air.6.col108 <- lm(pp2[pp2$month==6, 108] ~ pp2[pp2$month==6, 97] + pp2[pp2$month==6, 107]) # Jun
air.7.col108 <- lm(pp2[pp2$month==7, 108] ~ pp2[pp2$month==7, 97] + pp2[pp2$month==7, 107]) # Jul
air.8.col108 <- lm(pp2[pp2$month==8, 108] ~ pp2[pp2$month==8, 97] + pp2[pp2$month==8, 107]) # Aug
air.9.col108 <- lm(pp2[pp2$month==9, 108] ~ pp2[pp2$month==9, 97] + pp2[pp2$month==9, 107]) # Sep
air.10.col108 <- lm(pp2[pp2$month==10, 108] ~ pp2[pp2$month==10, 97] + pp2[pp2$month==10, 107]) # Oct
air.11.col108 <- lm(pp2[pp2$month==11, 108] ~ pp2[pp2$month==11, 97] + pp2[pp2$month==11, 107]) # Nov
air.12.col108 <- lm(pp2[pp2$month==12, 108] ~ pp2[pp2$month==12, 97] + pp2[pp2$month==12, 107]) # Dec
summary(air.1.col108)
summary(air.2.col108)
summary(air.3.col108)
summary(air.4.col108)
summary(air.5.col108)
summary(air.6.col108)
summary(air.7.col108)
summary(air.8.col108)
summary(air.9.col108)
summary(air.10.col108)
summary(air.11.col108)
summary(air.12.col108)

# Bind the coefficients together
air.col108 <- data.frame(rbind(coef(air.1.col108),coef(air.2.col108),coef(air.3.col108),coef(air.4.col108),coef(air.5.col108),coef(air.6.col108),
                               coef(air.7.col108),coef(air.8.col108),coef(air.9.col108),coef(air.10.col108),coef(air.11.col108),coef(air.12.col108)))
air.col108 <- cbind(air.col108, rbind(summary(air.1.col108)$adj.r.squared, summary(air.2.col108)$adj.r.squared, summary(air.3.col108)$adj.r.squared,
                                      summary(air.4.col108)$adj.r.squared, summary(air.5.col108)$adj.r.squared, summary(air.6.col108)$adj.r.squared,
                                      summary(air.7.col108)$adj.r.squared, summary(air.8.col108)$adj.r.squared, summary(air.9.col108)$adj.r.squared,
                                      summary(air.10.col108)$adj.r.squared, summary(air.11.col108)$adj.r.squared, summary(air.12.col108)$adj.r.squared))
names(air.col108) <- c("b0","air.col97","air.col107","r2adj")
rownames(air.col108) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPD air.wwBg.5 NAs
pp2$air.pp.0 <- ifelse(pp2$month == 1 & is.na(pp2$air.pp.0), (air.col108[1,1] + air.col108[1,2]*pp2$wsu.Temp.0cm
                                                                + air.col108[1,3]*pp2$air.pp.15), pp2$air.pp.0)
pp2$air.pp.0 <- ifelse(pp2$month == 2 & is.na(pp2$air.pp.0), (air.col108[2,1] + air.col108[2,2]*pp2$wsu.Temp.0cm
                                                                + air.col108[2,3]*pp2$air.pp.15), pp2$air.pp.0)
pp2$air.pp.0 <- ifelse(pp2$month == 3 & is.na(pp2$air.pp.0), (air.col108[3,1] + air.col108[3,2]*pp2$wsu.Temp.0cm
                                                                + air.col108[3,3]*pp2$air.pp.15), pp2$air.pp.0)
pp2$air.pp.0 <- ifelse(pp2$month == 4 & is.na(pp2$air.pp.0), (air.col108[4,1] + air.col108[4,2]*pp2$wsu.Temp.0cm
                                                                + air.col108[4,3]*pp2$air.pp.15), pp2$air.pp.0)
pp2$air.pp.0 <- ifelse(pp2$month == 5 & is.na(pp2$air.pp.0), (air.col108[5,1] + air.col108[5,2]*pp2$wsu.Temp.0cm
                                                                + air.col108[5,3]*pp2$air.pp.15), pp2$air.pp.0)
pp2$air.pp.0 <- ifelse(pp2$month == 6 & is.na(pp2$air.pp.0), (air.col108[6,1] + air.col108[6,2]*pp2$wsu.Temp.0cm
                                                                + air.col108[6,3]*pp2$air.pp.15), pp2$air.pp.0)
pp2$air.pp.0 <- ifelse(pp2$month == 7 & is.na(pp2$air.pp.0), (air.col108[7,1] + air.col108[7,2]*pp2$wsu.Temp.0cm
                                                                + air.col108[7,3]*pp2$air.pp.15), pp2$air.pp.0)
pp2$air.pp.0 <- ifelse(pp2$month == 8 & is.na(pp2$air.pp.0), (air.col108[8,1] + air.col108[8,2]*pp2$wsu.Temp.0cm
                                                                + air.col108[8,3]*pp2$air.pp.15), pp2$air.pp.0)
pp2$air.pp.0 <- ifelse(pp2$month == 9 & is.na(pp2$air.pp.0), (air.col108[9,1] + air.col108[9,2]*pp2$wsu.Temp.0cm
                                                                + air.col108[9,3]*pp2$air.pp.15), pp2$air.pp.0)
pp2$air.pp.0 <- ifelse(pp2$month == 10 & is.na(pp2$air.pp.0), (air.col108[10,1] + air.col108[10,2]*pp2$wsu.Temp.0cm
                                                                 + air.col108[10,3]*pp2$air.pp.15), pp2$air.pp.0)
pp2$air.pp.0 <- ifelse(pp2$month == 11 & is.na(pp2$air.pp.0), (air.col108[11,1] + air.col108[11,2]*pp2$wsu.Temp.0cm
                                                                 + air.col108[11,3]*pp2$air.pp.15), pp2$air.pp.0)
pp2$air.pp.0 <- ifelse(pp2$month == 12 & is.na(pp2$air.pp.0), (air.col108[12,1] + air.col108[12,2]*pp2$wsu.Temp.0cm
                                                                 + air.col108[12,3]*pp2$air.pp.15), pp2$air.pp.0)

plot(pp2$air.pp.0, type = "l", col = "red")
lines(pp$air.pp.0, col = "blue")




################################################
################################################
# pairs(pp[,c(109,51,107,107,108,127)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

plot(pp2[,51], type = "l", col = "red")
lines(pp[,127], col = "blue")
lines(pp[,107], col = "green")

# PPD air temperatures based on PPA and WSU
air.1.col109 <- lm(pp2[pp2$month==1, 109] ~ pp2[pp2$month==1, 107] + pp2[pp2$month==1, 127]) # Jan
air.2.col109 <- lm(pp2[pp2$month==2, 109] ~ pp2[pp2$month==2, 107] + pp2[pp2$month==2, 127]) # Feb
air.3.col109 <- lm(pp2[pp2$month==3, 109] ~ pp2[pp2$month==3, 107] + pp2[pp2$month==3, 127]) # Mar
air.4.col109 <- lm(pp2[pp2$month==4, 109] ~ pp2[pp2$month==4, 107] + pp2[pp2$month==4, 127]) # Apr
air.5.col109 <- lm(pp2[pp2$month==5, 109] ~ pp2[pp2$month==5, 107] + pp2[pp2$month==5, 127]) # May
air.6.col109 <- lm(pp2[pp2$month==6, 109] ~ pp2[pp2$month==6, 107] + pp2[pp2$month==6, 127]) # Jun
air.7.col109 <- lm(pp2[pp2$month==7, 109] ~ pp2[pp2$month==7, 107] + pp2[pp2$month==7, 127]) # Jul
air.8.col109 <- lm(pp2[pp2$month==8, 109] ~ pp2[pp2$month==8, 107] + pp2[pp2$month==8, 127]) # Aug
air.9.col109 <- lm(pp2[pp2$month==9, 109] ~ pp2[pp2$month==9, 107] + pp2[pp2$month==9, 127]) # Sep
air.10.col109 <- lm(pp2[pp2$month==10, 109] ~ pp2[pp2$month==10, 107] + pp2[pp2$month==10, 127]) # Oct
air.11.col109 <- lm(pp2[pp2$month==11, 109] ~ pp2[pp2$month==11, 107] + pp2[pp2$month==11, 127]) # Nov
air.12.col109 <- lm(pp2[pp2$month==12, 109] ~ pp2[pp2$month==12, 107] + pp2[pp2$month==12, 127]) # Dec
summary(air.1.col109)
summary(air.2.col109)
summary(air.3.col109)
summary(air.4.col109)
summary(air.5.col109)
summary(air.6.col109)
summary(air.7.col109)
summary(air.8.col109)
summary(air.9.col109)
summary(air.10.col109)
summary(air.11.col109)
summary(air.12.col109)

# Bind the coefficients together
air.col109 <- data.frame(rbind(coef(air.1.col109),coef(air.2.col109),coef(air.3.col109),coef(air.4.col109),coef(air.5.col109),coef(air.6.col109),
                               coef(air.7.col109),coef(air.8.col109),coef(air.9.col109),coef(air.10.col109),coef(air.11.col109),coef(air.12.col109)))
air.col109 <- cbind(air.col109, rbind(summary(air.1.col109)$adj.r.squared, summary(air.2.col109)$adj.r.squared, summary(air.3.col109)$adj.r.squared,
                                      summary(air.4.col109)$adj.r.squared, summary(air.5.col109)$adj.r.squared, summary(air.6.col109)$adj.r.squared,
                                      summary(air.7.col109)$adj.r.squared, summary(air.8.col109)$adj.r.squared, summary(air.9.col109)$adj.r.squared,
                                      summary(air.10.col109)$adj.r.squared, summary(air.11.col109)$adj.r.squared, summary(air.12.col109)$adj.r.squared))
names(air.col109) <- c("b0","air.col107","air.col127","r2adj")
rownames(air.col109) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPD air.wwBg.5 NAs
pp2$air.pp.neg0.8 <- ifelse(pp2$month == 1 & is.na(pp2$air.pp.neg0.8), (air.col109[1,1] + air.col109[1,2]*pp2$air.pp.15
                                                              + air.col109[1,3]*pp2$fen.neg200), pp2$air.pp.neg0.8)
pp2$air.pp.neg0.8 <- ifelse(pp2$month == 2 & is.na(pp2$air.pp.neg0.8), (air.col109[2,1] + air.col109[2,2]*pp2$air.pp.15
                                                              + air.col109[2,3]*pp2$fen.neg200), pp2$air.pp.neg0.8)
pp2$air.pp.neg0.8 <- ifelse(pp2$month == 3 & is.na(pp2$air.pp.neg0.8), (air.col109[3,1] + air.col109[3,2]*pp2$air.pp.15
                                                              + air.col109[3,3]*pp2$fen.neg200), pp2$air.pp.neg0.8)
pp2$air.pp.neg0.8 <- ifelse(pp2$month == 4 & is.na(pp2$air.pp.neg0.8), (air.col109[4,1] + air.col109[4,2]*pp2$air.pp.15
                                                              + air.col109[4,3]*pp2$fen.neg200), pp2$air.pp.neg0.8)
pp2$air.pp.neg0.8 <- ifelse(pp2$month == 5 & is.na(pp2$air.pp.neg0.8), (air.col109[5,1] + air.col109[5,2]*pp2$air.pp.15
                                                              + air.col109[5,3]*pp2$fen.neg200), pp2$air.pp.neg0.8)
pp2$air.pp.neg0.8 <- ifelse(pp2$month == 6 & is.na(pp2$air.pp.neg0.8), (air.col109[6,1] + air.col109[6,2]*pp2$air.pp.15
                                                              + air.col109[6,3]*pp2$fen.neg200), pp2$air.pp.neg0.8)
pp2$air.pp.neg0.8 <- ifelse(pp2$month == 7 & is.na(pp2$air.pp.neg0.8), (air.col109[7,1] + air.col109[7,2]*pp2$air.pp.15
                                                              + air.col109[7,3]*pp2$fen.neg200), pp2$air.pp.neg0.8)
pp2$air.pp.neg0.8 <- ifelse(pp2$month == 8 & is.na(pp2$air.pp.neg0.8), (air.col109[8,1] + air.col109[8,2]*pp2$air.pp.15
                                                              + air.col109[8,3]*pp2$fen.neg200), pp2$air.pp.neg0.8)
pp2$air.pp.neg0.8 <- ifelse(pp2$month == 9 & is.na(pp2$air.pp.neg0.8), (air.col109[9,1] + air.col109[9,2]*pp2$air.pp.15
                                                              + air.col109[9,3]*pp2$fen.neg200), pp2$air.pp.neg0.8)
pp2$air.pp.neg0.8 <- ifelse(pp2$month == 10 & is.na(pp2$air.pp.neg0.8), (air.col109[10,1] + air.col109[10,2]*pp2$air.pp.15
                                                               + air.col109[10,3]*pp2$fen.neg200), pp2$air.pp.neg0.8)
pp2$air.pp.neg0.8 <- ifelse(pp2$month == 11 & is.na(pp2$air.pp.neg0.8), (air.col109[11,1] + air.col109[11,2]*pp2$air.pp.15
                                                               + air.col109[11,3]*pp2$fen.neg200), pp2$air.pp.neg0.8)
pp2$air.pp.neg0.8 <- ifelse(pp2$month == 12 & is.na(pp2$air.pp.neg0.8), (air.col109[12,1] + air.col109[12,2]*pp2$air.pp.15
                                                               + air.col109[12,3]*pp2$fen.neg200), pp2$air.pp.neg0.8)

plot(pp2$air.pp.neg0.8, type = "l", col = "red")
lines(pp$air.pp.neg0.8, col = "blue")





################################################
################################################
# pairs(pp[,c(110,107:109)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPD air temperatures based on PPA and WSU
air.1.col110 <- lm(pp2[pp2$month==1, 110] ~ pp2[pp2$month==1, 107] + pp2[pp2$month==1, 109]) # Jan
air.2.col110 <- lm(pp2[pp2$month==2, 110] ~ pp2[pp2$month==2, 107] + pp2[pp2$month==2, 109]) # Feb
air.3.col110 <- lm(pp2[pp2$month==3, 110] ~ pp2[pp2$month==3, 107] + pp2[pp2$month==3, 109]) # Mar
air.4.col110 <- lm(pp2[pp2$month==4, 110] ~ pp2[pp2$month==4, 107] + pp2[pp2$month==4, 109]) # Apr
air.5.col110 <- lm(pp2[pp2$month==5, 110] ~ pp2[pp2$month==5, 107] + pp2[pp2$month==5, 109]) # May
air.6.col110 <- lm(pp2[pp2$month==6, 110] ~ pp2[pp2$month==6, 107] + pp2[pp2$month==6, 109]) # Jun
air.7.col110 <- lm(pp2[pp2$month==7, 110] ~ pp2[pp2$month==7, 107] + pp2[pp2$month==7, 109]) # Jul
air.8.col110 <- lm(pp2[pp2$month==8, 110] ~ pp2[pp2$month==8, 107] + pp2[pp2$month==8, 109]) # Aug
air.9.col110 <- lm(pp2[pp2$month==9, 110] ~ pp2[pp2$month==9, 107] + pp2[pp2$month==9, 109]) # Sep
air.10.col110 <- lm(pp2[pp2$month==10, 110] ~ pp2[pp2$month==10, 107] + pp2[pp2$month==10, 109]) # Oct
air.11.col110 <- lm(pp2[pp2$month==11, 110] ~ pp2[pp2$month==11, 107] + pp2[pp2$month==11, 109]) # Nov
air.12.col110 <- lm(pp2[pp2$month==12, 110] ~ pp2[pp2$month==12, 107] + pp2[pp2$month==12, 109]) # Dec
summary(air.1.col110)
summary(air.2.col110)
summary(air.3.col110)
summary(air.4.col110)
summary(air.5.col110)
summary(air.6.col110)
summary(air.7.col110)
summary(air.8.col110)
summary(air.9.col110)
summary(air.10.col110)
summary(air.11.col110)
summary(air.12.col110)

# Bind the coefficients together
air.col110 <- data.frame(rbind(coef(air.1.col110),coef(air.2.col110),coef(air.3.col110),coef(air.4.col110),coef(air.5.col110),coef(air.6.col110),
                               coef(air.7.col110),coef(air.8.col110),coef(air.9.col110),coef(air.10.col110),coef(air.11.col110),coef(air.12.col110)))
air.col110 <- cbind(air.col110, rbind(summary(air.1.col110)$adj.r.squared, summary(air.2.col110)$adj.r.squared, summary(air.3.col110)$adj.r.squared,
                                      summary(air.4.col110)$adj.r.squared, summary(air.5.col110)$adj.r.squared, summary(air.6.col110)$adj.r.squared,
                                      summary(air.7.col110)$adj.r.squared, summary(air.8.col110)$adj.r.squared, summary(air.9.col110)$adj.r.squared,
                                      summary(air.10.col110)$adj.r.squared, summary(air.11.col110)$adj.r.squared, summary(air.12.col110)$adj.r.squared))
names(air.col110) <- c("b0","air.col107","air.col109","r2adj")
rownames(air.col110) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPD air.wwBg.5 NAs
pp2$air.pp.neg1.5 <- ifelse(pp2$month == 1 & is.na(pp2$air.pp.neg1.5), (air.col110[1,1] + air.col110[1,2]*pp2$air.pp.15
                                                                        + air.col110[1,3]*pp2$air.pp.neg0.8), pp2$air.pp.neg1.5)
pp2$air.pp.neg1.5 <- ifelse(pp2$month == 2 & is.na(pp2$air.pp.neg1.5), (air.col110[2,1] + air.col110[2,2]*pp2$air.pp.15
                                                                        + air.col110[2,3]*pp2$air.pp.neg0.8), pp2$air.pp.neg1.5)
pp2$air.pp.neg1.5 <- ifelse(pp2$month == 3 & is.na(pp2$air.pp.neg1.5), (air.col110[3,1] + air.col110[3,2]*pp2$air.pp.15
                                                                        + air.col110[3,3]*pp2$air.pp.neg0.8), pp2$air.pp.neg1.5)
pp2$air.pp.neg1.5 <- ifelse(pp2$month == 4 & is.na(pp2$air.pp.neg1.5), (air.col110[4,1] + air.col110[4,2]*pp2$air.pp.15
                                                                        + air.col110[4,3]*pp2$air.pp.neg0.8), pp2$air.pp.neg1.5)
pp2$air.pp.neg1.5 <- ifelse(pp2$month == 5 & is.na(pp2$air.pp.neg1.5), (air.col110[5,1] + air.col110[5,2]*pp2$air.pp.15
                                                                        + air.col110[5,3]*pp2$air.pp.neg0.8), pp2$air.pp.neg1.5)
pp2$air.pp.neg1.5 <- ifelse(pp2$month == 6 & is.na(pp2$air.pp.neg1.5), (air.col110[6,1] + air.col110[6,2]*pp2$air.pp.15
                                                                        + air.col110[6,3]*pp2$air.pp.neg0.8), pp2$air.pp.neg1.5)
pp2$air.pp.neg1.5 <- ifelse(pp2$month == 7 & is.na(pp2$air.pp.neg1.5), (air.col110[7,1] + air.col110[7,2]*pp2$air.pp.15
                                                                        + air.col110[7,3]*pp2$air.pp.neg0.8), pp2$air.pp.neg1.5)
pp2$air.pp.neg1.5 <- ifelse(pp2$month == 8 & is.na(pp2$air.pp.neg1.5), (air.col110[8,1] + air.col110[8,2]*pp2$air.pp.15
                                                                        + air.col110[8,3]*pp2$air.pp.neg0.8), pp2$air.pp.neg1.5)
pp2$air.pp.neg1.5 <- ifelse(pp2$month == 9 & is.na(pp2$air.pp.neg1.5), (air.col110[9,1] + air.col110[9,2]*pp2$air.pp.15
                                                                        + air.col110[9,3]*pp2$air.pp.neg0.8), pp2$air.pp.neg1.5)
pp2$air.pp.neg1.5 <- ifelse(pp2$month == 10 & is.na(pp2$air.pp.neg1.5), (air.col110[10,1] + air.col110[10,2]*pp2$air.pp.15
                                                                         + air.col110[10,3]*pp2$air.pp.neg0.8), pp2$air.pp.neg1.5)
pp2$air.pp.neg1.5 <- ifelse(pp2$month == 11 & is.na(pp2$air.pp.neg1.5), (air.col110[11,1] + air.col110[11,2]*pp2$air.pp.15
                                                                         + air.col110[11,3]*pp2$air.pp.neg0.8), pp2$air.pp.neg1.5)
pp2$air.pp.neg1.5 <- ifelse(pp2$month == 12 & is.na(pp2$air.pp.neg1.5), (air.col110[12,1] + air.col110[12,2]*pp2$air.pp.15
                                                                         + air.col110[12,3]*pp2$air.pp.neg0.8), pp2$air.pp.neg1.5)

plot(pp2$air.pp.neg1.5, type = "l", col = "red")
lines(pp$air.pp.neg1.5, col = "blue")





################################################
################################################
# pairs(pp[,c(111,109,110)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPD air temperatures based on PPA and WSU
air.1.col111 <- lm(pp2[pp2$month==1, 111] ~ pp2[pp2$month==1, 109] + pp2[pp2$month==1, 110]) # Jan
air.2.col111 <- lm(pp2[pp2$month==2, 111] ~ pp2[pp2$month==2, 109] + pp2[pp2$month==2, 110]) # Feb
air.3.col111 <- lm(pp2[pp2$month==3, 111] ~ pp2[pp2$month==3, 109] + pp2[pp2$month==3, 110]) # Mar
air.4.col111 <- lm(pp2[pp2$month==4, 111] ~ pp2[pp2$month==4, 109] + pp2[pp2$month==4, 110]) # Apr
air.5.col111 <- lm(pp2[pp2$month==5, 111] ~ pp2[pp2$month==5, 109] + pp2[pp2$month==5, 110]) # May
air.6.col111 <- lm(pp2[pp2$month==6, 111] ~ pp2[pp2$month==6, 109] + pp2[pp2$month==6, 110]) # Jun
air.7.col111 <- lm(pp2[pp2$month==7, 111] ~ pp2[pp2$month==7, 109] + pp2[pp2$month==7, 110]) # Jul
air.8.col111 <- lm(pp2[pp2$month==8, 111] ~ pp2[pp2$month==8, 109] + pp2[pp2$month==8, 110]) # Aug
air.9.col111 <- lm(pp2[pp2$month==9, 111] ~ pp2[pp2$month==9, 109] + pp2[pp2$month==9, 110]) # Sep
air.10.col111 <- lm(pp2[pp2$month==10, 111] ~ pp2[pp2$month==10, 109] + pp2[pp2$month==10, 110]) # Oct
air.11.col111 <- lm(pp2[pp2$month==11, 111] ~ pp2[pp2$month==11, 109] + pp2[pp2$month==11, 110]) # Nov
air.12.col111 <- lm(pp2[pp2$month==12, 111] ~ pp2[pp2$month==12, 109] + pp2[pp2$month==12, 110]) # Dec
summary(air.1.col111)
summary(air.2.col111)
summary(air.3.col111)
summary(air.4.col111)
summary(air.5.col111)
summary(air.6.col111)
summary(air.7.col111)
summary(air.8.col111)
summary(air.9.col111)
summary(air.10.col111)
summary(air.11.col111)
summary(air.12.col111)

# Bind the coefficients together
air.col111 <- data.frame(rbind(coef(air.1.col111),coef(air.2.col111),coef(air.3.col111),coef(air.4.col111),coef(air.5.col111),coef(air.6.col111),
                               coef(air.7.col111),coef(air.8.col111),coef(air.9.col111),coef(air.10.col111),coef(air.11.col111),coef(air.12.col111)))
air.col111 <- cbind(air.col111, rbind(summary(air.1.col111)$adj.r.squared, summary(air.2.col111)$adj.r.squared, summary(air.3.col111)$adj.r.squared,
                                      summary(air.4.col111)$adj.r.squared, summary(air.5.col111)$adj.r.squared, summary(air.6.col111)$adj.r.squared,
                                      summary(air.7.col111)$adj.r.squared, summary(air.8.col111)$adj.r.squared, summary(air.9.col111)$adj.r.squared,
                                      summary(air.10.col111)$adj.r.squared, summary(air.11.col111)$adj.r.squared, summary(air.12.col111)$adj.r.squared))
names(air.col111) <- c("b0","air.col109","air.col110","r2adj")
rownames(air.col111) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPD air.wwBg.5 NAs
pp2$air.pp.neg2.3 <- ifelse(pp2$month == 1 & is.na(pp2$air.pp.neg2.3), (air.col111[1,1] + air.col111[1,2]*pp2$air.pp.neg0.8
                                                                        + air.col111[1,3]*pp2$air.pp.neg1.5), pp2$air.pp.neg2.3)
pp2$air.pp.neg2.3 <- ifelse(pp2$month == 2 & is.na(pp2$air.pp.neg2.3), (air.col111[2,1] + air.col111[2,2]*pp2$air.pp.neg0.8
                                                                        + air.col111[2,3]*pp2$air.pp.neg1.5), pp2$air.pp.neg2.3)
pp2$air.pp.neg2.3 <- ifelse(pp2$month == 3 & is.na(pp2$air.pp.neg2.3), (air.col111[3,1] + air.col111[3,2]*pp2$air.pp.neg0.8
                                                                        + air.col111[3,3]*pp2$air.pp.neg1.5), pp2$air.pp.neg2.3)
pp2$air.pp.neg2.3 <- ifelse(pp2$month == 4 & is.na(pp2$air.pp.neg2.3), (air.col111[4,1] + air.col111[4,2]*pp2$air.pp.neg0.8
                                                                        + air.col111[4,3]*pp2$air.pp.neg1.5), pp2$air.pp.neg2.3)
pp2$air.pp.neg2.3 <- ifelse(pp2$month == 5 & is.na(pp2$air.pp.neg2.3), (air.col111[5,1] + air.col111[5,2]*pp2$air.pp.neg0.8
                                                                        + air.col111[5,3]*pp2$air.pp.neg1.5), pp2$air.pp.neg2.3)
pp2$air.pp.neg2.3 <- ifelse(pp2$month == 6 & is.na(pp2$air.pp.neg2.3), (air.col111[6,1] + air.col111[6,2]*pp2$air.pp.neg0.8
                                                                        + air.col111[6,3]*pp2$air.pp.neg1.5), pp2$air.pp.neg2.3)
pp2$air.pp.neg2.3 <- ifelse(pp2$month == 7 & is.na(pp2$air.pp.neg2.3), (air.col111[7,1] + air.col111[7,2]*pp2$air.pp.neg0.8
                                                                        + air.col111[7,3]*pp2$air.pp.neg1.5), pp2$air.pp.neg2.3)
pp2$air.pp.neg2.3 <- ifelse(pp2$month == 8 & is.na(pp2$air.pp.neg2.3), (air.col111[8,1] + air.col111[8,2]*pp2$air.pp.neg0.8
                                                                        + air.col111[8,3]*pp2$air.pp.neg1.5), pp2$air.pp.neg2.3)
pp2$air.pp.neg2.3 <- ifelse(pp2$month == 9 & is.na(pp2$air.pp.neg2.3), (air.col111[9,1] + air.col111[9,2]*pp2$air.pp.neg0.8
                                                                        + air.col111[9,3]*pp2$air.pp.neg1.5), pp2$air.pp.neg2.3)
pp2$air.pp.neg2.3 <- ifelse(pp2$month == 10 & is.na(pp2$air.pp.neg2.3), (air.col111[10,1] + air.col111[10,2]*pp2$air.pp.neg0.8
                                                                         + air.col111[10,3]*pp2$air.pp.neg1.5), pp2$air.pp.neg2.3)
pp2$air.pp.neg2.3 <- ifelse(pp2$month == 11 & is.na(pp2$air.pp.neg2.3), (air.col111[11,1] + air.col111[11,2]*pp2$air.pp.neg0.8
                                                                         + air.col111[11,3]*pp2$air.pp.neg1.5), pp2$air.pp.neg2.3)
pp2$air.pp.neg2.3 <- ifelse(pp2$month == 12 & is.na(pp2$air.pp.neg2.3), (air.col111[12,1] + air.col111[12,2]*pp2$air.pp.neg0.8
                                                                         + air.col111[12,3]*pp2$air.pp.neg1.5), pp2$air.pp.neg2.3)

plot(pp2$air.pp.neg2.3, type = "l", col = "red")
lines(pp$air.pp.neg2.3, col = "blue")





################################################
################################################
# pairs(pp[,c(112,109:111)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPD air temperatures based on PPA and WSU
air.1.col112 <- lm(pp2[pp2$month==1, 112] ~ pp2[pp2$month==1, 110] + pp2[pp2$month==1, 111]) # Jan
air.2.col112 <- lm(pp2[pp2$month==2, 112] ~ pp2[pp2$month==2, 110] + pp2[pp2$month==2, 111]) # Feb
air.3.col112 <- lm(pp2[pp2$month==3, 112] ~ pp2[pp2$month==3, 110] + pp2[pp2$month==3, 111]) # Mar
air.4.col112 <- lm(pp2[pp2$month==4, 112] ~ pp2[pp2$month==4, 110] + pp2[pp2$month==4, 111]) # Apr
air.5.col112 <- lm(pp2[pp2$month==5, 112] ~ pp2[pp2$month==5, 110] + pp2[pp2$month==5, 111]) # May
air.6.col112 <- lm(pp2[pp2$month==6, 112] ~ pp2[pp2$month==6, 110] + pp2[pp2$month==6, 111]) # Jun
air.7.col112 <- lm(pp2[pp2$month==7, 112] ~ pp2[pp2$month==7, 110] + pp2[pp2$month==7, 111]) # Jul
air.8.col112 <- lm(pp2[pp2$month==8, 112] ~ pp2[pp2$month==8, 110] + pp2[pp2$month==8, 111]) # Aug
air.9.col112 <- lm(pp2[pp2$month==9, 112] ~ pp2[pp2$month==9, 110] + pp2[pp2$month==9, 111]) # Sep
air.10.col112 <- lm(pp2[pp2$month==10, 112] ~ pp2[pp2$month==10, 110] + pp2[pp2$month==10, 111]) # Oct
air.11.col112 <- lm(pp2[pp2$month==11, 112] ~ pp2[pp2$month==11, 110] + pp2[pp2$month==11, 111]) # Nov
air.12.col112 <- lm(pp2[pp2$month==12, 112] ~ pp2[pp2$month==12, 110] + pp2[pp2$month==12, 111]) # Dec
summary(air.1.col112)
summary(air.2.col112)
summary(air.3.col112)
summary(air.4.col112)
summary(air.5.col112)
summary(air.6.col112)
summary(air.7.col112)
summary(air.8.col112)
summary(air.9.col112)
summary(air.10.col112)
summary(air.11.col112)
summary(air.12.col112)

# Bind the coefficients together
air.col112 <- data.frame(rbind(coef(air.1.col112),coef(air.2.col112),coef(air.3.col112),coef(air.4.col112),coef(air.5.col112),coef(air.6.col112),
                               coef(air.7.col112),coef(air.8.col112),coef(air.9.col112),coef(air.10.col112),coef(air.11.col112),coef(air.12.col112)))
air.col112 <- cbind(air.col112, rbind(summary(air.1.col112)$adj.r.squared, summary(air.2.col112)$adj.r.squared, summary(air.3.col112)$adj.r.squared,
                                      summary(air.4.col112)$adj.r.squared, summary(air.5.col112)$adj.r.squared, summary(air.6.col112)$adj.r.squared,
                                      summary(air.7.col112)$adj.r.squared, summary(air.8.col112)$adj.r.squared, summary(air.9.col112)$adj.r.squared,
                                      summary(air.10.col112)$adj.r.squared, summary(air.11.col112)$adj.r.squared, summary(air.12.col112)$adj.r.squared))
names(air.col112) <- c("b0","air.col110","air.col111","r2adj")
rownames(air.col112) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPD air.wwBg.5 NAs
pp2$air.pp.neg3.0 <- ifelse(pp2$month == 1 & is.na(pp2$air.pp.neg3.0), (air.col112[1,1] + air.col112[1,2]*pp2$air.pp.neg1.5
                                                                        + air.col112[1,3]*pp2$air.pp.neg2.3), pp2$air.pp.neg3.0)
pp2$air.pp.neg3.0 <- ifelse(pp2$month == 2 & is.na(pp2$air.pp.neg3.0), (air.col112[2,1] + air.col112[2,2]*pp2$air.pp.neg1.5
                                                                        + air.col112[2,3]*pp2$air.pp.neg2.3), pp2$air.pp.neg3.0)
pp2$air.pp.neg3.0 <- ifelse(pp2$month == 3 & is.na(pp2$air.pp.neg3.0), (air.col112[3,1] + air.col112[3,2]*pp2$air.pp.neg1.5
                                                                        + air.col112[3,3]*pp2$air.pp.neg2.3), pp2$air.pp.neg3.0)
pp2$air.pp.neg3.0 <- ifelse(pp2$month == 4 & is.na(pp2$air.pp.neg3.0), (air.col112[4,1] + air.col112[4,2]*pp2$air.pp.neg1.5
                                                                        + air.col112[4,3]*pp2$air.pp.neg2.3), pp2$air.pp.neg3.0)
pp2$air.pp.neg3.0 <- ifelse(pp2$month == 5 & is.na(pp2$air.pp.neg3.0), (air.col112[5,1] + air.col112[5,2]*pp2$air.pp.neg1.5
                                                                        + air.col112[5,3]*pp2$air.pp.neg2.3), pp2$air.pp.neg3.0)
pp2$air.pp.neg3.0 <- ifelse(pp2$month == 6 & is.na(pp2$air.pp.neg3.0), (air.col112[6,1] + air.col112[6,2]*pp2$air.pp.neg1.5
                                                                        + air.col112[6,3]*pp2$air.pp.neg2.3), pp2$air.pp.neg3.0)
pp2$air.pp.neg3.0 <- ifelse(pp2$month == 7 & is.na(pp2$air.pp.neg3.0), (air.col112[7,1] + air.col112[7,2]*pp2$air.pp.neg1.5
                                                                        + air.col112[7,3]*pp2$air.pp.neg2.3), pp2$air.pp.neg3.0)
pp2$air.pp.neg3.0 <- ifelse(pp2$month == 8 & is.na(pp2$air.pp.neg3.0), (air.col112[8,1] + air.col112[8,2]*pp2$air.pp.neg1.5
                                                                        + air.col112[8,3]*pp2$air.pp.neg2.3), pp2$air.pp.neg3.0)
pp2$air.pp.neg3.0 <- ifelse(pp2$month == 9 & is.na(pp2$air.pp.neg3.0), (air.col112[9,1] + air.col112[9,2]*pp2$air.pp.neg1.5
                                                                        + air.col112[9,3]*pp2$air.pp.neg2.3), pp2$air.pp.neg3.0)
pp2$air.pp.neg3.0 <- ifelse(pp2$month == 10 & is.na(pp2$air.pp.neg3.0), (air.col112[10,1] + air.col112[10,2]*pp2$air.pp.neg1.5
                                                                         + air.col112[10,3]*pp2$air.pp.neg2.3), pp2$air.pp.neg3.0)
pp2$air.pp.neg3.0 <- ifelse(pp2$month == 11 & is.na(pp2$air.pp.neg3.0), (air.col112[11,1] + air.col112[11,2]*pp2$air.pp.neg1.5
                                                                         + air.col112[11,3]*pp2$air.pp.neg2.3), pp2$air.pp.neg3.0)
pp2$air.pp.neg3.0 <- ifelse(pp2$month == 12 & is.na(pp2$air.pp.neg3.0), (air.col112[12,1] + air.col112[12,2]*pp2$air.pp.neg1.5
                                                                         + air.col112[12,3]*pp2$air.pp.neg2.3), pp2$air.pp.neg3.0)

plot(pp2$air.pp.neg3.0, type = "l", col = "red")
lines(pp$air.pp.neg3.0, col = "blue")





################################################
################################################
# pairs(pp[,c(113,111,112)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPD air temperatures based on PPA and WSU
air.1.col113 <- lm(pp2[pp2$month==1, 113] ~ pp2[pp2$month==1, 111] + pp2[pp2$month==1, 112]) # Jan
air.2.col113 <- lm(pp2[pp2$month==2, 113] ~ pp2[pp2$month==2, 111] + pp2[pp2$month==2, 112]) # Feb
air.3.col113 <- lm(pp2[pp2$month==3, 113] ~ pp2[pp2$month==3, 111] + pp2[pp2$month==3, 112]) # Mar
air.4.col113 <- lm(pp2[pp2$month==4, 113] ~ pp2[pp2$month==4, 111] + pp2[pp2$month==4, 112]) # Apr
air.5.col113 <- lm(pp2[pp2$month==5, 113] ~ pp2[pp2$month==5, 111] + pp2[pp2$month==5, 112]) # May
air.6.col113 <- lm(pp2[pp2$month==6, 113] ~ pp2[pp2$month==6, 111] + pp2[pp2$month==6, 112]) # Jun
air.7.col113 <- lm(pp2[pp2$month==7, 113] ~ pp2[pp2$month==7, 111] + pp2[pp2$month==7, 112]) # Jul
air.8.col113 <- lm(pp2[pp2$month==8, 113] ~ pp2[pp2$month==8, 111] + pp2[pp2$month==8, 112]) # Aug
air.9.col113 <- lm(pp2[pp2$month==9, 113] ~ pp2[pp2$month==9, 111] + pp2[pp2$month==9, 112]) # Sep
air.10.col113 <- lm(pp2[pp2$month==10, 113] ~ pp2[pp2$month==10, 111] + pp2[pp2$month==10, 112]) # Oct
air.11.col113 <- lm(pp2[pp2$month==11, 113] ~ pp2[pp2$month==11, 111] + pp2[pp2$month==11, 112]) # Nov
air.12.col113 <- lm(pp2[pp2$month==12, 113] ~ pp2[pp2$month==12, 111] + pp2[pp2$month==12, 112]) # Dec
summary(air.1.col113)
summary(air.2.col113)
summary(air.3.col113)
summary(air.4.col113)
summary(air.5.col113)
summary(air.6.col113)
summary(air.7.col113)
summary(air.8.col113)
summary(air.9.col113)
summary(air.10.col113)
summary(air.11.col113)
summary(air.12.col113)

# Bind the coefficients together
air.col113 <- data.frame(rbind(coef(air.1.col113),coef(air.2.col113),coef(air.3.col113),coef(air.4.col113),coef(air.5.col113),coef(air.6.col113),
                               coef(air.7.col113),coef(air.8.col113),coef(air.9.col113),coef(air.10.col113),coef(air.11.col113),coef(air.12.col113)))
air.col113 <- cbind(air.col113, rbind(summary(air.1.col113)$adj.r.squared, summary(air.2.col113)$adj.r.squared, summary(air.3.col113)$adj.r.squared,
                                      summary(air.4.col113)$adj.r.squared, summary(air.5.col113)$adj.r.squared, summary(air.6.col113)$adj.r.squared,
                                      summary(air.7.col113)$adj.r.squared, summary(air.8.col113)$adj.r.squared, summary(air.9.col113)$adj.r.squared,
                                      summary(air.10.col113)$adj.r.squared, summary(air.11.col113)$adj.r.squared, summary(air.12.col113)$adj.r.squared))
names(air.col113) <- c("b0","air.col111","air.col112","r2adj")
rownames(air.col113) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPD air.wwBg.5 NAs
pp2$air.pp.neg6.1 <- ifelse(pp2$month == 1 & is.na(pp2$air.pp.neg6.1), (air.col113[1,1] + air.col113[1,2]*pp2$air.pp.neg2.3
                                                                        + air.col113[1,3]*pp2$air.pp.neg3.0), pp2$air.pp.neg6.1)
pp2$air.pp.neg6.1 <- ifelse(pp2$month == 2 & is.na(pp2$air.pp.neg6.1), (air.col113[2,1] + air.col113[2,2]*pp2$air.pp.neg2.3
                                                                        + air.col113[2,3]*pp2$air.pp.neg3.0), pp2$air.pp.neg6.1)
pp2$air.pp.neg6.1 <- ifelse(pp2$month == 3 & is.na(pp2$air.pp.neg6.1), (air.col113[3,1] + air.col113[3,2]*pp2$air.pp.neg2.3
                                                                        + air.col113[3,3]*pp2$air.pp.neg3.0), pp2$air.pp.neg6.1)
pp2$air.pp.neg6.1 <- ifelse(pp2$month == 4 & is.na(pp2$air.pp.neg6.1), (air.col113[4,1] + air.col113[4,2]*pp2$air.pp.neg2.3
                                                                        + air.col113[4,3]*pp2$air.pp.neg3.0), pp2$air.pp.neg6.1)
pp2$air.pp.neg6.1 <- ifelse(pp2$month == 5 & is.na(pp2$air.pp.neg6.1), (air.col113[5,1] + air.col113[5,2]*pp2$air.pp.neg2.3
                                                                        + air.col113[5,3]*pp2$air.pp.neg3.0), pp2$air.pp.neg6.1)
pp2$air.pp.neg6.1 <- ifelse(pp2$month == 6 & is.na(pp2$air.pp.neg6.1), (air.col113[6,1] + air.col113[6,2]*pp2$air.pp.neg2.3
                                                                        + air.col113[6,3]*pp2$air.pp.neg3.0), pp2$air.pp.neg6.1)
pp2$air.pp.neg6.1 <- ifelse(pp2$month == 7 & is.na(pp2$air.pp.neg6.1), (air.col113[7,1] + air.col113[7,2]*pp2$air.pp.neg2.3
                                                                        + air.col113[7,3]*pp2$air.pp.neg3.0), pp2$air.pp.neg6.1)
pp2$air.pp.neg6.1 <- ifelse(pp2$month == 8 & is.na(pp2$air.pp.neg6.1), (air.col113[8,1] + air.col113[8,2]*pp2$air.pp.neg2.3
                                                                        + air.col113[8,3]*pp2$air.pp.neg3.0), pp2$air.pp.neg6.1)
pp2$air.pp.neg6.1 <- ifelse(pp2$month == 9 & is.na(pp2$air.pp.neg6.1), (air.col113[9,1] + air.col113[9,2]*pp2$air.pp.neg2.3
                                                                        + air.col113[9,3]*pp2$air.pp.neg3.0), pp2$air.pp.neg6.1)
pp2$air.pp.neg6.1 <- ifelse(pp2$month == 10 & is.na(pp2$air.pp.neg6.1), (air.col113[10,1] + air.col113[10,2]*pp2$air.pp.neg2.3
                                                                         + air.col113[10,3]*pp2$air.pp.neg3.0), pp2$air.pp.neg6.1)
pp2$air.pp.neg6.1 <- ifelse(pp2$month == 11 & is.na(pp2$air.pp.neg6.1), (air.col113[11,1] + air.col113[11,2]*pp2$air.pp.neg2.3
                                                                         + air.col113[11,3]*pp2$air.pp.neg3.0), pp2$air.pp.neg6.1)
pp2$air.pp.neg6.1 <- ifelse(pp2$month == 12 & is.na(pp2$air.pp.neg6.1), (air.col113[12,1] + air.col113[12,2]*pp2$air.pp.neg2.3
                                                                         + air.col113[12,3]*pp2$air.pp.neg3.0), pp2$air.pp.neg6.1)

plot(pp2$air.pp.neg6.1, type = "l", col = "red")
lines(pp$air.pp.neg6.1, col = "blue")







################################################
################################################
# pairs(pp[,c(114,112,113)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPD air temperatures based on PPA and WSU
air.1.col114 <- lm(pp2[pp2$month==1, 114] ~ pp2[pp2$month==1, 112] + pp2[pp2$month==1, 113]) # Jan
air.2.col114 <- lm(pp2[pp2$month==2, 114] ~ pp2[pp2$month==2, 112] + pp2[pp2$month==2, 113]) # Feb
air.3.col114 <- lm(pp2[pp2$month==3, 114] ~ pp2[pp2$month==3, 112] + pp2[pp2$month==3, 113]) # Mar
air.4.col114 <- lm(pp2[pp2$month==4, 114] ~ pp2[pp2$month==4, 112] + pp2[pp2$month==4, 113]) # Apr
air.5.col114 <- lm(pp2[pp2$month==5, 114] ~ pp2[pp2$month==5, 112] + pp2[pp2$month==5, 113]) # May
air.6.col114 <- lm(pp2[pp2$month==6, 114] ~ pp2[pp2$month==6, 112] + pp2[pp2$month==6, 113]) # Jun
air.7.col114 <- lm(pp2[pp2$month==7, 114] ~ pp2[pp2$month==7, 112] + pp2[pp2$month==7, 113]) # Jul
air.8.col114 <- lm(pp2[pp2$month==8, 114] ~ pp2[pp2$month==8, 112] + pp2[pp2$month==8, 113]) # Aug
air.9.col114 <- lm(pp2[pp2$month==9, 114] ~ pp2[pp2$month==9, 112] + pp2[pp2$month==9, 113]) # Sep
air.10.col114 <- lm(pp2[pp2$month==10, 114] ~ pp2[pp2$month==10, 112] + pp2[pp2$month==10, 113]) # Oct
air.11.col114 <- lm(pp2[pp2$month==11, 114] ~ pp2[pp2$month==11, 112] + pp2[pp2$month==11, 113]) # Nov
air.12.col114 <- lm(pp2[pp2$month==12, 114] ~ pp2[pp2$month==12, 112] + pp2[pp2$month==12, 113]) # Dec
summary(air.1.col114)
summary(air.2.col114)
summary(air.3.col114)
summary(air.4.col114)
summary(air.5.col114)
summary(air.6.col114)
summary(air.7.col114)
summary(air.8.col114)
summary(air.9.col114)
summary(air.10.col114)
summary(air.11.col114)
summary(air.12.col114)

# Bind the coefficients together
air.col114 <- data.frame(rbind(coef(air.1.col114),coef(air.2.col114),coef(air.3.col114),coef(air.4.col114),coef(air.5.col114),coef(air.6.col114),
                               coef(air.7.col114),coef(air.8.col114),coef(air.9.col114),coef(air.10.col114),coef(air.11.col114),coef(air.12.col114)))
air.col114 <- cbind(air.col114, rbind(summary(air.1.col114)$adj.r.squared, summary(air.2.col114)$adj.r.squared, summary(air.3.col114)$adj.r.squared,
                                      summary(air.4.col114)$adj.r.squared, summary(air.5.col114)$adj.r.squared, summary(air.6.col114)$adj.r.squared,
                                      summary(air.7.col114)$adj.r.squared, summary(air.8.col114)$adj.r.squared, summary(air.9.col114)$adj.r.squared,
                                      summary(air.10.col114)$adj.r.squared, summary(air.11.col114)$adj.r.squared, summary(air.12.col114)$adj.r.squared))
names(air.col114) <- c("b0","air.col112","air.col113","r2adj")
rownames(air.col114) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPD air.wwBg.5 NAs
pp2$air.pp.neg9.1 <- ifelse(pp2$month == 1 & is.na(pp2$air.pp.neg9.1), (air.col114[1,1] + air.col114[1,2]*pp2$air.pp.neg3.0
                                                                        + air.col114[1,3]*pp2$air.pp.neg6.1), pp2$air.pp.neg9.1)
pp2$air.pp.neg9.1 <- ifelse(pp2$month == 2 & is.na(pp2$air.pp.neg9.1), (air.col114[2,1] + air.col114[2,2]*pp2$air.pp.neg3.0
                                                                        + air.col114[2,3]*pp2$air.pp.neg6.1), pp2$air.pp.neg9.1)
pp2$air.pp.neg9.1 <- ifelse(pp2$month == 3 & is.na(pp2$air.pp.neg9.1), (air.col114[3,1] + air.col114[3,2]*pp2$air.pp.neg3.0
                                                                        + air.col114[3,3]*pp2$air.pp.neg6.1), pp2$air.pp.neg9.1)
pp2$air.pp.neg9.1 <- ifelse(pp2$month == 4 & is.na(pp2$air.pp.neg9.1), (air.col114[4,1] + air.col114[4,2]*pp2$air.pp.neg3.0
                                                                        + air.col114[4,3]*pp2$air.pp.neg6.1), pp2$air.pp.neg9.1)
pp2$air.pp.neg9.1 <- ifelse(pp2$month == 5 & is.na(pp2$air.pp.neg9.1), (air.col114[5,1] + air.col114[5,2]*pp2$air.pp.neg3.0
                                                                        + air.col114[5,3]*pp2$air.pp.neg6.1), pp2$air.pp.neg9.1)
pp2$air.pp.neg9.1 <- ifelse(pp2$month == 6 & is.na(pp2$air.pp.neg9.1), (air.col114[6,1] + air.col114[6,2]*pp2$air.pp.neg3.0
                                                                        + air.col114[6,3]*pp2$air.pp.neg6.1), pp2$air.pp.neg9.1)
pp2$air.pp.neg9.1 <- ifelse(pp2$month == 7 & is.na(pp2$air.pp.neg9.1), (air.col114[7,1] + air.col114[7,2]*pp2$air.pp.neg3.0
                                                                        + air.col114[7,3]*pp2$air.pp.neg6.1), pp2$air.pp.neg9.1)
pp2$air.pp.neg9.1 <- ifelse(pp2$month == 8 & is.na(pp2$air.pp.neg9.1), (air.col114[8,1] + air.col114[8,2]*pp2$air.pp.neg3.0
                                                                        + air.col114[8,3]*pp2$air.pp.neg6.1), pp2$air.pp.neg9.1)
pp2$air.pp.neg9.1 <- ifelse(pp2$month == 9 & is.na(pp2$air.pp.neg9.1), (air.col114[9,1] + air.col114[9,2]*pp2$air.pp.neg3.0
                                                                        + air.col114[9,3]*pp2$air.pp.neg6.1), pp2$air.pp.neg9.1)
pp2$air.pp.neg9.1 <- ifelse(pp2$month == 10 & is.na(pp2$air.pp.neg9.1), (air.col114[10,1] + air.col114[10,2]*pp2$air.pp.neg3.0
                                                                         + air.col114[10,3]*pp2$air.pp.neg6.1), pp2$air.pp.neg9.1)
pp2$air.pp.neg9.1 <- ifelse(pp2$month == 11 & is.na(pp2$air.pp.neg9.1), (air.col114[11,1] + air.col114[11,2]*pp2$air.pp.neg3.0
                                                                         + air.col114[11,3]*pp2$air.pp.neg6.1), pp2$air.pp.neg9.1)
pp2$air.pp.neg9.1 <- ifelse(pp2$month == 12 & is.na(pp2$air.pp.neg9.1), (air.col114[12,1] + air.col114[12,2]*pp2$air.pp.neg3.0
                                                                         + air.col114[12,3]*pp2$air.pp.neg6.1), pp2$air.pp.neg9.1)

plot(pp2$air.pp.neg9.1, type = "l", col = "red")
lines(pp$air.pp.neg9.1, col = "blue")





################################################
################################################
# pairs(pp[,c(115,113,114)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPD air temperatures based on PPA and WSU
air.1.col115 <- lm(pp2[pp2$month==1, 115] ~ pp2[pp2$month==1, 113] + pp2[pp2$month==1, 114]) # Jan
air.2.col115 <- lm(pp2[pp2$month==2, 115] ~ pp2[pp2$month==2, 113] + pp2[pp2$month==2, 114]) # Feb
air.3.col115 <- lm(pp2[pp2$month==3, 115] ~ pp2[pp2$month==3, 113] + pp2[pp2$month==3, 114]) # Mar
air.4.col115 <- lm(pp2[pp2$month==4, 115] ~ pp2[pp2$month==4, 113] + pp2[pp2$month==4, 114]) # Apr
air.5.col115 <- lm(pp2[pp2$month==5, 115] ~ pp2[pp2$month==5, 113] + pp2[pp2$month==5, 114]) # May
air.6.col115 <- lm(pp2[pp2$month==6, 115] ~ pp2[pp2$month==6, 113] + pp2[pp2$month==6, 114]) # Jun
air.7.col115 <- lm(pp2[pp2$month==7, 115] ~ pp2[pp2$month==7, 113] + pp2[pp2$month==7, 114]) # Jul
air.8.col115 <- lm(pp2[pp2$month==8, 115] ~ pp2[pp2$month==8, 113] + pp2[pp2$month==8, 114]) # Aug
air.9.col115 <- lm(pp2[pp2$month==9, 115] ~ pp2[pp2$month==9, 113] + pp2[pp2$month==9, 114]) # Sep
air.10.col115 <- lm(pp2[pp2$month==10, 115] ~ pp2[pp2$month==10, 113] + pp2[pp2$month==10, 114]) # Oct
air.11.col115 <- lm(pp2[pp2$month==11, 115] ~ pp2[pp2$month==11, 113] + pp2[pp2$month==11, 114]) # Nov
air.12.col115 <- lm(pp2[pp2$month==12, 115] ~ pp2[pp2$month==12, 113] + pp2[pp2$month==12, 114]) # Dec
summary(air.1.col115)
summary(air.2.col115)
summary(air.3.col115)
summary(air.4.col115)
summary(air.5.col115)
summary(air.6.col115)
summary(air.7.col115)
summary(air.8.col115)
summary(air.9.col115)
summary(air.10.col115)
summary(air.11.col115)
summary(air.12.col115)

# Bind the coefficients together
air.col115 <- data.frame(rbind(coef(air.1.col115),coef(air.2.col115),coef(air.3.col115),coef(air.4.col115),coef(air.5.col115),coef(air.6.col115),
                               coef(air.7.col115),coef(air.8.col115),coef(air.9.col115),coef(air.10.col115),coef(air.11.col115),coef(air.12.col115)))
air.col115 <- cbind(air.col115, rbind(summary(air.1.col115)$adj.r.squared, summary(air.2.col115)$adj.r.squared, summary(air.3.col115)$adj.r.squared,
                                      summary(air.4.col115)$adj.r.squared, summary(air.5.col115)$adj.r.squared, summary(air.6.col115)$adj.r.squared,
                                      summary(air.7.col115)$adj.r.squared, summary(air.8.col115)$adj.r.squared, summary(air.9.col115)$adj.r.squared,
                                      summary(air.10.col115)$adj.r.squared, summary(air.11.col115)$adj.r.squared, summary(air.12.col115)$adj.r.squared))
names(air.col115) <- c("b0","air.col113","air.col114","r2adj")
rownames(air.col115) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPD air.wwBg.5 NAs
pp2$air.pp.neg12.2 <- ifelse(pp2$month == 1 & is.na(pp2$air.pp.neg12.2), (air.col115[1,1] + air.col115[1,2]*pp2$air.pp.neg6.1
                                                                        + air.col115[1,3]*pp2$air.pp.neg9.1), pp2$air.pp.neg12.2)
pp2$air.pp.neg12.2 <- ifelse(pp2$month == 2 & is.na(pp2$air.pp.neg12.2), (air.col115[2,1] + air.col115[2,2]*pp2$air.pp.neg6.1
                                                                        + air.col115[2,3]*pp2$air.pp.neg9.1), pp2$air.pp.neg12.2)
pp2$air.pp.neg12.2 <- ifelse(pp2$month == 3 & is.na(pp2$air.pp.neg12.2), (air.col115[3,1] + air.col115[3,2]*pp2$air.pp.neg6.1
                                                                        + air.col115[3,3]*pp2$air.pp.neg9.1), pp2$air.pp.neg12.2)
pp2$air.pp.neg12.2 <- ifelse(pp2$month == 4 & is.na(pp2$air.pp.neg12.2), (air.col115[4,1] + air.col115[4,2]*pp2$air.pp.neg6.1
                                                                        + air.col115[4,3]*pp2$air.pp.neg9.1), pp2$air.pp.neg12.2)
pp2$air.pp.neg12.2 <- ifelse(pp2$month == 5 & is.na(pp2$air.pp.neg12.2), (air.col115[5,1] + air.col115[5,2]*pp2$air.pp.neg6.1
                                                                        + air.col115[5,3]*pp2$air.pp.neg9.1), pp2$air.pp.neg12.2)
pp2$air.pp.neg12.2 <- ifelse(pp2$month == 6 & is.na(pp2$air.pp.neg12.2), (air.col115[6,1] + air.col115[6,2]*pp2$air.pp.neg6.1
                                                                        + air.col115[6,3]*pp2$air.pp.neg9.1), pp2$air.pp.neg12.2)
pp2$air.pp.neg12.2 <- ifelse(pp2$month == 7 & is.na(pp2$air.pp.neg12.2), (air.col115[7,1] + air.col115[7,2]*pp2$air.pp.neg6.1
                                                                        + air.col115[7,3]*pp2$air.pp.neg9.1), pp2$air.pp.neg12.2)
pp2$air.pp.neg12.2 <- ifelse(pp2$month == 8 & is.na(pp2$air.pp.neg12.2), (air.col115[8,1] + air.col115[8,2]*pp2$air.pp.neg6.1
                                                                        + air.col115[8,3]*pp2$air.pp.neg9.1), pp2$air.pp.neg12.2)
pp2$air.pp.neg12.2 <- ifelse(pp2$month == 9 & is.na(pp2$air.pp.neg12.2), (air.col115[9,1] + air.col115[9,2]*pp2$air.pp.neg6.1
                                                                        + air.col115[9,3]*pp2$air.pp.neg9.1), pp2$air.pp.neg12.2)
pp2$air.pp.neg12.2 <- ifelse(pp2$month == 10 & is.na(pp2$air.pp.neg12.2), (air.col115[10,1] + air.col115[10,2]*pp2$air.pp.neg6.1
                                                                         + air.col115[10,3]*pp2$air.pp.neg9.1), pp2$air.pp.neg12.2)
pp2$air.pp.neg12.2 <- ifelse(pp2$month == 11 & is.na(pp2$air.pp.neg12.2), (air.col115[11,1] + air.col115[11,2]*pp2$air.pp.neg6.1
                                                                         + air.col115[11,3]*pp2$air.pp.neg9.1), pp2$air.pp.neg12.2)
pp2$air.pp.neg12.2 <- ifelse(pp2$month == 12 & is.na(pp2$air.pp.neg12.2), (air.col115[12,1] + air.col115[12,2]*pp2$air.pp.neg6.1
                                                                         + air.col115[12,3]*pp2$air.pp.neg9.1), pp2$air.pp.neg12.2)

plot(pp2$air.pp.neg12.2, type = "l", col = "red")
lines(pp$air.pp.neg12.2, col = "blue")




################################################
################################################
# pairs(pp[,c(116,114,115)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPD air temperatures based on PPA and WSU
air.1.col116 <- lm(pp2[pp2$month==1, 116] ~ pp2[pp2$month==1, 114] + pp2[pp2$month==1, 115]) # Jan
air.2.col116 <- lm(pp2[pp2$month==2, 116] ~ pp2[pp2$month==2, 114] + pp2[pp2$month==2, 115]) # Feb
air.3.col116 <- lm(pp2[pp2$month==3, 116] ~ pp2[pp2$month==3, 114] + pp2[pp2$month==3, 115]) # Mar
air.4.col116 <- lm(pp2[pp2$month==4, 116] ~ pp2[pp2$month==4, 114] + pp2[pp2$month==4, 115]) # Apr
air.5.col116 <- lm(pp2[pp2$month==5, 116] ~ pp2[pp2$month==5, 114] + pp2[pp2$month==5, 115]) # May
air.6.col116 <- lm(pp2[pp2$month==6, 116] ~ pp2[pp2$month==6, 114] + pp2[pp2$month==6, 115]) # Jun
air.7.col116 <- lm(pp2[pp2$month==7, 116] ~ pp2[pp2$month==7, 114] + pp2[pp2$month==7, 115]) # Jul
air.8.col116 <- lm(pp2[pp2$month==8, 116] ~ pp2[pp2$month==8, 114] + pp2[pp2$month==8, 115]) # Aug
air.9.col116 <- lm(pp2[pp2$month==9, 116] ~ pp2[pp2$month==9, 114] + pp2[pp2$month==9, 115]) # Sep
air.10.col116 <- lm(pp2[pp2$month==10, 116] ~ pp2[pp2$month==10, 114] + pp2[pp2$month==10, 115]) # Oct
air.11.col116 <- lm(pp2[pp2$month==11, 116] ~ pp2[pp2$month==11, 114] + pp2[pp2$month==11, 115]) # Nov
air.12.col116 <- lm(pp2[pp2$month==12, 116] ~ pp2[pp2$month==12, 114] + pp2[pp2$month==12, 115]) # Dec
summary(air.1.col116)
summary(air.2.col116)
summary(air.3.col116)
summary(air.4.col116)
summary(air.5.col116)
summary(air.6.col116)
summary(air.7.col116)
summary(air.8.col116)
summary(air.9.col116)
summary(air.10.col116)
summary(air.11.col116)
summary(air.12.col116)

# Bind the coefficients together
air.col116 <- data.frame(rbind(coef(air.1.col116),coef(air.2.col116),coef(air.3.col116),coef(air.4.col116),coef(air.5.col116),coef(air.6.col116),
                               coef(air.7.col116),coef(air.8.col116),coef(air.9.col116),coef(air.10.col116),coef(air.11.col116),coef(air.12.col116)))
air.col116 <- cbind(air.col116, rbind(summary(air.1.col116)$adj.r.squared, summary(air.2.col116)$adj.r.squared, summary(air.3.col116)$adj.r.squared,
                                      summary(air.4.col116)$adj.r.squared, summary(air.5.col116)$adj.r.squared, summary(air.6.col116)$adj.r.squared,
                                      summary(air.7.col116)$adj.r.squared, summary(air.8.col116)$adj.r.squared, summary(air.9.col116)$adj.r.squared,
                                      summary(air.10.col116)$adj.r.squared, summary(air.11.col116)$adj.r.squared, summary(air.12.col116)$adj.r.squared))
names(air.col116) <- c("b0","air.col114","air.col115","r2adj")
rownames(air.col116) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPD air.wwBg.5 NAs
pp2$air.pp.neg15 <- ifelse(pp2$month == 1 & is.na(pp2$air.pp.neg15), (air.col116[1,1] + air.col116[1,2]*pp2$air.pp.neg9.1
                                                                          + air.col116[1,3]*pp2$air.pp.neg12.2), pp2$air.pp.neg15)
pp2$air.pp.neg15 <- ifelse(pp2$month == 2 & is.na(pp2$air.pp.neg15), (air.col116[2,1] + air.col116[2,2]*pp2$air.pp.neg9.1
                                                                          + air.col116[2,3]*pp2$air.pp.neg12.2), pp2$air.pp.neg15)
pp2$air.pp.neg15 <- ifelse(pp2$month == 3 & is.na(pp2$air.pp.neg15), (air.col116[3,1] + air.col116[3,2]*pp2$air.pp.neg9.1
                                                                          + air.col116[3,3]*pp2$air.pp.neg12.2), pp2$air.pp.neg15)
pp2$air.pp.neg15 <- ifelse(pp2$month == 4 & is.na(pp2$air.pp.neg15), (air.col116[4,1] + air.col116[4,2]*pp2$air.pp.neg9.1
                                                                          + air.col116[4,3]*pp2$air.pp.neg12.2), pp2$air.pp.neg15)
pp2$air.pp.neg15 <- ifelse(pp2$month == 5 & is.na(pp2$air.pp.neg15), (air.col116[5,1] + air.col116[5,2]*pp2$air.pp.neg9.1
                                                                          + air.col116[5,3]*pp2$air.pp.neg12.2), pp2$air.pp.neg15)
pp2$air.pp.neg15 <- ifelse(pp2$month == 6 & is.na(pp2$air.pp.neg15), (air.col116[6,1] + air.col116[6,2]*pp2$air.pp.neg9.1
                                                                          + air.col116[6,3]*pp2$air.pp.neg12.2), pp2$air.pp.neg15)
pp2$air.pp.neg15 <- ifelse(pp2$month == 7 & is.na(pp2$air.pp.neg15), (air.col116[7,1] + air.col116[7,2]*pp2$air.pp.neg9.1
                                                                          + air.col116[7,3]*pp2$air.pp.neg12.2), pp2$air.pp.neg15)
pp2$air.pp.neg15 <- ifelse(pp2$month == 8 & is.na(pp2$air.pp.neg15), (air.col116[8,1] + air.col116[8,2]*pp2$air.pp.neg9.1
                                                                          + air.col116[8,3]*pp2$air.pp.neg12.2), pp2$air.pp.neg15)
pp2$air.pp.neg15 <- ifelse(pp2$month == 9 & is.na(pp2$air.pp.neg15), (air.col116[9,1] + air.col116[9,2]*pp2$air.pp.neg9.1
                                                                          + air.col116[9,3]*pp2$air.pp.neg12.2), pp2$air.pp.neg15)
pp2$air.pp.neg15 <- ifelse(pp2$month == 10 & is.na(pp2$air.pp.neg15), (air.col116[10,1] + air.col116[10,2]*pp2$air.pp.neg9.1
                                                                           + air.col116[10,3]*pp2$air.pp.neg12.2), pp2$air.pp.neg15)
pp2$air.pp.neg15 <- ifelse(pp2$month == 11 & is.na(pp2$air.pp.neg15), (air.col116[11,1] + air.col116[11,2]*pp2$air.pp.neg9.1
                                                                           + air.col116[11,3]*pp2$air.pp.neg12.2), pp2$air.pp.neg15)
pp2$air.pp.neg15 <- ifelse(pp2$month == 12 & is.na(pp2$air.pp.neg15), (air.col116[12,1] + air.col116[12,2]*pp2$air.pp.neg9.1
                                                                           + air.col116[12,3]*pp2$air.pp.neg12.2), pp2$air.pp.neg15)

plot(pp2$air.pp.neg15, type = "l", col = "red")
lines(pp$air.pp.neg15, col = "blue")



################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################################################################


################################################
################################################
max(pp2[,104], na.rm = T)
pp2$ppd.Mast..230[pp2$ppd.Mast..230 == 6999] <- NA
pp2$ppd.Mast..230[pp2$ppd.Mast..230 == 162.5] <- NA
pp2$ppd.Mast..230[pp2$ppd.Mast..230 == 1.035] <- NA
pp2$ppd.Mast..230[pp2$ppd.Mast..230 == -0.998] <- NA
pp2$ppd.Mast..230[pp2$ppd.Mast..230 == -1.568] <- NA
pp2$ppd.Mast..230[pp2$ppd.Mast..230 == -1.575] <- NA
pp2$ppd.Mast..230[pp2$ppd.Mast..230 == -1.578] <- NA

# pairs(pp2[,c(104,110:116)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPD air temperatures based on PPA and WSU
air.1.col104 <- lm(pp2[pp2$month==1, 104] ~ pp2[pp2$month==1, 111] + pp2[pp2$month==1, 112]) # Jan
air.2.col104 <- lm(pp2[pp2$month==2, 104] ~ pp2[pp2$month==2, 111] + pp2[pp2$month==2, 112]) # Feb
air.3.col104 <- lm(pp2[pp2$month==3, 104] ~ pp2[pp2$month==3, 111] + pp2[pp2$month==3, 112]) # Mar
air.4.col104 <- lm(pp2[pp2$month==4, 104] ~ pp2[pp2$month==4, 111] + pp2[pp2$month==4, 112]) # Apr
air.5.col104 <- lm(pp2[pp2$month==5, 104] ~ pp2[pp2$month==5, 111] + pp2[pp2$month==5, 112]) # May
air.6.col104 <- lm(pp2[pp2$month==6, 104] ~ pp2[pp2$month==6, 111] + pp2[pp2$month==6, 112]) # Jun
air.7.col104 <- lm(pp2[pp2$month==7, 104] ~ pp2[pp2$month==7, 111] + pp2[pp2$month==7, 112]) # Jul
air.8.col104 <- lm(pp2[pp2$month==8, 104] ~ pp2[pp2$month==8, 111] + pp2[pp2$month==8, 112]) # Aug
air.9.col104 <- lm(pp2[pp2$month==9, 104] ~ pp2[pp2$month==9, 111] + pp2[pp2$month==9, 112]) # Sep
air.10.col104 <- lm(pp2[pp2$month==10, 104] ~ pp2[pp2$month==10, 111] + pp2[pp2$month==10, 112]) # Oct
air.11.col104 <- lm(pp2[pp2$month==11, 104] ~ pp2[pp2$month==11, 111] + pp2[pp2$month==11, 112]) # Nov
air.12.col104 <- lm(pp2[pp2$month==12, 104] ~ pp2[pp2$month==12, 111] + pp2[pp2$month==12, 112]) # Dec
summary(air.1.col104)
summary(air.2.col104)
summary(air.3.col104)
summary(air.4.col104)
summary(air.5.col104)
summary(air.6.col104)
summary(air.7.col104)
summary(air.8.col104)
summary(air.9.col104)
summary(air.10.col104)
summary(air.11.col104)
summary(air.12.col104)

# Bind the coefficients together
air.col104 <- data.frame(rbind(coef(air.1.col104),coef(air.2.col104),coef(air.3.col104),coef(air.4.col104),coef(air.5.col104),coef(air.6.col104),
                               coef(air.7.col104),coef(air.8.col104),coef(air.9.col104),coef(air.10.col104),coef(air.11.col104),coef(air.12.col104)))
air.col104 <- cbind(air.col104, rbind(summary(air.1.col104)$adj.r.squared, summary(air.2.col104)$adj.r.squared, summary(air.3.col104)$adj.r.squared,
                                      summary(air.4.col104)$adj.r.squared, summary(air.5.col104)$adj.r.squared, summary(air.6.col104)$adj.r.squared,
                                      summary(air.7.col104)$adj.r.squared, summary(air.8.col104)$adj.r.squared, summary(air.9.col104)$adj.r.squared,
                                      summary(air.10.col104)$adj.r.squared, summary(air.11.col104)$adj.r.squared, summary(air.12.col104)$adj.r.squared))
names(air.col104) <- c("b0","air.col111","air.col112","r2adj")
rownames(air.col104) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPD air.wwBg.5 NAs
pp2$ppd.Mast..230 <- ifelse(pp2$month == 1 & is.na(pp2$ppd.Mast..230), (air.col104[1,1] + air.col104[1,2]*pp2$air.pp.neg2.3
                                                                      + air.col104[1,3]*pp2$air.pp.neg3.0), pp2$ppd.Mast..230)
pp2$ppd.Mast..230 <- ifelse(pp2$month == 2 & is.na(pp2$ppd.Mast..230), (air.col104[2,1] + air.col104[2,2]*pp2$air.pp.neg2.3
                                                                      + air.col104[2,3]*pp2$air.pp.neg3.0), pp2$ppd.Mast..230)
pp2$ppd.Mast..230 <- ifelse(pp2$month == 3 & is.na(pp2$ppd.Mast..230), (air.col104[3,1] + air.col104[3,2]*pp2$air.pp.neg2.3
                                                                      + air.col104[3,3]*pp2$air.pp.neg3.0), pp2$ppd.Mast..230)
pp2$ppd.Mast..230 <- ifelse(pp2$month == 4 & is.na(pp2$ppd.Mast..230), (air.col104[4,1] + air.col104[4,2]*pp2$air.pp.neg2.3
                                                                      + air.col104[4,3]*pp2$air.pp.neg3.0), pp2$ppd.Mast..230)
pp2$ppd.Mast..230 <- ifelse(pp2$month == 5 & is.na(pp2$ppd.Mast..230), (air.col104[5,1] + air.col104[5,2]*pp2$air.pp.neg2.3
                                                                      + air.col104[5,3]*pp2$air.pp.neg3.0), pp2$ppd.Mast..230)
pp2$ppd.Mast..230 <- ifelse(pp2$month == 6 & is.na(pp2$ppd.Mast..230), (air.col104[6,1] + air.col104[6,2]*pp2$air.pp.neg2.3
                                                                      + air.col104[6,3]*pp2$air.pp.neg3.0), pp2$ppd.Mast..230)
pp2$ppd.Mast..230 <- ifelse(pp2$month == 7 & is.na(pp2$ppd.Mast..230), (air.col104[7,1] + air.col104[7,2]*pp2$air.pp.neg2.3
                                                                      + air.col104[7,3]*pp2$air.pp.neg3.0), pp2$ppd.Mast..230)
pp2$ppd.Mast..230 <- ifelse(pp2$month == 8 & is.na(pp2$ppd.Mast..230), (air.col104[8,1] + air.col104[8,2]*pp2$air.pp.neg2.3
                                                                      + air.col104[8,3]*pp2$air.pp.neg3.0), pp2$ppd.Mast..230)
pp2$ppd.Mast..230 <- ifelse(pp2$month == 9 & is.na(pp2$ppd.Mast..230), (air.col104[9,1] + air.col104[9,2]*pp2$air.pp.neg2.3
                                                                      + air.col104[9,3]*pp2$air.pp.neg3.0), pp2$ppd.Mast..230)
pp2$ppd.Mast..230 <- ifelse(pp2$month == 10 & is.na(pp2$ppd.Mast..230), (air.col104[10,1] + air.col104[10,2]*pp2$air.pp.neg2.3
                                                                       + air.col104[10,3]*pp2$air.pp.neg3.0), pp2$ppd.Mast..230)
pp2$ppd.Mast..230 <- ifelse(pp2$month == 11 & is.na(pp2$ppd.Mast..230), (air.col104[11,1] + air.col104[11,2]*pp2$air.pp.neg2.3
                                                                       + air.col104[11,3]*pp2$air.pp.neg3.0), pp2$ppd.Mast..230)
pp2$ppd.Mast..230 <- ifelse(pp2$month == 12 & is.na(pp2$ppd.Mast..230), (air.col104[12,1] + air.col104[12,2]*pp2$air.pp.neg2.3
                                                                       + air.col104[12,3]*pp2$air.pp.neg3.0), pp2$ppd.Mast..230)

plot(pp2$ppd.Mast..230, type = "l", col = "red")
lines(pp$ppd.Mast..230, col = "blue")






################################################
################################################
max(pp2[,105], na.rm = T)
pp2$ppd.Mast..200[pp2$ppd.Mast..200 == 6999] <- NA
pp2$ppd.Mast..200[pp2$ppd.Mast..200 == 129.7] <- NA
pp2$ppd.Mast..200[pp2$ppd.Mast..200 == -1.031] <- NA
pp2$ppd.Mast..200[pp2$ppd.Mast..200 == -1.058] <- NA
pp2$ppd.Mast..200[pp2$ppd.Mast..200 == -1.372] <- NA
pp2$ppd.Mast..200[pp2$ppd.Mast..200 == -1.374] <- NA
pp2$ppd.Mast..200[pp2$ppd.Mast..200 == -1.385] <- NA
pp2$ppd.Mast..200[pp2$ppd.Mast..200 == -1.386] <- NA
pp2$ppd.Mast..200[pp2$ppd.Mast..200 == -1.393] <- NA

pairs(pp2[,c(105,104,111,112)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPD air temperatures based on PPA and WSU
air.1.col105 <- lm(pp2[pp2$month==1, 105] ~ pp2[pp2$month==1, 104] + pp2[pp2$month==1, 111]) # Jan
air.2.col105 <- lm(pp2[pp2$month==2, 105] ~ pp2[pp2$month==2, 104] + pp2[pp2$month==2, 111]) # Feb
air.3.col105 <- lm(pp2[pp2$month==3, 105] ~ pp2[pp2$month==3, 104] + pp2[pp2$month==3, 111]) # Mar
air.4.col105 <- lm(pp2[pp2$month==4, 105] ~ pp2[pp2$month==4, 104] + pp2[pp2$month==4, 111]) # Apr
air.5.col105 <- lm(pp2[pp2$month==5, 105] ~ pp2[pp2$month==5, 104] + pp2[pp2$month==5, 111]) # May
air.6.col105 <- lm(pp2[pp2$month==6, 105] ~ pp2[pp2$month==6, 104] + pp2[pp2$month==6, 111]) # Jun
air.7.col105 <- lm(pp2[pp2$month==7, 105] ~ pp2[pp2$month==7, 104] + pp2[pp2$month==7, 111]) # Jul
air.8.col105 <- lm(pp2[pp2$month==8, 105] ~ pp2[pp2$month==8, 104] + pp2[pp2$month==8, 111]) # Aug
air.9.col105 <- lm(pp2[pp2$month==9, 105] ~ pp2[pp2$month==9, 104] + pp2[pp2$month==9, 111]) # Sep
air.10.col105 <- lm(pp2[pp2$month==10, 105] ~ pp2[pp2$month==10, 104] + pp2[pp2$month==10, 111]) # Oct
air.11.col105 <- lm(pp2[pp2$month==11, 105] ~ pp2[pp2$month==11, 104] + pp2[pp2$month==11, 111]) # Nov
air.12.col105 <- lm(pp2[pp2$month==12, 105] ~ pp2[pp2$month==12, 104] + pp2[pp2$month==12, 111]) # Dec
summary(air.1.col105)
summary(air.2.col105)
summary(air.3.col105)
summary(air.4.col105)
summary(air.5.col105)
summary(air.6.col105)
summary(air.7.col105)
summary(air.8.col105)
summary(air.9.col105)
summary(air.10.col105)
summary(air.11.col105)
summary(air.12.col105)

# Bind the coefficients together
air.col105 <- data.frame(rbind(coef(air.1.col105),coef(air.2.col105),coef(air.3.col105),coef(air.4.col105),coef(air.5.col105),coef(air.6.col105),
                               coef(air.7.col105),coef(air.8.col105),coef(air.9.col105),coef(air.10.col105),coef(air.11.col105),coef(air.12.col105)))
air.col105 <- cbind(air.col105, rbind(summary(air.1.col105)$adj.r.squared, summary(air.2.col105)$adj.r.squared, summary(air.3.col105)$adj.r.squared,
                                      summary(air.4.col105)$adj.r.squared, summary(air.5.col105)$adj.r.squared, summary(air.6.col105)$adj.r.squared,
                                      summary(air.7.col105)$adj.r.squared, summary(air.8.col105)$adj.r.squared, summary(air.9.col105)$adj.r.squared,
                                      summary(air.10.col105)$adj.r.squared, summary(air.11.col105)$adj.r.squared, summary(air.12.col105)$adj.r.squared))
names(air.col105) <- c("b0","air.col104","air.col111","r2adj")
rownames(air.col105) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPD air.wwBg.5 NAs
pp2$ppd.Mast..200 <- ifelse(pp2$month == 1 & is.na(pp2$ppd.Mast..200), (air.col105[1,1] + air.col105[1,2]*pp2$ppd.Mast..230
                                                                        + air.col105[1,3]*pp2$air.pp.neg2.3), pp2$ppd.Mast..200)
pp2$ppd.Mast..200 <- ifelse(pp2$month == 2 & is.na(pp2$ppd.Mast..200), (air.col105[2,1] + air.col105[2,2]*pp2$ppd.Mast..230
                                                                        + air.col105[2,3]*pp2$air.pp.neg2.3), pp2$ppd.Mast..200)
pp2$ppd.Mast..200 <- ifelse(pp2$month == 3 & is.na(pp2$ppd.Mast..200), (air.col105[3,1] + air.col105[3,2]*pp2$ppd.Mast..230
                                                                        + air.col105[3,3]*pp2$air.pp.neg2.3), pp2$ppd.Mast..200)
pp2$ppd.Mast..200 <- ifelse(pp2$month == 4 & is.na(pp2$ppd.Mast..200), (air.col105[4,1] + air.col105[4,2]*pp2$ppd.Mast..230
                                                                        + air.col105[4,3]*pp2$air.pp.neg2.3), pp2$ppd.Mast..200)
pp2$ppd.Mast..200 <- ifelse(pp2$month == 5 & is.na(pp2$ppd.Mast..200), (air.col105[5,1] + air.col105[5,2]*pp2$ppd.Mast..230
                                                                        + air.col105[5,3]*pp2$air.pp.neg2.3), pp2$ppd.Mast..200)
pp2$ppd.Mast..200 <- ifelse(pp2$month == 6 & is.na(pp2$ppd.Mast..200), (air.col105[6,1] + air.col105[6,2]*pp2$ppd.Mast..230
                                                                        + air.col105[6,3]*pp2$air.pp.neg2.3), pp2$ppd.Mast..200)
pp2$ppd.Mast..200 <- ifelse(pp2$month == 7 & is.na(pp2$ppd.Mast..200), (air.col105[7,1] + air.col105[7,2]*pp2$ppd.Mast..230
                                                                        + air.col105[7,3]*pp2$air.pp.neg2.3), pp2$ppd.Mast..200)
pp2$ppd.Mast..200 <- ifelse(pp2$month == 8 & is.na(pp2$ppd.Mast..200), (air.col105[8,1] + air.col105[8,2]*pp2$ppd.Mast..230
                                                                        + air.col105[8,3]*pp2$air.pp.neg2.3), pp2$ppd.Mast..200)
pp2$ppd.Mast..200 <- ifelse(pp2$month == 9 & is.na(pp2$ppd.Mast..200), (air.col105[9,1] + air.col105[9,2]*pp2$ppd.Mast..230
                                                                        + air.col105[9,3]*pp2$air.pp.neg2.3), pp2$ppd.Mast..200)
pp2$ppd.Mast..200 <- ifelse(pp2$month == 10 & is.na(pp2$ppd.Mast..200), (air.col105[10,1] + air.col105[10,2]*pp2$ppd.Mast..230
                                                                         + air.col105[10,3]*pp2$air.pp.neg2.3), pp2$ppd.Mast..200)
pp2$ppd.Mast..200 <- ifelse(pp2$month == 11 & is.na(pp2$ppd.Mast..200), (air.col105[11,1] + air.col105[11,2]*pp2$ppd.Mast..230
                                                                         + air.col105[11,3]*pp2$air.pp.neg2.3), pp2$ppd.Mast..200)
pp2$ppd.Mast..200 <- ifelse(pp2$month == 12 & is.na(pp2$ppd.Mast..200), (air.col105[12,1] + air.col105[12,2]*pp2$ppd.Mast..230
                                                                         + air.col105[12,3]*pp2$air.pp.neg2.3), pp2$ppd.Mast..200)

plot(pp2$ppd.Mast..200, type = "l", col = "red")
lines(pp$ppd.Mast..200, col = "blue")






################################################
################################################
max(pp2[,106], na.rm = T)
pp2$ppd.Mast..100[pp2$ppd.Mast..100 == 6999] <- NA
pp2$ppd.Mast..100[pp2$ppd.Mast..100 == 103.5] <- NA
pp2$ppd.Mast..100[pp2$ppd.Mast..100 == 10.23] <- NA
pp2$ppd.Mast..100[pp2$ppd.Mast..100 == 9.99] <- NA
pp2$ppd.Mast..100[pp2$ppd.Mast..100 == 9.96] <- NA

pairs(pp2[,c(106,109,110)], lower.panel=panel.smooth, upper.panel=panel.cor, cex.labels = 1)

# PPD air temperatures based on PPA and WSU
air.1.col106 <- lm(pp2[pp2$month==1, 106] ~ pp2[pp2$month==1, 109] + pp2[pp2$month==1, 110]) # Jan
air.2.col106 <- lm(pp2[pp2$month==2, 106] ~ pp2[pp2$month==2, 109] + pp2[pp2$month==2, 110]) # Feb
air.3.col106 <- lm(pp2[pp2$month==3, 106] ~ pp2[pp2$month==3, 109] + pp2[pp2$month==3, 110]) # Mar
air.4.col106 <- lm(pp2[pp2$month==4, 106] ~ pp2[pp2$month==4, 109] + pp2[pp2$month==4, 110]) # Apr
air.5.col106 <- lm(pp2[pp2$month==5, 106] ~ pp2[pp2$month==5, 109] + pp2[pp2$month==5, 110]) # May
air.6.col106 <- lm(pp2[pp2$month==6, 106] ~ pp2[pp2$month==6, 109] + pp2[pp2$month==6, 110]) # Jun
air.7.col106 <- lm(pp2[pp2$month==7, 106] ~ pp2[pp2$month==7, 109] + pp2[pp2$month==7, 110]) # Jul
air.8.col106 <- lm(pp2[pp2$month==8, 106] ~ pp2[pp2$month==8, 109] + pp2[pp2$month==8, 110]) # Aug
air.9.col106 <- lm(pp2[pp2$month==9, 106] ~ pp2[pp2$month==9, 109] + pp2[pp2$month==9, 110]) # Sep
air.10.col106 <- lm(pp2[pp2$month==10, 106] ~ pp2[pp2$month==10, 109] + pp2[pp2$month==10, 110]) # Oct
air.11.col106 <- lm(pp2[pp2$month==11, 106] ~ pp2[pp2$month==11, 109] + pp2[pp2$month==11, 110]) # Nov
air.12.col106 <- lm(pp2[pp2$month==12, 106] ~ pp2[pp2$month==12, 109] + pp2[pp2$month==12, 110]) # Dec
summary(air.1.col106)
summary(air.2.col106)
summary(air.3.col106)
summary(air.4.col106)
summary(air.5.col106)
summary(air.6.col106)
summary(air.7.col106)
summary(air.8.col106)
summary(air.9.col106)
summary(air.10.col106)
summary(air.11.col106)
summary(air.12.col106)

# Bind the coefficients together
air.col106 <- data.frame(rbind(coef(air.1.col106),coef(air.2.col106),coef(air.3.col106),coef(air.4.col106),coef(air.5.col106),coef(air.6.col106),
                               coef(air.7.col106),coef(air.8.col106),coef(air.9.col106),coef(air.10.col106),coef(air.11.col106),coef(air.12.col106)))
air.col106 <- cbind(air.col106, rbind(summary(air.1.col106)$adj.r.squared, summary(air.2.col106)$adj.r.squared, summary(air.3.col106)$adj.r.squared,
                                      summary(air.4.col106)$adj.r.squared, summary(air.5.col106)$adj.r.squared, summary(air.6.col106)$adj.r.squared,
                                      summary(air.7.col106)$adj.r.squared, summary(air.8.col106)$adj.r.squared, summary(air.9.col106)$adj.r.squared,
                                      summary(air.10.col106)$adj.r.squared, summary(air.11.col106)$adj.r.squared, summary(air.12.col106)$adj.r.squared))
names(air.col106) <- c("b0","air.col109","air.col110","r2adj")
rownames(air.col106) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Replace PPD air.wwBg.5 NAs
pp2$ppd.Mast..100 <- ifelse(pp2$month == 1 & is.na(pp2$ppd.Mast..100), (air.col106[1,1] + air.col106[1,2]*pp2$air.pp.neg0.8
                                                                        + air.col106[1,3]*pp2$air.pp.neg1.5), pp2$ppd.Mast..100)
pp2$ppd.Mast..100 <- ifelse(pp2$month == 2 & is.na(pp2$ppd.Mast..100), (air.col106[2,1] + air.col106[2,2]*pp2$air.pp.neg0.8
                                                                        + air.col106[2,3]*pp2$air.pp.neg1.5), pp2$ppd.Mast..100)
pp2$ppd.Mast..100 <- ifelse(pp2$month == 3 & is.na(pp2$ppd.Mast..100), (air.col106[3,1] + air.col106[3,2]*pp2$air.pp.neg0.8
                                                                        + air.col106[3,3]*pp2$air.pp.neg1.5), pp2$ppd.Mast..100)
pp2$ppd.Mast..100 <- ifelse(pp2$month == 4 & is.na(pp2$ppd.Mast..100), (air.col106[4,1] + air.col106[4,2]*pp2$air.pp.neg0.8
                                                                        + air.col106[4,3]*pp2$air.pp.neg1.5), pp2$ppd.Mast..100)
pp2$ppd.Mast..100 <- ifelse(pp2$month == 5 & is.na(pp2$ppd.Mast..100), (air.col106[5,1] + air.col106[5,2]*pp2$air.pp.neg0.8
                                                                        + air.col106[5,3]*pp2$air.pp.neg1.5), pp2$ppd.Mast..100)
pp2$ppd.Mast..100 <- ifelse(pp2$month == 6 & is.na(pp2$ppd.Mast..100), (air.col106[6,1] + air.col106[6,2]*pp2$air.pp.neg0.8
                                                                        + air.col106[6,3]*pp2$air.pp.neg1.5), pp2$ppd.Mast..100)
pp2$ppd.Mast..100 <- ifelse(pp2$month == 7 & is.na(pp2$ppd.Mast..100), (air.col106[7,1] + air.col106[7,2]*pp2$air.pp.neg0.8
                                                                        + air.col106[7,3]*pp2$air.pp.neg1.5), pp2$ppd.Mast..100)
pp2$ppd.Mast..100 <- ifelse(pp2$month == 8 & is.na(pp2$ppd.Mast..100), (air.col106[8,1] + air.col106[8,2]*pp2$air.pp.neg0.8
                                                                        + air.col106[8,3]*pp2$air.pp.neg1.5), pp2$ppd.Mast..100)
pp2$ppd.Mast..100 <- ifelse(pp2$month == 9 & is.na(pp2$ppd.Mast..100), (air.col106[9,1] + air.col106[9,2]*pp2$air.pp.neg0.8
                                                                        + air.col106[9,3]*pp2$air.pp.neg1.5), pp2$ppd.Mast..100)
pp2$ppd.Mast..100 <- ifelse(pp2$month == 10 & is.na(pp2$ppd.Mast..100), (air.col106[10,1] + air.col106[10,2]*pp2$air.pp.neg0.8
                                                                         + air.col106[10,3]*pp2$air.pp.neg1.5), pp2$ppd.Mast..100)
pp2$ppd.Mast..100 <- ifelse(pp2$month == 11 & is.na(pp2$ppd.Mast..100), (air.col106[11,1] + air.col106[11,2]*pp2$air.pp.neg0.8
                                                                         + air.col106[11,3]*pp2$air.pp.neg1.5), pp2$ppd.Mast..100)
pp2$ppd.Mast..100 <- ifelse(pp2$month == 12 & is.na(pp2$ppd.Mast..100), (air.col106[12,1] + air.col106[12,2]*pp2$air.pp.neg0.8
                                                                         + air.col106[12,3]*pp2$air.pp.neg1.5), pp2$ppd.Mast..100)

plot(pp2$ppd.Mast..100, type = "l", col = "red")
lines(pp$ppd.Mast..100, col = "blue")


write.csv(pp2,"~/Desktop/pp2.csv")

pp3 <- aggregate(pp2)

air.tc <- read.csv("~/Desktop/Workspace/Churchill/air.thermal.diffusivity.csv", header = T)

# colfunc <- colorRampPalette(c("blue4", "darkred"))
# col.s <- colfunc(12)
library(RColorBrewer)
colfunc <- colorRampPalette(brewer.pal(8, "Spectral"))

## Export at 5 x 7
# Thawed
par(oma = c(2,6,0,1), mar = c(2,1,0.5,0))
par(mfrow = c(2,1))
matplot(air.tc[,c(2:13)], air.tc$year, type = "l", ylim = rev(range(air.tc$year)), xlim = c(0,4), ylab = "", xlab = "", yaxt = "n", lwd = 2, lty = 1, col = colfunc(12))
legend("top","Thaw season", bty = "n")
axis(2, at = air.tc$year, labels = c("0-0.8","0.8-1.5","1.5-2.3","2.3-3.0","3.0-6.1","6.1-9.1","9.1-12.2","12.2-15.0"), las = 1)
legend("bottomright", legend = seq(2006,2017,1), lty = 1, lwd = 2, col = colfunc(12),
       bty = "n", pch = 15, pt.cex = c(1,1), text.width = 0.1, y.intersp = 0.6, inset = c(0.09,0))

# Frozen
matplot(air.tc[,c(14:25)], air.tc$year, type = "l", ylim = rev(range(air.tc$year)), xlim = c(0,4), ylab = "", xlab = "", yaxt = "n", lwd = 2, lty = 1, col = colfunc(12))
legend("top","Frozen season", bty = "n")
axis(2, at = air.tc$year, labels = c("0-0.8","0.8-1.5","1.5-2.3","2.3-3.0","3.0-6.1","6.1-9.1","9.1-12.2","12.2-15.0"), las = 1)
mtext(side = 1, expression(paste("Thermal diffusivity (m"^"2"~"s"^"-1",")")), line = 2.5)
mtext(side = 2, "Depth (m)", outer = TRUE, line = 4.5)


air.temps <- read.csv("~/Desktop/Workspace/Churchill/AIR_temps.csv", header = T)
air.temps.thaw <- subset(air.temps, season == "thaw")
air.temps.freeze <- subset(air.temps, season == "freeze")

air.thaw.150.lm <- lm(air.pp.15 ~ year, data = air.temps.thaw)
air.thaw.0.lm <- lm(air.pp.0 ~ year, data = air.temps.thaw)
air.thaw.neg0.8.lm <- lm(air.pp.neg0.8 ~ year, data = air.temps.thaw)
air.thaw.neg1.5.lm <- lm(air.pp.neg1.5 ~ year, data = air.temps.thaw)
air.thaw.neg2.3.lm <- lm(air.pp.neg2.3 ~ year, data = air.temps.thaw)
air.thaw.neg3.0.lm <- lm(air.pp.neg3.0 ~ year, data = air.temps.thaw)
air.thaw.neg6.1.lm <- lm(air.pp.neg6.1 ~ year, data = air.temps.thaw)
air.thaw.neg9.1.lm <- lm(air.pp.neg9.1 ~ year, data = air.temps.thaw)
air.thaw.neg12.2.lm <- lm(air.pp.neg12.2 ~ year, data = air.temps.thaw)
air.thaw.neg15.0.lm <- lm(air.pp.neg15 ~ year, data = air.temps.thaw)

air.freeze.150.lm <- lm(air.pp.15 ~ year, data = air.temps.freeze)
air.freeze.0.lm <- lm(air.pp.0 ~ year, data = air.temps.freeze)
air.freeze.neg0.8.lm <- lm(air.pp.neg0.8 ~ year, data = air.temps.freeze)
air.freeze.neg1.5.lm <- lm(air.pp.neg1.5 ~ year, data = air.temps.freeze)
air.freeze.neg2.3.lm <- lm(air.pp.neg2.3 ~ year, data = air.temps.freeze)
air.freeze.neg3.0.lm <- lm(air.pp.neg3.0 ~ year, data = air.temps.freeze)
air.freeze.neg6.1.lm <- lm(air.pp.neg6.1 ~ year, data = air.temps.freeze)
air.freeze.neg9.1.lm <- lm(air.pp.neg9.1 ~ year, data = air.temps.freeze)
air.freeze.neg12.2.lm <- lm(air.pp.neg12.2 ~ year, data = air.temps.freeze)
air.freeze.neg15.0.lm <- lm(air.pp.neg15 ~ year, data = air.temps.freeze)

summary(air.thaw.150.lm) # ns
summary(air.thaw.0.lm) # ns
summary(air.thaw.neg0.8.lm) # b0 = -0.027801, p = 0.0109
summary(air.thaw.neg1.5.lm) # b0 = -0.026684, p = 0.0234
summary(air.thaw.neg2.3.lm) # b0 = -0.024173, p = 0.0327
summary(air.thaw.neg3.0.lm) # b0 = -0.022853, p = 0.0335
summary(air.thaw.neg6.1.lm) # ns
summary(air.thaw.neg9.1.lm) # ns
summary(air.thaw.neg12.2.lm) # b0 = 0.004207, p = 0.00559
summary(air.thaw.neg15.0.lm) # b0 = 0.003559, p = 0.0206

summary(air.freeze.150.lm) # ns
summary(air.freeze.0.lm) # ns
summary(air.freeze.neg0.8.lm) # b0 = -0.04879, p = 0.0279
summary(air.freeze.neg1.5.lm) # b0 = -0.02793, p = 0.0331
summary(air.freeze.neg2.3.lm) # b0 = -0.016373, p = 0.0501
summary(air.freeze.neg3.0.lm) # b0 = -0.012778, p = 0.0522
summary(air.freeze.neg6.1.lm) # ns
summary(air.freeze.neg9.1.lm) # ns
summary(air.freeze.neg12.2.lm) # b0 = 0.006661, p = 0.00549
summary(air.freeze.neg15.0.lm) # b0 = 0.006227, p = 0.0120


## Export at 5 x 7


col.BR <- colorRampPalette(c("red", "blue"))(8)

# Thawed
# Aboveground
par(oma = c(2,2,2,1), mar = c(2,2,0.5,0))
par(mfrow = c(2,1))
plot(air.temps.thaw$year, air.temps.thaw$air.pp.15, type = "l", ylim = c(7,14),  ylab = "", xlab = "",  lwd = 2, lty = 1, col = "red")
lines(air.temps.thaw$year, air.temps.thaw$air.pp.0, ylab = "", xlab = "", lwd = 2, lty = 1, col = "blue")
legend("topleft","Thaw season - aboveground", bty = "n")
legend("topright", legend = c("T air","T soil"), lty = 1, lwd = 2, col = c("red","blue"),
       bty = "n", pt.cex = c(1,1), text.width = 0.1, y.intersp = 0.5, inset = c(0.09,0))

matplot(air.temps.thaw$year, air.temps.thaw[,c(5:12)], type = "l", ylim = c(-0.9,-0.3), ylab = "", xlab = "",
        lwd = 2, lty = 1, col = col.BR)
legend("bottomleft","Thaw season - subsurface", bty = "n")
mtext(side = 2, "Temperature (°C)", outer = T, line = 0.5)
mtext(side = 1, "Year", outer = T, line = 0.5)

par(xpd = NA)
legend(2005,0.66, legend = c("-0.8m","-1.5m","-2.3m","-3.0m"), 
       lty = 1, lwd = 2, col = col.BR[1:4], bty = "n", pt.cex = c(0.75,0.75), text.width = 1.5, 
       inset = c(0,0), horiz = T)
legend(2005,0.62, legend = c("6.0m","-9.1m","-12.2m","-15.0m"), 
       lty = 1, lwd = 2, col = col.BR[5:8], bty = "n", pt.cex = c(0.75,0.75), text.width = 1.5, 
       inset = c(0,0), horiz = T)

# Frozen
# Aboveground
par(oma = c(2,2,2,1), mar = c(2,2,0.5,0))
par(mfrow = c(2,1))
plot(air.temps.freeze$year, air.temps.freeze$air.pp.15, type = "l", ylim = c(-20,0),  ylab = "", xlab = "",  lwd = 2, lty = 1, col = "red")
lines(air.temps.freeze$year, air.temps.freeze$air.pp.0, ylab = "", xlab = "", lwd = 2, lty = 1, col = "blue")
legend("topleft","Freeze season - aboveground", bty = "n")
legend("topright", legend = c("T air","T soil"), lty = 1, lwd = 2, col = c("red","blue"),
       bty = "n", pt.cex = c(1,1), text.width = 0.1, y.intersp = 0.5, inset = c(0.09,0))

matplot(air.temps.freeze$year, air.temps.freeze[,c(5:12)], type = "l", ylim = c(-0.9,0), ylab = "", xlab = "",
        lwd = 2, lty = 1, col = col.BR)
legend("bottomleft","Freeze season - subsurface", bty = "n")
mtext(side = 2, "Temperature (°C)", outer = T, line = 0.5)
mtext(side = 1, "Year", outer = T, line = 0.5)

par(xpd = NA)
legend(2005,1.45, legend = c("-0.8m","-1.5m","-2.3m","-3.0m"), 
       lty = 1, lwd = 2, col = col.BR[1:4], bty = "n", pt.cex = c(0.75,0.75), text.width = 1.5, 
       inset = c(0,0), horiz = T)

legend(2005,1.39, legend = c("6.0m","-9.1m","-12.2m","-15.0m"), 
       lty = 1, lwd = 2, col = col.BR[5:8], bty = "n", pt.cex = c(0.75,0.75), text.width = 1.5, 
       inset = c(0,0), horiz = T)









write.csv(pp2, "~/Desktop/pp2.csv")
