setwd("~/Desktop/Workspace/Earthwatch")

###

rm(list = ls())

## Regressions for gap filling the met station data

micro <- read.csv('microclimate.csv', header = TRUE)

bsw0.jan <- lm(micro[micro$month==1, 10] ~ micro[micro$month==1, 11] + micro[micro$month==1, 17]) # Jan
bsw0.feb <- lm(micro[micro$month==2, 10] ~ micro[micro$month==2, 11] + micro[micro$month==2, 17]) # Feb
bsw0.mar <- lm(micro[micro$month==3, 10] ~ micro[micro$month==3, 11] + micro[micro$month==3, 17]) # Mar
bsw0.apr <- lm(micro[micro$month==4, 10] ~ micro[micro$month==4, 11]) # Apr
bsw0.may <- lm(micro[micro$month==5, 10] ~ micro[micro$month==5, 11]) # May
bsw0.jun <- lm(micro[micro$month==6, 10] ~ micro[micro$month==6, 11]) # Jun
bsw0.jul <- lm(micro[micro$month==7, 10] ~ micro[micro$month==7, 11]) # Jul
bsw0.aug <- lm(micro[micro$month==8, 10] ~ micro[micro$month==8, 11]) # Aug
bsw0.sep <- lm(micro[micro$month==9, 10] ~ micro[micro$month==9, 11]) # Sep
bsw0.oct <- lm(micro[micro$month==10, 10] ~ micro[micro$month==10, 11]) # Oct
bsw0.nov <- lm(micro[micro$month==11, 10] ~ micro[micro$month==11, 11] + micro[micro$month==11, 17]) # Nov
bsw0.dec <- lm(micro[micro$month==12, 10] ~ micro[micro$month==12, 11] + + micro[micro$month==12, 17]) # Dec
summary(bsw0.jan)
summary(bsw0.feb)
summary(bsw0.mar)
summary(bsw0.apr)
summary(bsw0.may)
summary(bsw0.jun)
summary(bsw0.jul)
summary(bsw0.aug)
summary(bsw0.sep)
summary(bsw0.oct)
summary(bsw0.nov)
summary(bsw0.dec)
bsw0.jun2 <- lm(micro[micro$month==6, 10] ~ micro[micro$month==6, 11]) # Jun
summary(bsw0.jun2)

bsw15.jan <- lm(micro[micro$month==1, 11] ~ micro[micro$month==1, 17]) # Jan
bsw15.feb <- lm(micro[micro$month==2, 11] ~ micro[micro$month==2, 17]) # Feb
bsw15.mar <- lm(micro[micro$month==3, 11] ~ micro[micro$month==3, 17]) # Mar
bsw15.apr <- lm(micro[micro$month==4, 11] ~ micro[micro$month==4, 17]) # Apr
bsw15.may <- lm(micro[micro$month==5, 11] ~ micro[micro$month==5, 17]) # May
bsw15.jun <- lm(micro[micro$month==6, 11] ~ micro[micro$month==6, 17]) # Jun
bsw15.jul <- lm(micro[micro$month==7, 11] ~ micro[micro$month==7, 17]) # Jul
bsw15.aug <- lm(micro[micro$month==8, 11] ~ micro[micro$month==8, 17]) # Aug
bsw15.sep <- lm(micro[micro$month==9, 11] ~ micro[micro$month==9, 17]) # Sep
bsw15.oct <- lm(micro[micro$month==10, 11] ~ micro[micro$month==10, 17]) # Oct
bsw15.nov <- lm(micro[micro$month==11, 11] ~ micro[micro$month==11, 17]) # Nov
bsw15.dec <- lm(micro[micro$month==12, 11] ~ micro[micro$month==12, 17]) # Dec
summary(bsw15.jan)
summary(bsw15.feb)
summary(bsw15.mar)
summary(bsw15.apr)
summary(bsw15.may)
summary(bsw15.jun)
summary(bsw15.jul)
summary(bsw15.aug)
summary(bsw15.sep)
summary(bsw15.oct)
summary(bsw15.nov)
summary(bsw15.dec)
bsw15.jun2 <- lm(micro[micro$month==6, 11] ~ micro[micro$month==6, 23] + micro[micro$month==6, 25]) # Jun
summary(bsw15.jun2)

bsw8.jan <- lm(micro[micro$month==1, 12] ~ micro[micro$month==1, 10] + micro[micro$month==1, 18]) # Jan
bsw8.feb <- lm(micro[micro$month==2, 12] ~ micro[micro$month==2, 10] + micro[micro$month==2, 18]) # Feb
bsw8.mar <- lm(micro[micro$month==3, 12] ~ micro[micro$month==3, 10] + micro[micro$month==3, 18]) # Mar
bsw8.apr <- lm(micro[micro$month==4, 12] ~ micro[micro$month==4, 10] + micro[micro$month==4, 18]) # Apr
bsw8.may <- lm(micro[micro$month==5, 12] ~ micro[micro$month==5, 10] + micro[micro$month==5, 18]) # May
bsw8.jun <- lm(micro[micro$month==6, 12] ~ micro[micro$month==6, 10] + micro[micro$month==6, 18]) # Jun
bsw8.jul <- lm(micro[micro$month==7, 12] ~ micro[micro$month==7, 10] + micro[micro$month==7, 18]) # Jul
bsw8.aug <- lm(micro[micro$month==8, 12] ~ micro[micro$month==8, 10] + micro[micro$month==8, 18]) # Aug
bsw8.sep <- lm(micro[micro$month==9, 12] ~ micro[micro$month==9, 10] + micro[micro$month==9, 18]) # Sep
bsw8.oct <- lm(micro[micro$month==10, 12] ~ micro[micro$month==10, 10] + micro[micro$month==10, 18]) # Oct
bsw8.nov <- lm(micro[micro$month==11, 12] ~ micro[micro$month==11, 10] + micro[micro$month==11, 18]) # Nov
bsw8.dec <- lm(micro[micro$month==12, 12] ~ micro[micro$month==12, 10] + micro[micro$month==12, 18]) # Dec
summary(bsw8.jan)
summary(bsw8.feb)
summary(bsw8.mar)
summary(bsw8.apr)
summary(bsw8.may)
summary(bsw8.jun)
summary(bsw8.jul)
summary(bsw8.aug)
summary(bsw8.sep)
summary(bsw8.oct)
summary(bsw8.nov)
summary(bsw8.dec)
bsw8.jun2 <- lm(micro[micro$month==6, 12] ~ micro[micro$month==6, 10] + micro[micro$month==6, 24]) # Jun
summary(bsw8.jun2)

wsu15.jun <- lm(micro[micro$month==6, 17] ~ micro[micro$month==6, 11]) # Jun
summary(wsu15.jun)
wsu0.jun <- lm(micro[micro$month==6, 16] ~ micro[micro$month==6, 22]+micro[micro$month==6, 24]) # Jun
summary(wsu0.jun)
wsu8.jun <- lm(micro[micro$month==6, 18] ~ micro[micro$month==6, 12]+micro[micro$month==6, 16]) # Jun
summary(wsu8.jun)
