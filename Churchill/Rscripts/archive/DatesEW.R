library(dplyr)      # Remove duplicate rows
library(lubridate)  # Handle dates

rm(list = ls())

## Make a date data frame to use for merging 24 hour data
date.24 <- seq(as.Date("2000/1/1"), as.Date("2019/12/31"), "days")
years.24 = c(rep(2000, each = 366), rep(2001, each = 365), rep(2002, each = 365), rep(2003, each = 365), 
            rep(2004, each = 366), rep(2005, each = 365), rep(2006, each = 365), rep(2007, each = 365), 
            rep(2008, each = 366), rep(2009, each = 365), rep(2010, each = 365), rep(2011, each = 365), 
            rep(2012, each = 366), rep(2013, each = 365), rep(2014, each = 365), rep(2015, each = 365), 
            rep(2016, each = 366), rep(2017, each = 365), rep(2018, each = 365), rep(2019, each = 365))
month.24 <- month(as.POSIXlt(date.24, format="%d/%m/%Y"))
day.24 <- as.POSIXlt(date.24)$mday
jd.24 <- as.POSIXlt(date.24, format = "%d%b%y")


ew.dates.24 <- cbind.data.frame(date = date.24,
                               years = years.24,
                               month = month.24,
                               day = day.24,
                               jd = jd.24$yday+1)

ew.dates.24$SampleID <- paste(ew.dates.24$year, ew.dates.24$jd, sep = "-")

air.24 <- read.csv("~/Desktop/Workspace/Earthwatch/AIR_24_align.csv", header = T)
bfr.24 <- read.csv("~/Desktop/Workspace/Earthwatch/BFR_24_align.csv", header = T)
bsw.24 <- read.csv("~/Desktop/Workspace/Earthwatch/BSW_24_align.csv", header = T)
bwp.24 <- read.csv("~/Desktop/Workspace/Earthwatch/BWP_24_align.csv", header = T)
fen.24 <- read.csv("~/Desktop/Workspace/Earthwatch/FEN_24_align.csv", header = T)
pfr.24 <- read.csv("~/Desktop/Workspace/Earthwatch/PFR_24_align.csv", header = T)
ppa.24 <- read.csv("~/Desktop/Workspace/Earthwatch/PPA_24_align.csv", header = T)
ppd.24 <- read.csv("~/Desktop/Workspace/Earthwatch/PPD_24_align.csv", header = T)
tis.24 <- read.csv("~/Desktop/Workspace/Earthwatch/TIS_24_align.csv", header = T)
tun.24 <- read.csv("~/Desktop/Workspace/Earthwatch/TUN_24_align.csv", header = T)
wsu.24 <- read.csv("~/Desktop/Workspace/Earthwatch/WSU_24_align.csv", header = T)

# Remove duplicate rows of the dataframe using cyl and vs variables
sum(duplicated(air.24$SampleID)) # 0 duplicate rows
sum(duplicated(bfr.24$SampleID)) # 0 duplicate rows
sum(duplicated(bsw.24$SampleID)) # 0 duplicate rows
sum(duplicated(bwp.24$SampleID)) # 0 duplicate rows
sum(duplicated(fen.24$SampleID)) # 0 duplicate rows
sum(duplicated(pfr.24$SampleID)) # 0 duplicate rows
sum(duplicated(ppa.24$SampleID)) # 19 duplicate rows
sum(duplicated(ppd.24$SampleID)) # 0 duplicate rows
sum(duplicated(tis.24$SampleID)) # 0 duplicate rows
sum(duplicated(tun.24$SampleID)) # 1 duplicate rows
sum(duplicated(wsu.24$SampleID)) # 1 duplicate rows
air.24 <- distinct(air.24, SampleID, .keep_all= TRUE)
bfr.24 <- distinct(bfr.24, SampleID, .keep_all= TRUE)
bsw.24 <- distinct(bsw.24, SampleID, .keep_all= TRUE)
bwp.24 <- distinct(bwp.24, SampleID, .keep_all= TRUE)
fen.24 <- distinct(fen.24, SampleID, .keep_all= TRUE)
pfr.24 <- distinct(pfr.24, SampleID, .keep_all= TRUE)
ppa.24 <- distinct(ppa.24, SampleID, .keep_all= TRUE)
ppd.24 <- distinct(ppd.24, SampleID, .keep_all= TRUE)
tis.24 <- distinct(tis.24, SampleID, .keep_all= TRUE)
tun.24 <- distinct(tun.24, SampleID, .keep_all= TRUE)
wsu.24 <- distinct(wsu.24, SampleID, .keep_all= TRUE)

air.24.merge <- merge(ew.dates.24, air.24, by = "SampleID", all.x = T)
bfr.24.merge <- merge(ew.dates.24, bfr.24, by = "SampleID", all.x = T)
bsw.24.merge <- merge(ew.dates.24, bsw.24, by = "SampleID", all.x = T)
bwp.24.merge <- merge(ew.dates.24, bwp.24, by = "SampleID", all.x = T)
fen.24.merge <- merge(ew.dates.24, fen.24, by = "SampleID", all.x = T)
pfr.24.merge <- merge(ew.dates.24, pfr.24, by = "SampleID", all.x = T)
ppa.24.merge <- merge(ew.dates.24, ppa.24, by = "SampleID", all.x = T)
ppd.24.merge <- merge(ew.dates.24, ppd.24, by = "SampleID", all.x = T)
tis.24.merge <- merge(ew.dates.24, tis.24, by = "SampleID", all.x = T)
tun.24.merge <- merge(ew.dates.24, tun.24, by = "SampleID", all.x = T)
wsu.24.merge <- merge(ew.dates.24, wsu.24, by = "SampleID", all.x = T)


write.csv(air.24.merge, "~/Desktop/Workspace/Earthwatch/AIR_24_merge.csv")
write.csv(bfr.24.merge, "~/Desktop/Workspace/Earthwatch/BFR_24_merge.csv")
write.csv(bsw.24.merge, "~/Desktop/Workspace/Earthwatch/BSW_24_merge.csv")
write.csv(bwp.24.merge, "~/Desktop/Workspace/Earthwatch/BWP_24_merge.csv")
write.csv(fen.24.merge, "~/Desktop/Workspace/Earthwatch/FEN_24_merge.csv")
write.csv(pfr.24.merge, "~/Desktop/Workspace/Earthwatch/PFR_24_merge.csv")
write.csv(ppa.24.merge, "~/Desktop/Workspace/Earthwatch/PPA_24_merge.csv")
write.csv(ppd.24.merge, "~/Desktop/Workspace/Earthwatch/PPD_24_merge.csv")
write.csv(tis.24.merge, "~/Desktop/Workspace/Earthwatch/TIS_24_merge.csv")
write.csv(tun.24.merge, "~/Desktop/Workspace/Earthwatch/TUN_24_merge.csv")
write.csv(wsu.24.merge, "~/Desktop/Workspace/Earthwatch/WSU_24_merge.csv")

############################################################
############################################################
############################################################
############################################################

## Make a date data frame to use for merging 4 hour data
test <- seq(as.Date("2000/1/1"), as.Date("2019/12/31"), "days")
date.4 <- rep(test, each = 6)
years.4 = c(rep(2000, each = 2196), rep(2001, each = 2190), rep(2002, each = 2190), rep(2003, each = 2190), 
            rep(2004, each = 2196), rep(2005, each = 2190), rep(2006, each = 2190), rep(2007, each = 2190), 
            rep(2008, each = 2196), rep(2009, each = 2190), rep(2010, each = 2190), rep(2011, each = 2190), 
            rep(2012, each = 2196), rep(2013, each = 2190), rep(2014, each = 2190), rep(2015, each = 2190), 
            rep(2016, each = 2196), rep(2017, each = 2190), rep(2018, each = 2190), rep(2019, each = 2190))
month.4 <- month(as.POSIXlt(date.4, format="%d/%m/%Y"))
day.4 <- as.POSIXlt(date.4)$mday
hour.4 <- c(rep(seq(400,2400,400),366), rep(seq(400,2400,400),365), rep(seq(400,2400,400),365), rep(seq(400,2400,400),365), rep(seq(400,2400,400),366),
            rep(seq(400,2400,400),365), rep(seq(400,2400,400),365), rep(seq(400,2400,400),365), rep(seq(400,2400,400),366), rep(seq(400,2400,400),365),
            rep(seq(400,2400,400),365), rep(seq(400,2400,400),365), rep(seq(400,2400,400),366), rep(seq(400,2400,400),365), rep(seq(400,2400,400),365), 
            rep(seq(400,2400,400),365), rep(seq(400,2400,400),366), rep(seq(400,2400,400),365), rep(seq(400,2400,400),365), rep(seq(400,2400,400),365))
jd.4 <- as.POSIXlt(date.4, format = "%d%b%y")


ew.dates.4 <- cbind.data.frame(date = date.4,
                             years = years.4,
                             month = month.4,
                             day = day.4,
                             hour = hour.4, 
                             jd = jd.4$yday+1)
                 
ew.dates.4$SampleID <- paste(ew.dates.4$year, ew.dates.4$jd, ew.dates.4$hour, sep = ".")
 

# ppd.4 <- read.csv("~/Desktop/Workspace/Earthwatch/PPD_align.csv", header = T, stringsAsFactors = F)
# tis.4 <- read.csv("~/Desktop/Workspace/Earthwatch/TIS_align.csv", header = T, stringsAsFactors = F)
# tun.4 <- read.csv("~/Desktop/Workspace/Earthwatch/TUN_align.csv", header = T, stringsAsFactors = F)
wsu.4 <- read.csv("~/Desktop/Workspace/Earthwatch/WSU_align.csv", header = T, stringsAsFactors = F)

# ppd.4.merge <- merge(ew.dates.4, ppd.4, by = "SampleID", all.x = T)
# tis.4.merge <- merge(ew.dates.4, tis.4, by = "SampleID", all.x = T)
# tun.4.merge <- merge(ew.dates.4, tun.4, by = "SampleID", all.x = T)
wsu.4.merge <- merge(ew.dates.4, wsu.4, by = "SampleID", all.x = T)

# write.csv(tis.4.merge, "~/Desktop/Workspace/Earthwatch/TIS_merge.csv")
# write.csv(tun.4.merge, "~/Desktop/Workspace/Earthwatch/TUN_merge.csv")
write.csv(wsu.4.merge, "~/Desktop/Workspace/Earthwatch/WSU_merge.csv")



