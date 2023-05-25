library(tidyverse)
library(broom)
library(vegan)
library(dplR)
library(wesanderson)
# library(plyr)

rm(list = ls())

# Read in the tree
ages <- read.csv("~/Desktop/Workspace/Archive/treeisland/Tree_age_histograms.csv", header = T)
recruits <- read.csv("~/Desktop/Workspace/Churchill/IPY_recruitment_residuals.csv")
ipy_raw <- read.csv("~/Desktop/Workspace/Churchill/IPY_recruitment_raw.csv")
ipy_raw[ipy_raw$Trees.Saps >= 1700,]

sum(62, 4, 48, 1, 42, 54, 21, 17, 23, 2, 35, 9, 195)

# h1 <- hist(ti_raw$doe,
#      col = 'skyblue3',
#      breaks = c(seq(round(min(ti_raw$doe),-1), round(max(ti_raw$doe),-1), 10), 2020))
# 
# plot(h1$density, type = "l")

##*******************
## Age structures

ages1 <- decostand(ages[,c(6,7)], "total", MARGIN = 2)*100
ages1 <- as.data.frame(cbind(ages[,1],ages1))
names(ages1)[1] <- "decade"

islands <- ages1 %>%
  mutate(island = island.trees + island.treelings) %>%
  select(-c(2:3)) %>%
  filter(decade > 1730)

ipy_raw1 <- decostand(ipy_raw[,-1], "total", MARGIN = 2, na.rm = T)*100
ipy_raw1 <- as.data.frame(cbind(ipy_raw[,1],ipy_raw1))
names(ipy_raw1)[1] <- "decade"

ipy <- ipy_raw1 %>%
  mutate(Fpg = BLKFpg + RIDFpg + ROKFpg,
         Xpg = BLKXpg + RIDXpg + ROKXpg,
         Fpm = BLKFpm,
         Xpm = BLKXpm,
         Fll = BLKFll + RIDFll,
         Xll = BLKXll + RIDXll) %>%
  select(-c(2:13)) %>%
  filter(decade > 1730)

age.df <- merge(ipy, islands, by = "decade")

# Export at 3.5 x 4
par(mfrow = c(3,1), mar = c(0.5,1,1,1), oma = c(3,3,0,0))
barplot(t(age.df[,8]), ylim = c(0,100), xaxt = "n", axes = F, col = "white", beside = F)
box()
axis(side = 1, at = seq(0.7,37.9,6), labels = NA)
axis(side = 2, at = seq(0,100,25))
legend("topleft", "a) Tree islands", bty = "n")
legend("topright", legend = c("White spruce", "Black spruce", "Larch"), x.intersp = 0.5, 
       fill = c("white","black","gray"), bty = "n", inset = c(0.2,0))
# legend("topright", legend = c(expression(paste("Trees (">="2 m)")), expression(paste("Treelings ("<"2 m)"))), 
#        x.intersp = -0.01, fill = c("gray","white"), bty = "n", inset = c(0.1,0))
barplot(t(age.df[,c(3,5,7)]), ylim = c(0,100), xaxt = "n", axes = F, col = c("white","black","gray"), beside = F)
box()
axis(side = 1, at = seq(0.7,37.9,6), labels = NA)
axis(side = 2, at = seq(0,100,25))
legend("topleft", "b) Treeline", bty = "n")

barplot(t(age.df[,c(2,4,6)]), ylim = c(0,100), xaxt = "n", axes = F, col = c("white","black","gray"), beside = F)
# barplot(t(ages2[,c(6,7)]), ylim = c(0,42), xaxt = "n", axes = F, col = c("gray","white"), beside = F)
box()
axis(side = 1, at = seq(0.7,37.9,6), labels = seq(1700,2000,50))
axis(side = 2, at = seq(0,100,25))
legend("topleft", "c) Forest", bty = "n")
mtext(side = 1, "Decade", outer = T, line = 1.75, cex = 0.7)
mtext(side = 2, "Recruitment (% of total)", outer = T, line = 1.5, cex = 0.7)

##*******************
## Ring widths

sum(dim(t10)[2],
    dim(t11)[2],
    dim(t12)[2],
    dim(t13)[2],
    dim(t14)[2],
    dim(t15)[2]) + 
  sum(46, 51, 35, 21, 32, 17, 21, 19, 8)

sum(29, 33, 21, 11, 21, 15, 17, 15, 8)

t10 <- read.csv("~/Desktop/Workspace/Churchill/T10_cores_2011.csv")
t11 <- read.csv("~/Desktop/Workspace/Churchill/T11_cores_2011.csv")
t12 <- read.csv("~/Desktop/Workspace/Churchill/T12_cores_2011.csv")
t13 <- read.csv("~/Desktop/Workspace/Churchill/T13_cores_2011.csv")
t14 <- read.csv("~/Desktop/Workspace/Churchill/T14_cores_2011.csv")
t15 <- read.csv("~/Desktop/Workspace/Churchill/TIS_cores_2004.csv")
t10.po <- read.csv("~/Desktop/Workspace/Churchill/t10.po.csv")
t11.po <- read.csv("~/Desktop/Workspace/Churchill/t11.po.csv")
t12.po <- read.csv("~/Desktop/Workspace/Churchill/t12.po.csv")
t13.po <- read.csv("~/Desktop/Workspace/Churchill/t13.po.csv")
t14.po <- read.csv("~/Desktop/Workspace/Churchill/t14.po.csv")
t15.po <- read.csv("~/Desktop/Workspace/Churchill/TIS.po.csv")
ipy.bai <- read.csv("~/Desktop/Workspace/Churchill/IPY_bai.csv")

matplot(t10[,-1], type = "l")
matplot(t11[,-1], type = "l")
matplot(t12[,-1], type = "l")
matplot(t13[,-1], type = "l")
matplot(t14[,-1], type = "l")
matplot(t15[,-1], type = "l")

t10.po$pith.offset[t10.po$pith.offset == 0 | is.na(t10.po$pith.offset)] <- 1
t11.po$pith.offset[t11.po$pith.offset == 0 | is.na(t11.po$pith.offset)] <- 1
t12.po$pith.offset[t12.po$pith.offset == 0 | is.na(t12.po$pith.offset)] <- 1
t13.po$pith.offset[t13.po$pith.offset == 0 | is.na(t13.po$pith.offset)] <- 1
t14.po$pith.offset[t14.po$pith.offset == 0 | is.na(t14.po$pith.offset)] <- 1
t15.po$pith.offset[t15.po$pith.offset == 0 | is.na(t15.po$pith.offset)] <- 1

t10.wc <- po.to.wc(t10.po)
t11.wc <- po.to.wc(t11.po)
t12.wc <- po.to.wc(t12.po)
t13.wc <- po.to.wc(t13.po)
t14.wc <- po.to.wc(t14.po)
t15.wc <- po.to.wc(t15.po)

x <- rownames(t10.wc)
x[grepl("^[[:digit:]]", x)] <- paste("X", x[grepl("^[[:digit:]]", x)], sep = "")
t10.wc <- data.frame(series = x, d2pith = t10.wc)
x <- rownames(t11.wc)
x[grepl("^[[:digit:]]", x)] <- paste("X", x[grepl("^[[:digit:]]", x)], sep = "")
t11.wc <- data.frame(series = x, d2pith = t11.wc)
x <- rownames(t12.wc)
x[grepl("^[[:digit:]]", x)] <- paste("X", x[grepl("^[[:digit:]]", x)], sep = "")
t12.wc <- data.frame(series = x, d2pith = t12.wc)
x <- rownames(t13.wc)
x[grepl("^[[:digit:]]", x)] <- paste("X", x[grepl("^[[:digit:]]", x)], sep = "")
t13.wc <- data.frame(series = x, d2pith = t13.wc)
x <- rownames(t14.wc)
x[grepl("^[[:digit:]]", x)] <- paste("X", x[grepl("^[[:digit:]]", x)], sep = "")
t14.wc <- data.frame(series = x, d2pith = t14.wc)
x <- rownames(t15.wc)
x[grepl("^[[:digit:]]", x)] <- paste("X", x[grepl("^[[:digit:]]", x)], sep = "")
t15.wc <- data.frame(series = x, d2pith = t15.wc)

x <- t10.po$series
x[grepl("^[[:digit:]]", x)] <- paste("X", x[grepl("^[[:digit:]]", x)], sep = "")
t10.po <- data.frame(series = x, pith.offset = t10.po$pith.offset)
x <- t11.po$series
x[grepl("^[[:digit:]]", x)] <- paste("X", x[grepl("^[[:digit:]]", x)], sep = "")
t11.po <- data.frame(series = x, pith.offset = t11.po$pith.offset)
x <- t12.po$series
x[grepl("^[[:digit:]]", x)] <- paste("X", x[grepl("^[[:digit:]]", x)], sep = "")
t12.po <- data.frame(series = x, pith.offset = t12.po$pith.offset)
x <- t13.po$series
x[grepl("^[[:digit:]]", x)] <- paste("X", x[grepl("^[[:digit:]]", x)], sep = "")
t13.po <- data.frame(series = x, pith.offset = t13.po$pith.offset)
x <- t14.po$series
x[grepl("^[[:digit:]]", x)] <- paste("X", x[grepl("^[[:digit:]]", x)], sep = "")
t14.po <- data.frame(series = x, pith.offset = t14.po$pith.offset)
x <- t15.po$series
x[grepl("^[[:digit:]]", x)] <- paste("X", x[grepl("^[[:digit:]]", x)], sep = "")
t15.po <- data.frame(series = x, pith.offset = t15.po$pith.offset)

names(t14) <- gsub("..", ".", names(t14), fixed = TRUE)
identical(sort(names(t14)[-1]), sort(t14.po$series))

names(t15) <- gsub("..", ".", names(t15), fixed = TRUE)
t15.po$series[c(2,3)] <- c("X.a","X.b")
identical(sort(names(t15)[-1]), sort(t15.po$series))

t10.yrs <- t10$Year; t10 <- t10[,t10.po$series]; rownames(t10) <- t10.yrs
t11.yrs <- t11$Year; t11 <- t11[,t11.po$series]; rownames(t11) <- t11.yrs
t12.yrs <- t12$Year; t12 <- t12[,t12.po$series]; rownames(t12) <- t12.yrs
t13.yrs <- t13$Year; t13 <- t13[,t13.po$series]; rownames(t13) <- t13.yrs
t14.yrs <- t14$Year; t14 <- t14[,t14.po$series]; rownames(t14) <- t14.yrs
t15.yrs <- t15$Year; t15 <- t15[,t15.po$series]; rownames(t15) <- t15.yrs

# t10.bai <- bai.in(rwl = t10, d2pith = t10.wc)
# t11.bai <- bai.in(rwl = t11, d2pith = t11.wc)
# t12.bai <- bai.in(rwl = t12, d2pith = t12.wc)
# t13.bai <- bai.in(rwl = t13, d2pith = t13.wc)
# t14.bai <- bai.in(rwl = t14, d2pith = t14.wc)
# t15.bai <- bai.in(rwl = t15, d2pith = t15.wc)

# data(gp.rwl)
# data(gp.po)
# gp.rwi <- cms(rwl = gp.rwl, po = gp.po)

t10 <- as.rwl(t10)
t11 <- as.rwl(t11)
t12 <- as.rwl(t12)
t13 <- as.rwl(t13)
t14 <- as.rwl(t14)
t15 <- as.rwl(t15)



t10.bai <- cms(rwl = t10, po = t10.po)
t11.bai <- cms(rwl = t11, po = t11.po)
t12.bai <- cms(rwl = t12, po = t12.po)
t13.bai <- cms(rwl = t13, po = t13.po)
t14.bai <- cms(rwl = t14, po = t14.po)
t15.bai <- cms(rwl = t15, po = t15.po)

t10.crn <- chron(t10.bai)
t11.crn <- chron(t11.bai)
t12.crn <- chron(t12.bai)
t13.crn <- chron(t13.bai)
t14.crn <- chron(t14.bai)
t15.crn <- chron(t15.bai)
t15.crn <- t15.crn[-c(1:23),]

t10.bai <- data.frame(year = rownames(t10.bai), t10.bai)
t11.bai <- data.frame(year = rownames(t11.bai), t11.bai)
t12.bai <- data.frame(year = rownames(t12.bai), t12.bai)
t13.bai <- data.frame(year = rownames(t13.bai), t13.bai)
t14.bai <- data.frame(year = rownames(t14.bai), t14.bai)
t15.bai <- data.frame(year = rownames(t15.bai), t15.bai)

t10 <- data.frame(year = rownames(t10), t10)
t11 <- data.frame(year = rownames(t11), t11)
t12 <- data.frame(year = rownames(t12), t12)
t13 <- data.frame(year = rownames(t13), t13)
t14 <- data.frame(year = rownames(t14), t14)
t15 <- data.frame(year = rownames(t15), t15)

all.ti.bai <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "year", all.x = T),
                     list(t10.bai,
                          t11.bai,
                          t12.bai,
                          t13.bai,
                          t14.bai,
                          t15.bai))

all.ti.rw <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "year", all.x = T),
                    list(t10,
                         t11,
                         t12,
                         t13,
                         t14,
                         t15))

rownames(all.ti.rw) <- all.ti.rw$year
all.ti.rw <- all.ti.rw[,!(colSums(is.na(all.ti.rw[,-1])) / nrow(all.ti.rw) == 1)]
all.ti.rw <- all.ti.rw %>%
  select_if(., is.numeric)

rs <- corr.rwl.seg(all.ti.rw[,-1], seg.length = 100, label.cex = 1.25)
to.keep <- rownames(rs$p.val[is.na(rs$p.val[,3]),])
all.ti.rw <- all.ti.rw[,to.keep]

all.ti.po <- rbind.data.frame(t10.po,t11.po,t12.po,t13.po,t14.po,t15.po)
all.ti.rw <- all.ti.rw[,colSums(is.na(all.ti.rw))<nrow(all.ti.rw)]


all.ti.po <- all.ti.po[all.ti.po$series %in% names(all.ti.rw),]
all.ti.rw <- as.rwl(all.ti.rw)

identical(names(all.ti.rw), all.ti.po$series)
all.ti.cms <- cms(rwl = all.ti.rw, po = all.ti.po)
all.ti.crn <- chron(all.ti.cms)

# rownames(all.ti.bai) <- all.ti.bai$year
# all.ti.bai$year <- NULL
# all.crn <- chron(all.ti.bai)

ipy.bai_ <- ipy.bai %>%
  rowwise() %>%
  mutate(F_piga = mean(c(RID_Fpg, ROK_Fpg, FRO_Fpg, BLK_Fpg), na.rm = T),
         X_piga = FRO_Xpg,
         F_pima = BLK_Fpm, 
         X_pima = BLM_Xpm,
         F_lala = BLK_Fll,
         X_lala = BLK_Xll) %>%
  select(-c(2:10)) %>%
  relocate(F_piga:X_lala, .before = BLK_Fll_n)


# Export at 3.5 x 4

col_pal <- wes_palette("Rushmore1")
age.sub <- age.df[c(17:26),]
ipy.bai.sub <- ipy.bai_[ipy.bai_$year >= 1900,]
all.ti.cr.sub <- all.ti.crn[rownames(all.ti.crn) %in% c(1900:2010),]


##
jpeg("Tree_dynamics.jpg", height = 5, width = 5, units = "in", res = 300)
par(mfcol = c(3,2), mar = c(0.25,3,1,0.5), oma = c(3,0.5,0,0.5))

plot(age.sub$decade, age.sub$island, type = "n", xlim = c(1900,1988), ylim = c(0,80), axes = F,
     ann=FALSE)
lines(age.sub$decade, age.sub$island, col = col_pal[3], lwd = 1)
lines(age.sub$decade, age.sub$Xpg, col = col_pal[4], lwd = 1.5)
lines(age.sub$decade, age.sub$Fpg, col = col_pal[1], lwd = 2)
box()
axis(1, at = seq(1900,2000,10), labels = F)
axis(2, at = seq(0,100,20), labels = seq(0,100,20)/100)
legend("topright", legend = c("Forest","Treeline","Tree islands"), lwd = c(2,1.5,1), 
       col = col_pal[c(1,4,3)], bty = "n")
legend("topleft", "a) White spruce", bty = "n")

plot(age.sub$decade, age.sub$island, type = "n", xlim = c(1900,1988), ylim = c(0,80), axes = F,
     ann=FALSE)
lines(age.sub$decade, age.sub$Xpm, col = col_pal[4], lwd = 1.5)
lines(age.sub$decade, age.sub$Fpm, col = col_pal[1], lwd = 2)
box()
axis(1, at = seq(1900,2000,10), labels = F)
axis(2, at = seq(0,100,20), labels = seq(0,100,20)/100)
legend("topleft", "b) Black spruce", bty = "n")
mtext("Recruitment (proportion of total)", side = 2, cex = 0.65, outer = T, line = -0.9)

plot(age.sub$decade, age.sub$island, type = "n", xlim = c(1900,1988), ylim = c(0,80), axes = F,
     ann=FALSE)
lines(age.sub$decade, age.sub$Xll, col = col_pal[4], lwd = 1.5)
lines(age.sub$decade, age.sub$Fll, col = col_pal[1], lwd = 2)
box()
axis(1, at = seq(1900,2000,10), labels = T)
axis(2, at = seq(0,100,20), labels = seq(0,100,20)/100)
legend("topleft", "c) Larch", bty = "n")
mtext("Decade", side = 1, cex = 0.65, line = 2)


plot(ipy.bai.sub$year, ipy.bai.sub$F_piga, type = "n", xlim = c(1900,2009), ylim = c(0,4), axes = F,
     ann=FALSE)
lines(rownames(all.ti.cr.sub), all.ti.cr.sub$xxxstd, col = col_pal[3], lwd = 1)
lines(ipy.bai.sub$year, ipy.bai.sub$X_piga, col = col_pal[4], lwd = 1.5)
lines(ipy.bai.sub$year, ipy.bai.sub$F_piga, col = col_pal[1], lwd = 2)
box()
axis(1, at = seq(1900,2010,10), labels = F)
axis(2, at = seq(0,4,1))
legend("topleft", "c) White spruce", bty = "n")

plot(ipy.bai.sub$year, ipy.bai.sub$F_piga, type = "n", xlim = c(1900,2009), ylim = c(0,4), axes = F,
     ann=FALSE)
lines(ipy.bai.sub$year, ipy.bai.sub$X_pima, col = col_pal[4], lwd = 1.5)
lines(ipy.bai.sub$year, ipy.bai.sub$F_pima, col = col_pal[1], lwd = 2)
box()
axis(1, at = seq(1900,2010,10), labels = F)
axis(2, at = seq(0,4,1))
legend("topleft", "d) Black spruce", bty = "n")
mtext("Growth index", side = 2, cex = 0.65, line = 2)

plot(ipy.bai.sub$year, ipy.bai.sub$F_piga, type = "n", xlim = c(1900,2009), ylim = c(0,4), axes = F,
     ann=FALSE)
lines(ipy.bai.sub$year, ipy.bai.sub$X_lala, col = col_pal[4], lwd = 1.5)
lines(ipy.bai.sub$year, ipy.bai.sub$F_lala, col = col_pal[1], lwd = 2)
box()
axis(1, at = seq(1900,2010,10), labels = T)
axis(2, at = seq(0,4,1))
legend("topleft", "e) Larch", bty = "n")
mtext("Year", side = 1, cex = 0.65, line = 2)
dev.off()

##*******************
## Snow
park.snow <- read.csv(file = "~/Desktop/Workspace/Earthwatch/Churchill/data/ParkSnow.csv", header = TRUE)
ipy.snow <- read.csv(file = "~/Desktop/Workspace/Earthwatch/Churchill/data/IPYsnow.csv", header = TRUE)
ltems.snow <- read.csv(file = "~/Desktop/Workspace/Earthwatch/Churchill/data/LTEMSSnow.csv", header = TRUE)
ti.snow <- read.csv(file = "~/Desktop/Workspace/Earthwatch/Churchill/data/TreeIslandSnow.csv", header = TRUE)
levels(as.factor(park.snow$type))

all.snow <- rbind.data.frame(park.snow, ipy.snow, ltems.snow, ti.snow)
all.snow$type <- tolower(all.snow$type)
all.snow$type[all.snow$type %in% c("center",
                                   "centre",
                                   "deflation",
                                   "deflation bay",
                                   "deflation road",
                                   "disturbed",
                                   "drift",
                                   "interior",
                                   "island",
                                   "leeward",
                                   "windward")] <- "island"
all.snow$type[(all.snow$type == "" & (all.snow$site == "BFR" | all.snow$site == "PFR"))] <- "burn"
all.snow$type[(all.snow$type == "" & all.snow$site == "BWP")] <- "gravel"
all.snow$type[(all.snow$type == "" & all.snow$site == "FEN")] <- "fen"
all.snow$type[(all.snow$type == "" & all.snow$site == "TIS")] <- "island"
all.snow$type[(all.snow$type == "" & all.snow$site == "TUN")] <- "tundra"
all.snow$type[all.snow$type == "island"] <- "treeisland"

# [1] "BFR" "BWP" "FEN" "PFR" "TIS" "TUN"

levels(as.factor(all.snow$type))

all.snow.df <-
  all.snow %>% 
  group_by(type) %>% 
  mutate(depth.mean = mean(depth, na.rm = T)) %>% 
  arrange(depth.mean) %>% 
  select(-depth.mean)

all.snow.df$type <- factor(all.snow.df$type,
                           levels = c("polygon",
                                      "tundra",
                                      "beach",
                                      "gravel",
                                      "fen",
                                      "wedge",
                                      "burn",
                                      "forest",
                                      "treeline",
                                      "island",
                                      "shrub"))


all.snow.df$type1 <- plyr::revalue(all.snow.df$type, 
                            c("polygon" = "A",
                              "tundra" = "B",
                              "beach" = "C",
                              "gravel" = "D",
                              "fen" = "E",
                              "wedge" = "F",
                              "burn" = "G",
                              "forest" = "H",
                              "treeline" = "I",
                              "island" = "J",
                              "shrub" = "K"))

all.snow.df <- all.snow.df %>%
  arrange(type1, year)

all.snow.df$plotting <- as.numeric(as.factor(paste(all.snow.df$type1, all.snow.df$year, sep = ".")))

bp_n <- all.snow.df %>%
  group_by(type) %>%
  dplyr::summarise(unique_types = n_distinct(year))
  
all.snow.df %>%
  group_by(type) %>%
  dplyr::summarise(start = min(year))

# Plot all snow data for 2006-2021

# Set the colors for the various groups in the boxplots
colors <- rev(wes_palette("Zissou1", 11, type = "continuous"))
col.pal <- c(rep(colors[01],bp_n$unique_types[01]),
             rep(colors[02],bp_n$unique_types[02]),
             rep(colors[03],bp_n$unique_types[03]),
             rep(colors[04],bp_n$unique_types[04]),
             rep(colors[05],bp_n$unique_types[05]),
             rep(colors[06],bp_n$unique_types[06]),
             rep(colors[07],bp_n$unique_types[07]),
             rep(colors[08],bp_n$unique_types[08]),
             rep(colors[09],bp_n$unique_types[09]),
             rep(colors[10],bp_n$unique_types[10]),
             rep(colors[11],bp_n$unique_types[11]))

# Export at 8 x 6
# par(ps = 10, cex = 1, cex.axis = 1)
jpeg("~/Desktop/Workspace/Earthwatch/Churchill/figures/All_snow.jpg", width = 6, height = 8, units = "in", res = 600)
par(mfrow = c(3, 1))
par(mar = c(0, 2, 1, 1), oma = c(5,1.3,0,0))
# Plot the first boxplot and color rectangles (depth)
par(xpd = F)
bp1 <- boxplot(swe ~ plotting, data = all.snow.df, outline=FALSE, ylim = c(0,1750), col = col.pal, xaxt = "n", type = "n", ann = F)
stripchart(swe ~ all.snow.df$plotting, data = all.snow.df, vertical = T, add = T, pch = 16, cex = 0.3, col = alpha(col.pal,0.5))
axis(1, at = seq(1,182,2),labels = NA, tick = TRUE)
axis(1, at = seq(2,182,2),labels = NA, tick = TRUE, tck = -0.025)
mtext(side=2, "SWE (mm) ", adj=0.5, line=2, cex = 0.65)
legend("topright", "SWE", bty = "n")

# Plot the second boxplot and color rectangles (density)
bp2 <- boxplot(density ~ plotting, data = all.snow.df, outline=FALSE, ylim = c(0,600), col = col.pal, xaxt = "n", type = "n", ann = F)
stripchart(density ~ all.snow.df$plotting, data = all.snow.df, vertical = T, add = T, pch = 16, cex = 0.3, col = alpha(col.pal,0.5))
axis(1, at = seq(1,182,2),labels = NA, tick = TRUE)
axis(1, at = seq(2,182,2),labels = NA, tick = TRUE, tck = -0.025)
mtext(side=2, expression(paste("Density (kg m"^3,")", sep = "")),adj=0.5, line=2, cex = 0.65)
legend("topright", "Snow density", bty = "n")

# Plot the second boxplot and color rectangles (HTC)
bp3 <- boxplot(htc ~ plotting, data = all.snow.df, outline=FALSE, ylim = c(0,40), col = col.pal, xaxt = "n", type = "n", ann = F)
stripchart(htc ~ all.snow.df$plotting, data = all.snow.df, vertical = T, add = T, pch = 16, cex = 0.3, col = alpha(col.pal,0.5))
axis(1, at = seq(1,182,2),labels = NA, tick = TRUE)
axis(1, at = seq(2,182,2),labels = NA, tick = TRUE, tck = -0.025)
mtext(expression(paste("HTC (W m"^"-2"," K"^"-1",")")), side=2, line=2, cex = 0.65)
legend("topright", "Heat Transfer Coefficient", bty = "n")
mtext(side = 1, "Year", line = 0.6, cex = 0.65)

# Now plot the colored bar at the bottom with labels
par(xpd = NA)
rect(1,-8,19,-6, col = colors[1], border = NA)
rect(19,-8,38,-6, col = colors[2], border = NA)
rect(38,-8,51,-6, col = colors[3], border = NA)
rect(51,-8,70,-6, col = colors[4], border = NA)
rect(70,-8,86,-6, col = colors[5], border = NA)
rect(86,-8,105,-6, col = colors[6], border = NA)
rect(105,-8,124,-6, col = colors[7], border = NA)
rect(124,-8,143,-6, col = colors[8], border = NA)
rect(143,-8,150,-6, col = colors[9], border = NA)
rect(150,-8,164,-6, col = colors[10], border = NA)
rect(164,-8,182,-6, col = colors[11], border = NA)
text(mean(c(1,19)),-10,"Polygon")
text(mean(c(19,38)),-10,"Tundra")
text(mean(c(38,51)),-10,"Beach")
text(mean(c(51,70)),-10,"Gravel")
text(mean(c(70,86)),-10,"Fen")
text(mean(c(86,105)),-10,"Wedge")
text(mean(c(105,124)),-10,"Burn")
text(mean(c(124,143)),-10,"Forest")
text(mean(c(143,150)),-10,"Treeline")
text(mean(c(150,164)),-10,"Island")
text(mean(c(164,182)),-10,"Shrub")
dev.off()


