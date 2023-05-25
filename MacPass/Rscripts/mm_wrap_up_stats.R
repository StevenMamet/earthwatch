library(dplyr)
library(lubridate)
library(pscl)
library(lmtest)
library(scales)
library(tidyverse)
library(wesanderson)

rm(list = ls())

setwd("~/Desktop/Workspace/")

# Read in the GTREE data ----
gtree_2021 <- read.csv("./Earthwatch/MacPass/data/GTREE_MM_2021.csv", stringsAsFactors = T)

# Count seedlings from each year
gtree_21 <- gtree_2021 %>%
        group_by(Site, Plot, Cage, Species, Year, Live) %>%
        count(Year)

# write.csv(gtree_21, "~/Desktop/gtree_21.csv")

##**************************
# Post cage-installation data ----
mm <- read.csv(file = "./Earthwatch/MacPass/data/gtree_mm_exclosures.csv", header = TRUE)
mm$germ.prop.via.int <- as.integer(rescale(mm$germ.prop.via, to = c(0,20)))
mm$germ.prop.via.int[is.na(mm$germ.prop.via.int)] <- 0

mm$treat <- as.factor(mm$treat)
mm$scarif <- as.factor(mm$scarif)
mm$seeded <- as.factor(mm$seeded)
mm$plot <- factor(mm$plot)

mm.nalp <- subset(mm, site == "nalp")
mm.salp <- subset(mm, site == "salp")
mm.sshr <- subset(mm, site == "sshr")
mm.scut <- subset(mm, site == "scut")

mm.nalp.fir.seed <- droplevels(subset(mm.nalp, seeded == 1 & species == "fir"))
mm.salp.fir.seed <- droplevels(subset(mm.salp, seeded == 1 & species == "fir"))
mm.sshr.fir.seed <- droplevels(subset(mm.sshr, seeded == 1 & species == "fir"))
mm.scut.fir.seed <- droplevels(subset(mm.scut, seeded == 1 & species == "fir"))

mm.nalp.spruce.seed <- droplevels(subset(mm.nalp, seeded == 1 & species == "spruce"))
mm.salp.spruce.seed <- droplevels(subset(mm.salp, seeded == 1 & species == "spruce"))
mm.sshr.spruce.seed <- droplevels(subset(mm.sshr, seeded == 1 & species == "spruce"))
mm.scut.spruce.seed <- droplevels(subset(mm.scut, seeded == 1 & species == "spruce"))

mm.nalp.fir.ex <- mm.nalp[mm.nalp$species == "fir" & mm.nalp$treatment %in% c("seeded","seeded.scarified") & mm.nalp$exclosure == "yes",]
mm.nalp.fir.no <- mm.nalp[mm.nalp$species == "fir" & mm.nalp$treatment %in% c("seeded","seeded.scarified") & mm.nalp$exclosure == "no",]
mm.salp.fir.ex <- mm.salp[mm.salp$species == "fir" & mm.salp$treatment %in% c("seeded","seeded.scarified") & mm.salp$exclosure == "yes",]
mm.salp.fir.no <- mm.salp[mm.salp$species == "fir" & mm.salp$treatment %in% c("seeded","seeded.scarified") & mm.salp$exclosure == "no",]
mm.sshr.fir.ex <- mm.scut[mm.scut$species == "fir" & mm.scut$treatment %in% c("seeded","seeded.scarified") & mm.scut$exclosure == "yes",]
mm.sshr.fir.no <- mm.scut[mm.scut$species == "fir" & mm.scut$treatment %in% c("seeded","seeded.scarified") & mm.scut$exclosure == "no",]
mm.scut.fir.ex <- mm.sshr[mm.sshr$species == "fir" & mm.sshr$treatment %in% c("seeded","seeded.scarified") & mm.sshr$exclosure == "yes",]
mm.scut.fir.no <- mm.sshr[mm.sshr$species == "fir" & mm.sshr$treatment %in% c("seeded","seeded.scarified") & mm.sshr$exclosure == "no",]

mm.nalp.spruce.ex <- mm.nalp[mm.nalp$species == "spruce" & mm.nalp$treatment %in% c("seeded","seeded.scarified") & mm.nalp$exclosure == "yes",]
mm.nalp.spruce.no <- mm.nalp[mm.nalp$species == "spruce" & mm.nalp$treatment %in% c("seeded","seeded.scarified") & mm.nalp$exclosure == "no",]
mm.salp.spruce.ex <- mm.salp[mm.salp$species == "spruce" & mm.salp$treatment %in% c("seeded","seeded.scarified") & mm.salp$exclosure == "yes",]
mm.salp.spruce.no <- mm.salp[mm.salp$species == "spruce" & mm.salp$treatment %in% c("seeded","seeded.scarified") & mm.salp$exclosure == "no",]
mm.sshr.spruce.ex <- mm.scut[mm.scut$species == "spruce" & mm.scut$treatment %in% c("seeded","seeded.scarified") & mm.scut$exclosure == "yes",]
mm.sshr.spruce.no <- mm.scut[mm.scut$species == "spruce" & mm.scut$treatment %in% c("seeded","seeded.scarified") & mm.scut$exclosure == "no",]
mm.scut.spruce.ex <- mm.sshr[mm.sshr$species == "spruce" & mm.sshr$treatment %in% c("seeded","seeded.scarified") & mm.sshr$exclosure == "yes",]
mm.scut.spruce.no <- mm.sshr[mm.sshr$species == "spruce" & mm.sshr$treatment %in% c("seeded","seeded.scarified") & mm.sshr$exclosure == "no",]

x1a <- factor(mm.nalp.fir.ex[,"treat"], levels = c("2","4"))
x1b <- factor(mm.nalp.fir.no[,"treat"], levels = c("2","4"))
x2a <- factor(mm.salp.fir.ex[,"treat"], levels = c("2","4"))
x2b <- factor(mm.salp.fir.no[,"treat"], levels = c("2","4"))
x3a <- factor(mm.scut.fir.ex[,"treat"], levels = c("2","4"))
x3b <- factor(mm.scut.fir.no[,"treat"], levels = c("2","4"))
x4a <- factor(mm.sshr.fir.ex[,"treat"], levels = c("2","4"))
x4b <- factor(mm.sshr.fir.no[,"treat"], levels = c("2","4"))

##**************************
# Proportion fir germination cage/uncaged ----
jpeg("./Earthwatch/MacPass/figures/fir.germination.jpg", width = 5, height = 7, units = "in", res = 300)
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(0.5, 2, 1, 1), oma = c(2,1,0,0))

## Nalp - Fir out of exclosures
boxplot(mm.nalp.fir.no$germ.prop.via ~ x1b, at = c(1,3), xlim = c(0.4,4.5), ylim = c(0,1.5), 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(mm.nalp.fir.no$germ.prop.via ~ x1b, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## Nalp - Fir in exclosures
boxplot(mm.nalp.fir.ex$germ.prop.via ~ x1a, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm.nalp.fir.ex$germ.prop.via ~ x1a, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
legend("topleft", "(a) North-facing alpine", bty = "n", inset = c(0,0))
legend("topright", c("Uncaged","Caged"), pt.bg = c("red", "blue"), col = "black",
       bty = "n", pch = 22, pt.cex = c(1,1), text.width = 1, 
       y.intersp = 0.7, inset = c(0.1,0.01), horiz = F)

## Salp - Fir out of exclosures
boxplot(mm.salp.fir.no$germ.prop.via ~ x1b, at = c(1,3), xlim = c(0.4,4.5), ylim = c(0,1.5), 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(mm.salp.fir.no$germ.prop.via ~ x1b, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## Salp - Fir in exclosures
boxplot(mm.salp.fir.ex$germ.prop.via ~ x1a, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm.salp.fir.ex$germ.prop.via ~ x1a, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
legend("topleft", "(b) South-facing alpine", bty = "n", inset = c(0,0))

## Scut - Fir out of exclosures
boxplot(mm.scut.fir.no$germ.prop.via ~ x1b, at = c(1,3), xlim = c(0.4,4.5), ylim = c(0,1.5), 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(mm.scut.fir.no$germ.prop.via ~ x1b, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## Scut - Fir in exclosures
boxplot(mm.scut.fir.ex$germ.prop.via ~ x1a, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm.scut.fir.ex$germ.prop.via ~ x1a, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
legend("topleft", "(c) South-facing shrub (cut)", bty = "n", inset = c(0,0))

## Sshr - Fir out of exclosures
boxplot(mm.sshr.fir.no$germ.prop.via ~ x1b, at = c(1,3), xlim = c(0.4,4.5), ylim = c(0,1.5), 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(mm.sshr.fir.no$germ.prop.via ~ x1b, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## sshr - Fir in exclosures
boxplot(mm.sshr.fir.ex$germ.prop.via ~ x1a, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm.sshr.fir.ex$germ.prop.via ~ x1a, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
legend("topleft", "(d) South-facing shrub", bty = "n", inset = c(0,0))

axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
axis(1, at = c(1.5,3.5), labels = c("Vegetated","Scarified"), tick = TRUE)
mtext(side = 2, "Germination proportion of viable", outer = TRUE)
dev.off()

##**************************
# Proportion spruce germination cage/uncaged ----
jpeg("./Earthwatch/MacPass/figures/spruce.germination.jpg", width = 5, height = 7, units = "in", res = 300)
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(0.5, 2, 1, 1), oma = c(2,1,0,0))

## Nalp - Spruce out of exclosures
boxplot(mm.nalp.spruce.no$germ.prop.via ~ x1b, at = c(1,3), xlim = c(0.4,4.5), ylim = c(0,1.5), 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(mm.nalp.spruce.no$germ.prop.via ~ x1b, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## Nalp - Spruce in exclosures
boxplot(mm.nalp.spruce.ex$germ.prop.via ~ x1a, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm.nalp.spruce.ex$germ.prop.via ~ x1a, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
legend("topleft", "(a) North-facing alpine", bty = "n", inset = c(0,0))
legend("topright", c("Uncaged","Caged"), pt.bg = c("red", "blue"), col = "black",
       bty = "n", pch = 22, pt.cex = c(1,1), text.width = 1, 
       y.intersp = 0.7, inset = c(0.1,0.01), horiz = F)

## Salp - Spruce out of exclosures
boxplot(mm.salp.spruce.no$germ.prop.via ~ x1b, at = c(1,3), xlim = c(0.4,4.5), ylim = c(0,1.5), 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(mm.salp.spruce.no$germ.prop.via ~ x1b, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## Salp - Spruce in exclosures
boxplot(mm.salp.spruce.ex$germ.prop.via ~ x1a, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm.salp.spruce.ex$germ.prop.via ~ x1a, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
legend("topleft", "(b) South-facing alpine", bty = "n", inset = c(0,0))

## Scut - Spruce out of exclosures
boxplot(mm.scut.spruce.no$germ.prop.via ~ x1b, at = c(1,3), xlim = c(0.4,4.5), ylim = c(0,1.5), 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(mm.scut.spruce.no$germ.prop.via ~ x1b, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## Scut - Spruce in exclosures
boxplot(mm.scut.spruce.ex$germ.prop.via ~ x1a, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm.scut.spruce.ex$germ.prop.via ~ x1a, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
legend("topleft", "(c) South-facing shrub (cut)", bty = "n", inset = c(0,0))

## Sshr - Spruce out of exclosures
boxplot(mm.sshr.spruce.no$germ.prop.via ~ x1b, at = c(1,3), xlim = c(0.4,4.5), ylim = c(0,1.5), 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(mm.sshr.spruce.no$germ.prop.via ~ x1b, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## sshr - Spruce in exclosures
boxplot(mm.sshr.spruce.ex$germ.prop.via ~ x1a, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm.sshr.spruce.ex$germ.prop.via ~ x1a, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
legend("topleft", "(d) South-facing shrub", bty = "n", inset = c(0,0))

axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
axis(1, at = c(1.5,3.5), labels = c("Vegetated","Scarified"), tick = TRUE)
mtext(side = 2, "Germination proportion of viable", outer = TRUE)
dev.off()

##**************************
# Proportion fir survival cage/uncaged ----
## NALP - fir ----
mm.nalp.fir.no.seed <- data.frame(year = (rep(c(1:5), c(length(mm.nalp.fir.no$surv.prop.0[mm.nalp.fir.no$treatment == "seeded"]),
                                                        length(mm.nalp.fir.no$surv.prop.1[mm.nalp.fir.no$treatment == "seeded"]),
                                                        length(mm.nalp.fir.no$surv.prop.2[mm.nalp.fir.no$treatment == "seeded"]),
                                                        length(mm.nalp.fir.no$surv.prop.3[mm.nalp.fir.no$treatment == "seeded"]),
                                                        length(mm.nalp.fir.no$surv.prop.4[mm.nalp.fir.no$treatment == "seeded"])))),
                                  surv = c(mm.nalp.fir.no$surv.prop.0[mm.nalp.fir.no$treatment == "seeded"],
                                           mm.nalp.fir.no$surv.prop.1[mm.nalp.fir.no$treatment == "seeded"],
                                           mm.nalp.fir.no$surv.prop.2[mm.nalp.fir.no$treatment == "seeded"],
                                           mm.nalp.fir.no$surv.prop.3[mm.nalp.fir.no$treatment == "seeded"],
                                           mm.nalp.fir.no$surv.prop.4[mm.nalp.fir.no$treatment == "seeded"]))
mm.nalp.fir.ex.seed <- data.frame(year = (rep(c(1:5), c(length(mm.nalp.fir.ex$surv.prop.0[mm.nalp.fir.ex$treatment == "seeded"]),
                                                        length(mm.nalp.fir.ex$surv.prop.1[mm.nalp.fir.ex$treatment == "seeded"]),
                                                        length(mm.nalp.fir.ex$surv.prop.2[mm.nalp.fir.ex$treatment == "seeded"]),
                                                        length(mm.nalp.fir.ex$surv.prop.3[mm.nalp.fir.ex$treatment == "seeded"]),
                                                        length(mm.nalp.fir.ex$surv.prop.4[mm.nalp.fir.ex$treatment == "seeded"])))),
                                  surv = c(mm.nalp.fir.ex$surv.prop.0[mm.nalp.fir.ex$treatment == "seeded"],
                                           mm.nalp.fir.ex$surv.prop.1[mm.nalp.fir.ex$treatment == "seeded"],
                                           mm.nalp.fir.ex$surv.prop.2[mm.nalp.fir.ex$treatment == "seeded"],
                                           mm.nalp.fir.ex$surv.prop.3[mm.nalp.fir.ex$treatment == "seeded"],
                                           mm.nalp.fir.ex$surv.prop.4[mm.nalp.fir.ex$treatment == "seeded"]))
mm.nalp.fir.no.seedscar <- data.frame(year = (rep(c(1:5), c(length(mm.nalp.fir.no$surv.prop.0[mm.nalp.fir.no$treatment == "seeded.scarified"]),
                                                            length(mm.nalp.fir.no$surv.prop.1[mm.nalp.fir.no$treatment == "seeded.scarified"]),
                                                            length(mm.nalp.fir.no$surv.prop.2[mm.nalp.fir.no$treatment == "seeded.scarified"]),
                                                            length(mm.nalp.fir.no$surv.prop.3[mm.nalp.fir.no$treatment == "seeded.scarified"]),
                                                            length(mm.nalp.fir.no$surv.prop.4[mm.nalp.fir.no$treatment == "seeded.scarified"])))),
                                      surv = c(mm.nalp.fir.no$surv.prop.0[mm.nalp.fir.no$treatment == "seeded.scarified"],
                                               mm.nalp.fir.no$surv.prop.1[mm.nalp.fir.no$treatment == "seeded.scarified"],
                                               mm.nalp.fir.no$surv.prop.2[mm.nalp.fir.no$treatment == "seeded.scarified"],
                                               mm.nalp.fir.no$surv.prop.3[mm.nalp.fir.no$treatment == "seeded.scarified"],
                                               mm.nalp.fir.no$surv.prop.4[mm.nalp.fir.no$treatment == "seeded.scarified"]))
mm.nalp.fir.ex.seedscar <- data.frame(year = (rep(c(1:5), c(length(mm.nalp.fir.ex$surv.prop.0[mm.nalp.fir.ex$treatment == "seeded.scarified"]),
                                                            length(mm.nalp.fir.ex$surv.prop.1[mm.nalp.fir.ex$treatment == "seeded.scarified"]),
                                                            length(mm.nalp.fir.ex$surv.prop.2[mm.nalp.fir.ex$treatment == "seeded.scarified"]),
                                                            length(mm.nalp.fir.ex$surv.prop.3[mm.nalp.fir.ex$treatment == "seeded.scarified"]),
                                                            length(mm.nalp.fir.ex$surv.prop.4[mm.nalp.fir.ex$treatment == "seeded.scarified"])))),
                                      surv = c(mm.nalp.fir.ex$surv.prop.0[mm.nalp.fir.ex$treatment == "seeded.scarified"],
                                               mm.nalp.fir.ex$surv.prop.1[mm.nalp.fir.ex$treatment == "seeded.scarified"],
                                               mm.nalp.fir.ex$surv.prop.2[mm.nalp.fir.ex$treatment == "seeded.scarified"],
                                               mm.nalp.fir.ex$surv.prop.3[mm.nalp.fir.ex$treatment == "seeded.scarified"],
                                               mm.nalp.fir.ex$surv.prop.4[mm.nalp.fir.ex$treatment == "seeded.scarified"]))


mm.nalp.fir.no.seed.mod <- lm(log1p(surv) ~ year, mm.nalp.fir.no.seed)
mm.nalp.fir.no.seed.ci <- data.frame(predict(mm.nalp.fir.no.seed.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.nalp.fir.no.seed.ci$fit <- exp(mm.nalp.fir.no.seed.ci$fit)-1
mm.nalp.fir.no.seed.mean <- data.frame(year.vals = seq(1,5,0.1),
                                  surv.mean = mm.nalp.fir.no.seed.ci$fit) #only doing wells with 2015-2020 data

mm.nalp.fir.ex.seed.mod <- lm(log1p(surv) ~ year, mm.nalp.fir.ex.seed)
mm.nalp.fir.ex.seed.ci <- data.frame(predict(mm.nalp.fir.ex.seed.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.nalp.fir.ex.seed.ci$fit <- exp(mm.nalp.fir.ex.seed.ci$fit)-1
mm.nalp.fir.ex.seed.mean <- data.frame(year.vals = seq(1,5,0.1),
                                       surv.mean = mm.nalp.fir.ex.seed.ci$fit) #only doing wells with 2015-2020 data

mm.nalp.fir.no.seedscar.mod <- lm(log1p(surv) ~ year, mm.nalp.fir.no.seedscar)
mm.nalp.fir.no.seedscar.ci <- data.frame(predict(mm.nalp.fir.no.seedscar.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.nalp.fir.no.seedscar.ci$fit <- exp(mm.nalp.fir.no.seedscar.ci$fit)-1
mm.nalp.fir.no.seedscar.mean <- data.frame(year.vals = seq(1,5,0.1),
                                       surv.mean = mm.nalp.fir.no.seedscar.ci$fit) #only doing wells with 2015-2020 data

mm.nalp.fir.ex.seedscar.mod <- lm(log1p(surv) ~ year, mm.nalp.fir.ex.seedscar)
mm.nalp.fir.ex.seedscar.ci <- data.frame(predict(mm.nalp.fir.ex.seedscar.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.nalp.fir.ex.seedscar.ci$fit <- exp(mm.nalp.fir.ex.seedscar.ci$fit)-1
mm.nalp.fir.ex.seedscar.mean <- data.frame(year.vals = seq(1,5,0.1),
                                       surv.mean = mm.nalp.fir.ex.seedscar.ci$fit) #only doing wells with 2015-2020 data

## SALP - fir ----
mm.salp.fir.no.seed <- data.frame(year = (rep(c(1:5), c(length(mm.salp.fir.no$surv.prop.0[mm.salp.fir.no$treatment == "seeded"]),
                                                        length(mm.salp.fir.no$surv.prop.1[mm.salp.fir.no$treatment == "seeded"]),
                                                        length(mm.salp.fir.no$surv.prop.2[mm.salp.fir.no$treatment == "seeded"]),
                                                        length(mm.salp.fir.no$surv.prop.3[mm.salp.fir.no$treatment == "seeded"]),
                                                        length(mm.salp.fir.no$surv.prop.4[mm.salp.fir.no$treatment == "seeded"])))),
                                  surv = c(mm.salp.fir.no$surv.prop.0[mm.salp.fir.no$treatment == "seeded"],
                                           mm.salp.fir.no$surv.prop.1[mm.salp.fir.no$treatment == "seeded"],
                                           mm.salp.fir.no$surv.prop.2[mm.salp.fir.no$treatment == "seeded"],
                                           mm.salp.fir.no$surv.prop.3[mm.salp.fir.no$treatment == "seeded"],
                                           mm.salp.fir.no$surv.prop.4[mm.salp.fir.no$treatment == "seeded"]))
mm.salp.fir.ex.seed <- data.frame(year = (rep(c(1:5), c(length(mm.salp.fir.ex$surv.prop.0[mm.salp.fir.ex$treatment == "seeded"]),
                                                        length(mm.salp.fir.ex$surv.prop.1[mm.salp.fir.ex$treatment == "seeded"]),
                                                        length(mm.salp.fir.ex$surv.prop.2[mm.salp.fir.ex$treatment == "seeded"]),
                                                        length(mm.salp.fir.ex$surv.prop.3[mm.salp.fir.ex$treatment == "seeded"]),
                                                        length(mm.salp.fir.ex$surv.prop.4[mm.salp.fir.ex$treatment == "seeded"])))),
                                  surv = c(mm.salp.fir.ex$surv.prop.0[mm.salp.fir.ex$treatment == "seeded"],
                                           mm.salp.fir.ex$surv.prop.1[mm.salp.fir.ex$treatment == "seeded"],
                                           mm.salp.fir.ex$surv.prop.2[mm.salp.fir.ex$treatment == "seeded"],
                                           mm.salp.fir.ex$surv.prop.3[mm.salp.fir.ex$treatment == "seeded"],
                                           mm.salp.fir.ex$surv.prop.4[mm.salp.fir.ex$treatment == "seeded"]))
mm.salp.fir.no.seedscar <- data.frame(year = (rep(c(1:5), c(length(mm.salp.fir.no$surv.prop.0[mm.salp.fir.no$treatment == "seeded.scarified"]),
                                                            length(mm.salp.fir.no$surv.prop.1[mm.salp.fir.no$treatment == "seeded.scarified"]),
                                                            length(mm.salp.fir.no$surv.prop.2[mm.salp.fir.no$treatment == "seeded.scarified"]),
                                                            length(mm.salp.fir.no$surv.prop.3[mm.salp.fir.no$treatment == "seeded.scarified"]),
                                                            length(mm.salp.fir.no$surv.prop.4[mm.salp.fir.no$treatment == "seeded.scarified"])))),
                                      surv = c(mm.salp.fir.no$surv.prop.0[mm.salp.fir.no$treatment == "seeded.scarified"],
                                               mm.salp.fir.no$surv.prop.1[mm.salp.fir.no$treatment == "seeded.scarified"],
                                               mm.salp.fir.no$surv.prop.2[mm.salp.fir.no$treatment == "seeded.scarified"],
                                               mm.salp.fir.no$surv.prop.3[mm.salp.fir.no$treatment == "seeded.scarified"],
                                               mm.salp.fir.no$surv.prop.4[mm.salp.fir.no$treatment == "seeded.scarified"]))
mm.salp.fir.ex.seedscar <- data.frame(year = (rep(c(1:5), c(length(mm.salp.fir.ex$surv.prop.0[mm.salp.fir.ex$treatment == "seeded.scarified"]),
                                                            length(mm.salp.fir.ex$surv.prop.1[mm.salp.fir.ex$treatment == "seeded.scarified"]),
                                                            length(mm.salp.fir.ex$surv.prop.2[mm.salp.fir.ex$treatment == "seeded.scarified"]),
                                                            length(mm.salp.fir.ex$surv.prop.3[mm.salp.fir.ex$treatment == "seeded.scarified"]),
                                                            length(mm.salp.fir.ex$surv.prop.4[mm.salp.fir.ex$treatment == "seeded.scarified"])))),
                                      surv = c(mm.salp.fir.ex$surv.prop.0[mm.salp.fir.ex$treatment == "seeded.scarified"],
                                               mm.salp.fir.ex$surv.prop.1[mm.salp.fir.ex$treatment == "seeded.scarified"],
                                               mm.salp.fir.ex$surv.prop.2[mm.salp.fir.ex$treatment == "seeded.scarified"],
                                               mm.salp.fir.ex$surv.prop.3[mm.salp.fir.ex$treatment == "seeded.scarified"],
                                               mm.salp.fir.ex$surv.prop.4[mm.salp.fir.ex$treatment == "seeded.scarified"]))


mm.salp.fir.no.seed.mod <- lm(log1p(surv) ~ year, mm.salp.fir.no.seed)
mm.salp.fir.no.seed.ci <- data.frame(predict(mm.salp.fir.no.seed.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.salp.fir.no.seed.ci$fit <- exp(mm.salp.fir.no.seed.ci$fit)-1
mm.salp.fir.no.seed.mean <- data.frame(year.vals = seq(1,5,0.1),
                                       surv.mean = mm.salp.fir.no.seed.ci$fit) #only doing wells with 2015-2020 data

mm.salp.fir.ex.seed.mod <- lm(log1p(surv) ~ year, mm.salp.fir.ex.seed)
mm.salp.fir.ex.seed.ci <- data.frame(predict(mm.salp.fir.ex.seed.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.salp.fir.ex.seed.ci$fit <- exp(mm.salp.fir.ex.seed.ci$fit)-1
mm.salp.fir.ex.seed.mean <- data.frame(year.vals = seq(1,5,0.1),
                                       surv.mean = mm.salp.fir.ex.seed.ci$fit) #only doing wells with 2015-2020 data

mm.salp.fir.no.seedscar.mod <- lm(log1p(surv) ~ year, mm.salp.fir.no.seedscar)
mm.salp.fir.no.seedscar.ci <- data.frame(predict(mm.salp.fir.no.seedscar.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.salp.fir.no.seedscar.ci$fit <- exp(mm.salp.fir.no.seedscar.ci$fit)-1
mm.salp.fir.no.seedscar.mean <- data.frame(year.vals = seq(1,5,0.1),
                                           surv.mean = mm.salp.fir.no.seedscar.ci$fit) #only doing wells with 2015-2020 data

mm.salp.fir.ex.seedscar.mod <- lm(log1p(surv) ~ year, mm.salp.fir.ex.seedscar)
mm.salp.fir.ex.seedscar.ci <- data.frame(predict(mm.salp.fir.ex.seedscar.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.salp.fir.ex.seedscar.ci$fit <- exp(mm.salp.fir.ex.seedscar.ci$fit)-1
mm.salp.fir.ex.seedscar.mean <- data.frame(year.vals = seq(1,5,0.1),
                                           surv.mean = mm.salp.fir.ex.seedscar.ci$fit) #only doing wells with 2015-2020 data



# Proportion spruce survival cage/uncaged ----
## NALP - spruce ----
mm.nalp.spruce.no.seed <- data.frame(year = (rep(c(1:5), c(length(mm.nalp.spruce.no$surv.prop.0[mm.nalp.spruce.no$treatment == "seeded"]),
                                                        length(mm.nalp.spruce.no$surv.prop.1[mm.nalp.spruce.no$treatment == "seeded"]),
                                                        length(mm.nalp.spruce.no$surv.prop.2[mm.nalp.spruce.no$treatment == "seeded"]),
                                                        length(mm.nalp.spruce.no$surv.prop.3[mm.nalp.spruce.no$treatment == "seeded"]),
                                                        length(mm.nalp.spruce.no$surv.prop.4[mm.nalp.spruce.no$treatment == "seeded"])))),
                                  surv = c(mm.nalp.spruce.no$surv.prop.0[mm.nalp.spruce.no$treatment == "seeded"],
                                           mm.nalp.spruce.no$surv.prop.1[mm.nalp.spruce.no$treatment == "seeded"],
                                           mm.nalp.spruce.no$surv.prop.2[mm.nalp.spruce.no$treatment == "seeded"],
                                           mm.nalp.spruce.no$surv.prop.3[mm.nalp.spruce.no$treatment == "seeded"],
                                           mm.nalp.spruce.no$surv.prop.4[mm.nalp.spruce.no$treatment == "seeded"]))
mm.nalp.spruce.ex.seed <- data.frame(year = (rep(c(1:5), c(length(mm.nalp.spruce.ex$surv.prop.0[mm.nalp.spruce.ex$treatment == "seeded"]),
                                                        length(mm.nalp.spruce.ex$surv.prop.1[mm.nalp.spruce.ex$treatment == "seeded"]),
                                                        length(mm.nalp.spruce.ex$surv.prop.2[mm.nalp.spruce.ex$treatment == "seeded"]),
                                                        length(mm.nalp.spruce.ex$surv.prop.3[mm.nalp.spruce.ex$treatment == "seeded"]),
                                                        length(mm.nalp.spruce.ex$surv.prop.4[mm.nalp.spruce.ex$treatment == "seeded"])))),
                                  surv = c(mm.nalp.spruce.ex$surv.prop.0[mm.nalp.spruce.ex$treatment == "seeded"],
                                           mm.nalp.spruce.ex$surv.prop.1[mm.nalp.spruce.ex$treatment == "seeded"],
                                           mm.nalp.spruce.ex$surv.prop.2[mm.nalp.spruce.ex$treatment == "seeded"],
                                           mm.nalp.spruce.ex$surv.prop.3[mm.nalp.spruce.ex$treatment == "seeded"],
                                           mm.nalp.spruce.ex$surv.prop.4[mm.nalp.spruce.ex$treatment == "seeded"]))
mm.nalp.spruce.no.seedscar <- data.frame(year = (rep(c(1:5), c(length(mm.nalp.spruce.no$surv.prop.0[mm.nalp.spruce.no$treatment == "seeded.scarified"]),
                                                            length(mm.nalp.spruce.no$surv.prop.1[mm.nalp.spruce.no$treatment == "seeded.scarified"]),
                                                            length(mm.nalp.spruce.no$surv.prop.2[mm.nalp.spruce.no$treatment == "seeded.scarified"]),
                                                            length(mm.nalp.spruce.no$surv.prop.3[mm.nalp.spruce.no$treatment == "seeded.scarified"]),
                                                            length(mm.nalp.spruce.no$surv.prop.4[mm.nalp.spruce.no$treatment == "seeded.scarified"])))),
                                      surv = c(mm.nalp.spruce.no$surv.prop.0[mm.nalp.spruce.no$treatment == "seeded.scarified"],
                                               mm.nalp.spruce.no$surv.prop.1[mm.nalp.spruce.no$treatment == "seeded.scarified"],
                                               mm.nalp.spruce.no$surv.prop.2[mm.nalp.spruce.no$treatment == "seeded.scarified"],
                                               mm.nalp.spruce.no$surv.prop.3[mm.nalp.spruce.no$treatment == "seeded.scarified"],
                                               mm.nalp.spruce.no$surv.prop.4[mm.nalp.spruce.no$treatment == "seeded.scarified"]))
mm.nalp.spruce.ex.seedscar <- data.frame(year = (rep(c(1:5), c(length(mm.nalp.spruce.ex$surv.prop.0[mm.nalp.spruce.ex$treatment == "seeded.scarified"]),
                                                            length(mm.nalp.spruce.ex$surv.prop.1[mm.nalp.spruce.ex$treatment == "seeded.scarified"]),
                                                            length(mm.nalp.spruce.ex$surv.prop.2[mm.nalp.spruce.ex$treatment == "seeded.scarified"]),
                                                            length(mm.nalp.spruce.ex$surv.prop.3[mm.nalp.spruce.ex$treatment == "seeded.scarified"]),
                                                            length(mm.nalp.spruce.ex$surv.prop.4[mm.nalp.spruce.ex$treatment == "seeded.scarified"])))),
                                      surv = c(mm.nalp.spruce.ex$surv.prop.0[mm.nalp.spruce.ex$treatment == "seeded.scarified"],
                                               mm.nalp.spruce.ex$surv.prop.1[mm.nalp.spruce.ex$treatment == "seeded.scarified"],
                                               mm.nalp.spruce.ex$surv.prop.2[mm.nalp.spruce.ex$treatment == "seeded.scarified"],
                                               mm.nalp.spruce.ex$surv.prop.3[mm.nalp.spruce.ex$treatment == "seeded.scarified"],
                                               mm.nalp.spruce.ex$surv.prop.4[mm.nalp.spruce.ex$treatment == "seeded.scarified"]))


mm.nalp.spruce.no.seed.mod <- lm(log1p(surv) ~ year, mm.nalp.spruce.no.seed)
mm.nalp.spruce.no.seed.ci <- data.frame(predict(mm.nalp.spruce.no.seed.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.nalp.spruce.no.seed.ci$fit <- exp(mm.nalp.spruce.no.seed.ci$fit)-1
mm.nalp.spruce.no.seed.mean <- data.frame(year.vals = seq(1,5,0.1),
                                       surv.mean = mm.nalp.spruce.no.seed.ci$fit) #only doing wells with 2015-2020 data

mm.nalp.spruce.ex.seed.mod <- lm(log1p(surv) ~ year, mm.nalp.spruce.ex.seed)
mm.nalp.spruce.ex.seed.ci <- data.frame(predict(mm.nalp.spruce.ex.seed.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.nalp.spruce.ex.seed.ci$fit <- exp(mm.nalp.spruce.ex.seed.ci$fit)-1
mm.nalp.spruce.ex.seed.mean <- data.frame(year.vals = seq(1,5,0.1),
                                       surv.mean = mm.nalp.spruce.ex.seed.ci$fit) #only doing wells with 2015-2020 data

mm.nalp.spruce.no.seedscar.mod <- lm(log1p(surv) ~ year, mm.nalp.spruce.no.seedscar)
mm.nalp.spruce.no.seedscar.ci <- data.frame(predict(mm.nalp.spruce.no.seedscar.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.nalp.spruce.no.seedscar.ci$fit <- exp(mm.nalp.spruce.no.seedscar.ci$fit)-1
mm.nalp.spruce.no.seedscar.mean <- data.frame(year.vals = seq(1,5,0.1),
                                           surv.mean = mm.nalp.spruce.no.seedscar.ci$fit) #only doing wells with 2015-2020 data

mm.nalp.spruce.ex.seedscar.mod <- lm(log1p(surv) ~ year, mm.nalp.spruce.ex.seedscar)
mm.nalp.spruce.ex.seedscar.ci <- data.frame(predict(mm.nalp.spruce.ex.seedscar.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.nalp.spruce.ex.seedscar.ci$fit <- exp(mm.nalp.spruce.ex.seedscar.ci$fit)-1
mm.nalp.spruce.ex.seedscar.mean <- data.frame(year.vals = seq(1,5,0.1),
                                           surv.mean = mm.nalp.spruce.ex.seedscar.ci$fit) #only doing wells with 2015-2020 data

## SALP - spruce ----
mm.salp.spruce.no.seed <- data.frame(year = (rep(c(1:5), c(length(mm.salp.spruce.no$surv.prop.0[mm.salp.spruce.no$treatment == "seeded"]),
                                                        length(mm.salp.spruce.no$surv.prop.1[mm.salp.spruce.no$treatment == "seeded"]),
                                                        length(mm.salp.spruce.no$surv.prop.2[mm.salp.spruce.no$treatment == "seeded"]),
                                                        length(mm.salp.spruce.no$surv.prop.3[mm.salp.spruce.no$treatment == "seeded"]),
                                                        length(mm.salp.spruce.no$surv.prop.4[mm.salp.spruce.no$treatment == "seeded"])))),
                                  surv = c(mm.salp.spruce.no$surv.prop.0[mm.salp.spruce.no$treatment == "seeded"],
                                           mm.salp.spruce.no$surv.prop.1[mm.salp.spruce.no$treatment == "seeded"],
                                           mm.salp.spruce.no$surv.prop.2[mm.salp.spruce.no$treatment == "seeded"],
                                           mm.salp.spruce.no$surv.prop.3[mm.salp.spruce.no$treatment == "seeded"],
                                           mm.salp.spruce.no$surv.prop.4[mm.salp.spruce.no$treatment == "seeded"]))
mm.salp.spruce.ex.seed <- data.frame(year = (rep(c(1:5), c(length(mm.salp.spruce.ex$surv.prop.0[mm.salp.spruce.ex$treatment == "seeded"]),
                                                        length(mm.salp.spruce.ex$surv.prop.1[mm.salp.spruce.ex$treatment == "seeded"]),
                                                        length(mm.salp.spruce.ex$surv.prop.2[mm.salp.spruce.ex$treatment == "seeded"]),
                                                        length(mm.salp.spruce.ex$surv.prop.3[mm.salp.spruce.ex$treatment == "seeded"]),
                                                        length(mm.salp.spruce.ex$surv.prop.4[mm.salp.spruce.ex$treatment == "seeded"])))),
                                  surv = c(mm.salp.spruce.ex$surv.prop.0[mm.salp.spruce.ex$treatment == "seeded"],
                                           mm.salp.spruce.ex$surv.prop.1[mm.salp.spruce.ex$treatment == "seeded"],
                                           mm.salp.spruce.ex$surv.prop.2[mm.salp.spruce.ex$treatment == "seeded"],
                                           mm.salp.spruce.ex$surv.prop.3[mm.salp.spruce.ex$treatment == "seeded"],
                                           mm.salp.spruce.ex$surv.prop.4[mm.salp.spruce.ex$treatment == "seeded"]))
mm.salp.spruce.no.seedscar <- data.frame(year = (rep(c(1:5), c(length(mm.salp.spruce.no$surv.prop.0[mm.salp.spruce.no$treatment == "seeded.scarified"]),
                                                            length(mm.salp.spruce.no$surv.prop.1[mm.salp.spruce.no$treatment == "seeded.scarified"]),
                                                            length(mm.salp.spruce.no$surv.prop.2[mm.salp.spruce.no$treatment == "seeded.scarified"]),
                                                            length(mm.salp.spruce.no$surv.prop.3[mm.salp.spruce.no$treatment == "seeded.scarified"]),
                                                            length(mm.salp.spruce.no$surv.prop.4[mm.salp.spruce.no$treatment == "seeded.scarified"])))),
                                      surv = c(mm.salp.spruce.no$surv.prop.0[mm.salp.spruce.no$treatment == "seeded.scarified"],
                                               mm.salp.spruce.no$surv.prop.1[mm.salp.spruce.no$treatment == "seeded.scarified"],
                                               mm.salp.spruce.no$surv.prop.2[mm.salp.spruce.no$treatment == "seeded.scarified"],
                                               mm.salp.spruce.no$surv.prop.3[mm.salp.spruce.no$treatment == "seeded.scarified"],
                                               mm.salp.spruce.no$surv.prop.4[mm.salp.spruce.no$treatment == "seeded.scarified"]))
mm.salp.spruce.ex.seedscar <- data.frame(year = (rep(c(1:5), c(length(mm.salp.spruce.ex$surv.prop.0[mm.salp.spruce.ex$treatment == "seeded.scarified"]),
                                                            length(mm.salp.spruce.ex$surv.prop.1[mm.salp.spruce.ex$treatment == "seeded.scarified"]),
                                                            length(mm.salp.spruce.ex$surv.prop.2[mm.salp.spruce.ex$treatment == "seeded.scarified"]),
                                                            length(mm.salp.spruce.ex$surv.prop.3[mm.salp.spruce.ex$treatment == "seeded.scarified"]),
                                                            length(mm.salp.spruce.ex$surv.prop.4[mm.salp.spruce.ex$treatment == "seeded.scarified"])))),
                                      surv = c(mm.salp.spruce.ex$surv.prop.0[mm.salp.spruce.ex$treatment == "seeded.scarified"],
                                               mm.salp.spruce.ex$surv.prop.1[mm.salp.spruce.ex$treatment == "seeded.scarified"],
                                               mm.salp.spruce.ex$surv.prop.2[mm.salp.spruce.ex$treatment == "seeded.scarified"],
                                               mm.salp.spruce.ex$surv.prop.3[mm.salp.spruce.ex$treatment == "seeded.scarified"],
                                               mm.salp.spruce.ex$surv.prop.4[mm.salp.spruce.ex$treatment == "seeded.scarified"]))


mm.salp.spruce.no.seed.mod <- lm(log1p(surv) ~ year, mm.salp.spruce.no.seed)
mm.salp.spruce.no.seed.ci <- data.frame(predict(mm.salp.spruce.no.seed.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.salp.spruce.no.seed.ci$fit <- exp(mm.salp.spruce.no.seed.ci$fit)-1
mm.salp.spruce.no.seed.mean <- data.frame(year.vals = seq(1,5,0.1),
                                       surv.mean = mm.salp.spruce.no.seed.ci$fit) #only doing wells with 2015-2020 data

mm.salp.spruce.ex.seed.mod <- lm(log1p(surv) ~ year, mm.salp.spruce.ex.seed)
mm.salp.spruce.ex.seed.ci <- data.frame(predict(mm.salp.spruce.ex.seed.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.salp.spruce.ex.seed.ci$fit <- exp(mm.salp.spruce.ex.seed.ci$fit)-1
mm.salp.spruce.ex.seed.mean <- data.frame(year.vals = seq(1,5,0.1),
                                       surv.mean = mm.salp.spruce.ex.seed.ci$fit) #only doing wells with 2015-2020 data

mm.salp.spruce.no.seedscar.mod <- lm(log1p(surv) ~ year, mm.salp.spruce.no.seedscar)
mm.salp.spruce.no.seedscar.ci <- data.frame(predict(mm.salp.spruce.no.seedscar.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.salp.spruce.no.seedscar.ci$fit <- exp(mm.salp.spruce.no.seedscar.ci$fit)-1
mm.salp.spruce.no.seedscar.mean <- data.frame(year.vals = seq(1,5,0.1),
                                           surv.mean = mm.salp.spruce.no.seedscar.ci$fit) #only doing wells with 2015-2020 data

mm.salp.spruce.ex.seedscar.mod <- lm(log1p(surv) ~ year, mm.salp.spruce.ex.seedscar)
mm.salp.spruce.ex.seedscar.ci <- data.frame(predict(mm.salp.spruce.ex.seedscar.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.salp.spruce.ex.seedscar.ci$fit <- exp(mm.salp.spruce.ex.seedscar.ci$fit)-1
mm.salp.spruce.ex.seedscar.mean <- data.frame(year.vals = seq(1,5,0.1),
                                           surv.mean = mm.salp.spruce.ex.seedscar.ci$fit) #only doing wells with 2015-2020 data



## SCUT - fir ----
mm.scut.fir.no.seed <- data.frame(year = (rep(c(1:5), c(length(mm.scut.fir.no$surv.prop.0[mm.scut.fir.no$treatment == "seeded"]),
                                                        length(mm.scut.fir.no$surv.prop.1[mm.scut.fir.no$treatment == "seeded"]),
                                                        length(mm.scut.fir.no$surv.prop.2[mm.scut.fir.no$treatment == "seeded"]),
                                                        length(mm.scut.fir.no$surv.prop.3[mm.scut.fir.no$treatment == "seeded"]),
                                                        length(mm.scut.fir.no$surv.prop.4[mm.scut.fir.no$treatment == "seeded"])))),
                                  surv = c(mm.scut.fir.no$surv.prop.0[mm.scut.fir.no$treatment == "seeded"],
                                           mm.scut.fir.no$surv.prop.1[mm.scut.fir.no$treatment == "seeded"],
                                           mm.scut.fir.no$surv.prop.2[mm.scut.fir.no$treatment == "seeded"],
                                           mm.scut.fir.no$surv.prop.3[mm.scut.fir.no$treatment == "seeded"],
                                           mm.scut.fir.no$surv.prop.4[mm.scut.fir.no$treatment == "seeded"]))
mm.scut.fir.ex.seed <- data.frame(year = (rep(c(1:5), c(length(mm.scut.fir.ex$surv.prop.0[mm.scut.fir.ex$treatment == "seeded"]),
                                                        length(mm.scut.fir.ex$surv.prop.1[mm.scut.fir.ex$treatment == "seeded"]),
                                                        length(mm.scut.fir.ex$surv.prop.2[mm.scut.fir.ex$treatment == "seeded"]),
                                                        length(mm.scut.fir.ex$surv.prop.3[mm.scut.fir.ex$treatment == "seeded"]),
                                                        length(mm.scut.fir.ex$surv.prop.4[mm.scut.fir.ex$treatment == "seeded"])))),
                                  surv = c(mm.scut.fir.ex$surv.prop.0[mm.scut.fir.ex$treatment == "seeded"],
                                           mm.scut.fir.ex$surv.prop.1[mm.scut.fir.ex$treatment == "seeded"],
                                           mm.scut.fir.ex$surv.prop.2[mm.scut.fir.ex$treatment == "seeded"],
                                           mm.scut.fir.ex$surv.prop.3[mm.scut.fir.ex$treatment == "seeded"],
                                           mm.scut.fir.ex$surv.prop.4[mm.scut.fir.ex$treatment == "seeded"]))
mm.scut.fir.no.seedscar <- data.frame(year = (rep(c(1:5), c(length(mm.scut.fir.no$surv.prop.0[mm.scut.fir.no$treatment == "seeded.scarified"]),
                                                            length(mm.scut.fir.no$surv.prop.1[mm.scut.fir.no$treatment == "seeded.scarified"]),
                                                            length(mm.scut.fir.no$surv.prop.2[mm.scut.fir.no$treatment == "seeded.scarified"]),
                                                            length(mm.scut.fir.no$surv.prop.3[mm.scut.fir.no$treatment == "seeded.scarified"]),
                                                            length(mm.scut.fir.no$surv.prop.4[mm.scut.fir.no$treatment == "seeded.scarified"])))),
                                      surv = c(mm.scut.fir.no$surv.prop.0[mm.scut.fir.no$treatment == "seeded.scarified"],
                                               mm.scut.fir.no$surv.prop.1[mm.scut.fir.no$treatment == "seeded.scarified"],
                                               mm.scut.fir.no$surv.prop.2[mm.scut.fir.no$treatment == "seeded.scarified"],
                                               mm.scut.fir.no$surv.prop.3[mm.scut.fir.no$treatment == "seeded.scarified"],
                                               mm.scut.fir.no$surv.prop.4[mm.scut.fir.no$treatment == "seeded.scarified"]))
mm.scut.fir.ex.seedscar <- data.frame(year = (rep(c(1:5), c(length(mm.scut.fir.ex$surv.prop.0[mm.scut.fir.ex$treatment == "seeded.scarified"]),
                                                            length(mm.scut.fir.ex$surv.prop.1[mm.scut.fir.ex$treatment == "seeded.scarified"]),
                                                            length(mm.scut.fir.ex$surv.prop.2[mm.scut.fir.ex$treatment == "seeded.scarified"]),
                                                            length(mm.scut.fir.ex$surv.prop.3[mm.scut.fir.ex$treatment == "seeded.scarified"]),
                                                            length(mm.scut.fir.ex$surv.prop.4[mm.scut.fir.ex$treatment == "seeded.scarified"])))),
                                      surv = c(mm.scut.fir.ex$surv.prop.0[mm.scut.fir.ex$treatment == "seeded.scarified"],
                                               mm.scut.fir.ex$surv.prop.1[mm.scut.fir.ex$treatment == "seeded.scarified"],
                                               mm.scut.fir.ex$surv.prop.2[mm.scut.fir.ex$treatment == "seeded.scarified"],
                                               mm.scut.fir.ex$surv.prop.3[mm.scut.fir.ex$treatment == "seeded.scarified"],
                                               mm.scut.fir.ex$surv.prop.4[mm.scut.fir.ex$treatment == "seeded.scarified"]))


# mm.scut.fir.no.seed.mod <- lm(log1p(surv) ~ year, mm.scut.fir.no.seed)
# mm.scut.fir.no.seed.ci <- data.frame(predict(mm.scut.fir.no.seed.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
# mm.scut.fir.no.seed.ci$fit <- exp(mm.scut.fir.no.seed.ci$fit)-1
# mm.scut.fir.no.seed.mean <- data.frame(year.vals = seq(1,5,0.1),
#                                        surv.mean = mm.scut.fir.no.seed.ci$fit) #only doing wells with 2015-2020 data

mm.scut.fir.ex.seed.mod <- lm(log1p(surv) ~ year, mm.scut.fir.ex.seed)
mm.scut.fir.ex.seed.ci <- data.frame(predict(mm.scut.fir.ex.seed.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.scut.fir.ex.seed.ci$fit <- exp(mm.scut.fir.ex.seed.ci$fit)-1
mm.scut.fir.ex.seed.mean <- data.frame(year.vals = seq(1,5,0.1),
                                       surv.mean = mm.scut.fir.ex.seed.ci$fit) #only doing wells with 2015-2020 data

# mm.scut.fir.no.seedscar.mod <- lm(log1p(surv) ~ year, mm.scut.fir.no.seedscar)
# mm.scut.fir.no.seedscar.ci <- data.frame(predict(mm.scut.fir.no.seedscar.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
# mm.scut.fir.no.seedscar.ci$fit <- exp(mm.scut.fir.no.seedscar.ci$fit)-1
# mm.scut.fir.no.seedscar.mean <- data.frame(year.vals = seq(1,5,0.1),
#                                            surv.mean = mm.scut.fir.no.seedscar.ci$fit) #only doing wells with 2015-2020 data

mm.scut.fir.ex.seedscar.mod <- lm(log1p(surv) ~ year, mm.scut.fir.ex.seedscar)
mm.scut.fir.ex.seedscar.ci <- data.frame(predict(mm.scut.fir.ex.seedscar.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.scut.fir.ex.seedscar.ci$fit <- exp(mm.scut.fir.ex.seedscar.ci$fit)-1
mm.scut.fir.ex.seedscar.mean <- data.frame(year.vals = seq(1,5,0.1),
                                           surv.mean = mm.scut.fir.ex.seedscar.ci$fit) #only doing wells with 2015-2020 data

## SSHR - fir ----
mm.sshr.fir.no.seed <- data.frame(year = (rep(c(1:5), c(length(mm.sshr.fir.no$surv.prop.0[mm.sshr.fir.no$treatment == "seeded"]),
                                                        length(mm.sshr.fir.no$surv.prop.1[mm.sshr.fir.no$treatment == "seeded"]),
                                                        length(mm.sshr.fir.no$surv.prop.2[mm.sshr.fir.no$treatment == "seeded"]),
                                                        length(mm.sshr.fir.no$surv.prop.3[mm.sshr.fir.no$treatment == "seeded"]),
                                                        length(mm.sshr.fir.no$surv.prop.4[mm.sshr.fir.no$treatment == "seeded"])))),
                                  surv = c(mm.sshr.fir.no$surv.prop.0[mm.sshr.fir.no$treatment == "seeded"],
                                           mm.sshr.fir.no$surv.prop.1[mm.sshr.fir.no$treatment == "seeded"],
                                           mm.sshr.fir.no$surv.prop.2[mm.sshr.fir.no$treatment == "seeded"],
                                           mm.sshr.fir.no$surv.prop.3[mm.sshr.fir.no$treatment == "seeded"],
                                           mm.sshr.fir.no$surv.prop.4[mm.sshr.fir.no$treatment == "seeded"]))
mm.sshr.fir.ex.seed <- data.frame(year = (rep(c(1:5), c(length(mm.sshr.fir.ex$surv.prop.0[mm.sshr.fir.ex$treatment == "seeded"]),
                                                        length(mm.sshr.fir.ex$surv.prop.1[mm.sshr.fir.ex$treatment == "seeded"]),
                                                        length(mm.sshr.fir.ex$surv.prop.2[mm.sshr.fir.ex$treatment == "seeded"]),
                                                        length(mm.sshr.fir.ex$surv.prop.3[mm.sshr.fir.ex$treatment == "seeded"]),
                                                        length(mm.sshr.fir.ex$surv.prop.4[mm.sshr.fir.ex$treatment == "seeded"])))),
                                  surv = c(mm.sshr.fir.ex$surv.prop.0[mm.sshr.fir.ex$treatment == "seeded"],
                                           mm.sshr.fir.ex$surv.prop.1[mm.sshr.fir.ex$treatment == "seeded"],
                                           mm.sshr.fir.ex$surv.prop.2[mm.sshr.fir.ex$treatment == "seeded"],
                                           mm.sshr.fir.ex$surv.prop.3[mm.sshr.fir.ex$treatment == "seeded"],
                                           mm.sshr.fir.ex$surv.prop.4[mm.sshr.fir.ex$treatment == "seeded"]))
mm.sshr.fir.no.seedscar <- data.frame(year = (rep(c(1:5), c(length(mm.sshr.fir.no$surv.prop.0[mm.sshr.fir.no$treatment == "seeded.scarified"]),
                                                            length(mm.sshr.fir.no$surv.prop.1[mm.sshr.fir.no$treatment == "seeded.scarified"]),
                                                            length(mm.sshr.fir.no$surv.prop.2[mm.sshr.fir.no$treatment == "seeded.scarified"]),
                                                            length(mm.sshr.fir.no$surv.prop.3[mm.sshr.fir.no$treatment == "seeded.scarified"]),
                                                            length(mm.sshr.fir.no$surv.prop.4[mm.sshr.fir.no$treatment == "seeded.scarified"])))),
                                      surv = c(mm.sshr.fir.no$surv.prop.0[mm.sshr.fir.no$treatment == "seeded.scarified"],
                                               mm.sshr.fir.no$surv.prop.1[mm.sshr.fir.no$treatment == "seeded.scarified"],
                                               mm.sshr.fir.no$surv.prop.2[mm.sshr.fir.no$treatment == "seeded.scarified"],
                                               mm.sshr.fir.no$surv.prop.3[mm.sshr.fir.no$treatment == "seeded.scarified"],
                                               mm.sshr.fir.no$surv.prop.4[mm.sshr.fir.no$treatment == "seeded.scarified"]))
mm.sshr.fir.ex.seedscar <- data.frame(year = (rep(c(1:5), c(length(mm.sshr.fir.ex$surv.prop.0[mm.sshr.fir.ex$treatment == "seeded.scarified"]),
                                                            length(mm.sshr.fir.ex$surv.prop.1[mm.sshr.fir.ex$treatment == "seeded.scarified"]),
                                                            length(mm.sshr.fir.ex$surv.prop.2[mm.sshr.fir.ex$treatment == "seeded.scarified"]),
                                                            length(mm.sshr.fir.ex$surv.prop.3[mm.sshr.fir.ex$treatment == "seeded.scarified"]),
                                                            length(mm.sshr.fir.ex$surv.prop.4[mm.sshr.fir.ex$treatment == "seeded.scarified"])))),
                                      surv = c(mm.sshr.fir.ex$surv.prop.0[mm.sshr.fir.ex$treatment == "seeded.scarified"],
                                               mm.sshr.fir.ex$surv.prop.1[mm.sshr.fir.ex$treatment == "seeded.scarified"],
                                               mm.sshr.fir.ex$surv.prop.2[mm.sshr.fir.ex$treatment == "seeded.scarified"],
                                               mm.sshr.fir.ex$surv.prop.3[mm.sshr.fir.ex$treatment == "seeded.scarified"],
                                               mm.sshr.fir.ex$surv.prop.4[mm.sshr.fir.ex$treatment == "seeded.scarified"]))


# mm.sshr.fir.no.seed.mod <- lm(log1p(surv) ~ year, mm.sshr.fir.no.seed)
# mm.sshr.fir.no.seed.ci <- data.frame(predict(mm.sshr.fir.no.seed.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
# mm.sshr.fir.no.seed.ci$fit <- exp(mm.sshr.fir.no.seed.ci$fit)-1
# mm.sshr.fir.no.seed.mean <- data.frame(year.vals = seq(1,5,0.1),
#                                        surv.mean = mm.sshr.fir.no.seed.ci$fit) #only doing wells with 2015-2020 data

mm.sshr.fir.ex.seed.mod <- lm(log1p(surv) ~ year, mm.sshr.fir.ex.seed)
mm.sshr.fir.ex.seed.ci <- data.frame(predict(mm.sshr.fir.ex.seed.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.sshr.fir.ex.seed.ci$fit <- exp(mm.sshr.fir.ex.seed.ci$fit)-1
mm.sshr.fir.ex.seed.mean <- data.frame(year.vals = seq(1,5,0.1),
                                       surv.mean = mm.sshr.fir.ex.seed.ci$fit) #only doing wells with 2015-2020 data

mm.sshr.fir.no.seedscar.mod <- lm(log1p(surv) ~ year, mm.sshr.fir.no.seedscar)
mm.sshr.fir.no.seedscar.ci <- data.frame(predict(mm.sshr.fir.no.seedscar.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.sshr.fir.no.seedscar.ci$fit <- exp(mm.sshr.fir.no.seedscar.ci$fit)-1
mm.sshr.fir.no.seedscar.mean <- data.frame(year.vals = seq(1,5,0.1),
                                           surv.mean = mm.sshr.fir.no.seedscar.ci$fit) #only doing wells with 2015-2020 data

mm.sshr.fir.ex.seedscar.mod <- lm(log1p(surv) ~ year, mm.sshr.fir.ex.seedscar)
mm.sshr.fir.ex.seedscar.ci <- data.frame(predict(mm.sshr.fir.ex.seedscar.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.sshr.fir.ex.seedscar.ci$fit <- exp(mm.sshr.fir.ex.seedscar.ci$fit)-1
mm.sshr.fir.ex.seedscar.mean <- data.frame(year.vals = seq(1,5,0.1),
                                           surv.mean = mm.sshr.fir.ex.seedscar.ci$fit) #only doing wells with 2015-2020 data








## SCUT - spruce ----
mm.scut.spruce.no.seed <- data.frame(year = (rep(c(1:5), c(length(mm.scut.spruce.no$surv.prop.0[mm.scut.spruce.no$treatment == "seeded"]),
                                                        length(mm.scut.spruce.no$surv.prop.1[mm.scut.spruce.no$treatment == "seeded"]),
                                                        length(mm.scut.spruce.no$surv.prop.2[mm.scut.spruce.no$treatment == "seeded"]),
                                                        length(mm.scut.spruce.no$surv.prop.3[mm.scut.spruce.no$treatment == "seeded"]),
                                                        length(mm.scut.spruce.no$surv.prop.4[mm.scut.spruce.no$treatment == "seeded"])))),
                                  surv = c(mm.scut.spruce.no$surv.prop.0[mm.scut.spruce.no$treatment == "seeded"],
                                           mm.scut.spruce.no$surv.prop.1[mm.scut.spruce.no$treatment == "seeded"],
                                           mm.scut.spruce.no$surv.prop.2[mm.scut.spruce.no$treatment == "seeded"],
                                           mm.scut.spruce.no$surv.prop.3[mm.scut.spruce.no$treatment == "seeded"],
                                           mm.scut.spruce.no$surv.prop.4[mm.scut.spruce.no$treatment == "seeded"]))
mm.scut.spruce.ex.seed <- data.frame(year = (rep(c(1:5), c(length(mm.scut.spruce.ex$surv.prop.0[mm.scut.spruce.ex$treatment == "seeded"]),
                                                        length(mm.scut.spruce.ex$surv.prop.1[mm.scut.spruce.ex$treatment == "seeded"]),
                                                        length(mm.scut.spruce.ex$surv.prop.2[mm.scut.spruce.ex$treatment == "seeded"]),
                                                        length(mm.scut.spruce.ex$surv.prop.3[mm.scut.spruce.ex$treatment == "seeded"]),
                                                        length(mm.scut.spruce.ex$surv.prop.4[mm.scut.spruce.ex$treatment == "seeded"])))),
                                  surv = c(mm.scut.spruce.ex$surv.prop.0[mm.scut.spruce.ex$treatment == "seeded"],
                                           mm.scut.spruce.ex$surv.prop.1[mm.scut.spruce.ex$treatment == "seeded"],
                                           mm.scut.spruce.ex$surv.prop.2[mm.scut.spruce.ex$treatment == "seeded"],
                                           mm.scut.spruce.ex$surv.prop.3[mm.scut.spruce.ex$treatment == "seeded"],
                                           mm.scut.spruce.ex$surv.prop.4[mm.scut.spruce.ex$treatment == "seeded"]))
mm.scut.spruce.no.seedscar <- data.frame(year = (rep(c(1:5), c(length(mm.scut.spruce.no$surv.prop.0[mm.scut.spruce.no$treatment == "seeded.scarified"]),
                                                            length(mm.scut.spruce.no$surv.prop.1[mm.scut.spruce.no$treatment == "seeded.scarified"]),
                                                            length(mm.scut.spruce.no$surv.prop.2[mm.scut.spruce.no$treatment == "seeded.scarified"]),
                                                            length(mm.scut.spruce.no$surv.prop.3[mm.scut.spruce.no$treatment == "seeded.scarified"]),
                                                            length(mm.scut.spruce.no$surv.prop.4[mm.scut.spruce.no$treatment == "seeded.scarified"])))),
                                      surv = c(mm.scut.spruce.no$surv.prop.0[mm.scut.spruce.no$treatment == "seeded.scarified"],
                                               mm.scut.spruce.no$surv.prop.1[mm.scut.spruce.no$treatment == "seeded.scarified"],
                                               mm.scut.spruce.no$surv.prop.2[mm.scut.spruce.no$treatment == "seeded.scarified"],
                                               mm.scut.spruce.no$surv.prop.3[mm.scut.spruce.no$treatment == "seeded.scarified"],
                                               mm.scut.spruce.no$surv.prop.4[mm.scut.spruce.no$treatment == "seeded.scarified"]))
mm.scut.spruce.ex.seedscar <- data.frame(year = (rep(c(1:5), c(length(mm.scut.spruce.ex$surv.prop.0[mm.scut.spruce.ex$treatment == "seeded.scarified"]),
                                                            length(mm.scut.spruce.ex$surv.prop.1[mm.scut.spruce.ex$treatment == "seeded.scarified"]),
                                                            length(mm.scut.spruce.ex$surv.prop.2[mm.scut.spruce.ex$treatment == "seeded.scarified"]),
                                                            length(mm.scut.spruce.ex$surv.prop.3[mm.scut.spruce.ex$treatment == "seeded.scarified"]),
                                                            length(mm.scut.spruce.ex$surv.prop.4[mm.scut.spruce.ex$treatment == "seeded.scarified"])))),
                                      surv = c(mm.scut.spruce.ex$surv.prop.0[mm.scut.spruce.ex$treatment == "seeded.scarified"],
                                               mm.scut.spruce.ex$surv.prop.1[mm.scut.spruce.ex$treatment == "seeded.scarified"],
                                               mm.scut.spruce.ex$surv.prop.2[mm.scut.spruce.ex$treatment == "seeded.scarified"],
                                               mm.scut.spruce.ex$surv.prop.3[mm.scut.spruce.ex$treatment == "seeded.scarified"],
                                               mm.scut.spruce.ex$surv.prop.4[mm.scut.spruce.ex$treatment == "seeded.scarified"]))


mm.scut.spruce.no.seed.mod <- lm(log1p(surv) ~ year, mm.scut.spruce.no.seed)
mm.scut.spruce.no.seed.ci <- data.frame(predict(mm.scut.spruce.no.seed.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.scut.spruce.no.seed.ci$fit <- exp(mm.scut.spruce.no.seed.ci$fit)-1
mm.scut.spruce.no.seed.mean <- data.frame(year.vals = seq(1,5,0.1),
                                       surv.mean = mm.scut.spruce.no.seed.ci$fit) #only doing wells with 2015-2020 data

mm.scut.spruce.ex.seed.mod <- lm(log1p(surv) ~ year, mm.scut.spruce.ex.seed)
mm.scut.spruce.ex.seed.ci <- data.frame(predict(mm.scut.spruce.ex.seed.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.scut.spruce.ex.seed.ci$fit <- exp(mm.scut.spruce.ex.seed.ci$fit)-1
mm.scut.spruce.ex.seed.mean <- data.frame(year.vals = seq(1,5,0.1),
                                       surv.mean = mm.scut.spruce.ex.seed.ci$fit) #only doing wells with 2015-2020 data

mm.scut.spruce.no.seedscar.mod <- lm(log1p(surv) ~ year, mm.scut.spruce.no.seedscar)
mm.scut.spruce.no.seedscar.ci <- data.frame(predict(mm.scut.spruce.no.seedscar.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.scut.spruce.no.seedscar.ci$fit <- exp(mm.scut.spruce.no.seedscar.ci$fit)-1
mm.scut.spruce.no.seedscar.mean <- data.frame(year.vals = seq(1,5,0.1),
                                           surv.mean = mm.scut.spruce.no.seedscar.ci$fit) #only doing wells with 2015-2020 data

mm.scut.spruce.ex.seedscar.mod <- lm(log1p(surv) ~ year, mm.scut.spruce.ex.seedscar)
mm.scut.spruce.ex.seedscar.ci <- data.frame(predict(mm.scut.spruce.ex.seedscar.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.scut.spruce.ex.seedscar.ci$fit <- exp(mm.scut.spruce.ex.seedscar.ci$fit)-1
mm.scut.spruce.ex.seedscar.mean <- data.frame(year.vals = seq(1,5,0.1),
                                           surv.mean = mm.scut.spruce.ex.seedscar.ci$fit) #only doing wells with 2015-2020 data

## SSHR - spruce ----
mm.sshr.spruce.no.seed <- data.frame(year = (rep(c(1:5), c(length(mm.sshr.spruce.no$surv.prop.0[mm.sshr.spruce.no$treatment == "seeded"]),
                                                        length(mm.sshr.spruce.no$surv.prop.1[mm.sshr.spruce.no$treatment == "seeded"]),
                                                        length(mm.sshr.spruce.no$surv.prop.2[mm.sshr.spruce.no$treatment == "seeded"]),
                                                        length(mm.sshr.spruce.no$surv.prop.3[mm.sshr.spruce.no$treatment == "seeded"]),
                                                        length(mm.sshr.spruce.no$surv.prop.4[mm.sshr.spruce.no$treatment == "seeded"])))),
                                  surv = c(mm.sshr.spruce.no$surv.prop.0[mm.sshr.spruce.no$treatment == "seeded"],
                                           mm.sshr.spruce.no$surv.prop.1[mm.sshr.spruce.no$treatment == "seeded"],
                                           mm.sshr.spruce.no$surv.prop.2[mm.sshr.spruce.no$treatment == "seeded"],
                                           mm.sshr.spruce.no$surv.prop.3[mm.sshr.spruce.no$treatment == "seeded"],
                                           mm.sshr.spruce.no$surv.prop.4[mm.sshr.spruce.no$treatment == "seeded"]))
mm.sshr.spruce.ex.seed <- data.frame(year = (rep(c(1:5), c(length(mm.sshr.spruce.ex$surv.prop.0[mm.sshr.spruce.ex$treatment == "seeded"]),
                                                        length(mm.sshr.spruce.ex$surv.prop.1[mm.sshr.spruce.ex$treatment == "seeded"]),
                                                        length(mm.sshr.spruce.ex$surv.prop.2[mm.sshr.spruce.ex$treatment == "seeded"]),
                                                        length(mm.sshr.spruce.ex$surv.prop.3[mm.sshr.spruce.ex$treatment == "seeded"]),
                                                        length(mm.sshr.spruce.ex$surv.prop.4[mm.sshr.spruce.ex$treatment == "seeded"])))),
                                  surv = c(mm.sshr.spruce.ex$surv.prop.0[mm.sshr.spruce.ex$treatment == "seeded"],
                                           mm.sshr.spruce.ex$surv.prop.1[mm.sshr.spruce.ex$treatment == "seeded"],
                                           mm.sshr.spruce.ex$surv.prop.2[mm.sshr.spruce.ex$treatment == "seeded"],
                                           mm.sshr.spruce.ex$surv.prop.3[mm.sshr.spruce.ex$treatment == "seeded"],
                                           mm.sshr.spruce.ex$surv.prop.4[mm.sshr.spruce.ex$treatment == "seeded"]))
mm.sshr.spruce.no.seedscar <- data.frame(year = (rep(c(1:5), c(length(mm.sshr.spruce.no$surv.prop.0[mm.sshr.spruce.no$treatment == "seeded.scarified"]),
                                                            length(mm.sshr.spruce.no$surv.prop.1[mm.sshr.spruce.no$treatment == "seeded.scarified"]),
                                                            length(mm.sshr.spruce.no$surv.prop.2[mm.sshr.spruce.no$treatment == "seeded.scarified"]),
                                                            length(mm.sshr.spruce.no$surv.prop.3[mm.sshr.spruce.no$treatment == "seeded.scarified"]),
                                                            length(mm.sshr.spruce.no$surv.prop.4[mm.sshr.spruce.no$treatment == "seeded.scarified"])))),
                                      surv = c(mm.sshr.spruce.no$surv.prop.0[mm.sshr.spruce.no$treatment == "seeded.scarified"],
                                               mm.sshr.spruce.no$surv.prop.1[mm.sshr.spruce.no$treatment == "seeded.scarified"],
                                               mm.sshr.spruce.no$surv.prop.2[mm.sshr.spruce.no$treatment == "seeded.scarified"],
                                               mm.sshr.spruce.no$surv.prop.3[mm.sshr.spruce.no$treatment == "seeded.scarified"],
                                               mm.sshr.spruce.no$surv.prop.4[mm.sshr.spruce.no$treatment == "seeded.scarified"]))
mm.sshr.spruce.ex.seedscar <- data.frame(year = (rep(c(1:5), c(length(mm.sshr.spruce.ex$surv.prop.0[mm.sshr.spruce.ex$treatment == "seeded.scarified"]),
                                                            length(mm.sshr.spruce.ex$surv.prop.1[mm.sshr.spruce.ex$treatment == "seeded.scarified"]),
                                                            length(mm.sshr.spruce.ex$surv.prop.2[mm.sshr.spruce.ex$treatment == "seeded.scarified"]),
                                                            length(mm.sshr.spruce.ex$surv.prop.3[mm.sshr.spruce.ex$treatment == "seeded.scarified"]),
                                                            length(mm.sshr.spruce.ex$surv.prop.4[mm.sshr.spruce.ex$treatment == "seeded.scarified"])))),
                                      surv = c(mm.sshr.spruce.ex$surv.prop.0[mm.sshr.spruce.ex$treatment == "seeded.scarified"],
                                               mm.sshr.spruce.ex$surv.prop.1[mm.sshr.spruce.ex$treatment == "seeded.scarified"],
                                               mm.sshr.spruce.ex$surv.prop.2[mm.sshr.spruce.ex$treatment == "seeded.scarified"],
                                               mm.sshr.spruce.ex$surv.prop.3[mm.sshr.spruce.ex$treatment == "seeded.scarified"],
                                               mm.sshr.spruce.ex$surv.prop.4[mm.sshr.spruce.ex$treatment == "seeded.scarified"]))


mm.sshr.spruce.no.seed.mod <- lm(log1p(surv) ~ year, mm.sshr.spruce.no.seed)
mm.sshr.spruce.no.seed.ci <- data.frame(predict(mm.sshr.spruce.no.seed.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.sshr.spruce.no.seed.ci$fit <- exp(mm.sshr.spruce.no.seed.ci$fit)-1
mm.sshr.spruce.no.seed.mean <- data.frame(year.vals = seq(1,5,0.1),
                                       surv.mean = mm.sshr.spruce.no.seed.ci$fit) #only doing wells with 2015-2020 data

mm.sshr.spruce.ex.seed.mod <- lm(log1p(surv) ~ year, mm.sshr.spruce.ex.seed)
mm.sshr.spruce.ex.seed.ci <- data.frame(predict(mm.sshr.spruce.ex.seed.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.sshr.spruce.ex.seed.ci$fit <- exp(mm.sshr.spruce.ex.seed.ci$fit)-1
mm.sshr.spruce.ex.seed.mean <- data.frame(year.vals = seq(1,5,0.1),
                                       surv.mean = mm.sshr.spruce.ex.seed.ci$fit) #only doing wells with 2015-2020 data

mm.sshr.spruce.no.seedscar.mod <- lm(log1p(surv) ~ year, mm.sshr.spruce.no.seedscar)
mm.sshr.spruce.no.seedscar.ci <- data.frame(predict(mm.sshr.spruce.no.seedscar.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.sshr.spruce.no.seedscar.ci$fit <- exp(mm.sshr.spruce.no.seedscar.ci$fit)-1
mm.sshr.spruce.no.seedscar.mean <- data.frame(year.vals = seq(1,5,0.1),
                                           surv.mean = mm.sshr.spruce.no.seedscar.ci$fit) #only doing wells with 2015-2020 data

mm.sshr.spruce.ex.seedscar.mod <- lm(log1p(surv) ~ year, mm.sshr.spruce.ex.seedscar)
mm.sshr.spruce.ex.seedscar.ci <- data.frame(predict(mm.sshr.spruce.ex.seedscar.mod, newdata=data.frame(year=seq(1,5,0.1)), interval="confidence", level = 0.95))
mm.sshr.spruce.ex.seedscar.ci$fit <- exp(mm.sshr.spruce.ex.seedscar.ci$fit)-1
mm.sshr.spruce.ex.seedscar.mean <- data.frame(year.vals = seq(1,5,0.1),
                                           surv.mean = mm.sshr.spruce.ex.seedscar.ci$fit) #only doing wells with 2015-2020 data

# Survival data collation ----
## Nalp and Salp fir ----
mm.nalp.fir.no %>%
        group_by(treatment) %>%
        summarise(surv.prop.0 = mean(surv.prop.0, na.rm = T),
                  surv.prop.1 = mean(surv.prop.1, na.rm = T),
                  surv.prop.2 = mean(surv.prop.2, na.rm = T),
                  surv.prop.3 = mean(surv.prop.3, na.rm = T),
                  surv.prop.4 = mean(surv.prop.4, na.rm = T))

mm.nalp.fir.ex %>%
        group_by(treatment) %>%
        summarise(surv.prop.0 = mean(surv.prop.0, na.rm = T),
                  surv.prop.1 = mean(surv.prop.1, na.rm = T),
                  surv.prop.2 = mean(surv.prop.2, na.rm = T),
                  surv.prop.3 = mean(surv.prop.3, na.rm = T),
                  surv.prop.4 = mean(surv.prop.4, na.rm = T))

mm.salp.fir.no %>%
        group_by(treatment) %>%
        summarise(surv.prop.0 = mean(surv.prop.0, na.rm = T),
                  surv.prop.1 = mean(surv.prop.1, na.rm = T),
                  surv.prop.2 = mean(surv.prop.2, na.rm = T),
                  surv.prop.2 = mean(surv.prop.2, na.rm = T),
                  surv.prop.2 = mean(surv.prop.2, na.rm = T))

mm.salp.fir.ex %>%
        group_by(treatment) %>%
        summarise(surv.prop.0 = mean(surv.prop.0, na.rm = T),
                  surv.prop.1 = mean(surv.prop.1, na.rm = T),
                  surv.prop.2 = mean(surv.prop.2, na.rm = T),
                  surv.prop.3 = mean(surv.prop.3, na.rm = T),
                  surv.prop.4 = mean(surv.prop.4, na.rm = T))

## Fir figure ----
jpeg("./Earthwatch/MacPass/figures/fir.survival.jpg", width = 5, height = 7, units = "in", res = 300)
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(0.5, 2, 1, 1), oma = c(2.8,1,0,0))

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,5.15), ylim=c(0,1))
points(jitter(rep(1.05,15)), mm.nalp.fir.no$surv.prop.0[mm.nalp.fir.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(2.05,15)), mm.nalp.fir.no$surv.prop.1[mm.nalp.fir.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(3.05,15)), mm.nalp.fir.no$surv.prop.2[mm.nalp.fir.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(4.05,15)), mm.nalp.fir.no$surv.prop.3[mm.nalp.fir.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(5.05,15)), mm.nalp.fir.no$surv.prop.4[mm.nalp.fir.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
lines(mm.nalp.fir.no.seed.mean$year.vals, mm.nalp.fir.no.seed.mean$surv, lwd = 2, lty = 3, col = "red")

points(jitter(rep(0.95,15)), mm.nalp.fir.ex$surv.prop.0[mm.nalp.fir.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(1.95,15)), mm.nalp.fir.ex$surv.prop.1[mm.nalp.fir.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(2.95,15)), mm.nalp.fir.ex$surv.prop.2[mm.nalp.fir.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(3.95,15)), mm.nalp.fir.ex$surv.prop.3[mm.nalp.fir.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(4.95,15)), mm.nalp.fir.ex$surv.prop.4[mm.nalp.fir.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
lines(mm.nalp.fir.ex.seed.mean$year.vals, mm.nalp.fir.ex.seed.mean$surv, lwd = 2, lty = 3, col = "blue")

legend("right", legend = c("Caged","Uncaged"), pch = 21, bty = "n", col = "black", pt.bg = c("blue","red"))
legend("topright", legend = "a) Seeded", bty = "n")
box()
axis(side = 1, at = c(1:5), labels = F)
axis(side = 2)

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,5.15), ylim=c(0,1))
points(jitter(rep(1.025,15)), mm.nalp.fir.no$surv.prop.0[mm.nalp.fir.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(2.025,15)), mm.nalp.fir.no$surv.prop.1[mm.nalp.fir.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(3.025,15)), mm.nalp.fir.no$surv.prop.2[mm.nalp.fir.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(4.025,15)), mm.nalp.fir.no$surv.prop.3[mm.nalp.fir.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(5.025,15)), mm.nalp.fir.no$surv.prop.4[mm.nalp.fir.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
lines(mm.nalp.fir.no.seedscar.mean$year.vals, mm.nalp.fir.no.seedscar.mean$surv, lwd = 2, lty = 1, col = "red")

points(jitter(rep(0.975,15)), mm.nalp.fir.ex$surv.prop.0[mm.nalp.fir.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(1.975,15)), mm.nalp.fir.ex$surv.prop.1[mm.nalp.fir.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(2.975,15)), mm.nalp.fir.ex$surv.prop.2[mm.nalp.fir.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(3.975,15)), mm.nalp.fir.ex$surv.prop.3[mm.nalp.fir.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(4.975,15)), mm.nalp.fir.ex$surv.prop.4[mm.nalp.fir.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
lines(mm.nalp.fir.ex.seedscar.mean$year.vals, mm.nalp.fir.ex.seedscar.mean$surv, lwd = 2, lty = 1, col = "blue")

legend("topright", legend = "b) Seeded & scarified", bty = "n")
box()
axis(side = 1, at = c(1:5), labels = F)
axis(side = 2)

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,5.15), ylim=c(0,1))

points(jitter(rep(1.05,15)), mm.salp.fir.no$surv.prop.0[mm.salp.fir.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(2.05,15)), mm.salp.fir.no$surv.prop.1[mm.salp.fir.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(3.05,15)), mm.salp.fir.no$surv.prop.2[mm.salp.fir.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(4.05,15)), mm.salp.fir.no$surv.prop.3[mm.salp.fir.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(5.05,15)), mm.salp.fir.no$surv.prop.4[mm.salp.fir.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
lines(mm.salp.fir.no.seed.mean$year.vals, mm.salp.fir.no.seed.mean$surv, lwd = 2, lty = 3, col = "red")

points(jitter(rep(0.95,15)), mm.salp.fir.ex$surv.prop.0[mm.salp.fir.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(1.95,15)), mm.salp.fir.ex$surv.prop.1[mm.salp.fir.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(2.95,15)), mm.salp.fir.ex$surv.prop.2[mm.salp.fir.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(3.95,15)), mm.salp.fir.ex$surv.prop.3[mm.salp.fir.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(4.95,15)), mm.salp.fir.ex$surv.prop.4[mm.salp.fir.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
lines(mm.salp.fir.ex.seed.mean$year.vals, mm.salp.fir.ex.seed.mean$surv, lwd = 2, lty = 3, col = "blue")

legend("topright", legend = "c) Seeded", bty = "n")
box()
axis(side = 1, at = c(1:5), labels = F)
axis(side = 2)

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,5.15), ylim=c(0,1))
points(jitter(rep(1.025,15)), mm.salp.fir.no$surv.prop.0[mm.salp.fir.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(2.025,15)), mm.salp.fir.no$surv.prop.1[mm.salp.fir.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(3.025,15)), mm.salp.fir.no$surv.prop.2[mm.salp.fir.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(4.025,15)), mm.salp.fir.no$surv.prop.3[mm.salp.fir.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(5.025,15)), mm.salp.fir.no$surv.prop.4[mm.salp.fir.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
lines(mm.salp.fir.no.seedscar.mean$year.vals, mm.salp.fir.no.seedscar.mean$surv, lwd = 2, lty = 1, col = "red")

points(jitter(rep(0.975,15)), mm.salp.fir.ex$surv.prop.0[mm.salp.fir.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(1.975,15)), mm.salp.fir.ex$surv.prop.1[mm.salp.fir.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(2.975,15)), mm.salp.fir.ex$surv.prop.2[mm.salp.fir.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(3.975,15)), mm.salp.fir.ex$surv.prop.3[mm.salp.fir.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(4.975,15)), mm.salp.fir.ex$surv.prop.4[mm.salp.fir.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
lines(mm.salp.fir.ex.seedscar.mean$year.vals, mm.salp.fir.ex.seedscar.mean$surv, lwd = 2, lty = 1, col = "blue")

legend("topright", legend = "d) Seeded & scarified", bty = "n")
box()
axis(side = 1, at = c(1:5))
axis(side = 2)
mtext("Year post germination", side = 1, outer = T, line = 1.5)
mtext("Survival proportion", side = 2, outer = T)
dev.off()


## Nalp and Salp spruce ----
mm.nalp.spruce.no %>%
  group_by(treatment) %>%
  summarise(surv.prop.0 = mean(surv.prop.0, na.rm = T),
            surv.prop.1 = mean(surv.prop.1, na.rm = T),
            surv.prop.2 = mean(surv.prop.2, na.rm = T),
            surv.prop.3 = mean(surv.prop.3, na.rm = T),
            surv.prop.4 = mean(surv.prop.4, na.rm = T))

mm.nalp.spruce.ex %>%
  group_by(treatment) %>%
  summarise(surv.prop.0 = mean(surv.prop.0, na.rm = T),
            surv.prop.1 = mean(surv.prop.1, na.rm = T),
            surv.prop.2 = mean(surv.prop.2, na.rm = T),
            surv.prop.3 = mean(surv.prop.3, na.rm = T),
            surv.prop.4 = mean(surv.prop.4, na.rm = T))

mm.salp.spruce.no %>%
  group_by(treatment) %>%
  summarise(surv.prop.0 = mean(surv.prop.0, na.rm = T),
            surv.prop.1 = mean(surv.prop.1, na.rm = T),
            surv.prop.2 = mean(surv.prop.2, na.rm = T),
            surv.prop.3 = mean(surv.prop.3, na.rm = T),
            surv.prop.4 = mean(surv.prop.4, na.rm = T))

mm.salp.spruce.ex %>%
  group_by(treatment) %>%
  summarise(surv.prop.0 = mean(surv.prop.0, na.rm = T),
            surv.prop.1 = mean(surv.prop.1, na.rm = T),
            surv.prop.2 = mean(surv.prop.2, na.rm = T),
            surv.prop.3 = mean(surv.prop.3, na.rm = T),
            surv.prop.4 = mean(surv.prop.4, na.rm = T))

## Spruce figure ----
jpeg("./Earthwatch/MacPass/figures/spruce.survival.jpg", width = 5, height = 7, units = "in", res = 300)
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(0.5, 2, 1, 1), oma = c(2.8,1,0,0))

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,5.15), ylim=c(0,1))
points(jitter(rep(1.05,15)), mm.nalp.spruce.no$surv.prop.0[mm.nalp.spruce.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(2.05,15)), mm.nalp.spruce.no$surv.prop.1[mm.nalp.spruce.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(3.05,15)), mm.nalp.spruce.no$surv.prop.2[mm.nalp.spruce.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(4.05,15)), mm.nalp.spruce.no$surv.prop.3[mm.nalp.spruce.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(5.05,15)), mm.nalp.spruce.no$surv.prop.4[mm.nalp.spruce.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
lines(mm.nalp.spruce.no.seed.mean$year.vals, mm.nalp.spruce.no.seed.mean$surv, lwd = 2, lty = 3, col = "red")

points(jitter(rep(0.95,15)), mm.nalp.spruce.ex$surv.prop.0[mm.nalp.spruce.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(1.95,15)), mm.nalp.spruce.ex$surv.prop.1[mm.nalp.spruce.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(2.95,15)), mm.nalp.spruce.ex$surv.prop.2[mm.nalp.spruce.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(3.95,15)), mm.nalp.spruce.ex$surv.prop.3[mm.nalp.spruce.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(4.95,15)), mm.nalp.spruce.ex$surv.prop.4[mm.nalp.spruce.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
lines(mm.nalp.spruce.ex.seed.mean$year.vals, mm.nalp.spruce.ex.seed.mean$surv, lwd = 2, lty = 3, col = "blue")

legend("right", legend = c("Caged","Uncaged"), pch = 21, bty = "n", col = "black", pt.bg = c("blue","red"))
legend("topright", legend = "a) Seeded", bty = "n")
box()
axis(side = 1, at = c(1:5), labels = F)
axis(side = 2)

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,5.15), ylim=c(0,1))
points(jitter(rep(1.025,15)), mm.nalp.spruce.no$surv.prop.0[mm.nalp.spruce.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(2.025,15)), mm.nalp.spruce.no$surv.prop.1[mm.nalp.spruce.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(3.025,15)), mm.nalp.spruce.no$surv.prop.2[mm.nalp.spruce.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(4.025,15)), mm.nalp.spruce.no$surv.prop.3[mm.nalp.spruce.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(5.025,15)), mm.nalp.spruce.no$surv.prop.4[mm.nalp.spruce.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
lines(mm.nalp.spruce.no.seedscar.mean$year.vals, mm.nalp.spruce.no.seedscar.mean$surv, lwd = 2, lty = 1, col = "red")

points(jitter(rep(0.975,15)), mm.nalp.spruce.ex$surv.prop.0[mm.nalp.spruce.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(1.975,15)), mm.nalp.spruce.ex$surv.prop.1[mm.nalp.spruce.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(2.975,15)), mm.nalp.spruce.ex$surv.prop.2[mm.nalp.spruce.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(3.975,15)), mm.nalp.spruce.ex$surv.prop.3[mm.nalp.spruce.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(4.975,15)), mm.nalp.spruce.ex$surv.prop.4[mm.nalp.spruce.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
lines(mm.nalp.spruce.ex.seedscar.mean$year.vals, mm.nalp.spruce.ex.seedscar.mean$surv, lwd = 2, lty = 1, col = "blue")

legend("topright", legend = "b) Seeded & scarified", bty = "n")
box()
axis(side = 1, at = c(1:5), labels = F)
axis(side = 2)

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,5.15), ylim=c(0,1))

points(jitter(rep(1.05,15)), mm.salp.spruce.no$surv.prop.0[mm.salp.spruce.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(2.05,15)), mm.salp.spruce.no$surv.prop.1[mm.salp.spruce.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(3.05,15)), mm.salp.spruce.no$surv.prop.2[mm.salp.spruce.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(4.05,15)), mm.salp.spruce.no$surv.prop.3[mm.salp.spruce.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(5.05,15)), mm.salp.spruce.no$surv.prop.4[mm.salp.spruce.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
lines(mm.salp.spruce.no.seed.mean$year.vals, mm.salp.spruce.no.seed.mean$surv, lwd = 2, lty = 3, col = "red")

points(jitter(rep(0.95,15)), mm.salp.spruce.ex$surv.prop.0[mm.salp.spruce.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(1.95,15)), mm.salp.spruce.ex$surv.prop.1[mm.salp.spruce.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(2.95,15)), mm.salp.spruce.ex$surv.prop.2[mm.salp.spruce.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(3.95,15)), mm.salp.spruce.ex$surv.prop.3[mm.salp.spruce.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(4.95,15)), mm.salp.spruce.ex$surv.prop.4[mm.salp.spruce.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
lines(mm.salp.spruce.ex.seed.mean$year.vals, mm.salp.spruce.ex.seed.mean$surv, lwd = 2, lty = 3, col = "blue")

legend("topright", legend = "c) Seeded", bty = "n")
box()
axis(side = 1, at = c(1:5), labels = F)
axis(side = 2)

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,5.15), ylim=c(0,1))
points(jitter(rep(1.025,15)), mm.salp.spruce.no$surv.prop.0[mm.salp.spruce.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(2.025,15)), mm.salp.spruce.no$surv.prop.1[mm.salp.spruce.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(3.025,15)), mm.salp.spruce.no$surv.prop.2[mm.salp.spruce.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(4.025,15)), mm.salp.spruce.no$surv.prop.3[mm.salp.spruce.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(5.025,15)), mm.salp.spruce.no$surv.prop.4[mm.salp.spruce.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
lines(mm.salp.spruce.no.seedscar.mean$year.vals, mm.salp.spruce.no.seedscar.mean$surv, lwd = 2, lty = 1, col = "red")

points(jitter(rep(0.975,15)), mm.salp.spruce.ex$surv.prop.0[mm.salp.spruce.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(1.975,15)), mm.salp.spruce.ex$surv.prop.1[mm.salp.spruce.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(2.975,15)), mm.salp.spruce.ex$surv.prop.2[mm.salp.spruce.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(3.975,15)), mm.salp.spruce.ex$surv.prop.3[mm.salp.spruce.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(4.975,15)), mm.salp.spruce.ex$surv.prop.4[mm.salp.spruce.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
lines(mm.salp.spruce.ex.seedscar.mean$year.vals, mm.salp.spruce.ex.seedscar.mean$surv, lwd = 2, lty = 1, col = "blue")

legend("topright", legend = "d) Seeded & scarified", bty = "n")
box()
axis(side = 1, at = c(1:5))
axis(side = 2)
mtext("Year post germination", side = 1, outer = T, line = 1.5)
mtext("Survival proportion", side = 2, outer = T)
dev.off()


## SCUT and SSHR fir ----
mm.scut.fir.no %>%
        group_by(treatment) %>%
        summarise(surv.prop.0 = mean(surv.prop.0, na.rm = T),
                  surv.prop.1 = mean(surv.prop.1, na.rm = T),
                  surv.prop.2 = mean(surv.prop.2, na.rm = T),
                  surv.prop.3 = mean(surv.prop.3, na.rm = T),
                  surv.prop.4 = mean(surv.prop.4, na.rm = T))

mm.scut.fir.ex %>%
        group_by(treatment) %>%
        summarise(surv.prop.0 = mean(surv.prop.0, na.rm = T),
                  surv.prop.1 = mean(surv.prop.1, na.rm = T),
                  surv.prop.2 = mean(surv.prop.2, na.rm = T),
                  surv.prop.3 = mean(surv.prop.3, na.rm = T),
                  surv.prop.4 = mean(surv.prop.4, na.rm = T))

mm.sshr.fir.no %>%
        group_by(treatment) %>%
        summarise(surv.prop.0 = mean(surv.prop.0, na.rm = T),
                  surv.prop.1 = mean(surv.prop.1, na.rm = T),
                  surv.prop.2 = mean(surv.prop.2, na.rm = T),
                  surv.prop.3 = mean(surv.prop.3, na.rm = T),
                  surv.prop.4 = mean(surv.prop.4, na.rm = T))

mm.sshr.fir.ex %>%
        group_by(treatment) %>%
        summarise(surv.prop.0 = mean(surv.prop.0, na.rm = T),
                  surv.prop.1 = mean(surv.prop.1, na.rm = T),
                  surv.prop.2 = mean(surv.prop.2, na.rm = T),
                  surv.prop.3 = mean(surv.prop.3, na.rm = T),
                  surv.prop.4 = mean(surv.prop.4, na.rm = T))

jpeg("./Earthwatch/MacPass/figures/fir.survival.shrubs.jpg", width = 5, height = 7, units = "in", res = 300)
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(0.5, 2, 1, 1), oma = c(2.8,1,0,0))

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,5.15), ylim=c(0,1))
# points(jitter(rep(1.05,15)), mm.scut.fir.no$surv.prop.0[mm.scut.fir.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(2.05,15)), mm.scut.fir.no$surv.prop.1[mm.scut.fir.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(3.05,15)), mm.scut.fir.no$surv.prop.2[mm.scut.fir.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(4.05,15)), mm.scut.fir.no$surv.prop.3[mm.scut.fir.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(5.05,15)), mm.scut.fir.no$surv.prop.4[mm.scut.fir.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# lines(mm.scut.fir.no.seed.mean$year.vals, mm.scut.fir.no.seed.mean$surv, lwd = 2, lty = 3, col = "red")

points(jitter(rep(0.95,15)), mm.scut.fir.ex$surv.prop.0[mm.scut.fir.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(1.95,15)), mm.scut.fir.ex$surv.prop.1[mm.scut.fir.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(2.95,15)), mm.scut.fir.ex$surv.prop.2[mm.scut.fir.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(3.95,15)), mm.scut.fir.ex$surv.prop.3[mm.scut.fir.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(4.95,15)), mm.scut.fir.ex$surv.prop.4[mm.scut.fir.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
lines(mm.scut.fir.ex.seed.mean$year.vals, mm.scut.fir.ex.seed.mean$surv, lwd = 2, lty = 3, col = "blue")

legend("top", legend = c("Caged","Uncaged"), pch = 21, bty = "n", col = "black", pt.bg = c("blue","red"))
legend("topright", legend = "a) Seeded", bty = "n")
box()
axis(side = 1, at = c(1:5), labels = F)
axis(side = 2)

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,5.15), ylim=c(0,1))
# points(jitter(rep(1.025,15)), mm.scut.fir.no$surv.prop.0[mm.scut.fir.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# points(jitter(rep(2.025,15)), mm.scut.fir.no$surv.prop.1[mm.scut.fir.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# points(jitter(rep(3.025,15)), mm.scut.fir.no$surv.prop.2[mm.scut.fir.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# points(jitter(rep(4.025,15)), mm.scut.fir.no$surv.prop.3[mm.scut.fir.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# points(jitter(rep(5.025,15)), mm.scut.fir.no$surv.prop.4[mm.scut.fir.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# lines(mm.scut.fir.no.seedscar.mean$year.vals, mm.scut.fir.no.seedscar.mean$surv, lwd = 2, lty = 1, col = "red")

points(jitter(rep(0.975,15)), mm.scut.fir.ex$surv.prop.0[mm.scut.fir.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(1.975,15)), mm.scut.fir.ex$surv.prop.1[mm.scut.fir.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(2.975,15)), mm.scut.fir.ex$surv.prop.2[mm.scut.fir.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(3.975,15)), mm.scut.fir.ex$surv.prop.3[mm.scut.fir.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(4.975,15)), mm.scut.fir.ex$surv.prop.4[mm.scut.fir.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
lines(mm.scut.fir.ex.seedscar.mean$year.vals, mm.scut.fir.ex.seedscar.mean$surv, lwd = 2, lty = 1, col = "blue")

legend("topright", legend = "b) Seeded & scarified", bty = "n")
box()
axis(side = 1, at = c(1:5), labels = F)
axis(side = 2)

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,5.15), ylim=c(0,1))
# points(jitter(rep(1.05,15)), mm.sshr.fir.no$surv.prop.0[mm.sshr.fir.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(2.05,15)), mm.sshr.fir.no$surv.prop.1[mm.sshr.fir.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(3.05,15)), mm.sshr.fir.no$surv.prop.2[mm.sshr.fir.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(4.05,15)), mm.sshr.fir.no$surv.prop.3[mm.sshr.fir.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(5.05,15)), mm.sshr.fir.no$surv.prop.4[mm.sshr.fir.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# lines(mm.sshr.fir.no.seed.mean$year.vals, mm.sshr.fir.no.seed.mean$surv, lwd = 2, lty = 3, col = "red")

points(jitter(rep(0.95,15)), mm.sshr.fir.ex$surv.prop.0[mm.sshr.fir.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(1.95,15)), mm.sshr.fir.ex$surv.prop.1[mm.sshr.fir.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(2.95,15)), mm.sshr.fir.ex$surv.prop.2[mm.sshr.fir.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(3.95,15)), mm.sshr.fir.ex$surv.prop.3[mm.sshr.fir.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(4.95,15)), mm.sshr.fir.ex$surv.prop.4[mm.sshr.fir.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
lines(mm.sshr.fir.ex.seed.mean$year.vals, mm.sshr.fir.ex.seed.mean$surv, lwd = 2, lty = 3, col = "blue")

legend("topright", legend = "c) Seeded", bty = "n")
box()
axis(side = 1, at = c(1:5), labels = F)
axis(side = 2)

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,5.15), ylim=c(0,1))
# points(jitter(rep(1.025,15)), mm.sshr.fir.no$surv.prop.0[mm.sshr.fir.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# points(jitter(rep(2.025,15)), mm.sshr.fir.no$surv.prop.1[mm.sshr.fir.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# points(jitter(rep(3.025,15)), mm.sshr.fir.no$surv.prop.2[mm.sshr.fir.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# points(jitter(rep(4.025,15)), mm.sshr.fir.no$surv.prop.3[mm.sshr.fir.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# points(jitter(rep(5.025,15)), mm.sshr.fir.no$surv.prop.4[mm.sshr.fir.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# lines(mm.sshr.fir.no.seedscar.mean$year.vals, mm.sshr.fir.no.seedscar.mean$surv, lwd = 2, lty = 1, col = "red")

points(jitter(rep(0.975,15)), mm.sshr.fir.ex$surv.prop.0[mm.sshr.fir.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(1.975,15)), mm.sshr.fir.ex$surv.prop.1[mm.sshr.fir.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(2.975,15)), mm.sshr.fir.ex$surv.prop.2[mm.sshr.fir.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(3.975,15)), mm.sshr.fir.ex$surv.prop.3[mm.sshr.fir.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(4.975,15)), mm.sshr.fir.ex$surv.prop.4[mm.sshr.fir.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
lines(mm.sshr.fir.ex.seedscar.mean$year.vals, mm.sshr.fir.ex.seedscar.mean$surv, lwd = 2, lty = 1, col = "blue")

legend("topright", legend = "d) Seeded & scarified", bty = "n")
box()
axis(side = 1, at = c(1:5))
axis(side = 2)
mtext("Year post germination", side = 1, outer = T, line = 1.5)
mtext("Survival proportion", side = 2, outer = T)
dev.off()

## SCUT and SSHR spruce ----
mm.scut.spruce.no %>%
  group_by(treatment) %>%
  summarise(surv.prop.0 = mean(surv.prop.0, na.rm = T),
            surv.prop.1 = mean(surv.prop.1, na.rm = T),
            surv.prop.2 = mean(surv.prop.2, na.rm = T),
            surv.prop.3 = mean(surv.prop.3, na.rm = T),
            surv.prop.4 = mean(surv.prop.4, na.rm = T))

mm.scut.spruce.ex %>%
  group_by(treatment) %>%
  summarise(surv.prop.0 = mean(surv.prop.0, na.rm = T),
            surv.prop.1 = mean(surv.prop.1, na.rm = T),
            surv.prop.2 = mean(surv.prop.2, na.rm = T),
            surv.prop.3 = mean(surv.prop.3, na.rm = T),
            surv.prop.4 = mean(surv.prop.4, na.rm = T))

mm.sshr.spruce.no %>%
  group_by(treatment) %>%
  summarise(surv.prop.0 = mean(surv.prop.0, na.rm = T),
            surv.prop.1 = mean(surv.prop.1, na.rm = T),
            surv.prop.2 = mean(surv.prop.2, na.rm = T),
            surv.prop.3 = mean(surv.prop.3, na.rm = T),
            surv.prop.4 = mean(surv.prop.4, na.rm = T))

mm.sshr.spruce.ex %>%
  group_by(treatment) %>%
  summarise(surv.prop.0 = mean(surv.prop.0, na.rm = T),
            surv.prop.1 = mean(surv.prop.1, na.rm = T),
            surv.prop.2 = mean(surv.prop.2, na.rm = T),
            surv.prop.3 = mean(surv.prop.3, na.rm = T),
            surv.prop.4 = mean(surv.prop.4, na.rm = T))

jpeg("./Earthwatch/MacPass/figures/spruce.survival.shrubs.jpg", width = 5, height = 7, units = "in", res = 300)
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(0.5, 2, 1, 1), oma = c(2.8,1,0,0))

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,5.15), ylim=c(0,1))
points(jitter(rep(1.05,15)), mm.scut.spruce.no$surv.prop.0[mm.scut.spruce.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(2.05,15)), mm.scut.spruce.no$surv.prop.1[mm.scut.spruce.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(3.05,15)), mm.scut.spruce.no$surv.prop.2[mm.scut.spruce.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(4.05,15)), mm.scut.spruce.no$surv.prop.3[mm.scut.spruce.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(5.05,15)), mm.scut.spruce.no$surv.prop.4[mm.scut.spruce.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
lines(mm.scut.spruce.no.seed.mean$year.vals, mm.scut.spruce.no.seed.mean$surv, lwd = 2, lty = 3, col = "red")

points(jitter(rep(0.95,15)), mm.scut.spruce.ex$surv.prop.0[mm.scut.spruce.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(1.95,15)), mm.scut.spruce.ex$surv.prop.1[mm.scut.spruce.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(2.95,15)), mm.scut.spruce.ex$surv.prop.2[mm.scut.spruce.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(3.95,15)), mm.scut.spruce.ex$surv.prop.3[mm.scut.spruce.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(4.95,15)), mm.scut.spruce.ex$surv.prop.4[mm.scut.spruce.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
lines(mm.scut.spruce.ex.seed.mean$year.vals, mm.scut.spruce.ex.seed.mean$surv, lwd = 2, lty = 3, col = "blue")

legend("top", legend = c("Caged","Uncaged"), pch = 21, bty = "n", col = "black", pt.bg = c("blue","red"))
legend("topright", legend = "a) Seeded", bty = "n")
box()
axis(side = 1, at = c(1:5), labels = F)
axis(side = 2)

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,5.15), ylim=c(0,1))
points(jitter(rep(1.025,15)), mm.scut.spruce.no$surv.prop.0[mm.scut.spruce.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(2.025,15)), mm.scut.spruce.no$surv.prop.1[mm.scut.spruce.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(3.025,15)), mm.scut.spruce.no$surv.prop.2[mm.scut.spruce.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(4.025,15)), mm.scut.spruce.no$surv.prop.3[mm.scut.spruce.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(5.025,15)), mm.scut.spruce.no$surv.prop.4[mm.scut.spruce.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
lines(mm.scut.spruce.no.seedscar.mean$year.vals, mm.scut.spruce.no.seedscar.mean$surv, lwd = 2, lty = 1, col = "red")

points(jitter(rep(0.975,15)), mm.scut.spruce.ex$surv.prop.0[mm.scut.spruce.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(1.975,15)), mm.scut.spruce.ex$surv.prop.1[mm.scut.spruce.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(2.975,15)), mm.scut.spruce.ex$surv.prop.2[mm.scut.spruce.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(3.975,15)), mm.scut.spruce.ex$surv.prop.3[mm.scut.spruce.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(4.975,15)), mm.scut.spruce.ex$surv.prop.4[mm.scut.spruce.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
lines(mm.scut.spruce.ex.seedscar.mean$year.vals, mm.scut.spruce.ex.seedscar.mean$surv, lwd = 2, lty = 1, col = "blue")

legend("topright", legend = "b) Seeded & scarified", bty = "n")
box()
axis(side = 1, at = c(1:5), labels = F)
axis(side = 2)

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,5.15), ylim=c(0,1))
# points(jitter(rep(1.05,15)), mm.sshr.spruce.no$surv.prop.0[mm.sshr.spruce.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(2.05,15)), mm.sshr.spruce.no$surv.prop.1[mm.sshr.spruce.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(3.05,15)), mm.sshr.spruce.no$surv.prop.2[mm.sshr.spruce.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(4.05,15)), mm.sshr.spruce.no$surv.prop.3[mm.sshr.spruce.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(5.05,15)), mm.sshr.spruce.no$surv.prop.4[mm.sshr.spruce.no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# lines(mm.sshr.spruce.no.seed.mean$year.vals, mm.sshr.spruce.no.seed.mean$surv, lwd = 2, lty = 3, col = "red")

points(jitter(rep(0.95,15)), mm.sshr.spruce.ex$surv.prop.0[mm.sshr.spruce.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(1.95,15)), mm.sshr.spruce.ex$surv.prop.1[mm.sshr.spruce.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(2.95,15)), mm.sshr.spruce.ex$surv.prop.2[mm.sshr.spruce.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(3.95,15)), mm.sshr.spruce.ex$surv.prop.3[mm.sshr.spruce.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(4.95,15)), mm.sshr.spruce.ex$surv.prop.4[mm.sshr.spruce.ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
lines(mm.sshr.spruce.ex.seed.mean$year.vals, mm.sshr.spruce.ex.seed.mean$surv, lwd = 2, lty = 3, col = "blue")

legend("topright", legend = "c) Seeded", bty = "n")
box()
axis(side = 1, at = c(1:5), labels = F)
axis(side = 2)

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,5.15), ylim=c(0,1))
points(jitter(rep(1.025,15)), mm.sshr.spruce.no$surv.prop.0[mm.sshr.spruce.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(2.025,15)), mm.sshr.spruce.no$surv.prop.1[mm.sshr.spruce.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(3.025,15)), mm.sshr.spruce.no$surv.prop.2[mm.sshr.spruce.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(4.025,15)), mm.sshr.spruce.no$surv.prop.3[mm.sshr.spruce.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(5.025,15)), mm.sshr.spruce.no$surv.prop.4[mm.sshr.spruce.no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
lines(mm.sshr.spruce.no.seedscar.mean$year.vals, mm.sshr.spruce.no.seedscar.mean$surv, lwd = 2, lty = 1, col = "red")

points(jitter(rep(0.975,15)), mm.sshr.spruce.ex$surv.prop.0[mm.sshr.spruce.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(1.975,15)), mm.sshr.spruce.ex$surv.prop.1[mm.sshr.spruce.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(2.975,15)), mm.sshr.spruce.ex$surv.prop.2[mm.sshr.spruce.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(3.975,15)), mm.sshr.spruce.ex$surv.prop.3[mm.sshr.spruce.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(4.975,15)), mm.sshr.spruce.ex$surv.prop.4[mm.sshr.spruce.ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
lines(mm.sshr.spruce.ex.seedscar.mean$year.vals, mm.sshr.spruce.ex.seedscar.mean$surv, lwd = 2, lty = 1, col = "blue")

legend("topright", legend = "d) Seeded & scarified", bty = "n")
box()
axis(side = 1, at = c(1:5))
axis(side = 2)
mtext("Year post germination", side = 1, outer = T, line = 1.5)
mtext("Survival proportion", side = 2, outer = T)
dev.off()



##***************
## Models (need to be updated: 2021-08-27)
f1 <- formula(germ.prop.via.int ~ treat * sow.year + exclosure | treat * sow.year + exclosure)
f1A <- formula(germ.prop.via.int ~ treat * exclosure | treat * exclosure)
f1B <- formula(germ.prop.via.int ~ treat + exclosure | treat + exclosure)
f1C <- formula(germ.prop.via.int ~ treat | treat)

## Fir
# Nalp ZINBs
nalp.fir.Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = mm.nalp.fir.seed)
nalp.fir.Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = mm.nalp.fir.seed)
nalp.fir.Nb1A <- zeroinfl(f1A, dist = "negbin", link = "logit", data = mm.nalp.fir.seed)
nalp.fir.Nb1B <- zeroinfl(f1B, dist = "negbin", link = "logit", data = mm.nalp.fir.seed)
nalp.fir.Nb1C <- zeroinfl(f1C, dist = "negbin", link = "logit", data = mm.nalp.fir.seed)
lrtest(nalp.fir.Zip1,nalp.fir.Nb1) # nb model is better than Poisson
lrtest(nalp.fir.Nb1,nalp.fir.Nb1A) # no diff
lrtest(nalp.fir.Nb1,nalp.fir.Nb1B) # no diff
lrtest(nalp.fir.Nb1,nalp.fir.Nb1C) # no diff
AIC(nalp.fir.Nb1,nalp.fir.Nb1A,nalp.fir.Nb1B,nalp.fir.Nb1C) # Model C has lowest AIC
summary(nalp.fir.Nb1A)
summary(nalp.fir.Nb1B)
summary(nalp.fir.Nb1C) # Treatment is significant (P = 0.00692)

# Salp ZINBs
salp.fir.Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = mm.salp.fir.seed)
salp.fir.Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = mm.salp.fir.seed)
salp.fir.Nb1A <- zeroinfl(f1A, dist = "negbin", link = "logit", data = mm.salp.fir.seed)
salp.fir.Nb1B <- zeroinfl(f1B, dist = "negbin", link = "logit", data = mm.salp.fir.seed)
salp.fir.Nb1C <- zeroinfl(f1C, dist = "negbin", link = "logit", data = mm.salp.fir.seed)
lrtest(salp.fir.Zip1,salp.fir.Nb1) # nb model is better
lrtest(salp.fir.Nb1,salp.fir.Nb1A) # A is better than the full
lrtest(salp.fir.Nb1,salp.fir.Nb1B) # B is better than the full
lrtest(salp.fir.Nb1,salp.fir.Nb1C) # C is better than the full
AIC(salp.fir.Nb1,salp.fir.Nb1A,salp.fir.Nb1B,salp.fir.Nb1C) # Model C has lowest AIC
summary(salp.fir.Nb1)
summary(salp.fir.Nb1A)
summary(salp.fir.Nb1B)
summary(salp.fir.Nb1C) # Treatment is significant (P = 0.00037)

# Scut ZINBs
scut.fir.Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = mm.scut.fir.seed) # Computationally singular
scut.fir.Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = mm.scut.fir.seed)
scut.fir.Nb1A <- zeroinfl(f1A, dist = "negbin", link = "logit", data = mm.scut.fir.seed) # Computationally singular
scut.fir.Nb1B <- zeroinfl(f1B, dist = "negbin", link = "logit", data = mm.scut.fir.seed)
scut.fir.Nb1C <- zeroinfl(f1C, dist = "negbin", link = "logit", data = mm.scut.fir.seed)
lrtest(scut.fir.Zip1,scut.fir.Nb1)
lrtest(scut.fir.Nb1,scut.fir.Nb1A)
lrtest(scut.fir.Nb1,scut.fir.Nb1B)
lrtest(scut.fir.Nb1,scut.fir.Nb1C)
AIC(scut.fir.Nb1,scut.fir.Nb1B,scut.fir.Nb1C) # Model B has lowest AIC
summary(scut.fir.Nb1)
summary(scut.fir.Nb1B) # Treatment (P = 0.00073) and exclosure (P = 0.00073) are significant
summary(scut.fir.Nb1C)

# Sshr ZINBs
sshr.fir.Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = mm.sshr.fir.seed) # Computationally singular
sshr.fir.Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = mm.sshr.fir.seed)   # Computationally singular
sshr.fir.Nb1A <- zeroinfl(f1A, dist = "negbin", link = "logit", data = mm.sshr.fir.seed) # Computationally singular
sshr.fir.Nb1B <- zeroinfl(f1B, dist = "negbin", link = "logit", data = mm.sshr.fir.seed) # Computationally singular
sshr.fir.Nb1C <- zeroinfl(f1C, dist = "negbin", link = "logit", data = mm.sshr.fir.seed) # Computationally singular
AIC(sshr.fir.Nb1C)
summary(sshr.fir.Nb1C) # Treatment is significant (P = 3.27e-05)


## Spruce
# Nalp ZINBs
nalp.spruce.Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = mm.nalp.spruce.seed)
nalp.spruce.Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = mm.nalp.spruce.seed)
nalp.spruce.Nb1A <- zeroinfl(f1A, dist = "negbin", link = "logit", data = mm.nalp.spruce.seed)
nalp.spruce.Nb1B <- zeroinfl(f1B, dist = "negbin", link = "logit", data = mm.nalp.spruce.seed)
nalp.spruce.Nb1C <- zeroinfl(f1C, dist = "negbin", link = "logit", data = mm.nalp.spruce.seed)
lrtest(nalp.spruce.Zip1,nalp.spruce.Nb1) # nb model is better than Poisson
lrtest(nalp.spruce.Nb1,nalp.spruce.Nb1A) # no diff
lrtest(nalp.spruce.Nb1,nalp.spruce.Nb1B) # no diff
lrtest(nalp.spruce.Nb1,nalp.spruce.Nb1C) # no diff
AIC(nalp.spruce.Nb1,nalp.spruce.Nb1A,nalp.spruce.Nb1B,nalp.spruce.Nb1C) # Model C has lowest AIC
summary(nalp.spruce.Nb1)
summary(nalp.spruce.Nb1A)
summary(nalp.spruce.Nb1B)
summary(nalp.spruce.Nb1C) # Treatment is ~significant (P = 0.0569)

# Salp ZINBs
salp.spruce.Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = mm.salp.spruce.seed)
salp.spruce.Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = mm.salp.spruce.seed) # Computationally singular
salp.spruce.Nb1A <- zeroinfl(f1A, dist = "negbin", link = "logit", data = mm.salp.spruce.seed)
salp.spruce.Nb1B <- zeroinfl(f1B, dist = "negbin", link = "logit", data = mm.salp.spruce.seed)
salp.spruce.Nb1C <- zeroinfl(f1C, dist = "negbin", link = "logit", data = mm.salp.spruce.seed)
# lrtest(salp.spruce.Zip1,salp.spruce.Nb1) # nb model is better
# lrtest(salp.spruce.Nb1,salp.spruce.Nb1A) # no diff
# lrtest(salp.spruce.Nb1,salp.spruce.Nb1B) # no diff
# lrtest(salp.spruce.Nb1,salp.spruce.Nb1C) # no diff
AIC(salp.spruce.Nb1A,salp.spruce.Nb1B,salp.spruce.Nb1C) # Model B has lowest AIC
# summary(salp.spruce.Nb1)
summary(salp.spruce.Nb1A)
summary(salp.spruce.Nb1B) # Treatment (P = 0.0174) and exclosure (P = 3.67e-05) are significant
summary(salp.spruce.Nb1C)

# Scut ZINBs
scut.spruce.Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = mm.scut.spruce.seed) # Computationally singular
scut.spruce.Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = mm.scut.spruce.seed) # Computationally singular
scut.spruce.Nb1A <- zeroinfl(f1A, dist = "negbin", link = "logit", data = mm.scut.spruce.seed)  # Computationally singular
scut.spruce.Nb1B <- zeroinfl(f1B, dist = "negbin", link = "logit", data = mm.scut.spruce.seed)
scut.spruce.Nb1C <- zeroinfl(f1C, dist = "negbin", link = "logit", data = mm.scut.spruce.seed)
# lrtest(scut.spruce.Zip1,scut.spruce.Nb1) # nb model is better
# lrtest(scut.spruce.Nb1,scut.spruce.Nb1A) # no diff
# lrtest(scut.spruce.Nb1,scut.spruce.Nb1B) # no diff
# lrtest(scut.spruce.Nb1,scut.spruce.Nb1C) # no diff
AIC(scut.spruce.Nb1B,scut.spruce.Nb1C) # Model B has lowest AIC
# summary(scut.spruce.Nb1)
# summary(scut.spruce.Nb1A)
summary(scut.spruce.Nb1B) # Treatment (P  = 7.79e-05) and exclosure (P = 7.79e-05) are significant
summary(scut.spruce.Nb1C)

# Sshr ZINBs
sshr.spruce.Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = mm.sshr.spruce.seed)
sshr.spruce.Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = mm.sshr.spruce.seed)  
sshr.spruce.Nb1A <- zeroinfl(f1A, dist = "negbin", link = "logit", data = mm.sshr.spruce.seed)
sshr.spruce.Nb1B <- zeroinfl(f1B, dist = "negbin", link = "logit", data = mm.sshr.spruce.seed)
sshr.spruce.Nb1C <- zeroinfl(f1C, dist = "negbin", link = "logit", data = mm.sshr.spruce.seed)
lrtest(sshr.spruce.Zip1,sshr.spruce.Nb1) # nb model is better
lrtest(sshr.spruce.Nb1,sshr.spruce.Nb1A) # no diff
lrtest(sshr.spruce.Nb1,sshr.spruce.Nb1B) # no diff
lrtest(sshr.spruce.Nb1,sshr.spruce.Nb1C) # C is better than full
AIC(sshr.spruce.Nb1, sshr.spruce.Nb1A, sshr.spruce.Nb1B, sshr.spruce.Nb1C) # Model B has lowest AIC
summary(sshr.spruce.Nb1)
summary(sshr.spruce.Nb1A)
summary(sshr.spruce.Nb1B) # Exclosure is significant (P = 0.000147)
summary(sshr.spruce.Nb1C)



#***************************************************************
# Microclimate ----
mm <- read.csv("./Earthwatch/MacPass/data/microclimate_19900721_20220818_filled.csv", header = T)[-1]

# Format date, add in cool/warm seasons, season_year
mm <- mm %>% 
  mutate(Date = ymd(Date), 
         season_year = ifelse(month(Date) == 10 | month(Date) == 11 | month(Date) == 12,
                              year(Date) + 1, year(Date)),
         season = case_when(month(Date) %in% c(10,11,12,1,2,3,4,5) ~ "cool",
                            month(Date) %in% c(6, 7, 8, 9) ~ "warm",
                            T ~ NA_character_
             ))

# mm_surf <- mm %>% 
#   group_by(season_year) %>% 
#   summarise(across(ends_with(".0"), ~ mean(.x, na.rm = TRUE)))
# 
# mm_airt <- mm %>% 
#   group_by(season_year) %>% 
#   summarise(across(ends_with(".150"), ~ mean(.x, na.rm = TRUE)))
# 
# # mm_airt <- aggregate(. ~ season_year, mm[,c(37,12,5,22,17,30)], mean)
# matplot(mm_airt[,-1], type = "l")
# mm_airt.1 <- rowMeans(mm_airt[-1,-1])
# mm_airt.2 <- cbind.data.frame(seq(1991,2021,1), mm_airt.1)
# names(mm_airt.2) <- c("year","airT")
# plot(airT ~ year, mm_airt.2, type = "l")
# summary(lm(airT ~ year, data = mm_airt.2))
# abline(coef(lm(airT ~ year, data = mm_airt.2)))

mm_temps_season <- mm %>% 
  group_by(season_year, season) %>% 
  summarise(across(ends_with(c(".150",".0",".neg150")), ~ mean(.x, na.rm = TRUE)))

# 
# # mm_airt_season <- aggregate(. ~ season_year*season, mm[,c(37,38,12,5,22,17,30)], mean)
# mm_airt_season.mean <- rowMeans(mm_airt_season[,-c(1,2)])
# mm_airt_season.mean2 <- cbind.data.frame(c(seq(1991,2021,1),seq(1990,2021,1)), mm_airt_season.mean)
# names(mm_airt_season.mean2) <- c("year","airT")
# mm_airt.warm <- mm_airt_season.mean2[c(1:30),]
# mm_airt.cool <- mm_airt_season.mean2[c(32:62),]
# plot(airT ~ year, mm_airt.warm, type = "l")
# plot(airT ~ year, mm_airt.cool, type = "l")
# summary(lm(airT ~ year, data = mm_airt.warm))
# summary(lm(airT ~ year, data = mm_airt.cool))
# 
# mm.agg <- aggregate(. ~ season_year*season, mm[,c(37,38, 
#                                                   12,13,16, 
#                                                   5,6,9, 
#                                                   22,23,28, 
#                                                   17,18,21, 
#                                                   30,31,34)], mean)

mm_temps_w <- subset(mm_temps_season, season == "warm")
mm_temps_c <- subset(mm_temps_season, season == "cool")
matplot(mm_temps_w[,-c(1,2)], type = "l")
matplot(mm_temps_c[,-c(1,2)], type = "l")

## Air temperature regressions
summary(bp.150.w.lm <- lm(bp.150 ~ season_year, data = mm_temps_w[-c(1,33),]))  # b1 = 0.046, P = 0.00705 **
summary(hf.150.w.lm <- lm(hf.150 ~ season_year, data = mm_temps_w[-c(1,33),]))  # b1 = 0.033, P = 0.0437 *
summary(d6.150.w.lm <- lm(d6.150 ~ season_year, data = mm_temps_w[c(8:32),]))   # b1 = 0.011, P = 0.5497
summary(d2.150.w.lm <- lm(d2.150 ~ season_year, data = mm_temps_w[-c(1,33),]))  # b1 = 0.026, P = 0.127
summary(gf.150.w.lm <- lm(gf.150 ~ season_year, data = mm_temps_w[-c(1,33),]))  # b1 = 0.043, P = 0.0277 *

summary(bp.150.c.lm <- lm(bp.150 ~ season_year, data = mm_temps_c[-32,]))       # b1 = 0.027, P = 0.462
summary(hf.150.c.lm <- lm(hf.150 ~ season_year, data = mm_temps_c[-32,]))       # b1 = 0.026, P = 0.5368
summary(d6.150.c.lm <- lm(d6.150 ~ season_year, data = mm_temps_c[c(6:31),]))   # b1 = 0.051, P = 0.281
summary(d2.150.c.lm <- lm(d2.150 ~ season_year, data = mm_temps_c[-32,]))       # b1 = 0.025, P = 0.440
summary(gf.150.c.lm <- lm(gf.150 ~ season_year, data = mm_temps_c[-32,]))       # b1 = 0.047, P = 0.1067  

## Ground surface regressions
summary(bp.0.w.lm <- lm(bp.0 ~ season_year, data = mm_temps_w[-c(1,33),]))  # b1 = 0.027, P = 0.0818 .
summary(hf.0.w.lm <- lm(hf.0 ~ season_year, data = mm_temps_w[-c(1,33),]))  # b1 = -0.014, P = 0.345
summary(d6.0.w.lm <- lm(d6.0 ~ season_year, data = mm_temps_w[c(8:32),]))   # b1 = 0.018, P = 0.563
summary(d2.0.w.lm <- lm(d2.0 ~ season_year, data = mm_temps_w[-c(1,33),]))  # b1 = 0.025, P = 0.112
summary(gf.0.w.lm <- lm(gf.0 ~ season_year, data = mm_temps_w[-c(1,33),]))  # b1 = 0.075, P = 0.000933 ***

summary(bp.0.c.lm <- lm(bp.0 ~ season_year, data = mm_temps_c[-32,]))       # b1 = 0.060, P = 0.0441 *
summary(hf.0.c.lm <- lm(hf.0 ~ season_year, data = mm_temps_c[-32,]))       # b1 = 0.081, P = 0.0241 *
summary(d6.0.c.lm <- lm(d6.0 ~ season_year, data = mm_temps_c[c(6:31),]))   # b1 = -0.032, P = 0.135
summary(d2.0.c.lm <- lm(d2.0 ~ season_year, data = mm_temps_c[-32,]))       # b1 = 0.030, P = 0.473
summary(gf.0.c.lm <- lm(gf.0 ~ season_year, data = mm_temps_c[-32,]))       # b1 = 0.166, P = 2.85e-05 ***

## Permafrost regressions
summary(bp.neg150.w.lm <- lm(bp.neg150 ~ season_year, data = mm_temps_w[-c(1,33),])) # b1 = 0.021, P = 0.0001665***
summary(hf.neg150.w.lm <- lm(hf.neg150 ~ season_year, data = mm_temps_w[-c(1,33),])) # b1 = 0.017, P = 4.90e-07 ***
summary(d6.neg150.w.lm <- lm(d6.neg150 ~ season_year, data = mm_temps_w[c(8:32),])) # b1 = -0.003, P = 0.012*
summary(d2.neg150.w.lm <- lm(d2.neg150 ~ season_year, data = mm_temps_w[-c(1,33),])) # b1 = 0.010, P = 0.0469*
summary(gf.neg150.w.lm <- lm(gf.neg150 ~ season_year, data = mm_temps_w[-c(1,33),])) # b1 = 0.016, P = 4.237e-08***

summary(bp.neg150.c.lm <- lm(bp.neg150 ~ season_year, data = mm_temps_c[-32,])) # b1 = 0.033, P = 0.00387 **
summary(hf.neg150.c.lm <- lm(hf.neg150 ~ season_year, data = mm_temps_c[-32,])) # b1 = 0.030, P = 0.0521 .
summary(d6.neg150.c.lm <- lm(d6.neg150 ~ season_year, data = mm_temps_c[c(6:31),])) # b1 = -0.004, P = 0.391
summary(d2.neg150.c.lm <- lm(d2.neg150 ~ season_year, data = mm_temps_c[-32,])) # b1 = 0.036, P = 0.0601 .
summary(gf.neg150.c.lm <- lm(gf.neg150 ~ season_year, data = mm_temps_c[-32,])) # b1 = 0.048, P = 9.54e-09 ***


## Export at 6 x 3.5

# Warm
jpeg("./Earthwatch/MacPass/figures/warmT.jpg", width = 5, height = 7, units = "in", res = 300)
par(xpd = F)
par(mar = c(2,2,1,1), oma = c(2,2,0,0))
par(mfrow = c(3,1))
matplot(mm_temps_w$season_year[-c(1,33)], mm_temps_w[-c(1,33),c(3:5,7)], 
        type = "l", col = c("darkred","red","yellow","blue"), 
        lty = 1, ylab = "", xlab = "", ylim = c(3,11))
lines(mm_temps_w$season_year[-c(1:7,33)], mm_temps_w$d6.150[-c(1:7,33)], col = "orange")
mtext("Air (°C)", side = 2, line = 2.5, cex = 0.7)
abline(coef(hf.150.w.lm),lty=2, col = "darkred")
# abline(coef(bp.150.w.lm),lty=2, col = "red")
# abline(coef(d6.150.w.lm),lty=2, col = "orange")
# abline(coef(d2.150.w.lm),lty=2, col = "yellow")
abline(coef(gf.150.w.lm),lty=2, col = "blue")

matplot(mm_temps_w$season_year[-c(1,33)], mm_temps_w[-c(1,33),c(9:11,13)], 
        type = "l", col = c("darkred","red","yellow","blue"), 
        lty = 1, ylab = "", xlab = "", ylim = c(2,11))
lines(mm_temps_w$season_year[-c(1:7,33)], mm_temps_w$d6.0[-c(1:7,33)], col = "orange")
mtext("Ground surface (°C)", side = 2, line = 2.5, cex = 0.7)
# abline(coef(hf.0.w.lm),lty=2, col = "darkred")
# abline(coef(bp.0.w.lm),lty=2, col = "red")
# abline(coef(d6.0.w.lm),lty=2, col = "orange")
# abline(coef(d2.0.w.lm),lty=2, col = "yellow")
abline(coef(gf.0.w.lm),lty=2, col = "blue")

matplot(mm_temps_w$season_year[-c(1,33)], mm_temps_w[-c(1,33),c(14:16,18)], type = "l", col = c("darkred","red","yellow","blue"), 
        lty = 1, ylab = "", xlab = "", ylim = c(-2,0.5))
lines(mm_temps_w$season_year[-c(1:7,33)], mm_temps_w$d6.neg150[-c(1:7,33)], col = "orange")
mtext("Subsurface (°C)", side = 2, line = 2.5, cex = 0.7)
mtext("Year", side = 1, line = 2.5, cex = 0.7)
abline(coef(hf.neg150.w.lm),lty=2, col = "darkred")
abline(coef(bp.neg150.w.lm),lty=2, col = "red")
abline(coef(d6.neg150.w.lm),lty=2, col = "orange")
# abline(coef(d2.neg150.w.lm),lty=2, col = "yellow")
abline(coef(gf.neg150.w.lm),lty=2, col = "blue")

par(xpd = NA)
legend(1994,5.2, c("HF", "BP", "D6", "D2","GF"), 
       col = c("darkred","red","orange","yellow","blue"), 
       horiz = T, lty = 1, cex = 1, bty = "n", y.intersp = 1, text.width = 2)
dev.off()

# mm_temps_c$hf.150[mm_temps_c$season_year == 1992] <- mm_temps_c$hf.150[mm_temps_c$season_year == 1992]-2

# Cool
jpeg("./Earthwatch/MacPass/figures/coolT.jpg", width = 5, height = 7, units = "in", res = 300)
par(xpd = F)
par(mar = c(2,2,1,1), oma = c(2,2,0,0))
par(mfrow = c(3,1))
matplot(mm_temps_c$season_year[-32], mm_temps_c[-32,c(3:5,7)], 
        type = "l", col = c("darkred","red","yellow","blue"), 
        lty = 1, ylab = "", xlab = "", ylim = c(-18,-8))
lines(mm_temps_c$season_year[-c(1:6,32)], mm_temps_c$d6.150[-c(1:6,32)], col = "orange")
mtext("Air (°C)", side = 2, line = 2.5, cex = 0.7)
# abline(coef(hf.150.c.lm),lty=2, col = "darkred")
# abline(coef(bp.150.c.lm),lty=2, col = "red")
# abline(coef(d6.150.c.lm),lty=2, col = "orange")
# abline(coef(d2.150.c.lm),lty=2, col = "yellow")
# abline(coef(gf.150.c.lm),lty=2, col = "blue")

matplot(mm_temps_c$season_year[-32], mm_temps_c[-32,c(9:11,13)], type = "l", col = c("darkred","red","yellow","blue"), 
        lty = 1, ylab = "", xlab = "", ylim = c(-12,0))
lines(mm_temps_c$season_year[-c(1:6,32)], mm_temps_c$d6.0[-c(1:6,32)], col = "orange")
mtext("Ground surface (°C)", side = 2, line = 2.5, cex = 0.7)
abline(coef(hf.0.c.lm),lty=2, col = "darkred")
# abline(coef(bp.0.c.lm),lty=2, col = "red")
abline(coef(d6.0.c.lm),lty=2, col = "orange")
# abline(coef(d2.0.c.lm),lty=2, col = "yellow")
abline(coef(gf.0.c.lm),lty=2, col = "blue")

matplot(mm_temps_c$season_year[-32], mm_temps_c[-32,c(14:16,18)], type = "l", col = c("darkred","red","yellow","blue"), 
        lty = 1, ylab = "", xlab = "", ylim = c(-5,0))
lines(mm_temps_c$season_year[-c(1:6,32)], mm_temps_c$d6.neg150[-c(1:6,32)], col = "orange")
mtext("Subsurface (°C)", side = 2, line = 2.5, cex = 0.7)
mtext("Year", side = 1, line = 2.5, cex = 0.7)
abline(coef(hf.neg150.c.lm),lty=2, col = "darkred")
# abline(coef(bp.neg150.c.lm),lty=2, col = "red")
# abline(coef(d6.neg150.c.lm),lty=2, col = "orange")
# abline(coef(d2.neg150.c.lm),lty=2, col = "yellow")
abline(coef(gf.neg150.c.lm),lty=2, col = "blue")

par(xpd = NA)
legend(1994,8.8, c("HF", "BP", "D6", "D2","GF"), 
       col = c("darkred","red","orange","yellow","blue"), 
       horiz = T, lty = 1, cex = 1, bty = "n", y.intersp = 1, text.width = 2)
dev.off()

##***************************************************************
# Thaw depths ----
thaw.mm <- read.csv("./Earthwatch/MacPass/data/thaw.mm.csv", header = TRUE)
thaw.mm$ci <- thaw.mm$se*qt(0.975, thaw.mm$n-1)
thaw.mm$c5 <- thaw.mm$mean - thaw.mm$ci
thaw.mm$c95 <- thaw.mm$mean + thaw.mm$ci

d2 <- subset(thaw.mm, site == "D2")
d6 <- subset(thaw.mm, site == "D6")
beaver <- subset(thaw.mm, site == "Beaver")
hare <- subset(thaw.mm, site == "Hare")
goose <- subset(thaw.mm, site == "Goose")
snow <- subset(thaw.mm, site == "Snow")
snow <- rbind.data.frame(snow[c(1:8),],
                         data.frame(year = seq(2002,2005,1),
                                    site = rep("Snow",4),
                                    min = rep(NA,4),
                                    mean = rep(NA,4),
                                    max = rep(NA,4),
                                    sd = rep(NA,4),
                                    se = rep(NA,4),
                                    n = rep(NA,4),
                                    ci = rep(NA,4),
                                    c5 = rep(NA,4),
                                    c95 = rep(NA,4)),
                         snow[c(9:nrow(snow)),])
porsild <- subset(thaw.mm, site == "Porsild")
porsild.1 <- subset(thaw.mm, site == "Porsild.1")
porsild.2 <- subset(thaw.mm, site == "Porsild.2")
pipeline <- subset(thaw.mm, site == "Pipeline")
pp.control.p <- subset(thaw.mm, site == "PP.Control.P")
pp.pipeline.p <- subset(thaw.mm, site == "PP.Pipeline.P")
pp.track.t <- subset(thaw.mm, site == "PP.Track.T")
pp.control.t <- subset(thaw.mm, site == "PP.Control.T")

snow.lm <- lm(snow$mean~snow$year)
pipeline.lm <- lm(pipeline$mean~pipeline$year)
goose.lm <- lm(goose$mean~goose$year)
d2.lm <- lm(d2$mean~d2$year)
d6.lm <- lm(d6$mean~d6$year)
porsild.lm <- lm(porsild$mean~porsild$year)
porsild.1.lm <- lm(porsild.1$mean~porsild.1$year)
porsild.2.lm <- lm(porsild.2$mean~porsild.2$year)
beaver.lm <- lm(beaver$mean~beaver$year)
hare.lm <- lm(hare$mean~hare$year)

summary(snow.lm) # Sig
summary(pipeline.lm) # Sig
summary(goose.lm) # Sig
summary(d2.lm) # Sig
summary(d6.lm) # --
summary(porsild.lm) # Sig
summary(porsild.1.lm) # --
summary(porsild.2.lm) # Sig
summary(beaver.lm) # Sig
summary(hare.lm) # Sig

snow.slope <- round(coef(snow.lm)[[2]], 3)
pipeline.slope <- round(coef(pipeline.lm)[[2]], 3)
goose.slope <- round(coef(goose.lm)[[2]], 3)
d2.slope <- round(coef(d2.lm)[[2]], 3)
d6.slope <- round(coef(d6.lm)[[2]], 3)
porsild.slope <- round(coef(porsild.lm)[[2]], 3)
porsild.1.slope <- round(coef(porsild.1.lm)[[2]], 3)
porsild.2.slope <- round(coef(porsild.2.lm)[[2]], 3)
beaver.slope <- round(coef(beaver.lm)[[2]], 3)
hare.slope <- round(coef(hare.lm)[[2]], 3)

# Export at 7 x 7
# Export at 5 x 3.5

# legend(1994,13.8, c("HF", "BP", "D6", "D2","GF"), 
#        col = c("darkred","red","orange","yellow","blue"), 
#        horiz = T, lty = 1, cex = 1, bty = "n", y.intersp = 1, text.width = 2)

jpeg("./Earthwatch/MacPass/figures/hare.thaw.jpeg", width = 6, height = 4, units = "in", res = 300)
par(mar = c(3.3, 3.3, 1, 3.3))
plot(hare$year, hare$mean, type='n', xlim = c(1990,2023), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rev(hare$year), hare$year), c(rev(hare$c95), hare$c5), col = alpha("darkred",0.6), border = NA)
lines(hare$year, hare$mean, col = 'darkred', lwd = 2)
# points(hare$year[32], hare$mean[32], col = 'darkred', pch = 16)
abline(coef(hare.lm), lty = 2, lwd = 2, col = "darkred")
par(new = T)
plot(hare$year, hare$n, col = 'darkred', type = "l", xlim = c(1990,2023), ylim = c(10,100), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(hare$year[32], hare$n[32], col = 'darkred', pch = 17)
axis(side = 4, at = seq(0,40,10))
legend("topleft", "HF (1260 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(hare.slope) ~ "cm yr"^-1), bty = "n")
mtext("Thaw depth (cm)", side=2, cex = 1, line = 2.2)
mtext("Probe points (n)", side=4, cex = 1, line = 2.2, adj = 0)
mtext("Year", side=1,  cex=1, line = 2.2)
dev.off()

jpeg("./Earthwatch/MacPass/figures/beaver.thaw.jpeg", width = 6, height = 4, units = "in", res = 300)
par(mar = c(3.3, 3.3, 1, 3.3))
plot(beaver$year, beaver$mean, type='n', xlim = c(1990,2023), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rev(beaver$year), beaver$year), c(rev(beaver$c95), beaver$c5), col = alpha("red",0.6), border = NA)
lines(beaver$year, beaver$mean, col = 'red', lwd = 2)
# points(beaver$year[32], beaver$mean[32], col = 'red', pch = 16)
abline(coef(beaver.lm), lty = 2, lwd = 2, col = "red")
par(new = T)
plot(beaver$year, beaver$n, col = 'red', type = "l", xlim = c(1990,2023), ylim = c(30,150), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(beaver$year[32], beaver$n[32], col = 'red', pch = 17)
axis(side = 4, at = seq(0,80,20))
legend("topleft", "BP (1272 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(beaver.slope) ~ "cm yr"^-1), bty = "n", inset = c(0,0.1))
mtext("Thaw depth (cm)", side=2, cex = 1, line = 2.2)
mtext("Probe points (n)", side=4, cex = 1, line = 2.2, adj = 0)
mtext("Year", side=1,  cex=1, line = 2.2)
dev.off()

jpeg("./Earthwatch/MacPass/figures/porsild.1.thaw.jpeg", width = 6, height = 4, units = "in", res = 300)
par(mar = c(3.3, 3.3, 1, 3.3))
plot(porsild.1$year, porsild.1$mean, type='n', xlim = c(1990,2023), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rev(porsild.1$year), porsild.1$year), c(rev(porsild.1$c95), porsild.1$c5), col = alpha("violet",0.6), border = NA)
lines(porsild.1$year, porsild.1$mean, col = 'violet', lwd = 2)
# points(porsild.1$year[16], porsild.1$mean[16], col = 'violet', pch = 16)
# abline(coef(porsild.1.lm), lty = 2, lwd = 2, col = "violet")
par(new = T)
plot(porsild.1$year, porsild.1$n, col = 'violet', type = "l", xlim = c(1990,2023), ylim = c(20,80), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(porsild.1$year[32], porsild.1$n[32], col = 'violet', pch = 17)
axis(side = 4, at = seq(0,40,5))
legend("topleft", "PF1 (1380 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
# legend("bottomleft", legend=bquote("Slope = " ~ .(porsild.1.slope) ~ "cm yr"^-1), bty = "n", inset = c(0,0.1))
mtext("Thaw depth (cm)", side=2, cex = 1, line = 2.2)
mtext("Probe points (n)", side=4, cex = 1, line = 2.2, adj = 0)
mtext("Year", side=1,  cex=1, line = 2.2)
dev.off()

jpeg("./Earthwatch/MacPass/figures/porsild.2.thaw.jpeg", width = 6, height = 4, units = "in", res = 300)
par(mar = c(3.3, 3.3, 1, 3.3))
plot(porsild.2$year, porsild.2$mean, type='n', xlim = c(1990,2023), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rev(porsild.2$year), porsild.2$year), c(rev(porsild.2$c95), porsild.2$c5), col = alpha("violet",0.6), border = NA)
lines(porsild.2$year, porsild.2$mean, col = 'violet', lwd = 2)
# points(porsild.2$year[16], porsild.2$mean[16], col = 'violet', pch = 16)
abline(coef(porsild.2.lm), lty = 2, lwd = 2, col = "violet")
par(new = T)
plot(porsild.2$year, porsild.2$n, col = 'violet', type = "l", xlim = c(1990,2023), ylim = c(0,120), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(porsild.2$year[32], porsild.2$n[32], col = 'violet', pch = 17)
axis(side = 4, at = seq(0,30,5))
legend("topleft", "PF2 (1380 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(porsild.2.slope) ~ "cm yr"^-1), bty = "n", inset = c(0,0.1))
mtext("Thaw depth (cm)", side=2, cex = 1, line = 2.2)
mtext("Probe points (n)", side=4, cex = 1, line = 2.2, adj = 0)
mtext("Year", side=1,  cex=1, line = 2.2)
dev.off()

jpeg("./Earthwatch/MacPass/figures/d6.thaw.jpeg", width = 6, height = 4, units = "in", res = 300)
par(mar = c(3.3, 3.3, 1, 3.3))
plot(d6$year, d6$mean, type='n', xlim = c(1990,2023), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rev(d6$year), d6$year), c(rev(d6$c95), d6$c5), col = alpha("orange",0.6), border = NA)
lines(d6$year, d6$mean, col = 'orange', lwd = 2)
# points(d6$year[32], d6$mean[32], col = 'orange', pch = 16)
# abline(coef(d6.lm), lty = 2, lwd = 2, col = "orange")
par(new = T)
plot(d6$year, d6$n, col = 'orange', type = "l", xlim = c(1990,2023), ylim = c(15,120), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(d6$year[32], d6$n[32], col = 'orange', pch = 17)
axis(side = 4, at = seq(0,40,10))
legend("topleft", "D6 (1473 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
# legend("bottomleft", legend=bquote("Slope = " ~ .(d6.slope) ~ "cm yr"^-1), bty = "n")
mtext("Thaw depth (cm)", side=2, cex = 1, line = 2.2)
mtext("Probe points (n)", side=4, cex = 1, line = 2.2, adj = 0)
mtext("Year", side=1,  cex=1, line = 2.2)
dev.off()

jpeg("./Earthwatch/MacPass/figures/d2.thaw.jpeg", width = 6, height = 4, units = "in", res = 300)
par(mar = c(3.3, 3.3, 1, 3.3))
plot(d2$year, d2$mean, type='n', xlim = c(1990,2023), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rev(d2$year), d2$year), c(rev(d2$c95), d2$c5), col = alpha("yellow3",0.6), border = NA)
lines(d2$year, d2$mean, col = 'yellow3', lwd = 2)
# points(d2$year[32], d2$mean[32], col = 'yellow3', pch = 16)
abline(coef(d2.lm), lty = 2, lwd = 2, col = "yellow3")
par(new = T)
plot(d2$year, d2$n, col = 'yellow3', type = "l", xlim = c(1990,2023), ylim = c(30,120), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
lines(d2$year[c(32,33)], d2$n[c(32,33)], col = 'yellow3', pch = 17)
axis(side = 4, at = seq(10,60,10))
legend("topleft", "D2 (1477 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(d2.slope) ~ "cm yr"^-1), bty = "n")
mtext("Thaw depth (cm)", side=2, cex = 1, line = 2.2)
mtext("Probe points (n)", side=4, cex = 1, line = 2.2, adj = 0)
mtext("Year", side=1,  cex=1, line = 2.2)
dev.off()

jpeg("./Earthwatch/MacPass/figures/gf.thaw.jpeg", width = 6, height = 4, units = "in", res = 300)
par(mar = c(3.3, 3.3, 1, 3.3))
plot(goose$year, goose$mean, type='n', xlim = c(1990,2023), ylim = rev(c(0,180)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rev(goose$year), goose$year), c(rev(goose$c95), goose$c5), col = alpha("blue",0.6), border = NA)
lines(goose$year, goose$mean, col = 'blue', lwd = 2)
# points(goose$year[c(32,33)], goose$mean[c(32,33)], col = 'blue', pch = 16)
abline(coef(goose.lm), lty = 2, lwd = 2, col = "blue")
par(new = T)
plot(goose$year, goose$n, col = 'blue', type = "l", xlim = c(1990,2023), ylim = c(5,160), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(goose$year[c(32,33)], goose$n[c(32,33)], col = 'blue', pch = 17)
axis(side = 4, at = seq(10,60,10))
legend("topleft", "GF (1621 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(goose.slope) ~ "cm yr"^-1), bty = "n", inset = c(0,0.2))
mtext("Thaw depth (cm)", side=2, cex = 1, line = 2.2)
mtext("Probe points (n)", side=4, cex = 1, line = 2.2, adj = 0)
mtext("Year", side=1,  cex=1, line = 2.2)
dev.off()

jpeg("./Earthwatch/MacPass/figures/sf.thaw.jpeg", width = 6, height = 4, units = "in", res = 300)
par(mar = c(3.3, 3.3, 1, 3.3))
plot(snow$year, snow$mean, type='n', xlim = c(1990,2023), ylim = rev(c(0,180)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rev(snow$year[c(1:8)]), snow$year[c(1:8)]), c(rev(snow$c95[c(1:8)]), snow$c5[c(1:8)]), col = alpha("royalblue1",0.6), border = NA)
polygon(c(rev(snow$year[c(13:nrow(snow))]), snow$year[c(13:nrow(snow))]), c(rev(snow$c95[c(13:nrow(snow))]), snow$c5[c(13:nrow(snow))]), col = alpha("royalblue1",0.6), border = NA)
lines(snow$year, snow$mean, col = 'royalblue1', lwd = 2)
# points(snow$year[28], snow$mean[28], col = 'royalblue1', pch = 16)
abline(coef(snow.lm), lty = 2, lwd = 2, col = "royalblue1")
par(new = T)
plot(snow$year, snow$n, col = 'royalblue1', type = "l", xlim = c(1990,2023), ylim = c(5,220), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(snow$year[28], snow$n[28], col = 'royalblue1', pch = 17)
axis(side = 4, at = seq(20,100,20))
legend("topleft", "SF (1660 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
legend("bottomright", legend=bquote("Slope = " ~ .(snow.slope) ~ "cm yr"^-1), bty = "n", inset = c(0,0.1))
mtext("Thaw depth (cm)", side=2, cex = 1, line = 2.2)
mtext("Probe points (n)", side=4, cex = 1, line = 2.2, adj = 0)
mtext("Year", side=1,  cex=1, line = 2.2)
dev.off()
##

# Export as 5 x 7
jpeg("./Earthwatch/MacPass/figures/Pipeline.jpeg", width = 5, height = 7.2, units = "in", res = 300)
par(mfrow = c(3, 1))
par(cex = 0.75)
par(mar = c(1.3, 2.3, 1, 2.3), oma = c(2, 1, 0, 1)+0.2)
plot(pp.control.p$year, pp.control.p$mean, type='n', xlim = c(1990,2023), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rev(pp.control.p$year), pp.control.p$year), c(rev(pp.control.p$c95), pp.control.p$c5), col = "firebrick1", border = NA)
lines(pp.control.p$year, pp.control.p$mean, col = 'firebrick4', lwd = 2)
# points(pp.control.p$year[12], pp.control.p$mean[12], col = 'firebrick4', pch = 16)
abline(h = mean(pp.control.p$mean, na.rm = T), lty = 2, lwd = 2)
par(new = T)
plot(pp.control.p$year, pp.control.p$n, col = 'firebrick4', type = "l", xlim = c(1990,2023), ylim = c(5,350), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(pp.control.p$year[12], pp.control.p$n[12], col = 'firebrick4', pch = 17)
axis(side = 4, at = seq(20,100,20))
legend("topleft", "PP: pipeline-control (1623 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))

plot(pp.pipeline.p$year, pp.pipeline.p$mean, type='n', xlim = c(1990,2023), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rev(pp.pipeline.p$year), pp.pipeline.p$year), c(rev(pp.pipeline.p$c95), pp.pipeline.p$c5), col = "firebrick1", border = NA)
lines(pp.pipeline.p$year, pp.pipeline.p$mean, col = 'firebrick4', lwd = 2)
# points(pp.pipeline.p$year[12], pp.pipeline.p$mean[12], col = 'firebrick4', pch = 16)
abline(h = mean(pp.pipeline.p$mean, na.rm = T), lty = 2, lwd = 2)
par(new = T)
plot(pp.pipeline.p$year, pp.pipeline.p$n, col = 'firebrick4', type = "l", xlim = c(1990,2023), ylim = c(5,350), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(pp.pipeline.p$year[12], pp.pipeline.p$n[12], col = 'firebrick4', pch = 17)
axis(side = 4, at = seq(20,100,20))
legend("topleft", "PP: pipeline (1623 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
mtext("Probe points (n)", side=4, cex = 0.75, line = 2.2, adj = 0)

plot(pp.track.t$year, pp.track.t$mean, type='n', xlim = c(1990,2023), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rev(pp.track.t$year), pp.track.t$year), c(rev(pp.track.t$c95), pp.track.t$c5), col = "firebrick1", border = NA)
lines(pp.track.t$year, pp.track.t$mean, col = 'firebrick4', lwd = 2)
# points(pp.track.t$year[12], pp.track.t$mean[12], col = 'firebrick4', pch = 16)
abline(h = mean(pp.track.t$mean, na.rm = T), lty = 2, lwd = 2)
par(new = T)
plot(pp.track.t$year, pp.track.t$n, col = 'firebrick4', type = "l", xlim = c(1990,2023), ylim = c(5,850), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(pp.track.t$year[12], pp.track.t$n[12], col = 'firebrick4', pch = 17)
axis(side = 4, at = seq(0,200,50))
legend("topleft", "PP: track (1623 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))

mtext("Thaw depth (cm)", side=2, outer=TRUE, cex = 0.75, adj=0.5)
mtext("Year", side=1, outer=TRUE, cex=0.75, line = 1)
dev.off()

## Single-panel figure
col_pal <- wes_palette("Darjeeling1", 8, type = "continuous")
y_limit <- c(20,160)
jpeg("/Users/sdmamet/Desktop/Workspace/Earthwatch/Macpass/figures/thaw_depth_2022.jpg",
     height = 5, width = 6, res = 300, units = "in")
par(mar = c(4,4,2,1))
par(xpd = FALSE)
# Hare Foot
plot(hare$year, hare$mean, type='n', xlim = c(1990,2022), ylim = rev(y_limit), xaxt='n',yaxt = "n", ann=FALSE)
# polygon(c(hare$year[-c((nrow(hare)-2):nrow(hare))], 
#           rev(hare$year[-c((nrow(hare)-2):nrow(hare))])), 
#         c(hare$c95[-c((nrow(hare)-2):nrow(hare))], 
#           rev(hare$c5[-c((nrow(hare)-2):nrow(hare))])), 
#         col = alpha(col_pal[1], 0.6), border = NA)
points(hare[c(nrow(hare)-1, nrow(hare)),1], hare[c(nrow(hare)-1, nrow(hare)),4], col = alpha(col_pal[1], 0.6), pch = 15)
lines(hare$year, hare$mean, xlim = c(2002,2022), ylim = rev(y_limit), type = "l", lwd=2, col = col_pal[1])
# legend("topleft", "PPA (17 m.a.s.l.)", bty = "n", inset = c(0,0.05))
abline(coef(hare.lm),lty=2, col = col_pal[1])
# Beaver Pond
# polygon(c(beaver$year[-c((nrow(beaver)-2):nrow(beaver))], 
#           rev(beaver$year[-c((nrow(beaver)-2):nrow(beaver))])), 
#         c(beaver$c95[-c((nrow(beaver)-2):nrow(beaver))], 
#           rev(beaver$c5[-c((nrow(beaver)-2):nrow(beaver))])), col = alpha(col_pal[3],0.6), border = NA)
lines(beaver$year, beaver$mean, xlim = c(2002,2022), ylim = rev(y_limit), type = "l", lwd=2, col = col_pal[3])
points(beaver[c(nrow(beaver)-1, nrow(beaver)),1], beaver[c(nrow(beaver)-1, nrow(beaver)),4], col = alpha(col_pal[3],0.6), pch = 15)
# legend("topleft", "PPD (17 m.a.s.l.)", bty = "n", inset = c(0,0.05))
abline(coef(beaver.lm),lty=2, col = col_pal[3])
# Porsild
# polygon(c(porsild$year[-c((nrow(porsild)-2):nrow(porsild))], 
#           rev(porsild$year[-c((nrow(porsild)-2):nrow(porsild))])), 
#         c(porsild$c95[-c((nrow(porsild)-2):nrow(porsild))], 
#           rev(porsild$c5[-c((nrow(porsild)-2):nrow(porsild))])), col = alpha(col_pal[5],0.6), border = NA)
lines(porsild$year, porsild$mean, xlim = c(2002,2022), ylim = rev(y_limit), type = "l", lwd=2, col = col_pal[4])
points(porsild[c(nrow(porsild)-1, nrow(porsild)),1], porsild[c(nrow(porsild)-1, nrow(porsild)),4], col = alpha(col_pal[4],0.6), pch = 15)
abline(coef(porsild.lm),lty=2, col = col_pal[4])
# D6
# polygon(c(d6$year[-c((nrow(d6)-2):nrow(d6))], 
#           rev(d6$year[-c((nrow(d6)-2):nrow(d6))])), 
#         c(d6$c95[-c((nrow(d6)-2):nrow(d6))], 
#           rev(d6$c5[-c((nrow(d6)-2):nrow(d6))])), col = alpha(col_pal[5],0.6), border = NA)
lines(d6$year, d6$mean, xlim = c(2002,2022), ylim = rev(y_limit), type = "l", lwd=2, col = col_pal[5])
points(d6[c(nrow(d6)-1, nrow(d6)),1], d6[c(nrow(d6)-1, nrow(d6)),4], col = alpha(col_pal[5],0.6), pch = 15)
# abline(coef(d6.lm),lty=2, col = col_pal[5])
# D2
# polygon(c(d2$year[-c((nrow(d2)-2):nrow(d2))], 
#           rev(d2$year[-c((nrow(d2)-2):nrow(d2))])), 
#         c(d2$c95[-c((nrow(d2)-2):nrow(d2))], 
#           rev(d2$c5[-c((nrow(d2)-2):nrow(d2))])), col = alpha(col_pal[6],0.6), border = NA)
lines(d2$year, d2$mean, xlim = c(2002,2022), ylim = rev(y_limit), type = "l", lwd=2, col = col_pal[6])
points(d2[c(nrow(d2)-1, nrow(d2)),1], d2[c(nrow(d2)-1, nrow(d2)),4], col = alpha(col_pal[6],0.6), pch = 15)
abline(coef(d2.lm),lty=2, col = col_pal[6])
# Goose Flats
# polygon(c(goose$year[-c((nrow(goose)-2):nrow(goose))], 
#           rev(goose$year[-c((nrow(goose)-2):nrow(goose))])), 
#         c(goose$c95[-c((nrow(goose)-2):nrow(goose))], 
#           rev(goose$c5[-c((nrow(goose)-2):nrow(goose))])), col = alpha(col_pal[7],0.6), border = NA)
lines(goose$year, goose$mean, xlim = c(2002,2022), ylim = rev(y_limit), type = "l", lwd=2, col = col_pal[7])
points(goose[c(nrow(goose)-1, nrow(goose)),1], goose[c(nrow(goose)-1, nrow(goose)),4], col = alpha(col_pal[7],0.6), pch = 15)
abline(coef(goose.lm),lty=2, col = col_pal[7])
# Snow Fence
# polygon(c(snow$year[-c((nrow(snow)-2):nrow(snow))], 
#           rev(snow$year[-c((nrow(snow)-2):nrow(snow))])), 
#         c(snow$c95[-c((nrow(snow)-2):nrow(snow))], 
#           rev(snow$c5[-c((nrow(snow)-2):nrow(snow))])), col = alpha(col_pal[8],0.6), border = NA)
lines(snow$year, snow$mean, xlim = c(2002,2022), ylim = rev(y_limit), type = "l", lwd=2, col = col_pal[8])
points(snow[c(nrow(snow)-1, nrow(snow)),1], snow[c(nrow(snow)-1, nrow(snow)),4], col = alpha(col_pal[8],0.6), pch = 15)
abline(coef(snow.lm),lty=2, col = col_pal[8])

# Regression slopes
legend(
  "bottomleft",
  legend = bquote("HF slope = " ~ .(-hare.slope) ~ "cm yr" ^ -1),
  bty = "n",
  inset = c(0.0, 0.30),
  text.col = col_pal[1]
)
legend(
  "bottomleft",
  legend = bquote("BP slope = " ~ .(-beaver.slope) ~ "cm yr" ^ -1),
  bty = "n",
  inset = c(0.0, 0.24),
  text.col = col_pal[3]
)
legend(
  "bottomleft",
  legend = bquote("PF slope = " ~ .(-porsild.slope) ~ "cm yr" ^ -1),
  bty = "n",
  inset = c(0.0, 0.18),
  text.col = col_pal[4]
)
legend(
  "bottomleft",
  legend = bquote("D2 slope = " ~ .(-d2.slope) ~ "cm yr" ^ -1),
  bty = "n",
  inset = c(0.0, 0.12),
  text.col = col_pal[6]
)
legend(
  "bottomleft",
  legend = bquote("GF slope = " ~ .(-goose.slope) ~ "cm yr" ^ -1),
  bty = "n",
  inset = c(0.0, 0.06),
  text.col = col_pal[7]
)
legend(
  "bottomleft",
  legend = bquote("SF slope = " ~ .(-snow.slope) ~ "cm yr" ^ -1),
  bty = "n",
  inset = c(0.0, 0.0),
  text.col = col_pal[8]
)
# Axes
axis(1, at = seq(1990,2022,1), labels = NA)
axis(1, at = seq(1990,2022,2), labels = seq(1990,2022,2))
axis(2, at = rev(seq(20,160,20)), labels = NA)
axis(2, at = rev(seq(20,160,20)), labels = rev(seq(20,160,20)), tick = FALSE)
mtext(side = 1, "Year", line = 2.5)
mtext(side = 2, "Thaw depth (cm)", line = 2.5)

# Legend
par(xpd = NA)
legend(1992,22, c("HF","BP","PF","D6","D2","GF","SF"), col = col_pal[c(1,3:8)], horiz = T, 
       bty = "n", pch = 15, pt.cex = c(1,1), text.width = 2, y.intersp = 1, inset = c(0.05,-0.02))
dev.off()

##***************************************************************
## GTREE
mm <- read.csv(file = "./gtree/gtree_mm.csv", header = TRUE)

mm$treat <- as.factor(mm$treat)
mm$scarif <- as.factor(mm$scarif)
mm$seeded <- as.factor(mm$seeded)
mm$plot <- factor(mm$plot)

mm.salp <- subset(mm, site == "salp")
mm.sshr <- subset(mm, site == "sshr")
mm.scut <- subset(mm, site == "scut")
mm.nalp <- subset(mm, site == "nalp")
mm.salp.fir <- subset(mm.salp, species == "fir")
mm.sshr.fir <- subset(mm.sshr, species == "fir")
mm.scut.fir <- subset(mm.scut, species == "fir")
mm.nalp.fir <- subset(mm.nalp, species == "fir")
mm.salp.spruce <- subset(mm.salp, species == "spruce")
mm.sshr.spruce <- subset(mm.sshr, species == "spruce")
mm.scut.spruce <- subset(mm.scut, species == "spruce")
mm.nalp.spruce <- subset(mm.nalp, species == "spruce")

##**********##
## Mac Pass ##
##**********##

x1a <- factor(mm.nalp.fir[mm.nalp.fir$sow.year==2013,"treat"], levels = c("1", "3", "2", "4"))
x2a <- factor(mm.nalp.fir[mm.nalp.fir$sow.year==2014,"treat"], levels = c("1", "3", "2", "4"))
x3a <- factor(mm.nalp.fir[mm.nalp.fir$sow.year==2015,"treat"], levels = c("1", "3", "2", "4"))
# x4a <- factor(mm.nalp.fir[mm.nalp.fir$sow.year==2016,"treat"], levels = c("1", "3", "2", "4"))
x1b <- factor(mm.salp.fir[mm.salp.fir$sow.year==2013,"treat"], levels = c("1", "3", "2", "4"))
x2b <- factor(mm.salp.fir[mm.salp.fir$sow.year==2014,"treat"], levels = c("1", "3", "2", "4"))
x3b <- factor(mm.salp.fir[mm.salp.fir$sow.year==2015,"treat"], levels = c("1", "3", "2", "4"))
# x4b <- factor(mm.salp.fir[mm.salp.fir$sow.year==2016,"treat"], levels = c("1", "3", "2", "4"))
x1c <- factor(mm.scut.fir[mm.scut.fir$sow.year==2013,"treat"], levels = c("1", "3", "2", "4"))
x2c <- factor(mm.scut.fir[mm.scut.fir$sow.year==2014,"treat"], levels = c("1", "3", "2", "4"))
x3c <- factor(mm.scut.fir[mm.scut.fir$sow.year==2015,"treat"], levels = c("1", "3", "2", "4"))
# x4c <- factor(mm.scut.fir[mm.scut.fir$sow.year==2016,"treat"], levels = c("1", "3", "2", "4"))
x1d <- factor(mm.sshr.fir[mm.sshr.fir$sow.year==2013,"treat"], levels = c("1", "3", "2", "4"))
x2d <- factor(mm.sshr.fir[mm.sshr.fir$sow.year==2014,"treat"], levels = c("1", "3", "2", "4"))
x3d <- factor(mm.sshr.fir[mm.sshr.fir$sow.year==2015,"treat"], levels = c("1", "3", "2", "4"))
# x4d <- factor(mm.sshr.fir[mm.sshr.fir$sow.year==2016,"treat"], levels = c("1", "3", "2", "4"))

# Proportion fir germination each year - Export at 5 x 7
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,1))
##
boxplot(mm.nalp.fir[mm.nalp.fir$sow.year==2013,"germ.prop.via"] ~ x1a, at = c(1,4,7,10), xlim = c(0, 13), ylim = c(0,1), xaxt = "n", yaxt = "n", col = "yellow")
boxplot(mm.nalp.fir[mm.nalp.fir$sow.year==2014,"germ.prop.via"] ~ x2a, at = c(2,5,8,11), xaxt = "n", add = TRUE, col = "darkorange")
boxplot(mm.nalp.fir[mm.nalp.fir$sow.year==2015,"germ.prop.via"] ~ x3a, at = c(3,6,9,12), xaxt = "n", add = TRUE, col = "red")
axis(1, at = c(2, 5, 8, 11), labels = c("","","",""), tick = TRUE)
legend("topleft", "(a) North-facing alpine", bty = "n", inset = c(0.0,-0.3))
# legend("topright", c("2013","2014","2015"), col = c("yellow","darkorange","red"), pch = 15, bty = "n", horiz = T)
boxplot(mm.salp.fir[mm.salp.fir$sow.year==2013,"germ.prop.via"] ~ x1b, at = c(1,4,7,10), xlim = c(0, 13), ylim = c(0,1), xaxt = "n", yaxt = "n", col = "yellow")
boxplot(mm.salp.fir[mm.salp.fir$sow.year==2014,"germ.prop.via"] ~ x2b, at = c(2,5,8,11), xaxt = "n", add = TRUE, col = "darkorange")
boxplot(mm.salp.fir[mm.salp.fir$sow.year==2015,"germ.prop.via"] ~ x3b, at = c(3,6,9,12), xaxt = "n", add = TRUE, col = "red")
axis(1, at = c(2, 5, 8, 11), labels = c("","","",""), tick = TRUE)
legend("topleft", "(b) South-facing alpine", bty = "n", inset = c(0.0,-0.3))
boxplot(mm.scut.fir[mm.scut.fir$sow.year==2013,"germ.prop.via"] ~ x1c, at = c(1,4,7,10), xlim = c(0, 13), ylim = c(0,1), xaxt = "n", yaxt = "n", col = "yellow")
boxplot(mm.scut.fir[mm.scut.fir$sow.year==2014,"germ.prop.via"] ~ x2c, at = c(2,5,8,11), xaxt = "n", add = TRUE, col = "darkorange")
boxplot(mm.scut.fir[mm.scut.fir$sow.year==2015,"germ.prop.via"] ~ x3c, at = c(3,6,9,12), xaxt = "n", add = TRUE, col = "red")
axis(1, at = c(2, 5, 8, 11), labels = c("","","",""), tick = TRUE)
legend("topleft", "(c) South-facing shrub (cut)", bty = "n", inset = c(0.0,-0.3))
boxplot(mm.sshr.fir[mm.sshr.fir$sow.year==2013,"germ.prop.via"] ~ x1c, at = c(1,4,7,10), xlim = c(0, 13), ylim = c(0,1), xaxt = "n", yaxt = "n", col = "yellow")
boxplot(mm.sshr.fir[mm.sshr.fir$sow.year==2014,"germ.prop.via"] ~ x2c, at = c(2,5,8,11), xaxt = "n", add = TRUE, col = "darkorange")
boxplot(mm.sshr.fir[mm.sshr.fir$sow.year==2015,"germ.prop.via"] ~ x3c, at = c(3,6,9,12), xaxt = "n", add = TRUE, col = "red")
axis(1, at = c(2, 5, 8, 11), labels = c("Vegetated","Scarified","Vegetated","Scarified"), tick = TRUE)
axis(1, at = c(3.5, 9.5), labels = c("Unseeded","Seeded"), tick = FALSE, line = 2)
axis(1, at = c(2, 5, 8, 11), labels = c("","","",""), tick = TRUE)
mtext(side = 2, "Germination proportion of viable", outer = TRUE)
legend("topleft", "(d) South-facing shrub", bty = "n", inset = c(0.0,-0.3))
par(xpd = NA)
rect(0.5,-0.55,6.4,-0.45, col = 'green', border = NA)
rect(6.6,-0.55,12.5,-0.45, col = 'blue', border = NA)


# Proportion spruce germination each year - Export at 5 x 7
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,1))
##
boxplot(mm.nalp.spruce[mm.nalp.spruce$sow.year==2013,"germ.prop.via"] ~ x1a, at = c(1,4,7,10), xlim = c(0, 13), ylim = c(0,1), xaxt = "n", yaxt = "n", col = c("white","white","darkorange","yellow"))
boxplot(mm.nalp.spruce[mm.nalp.spruce$sow.year==2014,"germ.prop.via"] ~ x2a, at = c(2,5,8,11), xaxt = "n", add = TRUE, col = c("white","white","darkorange","yellow"))
boxplot(mm.nalp.spruce[mm.nalp.spruce$sow.year==2015,"germ.prop.via"] ~ x3a, at = c(3,6,9,12), xaxt = "n", add = TRUE, col = c("white","white","darkorange","yellow"))
axis(1, at = c(2, 5, 8, 11), labels = c("","","",""), tick = TRUE)
legend("topleft", "(a) North-facing alpine", bty = "n", inset = c(0.0,-0.3))
boxplot(mm.salp.spruce[mm.salp.spruce$sow.year==2013,"germ.prop.via"] ~ x1b, at = c(1,4,7,10), xlim = c(0, 13), ylim = c(0,1), xaxt = "n", yaxt = "n", col = c("white","white","darkorange","yellow"))
boxplot(mm.salp.spruce[mm.salp.spruce$sow.year==2014,"germ.prop.via"] ~ x2b, at = c(2,5,8,11), xaxt = "n", add = TRUE, col = c("white","white","darkorange","yellow"))
boxplot(mm.salp.spruce[mm.salp.spruce$sow.year==2015,"germ.prop.via"] ~ x3b, at = c(3,6,9,12), xaxt = "n", add = TRUE, col = c("white","white","darkorange","yellow"))
axis(1, at = c(2, 5, 8, 11), labels = c("","","",""), tick = TRUE)
legend("topleft", "(b) South-facing alpine", bty = "n", inset = c(0.0,-0.3))
boxplot(mm.scut.spruce[mm.scut.spruce$sow.year==2013,"germ.prop.via"] ~ x1c, at = c(1,4,7,10), xlim = c(0, 13), ylim = c(0,1), xaxt = "n", yaxt = "n", col = c("white","white","darkorange","yellow"))
boxplot(mm.scut.spruce[mm.scut.spruce$sow.year==2014,"germ.prop.via"] ~ x2c, at = c(2,5,8,11), xaxt = "n", add = TRUE, col = c("white","white","darkorange","yellow"))
boxplot(mm.scut.spruce[mm.scut.spruce$sow.year==2015,"germ.prop.via"] ~ x3c, at = c(3,6,9,12), xaxt = "n", add = TRUE, col = c("white","white","darkorange","yellow"))
axis(1, at = c(2, 5, 8, 11), labels = c("","","",""), tick = TRUE)
legend("topleft", "(c) South-facing shrub (cut)", bty = "n", inset = c(0.0,-0.3))
boxplot(mm.sshr.spruce[mm.sshr.spruce$sow.year==2013,"germ.prop.via"] ~ x1c, at = c(1,4,7,10), xlim = c(0, 13), ylim = c(0,1), xaxt = "n", yaxt = "n", col = c("white","white","darkorange","yellow"))
boxplot(mm.sshr.spruce[mm.sshr.spruce$sow.year==2014,"germ.prop.via"] ~ x2c, at = c(2,5,8,11), xaxt = "n", add = TRUE, col = c("white","white","darkorange","yellow"))
boxplot(mm.sshr.spruce[mm.sshr.spruce$sow.year==2015,"germ.prop.via"] ~ x3c, at = c(3,6,9,12), xaxt = "n", add = TRUE, col = c("white","white","darkorange","yellow"))
axis(1, at = c(2, 5, 8, 11), labels = c("Vegetated","Scarified","Vegetated","Scarified"), tick = TRUE)
axis(1, at = c(3.5, 9.5), labels = c("Unseeded","Seeded"), tick = FALSE, line = 2)
axis(1, at = c(2, 5, 8, 11), labels = c("","","",""), tick = TRUE)
mtext(side = 2, "Germination proportion of viable", outer = TRUE)
legend("topleft", "(d) South-facing shrub", bty = "n", inset = c(0.0,-0.3))
par(xpd = NA)
rect(0.5,-0.55,6.4,-0.45, col = 'green', border = NA)
rect(6.6,-0.55,12.5,-0.45, col = 'blue', border = NA)

##********************
# Survival survival each year - Export at 5 x 7

x1 <- factor(mm.nalp.fir$treat, levels = c("1", "3", "2", "4"))
x2 <- factor(mm.salp.fir$treat, levels = c("1", "3", "2", "4"))
x3 <- factor(mm.scut.fir$treat, levels = c("1", "3", "2", "4"))
x4 <- factor(mm.sshr.fir$treat, levels = c("1", "3", "2", "4"))

## Fir
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,1))
##
boxplot(mm.nalp.fir$surv.1 ~ x1, at = c(1,4,7,10), xlim = c(0, 12), ylim = c(0,1), xaxt = "n", yaxt = "n", col = "darkorange")
boxplot(mm.nalp.fir$surv.2 ~ x1, at = c(2,5,8,11), xaxt = "n", add = TRUE, col = "red")
axis(1, at = c(2, 5, 8, 11), labels = c("","","",""), tick = TRUE)
legend("topleft", "(a) North-facing alpine", bty = "n", inset = c(0.0,-0.3))
boxplot(mm.salp.fir$surv.1 ~ x2, at = c(1,4,7,10), xlim = c(0, 12), ylim = c(0,1), xaxt = "n", yaxt = "n", col = "darkorange")
boxplot(mm.salp.fir$surv.2 ~ x2, at = c(2,5,8,11), xaxt = "n", add = TRUE, col = "red")
axis(1, at = c(2, 5, 8, 11), labels = c("","","",""), tick = TRUE)
legend("topleft", "(b) South-facing alpine", bty = "n", inset = c(0.0,-0.3))
boxplot(mm.scut.fir$surv.1 ~ x3, at = c(1,4,7,10), xlim = c(0, 12), ylim = c(0,1), xaxt = "n", yaxt = "n", col = "darkorange")
boxplot(mm.scut.fir$surv.2 ~ x3, at = c(2,5,8,11), xaxt = "n", add = TRUE, col = "red")
axis(1, at = c(2, 5, 8, 11), labels = c("","","",""), tick = TRUE)
legend("topleft", "(c) South-facing shrub (cut)", bty = "n", inset = c(0.0,-0.3))
boxplot(mm.sshr.fir$surv.1 ~ x4, at = c(1,4,7,10), xlim = c(0, 12), ylim = c(0,1), xaxt = "n", yaxt = "n", col = "darkorange")
boxplot(mm.sshr.fir$surv.2 ~ x4, at = c(2,5,8,11), xaxt = "n", add = TRUE, col = "red")
axis(1, at = c(2, 5, 8, 11), labels = c("","","",""), tick = TRUE)
axis(1, at = c(2, 5, 8, 11), labels = c("Vegetated","Scarified","Vegetated","Scarified"), tick = TRUE)
axis(1, at = c(3.5, 9.5), labels = c("Unseeded","Seeded"), tick = FALSE, line = 2)
axis(1, at = c(2, 5, 8, 11), labels = c("","","",""), tick = TRUE)
mtext(side = 2, "Survival proportion", outer = TRUE)
legend("topleft", "(d) South-facing shrub", bty = "n", inset = c(0.0,-0.3))
par(xpd = NA)
rect(0.5,-0.55,6.4,-0.45, col = 'green', border = NA)
rect(6.6,-0.55,12.5,-0.45, col = 'blue', border = NA)

## Spruce
x1 <- factor(mm.nalp.spruce$treat, levels = c("1", "3", "2", "4"))
x2 <- factor(mm.salp.spruce$treat, levels = c("1", "3", "2", "4"))
x3 <- factor(mm.scut.spruce$treat, levels = c("1", "3", "2", "4"))
x4 <- factor(mm.sshr.spruce$treat, levels = c("1", "3", "2", "4"))

par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,1))
par(xpd = F)
##
boxplot(mm.nalp.spruce$surv.1 ~ x1, at = c(1,4,7,10), xlim = c(0, 12), ylim = c(0,1), xaxt = "n", yaxt = "n", col = c("white","white","darkorange","yellow"))
boxplot(mm.nalp.spruce$surv.2 ~ x1, at = c(2,5,8,11), xaxt = "n", add = TRUE, col = c("white","white","darkorange","yellow"))
axis(1, at = c(2, 5, 8, 11), labels = c("","","",""), tick = TRUE)
legend("topleft", "(a) North-facing alpine", bty = "n", inset = c(0.0,-0.3))
boxplot(mm.salp.spruce$surv.1 ~ x2, at = c(1,4,7,10), xlim = c(0, 12), ylim = c(0,1), xaxt = "n", yaxt = "n", col = c("white","white","darkorange","yellow"))
boxplot(mm.salp.spruce$surv.2 ~ x2, at = c(2,5,8,11), xaxt = "n", add = TRUE, col = c("white","white","darkorange","yellow"))
axis(1, at = c(2, 5, 8, 11), labels = c("","","",""), tick = TRUE)
legend("topleft", "(b) South-facing alpine", bty = "n", inset = c(0.0,-0.3))
boxplot(mm.scut.spruce$surv.1 ~ x3, at = c(1,4,7,10), xlim = c(0, 12), ylim = c(0,1), xaxt = "n", yaxt = "n", col = c("white","white","darkorange","yellow"))
boxplot(mm.scut.spruce$surv.2 ~ x3, at = c(2,5,8,11), xaxt = "n", add = TRUE, col = c("white","white","darkorange","yellow"))
axis(1, at = c(2, 5, 8, 11), labels = c("","","",""), tick = TRUE)
legend("topleft", "(c) South-facing shrub (cut)", bty = "n", inset = c(0.0,-0.3))
boxplot(mm.sshr.spruce$surv.1 ~ x4, at = c(1,4,7,10), xlim = c(0, 12), ylim = c(0,1), xaxt = "n", yaxt = "n", col = c("white","white","darkorange","yellow"))
boxplot(mm.sshr.spruce$surv.2 ~ x4, at = c(2,5,8,11), xaxt = "n", add = TRUE, col = c("white","white","darkorange","yellow"))
axis(1, at = c(2, 5, 8, 11), labels = c("","","",""), tick = TRUE)
axis(1, at = c(2, 5, 8, 11), labels = c("Vegetated","Scarified","Vegetated","Scarified"), tick = TRUE)
axis(1, at = c(3.5, 9.5), labels = c("Unseeded","Seeded"), tick = FALSE, line = 2)
axis(1, at = c(2, 5, 8, 11), labels = c("","","",""), tick = TRUE)
mtext(side = 2, "Survival proportion", outer = TRUE)
legend("topleft", "(d) South-facing shrub", bty = "n", inset = c(0.0,-0.3))
par(xpd = NA)
rect(0.5,-0.55,6.4,-0.45, col = 'green', border = NA)
rect(6.6,-0.55,12.5,-0.45, col = 'blue', border = NA)

