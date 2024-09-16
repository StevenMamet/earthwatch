library(tidyverse)
library(lubridate)
library(pscl)
library(lmtest)
library(scales)
library(wesanderson)
library(patchwork)
library(purrr)
library(broom)

rm(list = ls())

setwd("~/Desktop/Workspace/")

# Constants ----
year <- 2024
n_yr <- 7

# Functions ----
source("./earthwatch/R_functions/R_functions.R")

# Read in the GTREE data ----
gtree_2021 <- read_csv("./earthwatch/MacPass/data/GTREE_MM_2021.csv")

# Count seedlings from each year
gtree_21 <- gtree_2021 %>%
        group_by(Site, Plot, Cage, Species, Year, Live) %>%
        count(Year)

# write.csv(gtree_21, "~/Desktop/gtree_21.csv")

#_______________________________----
# GTREE ----
## Post cage-installation data ----
mm <- read_csv(file = "./Earthwatch/MacPass/data/gtree_mm_exclosures.csv")
names(mm) <- gsub("\\.","_", names(mm))

# Data wrangling
mm <- mm %>% 
  mutate(germ_prop_via_int = as.integer(scales::rescale(germ_prop_via, to = c(0,20)))) %>% 
  mutate(germ_prop_via_int = ifelse(is.na(germ_prop_via_int), 0, germ_prop_via_int),
         treat = as.factor(treat),
         scarif = as.factor(scarif),
         seeded = as.factor(seeded),
         plot = factor(plot))

mm_list <- gtree_fun(mm, n_yr)

x1 <- unlist(droplevels(mm_list$x1b))
x2 <- unlist(droplevels(mm_list$x1a))

#______-----
## Proportion fir germination cage/uncaged ----
jpeg(sprintf("./Earthwatch/MacPass/figures/fir_germination_%s.jpg", year), width = 5, height = 7, units = "in", res = 300)
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(0.5, 2, 1, 1), oma = c(2,1,0,0))

## Nalp - Fir out of exclosures
boxplot(mm_list$mm_nalp_fir_no$germ_prop_via ~ x1, at = c(1,3), xlim = c(0.4,4.5), ylim = c(0,1.5), 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(mm_list$mm_nalp_fir_no$germ_prop_via ~ x1, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## Nalp - Fir in exclosures
boxplot(mm_list$mm_nalp_fir_ex$germ_prop_via ~ x2, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm_list$mm_nalp_fir_ex$germ_prop_via ~ x2, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
legend("topleft", "(a) North-facing alpine", bty = "n", inset = c(0,0))
legend("topright", c("Uncaged","Caged"), pt.bg = c("red", "blue"), col = "black",
       bty = "n", pch = 22, pt.cex = c(1,1), text.width = 1, 
       y.intersp = 0.7, inset = c(0.1,0.01), horiz = F)

## Salp - Fir out of exclosures
boxplot(mm_list$mm_salp_fir_no$germ_prop_via ~ x1, at = c(1,3), xlim = c(0.4,4.5), ylim = c(0,1.5), 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(mm_list$mm_salp_fir_no$germ_prop_via ~ x1, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## Salp - Fir in exclosures
boxplot(mm_list$mm_salp_fir_ex$germ_prop_via ~ x2, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm_list$mm_salp_fir_ex$germ_prop_via ~ x2, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
legend("topleft", "(b) South-facing alpine", bty = "n", inset = c(0,0))

## Scut - Fir out of exclosures
boxplot(mm_list$mm_scut_fir_no$germ_prop_via ~ x1, at = c(1,3), xlim = c(0.4,4.5), ylim = c(0,1.5), 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(mm_list$mm_scut_fir_no$germ_prop_via ~ x1, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## Scut - Fir in exclosures
boxplot(mm_list$mm_scut_fir_ex$germ_prop_via ~ x2, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm_list$mm_scut_fir_ex$germ_prop_via ~ x2, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
legend("topleft", "(c) South-facing shrub (cut)", bty = "n", inset = c(0,0))

## Sshr - Fir out of exclosures
boxplot(mm_list$mm_sshr_fir_no$germ_prop_via ~ x1, at = c(1,3), xlim = c(0.4,4.5), ylim = c(0,1.5), 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(mm_list$mm_sshr_fir_no$germ_prop_via ~ x1, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## sshr - Fir in exclosures
boxplot(mm_list$mm_sshr_fir_ex$germ_prop_via ~ x2, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm_list$mm_sshr_fir_ex$germ_prop_via ~ x2, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
legend("topleft", "(d) South-facing shrub", bty = "n", inset = c(0,0))

axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
axis(1, at = c(1.5,3.5), labels = c("Vegetated","Scarified"), tick = TRUE)
mtext(side = 2, "Germination proportion of viable", outer = TRUE)
dev.off()

#______-----
## Proportion spruce germination cage/uncaged ----
jpeg(sprintf("./Earthwatch/MacPass/figures/spruce_germination_%s.jpg", year), width = 5, height = 7, units = "in", res = 300)
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(0.5, 2, 1, 1), oma = c(2,1,0,0))

## Nalp - Spruce out of exclosures
boxplot(mm_list$mm_nalp_spruce_no$germ_prop_via ~ x1, at = c(1,3), xlim = c(0.4,4.5), ylim = c(0,1.5), 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(mm_list$mm_nalp_spruce_no$germ_prop_via ~ x1, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## Nalp - Spruce in exclosures
boxplot(mm_list$mm_nalp_spruce_ex$germ_prop_via ~ x2, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm_list$mm_nalp_spruce_ex$germ_prop_via ~ x2, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
legend("topleft", "(a) North-facing alpine", bty = "n", inset = c(0,0))
legend("topright", c("Uncaged","Caged"), pt.bg = c("red", "blue"), col = "black",
       bty = "n", pch = 22, pt.cex = c(1,1), text.width = 1, 
       y.intersp = 0.7, inset = c(0.1,0.01), horiz = F)

## Salp - Spruce out of exclosures
boxplot(mm_list$mm_salp_spruce_no$germ_prop_via ~ x1, at = c(1,3), xlim = c(0.4,4.5), ylim = c(0,1.5), 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(mm_list$mm_salp_spruce_no$germ_prop_via ~ x1, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## Salp - Spruce in exclosures
boxplot(mm_list$mm_salp_spruce_ex$germ_prop_via ~ x2, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm_list$mm_salp_spruce_ex$germ_prop_via ~ x2, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
legend("topleft", "(b) South-facing alpine", bty = "n", inset = c(0,0))

## Scut - Spruce out of exclosures
boxplot(mm_list$mm_scut_spruce_no$germ_prop_via ~ x1, at = c(1,3), xlim = c(0.4,4.5), ylim = c(0,1.5), 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(mm_list$mm_scut_spruce_no$germ_prop_via ~ x1, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## Scut - Spruce in exclosures
boxplot(mm_list$mm_scut_spruce_ex$germ_prop_via ~ x2, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm_list$mm_scut_spruce_ex$germ_prop_via ~ x2, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
legend("topleft", "(c) South-facing shrub (cut)", bty = "n", inset = c(0,0))

## Sshr - Spruce out of exclosures
boxplot(mm_list$mm_sshr_spruce_no$germ_prop_via ~ x1, at = c(1,3), xlim = c(0.4,4.5), ylim = c(0,1.5), 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(mm_list$mm_sshr_spruce_no$germ_prop_via ~ x1, at = c(1,3), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
## sshr - Spruce in exclosures
boxplot(mm_list$mm_sshr_spruce_ex$germ_prop_via ~ x2, at = c(2,4), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm_list$mm_sshr_spruce_ex$germ_prop_via ~ x2, at = c(2,4), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
legend("topleft", "(d) South-facing shrub", bty = "n", inset = c(0,0))

axis(1, at = c(1.5,3.5), labels = NA, tick = TRUE)
axis(1, at = c(1.5,3.5), labels = c("Vegetated","Scarified"), tick = TRUE)
mtext(side = 2, "Germination proportion of viable", outer = TRUE)
dev.off()

#______-----
## Survival data collation ----
## Nalp and Salp fir
mm_list$mm_nalp_fir_no %>%
        group_by(treatment) %>%
        summarise(surv_prop_0 = mean(surv_prop_0, na.rm = T),
                  surv_prop_1 = mean(surv_prop_1, na.rm = T),
                  surv_prop_2 = mean(surv_prop_2, na.rm = T),
                  surv_prop_3 = mean(surv_prop_3, na.rm = T),
                  surv_prop_4 = mean(surv_prop_4, na.rm = T),
                  surv_prop_5 = mean(surv_prop_5, na.rm = T),
                  surv_prop_6 = mean(surv_prop_6, na.rm = T))

mm_list$mm_nalp_fir_ex %>%
        group_by(treatment) %>%
        summarise(surv_prop_0 = mean(surv_prop_0, na.rm = T),
                  surv_prop_1 = mean(surv_prop_1, na.rm = T),
                  surv_prop_2 = mean(surv_prop_2, na.rm = T),
                  surv_prop_3 = mean(surv_prop_3, na.rm = T),
                  surv_prop_4 = mean(surv_prop_4, na.rm = T),
                  surv_prop_5 = mean(surv_prop_5, na.rm = T),
                  surv_prop_6 = mean(surv_prop_6, na.rm = T))

mm_list$mm_salp_fir_no %>%
        group_by(treatment) %>%
        summarise(surv_prop_0 = mean(surv_prop_0, na.rm = T),
                  surv_prop_1 = mean(surv_prop_1, na.rm = T),
                  surv_prop_2 = mean(surv_prop_2, na.rm = T),
                  surv_prop_3 = mean(surv_prop_3, na.rm = T),
                  surv_prop_4 = mean(surv_prop_4, na.rm = T),
                  surv_prop_5 = mean(surv_prop_5, na.rm = T),
                  surv_prop_6 = mean(surv_prop_6, na.rm = T))

mm_list$mm_salp_fir_ex %>%
        group_by(treatment) %>%
        summarise(surv_prop_0 = mean(surv_prop_0, na.rm = T),
                  surv_prop_1 = mean(surv_prop_1, na.rm = T),
                  surv_prop_2 = mean(surv_prop_2, na.rm = T),
                  surv_prop_3 = mean(surv_prop_3, na.rm = T),
                  surv_prop_4 = mean(surv_prop_4, na.rm = T),
                  surv_prop_5 = mean(surv_prop_5, na.rm = T),
                  surv_prop_6 = mean(surv_prop_6, na.rm = T))

## Fir figure
jpeg(sprintf("./Earthwatch/MacPass/figures/fir_survival_%s.jpg", year), width = 5, height = 7, units = "in", res = 300)
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(0.5, 2, 1, 1), oma = c(2.8,1,0,0))

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.05,6.15), ylim=c(0,1))
points(jitter(rep(0.05,15)), mm_list$mm_nalp_fir_no$surv_prop_0[mm_list$mm_nalp_fir_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(1.05,15)), mm_list$mm_nalp_fir_no$surv_prop_1[mm_list$mm_nalp_fir_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(2.05,15)), mm_list$mm_nalp_fir_no$surv_prop_2[mm_list$mm_nalp_fir_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(3.05,15)), mm_list$mm_nalp_fir_no$surv_prop_3[mm_list$mm_nalp_fir_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(4.05,15)), mm_list$mm_nalp_fir_no$surv_prop_4[mm_list$mm_nalp_fir_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(5.05,15)), mm_list$mm_nalp_fir_no$surv_prop_5[mm_list$mm_nalp_fir_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(6.05,15)), mm_list$mm_nalp_fir_no$surv_prop_6[mm_list$mm_nalp_fir_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
lines(mm_list$mm_nalp_fir_no_seed_mean$year.vals-1, mm_list$mm_nalp_fir_no_seed_mean$surv, lwd = 2, lty = 3, col = "red")

points(jitter(rep(0.15,15)), mm_list$mm_nalp_fir_ex$surv_prop_0[mm_list$mm_nalp_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(1.15,15)), mm_list$mm_nalp_fir_ex$surv_prop_1[mm_list$mm_nalp_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(2.15,15)), mm_list$mm_nalp_fir_ex$surv_prop_2[mm_list$mm_nalp_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(3.15,15)), mm_list$mm_nalp_fir_ex$surv_prop_3[mm_list$mm_nalp_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(4.15,15)), mm_list$mm_nalp_fir_ex$surv_prop_4[mm_list$mm_nalp_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(5.15,15)), mm_list$mm_nalp_fir_ex$surv_prop_5[mm_list$mm_nalp_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(6.15,15)), mm_list$mm_nalp_fir_ex$surv_prop_6[mm_list$mm_nalp_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
lines(mm_list$mm_nalp_fir_ex_seed_mean$year.vals-1, mm_list$mm_nalp_fir_ex_seed_mean$surv, lwd = 2, lty = 3, col = "blue")

legend("right", legend = c("Caged","Uncaged"), pch = 21, bty = "n", col = "black", pt.bg = c("blue","red"))
legend("topright", legend = "a) Seeded", bty = "n")
box()
axis(side = 1, at = c(0:n_yr), labels = F)
axis(side = 2)

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.05,6.15), ylim=c(0,1))
points(jitter(rep(0.025,15)), mm_list$mm_nalp_fir_no$surv_prop_0[mm_list$mm_nalp_fir_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(1.025,15)), mm_list$mm_nalp_fir_no$surv_prop_1[mm_list$mm_nalp_fir_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(2.025,15)), mm_list$mm_nalp_fir_no$surv_prop_2[mm_list$mm_nalp_fir_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(3.025,15)), mm_list$mm_nalp_fir_no$surv_prop_3[mm_list$mm_nalp_fir_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(4.025,15)), mm_list$mm_nalp_fir_no$surv_prop_4[mm_list$mm_nalp_fir_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(5.025,15)), mm_list$mm_nalp_fir_no$surv_prop_5[mm_list$mm_nalp_fir_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(6.025,15)), mm_list$mm_nalp_fir_no$surv_prop_6[mm_list$mm_nalp_fir_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
lines(mm_list$mm_nalp_fir_no_seedscar_mean$year.vals-1, mm_list$mm_nalp_fir_no_seedscar_mean$surv, lwd = 2, lty = 1, col = "red")

points(jitter(rep(0.075,15)), mm_list$mm_nalp_fir_ex$surv_prop_0[mm_list$mm_nalp_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(1.075,15)), mm_list$mm_nalp_fir_ex$surv_prop_1[mm_list$mm_nalp_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(2.075,15)), mm_list$mm_nalp_fir_ex$surv_prop_2[mm_list$mm_nalp_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(3.075,15)), mm_list$mm_nalp_fir_ex$surv_prop_3[mm_list$mm_nalp_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(4.075,15)), mm_list$mm_nalp_fir_ex$surv_prop_4[mm_list$mm_nalp_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(5.075,15)), mm_list$mm_nalp_fir_ex$surv_prop_5[mm_list$mm_nalp_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(6.075,15)), mm_list$mm_nalp_fir_ex$surv_prop_6[mm_list$mm_nalp_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
lines(mm_list$mm_nalp_fir_ex_seedscar_mean$year.vals-1, mm_list$mm_nalp_fir_ex_seedscar_mean$surv, lwd = 2, lty = 1, col = "blue")

legend("topright", legend = "b) Seeded & scarified", bty = "n")
box()
axis(side = 1, at = c(0:n_yr), labels = F)
axis(side = 2)

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.05,6.15), ylim=c(0,1))
points(jitter(rep(0.05,15)), mm_list$mm_salp_fir_no$surv_prop_0[mm_list$mm_salp_fir_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(1.05,15)), mm_list$mm_salp_fir_no$surv_prop_1[mm_list$mm_salp_fir_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(2.05,15)), mm_list$mm_salp_fir_no$surv_prop_2[mm_list$mm_salp_fir_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(3.05,15)), mm_list$mm_salp_fir_no$surv_prop_3[mm_list$mm_salp_fir_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(4.05,15)), mm_list$mm_salp_fir_no$surv_prop_4[mm_list$mm_salp_fir_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(5.05,15)), mm_list$mm_salp_fir_no$surv_prop_5[mm_list$mm_salp_fir_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(6.05,15)), mm_list$mm_salp_fir_no$surv_prop_6[mm_list$mm_salp_fir_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
lines(mm_list$mm_salp_fir_no_seed_mean$year.vals-1, mm_list$mm_salp_fir_no_seed_mean$surv, lwd = 2, lty = 3, col = "red")

points(jitter(rep(0.95,15)), mm_list$mm_salp_fir_ex$surv_prop_0[mm_list$mm_salp_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(1.95,15)), mm_list$mm_salp_fir_ex$surv_prop_1[mm_list$mm_salp_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(2.95,15)), mm_list$mm_salp_fir_ex$surv_prop_2[mm_list$mm_salp_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(3.95,15)), mm_list$mm_salp_fir_ex$surv_prop_3[mm_list$mm_salp_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(4.95,15)), mm_list$mm_salp_fir_ex$surv_prop_4[mm_list$mm_salp_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(5.95,15)), mm_list$mm_salp_fir_ex$surv_prop_5[mm_list$mm_salp_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
lines(mm_list$mm_salp_fir_ex_seed_mean$year.vals-1, mm_list$mm_salp_fir_ex_seed_mean$surv, lwd = 2, lty = 3, col = "blue")

legend("topright", legend = "c) Seeded", bty = "n")
box()
axis(side = 1, at = c(1:n_yr), labels = F)
axis(side = 2)

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,6.15), ylim=c(0,1))
points(jitter(rep(1.025,15)), mm_list$mm_salp_fir_no$surv_prop_0[mm_list$mm_salp_fir_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(2.025,15)), mm_list$mm_salp_fir_no$surv_prop_1[mm_list$mm_salp_fir_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(3.025,15)), mm_list$mm_salp_fir_no$surv_prop_2[mm_list$mm_salp_fir_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(4.025,15)), mm_list$mm_salp_fir_no$surv_prop_3[mm_list$mm_salp_fir_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(5.025,15)), mm_list$mm_salp_fir_no$surv_prop_4[mm_list$mm_salp_fir_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(6.025,15)), mm_list$mm_salp_fir_no$surv_prop_5[mm_list$mm_salp_fir_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
lines(mm_list$mm_salp_fir_no_seed_mean$year.vals-1, mm_list$mm_salp_fir_no_seed_mean$surv, lwd = 2, lty = 1, col = "red")

points(jitter(rep(0.975,15)), mm_list$mm_salp_fir_ex$surv_prop_0[mm_list$mm_salp_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(1.975,15)), mm_list$mm_salp_fir_ex$surv_prop_1[mm_list$mm_salp_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(2.975,15)), mm_list$mm_salp_fir_ex$surv_prop_2[mm_list$mm_salp_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(3.975,15)), mm_list$mm_salp_fir_ex$surv_prop_3[mm_list$mm_salp_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(4.975,15)), mm_list$mm_salp_fir_ex$surv_prop_4[mm_list$mm_salp_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(5.975,15)), mm_list$mm_salp_fir_ex$surv_prop_5[mm_list$mm_salp_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
lines(mm_list$mm_salp_fir_ex_seedscar_mean$year.vals-1, mm_list$mm_salp_fir_ex_seedscar_mean$surv, lwd = 2, lty = 1, col = "blue")

legend("topright", legend = "d) Seeded & scarified", bty = "n")
box()
axis(side = 1, at = c(1:n_yr))
axis(side = 2)
mtext("Year post germination", side = 1, outer = T, line = 1.5)
mtext("Survival proportion", side = 2, outer = T)
dev.off()

## Spruce figure
jpeg(sprintf("./Earthwatch/MacPass/figures/spruce_survival_%s.jpg", year), width = 5, height = 7, units = "in", res = 300)
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(0.5, 2, 1, 1), oma = c(2.8,1,0,0))

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,6.15), ylim=c(0,1))
points(jitter(rep(1.05,15)), mm_list$mm_nalp_spruce_no$surv_prop_0[mm_list$mm_nalp_spruce_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(2.05,15)), mm_list$mm_nalp_spruce_no$surv_prop_1[mm_list$mm_nalp_spruce_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(3.05,15)), mm_list$mm_nalp_spruce_no$surv_prop_2[mm_list$mm_nalp_spruce_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(4.05,15)), mm_list$mm_nalp_spruce_no$surv_prop_3[mm_list$mm_nalp_spruce_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(5.05,15)), mm_list$mm_nalp_spruce_no$surv_prop_4[mm_list$mm_nalp_spruce_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(6.05,15)), mm_list$mm_nalp_spruce_no$surv_prop_5[mm_list$mm_nalp_spruce_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
lines(mm_list$mm_nalp_spruce_no_seed_mean$year.vals-1, mm_list$mm_nalp_spruce_no_seed_mean$surv, lwd = 2, lty = 3, col = "red")

points(jitter(rep(0.95,15)), mm_list$mm_nalp_spruce_ex$surv_prop_0[mm_list$mm_nalp_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(1.95,15)), mm_list$mm_nalp_spruce_ex$surv_prop_1[mm_list$mm_nalp_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(2.95,15)), mm_list$mm_nalp_spruce_ex$surv_prop_2[mm_list$mm_nalp_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(3.95,15)), mm_list$mm_nalp_spruce_ex$surv_prop_3[mm_list$mm_nalp_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(4.95,15)), mm_list$mm_nalp_spruce_ex$surv_prop_4[mm_list$mm_nalp_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(5.95,15)), mm_list$mm_nalp_spruce_ex$surv_prop_5[mm_list$mm_nalp_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
lines(mm_list$mm_nalp_spruce_ex_seed_mean$year.vals-1, mm_list$mm_nalp_spruce_ex_seed_mean$surv, lwd = 2, lty = 3, col = "blue")

legend("right", legend = c("Caged","Uncaged"), pch = 21, bty = "n", col = "black", pt.bg = c("blue","red"))
legend("topright", legend = "a) Seeded", bty = "n")
box()
axis(side = 1, at = c(1:n_yr), labels = F)
axis(side = 2)

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,6.15), ylim=c(0,1))
points(jitter(rep(1.025,15)), mm_list$mm_nalp_spruce_no$surv_prop_0[mm_list$mm_nalp_spruce_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(2.025,15)), mm_list$mm_nalp_spruce_no$surv_prop_1[mm_list$mm_nalp_spruce_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(3.025,15)), mm_list$mm_nalp_spruce_no$surv_prop_2[mm_list$mm_nalp_spruce_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(4.025,15)), mm_list$mm_nalp_spruce_no$surv_prop_3[mm_list$mm_nalp_spruce_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(5.025,15)), mm_list$mm_nalp_spruce_no$surv_prop_4[mm_list$mm_nalp_spruce_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(6.025,15)), mm_list$mm_nalp_spruce_no$surv_prop_5[mm_list$mm_nalp_spruce_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
lines(mm_list$mm_nalp_spruce_no_seedscar_mean$year.vals-1, mm_list$mm_nalp_spruce_no_seedscar_mean$surv, lwd = 2, lty = 1, col = "red")

points(jitter(rep(0.975,15)), mm_list$mm_nalp_spruce_ex$surv_prop_0[mm_list$mm_nalp_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(1.975,15)), mm_list$mm_nalp_spruce_ex$surv_prop_1[mm_list$mm_nalp_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(2.975,15)), mm_list$mm_nalp_spruce_ex$surv_prop_2[mm_list$mm_nalp_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(3.975,15)), mm_list$mm_nalp_spruce_ex$surv_prop_3[mm_list$mm_nalp_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(4.975,15)), mm_list$mm_nalp_spruce_ex$surv_prop_4[mm_list$mm_nalp_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(5.975,15)), mm_list$mm_nalp_spruce_ex$surv_prop_5[mm_list$mm_nalp_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
lines(mm_list$mm_nalp_spruce_ex_seedscar_mean$year.vals-1, mm_list$mm_nalp_spruce_ex_seedscar_mean$surv, lwd = 2, lty = 1, col = "blue")

legend("topright", legend = "b) Seeded & scarified", bty = "n")
box()
axis(side = 1, at = c(1:n_yr), labels = F)
axis(side = 2)

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,6.15), ylim=c(0,1))
points(jitter(rep(1.05,15)), mm_list$mm_salp_spruce_no$surv_prop_0[mm_list$mm_salp_spruce_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(2.05,15)), mm_list$mm_salp_spruce_no$surv_prop_1[mm_list$mm_salp_spruce_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(3.05,15)), mm_list$mm_salp_spruce_no$surv_prop_2[mm_list$mm_salp_spruce_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(4.05,15)), mm_list$mm_salp_spruce_no$surv_prop_3[mm_list$mm_salp_spruce_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(5.05,15)), mm_list$mm_salp_spruce_no$surv_prop_4[mm_list$mm_salp_spruce_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
points(jitter(rep(6.05,15)), mm_list$mm_salp_spruce_no$surv_prop_5[mm_list$mm_salp_spruce_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
lines(mm_list$mm_salp_spruce_no_seed_mean$year.vals-1, mm_list$mm_salp_spruce_no_seed_mean$surv, lwd = 2, lty = 3, col = "red")

points(jitter(rep(0.95,15)), mm_list$mm_salp_spruce_ex$surv_prop_0[mm_list$mm_salp_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(1.95,15)), mm_list$mm_salp_spruce_ex$surv_prop_1[mm_list$mm_salp_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(2.95,15)), mm_list$mm_salp_spruce_ex$surv_prop_2[mm_list$mm_salp_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(3.95,15)), mm_list$mm_salp_spruce_ex$surv_prop_3[mm_list$mm_salp_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(4.95,15)), mm_list$mm_salp_spruce_ex$surv_prop_4[mm_list$mm_salp_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(5.95,15)), mm_list$mm_salp_spruce_ex$surv_prop_5[mm_list$mm_salp_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
lines(mm_list$mm_salp_spruce_ex_seed_mean$year.vals-1, mm_list$mm_salp_spruce_ex_seed_mean$surv, lwd = 2, lty = 3, col = "blue")

legend("topright", legend = "c) Seeded", bty = "n")
box()
axis(side = 1, at = c(1:n_yr), labels = F)
axis(side = 2)

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,6.15), ylim=c(0,1))
points(jitter(rep(1.025,15)), mm_list$mm_salp_spruce_no$surv_prop_0[mm_list$mm_salp_spruce_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(2.025,15)), mm_list$mm_salp_spruce_no$surv_prop_1[mm_list$mm_salp_spruce_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(3.025,15)), mm_list$mm_salp_spruce_no$surv_prop_2[mm_list$mm_salp_spruce_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(4.025,15)), mm_list$mm_salp_spruce_no$surv_prop_3[mm_list$mm_salp_spruce_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(5.025,15)), mm_list$mm_salp_spruce_no$surv_prop_4[mm_list$mm_salp_spruce_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
points(jitter(rep(6.025,15)), mm_list$mm_salp_spruce_no$surv_prop_5[mm_list$mm_salp_spruce_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
lines(mm_list$mm_salp_spruce_no_seedscar_mean$year.vals-1, mm_list$mm_salp_spruce_no_seedscar_mean$surv, lwd = 2, lty = 1, col = "red")

points(jitter(rep(0.975,15)), mm_list$mm_salp_spruce_ex$surv_prop_0[mm_list$mm_salp_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(1.975,15)), mm_list$mm_salp_spruce_ex$surv_prop_1[mm_list$mm_salp_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(2.975,15)), mm_list$mm_salp_spruce_ex$surv_prop_2[mm_list$mm_salp_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(3.975,15)), mm_list$mm_salp_spruce_ex$surv_prop_3[mm_list$mm_salp_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(4.975,15)), mm_list$mm_salp_spruce_ex$surv_prop_4[mm_list$mm_salp_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(5.975,15)), mm_list$mm_salp_spruce_ex$surv_prop_5[mm_list$mm_salp_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
lines(mm_list$mm_salp_spruce_ex_seedscar_mean$year.vals-1, mm_list$mm_salp_spruce_ex_seedscar_mean$surv, lwd = 2, lty = 1, col = "blue")

legend("topright", legend = "d) Seeded & scarified", bty = "n")
box()
axis(side = 1, at = c(1:n_yr))
axis(side = 2)
mtext("Year post germination", side = 1, outer = T, line = 1.5)
mtext("Survival proportion", side = 2, outer = T)
dev.off()

## Nalp and Salp spruce
mm_list$mm_nalp_spruce_no %>%
  group_by(treatment) %>%
  summarise(surv_prop_0 = mean(surv_prop_0, na.rm = T),
            surv_prop_1 = mean(surv_prop_1, na.rm = T),
            surv_prop_2 = mean(surv_prop_2, na.rm = T),
            surv_prop_3 = mean(surv_prop_3, na.rm = T),
            surv_prop_4 = mean(surv_prop_4, na.rm = T),
            surv_prop_5 = mean(surv_prop_5, na.rm = T),
            surv_prop_6 = mean(surv_prop_6, na.rm = T))

mm_list$mm_nalp_spruce_ex %>%
  group_by(treatment) %>%
  summarise(surv_prop_0 = mean(surv_prop_0, na.rm = T),
            surv_prop_1 = mean(surv_prop_1, na.rm = T),
            surv_prop_2 = mean(surv_prop_2, na.rm = T),
            surv_prop_3 = mean(surv_prop_3, na.rm = T),
            surv_prop_4 = mean(surv_prop_4, na.rm = T),
            surv_prop_5 = mean(surv_prop_5, na.rm = T),
            surv_prop_6 = mean(surv_prop_6, na.rm = T))

mm_list$mm_salp_spruce_no %>%
  group_by(treatment) %>%
  summarise(surv_prop_0 = mean(surv_prop_0, na.rm = T),
            surv_prop_1 = mean(surv_prop_1, na.rm = T),
            surv_prop_2 = mean(surv_prop_2, na.rm = T),
            surv_prop_3 = mean(surv_prop_3, na.rm = T),
            surv_prop_4 = mean(surv_prop_4, na.rm = T),
            surv_prop_5 = mean(surv_prop_5, na.rm = T),
            surv_prop_6 = mean(surv_prop_6, na.rm = T))

mm_list$mm_salp_spruce_ex %>%
  group_by(treatment) %>%
  summarise(surv_prop_0 = mean(surv_prop_0, na.rm = T),
            surv_prop_1 = mean(surv_prop_1, na.rm = T),
            surv_prop_2 = mean(surv_prop_2, na.rm = T),
            surv_prop_3 = mean(surv_prop_3, na.rm = T),
            surv_prop_4 = mean(surv_prop_4, na.rm = T),
            surv_prop_5 = mean(surv_prop_5, na.rm = T),
            surv_prop_6 = mean(surv_prop_6, na.rm = T))


## SCUT and SSHR fir
mm_list$mm_scut_fir_no %>%
        group_by(treatment) %>%
        summarise(surv_prop_0 = mean(surv_prop_0, na.rm = T),
                  surv_prop_1 = mean(surv_prop_1, na.rm = T),
                  surv_prop_2 = mean(surv_prop_2, na.rm = T),
                  surv_prop_3 = mean(surv_prop_3, na.rm = T),
                  surv_prop_4 = mean(surv_prop_4, na.rm = T),
                  surv_prop_5 = mean(surv_prop_5, na.rm = T),
                  surv_prop_6 = mean(surv_prop_6, na.rm = T))

mm_list$mm_scut_fir_ex %>%
        group_by(treatment) %>%
        summarise(surv_prop_0 = mean(surv_prop_0, na.rm = T),
                  surv_prop_1 = mean(surv_prop_1, na.rm = T),
                  surv_prop_2 = mean(surv_prop_2, na.rm = T),
                  surv_prop_3 = mean(surv_prop_3, na.rm = T),
                  surv_prop_4 = mean(surv_prop_4, na.rm = T),
                  surv_prop_5 = mean(surv_prop_5, na.rm = T),
                  surv_prop_6 = mean(surv_prop_6, na.rm = T))

mm_list$mm_sshr_fir_no %>%
        group_by(treatment) %>%
        summarise(surv_prop_0 = mean(surv_prop_0, na.rm = T),
                  surv_prop_1 = mean(surv_prop_1, na.rm = T),
                  surv_prop_2 = mean(surv_prop_2, na.rm = T),
                  surv_prop_3 = mean(surv_prop_3, na.rm = T),
                  surv_prop_4 = mean(surv_prop_4, na.rm = T),
                  surv_prop_5 = mean(surv_prop_5, na.rm = T),
                  surv_prop_6 = mean(surv_prop_6, na.rm = T))

mm_list$mm_sshr_fir_ex %>%
        group_by(treatment) %>%
        summarise(surv_prop_0 = mean(surv_prop_0, na.rm = T),
                  surv_prop_1 = mean(surv_prop_1, na.rm = T),
                  surv_prop_2 = mean(surv_prop_2, na.rm = T),
                  surv_prop_3 = mean(surv_prop_3, na.rm = T),
                  surv_prop_4 = mean(surv_prop_4, na.rm = T),
                  surv_prop_5 = mean(surv_prop_5, na.rm = T),
                  surv_prop_6 = mean(surv_prop_6, na.rm = T))

jpeg(sprintf("./Earthwatch/MacPass/figures/fir_survival_shrubs_%s.jpg", year), width = 5, height = 7, units = "in", res = 300)
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(0.5, 2, 1, 1), oma = c(2.8,1,0,0))

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,7.15), ylim=c(0,1))
# points(jitter(rep(1.05,15)), mm_scut_fir_no$surv_prop_0[mm_scut_fir_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(2.05,15)), mm_scut_fir_no$surv_prop_1[mm_scut_fir_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(3.05,15)), mm_scut_fir_no$surv_prop_2[mm_scut_fir_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(4.05,15)), mm_scut_fir_no$surv_prop_3[mm_scut_fir_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(5.05,15)), mm_scut_fir_no$surv_prop_4[mm_scut_fir_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# lines(mm_scut_fir_no_seed_mean$year.vals, mm_scut_fir_no_seed_mean$surv, lwd = 2, lty = 3, col = "red")

points(jitter(rep(0.95,15)), mm_list$mm_scut_fir_ex$surv_prop_0[mm_list$mm_scut_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(1.95,15)), mm_list$mm_scut_fir_ex$surv_prop_1[mm_list$mm_scut_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(2.95,15)), mm_list$mm_scut_fir_ex$surv_prop_2[mm_list$mm_scut_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(3.95,15)), mm_list$mm_scut_fir_ex$surv_prop_3[mm_list$mm_scut_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(4.95,15)), mm_list$mm_scut_fir_ex$surv_prop_4[mm_list$mm_scut_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(5.95,15)), mm_list$mm_scut_fir_ex$surv_prop_5[mm_list$mm_scut_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(6.95,15)), mm_list$mm_scut_fir_ex$surv_prop_6[mm_list$mm_scut_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
lines(mm_list$mm_scut_fir_ex_seed_mean$year.vals, mm_list$mm_scut_fir_ex_seed_mean$surv, lwd = 2, lty = 3, col = "blue")

legend("top", legend = c("Caged","Uncaged"), pch = 21, bty = "n", col = "black", pt.bg = c("blue","red"))
legend("topright", legend = "a) Seeded", bty = "n")
box()
axis(side = 1, at = c(1:n_yr), labels = F)
axis(side = 2)

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,7.15), ylim=c(0,1))
# points(jitter(rep(1.025,15)), mm_scut_fir_no$surv_prop_0[mm_scut_fir_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# points(jitter(rep(2.025,15)), mm_scut_fir_no$surv_prop_1[mm_scut_fir_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# points(jitter(rep(3.025,15)), mm_scut_fir_no$surv_prop_2[mm_scut_fir_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# points(jitter(rep(4.025,15)), mm_scut_fir_no$surv_prop_3[mm_scut_fir_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# points(jitter(rep(5.025,15)), mm_scut_fir_no$surv_prop_4[mm_scut_fir_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# lines(mm_scut_fir_no_seedscar_mean$year.vals, mm_scut_fir_no_seedscar_mean$surv, lwd = 2, lty = 1, col = "red")

points(jitter(rep(0.975,15)), mm_list$mm_scut_fir_ex$surv_prop_0[mm_list$mm_scut_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(1.975,15)), mm_list$mm_scut_fir_ex$surv_prop_1[mm_list$mm_scut_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(2.975,15)), mm_list$mm_scut_fir_ex$surv_prop_2[mm_list$mm_scut_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(3.975,15)), mm_list$mm_scut_fir_ex$surv_prop_3[mm_list$mm_scut_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(4.975,15)), mm_list$mm_scut_fir_ex$surv_prop_4[mm_list$mm_scut_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(5.975,15)), mm_list$mm_scut_fir_ex$surv_prop_5[mm_list$mm_scut_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(6.975,15)), mm_list$mm_scut_fir_ex$surv_prop_6[mm_list$mm_scut_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
lines(mm_list$mm_scut_fir_ex_seedscar_mean$year.vals, mm_list$mm_scut_fir_ex_seedscar_mean$surv, lwd = 2, lty = 1, col = "blue")

legend("topright", legend = "b) Seeded & scarified", bty = "n")
box()
axis(side = 1, at = c(1:n_yr), labels = F)
axis(side = 2)

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,7.15), ylim=c(0,1))
# points(jitter(rep(1.05,15)), mm_sshr_fir_no$surv_prop_0[mm_sshr_fir_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(2.05,15)), mm_sshr_fir_no$surv_prop_1[mm_sshr_fir_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(3.05,15)), mm_sshr_fir_no$surv_prop_2[mm_sshr_fir_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(4.05,15)), mm_sshr_fir_no$surv_prop_3[mm_sshr_fir_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(5.05,15)), mm_sshr_fir_no$surv_prop_4[mm_sshr_fir_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# lines(mm_sshr_fir_no_seed_mean$year.vals, mm_sshr_fir_no_seed_mean$surv, lwd = 2, lty = 3, col = "red")

points(jitter(rep(0.95,15)), mm_list$mm_sshr_fir_ex$surv_prop_0[mm_list$mm_sshr_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(1.95,15)), mm_list$mm_sshr_fir_ex$surv_prop_1[mm_list$mm_sshr_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(2.95,15)), mm_list$mm_sshr_fir_ex$surv_prop_2[mm_list$mm_sshr_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(3.95,15)), mm_list$mm_sshr_fir_ex$surv_prop_3[mm_list$mm_sshr_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(4.95,15)), mm_list$mm_sshr_fir_ex$surv_prop_4[mm_list$mm_sshr_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(5.95,15)), mm_list$mm_sshr_fir_ex$surv_prop_5[mm_list$mm_sshr_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(6.95,15)), mm_list$mm_sshr_fir_ex$surv_prop_6[mm_list$mm_sshr_fir_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
lines(mm_list$mm_sshr_fir_ex_seed_mean$year.vals, mm_list$mm_sshr_fir_ex_seed_mean$surv, lwd = 2, lty = 3, col = "blue")

legend("topright", legend = "c) Seeded", bty = "n")
box()
axis(side = 1, at = c(1:n_yr), labels = F)
axis(side = 2)

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,7.15), ylim=c(0,1))
# points(jitter(rep(1.025,15)), mm_sshr_fir_no$surv_prop_0[mm_sshr_fir_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# points(jitter(rep(2.025,15)), mm_sshr_fir_no$surv_prop_1[mm_sshr_fir_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# points(jitter(rep(3.025,15)), mm_sshr_fir_no$surv_prop_2[mm_sshr_fir_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# points(jitter(rep(4.025,15)), mm_sshr_fir_no$surv_prop_3[mm_sshr_fir_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# points(jitter(rep(5.025,15)), mm_sshr_fir_no$surv_prop_4[mm_sshr_fir_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# lines(mm_sshr_fir_no_seedscar_mean$year.vals, mm_sshr_fir_no_seedscar_mean$surv, lwd = 2, lty = 1, col = "red")

points(jitter(rep(0.975,15)), mm_list$mm_sshr_fir_ex$surv_prop_0[mm_list$mm_sshr_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(1.975,15)), mm_list$mm_sshr_fir_ex$surv_prop_1[mm_list$mm_sshr_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(2.975,15)), mm_list$mm_sshr_fir_ex$surv_prop_2[mm_list$mm_sshr_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(3.975,15)), mm_list$mm_sshr_fir_ex$surv_prop_3[mm_list$mm_sshr_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(4.975,15)), mm_list$mm_sshr_fir_ex$surv_prop_4[mm_list$mm_sshr_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(5.975,15)), mm_list$mm_sshr_fir_ex$surv_prop_5[mm_list$mm_sshr_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(6.975,15)), mm_list$mm_sshr_fir_ex$surv_prop_5[mm_list$mm_sshr_fir_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
lines(mm_list$mm_sshr_fir_ex_seedscar_mean$year.vals, mm_list$mm_sshr_fir_ex_seedscar_mean$surv, lwd = 2, lty = 1, col = "blue")

legend("topright", legend = "d) Seeded & scarified", bty = "n")
box()
axis(side = 1, at = c(1:n_yr))
axis(side = 2)
mtext("Year post germination", side = 1, outer = T, line = 1.5)
mtext("Survival proportion", side = 2, outer = T)
dev.off()

## SCUT and SSHR spruce
mm_list$mm_scut_spruce_no %>%
  group_by(treatment) %>%
  summarise(surv_prop_0 = mean(surv_prop_0, na.rm = T),
            surv_prop_1 = mean(surv_prop_1, na.rm = T),
            surv_prop_2 = mean(surv_prop_2, na.rm = T),
            surv_prop_3 = mean(surv_prop_3, na.rm = T),
            surv_prop_4 = mean(surv_prop_4, na.rm = T),
            surv_prop_5 = mean(surv_prop_5, na.rm = T),
            surv_prop_6 = mean(surv_prop_5, na.rm = T))

mm_list$mm_scut_spruce_ex %>%
  group_by(treatment) %>%
  summarise(surv_prop_0 = mean(surv_prop_0, na.rm = T),
            surv_prop_1 = mean(surv_prop_1, na.rm = T),
            surv_prop_2 = mean(surv_prop_2, na.rm = T),
            surv_prop_3 = mean(surv_prop_3, na.rm = T),
            surv_prop_4 = mean(surv_prop_4, na.rm = T),
            surv_prop_5 = mean(surv_prop_5, na.rm = T),
            surv_prop_6 = mean(surv_prop_6, na.rm = T))

mm_list$mm_sshr_spruce_no %>%
  group_by(treatment) %>%
  summarise(surv_prop_0 = mean(surv_prop_0, na.rm = T),
            surv_prop_1 = mean(surv_prop_1, na.rm = T),
            surv_prop_2 = mean(surv_prop_2, na.rm = T),
            surv_prop_3 = mean(surv_prop_3, na.rm = T),
            surv_prop_4 = mean(surv_prop_4, na.rm = T),
            surv_prop_5 = mean(surv_prop_5, na.rm = T),
            surv_prop_6 = mean(surv_prop_6, na.rm = T))

mm_list$mm_sshr_spruce_ex %>%
  group_by(treatment) %>%
  summarise(surv_prop_0 = mean(surv_prop_0, na.rm = T),
            surv_prop_1 = mean(surv_prop_1, na.rm = T),
            surv_prop_2 = mean(surv_prop_2, na.rm = T),
            surv_prop_3 = mean(surv_prop_3, na.rm = T),
            surv_prop_4 = mean(surv_prop_4, na.rm = T),
            surv_prop_5 = mean(surv_prop_5, na.rm = T),
            surv_prop_6 = mean(surv_prop_6, na.rm = T))

jpeg(sprintf("./Earthwatch/MacPass/figures/spruce_survival_shrubs_%s.jpg", year), width = 5, height = 7, units = "in", res = 300)
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(0.5, 2, 1, 1), oma = c(2.8,1,0,0))

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,7.15), ylim=c(0,1))
# points(jitter(rep(1.05,15)), mm_scut_spruce_no$surv_prop_0[mm_scut_spruce_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(2.05,15)), mm_scut_spruce_no$surv_prop_1[mm_scut_spruce_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(3.05,15)), mm_scut_spruce_no$surv_prop_2[mm_scut_spruce_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(4.05,15)), mm_scut_spruce_no$surv_prop_3[mm_scut_spruce_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(5.05,15)), mm_scut_spruce_no$surv_prop_4[mm_scut_spruce_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# lines(mm_scut_spruce_no_seed_mean$year.vals, mm_scut_spruce_no_seed_mean$surv, lwd = 2, lty = 3, col = "red")

points(jitter(rep(0.95,15)), mm_list$mm_scut_spruce_ex$surv_prop_0[mm_list$mm_scut_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(1.95,15)), mm_list$mm_scut_spruce_ex$surv_prop_1[mm_list$mm_scut_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(2.95,15)), mm_list$mm_scut_spruce_ex$surv_prop_2[mm_list$mm_scut_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(3.95,15)), mm_list$mm_scut_spruce_ex$surv_prop_3[mm_list$mm_scut_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(4.95,15)), mm_list$mm_scut_spruce_ex$surv_prop_4[mm_list$mm_scut_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(5.95,15)), mm_list$mm_scut_spruce_ex$surv_prop_5[mm_list$mm_scut_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(6.95,15)), mm_list$mm_scut_spruce_ex$surv_prop_6[mm_list$mm_scut_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
lines(mm_list$mm_scut_spruce_ex_seed_mean$year.vals, mm_list$mm_scut_spruce_ex_seed_mean$surv, lwd = 2, lty = 3, col = "blue")

legend("top", legend = c("Caged","Uncaged"), pch = 21, bty = "n", col = "black", pt.bg = c("blue","red"))
legend("topright", legend = "a) Seeded", bty = "n")
box()
axis(side = 1, at = c(1:n_yr), labels = F)
axis(side = 2)

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,7.15), ylim=c(0,1))
# points(jitter(rep(1.025,15)), mm_scut_spruce_no$surv_prop_0[mm_scut_spruce_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# points(jitter(rep(2.025,15)), mm_scut_spruce_no$surv_prop_1[mm_scut_spruce_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# points(jitter(rep(3.025,15)), mm_scut_spruce_no$surv_prop_2[mm_scut_spruce_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# points(jitter(rep(4.025,15)), mm_scut_spruce_no$surv_prop_3[mm_scut_spruce_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# points(jitter(rep(5.025,15)), mm_scut_spruce_no$surv_prop_4[mm_scut_spruce_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# lines(mm_scut_spruce_no_seedscar_mean$year.vals, mm_scut_spruce_no_seedscar_mean$surv, lwd = 2, lty = 1, col = "red")

points(jitter(rep(0.975,15)), mm_list$mm_scut_spruce_ex$surv_prop_0[mm_list$mm_scut_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(1.975,15)), mm_list$mm_scut_spruce_ex$surv_prop_1[mm_list$mm_scut_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(2.975,15)), mm_list$mm_scut_spruce_ex$surv_prop_2[mm_list$mm_scut_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(3.975,15)), mm_list$mm_scut_spruce_ex$surv_prop_3[mm_list$mm_scut_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(4.975,15)), mm_list$mm_scut_spruce_ex$surv_prop_4[mm_list$mm_scut_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(5.975,15)), mm_list$mm_scut_spruce_ex$surv_prop_5[mm_list$mm_scut_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(6.975,15)), mm_list$mm_scut_spruce_ex$surv_prop_6[mm_list$mm_scut_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
lines(mm_list$mm_scut_spruce_ex_seedscar_mean$year.vals, mm_list$mm_scut_spruce_ex_seedscar_mean$surv, lwd = 2, lty = 1, col = "blue")

legend("topright", legend = "b) Seeded & scarified", bty = "n")
box()
axis(side = 1, at = c(1:n_yr), labels = F)
axis(side = 2)

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,7.15), ylim=c(0,1))
# points(jitter(rep(1.05,15)), mm_sshr_spruce_no$surv_prop_0[mm_sshr_spruce_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(2.05,15)), mm_sshr_spruce_no$surv_prop_1[mm_sshr_spruce_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(3.05,15)), mm_sshr_spruce_no$surv_prop_2[mm_sshr_spruce_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(4.05,15)), mm_sshr_spruce_no$surv_prop_3[mm_sshr_spruce_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# points(jitter(rep(5.05,15)), mm_sshr_spruce_no$surv_prop_4[mm_sshr_spruce_no$treatment == "seeded"], bg = "red", pch = 21, cex = 0.75)
# lines(mm_sshr_spruce_no_seed_mean$year.vals, mm_sshr_spruce_no_seed_mean$surv, lwd = 2, lty = 3, col = "red")

points(jitter(rep(0.95,15)), mm_list$mm_sshr_spruce_ex$surv_prop_0[mm_list$mm_sshr_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(1.95,15)), mm_list$mm_sshr_spruce_ex$surv_prop_1[mm_list$mm_sshr_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(2.95,15)), mm_list$mm_sshr_spruce_ex$surv_prop_2[mm_list$mm_sshr_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(3.95,15)), mm_list$mm_sshr_spruce_ex$surv_prop_3[mm_list$mm_sshr_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(4.95,15)), mm_list$mm_sshr_spruce_ex$surv_prop_4[mm_list$mm_sshr_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(5.95,15)), mm_list$mm_sshr_spruce_ex$surv_prop_5[mm_list$mm_sshr_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
points(jitter(rep(6.95,15)), mm_list$mm_sshr_spruce_ex$surv_prop_6[mm_list$mm_sshr_spruce_ex$treatment == "seeded"], bg = "blue", pch = 21, cex = 0.75)
lines(mm_list$mm_sshr_spruce_ex_seed_mean$year.vals, mm_list$mm_sshr_spruce_ex_seed_mean$surv, lwd = 2, lty = 3, col = "blue")

legend("topright", legend = "c) Seeded", bty = "n")
box()
axis(side = 1, at = c(1:n_yr), labels = F)
axis(side = 2)

plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0.85,7.15), ylim=c(0,1))
# points(jitter(rep(1.025,15)), mm_sshr_spruce_no$surv_prop_0[mm_sshr_spruce_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# points(jitter(rep(2.025,15)), mm_sshr_spruce_no$surv_prop_1[mm_sshr_spruce_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# points(jitter(rep(3.025,15)), mm_sshr_spruce_no$surv_prop_2[mm_sshr_spruce_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# points(jitter(rep(4.025,15)), mm_sshr_spruce_no$surv_prop_3[mm_sshr_spruce_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# points(jitter(rep(5.025,15)), mm_sshr_spruce_no$surv_prop_4[mm_sshr_spruce_no$treatment == "seeded.scarified"], bg = "red", pch = 24, cex = 0.75)
# lines(mm_sshr_spruce_no_seedscar_mean$year.vals, mm_sshr_spruce_no_seedscar_mean$surv, lwd = 2, lty = 1, col = "red")

points(jitter(rep(0.975,15)), mm_list$mm_sshr_spruce_ex$surv_prop_0[mm_list$mm_sshr_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(1.975,15)), mm_list$mm_sshr_spruce_ex$surv_prop_1[mm_list$mm_sshr_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(2.975,15)), mm_list$mm_sshr_spruce_ex$surv_prop_2[mm_list$mm_sshr_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(3.975,15)), mm_list$mm_sshr_spruce_ex$surv_prop_3[mm_list$mm_sshr_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(4.975,15)), mm_list$mm_sshr_spruce_ex$surv_prop_4[mm_list$mm_sshr_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(5.975,15)), mm_list$mm_sshr_spruce_ex$surv_prop_5[mm_list$mm_sshr_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
points(jitter(rep(6.975,15)), mm_list$mm_sshr_spruce_ex$surv_prop_6[mm_list$mm_sshr_spruce_ex$treatment == "seeded.scarified"], bg = "blue", pch = 24, cex = 0.75)
lines(mm_list$mm_sshr_spruce_ex_seedscar_mean$year.vals, mm_list$mm_sshr_spruce_ex_seedscar_mean$surv, lwd = 2, lty = 1, col = "blue")

legend("topright", legend = "d) Seeded & scarified", bty = "n")
box()
axis(side = 1, at = c(1:n_yr))
axis(side = 2)
mtext("Year post germination", side = 1, outer = T, line = 1.5)
mtext("Survival proportion", side = 2, outer = T)
dev.off()

#_______________________________----
# Microclimate ----
mm <- read.csv("./Earthwatch/MacPass/data/microclimate_19900721_20240824_filled.csv", header = T)[-1]
names(mm) <- gsub("\\.","_", names(mm))

# Format date, add in cool/warm seasons, season_year
mm <- mm %>%
  mutate(
    Date = ymd(Date),
    season_year = ifelse(
      month(Date) == 10 | month(Date) == 11 | month(Date) == 12,
      year(Date) + 1,
      year(Date)
    ),
    season = case_when(
      month(Date) %in% c(10, 11, 12, 1, 2, 3, 4, 5) ~ "cool",
      month(Date) %in% c(6, 7, 8, 9) ~ "warm",
      T ~ NA_character_
    )
  )

mm_temps_season <- mm %>% 
  group_by(season_year, season) %>% 
  summarise(across(ends_with(c("_150","_0","_neg150")), ~ mean(.x, na.rm = TRUE))) %>% 
  ungroup()

mm_temps_w <- subset(mm_temps_season, season == "warm")
mm_temps_c <- subset(mm_temps_season, season == "cool")
matplot(mm_temps_w[,-c(1,2)], type = "l")
matplot(mm_temps_c[,-c(1,2)], type = "l")

n_pts_w <- c(1,35)
n_pts_w_d6 <- c(8:34)
n_pts_c <- 34
n_pts_c_d6 <- c(6:34)

## Air temperature regressions
air_reg_df <- bind_rows(data.frame(coef(summary(bp_150.w_lm <- lm(bp_150 ~ season_year, data = mm_temps_w[-n_pts_w,])))),
                        data.frame(coef(summary(hf_150.w_lm <- lm(hf_150 ~ season_year, data = mm_temps_w[-n_pts_w,])))),
                        data.frame(coef(summary(d6_150.w_lm <- lm(d6_150 ~ season_year, data = mm_temps_w[n_pts_w_d6,])))),
                        data.frame(coef(summary(d2_150.w_lm <- lm(d2_150 ~ season_year, data = mm_temps_w[-n_pts_w,])))),
                        data.frame(coef(summary(gf_150.w_lm <- lm(gf_150 ~ season_year, data = mm_temps_w[-n_pts_w,])))),
                        data.frame(coef(summary(bp_150.c_lm <- lm(bp_150 ~ season_year, data = mm_temps_c[-n_pts_c,])))),
                        data.frame(coef(summary(hf_150.c_lm <- lm(hf_150 ~ season_year, data = mm_temps_c[-n_pts_c,])))),
                        data.frame(coef(summary(d6_150.c_lm <- lm(d6_150 ~ season_year, data = mm_temps_c[n_pts_c_d6,])))),
                        data.frame(coef(summary(d2_150.c_lm <- lm(d2_150 ~ season_year, data = mm_temps_c[-n_pts_c,])))),
                        data.frame(coef(summary(gf_150.c_lm <- lm(gf_150 ~ season_year, data = mm_temps_c[-n_pts_c,]))))) %>% 
  mutate(
    season = rep(c("warm","cool"), each = 10),
    site = rep(rep(c("bp","hf","d6","d2","gf"), each = 2),2),
    coefficient = rep(c("intercept","slope"), 10)) %>% 
  rename(p_val = Pr...t..) %>% 
  relocate(any_of(c("site","season","coefficient")), .before = Estimate)
rownames(air_reg_df) <- NULL

air_reg_df <- air_reg_df %>% 
  pivot_wider(names_from = coefficient, values_from = c(Estimate, Std..Error, t.value, p_val)) %>% 
  mutate(keep = ifelse(p_val_slope < 0.05, "keep", ""))

## Ground surface regressions
grd_reg_df <- bind_rows(data.frame(coef(summary(bp_0.w_lm <- lm(bp_0 ~ season_year, data = mm_temps_w[-n_pts_w,])))),
                        data.frame(coef(summary(hf_0.w_lm <- lm(hf_0 ~ season_year, data = mm_temps_w[-n_pts_w,])))),
                        data.frame(coef(summary(d6_0.w_lm <- lm(d6_0 ~ season_year, data = mm_temps_w[n_pts_w_d6,])))),
                        data.frame(coef(summary(d2_0.w_lm <- lm(d2_0 ~ season_year, data = mm_temps_w[-n_pts_w,])))),
                        data.frame(coef(summary(gf_0.w_lm <- lm(gf_0 ~ season_year, data = mm_temps_w[-n_pts_w,])))),
                        data.frame(coef(summary(bp_0.c_lm <- lm(bp_0 ~ season_year, data = mm_temps_c[-n_pts_c,])))),
                        data.frame(coef(summary(hf_0.c_lm <- lm(hf_0 ~ season_year, data = mm_temps_c[-n_pts_c,])))),
                        data.frame(coef(summary(d6_0.c_lm <- lm(d6_0 ~ season_year, data = mm_temps_c[n_pts_c_d6,])))),
                        data.frame(coef(summary(d2_0.c_lm <- lm(d2_0 ~ season_year, data = mm_temps_c[-n_pts_c,])))),
                        data.frame(coef(summary(gf_0.c_lm <- lm(gf_0 ~ season_year, data = mm_temps_c[-n_pts_c,]))))) %>% 
  mutate(
    season = rep(c("warm","cool"), each = 10),
    site = rep(rep(c("bp","hf","d6","d2","gf"), each = 2),2),
    coefficient = rep(c("intercept","slope"), 10)) %>% 
  rename(p_val = Pr...t..) %>% 
  relocate(any_of(c("site","season","coefficient")), .before = Estimate)
rownames(grd_reg_df) <- NULL

grd_reg_df <- grd_reg_df %>% 
  pivot_wider(names_from = coefficient, values_from = c(Estimate, Std..Error, t.value, p_val)) %>% 
  mutate(keep = ifelse(p_val_slope < 0.05, "keep", ""))

## Permafrost regressions
per_reg_df <- bind_rows(data.frame(coef(summary(bp_neg150.w_lm <- lm(bp_neg150 ~ season_year, data = mm_temps_w[-n_pts_w,])))),
                        data.frame(coef(summary(hf_neg150.w_lm <- lm(hf_neg150 ~ season_year, data = mm_temps_w[-n_pts_w,])))),
                        data.frame(coef(summary(d6_neg150.w_lm <- lm(d6_neg150 ~ season_year, data = mm_temps_w[n_pts_w_d6,])))),
                        data.frame(coef(summary(d2_neg150.w_lm <- lm(d2_neg150 ~ season_year, data = mm_temps_w[-n_pts_w,])))),
                        data.frame(coef(summary(gf_neg150.w_lm <- lm(gf_neg150 ~ season_year, data = mm_temps_w[-n_pts_w,])))),
                        data.frame(coef(summary(bp_neg150.c_lm <- lm(bp_neg150 ~ season_year, data = mm_temps_c[-n_pts_c,])))),
                        data.frame(coef(summary(hf_neg150.c_lm <- lm(hf_neg150 ~ season_year, data = mm_temps_c[-n_pts_c,])))),
                        data.frame(coef(summary(d6_neg150.c_lm <- lm(d6_neg150 ~ season_year, data = mm_temps_c[n_pts_c_d6,])))),
                        data.frame(coef(summary(d2_neg150.c_lm <- lm(d2_neg150 ~ season_year, data = mm_temps_c[-n_pts_c,])))),
                        data.frame(coef(summary(gf_neg150.c_lm <- lm(gf_neg150 ~ season_year, data = mm_temps_c[-n_pts_c,]))))) %>% 
  mutate(
    season = rep(c("warm","cool"), each = 10),
    site = rep(rep(c("bp","hf","d6","d2","gf"), each = 2),2),
    coefficient = rep(c("intercept","slope"), 10)) %>% 
  rename(p_val = Pr...t..) %>% 
  relocate(any_of(c("site","season","coefficient")), .before = Estimate)
rownames(per_reg_df) <- NULL

per_reg_df <- per_reg_df %>% 
  pivot_wider(names_from = coefficient, values_from = c(Estimate, Std..Error, t.value, p_val)) %>% 
  mutate(keep = ifelse(p_val_slope < 0.05, "keep", ""))


df_plot <- mm_temps_season %>%
  select(-mean_150) %>%
  pivot_longer(cols = bp_150:gf_neg150) %>% 
  arrange(season, name, season_year) %>% 
  filter(!(name == "d6_0" & season_year < 1996)) %>% 
  filter(!(name == "d6_150" & season_year < 1996)) %>% 
  filter(!(name == "d6_neg150" & season_year < 1996)) 

col_pal <- c("darkred","red","orange","yellow3","blue")

fig4a <- df_plot %>% 
  filter(season_year != year, season == "warm") %>% 
  filter(grepl("_150", name)) %>% 
  mutate(name = factor(name, levels = c("bp_150","hf_150","d6_150","d2_150","gf_150"))) %>% 
  ggplot(aes(x = season_year, y = value, color = name)) + geom_line() + 
  geom_smooth(data = df_plot %>% filter(season_year != year, season == "warm") %>%
                filter(grepl("_150", name)) %>% subset(name %in% c("bp_150","hf_150","gf_150")), 
              aes(x = season_year, y = value, color = name),
              method="lm", se=FALSE, linetype = 2, linewidth = 0.6) +
  scale_colour_manual(values = col_pal) +
  guides(color = "none") +
  theme_bw() +
  labs(x = "", y = expression('Air ('*degree*C*')'))

fig4b <- df_plot %>% 
  filter(season_year != year, season == "warm") %>% 
  filter(grepl("_0", name)) %>% 
  mutate(name = factor(name, levels = c("bp_0","hf_0","d6_0","d2_0","gf_0"))) %>% 
  ggplot(aes(x = season_year, y = value, color = name)) + geom_line() + 
  geom_smooth(data = df_plot %>% filter(season_year != year, season == "warm") %>%
                filter(grepl("_0", name)) %>% subset(name %in% c("gf_0")), 
              aes(x = season_year, y = value, color = name),
              method="lm", se=FALSE, linetype = 2, linewidth = 0.6) +
  scale_colour_manual(values = col_pal) +
  guides(color = "none") +
  theme_bw() +
  labs(x = "", y = expression('Ground surface ('*degree*C*')'))

fig4c <- df_plot %>% 
  filter(season_year != year, season == "warm") %>% 
  filter(grepl("_neg150", name)) %>% 
  mutate(name = factor(name, levels = c("bp_neg150","hf_neg150","d6_neg150","d2_neg150","gf_neg150"))) %>% 
  ggplot(aes(x = season_year, y = value, color = name)) + geom_line() + 
  geom_smooth(method="lm", se=FALSE, linetype = 2, linewidth = 0.6) +
  scale_colour_manual(values = col_pal) +
  guides(color = "none") +
  theme_bw() +
  labs(x = "", y = expression('Subsurface ('*degree*C*')'))

fig4d <- df_plot %>% 
  filter(season_year != year, season == "cool") %>% 
  filter(grepl("_150", name)) %>% 
  mutate(name = factor(name, levels = c("bp_150","hf_150","d6_150","d2_150","gf_150"))) %>% 
  ggplot(aes(x = season_year, y = value, color = name)) + geom_line() + 
  scale_colour_manual(values = col_pal) +
  guides(color = "none") +
  theme_bw() +
  labs(x = "", y = "")

fig4e <- df_plot %>% 
  filter(season_year != year, season == "cool") %>% 
  filter(grepl("_0", name)) %>% 
  mutate(name = factor(name, levels = c("bp_0","hf_0","d6_0","d2_0","gf_0"))) %>% 
  ggplot(aes(x = season_year, y = value, color = name)) + geom_line() + 
  geom_smooth(data = df_plot %>% filter(season_year != year, season == "cool") %>%
                filter(grepl("_0", name)) %>% subset(name %in% c("bp_0","hf_0","gf_0")), 
              aes(x = season_year, y = value, color = name),
              method="lm", se=FALSE, linetype = 2, linewidth = 0.6) +
  scale_colour_manual(values = col_pal) +
  guides(color = "none") +
  theme_bw() +
  labs(x = "", y = "")

fig4f <- df_plot %>% 
  filter(season_year != year, season == "cool") %>% 
  filter(grepl("_neg150", name)) %>% 
  mutate(name = factor(name, levels = c("bp_neg150","hf_neg150","d6_neg150","d2_neg150","gf_neg150"))) %>% 
  ggplot(aes(x = season_year, y = value, color = name)) + geom_line() + 
  geom_smooth(data = df_plot %>% filter(season_year != year, season == "cool") %>%
                filter(grepl("_neg150", name)) %>% subset(name %in% c("bp_neg150","gf_neg150")), aes(x = season_year, y = value, color = name),
                method="lm", se=FALSE, linetype = 2, linewidth = 0.6) +
  scale_colour_manual(values = col_pal, labels = c("BP","HF","D6","D2","GF")) +
  theme_bw() +
  labs(x = "", y = "", color = "Site")

jpeg(sprintf("./earthwatch/MacPass/Reports/EW%s_Figure04.jpg", year), width = 6, height = 7, units = "in", res = 300)
combined <-
  ((fig4a | fig4d) / (fig4b | fig4e) / (fig4c | fig4f)) + plot_layout(guides = "collect") & xlab(NULL) &
  theme(
    plot.margin = margin(-15,0,1,0), # top, right, bottom, left
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
  )
wrap_elements(panel = combined) +
  labs(tag = "Year") +
  theme(
    plot.tag = element_text(size = rel(1)),
    plot.tag.position = "bottom"
  )
dev.off()

## Export at 6 x 3.5

# Warm
jpeg(sprintf("./Earthwatch/MacPass/figures/warmT_%s.jpg", year), width = 5, height = 7, units = "in", res = 300)
par(xpd = F)
par(mar = c(2,2,1,1), oma = c(2,2,0,0))
par(mfrow = c(3,1))
matplot(mm_temps_w$season_year[-n_pts_w], mm_temps_w[-n_pts_w,c(3:5,7)], 
        type = "l", col = c("darkred","red","yellow","blue"), 
        lty = 1, ylab = "", xlab = "", ylim = c(3,11))
lines(mm_temps_w$season_year[n_pts_w_d6], mm_temps_w$d6_150[n_pts_w_d6], col = "orange")
mtext("Air (°C)", side = 2, line = 2.5, cex = 0.7)
abline(coef(hf_150.w_lm),lty=2, col = "darkred")
abline(coef(bp_150.w_lm),lty=2, col = "red")
# abline(coef(d6_150.w_lm),lty=2, col = "orange")
# abline(coef(d2_150.w_lm),lty=2, col = "yellow")
abline(coef(gf_150.w_lm),lty=2, col = "blue")

matplot(mm_temps_w$season_year[-n_pts_w], mm_temps_w[-n_pts_w,c(9:11,13)], 
        type = "l", col = c("darkred","red","yellow","blue"), 
        lty = 1, ylab = "", xlab = "", ylim = c(2,11))
lines(mm_temps_w$season_year[-c(1:7,33)], mm_temps_w$d6_0[-c(1:7,33)], col = "orange")
mtext("Ground surface (°C)", side = 2, line = 2.5, cex = 0.7)
# abline(coef(hf_0.w_lm),lty=2, col = "darkred")
# abline(coef(bp_0.w_lm),lty=2, col = "red")
# abline(coef(d6_0.w_lm),lty=2, col = "orange")
abline(coef(d2_0.w_lm),lty=2, col = "yellow")
abline(coef(gf_0.w_lm),lty=2, col = "blue")

matplot(mm_temps_w$season_year[-n_pts_w], mm_temps_w[-n_pts_w,c(14:16,18)], type = "l", col = c("darkred","red","yellow","blue"), 
        lty = 1, ylab = "", xlab = "", ylim = c(-2,0.5))
lines(mm_temps_w$season_year[-c(1:7,33)], mm_temps_w$d6_neg150[-c(1:7,33)], col = "orange")
mtext("Subsurface (°C)", side = 2, line = 2.5, cex = 0.7)
mtext("Year", side = 1, line = 2.5, cex = 0.7)
abline(coef(hf_neg150.w_lm),lty=2, col = "darkred")
abline(coef(bp_neg150.w_lm),lty=2, col = "red")
# abline(coef(d6_neg150.w_lm),lty=2, col = "orange")
# abline(coef(d2_neg150.w_lm),lty=2, col = "yellow")
abline(coef(gf_neg150.w_lm),lty=2, col = "blue")

par(xpd = NA)
legend(1994,5.2, c("HF", "BP", "D6", "D2","GF"), 
       col = c("darkred","red","orange","yellow","blue"), 
       horiz = T, lty = 1, cex = 1, bty = "n", y.intersp = 1, text.width = 2)
dev.off()

# mm_temps_c$hf_150[mm_temps_c$season_year == 1992] <- mm_temps_c$hf_150[mm_temps_c$season_year == 1992]-2

# Cool
jpeg(sprintf("./Earthwatch/MacPass/figures/coolT_%s.jpg", year), width = 5, height = 7, units = "in", res = 300)
par(xpd = F)
par(mar = c(2,2,1,1), oma = c(2,2,0,0))
par(mfrow = c(3,1))
matplot(mm_temps_c$season_year[-n_pts_c], mm_temps_c[-n_pts_c,c(3:5,7)], 
        type = "l", col = c("darkred","red","yellow","blue"), 
        lty = 1, ylab = "", xlab = "", ylim = c(-18,-8))
lines(mm_temps_c$season_year[-c(1:6,n_pts_c)], mm_temps_c$d6_150[-c(1:6,n_pts_c)], col = "orange")
mtext("Air (°C)", side = 2, line = 2.5, cex = 0.7)
# abline(coef(hf_150.c_lm),lty=2, col = "darkred")
# abline(coef(bp_150.c_lm),lty=2, col = "red")
# abline(coef(d6_150.c_lm),lty=2, col = "orange")
# abline(coef(d2_150.c_lm),lty=2, col = "yellow")
# abline(coef(gf_150.c_lm),lty=2, col = "blue")

matplot(mm_temps_c$season_year[-n_pts_c], mm_temps_c[-n_pts_c,c(9:11,13)], type = "l", col = c("darkred","red","yellow","blue"), 
        lty = 1, ylab = "", xlab = "", ylim = c(-12,0))
lines(mm_temps_c$season_year[-c(1:6,n_pts_c)], mm_temps_c$d6_0[-c(1:6,n_pts_c)], col = "orange")
mtext("Ground surface (°C)", side = 2, line = 2.5, cex = 0.7)
# abline(coef(hf_0.c_lm),lty=2, col = "darkred")
abline(coef(bp_0.c_lm),lty=2, col = "red")
# abline(coef(d6_0.c_lm),lty=2, col = "orange")
# abline(coef(d2_0.c_lm),lty=2, col = "yellow")
abline(coef(gf_0.c_lm),lty=2, col = "blue")

matplot(mm_temps_c$season_year[-n_pts_c], mm_temps_c[-n_pts_c,c(14:16,18)], type = "l", col = c("darkred","red","yellow","blue"), 
        lty = 1, ylab = "", xlab = "", ylim = c(-5,0))
lines(mm_temps_c$season_year[-c(1:6,n_pts_c)], mm_temps_c$d6_neg150[-c(1:6,n_pts_c)], col = "orange")
mtext("Subsurface (°C)", side = 2, line = 2.5, cex = 0.7)
mtext("Year", side = 1, line = 2.5, cex = 0.7)
# abline(coef(hf_neg150.c_lm),lty=2, col = "darkred")
abline(coef(bp_neg150.c_lm),lty=2, col = "red")
# abline(coef(d6_neg150.c_lm),lty=2, col = "orange")
abline(coef(d2_neg150.c_lm),lty=2, col = "yellow")
abline(coef(gf_neg150.c_lm),lty=2, col = "blue")

par(xpd = NA)
legend(1994,8.8, c("HF", "BP", "D6", "D2","GF"), 
       col = c("darkred","red","orange","yellow","blue"), 
       horiz = T, lty = 1, cex = 1, bty = "n", y.intersp = 1, text.width = 2)
dev.off()

#_______________________________----
# Thaw depths ----
thaw_mm <- read.csv("./Earthwatch/MacPass/data/thaw.mm.csv", header = TRUE)
thaw_mm$site <- gsub("\\.","_", thaw_mm$site)
thaw_mm$ci <- thaw_mm$se*qt(0.975, thaw_mm$n-1)
thaw_mm$c5 <- thaw_mm$mean - thaw_mm$ci
thaw_mm$c95 <- thaw_mm$mean + thaw_mm$ci

d2 <- subset(thaw_mm, site == "D2")
d6 <- subset(thaw_mm, site == "D6")
beaver <- subset(thaw_mm, site == "Beaver")
hare <- subset(thaw_mm, site == "Hare")
goose <- subset(thaw_mm, site == "Goose")
snow <- subset(thaw_mm, site == "Snow")
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
porsild <- subset(thaw_mm, site == "Porsild")
porsild_1 <- subset(thaw_mm, site == "Porsild_1")
porsild_2 <- subset(thaw_mm, site == "Porsild_2")
pipeline <- subset(thaw_mm, site == "Pipeline")
pp_control_p <- subset(thaw_mm, site == "PP_Control_P")
pp_pipeline_p <- subset(thaw_mm, site == "PP_Pipeline_P")
pp_track_t <- subset(thaw_mm, site == "PP_Track_T")
pp_control_t <- subset(thaw_mm, site == "PP_Control_T")

snow_lm <- lm(snow$mean~snow$year)
pipeline_lm <- lm(pipeline$mean~pipeline$year)
goose_lm <- lm(goose$mean~goose$year)
d2_lm <- lm(d2$mean~d2$year)
d6_lm <- lm(d6$mean~d6$year)
porsild_lm <- lm(porsild$mean~porsild$year)
porsild_1_lm <- lm(porsild_1$mean~porsild_1$year)
porsild_2_lm <- lm(porsild_2$mean~porsild_2$year)
beaver_lm <- lm(beaver$mean~beaver$year)
hare_lm <- lm(hare$mean~hare$year)

summary(snow_lm) # Sig
summary(pipeline_lm) # Sig
summary(goose_lm) # Sig
summary(d2_lm) # Sig
summary(d6_lm) # Sig
summary(porsild_lm) # Sig
summary(porsild_1_lm) # Sig
summary(porsild_2_lm) # Sig
summary(beaver_lm) # Sig
summary(hare_lm) # Sig

snow_slope <- round(coef(snow_lm)[[2]], 3)
pipeline_slope <- round(coef(pipeline_lm)[[2]], 3)
goose_slope <- round(coef(goose_lm)[[2]], 3)
d2_slope <- round(coef(d2_lm)[[2]], 3)
d6_slope <- round(coef(d6_lm)[[2]], 3)
porsild_slope <- round(coef(porsild_lm)[[2]], 3)
porsild_1_slope <- round(coef(porsild_1_lm)[[2]], 3)
porsild_2_slope <- round(coef(porsild_2_lm)[[2]], 3)
beaver_slope <- round(coef(beaver_lm)[[2]], 3)
hare_slope <- round(coef(hare_lm)[[2]], 3)

# Create a function to fit a linear model and extract the required information
fit_lm <- function(data) {
  model <- lm(mean ~ year, data = data)
  summary <- summary(model)
  
  tibble(
    intercept = coef(summary)[1, "Estimate"],
    slope = coef(summary)[2, "Estimate"],
    r_squared = summary$r.squared,
    n = nrow(data)
  )
}

# Apply this function to each site
regression_results <- thaw_mm %>%
  group_by(site) %>%
  nest() %>%
  mutate(model_info = map(data, fit_lm)) %>%
  unnest(model_info) %>%
  select(-data)

# Export at 7 x 7
# Export at 5 x 3.5

# legend(1994,13.8, c("HF", "BP", "D6", "D2","GF"), 
#        col = c("darkred","red","orange","yellow","blue"), 
#        horiz = T, lty = 1, cex = 1, bty = "n", y.intersp = 1, text.width = 2)
len_yr <- 35

jpeg(sprintf("./Earthwatch/MacPass/figures/hare_thaw_%s.jpeg", year), width = 6, height = 4, units = "in", res = 300)
par(mar = c(3.3, 3.3, 1, 3.3))
plot(hare$year, hare$mean, type='n', xlim = c(1990,year), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rev(hare$year[-c(32:len_yr)]), hare$year[-c(32:len_yr)]), c(rev(hare$c95[-c(32:len_yr)]), hare$c5[-c(32:len_yr)]), col = scales::alpha("darkred",0.6), border = NA)
polygon(c(rev(hare$year[c(32:len_yr)]), hare$year[c(32:len_yr)]), c(rev(hare$c95[c(32:len_yr)]), hare$c5[c(32:len_yr)]), col = scales::alpha("darkred",0.6), border = NA)
lines(hare$year, hare$mean, col = 'darkred', lwd = 2)
# points(hare$year[n_pts_c], hare$mean[n_pts_c], col = 'darkred', pch = 16)
abline(coef(hare_lm), lty = 2, lwd = 2, col = "darkred")
par(new = T)
plot(hare$year, hare$n, col = 'darkred', type = "l", xlim = c(1990,year), ylim = c(10,100), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(hare$year[n_pts_c], hare$n[n_pts_c], col = 'darkred', pch = 17)
axis(side = 4, at = seq(0,40,10))
legend("topleft", "HF (1260 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(hare_slope) ~ "cm yr"^-1), bty = "n")
mtext("Thaw depth (cm)", side=2, cex = 1, line = 2.2)
mtext("Probe points (n)", side=4, cex = 1, line = 2.2, adj = 0)
mtext("Year", side=1,  cex=1, line = 2.2)
dev.off()

jpeg(sprintf("./Earthwatch/MacPass/figures/beaver_thaw_%s.jpeg", year), width = 6, height = 4, units = "in", res = 300)
par(mar = c(3.3, 3.3, 1, 3.3))
plot(beaver$year, beaver$mean, type='n', xlim = c(1990,year), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rev(beaver$year[-c(32:len_yr)]), beaver$year[-c(32:len_yr)]), c(rev(beaver$c95[-c(32:len_yr)]), beaver$c5[-c(32:len_yr)]), col = scales::alpha("red",0.6), border = NA)
polygon(c(rev(beaver$year[c(32:len_yr)]), beaver$year[c(32:len_yr)]), c(rev(beaver$c95[c(32:len_yr)]), beaver$c5[c(32:len_yr)]), col = scales::alpha("red",0.6), border = NA)
lines(beaver$year, beaver$mean, col = 'red', lwd = 2)
# points(beaver$year[n_pts_c], beaver$mean[n_pts_c], col = 'red', pch = 16)
abline(coef(beaver_lm), lty = 2, lwd = 2, col = "red")
par(new = T)
plot(beaver$year, beaver$n, col = 'red', type = "l", xlim = c(1990,year), ylim = c(30,150), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(beaver$year[n_pts_c], beaver$n[n_pts_c], col = 'red', pch = 17)
axis(side = 4, at = seq(0,80,20))
legend("topleft", "BP (1272 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(beaver_slope) ~ "cm yr"^-1), bty = "n", inset = c(0,0.1))
mtext("Thaw depth (cm)", side=2, cex = 1, line = 2.2)
mtext("Probe points (n)", side=4, cex = 1, line = 2.2, adj = 0)
mtext("Year", side=1,  cex=1, line = 2.2)
dev.off()

jpeg(sprintf("./Earthwatch/MacPass/figures/porsild_1_thaw_%s.jpeg", year), width = 6, height = 4, units = "in", res = 300)
par(mar = c(3.3, 3.3, 1, 3.3))
plot(porsild_1$year, porsild_1$mean, type='n', xlim = c(1990,year), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rev(porsild_1$year[-c(16:19)]), porsild_1$year[-c(16:19)]), c(rev(porsild_1$c95[-c(16:19)]), porsild_1$c5[-c(16:19)]), col = scales::alpha("violet",0.6), border = NA)
polygon(c(rev(porsild_1$year[c(16:19)]), porsild_1$year[c(16:19)]), c(rev(porsild_1$c95[c(16:19)]), porsild_1$c5[c(16:19)]), col = scales::alpha("violet",0.6), border = NA)
lines(porsild_1$year, porsild_1$mean, col = 'violet', lwd = 2)
abline(coef(porsild_1_lm), lty = 2, lwd = 2, col = "violet")
# points(porsild_1$year[16], porsild_1$mean[16], col = 'violet', pch = 16)
par(new = T)
plot(porsild_1$year, porsild_1$n, col = 'violet', type = "l", xlim = c(1990,year), ylim = c(20,80), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(porsild_1$year[n_pts_c], porsild_1$n[n_pts_c], col = 'violet', pch = 17)
axis(side = 4, at = seq(0,40,5))
legend("topleft", "PF1 (1380 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(porsild_1_slope) ~ "cm yr"^-1), bty = "n", inset = c(0,0.1))
mtext("Thaw depth (cm)", side=2, cex = 1, line = 2.2)
mtext("Probe points (n)", side=4, cex = 1, line = 2.2, adj = 0)
mtext("Year", side=1,  cex=1, line = 2.2)
dev.off()

jpeg(sprintf("./Earthwatch/MacPass/figures/porsild_2_thaw_%s.jpeg", year), width = 6, height = 4, units = "in", res = 300)
par(mar = c(3.3, 3.3, 1, 3.3))
plot(porsild_2$year, porsild_2$mean, type='n', xlim = c(1990,year), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rev(porsild_2$year[-c(16:19)]), porsild_2$year[-c(16:19)]), c(rev(porsild_2$c95[-c(16:19)]), porsild_2$c5[-c(16:19)]), col = scales::alpha("violet",0.6), border = NA)
polygon(c(rev(porsild_2$year[c(16:19)]), porsild_2$year[c(16:19)]), c(rev(porsild_2$c95[c(16:19)]), porsild_2$c5[c(16:19)]), col = scales::alpha("violet",0.6), border = NA)
lines(porsild_2$year, porsild_2$mean, col = 'violet', lwd = 2)
# points(porsild_2$year[16], porsild_2$mean[16], col = 'violet', pch = 16)
abline(coef(porsild_2_lm), lty = 2, lwd = 2, col = "violet")
par(new = T)
plot(porsild_2$year, porsild_2$n, col = 'violet', type = "l", xlim = c(1990,year), ylim = c(0,120), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(porsild_2$year[n_pts_c], porsild_2$n[n_pts_c], col = 'violet', pch = 17)
axis(side = 4, at = seq(0,30,5))
legend("topleft", "PF2 (1380 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(porsild_2_slope) ~ "cm yr"^-1), bty = "n", inset = c(0,0.1))
mtext("Thaw depth (cm)", side=2, cex = 1, line = 2.2)
mtext("Probe points (n)", side=4, cex = 1, line = 2.2, adj = 0)
mtext("Year", side=1,  cex=1, line = 2.2)
dev.off()

jpeg(sprintf("./Earthwatch/MacPass/figures/d6_thaw_%s.jpeg", year), width = 6, height = 4, units = "in", res = 300)
par(mar = c(3.3, 3.3, 1, 3.3))
plot(d6$year, d6$mean, type='n', xlim = c(1990,year), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rev(d6$year[-c(32:len_yr)]), d6$year[-c(32:len_yr)]), c(rev(d6$c95[-c(32:len_yr)]), d6$c5[-c(32:len_yr)]), col = scales::alpha("orange",0.6), border = NA)
polygon(c(rev(d6$year[c(32:len_yr)]), d6$year[c(32:len_yr)]), c(rev(d6$c95[c(32:len_yr)]), d6$c5[c(32:len_yr)]), col = scales::alpha("orange",0.6), border = NA)
lines(d6$year, d6$mean, col = 'orange', lwd = 2)
abline(coef(d6_lm), lty = 2, lwd = 2, col = "orange")
# points(d6$year[n_pts_c], d6$mean[n_pts_c], col = 'orange', pch = 16)
par(new = T)
plot(d6$year, d6$n, col = 'orange', type = "l", xlim = c(1990,year), ylim = c(5,120), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(d6$year[n_pts_c], d6$n[n_pts_c], col = 'orange', pch = 17)
axis(side = 4, at = seq(0,40,10))
legend("topleft", "D6 (1473 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(d6_slope) ~ "cm yr"^-1), bty = "n")
mtext("Thaw depth (cm)", side=2, cex = 1, line = 2.2)
mtext("Probe points (n)", side=4, cex = 1, line = 2.2, adj = 0)
mtext("Year", side=1,  cex=1, line = 2.2)
dev.off()

jpeg(sprintf("./Earthwatch/MacPass/figures/d2_thaw_%s.jpeg", year), width = 6, height = 4, units = "in", res = 300)
par(mar = c(3.3, 3.3, 1, 3.3))
plot(d2$year, d2$mean, type='n', xlim = c(1990,year), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rev(d2$year[-c(32:len_yr)]), d2$year[-c(32:len_yr)]), c(rev(d2$c95[-c(32:len_yr)]), d2$c5[-c(32:len_yr)]), col = scales::alpha("yellow3",0.6), border = NA)
polygon(c(rev(d2$year[c(32:len_yr)]), d2$year[c(32:len_yr)]), c(rev(d2$c95[c(32:len_yr)]), d2$c5[c(32:len_yr)]), col = scales::alpha("yellow3",0.6), border = NA)
lines(d2$year, d2$mean, col = 'yellow3', lwd = 2)
# points(d2$year[n_pts_c], d2$mean[n_pts_c], col = 'yellow3', pch = 16)
abline(coef(d2_lm), lty = 2, lwd = 2, col = "yellow3")
par(new = T)
plot(d2$year, d2$n, col = 'yellow3', type = "l", xlim = c(1990,year), ylim = c(30,120), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
lines(d2$year[c(n_pts_c,33)], d2$n[c(n_pts_c,33)], col = 'yellow3', pch = 17)
axis(side = 4, at = seq(10,60,10))
legend("topleft", "D2 (1477 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(d2_slope) ~ "cm yr"^-1), bty = "n")
mtext("Thaw depth (cm)", side=2, cex = 1, line = 2.2)
mtext("Probe points (n)", side=4, cex = 1, line = 2.2, adj = 0)
mtext("Year", side=1,  cex=1, line = 2.2)
dev.off()

jpeg(sprintf("./Earthwatch/MacPass/figures/gf_thaw_%s.jpeg", year), width = 6, height = 4, units = "in", res = 300)
par(mar = c(3.3, 3.3, 1, 3.3))
plot(goose$year, goose$mean, type='n', xlim = c(1990,year), ylim = rev(c(0,180)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rev(goose$year[-c(32:len_yr)]), goose$year[-c(32:len_yr)]), c(rev(goose$c95[-c(32:len_yr)]), goose$c5[-c(32:len_yr)]), col = scales::alpha("blue",0.6), border = NA)
polygon(c(rev(goose$year[c(32:len_yr)]), goose$year[c(32:len_yr)]), c(rev(goose$c95[c(32:len_yr)]), goose$c5[c(32:len_yr)]), col = scales::alpha("blue",0.6), border = NA)
lines(goose$year, goose$mean, col = 'blue', lwd = 2)
# points(goose$year[c(n_pts_c,33)], goose$mean[c(n_pts_c,33)], col = 'blue', pch = 16)
abline(coef(goose_lm), lty = 2, lwd = 2, col = "blue")
par(new = T)
plot(goose$year, goose$n, col = 'blue', type = "l", xlim = c(1990,year), ylim = c(5,160), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(goose$year[c(n_pts_c,33)], goose$n[c(n_pts_c,33)], col = 'blue', pch = 17)
axis(side = 4, at = seq(10,60,10))
legend("topleft", "GF (1621 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(goose_slope) ~ "cm yr"^-1), bty = "n", inset = c(0,0.2))
mtext("Thaw depth (cm)", side=2, cex = 1, line = 2.2)
mtext("Probe points (n)", side=4, cex = 1, line = 2.2, adj = 0)
mtext("Year", side=1,  cex=1, line = 2.2)
dev.off()

jpeg(sprintf("./Earthwatch/MacPass/figures/sf_thaw_%s.jpeg", year), width = 6, height = 4, units = "in", res = 300)
par(mar = c(3.3, 3.3, 1, 3.3))
plot(snow$year, snow$mean, type='n', xlim = c(1990,year), ylim = rev(c(0,180)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rev(snow$year[c(1:8)]), snow$year[c(1:8)]), c(rev(snow$c95[c(1:8)]), snow$c5[c(1:8)]), col = scales::alpha("royalblue1",0.6), border = NA)
polygon(c(rev(snow$year[c(13:26)]), snow$year[c(13:26)]), c(rev(snow$c95[c(13:26)]), snow$c5[c(13:26)]), col = scales::alpha("royalblue1",0.6), border = NA)
polygon(c(rev(snow$year[c(28:nrow(snow))]), snow$year[c(28:nrow(snow))]), c(rev(snow$c95[c(28:nrow(snow))]), snow$c5[c(28:nrow(snow))]), col = scales::alpha("royalblue1",0.6), border = NA)
lines(snow$year, snow$mean, col = 'royalblue1', lwd = 2)
# points(snow$year[28], snow$mean[28], col = 'royalblue1', pch = 16)
abline(coef(snow_lm), lty = 2, lwd = 2, col = "royalblue1")
par(new = T)
plot(snow$year, snow$n, col = 'royalblue1', type = "l", xlim = c(1990,year), ylim = c(5,220), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(snow$year[28], snow$n[28], col = 'royalblue1', pch = 17)
axis(side = 4, at = seq(20,100,20))
legend("topleft", "SF (1660 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
legend("bottomright", legend=bquote("Slope = " ~ .(snow_slope) ~ "cm yr"^-1), bty = "n", inset = c(0,0.1))
mtext("Thaw depth (cm)", side=2, cex = 1, line = 2.2)
mtext("Probe points (n)", side=4, cex = 1, line = 2.2, adj = 0)
mtext("Year", side=1,  cex=1, line = 2.2)
dev.off()
##

# Export as 5 x 7
jpeg(sprintf("./earthwatch/MacPass/Reports/EW%s_Figure07.jpg", year), width = 5, height = 7, units = "in", res = 300)
# jpeg(sprintf("./Earthwatch/MacPass/figures/pipeline_thaw_%s.jpeg", year), width = 5, height = 7, units = "in", res = 300)
par(mfrow = c(3, 1))
par(cex = 0.75)
par(mar = c(1.3, 2.3, 1, 2.3), oma = c(2, 1, 0, 1)+0.2)
plot(pp_control_p$year, pp_control_p$mean, type='n', xlim = c(1990,year), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rev(pp_control_p$year[-c(12:14)]), pp_control_p$year[-c(12:14)]), c(rev(pp_control_p$c95[-c(12:14)]), pp_control_p$c5[-c(12:14)]), col = "firebrick1", border = NA)
polygon(c(rev(pp_control_p$year[c(12:14)]), pp_control_p$year[c(12:14)]), c(rev(pp_control_p$c95[c(12:14)]), pp_control_p$c5[c(12:14)]), col = "firebrick1", border = NA)
lines(pp_control_p$year, pp_control_p$mean, col = 'firebrick4', lwd = 2)
# points(pp_control_p$year[12], pp_control_p$mean[12], col = 'firebrick4', pch = 16)
abline(h = mean(pp_control_p$mean, na.rm = T), lty = 2, lwd = 2)
par(new = T)
plot(pp_control_p$year, pp_control_p$n, col = 'firebrick4', type = "l", xlim = c(1990,year), ylim = c(5,350), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(pp_control_p$year[12], pp_control_p$n[12], col = 'firebrick4', pch = 17)
axis(side = 4, at = seq(20,100,20))
legend("topleft", "PP: pipeline-control (1623 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))

plot(pp_pipeline_p$year, pp_pipeline_p$mean, type='n', xlim = c(1990,year), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rev(pp_pipeline_p$year[-c(12:14)]), pp_pipeline_p$year[-c(12:14)]), c(rev(pp_pipeline_p$c95[-c(12:14)]), pp_pipeline_p$c5[-c(12:14)]), col = "firebrick1", border = NA)
polygon(c(rev(pp_pipeline_p$year[c(12:14)]), pp_pipeline_p$year[c(12:14)]), c(rev(pp_pipeline_p$c95[c(12:14)]), pp_pipeline_p$c5[c(12:14)]), col = "firebrick1", border = NA)
lines(pp_pipeline_p$year, pp_pipeline_p$mean, col = 'firebrick4', lwd = 2)
# points(pp_pipeline_p$year[12], pp_pipeline_p$mean[12], col = 'firebrick4', pch = 16)
abline(h = mean(pp_pipeline_p$mean, na.rm = T), lty = 2, lwd = 2)
par(new = T)
plot(pp_pipeline_p$year, pp_pipeline_p$n, col = 'firebrick4', type = "l", xlim = c(1990,year), ylim = c(5,350), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(pp_pipeline_p$year[12], pp_pipeline_p$n[12], col = 'firebrick4', pch = 17)
axis(side = 4, at = seq(20,100,20))
legend("topleft", "PP: pipeline (1623 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
mtext("Probe points (n)", side=4, cex = 0.75, line = 2.2, adj = 0)

plot(pp_track_t$year, pp_track_t$mean, type='n', xlim = c(1990,year), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rev(pp_track_t$year[-c(12:14)]), pp_track_t$year[-c(12:14)]), c(rev(pp_track_t$c95[-c(12:14)]), pp_track_t$c5[-c(12:14)]), col = "firebrick1", border = NA)
polygon(c(rev(pp_track_t$year[c(12:14)]), pp_track_t$year[c(12:14)]), c(rev(pp_track_t$c95[c(12:14)]), pp_track_t$c5[c(12:14)]), col = "firebrick1", border = NA)
lines(pp_track_t$year, pp_track_t$mean, col = 'firebrick4', lwd = 2)
# points(pp_track_t$year[12], pp_track_t$mean[12], col = 'firebrick4', pch = 16)
abline(h = mean(pp_track_t$mean, na.rm = T), lty = 2, lwd = 2)
par(new = T)
plot(pp_track_t$year, pp_track_t$n, col = 'firebrick4', type = "l", xlim = c(1990,year), ylim = c(5,850), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(pp_track_t$year[12], pp_track_t$n[12], col = 'firebrick4', pch = 17)
axis(side = 4, at = seq(0,200,50))
legend("topleft", "PP: track (1623 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))

mtext("Thaw depth (cm)", side=2, outer=TRUE, cex = 0.75, adj=0.5)
mtext("Year", side=1, outer=TRUE, cex=0.75, line = 1)
dev.off()

`%nin%` <- Negate(`%in%`)
thaw_mm %>% 
  filter(year == year, site %nin% c("Porsild","Pipeline")) %>% 
  summarise(n = sum(n))

## Single-panel figure
col_pal <- wes_palette("Darjeeling1", 8, type = "continuous")
y_limit <- c(20,160)
jpeg(sprintf("./earthwatch/MacPass/Reports/EW%s_Figure04.jpg", year), width = 6, height = 7, units = "in", res = 300)
jpeg(sprintf("./Earthwatch/Macpass/figures/thaw_depth_%s.jpg", year),
     height = 5, width = 6, res = 300, units = "in")
par(mar = c(4,4,2,1))
par(xpd = FALSE)
# Hare Foot
plot(hare$year, hare$mean, type='n', xlim = c(1990,year), ylim = rev(y_limit), xaxt='n',yaxt = "n", ann=FALSE)
points(hare[c(nrow(hare)-1, nrow(hare)),1], hare[c(nrow(hare)-1, nrow(hare)),4], col = scales::alpha(col_pal[1], 0.6), pch = 15)
lines(hare$year, hare$mean, xlim = c(2002,year), ylim = rev(y_limit), type = "l", lwd=2, col = col_pal[1])
# legend("topleft", "PPA (17 m.a.s.l.)", bty = "n", inset = c(0,0.05))
abline(coef(hare_lm),lty=2, col = col_pal[1])
# Beaver Pond
lines(beaver$year, beaver$mean, xlim = c(2002,year), ylim = rev(y_limit), type = "l", lwd=2, col = col_pal[3])
points(beaver[c(nrow(beaver)-1, nrow(beaver)),1], beaver[c(nrow(beaver)-1, nrow(beaver)),4], col = scales::alpha(col_pal[3],0.6), pch = 15)
# legend("topleft", "PPD (17 m.a.s.l.)", bty = "n", inset = c(0,0.05))
abline(coef(beaver_lm),lty=2, col = col_pal[3])
# Porsild
lines(porsild$year, porsild$mean, xlim = c(2002,year), ylim = rev(y_limit), type = "l", lwd=2, col = col_pal[4])
points(porsild[c(nrow(porsild)-1, nrow(porsild)),1], porsild[c(nrow(porsild)-1, nrow(porsild)),4], col = scales::alpha(col_pal[4],0.6), pch = 15)
abline(coef(porsild_lm),lty=2, col = col_pal[4])
# D6
lines(d6$year, d6$mean, xlim = c(2002,year), ylim = rev(y_limit), type = "l", lwd=2, col = col_pal[5])
points(d6[c(nrow(d6)-1, nrow(d6)),1], d6[c(nrow(d6)-1, nrow(d6)),4], col = scales::alpha(col_pal[5],0.6), pch = 15)
abline(coef(d6_lm),lty=2, col = col_pal[5])
# D2
lines(d2$year, d2$mean, xlim = c(2002,year), ylim = rev(y_limit), type = "l", lwd=2, col = col_pal[6])
points(d2[c(nrow(d2)-1, nrow(d2)),1], d2[c(nrow(d2)-1, nrow(d2)),4], col = scales::alpha(col_pal[6],0.6), pch = 15)
abline(coef(d2_lm),lty=2, col = col_pal[6])
# Goose Flats
lines(goose$year, goose$mean, xlim = c(2002,year), ylim = rev(y_limit), type = "l", lwd=2, col = col_pal[7])
points(goose[c(nrow(goose)-1, nrow(goose)),1], goose[c(nrow(goose)-1, nrow(goose)),4], col = scales::alpha(col_pal[7],0.6), pch = 15)
abline(coef(goose_lm),lty=2, col = col_pal[7])
# Snow Fence
lines(snow$year, snow$mean, xlim = c(2002,year), ylim = rev(y_limit), type = "l", lwd=2, col = col_pal[8])
points(snow[c(nrow(snow)-1, nrow(snow)),1], snow[c(nrow(snow)-1, nrow(snow)),4], col = scales::alpha(col_pal[8],0.6), pch = 15)
abline(coef(snow_lm),lty=2, col = col_pal[8])

# Regression slopes
legend(
  "bottomleft",
  legend = bquote("HF slope = " ~ .(-hare_slope) ~ "cm yr" ^ -1),
  bty = "n",
  inset = c(0.0, 0.36),
  text.col = col_pal[1]
)
legend(
  "bottomleft",
  legend = bquote("BP slope = " ~ .(-beaver_slope) ~ "cm yr" ^ -1),
  bty = "n",
  inset = c(0.0, 0.30),
  text.col = col_pal[3]
)
legend(
  "bottomleft",
  legend = bquote("PF slope = " ~ .(-porsild_slope) ~ "cm yr" ^ -1),
  bty = "n",
  inset = c(0.0, 0.24),
  text.col = col_pal[4]
)
legend(
  "bottomleft",
  legend = bquote("D6 slope = " ~ .(-d6_slope) ~ "cm yr" ^ -1),
  bty = "n",
  inset = c(0.0, 0.18),
  text.col = col_pal[5]
)
legend(
  "bottomleft",
  legend = bquote("D2 slope = " ~ .(-d2_slope) ~ "cm yr" ^ -1),
  bty = "n",
  inset = c(0.0, 0.12),
  text.col = col_pal[6]
)
legend(
  "bottomleft",
  legend = bquote("GF slope = " ~ .(-goose_slope) ~ "cm yr" ^ -1),
  bty = "n",
  inset = c(0.0, 0.06),
  text.col = col_pal[7]
)
legend(
  "bottomleft",
  legend = bquote("SF slope = " ~ .(-snow_slope) ~ "cm yr" ^ -1),
  bty = "n",
  inset = c(0.0, 0.0),
  text.col = col_pal[8]
)
# Axes
axis(1, at = seq(1990,year,1), labels = NA)
axis(1, at = seq(1990,year,2), labels = seq(1990,year,2))
axis(2, at = rev(seq(20,160,20)), labels = NA)
axis(2, at = rev(seq(20,160,20)), labels = rev(seq(20,160,20)), tick = FALSE)
mtext(side = 1, "Year", line = 2.5)
mtext(side = 2, "Thaw depth (cm)", line = 2.5)

# Legend
par(xpd = NA)
legend(1992,22, c("HF","BP","PF","D6","D2","GF","SF"), col = col_pal[c(1,3:8)], horiz = T, 
       bty = "n", pch = 15, pt.cex = c(1,1), text.width = 2, y.intersp = 1, inset = c(0.05,-0.02))
dev.off()








jpeg(sprintf("./earthwatch/MacPass/Reports/EW%s_Figure05.jpg", year), width = 6, height = 7, units = "in", res = 300)
par(mfrow = c(4,2))
par(mar = c(0.5, 2, 1, 2), oma = c(2.5,1.5,0,1.5))
plot(hare$year, hare$mean, type='n', xlim = c(1990,year), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "", xaxt = "n" )
polygon(c(rev(hare$year[-c(32:34)]), hare$year[-c(32:34)]), c(rev(hare$c95[-c(32:34)]), hare$c5[-c(32:34)]), col = scales::alpha("darkred",0.6), border = NA)
polygon(c(rev(hare$year[c(32:34)]), hare$year[c(32:34)]), c(rev(hare$c95[c(32:34)]), hare$c5[c(32:34)]), col = scales::alpha("darkred",0.6), border = NA)
lines(hare$year, hare$mean, col = 'darkred', lwd = 2)
# points(hare$year[n_pts_c], hare$mean[n_pts_c], col = 'darkred', pch = 16)
abline(coef(hare_lm), lty = 2, lwd = 2, col = "darkred")
par(new = T)
plot(hare$year, hare$n, col = 'darkred', type = "l", xlim = c(1990,year), ylim = c(10,100), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(hare$year[n_pts_c], hare$n[n_pts_c], col = 'darkred', pch = 17)
axis(side = 4, at = seq(0,40,10))
legend("topleft", "HF (1260 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(hare_slope) ~ "cm yr"^-1), bty = "n")
axis(1, labels = FALSE)

plot(beaver$year, beaver$mean, type='n', xlim = c(1990,year), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "", xaxt = "n" )
polygon(c(rev(beaver$year[-c(32:34)]), beaver$year[-c(32:34)]), c(rev(beaver$c95[-c(32:34)]), beaver$c5[-c(32:34)]), col = scales::alpha("red",0.6), border = NA)
polygon(c(rev(beaver$year[c(32:34)]), beaver$year[c(32:34)]), c(rev(beaver$c95[c(32:34)]), beaver$c5[c(32:34)]), col = scales::alpha("red",0.6), border = NA)
lines(beaver$year, beaver$mean, col = 'red', lwd = 2)
# points(beaver$year[n_pts_c], beaver$mean[n_pts_c], col = 'red', pch = 16)
abline(coef(beaver_lm), lty = 2, lwd = 2, col = "red")
par(new = T)
plot(beaver$year, beaver$n, col = 'red', type = "l", xlim = c(1990,year), ylim = c(30,150), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(beaver$year[n_pts_c], beaver$n[n_pts_c], col = 'red', pch = 17)
axis(side = 4, at = seq(0,80,20))
legend("topleft", "BP (1272 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(beaver_slope) ~ "cm yr"^-1), bty = "n", inset = c(0,0.1))
axis(1, labels = FALSE)

plot(porsild_1$year, porsild_1$mean, type='n', xlim = c(1990,year), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "", xaxt = "n" )
polygon(c(rev(porsild_1$year[-c(16:18)]), porsild_1$year[-c(16:18)]), c(rev(porsild_1$c95[-c(16:18)]), porsild_1$c5[-c(16:18)]), col = scales::alpha("violet",0.6), border = NA)
polygon(c(rev(porsild_1$year[c(16:18)]), porsild_1$year[c(16:18)]), c(rev(porsild_1$c95[c(16:18)]), porsild_1$c5[c(16:18)]), col = scales::alpha("violet",0.6), border = NA)
lines(porsild_1$year, porsild_1$mean, col = 'violet', lwd = 2)
abline(coef(porsild_1_lm), lty = 2, lwd = 2, col = "violet")
# points(porsild_1$year[16], porsild_1$mean[16], col = 'violet', pch = 16)
par(new = T)
plot(porsild_1$year, porsild_1$n, col = 'violet', type = "l", xlim = c(1990,year), ylim = c(20,80), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(porsild_1$year[n_pts_c], porsild_1$n[n_pts_c], col = 'violet', pch = 17)
axis(side = 4, at = seq(0,40,5))
legend("topleft", "PF1 (1380 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(porsild_1_slope) ~ "cm yr"^-1), bty = "n", inset = c(0,0.1))
axis(1, labels = FALSE)

plot(porsild_2$year, porsild_2$mean, type='n', xlim = c(1990,year), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "", xaxt = "n" )
polygon(c(rev(porsild_2$year[-c(16:18)]), porsild_2$year[-c(16:18)]), c(rev(porsild_2$c95[-c(16:18)]), porsild_2$c5[-c(16:18)]), col = scales::alpha("violet",0.6), border = NA)
polygon(c(rev(porsild_2$year[c(16:18)]), porsild_2$year[c(16:18)]), c(rev(porsild_2$c95[c(16:18)]), porsild_2$c5[c(16:18)]), col = scales::alpha("violet",0.6), border = NA)
lines(porsild_2$year, porsild_2$mean, col = 'violet', lwd = 2)
# points(porsild_2$year[16], porsild_2$mean[16], col = 'violet', pch = 16)
abline(coef(porsild_2_lm), lty = 2, lwd = 2, col = "violet")
par(new = T)
plot(porsild_2$year, porsild_2$n, col = 'violet', type = "l", xlim = c(1990,year), ylim = c(0,120), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(porsild_2$year[n_pts_c], porsild_2$n[n_pts_c], col = 'violet', pch = 17)
axis(side = 4, at = seq(0,30,5))
legend("topleft", "PF2 (1380 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(porsild_2_slope) ~ "cm yr"^-1), bty = "n", inset = c(0,0.1))
axis(1, labels = FALSE)

plot(d6$year, d6$mean, type='n', xlim = c(1990,year), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "", xaxt = "n" )
polygon(c(rev(d6$year[-c(32:34)]), d6$year[-c(32:34)]), c(rev(d6$c95[-c(32:34)]), d6$c5[-c(32:34)]), col = scales::alpha("orange",0.6), border = NA)
polygon(c(rev(d6$year[c(32:34)]), d6$year[c(32:34)]), c(rev(d6$c95[c(32:34)]), d6$c5[c(32:34)]), col = scales::alpha("orange",0.6), border = NA)
lines(d6$year, d6$mean, col = 'orange', lwd = 2)
abline(coef(d6_lm), lty = 2, lwd = 2, col = "orange")
# points(d6$year[n_pts_c], d6$mean[n_pts_c], col = 'orange', pch = 16)
par(new = T)
plot(d6$year, d6$n, col = 'orange', type = "l", xlim = c(1990,year), ylim = c(5,120), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(d6$year[n_pts_c], d6$n[n_pts_c], col = 'orange', pch = 17)
axis(side = 4, at = seq(0,40,10))
legend("topleft", "D6 (1473 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(d6_slope) ~ "cm yr"^-1), bty = "n")
axis(1, labels = FALSE)

plot(d2$year, d2$mean, type='n', xlim = c(1990,year), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "", xaxt = "n" )
polygon(c(rev(d2$year[-c(32:34)]), d2$year[-c(32:34)]), c(rev(d2$c95[-c(32:34)]), d2$c5[-c(32:34)]), col = scales::alpha("yellow3",0.6), border = NA)
polygon(c(rev(d2$year[c(32:34)]), d2$year[c(32:34)]), c(rev(d2$c95[c(32:34)]), d2$c5[c(32:34)]), col = scales::alpha("yellow3",0.6), border = NA)
lines(d2$year, d2$mean, col = 'yellow3', lwd = 2)
# points(d2$year[n_pts_c], d2$mean[n_pts_c], col = 'yellow3', pch = 16)
abline(coef(d2_lm), lty = 2, lwd = 2, col = "yellow3")
par(new = T)
plot(d2$year, d2$n, col = 'yellow3', type = "l", xlim = c(1990,year), ylim = c(30,120), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
lines(d2$year[c(n_pts_c,33)], d2$n[c(n_pts_c,33)], col = 'yellow3', pch = 17)
axis(side = 4, at = seq(10,60,10))
legend("topleft", "D2 (1477 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(d2_slope) ~ "cm yr"^-1), bty = "n")
axis(1, labels = FALSE)

plot(goose$year, goose$mean, type='n', xlim = c(1990,year), ylim = rev(c(0,180)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rev(goose$year[-c(32:34)]), goose$year[-c(32:34)]), c(rev(goose$c95[-c(32:34)]), goose$c5[-c(32:34)]), col = scales::alpha("blue",0.6), border = NA)
polygon(c(rev(goose$year[c(32:34)]), goose$year[c(32:34)]), c(rev(goose$c95[c(32:34)]), goose$c5[c(32:34)]), col = scales::alpha("blue",0.6), border = NA)
lines(goose$year, goose$mean, col = 'blue', lwd = 2)
# points(goose$year[c(n_pts_c,33)], goose$mean[c(n_pts_c,33)], col = 'blue', pch = 16)
abline(coef(goose_lm), lty = 2, lwd = 2, col = "blue")
par(new = T)
plot(goose$year, goose$n, col = 'blue', type = "l", xlim = c(1990,year), ylim = c(5,160), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(goose$year[c(n_pts_c,33)], goose$n[c(n_pts_c,33)], col = 'blue', pch = 17)
axis(side = 4, at = seq(10,60,10))
legend("topleft", "GF (1621 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
legend("bottomleft", legend=bquote("Slope = " ~ .(goose_slope) ~ "cm yr"^-1), bty = "n", inset = c(0,0.2))
mtext("Thaw depth (cm)", side=2, cex = 0.75, line = 0, outer = TRUE)
mtext("Year", side=1,  cex=0.75, line = 1.2, outer = TRUE)

plot(snow$year, snow$mean, type='n', xlim = c(1990,year), ylim = rev(c(0,180)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(rev(snow$year[c(1:8)]), snow$year[c(1:8)]), c(rev(snow$c95[c(1:8)]), snow$c5[c(1:8)]), col = scales::alpha("royalblue1",0.6), border = NA)
polygon(c(rev(snow$year[c(13:26)]), snow$year[c(13:26)]), c(rev(snow$c95[c(13:26)]), snow$c5[c(13:26)]), col = scales::alpha("royalblue1",0.6), border = NA)
polygon(c(rev(snow$year[c(28:nrow(snow))]), snow$year[c(28:nrow(snow))]), c(rev(snow$c95[c(28:nrow(snow))]), snow$c5[c(28:nrow(snow))]), col = scales::alpha("royalblue1",0.6), border = NA)
lines(snow$year, snow$mean, col = 'royalblue1', lwd = 2)
# points(snow$year[28], snow$mean[28], col = 'royalblue1', pch = 16)
abline(coef(snow_lm), lty = 2, lwd = 2, col = "royalblue1")
par(new = T)
plot(snow$year, snow$n, col = 'royalblue1', type = "l", xlim = c(1990,year), ylim = c(5,220), yaxs = "i", xaxs = "i", xlab = "", ylab = "", axes = F)
# points(snow$year[28], snow$n[28], col = 'royalblue1', pch = 17)
axis(side = 4, at = seq(20,100,20))
legend("topleft", "SF (1660 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))
legend("bottomright", legend=bquote("Slope = " ~ .(snow_slope) ~ "cm yr"^-1), bty = "n", inset = c(0,0.1))
mtext("Probe points (n)", side=4, cex = 0.75, line = 0, outer = TRUE)
dev.off()
