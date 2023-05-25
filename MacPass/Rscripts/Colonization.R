# BiocManager::install(version="3.11")
# BiocManager::install("AICcmodavg")

library(corrplot)
library(psych)
library(tidyverse)
library(scales)
library(vegan)
library(lme4)
library(AICcmodavg)
library(lubridate)
library(nlme)

rm(list = ls())

# canol.tree <- read.csv("~/Dropbox/School/School/Postdoc/Earthwatch/Mackenzie Mountains/Canol_Colonization/canol_tree_data_all.csv")
canol.tree <- read.csv("~/Dropbox/School/School/Postdoc/Earthwatch/Mackenzie Mountains/Canol_Colonization/canol_tree_data_all_elev.csv")
canol.tree$age <- canol.tree$last_year - canol.tree$first_year
names(canol.tree)
# canol.tree$DRC_cm[257] <- 20
canol.tree$species[canol.tree$species == "Bp"] <- "Pb"
levels(as.factor(canol.tree$species))
canol.tree$DRC_cm <- as.numeric(canol.tree$DRC_cm)
canol.dis <- subset(canol.tree, group == "road")
canol.nat <- subset(canol.tree, group == "off")

canol.dis.sub <- canol.dis[,c("ID","species","age","DRC_cm","DBH_cm","height_tot_cm","elev","last_year","first_year","latitude_deg","longitude_deg")]
canol.nat.sub <- canol.nat[,c("ID","species","age","DRC_cm","DBH_cm","height_tot_cm","elev","last_year","first_year","latitude_deg","longitude_deg")]
# canol.dis.sub <- canol.dis.sub[complete.cases(canol.dis.sub),]
# canol.nat.sub <- canol.nat.sub[complete.cases(canol.nat.sub),]

pairs.panels(canol.dis.sub, scale = T)
pairs.panels(canol.nat.sub, scale = T)

# Need to fill missing DBH values

## Disturbance
summary(lm(DBH_cm ~ DRC_cm + elev, canol.dis.sub)) # adjR2 = 0.9609
# Construct linear model based on non-NA pairs
canol.dis.sub.1 <- canol.dis.sub %>% filter(!is.na(DBH_cm))
canol.dis.sub.1.fit <- lm(DBH_cm ~ DRC_cm + elev, data = canol.dis.sub.1)
summary(canol.dis.sub.1.fit) # R2 = 0.8491
canol.dis.sub.2 <- canol.dis.sub %>% 
  mutate(pred = predict(canol.dis.sub.1.fit,.)) %>%
  # Replace NA with pred in var1
  mutate(DBH_cm = ifelse(is.na(DBH_cm), pred, DBH_cm))
canol.dis.sub.2 %>% as.data.frame()

par(mar = c(2,2,1,1))
plot(pred ~ DBH_cm, canol.dis.sub.2, pch = 16, col = alpha("blue",0.5))
pairs.panels(canol.dis.sub.2, scale = T)

## Natural
summary(lm(DBH_cm ~ DRC_cm + elev + species, canol.nat.sub)) # adjR2 = 0.9476
# Construct linear model based on non-NA pairs
canol.nat.sub.1 <- canol.nat.sub %>% filter(!is.na(DBH_cm))
canol.nat.sub.1.fit <- lm(DBH_cm ~ DRC_cm + elev + species, data = canol.nat.sub.1)
summary(canol.nat.sub.1.fit) # R2 = 0.8069
canol.nat.sub.2 <- canol.nat.sub %>% 
  mutate(pred = predict(canol.nat.sub.1.fit,.)) %>%
  # Replace NA with pred in var1
  mutate(DBH_cm = ifelse(is.na(DBH_cm), pred, DBH_cm))
canol.nat.sub.2 %>% as.data.frame()


# Need to fill missing total height values

## Disturbance
summary(lm(height_tot_cm ~ elev + species + DRC_cm, canol.dis.sub.2)) # adjR2 = 0.6545
# Construct linear model based on non-NA pairs
canol.dis.sub.3 <- canol.dis.sub.2 %>% filter(!is.na(height_tot_cm))
canol.dis.sub.3.fit <- lm(height_tot_cm ~ elev + species + DRC_cm, data = canol.dis.sub.3)
summary(canol.dis.sub.3.fit) # R2 = 0.6527
canol.dis.sub.4 <- canol.dis.sub.2 %>% 
  mutate(pred = predict(canol.dis.sub.3.fit,.)) %>%
  # Replace NA with pred in var1
  mutate(height_tot_cm = ifelse(is.na(height_tot_cm), pred, height_tot_cm))
canol.dis.sub.4 %>% as.data.frame()

par(mar = c(2,2,1,1))
plot(pred ~ height_tot_cm, canol.dis.sub.4, pch = 16, col = alpha("blue",0.5))
pairs.panels(canol.dis.sub.4, scale = T)

## Natural
summary(lm(height_tot_cm ~ elev + species + DRC_cm, canol.nat.sub.2)) # adjR2 = 0.198
# Construct linear model based on non-NA pairs
canol.nat.sub.3 <- canol.nat.sub.2 %>% filter(!is.na(height_tot_cm))
canol.nat.sub.3.fit <- lm(height_tot_cm ~ elev + species + DRC_cm, data = canol.nat.sub.3)
summary(canol.nat.sub.3.fit) # R2 = 0.7936
canol.nat.sub.4 <- canol.nat.sub.2 %>% 
  mutate(pred = predict(canol.nat.sub.3.fit,.)) %>%
  # Replace NA with pred in var1
  mutate(height_tot_cm = ifelse(is.na(height_tot_cm), pred, height_tot_cm))
canol.nat.sub.4 %>% as.data.frame()

par(mar = c(2,2,1,1))
plot(pred ~ height_tot_cm, canol.nat.sub.4, pch = 16, col = alpha("blue",0.5))
pairs.panels(canol.nat.sub.4, scale = T)

summary(canol.dis.sub.4)
summary(canol.nat.sub.4)

summary(lm(age ~ height_tot_cm + I(height_tot_cm^2), data = canol.dis.sub.4))     # Both sig
summary(lm(age ~ DBH_cm + I(DBH_cm^2), data = canol.dis.sub.4))                   # linear
summary(lm(age ~ DRC_cm + I(DRC_cm^2), data = canol.dis.sub.4))                   # linear
summary(lm(age ~ elev + I(elev^2), data = canol.dis.sub.4))                       # No diff

summary(lm(age ~ height_tot_cm + I(height_tot_cm^2), data = canol.nat.sub.4))     # Both sig
summary(lm(age ~ DBH_cm + I(DBH_cm^2), data = canol.nat.sub.4))                   # DBH^2 is sig
summary(lm(age ~ DRC_cm + I(DRC_cm^2), data = canol.nat.sub.4))                   # linear
summary(lm(age ~ elev + I(elev^2), data = canol.nat.sub.4))                       # No diff

summary(lm(age ~ species + height_tot_cm + elev, canol.dis.sub.4))
summary(lm(age ~ species + DRC_cm + elev, canol.nat.sub.4))

summary(lm1 <- lm(age ~ ., data = canol.dis.sub.4[-c(1,4,8:12)]))
slm1 <- step(lm1)
summary(slm1) # species, height, elev
slm1$anova

summary(lm2 <- lm(age ~ ., data = canol.nat.sub.4[-c(1,4,8:12)]))
slm2 <- step(lm2)
summary(slm2) # species, DBH, height, elev
slm2$anova


# Need to fill missing age values

## Disturbance
summary(lm(age ~ elev + species + height_tot_cm, canol.dis.sub.4))
# Construct linear model based on non-NA pairs
canol.dis.sub.5 <- canol.dis.sub.4 %>% filter(!is.na(age))
canol.dis.sub.5.fit <- lm(age ~ elev + species + height_tot_cm, data = canol.dis.sub.5)
summary(canol.dis.sub.5.fit) # R2 = 0.1244
canol.dis.sub.6 <- canol.dis.sub.4 %>% 
  mutate(pred = predict(canol.dis.sub.5.fit,.)) %>%
  # Replace NA with pred in var1
  mutate(age = ifelse(is.na(age), pred, age))
canol.dis.sub.6 %>% as.data.frame()

par(mar = c(2,2,1,1))
plot(pred ~ age, canol.dis.sub.6, pch = 16, col = alpha("blue",0.5))
pairs.panels(canol.dis.sub.6, scale = T)

## Natural
summary(lm(age ~ elev + species + DBH_cm + height_tot_cm, canol.nat.sub.4))
# Construct linear model based on non-NA pairs
canol.nat.sub.5 <- canol.nat.sub.4 %>% filter(!is.na(age))
canol.nat.sub.5.fit <- lm(age ~ elev + species + DRC_cm, data = canol.nat.sub.5)
summary(canol.nat.sub.5.fit) # R2 = 0.4305
canol.nat.sub.6 <- canol.nat.sub.4 %>% 
  mutate(pred = predict(canol.nat.sub.5.fit,.)) %>%
  # Replace NA with pred in var1
  mutate(age = ifelse(is.na(age), pred, age))
canol.nat.sub.6 %>% as.data.frame()

par(mar = c(2,2,1,1))
plot(pred ~ age, canol.nat.sub.6, pch = 16, col = alpha("blue",0.5))
pairs.panels(canol.nat.sub.6, scale = T)

# canol.dis.sub.6$first_year2 <- round(canol.dis.sub.6$last_year - canol.dis.sub.6$age,0)
# canol.nat.sub.6$first_year2 <- round(canol.nat.sub.6$last_year - canol.nat.sub.6$age,0)
canol.nat.sub.6 <- filter(canol.nat.sub.6, age != 0)

plot(latitude_deg ~ longitude_deg, canol.dis.sub.6, pch = 16, col = as.factor(canol.dis.sub.6$species), cex = canol.dis.sub.6$age*0.02, xlim = range(canol.tree$longitude_deg), ylim = range(canol.tree$latitude_deg))
points(latitude_deg ~ longitude_deg, canol.nat.sub.6, pch = 17, col = as.factor(canol.nat.sub.6$species), cex = canol.nat.sub.6$age*0.01)


d.dis <- density(canol.dis.sub.6$age, na.rm = T)
plot(d.dis)
d.nat <- density(canol.nat.sub.6$age, na.rm = T)
plot(d.nat)
d.nat$x[d.nat$x < 0] <- 0

## Age distribution in natural and disturbed areas
## Add species specific to supp??
# 4.2 x 3.5
par(mar = c(3.5,3.5,1,1))
plot(d.dis, axes = F, ylab = "", xlab = "", col = alpha("#98823c",0.2), lwd = 2, xlim = c(0,350), ylim = c(0.0025,0.06), main = "")
polygon(d.dis, col = alpha("#98823c",0.5), lty = 0)
lines(d.nat, col = alpha("#9a5ea1",0.2))
polygon(d.nat, col = alpha("#9a5ea1",0.5), lty = 0)
axis(1); axis(2)
mtext("Age", side = 1, line = 2.2)
mtext("Density", side = 2, line = 2.2)
abline(v = mean(canol.dis.sub.6$age, na.rm = T), col = "#98823c", lty = 2)
abline(v = mean(canol.nat.sub.6$age, na.rm = T), col = "#9a5ea1", lty = 2)
box()
legend("topright", legend = c("Natural","Disturbed"), fill = alpha(c("#9a5ea1","#98823c"),0.5),
       bty = "n", border = "white", inset = c(0.1,0))

canol.nat.al <- subset(canol.nat.sub.6, species == "Al")
canol.nat.pb <- subset(canol.nat.sub.6, species == "Pb")
canol.nat.pg <- subset(canol.nat.sub.6, species == "Pg")
canol.nat.al$elev <- jitter(canol.nat.al$elev, factor = 1)
canol.nat.pb$elev <- jitter(canol.nat.pb$elev, factor = 0.2)
canol.nat.pg$elev <- jitter(canol.nat.pg$elev, factor = 1)

canol.dis.al <- subset(canol.dis.sub.6, species == "Al")
canol.dis.pb <- subset(canol.dis.sub.6, species == "Pb")
canol.dis.pg <- subset(canol.dis.sub.6, species == "Pg")
canol.dis.pm <- subset(canol.dis.sub.6, species == "Pm")
canol.dis.al$elev <- jitter(canol.dis.al$elev, factor = 1)
canol.dis.pb$elev <- jitter(canol.dis.pb$elev, factor = 1)
canol.dis.pg$elev <- jitter(canol.dis.pg$elev, factor = 1)

canol.nat.al.std <- canol.nat.al
canol.nat.pb.std <- canol.nat.pb
canol.nat.pg.std <- canol.nat.pg
canol.dis.al.std <- canol.dis.al
canol.dis.pb.std <- canol.dis.pb
canol.dis.pg.std <- canol.dis.pg
canol.nat.al.std$age <- c(decostand(canol.nat.al.std$age, "standardize"))
canol.nat.pb.std$age <- c(decostand(canol.nat.pb.std$age, "standardize"))
canol.nat.pg.std$age <- c(decostand(canol.nat.pg.std$age, "standardize"))
canol.dis.al.std$age <- c(decostand(canol.dis.al.std$age, "standardize"))
canol.dis.pb.std$age <- c(decostand(canol.dis.pb.std$age, "standardize", na.rm = T))
canol.dis.pg.std$age <- c(decostand(canol.dis.pg.std$age, "standardize"))


nat.al.mod <- lm(elev ~ age, canol.nat.al)
nat.pb.mod <- lm(elev ~ age, canol.nat.pb)
nat.pg.mod <- lm(elev ~ age, canol.nat.pg)
dis.al.mod <- lm(elev ~ age, canol.dis.al)
dis.pb.mod <- lm(elev ~ age, canol.dis.pb)
dis.pg.mod <- lm(elev ~ age, canol.dis.pg)

nat.al.mod1 <- lm(elev ~ age, canol.nat.al.std)
nat.pb.mod1 <- lm(elev ~ age, canol.nat.pb.std)
nat.pg.mod1 <- lm(elev ~ age, canol.nat.pg.std)
dis.al.mod1 <- lm(elev ~ age, canol.dis.al.std)
dis.pb.mod1 <- lm(elev ~ age, canol.dis.pb.std)
dis.pg.mod1 <- lm(elev ~ age, canol.dis.pg.std)

summary(nat.al.mod)
summary(nat.pb.mod)
summary(nat.pg.mod)
summary(dis.al.mod)
summary(dis.pb.mod)
summary(dis.pg.mod)
summary(nat.al.mod1)
summary(nat.pb.mod1)
summary(nat.pg.mod1)
summary(dis.al.mod1)
summary(dis.pb.mod1)
summary(dis.pg.mod1)
regress.df <- cbind.data.frame(habitat = rep(c("natural","disturbed"), each = 3),
                               species = rep(c("Al","Pg","Pb"), 2),
                               coef = c(summary(nat.al.mod1)[[4]][2],
                                        summary(nat.pg.mod1)[[4]][2],
                                        summary(nat.pb.mod1)[[4]][2],
                                        summary(dis.al.mod1)[[4]][2],
                                        summary(dis.pg.mod1)[[4]][2],
                                        summary(dis.pb.mod1)[[4]][2]),
                               se = c(summary(nat.al.mod1)[[4]][4],
                                      summary(nat.pg.mod1)[[4]][4],
                                      summary(nat.pb.mod1)[[4]][4],
                                      summary(dis.al.mod1)[[4]][4],
                                      summary(dis.pg.mod1)[[4]][4],
                                      summary(dis.pb.mod1)[[4]][4]))
regress.df$ci <- qnorm(0.975)*regress.df$se
regress.df$ci5 <- regress.df$coef - regress.df$ci
regress.df$ci95 <- regress.df$coef + regress.df$ci

par(mar = c(2.5,3.5,1,1))
plot(coef ~ c(1.2,3.2,5.2), regress.df[regress.df$habitat == "natural",], 
     xlim = c(0.8,6.2), ylim = c(-52,52), pch = 16, 
     col = c("#921200","#9a6ce4","#cade69"),
     axes = F, xlab = "", ylab = "")
points(coef ~ c(1.8,3.8,5.8), regress.df[regress.df$habitat == "disturbed",], 
       col = c("#921200","#9a6ce4","#cade69"))
arrows(c(1.2,3.2,5.2), regress.df[regress.df$habitat == "natural","coef"], 
       c(1.2,3.2,5.2), regress.df[regress.df$habitat == "natural","ci5"],
       length = 0.0, angle = 90, col = c("#921200","#9a6ce4","#cade69"))
arrows(c(1.2,3.2,5.2), regress.df[regress.df$habitat == "natural","coef"], 
       c(1.2,3.2,5.2), regress.df[regress.df$habitat == "natural","ci95"],
       length = 0.0, angle = 90, col = c("#921200","#9a6ce4","#cade69"))
arrows(c(1.8,3.8,5.8), regress.df[regress.df$habitat == "disturbed","coef"], 
       c(1.8,3.8,5.8), regress.df[regress.df$habitat == "disturbed","ci5"],
       length = 0.0, angle = 90, col = c("#921200","#9a6ce4","#cade69"))
arrows(c(1.8,3.8,5.8), regress.df[regress.df$habitat == "disturbed","coef"], 
       c(1.8,3.8,5.8), regress.df[regress.df$habitat == "disturbed","ci95"],
       length = 0.0, angle = 90, col = c("#921200","#9a6ce4","#cade69"))
abline(h = 0, lty = 2)
box()
axis(1, at = c(1.5,3.5,5.5), labels = c(expression(italic("Abies")),
                                        expression(italic("Picea")),
                                        expression(italic("Populus"))))
axis(2, seq(-50,50,25))
mtext("Standardized coefficient estimate", side = 2, line = 2.2)
legend("topright", legend = c("Natural","Disturbed"), pch = c(16,1), lty = 1,
       bty = "n")

new.nat.al.dat <- seq(min(canol.nat.al$age), max(canol.nat.al$age), length.out = nrow(canol.nat.al))
new.nat.pb.dat <- seq(min(canol.nat.pb$age), max(canol.nat.pb$age), length.out = nrow(canol.nat.pb))
new.nat.pg.dat <- seq(min(canol.nat.pg$age), max(canol.nat.pg$age), length.out = nrow(canol.nat.pg))

new.dis.al.dat <- seq(min(canol.dis.al$age), max(canol.dis.al$age), length.out = nrow(canol.dis.al))
new.dis.pb.dat <- seq(min(canol.dis.pb$age, na.rm = T), max(canol.dis.pb$age, na.rm = T), length.out = nrow(canol.dis.pb))
new.dis.pg.dat <- seq(min(canol.dis.pg$age), max(canol.dis.pg$age), length.out = nrow(canol.dis.pg))

nat.al.pred <- predict(nat.al.mod, data.frame(age = new.nat.al.dat), interval="confidence")
nat.pb.pred <- predict(nat.pb.mod, data.frame(age = new.nat.pb.dat), interval="confidence")
nat.pg.pred <- predict(nat.pg.mod, data.frame(age = new.nat.pg.dat), interval="confidence")

dis.al.pred <- predict(dis.al.mod, data.frame(age = new.dis.al.dat), interval="confidence")
dis.pb.pred <- predict(dis.pb.mod, data.frame(age = new.dis.pb.dat), interval="confidence")
dis.pg.pred <- predict(dis.pg.mod, data.frame(age = new.dis.pg.dat), interval="confidence")


####
# 7 x 3.5
par(mfrow = c(1,2), mar = c(2,1.5,1,1), oma = c(1.2,1.6,0.7,0), xpd = T)

plot(elev ~ age, canol.nat.al, pch = 16, col = alpha("#921200", 0.5), 
     xlim = c(10,300), ylim = c(1220,1650), xlab = "", ylab = "", axes = F)
points(elev ~ age, canol.nat.pb, pch = 16, col = alpha("#cade69", 0.7))
points(elev ~ age, canol.nat.pg, pch = 16, col = alpha("#9a6ce4", 0.5))
legend("topleft", legend = "(a) Natural", bty = "n", inset = c(-0.15,0))
## Regression CI and fitted line

polygon(c(seq(min(new.nat.al.dat), max(new.nat.al.dat), length.out = nrow(nat.al.pred)),
          seq(max(new.nat.al.dat), min(new.nat.al.dat), length.out = nrow(nat.al.pred))), 
        c(nat.al.pred[, 2], rev(nat.al.pred[, 3])),
        col = alpha("#921200", 0.4), border = NA)
lines(c(seq(min(new.nat.al.dat), max(new.nat.al.dat), length.out = nrow(nat.al.pred))), 
      nat.al.pred[, 1], lwd = 2, col = "#921200")

polygon(c(seq(min(new.nat.pb.dat), max(new.nat.pb.dat), length.out = nrow(nat.pb.pred)),
          seq(max(new.nat.pb.dat), min(new.nat.pb.dat), length.out = nrow(nat.pb.pred))), 
        c(nat.pb.pred[, 2], rev(nat.pb.pred[, 3])),
        col = alpha("#cade69", 0.7), border = NA)
lines(c(seq(min(new.nat.pb.dat), max(new.nat.pb.dat), length.out = nrow(nat.pb.pred))), 
      nat.pb.pred[, 1], lwd = 2, col = "#cade69")

polygon(c(seq(min(new.nat.pg.dat), max(new.nat.pg.dat), length.out = nrow(nat.pg.pred)),
          seq(max(new.nat.pg.dat), min(new.nat.pg.dat), length.out = nrow(nat.pg.pred))), 
        c(nat.pg.pred[, 2], rev(nat.pg.pred[, 3])),
        col = alpha("#9a6ce4", 0.4), border = NA)
lines(c(seq(min(new.nat.pg.dat), max(new.nat.pg.dat), length.out = nrow(nat.pg.pred))), 
      nat.pg.pred[, 1], lwd = 2, col = "#9a6ce4")

axis(1); axis(2)
box()

plot(elev ~ age, canol.dis.al, pch = 16, col = alpha("#921200", 0.5), 
     xlim = c(10,85), ylim = c(1220,1650), xlab = "", ylab = "", axes = F)
points(elev ~ age, canol.dis.pb, pch = 16, col = alpha("#cade69", 0.7))
points(elev ~ age, canol.dis.pg, pch = 16, col = alpha("#9a6ce4", 0.5))
points(elev ~ age, canol.dis.pm, pch = 16, col = alpha("#350b64", 0.5))
legend("topleft", legend = "(b) Disturbed", bty = "n", inset = c(-0.15,0))
## Regression CI and fitted line

polygon(c(seq(min(new.dis.al.dat), max(new.dis.al.dat), length.out = nrow(dis.al.pred)),
          seq(max(new.dis.al.dat), min(new.dis.al.dat), length.out = nrow(dis.al.pred))), 
        c(dis.al.pred[, 2], rev(dis.al.pred[, 3])),
        col = alpha("#921200", 0.4), border = NA)
lines(c(seq(min(new.dis.al.dat), max(new.dis.al.dat), length.out = nrow(dis.al.pred))), 
      dis.al.pred[, 1], lwd = 2, col = "#921200")

polygon(c(seq(min(new.dis.pb.dat), max(new.dis.pb.dat), length.out = nrow(dis.pb.pred)),
          seq(max(new.dis.pb.dat), min(new.dis.pb.dat), length.out = nrow(dis.pb.pred))), 
        c(dis.pb.pred[, 2], rev(dis.pb.pred[, 3])),
        col = alpha("#cade69", 0.7), border = NA)
lines(c(seq(min(new.dis.pb.dat), max(new.dis.pb.dat), length.out = nrow(dis.pb.pred))), 
      dis.pb.pred[, 1], lwd = 2, col = "#cade69")

polygon(c(seq(min(new.dis.pg.dat), max(new.dis.pg.dat), length.out = nrow(dis.pg.pred)),
          seq(max(new.dis.pg.dat), min(new.dis.pg.dat), length.out = nrow(dis.pg.pred))), 
        c(dis.pg.pred[, 2], rev(dis.pg.pred[, 3])),
        col = alpha("#9a6ce4", 0.4), border = NA)
lines(c(seq(min(new.dis.pg.dat), max(new.dis.pg.dat), length.out = nrow(dis.pg.pred))), 
      dis.pg.pred[, 1], lwd = 2, col = "#9a6ce4")
axis(1); axis(2)
box()
mtext("Age (years)", side = 1, line = 0, cex = 1, outer = T)
mtext("Elevation (m.a.s.l.)", side = 2, line = 0.6, cex = 1, outer = T)
par(xpd = NA)
legend(-90,1730, legend = c(expression(italic("Abies")),
                             expression(italic("Populus")),
                             expression(italic("Picea"))),
       col = alpha(c("#921200","#cade69","#9a6ce4","#350b64"),0.7), 
       pch = 16, lty = 1, bty = "n", horiz = T, x.intersp = 0.7, text.width = c(10,20,26))


can.trees <- read.csv("~/Dropbox/School/School/Postdoc/Earthwatch/Mackenzie Mountains/Canol_Colonization/canol_trees_20200720_utm2.csv")
can.trees$species[can.trees$ID %in% seq(1476,1479,1)] <- "Pb"
can.trees$east2 <- can.trees$easting - 443585
can.trees$north2 <- can.trees$northing - 7013181
range(can.trees$easting - 443585)     # Should be all positive
range(can.trees$northing - 7013181)
can.trees$border.dist <- sqrt(can.trees$east2^2 + can.trees$north2^2)
can.trees$xy <- paste(can.trees$easting, can.trees$northing, can.trees$Reprojecte, sep = ".")
can.trees.nat <- subset(can.trees, group == "natural")
can.trees.dis <- subset(can.trees, group == "disturbed")

can.trees.nat.sub <- can.trees.nat[can.trees.nat$species %in% c("Al","Pg","Pb"),]
tmp.crap <- sample_n(can.trees.nat[is.na(can.trees.nat$DBH_cm),], (500 - nrow(can.trees.nat.sub)))
can.trees.nat.sub <- rbind.data.frame(can.trees.nat.sub, tmp.crap)

####**************

# Things to address in models:
# 1. Spatial dependency
# 2. Zero-inflation

#### Models for disturbed areas
can.trees.dis.sum <- can.trees.dis %>% 
  group_by(xy) %>% 
  add_count(species) %>%
  sample_n(1)
can.trees.dis.sum <- as.data.frame(can.trees.dis.sum)
can.trees.dis.sum$n <- can.trees.dis.sum$n - 1
can.trees.dis.sum <- can.trees.dis.sum[can.trees.dis.sum$n != 41,] ##########
can.trees.dis.sum$pa <- c(decostand(can.trees.dis.sum$n, "pa"))
can.trees.dis.sum$elev2 <- c(decostand(can.trees.dis.sum$Reprojecte, "standardize"))
can.trees.dis.sum$dist <- c(decostand(can.trees.dis.sum$dist, "standardize"))
can.trees.dis.sum$bord.dist <- c(decostand(can.trees.dis.sum$border.dist, "standardize"))
can.trees.dis.sum$slope <- c(decostand(can.trees.dis.sum$canol_slop, "standardize"))
can.trees.dis.sum$eastness <- c(decostand(can.trees.dis.sum$eastness1, "standardize"))
can.trees.dis.sum$northness <- c(decostand(can.trees.dis.sum$northness1, "standardize"))

# Check for best auto-correlation structure
pairs.panels(can.trees.dis.sum[,c(25,27:31)], scale = T) # elev2 is correlated with distance to border

dis.m2 <- gls(log1p(n) ~ 1, data = can.trees.dis.sum)
dis.vario2 <- Variogram(dis.m2, form = ~easting + northing, resType = "pearson")
plot(dis.vario2, smooth = TRUE, ylim = c(0.4, 1.3))

dis.m3 <- gls(log1p(n) ~ 1, correlation = corExp(form = ~easting + northing, nugget = TRUE), data = can.trees.dis.sum)
dis.m4 <- gls(log1p(n) ~ 1, correlation = corGaus(form = ~easting + northing, nugget = TRUE), data = can.trees.dis.sum)
dis.m5 <- gls(log1p(n) ~ 1, correlation = corSpher(form = ~easting + northing, nugget = TRUE), data = can.trees.dis.sum)
dis.m6 <- gls(log1p(n) ~ 1, correlation = corLin(form = ~easting + northing, nugget = TRUE), data = can.trees.dis.sum)

AIC(dis.m2, dis.m3, dis.m4, dis.m5, dis.m6)
# Model 3 is the best
dis.vario3 <- Variogram(dis.m3, form = ~easting + northing, resType = "pearson")
plot(dis.vario3, smooth = FALSE, ylim = c(0.4, 1.3))

dis.vario4 <- Variogram(dis.m3, form = ~easting + northing, resType = "normalized", maxDist = 10000)
plot(dis.vario4, smooth = FALSE, ylim = c(0.4, 1.3))

dis.resid <- exp(residuals(dis.m3))-1
dis.resid <- dis.resid + -(min(dis.resid))
can.trees.dis.sum$resid <- dis.resid[1:length(dis.resid)]
can.trees.dis.sum$resid <- round(can.trees.dis.sum$resid, digits = 0)
range(can.trees.dis.sum$resid)
range(can.trees.dis.sum$n)
plot(can.trees.dis.sum$resid)
plot(can.trees.dis.sum$resid, can.trees.dis.sum$n)

####**************
#### Models for natural areas
can.trees.nat.sum <- can.trees.nat.sub %>% 
  group_by(xy) %>% 
  add_count(species) %>%
  sample_n(1)
can.trees.nat.sum <- as.data.frame(can.trees.nat.sum)
can.trees.nat.sum$n <- can.trees.nat.sum$n - 1
can.trees.nat.sum$pa <- c(decostand(can.trees.nat.sum$n, "pa"))
can.trees.nat.sum$elev2 <- c(decostand(can.trees.nat.sum$Reprojecte, "standardize"))
can.trees.nat.sum$dist <- c(decostand(can.trees.nat.sum$dist, "standardize"))
can.trees.nat.sum$bord.dist <- c(decostand(can.trees.nat.sum$border.dist, "standardize"))
can.trees.nat.sum$slope <- c(decostand(can.trees.nat.sum$canol_slop, "standardize"))
can.trees.nat.sum$eastness <- c(decostand(can.trees.nat.sum$eastness1, "standardize"))
can.trees.nat.sum$northness <- c(decostand(can.trees.nat.sum$northness1, "standardize"))

# Check for best auto-correlation structure
pairs.panels(can.trees.nat.sum[,c(25,27:31)], scale = T) # elev2 is correlated with distance to border

nat.m2 <- gls(log1p(n) ~ 1, data = can.trees.nat.sum)
nat.vario2 <- Variogram(nat.m2, form = ~easting + northing, resType = "pearson")
plot(nat.vario2, smooth = TRUE, ylim = c(0.4, 1.3))

nat.m3 <- gls(log1p(n) ~ 1, correlation = corExp(form = ~easting + northing, nugget = TRUE), data = can.trees.nat.sum)
nat.m4 <- gls(log1p(n) ~ 1, correlation = corGaus(form = ~easting + northing, nugget = TRUE), data = can.trees.nat.sum)
nat.m5 <- gls(log1p(n) ~ 1, correlation = corSpher(form = ~easting + northing, nugget = TRUE), data = can.trees.nat.sum)
nat.m6 <- gls(log1p(n) ~ 1, correlation = corLin(form = ~easting + northing, nugget = TRUE), data = can.trees.nat.sum)

AIC(nat.m2, nat.m3, nat.m4, nat.m5, nat.m6)
# Model 3 is the best
nat.vario3 <- Variogram(nat.m3, form = ~easting + northing, resType = "pearson")
plot(nat.vario3, smooth = FALSE, ylim = c(0.4, 1.3))

nat.vario4 <- Variogram(nat.m3, form = ~easting + northing, resType = "normalized", maxDist = 10000)
plot(nat.vario4, smooth = FALSE, ylim = c(0.4, 1.3))

nat.resid <- exp(residuals(nat.m3))-1
nat.resid <- nat.resid + -(min(nat.resid))
can.trees.nat.sum$resid <- nat.resid[1:length(nat.resid)]
can.trees.nat.sum$resid <- round(can.trees.nat.sum$resid, digits = 0)
range(can.trees.nat.sum$resid)
range(can.trees.nat.sum$n)
plot(can.trees.nat.sum$resid)
plot(can.trees.nat.sum$resid, can.trees.nat.sum$n)

####************

## Check for zero-inflation
library(performance)
m <- glm(n ~ dist + elev2 + slope + northness + eastness, family = poisson, data = can.trees.dis.sum)
check_zeroinflation(m)

m <- glm(n ~ dist + elev2 + slope + northness + eastness, family = poisson, data = can.trees.nat.sum)
check_zeroinflation(m)

## Now for the zero-inflated modelling
library(pscl)
library(lmtest)

## Natural areas
f1 <- formula(resid ~ dist + elev2 + slope + northness + eastness)
nat.Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = can.trees.nat.sum)
nat.Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = can.trees.nat.sum)
lrtest(nat.Zip1,nat.Nb1)

nat.Zap1 <- hurdle(f1, dist = "poisson", link = "logit", data = can.trees.nat.sum)
nat.Zanb1 <- hurdle(f1, dist = "negbin", link = "logit", data = can.trees.nat.sum)
lrtest(nat.Zap1,nat.Zanb1)

AIC(nat.Zip1, nat.Nb1, nat.Zap1, nat.Zanb1)
# nat.Zip1 is best based on AIC
summary(nat.Zip1)

f1A <- formula(resid ~ elev2 * dist + slope + northness + eastness | elev2 * dist + slope + northness + eastness)
f1B <- formula(resid ~ elev2 + dist + slope * northness + eastness | elev2 * dist + slope + northness + eastness)
f1C <- formula(resid ~ elev2 + dist + slope * eastness + northness | elev2 * dist + slope + northness + eastness)
f1D <- formula(resid ~ dist + slope + northness + eastness | elev2 * dist + slope + northness + eastness)
f1E <- formula(resid ~ elev2 + slope + eastness + northness | elev2 * dist + slope + northness + eastness)
f1F <- formula(resid ~ elev2 + dist + eastness + northness | elev2 * dist + slope + northness + eastness)
f1G <- formula(resid ~ elev2 + dist + slope + northness | elev2 * dist + slope + northness + eastness)
f1H <- formula(resid ~ elev2 + dist + slope + eastness | elev2 * dist + slope + northness + eastness)
f1I <- formula(resid ~ elev2 + dist + slope + northness + eastness | elev2 + dist + slope * northness + eastness)
f1J <- formula(resid ~ elev2 + dist + slope + northness + eastness | elev2 + dist + slope * eastness + northness)
f1K <- formula(resid ~ elev2 + dist + slope + northness + eastness | dist + slope + northness + eastness)
f1L <- formula(resid ~ elev2 + dist + slope + northness + eastness | elev2 + slope + eastness + northness)
f1M <- formula(resid ~ elev2 + dist + slope + northness + eastness | elev2 + dist + eastness + northness)
f1N <- formula(resid ~ elev2 + dist + slope + northness + eastness | elev2 + dist + slope + northness)
f1O <- formula(resid ~ elev2 + dist + slope + northness + eastness | elev2 + dist + slope + eastness)

nat.Zip1A <- zeroinfl(f1A, dist = "poisson", link = "logit", data = can.trees.nat.sum)
nat.Zip1B <- zeroinfl(f1B, dist = "poisson", link = "logit", data = can.trees.nat.sum)
nat.Zip1C <- zeroinfl(f1C, dist = "poisson", link = "logit", data = can.trees.nat.sum)
nat.Zip1D <- zeroinfl(f1D, dist = "poisson", link = "logit", data = can.trees.nat.sum)
nat.Zip1E <- zeroinfl(f1E, dist = "poisson", link = "logit", data = can.trees.nat.sum)
nat.Zip1F <- zeroinfl(f1F, dist = "poisson", link = "logit", data = can.trees.nat.sum)
nat.Zip1G <- zeroinfl(f1G, dist = "poisson", link = "logit", data = can.trees.nat.sum)
nat.Zip1H <- zeroinfl(f1H, dist = "poisson", link = "logit", data = can.trees.nat.sum)
nat.Zip1I <- zeroinfl(f1I, dist = "poisson", link = "logit", data = can.trees.nat.sum)
nat.Zip1J <- zeroinfl(f1J, dist = "poisson", link = "logit", data = can.trees.nat.sum)
nat.Zip1K <- zeroinfl(f1K, dist = "poisson", link = "logit", data = can.trees.nat.sum)
nat.Zip1L <- zeroinfl(f1L, dist = "poisson", link = "logit", data = can.trees.nat.sum)
nat.Zip1M <- zeroinfl(f1M, dist = "poisson", link = "logit", data = can.trees.nat.sum)
nat.Zip1N <- zeroinfl(f1N, dist = "poisson", link = "logit", data = can.trees.nat.sum)
nat.Zip1O <- zeroinfl(f1O, dist = "poisson", link = "logit", data = can.trees.nat.sum)

lrtest(nat.Zip1A,nat.Zip1B) # Drop the interaction between elev and dist
lrtest(nat.Zip1A,nat.Zip1C) # Drop the interaction between northness and slope
lrtest(nat.Zip1A,nat.Zip1D) # 
lrtest(nat.Zip1A,nat.Zip1E) # 
lrtest(nat.Zip1A,nat.Zip1F) # 
lrtest(nat.Zip1A,nat.Zip1G) # 
lrtest(nat.Zip1A,nat.Zip1H) # Drop northness
lrtest(nat.Zip1A,nat.Zip1I) # 
lrtest(nat.Zip1A,nat.Zip1J) # 
lrtest(nat.Zip1A,nat.Zip1K) # 
lrtest(nat.Zip1A,nat.Zip1L) # 
lrtest(nat.Zip1A,nat.Zip1M) # 
lrtest(nat.Zip1A,nat.Zip1N) # 
lrtest(nat.Zip1A,nat.Zip1O) # 

f1P <- formula(resid ~ dist + slope + eastness | elev2 * dist + slope + northness + eastness)
nat.Zip1P <- zeroinfl(f1P, dist = "poisson", link = "logit", data = can.trees.nat.sum)
f1Q <- formula(resid ~ elev2 + northness | elev2)
nat.Zip1Q <- zeroinfl(f1Q, dist = "poisson", link = "logit", data = can.trees.nat.sum)

AIC(nat.Zip1A,nat.Zip1B,nat.Zip1C,nat.Zip1D,nat.Zip1E,nat.Zip1F,nat.Zip1G,
    nat.Zip1H,nat.Zip1I,nat.Zip1J,nat.Zip1K,nat.Zip1L,nat.Zip1M,nat.Zip1N,nat.Zip1O,
    nat.Zip1P, nat.Zip1Q)
# Model N is the best based on AIC

summary(nat.Zip1A)
summary(nat.Zip1B)
summary(nat.Zip1C)
summary(nat.Zip1D)
summary(nat.Zip1E)
summary(nat.Zip1F)
summary(nat.Zip1G)
summary(nat.Zip1H)
summary(nat.Zip1I)
summary(nat.Zip1J)
summary(nat.Zip1K)
summary(nat.Zip1L)
summary(nat.Zip1M)
summary(nat.Zip1N)
summary(nat.Zip1O)
summary(nat.Zip1P)
summary(nat.Zip1Q)

## Disturbed areas
f1 <- formula(resid ~ dist + elev2 + slope + northness + eastness)
dis.Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = can.trees.dis.sum)
dis.Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = can.trees.dis.sum)
lrtest(dis.Zip1,dis.Nb1)

dis.Zap1 <- hurdle(f1, dist = "poisson", link = "logit", data = can.trees.dis.sum)
dis.Zanb1 <- hurdle(f1, dist = "negbin", link = "logit", data = can.trees.dis.sum)
lrtest(dis.Zap1,dis.Zanb1)

AIC(dis.Zip1, dis.Nb1, dis.Zap1, dis.Zanb1)
# dis.Zip1 is best based on AIC
summary(dis.Zip1)

f1A <- formula(resid ~ elev2 * dist + slope + northness + eastness | elev2 * dist + slope + northness + eastness)
f1B <- formula(resid ~ elev2 + dist + slope * northness + eastness | elev2 * dist + slope + northness + eastness)
f1C <- formula(resid ~ elev2 + dist + slope * eastness + northness | elev2 * dist + slope + northness + eastness)
f1D <- formula(resid ~ dist + slope + northness + eastness | elev2 * dist + slope + northness + eastness)
f1E <- formula(resid ~ elev2 + slope + eastness + northness | elev2 * dist + slope + northness + eastness)
f1F <- formula(resid ~ elev2 + dist + eastness + northness | elev2 * dist + slope + northness + eastness)
f1G <- formula(resid ~ elev2 + dist + slope + northness | elev2 * dist + slope + northness + eastness)
f1H <- formula(resid ~ elev2 + dist + slope + eastness | elev2 * dist + slope + northness + eastness)
f1I <- formula(resid ~ elev2 + dist + slope + northness + eastness | elev2 + dist + slope * northness + eastness)
f1J <- formula(resid ~ elev2 + dist + slope + northness + eastness | elev2 + dist + slope * eastness + northness)
f1K <- formula(resid ~ elev2 + dist + slope + northness + eastness | dist + slope + northness + eastness)
f1L <- formula(resid ~ elev2 + dist + slope + northness + eastness | elev2 + slope + eastness + northness)
f1M <- formula(resid ~ elev2 + dist + slope + northness + eastness | elev2 + dist + eastness + northness)
f1N <- formula(resid ~ elev2 + dist + slope + northness + eastness | elev2 + dist + slope + northness)
f1O <- formula(resid ~ elev2 + dist + slope + northness + eastness | elev2 + dist + slope + eastness)

dis.Zip1A <- zeroinfl(f1A, dist = "poisson", link = "logit", data = can.trees.dis.sum)
dis.Zip1B <- zeroinfl(f1B, dist = "poisson", link = "logit", data = can.trees.dis.sum)
dis.Zip1C <- zeroinfl(f1C, dist = "poisson", link = "logit", data = can.trees.dis.sum)
dis.Zip1D <- zeroinfl(f1D, dist = "poisson", link = "logit", data = can.trees.dis.sum)
dis.Zip1E <- zeroinfl(f1E, dist = "poisson", link = "logit", data = can.trees.dis.sum)
dis.Zip1F <- zeroinfl(f1F, dist = "poisson", link = "logit", data = can.trees.dis.sum)
dis.Zip1G <- zeroinfl(f1G, dist = "poisson", link = "logit", data = can.trees.dis.sum)
dis.Zip1H <- zeroinfl(f1H, dist = "poisson", link = "logit", data = can.trees.dis.sum)
dis.Zip1I <- zeroinfl(f1I, dist = "poisson", link = "logit", data = can.trees.dis.sum)
dis.Zip1J <- zeroinfl(f1J, dist = "poisson", link = "logit", data = can.trees.dis.sum)
dis.Zip1K <- zeroinfl(f1K, dist = "poisson", link = "logit", data = can.trees.dis.sum)
dis.Zip1L <- zeroinfl(f1L, dist = "poisson", link = "logit", data = can.trees.dis.sum)
dis.Zip1M <- zeroinfl(f1M, dist = "poisson", link = "logit", data = can.trees.dis.sum)
dis.Zip1N <- zeroinfl(f1N, dist = "poisson", link = "logit", data = can.trees.dis.sum)
dis.Zip1O <- zeroinfl(f1O, dist = "poisson", link = "logit", data = can.trees.dis.sum)

lrtest(dis.Zip1A,dis.Zip1B) # Drop the interaction between elev and dist
lrtest(dis.Zip1A,dis.Zip1C) # Drop the interaction between northness and slope
lrtest(dis.Zip1A,dis.Zip1D) # Drop elev
lrtest(dis.Zip1A,dis.Zip1E) # 
lrtest(dis.Zip1A,dis.Zip1F) # 
lrtest(dis.Zip1A,dis.Zip1G) # 
lrtest(dis.Zip1A,dis.Zip1H) #
lrtest(dis.Zip1A,dis.Zip1I) # 
lrtest(dis.Zip1A,dis.Zip1J) # Drop interaction between elev and dist for zeroes
lrtest(dis.Zip1A,dis.Zip1K) # Drop the interaction between northness and slope for zeroes
lrtest(dis.Zip1A,dis.Zip1L) # Drop elev
lrtest(dis.Zip1A,dis.Zip1M) # 
lrtest(dis.Zip1A,dis.Zip1N) # 
lrtest(dis.Zip1A,dis.Zip1O) # Drop northness from zero model

f1P <- formula(resid ~ dist + slope + northness + eastness | dist + slope + eastness)
dis.Zip1P <- zeroinfl(f1P, dist = "poisson", link = "logit", data = can.trees.dis.sum)
f1Q <- formula(resid ~ elev2 + slope | elev2 + dist + northness)
dis.Zip1Q <- zeroinfl(f1Q, dist = "poisson", link = "logit", data = can.trees.dis.sum)

AIC(dis.Zip1A,dis.Zip1B,dis.Zip1C,dis.Zip1D,dis.Zip1E,dis.Zip1F,dis.Zip1G,
    dis.Zip1H,dis.Zip1I,dis.Zip1J,dis.Zip1K,dis.Zip1L,dis.Zip1M,dis.Zip1N,dis.Zip1O,
    dis.Zip1P, dis.Zip1Q)
# Model J is the best based on AIC

summary(dis.Zip1A)
summary(dis.Zip1B)
summary(dis.Zip1C)
summary(dis.Zip1D)
summary(dis.Zip1E)
summary(dis.Zip1F)
summary(dis.Zip1G)
summary(dis.Zip1H)
summary(dis.Zip1I)
summary(dis.Zip1J)
summary(dis.Zip1K)
summary(dis.Zip1L)
summary(dis.Zip1M)
summary(dis.Zip1N)
summary(dis.Zip1O)
summary(dis.Zip1P)
summary(dis.Zip1Q)


dis.EP <- residuals(dis.Zip1Q, type = "pearson")
nat.EP <- residuals(nat.Zip1Q, type = "pearson")

plot(dis.EP, ylim = c(-1,8))
plot(nat.EP, ylim = c(-1,8))

library(parameters)

dis.Zip1.summ <- summary(dis.Zip1Q)
dis.mod.df <- rbind.data.frame(dis.Zip1.summ$coefficients$count[c(1:2),c(1,2)],
                               dis.Zip1.summ$coefficients$zero[c(1:3),c(1,2)])
dis.mod.df <- cbind.data.frame(part = c(rep("count",2),rep("zeroinfl",3)),
                               dis.mod.df)
dis.Zip1.para <- model_parameters(dis.Zip1Q)
dis.mod.df$ci5 <- dis.Zip1.para$CI_low
dis.mod.df$ci95 <- dis.Zip1.para$CI_high

nat.Zip1.summ <- summary(nat.Zip1Q)
nat.mod.df <- rbind.data.frame(nat.Zip1.summ$coefficients$count[c(1:2),c(1,2)],
                               nat.Zip1.summ$coefficients$zero[c(1:3),c(1,2)])
nat.mod.df <- cbind.data.frame(part = c(rep("count",2),rep("zeroinfl",3)),
                               nat.mod.df)
nat.Zip1.para <- model_parameters(nat.Zip1Q)
nat.mod.df$ci5 <- nat.Zip1.para$CI_low
nat.mod.df$ci95 <- nat.Zip1.para$CI_high

par(mar = c(2,1.5,0.3,1), oma = c(1.5,1.6,0.7,0))
plot(Estimate ~ c(1,2,4:6), nat.mod.df, type = "p", pch = rep(c(16,17), each = 3), 
     xlim = c(0.5,13.5), ylim = c(-1,5.5), axes = F, xlab = "", ylab = "", col = "#9a5ea1")
arrows(c(1,2,4:6), nat.mod.df$Estimate, c(1,2,4:6), nat.mod.df$ci5, length = 0, col = "#9a5ea1")
arrows(c(1,2,4:6), nat.mod.df$Estimate, c(1,2,4:6), nat.mod.df$ci95, length = 0, col = "#9a5ea1")
points(dis.mod.df$Estimate ~ c(8,9,11:13), pch = rep(c(16,17), each = 3), col = "#98823c")
arrows(c(8,9,11:13), dis.mod.df$Estimate, c(8,9,11:13), dis.mod.df$ci5, length = 0, col = "#98823c")
arrows(c(8,9,11:13), dis.mod.df$Estimate, c(8,9,11:13), dis.mod.df$ci95, length = 0, col = "#98823c")
abline(h = 0, lty = 3)
text(x = c(1,4), 
     y = c((nat.mod.df$ci95[1]+0.4), (nat.mod.df$ci95[3]+0.4)),
     c("***","***"), col = "#9a5ea1")
text(x = c(8,9,11:13), 
     y = c((dis.mod.df$ci95[1]+0.4), (dis.mod.df$ci95[2]+0.4),
           (dis.mod.df$ci95[3]+0.4), (dis.mod.df$ci95[4]+0.4),
           (dis.mod.df$ci95[5]+0.4)),
     c("***","***","***","***","."), col = "#98823c")
box()
mtext(c("Int","Int","Dist","Int","Int","Dist"), 
      at = c(1,4,6,8,11,13), side = 1, line = 0.45)
mtext(rep("Elev", 4), at = c(2,5,9,12), side = 1, line = 1.4)
mtext(c("Natural","Disturbed"), at = c(4,11.5), side = 1, line = 2.35)
axis(1, at = c(1,3,5,7,9,12,14), labels = F, tick = T, tcl = -0.25)
axis(1, at = c(2,6,10,13), labels = F, tick = T, tcl = -0.55)
axis(2, seq(-2,6,2))
mtext("Standardized coefficient estimate", side = 2, line = 2.2)
legend("top", legend = c("Count","Zero"), pch = c(16,17), lty = 1, bty = "n", cex = .9)

## Look at climate gradients
mm <- read.csv("~/Desktop/Workspace/Earthwatch/mm_microclimate3.csv")

# Converts to date format (lubridate)
mm$Date <- ymd(mm$Date)

# This will add in 'season_year' and 'season' columns 
# 
mm <- mutate(mm,
             season_year = ifelse(month(Date) == 10 | month(Date) == 11 | month(Date) == 12,
                                  year(Date) + 1, year(Date)),
             season = case_when(
               month(Date) %in% c(10,11,12,1,2,3,4,5) ~ "cool",
               month(Date) %in% c(6, 7, 8, 9) ~ "warm",
               T ~ NA_character_
             ))

mm$lapse <- ((mm$gf.150 - mm$hf.150) / 361)*100

# round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

mm.avg <- mm %>%
  group_by(season_year, season) %>%
  summarise(mean = mean(lapse), lapse.sd = sd(lapse), n = length(lapse), 
            hf.air = mean(hf.150), gf.air = mean(gf.150), 
            hf.air.sd = sd(hf.150), gf.air.sd = sd(gf.150),
            hf.soil = mean(c(hf.0,hf.neg2p5,hf.neg.5), na.rm = T), 
            gf.soil = mean(c(gf.0,gf.neg2p5,gf.neg.5), na.rm = T),
            hf.soil.sd = sd(c(hf.0,hf.neg2p5,hf.neg.5), na.rm = T), 
            gf.soil.sd = sd(c(gf.0,gf.neg2p5,gf.neg.5), na.rm = T))
mm.cool <- subset(mm.avg, season == "cool" & season_year != 1990)
mm.warm <- subset(mm.avg, season == "warm" & season_year != 1990 & season_year != 2019)

mm.cool$mean[mm.cool$season_year == 1992] <- 0.423346
mm.cool$hf.air[mm.cool$season_year == 1992] <- -14.4
mm.cool$hf.soil[mm.cool$season_year == 2013] <- -6.506835
mm.cool$gf.air[mm.cool$season_year == 1999] <- -14.1

mm.cool$lapse.ci <- qnorm(0.975)*(mm.cool$lapse.sd / sqrt(mm.cool$n))
mm.cool$lapse.ci5 <- mm.cool$mean - mm.cool$lapse.ci
mm.cool$lapse.ci95 <- mm.cool$mean + mm.cool$lapse.ci
mm.warm$lapse.ci <- qnorm(0.975)*(mm.warm$lapse.sd / sqrt(mm.warm$n))
mm.warm$lapse.ci5 <- mm.warm$mean - mm.warm$lapse.ci
mm.warm$lapse.ci95 <- mm.warm$mean + mm.warm$lapse.ci

mm.cool$gf.air.ci <- qnorm(0.975)*(mm.cool$gf.air.sd / sqrt(mm.cool$n))
mm.cool$gf.air.ci5 <- mm.cool$gf.air - mm.cool$gf.air.ci
mm.cool$gf.air.ci95 <- mm.cool$gf.air + mm.cool$gf.air.ci
mm.warm$gf.air.ci <- qnorm(0.975)*(mm.warm$gf.air.sd / sqrt(mm.warm$n))
mm.warm$gf.air.ci5 <- mm.warm$gf.air - mm.warm$gf.air.ci
mm.warm$gf.air.ci95 <- mm.warm$gf.air + mm.warm$gf.air.ci

mm.cool$hf.air.ci <- qnorm(0.975)*(mm.cool$hf.air.sd / sqrt(mm.cool$n))
mm.cool$hf.air.ci5 <- mm.cool$hf.air - mm.cool$hf.air.ci
mm.cool$hf.air.ci95 <- mm.cool$hf.air + mm.cool$hf.air.ci
mm.warm$hf.air.ci <- qnorm(0.975)*(mm.warm$hf.air.sd / sqrt(mm.warm$n))
mm.warm$hf.air.ci5 <- mm.warm$hf.air - mm.warm$hf.air.ci
mm.warm$hf.air.ci95 <- mm.warm$hf.air + mm.warm$hf.air.ci

mm.cool$gf.soil.ci <- qnorm(0.975)*(mm.cool$gf.soil.sd / sqrt(mm.cool$n))
mm.cool$gf.soil.ci5 <- mm.cool$gf.soil - mm.cool$gf.soil.ci
mm.cool$gf.soil.ci95 <- mm.cool$gf.soil + mm.cool$gf.soil.ci
mm.warm$gf.soil.ci <- qnorm(0.975)*(mm.warm$gf.soil.sd / sqrt(mm.warm$n))
mm.warm$gf.soil.ci5 <- mm.warm$gf.soil - mm.warm$gf.soil.ci
mm.warm$gf.soil.ci95 <- mm.warm$gf.soil + mm.warm$gf.soil.ci

mm.cool$hf.soil.ci <- qnorm(0.975)*(mm.cool$hf.soil.sd / sqrt(mm.cool$n))
mm.cool$hf.soil.ci5 <- mm.cool$hf.soil - mm.cool$hf.soil.ci
mm.cool$hf.soil.ci95 <- mm.cool$hf.soil + mm.cool$hf.soil.ci
mm.warm$hf.soil.ci <- qnorm(0.975)*(mm.warm$hf.soil.sd / sqrt(mm.warm$n))
mm.warm$hf.soil.ci5 <- mm.warm$hf.soil - mm.warm$hf.soil.ci
mm.warm$hf.soil.ci95 <- mm.warm$hf.soil + mm.warm$hf.soil.ci



summary(lm(gf.air ~ season_year, mm.cool)) # P = 0.1706
summary(lm(gf.air ~ season_year, mm.warm)) # P = 0.8153
summary(lm(hf.air ~ season_year, mm.cool)) # P = 0.1490
summary(lm(hf.air ~ season_year, mm.warm)) # P = 0.0747.
summary(lm(gf.soil ~ season_year, mm.cool)) # P = 8.943e-07***
summary(lm(gf.soil ~ season_year, mm.warm)) # P = 0.7472
summary(lm(hf.soil ~ season_year, mm.cool)) # P = 0.0019**
summary(lm(hf.soil ~ season_year, mm.warm)) # P = 0.9981
summary(lm(mean ~ season_year, mm.cool)) # P = 0.1593
summary(lm(mean ~ season_year, mm.warm)) # P = 0.1988


## Plot the climate information
m <- rbind(c(1, 1), 
           c(1, 1),
           c(1, 1),
           c(1, 1),
           c(1, 1),
           c(6, 6),
           c(2, 2),
           c(2, 2),
           c(2, 2),
           c(2, 2),
           c(2, 2),
           c(7, 7),
           c(3, 3),
           c(3, 3),
           c(3, 3),
           c(3, 3),
           c(3, 3),
           c(8, 8),
           c(4, 4),
           c(4, 4),
           c(4, 4),
           c(4, 4),
           c(4, 4),
           c(9, 9),
           c(9, 9),
           c(5, 5),
           c(5, 5),
           c(5, 5),
           c(5, 5),
           c(5, 5),
           c(10, 10))

# 3.5 x 8
layout(m)
par(mar = c(0,0.3,0,0), oma = c(1.5,3,0.7,1))
# Warm air temperatures
plot(gf.air ~ season_year, mm.warm, type = "l", col = alpha("#44047a",1), 
     xlim = c(1990,2020), ylim = c(2,12), 
     lwd = 1.25, axes = F, xlab = "", ylab = "")
polygon(c(mm.warm$season_year, rev(mm.warm$season_year)), 
        c(mm.warm$gf.air.ci5, rev(mm.warm$gf.air.ci95)), col = alpha("#44047a", 0.3), border = NA)
lines(hf.air ~ season_year, mm.warm, col = alpha("#c9c232",1), lwd = 1.25)
polygon(c(mm.warm$season_year, rev(mm.warm$season_year)), 
        c(mm.warm$hf.air.ci5, rev(mm.warm$hf.air.ci95)), col = alpha("#c9c231", 0.4), border = NA)
axis(1, seq(1990,2020,5), labels = F); axis(2, seq(2, 12, 2))
box()
legend("topleft", "(a) J-S air (150 cm)", inset = c(-0.03,-0.1), bty = "n")
legend("topright", c("Low","High"), fill = alpha(c("#c9c231","#44047a"),0.7), bty = "n", horiz = T,
       inset = c(0.03,-0.1))

# Cool air temperatures
plot(gf.air ~ season_year, mm.cool, type = "l", col = alpha("#44047a",1), 
     xlim = c(1990,2020), ylim = c(-19,-8), 
     lwd = 1.25, axes = F, xlab = "", ylab = "")
polygon(c(mm.cool$season_year, rev(mm.cool$season_year)), 
        c(mm.cool$gf.air.ci5, rev(mm.cool$gf.air.ci95)), col = alpha("#44047a", 0.3), border = NA)
lines(hf.air ~ season_year, mm.cool, col = alpha("#c9c232",1), lwd = 1.25)
polygon(c(mm.cool$season_year, rev(mm.cool$season_year)), 
        c(mm.cool$hf.air.ci5, rev(mm.cool$hf.air.ci95)), col = alpha("#c9c231", 0.4), border = NA)
axis(1, seq(1990,2020,5), labels = F); axis(2, seq(-20,-8, 2))
box()
legend("topleft", "(b) O-M air (150 cm)", inset = c(-0.03,-0.1), bty = "n")

# Warm soil temperatures
plot(gf.soil ~ season_year, mm.warm, type = "l", col = alpha("#44047a",1), 
     xlim = c(1990,2020), ylim = c(3,14), 
     lwd = 1.25, axes = F, xlab = "", ylab = "")
polygon(c(mm.warm$season_year, rev(mm.warm$season_year)), 
        c(mm.warm$gf.soil.ci5, rev(mm.warm$gf.soil.ci95)), col = alpha("#44047a", 0.3), border = NA)
lines(hf.soil ~ season_year, mm.warm, col = alpha("#c9c232",1), lwd = 1.25)
polygon(c(mm.warm$season_year, rev(mm.warm$season_year)), 
        c(mm.warm$hf.soil.ci5, rev(mm.warm$hf.soil.ci95)), col = alpha("#c9c231", 0.4), border = NA)
axis(1, seq(1990,2020,5), labels = F); axis(2, seq(2, 14, 2))
box()
legend("topleft", "(c) J-S soil (0-5 cm)", inset = c(-0.03,-0.1), bty = "n")

# Cool soil temperatures
plot(gf.soil ~ season_year, mm.cool, type = "l", col = alpha("#44047a",1), 
     xlim = c(1990,2020), ylim = c(-11,-2), lwd = 1.25, axes = F, xlab = "", ylab = "")
abline(coef(lm(gf.soil ~ season_year, mm.cool)), col = alpha("#44047a",1), lty = 2)
polygon(c(mm.cool$season_year, rev(mm.cool$season_year)), 
        c(mm.cool$gf.soil.ci5, rev(mm.cool$gf.soil.ci95)), col = alpha("#44047a", 0.3), border = NA)
lines(hf.soil ~ season_year, mm.cool, col = alpha("#c9c232",1), lwd = 1.25)
abline(coef(lm(hf.soil ~ season_year, mm.cool)), col = alpha("#c9c232",1), lty = 2)
polygon(c(mm.cool$season_year, rev(mm.cool$season_year)), 
        c(mm.cool$hf.soil.ci5, rev(mm.cool$hf.soil.ci95)), col = alpha("#c9c231", 0.4), border = NA)
axis(1, seq(1990,2020,5)); axis(2, seq(-10,-2, 2))
box()
legend("topleft", "(d) O-M soil (0-5 cm)", inset = c(-0.03,-0.1), bty = "n")
mtext("Year", 1, cex = 0.65, line = 2)
mtext("Temperature (°C)", 2, cex = 0.65, line = 1.8, adj = 0.65, outer = T)

# Lapse rates
# par(mar = c(0.3,0.3,2,1))
plot(mean ~ season_year, mm.cool, type = "l", col = alpha("blue",0.5), 
     xlim = c(1990,2020), ylim = c(-1.5,1.5), lwd = 1.25)
polygon(c(mm.cool$season_year, rev(mm.cool$season_year)), 
        c(mm.cool$lapse.ci5, rev(mm.cool$lapse.ci95)), col = alpha("blue", 0.2), border = NA)
lines(mm.warm$mean ~ mm.warm$season_year, col = alpha("red",0.5), lwd = 1.25)
polygon(c(mm.warm$season_year, rev(mm.warm$season_year)), 
        c(mm.warm$lapse.ci5, rev(mm.warm$lapse.ci95)), col = alpha("red", 0.2), border = NA)
abline(h = 0, lwd = 0.5)
legend("topleft", "(e) Lapse rates", inset = c(-0.03,-0.1), bty = "n")
mtext("Year", 1, cex = 0.65, line = 2)
mtext(expression("°C 100 m"^-1), 2, cex = 0.65, line = 2.1)
legend("topright", c("J-S","O-M"), fill = alpha(c("red","blue"),0.7), inset = c(0.03,-0.1), 
       bty = "n", horiz = T)

