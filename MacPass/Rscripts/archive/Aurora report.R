rm(list = ls())

####set working directory#####

setwd("~/Desktop/Workspace/gtree")

mm <- read.csv(file = "gtree_mm_exclosures.csv", header = TRUE)

mm$treat <- as.factor(mm$treat)
mm$scarif <- as.factor(mm$scarif)
mm$seeded <- as.factor(mm$seeded)
mm$tag <- factor(mm$tag)

mm.salp <- subset(mm, site == "salp")
mm.sshr <- subset(mm, site == "sshr")
mm.scut <- subset(mm, site == "scut")
mm.nalp <- subset(mm, site == "nalp")
mm.nalp.fir.ex <- subset(mm.nalp, species == "fir" & exclosure == "yes")
mm.nalp.fir.no <- subset(mm.nalp, species == "fir" & exclosure == "no")
mm.salp.fir.ex <- subset(mm.salp, species == "fir" & exclosure == "yes")
mm.salp.fir.no <- subset(mm.salp, species == "fir" & exclosure == "no")
mm.sshr.fir.ex <- subset(mm.sshr, species == "fir" & exclosure == "yes")
mm.sshr.fir.no <- subset(mm.sshr, species == "fir" & exclosure == "no")
mm.scut.fir.ex <- subset(mm.scut, species == "fir" & exclosure == "yes")
mm.scut.fir.no <- subset(mm.scut, species == "fir" & exclosure == "no")

mm.nalp.spruce.ex <- subset(mm.nalp, species == "spruce" & exclosure == "yes")
mm.nalp.spruce.no <- subset(mm.nalp, species == "spruce" & exclosure == "no")
mm.salp.spruce.ex <- subset(mm.salp, species == "spruce" & exclosure == "yes")
mm.salp.spruce.no <- subset(mm.salp, species == "spruce" & exclosure == "no")
mm.sshr.spruce.ex <- subset(mm.sshr, species == "spruce" & exclosure == "yes")
mm.sshr.spruce.no <- subset(mm.sshr, species == "spruce" & exclosure == "no")
mm.scut.spruce.ex <- subset(mm.scut, species == "spruce" & exclosure == "yes")
mm.scut.spruce.no <- subset(mm.scut, species == "spruce" & exclosure == "no")

x1a <- factor(mm.nalp.fir.ex[,"treat"], levels = c("1", "3", "2", "4"))
x1b <- factor(mm.nalp.fir.no[,"treat"], levels = c("1", "3", "2", "4"))
x2a <- factor(mm.salp.fir.ex[,"treat"], levels = c("1", "3", "2", "4"))
x2b <- factor(mm.salp.fir.no[,"treat"], levels = c("1", "3", "2", "4"))
x3a <- factor(mm.scut.fir.ex[,"treat"], levels = c("1", "3", "2", "4"))
x3b <- factor(mm.scut.fir.no[,"treat"], levels = c("1", "3", "2", "4"))
x4a <- factor(mm.sshr.fir.ex[,"treat"], levels = c("1", "3", "2", "4"))
x4b <- factor(mm.sshr.fir.no[,"treat"], levels = c("1", "3", "2", "4"))

# Export at 7 x 7
# Export at 5 x 3.5
par(mfcol = c(4, 2))
par(cex = 1)
par(mar = c(1, 2, 0, 1), oma = c(2, 1, 2, 1)+0.2)

# Proportion fir germination each year in and out of cages - Export at 5 x 7
boxplot(mm.nalp.fir.no$germ.prop.via ~ x1b, at = c(1,3,5,7), xlim = c(0,9), ylim = c(0,1.6), 
        xaxt = "n", yaxt = "n", col = "red")
stripchart(mm.nalp.fir.no$germ.prop.via ~ x1b, at = c(1,3,5,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
boxplot(mm.nalp.fir.ex$germ.prop.via ~ x1a, at = c(2,4,6,8), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm.nalp.fir.ex$germ.prop.via ~ x1a, at = c(2,4,6,8), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5,5.5,7.5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(a) North-facing alpine", bty = "n", inset = c(0.0,-0.1))
legend("left", c("Uncaged","Caged"), col = c("red", "blue"), 
       bty = "n", pch = 15, pt.cex = c(1,1), text.width = 1, y.intersp = 0.5, inset = c(0.05,0))
boxplot(mm.salp.fir.no$germ.prop.via ~ x2b, at = c(1,3,5,7), xlim = c(0,9), ylim = c(0,1.6), xaxt = "n", yaxt = "n", col = "red")
stripchart(mm.salp.fir.no$germ.prop.via ~ x2b, at = c(1,3,5,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
boxplot(mm.salp.fir.ex$germ.prop.via ~ x2a, at = c(2,4,6,8), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm.salp.fir.ex$germ.prop.via ~ x2a, at = c(2,4,6,8), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5,5.5,7.5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(b) South-facing alpine", bty = "n", inset = c(0.0,-0.1))
boxplot(mm.scut.fir.no$germ.prop.via ~ x3b, at = c(1,3,5,7), xlim = c(0,9), ylim = c(0,1.6), xaxt = "n", yaxt = "n", col = "red")
stripchart(mm.scut.fir.no$germ.prop.via ~ x3b, at = c(1,3,5,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
boxplot(mm.scut.fir.ex$germ.prop.via ~ x3a, at = c(2,4,6,8), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm.scut.fir.ex$germ.prop.via ~ x3a, at = c(2,4,6,8), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5,5.5,7.5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(c) South-facing shrub (cut)", bty = "n", inset = c(0.0,-0.1))
boxplot(mm.sshr.fir.no$germ.prop.via ~ x4b, at = c(1,3,5,7), xlim = c(0,9), ylim = c(0,1.6), xaxt = "n", yaxt = "n", col = "red")
stripchart(mm.sshr.fir.no$germ.prop.via ~ x4b, at = c(1,3,5,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
boxplot(mm.sshr.fir.ex$germ.prop.via ~ x4a, at = c(2,4,6,8), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm.sshr.fir.ex$germ.prop.via ~ x4a, at = c(2,4,6,8), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5,5.5,7.5), labels = c("","","",""), tick = TRUE)
axis(1, at = c(1.5,3.5,5.5,7.5), labels = c("Vegetated","Scarified","Vegetated","Scarified"), tick = TRUE)
axis(1, at = c(2.5,6.5), labels = c("Unseeded","Seeded"), tick = FALSE, line = 1, font = 2)
mtext(side = 2, "Germination proportion of viable", outer = TRUE)
legend("topleft", "(d) South-facing shrub", bty = "n", inset = c(0.0,-0.1))
# par(xpd = NA)
# rect(0.5,-0.55,4.4,-0.45, col = 'gray75', border = NA)
# rect(4.6,-0.55,8.5,-0.45, col = 'gray75', border = NA)

# Proportion spruce germination each year in and out of cages - Export at 5 x 7
boxplot(mm.nalp.spruce.no$germ.prop.via ~ x1b, at = c(1,3,5,7), xlim = c(0,9), ylim = c(0,1.2), xaxt = "n", yaxt = "n", col = "red")
stripchart(mm.nalp.spruce.no$germ.prop.via ~ x1b, at = c(1,3,5,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
boxplot(mm.nalp.spruce.ex$germ.prop.via ~ x1a, at = c(2,4,6,8), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm.nalp.spruce.ex$germ.prop.via ~ x1a, at = c(2,4,6,8), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5,5.5,7.5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(a) North-facing alpine", bty = "n", inset = c(0.0,-0.1))
legend("left", c("Uncaged","Caged"), col = c("red", "blue"), 
       bty = "n", pch = 15, pt.cex = c(1,1), text.width = 1, y.intersp = 0.5, inset = c(0.05,0))
boxplot(mm.salp.spruce.no$germ.prop.via ~ x2b, at = c(1,3,5,7), xlim = c(0,9), ylim = c(0,1.2), xaxt = "n", yaxt = "n", col = "red")
stripchart(mm.salp.spruce.no$germ.prop.via ~ x2b, at = c(1,3,5,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
boxplot(mm.salp.spruce.ex$germ.prop.via ~ x2a, at = c(2,4,6,8), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm.salp.spruce.ex$germ.prop.via ~ x2a, at = c(2,4,6,8), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5,5.5,7.5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(b) South-facing alpine", bty = "n", inset = c(0.0,-0.1))
boxplot(mm.scut.spruce.no$germ.prop.via ~ x3b, at = c(1,3,5,7), xlim = c(0,9), ylim = c(0,1.2), xaxt = "n", yaxt = "n", col = "red")
stripchart(mm.scut.spruce.no$germ.prop.via ~ x3b, at = c(1,3,5,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
boxplot(mm.scut.spruce.ex$germ.prop.via ~ x3a, at = c(2,4,6,8), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm.scut.spruce.ex$germ.prop.via ~ x3a, at = c(2,4,6,8), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5,5.5,7.5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(c) South-facing shrub (cut)", bty = "n", inset = c(0.0,-0.1))
boxplot(mm.sshr.spruce.no$germ.prop.via ~ x4b, at = c(1,3,5,7), xlim = c(0,9), ylim = c(0,1.2), xaxt = "n", yaxt = "n", col = "red")
stripchart(mm.sshr.spruce.no$germ.prop.via ~ x4b, at = c(1,3,5,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
boxplot(mm.sshr.spruce.ex$germ.prop.via ~ x4a, at = c(2,4,6,8), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm.sshr.spruce.ex$germ.prop.via ~ x4a, at = c(2,4,6,8), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5,5.5,7.5), labels = c("","","",""), tick = TRUE)
axis(1, at = c(1.5,3.5,5.5,7.5), labels = c("Vegetated","Scarified","Vegetated","Scarified"), tick = TRUE)
axis(1, at = c(2.5,6.5), labels = c("Unseeded","Seeded"), tick = FALSE, line = 1, font = 2)
mtext(side = 2, "Germination proportion of viable", outer = TRUE)
legend("topleft", "(d) South-facing shrub", bty = "n", inset = c(0.0,-0.1))
# par(xpd = NA)
# rect(0.5,-0.55,4.4,-0.45, col = 'gray75', border = NA)
# rect(4.6,-0.55,8.5,-0.45, col = 'gray75', border = NA)







# Proportion fir survival each year in and out of cages - Export at 5 x 7
par(mfrow = c(4, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(0, 2, 1, 1), oma = c(5,2,0.5,1))
##
boxplot(mm.nalp.fir.no$surv.prop.0 ~ x1b, at = c(1,3,5,7), xlim = c(0,9), ylim = c(0,1.1), xaxt = "n", yaxt = "n", col = "red")
stripchart(mm.nalp.fir.no$surv.prop.0 ~ x1b, at = c(1,3,5,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
boxplot(mm.nalp.fir.ex$surv.prop.0 ~ x1a, at = c(2,4,6,8), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm.nalp.fir.ex$surv.prop.0 ~ x1a, at = c(2,4,6,8), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5,5.5,7.5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(a) North-facing alpine", bty = "n", inset = c(0.0,-0.1))
legend("left", c("Uncaged","Caged"), col = c("red", "blue"), 
       bty = "n", pch = 15, pt.cex = c(1,1), text.width = 1, y.intersp = 0.5, inset = c(0.05,0))
boxplot(mm.salp.fir.no$surv.prop.0 ~ x2b, at = c(1,3,5,7), xlim = c(0,9), ylim = c(0,1.1), xaxt = "n", yaxt = "n", col = "red")
stripchart(mm.salp.fir.no$surv.prop.0 ~ x2b, at = c(1,3,5,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
boxplot(mm.salp.fir.ex$surv.prop.0 ~ x2a, at = c(2,4,6,8), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm.salp.fir.ex$surv.prop.0 ~ x2a, at = c(2,4,6,8), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5,5.5,7.5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(b) South-facing alpine", bty = "n", inset = c(0.0,-0.1))
boxplot(mm.scut.fir.no$surv.prop.0 ~ x3b, at = c(1,3,5,7), xlim = c(0,9), ylim = c(0,1.1), xaxt = "n", yaxt = "n", col = "red")
stripchart(mm.scut.fir.no$surv.prop.0 ~ x3b, at = c(1,3,5,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
boxplot(mm.scut.fir.ex$surv.prop.0 ~ x3a, at = c(2,4,6,8), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm.scut.fir.ex$surv.prop.0 ~ x3a, at = c(2,4,6,8), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5,5.5,7.5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(c) South-facing shrub (cut)", bty = "n", inset = c(0.0,-0.1))
boxplot(mm.sshr.fir.no$surv.prop.0 ~ x4b, at = c(1,3,5,7), xlim = c(0,9), ylim = c(0,1.1), xaxt = "n", yaxt = "n", col = "red")
stripchart(mm.sshr.fir.no$surv.prop.0 ~ x4b, at = c(1,3,5,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
boxplot(mm.sshr.fir.ex$surv.prop.0 ~ x4a, at = c(2,4,6,8), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm.sshr.fir.ex$surv.prop.0 ~ x4a, at = c(2,4,6,8), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5,5.5,7.5), labels = c("","","",""), tick = TRUE)
axis(1, at = c(1.5,3.5,5.5,7.5), labels = c("Vegetated","Scarified","Vegetated","Scarified"), tick = TRUE)
axis(1, at = c(2.5,6.5), labels = c("Unseeded","Seeded"), tick = FALSE, line = 1, font = 2)
mtext(side = 2, "Survival proportion", outer = TRUE)
legend("topleft", "(d) South-facing shrub", bty = "n", inset = c(0.0,-0.1))
# par(xpd = NA)
# rect(0.5,-0.55,4.4,-0.45, col = 'gray75', border = NA)
# rect(4.6,-0.55,8.5,-0.45, col = 'gray75', border = NA)

# Proportion spruce germination each year in and out of cages - Export at 5 x 7
boxplot(mm.nalp.spruce.no$surv.prop.0 ~ x1b, at = c(1,3,5,7), xlim = c(0,9), ylim = c(0,1.1), xaxt = "n", yaxt = "n", col = "red")
stripchart(mm.nalp.spruce.no$surv.prop.0 ~ x1b, at = c(1,3,5,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
boxplot(mm.nalp.spruce.ex$surv.prop.0 ~ x1a, at = c(2,4,6,8), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm.nalp.spruce.ex$surv.prop.0 ~ x1a, at = c(2,4,6,8), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5,5.5,7.5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(a) North-facing alpine", bty = "n", inset = c(0.0,-0.1))
legend("left", c("Uncaged","Caged"), col = c("red", "blue"), 
       bty = "n", pch = 15, pt.cex = c(1,1), text.width = 1, y.intersp = 0.5, inset = c(0.05,0))
boxplot(mm.salp.spruce.no$surv.prop.0 ~ x2b, at = c(1,3,5,7), xlim = c(0,9), ylim = c(0,1.1), xaxt = "n", yaxt = "n", col = "red")
stripchart(mm.salp.spruce.no$surv.prop.0 ~ x2b, at = c(1,3,5,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
boxplot(mm.salp.spruce.ex$surv.prop.0 ~ x2a, at = c(2,4,6,8), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm.salp.spruce.ex$surv.prop.0 ~ x2a, at = c(2,4,6,8), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5,5.5,7.5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(b) South-facing alpine", bty = "n", inset = c(0.0,-0.1))
boxplot(mm.scut.spruce.no$surv.prop.0 ~ x3b, at = c(1,3,5,7), xlim = c(0,9), ylim = c(0,1.1), xaxt = "n", yaxt = "n", col = "red")
stripchart(mm.scut.spruce.no$surv.prop.0 ~ x3b, at = c(1,3,5,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
boxplot(mm.scut.spruce.ex$surv.prop.0 ~ x3a, at = c(2,4,6,8), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm.scut.spruce.ex$surv.prop.0 ~ x3a, at = c(2,4,6,8), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5,5.5,7.5), labels = c("","","",""), tick = TRUE)
legend("topleft", "(c) South-facing shrub (cut)", bty = "n", inset = c(0.0,-0.1))
boxplot(mm.sshr.spruce.no$surv.prop.0 ~ x4b, at = c(1,3,5,7), xlim = c(0,9), ylim = c(0,1.1), xaxt = "n", yaxt = "n", col = "red")
stripchart(mm.sshr.spruce.no$surv.prop.0 ~ x4b, at = c(1,3,5,7), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
boxplot(mm.sshr.spruce.ex$surv.prop.0 ~ x4a, at = c(2,4,6,8), xaxt = "n", add = TRUE, col = "blue")
stripchart(mm.sshr.spruce.ex$surv.prop.0 ~ x4a, at = c(2,4,6,8), vertical = TRUE, method = "jitter", 
           add = TRUE, bg = "gray50", pch = 21, cex = 0.75)
axis(1, at = c(1.5,3.5,5.5,7.5), labels = c("","","",""), tick = TRUE)
axis(1, at = c(1.5,3.5,5.5,7.5), labels = c("Vegetated","Scarified","Vegetated","Scarified"), tick = TRUE)
axis(1, at = c(2.5,6.5), labels = c("Unseeded","Seeded"), tick = FALSE, line = 1, font = 2)
mtext(side = 2, "Survival proportion", outer = TRUE)
legend("topleft", "(d) South-facing shrub", bty = "n", inset = c(0.0,-0.1))
# par(xpd = NA)
# rect(0.5,-0.55,4.4,-0.45, col = 'gray75', border = NA)
# rect(4.6,-0.55,8.5,-0.45, col = 'gray75', border = NA)
