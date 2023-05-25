library(tweenr)
library(ggplot2)
library(gganimate)
library(transformr)
library(gifski)
library(gapminder)

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

rm(list = ls())

p <- ggplot(
  airquality,
  aes(Day, Temp, group = Month, color = factor(Month))
) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Day of Month", y = "Temperature") +
  theme(legend.position = "top")
p

p + transition_reveal(Day)

# p <- ggplot(airquality, aes(Day, Temp)) + 
#   geom_line(size = 2, colour = 'steelblue') + 
#   transition_states(Month, 4, 1) + 
#   shadow_mark(size = 1, colour = 'grey')
# animate(p, renderer = ffmpeg_renderer())

### Thaw depths
thaw.mm <- read.csv(file = "~/Desktop/Workspace/Earthwatch/thaw.mm.csv", header = TRUE)
thaw.mm$ci <- thaw.mm$se*qt(0.975, thaw.mm$n-1)
thaw.mm$c5 <- thaw.mm$mean - thaw.mm$ci
thaw.mm$c95 <- thaw.mm$mean + thaw.mm$ci

d2 <- subset(thaw.mm, site == "D2")
d6 <- subset(thaw.mm, site == "D6")
beaver <- subset(thaw.mm, site == "Beaver")
hare <- subset(thaw.mm, site == "Hare")
goose <- subset(thaw.mm, site == "Goose")
snow <- subset(thaw.mm, site == "Snow")
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


# Basic line plot
p <- ggplot(data = beaver, aes(x = year, y = mean))+
  geom_line(color = "gold", size = 1) + 
  scale_y_reverse(name = "Thaw depth (cm)", limits = c(80,40)) + 
  scale_x_continuous(name ="Year", limits=c(1990,2020)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) 
p
p + transition_reveal(year)

p <- ggplot(data = hare, aes(x = year, y = mean))+
  geom_line(color = "pink", size = 1) + 
  scale_y_reverse(name = "Thaw depth (cm)", limits = c(80,40)) + 
  scale_x_continuous(name ="Year", limits=c(1990,2020)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) 
p

p + transition_reveal(year)



par(mar = c(4,4,1,1))
plot(hare$year, hare$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(hare$year, hare$c5, col = 'pink', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(hare$year, hare$c95, col = 'pink', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(hare$year, rev(hare$year)), c(hare$c95, rev(hare$c5)), col = "pink", border = NA)
par(new=T)
plot(hare$year, hare$mean, xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "red", xlab = "", ylab = "")
legend("topleft", "(h) HF (1260 m.a.s.l.)", bty = "n", inset = c(-0.05,0.0))
legend("bottomleft", expression(paste("Slope = 0.521 cm yr"^"-1")), bty = "n", inset = c(-0.05,0.0))
mtext(side = 1, "Year", line = 2.5)
mtext(side = 2, "Thaw depth (cm)", line = 2.5)
abline(coef(hare.lm),lty=2)






# snow.slope <- round(coef(snow.lm)[[2]], 3)

# Export at 7 x 7
# Export at 5 x 3.5
par(mfrow = c(4, 2))
par(cex = 0.75)
par(mar = c(3, 3, 0, 0), oma = c(2, 2, 2, 2)+0.2)

par(mar = c(4,4,1,1))
plot(snow$year, snow$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(snow$year, snow$c5, col = 'darkslategray1', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(snow$year, snow$c95, col = 'darkslategray1', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(snow$year, rev(snow$year)), c(snow$c95, rev(snow$c5)), col = "darkslategray1", border = NA)
par(new=T)
plot(snow$year, snow$mean, xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "darkslategray4", xlab = "", ylab = "")
legend("topleft", "SF (1660 m.a.s.l.)", bty = "n", inset = c(-0.05,0.0))
legend("bottomleft", expression(paste("Slope = 0.679 cm yr"^"-1")), bty = "n", 
       inset = c(-0.05,0.0))
mtext(side = 1, "Year", line = 2.5)
mtext(side = 2, "Thaw depth (cm)", line = 2.5)
abline(coef(snow.lm),lty=2)

plot(pipeline$year, pipeline$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(pipeline$year, pipeline$c5, col = 'firebrick1', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(pipeline$year, pipeline$c95, col = 'firebrick1', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(pipeline$year, rev(pipeline$year)), c(pipeline$c95, rev(pipeline$c5)), col = "firebrick1", border = NA)
par(new=T)
plot(pipeline$year, pipeline$mean, xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "firebrick4", xlab = "Year", ylab = "Thaw depth (cm)")
legend("topleft", "PP (1623 m.a.s.l.)", bty = "n", inset = c(-0.05,-0.08))
legend("bottomleft", expression(paste("Slope = 1.224 cm yr"^"-1")), bty = "n", inset = c(-0.05,-0.08))
abline(coef(pipeline.lm),lty=2)

# par(mar = c(4,4,1,1))
plot(goose$year, goose$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(goose$year, goose$c5, col = 'olivedrab1', xlim = c(1990,2020), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(goose$year, goose$c95, col = 'olivedrab1', xlim = c(1990,2020), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(goose$year, rev(goose$year)), c(goose$c95, rev(goose$c5)), col = "olivedrab1", border = NA)
par(new=T)
plot(goose$year, goose$mean, xlim = c(1990,2020), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "olivedrab4", xlab = "", ylab = "")
legend("topleft", "(c) GF (1621 m.a.s.l.)", bty = "n", inset = c(-0.05,0.0))
legend("bottomleft", expression(paste("Slope = 1.871 cm yr"^"-1")), bty = "n", inset = c(-0.05,0.0))
mtext(side = 1, "Year", line = 2.5)
mtext(side = 2, "Thaw depth (cm)", line = 2.5)
abline(coef(goose.lm),lty=2)

par(mar = c(4,4,1,1))
plot(d2$year, d2$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(d2$year, d2$c5, col = 'lightskyblue', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(d2$year, d2$c95, col = 'lightskyblue', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(d2$year, rev(d2$year)), c(d2$c95, rev(d2$c5)), col = "lightskyblue", border = NA)
par(new=T)
plot(d2$year, d2$mean, xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "blue", xlab = "", ylab = "")
legend("topleft", "(d) D2 (1477 m.a.s.l.)", bty = "n", inset = c(-0.05,-0.0))
legend("bottomleft", expression(paste("Slope = 1.265 cm yr"^"-1")), bty = "n", inset = c(-0.05,-0.0))
mtext(side = 1, "Year", line = 2.5)
mtext(side = 2, "Thaw depth (cm)", line = 2.5)
abline(coef(d2.lm),lty=2)

par(mar = c(4,4,1,1))
plot(d6$year, d6$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(d6$year, d6$c5, col = 'orange1', xlim = c(1990,2020), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(d6$year, d6$c95, col = 'orange1', xlim = c(1990,2020), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(d6$year, rev(d6$year)), c(d6$c95, rev(d6$c5)), col = "orange1", border = NA)
par(new=T)
plot(d6$year, d6$mean, xlim = c(1990,2020), ylim = rev(c(0,150)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "orangered2", xlab = "", ylab = "")
legend("topleft", "(e) D6 (1473 m.a.s.l.)", bty = "n", inset = c(-0.05,-0.0))
# legend("bottomleft", expression(paste("Slope = 0.912 cm yr"^"-1")), bty = "n", inset = c(0,0.05))
mtext(side = 1, "Year", line = 2.5)
mtext(side = 2, "Thaw depth (cm)", line = 2.5)
# abline(coef(d6.lm),lty=2)

par(mar = c(4,4,1,1))
plot(porsild$year, porsild$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(porsild$year, porsild$c5, col = 'darkorchid1', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(porsild$year, porsild$c95, col = 'darkorchid1', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(porsild$year, rev(porsild$year)), c(porsild$c95, rev(porsild$c5)), col = "darkorchid1", border = NA)
par(new=T)
plot(porsild$year, porsild$mean, xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "darkorchid4", xlab = "", ylab = "")
legend("topleft", "(f) PF (1380 m.a.s.l.)", bty = "n", inset = c(-0.05,-0.0))
legend("bottomleft", expression(paste("Slope = 1.158 cm yr"^"-1")), bty = "n", inset = c(-0.05,-0.0))
mtext(side = 1, "Year", line = 2.5)
mtext(side = 2, "Thaw depth (cm)", line = 2.5)
abline(coef(porsild.lm),lty=2)

par(mar = c(4,4,1,1))
plot(porsild.1$year, porsild.1$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(porsild.1$year, porsild.1$c5, col = 'darkorchid1', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(porsild.1$year, porsild.1$c95, col = 'darkorchid1', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(porsild.1$year, rev(porsild.1$year)), c(porsild.1$c95, rev(porsild.1$c5)), col = "darkorchid1", border = NA)
par(new=T)
plot(porsild.1$year, porsild.1$mean, xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "darkorchid4", xlab = "", ylab = "")
legend("topleft", "(f) PF1 (1380 m.a.s.l.)", bty = "n", inset = c(-0.05,-0.0))
# legend("bottomleft", expression(paste("Slope = 1.158 cm yr"^"-1")), bty = "n", inset = c(-0.05,-0.0))
mtext(side = 1, "Year", line = 2.5)
mtext(side = 2, "Thaw depth (cm)", line = 2.5)
# abline(coef(porsild.1.lm),lty=2)

par(mar = c(4,4,1,1))
plot(porsild.2$year, porsild.2$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(porsild.2$year, porsild.2$c5, col = 'darkorchid1', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(porsild.2$year, porsild.2$c95, col = 'darkorchid1', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(porsild.2$year, rev(porsild.2$year)), c(porsild.2$c95, rev(porsild.2$c5)), col = "darkorchid1", border = NA)
par(new=T)
plot(porsild.2$year, porsild.2$mean, xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "darkorchid4", xlab = "", ylab = "")
legend("topleft", "(f) PF2 (1380 m.a.s.l.)", bty = "n", inset = c(-0.05,-0.0))
legend("bottomleft", expression(paste("Slope = 1.690 cm yr"^"-1")), bty = "n", inset = c(-0.05,-0.0))
mtext(side = 1, "Year", line = 2.5)
mtext(side = 2, "Thaw depth (cm)", line = 2.5)
abline(coef(porsild.2.lm),lty=2)

par(mar = c(4,4,1,1))
plot(beaver$year, beaver$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(beaver$year, beaver$c5, col = 'gold', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(beaver$year, beaver$c95, col = 'gold', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(beaver$year, rev(beaver$year)), c(beaver$c95, rev(beaver$c5)), col = "gold", border = NA)
par(new=T)
plot(beaver$year, beaver$mean, xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "goldenrod3", xlab = "", ylab = "")
legend("topleft", "(g) BP (1272 m.a.s.l.)", bty = "n", inset = c(-0.05,-0.0))
legend("bottomleft", expression(paste("Slope = 0.689 cm yr"^"-1")), bty = "n", inset = c(-0.05,-0.0))
mtext(side = 1, "Year", line = 2.5)
mtext(side = 2, "Thaw depth (cm)", line = 2.5)
abline(coef(beaver.lm),lty=2)

par(mar = c(4,4,1,1))
plot(hare$year, hare$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(hare$year, hare$c5, col = 'pink', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(hare$year, hare$c95, col = 'pink', xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(hare$year, rev(hare$year)), c(hare$c95, rev(hare$c5)), col = "pink", border = NA)
par(new=T)
plot(hare$year, hare$mean, xlim = c(1990,2020), ylim = rev(c(0,100)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "red", xlab = "", ylab = "")
legend("topleft", "(h) HF (1260 m.a.s.l.)", bty = "n", inset = c(-0.05,0.0))
legend("bottomleft", expression(paste("Slope = 0.521 cm yr"^"-1")), bty = "n", inset = c(-0.05,0.0))
mtext(side = 1, "Year", line = 2.5)
mtext(side = 2, "Thaw depth (cm)", line = 2.5)
abline(coef(hare.lm),lty=2)


mtext("Thaw depth (cm)", side=2, outer=TRUE, cex = 0.85, adj=0.5)
mtext("Year", side=1, outer=TRUE, cex=0.85, adj=0.5)

##

# Export as 5 x 7
par(mfrow = c(3, 1))
par(cex = 0.75)
par(mar = c(3, 3, 0, 0), oma = c(2, 2, 2, 2)+0.2)

plot(pp.control.p$year, pp.control.p$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(pp.control.p$year, pp.control.p$c5, col = 'firebrick1', xlim = c(1990,2020), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(pp.control.p$year, pp.control.p$c95, col = 'firebrick1', xlim = c(1990,2020), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(pp.control.p$year, rev(pp.control.p$year)), c(pp.control.p$c95, rev(pp.control.p$c5)), col = "firebrick1", border = NA)
par(new=T)
plot(pp.control.p$year, pp.control.p$mean, xlim = c(1990,2020), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "firebrick4", xlab = "Year", ylab = "Thaw depth (cm)")
abline(h = mean(pp.control.p$mean), lty = 2, lwd = 2)
legend("topleft", "PP: pipeline-control (1623 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))

plot(pp.pipeline.p$year, pp.pipeline.p$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(pp.pipeline.p$year, pp.pipeline.p$c5, col = 'firebrick1', xlim = c(1990,2020), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(pp.pipeline.p$year, pp.pipeline.p$c95, col = 'firebrick1', xlim = c(1990,2020), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(pp.pipeline.p$year, rev(pp.pipeline.p$year)), c(pp.pipeline.p$c95, rev(pp.pipeline.p$c5)), col = "firebrick1", border = NA)
par(new=T)
plot(pp.pipeline.p$year, pp.pipeline.p$mean, xlim = c(1990,2020), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "firebrick4", xlab = "Year", ylab = "Thaw depth (cm)")
abline(h = mean(pp.pipeline.p$mean), lty = 2, lwd = 2)
legend("topleft", "PP: pipeline (1623 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))

plot(pp.track.t$year, pp.track.t$mean, type='n', xlim = c(1990,2020), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(pp.track.t$year, pp.track.t$c5, col = 'firebrick1', xlim = c(1990,2020), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
lines(pp.track.t$year, pp.track.t$c95, col = 'firebrick1', xlim = c(1990,2020), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", xlab = "", ylab = "")
polygon(c(pp.track.t$year, rev(pp.track.t$year)), c(pp.track.t$c95, rev(pp.track.t$c5)), col = "firebrick1", border = NA)
par(new=T)
plot(pp.track.t$year, pp.track.t$mean, xlim = c(1990,2020), ylim = rev(c(0,120)), yaxs = "i", xaxs = "i", type = "l", lwd=2, col = "firebrick4", xlab = "Year", ylab = "Thaw depth (cm)")
abline(h = mean(pp.track.t$mean), lty = 2, lwd = 2)
legend("topleft", "PP: track (1623 m.a.s.l.)", bty = "n")#, inset = c(-0.08,-0.15))

mtext("Thaw depth (cm)", side=2, outer=TRUE, cex = 0.75, adj=0.5)
mtext("Year", side=1, outer=TRUE, cex=0.75, adj=0.5)

