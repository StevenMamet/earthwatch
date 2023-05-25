rm(list = ls())


pp <- read.csv(file = "~/Desktop/Workspace/Churchill/ppa.ppd_MeanT.csv", header = TRUE)


library(RColorBrewer)
colfunc1 <- colorRampPalette(brewer.pal(11, "Spectral"))

## Export at 5 x 7
# Thawed
par(oma = c(2,2,0,1), mar = c(2,1,0.5,0))
par(mfrow = c(2,1))
matplot(pp$year, pp[,c(2,9:14)], type = "l", xlim = c(1990,2017), ylim = c(-9,0), ylab = "", xlab = "",  lwd = 2, lty = 1, col = colfunc1(7))
# legend("topleft","PPA", bty = "n")
legend("left", legend = colnames(pp[,c(2,9:14)]), lty = 1, lwd = 2, col = colfunc1(7),
       bty = "n", pch = 15, pt.cex = c(1,1), text.width = 0.1, y.intersp = 0.6, inset = c(0,0))

matplot(pp$year, pp[,c(23:32)], type = "l", xlim = c(1990,2017),ylim = c(-9,0), ylab = "", xlab = "",  lwd = 2, lty = 1, col = colfunc1(10))
# legend("topleft","PPA", bty = "n")
legend("left", legend = colnames(pp[,c(23:32)]), lty = 1, lwd = 2, col = colfunc1(10),
       bty = "n", pch = 15, pt.cex = c(1,1), text.width = 0.1, y.intersp = 0.7, inset = c(0,0))

mtext(side = 2, "Temperature (°C)", outer = T, line = 1)
mtext(side = 1, "Year", line = 2)
