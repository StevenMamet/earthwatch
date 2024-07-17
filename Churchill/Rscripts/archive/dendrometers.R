library(dendrometeR)

rm(list=ls())

dendro <- read.csv("~/Desktop/Workspace/Churchill/dendrometers.csv")

# The missing dendrometer at WSU
wsu.2801.1 <- lm(dendro[, 6] ~ dendro[, 7] + dendro[, 9])# + dendro[, 16])
# wsu.2801.1 <- lm(wsu.2801 ~ ., data = dendro[,-c(1:5)])# + dendro[, 16])
summary(wsu.2801.1)

## Replace 2801 NAs with calculated values
dendro2 <- dendro
dendro2$wsu.2801 <- ifelse(is.na(dendro2$wsu.2801), 
                           (coef(summary(wsu.2801.1))[1] + 
                            coef(summary(wsu.2801.1))[2]*dendro2$wsu.2802 +
                            coef(summary(wsu.2801.1))[3]*dendro2$wsu.2804), dendro2$wsu.2801)

plot(dendro2$wsu.2801, type = "l", col = "red", ylim = c(0,15000))
lines(dendro$wsu.2801, col = "blue")
lines(dendro$wsu.2802, col = "green")
lines(dendro$wsu.2803, col = "violet")
lines(dendro$wsu.2804, col = "orange")

plot(dendro2$rid.2805, type = "l", col = "red", ylim = c(0,25000))
lines(dendro$rid.2806, col = "blue")
lines(dendro$rid.2807, col = "green")
lines(dendro$rid.2808, col = "violet")

plot(dendro2$bay.2809, type = "l", col = "red", ylim = c(0,25000))
lines(dendro2$bay.2810, col = "blue")
lines(dendro2$bay.2811, col = "green")
lines(dendro2$bay.2812, col = "violet")


# Now fill the BAY values using RID
bay.2809.1 <- lm(dendro[, 14] ~ dendro[, 8] + dendro[, 9] + dendro[, 10] + dendro[, 11])
bay.2810.1 <- lm(dendro[, 15] ~ dendro[, 8] + dendro[, 9] + dendro[, 10] + dendro[, 11])
bay.2811.1 <- lm(dendro[, 16] ~ dendro[, 8] + dendro[, 9] + dendro[, 10] + dendro[, 11])
bay.2812.1 <- lm(dendro[, 17] ~ dendro[, 8] + dendro[, 9] + dendro[, 10] + dendro[, 11])
summary(bay.2809.1)
summary(bay.2810.1)
summary(bay.2811.1)
summary(bay.2812.1)

## Replace BAY NAs with calculated values
dendro2$bay.2809 <- ifelse(is.na(dendro2$bay.2809), 
                           (coef(summary(bay.2809.1))[1] + 
                              coef(summary(bay.2809.1))[2]*dendro2$wsu.2803 + 
                              coef(summary(bay.2809.1))[3]*dendro2$wsu.2804 + 
                              coef(summary(bay.2809.1))[4]*dendro2$rid.2805 + 
                              coef(summary(bay.2809.1))[5]*dendro2$rid.2806), dendro2$bay.2809)

dendro2$bay.2810 <- ifelse(is.na(dendro2$bay.2810), 
                           (coef(summary(bay.2810.1))[1] + 
                              coef(summary(bay.2810.1))[2]*dendro2$wsu.2803 + 
                              coef(summary(bay.2810.1))[3]*dendro2$wsu.2804 + 
                              coef(summary(bay.2810.1))[4]*dendro2$rid.2805 + 
                              coef(summary(bay.2810.1))[5]*dendro2$rid.2806), dendro2$bay.2810)

dendro2$bay.2811 <- ifelse(is.na(dendro2$bay.2811), 
                           (coef(summary(bay.2811.1))[1] + 
                              coef(summary(bay.2811.1))[2]*dendro2$wsu.2803 + 
                              coef(summary(bay.2811.1))[3]*dendro2$wsu.2804 + 
                              coef(summary(bay.2811.1))[4]*dendro2$rid.2805 + 
                              coef(summary(bay.2811.1))[5]*dendro2$rid.2806), dendro2$bay.2811)

dendro2$bay.2812 <- ifelse(is.na(dendro2$bay.2812), 
                           (coef(summary(bay.2812.1))[1] + 
                              coef(summary(bay.2812.1))[2]*dendro2$wsu.2803 + 
                              coef(summary(bay.2812.1))[3]*dendro2$wsu.2804 + 
                              coef(summary(bay.2812.1))[4]*dendro2$rid.2805 + 
                              coef(summary(bay.2812.1))[5]*dendro2$rid.2806), dendro2$bay.2812)



matplot(rownames(dendro2), dendro2[,c(6:17)], type = "l")

write.csv(dendro2,"~/Desktop/dendro2.csv")
