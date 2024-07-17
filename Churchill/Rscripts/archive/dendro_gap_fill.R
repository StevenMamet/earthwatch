rm(list=ls())

dendro <- read.csv(file = "~/Desktop/Workspace/Earthwatch/dendrometers_20180704.csv", header = TRUE)

matplot(rownames(dendro), dendro[,c(10:13)], type = "l")
plot(dendro[,6], type = "l")
plot(dendro[,7], type = "l")
plot(dendro[,8], type = "l")
plot(dendro[,9], type = "l")

plot(dendro[,10], type = "l")
plot(dendro[,11], type = "l")
plot(dendro[,12], type = "l")
plot(dendro[,13], type = "l")

plot(dendro[,14], type = "l")
plot(dendro[,15], type = "l")
plot(dendro[,16], type = "l")
plot(dendro[,17], type = "l")

# rid.2805
# rid.1.2805 <- lm(dendro[dendro$month==1, 10] ~ dendro[dendro$month==1, 14] + dendro[dendro$month==1, 15]) # Jan
# summary(rid.1.2805)
# rid.2.2805 <- lm(dendro[dendro$month==2, 10] ~ dendro[dendro$month==2, 14] + dendro[dendro$month==2, 15]) # Feb
# summary(rid.2.2805)
# rid.3.2805 <- lm(dendro[dendro$month==3, 10] ~ dendro[dendro$month==3, 14] + dendro[dendro$month==3, 15]) # Mar
# summary(rid.3.2805)
# rid.4.2805 <- lm(dendro[dendro$month==4, 10] ~ dendro[dendro$month==4, 14] + dendro[dendro$month==4, 15]) # Apr
# summary(rid.4.2805)
# rid.5.2805 <- lm(dendro[dendro$month==5, 10] ~ dendro[dendro$month==5, 14] + dendro[dendro$month==5, 15]) # May
# summary(rid.5.2805)
rid.6.2805 <- lm(dendro[dendro$month==6, 10] ~ dendro[dendro$month==6, 14] + dendro[dendro$month==6, 15]) # Jun
summary(rid.6.2805)
rid.7.2805 <- lm(dendro[dendro$month==7, 10] ~ dendro[dendro$month==7, 14] + dendro[dendro$month==7, 15]) # Jul
summary(rid.7.2805)
rid.8.2805 <- lm(dendro[dendro$month==8, 10] ~ dendro[dendro$month==8, 14] + dendro[dendro$month==8, 15]) # Aug
summary(rid.8.2805)
rid.9.2805 <- lm(dendro[dendro$month==9, 10] ~ dendro[dendro$month==9, 14] + dendro[dendro$month==9, 15]) # Sep
summary(rid.9.2805)
rid.10.2805 <- lm(dendro[dendro$month==10, 10] ~ dendro[dendro$month==10, 14] + dendro[dendro$month==10, 15]) # Oct
summary(rid.10.2805)
rid.11.2805 <- lm(dendro[dendro$month==11, 10] ~ dendro[dendro$month==11, 14] + dendro[dendro$month==11, 15]) # Nov
summary(rid.11.2805)
rid.12.2805 <- lm(dendro[dendro$month==12, 10] ~ dendro[dendro$month==12, 14] + dendro[dendro$month==12, 15]) # Dec
summary(rid.12.2805)

rid.2805 <- data.frame(rbind(coef(rid.1.2805),coef(rid.2.2805),coef(rid.3.2805),coef(rid.4.2805),coef(rid.5.2805),coef(rid.6.2805),
                             coef(rid.7.2805),coef(rid.8.2805),coef(rid.9.2805),coef(rid.10.2805),coef(rid.11.2805),coef(rid.12.2805)))

# names(rid.2805) <- c("b0","rid.2803","rid.2804")
# rownames(rid.2805) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
# write.csv(rid.2805, "~/Desktop/Workspace/Earthwatch/rid.2805.gap.csv")

# rid.2805
# rid.1.2805 <- lm(dendro[dendro$month==1, 10] ~ dendro[dendro$month==1, 11]) # Jan
# summary(rid.1.2805)
# rid.2.2805 <- lm(dendro[dendro$month==2, 10] ~ dendro[dendro$month==2, 11]) # Feb
# summary(rid.2.2805)
# rid.3.2805 <- lm(dendro[dendro$month==3, 10] ~ dendro[dendro$month==3, 11]) # Mar
# summary(rid.3.2805)
# rid.4.2805 <- lm(dendro[dendro$month==4, 10] ~ dendro[dendro$month==4, 11]) # Apr
# summary(rid.4.2805)
# rid.5.2805 <- lm(dendro[dendro$month==5, 10] ~ dendro[dendro$month==5, 11]) # May
# summary(rid.5.2805)
rid.6.2805 <- lm(dendro[dendro$month==6, 10] ~ dendro[dendro$month==6, 11]) # Jun
summary(rid.6.2805)
rid.7.2805 <- lm(dendro[dendro$month==7, 10] ~ dendro[dendro$month==7, 11]) # Jul
summary(rid.7.2805)
rid.8.2805 <- lm(dendro[dendro$month==8, 10] ~ dendro[dendro$month==8, 11]) # Aug
summary(rid.8.2805)
rid.9.2805 <- lm(dendro[dendro$month==9, 10] ~ dendro[dendro$month==9, 11]) # Sep
summary(rid.9.2805)
rid.10.2805 <- lm(dendro[dendro$month==10, 10] ~ dendro[dendro$month==10, 11]) # Oct
summary(rid.10.2805)
# rid.11.2805 <- lm(dendro[dendro$month==11, 10] ~ dendro[dendro$month==11, 11]) # Nov
# summary(rid.11.2805)
# rid.12.2805 <- lm(dendro[dendro$month==12, 10] ~ dendro[dendro$month==12, 11]) # Dec
# summary(rid.12.2805)

rid.2805 <- data.frame(rbind(coef(rid.1.2805),coef(rid.2.2805),coef(rid.3.2805),coef(rid.4.2805),coef(rid.5.2805),coef(rid.6.2805),
                             coef(rid.7.2805),coef(rid.8.2805),coef(rid.9.2805),coef(rid.10.2805),coef(rid.11.2805),coef(rid.12.2805)))


# rid.2806
# rid.1.2806 <- lm(dendro[dendro$month==1, 11] ~ dendro[dendro$month==1, 10]) # Jan
# summary(rid.1.2806)
# rid.2.2806 <- lm(dendro[dendro$month==2, 11] ~ dendro[dendro$month==2, 10]) # Feb
# summary(rid.2.2806)
# rid.3.2806 <- lm(dendro[dendro$month==3, 11] ~ dendro[dendro$month==3, 10]) # Mar
# summary(rid.3.2806)
# rid.4.2806 <- lm(dendro[dendro$month==4, 11] ~ dendro[dendro$month==4, 10]) # Apr
# summary(rid.4.2806)
# rid.5.2806 <- lm(dendro[dendro$month==5, 11] ~ dendro[dendro$month==5, 10]) # May
# summary(rid.5.2806)
rid.6.2806 <- lm(dendro[dendro$month==6, 11] ~ dendro[dendro$month==6, 10]) # Jun
summary(rid.6.2806)
rid.7.2806 <- lm(dendro[dendro$month==7, 11] ~ dendro[dendro$month==7, 10]) # Jul
summary(rid.7.2806)
rid.8.2806 <- lm(dendro[dendro$month==8, 11] ~ dendro[dendro$month==8, 10]) # Aug
summary(rid.8.2806)
rid.9.2806 <- lm(dendro[dendro$month==9, 11] ~ dendro[dendro$month==9, 10]) # Sep
summary(rid.9.2806)
rid.10.2806 <- lm(dendro[dendro$month==10, 11] ~ dendro[dendro$month==10, 10]) # Oct
summary(rid.10.2806)
# rid.11.2806 <- lm(dendro[dendro$month==11, 11] ~ dendro[dendro$month==11, 10]) # Nov
# summary(rid.11.2806)
# rid.12.2806 <- lm(dendro[dendro$month==12, 11] ~ dendro[dendro$month==12, 10]) # Dec
# summary(rid.12.2806)

rid.2806 <- data.frame(rbind(coef(rid.1.2806),coef(rid.2.2806),coef(rid.3.2806),coef(rid.4.2806),coef(rid.5.2806),coef(rid.6.2806),
                             coef(rid.7.2806),coef(rid.8.2806),coef(rid.9.2806),coef(rid.10.2806),coef(rid.11.2806),coef(rid.12.2806)))


# rid.2807
rid.1.2807 <- lm(dendro[dendro$month==1, 12] ~ dendro[dendro$month==1, 10] + dendro[dendro$month==1, 11]) # Jan
summary(rid.1.2807)
rid.2.2807 <- lm(dendro[dendro$month==2, 12] ~ dendro[dendro$month==2, 10] + dendro[dendro$month==2, 11]) # Feb
summary(rid.2.2807)
rid.3.2807 <- lm(dendro[dendro$month==3, 12] ~ dendro[dendro$month==3, 10] + dendro[dendro$month==3, 11]) # Mar
summary(rid.3.2807)
rid.4.2807 <- lm(dendro[dendro$month==4, 12] ~ dendro[dendro$month==4, 10] + dendro[dendro$month==4, 11]) # Apr
summary(rid.4.2807)
rid.5.2807 <- lm(dendro[dendro$month==5, 12] ~ dendro[dendro$month==5, 10] + dendro[dendro$month==5, 11]) # May
summary(rid.5.2807)
rid.6.2807 <- lm(dendro[dendro$month==6, 12] ~ dendro[dendro$month==6, 10] + dendro[dendro$month==6, 11]) # Jun
summary(rid.6.2807)
rid.7.2807 <- lm(dendro[dendro$month==7, 12] ~ dendro[dendro$month==7, 10] + dendro[dendro$month==7, 11]) # Jul
summary(rid.7.2807)
rid.8.2807 <- lm(dendro[dendro$month==8, 12] ~ dendro[dendro$month==8, 10] + dendro[dendro$month==8, 11]) # Aug
summary(rid.8.2807)
rid.9.2807 <- lm(dendro[dendro$month==9, 12] ~ dendro[dendro$month==9, 10] + dendro[dendro$month==9, 11]) # Sep
summary(rid.9.2807)
rid.10.2807 <- lm(dendro[dendro$month==10, 12] ~ dendro[dendro$month==10, 10] + dendro[dendro$month==10, 11]) # Oct
summary(rid.10.2807)
rid.11.2807 <- lm(dendro[dendro$month==11, 12] ~ dendro[dendro$month==11, 10] + dendro[dendro$month==11, 11]) # Nov
summary(rid.11.2807)
rid.12.2807 <- lm(dendro[dendro$month==12, 12] ~ dendro[dendro$month==12, 10] + dendro[dendro$month==12, 11]) # Dec
summary(rid.12.2807)

rid.2807 <- data.frame(rbind(coef(rid.1.2807),coef(rid.2.2807),coef(rid.3.2807),coef(rid.4.2807),coef(rid.5.2807),coef(rid.6.2807),
                             coef(rid.7.2807),coef(rid.8.2807),coef(rid.9.2807),coef(rid.10.2807),coef(rid.11.2807),coef(rid.12.2807)))


# rid.2808
# rid.1.2808 <- lm(dendro[dendro$month==1, 13] ~ dendro[dendro$month==1, 10] + dendro[dendro$month==1, 11] + dendro[dendro$month==1, 12]) # Jan
# summary(rid.1.2808)
# rid.2.2808 <- lm(dendro[dendro$month==2, 13] ~ dendro[dendro$month==2, 10] + dendro[dendro$month==2, 11] + dendro[dendro$month==2, 12]) # Feb
# summary(rid.2.2808)
# rid.3.2808 <- lm(dendro[dendro$month==3, 13] ~ dendro[dendro$month==3, 10] + dendro[dendro$month==3, 11] + dendro[dendro$month==3, 12]) # Mar
# summary(rid.3.2808)
# rid.4.2808 <- lm(dendro[dendro$month==4, 13] ~ dendro[dendro$month==4, 10] + dendro[dendro$month==4, 11] + dendro[dendro$month==4, 12]) # Apr
# summary(rid.4.2808)
# rid.5.2808 <- lm(dendro[dendro$month==5, 13] ~ dendro[dendro$month==5, 10] + dendro[dendro$month==5, 11] + dendro[dendro$month==5, 12]) # May
# summary(rid.5.2808)
rid.6.2808 <- lm(dendro[dendro$month==6, 13] ~ dendro[dendro$month==6, 10] + dendro[dendro$month==6, 11] + dendro[dendro$month==6, 12]) # Jun
summary(rid.6.2808)
rid.7.2808 <- lm(dendro[dendro$month==7, 13] ~ dendro[dendro$month==7, 10] + dendro[dendro$month==7, 11] + dendro[dendro$month==7, 12]) # Jul
summary(rid.7.2808)
rid.8.2808 <- lm(dendro[dendro$month==8, 13] ~ dendro[dendro$month==8, 10] + dendro[dendro$month==8, 11] + dendro[dendro$month==8, 12]) # Aug
summary(rid.8.2808)
rid.9.2808 <- lm(dendro[dendro$month==9, 13] ~ dendro[dendro$month==9, 10] + dendro[dendro$month==9, 11] + dendro[dendro$month==9, 12]) # Sep
summary(rid.9.2808)
rid.10.2808 <- lm(dendro[dendro$month==10, 13] ~ dendro[dendro$month==10, 10] + dendro[dendro$month==10, 11] + dendro[dendro$month==10, 12]) # Oct
summary(rid.10.2808)
# rid.11.2808 <- lm(dendro[dendro$month==11, 13] ~ dendro[dendro$month==11, 10] + dendro[dendro$month==11, 11] + dendro[dendro$month==11, 12]) # Nov
# summary(rid.11.2808)
# rid.12.2808 <- lm(dendro[dendro$month==12, 13] ~ dendro[dendro$month==12, 10] + dendro[dendro$month==12, 11] + dendro[dendro$month==12, 12]) # Dec
# summary(rid.12.2808)

rid.2808 <- data.frame(rbind(coef(rid.1.2808),coef(rid.2.2808),coef(rid.3.2808),coef(rid.4.2808),coef(rid.5.2808),coef(rid.6.2808),
                             coef(rid.7.2808),coef(rid.8.2808),coef(rid.9.2808),coef(rid.10.2808),coef(rid.11.2808),coef(rid.12.2808)))




# WSU.2801
wsu.1.2801 <- lm(dendro[dendro$month==1, 6] ~ dendro[dendro$month==1, 7] + dendro[dendro$month==1, 8]) # Jan
summary(wsu.1.2801)
wsu.2.2801 <- lm(dendro[dendro$month==2, 6] ~ dendro[dendro$month==2, 7] + dendro[dendro$month==2, 8]) # Feb
summary(wsu.2.2801)
wsu.3.2801 <- lm(dendro[dendro$month==3, 6] ~ dendro[dendro$month==3, 7] + dendro[dendro$month==3, 8]) # Mar
summary(wsu.3.2801)
wsu.4.2801 <- lm(dendro[dendro$month==4, 6] ~ dendro[dendro$month==4, 7] + dendro[dendro$month==4, 8]) # Apr
summary(wsu.4.2801)
wsu.5.2801 <- lm(dendro[dendro$month==5, 6] ~ dendro[dendro$month==5, 7] + dendro[dendro$month==5, 8]) # May
summary(wsu.5.2801)
wsu.6.2801 <- lm(dendro[dendro$month==6, 6] ~ dendro[dendro$month==6, 7] + dendro[dendro$month==6, 8]) # Jun
summary(wsu.6.2801)
wsu.7.2801 <- lm(dendro[dendro$month==7, 6] ~ dendro[dendro$month==7, 7] + dendro[dendro$month==7, 8]) # Jul
summary(wsu.7.2801)
wsu.8.2801 <- lm(dendro[dendro$month==8, 6] ~ dendro[dendro$month==8, 7] + dendro[dendro$month==8, 8]) # Aug
summary(wsu.8.2801)
wsu.9.2801 <- lm(dendro[dendro$month==9, 6] ~ dendro[dendro$month==9, 7] + dendro[dendro$month==9, 8]) # Sep
summary(wsu.9.2801)
wsu.10.2801 <- lm(dendro[dendro$month==10, 6] ~ dendro[dendro$month==10, 7] + dendro[dendro$month==10, 8]) # Oct
summary(wsu.10.2801)
wsu.11.2801 <- lm(dendro[dendro$month==11, 6] ~ dendro[dendro$month==11, 7] + dendro[dendro$month==11, 8]) # Nov
summary(wsu.11.2801)
wsu.12.2801 <- lm(dendro[dendro$month==12, 6] ~ dendro[dendro$month==12, 7] + dendro[dendro$month==12, 8]) # Dec
summary(wsu.12.2801)

wsu.2801 <- data.frame(rbind(coef(wsu.1.2801),coef(wsu.2.2801),coef(wsu.3.2801),coef(wsu.4.2801),coef(wsu.5.2801),coef(wsu.6.2801),
                             coef(wsu.7.2801),coef(wsu.8.2801),coef(wsu.9.2801),coef(wsu.10.2801),coef(wsu.11.2801),coef(wsu.12.2801)))

names(wsu.2801) <- c("b0","wsu.2803","wsu.2804")
rownames(wsu.2801) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(wsu.2801, "~/Desktop/Workspace/Earthwatch/wsu.2801.gap.csv")


# WSU.2802
wsu.1.2802 <- lm(dendro[dendro$month==1, 7] ~ dendro[dendro$month==1, 8] + dendro[dendro$month==1, 9]) # Jan
summary(wsu.1.2802)
wsu.2.2802 <- lm(dendro[dendro$month==2, 7] ~ dendro[dendro$month==2, 8] + dendro[dendro$month==2, 9]) # Feb
summary(wsu.2.2802)
wsu.3.2802 <- lm(dendro[dendro$month==3, 7] ~ dendro[dendro$month==3, 8] + dendro[dendro$month==3, 9]) # Mar
summary(wsu.3.2802)
wsu.4.2802 <- lm(dendro[dendro$month==4, 7] ~ dendro[dendro$month==4, 8] + dendro[dendro$month==4, 9]) # Apr
summary(wsu.4.2802)
wsu.5.2802 <- lm(dendro[dendro$month==5, 7] ~ dendro[dendro$month==5, 8] + dendro[dendro$month==5, 9]) # May
summary(wsu.5.2802)
wsu.6.2802 <- lm(dendro[dendro$month==6, 7] ~ dendro[dendro$month==6, 8] + dendro[dendro$month==6, 9]) # Jun
summary(wsu.6.2802)
wsu.7.2802 <- lm(dendro[dendro$month==7, 7] ~ dendro[dendro$month==7, 8] + dendro[dendro$month==7, 9]) # Jul
summary(wsu.7.2802)
wsu.8.2802 <- lm(dendro[dendro$month==8, 7] ~ dendro[dendro$month==8, 8] + dendro[dendro$month==8, 9]) # Aug
summary(wsu.8.2802)
wsu.9.2802 <- lm(dendro[dendro$month==9, 7] ~ dendro[dendro$month==9, 8] + dendro[dendro$month==9, 9]) # Sep
summary(wsu.9.2802)
wsu.10.2802 <- lm(dendro[dendro$month==10, 7] ~ dendro[dendro$month==10, 8] + dendro[dendro$month==10, 9]) # Oct
summary(wsu.10.2802)
wsu.11.2802 <- lm(dendro[dendro$month==11, 7] ~ dendro[dendro$month==11, 8] + dendro[dendro$month==11, 9]) # Nov
summary(wsu.11.2802)
wsu.12.2802 <- lm(dendro[dendro$month==12, 7] ~ dendro[dendro$month==12, 8] + dendro[dendro$month==12, 9]) # Dec
summary(wsu.12.2802)

wsu.2802 <- data.frame(rbind(coef(wsu.1.2802),coef(wsu.2.2802),coef(wsu.3.2802),coef(wsu.4.2802),coef(wsu.5.2802),coef(wsu.6.2802),
                              coef(wsu.7.2802),coef(wsu.8.2802),coef(wsu.9.2802),coef(wsu.10.2802),coef(wsu.11.2802),coef(wsu.12.2802)))

names(wsu.2802) <- c("b0","wsu.2803","wsu.2804")
rownames(wsu.2802) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(wsu.2802, "~/Desktop/Workspace/Earthwatch/wsu.2802.gap.csv")

# WSU.2803
wsu.1.2803 <- lm(dendro[dendro$month==1, 8] ~ dendro[dendro$month==1, 7] + dendro[dendro$month==1, 9]) # Jan
summary(wsu.1.2803)
wsu.2.2803 <- lm(dendro[dendro$month==2, 8] ~ dendro[dendro$month==2, 7] + dendro[dendro$month==2, 9]) # Feb
summary(wsu.2.2803)
wsu.3.2803 <- lm(dendro[dendro$month==3, 8] ~ dendro[dendro$month==3, 7] + dendro[dendro$month==3, 9]) # Mar
summary(wsu.3.2803)
wsu.4.2803 <- lm(dendro[dendro$month==4, 8] ~ dendro[dendro$month==4, 7] + dendro[dendro$month==4, 9]) # Apr
summary(wsu.4.2803)
wsu.5.2803 <- lm(dendro[dendro$month==5, 8] ~ dendro[dendro$month==5, 7] + dendro[dendro$month==5, 9]) # May
summary(wsu.5.2803)
wsu.6.2803 <- lm(dendro[dendro$month==6, 8] ~ dendro[dendro$month==6, 7] + dendro[dendro$month==6, 9]) # Jun
summary(wsu.6.2803)
wsu.7.2803 <- lm(dendro[dendro$month==7, 8] ~ dendro[dendro$month==7, 7] + dendro[dendro$month==7, 9]) # Jul
summary(wsu.7.2803)
wsu.8.2803 <- lm(dendro[dendro$month==8, 8] ~ dendro[dendro$month==8, 7] + dendro[dendro$month==8, 9]) # Aug
summary(wsu.8.2803)
wsu.9.2803 <- lm(dendro[dendro$month==9, 8] ~ dendro[dendro$month==9, 7] + dendro[dendro$month==9, 9]) # Sep
summary(wsu.9.2803)
wsu.10.2803 <- lm(dendro[dendro$month==10, 8] ~ dendro[dendro$month==10, 7] + dendro[dendro$month==10, 9]) # Oct
summary(wsu.10.2803)
wsu.11.2803 <- lm(dendro[dendro$month==11, 8] ~ dendro[dendro$month==11, 7] + dendro[dendro$month==11, 9]) # Nov
summary(wsu.11.2803)
wsu.12.2803 <- lm(dendro[dendro$month==12, 8] ~ dendro[dendro$month==12, 7] + dendro[dendro$month==12, 9]) # Dec
summary(wsu.12.2803)

wsu.2803 <- data.frame(rbind(coef(wsu.1.2803),coef(wsu.2.2803),coef(wsu.3.2803),coef(wsu.4.2803),coef(wsu.5.2803),coef(wsu.6.2803),
                             coef(wsu.7.2803),coef(wsu.8.2803),coef(wsu.9.2803),coef(wsu.10.2803),coef(wsu.11.2803),coef(wsu.12.2803)))

names(wsu.2803) <- c("b0","wsu.2802","wsu.2804")
rownames(wsu.2803) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(wsu.2803, "~/Desktop/Workspace/Earthwatch/wsu.2803.gap.csv")

# WSU.2804
wsu.1.2804 <- lm(dendro[dendro$month==1, 9] ~ dendro[dendro$month==1, 7] + dendro[dendro$month==1, 8]) # Jan
summary(wsu.1.2804)
wsu.2.2804 <- lm(dendro[dendro$month==2, 9] ~ dendro[dendro$month==2, 7] + dendro[dendro$month==2, 8]) # Feb
summary(wsu.2.2804)
wsu.3.2804 <- lm(dendro[dendro$month==3, 9] ~ dendro[dendro$month==3, 7] + dendro[dendro$month==3, 8]) # Mar
summary(wsu.3.2804)
wsu.4.2804 <- lm(dendro[dendro$month==4, 9] ~ dendro[dendro$month==4, 7] + dendro[dendro$month==4, 8]) # Apr
summary(wsu.4.2804)
wsu.5.2804 <- lm(dendro[dendro$month==5, 9] ~ dendro[dendro$month==5, 7] + dendro[dendro$month==5, 8]) # May
summary(wsu.5.2804)
wsu.6.2804 <- lm(dendro[dendro$month==6, 9] ~ dendro[dendro$month==6, 7] + dendro[dendro$month==6, 8]) # Jun
summary(wsu.6.2804)
wsu.7.2804 <- lm(dendro[dendro$month==7, 9] ~ dendro[dendro$month==7, 7] + dendro[dendro$month==7, 8]) # Jul
summary(wsu.7.2804)
wsu.8.2804 <- lm(dendro[dendro$month==8, 9] ~ dendro[dendro$month==8, 7] + dendro[dendro$month==8, 8]) # Aug
summary(wsu.8.2804)
wsu.9.2804 <- lm(dendro[dendro$month==9, 9] ~ dendro[dendro$month==9, 7] + dendro[dendro$month==9, 8]) # Sep
summary(wsu.9.2804)
wsu.10.2804 <- lm(dendro[dendro$month==10, 9] ~ dendro[dendro$month==10, 7] + dendro[dendro$month==10, 8]) # Oct
summary(wsu.10.2804)
wsu.11.2804 <- lm(dendro[dendro$month==11, 9] ~ dendro[dendro$month==11, 7] + dendro[dendro$month==11, 8]) # Nov
summary(wsu.11.2804)
wsu.12.2804 <- lm(dendro[dendro$month==12, 9] ~ dendro[dendro$month==12, 7] + dendro[dendro$month==12, 8]) # Dec
summary(wsu.12.2804)

wsu.2804 <- data.frame(rbind(coef(wsu.1.2804),coef(wsu.2.2804),coef(wsu.3.2804),coef(wsu.4.2804),coef(wsu.5.2804),coef(wsu.6.2804),
                             coef(wsu.7.2804),coef(wsu.8.2804),coef(wsu.9.2804),coef(wsu.10.2804),coef(wsu.11.2804),coef(wsu.12.2804)))

names(wsu.2804) <- c("b0","wsu.2802","wsu.2803")
rownames(wsu.2804) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
write.csv(wsu.2804, "~/Desktop/Workspace/Earthwatch/wsu.2804.gap.csv")
