library(tidyverse)
# library(zoo) # Interpolating snow data
library(imputeTS)  # Interpolating snow data
library(lubridate)
library(GGally)

rm(list = ls())

ch.snow <- read.csv("~/Desktop/Workspace/Earthwatch/Churchill/data/ch_trail_cam.csv")
ch.snow$date <- ymd(ch.snow$date)
plot(mlk.cam ~ date, ch.snow, type = "l", ylim = c(0,200))
plot(ppa.cam ~ date, ch.snow, type = "l", ylim = c(0,200))
plot(ppd.cam ~ date, ch.snow, type = "l", ylim = c(0,200))
plot(rpp.cam ~ date, ch.snow, type = "l", ylim = c(0,200))
plot(rti.cam ~ date, ch.snow, type = "l", ylim = c(0,200))
plot(tis.cam ~ date, ch.snow, type = "l", ylim = c(0,200))
plot(tun.cam ~ date, ch.snow, type = "l", ylim = c(0,200))
plot(wsu.cam ~ date, ch.snow, type = "l", ylim = c(0,200))

ch.snow$wsu.cam <- na_kalman(ch.snow$wsu.cam)
ch.snow$ppd.cam[(ch.snow$tun.cam == 0 & !is.na(ch.snow$ppd.cam))] <- 0

## PPD
# Nov-Mar
ppd.cam.1 <- ch.snow %>% filter(!is.na(ppd.cam) & month %in% c(10,11,12,1,2,3))
ppd.cam.1.fit <- lm(ppd.cam ~ wsu.cam, data = ppd.cam.1)
summary(ppd.cam.1.fit) # R2 = 0.5976
ch.snow <- ch.snow %>% 
  mutate(pred = predict(ppd.cam.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(ppd.cam = ifelse((is.na(ppd.cam) & month %in% c(10,11,12,1,2,3)), pred, ppd.cam))
plot(ppd.cam ~ date, ch.snow, type = "l", ylim = c(0,200))

# Nov-Mar
ppd.cam.2 <- ch.snow %>% filter(!is.na(ppd.cam) & month %in% c(4:10))
ppd.cam.2.fit <- lm(ppd.cam ~ wsu.cam, data = ppd.cam.2)
summary(ppd.cam.2.fit) # R2 = 0.7784
ch.snow <- ch.snow %>% 
  mutate(pred = predict(ppd.cam.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(ppd.cam = ifelse((is.na(ppd.cam) & month %in% c(4:10)), pred, ppd.cam))
plot(ppd.cam ~ date, ch.snow, type = "l", ylim = c(0,200))

##
ch.snow1 <- ch.snow %>%
  select(-c(id, year, month, day)) %>%
  pivot_longer(cols = ends_with("cam"), names_to = "site", values_to = "depth") %>%
  mutate(date = ymd(date)) %>%
  arrange(site,date) %>%
  group_by(site) %>% 
  do(na_kalman(.)) %>% ungroup

ch.snow1 %>%
  ggplot(aes(x=date, y=depth, group=site, color = site)) +
  geom_line() + 
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y-%b") +
  theme_bw()

plot(depth ~ date, ch.snow1[ch.snow1$site == "mlk.cam",], type = "l", ylim = c(0,200))
plot(depth ~ date, ch.snow1[ch.snow1$site == "ppa.cam",], type = "l", ylim = c(0,200))
plot(depth ~ date, ch.snow1[ch.snow1$site == "ppd.cam",], type = "l", ylim = c(0,50))
plot(depth ~ date, ch.snow1[ch.snow1$site == "rpp.cam",], type = "l", ylim = c(0,200))
plot(depth ~ date, ch.snow1[ch.snow1$site == "rti.cam",], type = "l", ylim = c(0,300))
plot(depth ~ date, ch.snow1[ch.snow1$site == "tis.cam",], type = "l", ylim = c(0,300))
plot(depth ~ date, ch.snow1[ch.snow1$site == "tun.cam",], type = "l", ylim = c(0,20))
plot(depth ~ date, ch.snow1[ch.snow1$site == "wsu.cam",], type = "l", ylim = c(0,200))

# ch.snow1 %>% 
#   spread(site, depth) %>% 
#   select(-date) %>% 
#   ggpairs() 

# Construct linear model based on non-NA pairs
# There is snow in every month except July

# mlk post-2018-10-30 regress
# ppa pre-2016-02-26
# rpp pre-2015-06-13, post-2020-02-01
# rti post-2020-02-01
# tis pre-2015-06-15, post-2020-05-21

`%notin%` <- Negate(`%in%`)



ch.snow1$date <- ymd(ch.snow1$date)
ch.snow1$depth[ch.snow1$site == "mlk.cam" & ch.snow1$date > "2018-10-30"] <- NA
ch.snow1$depth[ch.snow1$site == "ppa.cam" & ch.snow1$date < "2016-02-26"] <- NA
ch.snow1$depth[ch.snow1$site == "rpp.cam" & (ch.snow1$date < "2016-02-26" | ch.snow1$date > "2020-01-09")] <- NA
ch.snow1$depth[ch.snow1$site == "rti.cam" & (ch.snow1$date %in% seq.Date(ymd("2015-03-15"),ymd("2015-04-20"), by = "1 day"))] <- NA
ch.snow1$depth[ch.snow1$site == "rti.cam" & 
                 (ch.snow1$date %in% seq.Date(ymd("2018-09-19"),ymd("2019-10-22"), by = "1 day"))] <- NA
ch.snow1$depth[ch.snow1$site == "rti.cam" & ch.snow1$date > "2020-01-09"] <- NA
ch.snow1$depth[ch.snow1$site == "tis.cam" & ch.snow1$date < "2016-02-26"] <- NA
ch.snow1$depth[ch.snow1$site == "tis.cam" & (ch.snow1$date %in% seq.Date(ymd("2020-04-15"),ymd("2020-09-27"), by = "1 day"))] <- NA

ch.snow2 <- ch.snow1 %>%
  pivot_wider(
    .,
    # id_cols = c(site, date),
    names_from = site,
    values_from = depth
  ) %>%
  mutate(month = month(date)) %>%
  relocate(., month, .before = mlk.cam)

## MLK
# Nov-Mar
mlk.cam.1 <- ch.snow2 %>% filter(!is.na(mlk.cam) & month %in% c(10,11,12,1,2,3))
mlk.cam.1.fit <- lm(mlk.cam ~ wsu.cam, data = mlk.cam.1)
summary(mlk.cam.1.fit) # R2 = 0.5288
ch.snow2 <- ch.snow2 %>% 
  mutate(pred = predict(mlk.cam.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(mlk.cam = ifelse((is.na(mlk.cam) & month %in% c(10,11,12,1,2,3)), pred, mlk.cam))
plot(mlk.cam ~ date, ch.snow2, type = "l", ylim = c(0,200))

# Nov-Mar
mlk.cam.2 <- ch.snow2 %>% filter(!is.na(mlk.cam) & month %in% c(4:10))
mlk.cam.2.fit <- lm(mlk.cam ~ wsu.cam, data = mlk.cam.2)
summary(mlk.cam.2.fit) # R2 = 0.7255
ch.snow2 <- ch.snow2 %>% 
  mutate(pred = predict(mlk.cam.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(mlk.cam = ifelse((is.na(mlk.cam) & month %in% c(4:10)), pred, mlk.cam))
ch.snow2$mlk.cam[ch.snow2$mlk.cam < 0] <- 0
plot(mlk.cam ~ date, ch.snow2, type = "l", ylim = c(0,200))

## PPA
# Nov-Mar
ppa.cam.1 <- ch.snow2 %>% filter(!is.na(ppa.cam) & month %in% c(10,11,12,1,2,3))
ppa.cam.1.fit <- lm(ppa.cam ~ wsu.cam, data = ppa.cam.1)
summary(ppa.cam.1.fit) # R2 = 0.165
ch.snow2 <- ch.snow2 %>% 
  mutate(pred = predict(ppa.cam.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(ppa.cam = ifelse((is.na(ppa.cam) & month %in% c(10,11,12,1,2,3)), pred, ppa.cam))
plot(ppa.cam ~ date, ch.snow2, type = "l", ylim = c(0,200))

# Nov-Mar
ppa.cam.2 <- ch.snow2 %>% filter(!is.na(ppa.cam) & month %in% c(4:10))
ppa.cam.2.fit <- lm(ppa.cam ~ wsu.cam, data = ppa.cam.2)
summary(ppa.cam.2.fit) # R2 = 0.41
ch.snow2 <- ch.snow2 %>% 
  mutate(pred = predict(ppa.cam.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(ppa.cam = ifelse((is.na(ppa.cam) & month %in% c(4:10)), pred, ppa.cam))
ch.snow2$ppa.cam[ch.snow2$ppa.cam < 0] <- 0
plot(ppa.cam ~ date, ch.snow2, type = "l", ylim = c(0,200))

## RPP
# Nov-Mar
rpp.cam.1 <- ch.snow2 %>% filter(!is.na(rpp.cam) & month %in% c(10,11,12,1,2,3))
rpp.cam.1.fit <- lm(rpp.cam ~ wsu.cam, data = rpp.cam.1)
summary(rpp.cam.1.fit) # R2 = 0.5925
ch.snow2 <- ch.snow2 %>% 
  mutate(pred = predict(rpp.cam.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(rpp.cam = ifelse((is.na(rpp.cam) & month %in% c(10,11,12,1,2,3)), pred, rpp.cam))
plot(rpp.cam ~ date, ch.snow2, type = "l", ylim = c(0,200))

# Nov-Mar
rpp.cam.2 <- ch.snow2 %>% filter(!is.na(rpp.cam) & month %in% c(4:10))
rpp.cam.2.fit <- lm(rpp.cam ~ wsu.cam, data = rpp.cam.2)
summary(rpp.cam.2.fit) # R2 = 0.6609
ch.snow2 <- ch.snow2 %>% 
  mutate(pred = predict(rpp.cam.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(rpp.cam = ifelse((is.na(rpp.cam) & month %in% c(4:10)), pred, rpp.cam))
ch.snow2$rpp.cam[ch.snow2$rpp.cam < 0] <- 0
plot(rpp.cam ~ date, ch.snow2, type = "l", ylim = c(0,200))

## RTI
# Nov-Mar
rti.cam.1 <- ch.snow2 %>% filter(!is.na(rti.cam) & month %in% c(10,11,12,1,2,3))
rti.cam.1.fit <- lm(rti.cam ~ wsu.cam, data = rti.cam.1)
summary(rti.cam.1.fit) # R2 = 0.7158
ch.snow2 <- ch.snow2 %>% 
  mutate(pred = predict(rti.cam.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(rti.cam = ifelse((is.na(rti.cam) & month %in% c(10,11,12,1,2,3)), pred, rti.cam))
plot(rti.cam ~ date, ch.snow2, type = "l", ylim = c(0,200))

# Nov-Mar
rti.cam.2 <- ch.snow2 %>% filter(!is.na(rti.cam) & month %in% c(4:10))
rti.cam.2.fit <- lm(rti.cam ~ wsu.cam, data = rti.cam.2)
summary(rti.cam.2.fit) # R2 = 0.7422
ch.snow2 <- ch.snow2 %>% 
  mutate(pred = predict(rti.cam.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(rti.cam = ifelse((is.na(rti.cam) & month %in% c(4:10)), pred, rti.cam))
ch.snow2$rti.cam[ch.snow2$rti.cam < 0] <- 0
plot(rti.cam ~ date, ch.snow2, type = "l", ylim = c(0,200))

## TIS
# Nov-Mar
tis.cam.1 <- ch.snow2 %>% filter(!is.na(tis.cam) & month %in% c(10,11,12,1,2,3))
tis.cam.1.fit <- lm(tis.cam ~ wsu.cam, data = tis.cam.1)
summary(tis.cam.1.fit) # R2 = 0.7536
ch.snow2 <- ch.snow2 %>% 
  mutate(pred = predict(tis.cam.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tis.cam = ifelse((is.na(tis.cam) & month %in% c(10,11,12,1,2,3)), pred, tis.cam))
plot(tis.cam ~ date, ch.snow2, type = "l", ylim = c(0,200))

# Nov-Mar
tis.cam.2 <- ch.snow2 %>% filter(!is.na(tis.cam) & month %in% c(4:10))
tis.cam.2.fit <- lm(tis.cam ~ wsu.cam, data = tis.cam.2)
summary(tis.cam.2.fit) # R2 = 0.6329
ch.snow2 <- ch.snow2 %>% 
  mutate(pred = predict(tis.cam.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tis.cam = ifelse((is.na(tis.cam) & month %in% c(4:10)), pred, tis.cam))
ch.snow2$tis.cam[ch.snow2$tis.cam < 0] <- 0
ch.snow2$date <- ymd(ch.snow2$date)
ch.snow2$tis.cam[(ch.snow2$date %in% seq.Date(ymd("2015-05-21"),
                                              ymd("2015-09-30"), by = "1 day"))|
                   (ch.snow2$date %in% seq.Date(ymd("2020-06-03"),
                                                ymd("2020-09-27"), by = "1 day"))] <- 0
plot(tis.cam ~ date, ch.snow2, type = "l", ylim = c(0,300))

ch.snow2 %>%
  select(-c(month)) %>%
  pivot_longer(cols = ends_with("cam"), names_to = "site", values_to = "Depth") %>%
  mutate(date = ymd(date)) %>%
  arrange(site,date) %>%
  ggplot(aes(x=date, y=Depth, group=site, color = site)) +
  geom_line() + 
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y-%b") +
  theme_bw()

ch.snow3 <- ch.snow2 %>%
  rowwise() %>%
  mutate(tundra = tun.cam,
         polygon = mean(c(ppa.cam, ppd.cam,rpp.cam)),
         island = mean(c(rti.cam, tis.cam)),
         forest = mean(c(mlk.cam, wsu.cam))) %>%
  select(-c(mlk.cam, ppa.cam, ppd.cam, rpp.cam, rti.cam, tis.cam, tun.cam, wsu.cam, pred))


ch.snow3$island[c(2016:2042)] <- ch.snow3$island[c(2016:2042)]-(ch.snow3$island[c(2016:2042)]*0.2)

ch.snow3$tundra[ch.snow3$month %in% c(6:10)] <- 0
ch.snow3$polygon[ch.snow3$month %in% c(6:9)] <- 0
ch.snow3$forest[ch.snow3$month %in% c(7:9)] <- 0
ch.snow3$island[ch.snow3$month %in% c(7:9)] <- 0

# polygon: "#F21A00"
# tundra: "#EB5500"
# forest: "#8FBBA5"
# island: "#53A5B9"


jpeg("~/Desktop/Workspace/Earthwatch/Churchill/figures/Snow_cams.jpg", height = 4, width = 5, units = "in", res = 600)
par(mar = c(3.2,3.2,0.1,0.1))
plot(ch.snow3$date, ch.snow3$island, type="n" , xlab="", ylab="", 
     xlim=as.Date(c("2014-09-27", "2020-06-15")))
polygon(c(min(ch.snow3$date), ch.snow3$date , max(ch.snow3$date)), 
        c(min(ch.snow3$island), ch.snow3$island, min(ch.snow3$island)), 
        col="#53A5B9" , border=F)
polygon(c(min(ch.snow3$date), ch.snow3$date , max(ch.snow3$date)), 
        c(min(ch.snow3$forest), ch.snow3$forest, min(ch.snow3$forest)), 
        col="#8FBBA5" , border=F)
polygon(c(min(ch.snow3$date), ch.snow3$date , max(ch.snow3$date)), 
        c(min(ch.snow3$polygon), ch.snow3$polygon, min(ch.snow3$polygon)), 
        col="#F21A00" , border=F)
polygon(c(min(ch.snow3$date), ch.snow3$date , max(ch.snow3$date)), 
        c(min(ch.snow3$tundra), ch.snow3$tundra, min(ch.snow3$tundra)), 
        col="#EB5500" , border=F)
legend("topright", legend = c("Tundra","Polygon","Forest","Island"), pch = 15, bty = "n",
       col = c("#EB5500","#F21A00","#8FBBA5","#53A5B9"), y.intersp = 0.75, cex = 0.95)
mtext("Date", side = 1, line = 2)
mtext("Snow depth (cm)", side = 2, line = 2.2)
text(x = as.Date("2016-02-15"), y = 155, "El Niño")
text(x = as.Date("2017-12-15"), y = 255, "La Niña")
dev.off()
