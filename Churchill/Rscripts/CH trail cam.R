library(tidyverse)
# library(zoo) # Interpolating snow data
library(imputeTS)  # Interpolating snow data
library(forecast)
library(lubridate)
library(GGally)

rm(list = ls())

ch.snow <- read.csv("~/Desktop/Workspace/Earthwatch/Churchill/data/ch_trail_cam.csv")
ch.snow$date <- ymd(ch.snow$date)
plot(mlk_depth ~ date, ch.snow, type = "l", ylim = c(0,200))
plot(ppa_depth ~ date, ch.snow, type = "l", ylim = c(0,200))
plot(ppd_depth ~ date, ch.snow, type = "l", ylim = c(0,200))
plot(rpp_depth ~ date, ch.snow, type = "l", ylim = c(0,200))
plot(rti_depth ~ date, ch.snow, type = "l", ylim = c(0,200))
plot(tis_depth ~ date, ch.snow, type = "l", ylim = c(0,200))
plot(tun_depth ~ date, ch.snow, type = "l", ylim = c(0,200))
plot(wsu_depth ~ date, ch.snow, type = "l", ylim = c(0,200))

ch.snow$wsu_depth <- na_kalman(ch.snow$wsu_depth)
# ch.snow$ppd_depth[(ch.snow$tun_depth == 0 & !is.na(ch.snow$ppd_depth))] <- 0

## PPD
# Nov-Mar
ppd_depth.1 <- ch.snow %>% filter(!is.na(ppd_depth) & month %in% c(10,11,12,1,2,3))
ppd_depth.1.fit <- lm(ppd_depth ~ wsu_depth, data = ppd_depth.1)
summary(ppd_depth.1.fit) # R2 = 0.5976
ch.snow <- ch.snow %>% 
  mutate(pred = predict(ppd_depth.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(ppd_depth = ifelse((is.na(ppd_depth) & month %in% c(10,11,12,1,2,3)), pred, ppd_depth))
plot(ppd_depth ~ date, ch.snow, type = "l", ylim = c(0,200))

# Nov-Mar
ppd_depth.2 <- ch.snow %>% filter(!is.na(ppd_depth) & month %in% c(4:10))
ppd_depth.2.fit <- lm(ppd_depth ~ wsu_depth, data = ppd_depth.2)
summary(ppd_depth.2.fit) # R2 = 0.7784
ch.snow <- ch.snow %>% 
  mutate(pred = predict(ppd_depth.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(ppd_depth = ifelse((is.na(ppd_depth) & month %in% c(4:10)), pred, ppd_depth))
plot(ppd_depth ~ date, ch.snow, type = "l", ylim = c(0,200))

##
ch_snow1 <- ch.snow %>%
  select(-c(id, year, month, day)) %>%
  pivot_longer(cols = ends_with("depth"), names_to = "site", values_to = "depth") %>%
  mutate(date = ymd(date)) %>%
  arrange(site,date) %>%
  group_by(site) %>% 
  do(na_kalman(.)) %>% ungroup

ch_snow1 %>%
  filter(site == "wsu_depth") %>% 
  ggplot(aes(x=date, y=tsclean(depth), group=site, color = site)) +
  geom_line() + 
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y-%b") +
  theme_bw()

# mlk_depth
plot(depth ~ date, ch_snow1[ch_snow1$site == "mlk_depth",], type = "l", ylim = c(0,200))
plot(depth ~ date, ch_snow1[ch_snow1$site == "ppa_depth",], type = "l", ylim = c(0,200))
plot(depth ~ date, ch_snow1[ch_snow1$site == "ppd_depth",], type = "l", ylim = c(0,50))
plot(depth ~ date, ch_snow1[ch_snow1$site == "rpp_depth",], type = "l", ylim = c(0,200))
plot(depth ~ date, ch_snow1[ch_snow1$site == "rti_depth",], type = "l", ylim = c(0,300))
plot(depth ~ date, ch_snow1[ch_snow1$site == "tis_depth",], type = "l", ylim = c(0,300))
plot(depth ~ date, ch_snow1[ch_snow1$site == "tun_depth",], type = "l", ylim = c(0,20))
plot(depth ~ date, ch_snow1[ch_snow1$site == "wsu_depth",], type = "l", ylim = c(0,200))

# ch_snow1 %>% 
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



ch_snow1$date <- ymd(ch_snow1$date)
ch_snow1$depth[ch_snow1$site == "mlk_depth" & ch_snow1$date > "2018-10-30"] <- NA
ch_snow1$depth[ch_snow1$site == "ppa_depth" & ch_snow1$date < "2016-02-26"] <- NA
ch_snow1$depth[ch_snow1$site == "rpp_depth" & (ch_snow1$date < "2016-02-26" | ch_snow1$date > "2020-01-09")] <- NA
ch_snow1$depth[ch_snow1$site == "rti_depth" & (ch_snow1$date %in% seq.Date(ymd("2015-03-15"),ymd("2015-04-20"), by = "1 day"))] <- NA
ch_snow1$depth[ch_snow1$site == "rti_depth" & 
                 (ch_snow1$date %in% seq.Date(ymd("2018-09-19"),ymd("2019-10-22"), by = "1 day"))] <- NA
ch_snow1$depth[ch_snow1$site == "rti_depth" & ch_snow1$date > "2020-01-09"] <- NA
ch_snow1$depth[ch_snow1$site == "tis_depth" & ch_snow1$date < "2016-02-26"] <- NA
ch_snow1$depth[ch_snow1$site == "tis_depth" & (ch_snow1$date %in% seq.Date(ymd("2020-04-15"),ymd("2020-09-27"), by = "1 day"))] <- NA

ch_snow2 <- ch_snow1 %>%
  pivot_wider(
    .,
    # id_cols = c(site, date),
    names_from = site,
    values_from = depth
  ) %>%
  mutate(month = month(date)) %>%
  relocate(., month, .before = mlk_depth)

## MLK
# Nov-Mar
mlk_depth.1 <- ch_snow2 %>% filter(!is.na(mlk_depth) & month %in% c(10,11,12,1,2,3))
mlk_depth.1.fit <- lm(mlk_depth ~ wsu_depth, data = mlk_depth.1)
summary(mlk_depth.1.fit) # R2 = 0.5288
ch_snow2 <- ch_snow2 %>% 
  mutate(pred = predict(mlk_depth.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(mlk_depth = ifelse((is.na(mlk_depth) & month %in% c(10,11,12,1,2,3)), pred, mlk_depth))
plot(mlk_depth ~ date, ch_snow2, type = "l", ylim = c(0,200))

# Nov-Mar
mlk_depth.2 <- ch_snow2 %>% filter(!is.na(mlk_depth) & month %in% c(4:10))
mlk_depth.2.fit <- lm(mlk_depth ~ wsu_depth, data = mlk_depth.2)
summary(mlk_depth.2.fit) # R2 = 0.7255
ch_snow2 <- ch_snow2 %>% 
  mutate(pred = predict(mlk_depth.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(mlk_depth = ifelse((is.na(mlk_depth) & month %in% c(4:10)), pred, mlk_depth))
ch_snow2$mlk_depth[ch_snow2$mlk_depth < 0] <- 0
plot(mlk_depth ~ date, ch_snow2, type = "l", ylim = c(0,200))

## PPA
# Nov-Mar
ppa_depth.1 <- ch_snow2 %>% filter(!is.na(ppa_depth) & month %in% c(10,11,12,1,2,3))
ppa_depth.1.fit <- lm(ppa_depth ~ wsu_depth, data = ppa_depth.1)
summary(ppa_depth.1.fit) # R2 = 0.165
ch_snow2 <- ch_snow2 %>% 
  mutate(pred = predict(ppa_depth.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(ppa_depth = ifelse((is.na(ppa_depth) & month %in% c(10,11,12,1,2,3)), pred, ppa_depth))
plot(ppa_depth ~ date, ch_snow2, type = "l", ylim = c(0,200))

# Nov-Mar
ppa_depth.2 <- ch_snow2 %>% filter(!is.na(ppa_depth) & month %in% c(4:10))
ppa_depth.2.fit <- lm(ppa_depth ~ wsu_depth, data = ppa_depth.2)
summary(ppa_depth.2.fit) # R2 = 0.41
ch_snow2 <- ch_snow2 %>% 
  mutate(pred = predict(ppa_depth.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(ppa_depth = ifelse((is.na(ppa_depth) & month %in% c(4:10)), pred, ppa_depth))
ch_snow2$ppa_depth[ch_snow2$ppa_depth < 0] <- 0
plot(ppa_depth ~ date, ch_snow2, type = "l", ylim = c(0,200))

## RPP
# Nov-Mar
rpp_depth.1 <- ch_snow2 %>% filter(!is.na(rpp_depth) & month %in% c(10,11,12,1,2,3))
rpp_depth.1.fit <- lm(rpp_depth ~ wsu_depth, data = rpp_depth.1)
summary(rpp_depth.1.fit) # R2 = 0.5925
ch_snow2 <- ch_snow2 %>% 
  mutate(pred = predict(rpp_depth.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(rpp_depth = ifelse((is.na(rpp_depth) & month %in% c(10,11,12,1,2,3)), pred, rpp_depth))
plot(rpp_depth ~ date, ch_snow2, type = "l", ylim = c(0,200))

# Nov-Mar
rpp_depth.2 <- ch_snow2 %>% filter(!is.na(rpp_depth) & month %in% c(4:10))
rpp_depth.2.fit <- lm(rpp_depth ~ wsu_depth, data = rpp_depth.2)
summary(rpp_depth.2.fit) # R2 = 0.6609
ch_snow2 <- ch_snow2 %>% 
  mutate(pred = predict(rpp_depth.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(rpp_depth = ifelse((is.na(rpp_depth) & month %in% c(4:10)), pred, rpp_depth))
ch_snow2$rpp_depth[ch_snow2$rpp_depth < 0] <- 0
plot(rpp_depth ~ date, ch_snow2, type = "l", ylim = c(0,200))

## RTI
# Nov-Mar
rti_depth.1 <- ch_snow2 %>% filter(!is.na(rti_depth) & month %in% c(10,11,12,1,2,3))
rti_depth.1.fit <- lm(rti_depth ~ wsu_depth, data = rti_depth.1)
summary(rti_depth.1.fit) # R2 = 0.7158
ch_snow2 <- ch_snow2 %>% 
  mutate(pred = predict(rti_depth.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(rti_depth = ifelse((is.na(rti_depth) & month %in% c(10,11,12,1,2,3)), pred, rti_depth))
plot(rti_depth ~ date, ch_snow2, type = "l", ylim = c(0,200))

# Nov-Mar
rti_depth.2 <- ch_snow2 %>% filter(!is.na(rti_depth) & month %in% c(4:10))
rti_depth.2.fit <- lm(rti_depth ~ wsu_depth, data = rti_depth.2)
summary(rti_depth.2.fit) # R2 = 0.7422
ch_snow2 <- ch_snow2 %>% 
  mutate(pred = predict(rti_depth.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(rti_depth = ifelse((is.na(rti_depth) & month %in% c(4:10)), pred, rti_depth))
ch_snow2$rti_depth[ch_snow2$rti_depth < 0] <- 0
plot(rti_depth ~ date, ch_snow2, type = "l", ylim = c(0,200))

## TIS
# Nov-Mar
tis_depth.1 <- ch_snow2 %>% filter(!is.na(tis_depth) & month %in% c(10,11,12,1,2,3))
tis_depth.1.fit <- lm(tis_depth ~ wsu_depth, data = tis_depth.1)
summary(tis_depth.1.fit) # R2 = 0.7536
ch_snow2 <- ch_snow2 %>% 
  mutate(pred = predict(tis_depth.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tis_depth = ifelse((is.na(tis_depth) & month %in% c(10,11,12,1,2,3)), pred, tis_depth))
plot(tis_depth ~ date, ch_snow2, type = "l", ylim = c(0,200))

# Nov-Mar
tis_depth.2 <- ch_snow2 %>% filter(!is.na(tis_depth) & month %in% c(4:10))
tis_depth.2.fit <- lm(tis_depth ~ wsu_depth, data = tis_depth.2)
summary(tis_depth.2.fit) # R2 = 0.6329
ch_snow2 <- ch_snow2 %>% 
  mutate(pred = predict(tis_depth.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tis_depth = ifelse((is.na(tis_depth) & month %in% c(4:10)), pred, tis_depth))
ch_snow2$tis_depth[ch_snow2$tis_depth < 0] <- 0
ch_snow2$date <- ymd(ch_snow2$date)
ch_snow2$tis_depth[(ch_snow2$date %in% seq.Date(ymd("2015-05-21"),
                                              ymd("2015-09-30"), by = "1 day"))|
                   (ch_snow2$date %in% seq.Date(ymd("2020-06-03"),
                                                ymd("2020-09-27"), by = "1 day"))] <- 0
plot(tis_depth ~ date, ch_snow2, type = "l", ylim = c(0,300))

ch_snow2 %>%
  select(-c(month)) %>%
  pivot_longer(cols = ends_with("depth"), names_to = "site", values_to = "Depth") %>%
  mutate(date = ymd(date)) %>%
  arrange(site,date) %>%
  ggplot(aes(x=date, y=Depth, group=site, color = site)) +
  geom_line() + 
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y-%b") +
  theme_bw()

ch_snow3 <- ch_snow2 %>%
  rowwise() %>%
  mutate(tundra = tun_depth,
         polygon = mean(c(ppa_depth, ppd_depth,rpp_depth)),
         island = mean(c(rti_depth, tis_depth)),
         forest = mean(c(mlk_depth, wsu_depth))) %>%
  select(-c(mlk_depth, ppa_depth, ppd_depth, rpp_depth, rti_depth, tis_depth, tun_depth, wsu_depth, pred))


ch_snow3$island[c(2016:2042)] <- ch_snow3$island[c(2016:2042)]-(ch_snow3$island[c(2016:2042)]*0.2)

ch_snow3$tundra[ch_snow3$month %in% c(6:10)] <- 0
ch_snow3$polygon[ch_snow3$month %in% c(6:9)] <- 0
ch_snow3$forest[ch_snow3$month %in% c(7:9)] <- 0
ch_snow3$island[ch_snow3$month %in% c(7:9)] <- 0

# polygon: "#F21A00"
# tundra: "#EB5500"
# forest: "#8FBBA5"
# island: "#53A5B9"

min_date <- min(ch_snow3$date)
min_year <- year(min_date)
min_month <- sprintf("%02d", month(min_date))
min_day <- sprintf("%02d", day(min_date))
max_date <- max(ch_snow3$date)
max_year <- year(max_date)
max_month <- sprintf("%02d", month(max_date))
max_day <- sprintf("%02d", day(max_date))
save_path <- sprintf("~/Desktop/Workspace/Earthwatch/Churchill/figures/Snow_cams_%s%s%s_%s%s%s.jpg",
                     min_year,min_month,min_day,max_year,max_month,max_day)

jpeg(save_path, height = 4, width = 5, units = "in", res = 600)
par(mar = c(3.2,3.2,0.1,0.1))
plot(ch_snow3$date, ch_snow3$island, type="n" , xlab="", ylab="", xaxt='n',
     xlim=as.Date(c("2014-09-27", "2020-06-15")))
polygon(c(min(ch_snow3$date), ch_snow3$date , max(ch_snow3$date)), 
        c(min(ch_snow3$island), ch_snow3$island, min(ch_snow3$island)), 
        col="#53A5B9" , border=F)
polygon(c(min(ch_snow3$date), ch_snow3$date , max(ch_snow3$date)), 
        c(min(ch_snow3$forest), ch_snow3$forest, min(ch_snow3$forest)), 
        col="#8FBBA5" , border=F)
polygon(c(min(ch_snow3$date), ch_snow3$date , max(ch_snow3$date)), 
        c(min(ch_snow3$polygon), ch_snow3$polygon, min(ch_snow3$polygon)), 
        col="#F21A00" , border=F)
polygon(c(min(ch_snow3$date), ch_snow3$date , max(ch_snow3$date)), 
        c(min(ch_snow3$tundra), ch_snow3$tundra, min(ch_snow3$tundra)), 
        col="#EB5500" , border=F)
axis(1, at = c(seq(as_date("2014-01-01"), as_date("2020-01-01"), "year")), labels = 2014:2020)
legend("topright", legend = c("Tundra","Polygon","Forest","Island"), pch = 15, bty = "n",
       col = c("#EB5500","#F21A00","#8FBBA5","#53A5B9"), y.intersp = 0.75, cex = 0.95)
mtext("Date", side = 1, line = 2)
mtext("Snow depth (cm)", side = 2, line = 2.2)
text(x = as.Date("2016-02-15"), y = 155, "El Niño")
text(x = as.Date("2017-12-15"), y = 255, "La Niña")
dev.off()
