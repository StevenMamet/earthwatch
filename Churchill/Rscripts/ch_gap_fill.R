# install.packages("weathercan", 
#                  repos = c("https://ropensci.r-universe.dev", 
#                            "https://cloud.r-project.org"))

library(tidyverse)
library(splines)
library(psych)    # Pairs plot
library(lubridate)
library(dplyr)
library(scales)
library(vegan)
library(dplR)
library(forecast)
library(purrr)
library(weathercan) # Get weather station details from Environment Canada
library(patchwork)
library(wesanderson)

rm(list=ls())

col_pal <- wes_palette("Darjeeling1")

setwd("~/Desktop/Workspace")

# Churchill met station data
churchill <- read.csv("./Earthwatch/Churchill/data/ch_microclimate.csv", header = TRUE)

# Converts to date format (lubridate)
names(churchill)[1] <- "Date"
churchill$Date <- ymd(churchill$Date)
names(churchill)[3] <- "month"

churchill <- churchill %>% 
  mutate(pfr150 = as.numeric(pfr150),
         pfrneg80 = as.numeric(pfrneg80))

p1 <- churchill %>% 
  ggplot(aes(x = Date, y = airp150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = airp0), color = col_pal[3]) +
  geom_line(aes(y = airpneg80), color = col_pal[2])

p2 <- churchill %>% 
  ggplot(aes(x = Date, y = bfr150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = bfr0), color = col_pal[3]) +
  geom_line(aes(y = bfrneg80), color = col_pal[2])

p3 <- churchill %>% 
  ggplot(aes(x = Date, y = bsw150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = bsw0), color = col_pal[3]) +
  geom_line(aes(y = bswneg80), color = col_pal[2])

p4 <- churchill %>% 
  ggplot(aes(x = Date, y = bwp150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = bwp0), color = col_pal[3]) +
  geom_line(aes(y = bwpneg80), color = col_pal[2])

p5 <- churchill %>% 
  ggplot(aes(x = Date, y = fen150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = fen0), color = col_pal[3]) +
  geom_line(aes(y = fenneg80), color = col_pal[2])

p6 <- churchill %>% 
  ggplot(aes(x = Date, y = mlk150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = mlk0), color = col_pal[3]) +
  geom_line(aes(y = mlkneg80), color = col_pal[2])

p7 <- churchill %>% 
  ggplot(aes(x = Date, y = pfr150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = pfr0), color = col_pal[3]) +
  geom_line(aes(y = pfrneg80), color = col_pal[2])

p8 <- churchill %>% 
  ggplot(aes(x = Date, y = ppa150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = ppa0), color = col_pal[3]) +
  geom_line(aes(y = ppaneg80), color = col_pal[2])

p9 <- churchill %>% 
  ggplot(aes(x = Date, y = ppd150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = ppd0), color = col_pal[3]) +
  geom_line(aes(y = ppdneg80), color = col_pal[2])

p10 <- churchill %>% 
  ggplot(aes(x = Date, y = rlk150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = rlkneg5), color = col_pal[3]) +
  geom_line(aes(y = rlkneg80), color = col_pal[2])

p11 <- churchill %>% 
  ggplot(aes(x = Date, y = tis150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = tis0), color = col_pal[3]) +
  geom_line(aes(y = tisneg80), color = col_pal[2])

p12 <- churchill %>% 
  ggplot(aes(x = Date, y = tun150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = tun0), color = col_pal[3]) +
  geom_line(aes(y = tunneg80), color = col_pal[2])

p13 <- churchill %>% 
  ggplot(aes(x = Date, y = wsu150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = wsu0), color = col_pal[3]) +
  geom_line(aes(y = wsuneg80), color = col_pal[2])

p13

# check most recent summer of air temperatures for RLK
# check most recent two summers of -80 for WSU
churchill <-
  churchill %>% 
  mutate(rlk150 = ifelse(Date > "2022-03-01", NA, rlk150),
         wsuneg80 = ifelse(Date > "2021-01-01", NA, wsuneg80))# %>% 
  # ggplot(aes(x = Date, y = wsuneg80)) + geom_line()


## ********************
## ********************
# Step 1: Environment Canada data ----
# Get the correct station ID, make a df using that ID and the time period of interest, and download
# stations_dl() # re-download/update stations data
# Need to download two records to get the coverage I need
# station_id1 <- suppressMessages(as.numeric(stations_search("CHURCHILL A", dist = 50, interval = "day")[1,3]))
# station_id2 <- suppressMessages(as.numeric(stations_search("CHURCHILL A", dist = 50, interval = "day")[3,3]))
# # Now read in the data and tidy it up. Need to choose time period to cover
# start_date <- min(churchill$Date)
# end_date <- max(churchill$Date)
# w_df1 <- weather_dl(station_ids = station_id1, start = start_date, end = end_date)
# weather_df1 <- w_df1 %>%
#   rename(station_temp = temp,
#          datetime = time,
#          dew_point = temp_dew,
#          rel_humid = rel_hum,
#          precip = precip_amt) %>%
#   select(datetime, station_temp, datetime, dew_point, rel_humid, precip, pressure) %>%
#   mutate(datetime = as_datetime(datetime, tz="Canada/Central")) %>%
#   filter(datetime > start_date & datetime <= end_date) %>%
#   mutate(station_temp = tsclean(station_temp),
#          rel_humid = tsclean(rel_humid),
#          pressure = tsclean(pressure))
# w_df2 <- weather_dl(station_ids = station_id2, start = start_date, end = end_date)
# weather_df2 <- w_df2 %>%
#   rename(station_temp = temp,
#          datetime = time,
#          dew_point = temp_dew,
#          rel_humid = rel_hum,
#          precip = precip_amt) %>%
#   select(datetime, station_temp, datetime, dew_point, rel_humid, precip, pressure) %>%
#   mutate(datetime = as_datetime(datetime, tz="Canada/Central")) %>%
#   filter(datetime > start_date & datetime <= end_date) %>%
#   mutate(station_temp = tsclean(station_temp),
#          rel_humid = tsclean(rel_humid),
#          pressure = tsclean(pressure))
# 
# weather_df <-
#   bind_rows(weather_df1, weather_df2) %>%
#   mutate(year = year(datetime),
#          month = month(datetime),
#          day = day(datetime)) %>%
#   group_by(year, month, day) %>%
#   summarise(across(station_temp:pressure, ~mean(., na.rm = T))) %>%
#   ungroup() %>%
#   mutate(datetime = ymd(paste(year, month, day, sep = "-")))
# 
# min_date <- min(weather_df$datetime)
# min_year <- year(min_date)
# min_month <- sprintf("%02d", month(min_date))
# min_day <- sprintf("%02d", day(min_date))
# max_date <- max(weather_df$datetime)
# max_year <- year(max_date)
# max_month <- sprintf("%02d", month(max_date))
# max_day <- sprintf("%02d", day(max_date))
# save_path <- sprintf("./Earthwatch/Churchill/data/Churchill_EnvCan_%s%s%s_%s%s%s.csv",
#                      min_year, min_month, min_day,
#                      max_year, max_month, max_day)
# write.csv(weather_df, save_path)

# If already downloaded:
weather_df <- read.csv("./Earthwatch/Churchill/data/Churchill_EnvCan_20000101_20221010.csv")[-1]
weather_df %>% 
  ggplot(aes(x = as_datetime(datetime), y = station_temp)) + geom_line()

weather_daily <- weather_df %>% 
  mutate(Date = as_datetime(datetime),
         year = year(Date),
         month = month(Date),
         day = day(Date)) %>%
  group_by(year, month, day) %>% 
  summarise(across(everything(), ~mean(., na.rm = T))) %>% 
  ungroup() %>% 
  mutate(Date = ymd(paste(year, month, day, sep = "-")))

str(weather_daily)

churchill <- left_join(churchill, weather_daily, by = "Date") %>% 
  select(-c(month.x, day, datetime, dew_point, year, rel_humid, precip, pressure)) %>% 
  rename(month = month.y) %>% 
  relocate(month, .after = Year) %>% 
  rowwise() %>% 
  mutate(mean.150 = mean(c(airp150, bsw150, tis150, 
                           wsu150, fen150, bfr150, 
                           bwp150, ppa150, 
                           ppd150, tun150, station_temp), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(mean.150 = as.numeric(tsclean(mean.150)))

churchill %>% 
  ggplot(aes(x = as_datetime(Date), y = mean.150)) + geom_line()

## ********************
## ********************
# Step 2: Fill the missing air temperatures ----

# Function to fill missing values
fill_missing_with_lm <- function(dat, vars) {
  for(i in seq_along(vars)) {
    mod <- as.formula(paste0(vars[i], " ~ mean.150"))
    mod <- lm(mod, dat)
    misses <- which(is.na(dat[[ vars[i] ]]))
    for(j in misses) {
      newdat <- data.frame(mean.150 = dat$mean.150[j])
      dat[[ vars[i] ]][j] <- predict(mod, newdat)
    }
  }
  return(dat)
}

# Now can fill each month using EnvCan
churchill <- churchill %>%
  group_by(month) %>%
  nest %>%
  mutate(filled = map(data, fill_missing_with_lm, vars = c("airp150", "bsw150", "mlk150",
                                                           "tis150", "wsu150", "fen150", 
                                                           "bfr150", "bwp150", "pfr150",
                                                           "ppa150", "ppd150", "tun150",
                                                           "rlk150"))) %>%
  select(month, filled) %>%
  unnest(cols = c(filled)) %>% 
  arrange(Date) %>% 
  mutate(month = as.integer(month(Date)))

churchill %>% 
  ggplot(aes(x = as_datetime(Date), y = airp150)) + geom_line() +
  geom_line(aes(y = bsw150), color = col_pal[2]) +
  geom_line(aes(y = tis150), color = "red") +
  geom_line(aes(y = wsu150), color = col_pal[3]) +
  geom_line(aes(y = fen150), color = "green")

churchill %>% 
  ggplot(aes(x = as_datetime(Date), y = airp150)) + geom_line()

## ********************
## ********************
# Step 3: Fill the missing ground surface temperatures ----

##****************
### AIR - ground surface T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(airp0)) %>% 
  do(model = lm(airp0 ~ airp150 + mean.150, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(airp0 = ifelse(is.na(airp0), pred, airp0))
plot(ch$Date, ch$airp0, type = "l")
lines(churchill$Date, churchill$airp0, col = "red")
churchill$airp0 <- ch$airp0

##****************
### BSW - ground surface T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(bsw0)) %>% 
  do(model = lm(bsw0 ~ bsw150 + airp0, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(bsw0 = ifelse(is.na(bsw0), pred, bsw0))
plot(ch$Date, ch$bsw0, type = "l")
lines(churchill$Date, churchill$bsw0, col = "red")
churchill$bsw0 <- ch$bsw0

##****************
### BFR - ground surface T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(bfr0)) %>% 
  do(model = lm(bfr0 ~ airp0 + bsw0 + bfr150, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(bfr0 = ifelse(is.na(bfr0), pred, bfr0))
plot(ch$Date, ch$bfr0, type = "l")
lines(churchill$Date, churchill$bfr0, col = "red")
churchill$bfr0 <- ch$bfr0

##****************
### PFR - ground surface T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(pfr0)) %>% 
  do(model = lm(pfr0 ~ bfr0 + pfr150 + tun150, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(pfr0 = ifelse(is.na(pfr0), pred, pfr0))
plot(ch$Date, ch$pfr0, type = "l")
lines(churchill$Date, churchill$pfr0, col = "red")
churchill$pfr0 <- ch$pfr0

##****************
### WSU - ground surface T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(wsu0)) %>% 
  do(model = lm(wsu0 ~ wsu150 + airp0 + bsw0, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(wsu0 = ifelse(is.na(wsu0), pred, wsu0))
plot(ch$Date, ch$wsu0, type = "l")
lines(churchill$Date, churchill$wsu0, col = "red")
churchill$wsu0 <- ch$wsu0

##****************
### TIS - ground surface T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(tis0)) %>% 
  do(model = lm(tis0 ~ tis150 + airp0 + bsw0, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(tis0 = ifelse(is.na(tis0), pred, tis0))
plot(ch$Date, ch$tis0, type = "l")
lines(churchill$Date, churchill$tis0, col = "red")
churchill$tis0 <- ch$tis0

##****************
### MLK - ground surface T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(mlk0)) %>% 
  do(model = lm(mlk0 ~ airp0 + bsw0 + wsu0, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(mlk0 = ifelse(is.na(mlk0), pred, mlk0))
plot(ch$Date, ch$mlk0, type = "l")
lines(churchill$Date, churchill$mlk0, col = "red")
churchill$mlk0 <- ch$mlk0

##****************
### FEN - ground surface T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(fen0)) %>% 
  do(model = lm(fen0 ~ airp0 + bsw0 + wsu0, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(fen0 = ifelse(is.na(fen0), pred, fen0))
plot(ch$Date, ch$fen0, type = "l")
lines(churchill$Date, churchill$fen0, col = "red")
churchill$fen0 <- ch$fen0

##****************
### BWP - ground surface T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(bwp0)) %>% 
  do(model = lm(bwp0 ~ airp0 + bsw0 + wsu0, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(bwp0 = ifelse(is.na(bwp0), pred, bwp0))
plot(ch$Date, ch$bwp0, type = "l")
lines(churchill$Date, churchill$bwp0, col = "red")
churchill$bwp0 <- ch$bwp0

##****************
### PPA - ground surface T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(ppa0)) %>% 
  do(model = lm(ppa0 ~ airp0 + bsw0 + wsu0, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(ppa0 = ifelse(is.na(ppa0), pred, ppa0))
plot(ch$Date, ch$ppa0, type = "l")
lines(churchill$Date, churchill$ppa0, col = "red")
churchill$ppa0 <- ch$ppa0

##****************
### PPD - ground surface T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(ppd0)) %>% 
  do(model = lm(ppd0 ~ airp0 + bsw0 + wsu0, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(ppd0 = ifelse(is.na(ppd0), pred, ppd0))
plot(ch$Date, ch$ppd0, type = "l")
lines(churchill$Date, churchill$ppd0, col = "red")
churchill$ppd0 <- ch$ppd0

##****************
### TUN - ground surface T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(tun0)) %>% 
  do(model = lm(tun0 ~ ppa0 + ppd0 + fen0, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(tun0 = ifelse(is.na(tun0), pred, tun0))
plot(ch$Date, ch$tun0, type = "l")
lines(churchill$Date, churchill$tun0, col = "red")
churchill$tun0 <- ch$tun0

## ********************
## ********************
# Step 4: Fill the missing -5 cm temperatures ----

##****************
### BSW - -5 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(bswneg5)) %>% 
  do(model = lm(bswneg5 ~ bsw150 + bsw0, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(bswneg5 = ifelse(is.na(bswneg5), pred, bswneg5))
plot(ch$Date, ch$bswneg5, type = "l")
lines(churchill$Date, churchill$bswneg5, col = "red")
churchill$bswneg5 <- ch$bswneg5

##****************
### BFR - -5 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(bfrneg5)) %>% 
  do(model = lm(bfrneg5 ~ bfr0 + bswneg5 + bfr150, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(bfrneg5 = ifelse(is.na(bfrneg5), pred, bfrneg5))
plot(ch$Date, ch$bfrneg5, type = "l")
lines(churchill$Date, churchill$bfrneg5, col = "red")
churchill$bfrneg5 <- ch$bfrneg5

##****************
### PFR - -5 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(pfrneg5)) %>% 
  do(model = lm(pfrneg5 ~ pfr0 + bfrneg5 + tun150, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(pfrneg5 = ifelse(is.na(pfrneg5), pred, pfrneg5))
plot(ch$Date, ch$pfrneg5, type = "l")
lines(churchill$Date, churchill$pfrneg5, col = "red")
churchill$pfrneg5 <- ch$pfrneg5

##****************
### WSU - -5 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(wsuneg5)) %>% 
  do(model = lm(wsuneg5 ~ wsu0 + pfrneg5 + bswneg5, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(wsuneg5 = ifelse(is.na(wsuneg5), pred, wsuneg5))
plot(ch$Date, ch$wsuneg5, type = "l")
lines(churchill$Date, churchill$wsuneg5, col = "red")
churchill$wsuneg5 <- ch$wsuneg5

##****************
### TIS - -5 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(tisneg5)) %>% 
  do(model = lm(tisneg5 ~ tis0 + wsuneg5 + bswneg5, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(tisneg5 = ifelse(is.na(tisneg5), pred, tisneg5))
plot(ch$Date, ch$tisneg5, type = "l")
lines(churchill$Date, churchill$tisneg5, col = "red")
churchill$tisneg5 <- ch$tisneg5

##****************
### FEN - -5 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(fenneg5)) %>% 
  do(model = lm(fenneg5 ~ fen0 + tun0 + wsuneg5, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(fenneg5 = ifelse(is.na(fenneg5), pred, fenneg5))
plot(ch$Date, ch$fenneg5, type = "l")
lines(churchill$Date, churchill$fenneg5, col = "red")
churchill$fenneg5 <- ch$fenneg5

##****************
### BWP - -5 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(bwpneg5)) %>% 
  do(model = lm(bwpneg5 ~ wsuneg5, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(bwpneg5 = ifelse(is.na(bwpneg5), pred, bwpneg5))
plot(ch$Date, ch$bwpneg5, type = "l")
lines(churchill$Date, churchill$bwpneg5, col = "red")
churchill$bwpneg5 <- ch$bwpneg5

##****************
### PPA - -5 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(ppaneg5)) %>% 
  do(model = lm(ppaneg5 ~ ppa0 + bwpneg5 + wsuneg5, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(ppaneg5 = ifelse(is.na(ppaneg5), pred, ppaneg5))
plot(ch$Date, ch$ppaneg5, type = "l")
lines(churchill$Date, churchill$ppaneg5, col = "red")
churchill$ppaneg5 <- ch$ppaneg5

##****************
### PPD - -5 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(ppdneg5)) %>% 
  do(model = lm(ppdneg5 ~ ppd0 + ppaneg5, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(ppdneg5 = ifelse(is.na(ppdneg5), pred, ppdneg5))
plot(ch$Date, ch$ppdneg5, type = "l")
lines(churchill$Date, churchill$ppdneg5, col = "red")
churchill$ppdneg5 <- ch$ppdneg5

##****************
### RLK - -5 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(rlkneg5)) %>% 
  do(model = lm(rlkneg5 ~ ppd0 + ppa0 + ppaneg5, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(rlkneg5 = ifelse(is.na(rlkneg5), pred, rlkneg5))
plot(ch$Date, ch$rlkneg5, type = "l")
lines(churchill$Date, churchill$rlkneg5, col = "red")
churchill$rlkneg5 <- ch$rlkneg5

##****************
### TUN - -5 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(tunneg5)) %>% 
  do(model = lm(tunneg5 ~ tun0 + ppdneg5 + fenneg5, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(tunneg5 = ifelse(is.na(tunneg5), pred, tunneg5))
plot(ch$Date, ch$tunneg5, type = "l")
lines(churchill$Date, churchill$tunneg5, col = "red")
churchill$tunneg5 <- ch$tunneg5

## ********************
## ********************
# Step 5: Fill the missing -10 cm temperatures ----

##****************
### BSW - -10 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(bswneg10)) %>% 
  do(model = lm(bswneg10 ~ bsw0 + bswneg5, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(bswneg10 = ifelse(is.na(bswneg10), pred, bswneg10))
plot(ch$Date, ch$bswneg10, type = "l")
lines(churchill$Date, churchill$bswneg10, col = "red")
churchill$bswneg10 <- ch$bswneg10

##****************
### BFR - -10 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(bfrneg10)) %>% 
  do(model = lm(bfrneg10 ~ bfrneg5 + bfr0 + bswneg10, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(bfrneg10 = ifelse(is.na(bfrneg10), pred, bfrneg10))
plot(ch$Date, ch$bfrneg10, type = "l")
lines(churchill$Date, churchill$bfrneg10, col = "red")
churchill$bfrneg10 <- ch$bfrneg10

##****************
### PFR - -10 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(pfrneg10)) %>% 
  do(model = lm(pfrneg10 ~ pfr0 + pfrneg5 + bfrneg10, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(pfrneg10 = ifelse(is.na(pfrneg10), pred, pfrneg10))
plot(ch$Date, ch$pfrneg10, type = "l")
lines(churchill$Date, churchill$pfrneg10, col = "red")
churchill$pfrneg10 <- ch$pfrneg10

##****************
### WSU - -10 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(wsu0)) %>% 
  do(model = lm(wsuneg10 ~ wsuneg5 + wsu0 + airp0 + bsw0, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(wsuneg10 = ifelse(is.na(wsuneg10), pred, wsuneg10))
plot(ch$Date, ch$wsuneg10, type = "l")
lines(churchill$Date, churchill$wsuneg10, col = "red")
churchill$wsuneg10 <- ch$wsuneg10

##****************
### TIS - -10 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(tisneg10)) %>% 
  do(model = lm(tisneg10 ~ tisneg5 + wsuneg10 + bswneg10, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(tisneg10 = ifelse(is.na(tisneg10), pred, tisneg10))
plot(ch$Date, ch$tisneg10, type = "l")
lines(churchill$Date, churchill$tisneg10, col = "red")
churchill$tisneg10 <- ch$tisneg10

##****************
### FEN - -10 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(fenneg10)) %>% 
  do(model = lm(fenneg10 ~ fenneg5 + bswneg10, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(fenneg10 = ifelse(is.na(fenneg10), pred, fenneg10))
plot(ch$Date, ch$fenneg10, type = "l")
lines(churchill$Date, churchill$fenneg10, col = "red")
churchill$fenneg10 <- ch$fenneg10

##****************
### BWP - -10 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(bwpneg10)) %>% 
  do(model = lm(bwpneg10 ~ bwpneg5 + bswneg10 + wsuneg10, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(bwpneg10 = ifelse(is.na(bwpneg10), pred, bwpneg10))
plot(ch$Date, ch$bwpneg10, type = "l")
lines(churchill$Date, churchill$bwpneg10, col = "red")
churchill$bwpneg10 <- ch$bwpneg10

##****************
### PPA - -10 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(ppaneg10)) %>% 
  do(model = lm(ppaneg10 ~ ppaneg5 + bswneg10 + wsuneg10, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(ppaneg10 = ifelse(is.na(ppaneg10), pred, ppaneg10))
plot(ch$Date, ch$ppaneg10, type = "l")
lines(churchill$Date, churchill$ppaneg10, col = "red")
churchill$ppaneg10 <- ch$ppaneg10

##****************
### PPD - -10 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(ppdneg10)) %>% 
  do(model = lm(ppdneg10 ~ ppdneg5 + bswneg5 + wsuneg5, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(ppdneg10 = ifelse(is.na(ppdneg10), pred, ppdneg10))
ch <- ch %>%
  mutate(ppdneg10 = ifelse(ppdneg10 < -40, -20, ppdneg10))
plot(ch$Date, ch$ppdneg10, type = "l")
lines(churchill$Date, churchill$ppdneg10, col = "red")
churchill$ppdneg10 <- ch$ppdneg10

##****************
### TUN - -10 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(tunneg10)) %>% 
  do(model = lm(tunneg10 ~ ppaneg10 + ppdneg10 + fenneg10, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(tunneg10 = ifelse(is.na(tunneg10), pred, tunneg10))
plot(ch$Date, ch$tunneg10, type = "l")
lines(churchill$Date, churchill$tunneg10, col = "red")
churchill$tunneg10 <- ch$tunneg10

##****************
### RLK - -10 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(rlkneg10)) %>% 
  do(model = lm(rlkneg10 ~ ppaneg10 + ppdneg10 + fenneg10, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(rlkneg10 = ifelse(is.na(rlkneg10), pred, rlkneg10))
plot(ch$Date, ch$rlkneg10, type = "l")
lines(churchill$Date, churchill$rlkneg10, col = "red")
churchill$rlkneg10 <- ch$rlkneg10

## ********************
## ********************
# Step 6: Fill the missing -15 cm temperatures ----

##****************
### BSW - -15 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(bswneg15)) %>% 
  do(model = lm(bswneg15 ~ bswneg10 + bswneg5, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(bswneg15 = ifelse(is.na(bswneg15), pred, bswneg15))
plot(ch$Date, ch$bswneg15, type = "l")
lines(churchill$Date, churchill$bswneg15, col = "red")
churchill$bswneg15 <- ch$bswneg15

##****************
### BFR - -15 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(bfrneg15)) %>% 
  do(model = lm(bfrneg15 ~ bfrneg10 + bfrneg5 + bswneg15, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(bfrneg15 = ifelse(is.na(bfrneg15), pred, bfrneg15))
plot(ch$Date, ch$bfrneg15, type = "l")
lines(churchill$Date, churchill$bfrneg15, col = "red")
churchill$bfrneg15 <- ch$bfrneg15

##****************
### PFR - -15 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(pfrneg15)) %>% 
  do(model = lm(pfrneg15 ~ pfrneg10 + pfrneg5 + bfrneg15, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(pfrneg15 = ifelse(is.na(pfrneg15), pred, pfrneg15))
plot(ch$Date, ch$pfrneg15, type = "l")
lines(churchill$Date, churchill$pfrneg15, col = "red")
churchill$pfrneg15 <- ch$pfrneg15

##****************
### WSU - -15 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(wsu0)) %>% 
  do(model = lm(wsuneg15 ~ wsuneg10 + wsuneg5 + bswneg15, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(wsuneg15 = ifelse(is.na(wsuneg15), pred, wsuneg15))
plot(ch$Date, ch$wsuneg15, type = "l")
lines(churchill$Date, churchill$wsuneg15, col = "red")
churchill$wsuneg15 <- ch$wsuneg15

##****************
### TIS - -15 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(tisneg15)) %>% 
  do(model = lm(tisneg15 ~ tisneg10 + wsuneg15 + bswneg15, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(tisneg15 = ifelse(is.na(tisneg15), pred, tisneg15))
plot(ch$Date, ch$tisneg15, type = "l")
lines(churchill$Date, churchill$tisneg15, col = "red")
churchill$tisneg15 <- ch$tisneg15

##****************
### FEN - -15 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(fenneg15)) %>% 
  do(model = lm(fenneg15 ~ fenneg10 + bswneg15, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(fenneg15 = ifelse(is.na(fenneg15), pred, fenneg15))
plot(ch$Date, ch$fenneg15, type = "l")
lines(churchill$Date, churchill$fenneg15, col = "red")
churchill$fenneg15 <- ch$fenneg15

##****************
### BWP - -15 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(bwpneg15)) %>% 
  do(model = lm(bwpneg15 ~ bwpneg10 + bswneg15 + wsuneg15, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(bwpneg15 = ifelse(is.na(bwpneg15), pred, bwpneg15))
plot(ch$Date, ch$bwpneg15, type = "l")
lines(churchill$Date, churchill$bwpneg15, col = "red")
churchill$bwpneg15 <- ch$bwpneg15

##****************
### PPA - -15 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(ppaneg15)) %>% 
  do(model = lm(ppaneg15 ~ ppaneg10 + bswneg15 + wsuneg15, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(ppaneg15 = ifelse(is.na(ppaneg15), pred, ppaneg15))
plot(ch$Date, ch$ppaneg15, type = "l")
lines(churchill$Date, churchill$ppaneg15, col = "red")
churchill$ppaneg15 <- ch$ppaneg15

##****************
### PPD - -15 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(ppdneg15)) %>% 
  do(model = lm(ppdneg15 ~ ppdneg10 + bswneg15 + wsuneg15, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(ppdneg15 = ifelse(is.na(ppdneg15), pred, ppdneg15))
ch <- ch %>%
  mutate(ppdneg15 = ifelse(ppdneg15 < -40, -20, ppdneg15))
plot(ch$Date, ch$ppdneg15, type = "l")
lines(churchill$Date, churchill$ppdneg15, col = "red")
churchill$ppdneg15 <- ch$ppdneg15

##****************
### TUN - -15 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(tunneg15)) %>% 
  do(model = lm(tunneg15 ~ ppaneg15 + ppdneg15 + fenneg15, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(tunneg15 = ifelse(is.na(tunneg15), pred, tunneg15))
plot(ch$Date, ch$tunneg15, type = "l")
lines(churchill$Date, churchill$tunneg15, col = "red")
churchill$tunneg15 <- ch$tunneg15

##****************
### RLK - -15 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(rlkneg15)) %>% 
  do(model = lm(rlkneg15 ~ ppaneg15 + ppdneg15 + fenneg15, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(rlkneg15 = ifelse(is.na(rlkneg15), pred, rlkneg15))
plot(ch$Date, ch$rlkneg15, type = "l")
lines(churchill$Date, churchill$rlkneg15, col = "red")
churchill$rlkneg15 <- ch$rlkneg15

## ********************
## ********************
# Step 7: Fill the missing -20 cm temperatures ----

##****************
### BSW - -20 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(bswneg20)) %>% 
  do(model = lm(bswneg20 ~ bswneg10 + bswneg15, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(bswneg20 = ifelse(is.na(bswneg20), pred, bswneg20))
plot(ch$Date, ch$bswneg20, type = "l")
lines(churchill$Date, churchill$bswneg20, col = "red")
churchill$bswneg20 <- ch$bswneg20

##****************
### BFR - -20 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(bfrneg20)) %>% 
  do(model = lm(bfrneg20 ~ bfrneg10 + bfrneg15 + bswneg20, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(bfrneg20 = ifelse(is.na(bfrneg20), pred, bfrneg20))
plot(ch$Date, ch$bfrneg20, type = "l")
lines(churchill$Date, churchill$bfrneg20, col = "red")
churchill$bfrneg20 <- ch$bfrneg20

##****************
### PFR - -20 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(pfrneg20)) %>% 
  do(model = lm(pfrneg20 ~ pfrneg10 + pfrneg15 + bfrneg20, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(pfrneg20 = ifelse(is.na(pfrneg20), pred, pfrneg20))
plot(ch$Date, ch$pfrneg20, type = "l")
lines(churchill$Date, churchill$pfrneg20, col = "red")
churchill$pfrneg20 <- ch$pfrneg20

##****************
### WSU - -20 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(wsu0)) %>% 
  do(model = lm(wsuneg20 ~ wsuneg10 + wsuneg15 + bswneg20, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(wsuneg20 = ifelse(is.na(wsuneg20), pred, wsuneg20))
plot(ch$Date, ch$wsuneg20, type = "l")
lines(churchill$Date, churchill$wsuneg20, col = "red")
churchill$wsuneg20 <- ch$wsuneg20

##****************
### TIS - -20 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(tisneg20)) %>% 
  do(model = lm(tisneg20 ~ tisneg15 + wsuneg20 + bswneg20, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(tisneg20 = ifelse(is.na(tisneg20), pred, tisneg20))
plot(ch$Date, ch$tisneg20, type = "l")
lines(churchill$Date, churchill$tisneg20, col = "red")
churchill$tisneg20 <- ch$tisneg20

##****************
### FEN - -20 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(fenneg20)) %>% 
  do(model = lm(fenneg20 ~ fenneg15 + bswneg20, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(fenneg20 = ifelse(is.na(fenneg20), pred, fenneg20))
plot(ch$Date, ch$fenneg20, type = "l")
lines(churchill$Date, churchill$fenneg20, col = "red")
churchill$fenneg20 <- ch$fenneg20

##****************
### BWP - -20 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(bwpneg20)) %>% 
  do(model = lm(bwpneg20 ~ bwpneg15 + bswneg20 + wsuneg20, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(bwpneg20 = ifelse(is.na(bwpneg20), pred, bwpneg20))
plot(ch$Date, ch$bwpneg20, type = "l")
lines(churchill$Date, churchill$bwpneg20, col = "red")
churchill$bwpneg20 <- ch$bwpneg20

##****************
### PPA - -20 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(ppaneg20)) %>% 
  do(model = lm(ppaneg20 ~ ppaneg15 + bswneg20 + wsuneg20, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(ppaneg20 = ifelse(is.na(ppaneg20), pred, ppaneg20))
plot(ch$Date, ch$ppaneg20, type = "l")
lines(churchill$Date, churchill$ppaneg20, col = "red")
churchill$ppaneg20 <- ch$ppaneg20

##****************
### PPA - -25 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(ppaneg25)) %>% 
  do(model = lm(ppaneg25 ~ ppaneg15 + bswneg20 + wsuneg20, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(ppaneg25 = ifelse(is.na(ppaneg25), pred, ppaneg25))
plot(ch$Date, ch$ppaneg25, type = "l")
lines(churchill$Date, churchill$ppaneg25, col = "red")
churchill$ppaneg25 <- ch$ppaneg25

##****************
### PPD - -20 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(ppdneg20)) %>% 
  do(model = lm(ppdneg20 ~ ppdneg15 + bswneg20 + wsuneg20, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(ppdneg20 = ifelse(is.na(ppdneg20), pred, ppdneg20))
plot(ch$Date, ch$ppdneg20, type = "l")
lines(churchill$Date, churchill$ppdneg20, col = "red")
churchill$ppdneg20 <- ch$ppdneg20

##****************
### TUN - -20 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(tunneg20)) %>% 
  do(model = lm(tunneg20 ~ ppaneg20 + ppdneg20 + fenneg20, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(tunneg20 = ifelse(is.na(tunneg20), pred, tunneg20))
plot(ch$Date, ch$tunneg20, type = "l")
lines(churchill$Date, churchill$tunneg20, col = "red")
churchill$tunneg20 <- ch$tunneg20

##****************
### RLK - -20 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(rlkneg20)) %>% 
  do(model = lm(rlkneg20 ~ ppaneg20 + ppdneg20 + fenneg20, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(rlkneg20 = ifelse(is.na(rlkneg20), pred, rlkneg20))
plot(ch$Date, ch$rlkneg20, type = "l")
lines(churchill$Date, churchill$rlkneg20, col = "red")
churchill$rlkneg20 <- ch$rlkneg20

##****************
### MLK - -20 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(mlkneg20)) %>% 
  do(model = lm(mlkneg20 ~ ppaneg20 + ppdneg20 + fenneg20, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(mlkneg20 = ifelse(is.na(mlkneg20), pred, mlkneg20))
plot(ch$Date, ch$mlkneg20, type = "l")
lines(churchill$Date, churchill$mlkneg20, col = "red")
churchill$mlkneg20 <- ch$mlkneg20

## ********************
## ********************
# Step 8: Fill the missing -40 cm temperatures ----

##****************
### BSW - -40 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(bswneg40)) %>% 
  do(model = lm(bswneg40 ~ bswneg15 + bswneg20, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(bswneg40 = ifelse(is.na(bswneg40), pred, bswneg40))
plot(ch$Date, ch$bswneg40, type = "l")
lines(churchill$Date, churchill$bswneg40, col = "red")
churchill$bswneg40 <- ch$bswneg40

##****************
### BFR - -40 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(bfrneg40)) %>% 
  do(model = lm(bfrneg40 ~ bfrneg20 + bfrneg15 + bswneg40, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(bfrneg40 = ifelse(is.na(bfrneg40), pred, bfrneg40))
plot(ch$Date, ch$bfrneg40, type = "l")
lines(churchill$Date, churchill$bfrneg40, col = "red")
churchill$bfrneg40 <- ch$bfrneg40

##****************
### PFR - -40 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(pfrneg40)) %>% 
  do(model = lm(pfrneg40 ~ pfrneg20 + pfrneg15 + bfrneg40, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(pfrneg40 = ifelse(is.na(pfrneg40), pred, pfrneg40))
plot(ch$Date, ch$pfrneg40, type = "l")
lines(churchill$Date, churchill$pfrneg40, col = "red")
churchill$pfrneg40 <- ch$pfrneg40

##****************
### WSU - -40 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(wsu0)) %>% 
  do(model = lm(wsuneg40 ~ wsuneg20 + wsuneg15 + bswneg40, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(wsuneg40 = ifelse(is.na(wsuneg40), pred, wsuneg40))
plot(ch$Date, ch$wsuneg40, type = "l")
lines(churchill$Date, churchill$wsuneg40, col = "red")
churchill$wsuneg40 <- ch$wsuneg40

##****************
### TIS - -40 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(tisneg40)) %>% 
  do(model = lm(tisneg40 ~ tisneg20 + wsuneg40 + bswneg40, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(tisneg40 = ifelse(is.na(tisneg40), pred, tisneg40))
plot(ch$Date, ch$tisneg40, type = "l")
lines(churchill$Date, churchill$tisneg40, col = "red")
churchill$tisneg40 <- ch$tisneg40

##****************
### FEN - -40 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(fenneg40)) %>% 
  do(model = lm(fenneg40 ~ fenneg20 + bswneg40, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(fenneg40 = ifelse(is.na(fenneg40), pred, fenneg40))
plot(ch$Date, ch$fenneg40, type = "l")
lines(churchill$Date, churchill$fenneg40, col = "red")
churchill$fenneg40 <- ch$fenneg40

##****************
### BWP - -40 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(bwpneg40)) %>% 
  do(model = lm(bwpneg40 ~ bwpneg20 + bswneg40 + wsuneg40, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(bwpneg40 = ifelse(is.na(bwpneg40), pred, bwpneg40))
plot(ch$Date, ch$bwpneg40, type = "l")
lines(churchill$Date, churchill$bwpneg40, col = "red")
churchill$bwpneg40 <- ch$bwpneg40

##****************
### PPA - -40 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(ppaneg40)) %>% 
  do(model = lm(ppaneg40 ~ ppaneg20 + bswneg40 + wsuneg40, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(ppaneg40 = ifelse(is.na(ppaneg40), pred, ppaneg40))
plot(ch$Date, ch$ppaneg40, type = "l")
lines(churchill$Date, churchill$ppaneg40, col = "red")
churchill$ppaneg40 <- ch$ppaneg40

##****************
### PPD - -40 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(ppdneg40)) %>% 
  do(model = lm(ppdneg40 ~ ppdneg20 + bswneg40 + wsuneg40, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(ppdneg40 = ifelse(is.na(ppdneg40), pred, ppdneg40))
ch <- ch %>% 
  mutate(ppdneg40 = ifelse(ppdneg40 < -30, -15, ppdneg40))
plot(ch$Date, ch$ppdneg40, type = "l")
lines(churchill$Date, churchill$ppdneg40, col = "red")
churchill$ppdneg40 <- ch$ppdneg40

##****************
### TUN - -40 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(tunneg40)) %>% 
  do(model = lm(tunneg40 ~ ppaneg40 + ppdneg40 + fenneg40, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(tunneg40 = ifelse(is.na(tunneg40), pred, tunneg40))
plot(ch$Date, ch$tunneg40, type = "l")
lines(churchill$Date, churchill$tunneg40, col = "red")
churchill$tunneg40 <- ch$tunneg40

##****************
### RLK - -40 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(rlkneg40)) %>% 
  do(model = lm(rlkneg40 ~ ppaneg40 + ppdneg40 + fenneg40, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(rlkneg40 = ifelse(is.na(rlkneg40), pred, rlkneg40))
plot(ch$Date, ch$rlkneg40, type = "l")
lines(churchill$Date, churchill$rlkneg40, col = "red")
churchill$rlkneg40 <- ch$rlkneg40

##****************
### MLK - -40 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(mlkneg40)) %>% 
  do(model = lm(mlkneg40 ~ ppaneg40 + ppdneg40 + fenneg40, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(mlkneg40 = ifelse(is.na(mlkneg40), pred, mlkneg40))
ch <- ch %>% 
  mutate(mlkneg40 = ifelse(mlkneg40 > 10, 8, mlkneg40))
plot(ch$Date, ch$mlkneg40, type = "l")
lines(churchill$Date, churchill$mlkneg40, col = "red")
churchill$mlkneg40 <- ch$mlkneg40

## ********************
## ********************
# Step 9: Fill the missing -80 cm temperatures ----

##****************
### BSW - -80 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(bswneg80)) %>% 
  do(model = lm(bswneg80 ~ bswneg40 + bswneg20, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(bswneg80 = ifelse(is.na(bswneg80), pred, bswneg80))
plot(ch$Date, ch$bswneg80, type = "l")
lines(churchill$Date, churchill$bswneg80, col = "red")
churchill$bswneg80 <- ch$bswneg80

##****************
### BFR - -80 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(bfrneg80)) %>% 
  do(model = lm(bfrneg80 ~ bfrneg40 + bfrneg20 + bswneg80, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(bfrneg80 = ifelse(is.na(bfrneg80), pred, bfrneg80))
plot(ch$Date, ch$bfrneg80, type = "l")
lines(churchill$Date, churchill$bfrneg80, col = "red")
churchill$bfrneg80 <- ch$bfrneg80

##****************
### PFR - -80 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(pfrneg80)) %>% 
  do(model = lm(pfrneg80 ~ pfrneg40 + pfrneg20 + bfrneg80, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(pfrneg80 = ifelse(is.na(pfrneg80), pred, pfrneg80))
plot(ch$Date, ch$pfrneg80, type = "l")
lines(churchill$Date, churchill$pfrneg80, col = "red")
churchill$pfrneg80 <- ch$pfrneg80

##****************
### WSU - -80 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(wsu0)) %>% 
  do(model = lm(wsuneg80 ~ bfrneg40 + bswneg80, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(wsuneg80 = ifelse(is.na(wsuneg80), pred, wsuneg80))
plot(ch$Date, ch$wsuneg80, type = "l")
lines(churchill$Date, churchill$wsuneg80, col = "red")
churchill$wsuneg80 <- ch$wsuneg80

##****************
### AIRp - -80 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(airpneg80)) %>% 
  do(model = lm(airpneg80 ~ wsuneg80 + bswneg80, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(airpneg80 = ifelse(is.na(airpneg80), pred, airpneg80))
plot(ch$Date, ch$airpneg80, type = "l")
lines(churchill$Date, churchill$airpneg80, col = "red")
churchill$airpneg80 <- ch$airpneg80

##****************
### TIS - -80 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(tisneg80)) %>% 
  do(model = lm(tisneg80 ~ tisneg40 + wsuneg80 + bswneg80, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(tisneg80 = ifelse(is.na(tisneg80), pred, tisneg80))
plot(ch$Date, ch$tisneg80, type = "l")
lines(churchill$Date, churchill$tisneg80, col = "red")
churchill$tisneg80 <- ch$tisneg80

##****************
### FEN - -80 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(fenneg80)) %>% 
  do(model = lm(fenneg80 ~ fenneg40 + bswneg80, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(fenneg80 = ifelse(is.na(fenneg80), pred, fenneg80))
plot(ch$Date, ch$fenneg80, type = "l")
lines(churchill$Date, churchill$fenneg80, col = "red")
churchill$fenneg80 <- ch$fenneg80

##****************
### BWP - -80 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(bwpneg80)) %>% 
  do(model = lm(bwpneg80 ~ bwpneg40 + bswneg80 + wsuneg80, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(bwpneg80 = ifelse(is.na(bwpneg80), pred, bwpneg80))
plot(ch$Date, ch$bwpneg80, type = "l")
lines(churchill$Date, churchill$bwpneg80, col = "red")
churchill$bwpneg80 <- ch$bwpneg80

##****************
### PPA - -80 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(ppaneg80)) %>% 
  do(model = lm(ppaneg80 ~ ppaneg40 + bswneg80 + wsuneg80, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(ppaneg80 = ifelse(is.na(ppaneg80), pred, ppaneg80))
plot(ch$Date, ch$ppaneg80, type = "l")
lines(churchill$Date, churchill$ppaneg80, col = "red")
churchill$ppaneg80 <- ch$ppaneg80

##****************
### PPD - -80 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(ppdneg80)) %>% 
  do(model = lm(ppdneg80 ~ ppdneg40 + bswneg80 + wsuneg80, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(ppdneg80 = ifelse(is.na(ppdneg80), pred, ppdneg80))
plot(ch$Date, ch$ppdneg80, type = "l")
lines(churchill$Date, churchill$ppdneg80, col = "red")
churchill$ppdneg80 <- ch$ppdneg80

##****************
### TUN - -80 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(tunneg80)) %>% 
  do(model = lm(tunneg80 ~ ppaneg80 + ppdneg80 + fenneg80, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(tunneg80 = ifelse(is.na(tunneg80), pred, tunneg80))
ch <- ch %>% 
  mutate(tunneg80 = ifelse(tunneg80 > 5, 4, tunneg80))
plot(ch$Date, ch$tunneg80, type = "l")
lines(churchill$Date, churchill$tunneg80, col = "red")
churchill$tunneg80 <- ch$tunneg80

##****************
### RLK - -80 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(rlkneg80)) %>% 
  do(model = lm(rlkneg80 ~ ppaneg80 + ppdneg80 + fenneg80, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(rlkneg80 = ifelse(is.na(rlkneg80), pred, rlkneg80))
plot(ch$Date, ch$rlkneg80, type = "l")
lines(churchill$Date, churchill$rlkneg80, col = "red")
churchill$rlkneg80 <- ch$rlkneg80

##****************
### MLK - -80 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(mlkneg80)) %>% 
  do(model = lm(mlkneg80 ~ ppaneg80 + ppdneg80 + fenneg80, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(mlkneg80 = ifelse(is.na(mlkneg80), pred, mlkneg80))
plot(ch$Date, ch$mlkneg80, type = "l")
lines(churchill$Date, churchill$mlkneg80, col = "red")
churchill$mlkneg80 <- ch$mlkneg80

## ********************
## ********************
# Step 10: Fill the missing -100 to -200 cm temperatures ----

##****************
### PPD - -100 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(ppdneg100)) %>% 
  do(model = lm(ppdneg100 ~ ppdneg40 + ppdneg80, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(ppdneg100 = ifelse(is.na(ppdneg100), pred, ppdneg100))
plot(ch$Date, ch$ppdneg100, type = "l")
lines(churchill$Date, churchill$ppdneg100, col = "red")
churchill$ppdneg100 <- ch$ppdneg100

##****************
### FEN - -100 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(fenneg100)) %>% 
  do(model = lm(fenneg100 ~ fenneg80, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(fenneg100 = ifelse(is.na(fenneg100), pred, fenneg100))
plot(ch$Date, ch$fenneg100, type = "l")
lines(churchill$Date, churchill$fenneg100, col = "red")
churchill$fenneg100 <- ch$fenneg100

##****************
### AIRp - -150 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(airpneg150)) %>% 
  do(model = lm(airpneg150 ~ airpneg80, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(airpneg150 = ifelse(is.na(airpneg150), pred, airpneg150))
plot(ch$Date, ch$airpneg150, type = "l")
lines(churchill$Date, churchill$airpneg150, col = "red")
churchill$airpneg150 <- ch$airpneg150

##****************
### FEN - -200 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(fenneg200)) %>% 
  do(model = lm(fenneg200 ~ fenneg80, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(fenneg200 = ifelse(is.na(fenneg200), pred, fenneg200))
plot(ch$Date, ch$fenneg200, type = "l")
lines(churchill$Date, churchill$fenneg200, col = "red")
churchill$fenneg200 <- ch$fenneg200

##****************
### PPD - -200 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(ppdneg200)) %>% 
  do(model = lm(ppdneg200 ~ airpneg150, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(ppdneg200 = ifelse(is.na(ppdneg200), pred, ppdneg200))
plot(ch$Date, ch$ppdneg200, type = "l")
lines(churchill$Date, churchill$ppdneg200, col = "red")
churchill$ppdneg200 <- ch$ppdneg200

## ********************
## ********************
# Step 11: Fill the missing >-200 cm temperatures ----

##****************
### AIRp - -230 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(airpneg230)) %>% 
  do(model = lm(airpneg230 ~ airpneg150, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(airpneg230 = ifelse(is.na(airpneg230), pred, airpneg230))
plot(ch$Date, ch$airpneg230, type = "l")
lines(churchill$Date, churchill$airpneg230, col = "red")
churchill$airpneg230 <- ch$airpneg230

##****************
### AIRp - -300 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(airpneg300)) %>% 
  do(model = lm(airpneg300 ~ airpneg150 + airpneg230, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(airpneg300 = ifelse(is.na(airpneg300), pred, airpneg300))
plot(ch$Date, ch$airpneg300, type = "l")
lines(churchill$Date, churchill$airpneg300, col = "red")
churchill$airpneg300 <- ch$airpneg300

##****************
### AIRp - -610 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(airpneg610)) %>% 
  do(model = lm(airpneg610 ~ airpneg230, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(airpneg610 = ifelse(is.na(airpneg610), pred, airpneg610))
plot(ch$Date, ch$airpneg610, type = "l")
lines(churchill$Date, churchill$airpneg610, col = "red")
churchill$airpneg610 <- ch$airpneg610

##****************
### AIRp - -910 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(airpneg910)) %>% 
  do(model = lm(airpneg910 ~ airpneg610, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(airpneg910 = ifelse(is.na(airpneg910), pred, airpneg910))
plot(ch$Date, ch$airpneg910, type = "l")
lines(churchill$Date, churchill$airpneg910, col = "red")
churchill$airpneg910 <- ch$airpneg910

##****************
### AIRp - -1220 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(airpneg1220)) %>% 
  do(model = lm(airpneg1220 ~ airpneg910, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(airpneg1220 = ifelse(is.na(airpneg1220), pred, airpneg1220))
plot(ch$Date, ch$airpneg1220, type = "l")
lines(churchill$Date, churchill$airpneg1220, col = "red")
churchill$airpneg1220 <- ch$airpneg1220

#****************
### AIRp - -1500 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(airpneg1500)) %>% 
  do(model = lm(airpneg1500 ~ airpneg1220, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(airpneg1500 = ifelse(is.na(airpneg1500), pred, airpneg1500))
plot(ch$Date, ch$airpneg1500, type = "l")
lines(churchill$Date, churchill$airpneg1500, col = "red")
churchill$airpneg1500 <- ch$airpneg1500

#****************
### AIRf - -1500 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(airfneg1500)) %>% 
  do(model = lm(airfneg1500 ~ airpneg1220 + airpneg1500, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(airfneg1500 = ifelse(is.na(airfneg1500), pred, airfneg1500))
plot(ch$Date, ch$airfneg1500, type = "l")
lines(churchill$Date, churchill$airfneg1500, col = "red")
churchill$airfneg1500 <- ch$airfneg1500

#****************
### FEN - -300 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(fenneg300)) %>% 
  do(model = lm(fenneg300 ~ fenneg80 + airpneg230, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(fenneg300 = ifelse(is.na(fenneg300), pred, fenneg300))
plot(ch$Date, ch$fenneg300, type = "l")
lines(churchill$Date, churchill$fenneg300, col = "red")
churchill$fenneg300 <- ch$fenneg300

#****************
### FEN - -400 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(fenneg400)) %>% 
  do(model = lm(fenneg400 ~ fenneg300 + airpneg230, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(fenneg400 = ifelse(is.na(fenneg400), pred, fenneg400))
plot(ch$Date, ch$fenneg400, type = "l")
lines(churchill$Date, churchill$fenneg400, col = "red")
churchill$fenneg400 <- ch$fenneg400

#****************
### FEN - -500 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(fenneg500)) %>% 
  do(model = lm(fenneg500 ~ fenneg300 + fenneg400, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(fenneg500 = ifelse(is.na(fenneg500), pred, fenneg500))
plot(ch$Date, ch$fenneg500, type = "l")
lines(churchill$Date, churchill$fenneg500, col = "red")
churchill$fenneg500 <- ch$fenneg500

#****************
### FEN - -600 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(fenneg600)) %>% 
  do(model = lm(fenneg600 ~ fenneg300 + fenneg400 + fenneg500, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(fenneg600 = ifelse(is.na(fenneg600), pred, fenneg600))
ch <- ch %>% 
  mutate(fenneg600 = ifelse(fenneg600 > -1, -1.5, fenneg600))
plot(ch$Date, ch$fenneg600, type = "l")
lines(churchill$Date, churchill$fenneg600, col = "red")
churchill$fenneg600 <- ch$fenneg600

#****************
### FEN - -700 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(fenneg700)) %>% 
  do(model = lm(fenneg700 ~ fenneg400 + fenneg500 + fenneg600, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(fenneg700 = ifelse(is.na(fenneg700), pred, fenneg700))
plot(ch$Date, ch$fenneg700, type = "l")
lines(churchill$Date, churchill$fenneg700, col = "red")
churchill$fenneg700 <- ch$fenneg700

#****************
### PPD - -230 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(ppdneg230)) %>% 
  do(model = lm(ppdneg230 ~ ppdneg200, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(ppdneg230 = ifelse(is.na(ppdneg230), pred, ppdneg230))
plot(ch$Date, ch$ppdneg230, type = "l")
lines(churchill$Date, churchill$ppdneg230, col = "red")
churchill$ppdneg230 <- ch$ppdneg230

#****************
### TUN - -300 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(tunneg300)) %>% 
  do(model = lm(tunneg300 ~ tunneg80 + ppdneg230, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(tunneg300 = ifelse(is.na(tunneg300), pred, tunneg300))
plot(ch$Date, ch$tunneg300, type = "l")
lines(churchill$Date, churchill$tunneg300, col = "red")
churchill$tunneg300 <- ch$tunneg300

#****************
### TUN - -700 cm T ----

# Construct linear model based on non-NA pairs
models <- churchill %>% 
  group_by(month) %>% 
  filter(!is.na(tunneg700)) %>% 
  do(model = lm(tunneg700 ~ tunneg300 + fenneg700, data = .)) %>%
  ungroup()
ch <- left_join(as_tibble(churchill), models, by = "month")

# generate the extra column
ch <- ch %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(tunneg700 = ifelse(is.na(tunneg700), pred, tunneg700))
plot(ch$Date, ch$tunneg700, type = "l")
lines(churchill$Date, churchill$tunneg700, col = "red")
churchill$tunneg700 <- ch$tunneg700

## ********************
## ********************
# Step 12: Output the filled data ----

## Generate the date range
min_date <- min(churchill$Date)
min_year <- year(min_date)
min_month <- sprintf("%02d", month(min_date))
min_day <- sprintf("%02d", day(min_date))
max_date <- max(churchill$Date)
max_year <- year(max_date)
max_month <- sprintf("%02d", month(max_date))
max_day <- sprintf("%02d", day(max_date))

## Generate the file name and path and export
save_path <- sprintf("./Earthwatch/Churchill/data/microclimate_%s%s%s_%s%s%s_filled.csv",
                     min_year, min_month, min_day,
                     max_year, max_month, max_day)
write.csv(churchill, save_path, row.names=FALSE)
