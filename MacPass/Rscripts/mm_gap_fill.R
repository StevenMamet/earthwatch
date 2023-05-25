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

# Mac Pass met station data
macpass <- read.csv("./Earthwatch/MacPass/data/microclimate.mm2.csv", header = TRUE)
# macpass <- read.csv("~/Dropbox/School/School/Postdoc/Earthwatch/Mackenzie Mountains/GNWT/MM_daily.csv")
# Converts to date format (lubridate)
macpass$Date <- ymd(macpass$Date)
names(macpass)[1] <- "month"

p1 <- macpass %>% 
  ggplot(aes(x = Date, y = bp.150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = bp.0), color = col_pal[3]) +
  geom_line(aes(y = bp.neg150), color = col_pal[2])

p2 <- macpass %>% 
  ggplot(aes(x = Date, y = hf.150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = hf.0), color = col_pal[3]) +
  geom_line(aes(y = hf.neg150), color = col_pal[2])

p3 <- macpass %>% 
  ggplot(aes(x = Date, y = d6.150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = d6.0), color = col_pal[3]) +
  geom_line(aes(y = d6.neg150), color = col_pal[2])

p4 <- macpass %>% 
  ggplot(aes(x = Date, y = d2.150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = d2.0), color = col_pal[3]) +
  geom_line(aes(y = d2.neg150), color = col_pal[2])

p5 <- macpass %>% 
  ggplot(aes(x = Date, y = gf.150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = gf.0), color = col_pal[3]) +
  geom_line(aes(y = gf.neg150), color = col_pal[2])

p1 / p2 / p3 / p4 / p5

## ********************
## ********************
# Step 1: Environment Canada data ----
# Get the correct station ID, make a df using that ID and the time period of interest, and download
# stations_dl() # re-download/update stations data
# station_id <- suppressMessages(as.numeric(stations_search("MACMILLAN PASS", dist = 50, interval = "hour")[3]))
# # Now read in the data and tidy it up. Need to choose time period to cover
# start_date <- min(macpass$Date)
# end_date <- max(macpass$Date)
# w_df <- weather_dl(station_ids = station_id, start = start_date, end = end_date)
# weather_df <- w_df %>% 
#   rename(station_temp = temp,
#          datetime = time,
#          dew_point = temp_dew,
#          rel_humid = rel_hum,
#          precip = precip_amt) %>% 
#   select(datetime, station_temp, datetime, dew_point, rel_humid, precip, pressure) %>% 
#   mutate(datetime = as_datetime(datetime, tz="Canada/Saskatchewan")) %>% 
#   filter(datetime > start_date & datetime <= end_date) %>% 
#   mutate(station_temp = tsclean(station_temp),
#          rel_humid = tsclean(rel_humid),
#          pressure = tsclean(pressure))
# 
# min_date <- min(weather_df$datetime)
# min_year <- year(min_date)
# min_month <- sprintf("%02d", month(min_date))
# min_day <- sprintf("%02d", day(min_date))
# max_date <- max(weather_df$datetime)
# max_year <- year(max_date)
# max_month <- sprintf("%02d", month(max_date))
# max_day <- sprintf("%02d", day(max_date))
# save_path <- sprintf("./Earthwatch/MacPass/data/MacPass_EnvCan_%s%s%s_%s%s%s.csv",
#                      min_year, min_month, min_day,
#                      max_year, max_month, max_day)
# write.csv(weather_df, save_path)

# If already downloaded:
weather_df <- read.csv("./Earthwatch/MacPass/data/MacPass_EnvCan_19980628_20210819.csv")[-1]
weather_df %>% 
  ggplot(aes(x = as_datetime(datetime), y = station_temp)) + geom_line()

weather_daily <- weather_df %>% 
  mutate(datetime = as_datetime(datetime),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime)) %>%
  group_by(year, month, day) %>% 
  summarise(across(everything(), ~mean(., na.rm = T))) %>% 
  ungroup() %>% 
  mutate(Date = ymd(paste(year, month, day, sep = "-")))

macpass <- left_join(macpass, weather_daily, by = "Date") %>% 
  select(-c(month.x, day, datetime, dew_point, year, rel_humid, precip, pressure)) %>% 
  rename(month = month.y) %>% 
  relocate(month, .after = YEAR) %>% 
  rowwise() %>% 
  mutate(mean.150 = mean(c(bp.150, hf.150, d2.150, d6.150, gf.150, station_temp), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(mean.150 = as.numeric(tsclean(mean.150)))

macpass %>% 
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
macpass <- macpass %>%
  group_by(month) %>%
  nest %>%
  mutate(filled = map(data, fill_missing_with_lm, vars = c("hf.150","bp.150","d2.150","d6.150","gf.150"))) %>%
  select(month, filled) %>%
  unnest(cols = c(filled)) %>% 
  arrange(Date) %>% 
  mutate(month = as.integer(month(Date)))

macpass %>% 
  ggplot(aes(x = as_datetime(Date), y = hf.150)) + geom_line() +
  geom_line(aes(y = bp.150), color = col_pal[2]) +
  geom_line(aes(y = d2.150), color = "red") +
  geom_line(aes(y = gf.150), color = col_pal[3]) +
  geom_line(aes(y = d6.150), color = "green")

macpass %>% 
  ggplot(aes(x = as_datetime(Date), y = hf.150)) + geom_line()

## ********************
## ********************
# Step 3: Fill the missing ground surface temperatures ----

##****************
### Hare Foot - ground surface T ----

# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(hf.0)) %>% 
  do(model = lm(hf.0 ~ hf.150 + bp.150, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(hf.0 = ifelse(is.na(hf.0), pred, hf.0))
plot(mp$Date, mp$hf.0, type = "l")
lines(macpass$Date, macpass$hf.0, col = "red")
macpass$hf.0 <- mp$hf.0

##****************
### Beaver Pond - ground surface T ----

# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(bp.0)) %>% 
  do(model = lm(bp.0 ~ bp.150 + hf.150 + hf.0, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(bp.0 = ifelse(is.na(bp.0), pred, bp.0))
plot(mp$Date, mp$bp.0, type = "l")
lines(macpass$Date, macpass$bp.0, col = "red")
macpass$bp.0 <- mp$bp.0

##****************
### Dale 6 - ground surface T ----

# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(d6.0)) %>% 
  do(model = lm(d6.0 ~ d2.150 + bp.150 + hf.0 + bp.0, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(d6.0 = ifelse(is.na(d6.0), pred, d6.0))
plot(mp$Date, mp$d6.0, type = "l")
lines(macpass$Date, macpass$d6.0, col = "red")
macpass$d6.0 <- mp$d6.0

##****************
### Dale 2 - ground surface T ----

# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(d2.0)) %>% 
  do(model = lm(d2.0 ~ d6.0 + bp.0 + d2.150 + d6.150 + hf.150 + bp.150, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(d2.0 = ifelse(is.na(d2.0), pred, d2.0))
plot(mp$Date, mp$d2.0, type = "l")
lines(macpass$Date, macpass$d2.0, col = "red")
macpass$d2.0 <- mp$d2.0

##****************
### Goose Flats - ground surface T ----

# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(gf.0)) %>% 
  do(model = lm(gf.0 ~ d2.0 + d6.0 + bp.0 + bp.150, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(gf.0 = ifelse(is.na(gf.0), pred, gf.0))
plot(mp$Date, mp$gf.0, type = "l")
lines(macpass$Date, macpass$gf.0, col = "red")
macpass$gf.0 <- mp$gf.0

## ********************
## ********************
# Step 4: Fill the missing -150 cm temperatures ----

##****************
### Hare Foot - -150 cm ----

# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(hf.neg150)) %>% 
  do(model = step(lm(hf.neg150 ~ hf.150 + d6.150 + d2.150 + gf.150 + hf.0 + bp.0 + gf.0, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(hf.neg150 = ifelse(is.na(hf.neg150), pred, hf.neg150))
plot(mp$Date, mp$hf.neg150, type = "l")
lines(macpass$Date, macpass$hf.neg150, col = "red")
macpass$hf.neg150 <- mp$hf.neg150

##****************
### Beaver Pond - -150 cm ----

# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(bp.neg150)) %>% 
  do(model = step(lm(bp.neg150 ~ hf.neg150 + hf.150 + bp.150 + hf.0 + d6.0 + gf.0, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(bp.neg150 = ifelse(is.na(bp.neg150), pred, bp.neg150))
plot(mp$Date, mp$bp.neg150, type = "l")
lines(macpass$Date, macpass$bp.neg150, col = "red")
macpass$bp.neg150 <- mp$bp.neg150

##****************
### Dale 6 - -150 cm ----

# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(d6.neg150)) %>% 
  do(model = step(lm(d6.neg150 ~ hf.neg150 + bp.neg150 + hf.150 + bp.150 + d6.150 + d2.150 + gf.150 + hf.0 + bp.0 + gf.0 + d6.0 + d2.0, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(d6.neg150 = ifelse(is.na(d6.neg150), pred, d6.neg150))
plot(mp$Date, mp$d6.neg150, type = "l")
lines(macpass$Date, macpass$d6.neg150, col = "red")
macpass$d6.neg150 <- mp$d6.neg150

##****************
### Dale 2 - -150 cm ----

# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(d2.neg150)) %>% 
  do(model = step(lm(d2.neg150 ~ hf.neg150 + bp.neg150 + d6.neg150 + hf.150 + bp.150 + d6.150 + d2.150 + gf.150 + hf.0 + bp.0 + gf.0 + d6.0 + d2.0, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(d2.neg150 = ifelse(is.na(d2.neg150), pred, d2.neg150))
plot(mp$Date, mp$d2.neg150, type = "l")
lines(macpass$Date, macpass$d2.neg150, col = "red")
macpass$d2.neg150 <- mp$d2.neg150

##****************
### Goose Flats - -150 cm ----

# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(gf.neg150)) %>% 
  do(model = step(lm(gf.neg150 ~ hf.neg150 + bp.neg150 + d6.neg150 + d2.neg150 + gf.150 + hf.0 + bp.0 + gf.0 + d6.0 + d2.0, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  mutate(gf.neg150 = ifelse(is.na(gf.neg150), pred, gf.neg150))
plot(mp$Date, mp$gf.neg150, type = "l")
lines(macpass$Date, macpass$gf.neg150, col = "red")
macpass$gf.neg150 <- mp$gf.neg150

## Generate the date range
min_date <- min(macpass$Date)
min_year <- year(min_date)
min_month <- sprintf("%02d", month(min_date))
min_day <- sprintf("%02d", day(min_date))
max_date <- max(macpass$Date)
max_year <- year(max_date)
max_month <- sprintf("%02d", month(max_date))
max_day <- sprintf("%02d", day(max_date))

## Generate the file name and path and export
save_path <- sprintf("./Earthwatch/MacPass/data/microclimate_%s%s%s_%s%s%s_filled.csv",
                     min_year, min_month, min_day,
                     max_year, max_month, max_day)
write.csv(macpass, save_path, row.names=FALSE)



p1 <- macpass %>% 
  ggplot(aes(x = Date, y = bp.150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = bp.0), color = col_pal[3]) +
  geom_line(aes(y = bp.neg150), color = col_pal[2])

p2 <- macpass %>% 
  ggplot(aes(x = Date, y = hf.150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = hf.0), color = col_pal[3]) +
  geom_line(aes(y = hf.neg150), color = col_pal[2])

p3 <- macpass %>% 
  ggplot(aes(x = Date, y = d6.150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = d6.0), color = col_pal[3]) +
  geom_line(aes(y = d6.neg150), color = col_pal[2])
 
p4 <- macpass %>% 
  ggplot(aes(x = Date, y = d2.150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = d2.0), color = col_pal[3]) +
  geom_line(aes(y = d2.neg150), color = col_pal[2])

p5 <- macpass %>% 
  ggplot(aes(x = Date, y = gf.150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = gf.0), color = col_pal[3]) +
  geom_line(aes(y = gf.neg150), color = col_pal[2])

p1 / p2 / p3 / p4 / p5



