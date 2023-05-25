library(dplyr)
library(psych)    # Pairs plot
library(splines)
library(lubridate)
library(tidyverse)
library(scales)
library(vegan)
library(forecast)
library(wesanderson)
library(broom)

rm(list=ls())

# Mac Pass met station data
# macpass <- read.csv(file = "~/Desktop/Workspace/Earthwatch/microclimate.mm2.csv", header = TRUE)
macpass <- read.csv("~/Dropbox/School/Postdoc/Earthwatch/Mackenzie Mountains/GNWT/MM_daily.csv")
# Converts to date format (lubridate)
macpass$date <- ymd(macpass$date)
names(macpass) <- gsub("\\.t","", names(macpass))

# plot(sf150.mean ~ date, macpass, type = "l")
# plot(sf0.mean ~ date, macpass, type = "l")
# plot(sf150.max ~ date, macpass, type = "l")
# plot(sf0.max ~ date, macpass, type = "l")
# plot(sf150.min ~ date, macpass, type = "l")
# plot(sf0.min ~ date, macpass, type = "l")
# 
# plot(gf150.mean ~ date, macpass, type = "l")
# plot(gf0.mean ~ date, macpass, type = "l")
# plot(gf150.max ~ date, macpass, type = "l")
# plot(gf0.max ~ date, macpass, type = "l")
# plot(gf150.min ~ date, macpass, type = "l")
# plot(gf0.min ~ date, macpass, type = "l")
# 
# plot(hf150.mean ~ date, macpass, type = "l")
# lines(macpass$date, macpass$mean.150, col = "blue")
# plot(hf0.mean ~ date, macpass, type = "l")
# plot(hf150.max ~ date, macpass, type = "l")
# plot(hf0.max ~ date, macpass, type = "l")
# plot(hf150.min ~ date, macpass, type = "l")
# plot(hf0.min ~ date, macpass, type = "l")
# 
# plot(bp150.mean ~ date, macpass, type = "l")
# plot(bp0.mean ~ date, macpass, type = "l")
# plot(bp150.max ~ date, macpass, type = "l")
# plot(bp0.max ~ date, macpass, type = "l")
# plot(bp150.min ~ date, macpass, type = "l")
# plot(bp0.min ~ date, macpass, type = "l")
# 
# macpass[which.max(macpass$d2150.mean),]
# plot(d2150.mean ~ date, macpass, type = "l")
# plot(d20.mean ~ date, macpass, type = "l")
# macpass[which.max(macpass$d2150.max),]
# plot(d2150.max ~ date, macpass, type = "l")
# plot(d20.max ~ date, macpass, type = "l")
# plot(d2150.min ~ date, macpass, type = "l")
# macpass[which.min(macpass$d2150.min),]
# plot(d20.min ~ date, macpass, type = "l")
# 
# plot(d6150.mean ~ date, macpass, type = "l")
# plot(d60.mean ~ date, macpass, type = "l")
# plot(d6150.max ~ date, macpass, type = "l")
# plot(d60.max ~ date, macpass, type = "l")
# plot(d6150.min ~ date, macpass, type = "l")
# plot(d60.min ~ date, macpass, type = "l")

# If already downloaded:
weather_df <- read.csv("/Users/sdmamet/Desktop/Workspace/Earthwatch/MacPass/data/MacPass_EnvCan_19980628_20210819.csv")[-1]
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
  mutate(date = ymd(paste(year, month, day, sep = "-"))) %>% 
  select(-c(year, month, day))

macpass <- left_join(macpass, weather_daily, by = "date") %>% 
  select(-c(day, datetime, dew_point, rel_humid, precip, pressure)) %>% 
  # rename(month = month.y) %>% 
  relocate(month, .after = year) %>% 
  rowwise() %>% 
  mutate(mean.150 = mean(c(bp150.mean, hf150.mean, d2150.mean, d6150.mean, gf150.mean, station_temp), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(mean.150 = as.numeric(tsclean(mean.150))) %>% 
  filter(!is.na(month))

macpass %>% 
  ggplot(aes(x = as_datetime(date), y = mean.150)) + geom_line()

###################################################################
## ********************
### Step 1: Fill the missing air temperatures

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
  mutate(filled = map(data, fill_missing_with_lm, 
                      vars = c("bp150.mean", "hf150.mean", "d2150.mean", "d6150.mean", "gf150.mean", "sf150.mean",
                               "bp150.max", "hf150.max", "d2150.max", "d6150.max", "gf150.max", "sf150.max",
                               "bp150.min", "hf150.min", "d2150.min", "d6150.min", "gf150.min", "sf150.min"
                               ))) %>%
  select(month, filled) %>%
  unnest(cols = c(filled)) %>% 
  arrange(date) %>% 
  mutate(month = as.integer(month(date)))

col_pal <- wes_palette("Darjeeling1")

macpass %>% 
  ggplot(aes(x = as_datetime(date), y = hf150.mean)) + geom_line() +
  geom_line(aes(y = bp150.mean), color = col_pal[2]) +
  geom_line(aes(y = d2150.mean), color = "red") +
  geom_line(aes(y = gf150.mean), color = col_pal[3]) +
  geom_line(aes(y = d6150.mean), color = "green")


macpass %>% 
  mutate(bp0.min = as.numeric(ifelse(date > "2015-01-01" & date < "2019-12-31", NA, bp0.min))) %>%
  mutate(bp0.min = ifelse(bp0.min > 50 | bp0.min < -40, NA, bp0.min)) %>%
  ggplot(aes(x = as_datetime(date), y = bp0.min)) + geom_line()

macpass <- macpass %>% 
  mutate(bp0.max = ifelse(date > "2016-01-01" & date < "2017-12-31", NA, bp0.max)) %>% 
  mutate(bp0.min = as.numeric(ifelse(date > "2015-01-01" & date < "2019-12-31", NA, bp0.min))) %>%
  mutate(bp0.min = ifelse(bp0.min > 50 | bp0.min < -40, NA, bp0.min))

###################################################################
## ********************
### Step 2: Fill the missing ground surface temperatures

### Hare Foot - ground surface T ----

#### Mean ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(hf0.mean)) %>% 
  do(model = lm(hf0.mean ~ hf150.mean, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(hf0.mean = ifelse(is.na(hf0.mean), pred, hf0.mean))
plot(mp$date, mp$hf0.mean, type = "l")
lines(macpass$date, macpass$hf0.mean, col = "red")
macpass$hf0.mean <- mp$hf0.mean

#### Max ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(hf0.max)) %>% 
  do(model = lm(hf0.max ~ hf150.max, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(hf0.max = ifelse(is.na(hf0.max), pred, hf0.max))
plot(mp$date, mp$hf0.max, type = "l")
lines(macpass$date, macpass$hf0.max, col = "red")
macpass$hf0.max <- mp$hf0.max

#### Min ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(hf0.min)) %>% 
  do(model = lm(hf0.min ~ hf150.min, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(hf0.min = ifelse(is.na(hf0.min), pred, hf0.min))
plot(mp$date, mp$hf0.min, type = "l")
lines(macpass$date, macpass$hf0.min, col = "red")
macpass$hf0.min <- mp$hf0.min

### Beaver Pond - ground surface T ----

#### Mean ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(bp0.mean)) %>% 
  do(model = lm(bp0.mean ~ bp150.mean, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(bp0.mean = ifelse(is.na(bp0.mean), pred, bp0.mean))
plot(mp$date, mp$bp0.mean, type = "l")
lines(macpass$date, macpass$bp0.mean, col = "red")
macpass$bp0.mean <- mp$bp0.mean

#### Max ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(bp0.max)) %>% 
  do(model = lm(bp0.max ~ bp150.max, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(bp0.max = ifelse(is.na(bp0.max), pred, bp0.max))
plot(mp$date, mp$bp0.max, type = "l")
lines(macpass$date, macpass$bp0.max, col = "red")
macpass$bp0.max <- mp$bp0.max

#### Min ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(bp0.min)) %>% 
  do(model = lm(bp0.min ~ bp150.min, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(bp0.min = ifelse(is.na(bp0.min), pred, bp0.min))
plot(mp$date, mp$bp0.min, type = "l")
lines(macpass$date, macpass$bp0.min, col = "red")
macpass$bp0.min <- mp$bp0.min

### Dale 6 - ground surface T ----

#### Mean ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(d60.mean)) %>% 
  do(model = lm(d60.mean ~ d6150.mean, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(d60.mean = ifelse(is.na(d60.mean), pred, d60.mean))
plot(mp$date, mp$d60.mean, type = "l")
lines(macpass$date, macpass$d60.mean, col = "red")
macpass$d60.mean <- mp$d60.mean

#### Max ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(d60.max)) %>% 
  do(model = lm(d60.max ~ d6150.max, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(d60.max = ifelse(is.na(d60.max), pred, d60.max))
plot(mp$date, mp$d60.max, type = "l")
lines(macpass$date, macpass$d60.max, col = "red")
macpass$d60.max <- mp$d60.max

#### Min ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(d60.min)) %>% 
  do(model = lm(d60.min ~ d6150.min, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(d60.min = ifelse(is.na(d60.min), pred, d60.min))
plot(mp$date, mp$d60.min, type = "l")
lines(macpass$date, macpass$d60.min, col = "red")
macpass$d60.min <- mp$d60.min

### Dale 2- ground surface T ----

#### Mean ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(d20.mean)) %>% 
  do(model = lm(d20.mean ~ d2150.mean, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(d20.mean = ifelse(is.na(d20.mean), pred, d20.mean))
plot(mp$date, mp$d20.mean, type = "l")
lines(macpass$date, macpass$d20.mean, col = "red")
macpass$d20.mean <- mp$d20.mean

#### Max ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(d20.max)) %>% 
  do(model = lm(d20.max ~ d2150.max, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(d20.max = ifelse(is.na(d20.max), pred, d20.max))
plot(mp$date, mp$d20.max, type = "l")
lines(macpass$date, macpass$d20.max, col = "red")
macpass$d20.max <- mp$d20.max

#### Min ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(d20.min)) %>% 
  do(model = lm(d20.min ~ d2150.min, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(d20.min = ifelse(is.na(d20.min), pred, d20.min))
plot(mp$date, mp$d20.min, type = "l")
lines(macpass$date, macpass$d20.min, col = "red")
macpass$d20.min <- mp$d20.min

### Goose Flats - ground surface T ----

#### Mean ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(gf0.mean)) %>% 
  do(model = lm(gf0.mean ~ gf150.mean, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(gf0.mean = ifelse(is.na(gf0.mean), pred, gf0.mean))
plot(mp$date, mp$gf0.mean, type = "l")
lines(macpass$date, macpass$gf0.mean, col = "red")
macpass$gf0.mean <- mp$gf0.mean

#### Max ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(gf0.max)) %>% 
  do(model = lm(gf0.max ~ gf150.max, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(gf0.max = ifelse(is.na(gf0.max), pred, gf0.max))
plot(mp$date, mp$gf0.max, type = "l")
lines(macpass$date, macpass$gf0.max, col = "red")
macpass$gf0.max <- mp$gf0.max

#### Min ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(gf0.min)) %>% 
  do(model = lm(gf0.min ~ gf150.min, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(gf0.min = ifelse(is.na(gf0.min), pred, gf0.min))
plot(mp$date, mp$gf0.min, type = "l")
lines(macpass$date, macpass$gf0.min, col = "red")
macpass$gf0.min <- mp$gf0.min

### Snow Fence - ground surface T ----

#### Mean ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(sf0.mean)) %>% 
  do(model = lm(sf0.mean ~ sf150.mean, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(sf0.mean = ifelse(is.na(sf0.mean), pred, sf0.mean))
plot(mp$date, mp$sf0.mean, type = "l")
lines(macpass$date, macpass$sf0.mean, col = "red")
macpass$sf0.mean <- mp$sf0.mean

#### Max ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(sf0.max)) %>% 
  do(model = lm(sf0.max ~ sf150.max, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(sf0.max = ifelse(is.na(sf0.max), pred, sf0.max))
plot(mp$date, mp$sf0.max, type = "l")
lines(macpass$date, macpass$sf0.max, col = "red")
macpass$sf0.max <- mp$sf0.max

#### Min ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(sf0.min)) %>% 
  do(model = lm(sf0.min ~ sf150.min, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(sf0.min = ifelse(is.na(sf0.min), pred, sf0.min))
plot(mp$date, mp$sf0.min, type = "l")
lines(macpass$date, macpass$sf0.min, col = "red")
macpass$sf0.min <- mp$sf0.min

write.csv(macpass, "~/Dropbox/School/Postdoc/Earthwatch/Mackenzie Mountains/GNWT/MM_daily_2022_filled.csv")

## The below code no longer works since I changed the code used for gap-filling

# df.list <- c(paste("bp150.min",seq(1,12), "fit", sep = "."),
#              paste("bp150.mean",seq(1,12), "fit", sep = "."),
#              paste("bp150.max",seq(1,12), "fit", sep = "."),
#              paste("bp0.min",seq(1,12), "fit", sep = "."),
#              paste("bp0.mean",seq(1,12), "fit", sep = "."),
#              paste("bp0.max",seq(1,12), "fit", sep = "."),
#              paste("hf150.min",seq(1,12), "fit", sep = "."),
#              paste("hf150.mean",seq(1,12), "fit", sep = "."),
#              paste("hf150.max",seq(1,12), "fit", sep = "."),
#              paste("hf0.min",seq(1,12), "fit", sep = "."),
#              paste("hf0.mean",seq(1,12), "fit", sep = "."),
#              paste("hf0.max",seq(1,12), "fit", sep = "."),
#              paste("d6150.min",seq(1,12), "fit", sep = "."),
#              paste("d6150.mean",seq(1,12), "fit", sep = "."),
#              paste("d6150.max",seq(1,12), "fit", sep = "."),
#              paste("d60.min",seq(1,12), "fit", sep = "."),
#              paste("d60.mean",seq(1,12), "fit", sep = "."),
#              paste("d60.max",seq(1,12), "fit", sep = "."),
#              paste("d2150.min",seq(1,12), "fit", sep = "."),
#              paste("d2150.mean",seq(1,12), "fit", sep = "."),
#              paste("d2150.max",seq(1,12), "fit", sep = "."),
#              paste("d20.min",seq(1,12), "fit", sep = "."),
#              paste("d20.mean",seq(1,12), "fit", sep = "."),
#              paste("d20.max",seq(1,12), "fit", sep = "."),
#              paste("gf150.min",seq(1,12), "fit", sep = "."),
#              paste("gf150.mean",seq(1,12), "fit", sep = "."),
#              paste("gf150.max",seq(1,12), "fit", sep = "."),
#              paste("gf0.min",seq(1,12), "fit", sep = "."),
#              paste("gf0.mean",seq(1,12), "fit", sep = "."),
#              paste("gf0.max",seq(1,12), "fit", sep = "."),
#              paste("sf150.min",seq(1,12), "fit", sep = "."),
#              paste("sf150.mean",seq(1,12), "fit", sep = "."),
#              paste("sf150.max",seq(1,12), "fit", sep = "."),
#              paste("sf0.min",seq(1,12), "fit", sep = "."),
#              paste("sf0.mean",seq(1,12), "fit", sep = "."),
#              paste("sf0.max",seq(1,12), "fit", sep = "."))
#              
#              
# df_list <- lapply(df.list, get)                     # Convert that vector to a list
# 
# coefficients <- unlist(sapply(df_list, RsquareAdj))
# 
# r2.df <- cbind.data.frame(model = c(paste("bp150min",seq(1,12), "fit", sep = "."),
#                                     paste("bp150mean",seq(1,12), "fit", sep = "."),
#                                     paste("bp150max",seq(1,12), "fit", sep = "."),
#                                     paste("bp0min",seq(1,12), "fit", sep = "."),
#                                     paste("bp0mean",seq(1,12), "fit", sep = "."),
#                                     paste("bp0max",seq(1,12), "fit", sep = "."),
#                                     paste("hf150min",seq(1,12), "fit", sep = "."),
#                                     paste("hf150mean",seq(1,12), "fit", sep = "."),
#                                     paste("hf150max",seq(1,12), "fit", sep = "."),
#                                     paste("hf0min",seq(1,12), "fit", sep = "."),
#                                     paste("hf0mean",seq(1,12), "fit", sep = "."),
#                                     paste("hf0max",seq(1,12), "fit", sep = "."),
#                                     paste("d6150min",seq(1,12), "fit", sep = "."),
#                                     paste("d6150mean",seq(1,12), "fit", sep = "."),
#                                     paste("d6150max",seq(1,12), "fit", sep = "."),
#                                     paste("d60min",seq(1,12), "fit", sep = "."),
#                                     paste("d60mean",seq(1,12), "fit", sep = "."),
#                                     paste("d60max",seq(1,12), "fit", sep = "."),
#                                     paste("d2150min",seq(1,12), "fit", sep = "."),
#                                     paste("d2150mean",seq(1,12), "fit", sep = "."),
#                                     paste("d2150max",seq(1,12), "fit", sep = "."),
#                                     paste("d20min",seq(1,12), "fit", sep = "."),
#                                     paste("d20mean",seq(1,12), "fit", sep = "."),
#                                     paste("d20max",seq(1,12), "fit", sep = "."),
#                                     paste("gf150min",seq(1,12), "fit", sep = "."),
#                                     paste("gf150mean",seq(1,12), "fit", sep = "."),
#                                     paste("gf150max",seq(1,12), "fit", sep = "."),
#                                     paste("gf0min",seq(1,12), "fit", sep = "."),
#                                     paste("gf0mean",seq(1,12), "fit", sep = "."),
#                                     paste("gf0max",seq(1,12), "fit", sep = "."),
#                                     paste("sf150min",seq(1,12), "fit", sep = "."),
#                                     paste("sf150mean",seq(1,12), "fit", sep = "."),
#                                     paste("sf150max",seq(1,12), "fit", sep = "."),
#                                     paste("sf0min",seq(1,12), "fit", sep = "."),
#                                     paste("sf0mean",seq(1,12), "fit", sep = "."),
#                                     paste("sf0max",seq(1,12), "fit", sep = ".")),
#                           r2 = coefficients[seq(1,length(coefficients),2)])
# bp.air.min <- density(r2.df[c(1:12),2])
# bp.air.mean <- density(r2.df[c(13:24),2])
# bp.air.max <- density(r2.df[c(25:36),2])
# bp.grd.min <- density(r2.df[c(37:48),2])
# bp.grd.mean <- density(r2.df[c(49:60),2])
# bp.grd.max <- density(r2.df[c(61:72),2])
# 
# hf.air.min <- density(r2.df[c(73:84),2])
# hf.air.mean <- density(r2.df[c(85:96),2])
# hf.air.max <- density(r2.df[c(97:108),2])
# hf.grd.min <- density(r2.df[c(109:120),2])
# hf.grd.mean <- density(r2.df[c(121:132),2])
# hf.grd.max <- density(r2.df[c(133:144),2])
# 
# d6.air.min <- density(r2.df[c(145:156),2])
# d6.air.mean <- density(r2.df[c(157:168),2])
# d6.air.max <- density(r2.df[c(169:180),2])
# d6.grd.min <- density(r2.df[c(181:192),2])
# d6.grd.mean <- density(r2.df[c(193:204),2])
# d6.grd.max <- density(r2.df[c(205:216),2])
# 
# d2.air.min <- density(r2.df[c(217:228),2])
# d2.air.mean <- density(r2.df[c(229:240),2])
# d2.air.max <- density(r2.df[c(241:252),2])
# d2.grd.min <- density(r2.df[c(253:264),2])
# d2.grd.mean <- density(r2.df[c(265:276),2])
# d2.grd.max <- density(r2.df[c(277:288),2])
# 
# gf.air.min <- density(r2.df[c(289:300),2])
# gf.air.mean <- density(r2.df[c(301:312),2])
# gf.air.max <- density(r2.df[c(313:324),2])
# gf.grd.min <- density(r2.df[c(325:336),2])
# gf.grd.mean <- density(r2.df[c(337:348),2])
# gf.grd.max <- density(r2.df[c(349:360),2])
# 
# sf.air.min <- density(r2.df[c(361:372),2])
# sf.air.mean <- density(r2.df[c(373:384),2])
# sf.air.max <- density(r2.df[c(385:396),2])
# sf.grd.min <- density(r2.df[c(397:408),2])
# sf.grd.mean <- density(r2.df[c(409:420),2])
# sf.grd.max <- density(r2.df[c(421:432),2])
# 
# jpeg("bp.jpg", width = 6, height = 6, units = "in", res = 300)
# par(mfcol = c(3,2), mar = c(1.5,1.5,1,1), oma = c(2,2,0,0))
# plot(bp.air.min$x, bp.air.min$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(1:12),2]), lty = 3)
# legend("topleft", expression("Air"["min"]), bty = "n", cex = 0.85)
# plot(bp.air.mean$x, bp.air.mean$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(13:24),2]), lty = 3)
# legend("topleft", expression("Air"["mean"]), bty = "n", cex = 0.85)
# plot(bp.air.max$x, bp.air.max$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(25:36),2]), lty = 3)
# legend("topleft", expression("Air"["max"]), bty = "n", cex = 0.85)
# plot(bp.grd.min$x, bp.grd.min$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(37:48),2]), lty = 3)
# legend("topleft", expression("Surface"["min"]), bty = "n", cex = 0.85)
# plot(bp.grd.mean$x, bp.grd.mean$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(49:60),2]), lty = 3)
# legend("topleft", expression("Surface"["mean"]), bty = "n", cex = 0.85)
# plot(bp.grd.max$x, bp.grd.max$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(61:72),2]), lty = 3)
# legend("topleft", expression("Surface"["max"]), bty = "n", cex = 0.85)
# mtext(substitute(paste(italic(r)^2)), side = 1, outer = T, cex = 0.8, line = 0.7)
# mtext("Density", side = 2, outer = T, line = 0.7, cex = 0.7)
# dev.off()
# 
# jpeg("hf.jpg", width = 6, height = 6, units = "in", res = 300)
# par(mfcol = c(3,2), mar = c(1.5,1.5,1,1), oma = c(2,2,0,0))
# plot(hf.air.min$x, hf.air.min$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(73:84),2]), lty = 3)
# legend("topleft", expression("Air"["min"]), bty = "n", cex = 0.85)
# plot(hf.air.mean$x, hf.air.mean$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(85:96),2]), lty = 3)
# legend("topleft", expression("Air"["mean"]), bty = "n", cex = 0.85)
# plot(hf.air.max$x, hf.air.max$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(97:108),2]), lty = 3)
# legend("topleft", expression("Air"["max"]), bty = "n", cex = 0.85)
# plot(hf.grd.min$x, hf.grd.min$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(109:120),2]), lty = 3)
# legend("topleft", expression("Surface"["min"]), bty = "n", cex = 0.85)
# plot(hf.grd.mean$x, hf.grd.mean$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(121:132),2]), lty = 3)
# legend("topleft", expression("Surface"["mean"]), bty = "n", cex = 0.85)
# plot(hf.grd.max$x, hf.grd.max$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(133:144),2]), lty = 3)
# legend("topleft", expression("Surface"["max"]), bty = "n", cex = 0.85)
# mtext(substitute(paste(italic(r)^2)), side = 1, outer = T, cex = 0.8, line = 0.7)
# mtext("Density", side = 2, outer = T, line = 0.7, cex = 0.7)
# dev.off()
# 
# jpeg("d6.jpg", width = 6, height = 6, units = "in", res = 300)
# par(mfcol = c(3,2), mar = c(1.5,1.5,1,1), oma = c(2,2,0,0))
# plot(d6.air.min$x, d6.air.min$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(145:156),2]), lty = 3)
# legend("topleft", expression("Air"["min"]), bty = "n", cex = 0.85)
# plot(d6.air.mean$x, d6.air.mean$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(157:168),2]), lty = 3)
# legend("topleft", expression("Air"["mean"]), bty = "n", cex = 0.85)
# plot(d6.air.max$x, d6.air.max$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(169:180),2]), lty = 3)
# legend("topleft", expression("Air"["max"]), bty = "n", cex = 0.85)
# plot(d6.grd.min$x, d6.grd.min$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(181:192),2]), lty = 3)
# legend("topleft", expression("Surface"["min"]), bty = "n", cex = 0.85)
# plot(d6.grd.mean$x, d6.grd.mean$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(193:204),2]), lty = 3)
# legend("topleft", expression("Surface"["mean"]), bty = "n", cex = 0.85)
# plot(d6.grd.max$x, d6.grd.max$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(205:216),2]), lty = 3)
# legend("topleft", expression("Surface"["max"]), bty = "n", cex = 0.85)
# mtext(substitute(paste(italic(r)^2)), side = 1, outer = T, cex = 0.8, line = 0.7)
# mtext("Density", side = 2, outer = T, line = 0.7, cex = 0.7)
# dev.off()
# 
# jpeg("d2.jpg", width = 6, height = 6, units = "in", res = 300)
# par(mfcol = c(3,2), mar = c(1.5,1.5,1,1), oma = c(2,2,0,0))
# plot(d2.air.min$x, d2.air.min$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(217:228),2]), lty = 3)
# legend("topleft", expression("Air"["min"]), bty = "n", cex = 0.85)
# plot(d2.air.mean$x, d2.air.mean$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(229:240),2]), lty = 3)
# legend("topleft", expression("Air"["mean"]), bty = "n", cex = 0.85)
# plot(d2.air.max$x, d2.air.max$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(241:252),2]), lty = 3)
# legend("topleft", expression("Air"["max"]), bty = "n", cex = 0.85)
# plot(d2.grd.min$x, d2.grd.min$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(253:264),2]), lty = 3)
# legend("topleft", expression("Surface"["min"]), bty = "n", cex = 0.85)
# plot(d2.grd.mean$x, d2.grd.mean$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(265:276),2]), lty = 3)
# legend("topleft", expression("Surface"["mean"]), bty = "n", cex = 0.85)
# plot(d2.grd.max$x, d2.grd.max$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(277:288),2]), lty = 3)
# legend("topleft", expression("Surface"["max"]), bty = "n", cex = 0.85)
# mtext(substitute(paste(italic(r)^2)), side = 1, outer = T, cex = 0.8, line = 0.7)
# mtext("Density", side = 2, outer = T, line = 0.7, cex = 0.7)
# dev.off()
# 
# jpeg("gf.jpg", width = 6, height = 6, units = "in", res = 300)
# par(mfcol = c(3,2), mar = c(1.5,1.5,1,1), oma = c(2,2,0,0))
# plot(gf.air.min$x, gf.air.min$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(289:300),2]), lty = 3)
# legend("topleft", expression("Air"["min"]), bty = "n", cex = 0.85)
# plot(gf.air.mean$x, gf.air.mean$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(301:312),2]), lty = 3)
# legend("topleft", expression("Air"["mean"]), bty = "n", cex = 0.85)
# plot(gf.air.max$x, gf.air.max$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(313:324),2]), lty = 3)
# legend("topleft", expression("Air"["max"]), bty = "n", cex = 0.85)
# plot(gf.grd.min$x, gf.grd.min$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(325:336),2]), lty = 3)
# legend("topleft", expression("Surface"["min"]), bty = "n", cex = 0.85)
# plot(gf.grd.mean$x, gf.grd.mean$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(337:348),2]), lty = 3)
# legend("topleft", expression("Surface"["mean"]), bty = "n", cex = 0.85)
# plot(gf.grd.max$x, gf.grd.max$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(349:360),2]), lty = 3)
# legend("topleft", expression("Surface"["max"]), bty = "n", cex = 0.85)
# mtext(substitute(paste(italic(r)^2)), side = 1, outer = T, cex = 0.8, line = 0.7)
# mtext("Density", side = 2, outer = T, line = 0.7, cex = 0.7)
# dev.off()
# 
# jpeg("sf.jpg", width = 6, height = 6, units = "in", res = 300)
# par(mfcol = c(3,2), mar = c(1.5,1.5,1,1), oma = c(2,2,0,0))
# plot(sf.air.min$x, sf.air.min$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(361:372),2]), lty = 3)
# legend("topleft", expression("Air"["min"]), bty = "n", cex = 0.85)
# plot(sf.air.mean$x, sf.air.mean$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(373:384),2]), lty = 3)
# legend("topleft", expression("Air"["mean"]), bty = "n", cex = 0.85)
# plot(sf.air.max$x, sf.air.max$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(385:396),2]), lty = 3)
# legend("topleft", expression("Air"["max"]), bty = "n", cex = 0.85)
# plot(sf.grd.min$x, sf.grd.min$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(397:408),2]), lty = 3)
# legend("topleft", expression("Surface"["min"]), bty = "n", cex = 0.85)
# plot(sf.grd.mean$x, sf.grd.mean$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(409:420),2]), lty = 3)
# legend("topleft", expression("Surface"["mean"]), bty = "n", cex = 0.85)
# plot(sf.grd.max$x, sf.grd.max$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(421:432),2]), lty = 3)
# legend("topleft", expression("Surface"["max"]), bty = "n", cex = 0.85)
# mtext(substitute(paste(italic(r)^2)), side = 1, outer = T, cex = 0.8, line = 0.7)
# mtext("Density", side = 2, outer = T, line = 0.7, cex = 0.7)
# dev.off()
# 
