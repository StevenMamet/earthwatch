# install.packages("weathercan",
#                  repos = c("https://ropensci.r-universe.dev",
#                            "https://cloud.r-project.org"))

rm(list=ls())

#______________________________----
# Libraries ----
library(tidyverse)    # Tidy data
library(splines)      # Spline-fitting
library(psych)        # Pairs plot
library(lubridate)    # Wrangling dates
library(dplyr)        # Tidy notation
library(scales)       # Rescaling data and alpha
library(forecast)     # tsclean()
library(purrr)        # Functional programming tools
library(weathercan)   # Get weather station details from Environment Canada
library(patchwork)    # Tidy organizing of plots
library(wesanderson)  # Nice color palettes
library(modelr)       # Tidy modeling

#______________________________----
# Constants ----
col_pal <- wes_palette("Darjeeling1")
setwd("~/Desktop/Workspace")
time_zone <- "Canada/Yukon"
time_unit <- "hour"
weather_station <- "MACMILLAN PASS"
data_output <- "./Earthwatch/MacPass/"
location <- "MacPass"
p <- c("MacPass_EnvCan_19980628")

#______________________________----
# Functions----
source("./Earthwatch/R_functions/R_functions.R")

#______________________________----
# Read in station data ----
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

#______________________________----
# 👾 Step 1: Environment Canada data ----
# If recent data are needed, download here.
# Otherwise, read in the existing data below

# Get the correct station ID, make a df using that ID and the time period of interest, and download
# temp_df <- tibble(
#   Date = as_date(c(min(macpass$Date), "2024-08-12"))
# )
# weather_df <- weather_download(temp_df, weather_station, time_zone, time_unit, location)

# If already downloaded:
weather_df <- read_most_recent_weather(data_output, p)

weather_df %>%
  ggplot(aes(x = as_datetime(datetime), y = station_temp)) + geom_line()

macpass <- weather_join(weather_df, macpass)

macpass %>%
  ggplot(aes(x = as_datetime(Date), y = mean.150)) + geom_line()

#______________________________----
# 🫶 Step 2: Fill missing air temperatures ----

# Now can fill each month using EnvCan
# Select columns that match the criteria
air_t_cols <- macpass %>%
  select(matches("150"), -matches("neg150")) %>%
  names()

# Fill missing values using linear models
macpass <- macpass %>%
  mutate(month = as.integer(month(Date))) %>%
  group_by(month) %>%
  nest() %>%
  mutate(filled = map(data, ~ fill_missing_with_lm(.x, vars = air_t_cols, fill_var = "mean.150"))) %>%
  select(month, filled) %>%
  unnest(cols = c(filled)) %>%
  arrange(Date) %>%
  ungroup()

# Create the initial ggplot object with the x-aesthetic
p <- ggplot(macpass, aes(x = Date))

# Use lapply to create a list of geom_line layers for each selected column
line_layers <- lapply(1:length(air_t_cols), function(i) {
  geom_line(aes(y = .data[[air_t_cols[i]]]), color = i)  # Using i as a placeholder for color
})

# Add all the geom_line layers to the ggplot object
p + line_layers

macpass <- macpass %>% 
  arrange(Date)

#______________________________----
# 🤠 Step 3: Fill the missing ground surface temperatures ----

# Select columns that end with a zero and do not contain other numbers
ground_surface_cols <- macpass %>%
  select(matches("^[^0-9]*0$"), matches("d2.0"), matches("d6.0")) %>%
  colnames()
ground_surface_cols <- ground_surface_cols[c(2,1,5,4,3)]

#_________----
## Hare Foot - ground surface T ----

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
mp %>% ggplot(aes(x = Date, y = .data[[ground_surface_cols[1]]])) + geom_line() +
  geom_line(data = macpass, aes(y = .data[[ground_surface_cols[1]]]), color = "red")
macpass[[ground_surface_cols[1]]] <- mp[[ground_surface_cols[1]]]

#_________----
## Beaver Pond - ground surface T ----

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
mp %>% ggplot(aes(x = Date, y = .data[[ground_surface_cols[2]]])) + geom_line() +
  geom_line(data = macpass, aes(y = .data[[ground_surface_cols[2]]]), color = "red")
macpass[[ground_surface_cols[2]]] <- mp[[ground_surface_cols[2]]]

#_________----
## Dale 6 - ground surface T ----

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
mp %>% ggplot(aes(x = Date, y = .data[[ground_surface_cols[3]]])) + geom_line() +
  geom_line(data = macpass, aes(y = .data[[ground_surface_cols[3]]]), color = "red")
macpass[[ground_surface_cols[3]]] <- mp[[ground_surface_cols[3]]]

#_________----
## Dale 2 - ground surface T ----

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
mp %>% ggplot(aes(x = Date, y = .data[[ground_surface_cols[4]]])) + geom_line() +
  geom_line(data = macpass, aes(y = .data[[ground_surface_cols[4]]]), color = "red")
macpass[[ground_surface_cols[4]]] <- mp[[ground_surface_cols[4]]]

#_________----
## Goose Flats - ground surface T ----

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
mp %>% ggplot(aes(x = Date, y = .data[[ground_surface_cols[5]]])) + geom_line() +
  geom_line(data = macpass, aes(y = .data[[ground_surface_cols[5]]]), color = "red")
macpass[[ground_surface_cols[5]]] <- mp[[ground_surface_cols[5]]]

#______________________________----
# 👀 Step 4: Fill the missing -150 cm temperatures ----

# Select columns that end with neg150
sub_surface_cols <- macpass %>%
  select(matches("neg150")) %>%
  colnames()
sub_surface_cols <- sub_surface_cols[c(2,1,4,3,5)]

#_________----
## Hare Foot - -150 cm ----

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
mp %>% ggplot(aes(x = Date, y = .data[[sub_surface_cols[1]]])) + geom_line() +
  geom_line(data = macpass, aes(y = .data[[sub_surface_cols[1]]]), color = "red")
macpass[[sub_surface_cols[1]]] <- mp[[sub_surface_cols[1]]]

#_________----
## Beaver Pond - -150 cm ----

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
mp %>% ggplot(aes(x = Date, y = .data[[sub_surface_cols[2]]])) + geom_line() +
  geom_line(data = macpass, aes(y = .data[[sub_surface_cols[2]]]), color = "red")
macpass[[sub_surface_cols[2]]] <- mp[[sub_surface_cols[2]]]

#_________----
## Dale 6 - -150 cm ----

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
mp %>% ggplot(aes(x = Date, y = .data[[sub_surface_cols[3]]])) + geom_line() +
  geom_line(data = macpass, aes(y = .data[[sub_surface_cols[3]]]), color = "red")
macpass[[sub_surface_cols[3]]] <- mp[[sub_surface_cols[3]]]

#_________----
## Dale 2 - -150 cm ----

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
mp %>% ggplot(aes(x = Date, y = .data[[sub_surface_cols[4]]])) + geom_line() +
  geom_line(data = macpass, aes(y = .data[[sub_surface_cols[4]]]), color = "red")
macpass[[sub_surface_cols[4]]] <- mp[[sub_surface_cols[4]]]

#_________----
## Goose Flats - -150 cm ----

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
mp %>% ggplot(aes(x = Date, y = .data[[sub_surface_cols[5]]])) + geom_line() +
  geom_line(data = macpass, aes(y = .data[[sub_surface_cols[5]]]), color = "red")
macpass[[sub_surface_cols[5]]] <- mp[[sub_surface_cols[5]]]

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

