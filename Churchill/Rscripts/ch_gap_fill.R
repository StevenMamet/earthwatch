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
time_zone <- "Canada/Central"
time_unit <- "hour"
weather_station <- "CHURCHILL"
data_output <- "./Earthwatch/Churchill/"
location <- "Churchill"
p <- c("Churchill_EnvCan_20000101")

#______________________________----
# Functions----
source("./Earthwatch/R_functions/R_functions.R")

#______________________________----
# Read in station data ----
churchill <- read.csv("./Earthwatch/Churchill/data/ch_microclimate.csv", header = TRUE)
# churchill <- read.csv("~/Dropbox/School/School/Postdoc/Earthwatch/Mackenzie Mountains/GNWT/MM_daily.csv")

# Converts to date format (lubridate)
churchill$Date <- ymd(churchill$date1)
names(churchill)[3] <- "month"

churchill <- churchill %>% 
  mutate(pfr150 = as.numeric(pfr150),
         pfrneg80 = as.numeric(pfrneg80))

#______________________________----
# 👾 Step 1: Environment Canada data ----
# If recent data are needed, download here.
# Otherwise, read in the existing data below

# Get the correct station ID, make a df using that ID and the time period of interest, and download
# weather_df <- weather_download(churchill, weather_station, time_zone, time_unit, location)

# If already downloaded:
weather_df <- read_most_recent_weather(data_output, p)

weather_df %>%
  ggplot(aes(x = datetime, y = station_temp)) + geom_line()

churchill <- weather_df %>% weather_join(churchill)

churchill %>%
  ggplot(aes(x = Date, y = mean.150)) + geom_line()

#______________________________----
# 🫶 Step 2: Fill missing air temperatures ----

# Now can fill each month using EnvCan
# Select columns that match the criteria
air_t_cols <- churchill %>%
  select(matches("150"), -matches("neg150")) %>%
  names()

# Fill missing values using linear models
churchill <- churchill %>%
  mutate(month = as.integer(month(Date))) %>%
  group_by(month) %>%
  nest() %>%
  mutate(filled = map(data, ~ fill_missing_with_lm(.x, vars = air_t_cols, fill_var = "mean.150"))) %>%
  select(month, filled) %>%
  unnest(cols = c(filled)) %>%
  arrange(Date) %>%
  ungroup()

# Create the initial ggplot object with the x-aesthetic
p <- ggplot(churchill, aes(x = Date))

# Use lapply to create a list of geom_line layers for each selected column
line_layers <- lapply(1:length(air_t_cols), function(i) {
  geom_line(aes(y = .data[[air_t_cols[i]]]), color = i)  # Using i as a placeholder for color
})

# Add all the geom_line layers to the ggplot object
p + line_layers

churchill <- churchill %>% 
  arrange(Date)

#______________________________----
# 🤠 Step 3: Fill the missing ground surface temperatures ----

# Select columns that end with a zero and do not contain other numbers
ground_surface_cols <- churchill %>%
  select(matches("^[^0-9]*0$"), matches("rlkneg5")) %>%
  colnames()

#_________----
## AIR - ground surface T ----

# Construct linear models for ground surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(ground_surface_cols[1], " ~ ", 
                         air_t_cols[1], " + ", 
                         air_t_cols[2], " + ", 
                         air_t_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(ground_surface_cols[1]) := ifelse(is.na(.data[[ground_surface_cols[1]]]), pred, .data[[ground_surface_cols[1]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[ground_surface_cols[1]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[ground_surface_cols[1]]]), color = "red")
churchill[[ground_surface_cols[1]]] <- ch[[ground_surface_cols[1]]]

#_________----
## BSW - ground surface T ----

# Construct linear models for ground surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(ground_surface_cols[2], " ~ ", 
                         ground_surface_cols[1], " + ", 
                         air_t_cols[1], " + ", 
                         air_t_cols[2], " + ", 
                         air_t_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(ground_surface_cols[2]) := ifelse(is.na(.data[[ground_surface_cols[2]]]), pred, .data[[ground_surface_cols[2]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[ground_surface_cols[2]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[ground_surface_cols[2]]]), color = "red")
churchill[[ground_surface_cols[2]]] <- ch[[ground_surface_cols[2]]]

#_________----
## TIS - ground surface T ----

# Construct linear models for ground surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(ground_surface_cols[3], " ~ ", 
                         ground_surface_cols[1], " + ", 
                         ground_surface_cols[2], " + ", 
                         air_t_cols[1], " + ", 
                         air_t_cols[2], " + ", 
                         air_t_cols[3], " + ", 
                         air_t_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(ground_surface_cols[3]) := ifelse(is.na(.data[[ground_surface_cols[3]]]), pred, .data[[ground_surface_cols[3]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[ground_surface_cols[3]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[ground_surface_cols[3]]]), color = "red")
churchill[[ground_surface_cols[3]]] <- ch[[ground_surface_cols[3]]]

#_________----
## WSU - ground surface T ----

# Construct linear models for ground surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(ground_surface_cols[4], " ~ ", 
                         ground_surface_cols[1], " + ", 
                         ground_surface_cols[2], " + ", 
                         ground_surface_cols[3], " + ", 
                         air_t_cols[1], " + ", 
                         air_t_cols[2], " + ", 
                         air_t_cols[3], " + ", 
                         air_t_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(ground_surface_cols[4]) := ifelse(is.na(.data[[ground_surface_cols[4]]]), pred, .data[[ground_surface_cols[4]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[ground_surface_cols[4]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[ground_surface_cols[4]]]), color = "red")
churchill[[ground_surface_cols[4]]] <- ch[[ground_surface_cols[4]]]

#_________----
## MLK - ground surface T ----

# Construct linear models for ground surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(ground_surface_cols[5], " ~ ", 
                         ground_surface_cols[1], " + ", 
                         ground_surface_cols[2], " + ", 
                         ground_surface_cols[3], " + ", 
                         ground_surface_cols[4], " + ", 
                         air_t_cols[1], " + ", 
                         air_t_cols[2], " + ", 
                         air_t_cols[3], " + ", 
                         air_t_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(ground_surface_cols[5]) := ifelse(is.na(.data[[ground_surface_cols[5]]]), pred, .data[[ground_surface_cols[5]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[ground_surface_cols[5]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[ground_surface_cols[5]]]), color = "red")
churchill[[ground_surface_cols[5]]] <- ch[[ground_surface_cols[5]]]

#_________----
## FEN - ground surface T ----

# Construct linear models for ground surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(ground_surface_cols[6], " ~ ", 
                         ground_surface_cols[1], " + ", 
                         ground_surface_cols[2], " + ", 
                         ground_surface_cols[3], " + ", 
                         ground_surface_cols[4], " + ", 
                         ground_surface_cols[5], " + ", 
                         air_t_cols[1], " + ", 
                         air_t_cols[2], " + ", 
                         air_t_cols[3], " + ", 
                         air_t_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(ground_surface_cols[6]) := ifelse(is.na(.data[[ground_surface_cols[6]]]), pred, .data[[ground_surface_cols[6]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[ground_surface_cols[6]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[ground_surface_cols[6]]]), color = "red")
churchill[[ground_surface_cols[6]]] <- ch[[ground_surface_cols[6]]]

#_________----
## BFR - ground surface T ----

# Construct linear models for ground surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(ground_surface_cols[7], " ~ ", 
                         ground_surface_cols[1], " + ", 
                         ground_surface_cols[2], " + ", 
                         ground_surface_cols[3], " + ", 
                         ground_surface_cols[4], " + ", 
                         ground_surface_cols[5], " + ", 
                         ground_surface_cols[6], " + ", 
                         air_t_cols[1], " + ", 
                         air_t_cols[2], " + ", 
                         air_t_cols[3], " + ", 
                         air_t_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(ground_surface_cols[7]) := ifelse(is.na(.data[[ground_surface_cols[7]]]), pred, .data[[ground_surface_cols[7]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[ground_surface_cols[7]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[ground_surface_cols[7]]]), color = "red")
churchill[[ground_surface_cols[7]]] <- ch[[ground_surface_cols[7]]]

#_________----
## PFR - ground surface T ----

# Construct linear models for ground surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(ground_surface_cols[8], " ~ ", 
                         ground_surface_cols[1], " + ", 
                         ground_surface_cols[2], " + ", 
                         ground_surface_cols[3], " + ", 
                         ground_surface_cols[4], " + ", 
                         ground_surface_cols[5], " + ", 
                         ground_surface_cols[6], " + ", 
                         ground_surface_cols[7], " + ", 
                         air_t_cols[1], " + ", 
                         air_t_cols[2], " + ", 
                         air_t_cols[3], " + ", 
                         air_t_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(ground_surface_cols[8]) := ifelse(is.na(.data[[ground_surface_cols[8]]]), pred, .data[[ground_surface_cols[8]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[ground_surface_cols[8]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[ground_surface_cols[8]]]), color = "red")
churchill[[ground_surface_cols[8]]] <- ch[[ground_surface_cols[8]]]

#_________----
## BWP - ground surface T ----

# Construct linear models for ground surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(ground_surface_cols[9], " ~ ", 
                         ground_surface_cols[1], " + ", 
                         ground_surface_cols[2], " + ", 
                         ground_surface_cols[3], " + ", 
                         ground_surface_cols[4], " + ", 
                         ground_surface_cols[5], " + ", 
                         ground_surface_cols[6], " + ", 
                         ground_surface_cols[7], " + ", 
                         air_t_cols[1], " + ", 
                         air_t_cols[2], " + ", 
                         air_t_cols[3], " + ", 
                         air_t_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(ground_surface_cols[9]) := ifelse(is.na(.data[[ground_surface_cols[9]]]), pred, .data[[ground_surface_cols[9]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[ground_surface_cols[9]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[ground_surface_cols[9]]]), color = "red")
churchill[[ground_surface_cols[9]]] <- ch[[ground_surface_cols[9]]]

#_________----
## PPA - ground surface T ----

# Construct linear models for ground surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(ground_surface_cols[10], " ~ ", 
                         ground_surface_cols[1], " + ", 
                         ground_surface_cols[2], " + ", 
                         ground_surface_cols[3], " + ", 
                         ground_surface_cols[4], " + ", 
                         ground_surface_cols[5], " + ", 
                         ground_surface_cols[6], " + ", 
                         ground_surface_cols[7], " + ", 
                         air_t_cols[1], " + ", 
                         air_t_cols[2], " + ", 
                         air_t_cols[3], " + ", 
                         air_t_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(ground_surface_cols[10]) := ifelse(is.na(.data[[ground_surface_cols[10]]]), pred, .data[[ground_surface_cols[10]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[ground_surface_cols[10]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[ground_surface_cols[10]]]), color = "red")
churchill[[ground_surface_cols[10]]] <- ch[[ground_surface_cols[10]]]

#_________----
## PPD - ground surface T ----

# Construct linear models for ground surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(ground_surface_cols[11], " ~ ", 
                         ground_surface_cols[1], " + ", 
                         ground_surface_cols[2], " + ", 
                         ground_surface_cols[3], " + ", 
                         ground_surface_cols[4], " + ", 
                         ground_surface_cols[5], " + ", 
                         ground_surface_cols[6], " + ", 
                         ground_surface_cols[7], " + ", 
                         air_t_cols[1], " + ", 
                         air_t_cols[2], " + ", 
                         air_t_cols[3], " + ", 
                         air_t_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(ground_surface_cols[11]) := ifelse(is.na(.data[[ground_surface_cols[11]]]), pred, .data[[ground_surface_cols[11]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[ground_surface_cols[11]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[ground_surface_cols[11]]]), color = "red")
churchill[[ground_surface_cols[11]]] <- ch[[ground_surface_cols[11]]]

#_________----
## TUN - ground surface T ----

# Construct linear models for ground surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(ground_surface_cols[12], " ~ ", 
                         ground_surface_cols[1], " + ", 
                         ground_surface_cols[2], " + ", 
                         ground_surface_cols[3], " + ", 
                         ground_surface_cols[4], " + ", 
                         ground_surface_cols[5], " + ", 
                         ground_surface_cols[6], " + ", 
                         ground_surface_cols[7], " + ", 
                         air_t_cols[1], " + ", 
                         air_t_cols[2], " + ", 
                         air_t_cols[3], " + ", 
                         air_t_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(ground_surface_cols[12]) := ifelse(is.na(.data[[ground_surface_cols[12]]]), pred, .data[[ground_surface_cols[12]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[ground_surface_cols[12]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[ground_surface_cols[12]]]), color = "red")
churchill[[ground_surface_cols[12]]] <- ch[[ground_surface_cols[12]]]

#_________----
## RLK - ground surface T ----

# Construct linear models for ground surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(ground_surface_cols[13], " ~ ", 
                         ground_surface_cols[1], " + ", 
                         ground_surface_cols[2], " + ", 
                         ground_surface_cols[3], " + ", 
                         ground_surface_cols[4], " + ", 
                         ground_surface_cols[5], " + ", 
                         ground_surface_cols[6], " + ", 
                         ground_surface_cols[7], " + ", 
                         air_t_cols[1], " + ", 
                         air_t_cols[2], " + ", 
                         air_t_cols[3], " + ", 
                         air_t_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(ground_surface_cols[13]) := ifelse(is.na(.data[[ground_surface_cols[13]]]), pred, .data[[ground_surface_cols[13]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[ground_surface_cols[13]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[ground_surface_cols[13]]]), color = "red")
churchill[[ground_surface_cols[13]]] <- ch[[ground_surface_cols[13]]]

#______________________________----
# 👀 Step 4: Fill the missing -150 cm temperatures ----

# Select columns that end with a zero and do not contain other numbers
sub_surface_cols <- churchill %>%
  select(matches("neg80")) %>%
  colnames()

#_________----
## AIR - sub-surface T ----

# Construct linear models for ground surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(sub_surface_cols[1], " ~ ", 
                         ground_surface_cols[1], " + ", 
                         ground_surface_cols[2], " + ", 
                         ground_surface_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(sub_surface_cols[1]) := ifelse(is.na(.data[[sub_surface_cols[1]]]), pred, .data[[sub_surface_cols[1]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[sub_surface_cols[1]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[sub_surface_cols[1]]]), color = "red")
churchill[[sub_surface_cols[1]]] <- ch[[sub_surface_cols[1]]]

#_________----
## BSW - sub- surface T ----

# Construct linear models for sub- surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(sub_surface_cols[2], " ~ ", 
                         sub_surface_cols[1], " + ", 
                         ground_surface_cols[1], " + ", 
                         ground_surface_cols[2], " + ", 
                         ground_surface_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(sub_surface_cols[2]) := ifelse(is.na(.data[[sub_surface_cols[2]]]), pred, .data[[sub_surface_cols[2]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[sub_surface_cols[2]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[sub_surface_cols[2]]]), color = "red")
churchill[[sub_surface_cols[2]]] <- ch[[sub_surface_cols[2]]]

#_________----
## TIS - sub- surface T ----

# Construct linear models for sub- surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(sub_surface_cols[3], " ~ ", 
                         sub_surface_cols[1], " + ", 
                         sub_surface_cols[2], " + ", 
                         ground_surface_cols[1], " + ", 
                         ground_surface_cols[2], " + ", 
                         ground_surface_cols[3], " + ", 
                         ground_surface_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(sub_surface_cols[3]) := ifelse(is.na(.data[[sub_surface_cols[3]]]), pred, .data[[sub_surface_cols[3]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[sub_surface_cols[3]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[sub_surface_cols[3]]]), color = "red")
churchill[[sub_surface_cols[3]]] <- ch[[sub_surface_cols[3]]]

#_________----
## WSU - sub- surface T ----

# Construct linear models for sub- surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(sub_surface_cols[4], " ~ ", 
                         sub_surface_cols[1], " + ", 
                         sub_surface_cols[2], " + ", 
                         sub_surface_cols[3], " + ", 
                         ground_surface_cols[1], " + ", 
                         ground_surface_cols[2], " + ", 
                         ground_surface_cols[3], " + ", 
                         ground_surface_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(sub_surface_cols[4]) := ifelse(is.na(.data[[sub_surface_cols[4]]]), pred, .data[[sub_surface_cols[4]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[sub_surface_cols[4]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[sub_surface_cols[4]]]), color = "red")
churchill[[sub_surface_cols[4]]] <- ch[[sub_surface_cols[4]]]

#_________----
## MLK - sub- surface T ----

# Construct linear models for sub- surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(sub_surface_cols[5], " ~ ", 
                         sub_surface_cols[1], " + ", 
                         sub_surface_cols[2], " + ", 
                         sub_surface_cols[3], " + ", 
                         sub_surface_cols[4], " + ", 
                         ground_surface_cols[1], " + ", 
                         ground_surface_cols[2], " + ", 
                         ground_surface_cols[3], " + ", 
                         ground_surface_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(sub_surface_cols[5]) := ifelse(is.na(.data[[sub_surface_cols[5]]]), pred, .data[[sub_surface_cols[5]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[sub_surface_cols[5]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[sub_surface_cols[5]]]), color = "red")
churchill[[sub_surface_cols[5]]] <- ch[[sub_surface_cols[5]]]

#_________----
## FEN - sub- surface T ----

# Construct linear models for sub- surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(sub_surface_cols[6], " ~ ", 
                         sub_surface_cols[1], " + ", 
                         sub_surface_cols[2], " + ", 
                         sub_surface_cols[3], " + ", 
                         sub_surface_cols[4], " + ", 
                         sub_surface_cols[5], " + ", 
                         ground_surface_cols[1], " + ", 
                         ground_surface_cols[2], " + ", 
                         ground_surface_cols[3], " + ", 
                         ground_surface_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(sub_surface_cols[6]) := ifelse(is.na(.data[[sub_surface_cols[6]]]), pred, .data[[sub_surface_cols[6]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[sub_surface_cols[6]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[sub_surface_cols[6]]]), color = "red")
churchill[[sub_surface_cols[6]]] <- ch[[sub_surface_cols[6]]]

#_________----
## BFR - sub- surface T ----

# Construct linear models for sub- surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(sub_surface_cols[7], " ~ ", 
                         sub_surface_cols[1], " + ", 
                         sub_surface_cols[2], " + ", 
                         sub_surface_cols[3], " + ", 
                         sub_surface_cols[4], " + ", 
                         sub_surface_cols[5], " + ", 
                         sub_surface_cols[6], " + ", 
                         ground_surface_cols[1], " + ", 
                         ground_surface_cols[2], " + ", 
                         ground_surface_cols[3], " + ", 
                         ground_surface_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(sub_surface_cols[7]) := ifelse(is.na(.data[[sub_surface_cols[7]]]), pred, .data[[sub_surface_cols[7]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[sub_surface_cols[7]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[sub_surface_cols[7]]]), color = "red")
churchill[[sub_surface_cols[7]]] <- ch[[sub_surface_cols[7]]]

#_________----
## PFR - sub- surface T ----

# Construct linear models for sub- surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(sub_surface_cols[8], " ~ ", 
                         sub_surface_cols[1], " + ", 
                         sub_surface_cols[2], " + ", 
                         sub_surface_cols[3], " + ", 
                         sub_surface_cols[4], " + ", 
                         sub_surface_cols[5], " + ", 
                         sub_surface_cols[6], " + ", 
                         sub_surface_cols[7], " + ", 
                         ground_surface_cols[1], " + ", 
                         ground_surface_cols[2], " + ", 
                         ground_surface_cols[3], " + ", 
                         ground_surface_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(sub_surface_cols[8]) := ifelse(is.na(.data[[sub_surface_cols[8]]]), pred, .data[[sub_surface_cols[8]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[sub_surface_cols[8]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[sub_surface_cols[8]]]), color = "red")
churchill[[sub_surface_cols[8]]] <- ch[[sub_surface_cols[8]]]

#_________----
## BWP - sub- surface T ----

# Construct linear models for sub- surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(sub_surface_cols[9], " ~ ", 
                         sub_surface_cols[1], " + ", 
                         sub_surface_cols[2], " + ", 
                         sub_surface_cols[3], " + ", 
                         sub_surface_cols[4], " + ", 
                         sub_surface_cols[5], " + ", 
                         sub_surface_cols[6], " + ", 
                         sub_surface_cols[7], " + ", 
                         ground_surface_cols[1], " + ", 
                         ground_surface_cols[2], " + ", 
                         ground_surface_cols[3], " + ", 
                         ground_surface_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(sub_surface_cols[9]) := ifelse(is.na(.data[[sub_surface_cols[9]]]), pred, .data[[sub_surface_cols[9]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[sub_surface_cols[9]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[sub_surface_cols[9]]]), color = "red")
churchill[[sub_surface_cols[9]]] <- ch[[sub_surface_cols[9]]]

#_________----
## PPA - sub- surface T ----

# Construct linear models for sub- surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(sub_surface_cols[10], " ~ ", 
                         sub_surface_cols[1], " + ", 
                         sub_surface_cols[2], " + ", 
                         sub_surface_cols[3], " + ", 
                         sub_surface_cols[4], " + ", 
                         sub_surface_cols[5], " + ", 
                         sub_surface_cols[6], " + ", 
                         sub_surface_cols[7], " + ", 
                         ground_surface_cols[1], " + ", 
                         ground_surface_cols[2], " + ", 
                         ground_surface_cols[3], " + ", 
                         ground_surface_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(sub_surface_cols[10]) := ifelse(is.na(.data[[sub_surface_cols[10]]]), pred, .data[[sub_surface_cols[10]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[sub_surface_cols[10]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[sub_surface_cols[10]]]), color = "red")
churchill[[sub_surface_cols[10]]] <- ch[[sub_surface_cols[10]]]

#_________----
## PPD - sub- surface T ----

# Construct linear models for sub- surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(sub_surface_cols[11], " ~ ", 
                         sub_surface_cols[1], " + ", 
                         sub_surface_cols[2], " + ", 
                         sub_surface_cols[3], " + ", 
                         sub_surface_cols[4], " + ", 
                         sub_surface_cols[5], " + ", 
                         sub_surface_cols[6], " + ", 
                         sub_surface_cols[7], " + ", 
                         ground_surface_cols[1], " + ", 
                         ground_surface_cols[2], " + ", 
                         ground_surface_cols[3], " + ", 
                         ground_surface_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(sub_surface_cols[11]) := ifelse(is.na(.data[[sub_surface_cols[11]]]), pred, .data[[sub_surface_cols[11]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[sub_surface_cols[11]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[sub_surface_cols[11]]]), color = "red")
churchill[[sub_surface_cols[11]]] <- ch[[sub_surface_cols[11]]]

#_________----
## TUN - sub- surface T ----

# Construct linear models for sub- surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(sub_surface_cols[12], " ~ ", 
                         sub_surface_cols[1], " + ", 
                         sub_surface_cols[2], " + ", 
                         sub_surface_cols[3], " + ", 
                         sub_surface_cols[4], " + ", 
                         sub_surface_cols[5], " + ", 
                         sub_surface_cols[6], " + ", 
                         sub_surface_cols[7], " + ", 
                         ground_surface_cols[1], " + ", 
                         ground_surface_cols[2], " + ", 
                         ground_surface_cols[3], " + ", 
                         ground_surface_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(sub_surface_cols[12]) := ifelse(is.na(.data[[sub_surface_cols[12]]]), pred, .data[[sub_surface_cols[12]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[sub_surface_cols[12]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[sub_surface_cols[12]]]), color = "red")
churchill[[sub_surface_cols[12]]] <- ch[[sub_surface_cols[12]]]

#_________----
## PPA - sub- surface T ----

# Construct linear models for sub- surface temperature prediction
models <- churchill %>%
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    lm(as.formula(paste0(sub_surface_cols[13], " ~ ", 
                         sub_surface_cols[1], " + ", 
                         sub_surface_cols[2], " + ", 
                         sub_surface_cols[3], " + ", 
                         sub_surface_cols[4], " + ", 
                         sub_surface_cols[5], " + ", 
                         sub_surface_cols[6], " + ", 
                         sub_surface_cols[7], " + ", 
                         ground_surface_cols[1], " + ", 
                         ground_surface_cols[2], " + ", 
                         ground_surface_cols[3], " + ", 
                         ground_surface_cols[4])), data = .x)
  })) %>%
  select(month, model)

# Apply predictions for each month separately
predictions <- churchill %>%
  nest(data = -month) %>%
  left_join(models, by = "month") %>%
  mutate(predicted = map2(data, model, ~ {
    .x %>%
      mutate(pred = predict(.y, newdata = .x))
  })) %>%
  select(month, predicted) %>%
  unnest(cols = c(predicted)) %>%
  ungroup()

# Update the original data frame with the predictions
ch <- predictions %>%
  mutate(!!sym(sub_surface_cols[13]) := ifelse(is.na(.data[[sub_surface_cols[13]]]), pred, .data[[sub_surface_cols[13]]])) %>%
  select(-any_of(c("model", "pred"))) %>% 
  arrange(Date)
ch %>% ggplot(aes(x = Date, y = .data[[sub_surface_cols[13]]])) + geom_line() +
  geom_line(data = churchill, aes(y = .data[[sub_surface_cols[13]]]), color = "red")
churchill[[sub_surface_cols[13]]] <- ch[[sub_surface_cols[13]]]

#______________________________----
# 👾 Step 5: Output the filled data ----

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

# p1 <- 
churchill %>% 
  ggplot(aes(x = Date, y = airp150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(x = Date, y = airp0), color = col_pal[3]) +
  geom_line(aes(x = Date, y = airpneg80), color = col_pal[2])

# p2 <- 
churchill %>% 
  ggplot(aes(x = Date, y = bfr150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = bfr0), color = col_pal[3]) +
  geom_line(aes(y = bfrneg80), color = col_pal[2])

# p3 <- 
  churchill %>% 
  ggplot(aes(x = Date, y = bsw150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = bsw0), color = col_pal[3]) +
  geom_line(aes(y = bswneg80), color = col_pal[2])

# p4 <- 
  churchill %>% 
  ggplot(aes(x = Date, y = bwp150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = bwp0), color = col_pal[3]) +
  geom_line(aes(y = bwpneg80), color = col_pal[2])

# p5 <- 
  churchill %>% 
  ggplot(aes(x = Date, y = fen150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = fen0), color = col_pal[3]) +
  geom_line(aes(y = fenneg80), color = col_pal[2])

# p6 <- 
  churchill %>% 
  ggplot(aes(x = Date, y = mlk150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = mlk0), color = col_pal[3]) +
  geom_line(aes(y = mlkneg80), color = col_pal[2])

# p7 <- 
  churchill %>% 
  ggplot(aes(x = Date, y = pfr150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = pfr0), color = col_pal[3]) +
  geom_line(aes(y = pfrneg80), color = col_pal[2])

# p8 <- 
  churchill %>% 
  ggplot(aes(x = Date, y = ppa150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = ppa0), color = col_pal[3]) +
  geom_line(aes(y = ppaneg80), color = col_pal[2])

# p9 <- 
  churchill %>% 
  ggplot(aes(x = Date, y = ppd150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = ppd0), color = col_pal[3]) +
  geom_line(aes(y = ppdneg80), color = col_pal[2])

# p10 <- 
  churchill %>% 
  ggplot(aes(x = Date, y = rlk150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = rlkneg5), color = col_pal[3]) +
  geom_line(aes(y = rlkneg80), color = col_pal[2])

# p11 <- 
  churchill %>% 
  ggplot(aes(x = Date, y = tis150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = tis0), color = col_pal[3]) +
  geom_line(aes(y = tisneg80), color = col_pal[2])

# p12 <- 
  churchill %>% 
  ggplot(aes(x = Date, y = tun150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = tun0), color = col_pal[3]) +
  geom_line(aes(y = tunneg80), color = col_pal[2])

# p13 <- 
  churchill %>% 
  ggplot(aes(x = Date, y = wsu150)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = wsu0), color = col_pal[3]) +
  geom_line(aes(y = wsuneg80), color = col_pal[2])

# p1 / p2 / p3 / p4 / p5 / p6 | p7 / p8 / p9 / p10 / p11 / p12 / p13


