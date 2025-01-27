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
library(zoo)

rm(list = ls())

#______________________________----
# Constants ----
setwd("~/Desktop/Workspace")

#______________________________----
# Read in iButton data ----
df <- read.csv("/Users/sdmamet/Desktop/Workspace/earthwatch/MacPass/data/ibuttons_macpass.csv")

sum_df <- df %>%
  mutate(datetime = as_date(paste(year, month, day, sep = "-"))) %>%
  group_by(site, datetime) %>%
  summarise(across(starts_with("i"), 
                   list(min, max), 
                   .names = "{.col}_{.fn}")) %>%
  ungroup() %>% 
  mutate(p10_diff = ip10_2 - ip10_1,
         p20_diff = ip20_2 - ip20_1,
         p30_diff = ip30_2 - ip30_1,
         f10_diff = if10_2 - if10_1,
         f20_diff = if20_2 - if20_1,
         f30_diff = if30_2 - if30_1,
         f40_diff = if40_2 - if40_1,
         f50_diff = if50_2 - if50_1,
         f60_diff = if60_2 - if60_1,
         f70_diff = if70_2 - if70_1,
         f80_diff = if80_2 - if80_1,
         f90_diff = if90_2 - if90_1,
         f100_diff = if100_2 - if100_1,
         p10_snow = ifelse(p10_diff < 0.75, 10, 0),
         p20_snow = ifelse(p20_diff < 0.75, 20, 0),
         p30_snow = ifelse(p30_diff < 0.75, 30, 0),
         f10_snow = ifelse(f10_diff < 0.75, 10, 0),
         f20_snow = ifelse(f20_diff < 0.75, 20, 0),
         f30_snow = ifelse(f30_diff < 0.75, 30, 0),
         f40_snow = ifelse(f40_diff < 0.75, 40, 0),
         f50_snow = ifelse(f50_diff < 0.75, 50, 0),
         f60_snow = ifelse(f60_diff < 0.75, 60, 0),
         f70_snow = ifelse(f70_diff < 0.75, 70, 0),
         f80_snow = ifelse(f80_diff < 0.75, 80, 0),
         f90_snow = ifelse(f90_diff < 0.75, 90, 0),
         f100_snow = ifelse(f100_diff < 0.75, 100, 0)) %>% 
  group_by(site, datetime) %>% 
  mutate(depth_p = pmax(p10_snow,p20_snow,p30_snow, na.rm = T),
         depth_f = pmax(p10_snow,p20_snow,p30_snow,f10_snow,f20_snow,f30_snow,f40_snow,f50_snow,f60_snow,f70_snow,f80_snow,f90_snow,f100_snow, na.rm = T)) %>% 
  ungroup()#%>% 
  # mutate(month = month(datetime),
  #        depth_p = ifelse((month > 5 & month < 10) & is.na(depth_p), 0, depth_p),
  #        depth_f = ifelse((month > 5 & month < 10) & is.na(depth_f), 0, depth_f))

sum_df %>% ggplot(aes(x = datetime, y = depth_f, color = site)) + geom_point() + geom_line() +
  # geom_smooth(se = FALSE, span = 1) + 
  ylim(0,120)
    
sum_df %>% ggplot(aes(x = datetime, y = depth_p, color = site)) + geom_point() + geom_line() +
  # geom_smooth(se = FALSE, span = 1)    
ylim(0,120)

## Generate the date range ----
min_date <- min(sum_df$datetime)
min_year <- year(min_date)
min_month <- sprintf("%02d", month(min_date))
min_day <- sprintf("%02d", day(min_date))
max_date <- max(sum_df$datetime)
max_year <- year(max_date)
max_month <- sprintf("%02d", month(max_date))
max_day <- sprintf("%02d", day(max_date))

## Generate the file name and path and export ----
save_path <- sprintf("./Earthwatch/MacPass/data/mm_trail_cam_ibuttons_%s%s%s_%s%s%s.csv",
                     min_year, min_month, min_day,
                     max_year, max_month, max_day)
write.csv(sum_df, save_path, row.names=FALSE)

