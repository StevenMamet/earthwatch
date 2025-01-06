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
library(purrr)
library(patchwork)
library(magick)

rm(list=ls())

#_____________________________________-----
# Constants ----
curr_year <- 2024
visual_output <- "~/Desktop/Workspace/earthwatch/MacPass/figures"
data_output <- "./Earthwatch/MacPass/"
location <- "MacPass"
p <- c("MacPass_EnvCan_19980628")
time_zone <- "Canada/Yukon"
filter_date <- "2023-08-19"

#_____________________________________-----
# Source R functions ----
source("~/Desktop/Workspace/earthwatch/R_functions/R_functions.R")

#_____________________________________-----
# Wrangle Mac Pass met station data ----
# macpass <- read.csv(file = "~/Desktop/Workspace/Earthwatch/microclimate.mm2.csv", header = TRUE)
macpass <- read_csv("~/Dropbox/School/Postdoc/Earthwatch/Mackenzie Mountains/GNWT/data/MM_daily.csv")
names(macpass) <- gsub("\\.t","", names(macpass)) # rm any ".t" in column names

#_____________________________________-----
# Wrangle EnvCan data if already downloaded ----
weather_df <- read_most_recent_weather(data_output, p)
weather_df %>% 
  ggplot(aes(x = as_datetime(datetime), y = station_temp)) + geom_line()

# Convert hourly to daily
weather_daily <- weather_df %>% 
  mutate(year = year(datetime),
         month = month(datetime),
         day = day(datetime)) %>%
  group_by(year, month, day) %>% 
  summarise(across(everything(), ~mean(., na.rm = T))) %>% 
  ungroup() %>% 
  mutate(date = ymd(paste(year, month, day, sep = "-"))) %>% 
  select(-c(year, month, day))

#_____________________________________-----
# Join met station data with EnvCan and calculate mean|min|max 150 variable ----
macpass <- left_join(macpass, weather_daily, by = "date") %>% 
  select(-c(day, datetime, dew_point, rel_humid, precip, pressure)) %>% 
  # rename(month = month.y) %>% 
  relocate(month, .after = year) %>% 
  rowwise() %>% 
  mutate(min.150 = mean(c(bp150.min, hf150.min, d2150.min, d6150.min, gf150.min, station_temp), na.rm = T),
         mean.150 = mean(c(bp150.mean, hf150.mean, d2150.mean, d6150.mean, gf150.mean, station_temp), na.rm = T),
         max.150 = mean(c(bp150.max, hf150.max, d2150.max, d6150.max, gf150.max, station_temp), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(min.150 = as.numeric(tsclean(min.150)),
         mean.150 = as.numeric(tsclean(mean.150)),
         max.150 = as.numeric(tsclean(max.150))) %>% 
  filter(!is.na(month))

macpass %>% 
  ggplot(aes(x = as_datetime(date), y = mean.150)) + geom_line()

result_df <- macpass %>%
  filter(date > filter_date) %>% 
  select(5:ncol(macpass)) %>%  # Adjust this to select columns from the 5th to the last
  summarise(across(everything(), list(total = ~n(), na_count = ~sum(is.na(.))))) %>% 
  t() %>% 
  as.data.frame() %>% 

# Assuming the row names are important and contain the variable names and the metric
  tibble::rownames_to_column("name") %>%
  separate(name, into = c("variable", "metric"), sep = "_", extra = "merge") %>%
  pivot_wider(names_from = metric, values_from = V1) %>%
  
  # Compute non-NA count and percentage
  mutate(
    non_na_total = total - na_count,
    percent_complete = (non_na_total / total) * 100
  ) %>%
  select(variable, non_na_total, total, percent_complete)

# Clean up variable names if needed
result_df$variable <- gsub("\\.total", "", result_df$variable)

# Save RDS file
saveRDS(result_df, file = "./earthwatch/MacPass/Reports/Table01.rds", compress = FALSE)
tab1 <- readRDS("./earthwatch/MacPass/Reports/Table01.rds")


# Calculate R2s
# results <- map_dfr(names(macpass)[5:43], ~calc_monthly_r_squared(macpass, "mean.150", .x), .id = "variable")
min_results <-
  map_dfr(
    macpass %>% select(matches(".min")) %>% names(),
    ~ calc_monthly_r_squared(macpass, "min.150", .x)
  )
mean_results <-
  map_dfr(
    macpass %>% select(matches(".mean")) %>% names(),
    ~ calc_monthly_r_squared(macpass, "mean.150", .x)
  )
max_results <-
  map_dfr(
    macpass %>% select(matches(".max")) %>% names(),
    ~ calc_monthly_r_squared(macpass, "max.150", .x)
  )

# Bind results by row to use for plot generation
df150 <- bind_rows(min_results, mean_results, max_results)

# Summary stats for the report
summary(df150$r_squared)

bp150_plots <- generate_plots(df150, "bp", "150")
hf150_plots <- generate_plots(df150, "hf", "150")
d6150_plots <- generate_plots(df150, "d6", "150")
d2150_plots <- generate_plots(df150, "d2", "150")
gf150_plots <- generate_plots(df150, "gf", "150")

bp150_plots$min / bp150_plots$mean / bp150_plots$max

#_____________________________________-----
# Step 1: Fill the missing air temperatures ----

# Mean
macpass <- macpass %>%
  group_by(month) %>%
  nest() %>%
  mutate(filled = map(data, ~fill_missing_with_lm(.x, 
                                                  vars = c("bp150.mean", "hf150.mean", "d2150.mean", "d6150.mean", "gf150.mean", "sf150.mean"),
                                                  fill_var = "mean.150"))) %>%
  select(month, filled) %>%
  unnest(cols = c(filled)) %>% 
  arrange(date) %>% 
  mutate(month = as.integer(month(date))) %>% 
  ungroup()

# Min
macpass <- macpass %>%
  group_by(month) %>%
  nest() %>%
  mutate(filled = map(data, ~fill_missing_with_lm(.x, 
                                                  vars = c("bp150.min", "hf150.min", "d2150.min", "d6150.min", "gf150.min", "sf150.min"),
                                                  fill_var = "min.150"))) %>%
  select(month, filled) %>%
  unnest(cols = c(filled)) %>% 
  arrange(date) %>% 
  mutate(month = as.integer(month(date))) %>% 
  ungroup()

# Max
macpass <- macpass %>%
  group_by(month) %>%
  nest() %>%
  mutate(filled = map(data, ~fill_missing_with_lm(.x, 
                                                  vars = c("bp150.max", "hf150.max", "d2150.max", "d6150.max", "gf150.max", "sf150.max"),
                                                  fill_var = "max.150"))) %>%
  select(month, filled) %>%
  unnest(cols = c(filled)) %>% 
  arrange(date) %>% 
  mutate(month = as.integer(month(date))) %>% 
  ungroup()

col_pal <- wes_palette("Darjeeling1")

macpass %>% 
  ggplot(aes(x = as_datetime(date), y = hf150.mean)) + geom_line() +
  geom_line(aes(y = bp150.mean), color = col_pal[2]) +
  geom_line(aes(y = d2150.mean), color = "red") +
  geom_line(aes(y = gf150.mean), color = col_pal[3]) +
  geom_line(aes(y = d6150.mean), color = "green")

macpass %>% 
  ggplot(aes(x = as_datetime(date), y = hf150.min)) + geom_line() +
  geom_line(aes(y = bp150.min), color = col_pal[2]) +
  geom_line(aes(y = d2150.min), color = "red") +
  geom_line(aes(y = gf150.min), color = col_pal[3]) +
  geom_line(aes(y = d6150.min), color = "green")

macpass %>% 
  ggplot(aes(x = as_datetime(date), y = hf150.max)) + geom_line() +
  geom_line(aes(y = bp150.max), color = col_pal[2]) +
  geom_line(aes(y = d2150.max), color = "red") +
  geom_line(aes(y = gf150.max), color = col_pal[3]) +
  geom_line(aes(y = d6150.max), color = "green")

# Remove funky values
macpass %>% 
  # mutate(bp0.min = as.numeric(ifelse(date > "2015-01-01" & date < "2019-12-31", NA, bp0.min))) %>%
  mutate(bp0.min = ifelse(bp0.min > 50 | bp0.min < -40, NA, bp0.min)) %>%
  ggplot(aes(x = as_datetime(date), y = bp0.min)) + geom_line()

macpass <- macpass %>% 
  mutate(bp0.max = ifelse(date > "2016-01-01" & date < "2017-12-31", NA, bp0.max)) %>%
  mutate(bp0.min = as.numeric(ifelse(date > "2015-01-01" & date < "2019-12-31", NA, bp0.min))) %>%
  mutate(bp0.min = ifelse(bp0.min > 50 | bp0.min < -40, NA, bp0.min))

#_____________________________________-----
# Step 2: Fill the missing ground surface temperatures ----

sites <- c("hf", "gf", "bp", "sf", "d2", "d6")
temp_stats <- c("min", "mean", "max")

r_squared_combined <- tibble()

for (site in sites) {
  for (stat in temp_stats) {
    air_temp_var <- paste(paste(site, "150", sep = ""), stat, sep = ".")
    ground_temp_var <- paste(paste(site, "0", sep = ""), stat, sep = ".")
    
    models <- macpass %>% 
      group_by(month) %>% 
      filter(!is.na(!!sym(ground_temp_var))) %>% 
      do(model = lm(!!sym(ground_temp_var) ~ !!sym(air_temp_var), data = .),
         r_squared = summary(lm(!!sym(ground_temp_var) ~ !!sym(air_temp_var), data = .))$r.squared) %>%
      ungroup()
    
    r_squared_df <- models %>% 
      select(month, r_squared) %>% 
      mutate(site = site, stat = stat)
    
    r_squared_combined <- bind_rows(r_squared_combined, r_squared_df)
  }
}

r_squared_combined <- r_squared_combined %>% 
  mutate(r_squared = unlist(r_squared))

hf0_plots <- list()

hf0_plots$min <-
  r_squared_combined %>%
  mutate(r_squared = unlist(r_squared)) %>%
  filter(site == "hf", stat == "min") %>%
  ggplot(aes(x = r_squared)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", trim = F, adjust = 1/4, alpha = 0.8) +
  geom_vline(aes(xintercept = mean(r_squared)), linetype = "dashed", color = "darkgreen") +
  labs(x = bquote(italic("R")^2), y = "Density") +
  xlim(0, 1) +
  theme_bw()

hf0_plots$mean <-
  r_squared_combined %>%
  mutate(r_squared = unlist(r_squared)) %>%
  filter(site == "hf", stat == "mean") %>%
  ggplot(aes(x = r_squared)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", trim = F, adjust = 1/2, alpha = 0.8) +
  geom_vline(aes(xintercept = mean(r_squared)), linetype = "dashed", color = "darkgreen") +
  labs(x = bquote(italic("R")^2), y = "Density") +
  xlim(0, 1) +
  theme_bw()

hf0_plots$max <-
  r_squared_combined %>%
  mutate(r_squared = unlist(r_squared)) %>%
  filter(site == "hf", stat == "max") %>%
  ggplot(aes(x = r_squared)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", trim = F, adjust = 1/2, alpha = 0.8) +
  geom_vline(aes(xintercept = mean(r_squared)), linetype = "dashed", color = "darkgreen") +
  labs(x = bquote(italic("R")^2), y = "Density") +
  xlim(0, 1) +
  theme_bw()

bp0_plots <- list()

bp0_plots$min <-
  r_squared_combined %>%
  mutate(r_squared = unlist(r_squared)) %>%
  filter(site == "bp", stat == "min") %>%
  ggplot(aes(x = r_squared)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", trim = F, adjust = 1/3, alpha = 0.8) +
  geom_vline(aes(xintercept = mean(r_squared)), linetype = "dashed", color = "darkgreen") +
  labs(x = bquote(italic("R")^2), y = "Density") +
  xlim(0, 1) +
  theme_bw()

bp0_plots$mean <-
  r_squared_combined %>%
  mutate(r_squared = unlist(r_squared)) %>%
  filter(site == "bp", stat == "mean") %>%
  ggplot(aes(x = r_squared)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", trim = F, adjust = 1/3, alpha = 0.8) +
  geom_vline(aes(xintercept = mean(r_squared)), linetype = "dashed", color = "darkgreen") +
  labs(x = bquote(italic("R")^2), y = "Density") +
  xlim(0, 1) +
  theme_bw()

bp0_plots$max <-
  r_squared_combined %>%
  mutate(r_squared = unlist(r_squared)) %>%
  filter(site == "bp", stat == "max") %>%
  ggplot(aes(x = r_squared)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", trim = F, adjust = 1/2, alpha = 0.8) +
  geom_vline(aes(xintercept = mean(r_squared)), linetype = "dashed", color = "darkgreen") +
  labs(x = bquote(italic("R")^2), y = "Density") +
  xlim(0, 1) +
  theme_bw()

d60_plots <- list()

d60_plots$min <-
  r_squared_combined %>%
  mutate(r_squared = unlist(r_squared)) %>%
  filter(site == "d6", stat == "min") %>%
  ggplot(aes(x = r_squared)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", trim = F, adjust = 1/3, alpha = 0.8) +
  geom_vline(aes(xintercept = mean(r_squared)), linetype = "dashed", color = "darkgreen") +
  labs(x = bquote(italic("R")^2), y = "Density") +
  xlim(0, 1) +
  theme_bw()

d60_plots$mean <-
  r_squared_combined %>%
  mutate(r_squared = unlist(r_squared)) %>%
  filter(site == "d6", stat == "mean") %>%
  ggplot(aes(x = r_squared)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", trim = F, adjust = 1/3, alpha = 0.8) +
  geom_vline(aes(xintercept = mean(r_squared)), linetype = "dashed", color = "darkgreen") +
  labs(x = bquote(italic("R")^2), y = "Density") +
  xlim(0, 1) +
  theme_bw()

d60_plots$max <-
  r_squared_combined %>%
  mutate(r_squared = unlist(r_squared)) %>%
  filter(site == "d6", stat == "max") %>%
  ggplot(aes(x = r_squared)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", trim = F, adjust = 1/2, alpha = 0.8) +
  geom_vline(aes(xintercept = mean(r_squared)), linetype = "dashed", color = "darkgreen") +
  labs(x = bquote(italic("R")^2), y = "Density") +
  xlim(0, 1) +
  theme_bw()

d20_plots <- list()

d20_plots$min <-
  r_squared_combined %>%
  mutate(r_squared = unlist(r_squared)) %>%
  filter(site == "d2", stat == "min") %>%
  ggplot(aes(x = r_squared)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", trim = F, adjust = 1/2, alpha = 0.8) +
  geom_vline(aes(xintercept = mean(r_squared)), linetype = "dashed", color = "darkgreen") +
  labs(x = bquote(italic("R")^2), y = "Density") +
  xlim(0, 1) +
  theme_bw()

d20_plots$mean <-
  r_squared_combined %>%
  mutate(r_squared = unlist(r_squared)) %>%
  filter(site == "d2", stat == "mean") %>%
  ggplot(aes(x = r_squared)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", trim = F, adjust = 1/4, alpha = 0.8) +
  geom_vline(aes(xintercept = mean(r_squared)), linetype = "dashed", color = "darkgreen") +
  labs(x = bquote(italic("R")^2), y = "Density") +
  xlim(0, 1) +
  theme_bw()

d20_plots$max <-
  r_squared_combined %>%
  mutate(r_squared = unlist(r_squared)) %>%
  filter(site == "d2", stat == "max") %>%
  ggplot(aes(x = r_squared)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", trim = F, adjust = 1/3, alpha = 0.8) +
  geom_vline(aes(xintercept = mean(r_squared)), linetype = "dashed", color = "darkgreen") +
  labs(x = bquote(italic("R")^2), y = "Density") +
  xlim(0, 1) +
  theme_bw()

gf0_plots <- list()

gf0_plots$min <-
  r_squared_combined %>%
  mutate(r_squared = unlist(r_squared)) %>%
  filter(site == "gf", stat == "min") %>%
  ggplot(aes(x = r_squared)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", trim = F, adjust = 1/2, alpha = 0.8) +
  geom_vline(aes(xintercept = mean(r_squared)), linetype = "dashed", color = "darkgreen") +
  labs(x = bquote(italic("R")^2), y = "Density") +
  xlim(0, 1) +
  theme_bw()

gf0_plots$mean <-
  r_squared_combined %>%
  mutate(r_squared = unlist(r_squared)) %>%
  filter(site == "gf", stat == "mean") %>%
  ggplot(aes(x = r_squared)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", trim = F, adjust = 1/3, alpha = 0.8) +
  geom_vline(aes(xintercept = mean(r_squared)), linetype = "dashed", color = "darkgreen") +
  labs(x = bquote(italic("R")^2), y = "Density") +
  xlim(0, 1) +
  theme_bw()

gf0_plots$max <-
  r_squared_combined %>%
  mutate(r_squared = unlist(r_squared)) %>%
  filter(site == "gf", stat == "max") %>%
  ggplot(aes(x = r_squared)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", trim = F, adjust = 1/3, alpha = 0.8) +
  geom_vline(aes(xintercept = mean(r_squared)), linetype = "dashed", color = "darkgreen") +
  labs(x = bquote(italic("R")^2), y = "Density") +
  xlim(0, 1) +
  theme_bw()
  
(hf150_plots$min / hf150_plots$mean / hf150_plots$max) | (hf0_plots$min / hf0_plots$mean / hf0_plots$max)

setwd(visual_output)

#_____________________________________-----
# BP ----
bp150_plots$min <- bp150_plots$min + annotate("text", x = 0, y = Inf, label = "Air[\"min\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)
bp150_plots$mean <- bp150_plots$mean + annotate("text", x = 0, y = Inf, label = "Air[\"mean\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)
bp150_plots$max <- bp150_plots$max + annotate("text", x = 0, y = Inf, label = "Air[\"max\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)
bp0_plots$min <- bp0_plots$min + annotate("text", x = 0, y = Inf, label = "Surface[\"min\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)
bp0_plots$mean <- bp0_plots$mean + annotate("text", x = 0, y = Inf, label = "Surface[\"mean\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)
bp0_plots$max <- bp0_plots$max + annotate("text", x = 0, y = Inf, label = "Surface[\"max\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)

# Combine the plots
bp_six_panel_plot <- ((bp150_plots$min) / (bp150_plots$mean) / (bp150_plots$max)) & 
  xlab(NULL) & ylab(NULL) & theme(plot.margin = margin(5.5, 5.5, 0, 5.5)) | 
  ((bp0_plots$min) / (bp0_plots$mean) / (bp0_plots$max)) & 
  xlab(NULL) & ylab(NULL) & theme(plot.margin = margin(5.5, 5.5, 0, 5.5))

# Use the tag label as an x-axis label
bp_six_panel_plot <- wrap_elements(panel = bp_six_panel_plot) +
  labs(tag = bquote(italic("R")^2)) +
  theme(
    plot.tag = element_text(size = rel(1)),
    plot.tag.position = "bottom")

ggsave("GNWT_Figure10.jpg", height = 8, width = 7, dpi = 300)
bp_final_plot <- image_read("GNWT_Figure10.jpg")
bp_final_plot <- magick::image_annotate(bp_final_plot, text = "Density", degrees = 270, location = "+1+1250", color = "black", size = 42, font = "Arial-Bold")
image_write(bp_final_plot, path = "GNWT_Figure10.jpg", format = "jpg", quality = 100)

#_____________________________________-----
# HF ----
hf150_plots$min <- hf150_plots$min + annotate("text", x = 0, y = Inf, label = "Air[\"min\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)
hf150_plots$mean <- hf150_plots$mean + annotate("text", x = 0, y = Inf, label = "Air[\"mean\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)
hf150_plots$max <- hf150_plots$max + annotate("text", x = 0, y = Inf, label = "Air[\"max\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)
hf0_plots$min <- hf0_plots$min + annotate("text", x = 0, y = Inf, label = "Surface[\"min\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)
hf0_plots$mean <- hf0_plots$mean + annotate("text", x = 0, y = Inf, label = "Surface[\"mean\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)
hf0_plots$max <- hf0_plots$max + annotate("text", x = 0, y = Inf, label = "Surface[\"max\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)

# Combine the plots
hf_six_panel_plot <- ((hf150_plots$min) / (hf150_plots$mean) / (hf150_plots$max)) & 
  xlab(NULL) & ylab(NULL) & theme(plot.margin = margin(5.5, 5.5, 0, 5.5)) | 
  ((hf0_plots$min) / (hf0_plots$mean) / (hf0_plots$max)) & 
  xlab(NULL) & ylab(NULL) & theme(plot.margin = margin(5.5, 5.5, 0, 5.5))

# Use the tag label as an x-axis label
hf_six_panel_plot <- wrap_elements(panel = hf_six_panel_plot) +
  labs(tag = bquote(italic("R")^2)) +
  theme(
    plot.tag = element_text(size = rel(1)),
    plot.tag.position = "bottom")

ggsave("GNWT_Figure11.jpg", height = 8, width = 7, dpi = 300)
hf_final_plot <- image_read("GNWT_Figure11.jpg")
hf_final_plot <- magick::image_annotate(hf_final_plot, text = "Density", degrees = 270, location = "+1+1250", color = "black", size = 42, font = "Arial-Bold")
image_write(hf_final_plot, path = "GNWT_Figure11.jpg", format = "jpg", quality = 100)

#_____________________________________-----
# D6 ----
d6150_plots$min <- d6150_plots$min + annotate("text", x = 0, y = Inf, label = "Air[\"min\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)
d6150_plots$mean <- d6150_plots$mean + annotate("text", x = 0, y = Inf, label = "Air[\"mean\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)
d6150_plots$max <- d6150_plots$max + annotate("text", x = 0, y = Inf, label = "Air[\"max\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)
d60_plots$min <- d60_plots$min + annotate("text", x = 0, y = Inf, label = "Surface[\"min\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)
d60_plots$mean <- d60_plots$mean + annotate("text", x = 0, y = Inf, label = "Surface[\"mean\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)
d60_plots$max <- d60_plots$max + annotate("text", x = 0, y = Inf, label = "Surface[\"max\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)

# Combine the plots
d6_six_panel_plot <- ((d6150_plots$min) / (d6150_plots$mean) / (d6150_plots$max)) & 
  xlab(NULL) & ylab(NULL) & theme(plot.margin = margin(5.5, 5.5, 0, 5.5)) | 
  ((d60_plots$min) / (d60_plots$mean) / (d60_plots$max)) & 
  xlab(NULL) & ylab(NULL) & theme(plot.margin = margin(5.5, 5.5, 0, 5.5))

# Use the tag label as an x-axis label
d6_six_panel_plot <- wrap_elements(panel = d6_six_panel_plot) +
  labs(tag = bquote(italic("R")^2)) +
  theme(
    plot.tag = element_text(size = rel(1)),
    plot.tag.position = "bottom")

ggsave("GNWT_Figure12.jpg", height = 8, width = 7, dpi = 300)
d6_final_plot <- image_read("GNWT_Figure12.jpg")
d6_final_plot <- magick::image_annotate(d6_final_plot, text = "Density", degrees = 270, location = "+1+1250", color = "black", size = 42, font = "Arial-Bold")
image_write(d6_final_plot, path = "GNWT_Figure12.jpg", format = "jpg", quality = 100)

#_____________________________________-----
# D2 ----
d2150_plots$min <- d2150_plots$min + annotate("text", x = 0, y = Inf, label = "Air[\"min\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)
d2150_plots$mean <- d2150_plots$mean + annotate("text", x = 0, y = Inf, label = "Air[\"mean\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)
d2150_plots$max <- d2150_plots$max + annotate("text", x = 0, y = Inf, label = "Air[\"max\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)
d20_plots$min <- d20_plots$min + annotate("text", x = 0, y = Inf, label = "Surface[\"min\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)
d20_plots$mean <- d20_plots$mean + annotate("text", x = 0, y = Inf, label = "Surface[\"mean\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)
d20_plots$max <- d20_plots$max + annotate("text", x = 0, y = Inf, label = "Surface[\"max\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)

# Combine the plots
d2_six_panel_plot <- ((d2150_plots$min) / (d2150_plots$mean) / (d2150_plots$max)) & 
  xlab(NULL) & ylab(NULL) & theme(plot.margin = margin(5.5, 5.5, 0, 5.5)) | 
  ((d20_plots$min) / (d20_plots$mean) / (d20_plots$max)) & 
  xlab(NULL) & ylab(NULL) & theme(plot.margin = margin(5.5, 5.5, 0, 5.5))

# Use the tag label as an x-axis label
d2_six_panel_plot <- wrap_elements(panel = d2_six_panel_plot) +
  labs(tag = bquote(italic("R")^2)) +
  theme(
    plot.tag = element_text(size = rel(1)),
    plot.tag.position = "bottom")

ggsave("GNWT_Figure13.jpg", height = 8, width = 7, dpi = 300)
d2_final_plot <- image_read("GNWT_Figure13.jpg")
d2_final_plot <- magick::image_annotate(d2_final_plot, text = "Density", degrees = 270, location = "+1+1250", color = "black", size = 42, font = "Arial-Bold")
image_write(d2_final_plot, path = "GNWT_Figure13.jpg", format = "jpg", quality = 100)

#_____________________________________-----
# GF ----
gf150_plots$min <- gf150_plots$min + annotate("text", x = 0, y = Inf, label = "Air[\"min\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)
gf150_plots$mean <- gf150_plots$mean + annotate("text", x = 0, y = Inf, label = "Air[\"mean\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)
gf150_plots$max <- gf150_plots$max + annotate("text", x = 0, y = Inf, label = "Air[\"max\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)
gf0_plots$min <- gf0_plots$min + annotate("text", x = 0, y = Inf, label = "Surface[\"min\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)
gf0_plots$mean <- gf0_plots$mean + annotate("text", x = 0, y = Inf, label = "Surface[\"mean\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)
gf0_plots$max <- gf0_plots$max + annotate("text", x = 0, y = Inf, label = "Surface[\"max\"]", hjust = 0, vjust = 2, size = 3.5, parse = TRUE)

# Combine the plots
gf_six_panel_plot <- ((gf150_plots$min) / (gf150_plots$mean) / (gf150_plots$max)) & 
  xlab(NULL) & ylab(NULL) & theme(plot.margin = margin(5.5, 5.5, 0, 5.5)) | 
  ((gf0_plots$min) / (gf0_plots$mean) / (gf0_plots$max)) & 
  xlab(NULL) & ylab(NULL) & theme(plot.margin = margin(5.5, 5.5, 0, 5.5))

# Use the tag label as an x-axis label
gf_six_panel_plot <- wrap_elements(panel = gf_six_panel_plot) +
  labs(tag = bquote(italic("R")^2)) +
  theme(
    plot.tag = element_text(size = rel(1)),
    plot.tag.position = "bottom")

ggsave("GNWT_Figure14.jpg", height = 8, width = 7, dpi = 300)
gf_final_plot <- image_read("GNWT_Figure14.jpg")
gf_final_plot <- magick::image_annotate(gf_final_plot, text = "Density", degrees = 270, location = "+1+1250", color = "black", size = 42, font = "Arial-Bold")
image_write(gf_final_plot, path = "GNWT_Figure14.jpg", format = "jpg", quality = 100)

## Hare Foot ----

### Mean ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(hf0.mean)) %>% 
  do(model = lm(hf0.mean ~ hf150.mean, data = .),
     r_squared = summary(lm(hf0.mean ~ hf150.mean, data = .))$r.squared) %>%
  ungroup()
mp <- left_join(as_tibble(macpass), models, by = "month")
r_squared_df <- models %>% select(month, r_squared) %>% data.frame()

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

### Max ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(hf0.max)) %>% 
  do(model = lm(hf0.max ~ hf150.max, data = .),
     r_squared = summary(lm(hf0.max ~ hf150.max, data = .))$r.squared) %>%
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

### Min ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(hf0.min)) %>% 
  do(model = lm(hf0.min ~ hf150.min, data = .),
     r_squared = summary(lm(hf0.min ~ hf150.min, data = .))$r.squared) %>%
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

## Beaver Pond ----

### Mean ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(bp0.mean)) %>% 
  do(model = lm(bp0.mean ~ bp150.mean, data = .),
     r_squared = summary(lm(bp0.mean ~ bp150.mean, data = .))$r.squared) %>%
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

### Max ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(bp0.max)) %>% 
  do(model = lm(bp0.max ~ bp150.max, data = .),
     r_squared = summary(lm(bp0.max ~ bp150.max, data = .))$r.squared) %>%
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

### Min ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(bp0.min)) %>% 
  do(model = lm(bp0.min ~ bp150.min, data = .),
     r_squared = summary(lm(bp0.min ~ bp150.min, data = .))$r.squared) %>%
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

## Dale 6 ----

### Mean ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(d60.mean)) %>% 
  do(model = lm(d60.mean ~ d6150.mean, data = .),
     r_squared = summary(lm(d60.mean ~ d6150.mean, data = .))$r.squared) %>%
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

### Max ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(d60.max)) %>% 
  do(model = lm(d60.max ~ d6150.max, data = .),
     r_squared = summary(lm(d60.max ~ d6150.max, data = .))$r.squared) %>%
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

### Min ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(d60.min)) %>% 
  do(model = lm(d60.min ~ d6150.min, data = .),
     r_squared = summary(lm(d60.min ~ d6150.min, data = .))$r.squared) %>%
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

## Dale 2 ----

### Mean ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(d20.mean)) %>% 
  do(model = lm(d20.mean ~ d2150.mean, data = .),
     r_squared = summary(lm(d20.mean ~ d2150.mean, data = .))$r.squared) %>%
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

### Max ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(d20.max)) %>% 
  do(model = lm(d20.max ~ d2150.max, data = .),
     r_squared = summary(lm(d20.max ~ d2150.max, data = .))$r.squared) %>%
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

### Min ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(d20.min)) %>% 
  do(model = lm(d20.min ~ d2150.min, data = .),
     r_squared = summary(lm(d20.min ~ d2150.min, data = .))$r.squared) %>%
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

## Goose Flats ----

### Mean ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(gf0.mean)) %>% 
  do(model = lm(gf0.mean ~ gf150.mean, data = .),
     r_squared = summary(lm(gf0.mean ~ gf150.mean, data = .))$r.squared) %>%
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

### Max ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(gf0.max)) %>% 
  do(model = lm(gf0.max ~ gf150.max, data = .),
     r_squared = summary(lm(gf0.max ~ gf150.max, data = .))$r.squared) %>%
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

### Min ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(gf0.min)) %>% 
  do(model = lm(gf0.min ~ gf150.min, data = .),
     r_squared = summary(lm(gf0.min ~ gf150.min, data = .))$r.squared) %>%
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

## Snow Fence ----

### Mean ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(sf0.mean)) %>% 
  do(model = lm(sf0.mean ~ sf150.mean, data = .),
     r_squared = summary(lm(sf0.mean ~ sf150.mean, data = .))$r.squared) %>%
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

### Max ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(sf0.max)) %>% 
  do(model = lm(sf0.max ~ sf150.max, data = .),
     r_squared = summary(lm(sf0.max ~ sf150.max, data = .))$r.squared) %>%
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

### Min ----
# Construct linear model based on non-NA pairs
models <- macpass %>% 
  group_by(month) %>% 
  filter(!is.na(sf0.min)) %>% 
  do(model = lm(sf0.min ~ sf150.min, data = .),
     r_squared = summary(lm(sf0.min ~ sf150.min, data = .))$r.squared) %>%
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

macpass %>% 
  ggplot(aes(x = date, y = d60.mean)) + geom_line()

write_csv(macpass, sprintf("~/Dropbox/School/Postdoc/Earthwatch/Mackenzie Mountains/GNWT/MM_daily_%s_filled.csv", curr_year))

