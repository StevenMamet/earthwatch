# devtools::install_github("kaitlynstrickfaden/edger", build_vignettes = TRUE, force = TRUE)
# devtools::install_github("asgr/imager")
# Note that I had numerous issues where I would get an X11 error.
# To remedy this, I had to run the following in terminal:
# brew install xquartz --cask
## Note that in the end, you still have to manually extract the snow data from the images

library(tidyverse)
library(lubridate)
library(zoo) # Interpolating snow data
library(forecast)

rm(list = ls())

setwd("~/Desktop/Workspace")

#_____________________________________________----
# Step 1: Read and prep the cam data ----
mm_snow_filled <- read_csv("./Earthwatch/MacPass/data/mm_trail_cam_20150818_20230818_filled.csv") %>% 
  mutate(date = ymd(date))
mm_snow <- read_csv("./Earthwatch/MacPass/data/mm_trail_cam.csv")
mm_snow <- mm_snow %>% 
  mutate(date = ymd(date)) %>% 
  mutate(hf.top = as.numeric(hf.top),
         hf.side = as.numeric(hf.side),
         gf.top = as.numeric(gf.top),
         gf.side = as.numeric(gf.side))

# Filled data: GF is good up to the end of 2021
mm_snow_filled %>% 
  ggplot(aes(x = date, y = gf.top)) + geom_line(col = "dodgerblue2") + 
  geom_line(aes(y = gf.side), col = "blue")

# Filled data: HF is good up to July 2018
mm_snow_filled %>% 
  ggplot(aes(x = date, y = hf.top)) + geom_line(col = "dodgerblue2") + 
  geom_line(aes(y = hf.side), col = "blue")


# 
mm_snow %>% 
  ggplot(aes(x = date)) + 
  geom_line(aes(y = gf.top), col = "dodgerblue2") + 
  geom_line(aes(y = gf.side), col = "blue")

mm_snow %>% 
  ggplot(aes(x = date, y = hf.top)) + 
  geom_line(col = "dodgerblue2") + 
  geom_line(aes(y = hf.side), col = "blue")

mm_snow <-
  mm_snow %>%
  full_join(mm_snow_filled %>% 
              select(-c("id","year","month","day")) %>% 
              rename(gf_top_mod = gf.top,
                     gf_side_mod = gf.side,
                     hf_top_mod = hf.top,
                     hf_side_mod = hf.side), by = "date") #%>% 
    # mutate(gf.top = ifelse(date < "2020-06-01" & is.na(gf.top), gf_top_mod, gf.top),
    #        gf.side = ifelse(date < "2020-06-01" & is.na(gf.side), gf_side_mod, gf.side)) %>% 
    # mutate(gf.top = ifelse(date < "2020-06-01", tsclean(gf.top), gf.top),
    #        gf.side = ifelse(date < "2020-06-01", tsclean(gf.side), gf.side)) %>% 
    # mutate(hf.top = ifelse(date < "2020-06-01" & is.na(hf.top), hf_top_mod, hf.top),
    #        hf.side = ifelse(date < "2020-06-01" & is.na(hf.side), hf_side_mod, hf.side)) %>% 
    # mutate(hf.top = ifelse(date < "2020-06-01", tsclean(hf.top), hf.top),
    #        hf.side = ifelse(date < "2020-06-01", tsclean(hf.side), hf.side))# %>% 
    # ggplot(aes(x = date)) + 
    # geom_line(aes(y = hf.top), col = "dodgerblue2") + 
    # geom_line(aes(y = hf.side), col = "blue") +
    # ylim(0,205)

df <- mm_snow %>% 
  mutate(
    hf.top = ifelse(date < "2018-07-15", hf_top_mod, hf.top),
    hf.side = ifelse(date < "2018-07-15", hf_side_mod, hf.side),
    gf.top = ifelse(date < "2021-12-31", gf_top_mod, gf.top),
    gf.side = ifelse(date < "2021-12-31", gf_side_mod, gf.side))

df %>% ggplot(aes(x = date)) + 
  geom_line(aes(y = gf.top), color = "dodgerblue") +
  geom_line(aes(y = gf.side), color = "blue")

df %>% ggplot(aes(x = date)) + 
  geom_line(aes(y = hf.top), color = "dodgerblue") +
  geom_line(aes(y = hf.side), color = "blue")


df %>% 
  mutate(gf.top.ibuttons = ifelse((gf.top.ibuttons > 0), gf.top.ibuttons+10, gf.top.ibuttons),
         gf.side.ibuttons = ifelse((gf.side.ibuttons > 0), gf.side.ibuttons+25, gf.side.ibuttons),
         gf_top_ts = tsclean(gf.top.ibuttons),
         gf_side_ts = tsclean(gf.side.ibuttons)) %>% 
  mutate(gf.top = ifelse(date > "2021-08-01" & is.na(gf.top), gf.top.ibuttons, gf.top)) %>%
  mutate(gf.side = ifelse(date > "2021-08-01" & is.na(gf.side), gf_side_ts, gf.side)) %>%
  ggplot(aes(x = date)) + 
  geom_line(aes(y = gf.top), col = "dodgerblue2") +
  geom_line(aes(y = gf.side), col = "blue") #+
  # geom_line(aes(y = gf.top.ibuttons), col = "green1", alpha = 0.7) +
  # geom_line(aes(y = gf.side.ibuttons), col = "forestgreen", alpha = 0.7)

df %>% 
  mutate(
    hf.top.ibuttons = ifelse((hf.top.ibuttons > 0), hf.top.ibuttons+25, hf.top.ibuttons),
    hf.side.ibuttons = ifelse((hf.side.ibuttons > 0), hf.side.ibuttons+25, hf.side.ibuttons),
    hf_top_ts = tsclean(hf.top.ibuttons),
    hf_side_ts = tsclean(hf.side.ibuttons)) %>%
  mutate(
    hf.top = case_when(
      date > "2018-07-15" & date < "2019-07-15" ~ hf_top_ts,
      date > "2019-08-01" & date < "2020-07-01" ~ hf_top_ts,
      date > "2022-07-15" & date < "2023-07-15" ~ hf_top_ts,
      .default = hf.top
      ),
    hf.side = case_when(
      date > "2018-07-15" & date < "2019-07-15" ~ hf_side_ts,
      .default = hf.side
    )) %>% 
  ggplot(aes(x = date)) + 
  geom_line(aes(y = tsclean(hf.top)), col = "dodgerblue2") + 
  geom_line(aes(y = tsclean(hf.side)), col = "blue")# +
  # geom_line(aes(y = hf.top.ibuttons), col = "green1", alpha = 0.7) +
  # geom_line(aes(y = tsclean(hf.side.ibuttons)), col = "forestgreen", alpha = 0.7)

# Make the replacements
df <- df %>% 
  mutate(
    gf.top.ibuttons = ifelse((gf.top.ibuttons > 0), gf.top.ibuttons+10, gf.top.ibuttons),
    gf.side.ibuttons = ifelse((gf.side.ibuttons > 0), gf.side.ibuttons+25, gf.side.ibuttons),
    gf_top_ts = tsclean(gf.top.ibuttons),
    gf_side_ts = tsclean(gf.side.ibuttons)
    ) %>% 
  mutate(
    gf.top = ifelse(date > "2021-08-01" & is.na(gf.top), gf.top.ibuttons, gf.top),
    gf.side = ifelse(date > "2021-08-01" & is.na(gf.side), gf_side_ts, gf.side)
    ) %>%
  mutate(
    hf.top.ibuttons = ifelse((hf.top.ibuttons > 0), hf.top.ibuttons+25, hf.top.ibuttons),
    hf.side.ibuttons = ifelse((hf.side.ibuttons > 0), hf.side.ibuttons+25, hf.side.ibuttons),
    hf_top_ts = tsclean(hf.top.ibuttons),
    hf_side_ts = tsclean(hf.side.ibuttons)) %>%
  mutate(
    hf.top = case_when(
      date > "2018-07-15" & date < "2019-07-15" ~ hf_top_ts,
      date > "2019-08-01" & date < "2020-07-01" ~ hf_top_ts,
      date > "2022-07-15" & date < "2023-07-15" ~ hf_top_ts,
      .default = hf.top
    ),
    hf.side = case_when(
      date > "2018-07-15" & date < "2019-07-15" ~ hf_side_ts,
      .default = hf.side
    )) 

# # Step 2: Interpolate missing values for HF ----
# ## HF top ----
# hf.top.1 <- data.frame(x = c(mm_snow$id), y = c(mm_snow$hf.top))
# hf.top.approx <- as.data.frame(na.approx(hf.top.1))
# mm_snow2 <- mm_snow
# mm_snow$hf.top[is.na(mm_snow$hf.top)] <- hf.top.approx$y[match(hf.top.approx$x[is.na(mm_snow$hf.top)],hf.top.approx$x)]
# plot(mm_snow$hf.top, type = "l", ylim = c(0,200))
# lines(mm_snow$hf.top, col = "blue")
# ## HF side ----
# hf.side.1 <- data.frame(x = c(mm_snow$id), y = c(mm_snow$hf.side))
# hf.side.approx <- as.data.frame(na.approx(hf.side.1))
# mm_snow$hf.side[is.na(mm_snow$hf.side)] <- hf.side.approx$y[match(hf.side.approx$x[is.na(mm_snow$hf.side)],hf.side.approx$x)]
# plot(mm_snow$hf.top, type = "l", ylim = c(0,200))
# lines(mm_snow2$hf.top, col = "blue")
# lines(mm_snow2$hf.side, col = "green")
# lines(mm_snow$hf.side, col = "red")
# 
# # Step 3: Subset the data to fill missing GF data using HF ----
# mm_snow1 <- mm_snow
# mm_snow <- mm_snow %>% 
#   filter(date < "2018-05-24")
# 
# # Step 4: Interpolate missing GF values up to 2018 ----
# # There is snow in every month except July
# mm_snow <- mm_snow %>% 
#   mutate(gf.top = ifelse(hf.top == 0, 0, gf.top),
#          gf.side = ifelse(hf.side == 0, 0, gf.side))
# 
# ## GF top ----
# # Construct linear model based on non-NA pairs
# models <- mm_snow %>% 
#   group_by(month) %>% 
#   filter(!is.na(gf.top)) %>% 
#   do(model = lm(gf.top ~ hf.top, data = .)) %>%
#   ungroup()
# mp <- left_join(as_tibble(mm_snow), models, by = "month")
# # generate the extra column
# mp <- mp %>%
#   group_by(month) %>%
#   do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
#   mutate(model = NULL) %>% 
#   ungroup() %>% 
#   arrange(date) %>% 
#   mutate(gf.top = ifelse(is.na(gf.top), pred, gf.top))
# plot(mp$date, mp$gf.top, type = "l")
# lines(mm_snow$date, mm_snow$gf.top, col = "red")
# mm_snow$gf.top <- mp$gf.top
# 
# ## GF side ----
# # Construct linear model based on non-NA pairs
# models <- mm_snow %>% 
#   group_by(month) %>% 
#   filter(!is.na(gf.side)) %>% 
#   do(model = lm(gf.side ~ hf.side, data = .)) %>%
#   ungroup()
# mp <- left_join(as_tibble(mm_snow), models, by = "month")
# # generate the extra column
# mp <- mp %>%
#   group_by(month) %>%
#   do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
#   mutate(model = NULL) %>% 
#   ungroup() %>% 
#   arrange(date) %>% 
#   mutate(gf.side = ifelse(is.na(gf.side), pred, gf.side))
# plot(mp$date, mp$gf.side, type = "l")
# lines(mm_snow$date, mm_snow$gf.side, col = "red")
# mm_snow$gf.side <- mp$gf.side
# 
# # Step 5: Add the filled GF data to the original df ----
# mm_snow1[mm_snow1$date < "2018-05-24",] <- mm_snow[mm_snow$date < "2018-05-24",]
# mm_snow <- mm_snow1
# 
# # Step 6: Interpolate missing values for GF ----
# ## GF top ----
# gf.top.1 <- data.frame(x = c(mm_snow$id), y = c(mm_snow$gf.top))
# gf.top.approx <- as.data.frame(na.approx(gf.top.1))
# mm_snow2 <- mm_snow
# mm_snow$gf.top[is.na(mm_snow$gf.top)] <- gf.top.approx$y[match(gf.top.approx$x[is.na(mm_snow$gf.top)],gf.top.approx$x)]
# plot(mm_snow$gf.top, type = "l", ylim = c(0,200))
# lines(mm_snow$gf.top, col = "blue")
# # GF side
# gf.side.1 <- data.frame(x = c(mm_snow$id), y = c(mm_snow$gf.side))
# gf.side.approx <- as.data.frame(na.approx(gf.side.1))
# mm_snow$gf.side[is.na(mm_snow$gf.side)] <- gf.side.approx$y[match(gf.side.approx$x[is.na(mm_snow$gf.side)],gf.side.approx$x)]
# plot(mm_snow$gf.top, type = "l", ylim = c(0,200))
# lines(mm_snow2$gf.top, col = "blue")
# lines(mm_snow2$gf.side, col = "green")
# lines(mm_snow$gf.side, col = "red")
# 
# # Step 7: Impute missing values for HF ----
# ## HF top ----
# # Construct linear model based on non-NA pairs
# models <- mm_snow %>% 
#   group_by(month) %>% 
#   filter(!is.na(hf.top)) %>% 
#   do(model = lm(hf.top ~ gf.top, data = .)) %>%
#   ungroup()
# mp <- left_join(as_tibble(mm_snow), models, by = "month")
# # generate the extra column
# mp <- mp %>%
#   group_by(month) %>%
#   do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
#   mutate(model = NULL) %>% 
#   ungroup() %>% 
#   arrange(date) %>% 
#   mutate(hf.top = ifelse(is.na(hf.top), pred, hf.top))
# plot(mp$date, mp$hf.top, type = "l")
# lines(mm_snow$date, mm_snow$hf.top, col = "red")
# mm_snow$hf.top <- mp$hf.top
# 
# ## HF side ----
# # Construct linear model based on non-NA pairs
# models <- mm_snow %>% 
#   group_by(month) %>% 
#   filter(!is.na(hf.side)) %>% 
#   do(model = lm(hf.side ~ hf.top, data = .)) %>%
#   ungroup()
# mp <- left_join(as_tibble(mm_snow), models, by = "month")
# # generate the extra column
# mp <- mp %>%
#   group_by(month) %>%
#   do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
#   mutate(model = NULL) %>% 
#   ungroup() %>% 
#   arrange(date) %>% 
#   mutate(hf.side = ifelse(is.na(hf.side), pred, hf.side))
# plot(mp$date, mp$hf.side, type = "l")
# lines(mm_snow$date, mm_snow$hf.side, col = "red")
# mm_snow$hf.side <- mp$hf.side
# 
# mm_snow <- mm_snow %>%
#   mutate(
#     gf.top = ifelse(gf.top < 0, 0, gf.top),
#     gf.side = ifelse(gf.side < 0, 0, gf.side),
#     hf.top = ifelse(hf.top < 0, 0, hf.top),
#     hf.side = ifelse(hf.side < 0, 0, hf.side)
#   )
# 
# mm_snow %>% 
#   ggplot(aes(x = date, y = gf.top)) + geom_line(col = "dodgerblue2") + 
#   geom_line(aes(y = gf.side), col = "blue")
# mm_snow %>% 
#   ggplot(aes(x = date, y = hf.top)) + geom_line(col = "dodgerblue2") + 
#   geom_line(aes(y = hf.side), col = "blue")

# Step 8: Output the filled dataframe ----
## Generate the date range ----
min_date <- min(df$date)
min_year <- year(min_date)
min_month <- sprintf("%02d", month(min_date))
min_day <- sprintf("%02d", day(min_date))
max_date <- max(df$date)
max_year <- year(max_date)
max_month <- sprintf("%02d", month(max_date))
max_day <- sprintf("%02d", day(max_date))

## Generate the file name and path and export ----
save_path <- sprintf(
  "./Earthwatch/MacPass/data/mm_trail_cam_%s%s%s_%s%s%s_filled.csv",
  min_year, min_month, min_day,
  max_year, max_month, max_day
  )
write.csv(df, save_path, row.names=FALSE)

