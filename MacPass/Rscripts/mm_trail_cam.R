# devtools::install_github("kaitlynstrickfaden/edger", build_vignettes = TRUE, force = TRUE)

library(edger)
library(imager)
library(tidyverse)
library(lubridate)
library(zoo) # Interpolating snow data

rm(list = ls())

setwd("~/Desktop/Workspace")

im1 <- "/Users/sdmamet/Library/CloudStorage/Dropbox/School/Postdoc/Earthwatch/Mackenzie Mountains/TrailCams/GF/20200921.JPG"
# im1 <- "../images/image03.jpg"
par(mar = c(0,0,0,0))
plot(imager::load.image(im1), axes = F)

edger::edger_single(im1)

# Step 1: Read and prep the cam data ----
mm_snow <- read.csv("./Earthwatch/MacPass/data/mm_trail_cam.csv")
mm_snow <- mm_snow %>% 
  mutate(date = ymd(date))
mm_snow %>% 
  ggplot(aes(x = date, y = gf.top)) + geom_line(col = "dodgerblue2") + 
  geom_line(aes(y = gf.side), col = "blue")
mm_snow %>% 
  ggplot(aes(x = date, y = hf.top)) + geom_line(col = "dodgerblue2") + 
  geom_line(aes(y = hf.side), col = "blue")

# Step 2: Interpolate missing values for HF ----
## HF top ----
hf.top.1 <- data.frame(x = c(mm_snow$id), y = c(mm_snow$hf.top))
hf.top.approx <- as.data.frame(na.approx(hf.top.1))
mm_snow2 <- mm_snow
mm_snow$hf.top[is.na(mm_snow$hf.top)] <- hf.top.approx$y[match(hf.top.approx$x[is.na(mm_snow$hf.top)],hf.top.approx$x)]
plot(mm_snow$hf.top, type = "l", ylim = c(0,200))
lines(mm_snow$hf.top, col = "blue")
## HF side ----
hf.side.1 <- data.frame(x = c(mm_snow$id), y = c(mm_snow$hf.side))
hf.side.approx <- as.data.frame(na.approx(hf.side.1))
mm_snow$hf.side[is.na(mm_snow$hf.side)] <- hf.side.approx$y[match(hf.side.approx$x[is.na(mm_snow$hf.side)],hf.side.approx$x)]
plot(mm_snow$hf.top, type = "l", ylim = c(0,200))
lines(mm_snow2$hf.top, col = "blue")
lines(mm_snow2$hf.side, col = "green")
lines(mm_snow$hf.side, col = "red")

# Step 3: Subset the data to fill missing GF data using HF ----
mm_snow1 <- mm_snow
mm_snow <- mm_snow %>% 
  filter(date < "2018-05-24")

# Step 4: Interpolate missing GF values up to 2018 ----
# There is snow in every month except July
mm_snow <- mm_snow %>% 
  mutate(gf.top = ifelse(hf.top == 0, 0, gf.top),
         gf.side = ifelse(hf.side == 0, 0, gf.side))

## GF top ----
# Construct linear model based on non-NA pairs
models <- mm_snow %>% 
  group_by(month) %>% 
  filter(!is.na(gf.top)) %>% 
  do(model = lm(gf.top ~ hf.top, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(mm_snow), models, by = "month")
# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(gf.top = ifelse(is.na(gf.top), pred, gf.top))
plot(mp$date, mp$gf.top, type = "l")
lines(mm_snow$date, mm_snow$gf.top, col = "red")
mm_snow$gf.top <- mp$gf.top

## GF side ----
# Construct linear model based on non-NA pairs
models <- mm_snow %>% 
  group_by(month) %>% 
  filter(!is.na(gf.side)) %>% 
  do(model = lm(gf.side ~ hf.side, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(mm_snow), models, by = "month")
# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(gf.side = ifelse(is.na(gf.side), pred, gf.side))
plot(mp$date, mp$gf.side, type = "l")
lines(mm_snow$date, mm_snow$gf.side, col = "red")
mm_snow$gf.side <- mp$gf.side

# Step 5: Add the filled GF data to the original df ----
mm_snow1[mm_snow1$date < "2018-05-24",] <- mm_snow[mm_snow$date < "2018-05-24",]
mm_snow <- mm_snow1

# Step 6: Interpolate missing values for GF ----
## GF top ----
gf.top.1 <- data.frame(x = c(mm_snow$id), y = c(mm_snow$gf.top))
gf.top.approx <- as.data.frame(na.approx(gf.top.1))
mm_snow2 <- mm_snow
mm_snow$gf.top[is.na(mm_snow$gf.top)] <- gf.top.approx$y[match(gf.top.approx$x[is.na(mm_snow$gf.top)],gf.top.approx$x)]
plot(mm_snow$gf.top, type = "l", ylim = c(0,200))
lines(mm_snow$gf.top, col = "blue")
# GF side
gf.side.1 <- data.frame(x = c(mm_snow$id), y = c(mm_snow$gf.side))
gf.side.approx <- as.data.frame(na.approx(gf.side.1))
mm_snow$gf.side[is.na(mm_snow$gf.side)] <- gf.side.approx$y[match(gf.side.approx$x[is.na(mm_snow$gf.side)],gf.side.approx$x)]
plot(mm_snow$gf.top, type = "l", ylim = c(0,200))
lines(mm_snow2$gf.top, col = "blue")
lines(mm_snow2$gf.side, col = "green")
lines(mm_snow$gf.side, col = "red")

# Step 7: Impute missing values for HF ----
## HF top ----
# Construct linear model based on non-NA pairs
models <- mm_snow %>% 
  group_by(month) %>% 
  filter(!is.na(hf.top)) %>% 
  do(model = lm(hf.top ~ gf.top, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(mm_snow), models, by = "month")
# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(hf.top = ifelse(is.na(hf.top), pred, hf.top))
plot(mp$date, mp$hf.top, type = "l")
lines(mm_snow$date, mm_snow$hf.top, col = "red")
mm_snow$hf.top <- mp$hf.top

## HF side ----
# Construct linear model based on non-NA pairs
models <- mm_snow %>% 
  group_by(month) %>% 
  filter(!is.na(hf.side)) %>% 
  do(model = lm(hf.side ~ hf.top, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(mm_snow), models, by = "month")
# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(hf.side = ifelse(is.na(hf.side), pred, hf.side))
plot(mp$date, mp$hf.side, type = "l")
lines(mm_snow$date, mm_snow$hf.side, col = "red")
mm_snow$hf.side <- mp$hf.side

mm_snow <- mm_snow %>%
  mutate(
    gf.top = ifelse(gf.top < 0, 0, gf.top),
    gf.side = ifelse(gf.side < 0, 0, gf.side),
    hf.top = ifelse(hf.top < 0, 0, hf.top),
    hf.side = ifelse(hf.side < 0, 0, hf.side)
  )

mm_snow %>% 
  ggplot(aes(x = date, y = gf.top)) + geom_line(col = "dodgerblue2") + 
  geom_line(aes(y = gf.side), col = "blue")
mm_snow %>% 
  ggplot(aes(x = date, y = hf.top)) + geom_line(col = "dodgerblue2") + 
  geom_line(aes(y = hf.side), col = "blue")

# Step 8: Output the filled dataframe ----
## Generate the date range ----
min_date <- min(mm_snow$date)
min_year <- year(min_date)
min_month <- sprintf("%02d", month(min_date))
min_day <- sprintf("%02d", day(min_date))
max_date <- max(mm_snow$date)
max_year <- year(max_date)
max_month <- sprintf("%02d", month(max_date))
max_day <- sprintf("%02d", day(max_date))

## Generate the file name and path and export ----
save_path <- sprintf("./Earthwatch/MacPass/data/mm_trail_cam_%s%s%s_%s%s%s_filled.csv",
                     min_year, min_month, min_day,
                     max_year, max_month, max_day)
write.csv(mm_snow, save_path, row.names=FALSE)

##************************************
##************************************
# Scratch pad ----

## Collating regression stats into a list -- from old clunky code
# df.list <- c(paste("gf.top",c(1:6,9:12), "fit", sep = "."),
#              paste("gf.side",c(1:6,9:12), "fit", sep = "."),
#              paste("hf.top",c(1:6,9:12), "fit", sep = "."),
#              paste("hf.side",c(1:6,9:12), "fit", sep = "."))
# 
# 
# df_list <- lapply(df.list, get)                     # Convert that vector to a list
# 
# coefficients <- unlist(sapply(df_list, RsquareAdj))
# 
# r2.df <- cbind.data.frame(model = c(paste("gf.top",c(1:6,9:12), "fit", sep = "."),
#                                     paste("gf.side",c(1:6,9:12), "fit", sep = "."),
#                                     paste("hf.top",c(1:6,9:12), "fit", sep = "."),
#                                     paste("hf.side",c(1:6,9:12), "fit", sep = ".")),
#                           r2 = coefficients[seq(1,length(coefficients),2)])
# 
# range(r2.df$r2)
# mean(r2.df$r2)
# 
# gf.top <- density(r2.df[c(1:10),2])
# gf.side <- density(r2.df[c(11:20),2])
# hf.top <- density(r2.df[c(21:30),2])
# hf.side <- density(r2.df[c(31:40),2])

# jpeg("snow.jpg", width = 6, height = 4, units = "in", res = 300)
# par(mfcol = c(2,2), mar = c(1.5,1.5,1,1), oma = c(2,2,0,0))
# plot(gf.top$x, gf.top$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(1:10),2]), lty = 3)
# legend("topleft", "GF - top", bty = "n", cex = 0.85)
# plot(gf.side$x, gf.side$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(11:20),2]), lty = 3)
# legend("topleft", "GF - lee", bty = "n", cex = 0.85)
# plot(hf.top$x, hf.top$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(21:30),2]), lty = 3)
# legend("topleft", "HF - top", bty = "n", cex = 0.85)
# plot(hf.side$x, hf.side$y, type = "l", xlim = c(0,1))
# abline(v = mean(r2.df[c(31:40),2]), lty = 3)
# legend("topleft", "HF - lee", bty = "n", cex = 0.85)
# mtext(substitute(paste(italic(r)^2)), side = 1, outer = T, cex = 0.8, line = 0.7)
# mtext("Density", side = 2, outer = T, line = 0.7, cex = 0.7)
# dev.off()
