
#_________________________________________________-----
# This script will download the latest Env Can weather data for the station of interest
weather_download <- function(df, weather_station, time_zone, time_unit) {
  stations_dl() # re-download/update stations data
  station_id <- suppressMessages(as.numeric(stations_search(weather_station, dist = 50, interval = time_unit)[3]))
  start_date <- min(df$Date)
  end_date <- max(df$Date)
  w_df <-
    weather_dl(station_ids = station_id,
               start = start_date,
               end = end_date)
  weather_df <- w_df %>%
    rename(
      station_temp = temp,
      datetime = time,
      dew_point = temp_dew,
      rel_humid = rel_hum,
      precip = precip_amt
    ) %>%
    select(datetime,
           station_temp,
           datetime,
           dew_point,
           rel_humid,
           precip,
           pressure) %>%
    mutate(datetime = as_datetime(datetime, tz = time_zone)) %>%
    filter(datetime > start_date & datetime <= end_date) %>%
    mutate(
      station_temp = tsclean(station_temp),
      rel_humid = tsclean(rel_humid),
      pressure = tsclean(pressure)
    )
  
  min_date <- min(weather_df$datetime)
  min_year <- year(min_date)
  min_month <- sprintf("%02d", month(min_date))
  min_day <- sprintf("%02d", day(min_date))
  max_date <- max(weather_df$datetime)
  max_year <- year(max_date)
  max_month <- sprintf("%02d", month(max_date))
  max_day <- sprintf("%02d", day(max_date))
  save_path <-
    sprintf(
      "./earthwatch/MacPass/data/MacPass_EnvCan_%s%s%s_%s%s%s.csv",
      min_year,
      min_month,
      min_day,
      max_year,
      max_month,
      max_day
    )
  write.csv(weather_df, save_path)
  
  return(weather_df)
}

#_________________________________________________-----
# Reads in most recent Env Can file of interest
read_most_recent_weather <- function(data_output, p) {
  file_list <- file.info(list.files(paste(data_output, "data", sep = "/"), full.names = T))
  file_list <- file_list[apply(sapply(X = p, FUN = grepl, rownames(file_list)),
                               MARGIN =  1,
                               FUN = all), ]
  file_pattern <- rownames(file_list[which.max(file_list$mtime),])
  df <- read.csv(file_pattern) %>%
    # Make datetime column and generate full timestamps
    mutate(datetime = as_datetime(datetime, tz = time_zone))
  return(df)
  
}

#_________________________________________________-----
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

#_________________________________________________-----
# Join weather data frame with station data
weather_join <- function(weather_df, df) {
  weather_daily <- weather_df %>%
    select(-1) %>%
    mutate(
      datetime = as_datetime(datetime),
      year = year(datetime),
      month = month(datetime),
      day = day(datetime)
    ) %>%
    group_by(year, month, day) %>%
    summarise(across(everything(), ~ mean(., na.rm = T))) %>%
    ungroup() %>%
    mutate(Date = ymd(paste(year, month, day, sep = "-")))
  
  df <- left_join(df, weather_daily, by = "Date") %>%
    select(-c(
      month.x,
      day,
      datetime,
      dew_point,
      year,
      rel_humid,
      precip,
      pressure
    )) %>%
    rename(month = month.y) %>%
    relocate(month, .after = YEAR) %>%
    rowwise() %>%
    mutate(mean.150 = mean(
      c(bp.150, hf.150, d2.150, d6.150, gf.150, station_temp),
      na.rm = T
    )) %>%
    ungroup() %>%
    mutate(mean.150 = as.numeric(tsclean(mean.150)))
  
  return(df)
}