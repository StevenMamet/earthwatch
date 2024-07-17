#_________________________________________________-----
# Helper functions

# Function to standardize "YEAR" column
standardize_year_column <- function(df) {
  if ("YEAR" %in% colnames(df)) {
    df <- df %>% rename(YEAR = YEAR)
  } else if ("Year" %in% colnames(df)) {
    df <- df %>% rename(YEAR = Year)
  } else if ("year" %in% colnames(df)) {
    df <- df %>% rename(YEAR = year)
  }
  return(df)
}

#_________________________________________________-----
# This script will download the latest Env Can weather data for the station of interest
library(dplyr)
library(lubridate)
library(tsibble)
library(progress)
library(readr) # Ensure you have the readr package loaded for write_csv function

weather_download <- function(df, weather_station, time_zone, time_unit, location) {
  # Initialize the progress bar with 4 steps
  pb <- progress_bar$new(
    format = "  Downloading [:bar] :percent in :elapsed",
    total = 4,
    clear = FALSE,
    width = 60
  )
  
  # Step 1: Download/update stations data
  stations_dl()
  pb$tick() # Update the progress bar
  
  # Step 2: Search for station ID
  station_id <- stations_search(weather_station, dist = 50, interval = time_unit) %>% 
    filter(prov == "MB", end == 2023) %>%
    slice_min(start) %>%
    pull(station_id)
  pb$tick() # Update the progress bar
  
  # Step 3: Download weather data
  start_date <- min(df$Date)
  end_date <- max(df$Date)
  w_df <- weather_dl(station_ids = station_id, start = start_date, end = end_date)
  pb$tick() # Update the progress bar
  
  # Step 4: Data manipulation and saving CSV
  weather_df <- w_df %>%
    rename(
      station_temp = temp,
      datetime = time,
      dew_point = temp_dew,
      rel_humid = rel_hum,
      precip = precip_amt
    ) %>%
    select(datetime, station_temp, dew_point, rel_humid, precip, pressure) %>%
    mutate(
      datetime = as_datetime(datetime, tz = time_zone),
      station_temp = as.numeric(station_temp),
      dew_point = as.numeric(dew_point),
      rel_humid = as.numeric(rel_humid),
      precip = as.numeric(precip),
      pressure = as.numeric(pressure)
    ) %>%
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
  save_path <- sprintf(
    "./earthwatch/%s/data/%s_EnvCan_%s%s%s_%s%s%s.csv",
    location,
    location,
    min_year,
    min_month,
    min_day,
    max_year,
    max_month,
    max_day
  )
  
  write_csv(weather_df, save_path)
  pb$tick() # Final update of the progress bar
  
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
  df <- read_csv(file_pattern) %>%
    # Make datetime column and generate full timestamps
    mutate(datetime = as_datetime(datetime, tz = time_zone))
  return(df)
  
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
  
  df <- df %>%
    standardize_year_column() %>%
    left_join(weather_daily, by = "Date") %>%
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
    mutate(mean.150 = mean(c_across(matches("150")), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(mean.150 = as.numeric(tsclean(mean.150)))
  
  # Select only the columns containing "150" but not "neg150"
  cols_150 <- df %>% select(matches("150")) %>% select(-matches("neg150"))
  
  # Calculate the mean of the selected columns
  df <- df %>%
    rowwise() %>%
    mutate(mean.150 = mean(c_across(all_of(names(cols_150))), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(mean.150 = as.numeric(tsclean(mean.150)))
  
  return(df)
}

#__________________________________________----
# Function to move files to another folder (trail cam pic wrangling)

# Function to copy all files
copy_files <- function(x) {
  file.copy(
    from = file.path(input_path, x) ,
    to = file.path(output_path, x) ,
    copy.date = TRUE
  ) # Retain the modified dates for each file
}

#__________________________________________----
# Moving trail cam pics based on removing repeat dates

move_pics_date_only <- function(input_path) {
  setwd(input_path)
  
  # Create new directory to copy images to
  output_path <- paste(input_path, "testing", sep = "/")
  
  if (file.exists(output_path)) {
    setwd(output_path)
    
  } else {
    dir.create(output_path)    # create directories
    
  }
  
  # Collect the paths and file list
  collector <- list.files(getwd(),
                          full.names = T, # capture the file names along with the full path
                          recursive = T)  # look in subfolders
  
  temp_df <- file.info(collector)$mtime
  collector <- collector[which(!duplicated(as_date(file.info(collector)$mtime)))]
  file_list <- gsub(input_path, "", collector)
  
  # Copy the files
  lapply(file_list, copy_files)
  
}

#__________________________________________----
# Move trail can pics based on modified time and removing duplicate dates

move_pics <- function(input_path, time_unit) {
  setwd(input_path)
  
  # Create new directory to copy images to
  output_path <- paste(input_path, "testing", sep = "/")
  
  if (file.exists(output_path)) {
    setwd(output_path)
    
  } else {
    dir.create(output_path)    # create directories
    
  }
  
  # Create df containing the modified times for each file and use to create file index
  temp_df <- file.info(collector)$mtime
  collect_list <- collector[which(grepl(time_unit, temp_df))]
  collect_list <- collect_list[which(!duplicated(file.info(collect_list)$mtime))]
  collect_list <- collect_list[which(!duplicated(as_date(file.info(collect_list)$mtime)))]
  file_list <- gsub(input_path, "", collect_list)
  
  # Copy the files
  lapply(file_list, copy_files)
  
}

#_________________________________________________-----
# Function to fill missing values
# fill_missing_with_lm <- function(dat, vars) {
#   for(i in seq_along(vars)) {
#     mod <- as.formula(paste0(vars[i], " ~ mean.150"))
#     mod <- lm(mod, dat)
#     misses <- which(is.na(dat[[ vars[i] ]]))
#     for(j in misses) {
#       newdat <- data.frame(mean.150 = dat$mean.150[j])
#       dat[[ vars[i] ]][j] <- predict(mod, newdat)
#     }
#   }
#   return(dat)
# }

#__________________________________________----
# Fill missing values ----
fill_missing_with_lm <- function(dat, vars, fill_var) {
  for(i in seq_along(vars)) {
    mod <- as.formula(paste0(vars[i], " ~ ", fill_var))
    mod <- lm(mod, dat)
    misses <- which(is.na(dat[[vars[i]]]))
    for(j in misses) {
      newdat <- setNames(data.frame(dat[[fill_var]][j]), fill_var)
      dat[[vars[i]]][j] <- predict(mod, newdat)
    }
  }
  return(dat)
}

#__________________________________________----
# Calculate monthly R2 for gap-filling ----
calc_monthly_r_squared <- function(df, ind_var, dep_var) {
  df %>%
    group_by(month) %>%
    summarize(r_squared = summary(lm(reformulate(ind_var, dep_var), data = pick(all_of(dep_var), all_of(ind_var))))$r.squared,
              .groups = 'drop') %>%
    mutate(variable = dep_var)
}

#__________________________________________----
# Automated density plot generation ----
generate_plots <- function(data, site_prefix, height) {
  plot_list <- list()
  
  for (stat in c("min", "mean", "max")) {
    variable_name <- paste(paste(site_prefix, height, sep = ""), stat, sep = ".")
    plot <- data %>%
      filter(variable == variable_name) %>%
      ggplot(aes(x = r_squared)) +
      geom_density(fill = "#69b3a2", color = "#e9ecef", alpha = 0.8) +
      geom_vline(aes(xintercept = mean(r_squared)), linetype = "dashed", color = "darkgreen") +
      labs(x = bquote(italic("R")^2), y = "Density") +
      xlim(0, 1) +
      theme_bw()
    
    plot_list[[stat]] <- plot
  }
  
  return(plot_list)
}

#__________________________________________----
# Wrangling all the GTREE data for the various figures
gtree_fun <- function(df, n_yr) {
  df_list <- list()
  
  df_list$mm_nalp <- subset(mm, site == "nalp")
  df_list$mm_salp <- subset(mm, site == "salp")
  df_list$mm_sshr <- subset(mm, site == "sshr")
  df_list$mm_scut <- subset(mm, site == "scut")
  
  mm_nalp_fir_seed <-
    droplevels(subset(df_list$mm_nalp, seeded == 1 & species == "fir"))
  mm_salp_fir_seed <-
    droplevels(subset(df_list$mm_salp, seeded == 1 & species == "fir"))
  mm_sshr_fir_seed <-
    droplevels(subset(df_list$mm_sshr, seeded == 1 & species == "fir"))
  mm_scut_fir_seed <-
    droplevels(subset(df_list$mm_scut, seeded == 1 & species == "fir"))
  
  df_list$mm_nalp_spruce_seed <-
    droplevels(subset(df_list$mm_nalp, seeded == 1 &
                        species == "spruce"))
  df_list$mm_salp_spruce_seed <-
    droplevels(subset(df_list$mm_salp, seeded == 1 &
                        species == "spruce"))
  df_list$mm_sshr_spruce_seed <-
    droplevels(subset(df_list$mm_sshr, seeded == 1 &
                        species == "spruce"))
  df_list$mm_scut_spruce_seed <-
    droplevels(subset(df_list$mm_scut, seeded == 1 &
                        species == "spruce"))
  
  df_list$mm_nalp_fir_ex <-
    df_list$mm_nalp[df_list$mm_nalp$species == "fir" &
                      df_list$mm_nalp$treatment %in% c("seeded", "seeded.scarified") &
                      df_list$mm_nalp$exclosure == "yes", ]
  df_list$mm_nalp_fir_no <-
    df_list$mm_nalp[df_list$mm_nalp$species == "fir" &
                      df_list$mm_nalp$treatment %in% c("seeded", "seeded.scarified") &
                      df_list$mm_nalp$exclosure == "no", ]
  df_list$mm_salp_fir_ex <-
    df_list$mm_salp[df_list$mm_salp$species == "fir" &
                      df_list$mm_salp$treatment %in% c("seeded", "seeded.scarified") &
                      df_list$mm_salp$exclosure == "yes", ]
  df_list$mm_salp_fir_no <-
    df_list$mm_salp[df_list$mm_salp$species == "fir" &
                      df_list$mm_salp$treatment %in% c("seeded", "seeded.scarified") &
                      df_list$mm_salp$exclosure == "no", ]
  df_list$mm_sshr_fir_ex <-
    df_list$mm_scut[df_list$mm_scut$species == "fir" &
                      df_list$mm_scut$treatment %in% c("seeded", "seeded.scarified") &
                      df_list$mm_scut$exclosure == "yes", ]
  df_list$mm_sshr_fir_no <-
    df_list$mm_scut[df_list$mm_scut$species == "fir" &
                      df_list$mm_scut$treatment %in% c("seeded", "seeded.scarified") &
                      df_list$mm_scut$exclosure == "no", ]
  df_list$mm_scut_fir_ex <-
    df_list$mm_sshr[df_list$mm_sshr$species == "fir" &
                      df_list$mm_sshr$treatment %in% c("seeded", "seeded.scarified") &
                      df_list$mm_sshr$exclosure == "yes", ]
  df_list$mm_scut_fir_no <-
    df_list$mm_sshr[df_list$mm_sshr$species == "fir" &
                      df_list$mm_sshr$treatment %in% c("seeded", "seeded.scarified") &
                      df_list$mm_sshr$exclosure == "no", ]
  
  df_list$mm_nalp_spruce_ex <-
    df_list$mm_nalp[df_list$mm_nalp$species == "spruce" &
                      df_list$mm_nalp$treatment %in% c("seeded", "seeded.scarified") &
                      df_list$mm_nalp$exclosure == "yes", ]
  df_list$mm_nalp_spruce_no <-
    df_list$mm_nalp[df_list$mm_nalp$species == "spruce" &
                      df_list$mm_nalp$treatment %in% c("seeded", "seeded.scarified") &
                      df_list$mm_nalp$exclosure == "no", ]
  df_list$mm_salp_spruce_ex <-
    df_list$mm_salp[df_list$mm_salp$species == "spruce" &
                      df_list$mm_salp$treatment %in% c("seeded", "seeded.scarified") &
                      df_list$mm_salp$exclosure == "yes", ]
  df_list$mm_salp_spruce_no <-
    df_list$mm_salp[df_list$mm_salp$species == "spruce" &
                      df_list$mm_salp$treatment %in% c("seeded", "seeded.scarified") &
                      df_list$mm_salp$exclosure == "no", ]
  df_list$mm_sshr_spruce_ex <-
    df_list$mm_scut[df_list$mm_scut$species == "spruce" &
                      df_list$mm_scut$treatment %in% c("seeded", "seeded.scarified") &
                      df_list$mm_scut$exclosure == "yes", ]
  df_list$mm_sshr_spruce_no <-
    df_list$mm_scut[df_list$mm_scut$species == "spruce" &
                      df_list$mm_scut$treatment %in% c("seeded", "seeded.scarified") &
                      df_list$mm_scut$exclosure == "no", ]
  df_list$mm_scut_spruce_ex <-
    df_list$mm_sshr[df_list$mm_sshr$species == "spruce" &
                      df_list$mm_sshr$treatment %in% c("seeded", "seeded.scarified") &
                      df_list$mm_sshr$exclosure == "yes", ]
  df_list$mm_scut_spruce_no <-
    df_list$mm_sshr[df_list$mm_sshr$species == "spruce" &
                      df_list$mm_sshr$treatment %in% c("seeded", "seeded.scarified") &
                      df_list$mm_sshr$exclosure == "no", ]
  
  df_list$x1a <-
    factor(df_list$mm_nalp_fir_ex[, "treat"], levels = c("2", "4"))
  df_list$x1b <-
    factor(df_list$mm_nalp_fir_no[, "treat"], levels = c("2", "4"))
  df_list$x2a <-
    factor(df_list$mm_salp_fir_ex[, "treat"], levels = c("2", "4"))
  df_list$x2b <-
    factor(df_list$mm_salp_fir_no[, "treat"], levels = c("2", "4"))
  df_list$x3a <-
    factor(df_list$mm_scut_fir_ex[, "treat"], levels = c("2", "4"))
  df_list$x3b <-
    factor(df_list$mm_scut_fir_no[, "treat"], levels = c("2", "4"))
  df_list$x4a <-
    factor(df_list$mm_sshr_fir_ex[, "treat"], levels = c("2", "4"))
  df_list$x4b <-
    factor(df_list$mm_sshr_fir_no[, "treat"], levels = c("2", "4"))
  
  df_list$mm_nalp_fir_no_seed <-
    data.frame(
      year = c(rep(
        c(1:n_yr),
        c(
          length(df_list$mm_nalp_fir_no$surv_prop_0[df_list$mm_nalp_fir_no$treatment == "seeded"]),
          length(df_list$mm_nalp_fir_no$surv_prop_1[df_list$mm_nalp_fir_no$treatment == "seeded"]),
          length(df_list$mm_nalp_fir_no$surv_prop_2[df_list$mm_nalp_fir_no$treatment == "seeded"]),
          length(df_list$mm_nalp_fir_no$surv_prop_3[df_list$mm_nalp_fir_no$treatment == "seeded"]),
          length(df_list$mm_nalp_fir_no$surv_prop_4[df_list$mm_nalp_fir_no$treatment == "seeded"]),
          length(df_list$mm_nalp_fir_no$surv_prop_5[df_list$mm_nalp_fir_no$treatment == "seeded"])
        )
      )),
      surv = c(
        df_list$mm_nalp_fir_no$surv_prop_0[df_list$mm_nalp_fir_no$treatment == "seeded"],
        df_list$mm_nalp_fir_no$surv_prop_1[df_list$mm_nalp_fir_no$treatment == "seeded"],
        df_list$mm_nalp_fir_no$surv_prop_2[df_list$mm_nalp_fir_no$treatment == "seeded"],
        df_list$mm_nalp_fir_no$surv_prop_3[df_list$mm_nalp_fir_no$treatment == "seeded"],
        df_list$mm_nalp_fir_no$surv_prop_4[df_list$mm_nalp_fir_no$treatment == "seeded"],
        df_list$mm_nalp_fir_no$surv_prop_5[df_list$mm_nalp_fir_no$treatment == "seeded"]
      )
    )
  df_list$mm_nalp_fir_ex_seed <-
    data.frame(
      year = c(rep(
        c(1:n_yr),
        c(
          length(df_list$mm_nalp_fir_ex$surv_prop_0[df_list$mm_nalp_fir_ex$treatment == "seeded"]),
          length(df_list$mm_nalp_fir_ex$surv_prop_1[df_list$mm_nalp_fir_ex$treatment == "seeded"]),
          length(df_list$mm_nalp_fir_ex$surv_prop_2[df_list$mm_nalp_fir_ex$treatment == "seeded"]),
          length(df_list$mm_nalp_fir_ex$surv_prop_3[df_list$mm_nalp_fir_ex$treatment == "seeded"]),
          length(df_list$mm_nalp_fir_ex$surv_prop_4[df_list$mm_nalp_fir_ex$treatment == "seeded"]),
          length(df_list$mm_nalp_fir_ex$surv_prop_5[df_list$mm_nalp_fir_ex$treatment == "seeded"])
        )
      )),
      surv = c(
        df_list$mm_nalp_fir_ex$surv_prop_0[df_list$mm_nalp_fir_ex$treatment == "seeded"],
        df_list$mm_nalp_fir_ex$surv_prop_1[df_list$mm_nalp_fir_ex$treatment == "seeded"],
        df_list$mm_nalp_fir_ex$surv_prop_2[df_list$mm_nalp_fir_ex$treatment == "seeded"],
        df_list$mm_nalp_fir_ex$surv_prop_3[df_list$mm_nalp_fir_ex$treatment == "seeded"],
        df_list$mm_nalp_fir_ex$surv_prop_4[df_list$mm_nalp_fir_ex$treatment == "seeded"],
        df_list$mm_nalp_fir_ex$surv_prop_5[df_list$mm_nalp_fir_ex$treatment == "seeded"]
      )
    )
  df_list$mm_nalp_fir_no_seedscar <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_nalp_fir_no$surv_prop_0[df_list$mm_nalp_fir_no$treatment == "seeded.scarified"]),
          length(df_list$mm_nalp_fir_no$surv_prop_1[df_list$mm_nalp_fir_no$treatment == "seeded.scarified"]),
          length(df_list$mm_nalp_fir_no$surv_prop_2[df_list$mm_nalp_fir_no$treatment == "seeded.scarified"]),
          length(df_list$mm_nalp_fir_no$surv_prop_3[df_list$mm_nalp_fir_no$treatment == "seeded.scarified"]),
          length(df_list$mm_nalp_fir_no$surv_prop_4[df_list$mm_nalp_fir_no$treatment == "seeded.scarified"]),
          length(df_list$mm_nalp_fir_no$surv_prop_5[df_list$mm_nalp_fir_no$treatment == "seeded.scarified"])
        )
      )),
      surv = c(
        df_list$mm_nalp_fir_no$surv_prop_0[df_list$mm_nalp_fir_no$treatment == "seeded.scarified"],
        df_list$mm_nalp_fir_no$surv_prop_1[df_list$mm_nalp_fir_no$treatment == "seeded.scarified"],
        df_list$mm_nalp_fir_no$surv_prop_2[df_list$mm_nalp_fir_no$treatment == "seeded.scarified"],
        df_list$mm_nalp_fir_no$surv_prop_3[df_list$mm_nalp_fir_no$treatment == "seeded.scarified"],
        df_list$mm_nalp_fir_no$surv_prop_4[df_list$mm_nalp_fir_no$treatment == "seeded.scarified"],
        df_list$mm_nalp_fir_no$surv_prop_5[df_list$mm_nalp_fir_no$treatment == "seeded.scarified"]
      )
    )
  df_list$mm_nalp_fir_ex_seedscar <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_nalp_fir_ex$surv_prop_0[df_list$mm_nalp_fir_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_nalp_fir_ex$surv_prop_1[df_list$mm_nalp_fir_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_nalp_fir_ex$surv_prop_2[df_list$mm_nalp_fir_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_nalp_fir_ex$surv_prop_3[df_list$mm_nalp_fir_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_nalp_fir_ex$surv_prop_4[df_list$mm_nalp_fir_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_nalp_fir_ex$surv_prop_5[df_list$mm_nalp_fir_ex$treatment == "seeded.scarified"])
        )
      )),
      surv = c(
        df_list$mm_nalp_fir_ex$surv_prop_0[df_list$mm_nalp_fir_ex$treatment == "seeded.scarified"],
        df_list$mm_nalp_fir_ex$surv_prop_1[df_list$mm_nalp_fir_ex$treatment == "seeded.scarified"],
        df_list$mm_nalp_fir_ex$surv_prop_2[df_list$mm_nalp_fir_ex$treatment == "seeded.scarified"],
        df_list$mm_nalp_fir_ex$surv_prop_3[df_list$mm_nalp_fir_ex$treatment == "seeded.scarified"],
        df_list$mm_nalp_fir_ex$surv_prop_4[df_list$mm_nalp_fir_ex$treatment == "seeded.scarified"],
        df_list$mm_nalp_fir_ex$surv_prop_5[df_list$mm_nalp_fir_ex$treatment == "seeded.scarified"]
      )
    )
  
  
  df_list$mm_nalp_fir_no_seed_mod <-
    lm(log1p(surv) ~ year, df_list$mm_nalp_fir_no_seed)
  df_list$mm_nalp_fir_no_seed_ci <-
    data.frame(
      predict(
        df_list$mm_nalp_fir_no_seed_mod,
        newdata = data.frame(year = seq(1, n_yr, 0.1)),
        interval = "confidence",
        level = 0.95
      )
    )
  df_list$mm_nalp_fir_no_seed_ci$fit <-
    exp(df_list$mm_nalp_fir_no_seed_ci$fit) - 1
  df_list$mm_nalp_fir_no_seed_mean <-
    data.frame(
      year.vals = seq(1, n_yr, 0.1),
      surv_mean = df_list$mm_nalp_fir_no_seed_ci$fit
    ) #only doing wells with 2015-2020 data
  
  df_list$mm_nalp_fir_ex_seed_mod <-
    lm(log1p(surv) ~ year, df_list$mm_nalp_fir_ex_seed)
  df_list$mm_nalp_fir_ex_seed_ci <-
    data.frame(
      predict(
        df_list$mm_nalp_fir_ex_seed_mod,
        newdata = data.frame(year = seq(1, n_yr, 0.1)),
        interval = "confidence",
        level = 0.95
      )
    )
  df_list$mm_nalp_fir_ex_seed_ci$fit <-
    exp(df_list$mm_nalp_fir_ex_seed_ci$fit) - 1
  df_list$mm_nalp_fir_ex_seed_mean <-
    data.frame(
      year.vals = seq(1, n_yr, 0.1),
      surv_mean = df_list$mm_nalp_fir_ex_seed_ci$fit
    ) #only doing wells with 2015-2020 data
  
  df_list$mm_nalp_fir_no_seedscar_mod <-
    lm(log1p(surv) ~ year, df_list$mm_nalp_fir_no_seedscar)
  df_list$mm_nalp_fir_no_seedscar_ci <-
    data.frame(
      predict(
        df_list$mm_nalp_fir_no_seedscar_mod,
        newdata = data.frame(year = seq(1, n_yr, 0.1)),
        interval = "confidence",
        level = 0.95
      )
    )
  df_list$mm_nalp_fir_no_seedscar_ci$fit <-
    exp(df_list$mm_nalp_fir_no_seedscar_ci$fit) - 1
  df_list$mm_nalp_fir_no_seedscar_mean <-
    data.frame(
      year.vals = seq(1, n_yr, 0.1),
      surv_mean = df_list$mm_nalp_fir_no_seedscar_ci$fit
    ) #only doing wells with 2015-2020 data
  
  df_list$mm_nalp_fir_ex_seedscar_mod <-
    lm(log1p(surv) ~ year, df_list$mm_nalp_fir_ex_seedscar)
  df_list$mm_nalp_fir_ex_seedscar_ci <-
    data.frame(
      predict(
        df_list$mm_nalp_fir_ex_seedscar_mod,
        newdata = data.frame(year = seq(1, n_yr, 0.1)),
        interval = "confidence",
        level = 0.95
      )
    )
  df_list$mm_nalp_fir_ex_seedscar_ci$fit <-
    exp(df_list$mm_nalp_fir_ex_seedscar_ci$fit) - 1
  df_list$mm_nalp_fir_ex_seedscar_mean <-
    data.frame(
      year.vals = seq(1, n_yr, 0.1),
      surv_mean = df_list$mm_nalp_fir_ex_seedscar_ci$fit
    ) #only doing wells with 2015-2020 data
  
  ## SALP - fir
  df_list$mm_salp_fir_no_seed <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_salp_fir_no$surv_prop_0[df_list$mm_salp_fir_no$treatment == "seeded"]),
          length(df_list$mm_salp_fir_no$surv_prop_1[df_list$mm_salp_fir_no$treatment == "seeded"]),
          length(df_list$mm_salp_fir_no$surv_prop_2[df_list$mm_salp_fir_no$treatment == "seeded"]),
          length(df_list$mm_salp_fir_no$surv_prop_3[df_list$mm_salp_fir_no$treatment == "seeded"]),
          length(df_list$mm_salp_fir_no$surv_prop_4[df_list$mm_salp_fir_no$treatment == "seeded"]),
          length(df_list$mm_salp_fir_no$surv_prop_5[df_list$mm_salp_fir_no$treatment == "seeded"])
        )
      )),
      surv = c(
        df_list$mm_salp_fir_no$surv_prop_0[df_list$mm_salp_fir_no$treatment == "seeded"],
        df_list$mm_salp_fir_no$surv_prop_1[df_list$mm_salp_fir_no$treatment == "seeded"],
        df_list$mm_salp_fir_no$surv_prop_2[df_list$mm_salp_fir_no$treatment == "seeded"],
        df_list$mm_salp_fir_no$surv_prop_3[df_list$mm_salp_fir_no$treatment == "seeded"],
        df_list$mm_salp_fir_no$surv_prop_4[df_list$mm_salp_fir_no$treatment == "seeded"],
        df_list$mm_salp_fir_no$surv_prop_5[df_list$mm_salp_fir_no$treatment == "seeded"]
      )
    )
  df_list$mm_salp_fir_ex_seed <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_salp_fir_ex$surv_prop_0[df_list$mm_salp_fir_ex$treatment == "seeded"]),
          length(df_list$mm_salp_fir_ex$surv_prop_1[df_list$mm_salp_fir_ex$treatment == "seeded"]),
          length(df_list$mm_salp_fir_ex$surv_prop_2[df_list$mm_salp_fir_ex$treatment == "seeded"]),
          length(df_list$mm_salp_fir_ex$surv_prop_3[df_list$mm_salp_fir_ex$treatment == "seeded"]),
          length(df_list$mm_salp_fir_ex$surv_prop_4[df_list$mm_salp_fir_ex$treatment == "seeded"]),
          length(df_list$mm_salp_fir_ex$surv_prop_5[df_list$mm_salp_fir_ex$treatment == "seeded"])
        )
      )),
      surv = c(
        df_list$mm_salp_fir_ex$surv_prop_0[df_list$mm_salp_fir_ex$treatment == "seeded"],
        df_list$mm_salp_fir_ex$surv_prop_1[df_list$mm_salp_fir_ex$treatment == "seeded"],
        df_list$mm_salp_fir_ex$surv_prop_2[df_list$mm_salp_fir_ex$treatment == "seeded"],
        df_list$mm_salp_fir_ex$surv_prop_3[df_list$mm_salp_fir_ex$treatment == "seeded"],
        df_list$mm_salp_fir_ex$surv_prop_4[df_list$mm_salp_fir_ex$treatment == "seeded"],
        df_list$mm_salp_fir_ex$surv_prop_5[df_list$mm_salp_fir_ex$treatment == "seeded"]
      )
    )
  df_list$mm_salp_fir_no_seedscar <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_salp_fir_no$surv_prop_0[df_list$mm_salp_fir_no$treatment == "seeded.scarified"]),
          length(df_list$mm_salp_fir_no$surv_prop_1[df_list$mm_salp_fir_no$treatment == "seeded.scarified"]),
          length(df_list$mm_salp_fir_no$surv_prop_2[df_list$mm_salp_fir_no$treatment == "seeded.scarified"]),
          length(df_list$mm_salp_fir_no$surv_prop_3[df_list$mm_salp_fir_no$treatment == "seeded.scarified"]),
          length(df_list$mm_salp_fir_no$surv_prop_4[df_list$mm_salp_fir_no$treatment == "seeded.scarified"]),
          length(df_list$mm_salp_fir_no$surv_prop_5[df_list$mm_salp_fir_no$treatment == "seeded.scarified"])
        )
      )),
      surv = c(
        df_list$mm_salp_fir_no$surv_prop_0[df_list$mm_salp_fir_no$treatment == "seeded.scarified"],
        df_list$mm_salp_fir_no$surv_prop_1[df_list$mm_salp_fir_no$treatment == "seeded.scarified"],
        df_list$mm_salp_fir_no$surv_prop_2[df_list$mm_salp_fir_no$treatment == "seeded.scarified"],
        df_list$mm_salp_fir_no$surv_prop_3[df_list$mm_salp_fir_no$treatment == "seeded.scarified"],
        df_list$mm_salp_fir_no$surv_prop_4[df_list$mm_salp_fir_no$treatment == "seeded.scarified"],
        df_list$mm_salp_fir_no$surv_prop_5[df_list$mm_salp_fir_no$treatment == "seeded.scarified"]
      )
    )
  df_list$mm_salp_fir_ex_seedscar <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_salp_fir_ex$surv_prop_0[df_list$mm_salp_fir_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_salp_fir_ex$surv_prop_1[df_list$mm_salp_fir_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_salp_fir_ex$surv_prop_2[df_list$mm_salp_fir_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_salp_fir_ex$surv_prop_3[df_list$mm_salp_fir_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_salp_fir_ex$surv_prop_4[df_list$mm_salp_fir_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_salp_fir_ex$surv_prop_5[df_list$mm_salp_fir_ex$treatment == "seeded.scarified"])
        )
      )),
      surv = c(
        df_list$mm_salp_fir_ex$surv_prop_0[df_list$mm_salp_fir_ex$treatment == "seeded.scarified"],
        df_list$mm_salp_fir_ex$surv_prop_1[df_list$mm_salp_fir_ex$treatment == "seeded.scarified"],
        df_list$mm_salp_fir_ex$surv_prop_2[df_list$mm_salp_fir_ex$treatment == "seeded.scarified"],
        df_list$mm_salp_fir_ex$surv_prop_3[df_list$mm_salp_fir_ex$treatment == "seeded.scarified"],
        df_list$mm_salp_fir_ex$surv_prop_4[df_list$mm_salp_fir_ex$treatment == "seeded.scarified"],
        df_list$mm_salp_fir_ex$surv_prop_5[df_list$mm_salp_fir_ex$treatment == "seeded.scarified"]
      )
    )
  
  
  df_list$mm_salp_fir_no_seed_mod <-
    lm(log1p(surv) ~ year, df_list$mm_salp_fir_no_seed)
  df_list$mm_salp_fir_no_seed_ci <-
    data.frame(
      predict(
        df_list$mm_salp_fir_no_seed_mod,
        newdata = data.frame(year = seq(1, n_yr, 0.1)),
        interval = "confidence",
        level = 0.95
      )
    )
  df_list$mm_salp_fir_no_seed_ci$fit <-
    exp(df_list$mm_salp_fir_no_seed_ci$fit) - 1
  df_list$mm_salp_fir_no_seed_mean <-
    data.frame(
      year.vals = seq(1, n_yr, 0.1),
      surv_mean = df_list$mm_salp_fir_no_seed_ci$fit
    ) #only doing wells with 2015-2020 data
  
  df_list$mm_salp_fir_ex_seed_mod <-
    lm(log1p(surv) ~ year, df_list$mm_salp_fir_ex_seed)
  df_list$mm_salp_fir_ex_seed_ci <-
    data.frame(
      predict(
        df_list$mm_salp_fir_ex_seed_mod,
        newdata = data.frame(year = seq(1, n_yr, 0.1)),
        interval = "confidence",
        level = 0.95
      )
    )
  df_list$mm_salp_fir_ex_seed_ci$fit <-
    exp(df_list$mm_salp_fir_ex_seed_ci$fit) - 1
  df_list$mm_salp_fir_ex_seed_mean <-
    data.frame(
      year.vals = seq(1, n_yr, 0.1),
      surv_mean = df_list$mm_salp_fir_ex_seed_ci$fit
    ) #only doing wells with 2015-2020 data
  
  df_list$mm_salp_fir_no_seedscar_mod <-
    lm(log1p(surv) ~ year, df_list$mm_salp_fir_no_seedscar)
  df_list$mm_salp_fir_no_seedscar_ci <-
    data.frame(
      predict(
        df_list$mm_salp_fir_no_seedscar_mod,
        newdata = data.frame(year = seq(1, n_yr, 0.1)),
        interval = "confidence",
        level = 0.95
      )
    )
  df_list$mm_salp_fir_no_seedscar_ci$fit <-
    exp(df_list$mm_salp_fir_no_seedscar_ci$fit) - 1
  df_list$mm_salp_fir_no_seedscar_mean <-
    data.frame(
      year.vals = seq(1, n_yr, 0.1),
      surv_mean = df_list$mm_salp_fir_no_seedscar_ci$fit
    ) #only doing wells with 2015-2020 data
  
  df_list$mm_salp_fir_ex_seedscar_mod <-
    lm(log1p(surv) ~ year, df_list$mm_salp_fir_ex_seedscar)
  df_list$mm_salp_fir_ex_seedscar_ci <-
    data.frame(
      predict(
        df_list$mm_salp_fir_ex_seedscar_mod,
        newdata = data.frame(year = seq(1, n_yr, 0.1)),
        interval = "confidence",
        level = 0.95
      )
    )
  df_list$mm_salp_fir_ex_seedscar_ci$fit <-
    exp(df_list$mm_salp_fir_ex_seedscar_ci$fit) - 1
  df_list$mm_salp_fir_ex_seedscar_mean <-
    data.frame(
      year.vals = seq(1, n_yr, 0.1),
      surv_mean = df_list$mm_salp_fir_ex_seedscar_ci$fit
    ) #only doing wells with 2015-2020 data
  
  
  df_list$mm_nalp_spruce_no_seed <-
    data.frame(
      year = c(rep(
        c(1:n_yr),
        c(
          length(df_list$mm_nalp_spruce_no$surv_prop_0[df_list$mm_nalp_spruce_no$treatment == "seeded"]),
          length(df_list$mm_nalp_spruce_no$surv_prop_1[df_list$mm_nalp_spruce_no$treatment == "seeded"]),
          length(df_list$mm_nalp_spruce_no$surv_prop_2[df_list$mm_nalp_spruce_no$treatment == "seeded"]),
          length(df_list$mm_nalp_spruce_no$surv_prop_3[df_list$mm_nalp_spruce_no$treatment == "seeded"]),
          length(df_list$mm_nalp_spruce_no$surv_prop_4[df_list$mm_nalp_spruce_no$treatment == "seeded"]),
          length(df_list$mm_nalp_spruce_no$surv_prop_5[df_list$mm_nalp_spruce_no$treatment == "seeded"])
        )
      )),
      surv = c(
        df_list$mm_nalp_spruce_no$surv_prop_0[df_list$mm_nalp_spruce_no$treatment == "seeded"],
        df_list$mm_nalp_spruce_no$surv_prop_1[df_list$mm_nalp_spruce_no$treatment == "seeded"],
        df_list$mm_nalp_spruce_no$surv_prop_2[df_list$mm_nalp_spruce_no$treatment == "seeded"],
        df_list$mm_nalp_spruce_no$surv_prop_3[df_list$mm_nalp_spruce_no$treatment == "seeded"],
        df_list$mm_nalp_spruce_no$surv_prop_4[df_list$mm_nalp_spruce_no$treatment == "seeded"],
        df_list$mm_nalp_spruce_no$surv_prop_5[df_list$mm_nalp_spruce_no$treatment == "seeded"]
      )
    )
  df_list$mm_nalp_spruce_ex_seed <-
    data.frame(
      year = c(rep(
        c(1:n_yr),
        c(
          length(df_list$mm_nalp_spruce_ex$surv_prop_0[df_list$mm_nalp_spruce_ex$treatment == "seeded"]),
          length(df_list$mm_nalp_spruce_ex$surv_prop_1[df_list$mm_nalp_spruce_ex$treatment == "seeded"]),
          length(df_list$mm_nalp_spruce_ex$surv_prop_2[df_list$mm_nalp_spruce_ex$treatment == "seeded"]),
          length(df_list$mm_nalp_spruce_ex$surv_prop_3[df_list$mm_nalp_spruce_ex$treatment == "seeded"]),
          length(df_list$mm_nalp_spruce_ex$surv_prop_4[df_list$mm_nalp_spruce_ex$treatment == "seeded"]),
          length(df_list$mm_nalp_spruce_ex$surv_prop_5[df_list$mm_nalp_spruce_ex$treatment == "seeded"])
        )
      )),
      surv = c(
        df_list$mm_nalp_spruce_ex$surv_prop_0[df_list$mm_nalp_spruce_ex$treatment == "seeded"],
        df_list$mm_nalp_spruce_ex$surv_prop_1[df_list$mm_nalp_spruce_ex$treatment == "seeded"],
        df_list$mm_nalp_spruce_ex$surv_prop_2[df_list$mm_nalp_spruce_ex$treatment == "seeded"],
        df_list$mm_nalp_spruce_ex$surv_prop_3[df_list$mm_nalp_spruce_ex$treatment == "seeded"],
        df_list$mm_nalp_spruce_ex$surv_prop_4[df_list$mm_nalp_spruce_ex$treatment == "seeded"],
        df_list$mm_nalp_spruce_ex$surv_prop_5[df_list$mm_nalp_spruce_ex$treatment == "seeded"]
      )
    )
  df_list$mm_nalp_spruce_no_seedscar <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_nalp_spruce_no$surv_prop_0[df_list$mm_nalp_spruce_no$treatment == "seeded.scarified"]),
          length(df_list$mm_nalp_spruce_no$surv_prop_1[df_list$mm_nalp_spruce_no$treatment == "seeded.scarified"]),
          length(df_list$mm_nalp_spruce_no$surv_prop_2[df_list$mm_nalp_spruce_no$treatment == "seeded.scarified"]),
          length(df_list$mm_nalp_spruce_no$surv_prop_3[df_list$mm_nalp_spruce_no$treatment == "seeded.scarified"]),
          length(df_list$mm_nalp_spruce_no$surv_prop_4[df_list$mm_nalp_spruce_no$treatment == "seeded.scarified"]),
          length(df_list$mm_nalp_spruce_no$surv_prop_5[df_list$mm_nalp_spruce_no$treatment == "seeded.scarified"])
        )
      )),
      surv = c(
        df_list$mm_nalp_spruce_no$surv_prop_0[df_list$mm_nalp_spruce_no$treatment == "seeded.scarified"],
        df_list$mm_nalp_spruce_no$surv_prop_1[df_list$mm_nalp_spruce_no$treatment == "seeded.scarified"],
        df_list$mm_nalp_spruce_no$surv_prop_2[df_list$mm_nalp_spruce_no$treatment == "seeded.scarified"],
        df_list$mm_nalp_spruce_no$surv_prop_3[df_list$mm_nalp_spruce_no$treatment == "seeded.scarified"],
        df_list$mm_nalp_spruce_no$surv_prop_4[df_list$mm_nalp_spruce_no$treatment == "seeded.scarified"],
        df_list$mm_nalp_spruce_no$surv_prop_5[df_list$mm_nalp_spruce_no$treatment == "seeded.scarified"]
      )
    )
  df_list$mm_nalp_spruce_ex_seedscar <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_nalp_spruce_ex$surv_prop_0[df_list$mm_nalp_spruce_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_nalp_spruce_ex$surv_prop_1[df_list$mm_nalp_spruce_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_nalp_spruce_ex$surv_prop_2[df_list$mm_nalp_spruce_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_nalp_spruce_ex$surv_prop_3[df_list$mm_nalp_spruce_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_nalp_spruce_ex$surv_prop_4[df_list$mm_nalp_spruce_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_nalp_spruce_ex$surv_prop_5[df_list$mm_nalp_spruce_ex$treatment == "seeded.scarified"])
        )
      )),
      surv = c(
        df_list$mm_nalp_spruce_ex$surv_prop_0[df_list$mm_nalp_spruce_ex$treatment == "seeded.scarified"],
        df_list$mm_nalp_spruce_ex$surv_prop_1[df_list$mm_nalp_spruce_ex$treatment == "seeded.scarified"],
        df_list$mm_nalp_spruce_ex$surv_prop_2[df_list$mm_nalp_spruce_ex$treatment == "seeded.scarified"],
        df_list$mm_nalp_spruce_ex$surv_prop_3[df_list$mm_nalp_spruce_ex$treatment == "seeded.scarified"],
        df_list$mm_nalp_spruce_ex$surv_prop_4[df_list$mm_nalp_spruce_ex$treatment == "seeded.scarified"],
        df_list$mm_nalp_spruce_ex$surv_prop_5[df_list$mm_nalp_spruce_ex$treatment == "seeded.scarified"]
      )
    )
  
  
  df_list$mm_nalp_spruce_no_seed_mod <-
    lm(log1p(surv) ~ year, df_list$mm_nalp_spruce_no_seed)
  df_list$mm_nalp_spruce_no_seed_ci <-
    data.frame(
      predict(
        df_list$mm_nalp_spruce_no_seed_mod,
        newdata = data.frame(year = seq(1, n_yr, 0.1)),
        interval = "confidence",
        level = 0.95
      )
    )
  df_list$mm_nalp_spruce_no_seed_ci$fit <-
    exp(df_list$mm_nalp_spruce_no_seed_ci$fit) - 1
  df_list$mm_nalp_spruce_no_seed_mean <-
    data.frame(
      year.vals = seq(1, n_yr, 0.1),
      surv_mean = df_list$mm_nalp_spruce_no_seed_ci$fit
    ) #only doing wells with 2015-2020 data
  
  df_list$mm_nalp_spruce_ex_seed_mod <-
    lm(log1p(surv) ~ year, df_list$mm_nalp_spruce_ex_seed)
  df_list$mm_nalp_spruce_ex_seed_ci <-
    data.frame(
      predict(
        df_list$mm_nalp_spruce_ex_seed_mod,
        newdata = data.frame(year = seq(1, n_yr, 0.1)),
        interval = "confidence",
        level = 0.95
      )
    )
  df_list$mm_nalp_spruce_ex_seed_ci$fit <-
    exp(df_list$mm_nalp_spruce_ex_seed_ci$fit) - 1
  df_list$mm_nalp_spruce_ex_seed_mean <-
    data.frame(
      year.vals = seq(1, n_yr, 0.1),
      surv_mean = df_list$mm_nalp_spruce_ex_seed_ci$fit
    ) #only doing wells with 2015-2020 data
  
  df_list$mm_nalp_spruce_no_seedscar_mod <-
    lm(log1p(surv) ~ year, df_list$mm_nalp_spruce_no_seedscar)
  df_list$mm_nalp_spruce_no_seedscar_ci <-
    data.frame(
      predict(
        df_list$mm_nalp_spruce_no_seedscar_mod,
        newdata = data.frame(year = seq(1, n_yr, 0.1)),
        interval = "confidence",
        level = 0.95
      )
    )
  df_list$mm_nalp_spruce_no_seedscar_ci$fit <-
    exp(df_list$mm_nalp_spruce_no_seedscar_ci$fit) - 1
  df_list$mm_nalp_spruce_no_seedscar_mean <-
    data.frame(
      year.vals = seq(1, n_yr, 0.1),
      surv_mean = df_list$mm_nalp_spruce_no_seedscar_ci$fit
    ) #only doing wells with 2015-2020 data
  
  df_list$mm_nalp_spruce_ex_seedscar_mod <-
    lm(log1p(surv) ~ year, df_list$mm_nalp_spruce_ex_seedscar)
  df_list$mm_nalp_spruce_ex_seedscar_ci <-
    data.frame(
      predict(
        df_list$mm_nalp_spruce_ex_seedscar_mod,
        newdata = data.frame(year = seq(1, n_yr, 0.1)),
        interval = "confidence",
        level = 0.95
      )
    )
  df_list$mm_nalp_spruce_ex_seedscar_ci$fit <-
    exp(df_list$mm_nalp_spruce_ex_seedscar_ci$fit) - 1
  df_list$mm_nalp_spruce_ex_seedscar_mean <-
    data.frame(
      year.vals = seq(1, n_yr, 0.1),
      surv_mean = df_list$mm_nalp_spruce_ex_seedscar_ci$fit
    ) #only doing wells with 2015-2020 data
  
  ## SALP - spruce
  df_list$mm_salp_spruce_no_seed <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_salp_spruce_no$surv_prop_0[df_list$mm_salp_spruce_no$treatment == "seeded"]),
          length(df_list$mm_salp_spruce_no$surv_prop_1[df_list$mm_salp_spruce_no$treatment == "seeded"]),
          length(df_list$mm_salp_spruce_no$surv_prop_2[df_list$mm_salp_spruce_no$treatment == "seeded"]),
          length(df_list$mm_salp_spruce_no$surv_prop_3[df_list$mm_salp_spruce_no$treatment == "seeded"]),
          length(df_list$mm_salp_spruce_no$surv_prop_4[df_list$mm_salp_spruce_no$treatment == "seeded"]),
          length(df_list$mm_salp_spruce_no$surv_prop_5[df_list$mm_salp_spruce_no$treatment == "seeded"])
        )
      )),
      surv = c(
        df_list$mm_salp_spruce_no$surv_prop_0[df_list$mm_salp_spruce_no$treatment == "seeded"],
        df_list$mm_salp_spruce_no$surv_prop_1[df_list$mm_salp_spruce_no$treatment == "seeded"],
        df_list$mm_salp_spruce_no$surv_prop_2[df_list$mm_salp_spruce_no$treatment == "seeded"],
        df_list$mm_salp_spruce_no$surv_prop_3[df_list$mm_salp_spruce_no$treatment == "seeded"],
        df_list$mm_salp_spruce_no$surv_prop_4[df_list$mm_salp_spruce_no$treatment == "seeded"],
        df_list$mm_salp_spruce_no$surv_prop_5[df_list$mm_salp_spruce_no$treatment == "seeded"]
      )
    )
  df_list$mm_salp_spruce_ex_seed <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_salp_spruce_ex$surv_prop_0[df_list$mm_salp_spruce_ex$treatment == "seeded"]),
          length(df_list$mm_salp_spruce_ex$surv_prop_1[df_list$mm_salp_spruce_ex$treatment == "seeded"]),
          length(df_list$mm_salp_spruce_ex$surv_prop_2[df_list$mm_salp_spruce_ex$treatment == "seeded"]),
          length(df_list$mm_salp_spruce_ex$surv_prop_3[df_list$mm_salp_spruce_ex$treatment == "seeded"]),
          length(df_list$mm_salp_spruce_ex$surv_prop_4[df_list$mm_salp_spruce_ex$treatment == "seeded"]),
          length(df_list$mm_salp_spruce_ex$surv_prop_5[df_list$mm_salp_spruce_ex$treatment == "seeded"])
        )
      )),
      surv = c(
        df_list$mm_salp_spruce_ex$surv_prop_0[df_list$mm_salp_spruce_ex$treatment == "seeded"],
        df_list$mm_salp_spruce_ex$surv_prop_1[df_list$mm_salp_spruce_ex$treatment == "seeded"],
        df_list$mm_salp_spruce_ex$surv_prop_2[df_list$mm_salp_spruce_ex$treatment == "seeded"],
        df_list$mm_salp_spruce_ex$surv_prop_3[df_list$mm_salp_spruce_ex$treatment == "seeded"],
        df_list$mm_salp_spruce_ex$surv_prop_4[df_list$mm_salp_spruce_ex$treatment == "seeded"],
        df_list$mm_salp_spruce_ex$surv_prop_5[df_list$mm_salp_spruce_ex$treatment == "seeded"]
      )
    )
  df_list$mm_salp_spruce_no_seedscar <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_salp_spruce_no$surv_prop_0[df_list$mm_salp_spruce_no$treatment == "seeded.scarified"]),
          length(df_list$mm_salp_spruce_no$surv_prop_1[df_list$mm_salp_spruce_no$treatment == "seeded.scarified"]),
          length(df_list$mm_salp_spruce_no$surv_prop_2[df_list$mm_salp_spruce_no$treatment == "seeded.scarified"]),
          length(df_list$mm_salp_spruce_no$surv_prop_3[df_list$mm_salp_spruce_no$treatment == "seeded.scarified"]),
          length(df_list$mm_salp_spruce_no$surv_prop_4[df_list$mm_salp_spruce_no$treatment == "seeded.scarified"]),
          length(df_list$mm_salp_spruce_no$surv_prop_5[df_list$mm_salp_spruce_no$treatment == "seeded.scarified"])
        )
      )),
      surv = c(
        df_list$mm_salp_spruce_no$surv_prop_0[df_list$mm_salp_spruce_no$treatment == "seeded.scarified"],
        df_list$mm_salp_spruce_no$surv_prop_1[df_list$mm_salp_spruce_no$treatment == "seeded.scarified"],
        df_list$mm_salp_spruce_no$surv_prop_2[df_list$mm_salp_spruce_no$treatment == "seeded.scarified"],
        df_list$mm_salp_spruce_no$surv_prop_3[df_list$mm_salp_spruce_no$treatment == "seeded.scarified"],
        df_list$mm_salp_spruce_no$surv_prop_4[df_list$mm_salp_spruce_no$treatment == "seeded.scarified"],
        df_list$mm_salp_spruce_no$surv_prop_5[df_list$mm_salp_spruce_no$treatment == "seeded.scarified"]
      )
    )
  df_list$mm_salp_spruce_ex_seedscar <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_salp_spruce_ex$surv_prop_0[df_list$mm_salp_spruce_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_salp_spruce_ex$surv_prop_1[df_list$mm_salp_spruce_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_salp_spruce_ex$surv_prop_2[df_list$mm_salp_spruce_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_salp_spruce_ex$surv_prop_3[df_list$mm_salp_spruce_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_salp_spruce_ex$surv_prop_4[df_list$mm_salp_spruce_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_salp_spruce_ex$surv_prop_5[df_list$mm_salp_spruce_ex$treatment == "seeded.scarified"])
        )
      )),
      surv = c(
        df_list$mm_salp_spruce_ex$surv_prop_0[df_list$mm_salp_spruce_ex$treatment == "seeded.scarified"],
        df_list$mm_salp_spruce_ex$surv_prop_1[df_list$mm_salp_spruce_ex$treatment == "seeded.scarified"],
        df_list$mm_salp_spruce_ex$surv_prop_2[df_list$mm_salp_spruce_ex$treatment == "seeded.scarified"],
        df_list$mm_salp_spruce_ex$surv_prop_3[df_list$mm_salp_spruce_ex$treatment == "seeded.scarified"],
        df_list$mm_salp_spruce_ex$surv_prop_4[df_list$mm_salp_spruce_ex$treatment == "seeded.scarified"],
        df_list$mm_salp_spruce_ex$surv_prop_5[df_list$mm_salp_spruce_ex$treatment == "seeded.scarified"]
      )
    )
  
  
  df_list$mm_salp_spruce_no_seed_mod <-
    lm(log1p(surv) ~ year, df_list$mm_salp_spruce_no_seed)
  df_list$mm_salp_spruce_no_seed_ci <-
    data.frame(
      predict(
        df_list$mm_salp_spruce_no_seed_mod,
        newdata = data.frame(year = seq(1, n_yr, 0.1)),
        interval = "confidence",
        level = 0.95
      )
    )
  df_list$mm_salp_spruce_no_seed_ci$fit <-
    exp(df_list$mm_salp_spruce_no_seed_ci$fit) - 1
  df_list$mm_salp_spruce_no_seed_mean <-
    data.frame(
      year.vals = seq(1, n_yr, 0.1),
      surv_mean = df_list$mm_salp_spruce_no_seed_ci$fit
    ) #only doing wells with 2015-2020 data
  
  df_list$mm_salp_spruce_ex_seed_mod <-
    lm(log1p(surv) ~ year, df_list$mm_salp_spruce_ex_seed)
  df_list$mm_salp_spruce_ex_seed_ci <-
    data.frame(
      predict(
        df_list$mm_salp_spruce_ex_seed_mod,
        newdata = data.frame(year = seq(1, n_yr, 0.1)),
        interval = "confidence",
        level = 0.95
      )
    )
  df_list$mm_salp_spruce_ex_seed_ci$fit <-
    exp(df_list$mm_salp_spruce_ex_seed_ci$fit) - 1
  df_list$mm_salp_spruce_ex_seed_mean <-
    data.frame(
      year.vals = seq(1, n_yr, 0.1),
      surv_mean = df_list$mm_salp_spruce_ex_seed_ci$fit
    ) #only doing wells with 2015-2020 data
  
  df_list$mm_salp_spruce_no_seedscar_mod <-
    lm(log1p(surv) ~ year, df_list$mm_salp_spruce_no_seedscar)
  df_list$mm_salp_spruce_no_seedscar_ci <-
    data.frame(
      predict(
        df_list$mm_salp_spruce_no_seedscar_mod,
        newdata = data.frame(year = seq(1, n_yr, 0.1)),
        interval = "confidence",
        level = 0.95
      )
    )
  df_list$mm_salp_spruce_no_seedscar_ci$fit <-
    exp(df_list$mm_salp_spruce_no_seedscar_ci$fit) - 1
  df_list$mm_salp_spruce_no_seedscar_mean <-
    data.frame(
      year.vals = seq(1, n_yr, 0.1),
      surv_mean = df_list$mm_salp_spruce_no_seedscar_ci$fit
    ) #only doing wells with 2015-2020 data
  
  df_list$mm_salp_spruce_ex_seedscar_mod <-
    lm(log1p(surv) ~ year, df_list$mm_salp_spruce_ex_seedscar)
  df_list$mm_salp_spruce_ex_seedscar_ci <-
    data.frame(
      predict(
        df_list$mm_salp_spruce_ex_seedscar_mod,
        newdata = data.frame(year = seq(1, n_yr, 0.1)),
        interval = "confidence",
        level = 0.95
      )
    )
  df_list$mm_salp_spruce_ex_seedscar_ci$fit <-
    exp(df_list$mm_salp_spruce_ex_seedscar_ci$fit) - 1
  df_list$mm_salp_spruce_ex_seedscar_mean <-
    data.frame(
      year.vals = seq(1, n_yr, 0.1),
      surv_mean = df_list$mm_salp_spruce_ex_seedscar_ci$fit
    ) #only doing wells with 2015-2020 data
  
  ## SCUT - fir
  df_list$mm_scut_fir_no_seed <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_scut_fir_no$surv_prop_0[df_list$mm_scut_fir_no$treatment == "seeded"]),
          length(df_list$mm_scut_fir_no$surv_prop_1[df_list$mm_scut_fir_no$treatment == "seeded"]),
          length(df_list$mm_scut_fir_no$surv_prop_2[df_list$mm_scut_fir_no$treatment == "seeded"]),
          length(df_list$mm_scut_fir_no$surv_prop_3[df_list$mm_scut_fir_no$treatment == "seeded"]),
          length(df_list$mm_scut_fir_no$surv_prop_4[df_list$mm_scut_fir_no$treatment == "seeded"]),
          length(df_list$mm_scut_fir_no$surv_prop_5[df_list$mm_scut_fir_no$treatment == "seeded"])
        )
      )),
      surv = c(
        df_list$mm_scut_fir_no$surv_prop_0[df_list$mm_scut_fir_no$treatment == "seeded"],
        df_list$mm_scut_fir_no$surv_prop_1[df_list$mm_scut_fir_no$treatment == "seeded"],
        df_list$mm_scut_fir_no$surv_prop_2[df_list$mm_scut_fir_no$treatment == "seeded"],
        df_list$mm_scut_fir_no$surv_prop_3[df_list$mm_scut_fir_no$treatment == "seeded"],
        df_list$mm_scut_fir_no$surv_prop_4[df_list$mm_scut_fir_no$treatment == "seeded"],
        df_list$mm_scut_fir_no$surv_prop_5[df_list$mm_scut_fir_no$treatment == "seeded"]
      )
    )
  df_list$mm_scut_fir_ex_seed <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_scut_fir_ex$surv_prop_0[df_list$mm_scut_fir_ex$treatment == "seeded"]),
          length(df_list$mm_scut_fir_ex$surv_prop_1[df_list$mm_scut_fir_ex$treatment == "seeded"]),
          length(df_list$mm_scut_fir_ex$surv_prop_2[df_list$mm_scut_fir_ex$treatment == "seeded"]),
          length(df_list$mm_scut_fir_ex$surv_prop_3[df_list$mm_scut_fir_ex$treatment == "seeded"]),
          length(df_list$mm_scut_fir_ex$surv_prop_4[df_list$mm_scut_fir_ex$treatment == "seeded"]),
          length(df_list$mm_scut_fir_ex$surv_prop_5[df_list$mm_scut_fir_ex$treatment == "seeded"])
        )
      )),
      surv = c(
        df_list$mm_scut_fir_ex$surv_prop_0[df_list$mm_scut_fir_ex$treatment == "seeded"],
        df_list$mm_scut_fir_ex$surv_prop_1[df_list$mm_scut_fir_ex$treatment == "seeded"],
        df_list$mm_scut_fir_ex$surv_prop_2[df_list$mm_scut_fir_ex$treatment == "seeded"],
        df_list$mm_scut_fir_ex$surv_prop_3[df_list$mm_scut_fir_ex$treatment == "seeded"],
        df_list$mm_scut_fir_ex$surv_prop_4[df_list$mm_scut_fir_ex$treatment == "seeded"],
        df_list$mm_scut_fir_ex$surv_prop_5[df_list$mm_scut_fir_ex$treatment == "seeded"]
      )
    )
  df_list$mm_scut_fir_no_seedscar <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_scut_fir_no$surv_prop_0[df_list$mm_scut_fir_no$treatment == "seeded.scarified"]),
          length(df_list$mm_scut_fir_no$surv_prop_1[df_list$mm_scut_fir_no$treatment == "seeded.scarified"]),
          length(df_list$mm_scut_fir_no$surv_prop_2[df_list$mm_scut_fir_no$treatment == "seeded.scarified"]),
          length(df_list$mm_scut_fir_no$surv_prop_3[df_list$mm_scut_fir_no$treatment == "seeded.scarified"]),
          length(df_list$mm_scut_fir_no$surv_prop_4[df_list$mm_scut_fir_no$treatment == "seeded.scarified"]),
          length(df_list$mm_scut_fir_no$surv_prop_5[df_list$mm_scut_fir_no$treatment == "seeded.scarified"])
        )
      )),
      surv = c(
        df_list$mm_scut_fir_no$surv_prop_0[df_list$mm_scut_fir_no$treatment == "seeded.scarified"],
        df_list$mm_scut_fir_no$surv_prop_1[df_list$mm_scut_fir_no$treatment == "seeded.scarified"],
        df_list$mm_scut_fir_no$surv_prop_2[df_list$mm_scut_fir_no$treatment == "seeded.scarified"],
        df_list$mm_scut_fir_no$surv_prop_3[df_list$mm_scut_fir_no$treatment == "seeded.scarified"],
        df_list$mm_scut_fir_no$surv_prop_4[df_list$mm_scut_fir_no$treatment == "seeded.scarified"],
        df_list$mm_scut_fir_no$surv_prop_5[df_list$mm_scut_fir_no$treatment == "seeded.scarified"]
      )
    )
  df_list$mm_scut_fir_ex_seedscar <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_scut_fir_ex$surv_prop_0[df_list$mm_scut_fir_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_scut_fir_ex$surv_prop_1[df_list$mm_scut_fir_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_scut_fir_ex$surv_prop_2[df_list$mm_scut_fir_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_scut_fir_ex$surv_prop_3[df_list$mm_scut_fir_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_scut_fir_ex$surv_prop_4[df_list$mm_scut_fir_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_scut_fir_ex$surv_prop_5[df_list$mm_scut_fir_ex$treatment == "seeded.scarified"])
        )
      )),
      surv = c(
        df_list$mm_scut_fir_ex$surv_prop_0[df_list$mm_scut_fir_ex$treatment == "seeded.scarified"],
        df_list$mm_scut_fir_ex$surv_prop_1[df_list$mm_scut_fir_ex$treatment == "seeded.scarified"],
        df_list$mm_scut_fir_ex$surv_prop_2[df_list$mm_scut_fir_ex$treatment == "seeded.scarified"],
        df_list$mm_scut_fir_ex$surv_prop_3[df_list$mm_scut_fir_ex$treatment == "seeded.scarified"],
        df_list$mm_scut_fir_ex$surv_prop_4[df_list$mm_scut_fir_ex$treatment == "seeded.scarified"],
        df_list$mm_scut_fir_ex$surv_prop_5[df_list$mm_scut_fir_ex$treatment == "seeded.scarified"]
      )
    )
  
  
  
  
  
  
  # df_list$mm_scut_fir_no_seed_mod <- lm(log1p(surv) ~ year, df_list$mm_scut_fir_no_seed)
  # df_list$mm_scut_fir_no_seed_ci <- data.frame(predict(df_list$mm_scut_fir_no_seed_mod, newdata=data.frame(year=seq(1,n_yr,0.1)), interval="confidence", level = 0.95))
  # df_list$mm_scut_fir_no_seed_ci$fit <- exp(df_list$mm_scut_fir_no_seed_ci$fit)-1
  # df_list$mm_scut_fir_no_seed_mean <- data.frame(year.vals = seq(1,n_yr,0.1),
  #                                        surv_mean = df_list$mm_scut_fir_no_seed_ci$fit) #only doing wells with 2015-2020 data
  
  df_list$mm_scut_fir_ex_seed_mod <-
    lm(log1p(surv) ~ year, df_list$mm_scut_fir_ex_seed)
  df_list$mm_scut_fir_ex_seed_ci <-
    data.frame(
      predict(
        df_list$mm_scut_fir_ex_seed_mod,
        newdata = data.frame(year = seq(1, n_yr, 0.1)),
        interval = "confidence",
        level = 0.95
      )
    )
  df_list$mm_scut_fir_ex_seed_ci$fit <-
    exp(df_list$mm_scut_fir_ex_seed_ci$fit) - 1
  df_list$mm_scut_fir_ex_seed_mean <-
    data.frame(
      year.vals = seq(1, n_yr, 0.1),
      surv_mean = df_list$mm_scut_fir_ex_seed_ci$fit
    ) #only doing wells with 2015-2020 data
  
  # df_list$mm_scut_fir_no_seedscar_mod <- lm(log1p(surv) ~ year, df_list$mm_scut_fir_no_seedscar)
  # df_list$mm_scut_fir_no_seedscar_ci <- data.frame(predict(df_list$mm_scut_fir_no_seedscar_mod, newdata=data.frame(year=seq(1,n_yr,0.1)), interval="confidence", level = 0.95))
  # df_list$mm_scut_fir_no_seedscar_ci$fit <- exp(df_list$mm_scut_fir_no_seedscar_ci$fit)-1
  # df_list$mm_scut_fir_no_seedscar_mean <- data.frame(year.vals = seq(1,n_yr,0.1),
  #                                            surv_mean = df_list$mm_scut_fir_no_seedscar_ci$fit) #only doing wells with 2015-2020 data
  
  df_list$mm_scut_fir_ex_seedscar_mod <-
    lm(log1p(surv) ~ year, df_list$mm_scut_fir_ex_seedscar)
  df_list$mm_scut_fir_ex_seedscar_ci <-
    data.frame(
      predict(
        df_list$mm_scut_fir_ex_seedscar_mod,
        newdata = data.frame(year = seq(1, n_yr, 0.1)),
        interval = "confidence",
        level = 0.95
      )
    )
  df_list$mm_scut_fir_ex_seedscar_ci$fit <-
    exp(df_list$mm_scut_fir_ex_seedscar_ci$fit) - 1
  df_list$mm_scut_fir_ex_seedscar_mean <-
    data.frame(
      year.vals = seq(1, n_yr, 0.1),
      surv_mean = df_list$mm_scut_fir_ex_seedscar_ci$fit
    ) #only doing wells with 2015-2020 data
  
  ## SSHR - fir
  df_list$mm_sshr_fir_no_seed <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_sshr_fir_no$surv_prop_0[df_list$mm_sshr_fir_no$treatment == "seeded"]),
          length(df_list$mm_sshr_fir_no$surv_prop_1[df_list$mm_sshr_fir_no$treatment == "seeded"]),
          length(df_list$mm_sshr_fir_no$surv_prop_2[df_list$mm_sshr_fir_no$treatment == "seeded"]),
          length(df_list$mm_sshr_fir_no$surv_prop_3[df_list$mm_sshr_fir_no$treatment == "seeded"]),
          length(df_list$mm_sshr_fir_no$surv_prop_4[df_list$mm_sshr_fir_no$treatment == "seeded"]),
          length(df_list$mm_sshr_fir_no$surv_prop_5[df_list$mm_sshr_fir_no$treatment == "seeded"])
        )
      )),
      surv = c(
        df_list$mm_sshr_fir_no$surv_prop_0[df_list$mm_sshr_fir_no$treatment == "seeded"],
        df_list$mm_sshr_fir_no$surv_prop_1[df_list$mm_sshr_fir_no$treatment == "seeded"],
        df_list$mm_sshr_fir_no$surv_prop_2[df_list$mm_sshr_fir_no$treatment == "seeded"],
        df_list$mm_sshr_fir_no$surv_prop_3[df_list$mm_sshr_fir_no$treatment == "seeded"],
        df_list$mm_sshr_fir_no$surv_prop_4[df_list$mm_sshr_fir_no$treatment == "seeded"],
        df_list$mm_sshr_fir_no$surv_prop_5[df_list$mm_sshr_fir_no$treatment == "seeded"]
      )
    )
  df_list$mm_sshr_fir_ex_seed <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_sshr_fir_ex$surv_prop_0[df_list$mm_sshr_fir_ex$treatment == "seeded"]),
          length(df_list$mm_sshr_fir_ex$surv_prop_1[df_list$mm_sshr_fir_ex$treatment == "seeded"]),
          length(df_list$mm_sshr_fir_ex$surv_prop_2[df_list$mm_sshr_fir_ex$treatment == "seeded"]),
          length(df_list$mm_sshr_fir_ex$surv_prop_3[df_list$mm_sshr_fir_ex$treatment == "seeded"]),
          length(df_list$mm_sshr_fir_ex$surv_prop_4[df_list$mm_sshr_fir_ex$treatment == "seeded"]),
          length(df_list$mm_sshr_fir_ex$surv_prop_5[df_list$mm_sshr_fir_ex$treatment == "seeded"])
        )
      )),
      surv = c(
        df_list$mm_sshr_fir_ex$surv_prop_0[df_list$mm_sshr_fir_ex$treatment == "seeded"],
        df_list$mm_sshr_fir_ex$surv_prop_1[df_list$mm_sshr_fir_ex$treatment == "seeded"],
        df_list$mm_sshr_fir_ex$surv_prop_2[df_list$mm_sshr_fir_ex$treatment == "seeded"],
        df_list$mm_sshr_fir_ex$surv_prop_3[df_list$mm_sshr_fir_ex$treatment == "seeded"],
        df_list$mm_sshr_fir_ex$surv_prop_4[df_list$mm_sshr_fir_ex$treatment == "seeded"],
        df_list$mm_sshr_fir_ex$surv_prop_5[df_list$mm_sshr_fir_ex$treatment == "seeded"]
      )
    )
  df_list$mm_sshr_fir_no_seedscar <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_sshr_fir_no$surv_prop_0[df_list$mm_sshr_fir_no$treatment == "seeded.scarified"]),
          length(df_list$mm_sshr_fir_no$surv_prop_1[df_list$mm_sshr_fir_no$treatment == "seeded.scarified"]),
          length(df_list$mm_sshr_fir_no$surv_prop_2[df_list$mm_sshr_fir_no$treatment == "seeded.scarified"]),
          length(df_list$mm_sshr_fir_no$surv_prop_3[df_list$mm_sshr_fir_no$treatment == "seeded.scarified"]),
          length(df_list$mm_sshr_fir_no$surv_prop_4[df_list$mm_sshr_fir_no$treatment == "seeded.scarified"]),
          length(df_list$mm_sshr_fir_no$surv_prop_5[df_list$mm_sshr_fir_no$treatment == "seeded.scarified"])
        )
      )),
      surv = c(
        df_list$mm_sshr_fir_no$surv_prop_0[df_list$mm_sshr_fir_no$treatment == "seeded.scarified"],
        df_list$mm_sshr_fir_no$surv_prop_1[df_list$mm_sshr_fir_no$treatment == "seeded.scarified"],
        df_list$mm_sshr_fir_no$surv_prop_2[df_list$mm_sshr_fir_no$treatment == "seeded.scarified"],
        df_list$mm_sshr_fir_no$surv_prop_3[df_list$mm_sshr_fir_no$treatment == "seeded.scarified"],
        df_list$mm_sshr_fir_no$surv_prop_4[df_list$mm_sshr_fir_no$treatment == "seeded.scarified"],
        df_list$mm_sshr_fir_no$surv_prop_5[df_list$mm_sshr_fir_no$treatment == "seeded.scarified"]
      )
    )
  df_list$mm_sshr_fir_ex_seedscar <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_sshr_fir_ex$surv_prop_0[df_list$mm_sshr_fir_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_sshr_fir_ex$surv_prop_1[df_list$mm_sshr_fir_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_sshr_fir_ex$surv_prop_2[df_list$mm_sshr_fir_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_sshr_fir_ex$surv_prop_3[df_list$mm_sshr_fir_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_sshr_fir_ex$surv_prop_4[df_list$mm_sshr_fir_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_sshr_fir_ex$surv_prop_5[df_list$mm_sshr_fir_ex$treatment == "seeded.scarified"])
        )
      )),
      surv = c(
        df_list$mm_sshr_fir_ex$surv_prop_0[df_list$mm_sshr_fir_ex$treatment == "seeded.scarified"],
        df_list$mm_sshr_fir_ex$surv_prop_1[df_list$mm_sshr_fir_ex$treatment == "seeded.scarified"],
        df_list$mm_sshr_fir_ex$surv_prop_2[df_list$mm_sshr_fir_ex$treatment == "seeded.scarified"],
        df_list$mm_sshr_fir_ex$surv_prop_3[df_list$mm_sshr_fir_ex$treatment == "seeded.scarified"],
        df_list$mm_sshr_fir_ex$surv_prop_4[df_list$mm_sshr_fir_ex$treatment == "seeded.scarified"],
        df_list$mm_sshr_fir_ex$surv_prop_5[df_list$mm_sshr_fir_ex$treatment == "seeded.scarified"]
      )
    )
  
  # df_list$mm_sshr_fir_no_seed_mod <- lm(log1p(surv) ~ year, df_list$mm_sshr_fir_no_seed)
  # df_list$mm_sshr_fir_no_seed_ci <- data.frame(predict(df_list$mm_sshr_fir_no_seed_mod, newdata=data.frame(year=seq(1,n_yr,0.1)), interval="confidence", level = 0.95))
  # df_list$mm_sshr_fir_no_seed_ci$fit <- exp(df_list$mm_sshr_fir_no_seed_ci$fit)-1
  # df_list$mm_sshr_fir_no_seed_mean <- data.frame(year.vals = seq(1,n_yr,0.1),
  #                                        surv_mean = df_list$mm_sshr_fir_no_seed_ci$fit) #only doing wells with 2015-2020 data
  
  df_list$mm_sshr_fir_ex_seed_mod <-
    lm(log1p(surv) ~ year, df_list$mm_sshr_fir_ex_seed)
  df_list$mm_sshr_fir_ex_seed_ci <-
    data.frame(
      predict(
        df_list$mm_sshr_fir_ex_seed_mod,
        newdata = data.frame(year = seq(1, n_yr, 0.1)),
        interval = "confidence",
        level = 0.95
      )
    )
  df_list$mm_sshr_fir_ex_seed_ci$fit <-
    exp(df_list$mm_sshr_fir_ex_seed_ci$fit) - 1
  df_list$mm_sshr_fir_ex_seed_mean <-
    data.frame(
      year.vals = seq(1, n_yr, 0.1),
      surv_mean = df_list$mm_sshr_fir_ex_seed_ci$fit
    ) #only doing wells with 2015-2020 data
  
  # df_list$mm_sshr_fir_no_seedscar_mod <- lm(log1p(surv) ~ year, df_list$mm_sshr_fir_no_seedscar)
  # df_list$mm_sshr_fir_no_seedscar_ci <- data.frame(predict(df_list$mm_sshr_fir_no_seedscar_mod, newdata=data.frame(year=seq(1,n_yr,0.1)), interval="confidence", level = 0.95))
  # df_list$mm_sshr_fir_no_seedscar_ci$fit <- exp(df_list$mm_sshr_fir_no_seedscar_ci$fit)-1
  # df_list$mm_sshr_fir_no_seedscar_mean <- data.frame(year.vals = seq(1,n_yr,0.1),
  #                                            surv_mean = df_list$mm_sshr_fir_no_seedscar_ci$fit) #only doing wells with 2015-2020 data
  
  df_list$mm_sshr_fir_ex_seedscar_mod <-
    lm(log1p(surv) ~ year, df_list$mm_sshr_fir_ex_seedscar)
  df_list$mm_sshr_fir_ex_seedscar_ci <-
    data.frame(
      predict(
        df_list$mm_sshr_fir_ex_seedscar_mod,
        newdata = data.frame(year = seq(1, n_yr, 0.1)),
        interval = "confidence",
        level = 0.95
      )
    )
  df_list$mm_sshr_fir_ex_seedscar_ci$fit <-
    exp(df_list$mm_sshr_fir_ex_seedscar_ci$fit) - 1
  df_list$mm_sshr_fir_ex_seedscar_mean <-
    data.frame(
      year.vals = seq(1, n_yr, 0.1),
      surv_mean = df_list$mm_sshr_fir_ex_seedscar_ci$fit
    ) #only doing wells with 2015-2020 data
  
  ## SCUT - spruce
  df_list$mm_scut_spruce_no_seed <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_scut_spruce_no$surv_prop_0[df_list$mm_scut_spruce_no$treatment == "seeded"]),
          length(df_list$mm_scut_spruce_no$surv_prop_1[df_list$mm_scut_spruce_no$treatment == "seeded"]),
          length(df_list$mm_scut_spruce_no$surv_prop_2[df_list$mm_scut_spruce_no$treatment == "seeded"]),
          length(df_list$mm_scut_spruce_no$surv_prop_3[df_list$mm_scut_spruce_no$treatment == "seeded"]),
          length(df_list$mm_scut_spruce_no$surv_prop_4[df_list$mm_scut_spruce_no$treatment == "seeded"]),
          length(df_list$mm_scut_spruce_no$surv_prop_5[df_list$mm_scut_spruce_no$treatment == "seeded"])
        )
      )),
      surv = c(
        df_list$mm_scut_spruce_no$surv_prop_0[df_list$mm_scut_spruce_no$treatment == "seeded"],
        df_list$mm_scut_spruce_no$surv_prop_1[df_list$mm_scut_spruce_no$treatment == "seeded"],
        df_list$mm_scut_spruce_no$surv_prop_2[df_list$mm_scut_spruce_no$treatment == "seeded"],
        df_list$mm_scut_spruce_no$surv_prop_3[df_list$mm_scut_spruce_no$treatment == "seeded"],
        df_list$mm_scut_spruce_no$surv_prop_4[df_list$mm_scut_spruce_no$treatment == "seeded"],
        df_list$mm_scut_spruce_no$surv_prop_5[df_list$mm_scut_spruce_no$treatment == "seeded"]
      )
    )
  df_list$mm_scut_spruce_ex_seed <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_scut_spruce_ex$surv_prop_0[df_list$mm_scut_spruce_ex$treatment == "seeded"]),
          length(df_list$mm_scut_spruce_ex$surv_prop_1[df_list$mm_scut_spruce_ex$treatment == "seeded"]),
          length(df_list$mm_scut_spruce_ex$surv_prop_2[df_list$mm_scut_spruce_ex$treatment == "seeded"]),
          length(df_list$mm_scut_spruce_ex$surv_prop_3[df_list$mm_scut_spruce_ex$treatment == "seeded"]),
          length(df_list$mm_scut_spruce_ex$surv_prop_4[df_list$mm_scut_spruce_ex$treatment == "seeded"]),
          length(df_list$mm_scut_spruce_ex$surv_prop_5[df_list$mm_scut_spruce_ex$treatment == "seeded"])
        )
      )),
      surv = c(
        df_list$mm_scut_spruce_ex$surv_prop_0[df_list$mm_scut_spruce_ex$treatment == "seeded"],
        df_list$mm_scut_spruce_ex$surv_prop_1[df_list$mm_scut_spruce_ex$treatment == "seeded"],
        df_list$mm_scut_spruce_ex$surv_prop_2[df_list$mm_scut_spruce_ex$treatment == "seeded"],
        df_list$mm_scut_spruce_ex$surv_prop_3[df_list$mm_scut_spruce_ex$treatment == "seeded"],
        df_list$mm_scut_spruce_ex$surv_prop_4[df_list$mm_scut_spruce_ex$treatment == "seeded"],
        df_list$mm_scut_spruce_ex$surv_prop_5[df_list$mm_scut_spruce_ex$treatment == "seeded"]
      )
    )
  df_list$mm_scut_spruce_no_seedscar <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_scut_spruce_no$surv_prop_0[df_list$mm_scut_spruce_no$treatment == "seeded.scarified"]),
          length(df_list$mm_scut_spruce_no$surv_prop_1[df_list$mm_scut_spruce_no$treatment == "seeded.scarified"]),
          length(df_list$mm_scut_spruce_no$surv_prop_2[df_list$mm_scut_spruce_no$treatment == "seeded.scarified"]),
          length(df_list$mm_scut_spruce_no$surv_prop_3[df_list$mm_scut_spruce_no$treatment == "seeded.scarified"]),
          length(df_list$mm_scut_spruce_no$surv_prop_4[df_list$mm_scut_spruce_no$treatment == "seeded.scarified"]),
          length(df_list$mm_scut_spruce_no$surv_prop_5[df_list$mm_scut_spruce_no$treatment == "seeded.scarified"])
        )
      )),
      surv = c(
        df_list$mm_scut_spruce_no$surv_prop_0[df_list$mm_scut_spruce_no$treatment == "seeded.scarified"],
        df_list$mm_scut_spruce_no$surv_prop_1[df_list$mm_scut_spruce_no$treatment == "seeded.scarified"],
        df_list$mm_scut_spruce_no$surv_prop_2[df_list$mm_scut_spruce_no$treatment == "seeded.scarified"],
        df_list$mm_scut_spruce_no$surv_prop_3[df_list$mm_scut_spruce_no$treatment == "seeded.scarified"],
        df_list$mm_scut_spruce_no$surv_prop_4[df_list$mm_scut_spruce_no$treatment == "seeded.scarified"],
        df_list$mm_scut_spruce_no$surv_prop_5[df_list$mm_scut_spruce_no$treatment == "seeded.scarified"]
      )
    )
  df_list$mm_scut_spruce_ex_seedscar <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_scut_spruce_ex$surv_prop_0[df_list$mm_scut_spruce_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_scut_spruce_ex$surv_prop_1[df_list$mm_scut_spruce_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_scut_spruce_ex$surv_prop_2[df_list$mm_scut_spruce_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_scut_spruce_ex$surv_prop_3[df_list$mm_scut_spruce_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_scut_spruce_ex$surv_prop_4[df_list$mm_scut_spruce_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_scut_spruce_ex$surv_prop_5[df_list$mm_scut_spruce_ex$treatment == "seeded.scarified"])
        )
      )),
      surv = c(
        df_list$mm_scut_spruce_ex$surv_prop_0[df_list$mm_scut_spruce_ex$treatment == "seeded.scarified"],
        df_list$mm_scut_spruce_ex$surv_prop_1[df_list$mm_scut_spruce_ex$treatment == "seeded.scarified"],
        df_list$mm_scut_spruce_ex$surv_prop_2[df_list$mm_scut_spruce_ex$treatment == "seeded.scarified"],
        df_list$mm_scut_spruce_ex$surv_prop_3[df_list$mm_scut_spruce_ex$treatment == "seeded.scarified"],
        df_list$mm_scut_spruce_ex$surv_prop_4[df_list$mm_scut_spruce_ex$treatment == "seeded.scarified"],
        df_list$mm_scut_spruce_ex$surv_prop_5[df_list$mm_scut_spruce_ex$treatment == "seeded.scarified"]
      )
    )
  
  
  # df_list$mm_scut_spruce_no_seed_mod <- lm(log1p(surv) ~ year, df_list$mm_scut_spruce_no_seed)
  # df_list$mm_scut_spruce_no_seed_ci <- data.frame(predict(df_list$mm_scut_spruce_no_seed_mod, newdata=data.frame(year=seq(1,n_yr,0.1)), interval="confidence", level = 0.95))
  # df_list$mm_scut_spruce_no_seed_ci$fit <- exp(df_list$mm_scut_spruce_no_seed_ci$fit)-1
  # df_list$mm_scut_spruce_no_seed_mean <- data.frame(year.vals = seq(1,n_yr,0.1),
  #                                           surv_mean = df_list$mm_scut_spruce_no_seed_ci$fit) #only doing wells with 2015-2020 data
  
  df_list$mm_scut_spruce_ex_seed_mod <-
    lm(log1p(surv) ~ year, df_list$mm_scut_spruce_ex_seed)
  df_list$mm_scut_spruce_ex_seed_ci <-
    data.frame(
      predict(
        df_list$mm_scut_spruce_ex_seed_mod,
        newdata = data.frame(year = seq(1, n_yr, 0.1)),
        interval = "confidence",
        level = 0.95
      )
    )
  df_list$mm_scut_spruce_ex_seed_ci$fit <-
    exp(df_list$mm_scut_spruce_ex_seed_ci$fit) - 1
  df_list$mm_scut_spruce_ex_seed_mean <-
    data.frame(
      year.vals = seq(1, n_yr, 0.1),
      surv_mean = df_list$mm_scut_spruce_ex_seed_ci$fit
    ) #only doing wells with 2015-2020 data
  
  # df_list$mm_scut_spruce_no_seedscar_mod <- lm(log1p(surv) ~ year, df_list$mm_scut_spruce_no_seedscar)
  # df_list$mm_scut_spruce_no_seedscar_ci <- data.frame(predict(df_list$mm_scut_spruce_no_seedscar_mod, newdata=data.frame(year=seq(1,n_yr,0.1)), interval="confidence", level = 0.95))
  # df_list$mm_scut_spruce_no_seedscar_ci$fit <- exp(df_list$mm_scut_spruce_no_seedscar_ci$fit)-1
  # df_list$mm_scut_spruce_no_seedscar_mean <- data.frame(year.vals = seq(1,n_yr,0.1),
  #                                               surv_mean = df_list$mm_scut_spruce_no_seedscar_ci$fit) #only doing wells with 2015-2020 data
  
  df_list$mm_scut_spruce_ex_seedscar_mod <-
    lm(log1p(surv) ~ year, df_list$mm_scut_spruce_ex_seedscar)
  df_list$mm_scut_spruce_ex_seedscar_ci <-
    data.frame(
      predict(
        df_list$mm_scut_spruce_ex_seedscar_mod,
        newdata = data.frame(year = seq(1, n_yr, 0.1)),
        interval = "confidence",
        level = 0.95
      )
    )
  df_list$mm_scut_spruce_ex_seedscar_ci$fit <-
    exp(df_list$mm_scut_spruce_ex_seedscar_ci$fit) - 1
  df_list$mm_scut_spruce_ex_seedscar_mean <-
    data.frame(
      year.vals = seq(1, n_yr, 0.1),
      surv_mean = df_list$mm_scut_spruce_ex_seedscar_ci$fit
    ) #only doing wells with 2015-2020 data
  
  ## SSHR - spruce
  df_list$mm_sshr_spruce_no_seed <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_sshr_spruce_no$surv_prop_0[df_list$mm_sshr_spruce_no$treatment == "seeded"]),
          length(df_list$mm_sshr_spruce_no$surv_prop_1[df_list$mm_sshr_spruce_no$treatment == "seeded"]),
          length(df_list$mm_sshr_spruce_no$surv_prop_2[df_list$mm_sshr_spruce_no$treatment == "seeded"]),
          length(df_list$mm_sshr_spruce_no$surv_prop_3[df_list$mm_sshr_spruce_no$treatment == "seeded"]),
          length(df_list$mm_sshr_spruce_no$surv_prop_4[df_list$mm_sshr_spruce_no$treatment == "seeded"]),
          length(df_list$mm_sshr_spruce_no$surv_prop_5[df_list$mm_sshr_spruce_no$treatment == "seeded"])
        )
      )),
      surv = c(
        df_list$mm_sshr_spruce_no$surv_prop_0[df_list$mm_sshr_spruce_no$treatment == "seeded"],
        df_list$mm_sshr_spruce_no$surv_prop_1[df_list$mm_sshr_spruce_no$treatment == "seeded"],
        df_list$mm_sshr_spruce_no$surv_prop_2[df_list$mm_sshr_spruce_no$treatment == "seeded"],
        df_list$mm_sshr_spruce_no$surv_prop_3[df_list$mm_sshr_spruce_no$treatment == "seeded"],
        df_list$mm_sshr_spruce_no$surv_prop_4[df_list$mm_sshr_spruce_no$treatment == "seeded"],
        df_list$mm_sshr_spruce_no$surv_prop_5[df_list$mm_sshr_spruce_no$treatment == "seeded"]
      )
    )
  df_list$mm_sshr_spruce_ex_seed <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_sshr_spruce_ex$surv_prop_0[df_list$mm_sshr_spruce_ex$treatment == "seeded"]),
          length(df_list$mm_sshr_spruce_ex$surv_prop_1[df_list$mm_sshr_spruce_ex$treatment == "seeded"]),
          length(df_list$mm_sshr_spruce_ex$surv_prop_2[df_list$mm_sshr_spruce_ex$treatment == "seeded"]),
          length(df_list$mm_sshr_spruce_ex$surv_prop_3[df_list$mm_sshr_spruce_ex$treatment == "seeded"]),
          length(df_list$mm_sshr_spruce_ex$surv_prop_4[df_list$mm_sshr_spruce_ex$treatment == "seeded"]),
          length(df_list$mm_sshr_spruce_ex$surv_prop_5[df_list$mm_sshr_spruce_ex$treatment == "seeded"])
        )
      )),
      surv = c(
        df_list$mm_sshr_spruce_ex$surv_prop_0[df_list$mm_sshr_spruce_ex$treatment == "seeded"],
        df_list$mm_sshr_spruce_ex$surv_prop_1[df_list$mm_sshr_spruce_ex$treatment == "seeded"],
        df_list$mm_sshr_spruce_ex$surv_prop_2[df_list$mm_sshr_spruce_ex$treatment == "seeded"],
        df_list$mm_sshr_spruce_ex$surv_prop_3[df_list$mm_sshr_spruce_ex$treatment == "seeded"],
        df_list$mm_sshr_spruce_ex$surv_prop_4[df_list$mm_sshr_spruce_ex$treatment == "seeded"],
        df_list$mm_sshr_spruce_ex$surv_prop_5[df_list$mm_sshr_spruce_ex$treatment == "seeded"]
      )
    )
  df_list$mm_sshr_spruce_no_seedscar <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_sshr_spruce_no$surv_prop_0[df_list$mm_sshr_spruce_no$treatment == "seeded.scarified"]),
          length(df_list$mm_sshr_spruce_no$surv_prop_1[df_list$mm_sshr_spruce_no$treatment == "seeded.scarified"]),
          length(df_list$mm_sshr_spruce_no$surv_prop_2[df_list$mm_sshr_spruce_no$treatment == "seeded.scarified"]),
          length(df_list$mm_sshr_spruce_no$surv_prop_3[df_list$mm_sshr_spruce_no$treatment == "seeded.scarified"]),
          length(df_list$mm_sshr_spruce_no$surv_prop_4[df_list$mm_sshr_spruce_no$treatment == "seeded.scarified"]),
          length(df_list$mm_sshr_spruce_no$surv_prop_5[df_list$mm_sshr_spruce_no$treatment == "seeded.scarified"])
        )
      )),
      surv = c(
        df_list$mm_sshr_spruce_no$surv_prop_0[df_list$mm_sshr_spruce_no$treatment == "seeded.scarified"],
        df_list$mm_sshr_spruce_no$surv_prop_1[df_list$mm_sshr_spruce_no$treatment == "seeded.scarified"],
        df_list$mm_sshr_spruce_no$surv_prop_2[df_list$mm_sshr_spruce_no$treatment == "seeded.scarified"],
        df_list$mm_sshr_spruce_no$surv_prop_3[df_list$mm_sshr_spruce_no$treatment == "seeded.scarified"],
        df_list$mm_sshr_spruce_no$surv_prop_4[df_list$mm_sshr_spruce_no$treatment == "seeded.scarified"],
        df_list$mm_sshr_spruce_no$surv_prop_5[df_list$mm_sshr_spruce_no$treatment == "seeded.scarified"]
      )
    )
  df_list$mm_sshr_spruce_ex_seedscar <-
    data.frame(
      year = (rep(
        c(1:n_yr),
        c(
          length(df_list$mm_sshr_spruce_ex$surv_prop_0[df_list$mm_sshr_spruce_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_sshr_spruce_ex$surv_prop_1[df_list$mm_sshr_spruce_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_sshr_spruce_ex$surv_prop_2[df_list$mm_sshr_spruce_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_sshr_spruce_ex$surv_prop_3[df_list$mm_sshr_spruce_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_sshr_spruce_ex$surv_prop_4[df_list$mm_sshr_spruce_ex$treatment == "seeded.scarified"]),
          length(df_list$mm_sshr_spruce_ex$surv_prop_5[df_list$mm_sshr_spruce_ex$treatment == "seeded.scarified"])
        )
      )),
      surv = c(
        df_list$mm_sshr_spruce_ex$surv_prop_0[df_list$mm_sshr_spruce_ex$treatment == "seeded.scarified"],
        df_list$mm_sshr_spruce_ex$surv_prop_1[df_list$mm_sshr_spruce_ex$treatment == "seeded.scarified"],
        df_list$mm_sshr_spruce_ex$surv_prop_2[df_list$mm_sshr_spruce_ex$treatment == "seeded.scarified"],
        df_list$mm_sshr_spruce_ex$surv_prop_3[df_list$mm_sshr_spruce_ex$treatment == "seeded.scarified"],
        df_list$mm_sshr_spruce_ex$surv_prop_4[df_list$mm_sshr_spruce_ex$treatment == "seeded.scarified"],
        df_list$mm_sshr_spruce_ex$surv_prop_5[df_list$mm_sshr_spruce_ex$treatment == "seeded.scarified"]
      )
    )
  
  
  # mm_sshr_spruce_no_seed_mod <- lm(log1p(surv) ~ year, mm_sshr_spruce_no_seed)
  # mm_sshr_spruce_no_seed_ci <- data.frame(predict(mm_sshr_spruce_no_seed_mod, newdata=data.frame(year=seq(1,n_yr,0.1)), interval="confidence", level = 0.95))
  # mm_sshr_spruce_no_seed_ci$fit <- exp(mm_sshr_spruce_no_seed_ci$fit)-1
  # mm_sshr_spruce_no_seed_mean <- data.frame(year.vals = seq(1,n_yr,0.1),
  #                                        surv_mean = mm_sshr_spruce_no_seed_ci$fit) #only doing wells with 2015-2020 data
  
  df_list$mm_sshr_spruce_ex_seed_mod <-
    lm(log1p(surv) ~ year, df_list$mm_sshr_spruce_ex_seed)
  df_list$mm_sshr_spruce_ex_seed_ci <-
    data.frame(
      predict(
        df_list$mm_sshr_spruce_ex_seed_mod,
        newdata = data.frame(year = seq(1, n_yr, 0.1)),
        interval = "confidence",
        level = 0.95
      )
    )
  df_list$mm_sshr_spruce_ex_seed_ci$fit <-
    exp(df_list$mm_sshr_spruce_ex_seed_ci$fit) - 1
  df_list$mm_sshr_spruce_ex_seed_mean <-
    data.frame(
      year.vals = seq(1, n_yr, 0.1),
      surv_mean = df_list$mm_sshr_spruce_ex_seed_ci$fit
    ) #only doing wells with 2015-2020 data
  #
  # df_list$mm_sshr_spruce_no_seedscar_mod <- lm(log1p(surv) ~ year, df_list$mm_sshr_spruce_no_seedscar)
  # df_list$mm_sshr_spruce_no_seedscar_ci <- data.frame(predict(df_list$mm_sshr_spruce_no_seedscar_mod, newdata=data.frame(year=seq(1,n_yr,0.1)), interval="confidence", level = 0.95))
  # df_list$mm_sshr_spruce_no_seedscar_ci$fit <- exp(df_list$mm_sshr_spruce_no_seedscar_ci$fit)-1
  # df_list$mm_sshr_spruce_no_seedscar_mean <- data.frame(year.vals = seq(1,n_yr,0.1),
  #                                               surv_mean = df_list$mm_sshr_spruce_no_seedscar_ci$fit) #only doing wells with 2015-2020 data
  #
  df_list$mm_sshr_spruce_ex_seedscar_mod <-
    lm(log1p(surv) ~ year, df_list$mm_sshr_spruce_ex_seedscar)
  df_list$mm_sshr_spruce_ex_seedscar_ci <-
    data.frame(
      predict(
        df_list$mm_sshr_spruce_ex_seedscar_mod,
        newdata = data.frame(year = seq(1, n_yr, 0.1)),
        interval = "confidence",
        level = 0.95
      )
    )
  df_list$mm_sshr_spruce_ex_seedscar_ci$fit <-
    exp(df_list$mm_sshr_spruce_ex_seedscar_ci$fit) - 1
  df_list$mm_sshr_spruce_ex_seedscar_mean <-
    data.frame(
      year.vals = seq(1, n_yr, 0.1),
      surv_mean = df_list$mm_sshr_spruce_ex_seedscar_ci$fit
    ) #only doing wells with 2015-2020 data
  
  
  return(df_list)
  
}


# Zero-inflated modeling (archived)

# ##***************
# ## Models (need to be updated: 2021-08-27)
# f1 <- formula(germ_prop_via_int ~ treat * sow_year + exclosure | treat * sow_year + exclosure)
# f1A <- formula(germ_prop_via_int ~ treat * exclosure | treat * exclosure)
# f1B <- formula(germ_prop_via_int ~ treat + exclosure | treat + exclosure)
# f1C <- formula(germ_prop_via_int ~ treat | treat)
# 
# ## Fir
# # Nalp ZINBs
# nalp_fir_Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = mm_nalp_fir_seed)
# nalp_fir_Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = mm_nalp_fir_seed)
# nalp_fir_Nb1A <- zeroinfl(f1A, dist = "negbin", link = "logit", data = mm_nalp_fir_seed)
# nalp_fir_Nb1B <- zeroinfl(f1B, dist = "negbin", link = "logit", data = mm_nalp_fir_seed)
# nalp_fir_Nb1C <- zeroinfl(f1C, dist = "negbin", link = "logit", data = mm_nalp_fir_seed)
# lrtest(nalp_fir_Zip1,nalp_fir_Nb1) # nb model is better than Poisson
# lrtest(nalp_fir_Nb1,nalp_fir_Nb1A) # no diff
# lrtest(nalp_fir_Nb1,nalp_fir_Nb1B) # no diff
# lrtest(nalp_fir_Nb1,nalp_fir_Nb1C) # no diff
# AIC(nalp_fir_Nb1,nalp_fir_Nb1A,nalp_fir_Nb1B,nalp_fir_Nb1C) # Model C has lowest AIC
# summary(nalp_fir_Nb1A)
# summary(nalp_fir_Nb1B)
# summary(nalp_fir_Nb1C) # Treatment is significant (P = 0.00692)
# 
# # Salp ZINBs
# salp_fir_Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = mm_salp_fir_seed)
# salp_fir_Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = mm_salp_fir_seed)
# salp_fir_Nb1A <- zeroinfl(f1A, dist = "negbin", link = "logit", data = mm_salp_fir_seed)
# salp_fir_Nb1B <- zeroinfl(f1B, dist = "negbin", link = "logit", data = mm_salp_fir_seed)
# salp_fir_Nb1C <- zeroinfl(f1C, dist = "negbin", link = "logit", data = mm_salp_fir_seed)
# lrtest(salp_fir_Zip1,salp_fir_Nb1) # nb model is better
# lrtest(salp_fir_Nb1,salp_fir_Nb1A) # A is better than the full
# lrtest(salp_fir_Nb1,salp_fir_Nb1B) # B is better than the full
# lrtest(salp_fir_Nb1,salp_fir_Nb1C) # C is better than the full
# AIC(salp_fir_Nb1,salp_fir_Nb1A,salp_fir_Nb1B,salp_fir_Nb1C) # Model C has lowest AIC
# summary(salp_fir_Nb1)
# summary(salp_fir_Nb1A)
# summary(salp_fir_Nb1B)
# summary(salp_fir_Nb1C) # Treatment is significant (P = 0.00037)
# 
# # Scut ZINBs
# scut_fir_Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = mm_scut_fir_seed) # Computationally singular
# scut_fir_Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = mm_scut_fir_seed)
# scut_fir_Nb1A <- zeroinfl(f1A, dist = "negbin", link = "logit", data = mm_scut_fir_seed) # Computationally singular
# scut_fir_Nb1B <- zeroinfl(f1B, dist = "negbin", link = "logit", data = mm_scut_fir_seed)
# scut_fir_Nb1C <- zeroinfl(f1C, dist = "negbin", link = "logit", data = mm_scut_fir_seed)
# lrtest(scut_fir_Zip1,scut_fir_Nb1)
# lrtest(scut_fir_Nb1,scut_fir_Nb1A)
# lrtest(scut_fir_Nb1,scut_fir_Nb1B)
# lrtest(scut_fir_Nb1,scut_fir_Nb1C)
# AIC(scut_fir_Nb1,scut_fir_Nb1B,scut_fir_Nb1C) # Model B has lowest AIC
# summary(scut_fir_Nb1)
# summary(scut_fir_Nb1B) # Treatment (P = 0.00073) and exclosure (P = 0.00073) are significant
# summary(scut_fir_Nb1C)
# 
# # Sshr ZINBs
# sshr_fir_Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = mm_sshr_fir_seed) # Computationally singular
# sshr_fir_Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = mm_sshr_fir_seed)   # Computationally singular
# sshr_fir_Nb1A <- zeroinfl(f1A, dist = "negbin", link = "logit", data = mm_sshr_fir_seed) # Computationally singular
# sshr_fir_Nb1B <- zeroinfl(f1B, dist = "negbin", link = "logit", data = mm_sshr_fir_seed) # Computationally singular
# sshr_fir_Nb1C <- zeroinfl(f1C, dist = "negbin", link = "logit", data = mm_sshr_fir_seed) # Computationally singular
# AIC(sshr_fir_Nb1C)
# summary(sshr_fir_Nb1C) # Treatment is significant (P = 3.27e-05)
# 
# 
# ## Spruce
# # Nalp ZINBs
# nalp_spruce_Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = mm_nalp_spruce_seed)
# nalp_spruce_Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = mm_nalp_spruce_seed)
# nalp_spruce_Nb1A <- zeroinfl(f1A, dist = "negbin", link = "logit", data = mm_nalp_spruce_seed)
# nalp_spruce_Nb1B <- zeroinfl(f1B, dist = "negbin", link = "logit", data = mm_nalp_spruce_seed)
# nalp_spruce_Nb1C <- zeroinfl(f1C, dist = "negbin", link = "logit", data = mm_nalp_spruce_seed)
# lrtest(nalp_spruce_Zip1,nalp_spruce_Nb1) # nb model is better than Poisson
# lrtest(nalp_spruce_Nb1,nalp_spruce_Nb1A) # no diff
# lrtest(nalp_spruce_Nb1,nalp_spruce_Nb1B) # no diff
# lrtest(nalp_spruce_Nb1,nalp_spruce_Nb1C) # no diff
# AIC(nalp_spruce_Nb1,nalp_spruce_Nb1A,nalp_spruce_Nb1B,nalp_spruce_Nb1C) # Model C has lowest AIC
# summary(nalp_spruce_Nb1)
# summary(nalp_spruce_Nb1A)
# summary(nalp_spruce_Nb1B)
# summary(nalp_spruce_Nb1C) # Treatment is ~significant (P = 0.0569)
# 
# # Salp ZINBs
# salp_spruce_Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = mm_salp_spruce_seed)
# salp_spruce_Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = mm_salp_spruce_seed) # Computationally singular
# salp_spruce_Nb1A <- zeroinfl(f1A, dist = "negbin", link = "logit", data = mm_salp_spruce_seed)
# salp_spruce_Nb1B <- zeroinfl(f1B, dist = "negbin", link = "logit", data = mm_salp_spruce_seed)
# salp_spruce_Nb1C <- zeroinfl(f1C, dist = "negbin", link = "logit", data = mm_salp_spruce_seed)
# # lrtest(salp_spruce_Zip1,salp_spruce_Nb1) # nb model is better
# # lrtest(salp_spruce_Nb1,salp_spruce_Nb1A) # no diff
# # lrtest(salp_spruce_Nb1,salp_spruce_Nb1B) # no diff
# # lrtest(salp_spruce_Nb1,salp_spruce_Nb1C) # no diff
# AIC(salp_spruce_Nb1A,salp_spruce_Nb1B,salp_spruce_Nb1C) # Model B has lowest AIC
# # summary(salp_spruce_Nb1)
# summary(salp_spruce_Nb1A)
# summary(salp_spruce_Nb1B) # Treatment (P = 0.0174) and exclosure (P = 3.67e-05) are significant
# summary(salp_spruce_Nb1C)
# 
# # Scut ZINBs
# scut_spruce_Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = mm_scut_spruce_seed) # Computationally singular
# scut_spruce_Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = mm_scut_spruce_seed) # Computationally singular
# scut_spruce_Nb1A <- zeroinfl(f1A, dist = "negbin", link = "logit", data = mm_scut_spruce_seed)  # Computationally singular
# scut_spruce_Nb1B <- zeroinfl(f1B, dist = "negbin", link = "logit", data = mm_scut_spruce_seed)
# scut_spruce_Nb1C <- zeroinfl(f1C, dist = "negbin", link = "logit", data = mm_scut_spruce_seed)
# # lrtest(scut_spruce_Zip1,scut_spruce_Nb1) # nb model is better
# # lrtest(scut_spruce_Nb1,scut_spruce_Nb1A) # no diff
# # lrtest(scut_spruce_Nb1,scut_spruce_Nb1B) # no diff
# # lrtest(scut_spruce_Nb1,scut_spruce_Nb1C) # no diff
# AIC(scut_spruce_Nb1B,scut_spruce_Nb1C) # Model B has lowest AIC
# # summary(scut_spruce_Nb1)
# # summary(scut_spruce_Nb1A)
# summary(scut_spruce_Nb1B) # Treatment (P  = 7.79e-05) and exclosure (P = 7.79e-05) are significant
# summary(scut_spruce_Nb1C)
# 
# # Sshr ZINBs
# sshr_spruce_Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = mm_sshr_spruce_seed)
# sshr_spruce_Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = mm_sshr_spruce_seed)  
# sshr_spruce_Nb1A <- zeroinfl(f1A, dist = "negbin", link = "logit", data = mm_sshr_spruce_seed)
# sshr_spruce_Nb1B <- zeroinfl(f1B, dist = "negbin", link = "logit", data = mm_sshr_spruce_seed)
# sshr_spruce_Nb1C <- zeroinfl(f1C, dist = "negbin", link = "logit", data = mm_sshr_spruce_seed)
# lrtest(sshr_spruce_Zip1,sshr_spruce_Nb1) # nb model is better
# lrtest(sshr_spruce_Nb1,sshr_spruce_Nb1A) # no diff
# lrtest(sshr_spruce_Nb1,sshr_spruce_Nb1B) # no diff
# lrtest(sshr_spruce_Nb1,sshr_spruce_Nb1C) # C is better than full
# AIC(sshr_spruce_Nb1, sshr_spruce_Nb1A, sshr_spruce_Nb1B, sshr_spruce_Nb1C) # Model B has lowest AIC
# summary(sshr_spruce_Nb1)
# summary(sshr_spruce_Nb1A)
# summary(sshr_spruce_Nb1B) # Exclosure is significant (P = 0.000147)
# summary(sshr_spruce_Nb1C)

