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

