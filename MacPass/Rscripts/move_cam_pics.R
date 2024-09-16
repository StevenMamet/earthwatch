library(tidyverse)
library(exifr)
# library(dplyr)

setwd("/Users/sdmamet/Desktop/Workspace/earthwatch")
source("./R_functions/R_functions.R")

#_____________________________________----
# Moving photos ----

# Set the path containing the photos
input_path <- "/Users/Fuck"

# time_unit <- "11:58|11:59"
time_unit <- "12:00"

move_pics(input_path, time_unit)

#_____________________________________----
# Renaming photos ----

# Set the path to your folder with trail camera photos
# folder_path <- "/Users/sdmamet/Library/CloudStorage/Dropbox/School/Postdoc/Earthwatch/Mackenzie Mountains/TrailCams/GF/GF20240818/101RECNX/testing/"
# folder_path <- "/Users/sdmamet/Library/CloudStorage/Dropbox/School/Postdoc/Earthwatch/Mackenzie Mountains/TrailCams/HF/20240824/testing/"
folder_path <- "/Users/Fuck/testing"

# Read all EXIF data from the photos in the folder (including subfolders)
exif_data <- read_exif(folder_path, recursive = TRUE)

# Extract only the necessary columns: SourceFile (file path) and DateTimeOriginal (the date taken)
exif_data <- exif_data %>%
  select(SourceFile, DateTimeOriginal) %>%
  mutate(
    # Parse the DateTimeOriginal to a date-time format using lubridate
    new_name = format(ymd_hms(DateTimeOriginal), "%Y%m%d"),
    # Create the new file name by appending the ".jpg" extension
    new_file = paste0(new_name, ".jpg")
  )

# Rename the files
file.rename(exif_data$SourceFile, file.path(folder_path, exif_data$new_file))

# Optional: Print the renamed files for confirmation
print(exif_data %>% select(SourceFile, new_file))
