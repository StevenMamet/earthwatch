library(tidyverse)

setwd("/Users/sdmamet/Desktop/Workspace/earthwatch")
source("./R_functions/R_functions.R")

# Set the path containing the photos
input_path <- "/Users/sdmamet/Library/CloudStorage/Dropbox/School/Postdoc/Earthwatch/Churchill/Parks/Trail cams/Mary/100IMAGE"
time_unit <- "11:58|11:59"

move_pics(input_path, time_unit)
