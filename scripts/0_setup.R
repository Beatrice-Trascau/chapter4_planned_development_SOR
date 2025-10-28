##----------------------------------------------------------------------------##
# PAPER 4: PLANNED AREA DEVELOPMENTS AND SPECIES OCCURRENCE RECORDS
# 0_setup
# This script contains code which loads/installs necessary packages and defines
# functions used in the analysis
##----------------------------------------------------------------------------##

# 1. LOAD/INSTALL PACKAGES NEEDED FOR ANALYIS ----------------------------------

# Function to check to install/load packages

# Define function
install_load_package <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, repos = "http://cran.us.r-project.org")
  }
  require(x, character.only = TRUE)
}

# Define list of packages
package_vec <- c("here", "terra", "sf", "geodata", "mapview",
                 "tidyverse", "dplyr", "ggplot2", "gt", "cowplot", 
                 "data.table","tidyterra", "patchwork", "styler", 
                 "scales","plotly", "lme4", "DHARMa", "glmmTMB", 
                 "mgcv", "ggspatial", "htmlwidgets","htmltools",  
                 "webshot2", "rgbif", "CoordinateCleaner", "codyn",
                 "gratia", "lattice", "car", "kableExtra",
                 "betareg", "spdep", "corrplot", "leaflet",
                 "viridis", "DT", "broom", "nlme", "ordbetareg")

# Execute the function
sapply(package_vec, install_load_package)

# 2. CREATE NECESSARY FILE STRUCTURE -------------------------------------------

# Set working directory
library(here)

create_project_structure <- function() {
  # define the subdirectories at the project root
  dirs <- c(here("data"),
            here("scripts"),
            here("figures"),
            here("data", "raw_data"),
            here("data", "derived_data"))
  
  # create directories if they don't exist
  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      cat("Created directory:", dir, "\n")
    } else {
      cat("Directory already exists:", dir, "\n")
    }
  }
  
  cat("\nProject structure setup complete!\n")
}

# Run the function
create_project_structure()

# 3. FUNCTION TO DOWNLOAD FILES FROM GOOGLE DRIVE ------------------------------

# Authenticate with Google - will open a new browser window
#drive_auth()
# When running this for the first time:
# 1. New browser window will open
# 2. You will be asked to sign in to your Google account (you will need one)
# 3. You will be asked to give permission to the googledrive package
# 4. You can close the window after you approve
# 5. A success message should appear in R

# Check that authentication worked
#drive_user() # this should show your google account info

# The function will only download the files that are not already in the folders
# download_gdrive_files <- function(file_ids, filenames, dir = here("data", "raw_data")) {
#   if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
#   
#   # Authenticate (will use cached credentials after first time)
#   drive_auth()
#   
#   for (i in seq_along(file_ids)) {
#     file_path <- file.path(dir, filenames[i])
#     
#     if (!file.exists(file_path)) {
#       cat("Downloading:", filenames[i], "to", dir, "\n")
#       drive_download(as_id(file_ids[i]), path = file_path, overwrite = FALSE)
#       
#       # Check file size to confirm successful download
#       file_size <- file.size(file_path)
#       cat("Downloaded", round(file_size/1024/1024, 1), "MB\n")
#     } else {
#       cat("File already exists:", filenames[i], "\n")
#     }
#   }
# }

# END OF SCRIPT ----------------------------------------------------------------