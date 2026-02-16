##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
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
package_vec <- c("here", "sf", "dplyr", "ggplot2", "viridis", "ggspatial",
                 "scales", "terra", "geodata", "CoordinateCleaner", "cowplot",
                 "stringr", "tidyr", "tidyterra", "readxl", "rgbif", "glmmTMB",
                 "DHARMa", "emmeans", "ggeffects", "broom.mixed", "bbmle")

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

# END OF SCRIPT ----------------------------------------------------------------