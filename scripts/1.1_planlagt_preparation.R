##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 1.1_planlagt_preparation
# This script contains code which downloads and prepares the "Planned 
# development area in Norway" dataset from NINA
##----------------------------------------------------------------------------##

# 1. DOWNLOAD AREA PLAN DATASET ------------------------------------------------

# Source setup file
library(here)
source(here("scripts", "0_setup.R"))

# Dataset was originally downloaded on 28.10.2025 from https://maps.nina.no/planlagt-utbyggingsareal-i-norge
  # then uploaded to Google Drive for easier download in the R project

# Authenticate with Google - will open a new browser window
drive_auth()
# When running this for the first time:
# 1. New browser window will open
# 2. You will be asked to sign in to your Google account (you will need one)
# 3. You will be asked to give permission to the googledrive package
# 4. You can close the window after you approve
# 5. A success message should appear in R

# Check that authentication worked
drive_user() 

# Give file ID
file_id <- "1r2neRGS27f0OE9gr3Pd8-Ff3DMHaNdPj"

# Download file from drive
drive_download(file = as_id(file_id),
               path = here("data", "raw_data", "nina_planagt.gpkg"),  
  overwrite = FALSE)

# 2. INSPECT DATASET -----------------------------------------------------------

# Read in data
development_polygons <- st_read(here("data", "nina_planagt.gpkg"))

# Check number of polygons
nrow(development_polygons)

# Check CRS
st_crs(development_polygons)$input

# Check column namnes
colnames(development_polygons)

# Check broad strucutre of dataset
glimpse(development_polygons)

# END OF SCRIPT ----------------------------------------------------------------