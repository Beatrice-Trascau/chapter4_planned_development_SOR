##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 2.1_GBIF_download
# This script contains code which downloads the GBIF Occurrence records used in
# the analysis
##----------------------------------------------------------------------------##

# 1. CONNECT TO GBIF -----------------------------------------------------------

# Setting username, password and email 
Sys.setenv(GBIF_USER = "beatrice_trasc.au") # change with your details
Sys.setenv(GBIF_PWD = "I97zB%$z")
Sys.setenv(GBIF_EMAIL = "beatrice.m.trascau@ntnu.no")

# 2. CREATE DOWNLOAD REQUEST ---------------------------------------------------

# Send download request
download_key <- occ_download(pred("gadm", "NOR"), # Norway
                             pred_gte("year", 2008), # greater than or equal to year 2008
                             pred_lte("year", 2024), # lower than or equal to 2024
                             pred("hasCoordinate", TRUE), 
                             format = "DWCA") # download as a Darwin Core Archive file

# Check progress
occ_download_wait(download_key)

# Download key: 0004619-251025141854904 
# Download link: https://api.gbif.org/v1/occurrence/download/request/0004619-251025141854904.zip
# DOI: 10.15468/dl.ykqkyn

# 3. IMPORT GBIF DOWNLOAD AND SAVE ---------------------------------------------

# Import data
occurrence <- occ_download_get(download_key) |>
  occ_download_import()

# Save to file
save(occurrence, file = here::here("data","raw_data",
                                   "occurrences_chapter4_October2025.txt"))