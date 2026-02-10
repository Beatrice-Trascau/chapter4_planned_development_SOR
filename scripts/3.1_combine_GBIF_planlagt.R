##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 3.1_combine_GBIF_planlagt
# This script contains code which adds the species occurrence records to the
# polygons of planned development
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source setup file
library(here)
source(here("scripts", "0_setup.R"))

# Load development polygons
development_polygons <- st_read(here("data", "raw_data", "nina_planagt.gpkg"))

# Load cleaned occurrence records
clean_occurrences_15km <- read.csv(here("data", "derived_data",
                                        "clean_occurrences_15km.txt"))

# 2. PREPARE SPATIAL DATA ------------------------------------------------------

# Convert occurrences to spatial points
occurrences_sf <- clean_occurrences_15km |>
  # remove occurrences with NA for either longitude or latitude
  filter(!is.na(decimalLongitude),
         !is.na(decimalLatitude)) |>
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = 4326)

# Transform occurrences to match the CRS of the polygons
occurrences_sf_crs <- st_transform(occurrences_sf, st_crs(development_polygons))

# Check that the CRSs match
stopifnot(st_crs(development_polygons) == st_crs(occurrence_sf))





