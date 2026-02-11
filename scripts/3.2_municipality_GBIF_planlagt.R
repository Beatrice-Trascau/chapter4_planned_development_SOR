##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 3.2_municipality_GBIF_planlagt
# This script contains code which creates a map of Norway where each 
# municipality is coloured by the percentage of total species occurrence 
# records (SOR) that fall within planned development polygons
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source setup file
library(here)
source(here("scripts", "0_setup.R"))

# Load cleaned occurrence records
clean_occurrences_15km <- read.csv(here("data", "derived_data",
                                        "clean_occurrences_15km.txt"))

# Load polygon dataframe
polygon_all_data <- readRDS(here("data", "derived_data",
                                 "polygons_occurrences_all_data.rds"))

# Load municipality boundaries downloaded from GeoNorge
norway_municipalities_sf <- st_read(here("data", "raw_data",
                                         "Basisdata_0000_Norge_25833_Kommune_GeoJSON.geojson"))


# 2. PREPARE SPATIAL  DATA -----------------------------------------------------

## 2.1. Prepare municipality boundaries ----------------------------------------

# Define CRS to use throughout (ETRS89 / UTM zone 33N - matches development polygons)
project_crs <- 25833

# Check column names to confirm kommunenummer column name
colnames(norway_municipalities_sf)

# Verify CRS matches project CRS - transform if needed
if (st_crs(norway_municipalities_sf)$epsg != project_crs) {
  norway_municipalities_sf <- st_transform(norway_municipalities_sf, project_crs)
}

# Download Norway land boundary and clip municipalities to it
# this removes marine areas that extend offshore for some coastal municipalities
norway_land <- geodata::gadm(country = "NOR", level = 0,
                             path = tempdir(), version = "latest") |>
  st_as_sf() |>
  st_transform(project_crs)

# Clip municipality boundaries to land
norway_municipalities_sf <- st_intersection(norway_municipalities_sf, norway_land)

## 2.2. Convert occurrences to sf spatial points -------------------------------

# Convert occurrences to sf spatial points (GBIF uses WGS84)
occurrences_sf <- clean_occurrences_15km |>
  filter(!is.na(decimalLongitude),
         !is.na(decimalLatitude)) |>
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = 4326)

# Transform occurrences to project CRS
occurrences_sf <- st_transform(occurrences_sf, project_crs)

# Verify CRS match
stopifnot(st_crs(norway_municipalities_sf) == st_crs(occurrences_sf))
