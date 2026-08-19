##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 5.1_GeoNorge_add_population_density_for_H3
# This script contains code to add the population desnities to each development
# polygon to use in testing H3
# Data was downloaded from geonorge.no
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source the data
library(here)
library(sf)
source(here("scripts", "0_setup.R"))

# Load the development polygons with the geometry (i.e. before dropping it in script 3.5)
polygon_buffer_data <- readRDS(here("data", "derived_data", "polygon_buffer_data.rds"))

# Keep only development polygons 
development_poly <- polygon_buffer_data |>
  dplyr::filter(polygon_type == "Development")
cat("Development polygons:", nrow(polygon_buffer_data), "\n") # 259762

# Get paths for the Befolknings statistikk Rutenett250m
grid_path <- here("data", "raw_data", "ssb_population_grid_250m.gdb")
tettsted_path <- here("data", "raw_data", "ssb_tettsteder.gdb")

# Check the layers inside the population grid & choose the right one
print(st_layers(grid_path))
grid_layer <- "befolkningparutenett_befolkningparuter250m" 

# Load the Befolknings statistikk Rutenett250m and tettsteder
population_grid <- st_read(grid_path, quiet = TRUE)
tettsted <- st_read(tettsted_path, quiet = TRUE)

# Harmonise CRS to the polygons (if needed)
if (st_crs(population_grid) != st_crs(development_poly)) population_grid <- st_transform(population_grid, st_crs(development_poly))
if (st_crs(tettsted) != st_crs(development_poly)) tettsted <- st_transform(tettsted, st_crs(development_poly))
