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

# 2. PREPARE THE POPULATION GRID -----------------------------------------------

# Auto-detect the population-count column
population_col <- grep("pop|bef|folke|inhab|antall", names(population_grid),
                ignore.case = TRUE, value = TRUE)[1]

# Make sure that there are no NAs
stopifnot(!is.na(population_col))
cat("Using population column:", population_col, "\n") #poptot

# If the grid is points (i.e. using cell centroids) rather than polygons, build cell squares
# centred on them so area-weighting works. h = half the cell size in metres:
# use 125 for a 250 m grid
if (all(st_geometry_type(population_grid) %in% c("POINT", "MULTIPOINT"))) {
  cat("Grid is points - building cell squares from centroids...\n")
  xy <- st_coordinates(population_grid)
  make_cell <- function(x, y, h = 125) {   # 125 m = 250 m cell
    st_polygon(list(rbind(c(x-h, y-h), c(x+h, y-h), c(x+h, y+h), c(x-h, y+h), c(x-h, y-h))))
  }
  cells <- do.call(st_sfc, lapply(seq_len(nrow(xy)), \(i) make_cell(xy[i,1], xy[i,2])))
  population_grid <- st_sf(pop = population_grid[[population_col]], geometry = cells, crs = st_crs(dev_poly))
  population_col <- "pop"
}

# Calculate population/km2 for each cell (count/cell area in km2)
population_grid <- population_grid |>
  mutate(cell_area_km2 = as.numeric(st_area(population_grid)) / 1e6,
         pop_per_km2 = .data[[population_col]] / cell_area_km2) |>
  filter(is.finite(pop_per_km2))

# 3. AREA-WEIGHTED POPULATION DENSITY PER POLYGON ------------------------------

# Use st_intersection to cut each polygon by the grid
# weight each piece's cell density by the area
inter <- st_intersection(development_poly |> dplyr::select(id),
                         population_grid |> dplyr::select(pop_per_km2))
inter$piece_area <- as.numeric(st_area(inter))

population_by_poly <- inter |>
  st_drop_geometry() |>
  group_by(id) |>
  summarise(pop_density = weighted.mean(pop_per_km2, w = piece_area, na.rm = TRUE),
            .groups = "drop")

cat("Polygons with a population value:", nrow(population_by_poly),
    "of", nrow(development_poly), "\n") # 65476 of 129881










