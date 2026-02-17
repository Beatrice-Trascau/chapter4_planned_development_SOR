##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 4.1_H2a_buffer_landcover_GLMM
# This script contains code to test Hypothesis 2a: Area plan polygons have a 
# greater number of SOR than areas not planned for development
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source setup file
library(here)
source(here("scripts", "0_setup.R"))

# Load development polygons
development_polygons <- st_read(here("data", "raw_data", "nina_planagt.gpkg"))

# Load cleaned occurrence records
clean_occurrences_70m <- read.csv(here("data", "derived_data",
                                       "clean_occurrences_70m.txt"))

# Load land cover data
gdb_path <- here("data", "raw_data", "Hovedokosystem_nedlasting", "Hovedokosystem.gdb")
land_cover <- st_read(gdb_path, layer = "Hovedøkosystem", quiet = TRUE)


# 2. PREPARE DEVELOPMENT POLYGONS ----------------------------------------------

## 2.1. Clean developement polygons --------------------------------------------

# Filter out Ports & Marinas
development_polygons_filtered <- development_polygons |>
  filter(arealformalsgruppe != "16 Havner og småbåthavner") |>
  mutate(polygon_id = row_number(),
         area_m2_numeric = as.numeric(planlagt_areal_m2),
         english_categories = case_when(arealformalsgruppe == "01 Bolig eller sentrumsformål" ~ "Residential",
                                        arealformalsgruppe == "02 Fritidsbebyggelse" ~ "Recreational",
                                        arealformalsgruppe == "03 Tjenesteyting" ~ "Services",
                                        arealformalsgruppe == "04 Handel" ~ "Retail",
                                        arealformalsgruppe == "05 Turistformål" ~ "Tourism",
                                        arealformalsgruppe == "06 Næringsvirksomhet" ~ "Commercial",
                                        arealformalsgruppe == "07 Råstoffutvinning" ~ "Mining",
                                        arealformalsgruppe == "08 Kombinerte formål" ~ "Combined",
                                        arealformalsgruppe == "13 Forsvaret" ~ "Defense"))

## 2.2. Create buffers ---------------------------------------------------------

# Calculate buffer distance for each polygon based on its area
  # buffer distance  = radius of circle with same areas as the polygon
  # radius = sqrt(area/pi)
development_polygons_filtered <- development_polygons_filtered |>
  mutate(buffer_distance = sqrt(area_m2_numeric / pi))

# Create buffers
polygon_buffers <- st_buffer(development_polygons_filtered, 
                             dist = development_polygons_filtered$buffer_distance)

# Add identifier columns to distinguish polygons from buffers
development_polygons_filtered$polygon_type <- "Development"
polygon_buffers$polygon_type <- "Buffer"

# Create pair ID to link each polygon to its buffer
development_polygons_filtered$pair_id <- development_polygons_filtered$polygon_id
polygon_buffers$pair_id <- polygon_buffers$polygon_id

# 3. GET LAND-COVER DATA FOR POLYGONS AND BUFFERS ------------------------------

## 3.1. Prepare land-cover data ------------------------------------------------

# Check if the CRS matches
if (st_crs(land_cover)$epsg != 25833) {
  land_cover <- st_transform(land_cover, 25833)
}

# Add category names to the land-cover so that you can understand them
  # based on: https://nva.sikt.no/registration/0198cc623366-a2a951d5-8763-4125-8cdb-86885c44f5c5
land_cover <- land_cover |>
  mutate(land_cover_name = case_when(ecotype == 1 ~ "Settlements",
                                     ecotype == 2 ~ "Cropland",
                                     ecotype == 3 ~ "Grassland",
                                     ecotype == 4 ~ "Forest",
                                     ecotype == 5 ~ "Heathland",
                                     ecotype == 6 ~ "Sparsely_vegetated",
                                     ecotype == 7 ~ "Wetlands",
                                     ecotype == 8 ~ "Rivers",
                                     ecotype == 9 ~ "Lakes",
                                     ecotype == 10 ~ "Marine_inlets",
                                     ecotype == 11 ~ "Coastal",
                                     ecotype == 12 ~ "Marine_offshore",
                                     TRUE ~ "Unknown"))

# Keep only terrestrial land-covers (remove aquatic and marine)
land_cover_terrestrial <- land_cover |>
  filter(ecotype %in% 1:6)

## 3.2. Extract dominant land-cover for polygons and buffers -------------------

# Create a function to extract the dominant (>50%) land-cover for each set of polygons
extract_dominant_landcover <- function(polygons, land_cover_data){
  
  # intersect polygons with land-cover
  intersection <- st_intersection(polygons, land_cover_data)
  
  # calculate area of each intersection
  intersection <- intersection |>
    mutate(intersection_area = as.numeric(st_area(geometry)))
  
  # find the land-cover type with the largest area for each polygon
  dominant_lc <- intersection |>
    st_drop_geometry() |>
    group_by(polygon_id) |>
    slice_max(intersection_area, n = 1, with_ties = FALSE) |>
    select(polygon_id, land_cover_name, ecotype) |>
    ungroup()
  
  return(dominant_lc)
}

# Extract land-cover for the development polygons
polygon_landcover <- extract_dominant_landcover(development_polygons_filtered, 
                                                land_cover_terrestrial)

# Extract land-cover for the buffers
buffer_landcover <- extract_dominant_landcover(polygon_buffers, 
                                               land_cover_terrestrial)

# Join land-cover back to polygons and buffers
development_polygons_filtered <- development_polygons_filtered |>
  left_join(polygon_landcover, by = "polygon_id")
polygon_buffers <- polygon_buffers |>
  left_join(buffer_landcover, by = "polygon_id")


# Check if there are any polygons without land-cover data
cat("Polygons without land cover:", 
    sum(is.na(development_polygons_filtered$land_cover_name)), "\n")
cat("Buffers without land cover:", 
    sum(is.na(polygon_buffers$land_cover_name)), "\n")

# 4. GET OCCURRENCE & SPECIES DATA IN POLYGONS ---------------------------------

## 4.1. Prepare occurrence records ---------------------------------------------

# Convert occurrences to spatial points (keep only records with valid coordinates)
occurrences_sf <- clean_occurrences_70m |>
  filter(!is.na(decimalLongitude), !is.na(decimalLatitude)) |>
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# Transform occurrences to match the CRS of the polygons
occurrences_sf <- st_transform(occurrences_sf, 25833)

## 4.2. Count occurrences and extract species lists for polygons & buffers -----

# Create function to count occurrences and retain species-level information within each polygon/buffer
  # returns  one row per polygon with:
  #- n_occurrences: total SOR count 
  #- n_species: number of unique species 
  #- species_list: identity of species present 
count_occurrences_with_species <- function(polygons, occurrences) {
  
  # spatial join: one row per occurrence per polygon
  joined <- st_join(polygons |> select(polygon_id),
                    occurrences |> select(gbifID, species),
                    join = st_intersects,
                    left = TRUE)
  
  # summarise to one row per polygon
  summary <- joined |>
    st_drop_geometry() |>
    group_by(polygon_id) |>
    summarise(n_occurrences = sum(!is.na(gbifID)),
              n_species     = n_distinct(species[!is.na(species)]),
              species_list  = list(unique(species[!is.na(species)])),
              .groups = "drop")
  
  # join summary back to polygons to retain all polygon metadata
  polygons_with_counts <- polygons |>
    st_drop_geometry() |>
    left_join(summary, by = "polygon_id")
  
  return(polygons_with_counts)
}

# Count occurrences for development polygons
cat("  Counting in development polygons...\n")
development_with_counts <- count_occurrences_with_species(development_polygons_filtered,
                                                          occurrences_sf)

# Count occurrences for buffers
cat("  Counting in buffers...\n")
buffers_with_counts <- count_occurrences_with_species(polygon_buffers,
                                                      occurrences_sf)

cat("Occurrence counting complete.\n")

## 4.3. Combine polygons and buffers into a single df --------------------------

cat("\nCombining polygons and buffers into single dataset...\n")

# Select relevant columns and ensure they match
# species_list is retained as a list-column for use in H2b, H2c, H2d
polygon_data <- development_with_counts |>
  select(polygon_id, pair_id, polygon_type, area_m2_numeric,
         english_categories, kommune, land_cover_name,
         n_occurrences, n_species, species_list)

buffer_data <- buffers_with_counts |>
  select(polygon_id, pair_id, polygon_type, area_m2_numeric,
         english_categories, kommune, land_cover_name,
         n_occurrences, n_species, species_list)

# Combine
model_data <- bind_rows(polygon_data, buffer_data)

# Convert to factors and create log area
model_data <- model_data |>
  mutate(polygon_type    = factor(polygon_type, levels = c("Buffer", "Development")),
         land_cover_name = factor(land_cover_name),
         kommune_factor  = factor(kommune),
         pair_id_factor  = factor(pair_id),
         log_area        = log(area_m2_numeric))

# Remove any rows with missing data in modelling variables
# Note: species_list may be an empty list for zero-occurrence polygons - this is fine
model_data_complete <- model_data |>
  filter(!is.na(n_occurrences),
         !is.na(log_area),
         !is.na(polygon_type),
         !is.na(land_cover_name),
         !is.na(kommune_factor))

cat("Final dataset size:", nrow(model_data_complete), "rows\n")
cat("Number of pairs:", n_distinct(model_data_complete$pair_id), "\n")
cat("Number of municipalities:", n_distinct(model_data_complete$kommune_factor), "\n")

# Save the full dataset - contains everything needed for H2a, H2b, H2c and H2d:
saveRDS(model_data_complete,
        here("data", "derived_data", "h2_polygon_buffer_data.rds"))

cat("Dataset saved to data/derived_data/h2_polygon_buffer_data.rds\n")