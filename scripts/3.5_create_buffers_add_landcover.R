##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 3.5_create_buffers_add_landcover
# This script contains code to test Hypothesis 2a: Area plan polygons have a 
# greater number of SOR than areas not planned for development
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source setup file
library(here)
source(here("scripts", "0_setup.R"))

# Load development polygons
development_polygons <- st_read(here("data", "raw_data", "nina_planagt.gpkg"))

# Load pre-created buffers
polygon_buffers <- st_read(here("data", "derived_data", "NoAggPlanBufferNew.gpkg"))

# Load cleaned occurrence records
clean_occurrences_1km <- read.csv(here("data", "derived_data",
                                       "clean_occurrences_1km.txt"))

# Load land cover data
gdb_path <- here("data", "raw_data", "Hovedokosystem_nedlasting", "Hovedokosystem.gdb")
land_cover <- st_read(gdb_path, layer = "Hovedøkosystem", quiet = TRUE)


# 2. PREPARE DEVELOPMENT POLYGONS ----------------------------------------------

# Check that id column exists in polygons
if(!"id" %in% colnames(development_polygons)){
  stop("ERROR: Polygon dataframe does not contain 'id' column")
}

# Filter out Ports & Marinas
development_polygons_temp <- development_polygons |>
  filter(arealformalsgruppe != "16 Havner og småbåthavner") |>
  mutate(area_m2_numeric = as.numeric(planlagt_areal_m2),
         english_categories = case_when(arealformalsgruppe == "01 Bolig eller sentrumsformål" ~ "Residential",
                                        arealformalsgruppe == "02 Fritidsbebyggelse" ~ "Recreational",
                                        arealformalsgruppe == "03 Tjenesteyting" ~ "Services",
                                        arealformalsgruppe == "04 Handel" ~ "Retail",
                                        arealformalsgruppe == "05 Turistformål" ~ "Tourism",
                                        arealformalsgruppe == "06 Næringsvirksomhet" ~ "Commercial",
                                        arealformalsgruppe == "07 Råstoffutvinning" ~ "Mining",
                                        arealformalsgruppe == "08 Kombinerte formål" ~ "Combined",
                                        arealformalsgruppe == "13 Forsvaret" ~ "Defense"),
         polygon_type = "Development",
         pair_id = id)

# 3. PREPARE BUFFERS -----------------------------------------------------------

# Check that id column exists in polygons
if(!"id" %in% colnames(polygon_buffers)){
  stop("ERROR: Polygon dataframe does not contain 'id' column")
}

# Check you still have all the metadata columns ("CompleteInOcean", "CompleteInOtherPlanned", "InOceanAndOtherPlanned", "EndBufferDist", "EndBufferSize")
colnames(polygon_buffers)

# Filter buffers to match filtered polygons
polygon_buffers_temp <- polygon_buffers |>
  filter(id %in% development_polygons_temp$id)

# Check how many buffers are fully in the ocean or far from the development polygon (N.B. buffers were created further from the development polygon if the development polygon was completely surrounded by other development polygons and the buffer could not be created around it)
n_ocean <- sum(polygon_buffers_temp$CompleteInOcean ==1, na.rm = TRUE)
n_other_planned <- sum(polygon_buffers_temp$CompleteInOtherPlanned ==1, na.rm = TRUE)
n_mixed <- sum(polygon_buffers_temp$InOceanAndOtherPlanned == 1 & 
                 polygon_buffers_temp$CompleteInOcean == 0 & 
                 polygon_buffers_temp$CompleteInOtherPlanned == 0, na.rm = TRUE)

# Filter buffers 
polygon_buffers_filtered <- polygon_buffers  |>
  filter(id %in% development_polygons_temp$id,
         CompleteInOcean == 0,
         CompleteInOtherPlanned == 0) |>
  mutate(polygon_type = "Buffer",
         pair_id = id,
         buffer_area_m2 = EndBufferSize,
         buffer_distance_m = EndBufferDist)

# Filter development polygons to keep only those that still have buffers (i.e. that were not removed in the previous step) 
development_polygons_filtered <- development_polygons_temp |>
  filter(id %in% polygon_buffers_filtered$od)

# Check how many development polygons and buffers we are left with 
nrow(development_polygons_filtered)
nrow(polygon_buffers_filtered)

# Check that the number of development polygons left is the same as the number of buffers left
f (nrow(polygon_buffers_filtered) != nrow(development_polygons_filtered)) {
  cat("  WARNING: Number of buffers (", nrow(polygon_buffers_filtered), 
      ") doesn't match number of polygons (", nrow(development_polygons_filtered), ")\n")
  
  # Show which IDs are missing
  missing_in_buffer <- setdiff(development_polygons_filtered$id, 
                               polygon_buffers_filtered$id)
  missing_in_poly <- setdiff(polygon_buffers_filtered$id,
                             development_polygons_filtered$id)
  
  if (length(missing_in_buffer) > 0) {
    cat("  Missing in buffers:", length(missing_in_buffer), "ids\n")
  }
  if (length(missing_in_poly) > 0) {
    cat("  Extra in buffers (not in polygons):", length(missing_in_poly), "ids\n")
  }
} else {
  cat("  ✓ Number of buffers matches number of polygons\n")
}

# Add english_categories and kommune from polygons to buffers
# (buffers inherit these from their paired polygon for grouping/modeling)
buffer_metadata <- development_polygons_filtered |>
  st_drop_geometry() |>
  select(id, english_categories, kommune)

polygon_buffers_filtered <- polygon_buffers_filtered |>
  left_join(buffer_metadata, by = "id")

# 4. GET LAND-COVER DATA FOR POLYGONS AND BUFFERS ------------------------------

## 4.1. Prepare land-cover data ------------------------------------------------

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
  filter(ecotype %in% 1:7)

## 4.2. Extract dominant land-cover for polygons and buffers -------------------

# Create a function to extract the dominant (>50%) land-cover for each set of polygons
extract_dominant_landcover <- function(polygons, land_cover_data){
  
  # intersect polygons with land-cover
  intersection <- st_intersection(polygons, land_cover_data)
  
  # calculate area of each intersection
  intersection <- intersection |>
    mutate(intersection_area = as.numeric(st_area(intersection)))
  
  # find the land-cover type with the largest area for each polygon
  dominant_lc <- intersection |>
    st_drop_geometry() |>
    group_by(id) |>
    slice_max(intersection_area, n = 1, with_ties = FALSE) |>
    select(id, land_cover_name, ecotype) |>
    ungroup()
  
  return(dominant_lc)
}

# Extract land-cover for the development polygons (will assign buffers the same as the development polygons)
polygon_landcover <- extract_dominant_landcover(development_polygons_filtered, 
                                                land_cover_terrestrial)

# Join land-cover back to polygons and buffers
development_polygons_filtered <- development_polygons_filtered |>
  left_join(polygon_landcover, by = "polygon_id")

# Check if there are any polygons without land-cover data
cat("Polygons without land cover:", 
    sum(is.na(development_polygons_filtered$land_cover_name)), "\n")

## 4.3. Assign same land-cover to buffers --------------------------------------

# Give buffers the same land cover as their matching polygons based on the id
buffers_landcover <- development_polygons_filtered |>
  st_drop_geometry() |>
  select(id, land_cover_name, ecotype)

# Join land-cover to buffers
polygon_buffers_filtered <- polygon_buffers_filtered |>
  left_join(buffers_landcover, by = "id")

# Check if there are any buffers without land-cover data
cat("Buffers without land cover:", 
    sum(is.na(polygon_buffers_filtered$land_cover_name)), "\n")

# 5. GET OCCURRENCE & SPECIES DATA IN POLYGONS ---------------------------------

## 5.1. Prepare occurrence records ---------------------------------------------

# Convert occurrences to spatial points (keep only records with valid coordinates)
occurrences_sf <- clean_occurrences_1km |>
  filter(!is.na(decimalLongitude), !is.na(decimalLatitude)) |>
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# Transform occurrences to match the CRS of the polygons
occurrences_sf <- st_transform(occurrences_sf, 25833)

## 5.2. Count occurrences and extract species lists for polygons & buffers -----

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

## 5.3. Combine polygons and buffers into a single df --------------------------

# Select relevant columns and ensure they match
# species_list is retained as a list-column for use in H2b, H2c, H2d
polygon_data <- development_with_counts |>
  select(id, pair_id, polygon_type, area_m2_numeric,
         english_categories, kommune, land_cover_name,
         n_occurrences, n_species, species_list)

# Select relevant columns from buffers
buffer_data <- buffers_with_counts |>
  rename(area_m2_numeric = buffer_area_m2) |>
  select(id, pair_id, polygon_type,
         english_categories, kommune, land_cover_name,
         n_occurrences, n_species, species_list,
         # keep buffer creation info
         buffer_area_m2, buffer_distance_m,
         CompleteInOcean, CompleteInOtherPlanned, 
         InOceanAndOtherPlanned, EndBufferDist, EndBufferSize)

# Combine (N.B. Polygon df will have NA for the buffer-specific columns)
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

# 6. CHECK DATA WAS COMBINED CORRECTLY -----------------------------------------

## 6.1. Check basic structure --------------------------------------------------

# Count development and buffer polygons
n_dev <- sum(model_data_complete$polygon_type == "Development")
n_buf <- sum(model_data_complete$polygon_type == "Buffer")

if (n_dev == n_buf) {
  cat("YAAAS: Equal Development and Buffer rows (", n_dev, "each)\n")
} else {
  cat("OOPSIE: Unequal rows - Development:", n_dev, ", Buffer:", n_buf, "\n")
}

## 6.2. Check the pairing ------------------------------------------------------

# Get pairing
pair_counts <- model_data_complete |>
  group_by(pair_id) |>
  summarise(n_rows = n(),
            n_dev = sum(polygon_type == "Development"),
            n_buf = sum(polygon_type == "Buffer"),
            .groups = "drop")

if (all(pair_counts$n_rows == 2) && all(pair_counts$n_dev == 1 & pair_counts$n_buf == 1)) {
  cat("YAAAS: All pairs have exactly 1 Development + 1 Buffer\n")
} else {
  bad_pairs <- pair_counts |> filter(n_rows != 2 | n_dev != 1 | n_buf != 1)
  cat("OOPSIE:", nrow(bad_pairs), "pairs have incorrect composition\n")
}

## 6.3. Check land-cover matching ----------------------------------------------

# Check that buffers have the same land-cover as their paired polygons
land_cover_check <- model_data_complete |>
  select(pair_id, polygon_type, land_cover_name) |>
  tidyr::pivot_wider(names_from = polygon_type, values_from = land_cover_name) |>
  mutate(land_cover_match = Development == Buffer)

if (all(landcover_check$land_cover_match, na.rm = TRUE)) {
  cat("YAAAS: All buffers have same land cover as their paired polygons\n")
} else {
  n_mismatch <- sum(!landcover_check$land_cover_match, na.rm = TRUE)
  cat("OOPSIE:", n_mismatch, "pairs have mismatched land cover\n")
}

## 6.4. Summary statistics -----------------------------------------------------  
  
cat("\n=== SUMMARY STATISTICS ===\n")
cat("Development polygons:\n")
cat("  Mean occurrences:", round(mean(polygon_data$n_occurrences), 2), "\n")
cat("  Mean species:", round(mean(polygon_data$n_species), 2), "\n")

cat("\nBuffers:\n")
cat("  Mean occurrences:", round(mean(buffer_data$n_occurrences), 2), "\n")
cat("  Mean species:", round(mean(buffer_data$n_species), 2), "\n")

# Report buffer constraints if columns exist
if ("CompleteInOcean" %in% colnames(buffer_data)) {
  cat("\nBuffer creation info (remaining buffers after filtering):\n")
  
  # Check how many have non-zero buffer distance
  n_displaced <- sum(buffer_data$buffer_distance_m > 0, na.rm = TRUE)
  if (n_displaced > 0) {
    cat("  Buffers created at distance from polygon:", n_displaced, "\n")
    cat("  Mean distance for displaced buffers:", 
        round(mean(buffer_data$buffer_distance_m[buffer_data$buffer_distance_m > 0], na.rm = TRUE), 1), "m\n")
  }
  
  # Mixed constraint (ocean + other planned)
  n_mixed <- sum(buffer_data$InOceanAndOtherPlanned == 1 & 
                   buffer_data$CompleteInOcean == 0 & 
                   buffer_data$CompleteInOtherPlanned == 0, na.rm = TRUE)
  if (n_mixed > 0) {
    cat("  Buffers with mixed constraints (ocean + other planned):", n_mixed, "\n")
  }
  
  # Successfully created
  n_success <- sum(buffer_data$CompleteInOcean == 0 & 
                     buffer_data$CompleteInOtherPlanned == 0 &
                     buffer_data$InOceanAndOtherPlanned == 0, na.rm = TRUE)
  cat("  Buffers created without constraints:", n_success, "\n")
}

cat("\nLand cover distribution:\n")
print(table(model_data_complete$land_cover_name, model_data_complete$polygon_type))

# 7. CREATE OCCURRENCE-LEVEL DATA FOR H2D --------------------------------------

# Create occurrence-level join for polygons and buffers (needed for completeness calculations that require year and parentEventID)
# Spatial join for development polygons
development_occ_join <- st_join(development_polygons_filtered |>
                                  select(id, pair_id, polygon_type, area_m2_numeric, 
                                         english_categories, kommune, land_cover_name),
                                occurrences_sf |>
                                  select(gbifID, species, year, parentEventID),
                                join = st_intersects,
                                left = TRUE)

# Spatial join for buffers
buf_occ_join <- st_join(polygon_buffers_filtered |> 
                          select(id, pair_id, polygon_type, area_m2_numeric, 
                                 english_categories, kommune, land_cover_name),
                        occurrences_sf |> 
                          select(gbifID, species, year, parentEventID),
                        join = st_intersects,
                        left = TRUE)

# Combine both datasets
polygon_buffer_occurrence_join <- rbind(dev_occ_join, buf_occ_join)

# Save df for H2d
saveRDS(polygon_buffer_occurrence_join,
        here("data", "derived_data", "h2d_polygon_buffer_occurrence_join.rds"))

# END OF SCRIPT ----------------------------------------------------------------