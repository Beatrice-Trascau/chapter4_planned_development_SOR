##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 3.5_create_buffers_add_landcover
# This script contains code to prepare the development polygons and buffers
# for analyses
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source setup file
library(here)
source(here("scripts", "0_setup.R"))

# Load development polygons
development_polygons <- st_read(here("data", "raw_data", "nina_planagt.gpkg"))

# Load pre-created buffers
polygon_buffers <- st_read(here("data", "derived_data", "NoAggPlanBufferNew.gpkg"))

# Load land cover data
gdb_path <- here("data", "raw_data", "Hovedokosystem_nedlasting", "Hovedokosystem.gdb")
land_cover <- st_read(gdb_path, layer = "Hovedøkosystem", quiet = TRUE)

# 2. CHECK THE POLYGONS, BUFFERS AND LAND-COVER DATA ---------------------------

## 2.1. Check required columns -------------------------------------------------

# Check if development polygons contains every column used downstream
setdiff(c("id", "arealformalsgruppe", "planlagt_areal_m2", "kommune",
          "kommunenummer"),
        colnames(development_polygons))
stopifnot(all(c("id", "arealformalsgruppe", "planlagt_areal_m2", "kommune",
                "kommunenummer") %in% colnames(development_polygons))) # all good!

# Check if the buffers contain the shared id column
stopifnot("id" %in% colnames(polygon_buffers)) # all good!

# Check if land-cover contains the ecotype column
stopifnot("ecotype" %in% colnames(land_cover)) # all good!

## 2.2. Check the id column ----------------------------------------------------

# Check that the ids are unique and complete in both datasets
stopifnot(anyDuplicated(development_polygons$id) == 0,
          anyDuplicated(polygon_buffers$id) == 0,
          !any(is.na(development_polygons$id)),
          !any(is.na(polygon_buffers$id)))

cat("Development polygons loaded:", nrow(development_polygons), "\n") # 133644
cat("Buffers loaded:            ", nrow(polygon_buffers), "\n") # 133644
cat("Land cover features loaded:", nrow(land_cover), "\n") # 1001965

## 2.3. Check geometries -------------------------------------------------------

# Check that there are no empty geometries (which would return NA in spatial joins)
cat("\nEmpty geometries - polygons:", sum(st_is_empty(development_polygons)),
    " buffers:", sum(st_is_empty(polygon_buffers)),
    " land cover:", sum(st_is_empty(land_cover)), "\n") # 0 empty geometries

stopifnot(!any(st_is_empty(development_polygons)),
          !any(st_is_empty(polygon_buffers)))

# Check if the geomtry types are polygonal
cat("\nGeometry types:\n")
print(table(st_geometry_type(development_polygons)))
print(table(st_geometry_type(polygon_buffers)))

# Check if there are any invalid geomtries (which could fail the st_join)
cat("\nInvalid geometries - polygons:", sum(!st_is_valid(development_polygons)), "\n") # 9 development polygons with invalid geometries, need to fix before moving on
cat("Invalid geometries - buffers: ", sum(!st_is_valid(polygon_buffers)), "\n") # 0

# Repair invalid geometries for the 9 development polygons flagged above
development_polygons <- st_make_valid(development_polygons)

# Make sure the repair worked
stopifnot(all(st_is_valid(development_polygons)))
cat("Invalid geometries after repair:", sum(!st_is_valid(development_polygons)), "\n") #0!

# Make sure that st_make_valid didn't chnage geometries for the polygons
table(st_geometry_type(development_polygons))

# Some multipolygons were changed to polygons in the st_make_valid() process
# it's expected but just want to check that the area is still correct
summary(as.numeric(st_area(development_polygons)) -
          as.numeric(development_polygons$planlagt_areal_m2)) # there may be some issue here
# check script 3.5_polygons_area_checks for details

# 3. HARMONISE CRS -------------------------------------------------------------

# Set CRS for everything else based on the development polygons (ETRS89/UTM33N)
cat("\nPolygon CRS:   ", st_crs(development_polygons)$input, "\n")
cat("Buffer CRS:    ", st_crs(polygon_buffers)$input, "\n")
cat("Land cover CRS:", st_crs(land_cover)$input, "\n")

# Check that all three objects have a defined CRS
stopifnot(!is.na(st_crs(development_polygons)),
          !is.na(st_crs(polygon_buffers)),
          !is.na(st_crs(land_cover)))

# Transform buffers to match the polygons if needed
if (st_crs(polygon_buffers) != st_crs(development_polygons)) {
  cat("\nTransforming buffers to match polygon CRS...\n")
  polygon_buffers <- st_transform(polygon_buffers, st_crs(development_polygons))
}

# Transform land-cover to match the polygons if needed
if (st_crs(land_cover) != st_crs(development_polygons)) {
  cat("Transforming land cover to match polygon CRS...\n")
  land_cover <- st_transform(land_cover, st_crs(development_polygons))
}

# Check that everything now shares one CRS
stopifnot(st_crs(development_polygons) == st_crs(polygon_buffers),
          st_crs(development_polygons) == st_crs(land_cover))
cat("\nPASS: all layers share the same CRS\n")

# Check that the transformation went well and all layers still overlap in space
print(rbind(polygons   = st_bbox(development_polygons),
            buffers    = st_bbox(polygon_buffers),
            land_cover = st_bbox(land_cover)))

if (length(st_intersection(st_as_sfc(st_bbox(development_polygons)),
                           st_as_sfc(st_bbox(land_cover)))) == 0) {
  stop("ERROR: polygon and land cover bounding boxes do not overlap")
} else {
  cat("\nPASS: polygon and land cover extents overlap\n")
}

# 4. PREPARE DEVELOPMENT POLYGONS ----------------------------------------------

# Drop Ports & Marinas and translate category names
development_polygons_temp <- development_polygons |>
  filter(arealformalsgruppe != "16 Havner og småbåthavner")


# Calculate area_m2_numeric from the geomtry (st_area) and not from planlagt_areal_m2
# see script 3.5_polygons_area_checks for details
development_polygons_temp <- development_polygons_temp |>
  mutate(area_m2_numeric         = as.numeric(st_area(development_polygons_temp)),
         planlagt_area_reference = as.numeric(planlagt_areal_m2),
         english_categories = case_when(arealformalsgruppe == "01 Bolig eller sentrumsformål" ~ "Residential",
                                        arealformalsgruppe == "02 Fritidsbebyggelse" ~ "Recreational",
                                        arealformalsgruppe == "03 Tjenesteyting" ~ "Services",
                                        arealformalsgruppe == "04 Handel" ~ "Retail",
                                        arealformalsgruppe == "05 Turistformål" ~ "Tourism",
                                        arealformalsgruppe == "06 Næringsvirksomhet" ~ "Commercial",
                                        arealformalsgruppe == "07 Råstoffutvinning" ~ "Mining",
                                        arealformalsgruppe == "08 Kombinerte formål" ~ "Combined",
                                        arealformalsgruppe == "13 Forsvaret" ~ "Defense"))

# Check how many polygons are left after removing the ports and marinas
cat("\nPolygons after removing Ports & Marinas:", nrow(development_polygons_temp), "\n") # 131023

# Check that every category was translated
if (any(is.na(development_polygons_temp$english_categories))) {
  cat("FAIL: untranslated categories present:\n")
  print(unique(development_polygons_temp$arealformalsgruppe[is.na(development_polygons_temp$english_categories)]))
} else {
  cat("PASS: all development categories translated\n")
} # All good!

# Check that the area was converted cleanly and can be used later on a log scale
cat("Areas that failed to convert to numeric:",
    sum(is.na(development_polygons_temp$area_m2_numeric)), "\n") #0
cat("Areas of zero or less (log would be -Inf/NaN):",
    sum(development_polygons_temp$area_m2_numeric <= 0, na.rm = TRUE), "\n") #0

# 5. GET LAND-COVER DATA FOR POLYGONS AND BUFFERS ------------------------------

## 5.1. Prepare land-cover data ------------------------------------------------

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

# Check that every ecotype was recognised
cat("\nLand cover classes found:\n")
print(table(land_cover$land_cover_name))
if (any(land_cover$land_cover_name == "Unknown")) {
  cat("FAIL: unrecognised ecotype codes:\n")
  print(unique(land_cover$ecotype[land_cover$land_cover_name == "Unknown"]))
} else {
  cat("PASS: all ecotype codes recognised\n")
}

# Keep only terrestrial land-covers (1-7)
land_cover_terrestrial <- land_cover |>
  filter(ecotype %in% 1:7)

# Keep the water classes separately (8-9 freshwater, 10-12 marine)
# will be used later to check why a polygon doesn't have a terrestrial land-cover
land_cover_water <- land_cover |>
  filter(ecotype %in% 8:12)

# Check that neither subset is empty
stopifnot(nrow(land_cover_terrestrial) > 0,
          nrow(land_cover_water) > 0)
cat("\nTerrestrial land cover features:", nrow(land_cover_terrestrial), "\n") #613591
cat("Water land cover features:      ", nrow(land_cover_water), "\n") #388374
 
## 5.2. Extract dominant land-cover for polygons -------------------------------

# Define a function to extract land-cover for each polygon
# For each polygon, st_join with largest = TRUE keeps the land cover class covering the largest area
# Polygons with no land-cover overlap return NA
# Will be processed in batches to spare some of the memory
# Function stops if either the input or outpus is wrong
extract_dominant_landcover <- function(polygons, land_cover_data, batch_size = 5000) {
  
  # Check that the inputs are sf objects sharing one CRS
  stopifnot(inherits(polygons, "sf"),
            inherits(land_cover_data, "sf"),
            st_crs(polygons) == st_crs(land_cover_data))
  
  # Check that the id column is present, unique and complete
  stopifnot("id" %in% names(polygons),
            anyDuplicated(polygons$id) == 0,
            !any(is.na(polygons$id)))
  
  # Check that the land-cover carries the columns being transferred
  stopifnot(all(c("land_cover_name", "ecotype") %in% names(land_cover_data)))
  
  n_batches <- ceiling(nrow(polygons) / batch_size)
  results   <- vector("list", n_batches)
  
  for (i in seq_len(n_batches)) {
    
    cat("    Processing batch", i, "of", n_batches, "\n")
    
    start_idx <- (i - 1) * batch_size + 1
    end_idx   <- min(i * batch_size, nrow(polygons))
    
    results[[i]] <- polygons[start_idx:end_idx, ] |>
      dplyr::select(id) |>
      st_join(land_cover_data |> dplyr::select(land_cover_name, ecotype),
              join    = st_intersects,
              largest = TRUE) |>
      st_drop_geometry()
    
    gc()
  }
  
  out <- bind_rows(results)
  
  # Check that there is exactly one row returned per polygon (should be ensured by largest = TRUE)
  if (nrow(out) != nrow(polygons)) {
    stop("ERROR: land cover join returned ", nrow(out), " rows for ",
         nrow(polygons), " polygons")
  }
  if (anyDuplicated(out$id) > 0) {
    stop("ERROR: land cover join produced duplicated ids")
  }
  
  cat("  Polygons with no land cover match:", sum(is.na(out$land_cover_name)),
      "of", nrow(out), "\n")
  
  return(out)
}

# Extract dominant terrestrial land-cover
cat("\nExtracting dominant terrestrial land cover...\n")
polygon_landcover <- extract_dominant_landcover(development_polygons_temp,
                                                land_cover_terrestrial)

# Join land-cover back to the polygons
development_polygons_temp <- development_polygons_temp |>
  left_join(polygon_landcover, by = "id")

# Check the join added columns without adding rows
stopifnot(nrow(development_polygons_temp) == nrow(polygon_landcover))

## 5.3. Check for polygons entirely in water -----------------------------------

# Check the polygons with NA for land-cover (which do not overlap with any of the terrestrial classes)
cat("\nPolygons with no terrestrial land cover:",
    sum(is.na(development_polygons_temp$land_cover_name)), "\n")

# Check which land-covers the polygons with NA actually intersect with
water_only_polygons <- extract_dominant_landcover(
  development_polygons_temp |> filter(is.na(land_cover_name)),
  land_cover_water) |>
  mutate(water_type = case_when(ecotype %in% 10:12 ~ "Sea",
                                ecotype %in% 8:9   ~ "Freshwater",
                                TRUE ~ "Unclassified"))

# Check how many are marine vs freshwater
print(table(water_only_polygons$water_type, useNA = "ifany"))

# Get a detailed breakdown by land-cover class
print(table(water_only_polygons$land_cover_name, useNA = "ifany"))

# Check if there are any polygons that fall outside of terrestrial and marine/aquatic and if they fall outside of the land-cover dataset entirely
if (any(water_only_polygons$water_type == "Unclassified")) {
  cat("\nFAIL:", sum(water_only_polygons$water_type == "Unclassified"),
      "polygons match no land cover class at all - check their location\n")
} else {
  cat("\nPASS: every water-only polygon was classified as sea or freshwater\n")
}

# Save the diagnosis for later reporting in the methods
saveRDS(water_only_polygons,
        here("data", "derived_data", "polygons_removed_water_only.rds"))

# Remove polygons with no terrestrial land-cover
development_polygons_filtered <- development_polygons_temp |>
  filter(!is.na(land_cover_name)) |>
  mutate(polygon_type = "Development",
         pair_id      = id)

# Check how many we are left with after filtering
cat("\nPolygons retained after removing water-only polygons:",
    nrow(development_polygons_filtered), "\n")

# And check that the numbers are correct
stopifnot(nrow(development_polygons_filtered) + nrow(water_only_polygons) ==
            nrow(development_polygons_temp))
cat("PASS: retained + removed = total polygons\n")

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

# Remove polygons and buffers with NA for land-cover
development_polygons_filtered <- development_polygons_filtered |>
  filter(!is.na(land_cover_name))
polygon_buffers_filtered <- polygon_buffers_filtered |>
  filter(!is.na(land_cover_name))

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
  joined <- st_join(polygons |> select(id),
                    occurrences |> select(gbifID, species),
                    join = st_intersects,
                    left = TRUE)
  
  # summarise to one row per polygon
  summary <- joined |>
    st_drop_geometry() |>
    group_by(id) |>
    summarise(n_occurrences = sum(!is.na(gbifID)),
              n_species     = n_distinct(species[!is.na(species)]),
              species_list  = list(unique(species[!is.na(species)])),
              .groups = "drop")
  
  # join summary back to polygons to retain all polygon metadata
  polygons_with_counts <- polygons |>
    st_drop_geometry() |>
    left_join(summary, by = "id")
  
  return(polygons_with_counts)
}

# Count occurrences for development polygons
cat("  Counting in development polygons...\n")
development_with_counts <- count_occurrences_with_species(development_polygons_filtered,
                                                          occurrences_sf)

# Count occurrences for buffers
cat("  Counting in buffers...\n")
buffers_with_counts <- count_occurrences_with_species(polygon_buffers_filtered,
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
  rename(area_m2_numeric = mean_areal) |>
  select(id, pair_id, polygon_type, area_m2_numeric,
         english_categories, kommune, land_cover_name,
         n_occurrences, n_species, species_list)
         # keep buffer creation info
         # buffer_area_m2, buffer_distance_m,
         # CompleteInOcean, CompleteInOtherPlanned, 
         # InOceanAndOtherPlanned, EndBufferDist, EndBufferSize)

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
         !is.na(kommune_factor)) # something happens here and all buffer rows are lost - need to check why this happens!

cat("Final dataset size:", nrow(model_data_complete), "rows\n")
cat("Number of pairs:", n_distinct(model_data_complete$pair_id), "\n")
cat("Number of municipalities:", n_distinct(model_data_complete$kommune_factor), "\n")

# Save the full dataset - contains everything needed for H2a, H2b, H2c and H2d:
saveRDS(model_data,
        here("data", "derived_data", "h2_polygon_buffer_data.rds"))

# Check why the buffer rows are lost when we are filtering to get the model_data_complete df

# 6. CHECK DATA WAS COMBINED CORRECTLY -----------------------------------------

## 6.1. Check basic structure --------------------------------------------------

# Count development and buffer polygons
n_dev <- sum(model_data$polygon_type == "Development")
n_buf <- sum(model_data$polygon_type == "Buffer")

if (n_dev == n_buf) {
  cat("YAAAS: Equal Development and Buffer rows (", n_dev, "each)\n")
} else {
  cat("OOPSIE: Unequal rows - Development:", n_dev, ", Buffer:", n_buf, "\n")
}

## 6.2. Check the pairing ------------------------------------------------------

# Get pairing
pair_counts <- model_data |>
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
land_cover_check <- model_data |>
  select(pair_id, polygon_type, land_cover_name) |>
  tidyr::pivot_wider(names_from = polygon_type, values_from = land_cover_name) |>
  mutate(land_cover_match = Development == Buffer)

if (all(land_cover_check$land_cover_match, na.rm = TRUE)) {
  cat("YAAAS: All buffers have same land cover as their paired polygons\n")
} else {
  n_mismatch <- sum(!land_cover_check$land_cover_match, na.rm = TRUE)
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
                          select(id, pair_id, polygon_type, mean_areal, 
                                 english_categories, kommune, land_cover_name),
                        occurrences_sf |> 
                          select(gbifID, species, year, parentEventID),
                        join = st_intersects,
                        left = TRUE)

# Combine both datasets
polygon_buffer_occurrence_join <- rbind(development_occ_join, buf_occ_join)

# Save df for H2d
saveRDS(polygon_buffer_occurrence_join,
        here("data", "derived_data", "h2d_polygon_buffer_occurrence_join.rds"))

# END OF SCRIPT ----------------------------------------------------------------