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

# Function to find buffer distance that achieves target area (2x original)
  # returns both geometry and diagnostic information
find_buffer_distance_for_target_area <- function(geom, original_area, 
                                                 target_multiplier = 2,
                                                 tolerance = 0.001,
                                                 max_iterations = 20) {
  
  # initial guess based on area/perimeter approximation
  perimeter <- as.numeric(st_length(st_cast(st_geometry(geom), "MULTILINESTRING")))
  
  # for 2x area, we want buffer area = original area
  # initial guess: dist = original_area / perimeter
  dist_guess <- original_area / perimeter
  
  # target area
  target_area <- original_area * target_multiplier
  
  # track iterations
  iteration_count <- 0
  
  # iteratively adjust buffer distance to hit target area
  for(i in 1:max_iterations) {
    iteration_count <- i
    
    # create buffered polygon
    buffered <- st_buffer(geom, dist = dist_guess)
    current_area <- as.numeric(st_area(buffered))
    
    # check if we're close enough
    ratio <- current_area / target_area
    
    if(abs(ratio - 1) < tolerance) {
      # Success! Close enough to target
      break
    }
    
    # adjust distance proportionally
    # using square root because area scales with distance^2
    dist_guess <- dist_guess * sqrt(target_area / current_area)
  }
  
  # return the final buffered geometry AND diagnostic information
  buffered_final <- st_buffer(geom, dist = dist_guess)
  final_area <- as.numeric(st_area(buffered_final))
  
  return(list(
    buffered = buffered_final,
    iterations = iteration_count,
    buffer_distance = dist_guess,
    buffered_area = final_area,
    target_area = target_area,
    area_ratio = final_area / target_area
  ))
}

# Create buffers for all development polygons
polygon_buffers <- development_polygons_filtered

# Initialize vectors to store diagnostic information
buffer_diagnostics <- data.frame(polygon_id = development_polygons_filtered$polygon_id,
                                 iterations = integer(nrow(development_polygons_filtered)),
                                 buffer_distance_m = numeric(nrow(development_polygons_filtered)),
                                 buffered_area_m2 = numeric(nrow(development_polygons_filtered)),
                                 target_area_m2 = numeric(nrow(development_polygons_filtered)),
                                 buffered_area_ratio = numeric(nrow(development_polygons_filtered)),
                                 buffer_zone_area_m2 = numeric(nrow(development_polygons_filtered)),
                                 buffer_zone_ratio = numeric(nrow(development_polygons_filtered)))

# Progress tracking variables
start_time <- Sys.time()
progress_interval <- 10000

for(i in 1:nrow(polygon_buffers)) {
  # find optimal buffer distance and create buffered polygon
  result <- find_buffer_distance_for_target_area(st_geometry(development_polygons_filtered)[i],
                                                 development_polygons_filtered$area_m2_numeric[i],
                                                 target_multiplier = 2,
                                                 tolerance = 0.001,
                                                 max_iterations = 20)
  
  # subtract original to get buffer "donut"
  buffer_geom <- st_difference(result$buffered,
                               st_geometry(development_polygons_filtered)[i])
  st_geometry(polygon_buffers)[i] <- buffer_geom
  
  # store diagnostic information
  buffer_diagnostics$iterations[i] <- result$iterations
  buffer_diagnostics$buffer_distance_m[i] <- result$buffer_distance
  buffer_diagnostics$buffered_area_m2[i] <- result$buffered_area
  buffer_diagnostics$target_area_m2[i] <- result$target_area
  buffer_diagnostics$buffered_area_ratio[i] <- result$area_ratio
  buffer_diagnostics$buffer_zone_area_m2[i] <- as.numeric(st_area(buffer_geom))
  buffer_diagnostics$buffer_zone_ratio[i] <- as.numeric(st_area(buffer_geom)) / 
    development_polygons_filtered$area_m2_numeric[i]
  
  # progress indicator
  if(i %% progress_interval == 0) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    rate <- i / elapsed
    remaining <- (nrow(polygon_buffers) - i) / rate
    cat("  Processed", i, "of", nrow(polygon_buffers), "polygons")
    cat(" | Elapsed:", round(elapsed, 1), "min")
    cat(" | Est. remaining:", round(remaining, 1), "min\n")
  }
}

# Get the total time
total_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
cat("\nBuffer creation complete! Total time:", round(total_time, 1), "minutes\n")

# Save diagnostic information
saveRDS(buffer_diagnostics, 
        here("data", "derived_data", "buffer_creation_diagnostics.rds"))

# Verify the buffer areas
cat("\nBuffered polygon accuracy (target = 2.0x original):\n")
cat("  Mean buffered/target ratio:", round(mean(buffer_diagnostics$buffered_area_ratio, na.rm = TRUE), 4), "\n")
cat("  Median:", round(median(buffer_diagnostics$buffered_area_ratio, na.rm = TRUE), 4), "\n")
cat("  Std Dev:", round(sd(buffer_diagnostics$buffered_area_ratio, na.rm = TRUE), 6), "\n")
cat("  Range:", round(min(buffer_diagnostics$buffered_area_ratio, na.rm = TRUE), 6), "to", 
    round(max(buffer_diagnostics$buffered_area_ratio, na.rm = TRUE), 6), "\n")

cat("\nBuffer zone accuracy (target = 1.0x original):\n")
cat("  Mean buffer/original ratio:", round(mean(buffer_diagnostics$buffer_zone_ratio, na.rm = TRUE), 4), "\n")
cat("  Median:", round(median(buffer_diagnostics$buffer_zone_ratio, na.rm = TRUE), 4), "\n")
cat("  Std Dev:", round(sd(buffer_diagnostics$buffer_zone_ratio, na.rm = TRUE), 4), "\n")
cat("  Range:", round(min(buffer_diagnostics$buffer_zone_ratio, na.rm = TRUE), 4), "to", 
    round(max(buffer_diagnostics$buffer_zone_ratio, na.rm = TRUE), 4), "\n")

cat("\nIterations:\n")
cat("  Mean iterations per polygon:", round(mean(buffer_diagnostics$iterations, na.rm = TRUE), 1), "\n")
cat("  Median:", median(buffer_diagnostics$iterations, na.rm = TRUE), "\n")
cat("  Range:", min(buffer_diagnostics$iterations, na.rm = TRUE), "to", 
    max(buffer_diagnostics$iterations, na.rm = TRUE), "\n")

# Check for problematic ratios
problematic_1pct <- abs(buffer_diagnostics$buffer_zone_ratio - 1) > 0.01
problematic_5pct <- abs(buffer_diagnostics$buffer_zone_ratio - 1) > 0.05

cat("\nQuality assessment:\n")
cat("  Polygons within 1% of target:", sum(!problematic_1pct, na.rm = TRUE), 
    "(", round(100 * mean(!problematic_1pct, na.rm = TRUE), 1), "%)\n")
cat("  Polygons within 5% of target:", sum(!problematic_5pct, na.rm = TRUE), 
    "(", round(100 * mean(!problematic_5pct, na.rm = TRUE), 1), "%)\n")

if(sum(problematic_5pct, na.rm = TRUE) > 0) {
  cat("  Warning:", sum(problematic_5pct, na.rm = TRUE), 
      "polygons have buffer/original ratio >5% off from 1.0\n")
  cat("  (", round(100 * mean(problematic_5pct, na.rm = TRUE), 2), "% of total)\n")
  
  # Save list of problematic polygons
  problematic_ids <- buffer_diagnostics$polygon_id[problematic_5pct]
  saveRDS(problematic_ids, 
          here("data", "derived_data", "problematic_buffer_ids.rds"))
  cat("  List of problematic polygon IDs saved to: data/derived_data/problematic_buffer_ids.rds\n")
} else {
  cat("  ✓ All buffer areas within 5% of original areas\n")
}

# Create summary plot of buffer accuracy
png(filename = here("figures", "FigureS_buffer_creation_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)

par(mfrow = c(2, 3))

# Plot 1: Buffer zone ratio histogram
hist(buffer_diagnostics$buffer_zone_ratio, 
     breaks = 50,
     main = "Buffer Zone Area Ratio Distribution",
     xlab = "Buffer area / Original area",
     col = "steelblue",
     border = "white")
abline(v = 1, col = "red", lwd = 2, lty = 2)

# Plot 2: Iterations histogram
hist(buffer_diagnostics$iterations,
     breaks = max(buffer_diagnostics$iterations) - min(buffer_diagnostics$iterations) + 1,
     main = "Iterations Required",
     xlab = "Number of iterations",
     col = "steelblue",
     border = "white")

# Plot 3: Buffer distance vs polygon area
plot(development_polygons_filtered$area_m2_numeric, 
     buffer_diagnostics$buffer_distance_m,
     pch = 16, cex = 0.3, col = rgb(0, 0, 0, 0.3),
     main = "Buffer Distance vs Polygon Area",
     xlab = "Polygon area (m²)",
     ylab = "Buffer distance (m)",
     log = "xy")

# Plot 4: Buffer zone ratio vs polygon area
plot(development_polygons_filtered$area_m2_numeric, 
     buffer_diagnostics$buffer_zone_ratio,
     pch = 16, cex = 0.3, col = rgb(0, 0, 0, 0.3),
     main = "Buffer Accuracy vs Polygon Area",
     xlab = "Polygon area (m²)",
     ylab = "Buffer area / Original area",
     log = "x")
abline(h = 1, col = "red", lwd = 2, lty = 2)
abline(h = c(0.95, 1.05), col = "orange", lwd = 1, lty = 2)

# Plot 5: Buffered area ratio
hist(buffer_diagnostics$buffered_area_ratio, 
     breaks = 50,
     main = "Buffered Polygon Accuracy",
     xlab = "Buffered area / Target area (2x)",
     col = "steelblue",
     border = "white")
abline(v = 1, col = "red", lwd = 2, lty = 2)

# Plot 6: Iterations vs polygon area
plot(development_polygons_filtered$area_m2_numeric, 
     buffer_diagnostics$iterations,
     pch = 16, cex = 0.3, col = rgb(0, 0, 0, 0.3),
     main = "Iterations vs Polygon Area",
     xlab = "Polygon area (m²)",
     ylab = "Iterations required",
     log = "x")

par(mfrow = c(1, 1))
dev.off()

cat("\nDiagnostic figure saved to: figures/FigureS_buffer_creation_diagnostics.png\n")

cat("\nBuffer creation and verification complete!\n\n")

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
  filter(ecotype %in% 1:7)

## 3.2. Extract dominant land-cover for polygons and buffers -------------------

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

# Remove orphaned buffers (buffers without matching development polygons)
# This occurs when development polygons have no terrestrial land cover
cat("\nChecking for orphaned buffers...\n")
dev_ids <- polygon_data$polygon_id
buf_ids <- buffer_data$polygon_id
orphaned_buffers <- setdiff(buf_ids, dev_ids)

if (length(orphaned_buffers) > 0) {
  cat("  Found", length(orphaned_buffers), "orphaned buffers (no matching development polygon)\n")
  cat("  Removing orphaned buffers to ensure proper pairing...\n")
  model_data <- model_data |>
    filter(polygon_id %in% dev_ids)
  cat("  Orphaned buffers removed.\n")
} else {
  cat("  No orphaned buffers found - all pairs properly matched.\n")
}

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

# 5. CHECK DATA WAS COMBINED CORRECTLY -----------------------------------------

## 5.1. Check basic structure --------------------------------------------------

# Check dimensions
nrow(model_data)
ncol(model_data)
colnames(model_data)

# Break down by polygon type
print(table(model_data$polygon_type))

# Check how many development/buffer polygons there are
if (n_dev == n_buf) {
  cat("PASS: Equal Development and Buffer rows (", n_dev, "each)\n")
  has_pairing_issue <- FALSE
} else {
  cat("FAIL: Unequal rows - Development:", n_dev, ", Buffer:", n_buf, "\n")
  has_pairing_issue <- TRUE
}

## 5.2. Check pairing of development polygons and buffers ----------------------

# Check how many pairs there are
pair_counts <- model_data |>
  group_by(pair_id) |>
  summarise(n_rows = n(),
            n_dev = sum(polygon_type == "Development"),
            n_buf = sum(polygon_type == "Buffer"),
            .groups = "drop")

# Check if all pair_ids have 2 rows
if (all(pair_counts$n_rows == 2)) {
  cat("PASS: All pair_ids have exactly 2 rows\n")
} else {
  bad_pairs <- pair_counts |> filter(n_rows != 2)
  cat("FAIL:", nrow(bad_pairs), "pair_ids don't have 2 rows\n")
}

# Check if all pair_ids have 1 buffer + 1 development polygon
if (all(pair_counts$n_dev == 1 & pair_counts$n_buf == 1)) {
  cat("PASS: All pairs have 1 Development + 1 Buffer\n")
} else {
  bad_pairs <- pair_counts |> filter(n_dev != 1 | n_buf != 1)
  cat("FAIL:", nrow(bad_pairs), "pairs don't have correct composition\n")
}

# Check polygon_id matching
dev_ids <- model_data |> filter(polygon_type == "Development") |> pull(polygon_id) |> sort()
buf_ids <- model_data |> filter(polygon_type == "Buffer") |> pull(polygon_id) |> sort()

if (identical(dev_ids, buf_ids)) {
  cat("PASS: Development and Buffer have matching polygon_id sets\n")
  orphaned_buffer_ids <- integer(0)
} else {
  orphaned_buffer_ids <- setdiff(buf_ids, dev_ids)
  cat("FAIL:", length(orphaned_buffer_ids), "orphaned buffers (in Buffer but not Development)\n")
}

## 5.3. Check data integrity ---------------------------------------------------

# Define a list of key columns
critical_cols <- c("polygon_id", "pair_id", "polygon_type", "area_m2_numeric",
                   "kommune", "n_occurrences", "n_species", "log_area")

# Run through columns and check for missing values
all_clean <- TRUE
for (col in critical_cols) {
  n_missing <- sum(is.na(model_data[[col]]))
  if (n_missing == 0) {
    cat("  ✓", col, "\n")
  } else {
    cat("  ✗", col, ":", n_missing, "missing\n")
    all_clean <- FALSE
  }
}

# Check which values are missing
n_missing_lc <- sum(is.na(model_data$land_cover_name))

## 5.4. Check orphaned buffers -------------------------------------------------

# Check the buffers that do not have any development polygons associated with them
if (has_pairing_issue && length(orphaned_buffer_ids) > 0) {
  
 # extract buffers without development polygons
  orphaned_buffers <- model_data |>
    filter(polygon_type == "Buffer", polygon_id %in% orphaned_buffer_ids)
  
  # check if orphaned buffers have NA for land-cover
  cat("  Land-cover (non-NA):", sum(!is.na(orphaned_buffers$land_cover_name)), 
      "out of", nrow(orphaned_buffers), "\n")
  
  # extract the land-cover of orphaned buffers
  if (sum(!is.na(orphaned_buffers$land_cover_name)) > 0) {
    cat("\n  Land-cover types:\n")
    lc_table <- table(orphaned_buffers$land_cover_name)
    for (i in seq_along(lc_table)) {
      cat("    ", names(lc_table)[i], ":", lc_table[i], "\n")
    }
  }
  
  # load original polygons to check
  development_polygons <- st_read(here("data", "raw_data", "nina_planagt.gpkg"),
                                  quiet = TRUE)
  
  # filter out marine land-cover categories
  development_polygons_filtered <- development_polygons |>
    filter(arealformalsgruppe != "16 Havner og småbåthavner") |>
    mutate(polygon_id = row_number())
  
  # filter out polygons that do are not in the orphaned polygons list
  missing_dev_polygons <- development_polygons_filtered |>
    filter(polygon_id %in% orphaned_buffer_ids)
  
  # extract land cover for missing polygons using ALL categories
  gdb_path <- here("data", "raw_data", "Hovedokosystem_nedlasting", "Hovedokosystem.gdb")
  land_cover_all <- st_read(gdb_path, layer = "Hovedøkosystem", quiet = TRUE)
  
  # check if CRS is the same
  if (st_crs(land_cover_all)$epsg != 25833) {
    land_cover_all <- st_transform(land_cover_all, 25833)
  }
  
  # rename land-cover categories
  land_cover_all <- land_cover_all |>
    mutate(land_cover_category = case_when(ecotype %in% 1:6 ~ "Terrestrial",
                                           ecotype == 7 ~ "Wetlands",
                                           ecotype == 8 ~ "Rivers",
                                           ecotype == 9 ~ "Lakes",
                                           ecotype %in% 10:12 ~ "Marine",
                                           TRUE ~ "Unknown"))
  
  # add detailed land cover names
  land_cover_all <- land_cover_all |>
    mutate(land_cover_name_detailed = case_when(ecotype == 1 ~ "1_Settlements",
                                                ecotype == 2 ~ "2_Cropland",
                                                ecotype == 3 ~ "3_Grassland",
                                                ecotype == 4 ~ "4_Forest",
                                                ecotype == 5 ~ "5_Heathland",
                                                ecotype == 6 ~ "6_Sparsely_vegetated",
                                                ecotype == 7 ~ "7_Wetlands",
                                                ecotype == 8 ~ "8_Rivers",
                                                ecotype == 9 ~ "9_Lakes",
                                                ecotype == 10 ~ "10_Marine_inlets",
                                                ecotype == 11 ~ "11_Coastal",
                                                ecotype == 12 ~ "12_Marine_offshore",
                                                TRUE ~ "Unknown"))
  
  # extract dominant land cover
  extract_dominant_landcover <- function(polygons, land_cover_data) {
    intersection <- st_intersection(polygons, land_cover_data)
    if (nrow(intersection) == 0) {
      return(data.frame(polygon_id = integer(), ecotype = integer(), 
                        land_cover_category = character(),
                        land_cover_name_detailed = character()))
    }
    intersection |>
      mutate(intersection_area = as.numeric(st_area(intersection))) |>
      st_drop_geometry() |>
      group_by(polygon_id) |>
      slice_max(intersection_area, n = 1, with_ties = FALSE) |>
      select(polygon_id, ecotype, land_cover_category, land_cover_name_detailed) |>
      ungroup()
  }
  
  tryCatch({
    missing_lc <- extract_dominant_landcover(missing_dev_polygons, land_cover_all)
    
    cat("RESULTS:\n")
    cat("  Extracted land cover for:", nrow(missing_lc), "out of", 
        nrow(missing_dev_polygons), "polygons\n\n")
    
    if (nrow(missing_lc) > 0) {
      # detailed breakdown by specific ecotype
      cat("  Detailed land cover breakdown (by ecotype):\n")
      detailed_table <- table(missing_lc$land_cover_name_detailed)
      for (i in seq_along(detailed_table)) {
        pct <- round(100 * detailed_table[i] / sum(detailed_table), 1)
        cat("    ", names(detailed_table)[i], ":", detailed_table[i], 
            "(", pct, "%)\n")
      }
      
      # broad category breakdown
      cat("\n  Broad category breakdown:\n")
      lc_breakdown <- table(missing_lc$land_cover_category)
      for (i in seq_along(lc_breakdown)) {
        pct <- round(100 * lc_breakdown[i] / sum(lc_breakdown), 1)
        cat("    ", names(lc_breakdown)[i], ":", lc_breakdown[i], 
            "(", pct, "%)\n")
      }
      
      # count by specific type
      terrestrial_count <- sum(missing_lc$ecotype %in% 1:6)
      wetlands_count <- sum(missing_lc$ecotype == 7)
      rivers_count <- sum(missing_lc$ecotype == 8)
      lakes_count <- sum(missing_lc$ecotype == 9)
      marine_count <- sum(missing_lc$ecotype %in% 10:12)
      
      cat("\n  Summary by type:\n")
      cat("    Terrestrial (1-6):", terrestrial_count, "(", 
          round(100 * terrestrial_count / nrow(missing_lc), 1), "%)\n")
      cat("    Wetlands (7):", wetlands_count, "(", 
          round(100 * wetlands_count / nrow(missing_lc), 1), "%)\n")
      cat("    Rivers (8):", rivers_count, "(", 
          round(100 * rivers_count / nrow(missing_lc), 1), "%)\n")
      cat("    Lakes (9):", lakes_count, "(", 
          round(100 * lakes_count / nrow(missing_lc), 1), "%)\n")
      cat("    Marine (10-12):", marine_count, "(", 
          round(100 * marine_count / nrow(missing_lc), 1), "%)\n")
      
      cat("\n")
      if (terrestrial_count == 0) {
        cat("✓ DIAGNOSIS CONFIRMED:\n")
        cat("  All", nrow(missing_lc), "missing development polygons have\n")
        cat("  NO terrestrial land cover (ecotypes 1-6).\n")
        cat("  They contain only:\n")
        if (wetlands_count > 0) cat("    • Wetlands:", wetlands_count, "\n")
        if (rivers_count > 0) cat("    • Rivers:", rivers_count, "\n")
        if (lakes_count > 0) cat("    • Lakes:", lakes_count, "\n")
        if (marine_count > 0) cat("    • Marine/coastal:", marine_count, "\n")
        cat("\n  Their buffers extended into terrestrial areas → orphaned buffers.\n\n")
        cat("  RECOMMENDATION: Safe to remove orphaned buffers.\n")
        cat("                  Run fix_orphaned_buffers.R\n")
      } else {
        cat("⚠ WARNING:\n")
        cat("  ", terrestrial_count, "polygons have terrestrial land cover\n")
        cat("  but are missing. Further investigation needed.\n")
      }
      
      # Save detailed results
      saveRDS(missing_lc,
              here("data", "derived_data", "missing_polygons_landcover_diagnosis.rds"))
      cat("\nDetailed results saved to:\n")
      cat("  data/derived_data/missing_polygons_landcover_diagnosis.rds\n")
    }
  }, error = function(e) {
    cat("  Error during extraction:", e$message, "\n")
  })
  
} else {
  
}

# END OF SCRIPT ----------------------------------------------------------------