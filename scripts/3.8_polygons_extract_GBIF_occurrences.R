##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 3.8_polygons_extract_GBIF_occurrences
# This script contains code to extract the GBIF species occurrence records for 
# development polygons and buffers
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source the setup file
library(here)
source(here("scripts", "0_setup.R"))

# Load the combined polygon and buffer object created in 3.5
polygon_buffer_data <- readRDS(here("data", "derived_data",
                                    "polygon_buffer_data.rds"))

# Load the cleaned GBIF occurrences
clean_occurrences <- read.csv(here("data", "derived_data",
                                   "clean_occurrences_1km.txt"))

# 2. CHECK INPUT  --------------------------------------------------------------

# Check to make sure that the polygon + buffer output from script 3.5 is correct
stopifnot(inherits(polygon_buffer_data, "sf"),
          all(c("id", "pair_id", "polygon_type", "area_m2_numeric",
                "english_categories", "kommune", "kommune_factor",
                "land_cover_name", "log_area") %in% names(polygon_buffer_data)),
          all(table(polygon_buffer_data$polygon_type) > 0),   # both types present
          !any(is.na(polygon_buffer_data$log_area)))

# Check the number of rows in the loaded object
cat("Loaded combined object:", nrow(polygon_buffer_data), "rows\n") #259762
print(table(polygon_buffer_data$polygon_type))
# Buffer Development 
# 129881      129881 

# Check that id is unique whin each polygon type
stopifnot(anyDuplicated(polygon_buffer_data$id[polygon_buffer_data$polygon_type == "Development"]) == 0,
          anyDuplicated(polygon_buffer_data$id[polygon_buffer_data$polygon_type == "Buffer"]) == 0)

# Build a unique per-row key
polygon_buffer_data <- polygon_buffer_data |>
  mutate(poly_uid = paste(polygon_type, id, sep = "_"))

# Check that the poly_uid is unique across the whole object
stopifnot(anyDuplicated(polygon_buffer_data$poly_uid) == 0)
cat("Unique row keys (poly_uid):", n_distinct(polygon_buffer_data$poly_uid), "\n") # 259762

# 3. PREPARE SPECIES OCCURRENCE RECORDS ----------------------------------------

# Convert occurrences to spatial points (GBIF coordinates are WGS84 / EPSG:4326)
occurrences_sf <- clean_occurrences |>
  filter(!is.na(decimalLongitude), !is.na(decimalLatitude)) |>
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# Transform occurrences to the CRS of the polygons
occurrences_sf <- st_transform(occurrences_sf, st_crs(polygon_buffer_data))

# Check that CRS now matches and the extents overlap 
stopifnot(st_crs(occurrences_sf) == st_crs(polygon_buffer_data))

if (length(st_intersection(st_as_sfc(st_bbox(occurrences_sf)),
                           st_as_sfc(st_bbox(polygon_buffer_data)))) == 0) {
  stop("ERROR: occurrence and polygon bounding boxes do not overlap")
} else {
  cat("PASS: occurrence and polygon extents overlap\n")
} # PASS

cat("Occurrence points with valid coordinates:", nrow(occurrences_sf), "\n") # 18,811,161

# 4. JOIN OCCURRENCES TO POLYGONS & BUFFERS ------------------------------------

# The spatial join will give two outputs: the counts of the occurrences (for H2a,b)
# and an occurrence-level object for H2d
# Keeping the polygons and buffers that do not have any occurrences
# Only keeping the key + the occurrence field to keep this object as small and
# easy to run as possible and will add polygon metadata after
cat("\nJoining occurrences to polygons and buffers (this is the slow step)...\n")

occurrence_join <- st_join(polygon_buffer_data |> 
                             dplyr::select(poly_uid),
                           occurrences_sf |> 
                             dplyr::select(gbifID, species, year, parentEventID),
                           join = st_intersects,
                           left = TRUE) |>
  st_drop_geometry()

# Check how mnay rows we have
cat("Join produced", nrow(occurrence_join), "occurrence-polygon rows\n")

# 5. COUNT OCCURRENCES AND SPECIES PER POLYGON AND BUFFER ----------------------

# One row per polygon/buffer. For a polygon with no occurrences the join gave a
# single NA row, so n_occurrences = 0, n_species = 0, species_list = empty list.
occurrence_counts <- occurrence_join |>
  group_by(poly_uid) |>
  summarise(n_occurrences = sum(!is.na(gbifID)),
            n_species = n_distinct(species[!is.na(species)]),
            species_list = list(unique(species[!is.na(species)])),
            .groups = "drop")

# Check that there is exactly one count row per polygon/buffer
stopifnot(nrow(occurrence_counts) == nrow(polygon_buffer_data),
          anyDuplicated(occurrence_counts$poly_uid) == 0)

# Attach the counts to the full metadata
model_data <- polygon_buffer_data |>
  st_drop_geometry() |>
  left_join(occurrence_counts, by = "poly_uid")

# Check the join added countrs without adding or losing rows, and every row got its counts
# (i.e. there were no NAs introduced by a key mismatch)
stopifnot(nrow(model_data) == nrow(polygon_buffer_data),
          !any(is.na(model_data$n_occurrences)),
          !any(is.na(model_data$n_species)))
cat("\nModel data assembled:", nrow(model_data), "rows\n")

# 6. BUILD OCCURRENCE-LEVEL OBJECT FOR H2D -------------------------------------

# One row per occurrernce per polygon/buffer, with year and parentEventID for 
# completeness calculations and re-attache the poulygon metadata to the join from section 4
polygon_buffer_occurrence_join <- occurrence_join |>
  left_join(polygon_buffer_data |>
              st_drop_geometry() |>
              dplyr::select(poly_uid, id, pair_id, polygon_type,
                            area_m2_numeric, english_categories, kommune,
                            land_cover_name),
            by = "poly_uid")

# Check that no rows were gained or lost re-attaching metadata
stopifnot(nrow(polygon_buffer_occurrence_join) == nrow(occurrence_join))

# 7. CHECK THE RESULTS ---------------------------------------------------------

## 7.1. Check the data ---------------------------------------------------------

# Check that both polygon types survived
n_dev <- sum(model_data$polygon_type == "Development")
n_buf <- sum(model_data$polygon_type == "Buffer")

if (n_dev == n_buf && n_buf > 0) {
  cat("PASS: equal Development and Buffer rows (", n_dev, "each)\n")
} else {
  cat("FAIL: Development:", n_dev, " Buffer:", n_buf, "\n")
}

# Check that the pairing is still correct
pair_counts <- model_data |>
  group_by(pair_id) |>
  summarise(n_rows = n(),
            n_dev  = sum(polygon_type == "Development"),
            n_buf  = sum(polygon_type == "Buffer"),
            .groups = "drop")

if (all(pair_counts$n_rows == 2) &&
    all(pair_counts$n_dev == 1 & pair_counts$n_buf == 1)) {
  cat("PASS: every pair has exactly 1 Development + 1 Buffer\n")
} else {
  cat("FAIL:", nrow(pair_counts |> filter(n_rows != 2 | n_dev != 1 | n_buf != 1)),
      "pairs have incorrect composition\n")
}

# Check if there are any NA values in the variables that will be used in the models
cat("\nNA in key modelling columns:\n")
cat("  kommune_factor:", sum(is.na(model_data$kommune_factor)), "\n")
cat("  land_cover_name:", sum(is.na(model_data$land_cover_name)), "\n")
cat("  log_area:", sum(is.na(model_data$log_area)), "\n")

## 7.2. Summary statistics -----------------------------------------------------

cat("\n=== SUMMARY STATISTICS ===\n")
cat("Development - mean occurrences:",
    round(mean(model_data$n_occurrences[model_data$polygon_type == "Development"]), 2),
    " mean species:",
    round(mean(model_data$n_species[model_data$polygon_type == "Development"]), 2), "\n")
cat("Buffer      - mean occurrences:",
    round(mean(model_data$n_occurrences[model_data$polygon_type == "Buffer"]), 2),
    " mean species:",
    round(mean(model_data$n_species[model_data$polygon_type == "Buffer"]), 2), "\n")

cat("\nLand cover x polygon type:\n")
print(table(model_data$land_cover_name, model_data$polygon_type))

# 8. SAVE OUTPUT ---------------------------------------------------------------

# Model-ready dataset (one row per polygon/buffer) - used by H2a, H2b, H2c
saveRDS(model_data,
        here("data", "derived_data", "h2_polygon_buffer_data.rds"))

# Occurrence-level dataset (one row per occurrence per polygon/buffer) - H2d
saveRDS(polygon_buffer_occurrence_join,
        here("data", "derived_data", "h2d_polygon_buffer_occurrence_join.rds"))

# Check that both files were written and read back with the expected row counts
stopifnot(file.exists(here("data", "derived_data", "h2_polygon_buffer_data.rds")),
          file.exists(here("data", "derived_data", "h2d_polygon_buffer_occurrence_join.rds")),
          nrow(readRDS(here("data", "derived_data", "h2_polygon_buffer_data.rds"))) == nrow(polygon_buffer_data)
)

# END OF SCRIPT ----------------------------------------------------------------