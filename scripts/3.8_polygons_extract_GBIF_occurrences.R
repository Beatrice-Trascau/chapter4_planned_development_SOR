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
                                   "clean_occurrences_1km.txt"))[,
                                                                 c("gbifID", "species", "year", "parentEventID",
                                                                   "decimalLongitude", "decimalLatitude")]

clean_occurrences <- data.table::fread(here("data", "derived_data", "clean_occurrences_1km.txt"),
                                       select = c("gbifID","species","year","parentEventID",
                                                  "decimalLongitude","decimalLatitude")) |> 
  as.data.frame()

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

# Remove the occurrence data frame to free up space
rm(clean_occurrences)
gc()

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

# Set chunk size
chunk_size <- 2000
n_chunks   <- ceiling(nrow(polygon_buffer_data) / chunk_size)

# Create list to store results in
counts_list <- vector("list", n_chunks)
h2d_list    <- vector("list", n_chunks)

# The spatial join will give two outputs: the counts of the occurrences (for H2a,b)
# and an occurrence-level object for H2d
# Keeping the polygons and buffers that do not have any occurrences
# Only keeping the key + the occurrence field to keep this object as small and
# easy to run as possible and will add polygon metadata after
cat("\nJoining occurrences to", nrow(polygon_buffer_data),
    "polygons/buffers in", n_chunks, "chunks of", chunk_size, "...\n")

overall_start <- Sys.time()

for (i in seq_len(n_chunks)) {
  
  start_idx <- (i - 1) * chunk_size + 1
  end_idx   <- min(i * chunk_size, nrow(polygon_buffer_data))
  
  # this chunk of polygons/buffers, carrying only the key
  chunk <- polygon_buffer_data[start_idx:end_idx, ] |>
    dplyr::select(poly_uid)
  
  # join occurrences to this chunk (left = TRUE keeps zero-occurrence polygons)
  joined_chunk <- st_join(chunk,
                          occurrences_sf,
                          join = st_intersects,
                          left = TRUE) |>
    st_drop_geometry()
  
  # reduce to per-polygon counts immediately (this is what bounds memory)
  counts_list[[i]] <- joined_chunk |>
    group_by(poly_uid) |>
    summarise(n_occurrences = sum(!is.na(gbifID)),
              n_species     = n_distinct(species[!is.na(species)]),
              species_list  = list(unique(species[!is.na(species)])),
              .groups = "drop")
  
  # keep only MATCHED rows for the H2d occurrence-level object. Zero-occurrence
  # polygons contribute nothing to completeness, so dropping their NA rows here
  # is both correct and a large memory saving.
  h2d_list[[i]] <- joined_chunk |>
    filter(!is.na(gbifID)) |>
    dplyr::select(poly_uid, gbifID, species, year, parentEventID)
  
  # discard the big intermediate and reclaim memory before the next chunk
  rm(chunk, joined_chunk)
  
  if (i %% 5 == 0 || i == 1 || i == n_chunks) {
    elapsed <- round(as.numeric(difftime(Sys.time(), overall_start, units = "mins")), 1)
    cat("  chunk", i, "of", n_chunks, "done (", elapsed, "min elapsed )\n")
    gc()
  }
}

# Check how mnay rows we have
cat("Chunked join complete in",
    round(as.numeric(difftime(Sys.time(), overall_start, units = "mins")), 1),
    "minutes\n")

# 5. ASSEMBLE PER-POLYGON MODEL DATA -------------------------------------------

# Combine the per-chunk counts
occurrence_counts <- bind_rows(counts_list)
rm(counts_list); gc()

# Check that there is exactly one count row per polygon or buffer
stopifnot(nrow(occurrence_counts) == nrow(polygon_buffer_data),
          anyDuplicated(occurrence_counts$poly_uid) == 0)

# Attach counts to the full metadata (it does not need the geometry)
model_data <- polygon_buffer_data |>
  st_drop_geometry() |>
  left_join(occurrence_counts, by = "poly_uid")

# Check that the counts were added without gaining or losing rows and without causing key mismatch
stopifnot(nrow(model_data) == nrow(polygon_buffer_data),
          !any(is.na(model_data$n_occurrences)),
          !any(is.na(model_data$n_species)))
cat("\nModel data assembled:", nrow(model_data), "rows\n")

# 6. BUILD OCCURRENCE-LEVEL OBJECT FOR H2D -------------------------------------

# Combine the matched occurrence rows and then re-attach polygon metadata
polygon_buffer_occurrence_join <- bind_rows(h2d_list)
rm(h2d_list); gc()

polygon_buffer_occurrence_join <- polygon_buffer_occurrence_join |>
  left_join(polygon_buffer_data |>
              st_drop_geometry() |>
              dplyr::select(poly_uid, id, pair_id, polygon_type,
                            area_m2_numeric, english_categories, kommune,
                            land_cover_name),
            by = "poly_uid")

# Check how many rows we have
cat("H2d occurrence-level rows:", nrow(polygon_buffer_occurrence_join), "\n")

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