##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 3.9_polygon_GBIF_quality_check
# This script contains code to extract the GBIF species occurrence records for 
# development polygons and buffers
# N.B: the spatial join is done in chunks and processed on 4 cores in parallel
##----------------------------------------------------------------------------##

# 1. SETUP ---------------------------------------------------------------------

# Load libraries
library(here)
source(here("scripts", "0_setup.R"))

# Load data
model_data <- readRDS(here("data", "derived_data", "h2_polygon_buffer_data.rds"))
h2d        <- readRDS(here("data", "derived_data",
                           "h2d_polygon_buffer_occurrence_join.rds"))

# Set up a tiny helper function to create readable pass/fail lines
report <- function(label, ok) {
  cat(if (ok) "  PASS  " else "  FAIL  ", label, "\n")
  invisible(ok)
}

# 2. CHECK THE STRUCTURAL INTEGRITY --------------------------------------------

# Check that poly_uid is the unique roq key and is complete
report("poly_uid unique in model_data",
       anyDuplicated(model_data$poly_uid) == 0) # PASS
report("poly_uid complete in model_data",
       !any(is.na(model_data$poly_uid))) # PASS

# Check that both polygon types are present and balanced
n_dev <- sum(model_data$polygon_type == "Development")
n_buf <- sum(model_data$polygon_type == "Buffer")
report(paste0("equal Development (", n_dev, ") and Buffer (", n_buf, ") rows"),
       n_dev == n_buf && n_buf > 0) # PASS

# Check that every pair has exactly one of each type
pair_counts <- model_data |>
  group_by(pair_id) |>
  summarise(n_dev = sum(polygon_type == "Development"),
            n_buf = sum(polygon_type == "Buffer"),
            .groups = "drop")
report("every pair = 1 Development + 1 Buffer",
       all(pair_counts$n_dev == 1 & pair_counts$n_buf == 1)) # PASSS

# Check that expected model_data columns are present
needed <- c("poly_uid", "id", "pair_id", "polygon_type", "area_m2_numeric",
            "english_categories", "kommune", "kommune_factor",
            "land_cover_name", "log_area", "n_occurrences", "n_species",
            "species_list")
report("all expected columns present",
       all(needed %in% names(model_data))) # PASSS

# 3. CHECK THE VALUES ----------------------------------------------------------

# Check that all counts are non-negative integers
report("n_occurrences non-negative",
       all(model_data$n_occurrences >= 0)) # PASS
report("n_species non-negative",
       all(model_data$n_species >= 0)) # PASS

# Check that we do not have more species than records
report("n_species <= n_occurrences everywhere",
       all(model_data$n_species <= model_data$n_occurrences)) # PASS

# Check that the stored species list length is equal to the species count
report("length(species_list) == n_species",
       all(lengths(model_data$species_list) == model_data$n_species)) # PASS

# Check that zero-occurrence polygons have zero species and empty lists
zero_rows <- model_data |> filter(n_occurrences == 0)
report("zero-occurrence polygons have n_species == 0",
       all(zero_rows$n_species == 0)) # PASS
report("zero-occurrence polygons have empty species_list",
       all(lengths(zero_rows$species_list) == 0)) # PASS

# 4. CHECK THAT BOTH POLYGONS AND BUFFERS ARE CONSISTENT -----------------------

# Check that every h2d poly_uid exists in model data
report("all h2d poly_uid exist in model_data",
       all(h2d$poly_uid %in% model_data$poly_uid)) # PASS

# Check that the total occurrence count is equal to the number of matched rows in h2d
report(paste0("sum(n_occurrences) [", sum(model_data$n_occurrences),
              "] == nrow(h2d) [", nrow(h2d), "]"),
       sum(model_data$n_occurrences) == nrow(h2d)) # PASS

# Check that h2d row count per poly_uid == n_occurrences
h2d_occ_counts <- h2d |> count(poly_uid, name = "n_occ_h2d")
occ_compare <- model_data |>
  dplyr::select(poly_uid, n_occurrences) |>
  left_join(h2d_occ_counts, by = "poly_uid") |>
  mutate(n_occ_h2d = ifelse(is.na(n_occ_h2d), 0L, n_occ_h2d))
report("per-polygon n_occurrences matches h2d row counts",
       all(occ_compare$n_occurrences == occ_compare$n_occ_h2d)) # PASS

# Check that species count in h2d == n_species
h2d_sp_counts <- h2d |>
  group_by(poly_uid) |>
  summarise(n_sp_h2d = n_distinct(species[!is.na(species)]), .groups = "drop")
sp_compare <- model_data |>
  dplyr::select(poly_uid, n_species) |>
  left_join(h2d_sp_counts, by = "poly_uid") |>
  mutate(n_sp_h2d = ifelse(is.na(n_sp_h2d), 0L, n_sp_h2d))
report("per-polygon n_species matches h2d species counts",
       all(sp_compare$n_species == sp_compare$n_sp_h2d)) # PASS

# Check that stored species_list equals the actual species in h2d for a subsample of polygon(s)
set.seed(1)
sample_uids <- model_data |>
  filter(n_occurrences > 0) |>
  slice_sample(n = min(50, sum(model_data$n_occurrences > 0))) |>
  pull(poly_uid)

content_ok <- vapply(sample_uids, function(u) {
  stored <- sort(unique(model_data$species_list[model_data$poly_uid == u][[1]]))
  actual <- sort(unique(h2d$species[h2d$poly_uid == u & !is.na(h2d$species)]))
  setequal(stored, actual)
}, logical(1))
report(paste0("species_list matches h2d species (", length(sample_uids),
              " sampled polygons)"),
       all(content_ok)) # PASS

# 5. CHECK THE H2D FIELDS ------------------------------------------------------

# Check that h2d only has matched rows and gbifID is nevere NA
report("no NA gbifID in h2d (matched rows only)",
       !any(is.na(h2d$gbifID))) # PASS

# Check that year sits within the range we downloaded (2008-2024)
yr <- h2d$year[!is.na(h2d$year)]
report(paste0("years within 2008-2024 (range ", min(yr), "-", max(yr), ")"),
       all(yr >= 2008 & yr <= 2024)) # PASS

# Check that we still have the metadata
report("no NA poly_uid in h2d",
       !any(is.na(h2d$poly_uid))) # PASS

# 6. CHECK THE SPATIAL PROPERTIES ----------------------------------------------

# This check recomputes the counts from a random sample directly from geometry and points using st_intersect and compares it to model_data

# Load the polygon & buffer data
polygon_buffer_data <- readRDS(here("data", "derived_data",
                                    "polygon_buffer_data.rds")) |>
  mutate(poly_uid = paste(polygon_type, id, sep = "_"))

# Read the cleaned occurrences
if (!exists("occurrences_sf")) {
  cat("  Loading occurrences for the recomputation...\n")
  occ_raw <- read.csv(here("data", "derived_data",
                           "clean_occurrences_1km.txt"))[,
                                                         c("gbifID", "species", "decimalLongitude", "decimalLatitude")]
  occurrences_sf <- occ_raw |>
    filter(!is.na(decimalLongitude), !is.na(decimalLatitude)) |>
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) |>
    st_transform(st_crs(polygon_buffer_data))
  rm(occ_raw) 
  gc()
}

# Get a random sample across the whole dataset
set.seed(42)
check_uids <- polygon_buffer_data |>
  st_drop_geometry() |>
  slice_sample(n = 40) |>
  pull(poly_uid)

check_polys <- polygon_buffer_data |> filter(poly_uid %in% check_uids)

# Compare the counts with st_intersects
hits <- st_intersects(check_polys, occurrences_sf)
independent <- data.frame(poly_uid = check_polys$poly_uid,
                          n_occ_true = lengths(hits),
                          n_sp_true  = sapply(hits, function(idx)
                            length(unique(na.omit(occurrences_sf$species[idx])))))

# Compare to the model data
recompute_compare <- model_data |>
  dplyr::select(poly_uid, n_occurrences, n_species) |>
  inner_join(independent, by = "poly_uid")

occ_match <- all(recompute_compare$n_occurrences == recompute_compare$n_occ_true)
sp_match  <- all(recompute_compare$n_species    == recompute_compare$n_sp_true)

report(paste0("n_occurrences matches independent recomputation (",
              nrow(recompute_compare), " polygons)"), occ_match) # PASS
report("n_species matches independent recomputation", sp_match) # PASS

# show any mismatches explicitly
if (!occ_match || !sp_match) {
  cat("\n  MISMATCHES:\n")
  print(recompute_compare |>
          filter(n_occurrences != n_occ_true | n_species != n_sp_true))
}

# 7. CHECK THE DISTRIBUTION ----------------------------------------------------
cat("\nn_occurrences summary:\n"); print(summary(model_data$n_occurrences))
cat("\nZero-occurrence polygons:",
    sum(model_data$n_occurrences == 0), "of", nrow(model_data),
    paste0("(", round(100 * mean(model_data$n_occurrences == 0), 1), "%)"), "\n")

# Zero-occurrence polygons: 231142 of 259762 (89%) - A lot but expected!

cat("\nMean occurrences by type:\n")
print(model_data |>
        group_by(polygon_type) |>
        summarise(mean_occ = round(mean(n_occurrences), 2),
                  mean_sp  = round(mean(n_species), 2),
                  pct_zero = round(100 * mean(n_occurrences == 0), 1),
                  .groups = "drop"))
#   polygon_type mean_occ mean_sp pct_zero
# 1 Buffer           2.76    1.25     87.1
# 2 Development      2.49    0.95     90.9

cat("\nOccurrences by development category (Development rows only):\n")
print(model_data |>
        filter(polygon_type == "Development") |>
        group_by(english_categories) |>
        summarise(n_polygons = n(),
                  mean_occ   = round(mean(n_occurrences), 1),
                  total_occ  = sum(n_occurrences),
                  .groups = "drop") |>
        arrange(desc(mean_occ)))

# english_categories n_polygons mean_occ total_occ
# 1 Defense                    25     10.3       258
# 2 Combined                 4266      8.2     35170
# 3 Services                 5568      7.6     42408
# 4 Tourism                  5313      7.4     39055
# 5 Commercial               9662      5.9     57066
# 6 Mining                   4436      2.3     10173
# 7 Residential             44009      2.2     97068
# 8 Retail                    900      1.6      1457
# 9 Recreational            55702      0.7     40831

# END OF SCRIPT ----------------------------------------------------------------