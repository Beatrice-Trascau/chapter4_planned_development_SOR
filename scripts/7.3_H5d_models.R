##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 7.3_H5d_models
# This script contains code to test H4d: Polygons will have higher sampling
# completeness of alien species than areas outside (buffers)

# N.B. Alien species records are quite sparse so estimators that rely on 
# singletons and doubletons (Chao1) are noisier
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source scripts
library(here)
source(here("scripts", "0_setup.R"))

# Load the alien per-side data and occurrence-level join, built in 7.1
model_data <- readRDS(here("data", "derived_data", "h5_polygon_buffer_data.rds"))
polygon_occurrence_join <- readRDS(here("data", "derived_data",
                                        "h5_polygon_buffer_occurrence_join.rds"))

# Use function to display land-cover labels neatly
pretty_lc <- function(x) {
  x <- gsub("_", " ", x)
  gsub("(^|\\s)([a-z])", "\\1\\U\\2", x, perl = TRUE)
}

# 2. PREPARE DATA FOR THE COMPLETENESS CALCULATIONS ----------------------------

# Reshape data so Chao1 can be calculated (pseudo-abundance = records per species)
chao1_data <- polygon_occurrence_join |>
  filter(!is.na(gbifID)) |>
  group_by(poly_uid, species) |>
  summarise(n_occurrences = n(), .groups = "drop")

# Data for ICE with Time (use years as sampling units)
time_data <- polygon_occurrence_join |>
  filter(!is.na(gbifID), !is.na(year)) |>
  distinct(poly_uid, species, year) |>
  group_by(poly_uid, species) |>
  summarise(n_years = n(), .groups = "drop")

# Calculated total years per side
n_years_per_side <- polygon_occurrence_join |>
  filter(!is.na(gbifID), !is.na(year)) |>
  group_by(poly_uid) |>
  summarise(total_years = n_distinct(year), .groups = "drop")

# 3. CALCULATE COMPLETENESS ESTIMATES ------------------------------------------

# Calculate Chao1
chao1_results <- chao1_data |>
  group_by(poly_uid) |>
  summarise(n_species_obs = n(),
            n_occurrences_total = sum(n_occurrences),
            f1 = sum(n_occurrences == 1),   # singletons
            f2 = sum(n_occurrences == 2),   # doubletons
            chao1 = if_else(f2 > 0,
                            n_species_obs + (f1^2 / (2 * f2)),
                            n_species_obs + (f1 * (f1 - 1) / 2)),
            completeness_chao1 = n_species_obs / chao1,
            .groups = "drop")
