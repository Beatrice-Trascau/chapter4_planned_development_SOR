##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 6.3_H4d_models
# This script contains code to test H4d: Polygons will have higher sampling
# completeness of red-listed species than areas outside (buffers)

# N.B. Red-listed records are quite sparse so estimators that rely on 
# singletons and doubletons (Chao1) are noisier
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source the setup file
library(here)
source(here("scripts", "0_setup.R"))

# Load red-listed data built in script 6.1
model_data <- readRDS(here("data", "derived_data", "h4_polygon_buffer_data.rds"))

# Load the red-listed occurrence-level data with year and parentEventID from script 6.1
polygon_occurrence_join <- readRDS(here("data", "derived_data",
                                        "h4_polygon_buffer_occurrence_join.rds"))

# Create a function to display land-cover labels
pretty_lc <- function(x) {
  x <- gsub("_", " ", x)
  gsub("(^|\\s)([a-z])", "\\1\\U\\2", x, perl = TRUE)
}

# 2. PREPARE DATA FOR THE COMPLETENESS CALCULATIONS ----------------------------

# Reshape data so Chao1 can be calculated (i.e. using pseudo-absences)
chao1_data <- polygon_occurrence_join |>
  filter(!is.na(gbifID)) |>
  group_by(poly_uid, species) |>
  summarise(n_occurrences = n(), .groups = "drop")

# Create the data for Chao2 and ICE with Time (year = sampling units)
time_data <- polygon_occurrence_join |>
  filter(!is.na(gbifID), !is.na(year)) |>
  distinct(poly_uid, species, year) |>
  group_by(poly_uid, species) |>
  summarise(n_years = n(), .groups = "drop")

# Crate the data for Chao2 and ICE with Events (parentEventID = sampling units)
event_data <- polygon_occurrence_join |>
  filter(!is.na(gbifID), !is.na(parentEventID)) |>
  distinct(poly_uid, species, parentEventID) |>
  group_by(poly_uid, species) |>
  summarise(n_events = n(), .groups = "drop")

# Get total years events per polygon and buffer
n_years_per_side <- polygon_occurrence_join |>
  filter(!is.na(gbifID), !is.na(year)) |>
  group_by(poly_uid) |>
  summarise(total_years = n_distinct(year), .groups = "drop")

# Get total events per polygon and buffer
n_events_per_side <- polygon_occurrence_join |>
  filter(!is.na(gbifID), !is.na(parentEventID)) |>
  group_by(poly_uid) |>
  summarise(total_events = n_distinct(parentEventID), .groups = "drop")

# 3. CALCULATE COMPLETENESS ESTIMATES ------------------------------------------

## 3.1. Chao1 ------------------------------------------------------------------































