##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 4.3_H2d_models
# This script contains code to test Hypothesis 2d: Area plan polygons will 
# have higher sampling completeness than areas outside of planned developments
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

library(here)
source(here("scripts", "0_setup.R"))

# Load polygon/buffer summary data
model_data <- readRDS(here("data", "derived_data", "h2_polygon_buffer_data.rds"))

# Load occurrence-level data with year and parentEventID
# This file is created at the end of script 3.5
polygon_occurrence_join <- readRDS(here("data", "derived_data", 
                                        "h2d_polygon_buffer_occurrence_join.rds"))

# 2. PREPARE DATA FOR COMPLETENESS CALCULATIONS --------------------------------

# Remove orphaned buffers from model_data
dev_ids <- model_data |> filter(polygon_type == "Development") |> pull(polygon_id)
model_data <- model_data |> filter(polygon_id %in% dev_ids)

# Create data for Chao1 (abundance-based - occurrences per species)
chao1_data <- polygon_occurrence_join |>
  st_drop_geometry() |>
  filter(!is.na(gbifID)) |>
  group_by(polygon_id, species) |>
  summarise(n_occurrences = n(), .groups = "drop")

# Create data for Chao2 and ICE with Time (years as sampling units)
time_data <- polygon_occurrence_join |>
  st_drop_geometry() |>
  filter(!is.na(gbifID), !is.na(year)) |>
  distinct(polygon_id, species, year) |>
  group_by(polygon_id, species) |>
  summarise(n_years = n(), .groups = "drop")

# Create data for Chao2 and ICE with Events (parentEventID as sampling units)
event_data <- polygon_occurrence_join |>
  st_drop_geometry() |>
  filter(!is.na(gbifID), !is.na(parentEventID)) |>
  distinct(polygon_id, species, parentEventID) |>
  group_by(polygon_id, species) |>
  summarise(n_events = n(), .groups = "drop")

# 3. CALCULATE COMPLETENESS ESTIMATES ------------------------------------------

## 3.1. Chao1 ------------------------------------------------------------------

# Calculate Chao1
chao1_results <- chao1_data |>
  group_by(polygon_id) |>
  summarise(n_species_obs = n(),
            n_occurrences_total = sum(n_occurrences),
            # singletons and doubletons based on occurrence counts
            f1 = sum(n_occurrences == 1),
            f2 = sum(n_occurrences == 2),
            # Chao1 estimate
            chao1 = if_else(f2 > 0,
                            n_species_obs + (f1^2 / (2 * f2)),
                            n_species_obs + (f1 * (f1 - 1) / 2)),
            # completeness
            completeness_chao1 = n_species_obs / chao1,
            .groups = "drop")

## 3.2. Chao2 with Time (incidence-based) --------------------------------------

# First, get the total number of years sampled per polygon
n_years_per_polygon <- time_data |>
  group_by(polygon_id) |>
  summarise(total_years = n_distinct(year), .groups = "drop")

chao2_time_results <- time_data |>
  group_by(polygon_id) |>
  summarise(n_species_obs = n(),
            # uniques and duplicates (species in 1 vs 2 years)
            Q1 = sum(n_years == 1),
            Q2 = sum(n_years == 2),
            .groups = "drop") |>
  left_join(n_years_per_polygon, by = "polygon_id") |>
  # Chao2 estimate
  mutate(chao2_time = if_else(Q2 > 0,
                              n_species_obs + ((total_years - 1) / total_years) * 
                                (Q1^2 / (2 * Q2)),
                              n_species_obs + ((total_years - 1) / total_years) * 
                                (Q1 * (Q1 - 1) / 2)),
         # completeness
         completeness_chao2_time = n_species_obs / chao2_time) |>
  rename(n_years = total_years)

## 3.3. Chao2 with Events (incidence-based) ------------------------------------

# First, get the total number of events per polygon
n_events_per_polygon <- event_data |>
  group_by(polygon_id) |>
  summarise(total_events = n_distinct(parentEventID), .groups = "drop")

chao2_event_results <- event_data |>
  group_by(polygon_id) |>
  summarise(n_species_obs = n(),
            # uniques and duplicates (species in 1 vs 2 events)
            Q1 = sum(n_events == 1),
            Q2 = sum(n_events == 2),
            .groups = "drop") |>
  left_join(n_events_per_polygon, by = "polygon_id") |>
  # Chao2 estimate
  mutate(chao2_event = if_else(Q2 > 0,
                               n_species_obs + ((total_events - 1) / total_events) * 
                                 (Q1^2 / (2 * Q2)),
                               n_species_obs + ((total_events - 1) / total_events) * 
                                 (Q1 * (Q1 - 1) / 2)),
         # completeness
         completeness_chao2_event = n_species_obs / chao2_event) |>
  rename(n_events = total_events)
