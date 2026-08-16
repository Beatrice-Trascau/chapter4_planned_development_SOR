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

# Calcualte Chao 1
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

# Calcualte Chao 2 with time
chao2_time_results <- time_data |>
  group_by(poly_uid) |>
  summarise(n_species_obs = n(),
            Q1 = sum(n_years == 1),
            Q2 = sum(n_years == 2),
            .groups = "drop") |>
  left_join(n_years_per_side, by = "poly_uid") |>
  mutate(chao2_time = if_else(Q2 > 0,
                              n_species_obs + ((total_years - 1) / total_years) *
                                (Q1^2 / (2 * Q2)),
                              n_species_obs + ((total_years - 1) / total_years) *
                                (Q1 * (Q1 - 1) / 2)),
         completeness_chao2_time = n_species_obs / chao2_time) |>
  rename(n_years = total_years)

# Calculate Chao2 with events
chao2_event_results <- event_data |>
  group_by(poly_uid) |>
  summarise(n_species_obs = n(),
            Q1 = sum(n_events == 1),
            Q2 = sum(n_events == 2),
            .groups = "drop") |>
  left_join(n_events_per_side, by = "poly_uid") |>
  mutate(chao2_event = if_else(Q2 > 0,
                               n_species_obs + ((total_events - 1) / total_events) *
                                 (Q1^2 / (2 * Q2)),
                               n_species_obs + ((total_events - 1) / total_events) *
                                 (Q1 * (Q1 - 1) / 2)),
         completeness_chao2_event = n_species_obs / chao2_event) |>
  rename(n_events = total_events)

# Calculate ICE with time
ice_time_results <- time_data |>
  group_by(poly_uid) |>
  summarise(n_species_obs = n(),
            Q1 = sum(n_years == 1),
            Q2 = sum(n_years == 2),
            .groups = "drop") |>
  left_join(n_years_per_side, by = "poly_uid") |>
  rename(n_years = total_years) |>
  mutate(C_ice = 1 - (Q1 / n_years),
         ice_time = if_else(Q2 > 0,
                            {
                              gamma_ice_val <- pmax(((n_species_obs / C_ice) * (Q1 / n_years) *
                                                       ((n_years - 1) * Q1 / ((n_years - 1) * Q1 + 2 * Q2))) - 1, 0)
                              n_species_obs + (Q1 / C_ice) * gamma_ice_val
                            },
                            n_species_obs + (Q1 * (Q1 - 1) / 2)),
         completeness_ice_time = n_species_obs / ice_time,
         sample_coverage_time  = C_ice)

# Calcualte ICE with events
ice_event_results <- event_data |>
  group_by(poly_uid) |>
  summarise(n_species_obs = n(),
            Q1 = sum(n_events == 1),
            Q2 = sum(n_events == 2),
            .groups = "drop") |>
  left_join(n_events_per_side, by = "poly_uid") |>
  rename(n_events = total_events) |>
  mutate(C_ice = 1 - (Q1 / n_events),
         ice_event = if_else(Q2 > 0,
                             {
                               gamma_ice_val <- pmax(((n_species_obs / C_ice) * (Q1 / n_events) *
                                                        ((n_events - 1) * Q1 / ((n_events - 1) * Q1 + 2 * Q2))) - 1, 0)
                               n_species_obs + (Q1 / C_ice) * gamma_ice_val
                             },
                             n_species_obs + (Q1 * (Q1 - 1) / 2)),
         completeness_ice_event = n_species_obs / ice_event,
         sample_coverage_event  = C_ice)

# 4. COMBINE ALL COMPLETENESS ESTIMATES ----------------------------------------

# Base per-side data (N.B. n_species/n_occurrences are Red-listed occurrences here)
completeness_data <- model_data |>
  select(poly_uid, id, pair_id, polygon_type, area_m2_numeric,
         english_categories, kommune, land_cover_name, n_species, n_occurrences)

# Join all the completeness estimates by poly_uid
completeness_data <- completeness_data |>
  left_join(chao1_results |> select(poly_uid, completeness_chao1,
                                    n_occurrences_total, chao1),
            by = "poly_uid") |>
  left_join(chao2_time_results |> select(poly_uid, completeness_chao2_time,
                                         n_years, chao2_time),
            by = "poly_uid") |>
  left_join(chao2_event_results |> select(poly_uid, completeness_chao2_event,
                                          n_events, chao2_event),
            by = "poly_uid") |>
  left_join(ice_time_results |> select(poly_uid, completeness_ice_time,
                                       ice_time, sample_coverage_time),
            by = "poly_uid") |>
  left_join(ice_event_results |> select(poly_uid, completeness_ice_event,
                                        ice_event, sample_coverage_event),
            by = "poly_uid")

# Create the variables needed for the models
completeness_data <- completeness_data |>
  mutate(polygon_type = factor(polygon_type, levels = c("Buffer", "Development")),
         land_cover_name = factor(land_cover_name),
         kommune_factor = factor(kommune),
         pair_id_factor = factor(pair_id),
         area_km2 = area_m2_numeric / 1e6,
         log_area_km2 = log(area_km2))














