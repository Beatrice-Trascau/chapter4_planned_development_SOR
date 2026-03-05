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

## 3.4. ICE with Time (incidence-based) ----------------------------------------

ice_time_results <- time_data |>
  group_by(polygon_id) |>
  summarise(n_species_obs = n(),
            Q1 = sum(n_years == 1),
            Q2 = sum(n_years == 2),
            .groups = "drop") |>
  left_join(n_years_per_polygon, by = "polygon_id") |>
  rename(n_years = total_years) |>
  # sample coverage
  mutate(C_ice = 1 - (Q1 / n_years),
         # ICE estimate
         ice_time = if_else(Q2 > 0, {
           gamma_ice_val <- pmax(((n_species_obs / C_ice) * (Q1 / n_years) * 
                                    ((n_years - 1) * Q1 / ((n_years - 1) * Q1 + 2 * Q2))) - 1, 0)
           n_species_obs + (Q1 / C_ice) * gamma_ice_val},
           n_species_obs + (Q1 * (Q1 - 1) / 2)),
         # completeness
         completeness_ice_time = n_species_obs / ice_time,
         sample_coverage_time = C_ice)

## 3.5. ICE with Events (incidence-based) --------------------------------------

cat("\nCalculating ICE with Events...\n")

ice_event_results <- event_data |>
  group_by(polygon_id) |>
  summarise(n_species_obs = n(),
            Q1 = sum(n_events == 1),
            Q2 = sum(n_events == 2),
            .groups = "drop") |>
  left_join(n_events_per_polygon, by = "polygon_id") |>
  rename(n_events = total_events) |>
  # sample coverage
  mutate(C_ice = 1 - (Q1 / n_events),
         # ICE estimate
         ice_event = if_else(Q2 > 0, {
                          gamma_ice_val <- pmax(((n_species_obs / C_ice) * (Q1 / n_events) * 
                                                   ((n_events - 1) * Q1 / ((n_events - 1) * Q1 + 2 * Q2))) - 1, 0)
                          n_species_obs + (Q1 / C_ice) * gamma_ice_val
                        },
                        n_species_obs + (Q1 * (Q1 - 1) / 2)),
         # completeness
         completeness_ice_event = n_species_obs / ice_event,
         sample_coverage_event = C_ice)

# 4. COMBINE ALL COMPLETENESS ESTIMATES ----------------------------------------

# Start with base polygon data
completeness_data <- model_data |>
  select(polygon_id, polygon_type, area_m2_numeric, english_categories, 
         kommune, land_cover_name, n_species, n_occurrences)

# Join all completeness estimates
completeness_data <- completeness_data |>
  left_join(chao1_results |> select(polygon_id, completeness_chao1, 
                                    n_occurrences_total, chao1), 
            by = "polygon_id") |>
  left_join(chao2_time_results |> select(polygon_id, completeness_chao2_time, 
                                         n_years, chao2_time), 
            by = "polygon_id") |>
  left_join(chao2_event_results |> select(polygon_id, completeness_chao2_event, 
                                          n_events, chao2_event), 
            by = "polygon_id") |>
  left_join(ice_time_results |> select(polygon_id, completeness_ice_time, 
                                       ice_time, sample_coverage_time), 
            by = "polygon_id") |>
  left_join(ice_event_results |> select(polygon_id, completeness_ice_event, 
                                        ice_event, sample_coverage_event), 
            by = "polygon_id")

# Create modeling variables
completeness_data <- completeness_data |>
  mutate(polygon_type = factor(polygon_type, levels = c("Buffer", "Development")),
         land_cover_name = factor(land_cover_name),
         kommune_factor = factor(kommune),
         area_km2 = area_m2_numeric / 1e6,
         log_area_km2 = log(area_km2),
         # cap completeness at 1.0 (can exceed 1 due to estimation)
         completeness_chao1 = pmin(completeness_chao1, 1.0, na.rm = TRUE),
         completeness_chao2_time = pmin(completeness_chao2_time, 1.0, na.rm = TRUE),
         completeness_chao2_event = pmin(completeness_chao2_event, 1.0, na.rm = TRUE),
         completeness_ice_time = pmin(completeness_ice_time, 1.0, na.rm = TRUE),
         completeness_ice_event = pmin(completeness_ice_event, 1.0, na.rm = TRUE))

# 5. FILTER DATA FOR MODELING --------------------------------------------------

# Set minimum thresholds
min_species <- 5
min_occurrences <- 10
min_years <- 3
min_events <- 3

cat("Minimum thresholds:\n")
cat("  Species:", min_species, "\n")
cat("  Occurrences:", min_occurrences, "\n")
cat("  Years:", min_years, "\n")
cat("  Events:", min_events, "\n\n")

# Create filtered datasets for each model
model_data_chao1 <- completeness_data |>
  filter(n_species >= min_species,
         n_occurrences >= min_occurrences,
         !is.na(completeness_chao1),
         !is.infinite(completeness_chao1),
         completeness_chao1 > 0,
         completeness_chao1 <= 1)

model_data_time <- completeness_data |>
  filter(n_species >= min_species,
         n_years >= min_years,
         !is.na(completeness_ice_time),
         !is.na(completeness_chao2_time),
         !is.infinite(completeness_ice_time),
         !is.infinite(completeness_chao2_time),
         completeness_ice_time > 0,
         completeness_ice_time <= 1,
         completeness_chao2_time > 0,
         completeness_chao2_time <= 1)

model_data_event <- completeness_data |>
  filter(n_species >= min_species,
         n_events >= min_events,
         !is.na(completeness_ice_event),
         !is.na(completeness_chao2_event),
         !is.infinite(completeness_ice_event),
         !is.infinite(completeness_chao2_event),
         completeness_ice_event > 0,
         completeness_ice_event <= 1,
         completeness_chao2_event > 0,
         completeness_chao2_event <= 1)

cat("Data after filtering:\n")
cat("  Chao1 model:", nrow(model_data_chao1), "polygons\n")
cat("  Time-based models:", nrow(model_data_time), "polygons\n")
cat("  Event-based models:", nrow(model_data_event), "polygons\n\n")

# Save the completeness data
saveRDS(completeness_data, 
        here("data", "derived_data", "h2d_completeness_data.rds"))
