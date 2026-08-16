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

# 5. FILTER DATA FOR MODELLING -------------------------------------------------

## 5.1. Check the availability of the red-listed data --------------------------

# Check how many records there are in the sides so we can choose a threshold
cat("\n=== Red-listed data availability per side ===\n")
availability_redlist_records <- completeness_data |>
  summarise(sides_total = n(),
            sides_with_records = sum(n_occurrences > 0, na.rm = TRUE),
            sides_species_ge3 = sum(n_species >= 3,  na.rm = TRUE),
            sides_species_ge5 = sum(n_species >= 5,  na.rm = TRUE),
            sides_species_ge10 = sum(n_species >= 10, na.rm = TRUE),
            sides_occ_ge10 = sum(n_occurrences >= 10, na.rm = TRUE),
            sides_occ_ge30 = sum(n_occurrences >= 30, na.rm = TRUE),
            sides_years_ge3 = sum(n_years >= 3, na.rm = TRUE))

# Look at the summary
print(as.data.frame(availability_redlist_records))
# sides_total sides_with_records sides_species_ge3 sides_species_ge5 sides_species_ge10
#      259762              10187              2989              1583                551
# sides_occ_ge10 sides_occ_ge30 sides_years_ge3
#           1284            483            2110

## 5.2. Apply thresholds -------------------------------------------------------

# Set minimum thresholds 
min_species <- 5
min_occurrences <- 10
min_years <- 3
min_events <- 3

# Create filtered datasets for chao1 model
model_data_chao1 <- completeness_data |>
  filter(n_species >= min_species,
         n_occurrences >= min_occurrences,
         !is.na(completeness_chao1), !is.infinite(completeness_chao1),
         completeness_chao1 > 0, completeness_chao1 <= 1)

# Create filtered datasets for ICE time model
model_data_ice_time <- completeness_data |>
  filter(n_species >= min_species,
         n_years >= min_years,
         !is.na(completeness_ice_time), !is.infinite(completeness_ice_time),
         completeness_ice_time > 0, completeness_ice_time <= 1)

# Check how much data is left after filtering
cat("\nData after filtering:\n")
cat("Chao1 model:", nrow(model_data_chao1), "sides\n") # 1125 sides
cat("ICE-time model:", nrow(model_data_ice_time), "sides\n\n") # 1189 sides

# Add warning if the data is too little to model sensibly
if (nrow(model_data_chao1) < 100 ||
    n_distinct(model_data_chao1$polygon_type) < 2) {
  warning("Very few red-listed sides survive the Chao1 thresholds - consider ",
          "lowering min_species / min_occurrences (see availability report).")
}
if (nrow(model_data_ice_time) < 100 ||
    n_distinct(model_data_ice_time$polygon_type) < 2) {
  warning("Very few red-listed sides survive the ICE-time thresholds - consider ",
          "lowering min_species / min_years (see availability report).")
}

# Check how many complete polygon-buffer pairs qualify for the Chao1 models
cat("Qualifying sides by type (Chao1):\n")
print(table(model_data_chao1$polygon_type))
# Buffer Development 
# 611         514 
complete_pairs_chao1 <- model_data_chao1 |>
  count(pair_id) |> filter(n == 2) |> nrow()
cat("Complete PAIRS (both sides qualify), Chao1:", complete_pairs_chao1, "\n\n")
#Complete PAIRS (both sides qualify), Chao1: 102 

# Check how many complete polygon-buffer pairs qualify for the ICE-time models
cat("Qualifying sides by type (ICE-time):\n")
print(table(model_data_ice_time$polygon_type))
# Buffer Development 
# 661         528 
complete_pairs_ice <- model_data_ice_time |>
  count(pair_id) |> filter(n == 2) |> nrow()
cat("Complete PAIRS (both sides qualify), ICE-time:", complete_pairs_ice, "\n\n")
# Complete PAIRS (both sides qualify), ICE-time: 101 

# Save the completeness data
saveRDS(completeness_data,
        here("data", "derived_data", "h4d_completeness_data.rds"))

# 6. FIT MODELS ----------------------------------------------------------------

## 6.1. Chao1 additive models --------------------------------------------------

# Define model (ordered beta to deal with the 0 and 1s)
h4d_chao1_model1_additive <- glmmTMB(completeness_chao1 ~ polygon_type + log_area_km2 +
                                       land_cover_name + log(n_occurrences_total) +
                                       (1 | kommune_factor/pair_id_factor),
                                     data = model_data_chao1,
                                     family = ordbeta(link = "logit"))

# Save model output
save(h4d_chao1_model1_additive,
     file = here::here("data", "models", "h4d_chao1_model1_additive.RData"))

## 6.2. Chao1 interaction models -----------------------------------------------

# Define model
h4d_chao1_model2_interaction <- glmmTMB(completeness_chao1 ~ polygon_type * log_area_km2 +
                                          land_cover_name * log(n_occurrences_total) +
                                          (1 | kommune_factor/pair_id_factor),
                                        data = model_data_chao1,
                                        family = ordbeta(link = "logit"))

# Save model output
save(h4d_chao1_model2_interaction,
     file = here::here("data", "models", "h4d_chao1_model2_interaction.RData"))


## 6.3. Chao1 additive with disperison model -----------------------------------

# Define model
h4d_chao1_model3_disp <- glmmTMB(completeness_chao1 ~ polygon_type + log_area_km2 +
                                   land_cover_name + log(n_occurrences_total) +
                                   (1 | kommune_factor/pair_id_factor),
                                 dispformula = ~ log(n_occurrences_total),
                                 data = model_data_chao1,
                                 family = ordbeta(link = "logit"))

# Save output
save(h4d_chao1_model3_disp,
     file = here::here("data", "models", "h4d_chao1_model3_disp.RData"))

# Compare models
AICtab(h4d_chao1_model1_additive, h4d_chao1_model2_interaction,
       h4d_chao1_model3_disp, base = TRUE)
#                              AIC    dAIC   df
# h4d_chao1_model1_additive    -315.7    0.0 15
# h4d_chao1_model3_disp        -313.8    1.8 16
# h4d_chao1_model2_interaction -306.0    9.7 22

## 6.4. Chao1 dispersion and nonlinear effort with stricter thresholds ---------

# Set up stricter thresholds
min_species_strict     <- 10
min_occurrences_strict <- 30

# Filter data with the stricter filtering
# N.B! We are filtering the data, so we cannot compare the AIC of this model to the AIC of the others
model_data_chao1_strict <- completeness_data |>
  filter(n_species >= min_species_strict,
         n_occurrences >= min_occurrences_strict,
         !is.na(completeness_chao1), !is.infinite(completeness_chao1),
         completeness_chao1 > 0, completeness_chao1 <= 1)

# Check how many sides (and complete pairs) survive the strict thresholds
strict_pairs <- model_data_chao1_strict |>
  count(pair_id) |> filter(n == 2) |> nrow()
cat("\nChao1 strict-threshold sides:", nrow(model_data_chao1_strict),
    "(vs", nrow(model_data_chao1), "at the original thresholds)\n") #377 (vs 1125 at the original thresholds)
cat("Chao1 strict-threshold complete pairs:", strict_pairs, "\n") # 17
print(table(model_data_chao1_strict$polygon_type))
# Buffer Development 
# 190         187 

# N.B: the strict thresholds above leave too little data for this to be our main model
# we are only running this as a sensitivity check and only if there are more than 100 sides
chao1_final <- h4d_chao1_model1_additive
chao1_final_data <- model_data_chao1
cat("\nChao1 headline model: standard-threshold dispersion (model3_disp)\n")

# Strict nonlinear model as sensitivity only (needs enough strict sides AND
# enough complete strict pairs for the pair-level random effect to be identified)
strict_ok <- nrow(model_data_chao1_strict) >= 300 &&
  strict_pairs >= 50 &&
  n_distinct(model_data_chao1_strict$polygon_type) == 2

if (strict_ok) {
  h4d_chao1_model4_disp_poly <- glmmTMB(completeness_chao1 ~ polygon_type + log_area_km2 +
                                          land_cover_name + poly(log(n_occurrences_total), 2) +
                                          (1 | kommune_factor/pair_id_factor),
                                        dispformula = ~ log(n_occurrences_total),
                                        data   = model_data_chao1_strict,
                                        family = ordbeta(link = "logit"))
  save(h4d_chao1_model4_disp_poly,
       file = here::here("data", "models", "h4d_chao1_model4_disp_poly.RData"))
  cat("Strict nonlinear model (model4_disp_poly) fitted as a sensitivity check.\n")
  cat("  To use it as the headline instead, set chao1_final <- h4d_chao1_model4_disp_poly\n")
  cat("  and chao1_final_data <- model_data_chao1_strict.\n")
} else {
  cat("Strict thresholds leave too little red-listed data - model4_disp_poly skipped.\n")
} # too few sides, we skipped the model

## 6.5. ICE time additive ------------------------------------------------------

# Define model
h4d_ice_time_model1 <- glmmTMB(completeness_ice_time ~ polygon_type + log_area_km2 +
                                 land_cover_name + log(n_years) +
                                 (1 | kommune_factor/pair_id_factor),
                               data = model_data_ice_time,
                               family = ordbeta(link = "logit"))

# Save output
save(h4d_ice_time_model1,
     file = here::here("data", "models", "h4d_ice_time_model1.RData"))

## 6.6. ICE time intearcative model --------------------------------------------

# Define model
h4d_ice_time_model2 <- glmmTMB(completeness_ice_time ~ polygon_type * log_area_km2 *
                                 land_cover_name + log(n_years) +
                                 (1 | kommune_factor/pair_id_factor),
                               data = model_data_ice_time,
                               family = ordbeta(link = "logit"))

# Save output
save(h4d_ice_time_model2,
     file = here::here("data", "models", "h4d_ice_time_model2.RData"))

# Compare models
AICtab(h4d_ice_time_model1, h4d_ice_time_model2, base = TRUE)
#                     AIC   dAIC  df
# h4d_ice_time_model1 587.8   0.0 15
# h4d_ice_time_model2 612.1  24.3 34

## 6.7. Random-effect check ----------------------------------------------------

# After fitering, we are left with very few pairs have both sides (i.e both polygons and buffers) with enough data to use
# Use a helper to flag that pair variance has collapse towards zero, which would mean that it is not supported by the data
pair_var_near_zero <- function(model, tol = 1e-4) {
  vc <- VarCorr(model)$cond
  # the nested pair term is named "pair_id_factor:kommune_factor"
  nm <- grep("pair_id_factor", names(vc), value = TRUE)
  if (length(nm) == 0) return(FALSE)
  as.numeric(attr(vc[[nm[1]]], "stddev")[1]) < tol
}

# Check output
cat("\nPair-level RE near zero?  Chao1 headline:",
    pair_var_near_zero(chao1_final),
    "| ICE-time:", pair_var_near_zero(h4d_ice_time_model1), "\n")
# Pair-level RE near zero?  Chao1 headline: FALSE | ICE-time: TRUE 

# If either flag is TRUE, re-fit the model with a kommune-only random effect by dropping /pair_id_factor
# chao1_final <- glmmTMB(completeness_chao1 ~ polygon_type + log_area_km2 +
#                          land_cover_name + log(n_occurrences_total) +
#                          (1 | kommune_factor),
#                        dispformula = ~ log(n_occurrences_total),
#                        data   = chao1_final_data,
#                        family = ordbeta(link = "logit"))
#
h4d_ice_time_model1 <- glmmTMB(completeness_ice_time ~ polygon_type + log_area_km2 +
                                 land_cover_name + log(n_years) +
                                 (1 | kommune_factor),
                               data = model_data_ice_time,
                               family = ordbeta(link = "logit"))

# 7. MODEL SUMMARY -------------------------------------------------------------

## 7.1. Chao1 model ------------------------------------------------------------

# Print model summary
print(summary(h4d_chao1_model1_additive))

# Check convergence
if (h4d_chao1_model1_additive$sdr$pdHess) {
  cat("\nChao1 additive model converged successfully\n")
} else {
  cat("\nWarning: Chao1 headline model may not have converged\n")
} # Whohooo converged successfully!!

# Create coefficient table
coef_table_h4d <- broom.mixed::tidy(h4d_chao1_model1_additive,
                                    effects = "fixed", conf.int = TRUE) |>
  mutate(Estimate = round(estimate, 4),
         SE = round(std.error, 4),
         `z value` = round(statistic, 2),
         `p value` = ifelse(p.value < 0.001, "<0.001", round(p.value, 4))) |>
  select(Term = term, Estimate, SE, `z value`, `p value`)

# Save coefficient table
write.csv(coef_table_h4d,
          here("figures", "Table_H4d_Chao1_coefficients.csv"),
          row.names = FALSE)

## 7.2. ICE time model --------------------------------------------------------

# Print model summary
print(summary(h4d_ice_time_model1))

# Check convergence
if (h4d_ice_time_model1$sdr$pdHess) {
  cat("\nICE-time model converged successfully\n")
} else {
  cat("\nWarning: ICE-time model may not have converged\n")
} # Whohooo converged successfully!!

# Create coefficient table
coef_table_h4d_ice <- broom.mixed::tidy(h4d_ice_time_model1,
                                        effects = "fixed", conf.int = TRUE) |>
  mutate(Estimate = round(estimate, 4),
         SE = round(std.error, 4),
         `z value` = round(statistic, 2),
         `p value` = ifelse(p.value < 0.001, "<0.001", round(p.value, 4))) |>
  select(Term = term, Estimate, SE, `z value`, `p value`)

# Save coefficient table
write.csv(coef_table_h4d_ice,
          here("figures", "Table_H4d_ICE_additive_coefficients.csv"),
          row.names = FALSE)

# 8. MODEL DIAGNOSTICS ---------------------------------------------------------

## 8.1. Chao1 additive model ---------------------------------------------------

# Simulate residuals
sim_residuals_h4d_chao1 <- simulateResiduals(fittedModel = h4d_chao1_model1_additive,
                                             n = 1000)
# Create diagnostic plots
png(filename = here("figures", "Figure_H4d_Chao1_additive_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals_h4d_chao1)
dev.off()

# Test for dispersion
print(testDispersion(sim_residuals_h4d_chao1))

# Test for outliers
print(testOutliers(sim_residuals_h4d_chao1))

## 8.2. ICE time additive model ------------------------------------------------

# Simulate residuals
sim_residuals_h4d_ice <- simulateResiduals(fittedModel = h4d_ice_time_model1,
                                           n = 1000)

# Create diagnostic plots
png(filename = here("figures", "Figure_H4d_ICE_additive_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals_h4d_ice)
dev.off()

# Test for dispersion
print(testDispersion(sim_residuals_h4d_ice))

# Test for outliers
print(testOutliers(sim_residuals_h4d_ice))

# 9. MODEL PREDICTION FIGURE ---------------------------------------------------

# Define a colour scheme
polygon_colours <- c("Buffer" = "#E66101", "Development" = "#5E3C99")

# Use a function to build prediction df for the completeness model
make_pred_df <- function(model, data) {
  pred <- ggpredict(model,
                    terms = c("log_area_km2 [n=100]", "polygon_type",
                              "land_cover_name"),
                    type  = "fixed")
  pred_df <- as.data.frame(pred) |>
    rename(log_area_km2 = x, polygon_type = group, land_cover_name = facet)
  ranges <- data |>
    group_by(land_cover_name, polygon_type) |>
    summarise(lo = min(log_area_km2), hi = max(log_area_km2), .groups = "drop")
  pred_df |>
    left_join(ranges, by = c("land_cover_name", "polygon_type")) |>
    filter(log_area_km2 >= lo, log_area_km2 <= hi) |>
    select(-lo, -hi)
}

# Build a helper fucntion to assemble the completeness figure
completeness_figure <- function(pred_df) {
  ggplot(pred_df, aes(x = log_area_km2, y = predicted,
                      colour = polygon_type, fill = polygon_type)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, colour = NA) +
    geom_line(linewidth = 1.2) +
    facet_wrap(~land_cover_name, ncol = 3, labeller = as_labeller(pretty_lc)) +
    scale_colour_manual(values = polygon_colours, name = "Area Type") +
    scale_fill_manual(values = polygon_colours, name = "Area Type") +
    scale_y_continuous(labels = scales::percent) +
    labs(x = expression(paste("Log(Area (km"^2, "))")),
         y = "Estimated Red-listed Species Completeness") +
    theme_classic() +
    theme(panel.grid = element_blank(),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.position = "bottom",
          strip.background = element_rect(fill = "grey90", colour = "black"),
          strip.text = element_text(size = 12, face = "bold"))
}

## 9.1. Chao1 additive model ---------------------------------------------------

# Get predictions for area x polygon type x land-cover
pred_df_chao1 <- make_pred_df(chao1_final, chao1_final_data)

# Create the plot for area x polygon type and facet it by land-cover
fig_chao1_predictions <- completeness_figure(pred_df_chao1)

# Save plot to file
ggsave(filename = here("figures", "Figure_H4d_chao1_predictions.png"),
       plot = fig_chao1_predictions, width = 14, height = 10, dpi = 600)
ggsave(filename = here("figures", "Figure_H4d_chao1_predictions.pdf"),
       plot = fig_chao1_predictions, width = 14, height = 10, dpi = 600)

## 9.2. ICE time additive ------------------------------------------------------

# Get predictions for area x polygon type x land-cover
pred_df_ice <- make_pred_df(h4d_ice_time_model1, model_data_ice_time)

# Create the plot for area x polygon type and facet it by land-cover
fig_ice_predictions <- completeness_figure(pred_df_ice)

# Save plot to file
ggsave(filename = here("figures", "Figure_H4d_ICE_predictions.png"),
       plot = fig_ice_predictions, width = 14, height = 10, dpi = 600)
ggsave(filename = here("figures", "Figure_H4d_ICE_predictions.pdf"),
       plot = fig_ice_predictions, width = 14, height = 10, dpi = 600)

# 10. HYPOTHESIS TESTING -------------------------------------------------------

## 10.1. Chao1 model -----------------------------------------------------------

# Get marginal means for polygon type averaged across land-cover and area
emmeans_polygon_h4d_chao1 <- emmeans(chao1_final,
                                     specs = "polygon_type", type = "response")

# Get summary
cat("Estimated red-listed completeness by side (Chao1):\n")
print(summary(emmeans_polygon_h4d_chao1))
# polygon_type response     SE  df asymp.LCL asymp.UCL
# Buffer          0.681 0.0127 Inf     0.655     0.705
# Development     0.686 0.0127 Inf     0.660     0.710

# Calculate pairwise contrast
contrast_polygon_h4d_chao1 <- contrast(emmeans_polygon_h4d_chao1,
                                       method = "revpairwise", type = "response")

# Check summary
cat("\nDevelopment vs Buffer (Chao1 red-listed completeness):\n")
print(summary(contrast_polygon_h4d_chao1, infer = TRUE))
# contrast             odds.ratio     SE  df asymp.LCL asymp.UCL null z.ratio p.value
# Development / Buffer       1.02 0.0521 Inf     0.926      1.13    1   0.456  0.6483

# Hypothesis verdict from the odds-ratio CI
con_df <- as.data.frame(confint(contrast_polygon_h4d_chao1))
or_col <- grep("ratio|estimate", names(con_df), value = TRUE)[1]
or_lo  <- con_df[[grep("LCL|lower", names(con_df), value = TRUE)[1]]]
or_hi  <- con_df[[grep("UCL|upper", names(con_df), value = TRUE)[1]]]
or_est <- con_df[[or_col]]
cat(sprintf("\nChao1 completeness OR (Development / Buffer): %.3f  [%.3f, %.3f]\n",
            or_est, or_lo, or_hi)) # 1.024  [0.926, 1.131]
if (or_lo > 1) {
  cat("H4d SUPPORTED (Chao1): development polygons have higher red-listed completeness.\n")
} else if (or_hi < 1) {
  cat("H4d NOT supported (Chao1): development polygons have LOWER completeness.\n")
} else {
  cat("H4d inconclusive (Chao1): the completeness OR CI includes 1.\n")
} # H4d inconclusive (Chao1): the completeness OR CI includes 1.

## 10.2. ICE time additive -----------------------------------------------------

# Get marginal means for polygon type averaged across land-cover and area
emmeans_polygon_h4d_ice <- emmeans(h4d_ice_time_model1,
                                   specs = "polygon_type", type = "response")

# Get summary 
cat("\nEstimated red-listed completeness by side (ICE-time):\n")
print(summary(emmeans_polygon_h4d_ice))
# polygon_type response     SE  df asymp.LCL asymp.UCL
# Buffer          0.259 0.0129 Inf     0.235     0.285
# Development     0.259 0.0133 Inf     0.233     0.286

# Calculate pairwise contrast
contrast_polygon_h4d_ice <- contrast(emmeans_polygon_h4d_ice,
                                     method = "revpairwise", type = "response")

# Check output
cat("\nDevelopment vs Buffer (ICE-time red-listed completeness):\n")
print(summary(contrast_polygon_h4d_ice, infer = TRUE))
# contrast             odds.ratio     SE  df asymp.LCL asymp.UCL null z.ratio p.value
# Development / Buffer      0.997 0.0637 Inf      0.88      1.13    1  -0.047  0.9625

# Save inference objects
saveRDS(list(chao1_by_side = emmeans_polygon_h4d_chao1,
             chao1_contrast = contrast_polygon_h4d_chao1,
             ice_by_side = emmeans_polygon_h4d_ice,
             ice_contrast = contrast_polygon_h4d_ice),
        here("data", "models", "h4d_completeness_emmeans.rds"))
