##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 4.3_H2d_models
# This script contains code to test Hypothesis 2d: Area plan polygons will 
# have higher sampling completeness than areas outside of planned developments

# NB ON CHAO1: Chao1 treats occurrence counts per species as abundances
# (singltons/doubletons = species with 1 or 2 GBIF records)
# This is not true abundance so Chao2/ICE which are based on years and events
# tend to be better suited.
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

# Create a helper function for land-cover labels
pretty_lc <- function(x) {
  x <- gsub("_", " ", x)
  gsub("(^|\\s)([a-z])", "\\1\\U\\2", x, perl = TRUE)
}

# 2. PREPARE DATA FOR COMPLETENESS CALCULATIONS --------------------------------

# Reshape data so you can calculate Chao1 (use pseudo-abundance)
chao1_data <- polygon_occurrence_join |>
  filter(!is.na(gbifID)) |>
  group_by(poly_uid, species) |>
  summarise(n_occurrences = n(), .groups = "drop")

# Create data for Chao2 and ICE with Time (years as sampling units)
time_data <- polygon_occurrence_join |>
  filter(!is.na(gbifID), !is.na(year)) |>
  distinct(poly_uid, species, year) |>
  group_by(poly_uid, species) |>
  summarise(n_years = n(), .groups = "drop")

# Create data for Chao2 and ICE with Events (parentEventID as sampling units)
event_data <- polygon_occurrence_join |>
  filter(!is.na(gbifID), !is.na(parentEventID)) |>
  distinct(poly_uid, species, parentEventID) |>
  group_by(poly_uid, species) |>
  summarise(n_events = n(), .groups = "drop")

# Total years events per polygon and buffer
n_years_per_side <- polygon_occurrence_join |>
  filter(!is.na(gbifID), !is.na(year)) |>
  group_by(poly_uid) |>
  summarise(total_years = n_distinct(year), .groups = "drop")

# Total events per polygon and buffer
n_events_per_side <- polygon_occurrence_join |>
  filter(!is.na(gbifID), !is.na(parentEventID)) |>
  group_by(poly_uid) |>
  summarise(total_events = n_distinct(parentEventID), .groups = "drop")

# 3. CALCULATE COMPLETENESS ESTIMATES ------------------------------------------

## 3.1. Chao1 ------------------------------------------------------------------

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

## 3.2. Chao2 with Time (incidence-based) --------------------------------------

# Calculate Chao2 with time
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

## 3.3. Chao2 with Events (incidence-based) ------------------------------------

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

## 3.4. ICE with Time (incidence-based) ----------------------------------------

# Calculate index
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


## 3.5. ICE with Events (incidence-based) --------------------------------------

# Calculate index
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

# Base per-side data
completeness_data <- model_data |>
  select(poly_uid, id, pair_id, polygon_type, area_m2_numeric,
         english_categories, kommune, land_cover_name, n_species, n_occurrences)

# Join all completeness estimates by poly_uid
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

# 5. FILTER DATA FOR MODELING --------------------------------------------------

# Set minimum thresholds
min_species <- 5
min_occurrences <- 10
min_years <- 3
min_events <- 3

# Create filtered datasets for each model
model_data_chao1 <- completeness_data |>
  filter(n_species >= min_species,
         n_occurrences >= min_occurrences,
         !is.na(completeness_chao1), !is.infinite(completeness_chao1),
         completeness_chao1 > 0, completeness_chao1 <= 1)

model_data_ice_time <- completeness_data |>
  filter(n_species >= min_species,
         n_years >= min_years,
         !is.na(completeness_ice_time), !is.infinite(completeness_ice_time),
         completeness_ice_time > 0, completeness_ice_time <= 1)

cat("Data after filtering:\n")
cat("  Chao1 model:    ", nrow(model_data_chao1),    "sides\n") #7270
cat("  ICE-time model: ", nrow(model_data_ice_time), "sides\n\n") #5623

# Check how many complete polygon-buffer pairs qualify
cat("Qualifying sides by type (Chao1):\n")
print(table(model_data_chao1$polygon_type))
complete_pairs_chao1 <- model_data_chao1 |>
  count(pair_id) |> filter(n == 2) |> nrow()
# Buffer Development 
# 4166        3104
cat("Complete PAIRS (both sides qualify), Chao1:", complete_pairs_chao1, "\n\n") #1438 

cat("Qualifying sides by type (ICE-time):\n")
print(table(model_data_ice_time$polygon_type))
# Buffer Development 
# 3404        2219 
complete_pairs_ice <- model_data_ice_time |>
  count(pair_id) |> filter(n == 2) |> nrow()
cat("Complete PAIRS (both sides qualify), ICE-time:", complete_pairs_ice, "\n\n") #1077

# Save the completeness data
saveRDS(completeness_data,
        here("data", "derived_data", "h2d_completeness_data.rds"))

# 6. FIT MODELS ----------------------------------------------------------------

## 6.1. Chao1 model additive ---------------------------------------------------

# Define model (ordered beta to deal with 0 and 1)
h2d_chao1_model1_additive <- glmmTMB(completeness_chao1 ~ polygon_type + log_area_km2 +
                                       land_cover_name + log(n_occurrences_total) +
                                       (1 | kommune_factor/pair_id_factor),
                                     data   = model_data_chao1,
                                     family = ordbeta(link = "logit"))

# Save model output
save(h2d_chao1_model1_additive,
     file = here::here("data", "models", "h2d_chao1_model1_additive.RData"))

## 6.2. Chao1 model interaction ------------------------------------------------

# Define model
h2d_chao1_model2_interaction <- glmmTMB(completeness_chao1 ~ polygon_type * log_area_km2 +
                                          land_cover_name * log(n_occurrences_total) +
                                          (1 | kommune_factor/pair_id_factor),
                                        data   = model_data_chao1,
                                        family = ordbeta(link = "logit"))

# Save model output
save(h2d_chao1_model2_interaction,
     file = here::here("data", "models", "h2d_chao1_model2_interaction.RData"))

## 6.3. Chao1 additive with dispersion model -----------------------------------

# Define model
h2d_chao1_model3_disp <- glmmTMB(completeness_chao1 ~ polygon_type + log_area_km2 +
                                   land_cover_name + log(n_occurrences_total) +
                                   (1 | kommune_factor/pair_id_factor),
                                 dispformula = ~ log(n_occurrences_total),
                                 data   = model_data_chao1,
                                 family = ordbeta(link = "logit"))

# Save output
save(h2d_chao1_model3_disp,
     file = here::here("data", "models", "h2d_chao1_model3_disp.RData"))

# Compare models
AICtab(h2d_chao1_model1_additive, h2d_chao1_model2_interaction,
       h2d_chao1_model3_disp, base = TRUE)
#                             AIC     dAIC    df
# h2d_chao1_model3_disp        -5390.6     0.0 16
# h2d_chao1_model1_additive    -5363.1    27.5 15
# h2d_chao1_model2_interaction -5359.0    31.6 22

## 6.4. Chao1 dispersion and nonlinear effort with stricter thresholds ---------

# Set up sticter thresholds
min_species_strict     <- 10
min_occurrences_strict <- 30

# Filter data with the stricter filtering
# N.B! We are filtering the data, so we cannot compare the AIC of this model to the AIC of the others
model_data_chao1_strict <- completeness_data |>
  filter(n_species >= min_species_strict,
         n_occurrences >= min_occurrences_strict,
         !is.na(completeness_chao1), !is.infinite(completeness_chao1),
         completeness_chao1 > 0, completeness_chao1 <= 1)

# Chck how many pairs are left
cat("\nChao1 strict-threshold sides:", nrow(model_data_chao1_strict),
    "(vs", nrow(model_data_chao1), "at the original thresholds)\n") # 2977 (vs 7270 at the original thresholds)
print(table(model_data_chao1_strict$polygon_type))
# Buffer Development 
# 1689        1288 

# Define model
h2d_chao1_model4_disp_poly <- glmmTMB(completeness_chao1 ~ polygon_type + log_area_km2 +
                                        land_cover_name + poly(log(n_occurrences_total), 2) +
                                        (1 | kommune_factor/pair_id_factor),
                                      dispformula = ~ log(n_occurrences_total),
                                      data   = model_data_chao1_strict,
                                      family = ordbeta(link = "logit"))

# Save model output
save(h2d_chao1_model4_disp_poly,
     file = here::here("data", "models", "h2d_chao1_model4_disp_poly.RData"))

## 6.5. ICE time additive ------------------------------------------------------

# Define model
h2d_ice_time_model1 <- glmmTMB(completeness_ice_time ~ polygon_type + log_area_km2 +
                                 land_cover_name + log(n_years) +
                                 (1 | kommune_factor/pair_id_factor),
                               data   = model_data_ice_time,
                               family = ordbeta(link = "logit"))

# Save model output
save(h2d_ice_time_model1,
     file = here::here("data", "models", "h2d_ice_time_model1.RData"))

## 6.6. ICE time interactive ---------------------------------------------------

# Define model
h2d_ice_time_model2 <- glmmTMB(completeness_ice_time ~ polygon_type * log_area_km2 *
                                 land_cover_name + log(n_years) +
                                 (1 | kommune_factor/pair_id_factor),
                               data   = model_data_ice_time,
                               family = ordbeta(link = "logit"))

# Save model output
save(h2d_ice_time_model2,
     file = here::here("data", "models", "h2d_ice_time_model2.RData"))

# Compare models
AICtab(h2d_ice_time_model1, h2d_ice_time_model2, base = TRUE)
#                     AIC    dAIC   df
# h2d_ice_time_model1 3618.1    0.0 15
# h2d_ice_time_model2 3625.3    7.2 34

# 7. MODEL SUMMARY -------------------------------------------------------------

## 7.1. Chao1 additive model ---------------------------------------------------

# Print model summary
cat("\n=== Chao1 additive summary ===\n")
print(summary(h2d_chao1_model1_additive))

# Check convergence
if (h2d_chao1_model1_additive$sdr$pdHess) {
  cat("\nChao1 model converged successfully\n")
} else {
  cat("\nWarning: Chao1 model may not have converged\n")
}

# Create coefficient table
coef_table_h2d <- broom.mixed::tidy(h2d_chao1_model1_additive,
                                    effects = "fixed", conf.int = TRUE) |>
  mutate(Estimate  = round(estimate, 4),
         SE        = round(std.error, 4),
         `z value` = round(statistic, 2),
         `p value` = ifelse(p.value < 0.001, "<0.001", round(p.value, 4))) |>
  select(Term = term, Estimate, SE, `z value`, `p value`)

# Save coefficient table
write.csv(coef_table_h2d,
          here("figures", "Table_H2d_Chao1_additive_coefficients.csv"),
          row.names = FALSE)

## 7.2. ICE time additive ------------------------------------------------------

# Print model summary
print(summary(h2d_ice_time_model1))

# Check convergence
if (h2d_ice_time_model1$sdr$pdHess) {
  cat("\nICE-time model converged successfully\n")
} else {
  cat("\nWarning: ICE-time model may not have converged\n")
}


# Create coefficient table
coef_table_h2d_ice <- broom.mixed::tidy(h2d_ice_time_model1,
                                        effects = "fixed", conf.int = TRUE) |>
  mutate(Estimate  = round(estimate, 4),
         SE        = round(std.error, 4),
         `z value` = round(statistic, 2),
         `p value` = ifelse(p.value < 0.001, "<0.001", round(p.value, 4))) |>
  select(Term = term, Estimate, SE, `z value`, `p value`)


# Save coefficient table
write.csv(coef_table_h2d_ice,
          here("figures", "Table_H2d_ICE_additive_coefficients.csv"),
          row.names = FALSE)

# 8. MODEL DIAGNOSTICS WITH DHARMA ---------------------------------------------

## 8.1. Chao1 additive model ---------------------------------------------------

# Simulate residuals
sim_residuals_h2d_chao1 <- simulateResiduals(fittedModel = h2d_chao1_model1_additive,
                                             n = 1000)
# Create diagnostic plots
png(filename = here("figures", "Figure_H2d_Chao1_additive_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals_h2d_chao1)
dev.off()

# Test for dispersion
print(testDispersion(sim_residuals_h2d_chao1))

# Test for outliers
print(testOutliers(sim_residuals_h2d_chao1))

## 8.2. Chao1 with dispersion model --------------------------------------------

# Simulate residuals
sim_residuals_h2d_chao1_disp <- simulateResiduals(fittedModel = h2d_chao1_model3_disp,
                                                  n = 1000)

# Create diagnostic plots
png(filename = here("figures", "Figure_H2d_Chao1_dispmodel_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals_h2d_chao1_disp)
dev.off()

# Test for dispersion
print(testDispersion(sim_residuals_h2d_chao1_disp))

# Test for outliers
print(testOutliers(sim_residuals_h2d_chao1_disp))

## 8.3. Chao1 with nonlinear effort and stricter thresholds --------------------

# Simulate residuals
sim_residuals_h2d_chao1_disp_poly <- simulateResiduals(fittedModel = h2d_chao1_model4_disp_poly,
                                                       n = 1000)

# Plot diagnostics
png(filename = here("figures", "Figure_H2d_Chao1_disp_poly_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals_h2d_chao1_disp_poly)
dev.off()

# Test dispersion
print(testDispersion(sim_residuals_h2d_chao1_disp_poly))

# Test outliers
print(testOutliers(sim_residuals_h2d_chao1_disp_poly))

## 8.4. ICE time additive ------------------------------------------------------

# Simulate residuals
sim_residuals_h2d_ice <- simulateResiduals(fittedModel = h2d_ice_time_model1,
                                           n = 1000)

# Create diagnostic plots
png(filename = here("figures", "Figure_H2d_ICE_additive_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals_h2d_ice)
dev.off()

# Test for dispersion
print(testDispersion(sim_residuals_h2d_ice))

# Test for outliers
print(testOutliers(sim_residuals_h2d_ice))


# 9. MODEL PREDICTION FIGURE ---------------------------------------------------

# Define colour scheme
polygon_colours <- c("Buffer" = "#E66101", "Development" = "#5E3C99")

# Function to build prediction df for the completeness model
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

# Helper function to assemble completeness figure
completeness_figure <- function(pred_df) {
  ggplot(pred_df, aes(x = log_area_km2, y = predicted,
                      colour = polygon_type, fill = polygon_type)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, colour = NA) +
    geom_line(linewidth = 1.2) +
    facet_wrap(~land_cover_name, ncol = 3, labeller = as_labeller(pretty_lc)) +
    scale_colour_manual(values = polygon_colours, name = "Area type") +
    scale_fill_manual(values = polygon_colours, name = "Area type") +
    scale_y_continuous(labels = scales::percent) +
    labs(x = expression(paste("Log(Area (m"^2, "))")),
         y = "Estimated Completeness") +
    theme_classic() +
    theme(panel.grid = element_blank(),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.position = "bottom",
          strip.background = element_rect(fill = "grey90", colour = "black"),
          strip.text = element_text(size = 12, face = "bold"))
}

## 9.1. Chao1 stricter model ---------------------------------------------------

# Get predictions for area × polygon type × land cover
pred_df_chao1 <- make_pred_df(h2d_chao1_model4_disp_poly, model_data_chao1_strict)

# Main plot: Area × Polygon Type, faceted by Land Cover
fig_chao1_predictions <- completeness_figure(pred_df_chao1)

# Save plot to file
ggsave(filename = here("figures", "Figure_H2d_chao1_predictions.png"),
       plot = fig_chao1_predictions, width = 14, height = 10, dpi = 600)
ggsave(filename = here("figures", "Figure_H2d_chao1_predictions.pdf"),
       plot = fig_chao1_predictions, width = 14, height = 10, dpi = 600)

## 9.2. ICE time additive ------------------------------------------------------

# Get predictions for area × polygon type × land cover
pred_df_ice <- make_pred_df(h2d_ice_time_model1, model_data_ice_time)

# Main plot: Area × Polygon Type, faceted by Land Cover
fig_ice_predictions <- completeness_figure(pred_df_ice)

# Save plot to file
ggsave(filename = here("figures", "Figure_H2d_ICE_predictions.png"),
       plot = fig_ice_predictions, width = 14, height = 10, dpi = 600)
ggsave(filename = here("figures", "Figure_H2d_ICE_predictions.pdf"),
       plot = fig_ice_predictions, width = 14, height = 10, dpi = 600)

# 10. HYPOTHESIS TESTING -------------------------------------------------------

## 10.1. Chao1 stricter model --------------------------------------------------

# Get marginal means for polygon type (averaged across land cover and area)
emmeans_polygon_h2d_chao1 <- emmeans(h2d_chao1_model4_disp_poly,
                                     specs = "polygon_type", type = "response")
cat("Estimated completeness by side (Chao1):\n")
print(summary(emmeans_polygon_h2d_chao1))
# polygon_type response     SE  df asymp.LCL asymp.UCL
# Buffer          0.552 0.0106 Inf     0.531     0.573
# Development     0.561 0.0109 Inf     0.540     0.582

# Calculate pairwise contrast
contrast_polygon_h2d_chao1 <- contrast(emmeans_polygon_h2d_chao1,
                                       method = "revpairwise", type = "response")
cat("\nDevelopment vs Buffer (Chao1 completeness):\n")
print(summary(contrast_polygon_h2d_chao1, infer = TRUE))
# contrast             odds.ratio     SE  df asymp.LCL asymp.UCL null z.ratio p.value
# Development / Buffer       1.04 0.0315 Inf     0.978       1.1    1   1.215  0.2244

## 10.2. ICE time additive -----------------------------------------------------

# Get marginal means for polygon type (averaged across land cover and area)
emmeans_polygon_h2d_ice <- emmeans(h2d_ice_time_model1,
                                   specs = "polygon_type", type = "response")
cat("\nEstimated completeness by side (ICE-time):\n")
print(summary(emmeans_polygon_h2d_ice))
# polygon_type response      SE  df asymp.LCL asymp.UCL
# Buffer          0.210 0.00671 Inf     0.197     0.223
# Development     0.217 0.00759 Inf     0.202     0.232

# Calculate pairwise contrast
contrast_polygon_h2d_ice <- contrast(emmeans_polygon_h2d_ice,
                                     method = "revpairwise", type = "response")
cat("\nDevelopment vs Buffer (ICE-time completeness):\n")
print(summary(contrast_polygon_h2d_ice, infer = TRUE))
# contrast             odds.ratio     SE  df asymp.LCL asymp.UCL null z.ratio p.value
# Development / Buffer       1.04 0.0318 Inf     0.981      1.11    1   1.325  0.1853

# Save inference objects
saveRDS(list(chao1_by_side  = emmeans_polygon_h2d_chao1,
             chao1_contrast = contrast_polygon_h2d_chao1,
             ice_by_side    = emmeans_polygon_h2d_ice,
             ice_contrast   = contrast_polygon_h2d_ice),
        here("data", "models", "h2d_completeness_emmeans.rds"))

# END OF SCRIPT ----------------------------------------------------------------