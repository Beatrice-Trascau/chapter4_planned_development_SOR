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

# Total years per polygon
n_years_per_polygon <- polygon_occurrence_join |>
  st_drop_geometry() |>
  filter(!is.na(gbifID), !is.na(year)) |>
  group_by(polygon_id) |>
  summarise(total_years = n_distinct(year), .groups = "drop")

# Total events per polygon
n_events_per_polygon <- polygon_occurrence_join |>
  st_drop_geometry() |>
  filter(!is.na(gbifID), !is.na(parentEventID)) |>
  group_by(polygon_id) |>
  summarise(total_events = n_distinct(parentEventID), .groups = "drop")


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
# Need to go back to the original polygon_occurrence_join for this
n_years_per_polygon <- polygon_occurrence_join |>
  st_drop_geometry() |>
  filter(!is.na(gbifID), !is.na(year)) |>
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
# Need to go back to the original polygon_occurrence_join for this
n_events_per_polygon <- polygon_occurrence_join |>
  st_drop_geometry() |>
  filter(!is.na(gbifID), !is.na(parentEventID)) |>
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
         ice_time = if_else(Q2 > 0,
                       {
                         gamma_ice_val <- pmax(((n_species_obs / C_ice) * (Q1 / n_years) * 
                                                  ((n_years - 1) * Q1 / ((n_years - 1) * Q1 + 2 * Q2))) - 1, 0)
                         n_species_obs + (Q1 / C_ice) * gamma_ice_val
                       },
                       n_species_obs + (Q1 * (Q1 - 1) / 2)),
         # completeness
         completeness_ice_time = n_species_obs / ice_time,
         sample_coverage_time = C_ice)

## 3.5. ICE with Events (incidence-based) --------------------------------------

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
         ice_event = if_else(Q2 > 0,
                        {
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
         !is.na(completeness_chao1),
         !is.infinite(completeness_chao1),
         completeness_chao1 > 0,
         completeness_chao1 <= 1) |>
  mutate(n_obs = n(),
         completeness_chao1_trans = (completeness_chao1 * (n_obs - 1) + 0.5) / n_obs)

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

# 6. FIT MODELS ----------------------------------------------------------------

## 6.1. Chao1 model additive ---------------------------------------------------

# Define model (ordered beta to deal with 0 and 1)
h2d_chao1_model1_additive <- glmmTMB(completeness_chao1 ~ polygon_type + log_area_km2 + 
                                       land_cover_name + log(n_occurrences_total) + (1|kommune_factor),
                                     data = model_data_chao1,
                                     family = ordbeta(link = "logit"))
# Save model output
save(h2d_chao1_model1_additive, 
     file = here:: here("data", "models", "h2d_chao1_model1_additive.rds"))

## 6.2. Chao1 model interaction ------------------------------------------------

# Define model
h2d_chao1_model2_interaction <- glmmTMB(completeness_chao1 ~ polygon_type * log_area_km2 +
                                          land_cover_name * log(n_occurrences_total) + (1|kommune_factor),
                                        data = model_data_chao1,
                                        family = ordbeta(link = "logit"))

# Save model output
save(h2d_chao1_model2_interaction, 
     file = here:: here("data", "models", "h2d_chao1_model2_interaction.rds"))


# Compare models
AICtab(h2d_chao1_model1_additive, h2d_chao1_model2_interaction, base = TRUE)

## 6.3. ICE time additive ------------------------------------------------------

# Filter and transform data
model_data_ice_time <- completeness_data |>
  filter(n_species >= min_species,
         n_years >= min_years,
         !is.na(completeness_ice_time),
         !is.infinite(completeness_ice_time),
         completeness_ice_time >= 0,
         completeness_ice_time <= 1) |>
  mutate(n_obs = n())

# Define model
h2d_ice_time_model1 <- glmmTMB(completeness_ice_time ~ polygon_type + 
                                 log_area_km2 + land_cover_name + 
                                 log(n_years) + (1|kommune_factor),
                               data = model_data_ice_time,
                               family = ordbeta(link = "logit"))

# Save model output
save(h2d_ice_time_model1, 
     file = here:: here("data", "models", "h2d_ice_time_model1.rds"))

## 6.4. ICE time interactive ---------------------------------------------------

# Define model
h2d_ice_time_model2 <- glmmTMB(completeness_ice_time ~ polygon_type * 
                                 log_area_km2 * land_cover_name + 
                                 log(n_years) + (1|kommune_factor),
                               data = model_data_ice_time,
                               family = ordbeta(link = "logit"))

# Save model output
save(h2d_ice_time_model2, 
     file = here:: here("data", "models", "h2d_ice_time_model2.rds"))

# Compare models
AICtab(h2d_ice_time_model1, h2d_ice_time_model2, base = TRUE)

# 7. MODEL SUMMARY -------------------------------------------------------------

## 7.1. Chao1 additive model ---------------------------------------------------

# Print model summary
print(summary(h2d_chao1_model1_additive))

# Check convergence
if(h2d_chao1_model1_additive$sdr$pdHess) {
  cat("\n✓ H2d model converged successfully\n")
} else {
  cat("\n⚠ Warning: H2d model may not have converged properly\n")
}


# Create coefficient table
coef_table_h2d <- broom.mixed::tidy(h2d_chao1_model1_additive, 
                                    effects = "fixed",
                                    conf.int = TRUE) |>
  mutate(Estimate = round(estimate, 4),
         SE = round(std.error, 4),
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
if(h2d_ice_time_model1$sdr$pdHess) {
  cat("\n✓ H2d model converged successfully\n")
} else {
  cat("\n⚠ Warning: H2d model may not have converged properly\n")
}


# Create coefficient table
coef_table_h2d_ice <- broom.mixed::tidy(h2d_ice_time_model1, 
                                    effects = "fixed",
                                    conf.int = TRUE) |>
  mutate(Estimate = round(estimate, 4),
         SE = round(std.error, 4),
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
dispersion_test_h2d_chao1 <- testDispersion(sim_residuals_h2d_chao1)
print(dispersion_test_h2d_chao1)

# Test for zero-inflation
zeroinflation_test_h2d_chao1 <- testZeroInflation(sim_residuals_h2d_chao1)
print(zeroinflation_test_h2d_chao1)

# Test for outliers
outlier_test_h2d_chao1 <- testOutliers(sim_residuals_h2d_chao1)
print(outlier_test_h2d_chao1)

## 8.2. ICE time additive ------------------------------------------------------

# Simulate residuals
sim_residuals_h2d_ice <- simulateResiduals(fittedModel = h2d_ice_time_model1, 
                                             n = 1000)

# Create diagnostic plots
png(filename = here("figures", "Figure_H2d_ICE_additive_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals_h2d_ice)
dev.off()

# Test for dispersion
dispersion_test_h2d_ice <- testDispersion(sim_residuals_h2d_ice)
print(dispersion_test_h2d_ice)

# Test for zero-inflation
zeroinflation_test_h2d_ice <- testZeroInflation(sim_residuals_h2d_ice)
print(zeroinflation_test_h2d_ice)

# Test for outliers
outlier_test_h2d_ice <- testOutliers(sim_residuals_h2d_ice)
print(outlier_test_h2d_ice)

# 9. EXTRACT RANDOM EFFECTS AND MODEL PARAMETERS -------------------------------

## 9.1. Chao1 additive model ---------------------------------------------------

# Get predictions for area × polygon type × land cover
pred_full <- ggpredict(h2d_chao1_model, 
                       terms = c("log_area_km2 [all]", 
                                 "polygon_type", 
                                 "land_cover_name"),
                       type = "fixed")

# Convert to dataframe for plotting
pred_df <- as.data.frame(pred_full) |>
  rename(log_area_km2 = x,
         polygon_type = group,
         land_cover = facet)

# Main plot: Area × Polygon Type, faceted by Land Cover
fig_chao1_predictions <- ggplot(pred_df, 
                                aes(x = log_area_km2, 
                                    y = predicted,
                                    color = polygon_type,
                                    fill = polygon_type)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~land_cover, ncol = 3) +
  scale_color_manual(values = c("Buffer" = "#2ca02c", 
                                "Development" = "#d62728"),
                     name = "Polygon Type") +
  scale_fill_manual(values = c("Buffer" = "#2ca02c", 
                               "Development" = "#d62728"),
                    name = "Polygon Type") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = expression(paste("Log(Area (km"^2, "))")),
       y = "Estimated Completeness") +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        strip.background = element_rect(fill = "grey90", color = "black"),
        strip.text = element_text(size = 10, face = "bold"))

# Save plot to file
ggsave(filename = here("figures", "Figure_H2d_chao1_predictions.png"),
       plot = fig_chao1_predictions, width = 12, height = 8, dpi = 600)
ggsave(filename = here("figures", "Figure_H2d_chao1_predictions.pdf"),
       plot = fig_chao1_predictions, width = 12, height = 8, dpi = 600)

## 9.2. ICE time additive ------------------------------------------------------

# Get predictions for area × polygon type × land cover
pred_full_ice <- ggpredict(h2d_ice_time_model1, 
                       terms = c("log_area_km2 [all]", 
                                 "polygon_type", 
                                 "land_cover_name"),
                       type = "fixed")

# Convert to dataframe for plotting
pred_df_ice <- as.data.frame(pred_full_ice) |>
  rename(log_area_km2 = x,
         polygon_type = group,
         land_cover = facet)

# Main plot: Area × Polygon Type, faceted by Land Cover
fig_ice_predictions <- ggplot(pred_df_ice, 
                                aes(x = log_area_km2, 
                                    y = predicted,
                                    color = polygon_type,
                                    fill = polygon_type)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~land_cover, ncol = 3) +
  scale_color_manual(values = c("Buffer" = "#2ca02c", 
                                "Development" = "#d62728"),
                     name = "Polygon Type") +
  scale_fill_manual(values = c("Buffer" = "#2ca02c", 
                               "Development" = "#d62728"),
                    name = "Polygon Type") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = expression(paste("Log(Area (km"^2, "))")),
       y = "Estimated Completeness") +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        strip.background = element_rect(fill = "grey90", color = "black"),
        strip.text = element_text(size = 10, face = "bold"))

# Save plot to file
ggsave(filename = here("figures", "Figure_H2d_ICE_predictions.png"),
       plot = fig_ice_predictions, width = 12, height = 8, dpi = 600)
ggsave(filename = here("figures", "Figure_H2d_ICE_predictions.pdf"),
       plot = fig_ice_predictions, width = 12, height = 8, dpi = 600)

# 10. CALCULATE EFFECT SIZES ---------------------------------------------------

## 10.1. Chao1 additive model --------------------------------------------------

# Get marginal means for polygon type (averaged across land cover and area)
emmeans_polygon_h2d_chao1 <- emmeans(h2d_chao1_model, 
                               specs = "polygon_type",
                               type = "response")

print(summary(emmeans_polygon_h2d_chao1))

# Calculate pairwise contrast
contrast_polygon_h2d_chao1 <- contrast(emmeans_polygon_h2d_chao1, method = "pairwise", type = "response")
print(summary(contrast_polygon_h2d_chao1))

## 10.2. ICE time additive -----------------------------------------------------

# Get marginal means for polygon type (averaged across land cover and area)
emmeans_polygon_h2d_ice <- emmeans(h2d_ice_time_model1, 
                                     specs = "polygon_type",
                                     type = "response")

print(summary(emmeans_polygon_h2d_ice))

# Calculate pairwise contrast
contrast_polygon_h2d_ice <- contrast(emmeans_polygon_h2d_ice, method = "pairwise", type = "response")
print(summary(contrast_polygon_h2d_ice))

# END OF SCRIPT ----------------------------------------------------------------