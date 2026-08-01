##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 4.1_H2ab_models
# This script contains code to test Hypothesis 2a: Area plan polygons have a 
# greater number of SOR than areas not planned for development
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

library(here)
source(here("scripts", "0_setup.R"))

# Make sure there is a directory in which to save the model outputs
if (!dir.exists(here("data", "models"))) {
  dir.create(here("data", "models"), recursive = TRUE)
}

# Load polygons data
model_data <- readRDS(here("data", "derived_data", "h2_polygon_buffer_data.rds"))

# Quickly inspect the data that was loaded
cat("Rows loaded:", nrow(model_data), "\n") # 259762 (2 per pair)
print(table(model_data$polygon_type))

# 2. PREPARE DATA FOR MODELING -------------------------------------------------

## 2.1. Reshape df to one row per polygon-buffer pair --------------------------

# Pivot from long to wide and have the polygon and buffer counts side by side
pair_data <- model_data |>
  select(pair_id, kommune, english_categories, land_cover_name,
         area_m2_numeric, polygon_type, n_occurrences) |>
  tidyr::pivot_wider(names_from  = polygon_type,
                     values_from = c(n_occurrences, area_m2_numeric)) |>
  rename(sor_polygon  = n_occurrences_Development,
         sor_buffer   = n_occurrences_Buffer,
         area_polygon = area_m2_numeric_Development,
         area_buffer  = area_m2_numeric_Buffer)

# Create variables needed for the model
pair_data <- pair_data |>
  mutate(sor_total = sor_polygon + sor_buffer,
         # calculate the share of the SOR belonging to the polygons
         share_polygon   = ifelse(sor_total > 0, sor_polygon / sor_total, NA_real_),
         # calculate a centered log polygon area so that the intercept in H2a refers to a pair of average size
         log_area_c = as.numeric(scale(log(area_polygon), scale = FALSE)),
         # calcualte and area offset to use in H2a to deal with the pairs where the buffer is larger
         area_offset = log(area_polygon / area_buffer),
         # did this pair have any records at all? (response for the H1 model)
         any_records = as.integer(sor_total > 0),
         # factorise kommune and land-cover name
         kommune_factor = factor(kommune),
         land_cover_name = factor(land_cover_name))

## 2.2. Check the reshaped df --------------------------------------------------

# Check we have exactly one row per pair
stopifnot(nrow(pair_data) == n_distinct(model_data$pair_id))
cat("\nPairs after reshape:", nrow(pair_data), "\n")

# Check that we did not introduce NAs in the counts and that we have finit values everywhere
stopifnot(!any(is.na(pair_data$sor_polygon)),
          !any(is.na(pair_data$sor_buffer)),
          all(is.finite(pair_data$area_offset)),
          all(is.finite(pair_data$log_area_c)))
cat("PASS: counts complete and offset/area finite\n")

# Quick glance at the response variable
cat("Pairs with any records:", sum(pair_data$any_records), "of",
    nrow(pair_data), "(",
    round(100 * mean(pair_data$any_records), 1), "%)\n")
cat("Pairs with zero records in BOTH halves:", sum(pair_data$sor_total == 0), "\n")
cat("Polygon share of records (record-bearing pairs only):\n")
print(summary(pair_data$share_polygon))

# Check that the area offset sits near 0 for most pairs
cat("\nArea offset log(area_polygon/area_buffer) summary:\n")
print(summary(pair_data$area_offset))

# 3. FIT MODELS ----------------------------------------------------------------

## 3.1. H2a Full three-way interaction -----------------------------------------

# Set up model
h2a_tweedie_full_interaction <- glmmTMB(occ_per_km2 ~ log_area_km2 * polygon_type * land_cover_name + 
                                  (1|kommune_factor),
                                data = model_data_complete,
                                family = tweedie())

# Save model output
save(h2a_tweedie_full_interaction, file = here::here("data", "models", "h2a_tweedie_full_interaction.RData"))

## 3.2. H2a Two-way interaction -----------------------------------------

# Set up model
h2a_tweedie_two_way_interaction <- glmmTMB(occ_per_km2 ~ log_area_km2 * polygon_type + log_area_km2 * land_cover_name + 
                                             polygon_type * land_cover_name + 
                                          (1|kommune_factor),
                                        data = model_data_complete,
                                        family = tweedie())

# Save model output
save(h2a_tweedie_two_way_interaction, file = here::here("data", "models", "h2a_tweedie_two_way_interaction.RData"))

# Compare full interaction and two-way interaction models
AICtab(h2a_tweedie_full_interaction, h2a_tweedie_two_way_interaction, base = TRUE)

# Get the best model
best_model <- h2a_tweedie_full_interaction

## 3.3. H2b Additive Model -----------------------------------------------------

# Set up model
h2b_tweedie_additive <- glmmTMB(occ_per_km2 ~ log_area_km2 + polygon_type + land_cover_name + 
                                  (1|kommune_factor),
                                data = model_data_complete,
                                family = tweedie())

# Save model output
save(h2b_tweedie_additive, file = here::here("data", "models", "h2b_tweedie_additive.RData"))

# 4. MODEL SUMMARY -------------------------------------------------------------

## 4.1. H2a Full interaction model ---------------------------------------------

# Get summary
print(summary(best_model))

# Create simple coefficient table
coef_table_h2a <- broom.mixed::tidy(best_model, 
                                    effects = "fixed",
                                    conf.int = TRUE) |>
  mutate(Estimate = round(estimate, 3),
         SE = round(std.error, 3),
         `z value` = round(statistic, 2),
         `p value` = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))) |>
  select(Term = term, Estimate, SE, `z value`, `p value`)

# Save as CSV
write.csv(coef_table_h2a,
          here("figures", "Table_H2a_model1_full_interaction_coefficients.csv"),
          row.names = FALSE)

## 4.2. H2b Additive model -----------------------------------------------------

# Get summary
print(summary(h2b_tweedie_additive))

# Create simple coefficient table
coef_table_h2b <- broom.mixed::tidy(h2b_tweedie_additive, 
                                effects = "fixed",
                                conf.int = TRUE) |>
  mutate(Estimate = round(estimate, 3),
         SE = round(std.error, 3),
         `z value` = round(statistic, 2),
         `p value` = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))) |>
  select(Term = term, Estimate, SE, `z value`, `p value`)

# Save as CSV
write.csv(coef_table_h2b,
          here("figures", "Table_H2b_model1_additive_coefficients.csv"),
          row.names = FALSE)

# 5. MODEL DIAGNOSTICS WITH DHARMA ---------------------------------------------

## 5.1. H2a Full interaction model ---------------------------------------------

# Simulate residuals
sim_residuals_h2a <- simulateResiduals(fittedModel = best_model, 
                                   n = 1000)

# Create diagnostic plots
png(filename = here("figures", "Figure_H2a_tweedie_diagnostics_full_interaction.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals_h2a)
dev.off()

# Test for dispersion
dispersion_test <- testDispersion(sim_residuals_h2a)
print(dispersion_test)
cat("\n")

# Test for zero-inflation
zeroinflation_test <- testZeroInflation(sim_residuals_h2a)
print(zeroinflation_test)
cat("\n")

# Test for outliers
outlier_test <- testOutliers(sim_residuals_h2a)
print(outlier_test)
cat("\n")

## 5.2. H2b Additive model -----------------------------------------------------

# Simulate residuals
sim_residuals <- simulateResiduals(fittedModel = h2b_tweedie_additive, 
                                   n = 1000)

# Create diagnostic plots
png(filename = here("figures", "Figure_H2b_tweedie_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals)
dev.off()

# Test for dispersion
dispersion_test <- testDispersion(sim_residuals)
print(dispersion_test)
cat("\n")

# Test for zero-inflation
zeroinflation_test <- testZeroInflation(sim_residuals)
print(zeroinflation_test)
cat("\n")

# Test for outliers
outlier_test <- testOutliers(sim_residuals)
print(outlier_test)
cat("\n")

# 6. EXTRACT RANDOM EFFECTS ----------------------------------------------------

## 6.1. H2a Full interaction model ---------------------------------------------
random_effects_h2a <- VarCorr(best_model)
print(random_effects_h2a)

# Calculate ICC (intraclass correlation coefficient)
re_var <- as.numeric(random_effects_h2a$cond$kommune_factor[1])
cat("\nRandom effect variance (kommune):", round(re_var, 4), "\n")

## 6.2. H2b Additive model -----------------------------------------------------
random_effects <- VarCorr(h2b_tweedie_additive)
print(random_effects)

# Calculate ICC (intraclass correlation coefficient)
re_var <- as.numeric(random_effects$cond$kommune_factor[1])
cat("\nRandom effect variance (kommune):", round(re_var, 4), "\n")

# 7. EXTRACT PREDICTIONS -------------------------------------------------------

## 7.1. H2a Full interaction model ---------------------------------------------

# Generate predictions
predictions_h2a <- ggpredict(best_model, 
                             terms = c("log_area_km2 [all]", "polygon_type", "land_cover_name"),
                             type = "fixed")

# Convert predictions to data frame
pred_df_h2a <- as.data.frame(predictions_h2a) |>
  rename(log_area_km2 = x, polygon_type = group, land_cover_name = facet)

# Define colours for polygon types
polygon_colours <- c("Buffer" = "#2ca02c", "Development" = "#d62728")

# Plot the predictions (faceted by land-cover)
fig_h2a_predictions <- ggplot(pred_df_h2a, aes(x = log_area_km2, y = predicted,
                                               color = polygon_type, fill = polygon_type)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~land_cover_name, scales = "free_y", ncol = 3) +
  scale_color_manual(values = polygon_colours, name = "Area type") +
  scale_fill_manual(values = polygon_colours, name = "Area type") +
  labs(x = expression(paste("Log(Area (km"^2, "))")),
       y = expression(paste("Predicted occurrences per km"^2))) +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        strip.background = element_rect(fill = "grey90", color = "black"),
        strip.text = element_text(size = 14, face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

# Save H2a prediction figure
ggsave(filename = here("figures", "Figure_H2a_full_interaction_predictions_by_landcover.png"),
       plot = fig_h2a_predictions, width = 14, height = 10, dpi = 600)

ggsave(filename = here("figures", "Figure_H2a_full_interaction_predictions_by_landcover.pdf"),
       plot = fig_h2a_predictions, width = 14, height = 10, dpi = 600)


## 7.2. H2b Additive model -----------------------------------------------------

# Generate predictions across the range of observed areas for each combination
# of polygon type and land cover
predictions_h2b <- ggpredict(h2b_tweedie_additive, 
                             terms = c("log_area_km2 [all]", "polygon_type", "land_cover_name"),
                             type = "fixed")

# Convert predictions to data frame
pred_df_h2b <- as.data.frame(predictions_h2b) |>
  rename(log_area_km2 = x, polygon_type = group, land_cover_name = facet)

# Plot the predictions (faceted by land-cover)
fig_h2b_predictions <- ggplot(pred_df_h2b, aes(x = log_area_km2, y = predicted,
                                           color = polygon_type, fill = polygon_type)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~land_cover_name, scales = "free_y", ncol = 3) +
  scale_color_manual(values = polygon_colours, name = "Area type") +
  scale_fill_manual(values = polygon_colours, name = "Area type") +
  labs(x = expression(paste("Log(Area (km"^2, "))")),
       y = expression(paste("Predicted occurrences per km"^2))) +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        strip.background = element_rect(fill = "grey90", color = "black"),
        strip.text = element_text(size = 14, face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

# Save main prediction figure
ggsave(filename = here("figures", "Figure_H2b_additive_tweedie_predictions_by_landcover.png"),
       plot = fig_h2b_predictions, width = 14, height = 10, dpi = 600)

ggsave(filename = here("figures", "Figure_H2b_additive_tweedie_predictions_by_landcover.pdf"),
       plot = fig_h2b_predictions, width = 14, height = 10, dpi = 600)

# 8. CALCULATE EFFECT SIZES ----------------------------------------------------

## 8.1. H2a Full interaction model ---------------------------------------------

# Get marginal means for polygon type (averaged across land cover and area)
emmeans_polygon_h2a <- emmeans(h2a_tweedie_full_interaction, 
                               specs = "polygon_type",
                               type = "response")
print(summary(emmeans_polygon_h2a))

# Calculate pairwise contrast (Buffer vs Development)
contrast_polygon_h2a <- contrast(emmeans_polygon_h2a, method = "pairwise", type = "response")
print(summary(contrast_polygon_h2a))

## 8.2. H2b Additive model -----------------------------------------------------

# Get marginal means for polygon type
emmeans_polygon_h2b <- emmeans(h2b_tweedie_additive, 
                               specs = "polygon_type",
                               type = "response")
print(summary(emmeans_polygon_h2b))

# Calculate pairwise contrast
contrast_polygon_h2b <- contrast(emmeans_polygon_h2b, method = "pairwise", type = "response")
print(summary(contrast_polygon_h2b))

# Save emmeans results for both models
saveRDS(list(h2a_polygon_type = emmeans_polygon_h2a,
             h2a_contrast = contrast_polygon_h2a,
             h2b_polygon_type = emmeans_polygon_h2b,
             h2b_contrast = contrast_polygon_h2b),
        here("data", "models", "h2ab_tweedie_emmeans.rds"))

# END OF SCRIPT ----------------------------------------------------------------