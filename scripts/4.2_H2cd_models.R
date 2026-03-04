##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 4.1_H2cd_models
# This script contains code to test Hypothesis 2c: Area plan polygons will have
# higher species richness than areas outside of the planned developments and
# H2d: Completenss of species records will be greater in the devekopment
# polygons than outside
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

library(here)
source(here("scripts", "0_setup.R"))

# Load polygons data
model_data <- readRDS(here("data", "derived_data", "h2_polygon_buffer_data.rds"))

# 2. PREPARE DATA FOR MODELING -------------------------------------------------

# Remove orphaned buffers
dev_ids <- model_data |> filter(polygon_type == "Development") |> pull(polygon_id)
model_data <- model_data |> filter(polygon_id %in% dev_ids)

# Calculate species richness per km²
model_data_complete <- model_data |>
  mutate(polygon_type    = factor(polygon_type, levels = c("Buffer", "Development")),
         land_cover_name = factor(land_cover_name),
         kommune_factor  = factor(kommune),
         # convert area to km²
         area_km2        = area_m2_numeric / 1e6,
         # calculate species richness per km²
         species_per_km2 = n_species / area_km2,
         # log-transform area in km² for model (consistent units)
         log_area_km2    = log(area_km2))

# Check the distribution of the response variable
cat("\n=== Response Variable Summary ===\n")
cat("Species richness per km² (species_per_km2):\n")
print(summary(model_data_complete$species_per_km2))
cat("\nProportion of zeros:", mean(model_data_complete$species_per_km2 == 0), "\n")
cat("Number of zeros:", sum(model_data_complete$species_per_km2 == 0), "\n")
cat("Number of non-zeros:", sum(model_data_complete$species_per_km2 > 0), "\n")

# Check the predictor variable
cat("\nArea (km²):\n")
print(summary(model_data_complete$area_km2))
cat("\nLog(Area km²):\n")
print(summary(model_data_complete$log_area_km2))
cat("\n")

# 3. FIT MODELS ----------------------------------------------------------------

## 3.1. H2c Full three-way interaction -----------------------------------------

# Set up model
h2c_tweedie_full_interaction <- glmmTMB(species_per_km2 ~ log_area_km2 * polygon_type * land_cover_name + 
                                          (1|kommune_factor),
                                        data = model_data_complete,
                                        family = tweedie())

# Save model output
save(h2c_tweedie_full_interaction,
     file = here::here("data", "models", "h2c_tweedie_full_interaction.RData"))

## 3.2. H2c Two-way interaction ------------------------------------------------

# Set up model
h2c_tweedie_two_way_interaction <- glmmTMB(species_per_km2 ~ log_area_km2 * polygon_type + 
                                             log_area_km2 * land_cover_name + 
                                             polygon_type * land_cover_name + 
                                             (1|kommune_factor),
                                           data = model_data_complete,
                                           family = tweedie())

# Save model output
save(h2c_tweedie_two_way_interaction,
     file = here::here("data", "models", "h2c_tweedie_two_way_interaction.RData"))

## 3.3. H2c Additive Model -----------------------------------------------------

# Set up model
h2c_tweedie_additive <- glmmTMB(species_per_km2 ~ log_area_km2 + polygon_type + land_cover_name + 
                                  (1|kommune_factor),
                                data = model_data_complete,
                                family = tweedie())

# Save model output
saveRDS(h2c_tweedie_additive,
        here("data", "models", "h2c_tweedie_additive.rds"))

# Compare full interaction and two-way interaction models
AICtab(h2c_tweedie_full_interaction, h2c_tweedie_two_way_interaction,
       h2c_tweedie_additive, base = TRUE)

# Select best H2c model
best_model_h2c <- h2c_tweedie_two_way_interaction

# 4. MODEL SUMMARY -------------------------------------------------------------

## 4.1. H2c Full interaction model ---------------------------------------------
print(summary(best_model_h2c))

# Check convergence
if(best_model_h2c$sdr$pdHess) {
  cat("\n✓ H2c model converged successfully\n")
} else {
  cat("\n⚠ Warning: H2c model may not have converged properly\n")
}


# Create coefficient table
coef_table_h2c <- broom.mixed::tidy(best_model_h2c, 
                                    effects = "fixed",
                                    conf.int = TRUE) |>
  mutate(Estimate = round(estimate, 4),
         SE = round(std.error, 4),
         `z value` = round(statistic, 2),
         `p value` = ifelse(p.value < 0.001, "<0.001", round(p.value, 4))) |>
  select(Term = term, Estimate, SE, `z value`, `p value`)

# Save coefficient table
write.csv(coef_table_h2c,
          here("figures", "Table_H2c_twoway_interaction_coefficients.csv"),
          row.names = FALSE)

# 5. MODEL DIAGNOSTICS WITH DHARMA ---------------------------------------------

## 5.1. H2c Two-way interaction model ------------------------------------------

# Simulate residuals
sim_residuals_h2c <- simulateResiduals(fittedModel = best_model_h2c, 
                                       n = 1000)

# Create diagnostic plots
png(filename = here("figures", "Figure_H2c_twoway_interaction_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals_h2c)
dev.off()

# Test for dispersion
dispersion_test_h2c <- testDispersion(sim_residuals_h2c)
print(dispersion_test_h2c)

# Test for zero-inflation
zeroinflation_test_h2c <- testZeroInflation(sim_residuals_h2c)
print(zeroinflation_test_h2c)

# Test for outliers
outlier_test_h2c <- testOutliers(sim_residuals_h2c)
print(outlier_test_h2c)

# 6. EXTRACT RANDOM EFFECTS AND MODEL PARAMETERS -------------------------------

## 6.1. H2c Two-way interaction model ------------------------------------------

random_effects_h2c <- VarCorr(best_model_h2c)
print(random_effects_h2c)

# Calculate ICC (intraclass correlation coefficient)
re_var_h2c <- as.numeric(random_effects_h2c$cond$kommune_factor[1])
cat("\nRandom effect variance (kommune):", round(re_var_h2c, 4), "\n")

# 7. EXTRACT PREDICTIONS -------------------------------------------------------

# Define colours for polygon types (used in both H2c models)
polygon_colours <- c("Buffer" = "#2ca02c", "Development" = "#d62728")

## 7.1. H2c Two-way interaction model ------------------------------------------

predictions_h2c <- ggpredict(best_model_h2c, 
                             terms = c("log_area_km2 [all]", "polygon_type", "land_cover_name"),
                             type = "fixed")

# Convert predictions to data frame
pred_df_h2c <- as.data.frame(predictions_h2c) |>
  rename(log_area_km2 = x, polygon_type = group, land_cover_name = facet)

# Plot the predictions
fig_h2c_predictions <- ggplot(pred_df_h2c, aes(x = log_area_km2, y = predicted,
                                               color = polygon_type, fill = polygon_type)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~land_cover_name, scales = "free_y", ncol = 3) +
  scale_color_manual(values = polygon_colours, name = "Area type") +
  scale_fill_manual(values = polygon_colours, name = "Area type") +
  labs(x = expression(paste("Log(Area (km"^2, "))")),
       y = expression(paste("Predicted species per km"^2))) +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        strip.background = element_rect(fill = "grey90", color = "black"),
        strip.text = element_text(size = 14, face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

# Save H2c prediction figure
ggsave(filename = here("figures", "Figure_H2c_twoway_interaction_predictions_by_landcover.png"),
       plot = fig_h2c_predictions, width = 14, height = 10, dpi = 600)

ggsave(filename = here("figures", "Figure_H2c_twoway_interaction_predictions_by_landcover.pdf"),
       plot = fig_h2c_predictions, width = 14, height = 10, dpi = 600)

# 8. CALCULATE EFFECT SIZES ----------------------------------------------------

## 8.1. H2c Two-way interaction model ------------------------------------------

# Get marginal means for polygon type (averaged across land cover and area)
emmeans_polygon_h2c <- emmeans(best_model_h2c, 
                               specs = "polygon_type",
                               type = "response")

print(summary(emmeans_polygon_h2c))

# Calculate pairwise contrast
contrast_polygon_h2c <- contrast(emmeans_polygon_h2c, method = "pairwise", type = "response")
print(summary(contrast_polygon_h2c))


# Save emmeans results for both models - after you ad H2d models!
# saveRDS(list(h2c_full_polygon_type = emmeans_polygon_h2c,
#              h2c_full_contrast = contrast_polygon_h2c,
#              h2c_add_polygon_type = emmeans_polygon_h2c_add,
#              h2c_add_contrast = contrast_polygon_h2c_add),
#         here("data", "models", "h2c_tweedie_emmeans.rds"))

# END OF SCRIPT ----------------------------------------------------------------