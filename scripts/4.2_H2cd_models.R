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


# Compare full interaction and two-way interaction models
AICtab(h2c_tweedie_full_interaction, h2c_tweedie_two_way_interaction, base = TRUE)

# Select best H2c model
best_model_h2c <- h2c_tweedie_full_interaction

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
          here("figures", "Table_H2c_full_interaction_coefficients.csv"),
          row.names = FALSE)

# 5. MODEL DIAGNOSTICS WITH DHARMA ---------------------------------------------

## 5.1. H2c Full interaction model ---------------------------------------------

# Simulate residuals
sim_residuals_h2c <- simulateResiduals(fittedModel = best_model_h2c, 
                                       n = 1000)

# Create diagnostic plots
png(filename = here("figures", "Figure_H2c_full_interaction_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals_h2c, main = "DHARMa Diagnostics - H2c Full Interaction Model")
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
