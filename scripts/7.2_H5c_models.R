##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 7.2_H5c_models
# This script contains code to test H5c: Polygons will have higher richness of 
# alien species than buffers than areas outside of the planned developments
##-----------------------------------------------------------------------------#

# 1. LOAD DATA -----------------------------------------------------------------

# Source code
library(here)
source(here("scripts", "0_setup.R"))

# Load the per-side data built in 7.1
model_data <- readRDS(here("data", "derived_data", "h5_polygon_buffer_data.rds"))

# Load the impact-split per-side datasets that is also built in 7.1
model_data_high  <- readRDS(here("data", "derived_data",
                                 "h5_highimpact_polygon_buffer_data.rds"))
model_data_lower <- readRDS(here("data", "derived_data",
                                 "h5_lowerimpact_polygon_buffer_data.rds"))

# Quick summary
cat("Rows loaded:", nrow(model_data), "\n") # 259762 (2 per pair)
print(table(model_data$polygon_type))
# Buffer  Development 
# 129881      129881 

# 2. PREPARE DATA FOR MODELLING ------------------------------------------------

# Create the dataset for modelling
richness_data <- model_data |>
  mutate(polygon_type = factor(polygon_type, levels = c("Buffer", "Development")),
         log_area_c = as.numeric(scale(log(area_m2_numeric), scale = FALSE)),
         land_cover_name = factor(land_cover_name),
         kommune_factor = factor(kommune),
         pair_id_factor = factor(pair_id))

# Summarise the response
cat("\n=== Alien species richness summary ===\n")
print(summary(richness_data$n_species))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.0000   0.0000   0.0000   0.1009   0.0000 121.0000 
cat("Proportion of sides with zero alien species:",
    round(mean(richness_data$n_species == 0), 3), "\n")   # expect ~0.956
cat("\nRichness distribution among record-bearing sides (note the dominance of 1):\n")
print(table(richness_data$n_species[richness_data$n_species > 0]))
cat("\nMean alien richness by side:\n")
print(tapply(richness_data$n_species, richness_data$polygon_type, mean))
# Buffer     Development 
# 0.12697007  0.07493013 

# 3. FIT MODELS (ALL ALIENS) ---------------------------------------------------

## 3.1. H5c Negative binomial with full interaction ----------------------------

# Set up model
h5c_nbinom_full <- glmmTMB(n_species ~ polygon_type * (log_area_c + land_cover_name) +
                             (1 | kommune_factor/pair_id_factor),
                           data = richness_data,
                           family = nbinom2)

# Save model output
save(h5c_nbinom_full,
     file = here::here("data", "models", "h5c_nbinom_full.RData"))

## 3.2. H5c Negative binomial, additive ----------------------------------------

# Set up model
h5c_nbinom_additive <- glmmTMB(n_species ~ polygon_type + log_area_c +
                                 land_cover_name +
                                 (1 | kommune_factor/pair_id_factor),
                               data = richness_data,
                               family = nbinom2)

# Save model output
save(h5c_nbinom_additive,
     file = here::here("data", "models", "h5c_nbinom_additive.RData"))

# Compare models
AICtab(h5c_nbinom_full, h5c_nbinom_additive, base = TRUE)

## 3.3. H5c Zero-inflated, interaction -----------------------------------------

# Set up model
h5c_zinb_full <- glmmTMB(n_species ~ polygon_type * (log_area_c + land_cover_name) +
                           (1 | kommune_factor/pair_id_factor),
                         data = richness_data,
                         family = nbinom2,
                         ziformula = ~ polygon_type + log_area_c)

# Save model
save(h5c_zinb_full, file = here::here("data", "models", "h5c_zinb_full.RData"))

## 3.4. H5c Zero-inflated, additive --------------------------------------------

# Set up model
h5c_zinb_additive <- glmmTMB(n_species ~ polygon_type + log_area_c + land_cover_name +
                               (1 | kommune_factor/pair_id_factor),
                             data = richness_data,
                             family = nbinom2,
                             ziformula = ~ polygon_type + log_area_c)

# Save model output
save(h5c_zinb_additive, file = here::here("data", "models", "h5c_zinb_additive.RData"))

# Compare the models
AICtab(h5c_zinb_full, h5c_zinb_additive, base = TRUE)

# Use the model with the lower AIC
best_model_h5c <- h5c_zinb_full

## 3.5. H5c Hurdle (zero-truncated NB), interaction ----------------------------

# Set up model
h5c_hurdle_full <- glmmTMB(n_species ~ polygon_type * (log_area_c + land_cover_name) +
                             (1 | kommune_factor/pair_id_factor),
                           data = richness_data,
                           family = truncated_nbinom2,
                           ziformula = ~ polygon_type + log_area_c)

# Save model output
save(h5c_hurdle_full, file = here::here("data", "models", "h5c_hurdle_full.RData"))

## 3.6. H5c Hurdle (zero-truncated NB), additive -------------------------------

# Set up model
h5c_hurdle_additive <- glmmTMB(n_species ~ polygon_type + log_area_c + land_cover_name +
                                 (1 | kommune_factor/pair_id_factor),
                               data      = richness_data,
                               family    = truncated_nbinom2,
                               ziformula = ~ polygon_type + log_area_c)

# Save modek output
save(h5c_hurdle_additive,
     file = here::here("data", "models", "h5c_hurdle_additive.RData"))

# Compare models
AICtab(h5c_hurdle_full, h5c_hurdle_additive, base = TRUE)

# Check convergence of the hurdle models
if (h5c_hurdle_full$sdr$pdHess) {
  cat("\nH5c hurdle (full) converged successfully\n")
} else {
  cat("\nWarning: H5c hurdle (full) may not have converged properly\n")
}

## 3.7. Compare all three model families ---------------------------------------

# Extract AIC
cat("\nNegative binomial vs zero-inflated vs hurdle (full versions):\n")
AICtab(h5c_nbinom_full, h5c_zinb_full, h5c_hurdle_full, base = TRUE)

# Select best H5c model
best_model_h5c <- h5c_zinb_full