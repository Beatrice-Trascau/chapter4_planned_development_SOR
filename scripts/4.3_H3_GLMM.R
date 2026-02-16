##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 4.3_H3_GLMM
# This script contains code to test Hypothesis 3: Urban and near-urban polygons
# will have more SOR than othe planned development types
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source setup file
library(here)
source(here("scripts", "0_setup.R"))

# Load polygon all data
polygon_all_data <- readRDS(here("data", "derived_data",
                                 "polygons_occurrences_all_data.rds"))

# 2. PREPARE DATA FOR MODELING -------------------------------------------------

# Filter out Ports & Marinas from planned development category
model_data <- polygon_all_data |>
  filter(english_categories != "Ports") |>
  # log-transform area
  mutate(log_area = log(area_m2_numeric),
        # convert development category and kommune to factors
        development_category = as.factor(english_categories),
        kommune_factor = as.factor(kommune))

# Check if there are any NAs in key variables
cat("n_occurrences:", sum(is.na(model_data$n_occurrences)), "\n")
cat("log_area:", sum(is.na(model_data$log_area)), "\n")
cat("development_category:", sum(is.na(model_data$development_category)), "\n")
cat("kommune_factor:", sum(is.na(model_data$kommune_factor)), "\n")

# Remove rows with NAs in model variables
model_data <- model_data |>
  filter(!is.na(n_occurrences),
         !is.na(log_area),
         !is.na(development_category),
         !is.na(kommune_factor))

# Check sample size for model
cat("Number of municipalities:", n_distinct(model_data$kommune_factor), "\n")
cat("Development categories:", paste(levels(model_data$development_category), collapse = ", "), "\n")

# 3. FIT MODELS ----------------------------------------------------------------

## 3.1. Zero inflated + interaction --------------------------------------------

# Fit model
h3_model1_zinb_interaction <- glmmTMB(n_occurrences ~ log_area * development_category + (1|kommune_factor),
                                      data = model_data,
                                      family = nbinom2,
                                      ziformula = ~log_area + development_category)

# Save model
save(h3_model1_zinb_interaction, file = here::here("data", "models",
                                                   "h3_model1_zinb_interaction.RData"))

## 3.2. Zero infalted no interaction -------------------------------------------

# Fit model
h3_model2_zinb_no_interaction <- glmmTMB(n_occurrences ~ log_area + development_category + (1|kommune_factor),
                                      data = model_data,
                                      family = nbinom2,
                                      ziformula = ~log_area + development_category)

# Save model
save(h3_model2_zinb_no_interaction, file = here::here("data", "models",
                                                   "h3_model2_zinb_no_interaction.RData"))

# Compare models
AICtab(h3_model1_zinb_interaction, h3_model2_zinb_no_interaction, base = TRUE)

# 4. MODEL SUMMARY AND DIAGNOSTICS ---------------------------------------------

## 4.1. Model summary ----------------------------------------------------------

# Get table of the fixed effects
fixed_effects <- tidy(h3_model1_zinb_interaction, 
                      effects = "fixed", conf.int = TRUE)
print(fixed_effects)

# Get table of the random effects
random_effects <- tidy(h3_model1_zinb_interaction, effects = "ran_pars")
print(random_effects)

## 4.2. Check for convergence --------------------------------------------------

if(model_zinb$sdr$pdHess) {
  cat("\nModel converged successfully\n")
} else {
  cat("\nWarning: Model may not have converged properly\n")
}

## 4.3. Model diagnostics with DHARMa ------------------------------------------

sim_residuals <- simulateResiduals(fittedModel = h3_model1_zinb_interaction, 
                                   n = 1000)

# Create diagnostic plots
png(filename = here("figures", "Figure_H3_model_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals)
dev.off()

# Test for dispersion
dispersion_test <- testDispersion(sim_residuals)
cat("\nDispersion test p-value:", dispersion_test$p.value, "\n")

# Test for zero-inflation
zeroinflation_test <- testZeroInflation(sim_residuals)
cat("Zero-inflation test p-value:", zeroinflation_test$p.value, "\n")

## 4.4. Extract random effects -------------------------------------------------

# Get random effect variance
random_effects <- VarCorr(model_zinb)
cat("\nRandom effect variance (kommune):\n")
print(random_effects)

# Calculate ICC (intraclass correlation coefficient)
# Proportion of variance explained by municipality
re_var <- as.numeric(random_effects$cond$kommune_factor[1])
cat("\nRandom effect variance:", round(re_var, 4), "\n")