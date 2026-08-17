##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 7.3_H5d_models
# This script contains code to test H4d: Polygons will have higher sampling
# completeness of alien species than areas outside (buffers)

# N.B. Alien species records are quite sparse so estimators that rely on 
# singletons and doubletons (Chao1) are noisier
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source scripts
library(here)
source(here("scripts", "0_setup.R"))

# Load the alien per-side data and occurrence-level join, built in 7.1
model_data <- readRDS(here("data", "derived_data", "h5_polygon_buffer_data.rds"))
polygon_occurrence_join <- readRDS(here("data", "derived_data",
                                        "h5_polygon_buffer_occurrence_join.rds"))

# Use function to display land-cover labels neatly
pretty_lc <- function(x) {
  x <- gsub("_", " ", x)
  gsub("(^|\\s)([a-z])", "\\1\\U\\2", x, perl = TRUE)
}

# 2. PREPARE DATA FOR THE COMPLETENESS CALCULATIONS ----------------------------

# Reshape data so Chao1 can be calculated (pseudo-abundance = records per species)
chao1_data <- polygon_occurrence_join |>
  filter(!is.na(gbifID)) |>
  group_by(poly_uid, species) |>
  summarise(n_occurrences = n(), .groups = "drop")

# Data for ICE with Time (use years as sampling units)
time_data <- polygon_occurrence_join |>
  filter(!is.na(gbifID), !is.na(year)) |>
  distinct(poly_uid, species, year) |>
  group_by(poly_uid, species) |>
  summarise(n_years = n(), .groups = "drop")

# Calculated total years per side
n_years_per_side <- polygon_occurrence_join |>
  filter(!is.na(gbifID), !is.na(year)) |>
  group_by(poly_uid) |>
  summarise(total_years = n_distinct(year), .groups = "drop")

# 3. CALCULATE COMPLETENESS ESTIMATES ------------------------------------------

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

# Calculate ICE with time as the sampling unit
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

# 4. COMBINE COMPLETENESS ESTIMATES --------------------------------------------

# Base per-side data (n_species / n_occurrences are ALIEN counts here)
completeness_data <- model_data |>
  select(poly_uid, id, pair_id, polygon_type, area_m2_numeric,
         english_categories, kommune, land_cover_name, n_species, n_occurrences)

# Join Chao1 and ICE-time estimates by poly_uid
completeness_data <- completeness_data |>
  left_join(chao1_results |> select(poly_uid, completeness_chao1,
                                    n_occurrences_total, chao1),
            by = "poly_uid") |>
  left_join(ice_time_results |> select(poly_uid, completeness_ice_time,
                                       n_years, ice_time, sample_coverage_time),
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

## 5.1. Check availability of alien data ---------------------------------------

# Try stricter filtering first
availability_alien_records <- completeness_data |>
  summarise(sides_total = n(),
            sides_with_records = sum(n_occurrences > 0, na.rm = TRUE),
            sides_species_ge3 = sum(n_species >= 3,  na.rm = TRUE),
            sides_species_ge5 = sum(n_species >= 5,  na.rm = TRUE),
            sides_species_ge10 = sum(n_species >= 10, na.rm = TRUE),
            sides_occ_ge10 = sum(n_occurrences >= 10, na.rm = TRUE),
            sides_years_ge3 = sum(n_years >= 3, na.rm = TRUE))

# Check the summary of the availability
print(as.data.frame(availability_alien_records))
# sides_total sides_with_records sides_species_ge3 sides_species_ge5 sides_species_ge10 sides_occ_ge10
# 1      259762              11346              2562              1194                315            711
# sides_years_ge3
# 1            1089
# Surprisingly this looks quite good! But if it were <~200, we would need to lower the min_species below to 3

## 5.2. Apply thresholds -------------------------------------------------------

# Minimum thresholds (start at the H4d values; lower min_species to 3 if the
# availability report shows too few sides at >= 5)
min_species     <- 5
min_occurrences <- 10
min_years       <- 3

# Filtered dataset for the Chao1 model
model_data_chao1 <- completeness_data |>
  filter(n_species >= min_species,
         n_occurrences >= min_occurrences,
         !is.na(completeness_chao1), !is.infinite(completeness_chao1),
         completeness_chao1 > 0, completeness_chao1 <= 1)

# Filtered dataset for the ICE-time model
model_data_ice_time <- completeness_data |>
  filter(n_species >= min_species,
         n_years >= min_years,
         !is.na(completeness_ice_time), !is.infinite(completeness_ice_time),
         completeness_ice_time > 0, completeness_ice_time <= 1)

# Check how much data there is in each filtered df
cat("\nData after filtering:\n")
cat("  Chao1 model:   ", nrow(model_data_chao1),    "sides\n") # 550
cat("  ICE-time model:", nrow(model_data_ice_time), "sides\n\n") # 518

# Add warning if the alien data is too little to model properly
if (nrow(model_data_chao1) < 100 ||
    n_distinct(model_data_chao1$polygon_type) < 2) {
  warning("Very few alien sides survive the Chao1 thresholds - lower min_species ",
          "(most alien sides hold 1 species) or reconsider per-side completeness.")
}
if (nrow(model_data_ice_time) < 100 ||
    n_distinct(model_data_ice_time$polygon_type) < 2) {
  warning("Very few alien sides survive the ICE-time thresholds - lower ",
          "min_species / min_years, or reconsider per-side completeness.")
} # wohoooo! it looks like we can proceed!

# Quick summary of the complete polygon-buffer pairs qualifying for Chao1
cat("Qualifying sides by type (Chao1):\n")
print(table(model_data_chao1$polygon_type))
# Buffer Development 
# 341         209 
complete_pairs_chao1 <- model_data_chao1 |>
  count(pair_id) |> filter(n == 2) |> nrow()
cat("Complete PAIRS (both sides qualify), Chao1:", complete_pairs_chao1, "\n\n") #95

# Quick summary of the complete polygon-buffer pairs qualifying for ICE
cat("Qualifying sides by type (ICE-time):\n")
print(table(model_data_ice_time$polygon_type))
# Buffer Development 
# 350         168 
complete_pairs_ice <- model_data_ice_time |>
  count(pair_id) |> filter(n == 2) |> nrow()
cat("Complete PAIRS (both sides qualify), ICE-time:", complete_pairs_ice, "\n\n") # 81

# Save the completeness data
saveRDS(completeness_data,
        here("data", "derived_data", "h5d_completeness_data.rds"))

# 6. FIT MODELS ----------------------------------------------------------------

## 6.1. Chao1 additive ---------------------------------------------------------

# Fit ordered beta to deal with the 0s and 1s
h5d_chao1_model1_additive <- glmmTMB(completeness_chao1 ~ polygon_type + log_area_km2 +
                                       land_cover_name + log(n_occurrences_total) +
                                       (1 | kommune_factor/pair_id_factor),
                                     data = model_data_chao1,
                                     family = ordbeta(link = "logit"))
# Save model output
save(h5d_chao1_model1_additive,
     file = here::here("data", "models", "h5d_chao1_model1_additive.RData"))

## 6.2. Chao1 interaction ------------------------------------------------------

# Set up model
h5d_chao1_model2_interaction <- glmmTMB(completeness_chao1 ~ polygon_type * log_area_km2 +
                                          land_cover_name * log(n_occurrences_total) +
                                          (1 | kommune_factor/pair_id_factor),
                                        data   = model_data_chao1,
                                        family = ordbeta(link = "logit"))

# Save model output
save(h5d_chao1_model2_interaction,
     file = here::here("data", "models", "h5d_chao1_model2_interaction.RData"))

## 6.3. Chao1 additive with dispersion model -----------------------------------

# Set up model
h5d_chao1_model3_disp <- glmmTMB(completeness_chao1 ~ polygon_type + log_area_km2 +
                                   land_cover_name + log(n_occurrences_total) +
                                   (1 | kommune_factor/pair_id_factor),
                                 dispformula = ~ log(n_occurrences_total),
                                 data   = model_data_chao1,
                                 family = ordbeta(link = "logit"))

# Save model output
save(h5d_chao1_model3_disp,
     file = here::here("data", "models", "h5d_chao1_model3_disp.RData"))

# Compare models
AICtab(h5d_chao1_model1_additive, h5d_chao1_model2_interaction,
       h5d_chao1_model3_disp, base = TRUE)
#                              AIC    dAIC   df
# h5d_chao1_model1_additive    -138.1    0.0 15
# h5d_chao1_model3_disp        -136.3    1.8 16
# h5d_chao1_model2_interaction -130.5    7.6 22

# Choose the better model
chao1_final <- h5d_chao1_model1_additive
chao1_final_data <- model_data_chao1

## 6.4. Chao1 strict-threshold sensitivity check ---------------------------

# Use a stricter threshold for the number of species and occurrences
min_species_strict <- 10
min_occurrences_strict <- 30

# Filter the data
model_data_chao1_strict <- completeness_data |>
  filter(n_species >= min_species_strict,
         n_occurrences >= min_occurrences_strict,
         !is.na(completeness_chao1), !is.infinite(completeness_chao1),
         completeness_chao1 > 0, completeness_chao1 <= 1)

# Keep only the strict pairs
strict_pairs <- model_data_chao1_strict |>
  count(pair_id) |> filter(n == 2) |> nrow()
cat("\nChao1 strict-threshold sides:", nrow(model_data_chao1_strict),
    "| complete pairs:", strict_pairs, "\n")
#Chao1 strict-threshold sides: 83 | complete pairs: 15 
print(table(model_data_chao1_strict$polygon_type))
# Buffer Development 
# 46          37 

# Only fit the strict nonlinear model if there is genuinely enough strict data
strict_ok <- nrow(model_data_chao1_strict) >= 300 &&
  strict_pairs >= 50 &&
  n_distinct(model_data_chao1_strict$polygon_type) == 2

if (strict_ok) {
  h5d_chao1_model4_disp_poly <- glmmTMB(completeness_chao1 ~ polygon_type + log_area_km2 +
                                          land_cover_name + poly(log(n_occurrences_total), 2) +
                                          (1 | kommune_factor/pair_id_factor),
                                        dispformula = ~ log(n_occurrences_total),
                                        data   = model_data_chao1_strict,
                                        family = ordbeta(link = "logit"))
  save(h5d_chao1_model4_disp_poly,
       file = here::here("data", "models", "h5d_chao1_model4_disp_poly.RData"))
  cat("Strict nonlinear model fitted as a sensitivity check.\n")
} else {
  cat("Strict thresholds leave too little alien data - model4_disp_poly skipped.\n")
} # Strict thresholds leave too little alien data - model4_disp_poly skipped

## 6.5. ICE-time additive ------------------------------------------------------

# Set up model
h5d_ice_time_model1 <- glmmTMB(completeness_ice_time ~ polygon_type + log_area_km2 +
                                 land_cover_name + log(n_years) +
                                 (1 | kommune_factor/pair_id_factor),
                               data = model_data_ice_time,
                               family = ordbeta(link = "logit"))

# Save model output
save(h5d_ice_time_model1,
     file = here::here("data", "models", "h5d_ice_time_model1.RData"))

## 6.6. ICE-time interaction ---------------------------------------------------

# Set up model
h5d_ice_time_model2 <- glmmTMB(completeness_ice_time ~ polygon_type * log_area_km2 *
                                 land_cover_name + log(n_years) +
                                 (1 | kommune_factor/pair_id_factor),
                               data = model_data_ice_time,
                               family = ordbeta(link = "logit"))

# Issue with convergence!

# Save model output
save(h5d_ice_time_model2,
     file = here::here("data", "models", "h5d_ice_time_model2.RData"))

# Compare models
AICtab(h5d_ice_time_model1, h5d_ice_time_model2, base = TRUE)
# AIC   dAIC  df
# h5d_ice_time_model1 409.9   0.0 15
# h5d_ice_time_model2 435.1  25.2 33

## 6.7. Random-effect check ----------------------------------------------------

# With few complete pairs the nested pair-level variance can collapse toward zero , which would make it unidentifiable 
# Check if this is the case and automatically refit the model with a kommune-only random effect 
pair_var_near_zero <- function(model, tol = 1e-4) {
  vc <- VarCorr(model)$cond
  nm <- grep("pair_id_factor", names(vc), value = TRUE)
  if (length(nm) == 0) return(FALSE)
  as.numeric(attr(vc[[nm[1]]], "stddev")[1]) < tol
}

# Check if any of the estimators need to be refitted
cat("\nPair-level RE near zero?  Chao1 headline:", pair_var_near_zero(chao1_final),
    "| ICE-time:", pair_var_near_zero(h5d_ice_time_model1), "\n")
# Pair-level RE near zero?  Chao1 headline: FALSE | ICE-time: FALSE

# Chao1 check & refit if needed
if (pair_var_near_zero(chao1_final)) {
  cat("  -> refitting Chao1 headline with kommune-only RE\n")
  chao1_final <- glmmTMB(completeness_chao1 ~ polygon_type + log_area_km2 +
                           land_cover_name + log(n_occurrences_total) +
                           (1 | kommune_factor),
                         data = chao1_final_data,
                         family = ordbeta(link = "logit"))
}

# ICE-time check & refit if needed
if (pair_var_near_zero(h5d_ice_time_model1)) {
  cat("  -> refitting ICE-time with kommune-only RE\n")
  h5d_ice_time_model1 <- glmmTMB(completeness_ice_time ~ polygon_type + log_area_km2 +
                                   land_cover_name + log(n_years) +
                                   (1 | kommune_factor),
                                 data = model_data_ice_time,
                                 family = ordbeta(link = "logit"))
}

# 7. MODEL SUMMARY -------------------------------------------------------------

## 7.1. Chao1 ------------------------------------------------------------------

# Quick summary
print(summary(chao1_final))

# Check convergence
if (chao1_final$sdr$pdHess) {
  cat("\nChao1 headline model converged successfully\n")
} else {
  cat("\nWarning: Chao1 headline model may not have converged\n")
} # Chao1 headline model converged successfully

# Create a tidy coefficient table to use in the manuscript
coef_table_h5d <- broom.mixed::tidy(chao1_final, effects = "fixed", conf.int = TRUE) |>
  mutate(Estimate = round(estimate, 4), SE = round(std.error, 4),
         `z value` = round(statistic, 2),
         `p value` = ifelse(p.value < 0.001, "<0.001", round(p.value, 4))) |>
  select(Term = term, Estimate, SE, `z value`, `p value`)

# Save to file 
write.csv(coef_table_h5d,
          here("figures", "Table_H5d_Chao1_coefficients.csv"), row.names = FALSE)

## 7.2. ICE-time ---------------------------------------------------------------

# Quick summary
print(summary(h5d_ice_time_model1))

# Check convergence
if (h5d_ice_time_model1$sdr$pdHess) {
  cat("\nICE-time model converged successfully\n")
} else {
  cat("\nWarning: ICE-time model may not have converged\n")
}

# Create a tidy coefficient table to use in the manuscript
coef_table_h5d_ice <- broom.mixed::tidy(h5d_ice_time_model1, effects = "fixed",
                                        conf.int = TRUE) |>
  mutate(Estimate = round(estimate, 4), SE = round(std.error, 4),
         `z value` = round(statistic, 2),
         `p value` = ifelse(p.value < 0.001, "<0.001", round(p.value, 4))) |>
  select(Term = term, Estimate, SE, `z value`, `p value`)

# Save to file
write.csv(coef_table_h5d_ice,
          here("figures", "Table_H5d_ICE_additive_coefficients.csv"), row.names = FALSE)

# 8. MODEL DIAGNOSTICS ---------------------------------------------------------

## 8.1. Chao1 model ------------------------------------------------------------

# Simulate residuals 
sim_residuals_h5d_chao1 <- simulateResiduals(fittedModel = chao1_final, n = 1000)

# Plot diagnostics
png(filename = here("figures", "Figure_H5d_Chao1_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals_h5d_chao1)
dev.off()

# Check dispersion
print(testDispersion(sim_residuals_h5d_chao1))

# Check outliers
print(testOutliers(sim_residuals_h5d_chao1))

## 8.2. ICE-time ---------------------------------------------------------------

# Simulate residuals 
sim_residuals_h5d_ice <- simulateResiduals(fittedModel = h5d_ice_time_model1, n = 1000)

# Plot diagnostics
png(filename = here("figures", "Figure_H5d_ICE_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals_h5d_ice)
dev.off()

# Check dispersion
print(testDispersion(sim_residuals_h5d_ice))

# Check outliers
print(testOutliers(sim_residuals_h5d_ice))

# 9. MODEL PREDICTION FIGURES --------------------------------------------------

# Define colour scheme
polygon_colours <- c("Buffer" = "#E66101", "Development" = "#5E3C99")

# Function to extract predictions
make_pred_df <- function(model, data) {
  pred <- ggpredict(model,
                    terms = c("log_area_km2 [n=100]", "polygon_type", "land_cover_name"),
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

# Function to plot predictions
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
         y = "Estimated Alien Species Completeness") +
    theme_classic() +
    theme(panel.grid = element_blank(), axis.title = element_text(size = 14),
          axis.text = element_text(size = 12), legend.position = "bottom",
          strip.background = element_rect(fill = "grey90", colour = "black"),
          strip.text = element_text(size = 12, face = "bold"))
}

## 9.1. Chao1 model ------------------------------------------------------------

# Predict values
pred_df_chao1 <- make_pred_df(chao1_final, chao1_final_data)

# Plot predictions
fig_chao1_predictions <- completeness_figure(pred_df_chao1)

# Save figure to file
ggsave(here("figures", "Figure_H5d_chao1_predictions.png"),
       fig_chao1_predictions, width = 14, height = 10, dpi = 600)
ggsave(here("figures", "Figure_H5d_chao1_predictions.pdf"),
       fig_chao1_predictions, width = 14, height = 10, dpi = 600)

## 9.2. ICE-time model ---------------------------------------------------------

# Predict values
pred_df_ice <- make_pred_df(h5d_ice_time_model1, model_data_ice_time)

# Plot predictions
fig_ice_predictions <- completeness_figure(pred_df_ice)

# Save figure to file
ggsave(here("figures", "Figure_H5d_ICE_predictions.png"),
       fig_ice_predictions, width = 14, height = 10, dpi = 600)
ggsave(here("figures", "Figure_H5d_ICE_predictions.pdf"),
       fig_ice_predictions, width = 14, height = 10, dpi = 600)

# 10. HYPOTHESIS TESTING -------------------------------------------------------

## 10.1. Chao1 model -----------------------------------------------------------

# Get marginal means for polygon type averaged across land-cover and area
emmeans_polygon_h5d_chao1 <- emmeans(chao1_final, specs = "polygon_type",
                                     type = "response")

# Get summary
cat("Estimated alien completeness by side (Chao1):\n")
print(summary(emmeans_polygon_h5d_chao1))
# polygon_type response     SE  df asymp.LCL asymp.UCL
# Buffer          0.518 0.0338 Inf     0.451     0.583
# Development     0.505 0.0340 Inf     0.439     0.571

# Calculate pairwise contrast
contrast_polygon_h5d_chao1 <- contrast(emmeans_polygon_h5d_chao1,
                                       method = "revpairwise", type = "response")

# Check summary of contrast
cat("\nDevelopment vs Buffer (Chao1 alien completeness):\n")
print(summary(contrast_polygon_h5d_chao1, infer = TRUE))
# contrast             odds.ratio     SE  df asymp.LCL asymp.UCL null z.ratio p.value
# Development / Buffer      0.951 0.0745 Inf     0.816      1.11    1  -0.637  0.5240

# Hypothesis verdict from the odds-ratio CI
con_df <- as.data.frame(confint(contrast_polygon_h5d_chao1))
or_col <- grep("ratio|estimate", names(con_df), value = TRUE)[1]
or_lo  <- con_df[[grep("LCL|lower", names(con_df), value = TRUE)[1]]]
or_hi  <- con_df[[grep("UCL|upper", names(con_df), value = TRUE)[1]]]
cat(sprintf("\nChao1 completeness OR (Development / Buffer): %.3f  [%.3f, %.3f]\n",
            con_df[[or_col]], or_lo, or_hi))
if (or_lo > 1) {
  cat("H5d SUPPORTED (Chao1): development polygons have higher alien completeness.\n")
} else if (or_hi < 1) {
  cat("H5d NOT supported (Chao1): development polygons have LOWER completeness.\n")
} else {
  cat("H5d inconclusive (Chao1): the completeness OR CI includes 1.\n")
} # H5d inconclusive (Chao1): the completeness OR CI includes 1.

## 10.2. ICE-time model --------------------------------------------------------

# Get marginal means for polygon type averaged across land-cover and area
emmeans_polygon_h5d_ice <- emmeans(h5d_ice_time_model1, specs = "polygon_type",
                                   type = "response")

# Get summary
cat("\nEstimated alien completeness by side (ICE-time):\n")
print(summary(emmeans_polygon_h5d_ice))
# polygon_type response     SE  df asymp.LCL asymp.UCL
# Buffer          0.273 0.0218 Inf     0.233     0.318
# Development     0.281 0.0250 Inf     0.235     0.333

# Calculate pairwise contrast
contrast_polygon_h5d_ice <- contrast(emmeans_polygon_h5d_ice,
                                     method = "revpairwise", type = "response")

# Check summary of contrast
cat("\nDevelopment vs Buffer (ICE-time alien completeness):\n")
print(summary(contrast_polygon_h5d_ice, infer = TRUE))
# contrast             odds.ratio     SE  df asymp.LCL asymp.UCL null z.ratio p.value
# Development / Buffer       1.04 0.0976 Inf     0.867      1.25    1   0.434  0.6645

# Save inference objects
saveRDS(list(chao1_by_side  = emmeans_polygon_h5d_chao1,
             chao1_contrast = contrast_polygon_h5d_chao1,
             ice_by_side    = emmeans_polygon_h5d_ice,
             ice_contrast   = contrast_polygon_h5d_ice),
        here("data", "models", "h5d_completeness_emmeans.rds"))

## 10.3. Check robustness across estimators ------------------------------------

# Pull the polygon vs buffer completeness odds ration from every fitted estimator into one table so we can check
# if the direction and significance (or lack thereof) hold across the estimators
completeness_contrast <- function(model, label) {
  emm <- emmeans(model, specs = "polygon_type", type = "response")
  d <- as.data.frame(confint(contrast(emm, method = "revpairwise", type = "response")))
  or_col <- grep("ratio|estimate", names(d), value = TRUE)[1]
  or <- d[[or_col]]
  lo <- d[[grep("LCL|lower", names(d), value = TRUE)[1]]]
  hi <- d[[grep("UCL|upper", names(d), value = TRUE)[1]]]
  data.frame(estimator = label,
             OR = round(or, 3), lower = round(lo, 3), upper = round(hi, 3),
             verdict = if (lo > 1) "higher in polygons"
             else if (hi < 1) "lower in polygons"
             else "inconclusive")
}

# Combine the estimator comparisons
completeness_comparison <- bind_rows(completeness_contrast(chao1_final, "Chao1 (abundance)"),
                                     completeness_contrast(h5d_ice_time_model1, "ICE-time (incidence)"))

# Print the comparisons
print(completeness_comparison)
# estimator    OR lower upper      verdict
# 1    Chao1 (abundance) 0.951 0.816 1.109 inconclusive
# 2 ICE-time (incidence) 1.041 0.867 1.251 inconclusive

# Save comparison
write.csv(completeness_comparison,
          here("figures", "Table_H5d_completeness_estimator_comparison.csv"),
          row.names = FALSE)

# END OF SCRIPT ----------------------------------------------------------------