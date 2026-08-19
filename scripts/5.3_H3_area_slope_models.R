##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 5.3_H3_area_slope_models
# This script contains code to test Hypothesis 3 in a different way: look at
# occurrence density scales with polygon area depending on land cover
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# SOurce setup script
library(here)
source(here("scripts", "0_setup.R"))

# Read in the model data
model_data_raw <- readRDS(here("data", "derived_data", "h2_polygon_buffer_data.rds"))

# Use a helper function to display the land-cover names nicely
pretty_lc <- function(x) {
  x <- gsub("_", " ", x)
  gsub("(^|\\s)([a-z])", "\\1\\U\\2", x, perl = TRUE)
}

# 2. PREPARE DATA FOR MODELLING ------------------------------------------------

# Keep only the development polygons
model_data <- model_data_raw |>
  filter(polygon_type == "Development",
         english_categories != "Ports") |>
  mutate(area_km2 = area_m2_numeric / 1e6,
         log_area_km2 = log(area_km2),
         land_cover_name = factor(land_cover_name),
         kommune_factor = factor(kommune)) |>
  filter(!is.na(n_occurrences), !is.na(log_area_km2),
         !is.na(land_cover_name), !is.na(kommune_factor))

# Quickly glance at the data
cat("Development polygons:", nrow(model_data), "\n") #129881
cat("Municipalities:", n_distinct(model_data$kommune_factor), "\n") #353
cat("\nPolygons per land cover:\n")
print(table(model_data$land_cover_name))
# Cropland             Forest          Grassland          Heathland        Settlements Sparsely_vegetated 
# 10036              89381               2473               8581              14734               1903 
# Wetlands 
# 2773 
cat("\nOccurrence count summary:\n")
print(summary(model_data$n_occurrences))
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.000     0.000     0.000     2.491     0.000 22318.000 
cat("Proportion of zero-occurrence polygons:",
    round(mean(model_data$n_occurrences == 0), 3), "\n") #0.909

# 3. FIT MODELS ----------------------------------------------------------------

## 3.1. NB interaction ---------------------------------------------------------

# Define model
h3_nb_interaction <- glmmTMB(n_occurrences ~ log_area_km2 * land_cover_name +
                               (1 | kommune_factor),
                             data = model_data,
                             family = nbinom2)

# Save model output
save(h3_nb_interaction, file = here::here("data", "models", "h3_nb_interaction.RData"))

## 3.2. NB additive ------------------------------------------------------------

# Define model
h3_nb_additive <- glmmTMB(n_occurrences ~ log_area_km2 + land_cover_name +
                            (1 | kommune_factor),
                          data = model_data,
                          family = nbinom2)

# Save the output
save(h3_nb_additive, file = here::here("data", "models", "h3_nb_additive.RData"))

## 3.3. Zero inflated NB interaction -------------------------------------------

# Define model
h3_zinb_interaction <- glmmTMB(n_occurrences ~ log_area_km2 * land_cover_name +
                                 (1 | kommune_factor),
                               data = model_data,
                               family = nbinom2,
                               ziformula = ~ log_area_km2 + land_cover_name)

# Save model output
save(h3_zinb_interaction, file = here::here("data", "models", "h3_zinb_interaction.RData"))

# Compare models
AICtab(h3_nb_interaction, h3_nb_additive, base = TRUE)
#                   AIC      dAIC     df
# h3_nb_interaction 143111.4      0.0 16
# h3_nb_additive    143125.0     13.6 10

## 3.4. Zero inflation NB additive ---------------------------------------------

# Define model
h3_zinb_additive <- glmmTMB(n_occurrences ~ log_area_km2 + land_cover_name +
                              (1 | kommune_factor),
                               data = model_data,
                               family = nbinom2,
                               ziformula = ~ log_area_km2 + land_cover_name)

# Save model output
save(h3_zinb_additive, file = here::here("data", "models", "h3_zinb_additive.RData"))

# Compare models
AICtab(h3_zinb_additive, h3_zinb_interaction, base = TRUE)
#                     AIC      dAIC     df
# h3_zinb_additive    139078.9      0.0 18
# h3_zinb_interaction 139085.5      6.7 24

# Keep the best model
best_model_h3 <- h3_zinb_additive

# 4. MODEL SUMMARY, DIGANOSTICS & RANDOM EFFECTS -------------------------------

## 4.1. Model summary ----------------------------------------------------------

# Quick look at the summary
print(summary(best_model_h3))

# Check if the model converged
if (best_model_h3$sdr$pdHess) {
  cat("\nH3 model converged successfully\n")
} else {
  cat("\nWarning: H3 model may not have converged\n")
}

# Extract coefficient table
coef_table_h3 <- broom.mixed::tidy(best_model_h3,
                                   effects = "fixed", conf.int = TRUE) |>
  mutate(Estimate  = round(estimate, 3),
         SE = round(std.error, 3),
         `z value` = round(statistic, 2),
         `p value` = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))) |>
  select(Term = term, Estimate, SE, `z value`, `p value`)

# Save model output to file
write.csv(coef_table_h3,
          here("figures", "Table_H3_areaslope_coefficients.csv"),
          row.names = FALSE)

## 4.2. Model diagnostic -------------------------------------------------------

# Simulate residuals
sim_residuals_h3 <- simulateResiduals(fittedModel = best_model_h3, n = 1000)

# Save diagnostic figure to file
png(filename = here("figures", "Figure_H3_areaslope_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals_h3)
dev.off()

# Test dispersion
print(testDispersion(sim_residuals_h3))

# Test zero inflation
print(testZeroInflation(sim_residuals_h3))

# Test outliers
print(testOutliers(sim_residuals_h3))

## 4.3. Random effects ---------------------------------------------------------

# Extract random effects
random_effects_h3 <- VarCorr(best_model_h3)
print(random_effects_h3)
# Conditional model:
#   Groups         Name        Std.Dev.
# kommune_factor (Intercept) 1.3105   

# Check the structure with zero inflation
re_var_h3 <- as.numeric(attr(random_effects_h3$cond$kommune_factor, "stddev"))^2

# Alternatively
vc <- VarCorr(best_model_h3)
print(vc)
#  Groups         Name        Std.Dev.
# kommune_factor (Intercept) 1.3105 
kommune_var <- attr(vc$cond$kommune_factor, "stddev")^2
cat("Random effect variance (kommune):", round(kommune_var, 4), "\n") # 1.7175

# 5. HYPOTHESIS TESTING --------------------------------------------------------

## 5.1. Does the area slope differ by land cover -------------------------------

cat("\n=== HYPOTHESIS 3 TESTING ===\n")
cat("H3: the area-density scaling relationship differs across land covers.\n\n")
cat("Interaction LRT (additive vs interaction, ZINB conditional part):\n")
lrt_h3 <- anova(h3_zinb_additive, h3_zinb_interaction)
print(lrt_h3)

if (lrt_h3$`Pr(>Chisq)`[2] < 0.05) {
  cat("\nH3 SUPPORTED: the area slope differs by land cover (interaction p < 0.05).\n")
} else {
  cat("\nH3 not supported: no evidence the area slope differs by land cover.\n")
}

# H3 not supported: no evidence the area slope differs by land cover
# The additive model is also the one that is prefferred by the AIC comparison
# But for plotting purposes, we will still use the model with an interaction term

## 5.2. Per-land-cover area slopers --------------------------------------------

# Use emtrends on the conditional component of the model
slopes_emtrends <- emtrends(h3_zinb_interaction, ~ land_cover_name, var = "log_area_km2")
cat("\nCount-scale elasticities (d log count / d log area) by land cover:\n")

# Look at the summary
print(summary(slopes_emtrends))

# Add them to df
slopes_df <- as.data.frame(summary(slopes_emtrends))
el_col <- grep("trend", names(slopes_df), value = TRUE)[1]
lo_col <- grep("LCL|lower", names(slopes_df), value = TRUE)[1]
hi_col <- grep("UCL|upper", names(slopes_df), value = TRUE)[1]

# Add effect of area on density to the df
slopes_df <- slopes_df |>
  mutate(density_slope = .data[[el_col]] - 1,   # effect of area on DENSITY
         density_lo = .data[[lo_col]] - 1,
         density_hi = .data[[hi_col]] - 1,
         direction = ifelse(density_slope > 0, "Positive", "Negative"))

# Save to file
write.csv(slopes_df,
          here("figures", "Table_H3_areaslope_by_landcover.csv"),
          row.names = FALSE)

## 5.3. Pairwise slope contrasts -----------------------------------------------

# Get the Tukey-adjusted contrasts
slope_contrasts <- contrast(slopes_emtrends, method = "pairwise", adjust = "tukey")
cat("\nPairwise slope comparisons (Tukey-adjusted):\n")

# Quick look at the summary
print(summary(slope_contrasts, infer = TRUE))

# Save to file
write.csv(as.data.frame(summary(slope_contrasts, infer = TRUE)),
          here("figures", "Table_H3_pairwise_slope_comparisons.csv"),
          row.names = FALSE)
saveRDS(list(interaction_lrt = lrt_h3,
             slopes          = slopes_emtrends,
             pairwise        = slope_contrasts),
        here("data", "models", "h3_areaslope_inference.rds"))

# 6. FIGURES -------------------------------------------------------------------

# Create a colour pallette to use in the figures
slope_colours <- c("Positive" = "#E66101", "Negative" = "#5E3C99")   # PuOr

## 6.1. Area-density slope by land-cover ---------------------------------------

# Plot figure
(fig_h3_slopes <- ggplot(slopes_df,
                        aes(x = reorder(land_cover_name, density_slope),
                            y = density_slope, colour = direction)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.6) +
  geom_pointrange(aes(ymin = density_lo, ymax = density_hi),
                  linewidth = 0.9, size = 0.6) +
  scale_colour_manual(values = slope_colours, name = "Effect direction") +
  scale_x_discrete(labels = pretty_lc) +
  coord_flip() +
  labs(x = "Land-cover",
       y = "Effect of log(Area) on Occurrence Density") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 13),
        axis.text  = element_text(size = 12),
        legend.position = "bottom"))

# Save to file
ggsave(filename = here("figures", "Figure_H3_areaslope_by_landcover.png"),
       plot = fig_h3_slopes, width = 11, height = 7, dpi = 600)
ggsave(filename = here("figures", "Figure_H3_areaslope_by_landcover.pdf"),
       plot = fig_h3_slopes, width = 11, height = 7, dpi = 600)

## 6.2. Pairwise slope contrasts -----------------------------------------------

# Create df of the slope contrasts to use for plotting
pw_df <- as.data.frame(summary(slope_contrasts, infer = TRUE))
est_col <- grep("estimate", names(pw_df), value = TRUE)[1]
plo_col <- grep("LCL|lower", names(pw_df), value = TRUE)[1]
phi_col <- grep("UCL|upper", names(pw_df), value = TRUE)[1]
pw_df <- pw_df |>
  rename(estimate = all_of(est_col), conf.low = all_of(plo_col), conf.high = all_of(phi_col)) |>
  mutate(contrast = gsub("_", " ", contrast),
         significant = ifelse(p.value < 0.05, "Yes", "No"))

# Plot figure
(fig_h3_contrasts <- ggplot(pw_df,
                           aes(x = estimate, y = reorder(contrast, estimate),
                               colour = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), linewidth = 0.6) +
  scale_colour_manual(values = c("No" = "grey65", "Yes" = "#5E3C99"),
                      name = "Significant (p < 0.05)") +
  labs(x = "Difference in Area Slopes (95% CI)", y = "Land-cover Comparison") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 13),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 11),
        legend.position = "bottom"))

# Save to file
ggsave(filename = here("figures", "Figure_H3_pairwise_slope_contrasts.png"),
       plot = fig_h3_contrasts, width = 12, height = 10, dpi = 600)
ggsave(filename = here("figures", "Figure_H3_pairwise_slope_contrasts.pdf"),
       plot = fig_h3_contrasts, width = 12, height = 10, dpi = 600)

## 5.3. Predicted density vs area, faceted by land-cover -----------------------

# Function to predict each land-cover over its own observed area range
predict_density_by_lc <- function(model, data, n = 100) {
  lcs <- levels(droplevels(factor(data$land_cover_name)))
  preds <- lapply(lcs, function(lc) {
    rng      <- range(data$log_area_km2[data$land_cover_name == lc], na.rm = TRUE)
    seq_vals <- round(seq(rng[1], rng[2], length.out = n), 5)
    term_str <- paste0("log_area_km2 [", paste(seq_vals, collapse = ","), "]")
    ggpredict(model, terms = term_str,
              condition = c(land_cover_name = lc), type = "fixed") |>
      as.data.frame() |>
      rename(log_area_km2 = x) |>
      mutate(land_cover_name = lc,
             area_km2 = exp(log_area_km2),
             density = predicted / area_km2,
             d_low = conf.low  / area_km2,
             d_high = conf.high / area_km2)
  })
  bind_rows(preds)
}

# Predict the values
pred_df_h3 <- predict_density_by_lc(best_model_h3, model_data)

# Plot figure
(fig_h3_predictions <- ggplot(pred_df_h3,
                             aes(x = log_area_km2, y = density)) +
  geom_ribbon(aes(ymin = d_low, ymax = d_high), fill = "#5E3C99", alpha = 0.2) +
  geom_line(colour = "#5E3C99", linewidth = 1) +
  facet_wrap(~land_cover_name, scales = "free_y", ncol = 3,
             labeller = as_labeller(pretty_lc)) +
  labs(x = expression(paste("Log(Area (km"^2, "))")),
       y = expression(atop("Predicted Conditional Occurrence Density",
                           "(records per km"^2*", where records occur)"))) +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.background = element_rect(fill = "grey90", colour = "black"),
        strip.text = element_text(size = 12, face = "bold")))

# Save to file
ggsave(filename = here("figures", "Figure_H3_density_predictions_by_landcover.png"),
       plot = fig_h3_predictions, width = 14, height = 8, dpi = 600)
ggsave(filename = here("figures", "Figure_H3_density_predictions_by_landcover.pdf"),
       plot = fig_h3_predictions, width = 14, height = 8, dpi = 600)

# END OF SCRIPT ----------------------------------------------------------------