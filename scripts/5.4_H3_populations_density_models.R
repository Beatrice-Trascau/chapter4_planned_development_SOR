##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 5.4_H3_population_density_models
# This script contains code to test H3: Urban and near-urban polygons will have 
# more SOR than other planned development types
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Load source script
library(here)
source(here("scripts", "0_setup.R"))

# Load data for the model
model_data_raw <- readRDS(here("data", "derived_data", "h2_polygon_buffer_data.rds"))

# Load population data
pop_lookup <- readRDS(here("data", "derived_data", "development_population_density.rds"))

# Helper figures to display land-cover names nicely
pretty_lc <- function(x) {
  x <- gsub("_", " ", x)
  gsub("(^|\\s)([a-z])", "\\1\\U\\2", x, perl = TRUE)
}

# 2. PREPARE DATA FOR MODELS ---------------------------------------------------

# Clean data and create the variables that are needed for the model
model_data <- model_data_raw |>
  filter(polygon_type == "Development",
         english_categories != "Ports") |>
  left_join(pop_lookup, by = "id") |>
  mutate(area_km2 = area_m2_numeric / 1e6,
         log_area_km2 = log(area_km2),
         land_cover_name = factor(land_cover_name),
         kommune_factor = factor(kommune)) |>
  # pop_density is 0 (not NA) for unpopulated/suppressed polygons - see 5.0
  filter(!is.na(n_occurrences), !is.na(log_area_km2),
         !is.na(pop_density), !is.na(kommune_factor))

# Check that data was loaded/processed correctly
cat("Development polygons with population data:", nrow(model_data), "\n") #129881
cat("pop_density summary:\n"); print(summary(model_data$pop_density))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0     0.0    16.0   227.0   206.2 11280.0 
cat("Share of polygons with zero population:",
    round(mean(model_data$pop_density == 0), 3), "\n") # 0.496

# 3. MODELS --------------------------------------------------------------------

## 3.1. Model 1: Negative binomial additive ------------------------------------

# Set up model
h3pop_nb_additive <- glmmTMB(n_occurrences ~ log1p_pop_density + land_cover_name +
                             offset(log_area_km2) + (1 | kommune_factor),
                           data = model_data, family = nbinom2)

# Save model
save(h3pop_nb_additive, file = here::here("data", "models", 
                                        "h3pop_nb_additive.RData"))

## 3.2. Model 2: Negative binomial interaction ---------------------------------

# Set up model
h3pop_nb_interaction <- glmmTMB(n_occurrences ~ log1p_pop_density * land_cover_name +
                                  offset(log_area_km2) + (1 | kommune_factor),
                                data = model_data, family = nbinom2)

# Save model
save(h3pop_nb_interaction, file = here::here("data", "models", 
                                          "h3pop_nb_interaction.RData"))

# Compare models
AICtab(h3pop_nb_additive, h3pop_nb_interaction, base = TRUE)
#                      AIC      dAIC     df
# h3pop_nb_interaction 142817.1      0.0 16
# h3pop_nb_additive    143074.4    257.2 10

# Choose the better model
best_model_h3 <- h3pop_nb_interaction

## 3.3. Model 3: Zero-inflated additive ----------------------------------------

# Set up model
h3pop_zinb_additive <- glmmTMB(n_occurrences ~ log1p_pop_density + land_cover_name +
                                 offset(log_area_km2) + (1 | kommune_factor),
                               data = model_data, family = nbinom2,
                               ziformula = ~ log1p_pop_density + land_cover_name)

# Save model
save(h3pop_zinb_additive, file = here::here("data", "models", 
                                            "h3pop_zinb_additive.RData"))


## 3.4. Model 4: Zero-inflated interactive -------------------------------------

# Set up model
h3pop_zinb_interaction <- glmmTMB(n_occurrences ~ log1p_pop_density * land_cover_name +
                                 offset(log_area_km2) + (1 | kommune_factor),
                               data = model_data, family = nbinom2,
                               ziformula = ~ log1p_pop_density + land_cover_name)

# Save model
save(h3pop_zinb_interaction, file = here::here("data", "models", 
                                            "h3pop_zinb_interaction.RData"))

# Compare models
AICtab(h3pop_zinb_additive, h3pop_zinb_interaction, base = TRUE)
#                        AIC      dAIC     df
# h3pop_zinb_interaction 142654.9      0.0 24
# h3pop_zinb_additive    142907.4    252.5 18

# 4. MODEL DIAGNOSTICS ---------------------------------------------------------

## 4.1. Negative binomial model ------------------------------------------------

# Simulate residuals
sim_residuals_H3_nb <- simulateResiduals(h3pop_nb_interaction, n = 1000)

# Save diagnostic figure to file
png(filename = here("figures", "Figure_H3_pop_density_nb.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals_H3_nb)
dev.off()

# Test dispersion
print(testDispersion(sim_residuals_H3_nb))

# Test zero inflation
print(testZeroInflation(sim_residuals_H3_nb)) # no significant zero inflation (????)

# Test outliers
print(testOutliers(sim_residuals_H3_nb))

## 4.2. Zero inflated model ----------------------------------------------------

# Simulate residuals
sim_residuals_H3_zinb <- simulateResiduals(h3pop_zinb_interaction, n = 1000)

# Save diagnostic figure to file
png(filename = here("figures", "Figure_H3_pop_density_zinb.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals_H3_zinb)
dev.off()

# Test dispersion
print(testDispersion(sim_residuals_H3_zinb))

# Test zero inflation
print(testZeroInflation(sim_residuals_H3_zinb))

# Test outliers
print(testOutliers(sim_residuals_H3_zinb))

# Since there is no zero-inflation in the NB model, this is the one that will be used

# 5. MODEL SUMMARY -------------------------------------------------------------

# Quick look at the summary
print(summary(best_model_h3))

# Check if the model converged
if (best_model_h3$sdr$pdHess) {
  cat("\nH3 model converged successfully\n")
} else {
  cat("\nWarning: H3 model may not have converged\n")
} # H3 model converged successfully!

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
          here("figures", "Table_H3_pop_density_nb_coefficients.csv"),
          row.names = FALSE)

# 6. RANDOM EFFECTS ------------------------------------------------------------

# Extract random effects
random_effects_h3 <- VarCorr(best_model_h3)
print(random_effects_h3)
# Groups         Name        Std.Dev.
# kommune_factor (Intercept) 1.3251    

# Check the structure with zero inflation
re_var_h3 <- as.numeric(attr(random_effects_h3$cond$kommune_factor, "stddev"))^2

# Alternatively
vc <- VarCorr(best_model_h3)
print(vc)
#  Groups         Name        Std.Dev.
# kommune_factor (Intercept) 1.3251 
kommune_var <- attr(vc$cond$kommune_factor, "stddev")^2
cat("Random effect variance (kommune):", round(kommune_var, 4), "\n") # 1.756

# 7. HYPOTHESIS TESTING --------------------------------------------------------

## 7.1. Overall effort effect of land-cover -------------------------------------

# Get slope of record density on population density, averaged over land cover.
trend_overall <- emtrends(best_model_h3, ~ 1, var = "log1p_pop_density")
cat("\nOverall effect of population density on record density (net of land cover):\n")
print(summary(trend_overall, infer = TRUE))
# 1       log1p_pop_density.trend     SE  df asymp.LCL asymp.UCL z.ratio p.value
# overall                  0.0456 0.0134 Inf    0.0194    0.0719   3.404  0.0007

# Convert to df
to_df <- as.data.frame(summary(trend_overall, infer = TRUE))
tcol <- grep("trend", names(to_df), value = TRUE)[1]
tlo <- to_df[[grep("LCL|lower", names(to_df), value = TRUE)[1]]]
thi <- to_df[[grep("UCL|upper", names(to_df), value = TRUE)[1]]]
cat(sprintf("\nEffort slope: %.3f  [%.3f, %.3f]\n", to_df[[tcol]], tlo, thi))
if (tlo > 0) {
  cat("H3 SUPPORTED: record density rises with population density (effort bias).\n")
} else if (thi < 0) {
  cat("H3 reversed: record density FALLS with population density.\n")
} else {
  cat("H3 inconclusive: the effort-slope CI includes 0.\n")
} # H3 SUPPORTED: record density rises with population density (effort bias).

## 7.2. Does the effort slope differ by land cover? (interaction LRT) -----------

# Compare effort 
cat("\nInteraction LRT (additive vs interaction, ZINB conditional part):\n")
lrt_pop <- anova(h3pop_nb_additive, h3pop_nb_interaction)
print(lrt_pop)
if (lrt_pop$`Pr(>Chisq)`[2] < 0.05) {
  cat("\nThe effort slope DIFFERS by land cover (interaction p < 0.05).\n")
} else {
  cat("\nNo evidence the effort slope differs by land cover; the slope-by-land-cover\n")
  cat("figures below are illustrative (report the overall slope from 5.1).\n")
} # The effort slope DIFFERS by land cover

## 7.3. Per-land-cover effort slopes -------------------------------------------

# Get slopes from the interaction model
slopes_pop <- emtrends(h3pop_nb_interaction, ~ land_cover_name,
                       var = "log1p_pop_density")
cat("\nEffort slope (effect of population density on record density) by land cover:\n")
print(summary(slopes_pop, infer = TRUE))

# Convert to df
slopes_df <- as.data.frame(summary(slopes_pop, infer = TRUE))
sc <- grep("trend", names(slopes_df), value = TRUE)[1]
sl <- grep("LCL|lower", names(slopes_df), value = TRUE)[1]
sh <- grep("UCL|upper", names(slopes_df), value = TRUE)[1]
slopes_df <- slopes_df |>
  rename(slope = all_of(sc), conf.low = all_of(sl), conf.high = all_of(sh)) |>
  mutate(direction = ifelse(slope > 0, "Positive", "Negative"))

# Save to file
write.csv(slopes_df,
          here("figures", "Table_H3pop_slope_by_landcover.csv"), row.names = FALSE)

## 7.4. Pairwise slope contrasts -----------------------------------------------

# Get contrast of slopes
slope_contrasts <- contrast(slopes_pop, method = "pairwise", adjust = "tukey")
cat("\nPairwise effort-slope comparisons (Tukey-adjusted):\n")
print(summary(slope_contrasts, infer = TRUE))

# Write comparisons to file
write.csv(as.data.frame(summary(slope_contrasts, infer = TRUE)),
          here("figures", "Table_H3pop_pairwise_slope_comparisons.csv"), row.names = FALSE)

# Save the output of the hypotheses
saveRDS(list(overall_trend = trend_overall,
             interaction_lrt = lrt_pop,
             slopes = slopes_pop,
             pairwise = slope_contrasts),
        here("data", "models", "h3pop_inference.rds"))

# 8. FIGURES -------------------------------------------------------------------

# Set colour scheme
slope_colours <- c("Positive" = "#E66101", "Negative" = "#5E3C99")

## 8.1. Overall effort curve ---------------------------------------------------

# From the additive model; land-cover held at reference (additive, so the SHAPE
# is the same for any land cover). offset = 0 -> density per km2.
rng <- range(model_data$log1p_pop_density, na.rm = TRUE)
pred_overall <- ggpredict(best_model_h3,
                          terms = paste0("log1p_pop_density [",
                                         paste(round(seq(rng[1], rng[2], length.out = 100), 4),
                                               collapse = ","), "]"),
                          condition = c(log_area_km2 = 0), type = "fixed") |>
  as.data.frame()

# Plot figure
(fig_overall <- ggplot(pred_overall, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#5E3C99", alpha = 0.2) +
  geom_line(colour = "#5E3C99", linewidth = 1) +
  labs(x = "log(1 + Population Density) (Population/km2)",
       y = expression(paste("Predicted Record Density (Records/km"^2, ")"))) +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 13), axis.text = element_text(size = 12)))

# Save figures
ggsave(here("figures", "Figure_H3pop_overall_effect.png"), fig_overall,
       width = 10, height = 7, dpi = 600)
ggsave(here("figures", "Figure_H3pop_overall_effect.pdf"), fig_overall,
       width = 10, height = 7, dpi = 600)

## 8.2. Effort slope by land cover (point-range) -------------------------------

# Plot figure
(fig_slopes <- ggplot(slopes_df,
                     aes(x = reorder(land_cover_name, slope), y = slope,
                         colour = direction)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.6) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  linewidth = 0.9, size = 0.6) +
  scale_colour_manual(values = slope_colours, name = "Effect direction") +
  scale_x_discrete(labels = pretty_lc) +
  coord_flip() +
  labs(x = "Land cover",
       y = "Effect of Population Density on Record Density\n(slope, 95% CI)") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 13), axis.text = element_text(size = 12),
        legend.position = "bottom"))

# Save figure
ggsave(here("figures", "Figure_H3pop_slope_by_landcover.png"), fig_slopes,
       width = 11, height = 7, dpi = 600)
ggsave(here("figures", "Figure_H3pop_slope_by_landcover.pdf"), fig_slopes,
       width = 11, height = 7, dpi = 600)

## 6.3. Pairwise slope contrasts (point-range) ---------------------------------

# Create df of the pairwise slope comparisons
pw_df <- as.data.frame(summary(slope_contrasts, infer = TRUE))
ec <- grep("estimate", names(pw_df), value = TRUE)[1]
pl <- grep("LCL|lower", names(pw_df), value = TRUE)[1]
ph <- grep("UCL|upper", names(pw_df), value = TRUE)[1]
pw_df <- pw_df |>
  rename(estimate = all_of(ec), conf.low = all_of(pl), conf.high = all_of(ph)) |>
  mutate(contrast = gsub("_", " ", contrast),
         significant = ifelse(p.value < 0.05, "Yes", "No"))

# Plot figure
(fig_contrasts <- ggplot(pw_df,
                        aes(x = estimate, y = reorder(contrast, estimate),
                            colour = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), linewidth = 0.6) +
  scale_colour_manual(values = c("No" = "grey65", "Yes" = "#5E3C99")) +
  labs(x = "Difference in Effort Slopes (95% CI)", y = "Land-cover Comparison") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 11),
        legend.position = "none"))

# Save figure
ggsave(here("figures", "Figure_H3pop_pairwise_slope_contrasts.png"), fig_contrasts,
       width = 12, height = 10, dpi = 600)
ggsave(here("figures", "Figure_H3pop_pairwise_slope_contrasts.pdf"), fig_contrasts,
       width = 12, height = 10, dpi = 600)

## 8.4. Predicted record vs population density, faceted by land cover ----------

# From the interaction model, each land cover predicted over its OWN observed
# population-density range (no extrapolation). offset = 0 -> density per km2.
predict_by_pop_lc <- function(model, data, n = 100) {
  lcs <- levels(droplevels(factor(data$land_cover_name)))
  preds <- lapply(lcs, function(lc) {
    rng      <- range(data$log1p_pop_density[data$land_cover_name == lc], na.rm = TRUE)
    seq_vals <- round(seq(rng[1], rng[2], length.out = n), 5)
    term_str <- paste0("log1p_pop_density [", paste(seq_vals, collapse = ","), "]")
    ggpredict(model, terms = term_str,
              condition = c(land_cover_name = lc, log_area_km2 = 0),
              type = "fixed") |>
      as.data.frame() |>
      rename(log1p_pop_density = x) |>
      mutate(land_cover_name = lc)
  })
  bind_rows(preds)
}

# Convert to df
pred_df <- predict_by_pop_lc(h3pop_zinb_interaction, model_data)

# Plot figure
(fig_predictions <- ggplot(pred_df, aes(x = log1p_pop_density, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#5E3C99", alpha = 0.2) +
  geom_line(colour = "#5E3C99", linewidth = 1) +
  facet_wrap(~land_cover_name, scales = "free_y", ncol = 3,
             labeller = as_labeller(pretty_lc)) +
  labs(x = "log(1 + population density) (people/km2)",
       y = expression(atop("Predicted conditional record density",
                           "(records per km"^2*", where records occur)"))) +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        strip.background = element_rect(fill = "grey90", colour = "black"),
        strip.text = element_text(size = 12, face = "bold")))

# Save figure
ggsave(here("figures", "Figure_H3pop_density_predictions_by_landcover.png"),
       fig_predictions, width = 14, height = 8, dpi = 600)
ggsave(here("figures", "Figure_H3pop_density_predictions_by_landcover.pdf"),
       fig_predictions, width = 14, height = 8, dpi = 600)

# END OF SCRIPT ----------------------------------------------------------------