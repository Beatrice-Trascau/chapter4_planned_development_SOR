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
#                     AIC     dAIC    df
# h5c_nbinom_full     97041.2     0.0 19
# h5c_nbinom_additive 97214.2   173.0 12

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
#                   AIC      dAIC     df
# h5c_zinb_full      96837.9      0.0 22
# h5c_zinb_additive 100257.5   3419.6 15

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
#                     AIC      dAIC     df
# h5c_hurdle_full     113534.4      0.0 22
# h5c_hurdle_additive 113595.5     61.1 15

# Check convergence of the hurdle models
if (h5c_hurdle_full$sdr$pdHess) {
  cat("\nH5c hurdle (full) converged successfully\n")
} else {
  cat("\nWarning: H5c hurdle (full) may not have converged properly\n")
} # Converged successfully!

## 3.7. Compare all three model families ---------------------------------------

# Extract AIC
cat("\nNegative binomial vs zero-inflated vs hurdle (full versions):\n")
AICtab(h5c_nbinom_full, h5c_zinb_full, h5c_hurdle_full, base = TRUE)
#                 AIC      dAIC     df
# h5c_zinb_full    96837.9      0.0 22
# h5c_nbinom_full  97041.2    203.3 19
# h5c_hurdle_full 113534.4  16696.4 22 - No surprises there!

# Select best H5c model
best_model_h5c <- h5c_zinb_full

# 4. MODEL SUMMARY -------------------------------------------------------------

# Get a summary of the model
print(summary(best_model_h5c))

# Check convergence
if (best_model_h5c$sdr$pdHess) {
  cat("\nH5c model converged successfully\n")
} else {
  cat("\nWarning: H5c model may not have converged properly\n")
}

# Create a tidy summary table 
coef_table_h5c <- broom.mixed::tidy(best_model_h5c,
                                    effects  = "fixed",
                                    conf.int = TRUE) |>
  mutate(Estimate = round(estimate, 4),
         SE = round(std.error, 4),
         `z value` = round(statistic, 2),
         `p value` = ifelse(p.value < 0.001, "<0.001", round(p.value, 4))) |>
  select(Component = component, Term = term, Estimate, SE, `z value`, `p value`)

# Save the tidy summary table
write.csv(coef_table_h5c,
          here("figures", "Table_H5c_richness_model_coefficients.csv"),
          row.names = FALSE)

# 5. MODEL DIAGNOSTICS ---------------------------------------------------------

# Simulate residuals
sim_residuals_h5c <- simulateResiduals(fittedModel = best_model_h5c, n = 1000)

# Plot diagnostic plot
png(filename = here("figures", "Figure_H5c_richness_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals_h5c)
dev.off()

# Test dispersion
print(testDispersion(sim_residuals_h5c))

# Test outliers
print(testOutliers(sim_residuals_h5c))

# 6. RANDOM EFFECTS ------------------------------------------------------------

# Extract random effects
random_effects_h5c <- VarCorr(best_model_h5c)

# Display random effects
print(random_effects_h5c)
# Conditional model:
# Groups                        Name        Std.Dev.
# pair_id_factor:kommune_factor (Intercept) 5.308261
# kommune_factor                (Intercept) 0.053154

# 7. HYPOTHESIS TESTING --------------------------------------------------------

## 7.1. Expected richness per side ---------------------------------------------

# Expected richness per side, averaged over area and land cover
emmeans_polygon_h5c <- emmeans(best_model_h5c,
                               specs = "polygon_type",
                               type = "response")

# Check summary
cat("Expected alien species richness by side (conditional component):\n")
print(summary(emmeans_polygon_h5c))
# polygon_type response       SE  df asymp.LCL asymp.UCL
# Buffer       0.000558 4.01e-05 Inf  0.000485  0.000642
# Development  0.000637 4.99e-05 Inf  0.000546  0.000743

# Compare polygons and buffers
contrast_polygon_h5c <- contrast(emmeans_polygon_h5c,
                                 method = "revpairwise", type = "response")

# Check comparison
cat("\nDevelopment vs Buffer (alien richness rate ratio):\n")
print(summary(contrast_polygon_h5c, infer = TRUE))
# contrast             ratio     SE  df asymp.LCL asymp.UCL null z.ratio p.value
# Development / Buffer  1.14 0.0654 Inf      1.02      1.28    1   2.305  0.0212

# Hypothesis verdict from the rate-ratio CI
con_df <- as.data.frame(confint(contrast_polygon_h5c))
rr_col <- grep("ratio|estimate", names(con_df), value = TRUE)[1]
rr_lo  <- con_df[[grep("LCL|lower", names(con_df), value = TRUE)[1]]]
rr_hi  <- con_df[[grep("UCL|upper", names(con_df), value = TRUE)[1]]]
rr_est <- con_df[[rr_col]]
stopifnot(length(rr_est) == 1)
cat(sprintf("\nAlien richness rate ratio (Development / Buffer): %.3f  [%.3f, %.3f]\n",
            rr_est, rr_lo, rr_hi))
# Alien richness rate ratio (Development / Buffer): 1.141  [1.020, 1.277]
if (rr_lo > 1) {
  cat("H5c SUPPORTED: development polygons hold higher alien richness (RR CI entirely > 1).\n")
} else if (rr_hi < 1) {
  cat("H5c NOT supported: development polygons hold LOWER alien richness than buffers.\n")
} else {
  cat("H5c inconclusive: the rate-ratio CI includes 1.\n")
} #H5c SUPPORTED: development polygons hold higher alien richness (RR CI entirely > 1)

# Save the inference object
saveRDS(list(richness_by_side = emmeans_polygon_h5c,
             dev_vs_buffer = contrast_polygon_h5c),
        here("data", "models", "h5c_richness_inference.rds"))

## 7.2. Marginal richness per side & land-cover including zeros ----------------

# Zero inflated model has conditional (non-0) part and marginal part (0s)
# Emmeans does not combine the two zero-inflation components cleanly, so I will
# use ggpreict and verify that the estimates are correct
marg_pred <- ggpredict(best_model_h5c,
                       terms = c("polygon_type", "land_cover_name"),
                       type = "zero_inflated") |>
  as.data.frame() |>
  rename(polygon_type = x, land_cover_name = group)

# Convert to a wide table for Buffer vs Development Polygon
marg_wide <- marg_pred |>
  transmute(land_cover = gsub("_", " ", as.character(land_cover_name)),
            polygon_type,
            cell = sprintf("%.5f [%.5f–%.5f]", predicted, conf.low, conf.high)) |>
  tidyr::pivot_wider(names_from = polygon_type, values_from = cell) |>
  rename(`Land-cover` = land_cover,
         `Buffer richness (marginal)` = Buffer,
         `Development richness (marginal)` = Development)

# Check the table before saving it
print(marg_wide)

# Save table
write.csv(marg_wide,
          here("figures", "Table_H5c_marginal_richness_by_landcover_wide.csv"),
          row.names = FALSE)

# Check that ggpredict estimated the marginal points correctly
ref_lc <- levels(richness_data$land_cover_name)[1]
grid <- expand.grid(polygon_type = levels(richness_data$polygon_type),
                    land_cover_name = levels(richness_data$land_cover_name),
                    log_area_c = 0,
                    kommune_factor = levels(richness_data$kommune_factor)[1],
                    pair_id_factor = levels(richness_data$pair_id_factor)[1])
grid$predict_marginal <- predict(best_model_h5c, newdata = grid,
                                 type = "response", re.form = NA)
verify <- grid |>
  transmute(polygon_type, land_cover_name = gsub("_", " ", as.character(land_cover_name)),
            predict_marginal = round(predict_marginal, 5))
cat("\nVerification (predict marginal richness; compare to ggpredict table):\n")
print(verify) # loooks ok!


# 8. PREDICTION FIGURE ---------------------------------------------------------

# Use a function to display the land-cover names neatly
pretty_lc <- function(x) {
  x <- gsub("_", " ", x)
  gsub("(^|\\s)([a-z])", "\\1\\U\\2", x, perl = TRUE)
}

# Back-transform the centered log-area x axis to hectares (same as in 4.1)
mean_log_rich <- mean(log(richness_data$area_m2_numeric))
breaks_m2_rich <- c(min(richness_data$area_m2_numeric),
                    1e3, 1e4, 1e5, 1e6,
                    max(richness_data$area_m2_numeric))
ha_label <- function(x_m2) {
  v <- x_m2 / 1e4
  vapply(v, function(a) {
    if (a >= 1) formatC(a, format = "f", digits = 0, big.mark = ",")
    else        formatC(signif(a, 2), format = "g")
  }, character(1))
}

# Define colour pallette
polygon_colours <- c("Buffer" = "#E66101", "Development" = "#5E3C99")

# Function to predict richness across area, each land-cover over its own observed log-area range
predict_within_lc_range <- function(model, data, n = 100) {
  lcs <- levels(droplevels(factor(data$land_cover_name)))
  preds <- lapply(lcs, function(lc) {
    rng      <- range(data$log_area_c[data$land_cover_name == lc], na.rm = TRUE)
    seq_vals <- round(seq(rng[1], rng[2], length.out = n), 5)
    term_str <- paste0("log_area_c [", paste(seq_vals, collapse = ","), "]")
    ggpredict(model,
              terms = c(term_str, "polygon_type"),
              condition = c(land_cover_name = lc),
              type = "fixed") |>
      as.data.frame() |>
      rename(log_area_c = x, polygon_type = group) |>
      mutate(land_cover_name = lc)
  })
  bind_rows(preds)
}

# Use the function defined above
pred_df_h5c <- predict_within_lc_range(best_model_h5c, richness_data)

# Plot predictions
(fig_h5c_predictions <- ggplot(pred_df_h5c,
                               aes(x = log_area_c, y = predicted,
                                   colour = polygon_type, fill = polygon_type)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, colour = NA) +
    geom_line(linewidth = 1.2) +
    facet_wrap(~land_cover_name, scales = "free_y", ncol = 3,
               labeller = as_labeller(pretty_lc)) +
    scale_colour_manual(values = polygon_colours, name = "Area Type") +
    scale_fill_manual(values = polygon_colours, name = "Area Type") +
    scale_x_continuous(breaks = log(breaks_m2_rich) - mean_log_rich,
                       labels = ha_label(breaks_m2_rich)) +
    labs(x = "Area (ha)",
         y = "Predicted Alien Species Richness") +
    theme_classic() +
    theme(panel.grid = element_blank(),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          strip.background = element_rect(fill = "grey90", colour = "black"),
          strip.text = element_text(size = 14, face = "bold"),
          legend.position = "right"))

# Save the figures
ggsave(filename = here("figures", "Figure_H5c_richness_by_side_and_landcover.png"),
       plot = fig_h5c_predictions, width = 14, height = 10, dpi = 600)
ggsave(filename = here("figures", "Figure_H5c_richness_by_side_and_landcover.pdf"),
       plot = fig_h5c_predictions, width = 14, height = 10, dpi = 600)

# 9. HIGH-IMPACT vs LOWER-IMPACT RICHNESS COMPARISON ---------------------------

# Development/Buffer alien-richness rate ratio for all aliens, high-impact (SE+HI)
# and lower-impact (PH/LO/NK/NR), each from a ZINB additive model (matching the
# primary's family) so the three are comparable

# Use a helper function to set up the modelling variables from a per-side dataset
prep_richness <- function(side) {
  side |>
    mutate(polygon_type = factor(polygon_type, levels = c("Buffer", "Development")),
           log_area_c = as.numeric(scale(log(area_m2_numeric), scale = FALSE)),
           land_cover_name = factor(land_cover_name),
           kommune_factor = factor(kommune),
           pair_id_factor = factor(pair_id))
}

# Function to extract rate-ratio
richness_rate_ratio <- function(model) {
  emm <- emmeans(model, specs = "polygon_type", type = "response")
  d   <- as.data.frame(confint(contrast(emm, method = "revpairwise",
                                        type = "response")))
  rr_col <- grep("ratio|estimate", names(d), value = TRUE)[1]
  c(RR = d[[rr_col]],
    lower = d[[grep("LCL|lower", names(d), value = TRUE)[1]]],
    upper = d[[grep("UCL|upper", names(d), value = TRUE)[1]]])
}

# Fit a ZINB additive richness model for one group and return a comparison row
run_richness_group <- function(label, side, min_present = 30) {
  rd <- prep_richness(side)
  n_present <- sum(rd$n_species > 0)
  mean_dev <- mean(rd$n_species[rd$polygon_type == "Development"])
  mean_buf <- mean(rd$n_species[rd$polygon_type == "Buffer"])
  
  # add to a df
  na_row <- data.frame(group = label, present_sides = n_present,
                       mean_rich_dev = round(mean_dev, 4),
                       mean_rich_buf = round(mean_buf, 4),
                       RR = NA, RR_lo = NA, RR_hi = NA, note = "sparse / failed")
  
  # check if the data is too sparse
  if (n_present < min_present) {
    cat("  ", label, "- too sparse (present sides", n_present, ") - skipped\n")
    return(na_row)
  }
  tryCatch({
    m <- glmmTMB(n_species ~ polygon_type + log_area_c + land_cover_name +
                   (1 | kommune_factor/pair_id_factor),
                 data      = rd,
                 family    = nbinom2,
                 ziformula = ~ polygon_type + log_area_c)
    rr <- richness_rate_ratio(m)
    data.frame(group = label, present_sides = n_present,
               mean_rich_dev = round(mean_dev, 4),
               mean_rich_buf = round(mean_buf, 4),
               RR = round(rr["RR"], 3),
               RR_lo = round(rr["lower"], 3), RR_hi = round(rr["upper"], 3),
               note = "")
  }, error = function(e) {
    cat("  ", label, "- model fit failed:", conditionMessage(e), "\n")
    na_row
  })
}

# Check the comparison by risk level
impact_richness_comparison <- bind_rows(run_richness_group("All aliens", model_data),
                                        run_richness_group("High impact (SE+HI)", model_data_high),
                                        run_richness_group("Lower impact (PH/LO/NK/NR)", model_data_lower))

# Check the comparison
print(impact_richness_comparison)

# Save the comparison to file
write.csv(impact_richness_comparison,
          here("figures", "Table_H5c_impact_richness_comparison.csv"),
          row.names = FALSE)

# 10. SUMMARY STATISTICS -------------------------------------------------------

# Get the % of polygons and buffers with any species at all 
richness_presence_summary <- model_data |>
  group_by(polygon_type) |>
  summarise(n_sides = n(),
            n_with_species = sum(n_species > 0),
            pct_with_species = round(100 * mean(n_species > 0), 1),
            .groups = "drop")
print(richness_presence_summary)

# Get summary of the species richness across all sides with 0s included
richness_all <- model_data |>
  group_by(polygon_type) |>
  summarise(mean = round(mean(n_species), 2), median = median(n_species),
            q25 = quantile(n_species, 0.25), q75 = quantile(n_species, 0.75),
            IQR = IQR(n_species), max = max(n_species), .groups = "drop")
print(richness_all)

# Get the species richness for polygons and buffers that have any records
richness_nonzero <- model_data |>
  filter(n_species > 0) |>
  group_by(polygon_type) |>
  summarise(n_sides = n(), mean = round(mean(n_species), 2),
            median = median(n_species), q25 = quantile(n_species, 0.25),
            q75 = quantile(n_species, 0.75), IQR = IQR(n_species),
            max = max(n_species), .groups = "drop")
print(richness_nonzero)

# Save to file
write.csv(richness_presence_summary,
          here("figures", "Table_H5c_richness_presence_summary_by_side.csv"), row.names = FALSE)
write.csv(richness_nonzero,
          here("figures", "Table_H5c_richness_summary_by_side.csv"), row.names = FALSE)

# END OF SCRIPT ----------------------------------------------------------------