##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 4.1_H2c_models
# This script contains code to test Hypothesis 2c: Area plan polygons will have
# higher species richness than areas outside of the planned developments and
# H2d: Completenss of species records will be greater in the development
# polygons than outside
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

library(here)
source(here("scripts", "0_setup.R"))

# Load polygons data
model_data <- readRDS(here("data", "derived_data", "h2_polygon_buffer_data.rds"))

# Quick check the df has all the data
cat("Rows loaded:", nrow(model_data), "\n") # 259762 (2 per pair)
print(table(model_data$polygon_type))

# 2. PREPARE DATA FOR MODELING -------------------------------------------------

# Calculate variables needed for model
richness_data <- model_data |>
  mutate(polygon_type = factor(polygon_type, levels = c("Buffer", "Development")),
         # centred log area (m^2) as a covariate
         log_area_c = as.numeric(scale(log(area_m2_numeric), scale = FALSE)),
         land_cover_name = factor(land_cover_name),
         kommune_factor = factor(kommune),
         pair_id_factor = factor(pair_id))

# Get a summary of the response variable
cat("\n=== Species richness summary ===\n")
print(summary(richness_data$n_species))
cat("Proportion of sides with zero species:",
    round(mean(richness_data$n_species == 0), 3), "\n")
cat("\nMean richness by side:\n")
print(tapply(richness_data$n_species, richness_data$polygon_type, mean))

# 3. FIT MODELS ----------------------------------------------------------------

## 3.1. H2c Negative binomial, full interaction --------------------------------

# Set up model
h2c_nbinom_full <- glmmTMB(n_species ~ polygon_type * (log_area_c + land_cover_name) +
                             (1 | kommune_factor/pair_id_factor),
                           data   = richness_data,
                           family = nbinom2)
# Save model output
save(h2c_nbinom_full,
     file = here::here("data", "models", "h2c_nbinom_full.RData"))

## 3.2. H2c Negative binomial, additive ----------------------------------------

# Set up model
h2c_nbinom_additive <- glmmTMB(n_species ~ polygon_type + log_area_c +
                                 land_cover_name +
                                 (1 | kommune_factor/pair_id_factor),
                               data   = richness_data,
                               family = nbinom2)

# Save model output
save(h2c_nbinom_additive,
     file = here::here("data", "models", "h2c_nbinom_additive.RData"))

# Compare models
AICtab(h2c_nbinom_full, h2c_nbinom_additive, base = TRUE)
#                     AIC      dAIC     df
# h2c_nbinom_full     288591.3      0.0 19
# h2c_nbinom_additive 288771.7    180.4 12

# Use the better model
best_model_h2c <- h2c_nbinom_full

## 3.3. H2c Zero-inflated, interaction -----------------------------------------

# Only run these models if you find significant zero-inflation in the DHARMa diagnostics

# Set up model
h2c_zinb_full <- glmmTMB(n_species ~ polygon_type * (log_area_c + land_cover_name) +
                           (1 | kommune_factor/pair_id_factor),
                         data      = richness_data,
                         family    = nbinom2,
                         ziformula = ~ polygon_type + log_area_c)

# Save model output
save(h2c_zinb_full, file = here::here("data", "models", "h2c_zinb_full.RData"))

## 3.4. H2c Zero-inflated, additive --------------------------------------------

# Set up model
h2c_zinb_additive <- glmmTMB(n_species ~ polygon_type + log_area_c + land_cover_name +
                           (1 | kommune_factor/pair_id_factor),
                         data      = richness_data,
                         family    = nbinom2,
                         ziformula = ~ polygon_type + log_area_c)

# Save model output
save(h2c_zinb_additive, file = here::here("data", "models", "h2c_zinb_additive.RData"))

# Compare full interaction and two-way interaction models
AICtab(h2c_zinb_full, h2c_zinb_additive, base = TRUE)
# AIC      dAIC     df
# h2c_zinb_full     288434.0      0.0 22
# h2c_zinb_additive 288725.8    291.8 15

# Select best H2c model
best_model_h2c <- h2c_zinb_full

# 4. MODEL SUMMARY -------------------------------------------------------------

## 4.1. H2c Full interaction model ---------------------------------------------

# Get a summary 
print(summary(best_model_h2c))

# Check convergence
if (best_model_h2c$sdr$pdHess) {
  cat("\nH2c model converged successfully\n")
} else {
  cat("\nWarning: H2c model may not have converged properly\n")
}

# Create coefficient table
coef_table_h2c <- broom.mixed::tidy(best_model_h2c,
                                    effects  = "fixed",
                                    conf.int = TRUE) |>
  mutate(Estimate  = round(estimate, 4),
         SE        = round(std.error, 4),
         `z value` = round(statistic, 2),
         `p value` = ifelse(p.value < 0.001, "<0.001", round(p.value, 4))) |>
  select(Term = term, Estimate, SE, `z value`, `p value`)

# Save coefficient table
write.csv(coef_table_h2c,
          here("figures", "Table_H2c_richness_model_coefficients.csv"),
          row.names = FALSE)

# 5. MODEL DIAGNOSTICS WITH DHARMA ---------------------------------------------

# Simulate residuals
sim_residuals_h2c <- simulateResiduals(fittedModel = best_model_h2c, n = 1000)

# Create diagnostic plots
png(filename = here("figures", "Figure_H2c_richness_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals_h2c)
dev.off()

# Test for dispersion
print(testDispersion(sim_residuals_h2c))

# Test for zero-inflation
print(testZeroInflation(sim_residuals_h2c))

# Test for outliers
print(testOutliers(sim_residuals_h2c))

# 6. EXTRACT RANDOM EFFECTS AND MODEL PARAMETERS -------------------------------

# Get the random effects
random_effects_h2c <- VarCorr(best_model_h2c)
print(random_effects_h2c)

# 7. HYPOTHESIS TESTING --------------------------------------------------------

## 7.1. Expected richness per side ---------------------------------------------

# Use emmans to extract expected richness per side, averaged over area and land cover
emmeans_polygon_h2c <- emmeans(best_model_h2c,
                               specs = "polygon_type",
                               type  = "response")
cat("Expected species richness by side:\n")
print(summary(emmeans_polygon_h2c))

# Development Polygon vs Buffer
contrast_polygon_h2c <- contrast(emmeans_polygon_h2c,
                                 method = "revpairwise", type = "response")
cat("\nDevelopment vs Buffer (richness rate ratio):\n")
print(summary(contrast_polygon_h2c, infer = TRUE))   # infer = TRUE adds the CI

# Hypothesis verdict from the rate-ratio CI
con_df <- as.data.frame(confint(contrast_polygon_h2c))
rr_col <- grep("ratio|estimate", names(con_df), value = TRUE)[1]
rr_lo  <- con_df[[grep("LCL|lower", names(con_df), value = TRUE)[1]]]
rr_hi  <- con_df[[grep("UCL|upper", names(con_df), value = TRUE)[1]]]
rr_est <- con_df[[rr_col]]
stopifnot(length(rr_est) == 1, length(rr_lo) == 1, length(rr_hi) == 1)

cat(sprintf("\nRichness rate ratio (Development / Buffer): %.3f  [%.3f, %.3f]\n",
            rr_est, rr_lo, rr_hi))
if (rr_lo > 1) {
  cat("H2c SUPPORTED: development polygons hold higher richness (RR CI entirely > 1).\n")
} else if (rr_hi < 1) {
  cat("H2c NOT supported: development polygons hold LOWER richness than buffers.\n")
} else {
  cat("H2c inconclusive: the rate-ratio CI includes 1.\n")
}

# Save inference objects
saveRDS(list(richness_by_side = emmeans_polygon_h2c,
             dev_vs_buffer    = contrast_polygon_h2c),
        here("data", "models", "h2c_richness_inference.rds"))

## 7.2. Marginal richness per side & land-cover including zeros ----------------

# Zero inflated model has conditional (non-0) part and marginal part (0s)
# Emmeans does not combine the two zero-inflation components cleanly, so I will
# use ggpreict and verify that the estimates are correct
marg_pred <- ggpredict(best_model_h2c,
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
          here("figures", "Table_H2c_marginal_richness_by_landcover_wide.csv"),
          row.names = FALSE)

# Check that ggpredict estimated the marginal points correctly
ref_lc <- levels(richness_data$land_cover_name)[1]
grid <- expand.grid(polygon_type = levels(richness_data$polygon_type),
                    land_cover_name = levels(richness_data$land_cover_name),
                    log_area_c = 0,
                    kommune_factor = levels(richness_data$kommune_factor)[1],
                    pair_id_factor = levels(richness_data$pair_id_factor)[1])
grid$predict_marginal <- predict(best_model_h2c, newdata = grid,
                                 type = "response", re.form = NA)
verify <- grid |>
  transmute(polygon_type, land_cover_name = gsub("_", " ", as.character(land_cover_name)),
            predict_marginal = round(predict_marginal, 2))
cat("\nVerification (predict marginal richness; compare to ggpredict table):\n")
print(verify) # loooks ok!

# 8. PREDICTION FIGURES --------------------------------------------------------

# Use a function to display land-cover names properly
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

# Predict species richness across area and predict each land-cover over its own observed log-area range
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

# Use the function defined above to predict the values
pred_df_h2c <- predict_within_lc_range(best_model_h2c, richness_data)

# Create the figure
(fig_h2c_predictions <- ggplot(pred_df_h2c,
                               aes(x = log_area_c, y = predicted,
                                   colour = polygon_type, fill = polygon_type)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, colour = NA) +
    geom_line(linewidth = 1.2) +
    facet_wrap(~land_cover_name, scales = "free_y", ncol = 3,
               labeller = as_labeller(pretty_lc)) +
    scale_colour_manual(values = polygon_colours, name = "Area type") +
    scale_fill_manual(values = polygon_colours, name = "Area type") +
    scale_x_continuous(breaks = log(breaks_m2_rich) - mean_log_rich,
                       labels = ha_label(breaks_m2_rich)) +
    labs(x = "Area (ha)",
         y = "Predicted Species Richness") +
    theme_classic() +
    theme(panel.grid = element_blank(),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          strip.background = element_rect(fill = "grey90", colour = "black"),
          strip.text = element_text(size = 14, face = "bold"),
          legend.position = "right",
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14)))

# Save figure to file
ggsave(filename = here("figures", "Figure_H2c_richness_by_side_and_landcover.png"),
       plot = fig_h2c_predictions, width = 14, height = 10, dpi = 600)
ggsave(filename = here("figures", "Figure_H2c_richness_by_side_and_landcover.pdf"),
       plot = fig_h2c_predictions, width = 14, height = 10, dpi = 600)

# 9. SUMMARY STATISTICS --------------------------------------------------------

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
          here("figures", "Table_H2c_richness_presence_summary_by_side.csv"), row.names = FALSE)
write.csv(richness_nonzero,
          here("figures", "Table_H2c_richness_summary_by_side.csv"), row.names = FALSE)

# END OF SCRIPT ----------------------------------------------------------------