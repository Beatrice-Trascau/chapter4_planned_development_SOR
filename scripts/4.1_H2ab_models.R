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

# Build the presence data
# Keep the long form (one row per polygon AND per buffer) and flag presence
presence_data <- model_data |>
  mutate(presence = as.integer(n_occurrences > 0),
         polygon_type = factor(polygon_type, levels = c("Buffer", "Development")),
         # centred log area of THIS unit (polygon or buffer)
         log_area_c = as.numeric(scale(log(area_m2_numeric), scale = FALSE)),
         land_cover_name = factor(land_cover_name),
         kommune_factor = factor(kommune),
         pair_id_factor = factor(pair_id))

## 2.2. Check the reshaped df --------------------------------------------------

# Check we have exactly one row per pair
stopifnot(nrow(pair_data) == n_distinct(model_data$pair_id))
cat("\nPairs after reshape:", nrow(pair_data), "\n")

# Check that we did not introduce NAs in the counts and that we have finit values everywhere
stopifnot(!any(is.na(pair_data$sor_polygon)),
          !any(is.na(pair_data$sor_buffer)),
          all(is.finite(pair_data$area_offset)),
          all(is.finite(pair_data$log_area_c)))
cat("PASS: counts complete and offset/area finite\n") # PASS

# Quick glance at the response variable
cat("Pairs with any records:", sum(pair_data$any_records), "of",
    nrow(pair_data), "(",
    round(100 * mean(pair_data$any_records), 1), "%)\n")
cat("Pairs with zero records in BOTH halves:", sum(pair_data$sor_total == 0), "\n") # 21513 of 129881 (16.6 %)
cat("Polygon share of records (record-bearing pairs only):\n") # 108368
print(summary(pair_data$share_polygon))

# Check that the area offset sits near 0 for most pairs
cat("\nArea offset log(area_polygon/area_buffer) summary:\n")
print(summary(pair_data$area_offset))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs 
# 0.0000  0.0000  0.1538  0.3757  0.8750  1.0000  108368

# 3. FIT MODELS ----------------------------------------------------------------

# Set up models so that we separate pairs that do not have any species occurrence records
# from those that have some records
pair_records <- pair_data |> filter(sor_total > 0)
cat("\nPairs entering the split model (H2a / H2b):", nrow(pair_records), "\n") #21513

## 3.1. H2ab split model with full interaction ---------------------------------

# Set up model with full interaction (i.e. does the split and its area slope depend on the land-cover?)
# Use the area offset to adjust the response to the area
h2ab_betabin_full <- glmmTMB(cbind(sor_polygon, sor_buffer) ~
                               log_area_c * land_cover_name +
                               offset(area_offset) + (1 | kommune_factor),
                             data   = pair_records,
                             family = betabinomial)

# Save model output
save(h2ab_betabin_full,
     file = here::here("data", "models", "h2ab_betabin_full.RData"))

## 3.2. H2ab additive split model ----------------------------------------------

# Set up model
h2ab_betabin_additive <- glmmTMB(cbind(sor_polygon, sor_buffer) ~
                                   log_area_c + land_cover_name +
                                   offset(area_offset) + (1 | kommune_factor),
                                 data   = pair_records,
                                 family = betabinomial)

# Save model output
save(h2ab_betabin_additive,
     file = here::here("data", "models", "h2ab_betabin_additive.RData"))

# Compare full interaction and additive models
AICtab(h2ab_betabin_full, h2ab_betabin_additive, base = TRUE)
#                       AIC     dAIC    df
# h2ab_betabin_additive 73638.6     0.0 10
# h2ab_betabin_full     73645.1     6.5 16

# Get the best model
best_split <- h2ab_betabin_additive

## 3.3. H1 presence model with full interaction  ------------------------

# Do polygons differ from buffers in the probability of having any records at all?
# Set up model
h1_presence_full <- glmmTMB(presence ~ polygon_type * (log_area_c + land_cover_name) +
                              (1 | kommune_factor/pair_id_factor),
                            data   = presence_data,
                            family = binomial)
# Save model output
save(h1_presence_full,
     file = here::here("data", "models", "h1_presence_full.RData"))


## 3.4. H1 presence model (additive) ------------------------------------

# Set up models
h1_presence_additive <- glmmTMB(presence ~ polygon_type + log_area_c +
                                  land_cover_name +
                                  (1 | kommune_factor/pair_id_factor),
                                data   = presence_data,
                                family = binomial)

# Save models
save(h1_presence_additive,
     file = here::here("data", "models", "h1_presence_additive.RData"))

# Compare the models
AICtab(h1_presence_full, h1_presence_additive, base = TRUE)
#                      AIC      dAIC     df
# h1_presence_full     131345.1      0.0 18
# h1_presence_additive 131604.5    259.4 11

# Use the best model
best_presence <- h1_presence_full

# 4. MODEL SUMMARIES -----------------------------------------------------------

## 4.1. H2ab model -------------------------------------------------------------

# Get summary
print(summary(best_split))

# Create simple coefficient table
coef_table_split <- broom.mixed::tidy(best_split,
                                      effects  = "fixed",
                                      conf.int = TRUE) |>
  mutate(Estimate   = round(estimate, 3),
         SE         = round(std.error, 3),
         `z value`  = round(statistic, 2),
         `p value`  = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))) |>
  select(Term = term, Estimate, SE, `z value`, `p value`)

# Save as CSV
write.csv(coef_table_split,
          here("figures", "Table_H2ab_split_model_coefficients.csv"),
          row.names = FALSE)

## 4.2. H1 presence model ------------------------------------------------------

# Get summary
print(summary(best_presence))

# Create simple coefficient table
coef_table_presence <- broom.mixed::tidy(best_presence,
                                         effects  = "fixed",
                                         conf.int = TRUE) |>
  mutate(Estimate   = round(estimate, 3),
         SE         = round(std.error, 3),
         `z value`  = round(statistic, 2),
         `p value`  = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))) |>
  select(Term = term, Estimate, SE, `z value`, `p value`)

# Save as CSV
write.csv(coef_table_presence,
          here("figures", "Table_H1_presence_model_coefficients.csv"),
          row.names = FALSE)

# 5. MODEL DIAGNOSTICS WITH DHARMA ---------------------------------------------

## 5.1. H2ab model --------................-------------------------------------

# Simulate residuals
sim_residuals_split <- simulateResiduals(fittedModel = best_split, n = 1000)

# Create diagnostic plots
png(filename = here("figures", "Figure_H2ab_betabinomial_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals_split)
dev.off()

# Test for dispersion
print(testDispersion(sim_residuals_split))

# Test for outliers
print(testOutliers(sim_residuals_split))

## 5.2. H1 presence model ------------------------------------------------------

# Simulate residuals
sim_residuals_presence <- simulateResiduals(fittedModel = best_presence, n = 1000)

# Create diagnostic plots
png(filename = here("figures", "Figure_H1_presence_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals_presence)
dev.off()

# Test for dispersion
print(testDispersion(sim_residuals_presence))

# 6. EXTRACT RANDOM EFFECTS ----------------------------------------------------

## 6.1. H2ab model -------------------------------------------------------------
random_effects_split <- VarCorr(best_split)
cat("\n=== H2a/H2b random effects (kommune) ===\n")
print(random_effects_split)
re_var_split <- as.numeric(random_effects_split$cond$kommune_factor[1])
cat("Random effect variance (kommune):", round(re_var_split, 4), "\n")

## 6.2. H1 presence model ------------------------------------------------------
random_effects_presence <- VarCorr(best_presence)
cat("\n=== H1 random effects (kommune / pair) ===\n")
print(random_effects_presence)

# 7. HYPOTHESIS TESTING --------------------------------------------------------

## 7.1. H2a - is the polygon share of SOR above 0.5 (i.e. the intercept)?
cat("H2a: polygons hold a greater (area-adjusted) share of paired SOR than\n")
cat("     their buffers (share > 0.5).\n\n")

# Get the average share over land cover; estimate is area-adjusted share
# a value of 0.5 = no difference in density
emm_overall <- emmeans(best_split, ~ 1, offset = 0, type = "response")

cat("Estimated polygon share of records (averaged over land cover):\n")
print(summary(emm_overall))

# Get estimate and confidence interval and convert it to scales you can report
emm_df   <- as.data.frame(emm_overall)
pi_hat   <- emm_df$prob
ci_lo    <- emm_df[[grep("LCL|lower", names(emm_df), value = TRUE)[1]]]
ci_hi    <- emm_df[[grep("UCL|upper", names(emm_df), value = TRUE)[1]]]

to_ratio <- function(p) p / (1 - p)
to_index <- function(p) 2 * p - 1

cat("\n--- H2a effect size ---\n")
cat(sprintf("Polygon share:  %.3f  [%.3f, %.3f]\n", pi_hat, ci_lo, ci_hi))
cat(sprintf("Polygon:buffer ratio:  %.3f  [%.3f, %.3f]\n",
            to_ratio(pi_hat), to_ratio(ci_lo), to_ratio(ci_hi)))
cat(sprintf("Symmetric index (2p-1): %.3f  [%.3f, %.3f]\n",
            to_index(pi_hat), to_index(ci_lo), to_index(ci_hi)))

if (ci_lo > 0.5) {
  cat("\nH2a SUPPORTED: the CI for the polygon share lies entirely above 0.5.\n")
} else if (ci_hi < 0.5) {
  cat("\nH2a NOT supported: the share lies below 0.5 (buffers hold more).\n")
} else {
  cat("\nH2a inconclusive: the CI for the polygon share includes 0.5.\n")
}

## 7.2. H2b - does the share of SOR increase with area? -----------------------

cat("H2b: SOR rises with area faster inside polygons than outside.\n")
cat("     In the paired model this is the effect of area on the polygon's\n")
cat("     share: a POSITIVE log_area_c slope means the polygon pulls ahead of\n")
cat("     its buffer as pairs get larger.\n\n")

# Get the average area slope of the share, across land-cover
slope_overall <- emtrends(best_split, ~ 1, var = "log_area_c")
cat("Average effect of log(area) on the polygon share (logit scale):\n")
print(summary(slope_overall))

# Extract slope confidence intervals
slope_df  <- as.data.frame(slope_overall)
trend_col <- grep("trend", names(slope_df), value = TRUE)[1]
slo_lo    <- slope_df[[grep("LCL|lower", names(slope_df), value = TRUE)[1]]]
slo_hi    <- slope_df[[grep("UCL|upper", names(slope_df), value = TRUE)[1]]]
slo_est   <- slope_df[[trend_col]]

cat(sprintf("\nArea slope: %.3f  [%.3f, %.3f]\n", slo_est, slo_lo, slo_hi))
if (slo_lo > 0) {
  cat("H2b SUPPORTED: the share increases with area (slope CI entirely > 0).\n")
} else if (slo_hi < 0) {
  cat("H2b NOT supported: the share DECREASES with area (buffer pulls ahead).\n")
} else {
  cat("H2b inconclusive: the area slope CI includes 0.\n")
}

# Get area slope per land-cover
slope_landcover <- emtrends(best_split, ~ land_cover_name, var = "log_area_c")
cat("\nArea slope of the share by land cover (logit scale):\n")
print(summary(slope_landcover))

# Save slopes to file
write.csv(as.data.frame(slope_landcover),
          here("figures", "Table_H2b_area_slope_by_landcover.csv"),
          row.names = FALSE)

## 7.3. DOes the split of SOR depend on land-cover? ----------------------------

cat("\n=== LRT for the area x land cover interaction (split model) ===\n")
lrt_split <- anova(h2ab_betabin_additive, h2ab_betabin_full)
print(lrt_split)


## 7.4. h2a share by lan-cover -------------------------------------------------

# Extract emmeans
emm_landcover <- emmeans(best_split, ~ land_cover_name, offset = 0, type = "response")

# Get a summary
cat("\n=== Estimated polygon share by land cover ===\n")
print(summary(emm_landcover))

# Convert to df
landcover_df <- as.data.frame(emm_landcover)

# Save output to file
write.csv(landcover_df,
          here("figures", "Table_H2a_share_by_landcover.csv"),
          row.names = FALSE)

# Save the model results for later
saveRDS(list(h2a_overall_share = emm_overall,
             h2a_share_by_lc = emm_landcover,
             h2b_area_slope = slope_overall,
             h2b_slope_by_lc = slope_landcover,
             lrt_interaction = lrt_split),
        here("data", "models", "h2ab_betabin_inference.rds"))

## 7.5. H1 - development polygon less likely to be empty than the buffer -------

cat("H1: development polygons are better surveyed - i.e. LESS likely to hold\n")
cat("    zero records than their paired buffers.\n\n")

# Get the probability of presence for polygons and buffer averaged over area and land-cover
emm_presence <- emmeans(best_presence, ~ polygon_type, type = "response")
cat("Probability a unit holds any records, by side:\n")
print(summary(emm_presence))

# Compare development polygons vs buffers as an odds ratio
contrast_presence <- contrast(emm_presence, method = "revpairwise", type = "response")
cat("\nDevelopment vs Buffer (odds ratio for holding any records):\n")
print(summary(contrast_presence, infer = TRUE))   # infer = TRUE adds the CI

# Get the odds-ratio CI
con_df <- as.data.frame(confint(contrast_presence))
or_col <- grep("ratio|estimate", names(con_df), value = TRUE)[1]
or_lo  <- con_df[[grep("LCL|lower", names(con_df), value = TRUE)[1]]]
or_hi  <- con_df[[grep("UCL|upper", names(con_df), value = TRUE)[1]]]
or_est <- con_df[[or_col]]

stopifnot(length(or_est) == 1, length(or_lo) == 1, length(or_hi) == 1)

cat(sprintf("\nOdds ratio (Development / Buffer): %.3f  [%.3f, %.3f]\n",
            or_est, or_lo, or_hi))
if (or_lo > 1) {
  cat("H1 SUPPORTED: development polygons are more likely to hold records\n")
  cat("   (less likely to be empty) than their buffers (OR CI entirely > 1).\n")
} else if (or_hi < 1) {
  cat("H1 NOT supported: development polygons are MORE likely to be empty.\n")
} else {
  cat("H1 inconclusive: the odds-ratio CI includes 1.\n")
}

# Save H1 output
saveRDS(list(presence_by_side = emm_presence,
             dev_vs_buffer    = contrast_presence),
        here("data", "models", "h1_presence_inference.rds"))

# 8. PREDICTION FIGURES --------------------------------------------------------

# Little function to display the land-cover names properly
pretty_lc <- function(x) {
  x <- gsub("_", " ", x)
  gsub("(^|\\s)([a-z])", "\\1\\U\\2", x, perl = TRUE)
}

## 8.1. H2ab - predicted share of SOR by area and land-cover -------------------

# Predict values
predictions_split <- ggpredict(best_split,
                               terms     = c("log_area_c [all]", "land_cover_name"),
                               condition = c(area_offset = 0))

# Convert to df
pred_df_split <- as.data.frame(predictions_split) |>
  rename(log_area_c = x, land_cover_name = group)

# Clip each facet to the log-area range observed in the land-cover (i.e. do not predict beyond the range of log(area) values for that specific land-cover)
lc_ranges_split <- pair_records |>
  group_by(land_cover_name) |>
  summarise(lo = min(log_area_c), hi = max(log_area_c), .groups = "drop")

pred_df_split <- pred_df_split |>
  left_join(lc_ranges_split, by = "land_cover_name") |>
  filter(log_area_c >= lo, log_area_c <= hi) |>
  select(-lo, -hi)

(fig_split_predictions <- ggplot(pred_df_split,
                                aes(x = log_area_c, y = predicted)) +
  geom_hline(yintercept = 0.5, linetype = "dashed",
             colour = "grey40", linewidth = 0.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              fill = "#E66101", alpha = 0.2) +
  geom_line(colour = "#E66101", linewidth = 1.2) +
  facet_wrap(~land_cover_name, scales = "free_y", ncol = 3,
             labeller = as_labeller(pretty_lc)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = expression(paste("log(Polygon Area (m"^2, "))")),
       y = "Predicted share of SOR Within the Development Polygons") +
  theme_classic() +
  theme(panel.grid       = element_blank(),
        axis.title       = element_text(size = 16),
        axis.text        = element_text(size = 14),
        strip.background  = element_rect(fill = "grey90", colour = "black"),
        strip.text       = element_text(size = 14, face = "bold")))

# Save figure to file
ggsave(filename = here("figures", "Figure_H2ab_predicted_share_by_landcover.png"),
       plot = fig_split_predictions, width = 14, height = 10, dpi = 600)
ggsave(filename = here("figures", "Figure_H2ab_predicted_share_by_landcover.pdf"),
       plot = fig_split_predictions, width = 14, height = 10, dpi = 600)

## 8.2. H2a - estimated share of SOR by land-cover -----------------------------

# Convert to df 
landcover_plot_df <- landcover_df |>
  rename(share = prob) |>
  rename(conf.low  = grep("LCL|lower", names(landcover_df), value = TRUE)[1],
         conf.high = grep("UCL|upper", names(landcover_df), value = TRUE)[1])

# Plot figure
(fig_h2a_landcover <- ggplot(landcover_plot_df,
                            aes(x = reorder(land_cover_name, share),
                                y = share)) +
  geom_hline(yintercept = 0.5, linetype = "dashed",
             colour = "grey40", linewidth = 0.5) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  colour = "#E66101", linewidth = 0.8, size = 0.6) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = pretty_lc) +
  coord_flip() +
  labs(x = "Land-cover Type",
       y = "Estimated Share of SOR Within the Development Polygons") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text  = element_text(size = 12)))

# Save figure to file
ggsave(filename = here("figures", "Figure_H2a_share_by_landcover_pointrange.png"),
       plot = fig_h2a_landcover, width = 10, height = 7, dpi = 600)
ggsave(filename = here("figures", "Figure_H2a_share_by_landcover_pointrange.pdf"),
       plot = fig_h2a_landcover, width = 10, height = 7, dpi = 600)

## 8.3. H2b - area slope by land-cover -----------------------------------------

# Get df
slope_lc_df <- as.data.frame(slope_landcover)
slope_plot_df <- slope_lc_df |>
  rename(slope     = grep("trend", names(slope_lc_df), value = TRUE)[1],
         conf.low  = grep("LCL|lower", names(slope_lc_df), value = TRUE)[1],
         conf.high = grep("UCL|upper", names(slope_lc_df), value = TRUE)[1])

# Plot figure
(fig_h2b_slope <- ggplot(slope_plot_df,
                        aes(x = reorder(land_cover_name, slope), y = slope)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey40", linewidth = 0.5) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  colour = "#1f77b4", linewidth = 0.8, size = 0.6) +
  coord_flip() +
  scale_x_discrete(labels = pretty_lc) +
  labs(x = "Land cover",
       y = "Effect of log(area) on polygon share (logit-scale slope)") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text  = element_text(size = 12)))

# This figure isn't telling us much so I won't save it

## 8.4. H1 - probability of SOR presence by land-cover -------------------------

# Get per-side presence probability for development vs buffer within land-cover
predictions_presence <- ggpredict(best_presence,
                                  terms = c("log_area_c [n=100]", "polygon_type",
                                            "land_cover_name"))

# Get df
pred_df_presence <- as.data.frame(predictions_presence) |>
  rename(log_area_c = x, polygon_type = group, land_cover_name = facet)

# CLip each land-cover by side to its own observed log-area range
lc_ranges_presence <- presence_data |>
  group_by(land_cover_name, polygon_type) |>
  summarise(lo = min(log_area_c), hi = max(log_area_c), .groups = "drop")

# Add to df
pred_df_presence <- pred_df_presence |>
  left_join(lc_ranges_presence, by = c("land_cover_name", "polygon_type")) |>
  filter(log_area_c >= lo, log_area_c <= hi) |>
  select(-lo, -hi)

# Set colours
polygon_colours <- c("Buffer" = "#E66101", "Development" = "#5E3C99")

# Create figure
(fig_presence_predictions <- ggplot(pred_df_presence,
                                   aes(x = log_area_c, y = predicted,
                                       colour = polygon_type, fill = polygon_type)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, colour = NA) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~land_cover_name, scales = "free_y", ncol = 3,
             labeller = as_labeller(pretty_lc)) +
  scale_colour_manual(values = polygon_colours, name = "Area type") +
  scale_fill_manual(values = polygon_colours, name = "Area type") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = expression(paste("log(Area (m"^2, "))")),
       y = "Probability of Unit Containing Any SOR") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        strip.background  = element_rect(fill = "grey90", colour = "black"),
        strip.text = element_text(size = 14, face = "bold"),
        legend.position  = "right",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14)))

# Save figure to file
ggsave(filename = here("figures", "Figure_H1_presence_by_side_and_landcover.png"),
       plot = fig_presence_predictions, width = 14, height = 10, dpi = 600)
ggsave(filename = here("figures", "Figure_H1_presence_by_side_and_landcover.pdf"),
       plot = fig_presence_predictions, width = 14, height = 10, dpi = 600)

# END OF SCRIPT ----------------------------------------------------------------