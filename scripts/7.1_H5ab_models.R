##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 7.1_H5ab_models
# This script contains code to test Hypothesis 5a: Development Polygons have a
# greatr number of alien SOR than areas not planned for development and
# H5b: the share of alien SOR inside polygons increases with area
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source setup script
library(here)
source(here("scripts", "0_setup.R"))

# Load model data
model_data <- readRDS(here("data", "derived_data", "h2_polygon_buffer_data.rds"))
occ_join <- readRDS(here("data", "derived_data",
                         "h2d_polygon_buffer_occurrence_join.rds"))

# Load the resolved alien list from script 3.4
alien_cleaned <- readRDS(here("data", "derived_data",
                              "alien_species_list_resolved.rds"))

# Check that the data was loaded ok
cat("Sides loaded:", nrow(model_data), "\n") # 259762
cat("Alien species on the resolved list:", nrow(alien_cleaned), "\n") # 4676
print(table(alien_cleaned$risk_category))
# HI   LO   NK   NR   PH   SE 
# 199 1113  333 2445  365  221 

# 2. SET UP GROUPINGS AND HELPER FUNCTIONS -------------------------------------

# Order alien risk categories from most to least severe
severity_order <- c("SE", "HI", "PH", "LO", "NK", "NR")

# Group impact categories for H5
high_impact_categories  <- c("SE", "HI")
lower_impact_categories <- c("PH", "LO", "NK", "NR")

# Get the tiers actually present in the data, in severity order
alien_tiers <- intersect(severity_order, unique(alien_cleaned$risk_category))

# Normalise the whitespace so that the occurrence species names match the alien list
clean_name <- function(x) {
  x <- gsub("\u00a0", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# Function to build a per-side dataset (keeps ALL sides; sides with no record in the chosen
# categories become 0) for a given set of risk categories
build_side_data <- function(occ_alien, model_data, keep_categories) {
  counts <- occ_alien |>
    filter(risk_category %in% keep_categories) |>
    group_by(poly_uid) |>
    summarise(sor = n(), nsp = n_distinct(species), .groups = "drop")
  
  model_data |>
    select(poly_uid, id, pair_id, polygon_type, area_m2_numeric,
           english_categories, kommune, land_cover_name, log_area) |>
    left_join(counts, by = "poly_uid") |>
    mutate(n_occurrences = coalesce(sor, 0L),
           n_species = coalesce(nsp, 0L),
           polygon_type = factor(polygon_type, levels = c("Buffer", "Development")),
           land_cover_name = factor(land_cover_name),
           kommune_factor = factor(kommune),
           pair_id_factor = factor(pair_id)) |>
    select(-sor, -nsp)
}

# Function to reshape a per-side dataset to one row per pair
make_pair_data <- function(side) {
  side |>
    select(pair_id, kommune, english_categories, land_cover_name,
           area_m2_numeric, polygon_type, n_occurrences) |>
    tidyr::pivot_wider(names_from  = polygon_type,
                       values_from = c(n_occurrences, area_m2_numeric)) |>
    rename(sor_polygon = n_occurrences_Development,
           sor_buffer = n_occurrences_Buffer,
           area_polygon = area_m2_numeric_Development,
           area_buffer = area_m2_numeric_Buffer) |>
    mutate(sor_total = sor_polygon + sor_buffer,
           share_polygon = ifelse(sor_total > 0, sor_polygon / sor_total, NA_real_),
           log_area_c = as.numeric(scale(log(area_polygon), scale = FALSE)),
           area_offset = log(area_polygon / area_buffer),
           any_records = as.integer(sor_total > 0),
           kommune_factor = factor(kommune),
           land_cover_name = factor(land_cover_name))
}

# Function to create one row per side with a presence flag
make_presence_data <- function(side) {
  side |>
    mutate(presence = as.integer(n_occurrences > 0),
           polygon_type = factor(polygon_type, levels = c("Buffer", "Development")),
           log_area_c = as.numeric(scale(log(area_m2_numeric), scale = FALSE)),
           land_cover_name = factor(land_cover_name),
           kommune_factor = factor(kommune),
           pair_id_factor = factor(pair_id))
}

# Function to extract estimates from the models
share_estimate <- function(split_model) {
  d <- as.data.frame(emmeans(split_model, ~ 1, offset = 0, type = "response"))
  c(estimate = d$prob,
    lower = d[[grep("LCL|lower", names(d), value = TRUE)[1]]],
    upper = d[[grep("UCL|upper", names(d), value = TRUE)[1]]])
}
slope_estimate <- function(split_model) {
  d <- as.data.frame(emtrends(split_model, ~ 1, var = "log_area_c"))
  tcol <- grep("trend", names(d), value = TRUE)[1]
  c(estimate = d[[tcol]],
    lower = d[[grep("LCL|lower", names(d), value = TRUE)[1]]],
    upper = d[[grep("UCL|upper", names(d), value = TRUE)[1]]])
}
presence_or <- function(presence_model) {
  emm <- emmeans(presence_model, ~ polygon_type, type = "response")
  d   <- as.data.frame(confint(contrast(emm, method = "revpairwise",
                                        type = "response")))
  ocol <- grep("ratio|estimate", names(d), value = TRUE)[1]
  c(estimate = d[[ocol]],
    lower = d[[grep("LCL|lower", names(d), value = TRUE)[1]]],
    upper = d[[grep("UCL|upper", names(d), value = TRUE)[1]]])
}

# Function to assemble one comparison-table row from fitted models
row_from_models <- function(label, split_model, presence_model,
                            pair_records, presence_data) {
  sh <- share_estimate(split_model)
  sl <- slope_estimate(split_model)
  po <- presence_or(presence_model)
  data.frame(group = label,
             record_pairs = nrow(pair_records),
             present_sides = sum(presence_data$presence),
             share = round(sh["estimate"], 3),
             share_lo = round(sh["lower"], 3), share_hi = round(sh["upper"], 3),
             area_slope = round(sl["estimate"], 3),
             slope_lo = round(sl["lower"], 3), slope_hi = round(sl["upper"], 3),
             presence_OR = round(po["estimate"], 3),
             OR_lo = round(po["lower"], 3), OR_hi = round(po["upper"], 3),
             row.names = NULL)
}

# Function to fit the compact models for a category set and return a comparison row
# skips/returns NA if the data is too sparse or the fit fails
run_group_compact <- function(label, keep_categories, min_n = 30) {
  side <- build_side_data(occ_alien, model_data, keep_categories)
  pd <- make_pair_data(side)
  prd <- make_presence_data(side)
  pr <- pd |> filter(sor_total > 0) |> droplevels()
  present <- sum(prd$presence)
  
  na_row <- data.frame(group = label, record_pairs = nrow(pr),
                       present_sides = present,
                       share = NA, share_lo = NA, share_hi = NA,
                       area_slope = NA, slope_lo = NA, slope_hi = NA,
                       presence_OR = NA, OR_lo = NA, OR_hi = NA,
                       row.names = NULL)
  
  if (nrow(pr) < min_n || present < min_n) {
    cat("  ", label, "- too sparse (record pairs", nrow(pr),
        "/ present sides", present, ") - skipped\n")
    return(na_row)
  }
  tryCatch({
    sp <- glmmTMB(cbind(sor_polygon, sor_buffer) ~ log_area_c + land_cover_name +
                    offset(area_offset) + (1 | kommune_factor),
                  data = pr, family = betabinomial)
    pm <- glmmTMB(presence ~ polygon_type * (log_area_c + land_cover_name) +
                    (1 | kommune_factor/pair_id_factor),
                  data = prd, family = binomial)
    row_from_models(label, sp, pm, pr, prd)
  }, error = function(e) {
    cat("  ", label, "- model fit failed:", conditionMessage(e), "\n")
    na_row
  })
}

# 3. FLAG ALIEN OCCURRENCES AND BULD PER-SIDE DATASETS -------------------------

# Keep only alien occurrences and attach the risk category
occ_alien <- occ_join |>
  filter(!is.na(gbifID)) |>
  mutate(species = clean_name(species)) |>
  inner_join(alien_cleaned, by = c("species" = "gbif_species"))

# Get a summary
cat("\nAlien occurrence records:", nrow(occ_alien),
    "of", sum(!is.na(occ_join$gbifID)), "matched records\n") # 40670 of 682121 matched records
cat("Alien occurrences by risk category:\n")
print(table(occ_alien$risk_category))
# HI    LO    NK    NR    PH    SE 
# 2482  3703  1156  8183  3577 21569 

# Save the alien occurrence-level join (with risk_category)
saveRDS(occ_alien,
        here("data", "derived_data", "h5_polygon_buffer_occurrence_join.rds"))

# Build the primary per-side data (i.e. any alien category)
all_alien_categories <- alien_tiers
model_data_alien <- build_side_data(occ_alien, model_data, all_alien_categories)
saveRDS(model_data_alien,
        here("data", "derived_data", "h5_polygon_buffer_data.rds"))

# Save the impact-split per-side datasets to reuse
saveRDS(build_side_data(occ_alien, model_data, high_impact_categories),
        here("data", "derived_data", "h5_highimpact_polygon_buffer_data.rds"))
saveRDS(build_side_data(occ_alien, model_data, lower_impact_categories),
        here("data", "derived_data", "h5_lowerimpact_polygon_buffer_data.rds"))

# Check that the primary dataset is built correctly
stopifnot(nrow(model_data_alien) == nrow(model_data),
          anyDuplicated(model_data_alien$poly_uid) == 0,
          !any(is.na(model_data_alien$n_occurrences)))
cat("\nAll-alien sides with >=1 record:", sum(model_data_alien$n_occurrences > 0),
    "(", round(100 * mean(model_data_alien$n_occurrences > 0), 1), "%)\n") # 11346 ( 4.4 %)

# 4. PREPARE ALL ALIENS DATA ---------------------------------------------------

# Use the functions defined above
pair_data <- make_pair_data(model_data_alien)
presence_data <- make_presence_data(model_data_alien)

# Check how many pairs we have to use
stopifnot(nrow(pair_data) == n_distinct(model_data_alien$pair_id),
          !any(is.na(pair_data$sor_polygon)),
          all(is.finite(pair_data$area_offset)))
cat("\nPairs:", nrow(pair_data),
    "| with any alien records:", sum(pair_data$any_records),
    "(", round(100 * mean(pair_data$any_records), 1), "%)\n")
# Pairs: 129881 | with any alien records: 9230 ( 7.1 %)

# Remove pairs that do not have any occurrences
pair_records <- pair_data |>
  filter(sor_total > 0) |>
  droplevels()

# Quick summary
cat("Pairs entering the split model (H5a / H5b):", nrow(pair_records), "\n")
# Pairs entering the split model (H5a / H5b): 9230
cat("Record-bearing pairs by land cover:\n")
print(table(pair_records$land_cover_name))
# Cropland             Forest          Grassland          Heathland        Settlements 
# 1203               5340                220                584               1593 
# Sparsely_vegetated           Wetlands 
# 168                122 

# 5. FIT ALL ALIEN MODELS ------------------------------------------------------

## 5.1. H5ab split model with full interaction ---------------------------------

# Set up the model
h5ab_betabin_full <- glmmTMB(cbind(sor_polygon, sor_buffer) ~
                               log_area_c * land_cover_name +
                               offset(area_offset) + (1 | kommune_factor),
                             data = pair_records, family = betabinomial)

# Save the model output
save(h5ab_betabin_full,
     file = here::here("data", "models", "h5ab_betabin_full.RData"))

## 5.2. H5ab split additive model ----------------------------------------------

# Set up the model
h5ab_betabin_additive <- glmmTMB(cbind(sor_polygon, sor_buffer) ~
                                   log_area_c + land_cover_name +
                                   offset(area_offset) + (1 | kommune_factor),
                                 data = pair_records, family = betabinomial)

# Save the model output
save(h5ab_betabin_additive,
     file = here::here("data", "models", "h5ab_betabin_additive.RData"))

# Compare models
AICtab(h5ab_betabin_full, h5ab_betabin_additive, base = TRUE)
#                       AIC     dAIC    df
# h5ab_betabin_additive 20685.1     0.0 10
# h5ab_betabin_full     20691.1     6.1 16

# Use the model with lower AIC
best_split <- h5ab_betabin_additive

## 5.3. H5 presence model with full interaction --------------------------------

# Define model
h5_presence_full <- glmmTMB(presence ~ polygon_type * (log_area_c + land_cover_name) +
                              (1 | kommune_factor/pair_id_factor),
                            data = presence_data, family = binomial)

# Save model output
save(h5_presence_full,
     file = here::here("data", "models", "h5_presence_full.RData"))

## 5.4. H5 additive presence model ---------------------------------------------

# Define model
h5_presence_additive <- glmmTMB(presence ~ polygon_type + log_area_c +
                                  land_cover_name +
                                  (1 | kommune_factor/pair_id_factor),
                                data = presence_data, family = binomial)

# Save model output
save(h5_presence_additive,
     file = here::here("data", "models", "h5_presence_additive.RData"))

# Compare models
AICtab(h5_presence_full, h5_presence_additive, base = TRUE)
#                      AIC     dAIC    df
# h5_presence_full     59369.7     0.0 18
# h5_presence_additive 59824.3   454.6 11

# Use the model with lower AIC
best_presence <- h5_presence_full

# 6. ALL ALIENS MODEL SUMMARIES ------------------------------------------------

## 6.1. All aliens split model -------------------------------------------------

# Quick look at the summary 
print(summary(best_split))

# Create tidy coefficient table to use in manuscript 
coef_table_split <- broom.mixed::tidy(best_split, effects = "fixed",
                                      conf.int = TRUE) |>
  mutate(Estimate = round(estimate, 3), SE = round(std.error, 3),
         `z value` = round(statistic, 2),
         `p value` = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))) |>
  select(Term = term, Estimate, SE, `z value`, `p value`)

# Save the tidy table
write.csv(coef_table_split,
          here("figures", "Table_H5ab_split_model_coefficients.csv"),
          row.names = FALSE)

## 6.2. All aliens presence model ----------------------------------------------

# Summary
print(summary(best_presence))

# Tidy coeffiecint table
coef_table_presence <- broom.mixed::tidy(best_presence, effects = "fixed",
                                         conf.int = TRUE) |>
  mutate(Estimate = round(estimate, 3), SE = round(std.error, 3),
         `z value` = round(statistic, 2),
         `p value` = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))) |>
  select(Term = term, Estimate, SE, `z value`, `p value`)

# Save the dify table
write.csv(coef_table_presence,
          here("figures", "Table_H5_presence_model_coefficients.csv"),
          row.names = FALSE)

# 7. ALL ALIENS MODEL DIAGNOSTICS ----------------------------------------------

## 7.1. All aliens split model -------------------------------------------------

# Simulate residuals
sim_residuals_split <- simulateResiduals(fittedModel = best_split, n = 1000)

# Create diagnostic plots
png(filename = here("figures", "Figure_H5ab_betabinomial_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals_split)
dev.off()

# Test dispersion
print(testDispersion(sim_residuals_split))

# Test outliers
print(testOutliers(sim_residuals_split))

## 7.2. All aliens presence model ----------------------------------------------

# Simulate residuals
sim_residuals_presence <- simulateResiduals(fittedModel = best_presence, n = 1000)

# Create diagnostic plots
png(filename = here("figures", "Figure_H5_presence_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals_presence)
dev.off()

# Test dispersion
print(testDispersion(sim_residuals_presence))

# 8. EXTRACT RANDOM EFFECTS ----------------------------------------------------

# Extract random effects for the split model
cat("\n=== H5a/H5b random effects (kommune) ===\n")
print(VarCorr(best_split))
# Conditional model:
# Groups         Name        Std.Dev.
# kommune_factor (Intercept) 0.32237 

# Extract random effects for the presence model
cat("\n=== H5 presence random effects (kommune / pair) ===\n")
print(VarCorr(best_presence))
# Conditional model:
# Groups                        Name        Std.Dev.
# pair_id_factor:kommune_factor (Intercept) 10.72053
# kommune_factor                (Intercept)  0.13531

# 9. ALL ALIENS MODELS HYPOTHESIS TESTING --------------------------------------

## 9.1. H5a - is the polygon share of alien SOR above 0.5? ---------------------

# Get the average share over land cover; estimate is area-adjusted share
# a value of 0.5 = no difference in density
emm_overall <- emmeans(best_split, ~ 1, offset = 0, type = "response")

# Get a summary
print(summary(emm_overall))

# Convert to df
emm_df <- as.data.frame(emm_overall)

# Get estimate and confidence interaval and covert it to scales that can be used in the manuscript
pi_hat <- emm_df$prob
ci_lo  <- emm_df[[grep("LCL|lower", names(emm_df), value = TRUE)[1]]]
ci_hi  <- emm_df[[grep("UCL|upper", names(emm_df), value = TRUE)[1]]]
cat(sprintf("\nPolygon share: %.3f  [%.3f, %.3f]\n", pi_hat, ci_lo, ci_hi)) # 0.333  [0.312, 0.353]
if (ci_lo > 0.5) {
  cat("H5a SUPPORTED: the polygon share CI lies entirely above 0.5.\n")
} else if (ci_hi < 0.5) {
  cat("H5a NOT supported: the share lies below 0.5 (buffers hold more).\n")
} else {
  cat("H5a inconclusive: the CI for the polygon share includes 0.5.\n")
} # H5a NOT supported: the share lies below 0.5 (buffers hold more)

## 9.2. H5b - does the sahre of alien SOR increase with area? ------------------

# Get the average area slope of the share, across land-cover
slope_overall <- emtrends(best_split, ~ 1, var = "log_area_c")

# Print summary of the slope
print(summary(slope_overall))

# Extract slope confidence intervals 
slope_df <- as.data.frame(slope_overall)
tcol <- grep("trend", names(slope_df), value = TRUE)[1]
slo_lo <- slope_df[[grep("LCL|lower", names(slope_df), value = TRUE)[1]]]
slo_hi <- slope_df[[grep("UCL|upper", names(slope_df), value = TRUE)[1]]]
cat(sprintf("\nArea slope: %.3f  [%.3f, %.3f]\n", slope_df[[tcol]], slo_lo, slo_hi)) # -0.108  [-0.133, -0.082]
if (slo_lo > 0) {
  cat("H5b SUPPORTED: the share increases with area (slope CI entirely > 0).\n")
} else if (slo_hi < 0) {
  cat("H5b NOT supported: the share DECREASES with area (buffer pulls ahead).\n")
} else {
  cat("H5b inconclusive: the area slope CI includes 0.\n")
} # H5b NOT supported: the share DECREASES with area (buffer pulls ahead)

# Area slope + share by land-cover
slope_landcover <- emtrends(best_split, ~ land_cover_name, var = "log_area_c")

# Save slopes to file
write.csv(as.data.frame(slope_landcover),
          here("figures", "Table_H5b_area_slope_by_landcover.csv"), row.names = FALSE)
emm_landcover <- emmeans(best_split, ~ land_cover_name, offset = 0, type = "response")
landcover_df <- as.data.frame(emm_landcover)
write.csv(landcover_df,
          here("figures", "Table_H5a_share_by_landcover.csv"), row.names = FALSE)

# LRT for the area x land cover interaction
lrt_split <- anova(h5ab_betabin_additive, h5ab_betabin_full)
print(lrt_split)

# Save outputs
saveRDS(list(h5a_overall_share = emm_overall, h5a_share_by_lc = emm_landcover,
             h5b_area_slope = slope_overall, h5b_slope_by_lc = slope_landcover,
             lrt_interaction = lrt_split),
        here("data", "models", "h5ab_betabin_inference.rds"))

## 9.3 Combined emmeans share table (overall + land-cover) ---------------------

# Extract values
grab <- function(df) {
  lo <- df[[grep("LCL|lower", names(df), value = TRUE)[1]]]
  hi <- df[[grep("UCL|upper", names(df), value = TRUE)[1]]]
  data.frame(share = df$prob, conf.low = lo, conf.high = hi)
}

# Convert to df
overall_row <- data.frame(
  land_cover = "Overall (averaged)",
  share = emm_overall_df$prob,
  conf.low = emm_overall_df$asymp.LCL,
  conf.high = emm_overall_df$asymp.UCL)

landcover_rows <- data.frame(
  land_cover = gsub("_", " ", as.character(emm_landcover_df$land_cover_name)),
  share = emm_landcover_df$prob,
  conf.low = emm_landcover_df$asymp.LCL,
  conf.high = emm_landcover_df$asymp.UCL)

# Combine into single table
share_table <- bind_rows(overall_row, landcover_rows) |>
  mutate(odds_ratio = round(share / (1 - share), 2),
         index = round(2 * share - 1, 3),
         share = round(share, 3),
         conf.low = round(conf.low, 3),
         conf.high = round(conf.high, 3)) |>
  transmute(`Land cover`  = land_cover,
            `Polygon share` = share,
            `CI lower` = conf.low,
            `CI upper` = conf.high,
            `Odds ratio (polygon:buffer)` = odds_ratio,
            `Index (2p-1)` = index)

# Quick check 
print(share_table)

# Save to file
write.csv(share_table,
          here("figures", "Table_S_H5a_share_by_landcover_full.csv"),
          row.names = FALSE)

## 9.4. H5 presebce - are polygons less likely to be empty of alien SOR? -------

# Get the probability of presence for polygons and buffers averaged over area and land-cover
cat("\nH5 presence: development polygons vs buffers holding any alien record.\n\n")
emm_presence <- emmeans(best_presence, ~ polygon_type, type = "response")
print(summary(emm_presence))

# Compare development polygons vs buffers as an odds ratio
contrast_presence <- contrast(emm_presence, method = "revpairwise", type = "response")
print(summary(contrast_presence, infer = TRUE))

# Get the odds-ratio CI
con_df <- as.data.frame(confint(contrast_presence))
or_col <- grep("ratio|estimate", names(con_df), value = TRUE)[1]
or_lo <- con_df[[grep("LCL|lower", names(con_df), value = TRUE)[1]]]
or_hi <- con_df[[grep("UCL|upper", names(con_df), value = TRUE)[1]]]
cat(sprintf("\nOdds ratio (Development / Buffer): %.3f  [%.3f, %.3f]\n",
            con_df[[or_col]], or_lo, or_hi))
if (or_lo > 1) {
  cat("H5 presence SUPPORTED: development polygons more likely to hold alien records.\n")
} else if (or_hi < 1) {
  cat("H5 presence NOT supported: development polygons MORE likely to be empty.\n")
} else {
  cat("H5 presence inconclusive: the odds-ratio CI includes 1.\n")
} # H5 presence NOT supported: development polygons MORE likely to be empty.

# Save output
saveRDS(list(presence_by_side = emm_presence, dev_vs_buffer = contrast_presence),
        here("data", "models", "h5_presence_inference.rds"))

# 10. ALL ALIENS MODELS PREDICTION FIGURES -------------------------------------

# Define a function to display land-cover names neatly
pretty_lc <- function(x) {
  x <- gsub("_", " ", x)
  gsub("(^|\\s)([a-z])", "\\1\\U\\2", x, perl = TRUE)
}

# Back-transform the centred log-area axis to hectares
mean_log_split    <- mean(log(pair_data$area_polygon))         # 10.1 (split)
mean_log_presence <- mean(log(presence_data$area_m2_numeric))  # 10.2 (presence)

# Get x axis min and max based on values
breaks_m2_split <- c(min(pair_records$area_polygon),
                     1e3, 1e4, 1e5, 1e6,
                     max(pair_records$area_polygon))
breaks_m2_pres  <- c(min(presence_data$area_m2_numeric),
                     1e3, 1e4, 1e5, 1e6,
                     max(presence_data$area_m2_numeric))

# Label in hectares
ha_label <- function(x_m2) {
  v <- x_m2 / 1e4
  vapply(v, function(a) {
    if (a >= 1) formatC(a, format = "f", digits = 0, big.mark = ",")
    else        formatC(signif(a, 2), format = "g")
  }, character(1))
}

## 10.1 Predicted share of Alien SOR by area and land-cover --------------------

# Predict values
predictions_split <- ggpredict(best_split,
                               terms = c("log_area_c [all]", "land_cover_name"),
                               condition = c(area_offset = 0))

# Convert to df
pred_df_split <- as.data.frame(predictions_split) |>
  rename(log_area_c = x, land_cover_name = group)

# Add pair records
lc_ranges_split <- pair_records |>
  group_by(land_cover_name) |>
  summarise(lo = min(log_area_c), hi = max(log_area_c), .groups = "drop")
pred_df_split <- pred_df_split |>
  left_join(lc_ranges_split, by = "land_cover_name") |>
  filter(log_area_c >= lo, log_area_c <= hi) |>
  select(-lo, -hi)

# Plot prediction
(fig_split_predictions <- ggplot(pred_df_split, aes(x = log_area_c, y = predicted)) +
    geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#E66101", alpha = 0.2) +
    geom_line(colour = "#E66101", linewidth = 1.2) +
    facet_wrap(~land_cover_name, scales = "free_y", ncol = 3,
               labeller = as_labeller(pretty_lc)) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = log(breaks_m2_split) - mean_log_split,
                       labels = ha_label(breaks_m2_split)) +
    labs(x = "Polygon Area (ha)",
         y = "Predicted Share of Alien SOR Within the Development Polygons") +
    theme_classic() +
    theme(panel.grid = element_blank(), axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          strip.background = element_rect(fill = "grey90", colour = "black"),
          strip.text = element_text(size = 14, face = "bold")))

# Save figure to file
ggsave(here("figures", "Figure_H5ab_predicted_share_by_landcover.png"),
       fig_split_predictions, width = 14, height = 10, dpi = 600)
ggsave(here("figures", "Figure_H5ab_predicted_share_by_landcover.pdf"),
       fig_split_predictions, width = 14, height = 10, dpi = 600)

## 10.2. Presence probability by side and land-cover ---------------------------

# Get per-side presence probability for development vs buffer within land-cover
predictions_presence <- ggpredict(best_presence,
                                  terms = c("log_area_c [n=100]", "polygon_type",
                                            "land_cover_name"))

# Get df
pred_df_presence <- as.data.frame(predictions_presence) |>
  rename(log_area_c = x, polygon_type = group, land_cover_name = facet)
lc_ranges_presence <- presence_data |>
  group_by(land_cover_name, polygon_type) |>
  summarise(lo = min(log_area_c), hi = max(log_area_c), .groups = "drop")
pred_df_presence <- pred_df_presence |>
  left_join(lc_ranges_presence, by = c("land_cover_name", "polygon_type")) |>
  filter(log_area_c >= lo, log_area_c <= hi) |>
  select(-lo, -hi)

# Set colours
polygon_colours <- c("Buffer" = "#E66101", "Development" = "#5E3C99")

# Plot figure
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
    scale_x_continuous(breaks = log(breaks_m2_pres) - mean_log_presence,
                       labels = ha_label(breaks_m2_pres)) +
    labs(x = "Area (ha)",
         y = "Probability of Side Containing Any Alien SOR") +
    theme_classic() +
    theme(panel.grid = element_blank(), axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          strip.background = element_rect(fill = "grey90", colour = "black"),
          strip.text = element_text(size = 14, face = "bold"),
          legend.position = "right"))

# Save figure
ggsave(here("figures", "Figure_H5_presence_by_side_and_landcover.png"),
       fig_presence_predictions, width = 14, height = 10, dpi = 600)
ggsave(here("figures", "Figure_H5_presence_by_side_and_landcover.pdf"),
       fig_presence_predictions, width = 14, height = 10, dpi = 600)

# 11. HIGH-IMPACT (SE+HI) vs LOWER-IMPACT (PH/LO/NK/NR) ------------------------

# All aliens (from the primary models) + the two impact groups, same additive
# split model + full presence model, side by side
cat("\n=== H5ab impact split ===\n")
impact_comparison <- bind_rows(
  row_from_models("All aliens", best_split, best_presence, pair_records, presence_data),
  run_group_compact("High impact (SE+HI)",        high_impact_categories),
  run_group_compact("Lower impact (PH/LO/NK/NR)", lower_impact_categories)
)

# Check the comparisons
print(impact_comparison)

# Save the comparison
write.csv(impact_comparison,
          here("figures", "Table_H5ab_impact_split_comparison.csv"), row.names = FALSE)

# 12. BREAKDOWN BY RISK CATEGORY -----------------------------------------------

# Look at each risk category on its own 
# Rare risk categories (often SE) may be too sparse to fit -
# these come back as NA rows with a printed note rather than stopping the script
cat("\n=== H5ab per-tier breakdown ===\n")
tier_comparison <- bind_rows(lapply(alien_tiers, function(tc) run_group_compact(tc, tc)))

# Quick look at the comparison
print(tier_comparison)

# Save the by-risk category comparison
write.csv(tier_comparison,
          here("figures", "Table_H5ab_risk_tier_comparison.csv"), row.names = FALSE)

# 13. SUMMARY STATISTICS -------------------------------------------------------

# Build the three per-side datasets 
model_data_high  <- build_side_data(occ_alien, model_data, high_impact_categories)
model_data_lower <- build_side_data(occ_alien, model_data, lower_impact_categories)

# Helper function to get: presence %, all-sides counts, and record-bearing counts, per side, tagged
side_summary <- function(side, group_label) {
  presence <- side |> group_by(polygon_type) |>
    summarise(group = group_label, n_sides = n(),
              n_with_records = sum(n_occurrences > 0),
              pct_with_records = round(100 * mean(n_occurrences > 0), 1),
              .groups = "drop")
  nonzero <- side |> filter(n_occurrences > 0) |> group_by(polygon_type) |>
    summarise(group = group_label, n_record_sides = n(),
              mean = round(mean(n_occurrences), 2), median = median(n_occurrences),
              q25 = quantile(n_occurrences, 0.25), q75 = quantile(n_occurrences, 0.75),
              IQR = IQR(n_occurrences), max = max(n_occurrences), .groups = "drop")
  list(presence = presence, nonzero = nonzero)
}

# Apply function 
groups <- list(side_summary(model_data_alien, "All aliens"),
               side_summary(model_data_high,  "High impact (SE+HI)"),
               side_summary(model_data_lower, "Lower impact (PH/LO/NK/NR)"))

# % of polygons and buffers with ANY records, by group
presence_summary_alien <- bind_rows(lapply(groups, `[[`, "presence")) |>
  select(group, polygon_type, n_sides, n_with_records, pct_with_records)
cat("\n=== % of sides with any alien records ===\n"); print(presence_summary_alien)

# Record counts on RECORD-BEARING sides only (meaningful median/IQR), by group
occ_summary_nonzero_alien <- bind_rows(lapply(groups, `[[`, "nonzero")) |>
  select(group, polygon_type, n_record_sides, mean, median, q25, q75, IQR, max)
cat("\n=== Alien SOR per side, record-bearing sides only ===\n")
print(occ_summary_nonzero_alien)

# Save
write.csv(presence_summary_alien,
          here("figures", "Table_H5_presence_summary_by_side_alien.csv"), row.names = FALSE)
write.csv(occ_summary_nonzero_alien,
          here("figures", "Table_H5_SOR_summary_by_side_alien.csv"), row.names = FALSE)

# END OF SCRIPT ----------------------------------------------------------------