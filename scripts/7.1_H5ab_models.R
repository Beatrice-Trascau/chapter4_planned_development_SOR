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

