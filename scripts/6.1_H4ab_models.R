##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 6.1_H4ab_models
# This script contains code to test H4a: Area plan polygons have a
# greater number of red-listed SOR than areas not planned for development, and
# H4b: the share of red-listed SOR inside polygons increases with area.
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source setup file
library(here)
source(here("scripts", "0_setup.R"))

# Load model data
model_data <- readRDS(here("data", "derived_data", "h2_polygon_buffer_data.rds"))

# Load the occurrence-level join
occ_join <- readRDS(here("data", "derived_data",
                         "h2d_polygon_buffer_occurrence_join.rds"))

# Quick glance at the data
cat("Sides loaded:", nrow(model_data), "\n") # 259762 (2 per pair)
print(table(model_data$polygon_type))

# 2. INCORPORATE RED LIST INTO MODEL DATA --------------------------------------

# Create two groups: 1) the red-listed categories and 2) the LC category as a baseline to compare to
redlisted_categories <- c("CR", "EN", "VU", "NT", "DD")
baseline_categories  <- c("LC")

# Combine all assessed categories to retain in the canonical list
assessed_categories <- c(redlisted_categories, baseline_categories)

# Order the categories by severity
severity_order <- c("CR", "EN", "VU", "NT", "DD", "LC")

# Create a helped function to deal with the spreadsheet
clean_name <- function(x) {
  x <- gsub("\u00a0", " ", x)   # non-breaking space -> normal space
  x <- gsub("\\s+", " ", x)     # collapse repeated whitespace
  trimws(x)
}

## 2.1. Reconstruct the harmonisted red list as it is in script 3.3 ------------

# Load the raw red list
redlist_raw <- read_excel(here("data", "raw_data", "rødliste-2021.xlsx"))

# Clean the raw red list following the same steps as in script 3.3
redlist_clean <- redlist_raw |>
  filter(`Vurderingsområde` == "Norge") |>
  select(scientific_name = `Vitenskapelig navn`,
         redlist_category = `Kategori 2021`) |>
  # keep the same six categories 3.3 kept
  filter(redlist_category %in% c("CR", "EN", "VU", "NT", "LC", "DD")) |>
  distinct(scientific_name, .keep_all = TRUE)

# Load the backbone lookup produced in script 3.3
backbone_path <- here("data", "derived_data", "redlist_backbone_lookup.rds")
backbone_lookup <- readRDS(backbone_path)

# Check that the two have the same length (to make sure the koin will work)
stopifnot(nrow(backbone_lookup) == nrow(redlist_clean))

# Join the backbone lookup into the clean redlist
redlist_harmonised <- redlist_clean |>
  mutate(gbif_species = backbone_lookup$species,
         match_type = backbone_lookup$matchType) |>
  # keep original name where GBIF match is unavailable
  mutate(gbif_species = ifelse(is.na(gbif_species),
                               scientific_name, gbif_species))

# Quick look at the harmonised redlist
print(table(redlist_harmonised$match_type))
# EXACT      FUZZY HIGHERRANK       NONE 
# 23295        191         75         10 

## 2.2. Resolve exact matches --------------------------------------------------

# Keep the exact matches
redlist_exact <- redlist_harmonised |>
  filter(match_type == "EXACT") |>
  transmute(species = clean_name(gbif_species),
            redlist_category = redlist_category)

# Check how many rows had an exact match
cat("EXACT-match red list species:", nrow(redlist_exact), "\n") # 23295

## 2.3. Manually resolve non-exact matches -------------------------------------

# Load the spreadsheet with the manually resolved mismatches
manual_path <- here("data", "raw_data", "Species_Taxonomic_Harmonisation.xlsx")
manual_raw <- read_excel(manual_path, sheet = "Red List of Species")

# Rules (because apparently I can't make a spreadsheet with coherent logic:
#   Accepted = Y -> use the GBIF Species Name.
#       (Exception: a handful of Accepted = Y rows carry a New Name that DIFFERS
#        from the GBIF Species Name because the GBIF string is a synonym /
#        subspecies / aggregate that would not match any GBIF occurrence record -
#        e.g. Ammophila arenaria -> Calamagrostis arenaria. For those we will take the
#        New Name so the species can actually be matched. 
#   Accepted = N -> use the New Name, UNLESS it says "remove" or is blank -> drop
use_newname_when_differs <- TRUE
manual_resolved <- manual_raw |>
  rename(scientific_name = `Red List Species Name (Scientific Name)`,
         redlist_category = `Risk Category`,
         gbif_species = `GBIF Species Name`,
         match_type = `Match Type`,
         accepted = `Accepted?`,
         new_name = `New Name?`) |>
  mutate(accepted = toupper(trimws(accepted)),
         gbif_clean = clean_name(gbif_species),
         new_clean = clean_name(new_name),
         has_new = !is.na(new_clean) & nzchar(new_clean),
         is_remove = has_new & tolower(new_clean) == "remove") |>
  mutate(resolved = case_when(
    # Accepted = Y: GBIF Species Name, but prefer a differing New Name
    accepted == "Y" & use_newname_when_differs & has_new &
      new_clean != gbif_clean               ~ new_clean,
    accepted == "Y"                          ~ gbif_clean,
    # Accepted = N: New Name, unless "remove" or blank -> drop (NA)
    accepted == "N" & has_new & !is_remove   ~ new_clean,
    TRUE                                      ~ NA_character_))

# Check that it all went ok
cat("rows total: ", nrow(manual_resolved), "\n") #276
cat("Accepted = Y kept: ", sum(manual_resolved$accepted == "Y"), "\n") #214
cat("Accepted = N with New Name kept: ", sum(manual_resolved$accepted == "N" & !is.na(manual_resolved$resolved)), "\n") #54
cat("dropped ('remove' or blank): ", sum(is.na(manual_resolved$resolved)), "\n") # 8
cat("  Accepted = Y rows where New Name overrode GBIF name:\n")
print(manual_resolved |>
        filter(accepted == "Y", resolved == new_clean, new_clean != gbif_clean) |>
        select(gbif_clean, new_clean))
# gbif_clean                      new_clean             
# <chr>                           <chr>                 
# 1 Ammophila arenaria            Calamagrostis arenaria
# 2 Erigeron acris subsp. politus Erigeron acris        
# 3 Ranunculus auricomus agg.     Ranunculus auricomus  

# Keep only the resolved rows
manual_resolved <- manual_resolved |>
  filter(!is.na(resolved)) |>
  transmute(species = resolved, redlist_category)

# Check how many rows are left
cat("Accepted manually-resolved species:", nrow(manual_resolved), "\n") #268

## 2.4. Combine into single df -------------------------------------------------

# Combine the resolved list with the red-list
redlist_final <- bind_rows(redlist_exact, manual_resolved) |>
  filter(!is.na(species), nzchar(species)) |>
  # order most-threatened first, then keep one row per species (most severe wins)
  mutate(redlist_category = factor(redlist_category, levels = severity_order)) |>
  arrange(species, redlist_category) |>
  distinct(species, .keep_all = TRUE) |>
  mutate(redlist_category = as.character(redlist_category)) |>
  # safety filter: keep only assessed categories (drops any stray values)
  filter(redlist_category %in% assessed_categories) |>
  # tag the group so downstream code can split red-listed vs LC baseline
  mutate(group = ifelse(redlist_category %in% redlisted_categories,
                        "redlisted", "LC_baseline"))

# Check how many species are in each category 
cat("\nCanonical list:", nrow(redlist_final), "species\n") # 23101 species
cat("Category breakdown:\n")
print(table(redlist_final$redlist_category))
# CR    DD    EN    LC    NT    VU 
# 296   738   959 18265  1360  1483 
cat("Group breakdown:\n")
print(table(redlist_final$group))
# LC_baseline   redlisted 
# 18265        4836 

# Save the resolved red list to use in 6.2. and 6.3
saveRDS(redlist_final,
        here("data", "derived_data", "redlist_harmonised_final.rds"))

# 3. BUILD THE REDLISTED RECORDS PER DATASET -----------------------------------

## 3.1. Flag and keep red-listed occurrences -----------------------------------

# Attach the category 
occ_assessed <- occ_join |>
  filter(!is.na(gbifID)) |>
  mutate(species = clean_name(species)) |>
  inner_join(redlist_final, by = "species")   # keeps only assessed occurrences

# Check hoe many records are redlisted
cat("\nAssessed occurrence records:", nrow(occ_assessed), "of", sum(!is.na(occ_join$gbifID)), "matched records\n")
#615302 of 682121 matched records
cat("Occurrences by category:\n")
print(table(occ_assessed$redlist_category))
# CR     DD     EN     LC     NT     VU 
# 5501    671   7969 508868  42662  49631 
cat("Occurrences by group:\n")
print(table(occ_assessed$group))
# LC_baseline   redlisted 
# 508868      106434

# Save the red-listed occurrence-level join for H4d
saveRDS(occ_assessed |> filter(group == "redlisted"),
        here("data", "derived_data", "h4_polygon_buffer_occurrence_join.rds"))
saveRDS(occ_assessed |> filter(group == "LC_baseline"),
        here("data", "derived_data",
             "h4_LC_baseline_polygon_buffer_occurrence_join.rds"))

## 3.2. Create a helper function to build a per-side df with category ----------

# Keep all sides
build_side_data <- function(occ_assessed, model_data, keep_categories) {
  counts <- occ_assessed |>
    filter(redlist_category %in% keep_categories) |>
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

# Red-listed (primary) and LC baseline per-side datasets
model_data_rl <- build_side_data(occ_assessed, model_data, redlisted_categories)
model_data_lc <- build_side_data(occ_assessed, model_data, baseline_categories)

## 3.3. CHeck the red-listed side data -----------------------------------------

# Use one row per side, no rows gained or lost, no NA counts
stopifnot(nrow(model_data_rl) == nrow(model_data),
          anyDuplicated(model_data_rl$poly_uid) == 0,
          !any(is.na(model_data_rl$n_occurrences)),
          !any(is.na(model_data_rl$n_species)))

# Check that the pairing (1 Development polgygon + 1 Buffer per pair) is still looking good
pair_counts <- model_data_rl |>
  group_by(pair_id) |>
  summarise(n_dev = sum(polygon_type == "Development"),
            n_buf = sum(polygon_type == "Buffer"), .groups = "drop")
stopifnot(all(pair_counts$n_dev == 1 & pair_counts$n_buf == 1))
cat("\nPASS: red-listed side data assembled, pairing intact\n") # PASS

# Quickly inspect the data
cat("\nRed-listed (CR/EN/VU/NT/DD):\n")
cat(" mean SOR by side:\n"); print(tapply(model_data_rl$n_occurrences,
                                           model_data_rl$polygon_type, mean))
# mean SOR by side
# Buffer      Development 
# 0.3744967   0.4449766 
cat(" sides with >=1 record:", sum(model_data_rl$n_occurrences > 0),
    "(", round(100 * mean(model_data_rl$n_occurrences > 0), 1), "%)\n")
# sides with >=1 record: 10187 ( 3.9 %)
cat("LC baseline:\n")
cat("  sides with >=1 record:", sum(model_data_lc$n_occurrences > 0),
    "(", round(100 * mean(model_data_lc$n_occurrences > 0), 1), "%)\n")
# LC baseline:  sides with >=1 record: 22810 ( 8.8 %)

# Save the red-listed per-side dataset
saveRDS(model_data_rl,
        here("data", "derived_data", "h4_polygon_buffer_data.rds"))
saveRDS(model_data_lc,
        here("data", "derived_data", "h4_LC_baseline_polygon_buffer_data.rds"))

# 4. PREPARE DATA FOR MODELLING- -----------------------------------------------

## 4.1. Use a helper function to reshape a per-side dataset for both groups ----

# One row per pair, polygon and buffer counts side by side
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
           # share of the SOR belonging to the polygon
           share_polygon = ifelse(sor_total > 0, sor_polygon / sor_total, NA_real_),
           # centred log polygon area so the intercept refers to an average pair
           log_area_c = as.numeric(scale(log(area_polygon), scale = FALSE)),
           # area offset to adjust the share for the polygon/buffer area difference
           area_offset = log(area_polygon / area_buffer),
           any_records = as.integer(sor_total > 0),
           kommune_factor  = factor(kommune),
           land_cover_name = factor(land_cover_name))
}

# One row per side, presence flag for the group's records
make_presence_data <- function(side) {
  side |>
    mutate(presence = as.integer(n_occurrences > 0),
           polygon_type = factor(polygon_type, levels = c("Buffer", "Development")),
           log_area_c = as.numeric(scale(log(area_m2_numeric), scale = FALSE)),
           land_cover_name = factor(land_cover_name),
           kommune_factor = factor(kommune),
           pair_id_factor = factor(pair_id))
}

## 4.2. Reshape df -------------------------------------------------------------

# Use the function to reshape the data
pair_data <- make_pair_data(model_data_rl)
presence_data <- make_presence_data(model_data_rl)

# Check that we still have one row per pair
stopifnot(nrow(pair_data) == n_distinct(model_data_rl$pair_id))
cat("\nPairs after reshape:", nrow(pair_data), "\n") # 129881

# Make sure the counts are complete and the area values are finite
stopifnot(!any(is.na(pair_data$sor_polygon)),
          !any(is.na(pair_data$sor_buffer)),
          all(is.finite(pair_data$area_offset)),
          all(is.finite(pair_data$log_area_c)))
cat("PASS: counts complete and offset/area finite\n") # PASS
cat("Pairs with any red-listed records:", sum(pair_data$any_records), "of",
    nrow(pair_data), "(", round(100 * mean(pair_data$any_records), 1), "%)\n")
# Pairs with any red-listed records: 8405 of 129881 ( 6.5 %)
cat("Polygon share of red-listed records (record-bearing pairs only):\n")
print(summary(pair_data$share_polygon))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs 
# 0.0000  0.0000  0.2000  0.4233  1.0000  1.0000  121476 

# 5. FIT MODELS  ---------------------------------------------------------------

# Separate the pairs with no red-listed records from those that have red-listed records
pair_records <- pair_data |>
  filter(sor_total > 0) |>
  droplevels()   # drop any land-cover / kommune levels with no record-bearing pairs
cat("\nPairs entering the split model (H4a / H4b):", nrow(pair_records), "\n") # 8405
cat("Record-bearing pairs by land cover:\n")
print(table(pair_records$land_cover_name))
# Cropland             Forest          Grassland          Heathland        Settlements Sparsely_vegetated 
# 938               5169                233                761                988                168 
# Wetlands 
# 148 

## 5.1. H4ab split model with full interaction ---------------------------------

# Build model
h4ab_betabin_full <- glmmTMB(cbind(sor_polygon, sor_buffer) ~
                               log_area_c * land_cover_name +
                               offset(area_offset) + (1 | kommune_factor),
                             data   = pair_records,
                             family = betabinomial)

# Save model output
save(h4ab_betabin_full,
     file = here::here("data", "models", "h4ab_betabin_full.RData"))

## 5.2. H4ab additive split model ----------------------------------------------

# Build the model
h4ab_betabin_additive <- glmmTMB(cbind(sor_polygon, sor_buffer) ~
                                   log_area_c + land_cover_name +
                                   offset(area_offset) + (1 | kommune_factor),
                                 data   = pair_records,
                                 family = betabinomial)

# SAve the output
save(h4ab_betabin_additive,
     file = here::here("data", "models", "h4ab_betabin_additive.RData"))

# Compare the models
AICtab(h4ab_betabin_full, h4ab_betabin_additive, base = TRUE)
# AIC     dAIC    df
# h4ab_betabin_additive 21492.8     0.0 10
# h4ab_betabin_full     21501.6     8.8 16

# Pick the better model
best_split <- h4ab_betabin_additive

## 5.3. H4 presence model with full interaction --------------------------------

# Do polygons differ from buffers in the probability of holding ANY red-listed
# record at all? (analogue of the H1 presence model in 4.1) 
h4_presence_full <- glmmTMB(presence ~ polygon_type * (log_area_c + land_cover_name) +
                              (1 | kommune_factor/pair_id_factor),
                            data   = presence_data,
                            family = binomial)

# Save model
save(h4_presence_full,
     file = here::here("data", "models", "h4_presence_full.RData"))

## 5.4. H4 presence model additive ---------------------------------------------

# Build the model
h4_presence_additive <- glmmTMB(presence ~ polygon_type + log_area_c +
                                  land_cover_name +
                                  (1 | kommune_factor/pair_id_factor),
                                data   = presence_data,
                                family = binomial)

# Save the model output
save(h4_presence_additive,
     file = here::here("data", "models", "h4_presence_additive.RData"))

# Compare the models
AICtab(h4_presence_full, h4_presence_additive, base = TRUE)
#                      AIC     dAIC    df
# h4_presence_full     57090.2     0.0 18
# h4_presence_additive 57203.1   112.9 11


# Use the better presence model
best_presence <- h4_presence_full

# 6. MODEL SUMMARIES -----------------------------------------------------------

## 6.1. H4ab model -------------------------------------------------------------

# Quick look at the model summary
print(summary(best_split))

# Create a simple coefficient table to use in the manuscript
coef_table_split <- broom.mixed::tidy(best_split,
                                      effects  = "fixed",
                                      conf.int = TRUE) |>
  mutate(Estimate = round(estimate, 3),
         SE = round(std.error, 3),
         `z value` = round(statistic, 2),
         `p value` = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))) |>
  select(Term = term, Estimate, SE, `z value`, `p value`)

# Save to file
write.csv(coef_table_split,
          here("figures", "Table_H4ab_split_model_coefficients.csv"),
          row.names = FALSE)

## 6.2. H4 presence model ------------------------------------------------------

# Quick look at the model summary
print(summary(best_presence))

# Create a simple coefficient table to use in the manuscript
coef_table_presence <- broom.mixed::tidy(best_presence,
                                         effects  = "fixed",
                                         conf.int = TRUE) |>
  mutate(Estimate = round(estimate, 3),
         SE = round(std.error, 3),
         `z value` = round(statistic, 2),
         `p value` = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))) |>
  select(Term = term, Estimate, SE, `z value`, `p value`)

# Save to file
write.csv(coef_table_presence,
          here("figures", "Table_H4_presence_model_coefficients.csv"),
          row.names = FALSE)

# 7. MODEL DIAGNOSTIC WITH DHARMA ----------------------------------------------

## 7.1 H4ab model --------------------------------------------------------------

# Simulate residuals
sim_residuals_split <- simulateResiduals(fittedModel = best_split, n = 1000)

# Create & save diagnostic plots
png(filename = here("figures", "Figure_H4ab_betabinomial_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals_split)
dev.off()

# Test dispersion 
print(testDispersion(sim_residuals_split))

# Test outliers
print(testOutliers(sim_residuals_split))

## 7.2. H4 presence model ------------------------------------------------------

# Simulate residuals
sim_residuals_presence <- simulateResiduals(fittedModel = best_presence, n = 1000)

# Create & save diagnostic plots
png(filename = here("figures", "Figure_H4_presence_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals_presence)
dev.off()

# Test dispersion
print(testDispersion(sim_residuals_presence))

# Test outliers
print(testOutliers(sim_residuals_presence))

# 8. EXTRACT RANDOM EFFECTS ----------------------------------------------------

# Extract random effects for H4ab model
random_effects_split <- VarCorr(best_split)
cat("\n=== H4a/H4b random effects (kommune) ===\n") #0.20113 
print(random_effects_split)
# Conditional model:
#   Groups         Name        Std.Dev.
# kommune_factor (Intercept) 0.20113 
re_var_split <- as.numeric(random_effects_split$cond$kommune_factor[1])
cat("Random effect variance (kommune):", round(re_var_split, 4), "\n") #0.0405

# Extract random effects for H4 presence model
random_effects_presence <- VarCorr(best_presence)
cat("\n=== H4 presence random effects (kommune / pair) ===\n") 
print(random_effects_presence)
# Conditional model:
# Groups                        Name        Std.Dev.  
# pair_id_factor:kommune_factor (Intercept) 8.0832e+00
# kommune_factor                (Intercept) 5.3505e-05

# 9. HYPOTHESIS TESTING --------------------------------------------------------

# Define functions to extract model output for the least concern (LC) baseline comparisons 
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

## 9.1. H4a - is the polygon share of red-listed SOR above 0.5? ----------------

# Average share of SOR over land-cover (a value of 0.5 = no difference in density)
emm_overall <- emmeans(best_split, ~ 1, offset = 0, type = "response")
cat("Estimated polygon share of red-listed records (averaged over land cover):\n")
print(summary(emm_overall))

# Get estimate and CI and covert to scales you can report
emm_df <- as.data.frame(emm_overall)
pi_hat <- emm_df$prob
ci_lo  <- emm_df[[grep("LCL|lower", names(emm_df), value = TRUE)[1]]]
ci_hi  <- emm_df[[grep("UCL|upper", names(emm_df), value = TRUE)[1]]]

# Interpret results
to_ratio <- function(p) p / (1 - p)
to_index <- function(p) 2 * p - 1

cat("\n--- H4a effect size ---\n")
cat(sprintf("Polygon share:  %.3f  [%.3f, %.3f]\n", pi_hat, ci_lo, ci_hi))
#Polygon share:  0.455  [0.435, 0.474]
cat(sprintf("Polygon:buffer ratio:  %.3f  [%.3f, %.3f]\n",
            to_ratio(pi_hat), to_ratio(ci_lo), to_ratio(ci_hi)))
#Polygon:buffer ratio:  0.833  [0.771, 0.901]
cat(sprintf("Symmetric index (2p-1): %.3f  [%.3f, %.3f]\n",
            to_index(pi_hat), to_index(ci_lo), to_index(ci_hi)))
# Symmetric index (2p-1): -0.091  [-0.129, -0.052]

if (ci_lo > 0.5) {
  cat("\nH4a SUPPORTED: the CI for the polygon share lies entirely above 0.5.\n")
} else if (ci_hi < 0.5) {
  cat("\nH4a NOT supported: the share lies below 0.5 (buffers hold more).\n")
} else {
  cat("\nH4a inconclusive: the CI for the polygon share includes 0.5.\n")
}
# H4a NOT supported: the share lies below 0.5 (buffers hold more)

## 9.2. H4b - does the share of red-listed SOR increase with area? -------------
cat("\nH4b: red-listed SOR rises with area faster inside polygons than outside.\n")
# H4b: red-listed SOR rises with area faster inside polygons than outside.
cat("     A POSITIVE log_area_c slope means the polygon pulls ahead of its\n")
cat("     buffer as pairs get larger.\n\n")

# Average area slope of the share of SOR across land-covers
slope_overall <- emtrends(best_split, ~ 1, var = "log_area_c")
cat("Average effect of log(area) on the polygon share (logit scale):\n")
print(summary(slope_overall))
# 1       log_area_c.trend     SE  df asymp.LCL asymp.UCL
# overall          -0.0467 0.0123 Inf   -0.0709   -0.0225

# Extract slope CI
slope_df  <- as.data.frame(slope_overall)
trend_col <- grep("trend", names(slope_df), value = TRUE)[1]
slo_lo    <- slope_df[[grep("LCL|lower", names(slope_df), value = TRUE)[1]]]
slo_hi    <- slope_df[[grep("UCL|upper", names(slope_df), value = TRUE)[1]]]
slo_est   <- slope_df[[trend_col]]

cat(sprintf("\nArea slope: %.3f  [%.3f, %.3f]\n", slo_est, slo_lo, slo_hi)) #Area slope: -0.047  [-0.071, -0.023]
if (slo_lo > 0) {
  cat("H4b SUPPORTED: the share increases with area (slope CI entirely > 0).\n")
} else if (slo_hi < 0) {
  cat("H4b NOT supported: the share DECREASES with area (buffer pulls ahead).\n")
} else {
  cat("H4b inconclusive: the area slope CI includes 0.\n")
}
#H4b NOT supported: the share DECREASES with area (buffer pulls ahead).

# Get area slope per land-cover
slope_landcover <- emtrends(best_split, ~ land_cover_name, var = "log_area_c")
cat("\nArea slope of the share by land cover (logit scale):\n")
print(summary(slope_landcover))

# Save slopes to file
write.csv(as.data.frame(slope_landcover),
          here("figures", "Table_H4b_area_slope_by_landcover.csv"),
          row.names = FALSE)

## 9.3. Does the split of red-listed SOR depend on land cover? -----------------

cat("\n=== LRT for the area x land cover interaction (split model) ===\n")
lrt_split <- anova(h4ab_betabin_additive, h4ab_betabin_full)
print(lrt_split)

## 9.4. H4a share by land-cover ------------------------------------------------

# Extract emmeans by land-cover
emm_landcover <- emmeans(best_split, ~ land_cover_name, offset = 0, type = "response")
cat("\n=== Estimated polygon share of red-listed SOR by land cover ===\n")
print(summary(emm_landcover))

# Convert to df and save
landcover_df <- as.data.frame(emm_landcover)
write.csv(landcover_df,
          here("figures", "Table_H4a_share_by_landcover.csv"),
          row.names = FALSE)

# Save the H4ab inference objects for later use
saveRDS(list(h4a_overall_share = emm_overall,
             h4a_share_by_lc = emm_landcover,
             h4b_area_slope = slope_overall,
             h4b_slope_by_lc = slope_landcover,
             lrt_interaction = lrt_split),
        here("data", "models", "h4ab_betabin_inference.rds"))

## 9.5. H4 presence: are polygons less likely to be empty of red-listed SOR? ----

cat("\nH4 presence: development polygons are LESS likely to hold zero red-listed\n")
cat("             records than their paired buffers.\n\n")
# H4 presence: development polygons are LESS likely to hold zero red-listed records than their paired buffers.

# Probability of presence for polygon vs buffer, averaged over area and land cover
emm_presence <- emmeans(best_presence, ~ polygon_type, type = "response")
cat("Probability a side holds any red-listed record, by side:\n")
print(summary(emm_presence))

# Development polygon vs buffer as an odds ratio
contrast_presence <- contrast(emm_presence, method = "revpairwise", type = "response")
cat("\nDevelopment vs Buffer (odds ratio for holding any red-listed record):\n")
print(summary(contrast_presence, infer = TRUE))   # infer = TRUE adds the CI
# contrast             odds.ratio     SE  df asymp.LCL asymp.UCL null z.ratio p.value
# Development / Buffer      0.724 0.0572 Inf      0.62     0.845    1  -4.096 <0.0001

# Get the odds-ratio CI
con_df <- as.data.frame(confint(contrast_presence))
or_col <- grep("ratio|estimate", names(con_df), value = TRUE)[1]
or_lo  <- con_df[[grep("LCL|lower", names(con_df), value = TRUE)[1]]]
or_hi  <- con_df[[grep("UCL|upper", names(con_df), value = TRUE)[1]]]
or_est <- con_df[[or_col]]

stopifnot(length(or_est) == 1, length(or_lo) == 1, length(or_hi) == 1)

cat(sprintf("\nOdds ratio (Development / Buffer): %.3f  [%.3f, %.3f]\n",
            or_est, or_lo, or_hi))
# Odds ratio (Development / Buffer): 0.724  [0.620, 0.845]
if (or_lo > 1) {
  cat("H4 presence SUPPORTED: development polygons are more likely to hold\n")
  cat("   red-listed records (less likely to be empty) than their buffers.\n")
} else if (or_hi < 1) {
  cat("H4 presence NOT supported: development polygons are MORE likely to be empty.\n")
} else {
  cat("H4 presence inconclusive: the odds-ratio CI includes 1.\n")
}
# H4 presence NOT supported: development polygons are MORE likely to be empty.

# Save presence inference
saveRDS(list(presence_by_side = emm_presence,
             dev_vs_buffer    = contrast_presence),
        here("data", "models", "h4_presence_inference.rds"))

## 9.6. Combined emmeans share table (overall + by land cover) -----------------

# Use helper function to pull the share and confidence intervals from the emmeans summary data frame
grab <- function(df) {
  lo <- df[[grep("LCL|lower", names(df), value = TRUE)[1]]]
  hi <- df[[grep("UCL|upper", names(df), value = TRUE)[1]]]
  data.frame(share = df$prob, conf.low = lo, conf.high = hi)
}

# Make into df
emm_overall_df <- as.data.frame(emm_overall)
emm_landcover_df <- as.data.frame(emm_landcover)

# Create pretty table to  use in the supplementary information
share_table <- bind_rows(cbind(land_cover = "Overall (averaged)", grab(emm_overall_df)),
                         cbind(land_cover = gsub("_", " ", as.character(emm_landcover_df$land_cover_name)),
                               grab(emm_landcover_df))) |>
  mutate(odds_ratio = round(share / (1 - share), 2),   # polygon:buffer OR
         index = round(2 * share - 1, 3),          # symmetric index
         share = round(share, 3),
         conf.low = round(conf.low, 3),
         conf.high = round(conf.high, 3)) |>
  transmute(`Land-cover` = land_cover,
            `Polygon share` = share,
            `CI lower` = conf.low,
            `CI upper` = conf.high,
            `Odds ratio (polygon:buffer)` = odds_ratio,
            `Index (2p-1)` = index)

# Print table
print(share_table)

# Save to file
write.csv(share_table,
          here("figures", "Table_S_H4a_share_by_landcover_full.csv"),
          row.names = FALSE)

## 9.7. H4 presence: ODDS RATIO within each land-cover -------------------------

# Extract contrast separately for each land-cover
emm_pres_lc <- emmeans(best_presence, ~ polygon_type | land_cover_name,
                       type = "response")

# Get odds ratio by land-cover
or_by_lc <- contrast(emm_pres_lc, method = "revpairwise", type = "response")

# Print summary
print(summary(or_by_lc, infer = TRUE))

# Create a neat table to use in the supplementary information
or_lc_df <- as.data.frame(confint(or_by_lc))
or_col <- grep("ratio|estimate", names(or_lc_df), value = TRUE)[1]
lo_col <- grep("LCL|lower", names(or_lc_df), value = TRUE)[1]
hi_col <- grep("UCL|upper", names(or_lc_df), value = TRUE)[1]
p_df <- as.data.frame(summary(or_by_lc, infer = TRUE))

# Extract the variables to use in the table 
or_table <- data.frame(`Land-cover` = gsub("_", " ", as.character(or_lc_df$land_cover_name)),
                       `Odds ratio (Dev/Buffer)` = round(or_lc_df[[or_col]], 3),
                       `CI lower` = round(or_lc_df[[lo_col]], 3),
                       `CI upper` = round(or_lc_df[[hi_col]], 3),
                       `p value` = ifelse(p_df$p.value < 0.001, "<0.001",
                                          round(p_df$p.value, 3)),
                       check.names = FALSE)

# Quick look at the table
print(or_table)

# Save to file
write.csv(or_table,
          here("figures", "Table_H4_presence_OddsRatio_by_landcover.csv"),
          row.names = FALSE)

## 9.8. H4 presence: proability of Red-listed SOR presence by land-cover -------

# Convert probability comparisons to df
prob_df <- as.data.frame(summary(emm_pres_lc))

# Make the df wide
prob_wide <- prob_df |>
  transmute(land_cover = gsub("_", " ", as.character(land_cover_name)),
            polygon_type,
            cell = sprintf("%.5f%% [%.5f–%.5f]",
                           100 * prob, 100 * asymp.LCL, 100 * asymp.UCL)) |>
  tidyr::pivot_wider(names_from = polygon_type, values_from = cell) |>
  rename(`Land cover` = land_cover,
         `Buffer, P(any records)` = Buffer,
         `Development, P(any records)` = Development)

# Check the table
print(prob_wide)

# Save to file
write.csv(prob_wide,
          here("figures", "Table_H4_presence_probability_by_landcover_wide.csv"),
          row.names = FALSE)

# 10. PLOT PREDICTIONS ---------------------------------------------------------

# Use a function to display the land-cover names corrrectly
pretty_lc <- function(x) {
  x <- gsub("_", " ", x)
  gsub("(^|\\s)([a-z])", "\\1\\U\\2", x, perl = TRUE)
}

# Back-transform the centered log-area axis to hectares
mean_log_split <- mean(log(pair_data$area_polygon))         # 10.1 (split)
mean_log_presence <- mean(log(presence_data$area_m2_numeric))  # 10.3 (presence)

# Use min and max for the tick marks
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

## 10.1. H4ab - predicted share of Red-Listed SOR by area and land-cover -------

# Predict values
predictions_split <- ggpredict(best_split,
                               terms     = c("log_area_c [all]", "land_cover_name"),
                               condition = c(area_offset = 0))

# Convert to df
pred_df_split <- as.data.frame(predictions_split) |>
  rename(log_area_c = x, land_cover_name = group)

# Only use observed values for log-area for each land-cover
lc_ranges_split <- pair_records |>
  group_by(land_cover_name) |>
  summarise(lo = min(log_area_c), hi = max(log_area_c), .groups = "drop")

# Add them to tehm prediction df
pred_df_split <- pred_df_split |>
  left_join(lc_ranges_split, by = "land_cover_name") |>
  filter(log_area_c >= lo, log_area_c <= hi) |>
  select(-lo, -hi)

# Plot figure
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
    scale_x_continuous(breaks = log(breaks_m2_split) - mean_log_split,
                       labels = ha_label(breaks_m2_split)) +
    labs(x = "Polygon Area (ha)",
         y = "Predicted Share of Red-listed SOR Within the Development Polygons") +
    theme_classic() +
    theme(panel.grid = element_blank(),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          strip.background = element_rect(fill = "grey90", colour = "black"),
          strip.text = element_text(size = 14, face = "bold")))

# Save to file
ggsave(filename = here("figures", "Figure_H4ab_predicted_share_by_landcover.png"),
       plot = fig_split_predictions, width = 14, height = 10, dpi = 600)
ggsave(filename = here("figures", "Figure_H4ab_predicted_share_by_landcover.pdf"),
       plot = fig_split_predictions, width = 14, height = 10, dpi = 600)

## 10.2. H4a - estimated share of Red-Listed SOR by land-cover -----------------

# Change the land coevr df for better plotting
landcover_plot_df <- landcover_df |>
  rename(share = prob) |>
  rename(conf.low  = grep("LCL|lower", names(landcover_df), value = TRUE)[1],
         conf.high = grep("UCL|upper", names(landcover_df), value = TRUE)[1])

# Plot figure
(fig_h4a_landcover <- ggplot(landcover_plot_df,
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
         y = "Estimated Share of Red-listed SOR Within the Development Polygons") +
    theme_classic() +
    theme(panel.grid = element_blank(),
          axis.title = element_text(size = 14),
          axis.text  = element_text(size = 12)))

# Save figure
ggsave(filename = here("figures", "Figure_H4a_share_by_landcover_pointrange.png"),
       plot = fig_h4a_landcover, width = 10, height = 7, dpi = 600)
ggsave(filename = here("figures", "Figure_H4a_share_by_landcover_pointrange.pdf"),
       plot = fig_h4a_landcover, width = 10, height = 7, dpi = 600)

## 10.3. H4 presence - probability of having any RL SOR by land-cover ----------

# Predict values
predictions_presence <- ggpredict(best_presence,
                                  terms = c("log_area_c [n=100]", "polygon_type",
                                            "land_cover_name"))

# Convert predictions to df
pred_df_presence <- as.data.frame(predictions_presence) |>
  rename(log_area_c = x, polygon_type = group, land_cover_name = facet)

# Only use observed log area values for each land-cover
lc_ranges_presence <- presence_data |>
  group_by(land_cover_name, polygon_type) |>
  summarise(lo = min(log_area_c), hi = max(log_area_c), .groups = "drop")

# Add the observed log area values for land-cover to the predictions df
pred_df_presence <- pred_df_presence |>
  left_join(lc_ranges_presence, by = c("land_cover_name", "polygon_type")) |>
  filter(log_area_c >= lo, log_area_c <= hi) |>
  select(-lo, -hi)

# Define colour scheme
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
         y = "Probability of Side Containing Any Red-listed SOR") +
    theme_classic() +
    theme(panel.grid = element_blank(),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          strip.background = element_rect(fill = "grey90", colour = "black"),
          strip.text = element_text(size = 14, face = "bold"),
          legend.position = "right",
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14)))

# Save to file
ggsave(filename = here("figures", "Figure_H4_presence_by_side_and_landcover.png"),
       plot = fig_presence_predictions, width = 14, height = 10, dpi = 600)
ggsave(filename = here("figures", "Figure_H4_presence_by_side_and_landcover.pdf"),
       plot = fig_presence_predictions, width = 14, height = 10, dpi = 600)

# 11. LEAST CONCERN (LC) BASELINE MODELS --------------------------------------

# Fit models with the same form as the previous red-listed models (i.e. one beta binomial split model and one presence model)
# Shape dfs
pair_data_lc <- make_pair_data(model_data_lc)
presence_data_lc <- make_presence_data(model_data_lc)

# Remove the pairs where both polygons and buffers are empty
pair_records_lc <- pair_data_lc |>
  filter(sor_total > 0) |>
  droplevels()
cat("\nLC baseline: pairs entering the split model:", nrow(pair_records_lc), "\n") #17411

# Run the split model
lc_split <- glmmTMB(cbind(sor_polygon, sor_buffer) ~
                      log_area_c + land_cover_name +
                      offset(area_offset) + (1 | kommune_factor),
                    data = pair_records_lc,
                    family = betabinomial)

# Save to file
save(lc_split, file = here::here("data", "models", "h4_LC_baseline_betabin.RData"))

# Run the presence model
lc_presence <- glmmTMB(presence ~ polygon_type * (log_area_c + land_cover_name) +
                         (1 | kommune_factor/pair_id_factor),
                       data   = presence_data_lc,
                       family = binomial)

# Save model output
save(lc_presence, file = here::here("data", "models", "h4_LC_baseline_presence.RData"))
saveRDS(list(share = share_estimate(lc_split),
             slope = slope_estimate(lc_split),
             presence_or = presence_or(lc_presence)),
        here("data", "models", "h4_LC_baseline_inference.rds"))

# 12. COMPARE RED-LISTED AND LEAST CONCERN BASELINE ----------------------------

# Assemble metrics side by side
row_from <- function(group, metric, v) {
  data.frame(group = group, metric = metric,
             estimate = unname(v["estimate"]),
             lower = unname(v["lower"]),
             upper = unname(v["upper"]))
}

comparison_h4ab <- bind_rows(row_from("Red-listed (CR/EN/VU/NT/DD)", "Polygon share", share_estimate(best_split)),
                             row_from("LC baseline", "Polygon share", share_estimate(lc_split)),
                             row_from("Red-listed (CR/EN/VU/NT/DD)", "Area slope (logit)", slope_estimate(best_split)),
                             row_from("LC baseline", "Area slope (logit)", slope_estimate(lc_split)),
                             row_from("Red-listed (CR/EN/VU/NT/DD)", "Presence OR (Dev/Buffer)", presence_or(best_presence)),
                             row_from("LC baseline", "Presence OR (Dev/Buffer)", presence_or(lc_presence))) |>
  mutate(across(c(estimate, lower, upper), \(x) round(x, 3))) |>
  arrange(metric, group)

# Print summary
print(comparison_h4ab)

# Save to file
write.csv(comparison_h4ab,
          here("figures", "Table_H4ab_redlisted_vs_LC_baseline.csv"),
          row.names = FALSE)

# 13. SUMMARY STATISTICS (RED-LISTED RECORDS) ----------------------------------

# % of polygons and buffers with ANY red-listed records
presence_summary_rl <- model_data_rl |>
  group_by(polygon_type) |>
  summarise(n_sides = n(),
            n_with_records = sum(n_occurrences > 0),
            pct_with_records = round(100 * mean(n_occurrences > 0), 1),
            .groups = "drop")
print(presence_summary_rl)

# Red-listed occurrence counts across ALL sides (zeros included)
occ_summary_all_rl <- model_data_rl |>
  group_by(polygon_type) |>
  summarise(mean = round(mean(n_occurrences), 2), median = median(n_occurrences),
            q25 = quantile(n_occurrences, 0.25), q75 = quantile(n_occurrences, 0.75),
            IQR = IQR(n_occurrences), max = max(n_occurrences), .groups = "drop")
print(occ_summary_all_rl)

# Red-listed occurrence counts for RECORD-BEARING sides only (n > 0)
occ_summary_nonzero_rl <- model_data_rl |>
  filter(n_occurrences > 0) |>
  group_by(polygon_type) |>
  summarise(n_sides = n(), mean = round(mean(n_occurrences), 2),
            median = median(n_occurrences), q25 = quantile(n_occurrences, 0.25),
            q75 = quantile(n_occurrences, 0.75), IQR = IQR(n_occurrences),
            max = max(n_occurrences), .groups = "drop")
print(occ_summary_nonzero_rl)

# Save the two most report-useful summaries
write.csv(presence_summary_rl,
          here("figures", "Table_H4_presence_summary_by_side_redlisted.csv"), row.names = FALSE)
write.csv(occ_summary_nonzero_rl,
          here("figures", "Table_H4_SOR_summary_by_side_redlisted.csv"), row.names = FALSE)

# END OF SCRIPT ----------------------------------------------------------------