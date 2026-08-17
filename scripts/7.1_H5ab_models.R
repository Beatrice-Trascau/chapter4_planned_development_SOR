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




















