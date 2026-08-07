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

# Create a list of the categories we are keeping
redlist_categories <- c("CR", "EN", "VU", "NT", "DD", "LC")

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

## 2.2. Resolve exact matches --------------------------------------------------

# Keep the exact matches
redlist_exact <- redlist_harmonised |>
  filter(match_type == "EXACT") |>
  transmute(species = clean_name(gbif_species),
            redlist_category = redlist_category)

# Check how many rows had an exact match
cat("EXACT-match red list species:", nrow(redlist_exact), "\n")

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
cat("  rows total:                       ", nrow(manual_resolved), "\n")
cat("  Accepted = Y kept:                ",
    sum(manual_resolved$accepted == "Y"), "\n")
cat("  Accepted = N with New Name kept:  ",
    sum(manual_resolved$accepted == "N" & !is.na(manual_resolved$resolved)), "\n")
cat("  dropped ('remove' or blank):      ",
    sum(is.na(manual_resolved$resolved)), "\n")
cat("  Accepted = Y rows where New Name overrode GBIF name:\n")
print(manual_resolved |>
        filter(accepted == "Y", resolved == new_clean, new_clean != gbif_clean) |>
        select(gbif_clean, new_clean))

# Keep only the resolved rows
manual_resolved <- manual_resolved |>
  filter(!is.na(resolved)) |>
  transmute(species = resolved, redlist_category)

# Check how many rows are left
cat("Accepted manually-resolved species:", nrow(manual_resolved), "\n")

## 2.4. Combine into single df -------------------------------------------------

# Combine the resolved list with the red-list
redlist_final <- bind_rows(redlist_exact, manual_resolved) |>
  filter(!is.na(species), nzchar(species)) |>
  # order most-threatened first, then keep one row per species (most severe category wins in the case of species with multiple categorisations)
  mutate(redlist_category = factor(redlist_category, levels = severity_order)) |>
  arrange(species, redlist_category) |>
  distinct(species, .keep_all = TRUE) |>
  mutate(redlist_category = as.character(redlist_category)) |>
  # safety filter: keep only the categories we retain (drops any stray values)
  filter(redlist_category %in% redlist_categories)

# Check how many species are in each category 
print(table(redlist_final$redlist_category))

# Save the resolved red list to use in 6.2. and 6.3
saveRDS(redlist_final,
        here("data", "derived_data", "redlist_harmonised_final.rds"))

# 3. BUILD THE REDLISTED RECORDS PER DATASET -----------------------------------

# Get a unique list of the species in the red-list
redlist_species <- unique(redlist_final$species)

## 3.1. Flag and keep red-listed occurrences -----------------------------------

# Attach the category 
occ_join_rl <- occ_join |>
  filter(!is.na(gbifID)) |>
  mutate(species = clean_name(species)) |>
  inner_join(redlist_final, by = "species")   # keeps only red-list-assessed occ.

# Check hoe many records are redlisted
cat("\nRed-listed occurrence records:", nrow(occ_join_rl),
    "of", sum(!is.na(occ_join$gbifID)), "matched records\n")
cat("Distinct red-listed species observed in the data:",
    n_distinct(occ_join_rl$species), "of", length(redlist_species),
    "on the list\n")
cat("Red-listed occurrences by category:\n")
print(table(occ_join_rl$redlist_category))

# Save the red-listed occurrence-level join for H4d
saveRDS(occ_join_rl,
        here("data", "derived_data", "h4_polygon_buffer_occurrence_join.rds"))

## 3.2. Recompute per-side red-listed counts

# Count red-listed SOR and red-listed species per side
redlist_counts <- occ_join_rl |>
  group_by(poly_uid) |>
  summarise(sor_redlist = n(),
            n_species_redlist = n_distinct(species),
            .groups = "drop")

# Attach to the fulside frame so sides with no red-listed records become 0
model_data_rl <- model_data |>
  select(poly_uid, id, pair_id, polygon_type, area_m2_numeric,
         english_categories, kommune, land_cover_name, log_area) |>
  left_join(redlist_counts, by = "poly_uid") |>
  mutate(n_occurrences = coalesce(sor_redlist, 0L),
         n_species = coalesce(n_species_redlist, 0L),
         polygon_type = factor(polygon_type, levels = c("Buffer", "Development")),
         land_cover_name = factor(land_cover_name),
         kommune_factor = factor(kommune),
         pair_id_factor = factor(pair_id)) |>
  select(-sor_redlist, -n_species_redlist)

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
cat("\nPASS: red-listed side data assembled, pairing intact\n")

# Quickly inspect the data
cat("\nMean red-listed SOR by side:\n")
print(tapply(model_data_rl$n_occurrences, model_data_rl$polygon_type, mean))
cat("Proportion of sides with zero red-listed records:",
    round(mean(model_data_rl$n_occurrences == 0), 3), "\n")

# Save the red-listed per-side dataset
saveRDS(model_data_rl,
        here("data", "derived_data", "h4_polygon_buffer_data.rds"))

# 4. PREPARE DATA FOR MODELLING- -----------------------------------------------

## 4.1. Reshape df to one row per polygon-buffer pair --------------------------

# Pivot to wide to get the polygon and buffer counts side by side
pair_data <- model_data_rl |>
  select(pair_id, kommune, english_categories, land_cover_name,
         area_m2_numeric, polygon_type, n_occurrences) |>
  tidyr::pivot_wider(names_from  = polygon_type,
                     values_from = c(n_occurrences, area_m2_numeric)) |>
  rename(sor_polygon = n_occurrences_Development,
         sor_buffer = n_occurrences_Buffer,
         area_polygon = area_m2_numeric_Development,
         area_buffer = area_m2_numeric_Buffer)

# Create the variables needed for the model
pair_data <- pair_data |>
  mutate(sor_total = sor_polygon + sor_buffer,
         # share of the red-listed SOR belonging to the polygon
         share_polygon = ifelse(sor_total > 0, sor_polygon / sor_total, NA_real_),
         # centred log polygon area so the intercept in H4a refers to an average pair
         log_area_c = as.numeric(scale(log(area_polygon), scale = FALSE)),
         # area offset to adjust the share for the polygon/buffer area difference
         area_offset = log(area_polygon / area_buffer),
         # did this pair have any red-listed records at all? (presence response)
         any_records = as.integer(sor_total > 0),
         # factorise kommune and land-cover name
         kommune_factor  = factor(kommune),
         land_cover_name = factor(land_cover_name))

# Build the presence data and flag the presence of any red-listed record
presence_data <- model_data_rl |>
  mutate(presence = as.integer(n_occurrences > 0),
         polygon_type = factor(polygon_type, levels = c("Buffer", "Development")),
         # centred log area of THIS side (polygon or buffer)
         log_area_c = as.numeric(scale(log(area_m2_numeric), scale = FALSE)),
         land_cover_name = factor(land_cover_name),
         kommune_factor  = factor(kommune),
         pair_id_factor  = factor(pair_id))

## 4.2. Check the reshaped df --------------------------------------------------

# Check that there is exactly one row per pair
stopifnot(nrow(pair_data) == n_distinct(model_data_rl$pair_id))
cat("\nPairs after reshape:", nrow(pair_data), "\n")

## Check that counts are complete and offset/area are finite everywhere
stopifnot(!any(is.na(pair_data$sor_polygon)),
          !any(is.na(pair_data$sor_buffer)),
          all(is.finite(pair_data$area_offset)),
          all(is.finite(pair_data$log_area_c)))
cat("PASS: counts complete and offset/area finite\n")

# Checkt the response
cat("Pairs with any red-listed records:", sum(pair_data$any_records), "of",
    nrow(pair_data), "(",
    round(100 * mean(pair_data$any_records), 1), "%)\n")
cat("Pairs with zero red-listed records in BOTH halves:",
    sum(pair_data$sor_total == 0), "\n")
cat("Polygon share of red-listed records (record-bearing pairs only):\n")
print(summary(pair_data$share_polygon))