##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 3.3_redlist_GBIF_planlagt
# This script contains code which creates harmonises the taxonomy of the
# Norwegian red list and adds redlisted species to planned development polygons
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source setup file
library(here)
source(here("scripts", "0_setup.R"))

# Load red list
redlist_raw <- read_excel(here("data", "raw_data", "redlist.xlsx"))

# Load polygon occurrence join (one row per occurrence, needed for SOR-based figures)
polygon_all_data <- readRDS(here("data", "derived_data",
                                 "polygons_occurrences_all_data.rds"))

# 2. PREPARE RED LIST ----------------------------------------------------------

## 2.1. Filter and cleam red list ----------------------------------------------

# Keep only mainland Norway assessments and relevant columns
redlist_clean <- redlist_raw |>
  filter(`Vurderingsområde` == "Norge") |>
  select(scientific_name  = `Vitenskapelig navn`,
         redlist_category = `Kategori 2021`) |>
  # filter to only threatened categories (CR, EN, VU, NT, LC, DD)
  filter(redlist_category %in% c("CR", "EN", "VU", "NT", "LC", "DD")) |>
  # remove any duplicate species entries keeping first occurrence
  distinct(scientific_name, .keep_all = TRUE)

# Check how many species there are
print(table(redlist_clean$redlist_category))

## 2.2. Harmonise species names with GBIF backbone -----------------------------

# Look up all red list species names against the GBIF backbone taxonomy
# Results are cached to file so the API call is only made once
if (file.exists(here("data", "derived_data", "redlist_backbone_lookup.rds"))) {
  cat("\nLoading cached backbone lookup...\n")
  backbone_lookup <- readRDS(here("data", "derived_data",
                                  "redlist_backbone_lookup.rds"))
} else {
  cat("\nLooking up", nrow(redlist_clean), "species names against GBIF backbone...\n")
  cat("This may take several minutes - results will be cached for future use.\n")
  
  # Split into batches of 1000 to avoid API rate limits
  batch_size <- 1000
  batches <- split(redlist_clean$scientific_name,
                         ceiling(seq_along(redlist_clean$scientific_name) / batch_size))
  backbone_results <- list()
  
  for (i in seq_along(batches)) {
    cat("Processing batch", i, "of", length(batches), "\n")
    backbone_results[[i]] <- name_backbone_checklist(
      data.frame(name = batches[[i]])
    )
    Sys.sleep(2) # pause between batches to avoid overwhelming the API
  }
  
  backbone_lookup <- do.call(rbind, backbone_results)
  
  # aave to file so this never needs to be rerun
  saveRDS(backbone_lookup,
          here("data", "derived_data", "redlist_backbone_lookup.rds"))
  cat("Backbone lookup saved to derived_data.\n")
}

# Join backbone results back to red list
redlist_harmonised <- redlist_clean |>
  mutate(gbif_species = backbone_lookup$species,
         match_type   = backbone_lookup$matchType) |>
  # keep original name where GBIF match is unavailable
  mutate(gbif_species = ifelse(is.na(gbif_species),
                               scientific_name, gbif_species))

# Check match quality
cat("\nMatch type breakdown:\n")
print(table(redlist_harmonised$match_type))