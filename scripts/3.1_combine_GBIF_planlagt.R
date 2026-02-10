##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 3.1_combine_GBIF_planlagt
# This script contains code which adds the species occurrence records to the
# polygons of planned development
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source setup file
library(here)
source(here("scripts", "0_setup.R"))

# Load development polygons
development_polygons <- st_read(here("data", "raw_data", "nina_planagt.gpkg"))

# Load cleaned occurrence records
clean_occurrences_15km <- read.csv(here("data", "derived_data",
                                        "clean_occurrences_15km.txt"))

# 2. JOIN OCCURRENCES TO POLYGONS ----------------------------------------------

## 2.1. Prepare spatial data ---------------------------------------------------

# Convert occurrences to spatial points
occurrences_sf <- clean_occurrences_15km |>
  # remove occurrences with NA for either longitude or latitude
  filter(!is.na(decimalLongitude),
         !is.na(decimalLatitude)) |>
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = 4326)

# Transform occurrences to match the CRS of the polygons
occurrences_sf_crs <- st_transform(occurrences_sf, st_crs(development_polygons))

# Check that the CRSs match
stopifnot(st_crs(development_polygons) == st_crs(occurrences_sf_crs))

## 2.2. Spatial join -----------------------------------------------------------

# Add unique polygon ID to use as grouping factor after the join
development_polygons <- development_polygons |>
  mutate(polygon_id = row_number())

# Spatial join occurrences to polygon
# each row in the result = one occurrence within one polygon
# left = TRUE retains polygons with no occurrences (as rows with NA occurrence columns)
polygon_occurrence_join <- st_join(development_polygons, occurrences_sf_crs,
                                   join = st_intersects,
                                   left = TRUE)

## 2.3. Convert to one big dataframe with all the information needed -----------

# Convert planlagt_areal_m2 from character to numeric and translate category names
# before summarising (as in 1_2_planlagt_exploration.R)
polygon_occurrence_join <- polygon_occurrence_join |>
  mutate(area_m2_numeric    = as.numeric(planlagt_areal_m2),
         english_categories = case_when(arealformalsgruppe == "01 Bolig eller sentrumsformål" ~ "Residential",
                                        arealformalsgruppe == "02 Fritidsbebyggelse"          ~ "Recreational",
                                        arealformalsgruppe == "03 Tjenesteyting"              ~ "Services",
                                        arealformalsgruppe == "04 Handel"                     ~ "Retail",
                                        arealformalsgruppe == "05 Turistformål"               ~ "Tourism",
                                        arealformalsgruppe == "06 Næringsvirksomhet"          ~ "Commercial",
                                        arealformalsgruppe == "07 Råstoffutvinning"           ~ "Mining",
                                        arealformalsgruppe == "08 Kombinerte formål"          ~ "Combined",
                                        arealformalsgruppe == "13 Forsvaret"                  ~ "Defense",
                                        arealformalsgruppe == "16 Havner og småbåthavner"     ~ "Ports"))

# Convert dataframe to have 1 row per polygon 
# retains key polygon metadata, occurrence counts, species counts and species identities
polygon_all_data <- polygon_occurrence_join |>
  st_drop_geometry() |>
  group_by(polygon_id,
           arealformalsgruppe,
           english_categories,
           kommunenummer,
           kommune,
           planlagt_areal_m2,
           area_m2_numeric) |>
  summarise(n_occurrences = sum(!is.na(gbifID)), # count non-NA rows = occurrences
            n_species     = n_distinct(species[!is.na(species)]),   # number of unique species
            species_list  = list(unique(species[!is.na(species)])), # identity of species present
            kingdoms      = list(unique(kingdom[!is.na(kingdom)])), # kingdoms represented
            .groups = "drop")

# Quick check that it makes sense
cat("Total polygons in master df:  ", nrow(polygon_all_data), "\n")
cat("Polygons with occurrences:    ", sum(polygon_all_data$n_occurrences > 0), "\n")
cat("Polygons without occurrences: ", sum(polygon_all_data$n_occurrences == 0), "\n")

# Save df to file
saveRDS(polygon_all_data,
        here("data", "derived_data", "polygons_occurrences_all_data.rds"))

