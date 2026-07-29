##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 3.5_create_buffers_add_landcover
# This script contains code to prepare the development polygons and buffers
# for analyses
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source setup file
library(here)
source(here("scripts", "0_setup.R"))

# Load development polygons
development_polygons <- st_read(here("data", "raw_data", "nina_planagt.gpkg"))

# Load pre-created buffers
polygon_buffers <- st_read(here("data", "derived_data", "NoAggPlanBufferNew.gpkg"))

# Load land cover data
gdb_path <- here("data", "raw_data", "Hovedokosystem_nedlasting", "Hovedokosystem.gdb")
land_cover <- st_read(gdb_path, layer = "Hovedøkosystem", quiet = TRUE)

# 2. CHECK THE POLYGONS, BUFFERS AND LAND-COVER DATA ---------------------------

## 2.1. Check required columns -------------------------------------------------

# Check if development polygons contains every column used downstream
setdiff(c("id", "arealformalsgruppe", "planlagt_areal_m2", "kommune",
          "kommunenummer"),
        colnames(development_polygons))
stopifnot(all(c("id", "arealformalsgruppe", "planlagt_areal_m2", "kommune",
                "kommunenummer") %in% colnames(development_polygons))) # all good!

# Check if the buffers contain the shared id column
stopifnot("id" %in% colnames(polygon_buffers)) # all good!

# Check if land-cover contains the ecotype column
stopifnot("ecotype" %in% colnames(land_cover)) # all good!

## 2.2. Check the id column ----------------------------------------------------

# Check that the ids are unique and complete in both datasets
stopifnot(anyDuplicated(development_polygons$id) == 0,
          anyDuplicated(polygon_buffers$id) == 0,
          !any(is.na(development_polygons$id)),
          !any(is.na(polygon_buffers$id)))

cat("Development polygons loaded:", nrow(development_polygons), "\n") # 133644
cat("Buffers loaded:            ", nrow(polygon_buffers), "\n") # 133643
cat("Land cover features loaded:", nrow(land_cover), "\n") # 1001965

## 2.3. Check geometries -------------------------------------------------------

# Check that there are no empty geometries (which would return NA in spatial joins)
cat("\nEmpty geometries - polygons:", sum(st_is_empty(development_polygons)),
    " buffers:", sum(st_is_empty(polygon_buffers)),
    " land cover:", sum(st_is_empty(land_cover)), "\n") # 0 empty geometries

stopifnot(!any(st_is_empty(development_polygons)),
          !any(st_is_empty(polygon_buffers)))

# Check if the geomtry types are polygonal
cat("\nGeometry types:\n")
print(table(st_geometry_type(development_polygons)))
print(table(st_geometry_type(polygon_buffers)))

# Check if there are any invalid geomtries (which could fail the st_join)
cat("\nInvalid geometries - polygons:", sum(!st_is_valid(development_polygons)), "\n") # 9 development polygons with invalid geometries, need to fix before moving on
cat("Invalid geometries - buffers: ", sum(!st_is_valid(polygon_buffers)), "\n") # 0

# Repair invalid geometries for the 9 development polygons flagged above
development_polygons <- st_make_valid(development_polygons)

# Make sure the repair worked
stopifnot(all(st_is_valid(development_polygons)))
cat("Invalid geometries after repair:", sum(!st_is_valid(development_polygons)), "\n") #0!

# Make sure that st_make_valid didn't chnage geometries for the polygons
table(st_geometry_type(development_polygons))

# Some multipolygons were changed to polygons in the st_make_valid() process
# it's expected but just want to check that the area is still correct
summary(as.numeric(st_area(development_polygons)) -
          as.numeric(development_polygons$planlagt_areal_m2)) # there may be some issue here
# check script 3.5_polygons_area_checks for details

# 3. HARMONISE CRS -------------------------------------------------------------

# Set CRS for everything else based on the development polygons (ETRS89/UTM33N)
cat("\nPolygon CRS:   ", st_crs(development_polygons)$input, "\n")
cat("Buffer CRS:    ", st_crs(polygon_buffers)$input, "\n")
cat("Land cover CRS:", st_crs(land_cover)$input, "\n")

# Check that all three objects have a defined CRS
stopifnot(!is.na(st_crs(development_polygons)),
          !is.na(st_crs(polygon_buffers)),
          !is.na(st_crs(land_cover)))

# Transform buffers to match the polygons if needed
if (st_crs(polygon_buffers) != st_crs(development_polygons)) {
  cat("\nTransforming buffers to match polygon CRS...\n")
  polygon_buffers <- st_transform(polygon_buffers, st_crs(development_polygons))
}

# Transform land-cover to match the polygons if needed
if (st_crs(land_cover) != st_crs(development_polygons)) {
  cat("Transforming land cover to match polygon CRS...\n")
  land_cover <- st_transform(land_cover, st_crs(development_polygons))
}

# Check that everything now shares one CRS
stopifnot(st_crs(development_polygons) == st_crs(polygon_buffers),
          st_crs(development_polygons) == st_crs(land_cover))
cat("\nPASS: all layers share the same CRS\n") # PASS

# Check that the transformation went well and all layers still overlap in space
print(rbind(polygons   = st_bbox(development_polygons),
            buffers    = st_bbox(polygon_buffers),
            land_cover = st_bbox(land_cover)))

if (length(st_intersection(st_as_sfc(st_bbox(development_polygons)),
                           st_as_sfc(st_bbox(land_cover)))) == 0) {
  stop("ERROR: polygon and land cover bounding boxes do not overlap")
} else {
  cat("\nPASS: polygon and land cover extents overlap\n")
} # PASS

# 4. PREPARE DEVELOPMENT POLYGONS ----------------------------------------------

# Drop Ports & Marinas and translate category names
development_polygons_temp <- development_polygons |>
  filter(arealformalsgruppe != "16 Havner og småbåthavner")

# Calculate area_m2_numeric from the geomtry (st_area) and not from planlagt_areal_m2
# see script 3.5_polygons_area_checks for details
development_polygons_temp <- development_polygons_temp |>
  mutate(area_m2_numeric         = as.numeric(st_area(development_polygons_temp)),
         planlagt_area_reference = as.numeric(planlagt_areal_m2),
         english_categories = case_when(arealformalsgruppe == "01 Bolig eller sentrumsformål" ~ "Residential",
                                        arealformalsgruppe == "02 Fritidsbebyggelse" ~ "Recreational",
                                        arealformalsgruppe == "03 Tjenesteyting" ~ "Services",
                                        arealformalsgruppe == "04 Handel" ~ "Retail",
                                        arealformalsgruppe == "05 Turistformål" ~ "Tourism",
                                        arealformalsgruppe == "06 Næringsvirksomhet" ~ "Commercial",
                                        arealformalsgruppe == "07 Råstoffutvinning" ~ "Mining",
                                        arealformalsgruppe == "08 Kombinerte formål" ~ "Combined",
                                        arealformalsgruppe == "13 Forsvaret" ~ "Defense"))

# Check how many polygons are left after removing the ports and marinas
cat("\nPolygons after removing Ports & Marinas:", nrow(development_polygons_temp), "\n") # 131023

# Check that every category was translated
if (any(is.na(development_polygons_temp$english_categories))) {
  cat("FAIL: untranslated categories present:\n")
  print(unique(development_polygons_temp$arealformalsgruppe[is.na(development_polygons_temp$english_categories)]))
} else {
  cat("PASS: all development categories translated\n")
} # All good!

# Check that the area was converted cleanly and can be used later on a log scale
cat("Areas that failed to convert to numeric:",
    sum(is.na(development_polygons_temp$area_m2_numeric)), "\n") #0
cat("Areas of zero or less (log would be -Inf/NaN):",
    sum(development_polygons_temp$area_m2_numeric <= 0, na.rm = TRUE), "\n") #0

# 5. GET LAND-COVER DATA FOR POLYGONS AND BUFFERS ------------------------------

## 5.1. Prepare land-cover data ------------------------------------------------

# Add category names to the land-cover so that you can understand them
  # based on: https://nva.sikt.no/registration/0198cc623366-a2a951d5-8763-4125-8cdb-86885c44f5c5
land_cover <- land_cover |>
  mutate(land_cover_name = case_when(ecotype == 1 ~ "Settlements",
                                     ecotype == 2 ~ "Cropland",
                                     ecotype == 3 ~ "Grassland",
                                     ecotype == 4 ~ "Forest",
                                     ecotype == 5 ~ "Heathland",
                                     ecotype == 6 ~ "Sparsely_vegetated",
                                     ecotype == 7 ~ "Wetlands",
                                     ecotype == 8 ~ "Rivers",
                                     ecotype == 9 ~ "Lakes",
                                     ecotype == 10 ~ "Marine_inlets",
                                     ecotype == 11 ~ "Coastal",
                                     ecotype == 12 ~ "Marine_offshore",
                                     TRUE ~ "Unknown"))

# Check that every ecotype was recognised
cat("\nLand cover classes found:\n")
print(table(land_cover$land_cover_name))
if (any(land_cover$land_cover_name == "Unknown")) {
  cat("FAIL: unrecognised ecotype codes:\n")
  print(unique(land_cover$ecotype[land_cover$land_cover_name == "Unknown"]))
} else {
  cat("PASS: all ecotype codes recognised\n")
}

# Keep only terrestrial land-covers (1-7)
land_cover_terrestrial <- land_cover |>
  filter(ecotype %in% 1:7)

# Keep the water classes separately (8-9 freshwater, 10-12 marine)
# will be used later to check why a polygon doesn't have a terrestrial land-cover
land_cover_water <- land_cover |>
  filter(ecotype %in% 8:12)

# Check that neither subset is empty
stopifnot(nrow(land_cover_terrestrial) > 0,
          nrow(land_cover_water) > 0)
cat("\nTerrestrial land cover features:", nrow(land_cover_terrestrial), "\n") #613591
cat("Water land cover features:      ", nrow(land_cover_water), "\n") #388374
 
## 5.2. Extract dominant land-cover for polygons -------------------------------

# Define a function to extract land-cover for each polygon
# For each polygon, st_join with largest = TRUE keeps the land cover class covering the largest area
# Polygons with no land-cover overlap return NA
# Will be processed in batches to spare some of the memory
# Function stops if either the input or outpus is wrong
extract_dominant_landcover <- function(polygons, land_cover_data, batch_size = 5000) {
  
  # Check that the inputs are sf objects sharing one CRS
  stopifnot(inherits(polygons, "sf"),
            inherits(land_cover_data, "sf"),
            st_crs(polygons) == st_crs(land_cover_data))
  
  # Check that the id column is present, unique and complete
  stopifnot("id" %in% names(polygons),
            anyDuplicated(polygons$id) == 0,
            !any(is.na(polygons$id)))
  
  # Check that the land-cover carries the columns being transferred
  stopifnot(all(c("land_cover_name", "ecotype") %in% names(land_cover_data)))
  
  n_batches <- ceiling(nrow(polygons) / batch_size)
  results   <- vector("list", n_batches)
  
  for (i in seq_len(n_batches)) {
    
    cat("    Processing batch", i, "of", n_batches, "\n")
    
    start_idx <- (i - 1) * batch_size + 1
    end_idx   <- min(i * batch_size, nrow(polygons))
    
    results[[i]] <- polygons[start_idx:end_idx, ] |>
      dplyr::select(id) |>
      st_join(land_cover_data |> dplyr::select(land_cover_name, ecotype),
              join    = st_intersects,
              largest = TRUE) |>
      st_drop_geometry()
    
    gc()
  }
  
  out <- bind_rows(results)
  
  # Check that there is exactly one row returned per polygon (should be ensured by largest = TRUE)
  if (nrow(out) != nrow(polygons)) {
    stop("ERROR: land cover join returned ", nrow(out), " rows for ",
         nrow(polygons), " polygons")
  }
  if (anyDuplicated(out$id) > 0) {
    stop("ERROR: land cover join produced duplicated ids")
  }
  
  cat("  Polygons with no land cover match:", sum(is.na(out$land_cover_name)),
      "of", nrow(out), "\n")
  
  return(out)
}

# Extract dominant terrestrial land-cover
cat("\nExtracting dominant terrestrial land cover...\n")
polygon_landcover <- extract_dominant_landcover(development_polygons_temp,
                                                land_cover_terrestrial)

# Join land-cover back to the polygons
development_polygons_temp <- development_polygons_temp |>
  left_join(polygon_landcover, by = "id")

# Check the join added columns without adding rows
stopifnot(nrow(development_polygons_temp) == nrow(polygon_landcover))

## 5.3. Check for polygons entirely in water -----------------------------------

# Check the polygons with NA for land-cover (which do not overlap with any of the terrestrial classes)
cat("\nPolygons with no terrestrial land cover:",
    sum(is.na(development_polygons_temp$land_cover_name)), "\n") #1141

# Check which land-covers the polygons with NA actually intersect with
water_only_polygons <- extract_dominant_landcover(
  development_polygons_temp |> filter(is.na(land_cover_name)),
  land_cover_water) |>
  mutate(water_type = case_when(ecotype %in% 10:12 ~ "Sea",
                                ecotype %in% 8:9   ~ "Freshwater",
                                TRUE ~ "Unclassified"))

# Check how many are marine vs freshwater
print(table(water_only_polygons$water_type, useNA = "ifany"))
# Freshwater = 73    Sea = 1068

# Get a detailed breakdown by land-cover class
print(table(water_only_polygons$land_cover_name, useNA = "ifany"))
# Coastal           Lakes Marine_offshore          Rivers 
# 597              59             471              14 

# Check if there are any polygons that fall outside of terrestrial and marine/aquatic and if they fall outside of the land-cover dataset entirely
if (any(water_only_polygons$water_type == "Unclassified")) {
  cat("\nFAIL:", sum(water_only_polygons$water_type == "Unclassified"),
      "polygons match no land cover class at all - check their location\n")
} else {
  cat("\nPASS: every water-only polygon was classified as sea or freshwater\n")
}

# Save the diagnosis for later reporting in the methods
saveRDS(water_only_polygons,
        here("data", "derived_data", "polygons_removed_water_only.rds"))

# Remove polygons with no terrestrial land-cover
development_polygons_filtered <- development_polygons_temp |>
  filter(!is.na(land_cover_name)) |>
  mutate(polygon_type = "Development",
         pair_id      = id)

# Check how many we are left with after filtering
cat("\nPolygons retained after removing water-only polygons:",
    nrow(development_polygons_filtered), "\n") #129882 

# And check that the numbers are correct
stopifnot(nrow(development_polygons_filtered) + nrow(water_only_polygons) ==
            nrow(development_polygons_temp))
cat("PASS: retained + removed = total polygons\n")

# 6. MATCH BUFFERS TO POLYGONS -------------------------------------------------

## 6.1. Keep only the buffers with a matching polygon --------------------------

# Remove buffers whose id is not in the retained development polygons
polygon_buffers_filtered <- polygon_buffers |>
  filter(id %in% development_polygons_filtered$id)

# Check how many buffers were removed and how many were retained
cat("\nBuffers removed (no matching polygon):",
    nrow(polygon_buffers) - nrow(polygon_buffers_filtered), "\n") #3762
cat("Buffers retained:", nrow(polygon_buffers_filtered), "\n") #129881

## 6.2. Keep only polygons that still have a buffer ----------------------------

# Drop the polygons that did not have a buffer created (Ivar reported 1)
cat("Polygons dropped because they have no buffer:",
    sum(!development_polygons_filtered$id %in% polygon_buffers_filtered$id), "\n") #1
development_polygons_filtered <- development_polygons_filtered |>
  filter(id %in% polygon_buffers_filtered$id)

# Check that the two id sets are identical 
stopifnot(setequal(development_polygons_filtered$id, polygon_buffers_filtered$id))
cat("PASS: polygons and buffers have identical id sets\n")
cat("Final number of pairs:", nrow(development_polygons_filtered), "\n") # 129881

## 6.3. Transfer polygons metadata and land-cover to the buffers ---------------

# Buffers will inherit the category, municipality, and land-cover category from their paired polygon
polygon_buffers_filtered <- polygon_buffers_filtered |>
  left_join(development_polygons_filtered |>
              st_drop_geometry() |>
              dplyr::select(id, arealformalsgruppe, english_categories,
                            kommunenummer, kommune, land_cover_name, ecotype),
            by = "id") |>
  mutate(polygon_type = "Buffer",
         pair_id      = id,
         # buffers have no NINA-recorded area; reference column is NA for them
         planlagt_area_reference = NA_real_)

# Calculate buffer are from the geometry
# (st_area() on the sf object uses the active geometry column, whatever it is called)
polygon_buffers_filtered$area_m2_numeric <- as.numeric(st_area(polygon_buffers_filtered))

# Check that the join transferred the metadata without duplicating the rows
stopifnot(anyDuplicated(polygon_buffers_filtered$id) == 0)
cat("\nBuffers without land cover after join:",
    sum(is.na(polygon_buffers_filtered$land_cover_name)), "\n") #0

# Check that the buffer areas are usable
cat("Buffers with zero or negative area:",
    sum(polygon_buffers_filtered$area_m2_numeric <= 0), "\n") #0

# 7. COMBINE POLYGONS AND BUFFERS INTO SINGLE OBJECTS --------------------------

# Keep matching columns in both objects 
# Select the columns we want to keep in the polygons
polygon_out <- development_polygons_filtered |>
  dplyr::select(id, pair_id, polygon_type, area_m2_numeric,
                planlagt_area_reference,
                arealformalsgruppe, english_categories,
                kommunenummer, kommune, land_cover_name, ecotype)

# Select the columns we want to keep in the buffers
buffer_out <- polygon_buffers_filtered |>
  dplyr::select(id, pair_id, polygon_type, area_m2_numeric,
                planlagt_area_reference,
                arealformalsgruppe, english_categories,
                kommunenummer.x, kommune, land_cover_name, ecotype) |>
  # there are two columns in  polygon_buffers_filtered: kommunenummer.x and kommunenummer.y
  # identical(polygon_buffers_filtered$kommunenummer.x, polygon_buffers_filtered$kommunenummer.y) gives TRUE
  # rename kommunenummer.x to just have kommunenummer 
  dplyr::rename(kommunenummer = kommunenummer.x)

# Rename geometry columns of both buffers and polygons before biding (rbind on sf object needs the same name)
names(polygon_out)[names(polygon_out) == attr(polygon_out, "sf_column")] <- "geometry"
st_geometry(polygon_out) <- "geometry"
names(buffer_out)[names(buffer_out) == attr(buffer_out, "sf_column")] <- "geometry"
st_geometry(buffer_out) <- "geometry"

# Check that the identical columns are in order and have the same CRS
stopifnot(identical(names(polygon_out), names(buffer_out)),
          st_crs(polygon_out) == st_crs(buffer_out)) 
cat("\nPASS: polygon and buffer objects are structurally identical\n") # PASS

# Bind datasets and add variables for later models 
polygon_buffer_data <- rbind(polygon_out, buffer_out) |>
  mutate(polygon_type = factor(polygon_type, levels = c("Buffer", "Development")),
         land_cover_name = factor(land_cover_name),
         kommune_factor = factor(kommune),
         pair_id_factor = factor(pair_id),
         log_area = log(area_m2_numeric))

# Check that no rows were gained or lost in the bind
stopifnot(nrow(polygon_buffer_data) == nrow(polygon_out) + nrow(buffer_out))
cat("\nFinal dataset:", nrow(polygon_buffer_data), "rows\n") # 259762
print(table(polygon_buffer_data$polygon_type))
# Buffer Development 
# 129881      129881

# 8. FINAL CHECKS --------------------------------------------------------------

# Check the pairing 
pair_counts <- polygon_buffer_data |>
  st_drop_geometry() |>
  group_by(pair_id) |>
  summarise(n_rows = n(),
            n_dev = sum(polygon_type == "Development"),
            n_buf = sum(polygon_type == "Buffer"),
            .groups = "drop")

if (all(pair_counts$n_rows == 2) &&
    all(pair_counts$n_dev == 1 & pair_counts$n_buf == 1)) {
  cat("\nPASS: every pair_id has exactly 1 Development + 1 Buffer\n")
} else {
  cat("\nFAIL:", nrow(pair_counts |> filter(n_rows != 2 | n_dev != 1 | n_buf != 1)),
      "pairs have incorrect composition\n")
} # PASS

# Check that land-cover is matching within pairs (i.e. both the polygons and buffers within a pair have the same land-cover category)
land_cover_check <- polygon_buffer_data |>
  st_drop_geometry() |>
  dplyr::select(pair_id, polygon_type, land_cover_name) |>
  tidyr::pivot_wider(names_from = polygon_type, values_from = land_cover_name) |>
  mutate(land_cover_match = Development == Buffer)

if (all(land_cover_check$land_cover_match, na.rm = TRUE)) {
  cat("PASS: all buffers share the land cover of their paired polygon\n")
} else {
  cat("FAIL:", sum(!land_cover_check$land_cover_match, na.rm = TRUE),
      "pairs have mismatched land cover\n")
} # PASS

# Check for missing values
print(colSums(is.na(st_drop_geometry(polygon_buffer_data)))) #planlagt_area_reference does not have values for any of the rows but it's ok

# Check that log-area values are finite everywhere
cat("\nNon-finite log_area values:",
    sum(!is.finite(polygon_buffer_data$log_area)), "\n") #Non-finite log_area values: 0 
stopifnot(all(is.finite(polygon_buffer_data$log_area)))

# Inspect range of log_area values going into the models later
# small polygons may give large-negative log_area values
cat("\nlog_area summary (this is what enters the GLMMs):\n")
print(summary(polygon_buffer_data$log_area))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.011   7.195   7.914   8.193   8.857  16.700 

# Check the buffer:polygon ration on the final paired object
area_ratio_check <- polygon_buffer_data |>
  st_drop_geometry() |>
  dplyr::select(pair_id, polygon_type, area_m2_numeric) |>
  tidyr::pivot_wider(names_from  = polygon_type,
                     values_from = area_m2_numeric) |>
  mutate(buffer_polygon_ratio = Buffer / Development)

cat("\nBuffer / Development area ratio on final paired object:\n")
print(summary(area_ratio_check$buffer_polygon_ratio))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.018   1.044   1.059   1.086  10.889 

# Flag if the median has drifted away from ~1 (might mean that the pairing has gone wrong)
if (abs(median(area_ratio_check$buffer_polygon_ratio, na.rm = TRUE) - 1) > 0.25) {
  cat("WARNING: median buffer/polygon ratio is far from 1 - check pairing\n")
} else {
  cat("PASS: median buffer/polygon area ratio is near 1\n")
} # PASS

# Check distributions
cat("\nLand cover distribution:\n")
print(table(polygon_buffer_data$land_cover_name,
            polygon_buffer_data$polygon_type))
#                     Buffer   Development
# Cropland            10036       10036
# Forest              89381       89381
# Grassland            2473        2473
# Heathland            8581        8581
# Settlements         14734       14734
# Sparsely_vegetated   1903        1903
# Wetlands             2773        2773

cat("\nDevelopment category distribution:\n")
print(table(polygon_buffer_data$english_categories,
            polygon_buffer_data$polygon_type))
#                Buffer Development
# Combined       4266        4266
# Commercial     9662        9662
# Defense          25          25
# Mining         4436        4436
# Recreational  55702       55702
# Residential   44009       44009
# Retail          900         900
# Services       5568        5568
# Tourism        5313        5313

cat("\nMunicipalities represented:",
    n_distinct(polygon_buffer_data$kommune_factor), "\n") #353

# Final list of how many polygons we lost along the way and where
cat("\n--- POLYGON ACCOUNTING ---\n")
cat("Loaded from file:", nrow(development_polygons), "\n") #133644 
cat("After removing Ports:", nrow(development_polygons_temp), "\n") #131023 
cat("Removed as water-only:", nrow(water_only_polygons), "\n") # 1141 
cat("Removed for having no buffer:",
    nrow(development_polygons_temp) - nrow(water_only_polygons) -
      nrow(development_polygons_filtered), "\n") #1
cat("Final paired polygons: ", nrow(development_polygons_filtered), "\n") #129881 

# Save final paired polygon-buffer object
saveRDS(polygon_buffer_data,
        here("data", "derived_data", "polygon_buffer_data.rds"))

# Check that the file was written and reads back with the expected dimensions
stopifnot(file.exists(here("data", "derived_data", "polygon_buffer_data.rds")))
stopifnot(identical(dim(readRDS(here("data", "derived_data",
                                     "polygon_buffer_data.rds"))),
                    dim(polygon_buffer_data)))

# END OF SCRIPT ----------------------------------------------------------------