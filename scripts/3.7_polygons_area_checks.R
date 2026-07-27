##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 3.7_polygons_area_checks
# This script contains code to check the area of the development polygons
##----------------------------------------------------------------------------##

# 1. SETUP ---------------------------------------------------------------------

# Load setup file
library(here)
source(here("scripts", "0_setup.R"))

# Load the development polygons and their buffers from Ivar
development_polygons <- st_read(here("data", "raw_data", "nina_planagt.gpkg"))
polygon_buffers      <- st_read(here("data", "derived_data", "NoAggPlanBufferNew.gpkg"))


# 2. GEOMETRY VALIDITY ---------------------------------------------------------

# After running st_make_valid() in script 3.5, 9 polygons had inavlid geometires
# which can cause errors in the st_join

# Count the invalid geometries in the raw data
cat("Invalid geometries (raw polygons):", sum(!st_is_valid(development_polygons)), "\n")
cat("Invalid geometries (buffers):     ", sum(!st_is_valid(polygon_buffers)), "\n")

# Repair the 9 invalid polygons
development_polygons <- st_make_valid(development_polygons)

# Double check that st_make_valid worked
print(table(st_geometry_type(development_polygons)))
# the type changed from multipolygon to polygon for some of the records
# i.e. st_make_valid converted single-part multipolygons to plain polygons which is not a problem
# since it does not affect extent or area

# 3. GEOMTRY AREA VS RECORDED AREA ---------------------------------------------

# planlagt_areal_m2 is stored as character in the source and was originally used
# as the model area. Comparing it against the geometry footprint revealed a
# large, systematic discrepancy.

# Extract absolute difference between the geometry area and the recorded area
area_diff <- as.numeric(st_area(development_polygons)) -
  as.numeric(development_polygons$planlagt_areal_m2)

cat("\nSummary of (geometry area - recorded area):\n")
print(summary(area_diff))

# Output:
#        Min.    1st Qu.     Median       Mean    3rd Qu.       Max.
#  -7450496.9    -4791.3     -645.9    -7479.0       -0.3    35011.2
# This shows a median near 0 but a long negative tail and one large positive outlier
# There appear to be some extreme rows which means that we need a relative measure to make sure
# When checking this difference on the un-repaired polygons, the difference is identical to this
# THis means that the repair is not causing this issue in the data

# 4. RELATIVE CHECK OF DISCREPANCY ---------------------------------------------

# Calculate relative differences
rel <- abs(as.numeric(st_area(development_polygons)) -
             as.numeric(development_polygons$planlagt_areal_m2)) /
  as.numeric(development_polygons$planlagt_areal_m2)

# Check how many polygons differ by 10% and 50%
cat("\nPolygons where geometry differs from recorded by >10%:",
    sum(rel > 0.10, na.rm = TRUE), "\n")
cat("Polygons where geometry differs from recorded by >50%:",
    sum(rel > 0.50, na.rm = TRUE), "\n")
cat("\nSummary of relative difference:\n")
print(summary(rel))

#   >10% different: 69552 polygons
#   >50% different: 38793 polygons
#   Median relative difference 0.125, mean 0.294, max 35.0
# The area values seem to disagree across most of the polygons in the dataset

# 5. CHECK WHY -----------------------------------------------------------------

# planlagt_areal_m2 should be continuous if it was a real per-polygon measurement
# But we have many identical values which suggestst that there may have been a template or
# or a plan figure attached to many polygons at once

# Check the most frequent recoded-area values
print(development_polygons |>
        st_drop_geometry() |>
        count(planlagt_areal_m2, sort = TRUE) |>
        head(10))

# Top values are round numbers near 1000m^2 shared by a dozens of polygons
# This suggests that there is some plot-level/plan-level attribute and not a real per-polygon measurement

# 6. WHICH AREA VALUE WERE THE BUFFERS BUILT FROM? -----------------------------

# Compare buffer geometry area to BOTH polygon areas to see which was used when building the buffers
buf_area  <- as.numeric(st_area(polygon_buffers))
poly_geom <- as.numeric(st_area(development_polygons))[match(polygon_buffers$id,
                                                             development_polygons$id)]
poly_rec  <- as.numeric(development_polygons$planlagt_areal_m2)[match(polygon_buffers$id,
                                                                      development_polygons$id)]
cat("\nMedian buffer / polygon-GEOMETRY-area ratio:",
    round(median(buf_area / poly_geom, na.rm = TRUE), 2), "\n")
cat("Median buffer / RECORDED-area ratio:        ",
    round(median(buf_area / poly_rec,  na.rm = TRUE), 2), "\n")

#   buffer / geometry-area ratio = 1.04  (tight, just above 1 as expected for a
#                                         buffer built slightly larger by design)
#   buffer / recorded-area ratio = 0.92  (looser, wrong side of 1)
# This means that the buffers were built from the geometry, not from the planlagt_areal_m2
# So using planlagt_areal_m2 in the models would mean that buffers and polygons use different definitions of area

# 7. CHECK THE POSITIVE OUTLIERS -----------------------------------------------

# Most of the discrepancies are negative, but there are some positive ones (even 35x)
# Check if these are geometry errors or a structural feature
# Extract the positive difference rows where the geometry is larger than recorded
print(development_polygons |>
        st_drop_geometry() |>
        mutate(area_geom  = as.numeric(st_area(development_polygons)),
               area_diff  = area_geom - as.numeric(planlagt_areal_m2),
               area_ratio = area_geom / as.numeric(planlagt_areal_m2)) |>
        filter(area_diff > 1e4) |>
        dplyr::select(id, arealformalsgruppe, planlagt_areal_m2, area_geom,
                      area_diff, area_ratio) |>
        arrange(desc(area_diff)))

# Count the parts of each geometry
development_polygons$n_parts <- sapply(st_geometry(development_polygons),
                                       function(g) if (inherits(g, "MULTIPOLYGON")) length(g) else 1L)

# List part counts for the most extreme ratio cases
worst_ids <- development_polygons |>
  st_drop_geometry() |>
  mutate(area_ratio = as.numeric(st_area(development_polygons)) /
           as.numeric(planlagt_areal_m2)) |>
  filter(area_ratio > 5) |>
  pull(id)
cat("\nPart counts for extreme-ratio polygons:\n")
print(development_polygons |>
        filter(id %in% worst_ids) |>
        st_drop_geometry() |>
        dplyr::select(id, arealformalsgruppe, n_parts) |>
        arrange(desc(n_parts)))

# The positive tail is almost entirely "02 Fritidsbebyggelse" (Recreational /
#   holiday cabins). The area ratio tracks the part count almost 1:1 - e.g.
#   id 243640 is 36x with 36 parts, id 77935 is 35x with 35 parts, id 101353 is
#   26x with 26 parts. These are MULTIPOLYGONs bundling many separate cabin
#   plots into one record, where planlagt_areal_m2 holds the area of ONE ~1000
#   m2 plot while the geometry spans all the plots.
# These are real, multi-part polygons (it's not a digitising error)

# 8. MULTI-PART STRUCTURE BY DEVELOPMENT CATEGORY ------------------------------

# Quantify multi-part structure per category for the methods section
print(development_polygons |>
        st_drop_geometry() |>
        group_by(arealformalsgruppe) |>
        summarise(n_polygons    = n(),
                  median_parts  = median(n_parts),
                  max_parts     = max(n_parts),
                  pct_multipart = round(100 * mean(n_parts > 1), 1),
                  .groups = "drop") |>
        arrange(desc(pct_multipart)))

# 9. CONCLUSION ----------------------------------------------------------------

# Should use the geometry-derived area (st_area) as area_m2_numeric for both development polygons and buffers
# Will retain planlagt_areal_m2 unchanged as a reference column but will not use it in models

# Things to remember to add in the methods:
# 1. The area used is the GIS footprint and not the planlagt_areal_m2
# 2. The Recreational category systematically contains multi-part, spatially dispersed polygons

# END OF SCRIPT ----------------------------------------------------------------