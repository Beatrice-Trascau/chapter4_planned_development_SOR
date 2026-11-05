##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 9.2_supplementary_tables
# This script creates supplementary tables for the manuscript
##----------------------------------------------------------------------------##

# 1. SETUP ---------------------------------------------------------------------

# Load libraries
library(here)
source(here("scripts", "0_setup.R"))

# Load the paired polygon-buffer object
polygon_buffer_data <- readRDS(here("data", "derived_data", 
                                    "polygon_buffer_data.rds"))

# Change the df so that we have one row per pair with development polygon, buffer area and their ratio
area_ratio_pairs <- polygon_buffer_data |>
  st_drop_geometry() |>
  dplyr::select(pair_id, polygon_type, english_categories, area_m2_numeric) |>
  tidyr::pivot_wider(names_from  = polygon_type,
                     values_from = area_m2_numeric) |>
  mutate(buffer_polygon_ratio = Buffer / Development)

# Check that every pair has both areas before summarising
stopifnot(!anyNA(area_ratio_pairs$buffer_polygon_ratio))

# Create a summary per development category
area_ratio_table <- bind_rows(area_ratio_pairs,
                              area_ratio_pairs |> 
                                mutate(english_categories = "All categories")) |>
  group_by(`Development category` = english_categories) |>
  summarise(`N pairs` = n(),
            `Median development area (m²)`= round(median(Development)),
            `Median buffer area (m²)` = round(median(Buffer)),
            Min  = round(min(buffer_polygon_ratio), 2),
            `Q1` = round(quantile(buffer_polygon_ratio, 0.25), 2),
            Median = round(median(buffer_polygon_ratio), 2),
            Mean = round(mean(buffer_polygon_ratio), 2),
            `Q3` = round(quantile(buffer_polygon_ratio, 0.75), 2),
            IQR = round(IQR(buffer_polygon_ratio), 2),
            Max = round(max(buffer_polygon_ratio), 2),
            `% pairs with ratio > 1.25` = round(100 * mean(buffer_polygon_ratio > 1.25), 1),
            .groups = "drop") |>
  # sort categories by size and keep the overall row at the bottom
  arrange(`Development category` == "All categories", desc(`N pairs`))

# Quick check of the table
print(area_ratio_table, width = Inf)

# Check that the category rows add up to the total number of pairs
stopifnot(sum(area_ratio_table$`N pairs`[area_ratio_table$`Development category` != "All categories"]) ==
            nrow(area_ratio_pairs)) # all good!

# Save as .csv
write.csv(area_ratio_table,
          here("figures", "Table_S_buffer_polygon_area_ratios.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# END OF SCRIPT ----------------------------------------------------------------