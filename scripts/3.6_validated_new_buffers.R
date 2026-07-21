##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 3.8_final_buffer_validation
# Final validation of the first 1000 buffers before processing remaining polygons
# This script uses the correct ID matching method
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

library(here)
source(here("scripts", "0_setup.R"))

# Load the buffers
load(here("data", "raw_data", "bufferssf.rda"))

# Load original development polygons
development_polygons <- st_read(here("data", "raw_data", "nina_planagt.gpkg"))

# Add numeric area column
development_polygons <- development_polygons |>
  mutate(area_m2_numeric = as.numeric(planlagt_areal_m2))

cat("=== BUFFER VALIDATION REPORT ===\n")
cat("Generated:", Sys.time(), "\n\n")

cat("Data loaded:\n")
cat("  Development polygons:", nrow(development_polygons), "\n")
cat("  Buffers to validate:", nrow(bufferssf), "\n\n")

# 2. CHECK DATA INTEGRITY ------------------------------------------------------

cat("=== DATA INTEGRITY CHECKS ===\n\n")

# Check CRS match
cat("CRS Compatibility:\n")
cat("  Polygons CRS:", st_crs(development_polygons)$input, "\n")
cat("  Buffers CRS:", st_crs(bufferssf)$input, "\n")
if(st_crs(bufferssf) == st_crs(development_polygons)) {
  cat("  ✓ CRS match confirmed\n\n")
} else {
  cat("  ✗ CRS MISMATCH - This will cause problems!\n\n")
}

# Check ID matching
buffer_ids <- bufferssf$id
polygon_ids <- development_polygons$id

cat("ID Matching:\n")
cat("  Buffer ID range:", min(buffer_ids), "-", max(buffer_ids), "\n")
cat("  Polygon ID range:", min(polygon_ids), "-", max(polygon_ids), "\n")

ids_in_both <- intersect(buffer_ids, polygon_ids)
ids_only_buffers <- setdiff(buffer_ids, polygon_ids)
ids_only_polygons <- setdiff(polygon_ids, buffer_ids)

cat("  IDs in both datasets:", length(ids_in_both), "\n")
cat("  IDs only in buffers:", length(ids_only_buffers), "\n")
cat("  IDs only in polygons:", length(ids_only_polygons), "\n")

if(length(ids_only_buffers) > 0) {
  cat("  ⚠ Some buffer IDs don't match polygons!\n\n")
} else {
  cat("  ✓ All buffer IDs found in polygon dataset\n\n")
}

# Check geometry validity
invalid_geoms <- sum(!st_is_valid(bufferssf))
empty_geoms <- sum(st_is_empty(bufferssf))

cat("Geometry Quality:\n")
cat("  Invalid geometries:", invalid_geoms, "\n")
cat("  Empty geometries:", empty_geoms, "\n")

if(invalid_geoms > 0 | empty_geoms > 0) {
  cat("  ✗ GEOMETRY ISSUES DETECTED\n\n")
} else {
  cat("  ✓ All geometries valid\n\n")
}

# Check for missing values
cat("Missing Values:\n")
na_check <- bufferssf |>
  st_drop_geometry() |>
  summarise(
    na_id = sum(is.na(id)),
    na_planid = sum(is.na(planid)),
    na_ocean = sum(is.na(CompleteInOcean)),
    na_planned = sum(is.na(CompleteInOtherPlanned)),
    na_dist = sum(is.na(EndBufferDist)),
    na_size = sum(is.na(EndBufferSize))
  )

print(na_check)
if(sum(na_check) > 0) {
  cat("  ⚠ Missing values detected\n\n")
} else {
  cat("  ✓ No missing values\n\n")
}

# 3. SPATIAL RELATIONSHIP VALIDATION -------------------------------------------

cat("=== SPATIAL RELATIONSHIP VALIDATION ===\n\n")

# Sample for analysis (100 random buffers)
set.seed(42)
sample_size <- min(100, nrow(bufferssf))
sample_ids <- sample(ids_in_both, sample_size)

cat("Testing spatial relationships for", sample_size, "random samples...\n\n")

# Function to check spatial relationship
check_spatial_relationship <- function(check_id, polys, buffs) {
  poly <- polys |> filter(id == check_id)
  buff <- buffs |> filter(id == check_id)
  
  if(nrow(poly) == 0 | nrow(buff) == 0) {
    return(NULL)
  }
  
  # Calculate distance
  dist <- as.numeric(st_distance(poly, buff))
  
  # Check if they touch
  touches <- st_intersects(poly, buff, sparse = FALSE)[1,1]
  
  # Get areas
  poly_area <- st_area(poly)
  buff_area <- st_area(buff)
  
  # Get buffer info
  buff_data <- buff |> st_drop_geometry()
  
  data.frame(
    id = check_id,
    distance_m = dist,
    touches = touches,
    poly_area = as.numeric(poly_area),
    buff_area = as.numeric(buff_area),
    area_ratio = as.numeric(buff_area / poly_area),
    CompleteInOcean = buff_data$CompleteInOcean,
    CompleteInOtherPlanned = buff_data$CompleteInOtherPlanned,
    EndBufferDist = buff_data$EndBufferDist,
    EndBufferSize = buff_data$EndBufferSize
  )
}

# Run checks
spatial_results <- lapply(sample_ids, check_spatial_relationship,
                          polys = development_polygons,
                          buffs = bufferssf)
spatial_df <- bind_rows(spatial_results)

# Categorize buffers
spatial_df <- spatial_df |>
  mutate(
    category = case_when(
      CompleteInOcean == 1 ~ "Ocean",
      CompleteInOtherPlanned == 1 ~ "Enclosed",
      TRUE ~ "Normal"
    ),
    size_diff_pct = abs(area_ratio - 1) * 100
  )

# Summary by category
cat("Spatial relationship by category:\n")
summary_by_cat <- spatial_df |>
  group_by(category) |>
  summarise(
    n = n(),
    n_touching = sum(touches),
    pct_touching = round(100 * n_touching / n, 1),
    avg_distance = round(mean(distance_m), 1),
    median_distance = round(median(distance_m), 1),
    max_distance = round(max(distance_m), 1),
    avg_area_ratio = round(mean(area_ratio), 2),
    within_20pct_size = sum(size_diff_pct <= 20),
    .groups = "drop"
  )

print(summary_by_cat)
cat("\n")

# 4. BUFFER QUALITY ASSESSMENT -------------------------------------------------

cat("=== BUFFER QUALITY ASSESSMENT ===\n\n")

# Overall statistics
total_buffers <- nrow(bufferssf)
n_ocean <- sum(bufferssf$CompleteInOcean == 1, na.rm = TRUE)
n_enclosed <- sum(bufferssf$CompleteInOtherPlanned == 1, na.rm = TRUE)
n_normal <- total_buffers - n_ocean - n_enclosed

cat("Buffer Classification:\n")
cat("  Normal buffers:", n_normal, 
    "(", round(100 * n_normal / total_buffers, 1), "%)\n")
cat("  Ocean-surrounded:", n_ocean,
    "(", round(100 * n_ocean / total_buffers, 1), "%)\n")
cat("  Enclosed by development:", n_enclosed,
    "(", round(100 * n_enclosed / total_buffers, 1), "%)\n\n")

# Buffer distance statistics
cat("Buffer Distance Statistics (meters):\n")
dist_summary <- summary(bufferssf$EndBufferDist)
print(dist_summary)
cat("\n")

# Count extreme distances
extreme_1km <- sum(bufferssf$EndBufferDist > 1000, na.rm = TRUE)
extreme_5km <- sum(bufferssf$EndBufferDist > 5000, na.rm = TRUE)

cat("Extreme Buffer Distances:\n")
cat("  >1 km:", extreme_1km, "(", round(100 * extreme_1km / total_buffers, 1), "%)\n")
cat("  >5 km:", extreme_5km, "(", round(100 * extreme_5km / total_buffers, 1), "%)\n\n")

# Size matching for normal buffers only
normal_buffers <- spatial_df |> filter(category == "Normal")
within_10pct <- sum(normal_buffers$size_diff_pct <= 10, na.rm = TRUE)
within_20pct <- sum(normal_buffers$size_diff_pct <= 20, na.rm = TRUE)

cat("Size Matching for Normal Buffers:\n")
cat("  Within 10% of polygon area:", within_10pct,
    "(", round(100 * within_10pct / nrow(normal_buffers), 1), "%)\n")
cat("  Within 20% of polygon area:", within_20pct,
    "(", round(100 * within_20pct / nrow(normal_buffers), 1), "%)\n\n")

# 5. CREATE VISUALIZATIONS -----------------------------------------------------

cat("=== CREATING VALIDATION VISUALIZATIONS ===\n\n")

# 5.1 Distance distribution by category
dist_plot <- spatial_df |>
  ggplot(aes(x = category, y = distance_m, fill = category)) +
  geom_boxplot() +
  scale_y_log10(labels = scales::comma,
                breaks = c(0.1, 1, 10, 100, 1000, 10000)) +
  labs(title = "Buffer-to-Polygon Distance by Category",
       subtitle = paste("Sample of", nrow(spatial_df), "buffers"),
       x = "Category",
       y = "Distance (m, log scale)") +
  theme_classic() +
  theme(legend.position = "none")

ggsave(filename = here("figures", "Final_validation_distance_by_category.png"),
       plot = dist_plot,
       width = 10,
       height = 6,
       dpi = 300)

cat("  ✓ Distance distribution plot saved\n")

# 5.2 Size comparison for normal buffers
size_plot <- spatial_df |>
  filter(category == "Normal") |>
  ggplot(aes(x = poly_area, y = buff_area)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", 
              linetype = "dashed", linewidth = 1) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Polygon Area vs Buffer Area (Normal Buffers Only)",
       subtitle = "Red line = perfect 1:1 match",
       x = expression(paste("Polygon area (m"^2, ", log scale)")),
       y = expression(paste("Buffer area (m"^2, ", log scale)"))) +
  theme_classic()

ggsave(filename = here("figures", "Final_validation_size_comparison.png"),
       plot = size_plot,
       width = 10,
       height = 8,
       dpi = 300)

cat("  ✓ Size comparison plot saved\n")

# 5.3 Buffer distance histogram
dist_hist <- ggplot(bufferssf, aes(x = EndBufferDist)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  scale_x_log10(labels = scales::comma) +
  geom_vline(xintercept = 1000, color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = 1000, y = Inf, label = "1 km threshold", 
           vjust = 1.5, hjust = -0.1, color = "red") +
  labs(title = "Distribution of Buffer Distances",
       x = "Buffer distance (m, log scale)",
       y = "Count") +
  theme_classic()

ggsave(filename = here("figures", "Final_validation_distance_histogram.png"),
       plot = dist_hist,
       width = 10,
       height = 6,
       dpi = 300)

cat("  ✓ Distance histogram saved\n")

# 5.4 Example cases visualization
cat("\n  Creating example case visualizations...\n")

# Select examples
normal_id <- spatial_df |> filter(category == "Normal") |> head(1) |> pull(id)
ocean_id <- spatial_df |> filter(category == "Ocean") |> head(1) |> pull(id)
enclosed_id <- spatial_df |> filter(category == "Enclosed") |> head(1) |> pull(id)
large_dist_id <- spatial_df |> arrange(desc(distance_m)) |> head(1) |> pull(id)

# Function to create example plot
plot_example <- function(example_id, polys, buffs, title) {
  poly <- polys |> filter(id == example_id)
  buff <- buffs |> filter(id == example_id)
  
  if(nrow(poly) == 0 | nrow(buff) == 0) return(NULL)
  
  # Get info
  buff_info <- buff |> st_drop_geometry()
  dist <- as.numeric(st_distance(poly, buff))
  
  # Create bounding box
  bbox <- st_bbox(st_union(poly, buff))
  bbox_exp <- bbox + c(-500, -500, 500, 500)
  
  # Plot
  p <- ggplot() +
    geom_sf(data = buff, fill = "lightblue", color = "blue", 
            alpha = 0.4, linewidth = 0.8) +
    geom_sf(data = poly, fill = "coral", color = "red", 
            linewidth = 1) +
    coord_sf(xlim = c(bbox_exp["xmin"], bbox_exp["xmax"]),
             ylim = c(bbox_exp["ymin"], bbox_exp["ymax"])) +
    labs(title = title,
         subtitle = paste0("ID: ", example_id, 
                           " | Distance: ", round(dist, 1), "m",
                           " | Buffer dist: ", round(buff_info$EndBufferDist, 1), "m")) +
    theme_minimal() +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  return(p)
}

# Create plots for different categories
plots <- list()
if(length(normal_id) > 0) {
  plots[[1]] <- plot_example(normal_id, development_polygons, bufferssf,
                             "Normal Buffer")
}
if(length(ocean_id) > 0) {
  plots[[2]] <- plot_example(ocean_id, development_polygons, bufferssf,
                             "Ocean-Surrounded Polygon")
}
if(length(enclosed_id) > 0) {
  plots[[3]] <- plot_example(enclosed_id, development_polygons, bufferssf,
                             "Enclosed by Development")
}
if(length(large_dist_id) > 0) {
  plots[[4]] <- plot_example(large_dist_id, development_polygons, bufferssf,
                             "Largest Distance Case")
}

# Remove NULL plots
plots <- plots[!sapply(plots, is.null)]

if(length(plots) > 0) {
  combined <- plot_grid(plotlist = plots, ncol = 2)
  
  ggsave(filename = here("figures", "Final_validation_example_cases.png"),
         plot = combined,
         width = 14,
         height = 12,
         dpi = 300)
  
  cat("  ✓ Example cases plot saved\n")
}

# 5.5 Random sample of normal buffers for detailed inspection
cat("  Creating random sample of normal buffers...\n")

# Get random normal buffer IDs
normal_buffers_all <- spatial_df |> filter(category == "Normal")

if(nrow(normal_buffers_all) >= 12) {
  set.seed(4561)  # For reproducibility
  random_normal_ids <- sample(normal_buffers_all$id, 12)
  
  # Create plots for each
  normal_plots <- lapply(seq_along(random_normal_ids), function(i) {
    plot_example(random_normal_ids[i], development_polygons, bufferssf,
                 paste0("Normal #", i))
  })
  
  # Remove NULL plots
  normal_plots <- normal_plots[!sapply(normal_plots, is.null)]
  
  if(length(normal_plots) > 0) {
    # Create grid (3 rows x 4 columns = 12 plots)
    combined_normal <- plot_grid(plotlist = normal_plots, ncol = 4)
    
    ggsave(filename = here("figures", "Final_validation_normal_buffers_sample.png"),
           plot = combined_normal,
           width = 16,
           height = 12,
           dpi = 300)
    
    cat("  ✓ Random normal buffers plot saved (12 examples)\n")
    cat("  IDs plotted:", paste(random_normal_ids, collapse = ", "), "\n")
  }
} else {
  cat("  ⚠ Not enough normal buffers for random sample (need 12, have ", 
      nrow(normal_buffers_all), ")\n")
}

cat("\n")

# 6. PLANID ANALYSIS -----------------------------------------------------------

cat("=== PLANID ANALYSIS ===\n\n")

# Check if problematic buffers cluster by planid
planid_summary <- bufferssf |>
  st_drop_geometry() |>
  group_by(planid) |>
  summarise(
    n_polygons = n(),
    n_ocean = sum(CompleteInOcean == 1, na.rm = TRUE),
    n_enclosed = sum(CompleteInOtherPlanned == 1, na.rm = TRUE),
    n_problematic = n_ocean + n_enclosed,
    pct_problematic = round(100 * n_problematic / n_polygons, 1),
    avg_buffer_dist = mean(EndBufferDist, na.rm = TRUE),
    max_buffer_dist = max(EndBufferDist, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(pct_problematic))

cat("Total unique planids:", nrow(planid_summary), "\n")
cat("Average polygons per planid:", round(mean(planid_summary$n_polygons), 1), "\n\n")

# Identify highly problematic planids
highly_problematic <- planid_summary |>
  filter(pct_problematic > 50)

cat("Planids with >50% problematic buffers:", nrow(highly_problematic), "\n")
if(nrow(highly_problematic) > 0) {
  cat("\nTop 10 most problematic planids:\n")
  print(head(highly_problematic, 10))
  cat("\n⚠ These development plans may need to be excluded entirely\n\n")
}

# Save planid summary
write.csv(planid_summary,
          here("data", "derived_data", "buffer_validation_planid_summary.csv"),
          row.names = FALSE)

cat("Planid summary saved to: buffer_validation_planid_summary.csv\n\n")

# 7. VALIDATION DECISION MATRIX ------------------------------------------------

cat("=== VALIDATION DECISION MATRIX ===\n\n")

# Calculate quality metrics
pct_normal_touching <- summary_by_cat |>
  filter(category == "Normal") |>
  pull(pct_touching)

pct_size_match <- if(nrow(normal_buffers) > 0) {
  round(100 * within_20pct / nrow(normal_buffers), 1)
} else {
  NA
}

# Decision criteria
decisions <- data.frame(
  Check = c(
    "CRS Match",
    "ID Matching",
    "Geometry Validity",
    "Normal Buffers Touch Polygon",
    "Size Match (within 20%)",
    "Proportion of Usable Buffers"
  ),
  Result = c(
    ifelse(st_crs(bufferssf) == st_crs(development_polygons), "PASS", "FAIL"),
    ifelse(length(ids_only_buffers) == 0, "PASS", "FAIL"),
    ifelse(invalid_geoms == 0 & empty_geoms == 0, "PASS", "FAIL"),
    ifelse(!is.null(pct_normal_touching) && pct_normal_touching > 90, "PASS", 
           ifelse(!is.null(pct_normal_touching) && pct_normal_touching > 70, "WARNING", "FAIL")),
    ifelse(!is.na(pct_size_match) && pct_size_match > 80, "PASS",
           ifelse(!is.na(pct_size_match) && pct_size_match > 60, "WARNING", "FAIL")),
    ifelse(n_normal / total_buffers > 0.8, "PASS",
           ifelse(n_normal / total_buffers > 0.6, "WARNING", "FAIL"))
  ),
  Value = c(
    paste0(st_crs(bufferssf)$input),
    paste0(length(ids_only_buffers), " mismatches"),
    paste0(invalid_geoms + empty_geoms, " issues"),
    paste0(pct_normal_touching, "%"),
    paste0(pct_size_match, "%"),
    paste0(round(100 * n_normal / total_buffers, 1), "%")
  )
)

print(decisions)
cat("\n")

# Overall decision
all_pass <- all(decisions$Result == "PASS")
any_fail <- any(decisions$Result == "FAIL")

cat("OVERALL VALIDATION RESULT:\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
if(all_pass) {
  cat("✓ ✓ ✓  ALL CHECKS PASSED  ✓ ✓ ✓\n")
  cat("Buffer creation methodology is working correctly.\n")
  cat("Proceed with full buffer generation.\n\n")
} else if(any_fail) {
  cat("✗ ✗ ✗  CRITICAL ISSUES DETECTED  ✗ ✗ ✗\n")
  cat("Address the FAIL items before proceeding.\n\n")
} else {
  cat("⚠ ⚠ ⚠  WARNINGS DETECTED  ⚠ ⚠ ⚠\n")
  cat("Review WARNING items before proceeding.\n")
  cat("Consider adjusting methodology or filtering criteria.\n\n")
}

# 8. RECOMMENDATIONS -----------------------------------------------------------

cat("=== RECOMMENDATIONS ===\n\n")

cat("Based on validation of", total_buffers, "buffers:\n\n")

# Data quality
if(invalid_geoms > 0 | empty_geoms > 0) {
  cat("🔴 CRITICAL - Geometry Issues:\n")
  cat("   Fix invalid/empty geometries before proceeding\n\n")
}

# Buffer quality
if(!is.null(pct_normal_touching) && pct_normal_touching > 90) {
  cat("✓ Buffer Placement: Excellent (", pct_normal_touching, "% touch polygon)\n")
} else if(!is.null(pct_normal_touching)) {
  cat("⚠ Buffer Placement: Needs review (", pct_normal_touching, "% touch polygon)\n")
}

if(!is.na(pct_size_match) && pct_size_match > 80) {
  cat("✓ Size Matching: Good (", pct_size_match, "% within 20%)\n")
} else if(!is.na(pct_size_match)) {
  cat("⚠ Size Matching: Moderate (", pct_size_match, "% within 20%)\n")
}
cat("\n")

# Filtering recommendations
cat("SUGGESTED FILTERING CRITERIA FOR ANALYSIS:\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

cat("Exclude buffers where:\n")
cat("  1. CompleteInOcean == 1 (", n_ocean, " buffers)\n")
cat("  2. CompleteInOtherPlanned == 1 (", n_enclosed, " buffers)\n")

# Suggest distance threshold
recommended_dist <- quantile(bufferssf$EndBufferDist[bufferssf$CompleteInOcean == 0 & 
                                                       bufferssf$CompleteInOtherPlanned == 0], 
                             0.95, na.rm = TRUE)
cat("  3. EndBufferDist >", round(recommended_dist, 0), "m (suggested 95th percentile)\n\n")

usable_after_filter <- sum(bufferssf$CompleteInOcean == 0 & 
                             bufferssf$CompleteInOtherPlanned == 0 &
                             bufferssf$EndBufferDist <= recommended_dist, na.rm = TRUE)

cat("Expected usable buffers after filtering:", usable_after_filter,
    "(", round(100 * usable_after_filter / total_buffers, 1), "%)\n\n")

# Next steps
cat("NEXT STEPS:\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

if(all_pass) {
  cat("1. ✓ Review generated figures in the 'figures' folder\n")
  cat("2. ✓ Spot-check a few example IDs in GIS software\n")
  cat("3. ✓ Proceed with buffer generation for remaining polygons\n")
  cat("4. ✓ Apply same validation to full dataset\n")
  cat("5. ✓ Use filtering criteria above in your analysis\n\n")
} else {
  cat("1. ⚠ Review FAIL/WARNING items in decision matrix\n")
  cat("2. ⚠ Examine figures to understand issues\n")
  cat("3. ⚠ Investigate problematic cases\n")
  cat("4. ⚠ Consider methodology adjustments\n")
  cat("5. ⚠ Re-run validation after fixes\n\n")
}

# 9. SAVE VALIDATION RESULTS ---------------------------------------------------

validation_results <- list(
  summary_statistics = data.frame(
    total_buffers = total_buffers,
    n_normal = n_normal,
    n_ocean = n_ocean,
    n_enclosed = n_enclosed,
    pct_usable = round(100 * n_normal / total_buffers, 1),
    median_buffer_dist = median(bufferssf$EndBufferDist, na.rm = TRUE),
    recommended_dist_threshold = recommended_dist
  ),
  spatial_sample = spatial_df,
  decision_matrix = decisions,
  planid_summary = planid_summary,
  validation_passed = all_pass,
  timestamp = Sys.time()
)

saveRDS(validation_results,
        here("data", "derived_data", "final_buffer_validation_results.rds"))

cat("Validation results saved to: final_buffer_validation_results.rds\n\n")

cat("=== VALIDATION COMPLETE ===\n")
cat("Report generated:", Sys.time(), "\n")

# END OF SCRIPT ----------------------------------------------------------------