##----------------------------------------------------------------------------##
# TEST SCRIPT: Iterative st_buffer to achieve exact 2x area
# This script uses an iterative approach to find the perfect buffer distance
##----------------------------------------------------------------------------##

library(here)
library(sf)
library(dplyr)

# 1. LOAD REAL DATA ------------------------------------------------------------

cat("Loading real development polygons...\n\n")

# Load development polygons
development_polygons <- st_read(here("data", "raw_data", "nina_planagt.gpkg"))

# Filter out Ports & Marinas (as you do in the main script)
development_polygons_filtered <- development_polygons |>
  filter(arealformalsgruppe != "16 Havner og småbåthavner") |>
  mutate(polygon_id = row_number(),
         area_m2_numeric = as.numeric(planlagt_areal_m2))

cat("Total polygons available:", nrow(development_polygons_filtered), "\n\n")

# 2. SELECT TEST POLYGONS ------------------------------------------------------

cat("Selecting test polygons...\n")

set.seed(123)  # for reproducibility

# Get 10 random polygons across different size categories
quantiles <- quantile(development_polygons_filtered$area_m2_numeric, 
                      probs = c(0.1, 0.25, 0.5, 0.75, 0.9))

test_polygons <- development_polygons_filtered |>
  mutate(size_category = case_when(
    area_m2_numeric <= quantiles[1] ~ "Very small",
    area_m2_numeric <= quantiles[2] ~ "Small",
    area_m2_numeric <= quantiles[3] ~ "Medium",
    area_m2_numeric <= quantiles[4] ~ "Large",
    TRUE ~ "Very large"
  )) |>
  group_by(size_category) |>
  slice_sample(n = 2) |>
  ungroup() |>
  select(polygon_id, area_m2_numeric, size_category, arealformalsgruppe)

cat("Selected", nrow(test_polygons), "test polygons:\n")
print(test_polygons)
cat("\n")

# Store original areas
test_polygons$original_area <- as.numeric(st_area(test_polygons))

# 3. ITERATIVE BUFFER APPROACH -------------------------------------------------

cat("=== ITERATIVE ST_BUFFER TO ACHIEVE 2X AREA ===\n\n")

# Function to find buffer distance that achieves target area
find_buffer_distance_for_target_area <- function(geom, original_area, 
                                                 target_multiplier = 2,
                                                 tolerance = 0.001,
                                                 max_iterations = 20) {
  
  # Initial guess based on area/perimeter approximation
  perimeter <- as.numeric(st_length(st_cast(st_geometry(geom), "MULTILINESTRING")))
  
  # For 2x area, we want buffer area = original area
  # So initial guess: dist = original_area / perimeter
  dist_guess <- original_area / perimeter
  
  # Target area
  target_area <- original_area * target_multiplier
  
  # Track iterations
  iteration_count <- 0
  
  # Iteratively adjust buffer distance to hit target area
  for(i in 1:max_iterations) {
    iteration_count <- i
    
    # Create buffered polygon
    buffered <- st_buffer(geom, dist = dist_guess)
    current_area <- as.numeric(st_area(buffered))
    
    # Check if we're close enough
    ratio <- current_area / target_area
    
    if(abs(ratio - 1) < tolerance) {
      # Success! Close enough to target
      break
    }
    
    # Adjust distance proportionally
    # Using square root because area scales with distance^2
    dist_guess <- dist_guess * sqrt(target_area / current_area)
  }
  
  # Return both the distance and the final buffered geometry
  buffered_final <- st_buffer(geom, dist = dist_guess)
  
  return(list(
    distance = dist_guess,
    buffered = buffered_final,
    iterations = iteration_count,
    final_area = as.numeric(st_area(buffered_final))
  ))
}

# Apply to all test polygons
cat("Finding optimal buffer distances...\n")
buffer_results <- list()

for(i in 1:nrow(test_polygons)) {
  cat("  Processing polygon", i, "of", nrow(test_polygons), "...\n")
  
  result <- find_buffer_distance_for_target_area(
    st_geometry(test_polygons)[i],
    test_polygons$original_area[i],
    target_multiplier = 2,
    tolerance = 0.001,
    max_iterations = 20
  )
  
  buffer_results[[i]] <- result
  cat("    Iterations:", result$iterations, 
      "| Final area ratio:", round(result$final_area / (test_polygons$original_area[i] * 2), 6), "\n")
}

cat("\n")

# 4. CREATE BUFFERS BY SUBTRACTION ---------------------------------------------

cat("=== CREATING BUFFER ZONES (DONUTS) ===\n\n")

buffers <- test_polygons

for(i in 1:nrow(buffers)) {
  # Subtract original from buffered to get the "donut"
  st_geometry(buffers)[i] <- st_difference(
    buffer_results[[i]]$buffered,
    st_geometry(test_polygons)[i]
  )
  cat("  Created buffer zone", i, "of", nrow(buffers), "\n")
}

cat("\n")

# Calculate buffer areas
buffers$buffer_area <- as.numeric(st_area(buffers))
buffers$buffered_total_area <- sapply(buffer_results, function(x) x$final_area)
buffers$iterations <- sapply(buffer_results, function(x) x$iterations)
buffers$buffer_distance <- sapply(buffer_results, function(x) x$distance)

# 5. RESULTS -------------------------------------------------------------------

cat("=== DETAILED RESULTS ===\n\n")

results_df <- data.frame(
  polygon_id = test_polygons$polygon_id,
  size_category = test_polygons$size_category,
  original_area = test_polygons$original_area,
  buffered_total = buffers$buffered_total_area,
  buffer_zone_area = buffers$buffer_area,
  buffer_ratio = buffers$buffer_area / test_polygons$original_area,
  iterations = buffers$iterations,
  buffer_dist = round(buffers$buffer_distance, 2)
)

print(results_df)
cat("\n")

# 6. DIAGNOSTICS ---------------------------------------------------------------

cat("=== DETAILED DIAGNOSTICS ===\n\n")

for(i in 1:nrow(buffers)) {
  cat("Polygon ID:", test_polygons$polygon_id[i], "\n")
  cat("  Size category:", test_polygons$size_category[i], "\n")
  cat("  Original area:", round(test_polygons$original_area[i], 2), "m²\n")
  cat("  Target buffered area (2x):", round(test_polygons$original_area[i] * 2, 2), "m²\n")
  cat("  Actual buffered area:", round(buffers$buffered_total_area[i], 2), "m²\n")
  cat("  Buffered area ratio (actual/target):", 
      round(buffers$buffered_total_area[i] / (test_polygons$original_area[i] * 2), 6), "\n")
  cat("  Buffer zone area:", round(buffers$buffer_area[i], 2), "m²\n")
  cat("  Buffer/Original ratio:", round(buffers$buffer_area[i] / test_polygons$original_area[i], 4), "\n")
  cat("  Iterations needed:", buffers$iterations[i], "\n")
  cat("  Buffer distance used:", round(buffers$buffer_distance[i], 2), "m\n")
  cat("  Buffer valid?", st_is_valid(st_geometry(buffers)[i]), "\n")
  cat("  Buffer geometry type:", st_geometry_type(st_geometry(buffers)[i]), "\n")
  cat("\n")
}

# 7. VISUAL CHECK --------------------------------------------------------------

cat("=== CREATING VISUALIZATION ===\n\n")

n_plots <- min(6, nrow(test_polygons))

png(filename = here("figures", "buffer_iterative_test_real_polygons.png"),
    width = 12, height = 8, units = "in", res = 300)

par(mfrow = c(2, 3))

for(i in 1:n_plots) {
  # Plot buffer first (so it's behind)
  plot(st_geometry(buffers)[i], col = "lightgreen", border = "darkgreen",
       main = paste0("ID:", test_polygons$polygon_id[i], "\n", 
                     test_polygons$size_category[i], " (", buffers$iterations[i], " iter)"),
       lwd = 2)
  # Plot original on top
  plot(st_geometry(test_polygons)[i], col = "lightcoral", border = "red",
       add = TRUE, lwd = 2)
  
  # Add ratio text
  centroid_coords <- st_coordinates(st_centroid(st_geometry(test_polygons)[i]))
  text(centroid_coords[1], centroid_coords[2],
       labels = paste0("Ratio: ", round(buffers$buffer_area[i] / test_polygons$original_area[i], 3)),
       cex = 0.8)
  
  if(i == 1) {
    legend("topright", 
           legend = c("Original", "Buffer"),
           fill = c("lightcoral", "lightgreen"),
           border = c("red", "darkgreen"),
           cex = 0.7)
  }
}

par(mfrow = c(1, 1))
dev.off()

cat("Visualization saved to: figures/buffer_iterative_test_real_polygons.png\n\n")

# 8. SUMMARY -------------------------------------------------------------------

cat("=== SUMMARY ===\n\n")
cat("ITERATIVE ST_BUFFER APPROACH:\n")
cat("Goal: Find buffer distance that makes buffered polygon exactly 2x original area\n")
cat("Method: Iteratively adjust distance until target is reached\n\n")

cat("Convergence:\n")
cat("  Mean iterations:", round(mean(buffers$iterations), 2), "\n")
cat("  Max iterations:", max(buffers$iterations), "\n")
cat("  Min iterations:", min(buffers$iterations), "\n\n")

cat("Buffered area accuracy (should be 2.0x original):\n")
buffered_ratios <- buffers$buffered_total_area / test_polygons$original_area
cat("  Mean ratio:", round(mean(buffered_ratios), 6), "\n")
cat("  Range:", round(min(buffered_ratios), 6), "to", round(max(buffered_ratios), 6), "\n\n")

cat("Buffer zone area accuracy (should be 1.0x original):\n")
buffer_ratios <- buffers$buffer_area / test_polygons$original_area
cat("  Mean ratio:", round(mean(buffer_ratios), 4), "\n")
cat("  Median ratio:", round(median(buffer_ratios), 4), "\n")
cat("  Std Dev:", round(sd(buffer_ratios), 4), "\n")
cat("  Range:", round(min(buffer_ratios), 4), "to", round(max(buffer_ratios), 4), "\n\n")

# Check accuracy at different thresholds
problems_1pct <- abs((buffer_ratios) - 1) > 0.01
problems_5pct <- abs((buffer_ratios) - 1) > 0.05
problems_10pct <- abs((buffer_ratios) - 1) > 0.10

cat("Buffer zone area deviations from expected (1.0x):\n")
cat("  >1% off:", sum(problems_1pct), "polygons\n")
cat("  >5% off:", sum(problems_5pct), "polygons\n")
cat("  >10% off:", sum(problems_10pct), "polygons\n\n")

if(sum(problems_5pct) > 0) {
  cat("⚠ Polygons with buffer ratios >5% off from 1.0:\n")
  print(results_df[problems_5pct, ])
  cat("\n")
} else {
  cat("✓ All buffer ratios within 5% of expected value (1.0)\n\n")
}

# Overall assessment
if(all(abs(buffer_ratios - 1) < 0.01)) {
  cat("✓✓✓ EXCELLENT: All buffer areas within 1% of original!\n")
} else if(all(abs(buffer_ratios - 1) < 0.05)) {
  cat("✓✓ VERY GOOD: All buffer areas within 5% of original!\n")
} else if(all(abs(buffer_ratios - 1) < 0.10)) {
  cat("✓ GOOD: All buffer areas within 10% of original.\n")
} else {
  cat("⚠ MIXED: Some buffer areas differ significantly.\n")
}

cat("\n=== COMPARISON WITH OTHER APPROACHES ===\n")
cat("Scaling approach:     2/10 problematic (20% failure rate, but very accurate when it works)\n")
cat("Simple st_buffer:     9/10 problematic (90% failure rate)\n")
cat("Iterative st_buffer:  see results above\n\n")

cat("Computational cost:\n")
cat("  Total iterations for 10 polygons:", sum(buffers$iterations), "\n")
cat("  Average per polygon:", round(mean(buffers$iterations), 1), "\n")
cat("  Estimated for full dataset (131,023 polygons):", 
    round(mean(buffers$iterations) * 131023), "iterations\n")

cat("\nEND OF TEST\n")