##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 3.8_polygons_extract_GBIF_occurrences
# This script contains code to extract the GBIF species occurrence records for 
# development polygons and buffers
# N.B: the spatial join is done in chunks and processed on 4 cores in parallel
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source the setup file
library(parallel)
library(here)
source(here("scripts", "0_setup.R"))

# Load the combined polygon and buffer object created in 3.5
polygon_buffer_data <- readRDS(here("data", "derived_data",
                                    "polygon_buffer_data.rds"))

# Load the cleaned GBIF occurrences
clean_occurrences <- read.csv(here("data", "derived_data",
                                   "clean_occurrences_1km.txt"))[,
                                                                 c("gbifID", "species", "year", "parentEventID",
                                                                   "kingdom", "phylum", "class",
                                                                   "decimalLongitude", "decimalLatitude")]

# 2. CHECK INPUT  --------------------------------------------------------------

# Check to make sure that the polygon + buffer output from script 3.5 is correct
stopifnot(inherits(polygon_buffer_data, "sf"),
          all(c("id", "pair_id", "polygon_type", "area_m2_numeric",
                "english_categories", "kommune", "kommune_factor",
                "land_cover_name", "log_area") %in% names(polygon_buffer_data)),
          all(table(polygon_buffer_data$polygon_type) > 0),   # both types present
          !any(is.na(polygon_buffer_data$log_area)))

# Check the number of rows in the loaded object
cat("Loaded combined object:", nrow(polygon_buffer_data), "rows\n") #259762
print(table(polygon_buffer_data$polygon_type))
# Buffer Development 
# 129881      129881 

# Check that id is unique whin each polygon type
stopifnot(anyDuplicated(polygon_buffer_data$id[polygon_buffer_data$polygon_type == "Development"]) == 0,
          anyDuplicated(polygon_buffer_data$id[polygon_buffer_data$polygon_type == "Buffer"]) == 0)

# Build a unique per-row key
polygon_buffer_data <- polygon_buffer_data |>
  mutate(poly_uid = paste(polygon_type, id, sep = "_"))

# Check that the poly_uid is unique across the whole object
stopifnot(anyDuplicated(polygon_buffer_data$poly_uid) == 0)
cat("Unique row keys (poly_uid):", n_distinct(polygon_buffer_data$poly_uid), "\n") # 259762

# 3. PREPARE SPECIES OCCURRENCE RECORDS ----------------------------------------

# Convert occurrences to spatial points (GBIF coordinates are WGS84 / EPSG:4326)
occurrences_sf <- clean_occurrences |>
  filter(!is.na(decimalLongitude), !is.na(decimalLatitude)) |>
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# Remove the occurrence data frame to free up space
rm(clean_occurrences)
gc()

# Transform occurrences to the CRS of the polygons
occurrences_sf <- st_transform(occurrences_sf, st_crs(polygon_buffer_data))

# Check that CRS now matches and the extents overlap 
stopifnot(st_crs(occurrences_sf) == st_crs(polygon_buffer_data))

if (length(st_intersection(st_as_sfc(st_bbox(occurrences_sf)),
                           st_as_sfc(st_bbox(polygon_buffer_data)))) == 0) {
  stop("ERROR: occurrence and polygon bounding boxes do not overlap")
} else {
  cat("PASS: occurrence and polygon extents overlap\n")
} # PASS

cat("Occurrence points with valid coordinates:", nrow(occurrences_sf), "\n") # 18,811,161

# 4. JOIN OCCURRENCES TO POLYGONS & BUFFERS ------------------------------------

# Set chunk size
chunk_size <- 2000
n_chunks   <- ceiling(nrow(polygon_buffer_data) / chunk_size)

# Set number of forked workers
n_cores <- 4
cat("\nRunning", n_chunks, "chunks of", chunk_size,
    "across", n_cores, "forked workers...\n")

# The workers join one chunk of polygons to the occurrences, then reduce to small outputs => each worker returns little data
# The forked workers will inherit polygon_buffer_data, occurrences_sf and chunk_size from 

process_chunk <- function(i) {
  
  start_idx <- (i - 1) * chunk_size + 1
  end_idx   <- min(i * chunk_size, nrow(polygon_buffer_data))
  
  chunk <- polygon_buffer_data[start_idx:end_idx, ] |>
    dplyr::select(poly_uid)
  
  joined_chunk <- st_join(chunk, occurrences_sf,
                          join = st_intersects, left = TRUE) |>
    st_drop_geometry()
  
  # per-polygon counts (all polygons in chunk, incl. zero-occurrence)
  counts <- joined_chunk |>
    group_by(poly_uid) |>
    summarise(n_occurrences = sum(!is.na(gbifID)),
              n_species     = n_distinct(species[!is.na(species)]),
              species_list  = list(unique(species[!is.na(species)])),
              .groups = "drop")
  
  # matched occurrence rows only (for H2d) - zero-occurrence polygons contribute
  # nothing to completeness, so their NA rows are dropped here
  h2d <- joined_chunk |>
    filter(!is.na(gbifID)) |>
    dplyr::select(poly_uid, gbifID, species, year, parentEventID)
  
  list(counts = counts, h2d = h2d)
}

overall_start <- Sys.time()

# Run the function in parallel
results <- mclapply(seq_len(n_chunks), process_chunk,
                    mc.cores = n_cores, mc.preschedule = TRUE)

cat("Parallel join complete in",
    round(as.numeric(difftime(Sys.time(), overall_start, units = "mins")), 1),
    "minutes\n")

# Check that none of the loops gave errors
errored <- vapply(results, function(x) inherits(x, "try-error"), logical(1))
if (any(errored)) {
  stop("ERROR: ", sum(errored), " chunk(s) failed in parallel. First message:\n",
       as.character(results[[which(errored)[1]]]))
}
cat("PASS: all", n_chunks, "chunks completed without error\n")

# 5. ASSEMBLE PER-POLYGON MODEL DATA -------------------------------------------

# Combine the per-chunk counts
occurrence_counts <- bind_rows(lapply(results, `[[`, "counts"))

# Check that there is exactly one count row per polygon or buffer
stopifnot(nrow(occurrence_counts) == nrow(polygon_buffer_data),
          anyDuplicated(occurrence_counts$poly_uid) == 0)

# Attach counts to the full metadata (it does not need the geometry)
model_data <- polygon_buffer_data |>
  st_drop_geometry() |>
  left_join(occurrence_counts, by = "poly_uid")

# Check that the counts were added without gaining or losing rows and without causing key mismatch
stopifnot(nrow(model_data) == nrow(polygon_buffer_data),
          !any(is.na(model_data$n_occurrences)),
          !any(is.na(model_data$n_species)))
cat("\nModel data assembled:", nrow(model_data), "rows\n")

# 6. BUILD OCCURRENCE-LEVEL OBJECT FOR H2D -------------------------------------

# Combine the matched occurrence rows and then re-attach polygon metadata
polygon_buffer_occurrence_join <- bind_rows(lapply(results, `[[`, "h2d"))
rm(results)
gc()

polygon_buffer_occurrence_join <- polygon_buffer_occurrence_join |>
  left_join(polygon_buffer_data |>
              st_drop_geometry() |>
              dplyr::select(poly_uid, id, pair_id, polygon_type,
                            area_m2_numeric, english_categories, kommune,
                            land_cover_name),
            by = "poly_uid")

# Check how many rows we have
cat("H2d occurrence-level rows:", nrow(polygon_buffer_occurrence_join), "\n")

# 7. CHECK THE RESULTS ---------------------------------------------------------

## 7.1. Check the data ---------------------------------------------------------

# Check that both polygon types survived
n_dev <- sum(model_data$polygon_type == "Development")
n_buf <- sum(model_data$polygon_type == "Buffer")

if (n_dev == n_buf && n_buf > 0) {
  cat("PASS: equal Development and Buffer rows (", n_dev, "each)\n")
} else {
  cat("FAIL: Development:", n_dev, " Buffer:", n_buf, "\n")
}

# Check that the pairing is still correct
pair_counts <- model_data |>
  group_by(pair_id) |>
  summarise(n_rows = n(),
            n_dev  = sum(polygon_type == "Development"),
            n_buf  = sum(polygon_type == "Buffer"),
            .groups = "drop")
if (all(pair_counts$n_rows == 2) &&
    all(pair_counts$n_dev == 1 & pair_counts$n_buf == 1)) {
  cat("PASS: every pair has exactly 1 Development + 1 Buffer\n")
} else {
  cat("FAIL:", nrow(pair_counts |> filter(n_rows != 2 | n_dev != 1 | n_buf != 1)),
      "pairs have incorrect composition\n")
}

# Check if there are any NA values in the variables that will be used in the models
cat("\nNA in key modelling columns:\n")
cat("  kommune_factor:", sum(is.na(model_data$kommune_factor)), "\n")
cat("  land_cover_name:", sum(is.na(model_data$land_cover_name)), "\n")
cat("  log_area:", sum(is.na(model_data$log_area)), "\n")

## 7.2. Summary statistics -----------------------------------------------------

cat("\n=== SUMMARY STATISTICS ===\n")
cat("Development - mean occurrences:",
    round(mean(model_data$n_occurrences[model_data$polygon_type == "Development"]), 2),
    " mean species:",
    round(mean(model_data$n_species[model_data$polygon_type == "Development"]), 2), "\n")
cat("Buffer      - mean occurrences:",
    round(mean(model_data$n_occurrences[model_data$polygon_type == "Buffer"]), 2),
    " mean species:",
    round(mean(model_data$n_species[model_data$polygon_type == "Buffer"]), 2), "\n")

cat("\nLand cover x polygon type:\n")
print(table(model_data$land_cover_name, model_data$polygon_type))

# 8. SAVE OUTPUT ---------------------------------------------------------------

# Model-ready dataset (one row per polygon/buffer) - used by H2a, H2b, H2c
saveRDS(model_data,
        here("data", "derived_data", "h2_polygon_buffer_data.rds"))

# Occurrence-level dataset (one row per occurrence per polygon/buffer) - H2d
saveRDS(polygon_buffer_occurrence_join,
        here("data", "derived_data", "h2d_polygon_buffer_occurrence_join.rds"))

# Check that both files were written and read back with the expected row counts
stopifnot(file.exists(here("data", "derived_data", "h2_polygon_buffer_data.rds")),
          file.exists(here("data", "derived_data", "h2d_polygon_buffer_occurrence_join.rds")),
          nrow(readRDS(here("data", "derived_data", "h2_polygon_buffer_data.rds"))) == nrow(polygon_buffer_data))

# 9. PLOT FIGURES --------------------------------------------------------------

# Filter out buffers from the per-polygon data
dev_data <- model_data |>
  filter(polygon_type == "Development")

# Filter out buffers from the occurrence-level development data
dev_occurrences <- polygon_buffer_occurrence_join |>
  filter(polygon_type == "Development")

# Check if you need to add the kingdom, phylum and class columns
if (!all(c("kingdom", "phylum", "class") %in% names(dev_occurrences))) {
  message("kingdom/phylum/class missing - rejoining taxonomy by gbifID")
  tax_lookup <- read.csv(here("data", "derived_data",
                              "clean_occurrences_1km.txt"))[,
                                                            c("gbifID", "kingdom", "phylum", "class")]
  dev_occurrences <- dev_occurrences |>
    left_join(tax_lookup, by = "gbifID")
  rm(tax_lookup); gc()
}
stopifnot(all(c("kingdom", "phylum", "class") %in% names(dev_occurrences)))

# Quick check of the summary
cat("\nDevelopment polygons used for figures:", nrow(dev_data), "\n") # 129881
cat("Development-polygon occurrence records used for Figure 4:",
    nrow(dev_occurrences), "\n") # 323486

# Set colour scale for figures
type_colours <- c("Development" = "#5E3C99", "Buffer" = "#E66101")

# Set legend labels for polygon type
type_labels <- c("Development" = "Development Polygon", "Buffer" = "Buffer")

# Create a copy of the data for plotting
plot_data <- model_data |>
  mutate(polygon_type = factor(polygon_type,
                               levels = c("Development", "Buffer")))

# Set a shared style for the legend
legend_theme <- theme(legend.position        = "inside",
                      legend.position.inside = c(0.98, 0.03),
                      legend.justification.inside = c(1, 0),
                      legend.direction = "vertical",
                      legend.title = element_blank(),
                      legend.text = element_text(size = 20,
                                                 margin = margin(l = 8)),
                      legend.key.size = grid::unit(1, "cm"),
                      legend.key.spacing.y = grid::unit(0.3, "cm"),
                      # semi-transparent white box so points behind it don't clash
                      legend.background = element_rect(fill = scales::alpha("white", 0.8),
                                                            colour = NA))

# Set a relative height for the legend row
legend_height <- 0.13

# Put legend in a row above the plot body
add_legend_row <- function(plot_body, legend_source) {
  leg <- get_legend(legend_source + legend_theme)
  plot_grid(leg, plot_body, ncol = 1, rel_heights = c(legend_height, 1))
}

# Use "//" marks on the y axis at the break (y_npc = 0 bottom, 1 top of panel)
break_mark <- function(y_npc) {
  annotation_custom(grid::segmentsGrob(x0 = grid::unit(0, "npc") - grid::unit(5, "pt"),
                                       x1 = grid::unit(0, "npc") + grid::unit(5, "pt"),
                                       y0 = grid::unit(y_npc, "npc") - grid::unit(3, "pt"),
                                       y1 = grid::unit(y_npc, "npc") + grid::unit(3, "pt"),
                                       gp = grid::gpar(lwd = 1.2)))
}

# Histogram with broken y axis, Development and Buffer bars side by side
plot_broken_hist <- function(data, var, x_lab, n_bins = 50,
                             y_lab = "Number of Polygons",
                             show_legend = FALSE) {
  
  # Non-zero values on log10(n + 1) scale so 1 -> log10(2), matching the labels
  nz_df <- data |>
    filter(.data[[var]] > 0) |>
    mutate(x_log = log10(.data[[var]] + 1))
  
  # Shared bin edges
  bin_breaks <- seq(min(nz_df$x_log) - 1e-6, max(nz_df$x_log) + 1e-6,
                    length.out = n_bins + 1)
  bin_width  <- diff(bin_breaks)[1]
  bar_w      <- bin_width * 0.95 / 2   # width of one bar (two per bin)
  
  # Pre-binned non-zero counts
  nz_counts <- nz_df |>
    mutate(bin = cut(x_log, breaks = bin_breaks,
                     include.lowest = TRUE, labels = FALSE)) |>
    count(polygon_type, bin, name = "n") |>
    mutate(x_mid = (bin_breaks[bin] + bin_breaks[bin + 1]) / 2) |>
    dplyr::select(-bin)
  
  # Zero counts at x = 0
  zero_df <- data |>
    filter(.data[[var]] == 0) |>
    count(polygon_type, name = "n") |>
    mutate(x_mid = 0)
  
  # Explicit bar positions: Development on the left, Buffer on the right
  all_counts <- bind_rows(zero_df, nz_counts) |>
    mutate(xmin = ifelse(polygon_type == "Development", x_mid - bar_w, x_mid),
           xmax = xmin + bar_w)
  
  # Panel ranges
  lower_max  <- max(nz_counts$n) * 1.1
  tall_zeros <- zero_df$n[zero_df$n > lower_max]
  x_range    <- c(-bin_width * 1.5, max(bin_breaks) + bin_width * 0.5)
  
  # Legend (only added to the panel that should carry it)
  legend_layer <- if (show_legend) legend_theme else theme(legend.position = "none")
  
  # Base: scales and theme only, bars are added per panel
  base <- ggplot(all_counts) +
    scale_fill_manual(values = type_colours, labels = type_labels, name = NULL) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = log10(c(1, 2, 11, 101, 1001, 10001)),
                       labels = c("0", "1", "10", "100", "1,000", "10,000")) +
    theme_classic() +
    theme(panel.grid = element_blank(),
          axis.title = element_text(size = 16),
          axis.text  = element_text(size = 16),
          legend.position = "none")
  
  if (length(tall_zeros) == 0) {
    # No zero bar is taller than the histogram, so no break is needed
    message("No axis break needed for ", var)
    return(base +
             geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = n,
                           fill = polygon_type),
                       colour = "white", linewidth = 0.15) +
             labs(x = x_lab, y = y_lab) +
             legend_layer)
  }
  
  upper_min <- max(min(tall_zeros) * 0.9, lower_max)
  upper_max <- max(zero_df$n) * 1.05
  
  # Bottom panel: every bar capped at the top of this panel; carries the legend
  bottom <- base +
    geom_rect(data = all_counts |> filter(n > 0),
              aes(xmin = xmin, xmax = xmax,
                  ymin = 0, ymax = pmin(n, lower_max),
                  fill = polygon_type),
              colour = "white", linewidth = 0.15) +
    coord_cartesian(xlim = x_range, ylim = c(0, lower_max),
                    expand = FALSE, clip = "off") +
    break_mark(1) +
    labs(x = x_lab, y = NULL) +
    theme(plot.margin = margin(3, 5.5, 5.5, 5.5)) +
    legend_layer
  
  # Top panel: only the part of the tall bars above the break
  top <- base +
    geom_rect(data = all_counts |> filter(n > upper_min),
              aes(xmin = xmin, xmax = xmax,
                  ymin = upper_min, ymax = n,
                  fill = polygon_type),
              colour = "white", linewidth = 0.15) +
    coord_cartesian(xlim = x_range, ylim = c(upper_min, upper_max),
                    expand = FALSE, clip = "off") +
    break_mark(0) +
    labs(x = NULL, y = NULL) +
    theme(axis.text.x  = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x  = element_blank(),
          plot.margin  = margin(5.5, 5.5, 3, 5.5))
  
  # Stack the panels and add one shared y-axis title
  panels  <- plot_grid(top, bottom, ncol = 1, align = "v",
                       rel_heights = c(1, 3))
  y_title <- ggdraw() + draw_label(y_lab, angle = 90, size = 16)
  
  plot_grid(y_title, panels, nrow = 1, rel_widths = c(0.05, 1))
}

# Area vs SOR/species scatter, Development and Buffer
plot_area_scatter <- function(data, var, x_lab, show_legend = FALSE) {
  
  ggplot(data, aes(x = .data[[var]] + 1, y = area_m2_numeric,
                   colour = polygon_type, fill = polygon_type)) +
    geom_point(alpha = 0.2, size = 0.8, show.legend = FALSE) +
    geom_smooth(linewidth = 1.1, se = TRUE) +
    scale_colour_manual(values = type_colours, labels = type_labels, name = NULL) +
    scale_fill_manual(values = type_colours, labels = type_labels, name = NULL) +
    scale_x_log10(labels = scales::comma,
                  breaks = c(1, 10, 100, 1000, 10000)) +
    scale_y_log10(labels = scales::comma,
                  breaks = c(100, 1000, 10000, 100000, 1000000)) +
    labs(x = x_lab,
         y = expression(paste("log(Polygon Area (m"^2, "))"))) +
    theme_classic() +
    theme(panel.grid = element_blank(),
          axis.title = element_text(size = 16),
          axis.text  = element_text(size = 16)) +
    if (show_legend) legend_theme else theme(legend.position = "none")
}

## 9.1. Figure 1 - Number of SOR per polygon  ----------------------------------

# Plot the broken histogram
fig1a <- plot_broken_hist(plot_data, "n_occurrences", "Number of SOR",
                          show_legend = TRUE)

# Plot the scatter by area
fig1b <- plot_area_scatter(plot_data, "n_occurrences",
                           "log(Number of SOR + 1)", show_legend = TRUE)

# Combine into a single figure
(figure1 <- plot_grid(fig1a, fig1b, labels = c("a)", "b)")))

# Save figure as .png
ggsave(filename = here("figures", "Figure1ab_SOR_per_polygon.png"),
       plot = figure1,
       width = 22,
       height = 16,
       dpi = 600)

# Save figure as .pdf
ggsave(filename = here("figures", "Figure1ab_SOR_per_polygon.pdf"),
       plot = figure1,
       width = 22,
       height = 16,
       dpi = 600)

## 9.2. Figure 2 - Number of Species per polygon -------------------------------

# Standalone figure 2 with own legend
fig2a <- plot_broken_hist(plot_data, "n_species", "Number of Species",
                          show_legend = TRUE)
fig2b <- plot_area_scatter(plot_data, "n_species",
                           "log(Number of Species + 1)", show_legend = TRUE)

# Combine into single figure
figure2 <- plot_grid(fig2a, fig2b, labels = c("c)", "d)"))

# Save to file
ggsave(here("figures", "Figure1cd_species_per_polygon.png"),
       plot = figure2, width = 22, height = 16, dpi = 600)
ggsave(here("figures", "Figure1cd_species_per_polygon.pdf"),
       plot = figure2, width = 22, height = 16, dpi = 600)

## 9.3. Combine Figure 1 and "2" into single figure ----------------------------

# Plot histogram without a legend
fig2a_noleg <- plot_broken_hist(plot_data, "n_species", "Number of Species")

# Plot scatter by area without a legend
fig2b_noleg <- plot_area_scatter(plot_data, "n_species",
                                 "log(Number of Species + 1)")

# Combine into single figure
(figure1_2_combined <- plot_grid(fig1a, fig1b, fig2a_noleg, fig2b_noleg,
                                labels = c("a)", "b)", "c)", "d)"),
                                ncol = 2))

# Save figure as .png
ggsave(filename = here("figures", "Figure1_2_combined.png"),
       plot = figure1_2_combined,
       width = 26,
       height = 22,
       dpi = 600)

# Save figure as .pdf
ggsave(filename = here("figures", "Figure1_2_combined.pdf"),
       plot = figure1_2_combined,
       width = 26,
       height = 22,
       dpi = 600)

## 9.4. Figure 4 - Taxonomic breakdown of SOR in polygons ----------------------

# Classify occurrences into taxonomic groups
polygon_tax_join <- dev_occurrences |>
  mutate(taxonomic_group = case_when(kingdom == "Plantae" ~ "Plants",
                                     class   == "Aves" ~ "Birds",
                                     phylum  == "Arthropoda" ~ "Arthropods",
                                     class   == "Mammalia" ~ "Mammals",
                                     kingdom == "Fungi" ~ "Fungi",
                                     TRUE  ~ "Other"))

# Calculate proportion of each group per development category
tax_proportions <- polygon_tax_join |>
  group_by(english_categories, taxonomic_group) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(english_categories) |>
  mutate(proportion = n / sum(n)) |>
  ungroup()

# Define colour palette for taxonomic groups
tax_colours <- c("Plants" = "#009E73",
                 "Birds" = "#0072B2",
                 "Arthropods" = "#E69F00",
                 "Mammals" = "#D55E00",
                 "Fungi" = "#CC79A7",
                 "Other" = "#F0E442")

# Plot stacked barplot of proportion of occurrences belongoing to each group
# within the planned development polygons
(figure4 <- ggplot(tax_proportions, aes(x = english_categories, y = proportion,
                                       fill = taxonomic_group)) +
  geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.3) +
  scale_y_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = tax_colours,
                    name   = "Taxonomic Group") +
  labs(x = "Development Category",
       y = "Proportion of SOR") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13)))

# Save figure as .png
ggsave(filename = here("figures", "Figure4_taxonomic_breakdown_per_development_type.png"),
       plot = figure4,
       width = 20,
       height = 16,
       dpi = 600)

# Save figure as .pdf
ggsave(filename = here("figures", "Figure4_taxonomic_breakdown_per_development_type.pdf"),
       plot = figure4,
       width = 20,
       height = 16,
       dpi = 600)

## 9.5. Figure 8 - Number of SOR vs Number of Species per Polygon -------------

# Plot figure
(figure8 <- ggplot(dev_data,
                  aes(x = n_species + 1,
                      y = n_occurrences + 1)) +
  # 1:1 reference line (n_occurrences == n_species)
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "grey50", linewidth = 0.6) +
  geom_point(alpha = 0.3, size = 0.8, color = "#5E3C99") +
  geom_smooth(color = "black", linewidth = 0.8, se = TRUE) +
  scale_x_log10(labels = scales::comma,
                breaks = c(1, 10, 100, 1000, 10000)) +
  scale_y_log10(labels = scales::comma,
                breaks = c(1, 10, 100, 1000, 10000)) +
  labs(x = "log(Number of Species)",
       y = "log(Number of SOR)")+
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)))

# Save figure as .png
ggsave(filename = here("figures", "Figure8_SOR_vs_species_per_polygon.png"),
       plot = figure8,
       width = 20,
       height = 16,
       dpi = 600)

# Save figure as .pdf
ggsave(filename = here("figures", "Figure8_SOR_vs_species_per_polygon.pdf"),
       plot = figure8,
       width = 20,
       height = 16,
       dpi = 600)

# 10. FIGURE 3 - MUNICIPALITY MAP OF % SOR IN DEVELOPMENT POLYGONS -------------

# Set projection
project_crs <- 25833

## 10.1. Prepare municipality boundaries ---------------------------------------

# Full boundaries (used to assign pairs to municipalities)
municipalities_full <- st_read(here("data", "raw_data",
                                    "Basisdata_0000_Norge_25833_Kommune_GeoJSON.geojson")) |>
  st_transform(project_crs) |>
  mutate(kommunenummer = as.character(kommunenummer))

# Land-clipped boundaries (used for plotting only)
norway_land <- geodata::gadm(country = "NOR", level = 0,
                             path = tempdir(), version = "latest") |>
  st_as_sf() |>
  st_transform(project_crs)

# Combine the two
norway_municipalities_sf <- st_intersection(municipalities_full, norway_land)

## 10.2. Assign each pair to a current (2024) municipality ---------------------

# Read in polygon data if needed
if (!exists("polygon_buffer_data")) {
  polygon_buffer_data <- readRDS(here("data", "derived_data",
                                      "polygon_buffer_data.rds"))
}


# The planning data use pre-2024 kommunenummer (e.g. Viken 30xx), which are not
# in the boundary file, so pairs are assigned spatially from the development
# polygon's location instead
pair_kommune <- polygon_buffer_data |>
  filter(polygon_type == "Development") |>
  dplyr::select(pair_id) |>
  st_transform(project_crs) |>
  st_point_on_surface() |>
  st_join(municipalities_full |> dplyr::select(kommunenummer),
          join = st_intersects, left = TRUE)

# Points that fall outside every municipality go to the nearest one
no_match <- is.na(pair_kommune$kommunenummer)
cat("Pairs assigned by nearest municipality:", sum(no_match), "\n")
if (any(no_match)) {
  nearest <- st_nearest_feature(pair_kommune[no_match, ], municipalities_full)
  pair_kommune$kommunenummer[no_match] <- municipalities_full$kommunenummer[nearest]
}

# One municipality per pair (a point exactly on a border can match two)
pair_kommune <- pair_kommune |>
  st_drop_geometry() |>
  distinct(pair_id, .keep_all = TRUE)

# Check if the number of kommune pairs is contained in the model data
stopifnot(nrow(pair_kommune) == n_distinct(model_data$pair_id),
          !any(is.na(pair_kommune$kommunenummer)))

## 10.3. One row per development-buffer pair -----------------------------------

# Create a df with one row per development-buffer pair
map_pairs <- model_data |>
  dplyr::select(pair_id, polygon_type, n_occurrences) |>
  tidyr::pivot_wider(names_from  = polygon_type,
                     values_from = n_occurrences) |>
  rename(sor_polygon = Development,
         sor_buffer  = Buffer) |>
  left_join(pair_kommune, by = "pair_id")

# Check for NA values
stopifnot(nrow(map_pairs) == n_distinct(model_data$pair_id),
          !any(is.na(map_pairs$sor_polygon)),
          !any(is.na(map_pairs$sor_buffer)),
          !any(is.na(map_pairs$kommunenummer)))

## 10.4. Municipality ratios and percentages -----------------------------------

# Calculate ratios and %s for municipalities
municipality_or <- map_pairs |>
  group_by(kommunenummer) |>
  # discordant-pair counts first, before sor_polygon/sor_buffer are summed
  summarise(n_pairs = n(),
            n_dev_only = sum(sor_polygon > 0 & sor_buffer == 0),
            n_buffer_only = sum(sor_polygon == 0 & sor_buffer > 0),
            sor_polygon = sum(sor_polygon),
            sor_buffer = sum(sor_buffer),
            .groups = "drop") |>
  mutate(sor_total = sor_polygon + sor_buffer,
         n_discordant = n_dev_only + n_buffer_only,
         # Map 1: ratio of SOR in development polygons to SOR in buffers (+1)
         share_or = ifelse(sor_total > 0,
                            (sor_polygon + 1) / (sor_buffer + 1), NA_real_),
         # ...and the same expressed as % of SOR in development polygons
         share_pct = ifelse(sor_total > 0,
                            100 * (sor_polygon + 1) / (sor_total + 2), NA_real_),
         # Map 2: matched-pair (McNemar) odds ratio of holding any SOR (+0.5)
         presence_or = ifelse(n_discordant > 0,
                               (n_dev_only + 0.5) / (n_buffer_only + 0.5), NA_real_),
         # ...and the same expressed as % of discordant pairs where only the
         # development polygon holds SOR
         presence_pct = ifelse(n_discordant > 0,
                               100 * (n_dev_only + 0.5) / (n_discordant + 1), NA_real_))

# Quick summary
cat("Municipalities with pairs:", nrow(municipality_or), "\n")
cat("Municipalities with a Map 1 value:", sum(!is.na(municipality_or$share_pct)), "\n")
cat("Municipalities with a Map 2 value:", sum(!is.na(municipality_or$presence_pct)), "\n")

#  Save as Supplementary table
write.csv(municipality_or,
          here("data", "derived_data", "municipality_odds_ratios.csv"),
          row.names = FALSE)

## 10.5. Percentage classes (shared by maps and spread figures) ----------------

# Symmetric around 50% (= equal SOR in polygons and buffers)
pct_breaks <- c(0, 10, 25, 40, 60, 75, 90, 100)
pct_labels <- c("< 10%", "10\u201325%", "25\u201340%", "40\u201360%",
                "60\u201375%", "75\u201390%", "\u2265 90%")

# Orange = more in buffers, purple = more in development polygons (as in Fig. 1)
pct_colours <- c(setNames(scales::brewer_pal(palette = "PuOr")(7), pct_labels),
                 "No SOR" = "grey85")

# Inner class boundaries expressed as ratios (p / (100 - p)), for the spread plots
inner_pct   <- pct_breaks[-c(1, length(pct_breaks))]
inner_ratio <- inner_pct / (100 - inner_pct)

# Print the spread and the number of municipalities per class
check_spread <- function(ratio_col, pct_col) {
  d <- municipality_or |> filter(!is.na(.data[[ratio_col]]))
  cat("\n", ratio_col, "- municipalities:", nrow(d), "\n")
  print(round(quantile(d[[ratio_col]],
                       c(0, 0.01, 0.05, 0.10, 0.25, 0.50,
                         0.75, 0.90, 0.95, 0.99, 1)), 3))
  print(table(cut(d[[pct_col]], breaks = pct_breaks, labels = pct_labels,
                  right = FALSE, include.lowest = TRUE)))
}
check_spread("share_or", "share_pct")
check_spread("presence_or", "presence_pct")

## 10.6. Supplementary figures - spread of the municipality ratios -------------

# Function to plot the ratio spread
plot_ratio_spread <- function(ratio_col, y_col, x_lab, y_lab, pct_axis_lab) {
  
  d <- municipality_or |>
    filter(!is.na(.data[[ratio_col]])) |>
    mutate(ratio = .data[[ratio_col]],
           y= .data[[y_col]])
  
  # bottom axis: ratio (log scale); top axis: matching % class boundaries
  ratio_axis <- scale_x_log10(breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
                              labels = c("0.001", "0.01", "0.1", "1",
                                         "10", "100", "1,000"),
                              sec.axis = sec_axis(~ .,
                                                  breaks = inner_ratio,
                                                  labels = paste0(inner_pct, "%"),
                                                  name   = pct_axis_lab))
  # histogram
  p_hist <- ggplot(d, aes(x = ratio)) +
    geom_histogram(bins = 40, fill = "#5E3C99", colour = "white") +
    geom_vline(xintercept = inner_ratio, linetype = "dashed", colour = "grey40") +
    geom_vline(xintercept = 1, colour = "black") +
    ratio_axis +
    labs(x = x_lab, y = "Number of municipalities") +
    theme_classic(base_size = 14)
  
  # scatter plot
  p_scatter <- ggplot(d, aes(x = ratio, y = y)) +
    geom_point(alpha = 0.6, colour = "#5E3C99") +
    geom_vline(xintercept = inner_ratio, linetype = "dashed", colour = "grey40") +
    geom_vline(xintercept = 1, colour = "black") +
    ratio_axis +
    scale_y_log10(labels = scales::comma) +
    labs(x = x_lab, y = y_lab) +
    theme_classic(base_size = 14)
  
  # combine the two
  plot_grid(p_hist, p_scatter, labels = c("a)", "b)"))
}

# Spread of the SOR ratio (Map 1)
(figureS_share_spread <- plot_ratio_spread("share_or", "sor_total",
                                           "Ratio of SOR (Development Polygon / Buffer)",
                                           "Total SOR in pairs",
                                           "% of SOR in development polygons"))

# Save supplementary figure
ggsave(here("figures", "FigureS_municipality_SOR_ratio_spread.png"),
       plot = figureS_share_spread, width = 16, height = 8, dpi = 600)
ggsave(here("figures", "FigureS_municipality_SOR_ratio_spread.pdf"),
       plot = figureS_share_spread, width = 16, height = 8, device = cairo_pdf)

# Spread of the presence odds ratio (Map 2)
(figureS_presence_spread <- plot_ratio_spread("presence_or", "n_discordant",
                                              "Odds ratio of holding any SOR (Development Polygon / Buffer)",
                                              "Number of discordant pairs",
                                              "% of discordant pairs with SOR only in development polygon"))

# Save supplementary figure
ggsave(here("figures", "FigureS_municipality_presence_OR_spread.png"),
       plot = figureS_presence_spread, width = 16, height = 8, dpi = 600)
ggsave(here("figures", "FigureS_municipality_presence_OR_spread.pdf"),
       plot = figureS_presence_spread, width = 16, height = 8, device = cairo_pdf)

## 10.7. Maps ------------------------------------------------------------------

# Function to plot % map
plot_pct_map <- function(pct_col, legend_title) {
  
  map_sf <- norway_municipalities_sf |>
    left_join(municipality_or, by = "kommunenummer") |>
    mutate(pct_class = cut(.data[[pct_col]], breaks = pct_breaks,
                           labels = pct_labels, right = FALSE,
                           include.lowest = TRUE),
           pct_class = forcats::fct_na_value_to_level(pct_class, level = "No SOR"))
  
  ggplot(map_sf) +
    geom_sf(aes(fill = pct_class), colour = "grey60", linewidth = 0.05) +
    scale_fill_manual(name   = legend_title,
                      values = pct_colours,
                      breaks = c(rev(pct_labels), "No SOR"),
                      drop   = FALSE) +
    annotation_north_arrow(location = "tl", which_north = "true",
                           pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                           style = north_arrow_fancy_orienteering()) +
    annotation_scale(location = "bl", width_hint = 0.25,
                     pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = "right",
          legend.title = element_text(size = 14),
          legend.text  = element_text(size = 13),
          legend.key.size = unit(0.8, "cm"))
}

# Figure 3 (to use in the main text)
(figure3 <- plot_pct_map("share_pct",
                         "% of SOR in\nDevelopment Polygons\n(vs Buffers)"))

# Save figure to file
ggsave(here("figures", "Figure3_municipality_map_SOR_pct.png"),
       plot = figure3, width = 12, height = 14, dpi = 600)
ggsave(here("figures", "Figure3_municipality_map_SOR_pct.pdf"),
       plot = figure3, width = 12, height = 14, device = cairo_pdf)

# Supplementary figure
(figureS_presence <- plot_pct_map("presence_pct",
                                  "% of pairs\nwith SOR only in the\nDevelopment Polygon"))

# Save figure to file
ggsave(here("figures", "FigureS_municipality_map_presence_pct.png"),
       plot = figureS_presence, width = 12, height = 14, dpi = 600)
ggsave(here("figures", "FigureS_municipality_map_presence_pct.pdf"),
       plot = figureS_presence, width = 12, height = 14, device = cairo_pdf)

# END OF SCRIPT ----------------------------------------------------------------