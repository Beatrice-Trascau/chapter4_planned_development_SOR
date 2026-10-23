##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 9.1_supplementary_figures
##----------------------------------------------------------------------------##

# 1. SETUP ---------------------------------------------------------------------

# Load libraries
library(here)
source(here("scripts", "0_setup.R"))

# Land-cover order (follows the ecotype codes in script 3.5)
# (defined here because it is needed when the factor levels are set below)
lc_levels <- c("Settlements", "Cropland", "Grassland", "Forest",
               "Heathland", "Sparsely_vegetated", "Wetlands")

# Load data
# Model-ready dataset (one row per polygon/buffer) created in script 3.8
model_data <- readRDS(here("data", "derived_data", "h2_polygon_buffer_data.rds"))

# Set factor orders for plotting
plot_data <- model_data |>
  mutate(polygon_type    = factor(polygon_type, levels = c("Development", "Buffer")),
         land_cover_name = factor(land_cover_name, levels = lc_levels))

# Check that every land-cover class is in lc_levels
stopifnot(!any(is.na(plot_data$land_cover_name)),
          !any(is.na(plot_data$polygon_type)))

## 1.1. Colours and labels -----------------------------------------------------

# Set colour scale for figures
type_colours <- c("Development" = "#5E3C99", "Buffer" = "#E66101")

# Set legend labels for polygon type
type_labels <- c("Development" = "Development Polygon", "Buffer" = "Buffer")

# Land-cover labels: "Sparsely_vegetated" -> "Sparsely Vegetated"
pretty_lc <- function(x) {
  x <- gsub("_", " ", x)
  gsub("(^|\\s)([a-z])", "\\1\\U\\2", x, perl = TRUE)
}

## 1.2. Shared themes ----------------------------------------------------------

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

## 1.3. Broken axis histogram --------------------------------------------------

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
  
  # Legend (only added to the panel that should carry it), placed top-right
  legend_layer <- if (show_legend) {
    legend_theme +
      theme(legend.position.inside      = c(0.98, 0.97),
            legend.justification.inside = c(1, 1))
  } else theme(legend.position = "none")
  
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
  
  # Bottom panel: every bar capped at the top of this panel
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
    theme(plot.margin = margin(3, 5.5, 5.5, 5.5))
  
  # Top panel: only the part of the tall bars above the break; carries the legend
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
          plot.margin  = margin(5.5, 5.5, 3, 5.5)) +
    legend_layer
  
  # Stack the panels and add one shared y-axis title
  panels  <- plot_grid(top, bottom, ncol = 1, align = "v",
                       rel_heights = c(1, 3))
  y_title <- ggdraw() + draw_label(y_lab, angle = 90, size = 16)
  
  plot_grid(y_title, panels, nrow = 1, rel_widths = c(0.05, 1))
}

## 1.4. Area scatter -----------------------------------------------------------

# Area (x) vs SOR/species (y) scatter, Development and Buffer
plot_area_scatter <- function(data, var, count_lab, show_legend = FALSE) {
  
  ggplot(data, aes(x = area_m2_numeric, y = .data[[var]] + 1,
                   colour = polygon_type, fill = polygon_type)) +
    geom_point(alpha = 0.2, size = 0.8, show.legend = FALSE) +
    geom_smooth(linewidth = 1.1, se = TRUE) +
    scale_colour_manual(values = type_colours, labels = type_labels, name = NULL) +
    scale_fill_manual(values = type_colours, labels = type_labels, name = NULL) +
    scale_x_log10(labels = scales::comma,
                  breaks = c(100, 1000, 10000, 100000, 1000000)) +
    scale_y_log10(labels = scales::comma,
                  breaks = c(1, 10, 100, 1000, 10000)) +
    labs(x = expression(paste("log(Polygon Area (m"^2, "))")),
         y = count_lab) +
    theme_classic() +
    theme(panel.grid = element_blank(),
          axis.title = element_text(size = 16),
          axis.text  = element_text(size = 16)) +
    if (show_legend) {
      legend_theme +
        theme(legend.position.inside      = c(0.02, 0.98),
              legend.justification.inside = c(0, 1))
    } else theme(legend.position = "none")
}

## 1.5. Example pair inset (replaces the legend in panel a) --------------------

# The inset shows one real Development Polygon - Buffer pair in the same colours
# as the histogram, with its SOR drawn as dots, so it also works as the legend.
# The occurrence coordinates are not stored in the script 3.8 outputs, so they
# are read from the cleaned GBIF file once and the chosen pair is cached.
# Delete the cache file (or set example_pair_id) to choose a different pair.

example_cache   <- here("data", "derived_data", "fig1a_example_pair.rds")
example_pair_id <- NULL         # set to a pair_id to force a specific pair

# Selection criteria for the automatic choice
example_sor_range     <- c(10, 40)  # SOR in BOTH the polygon and its buffer
example_min_locations <- 6          # distinct dot locations in BOTH areas

# Read coordinates for a set of gbifIDs, loading only the three needed columns
read_occurrence_coords <- function(ids) {
  occ_file  <- here("data", "derived_data", "clean_occurrences_1km.txt")
  occ_cols  <- names(read.csv(occ_file, nrows = 1))
  keep_cols <- c("gbifID", "decimalLongitude", "decimalLatitude")
  read.csv(occ_file,
           colClasses = ifelse(occ_cols %in% keep_cols, NA, "NULL")) |>
    filter(gbifID %in% ids,
           !is.na(decimalLongitude), !is.na(decimalLatitude)) |>
    distinct(gbifID, .keep_all = TRUE)
}

# Choose a pair (or use chosen_pair) and return its geometries and SOR points
build_example_pair <- function(model_data, chosen_pair = NULL,
                               sor_range = example_sor_range,
                               min_locations = example_min_locations) {
  
  # Polygon and buffer geometries (script 3.5) and SOR per area (script 3.8)
  geoms <- readRDS(here("data", "derived_data", "polygon_buffer_data.rds")) |>
    dplyr::select(pair_id, polygon_type)
  occ_join <- readRDS(here("data", "derived_data",
                           "h2d_polygon_buffer_occurrence_join.rds")) |>
    dplyr::select(gbifID, pair_id, polygon_type) |>
    mutate(polygon_type = as.character(polygon_type))
  
  if (is.null(chosen_pair)) {
    
    # Pairs where both the polygon and its buffer have a moderate number of SOR
    sor_ok <- model_data |>
      group_by(pair_id) |>
      summarise(ok = n() == 2 &
                  all(n_occurrences >= sor_range[1] &
                        n_occurrences <= sor_range[2]),
                .groups = "drop") |>
      filter(ok) |>
      pull(pair_id)
    
    # Single-part development polygons only, scored by compactness
    # (Polsby-Popper: 4 * pi * area / perimeter^2, where 1 = a circle)
    dev <- geoms |>
      filter(polygon_type == "Development", pair_id %in% sor_ok)
    single_part <- vapply(st_geometry(dev),
                          function(g) !inherits(g, "MULTIPOLYGON") || length(g) == 1,
                          logical(1))
    dev <- dev[single_part, ]
    dev$compactness <- 4 * pi * as.numeric(st_area(dev)) /
      as.numeric(st_length(st_boundary(dev)))^2
    
    candidates <- dev$pair_id
    cat("Candidate pairs for the inset:", length(candidates), "\n")
    stopifnot(length(candidates) > 0)
  } else {
    candidates <- chosen_pair
  }
  
  # Coordinates of the SOR in the candidate pairs
  cand_occ <- occ_join |> filter(pair_id %in% candidates)
  cand_occ <- cand_occ |>
    inner_join(read_occurrence_coords(unique(cand_occ$gbifID)), by = "gbifID")
  
  if (is.null(chosen_pair)) {
    
    # Many SOR share exact coordinates, so require enough distinct dot
    # locations in both areas, then take the most compact polygon
    locs <- cand_occ |>
      distinct(pair_id, polygon_type, decimalLongitude, decimalLatitude) |>
      count(pair_id, polygon_type) |>
      group_by(pair_id) |>
      summarise(min_locs = if (n() == 2) min(n) else 0L, .groups = "drop")
    
    chosen_pair <- dev |>
      st_drop_geometry() |>
      inner_join(locs, by = "pair_id") |>
      filter(min_locs >= min_locations) |>
      dplyr::slice_max(compactness, n = 1, with_ties = FALSE) |>
      pull(pair_id)
    
    if (length(chosen_pair) == 0) {
      stop("No pair meets the criteria - relax example_sor_range or ",
           "example_min_locations")
    }
  }
  
  pair_geoms <- geoms |>
    filter(pair_id == chosen_pair) |>
    mutate(polygon_type = as.character(polygon_type))
  pair_points <- cand_occ |>
    filter(pair_id == chosen_pair) |>
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) |>
    st_transform(st_crs(pair_geoms))
  
  # Check that the pair is complete and every dot falls in one of the two areas
  stopifnot(nrow(pair_geoms) == 2,
            setequal(pair_geoms$polygon_type, c("Development", "Buffer")),
            all(lengths(st_intersects(pair_points, pair_geoms)) > 0))
  
  cat("Example pair:", chosen_pair, "|",
      paste(names(table(pair_points$polygon_type)),
            table(pair_points$polygon_type), "SOR", collapse = ", "), "\n")
  
  list(pair_id = chosen_pair, geoms = pair_geoms, points = pair_points)
}

# Load the cached pair, or build it (slow the first time: reads the GBIF file)
if (file.exists(example_cache)) example_pair <- readRDS(example_cache)
if (!file.exists(example_cache) ||
    (!is.null(example_pair_id) && example_pair$pair_id != example_pair_id)) {
  example_pair <- build_example_pair(model_data, chosen_pair = example_pair_id)
  saveRDS(example_pair, example_cache)
}

# Map of the pair: buffer, development polygon and SOR dots (no labels)
plot_example_inset <- function(example, dot_alpha = 0.5) {
  
  # Draw the buffer first so the development polygon sits on top
  g <- example$geoms |>
    mutate(polygon_type = factor(polygon_type, levels = c("Buffer", "Development"))) |>
    arrange(polygon_type)
  
  ggplot() +
    geom_sf(data = g, aes(fill = polygon_type),
            colour = "grey25", linewidth = 0.3, alpha = 0.85) +
    # dot_alpha sets how see-through the dots are (1 = fully dark)
    geom_sf(data = example$points, shape = 21, size = 1.8,
            fill = "black", colour = "white", stroke = 0.3,
            alpha = dot_alpha) +
    scale_fill_manual(values = type_colours) +
    # annotation_scale(location = "bl", width_hint = 0.25, style = "ticks",
    #                  text_cex = 1, line_width = 0.6) +
    coord_sf(datum = NA, expand = TRUE, clip = "off") +
    theme_void() +
    theme(legend.position = "none",
          plot.background = element_rect(fill = scales::alpha("white", 0.8),
                                         colour = NA))
}

example_inset <- plot_example_inset(example_pair)

# Position of the inset within panel a (fractions of the panel; tweak if needed)
inset_pos <- c(x = 0.48, y = 0.50, width = 0.50, height = 0.47)

# 2. SHARED SETTINGS FOR ROWS 3-5 ----------------------------------------------

# Shared theme (matches the font sizes in rows 1-2)
row_theme <- theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 16),
        axis.text  = element_text(size = 16),
        legend.position = "none")

# x-axis labels for the two kinds of panel
type_x_axis <- scale_x_discrete(labels = type_labels)
lc_x_axis   <- scale_x_discrete(labels = function(x) stringr::str_wrap(pretty_lc(x), 10))

# Dodge used for Development/Buffer side by side within each land cover
dodge <- position_dodge(width = 0.85)

# 3. ROW 3: % OF AREAS WITH ANY SOR --------------------------------------------

## 3.1. Calculate proportions with Wilson 95% CIs ------------------------------

# Wilson score interval (behaves well near 0% and 100%)
wilson_ci <- function(x, n, z = qnorm(0.975)) {
  p      <- x / n
  denom  <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  half   <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom
  list(lower = centre - half, upper = centre + half)
}

# Proportion of areas holding at least one SOR, for any grouping
pct_with_sor <- function(data, ...) {
  data |>
    group_by(...) |>
    summarise(n_areas    = n(),
              n_with_sor = sum(n_occurrences > 0),
              .groups = "drop") |>
    mutate(prop  = n_with_sor / n_areas,
           lower = wilson_ci(n_with_sor, n_areas)$lower,
           upper = wilson_ci(n_with_sor, n_areas)$upper)
}

# By area type, and by land cover x area type
pct_by_type <- pct_with_sor(plot_data, polygon_type)
pct_by_lc   <- pct_with_sor(plot_data, land_cover_name, polygon_type)

# Quick check
print(pct_by_type)
print(pct_by_lc, n = Inf)

# Save as tables (useful for reporting the numbers in the text)
# write.csv(pct_by_type,
#           here("data", "derived_data", "pct_areas_with_SOR_by_type.csv"),
#           row.names = FALSE)
# write.csv(pct_by_lc,
#           here("data", "derived_data", "pct_areas_with_SOR_by_landcover.csv"),
#           row.names = FALSE)

## 3.2. Plot panels e and f ----------------------------------------------------

# Shared y axis for the two panels so they can be compared directly
pct_y_max  <- max(c(pct_by_type$upper, pct_by_lc$upper)) * 1.05
pct_y_axis <- scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                                 limits = c(0, pct_y_max),
                                 expand = expansion(mult = c(0, 0)))

# Panel e: by area type
fig_e <- ggplot(pct_by_type, aes(x = polygon_type, y = prop, fill = polygon_type)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.7) +
  scale_fill_manual(values = type_colours) +
  type_x_axis +
  pct_y_axis +
  labs(x = NULL, y = "Areas with SOR (%)") +
  row_theme

# Panel f: by land cover, Development and Buffer side by side
fig_f <- ggplot(pct_by_lc, aes(x = land_cover_name, y = prop, fill = polygon_type)) +
  geom_col(position = dodge, width = 0.8) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = dodge, width = 0.25, linewidth = 0.7) +
  scale_fill_manual(values = type_colours) +
  lc_x_axis +
  pct_y_axis +
  labs(x = NULL, y = "Areas with SOR (%)") +
  row_theme

# 4. ROWS 4-5: VIOLIN PLOTS OF SOR AND SPECIES (AREAS WITH >= 1 RECORD) --------

# Violin + narrow boxplot on a log10 y axis, for areas where var > 0
# x_var is either "polygon_type" (panels g, i) or "land_cover_name" (h, j)
plot_violin <- function(data, var, x_var, y_lab, y_limits = NULL) {
  
  d <- data |> filter(.data[[var]] > 0)
  
  ggplot(d, aes(x = .data[[x_var]], y = .data[[var]])) +
    geom_violin(aes(fill = polygon_type),
                position = dodge, scale = "width", width = 0.8,
                colour = NA, alpha = 0.8) +
    # explicit group so the white boxplots dodge the same way as the violins
    geom_boxplot(aes(group = interaction(.data[[x_var]], polygon_type)),
                 position = dodge, width = 0.12, fill = "white",
                 outlier.shape = NA, linewidth = 0.5) +
    scale_fill_manual(values = type_colours) +
    scale_y_log10(labels = scales::comma,
                  breaks = c(1, 10, 100, 1000, 10000),
                  limits = y_limits) +
    labs(x = NULL, y = y_lab) +
    row_theme
}

# Shared y ranges so the left and right panel in each row match
sor_range     <- range(plot_data$n_occurrences[plot_data$n_occurrences > 0])
species_range <- range(plot_data$n_species[plot_data$n_species > 0])

# Row 4: number of SOR, given at least one SOR
fig_g <- plot_violin(plot_data, "n_occurrences", "polygon_type",
                     "log(Number of SOR)", sor_range) + type_x_axis
fig_h <- plot_violin(plot_data, "n_occurrences", "land_cover_name",
                     "log(Number of SOR)", sor_range) + lc_x_axis

# Row 5: number of species, given at least one species
fig_i <- plot_violin(plot_data, "n_species", "polygon_type",
                     "log(Number of Species)", species_range) + type_x_axis
fig_j <- plot_violin(plot_data, "n_species", "land_cover_name",
                     "log(Number of Species)", species_range) + lc_x_axis

# 5. ROWS 1-2: SOR AND SPECIES PER AREA ----------------------------------------

# Row 1: panel a has no legend; the example pair inset takes its place
fig1a <- ggdraw(plot_broken_hist(plot_data, "n_occurrences", "Number of SOR")) +
  draw_plot(example_inset,
            x = inset_pos["x"], y = inset_pos["y"],
            width = inset_pos["width"], height = inset_pos["height"])
fig1b <- plot_area_scatter(plot_data, "n_occurrences",
                           "log(Number of SOR + 1)", show_legend = TRUE)

# Row 2 without legends
fig2a <- plot_broken_hist(plot_data, "n_species", "Number of Species")
fig2b <- plot_area_scatter(plot_data, "n_species", "log(Number of Species + 1)")

# 6. COMBINE INTO ONE FIGURE ---------------------------------------------------

# Relative widths for rows 3-5 (left panel has 2 categories, right has 7 x 2)
cat_widths <- c(1, 2)

row1 <- plot_grid(fig1a, fig1b, labels = c("a)", "b)"))
row2 <- plot_grid(fig2a, fig2b, labels = c("c)", "d)"))
row3 <- plot_grid(fig_e, fig_f, labels = c("e)", "f)"),
                  rel_widths = cat_widths, align = "h", axis = "tb")
row4 <- plot_grid(fig_g, fig_h, labels = c("g)", "h)"),
                  rel_widths = cat_widths, align = "h", axis = "tb")
row5 <- plot_grid(fig_i, fig_j, labels = c("i)", "j)"),
                  rel_widths = cat_widths, align = "h", axis = "tb")

# Stack the rows (rows 3-5 a bit shorter than the histogram/scatter rows)
figure_combined <- plot_grid(row1, row2, row3, row4, row5,
                             ncol = 1, rel_heights = c(1, 1, 0.75, 0.75, 0.75))

# 7. SAVE FIGURE ---------------------------------------------------------------

# Save figure as .png
ggsave(filename = here("figures", "Figure1_SOR_species_combined.png"),
       plot = figure_combined,
       width = 17,
       height = 23.5,
       dpi = 600,
       limitsize = FALSE)

# Save figure as .pdf
ggsave(filename = here("figures", "Figure1_SOR_species_combined.pdf"),
       plot = figure_combined,
       width = 17,
       height = 23.5,
       limitsize = FALSE)

# END OF SCRIPT ----------------------------------------------------------------