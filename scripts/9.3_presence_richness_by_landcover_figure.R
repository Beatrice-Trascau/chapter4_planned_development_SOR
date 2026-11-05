##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 9.2_presence_richness_by_landcover_figure
##----------------------------------------------------------------------------##

# 1. SETUP ---------------------------------------------------------------------

# Load libraries
library(here)
source(here("scripts", "0_setup.R"))

# Land-cover order (alphabetical, top to bottom)
lc_levels <- sort(c("Settlements", "Cropland", "Grassland", "Forest",
                    "Heathland", "Sparsely_vegetated", "Wetlands"))

# Colours and legend labels (same as the other figures)
type_colours <- c("Development" = "#5E3C99", "Buffer" = "#E66101")
type_labels  <- c("Development" = "Development Polygon", "Buffer" = "Buffer")

# Land-cover labels: "Sparsely_vegetated" -> "Sparsely Vegetated"
pretty_lc <- function(x) {
  x <- gsub("_", " ", x)
  gsub("(^|\\s)([a-z])", "\\1\\U\\2", x, perl = TRUE)
}

# 2. LOAD DATA AND MODELS ------------------------------------------------------

# Model-ready dataset (one row per polygon/buffer) created in script 3.8
model_data <- readRDS(here("data", "derived_data", "h2_polygon_buffer_data.rds"))

# Rebuild the model data exactly as in scripts 4.1 and 4.2. Both models centre
# log-area on the same long dataset, so the two columns share one x axis
fig_data <- model_data |>
  mutate(polygon_type    = factor(polygon_type, levels = c("Buffer", "Development")),
         log_area_c      = as.numeric(scale(log(area_m2_numeric), scale = FALSE)),
         land_cover_name = factor(land_cover_name),
         kommune_factor  = factor(kommune),
         pair_id_factor  = factor(pair_id))

# Check that every land-cover class is in lc_levels
stopifnot(all(levels(fig_data$land_cover_name) %in% lc_levels))

# Load the selected models (saved in scripts 4.1 and 4.2)
load(here("data", "models", "h1_presence_full.RData"))  # h1_presence_full
load(here("data", "models", "h2c_zinb_full.RData"))     # h2c_zinb_full

# 3. PREDICTIONS ---------------------------------------------------------------

# Predict across area for each land-cover, then keep each land-cover x area type
# combination within its own observed log-area range (no extrapolation)
predict_by_lc <- function(model, data, type = "fixed", n = 100) {
  
  lcs <- levels(droplevels(data$land_cover_name))
  
  preds <- bind_rows(lapply(lcs, function(lc) {
    rng      <- range(data$log_area_c[data$land_cover_name == lc])
    seq_vals <- round(seq(rng[1], rng[2], length.out = n), 5)
    term_str <- paste0("log_area_c [", paste(seq_vals, collapse = ","), "]")
    ggpredict(model,
              terms     = c(term_str, "polygon_type"),
              condition = c(land_cover_name = lc),
              type      = type) |>
      as.data.frame() |>
      rename(log_area_c = x, polygon_type = group) |>
      mutate(land_cover_name = lc)
  }))
  
  ranges <- data |>
    group_by(land_cover_name, polygon_type) |>
    summarise(lo = min(log_area_c), hi = max(log_area_c), .groups = "drop") |>
    mutate(land_cover_name = as.character(land_cover_name),
           polygon_type    = as.character(polygon_type))
  
  preds |>
    mutate(polygon_type = as.character(polygon_type)) |>
    left_join(ranges, by = c("land_cover_name", "polygon_type")) |>
    filter(log_area_c >= lo, log_area_c <= hi) |>
    dplyr::select(-lo, -hi) |>
    mutate(land_cover_name = factor(land_cover_name, levels = lc_levels),
           polygon_type    = factor(polygon_type, levels = c("Development", "Buffer")))
}

# Left column: probability of containing any SOR
pred_presence <- predict_by_lc(h1_presence_full, fig_data)

# Right column: species richness, conditional (count part, as in script 4.2)
pred_richness <- predict_by_lc(h2c_zinb_full, fig_data, type = "fixed")

# Quick check: 7 land-covers x 2 area types in each
stopifnot(nrow(distinct(pred_presence, land_cover_name, polygon_type)) == 14,
          nrow(distinct(pred_richness, land_cover_name, polygon_type)) == 14)

# 4. SHARED PLOT SETTINGS ------------------------------------------------------

# x axis: centred log-area back-transformed to hectares, min and max marked
mean_log_area <- mean(log(fig_data$area_m2_numeric))
breaks_m2     <- c(min(fig_data$area_m2_numeric),
                   1e3, 1e4, 1e5, 1e6,
                   max(fig_data$area_m2_numeric))
ha_label <- function(x_m2) {
  v <- x_m2 / 1e4
  vapply(v, function(a) {
    if (a >= 1) formatC(a, format = "f", digits = 0, big.mark = ",")
    else        formatC(signif(a, 2), format = "g")
  }, character(1))
}
area_x_axis <- scale_x_continuous(breaks = log(breaks_m2) - mean_log_area,
                                  labels = ha_label(breaks_m2))

# Shared theme
fig_theme <- theme_classic() +
  theme(panel.grid       = element_blank(),
        axis.title       = element_text(size = 16),
        axis.text        = element_text(size = 13),
        plot.title       = element_text(size = 17, face = "bold", hjust = 0.5),
        panel.spacing.y  = grid::unit(0.6, "cm"),
        legend.position  = "none")

# Base for both columns: lines and ribbons by area type, one row per land-cover
pred_panel <- function(pred_df, title, scales) {
  
  # Set the row order here, so it always follows lc_levels when plotting
  pred_df <- pred_df |>
    mutate(land_cover_name = factor(as.character(land_cover_name),
                                    levels = lc_levels))
  stopifnot(!any(is.na(pred_df$land_cover_name)))
  
  ggplot(pred_df, aes(x = log_area_c, y = predicted,
                      colour = polygon_type, fill = polygon_type)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, colour = NA) +
    geom_line(linewidth = 1.1) +
    facet_grid(rows = vars(land_cover_name), scales = scales, switch = "y",
               # wrap long names ("Sparsely Vegetated") onto two lines
               labeller = labeller(land_cover_name = function(x)
                 stringr::str_wrap(pretty_lc(x), 10))) +
    scale_colour_manual(values = type_colours, labels = type_labels, name = NULL) +
    scale_fill_manual(values = type_colours, labels = type_labels, name = NULL) +
    area_x_axis +
    labs(x = "Area (ha)", y = NULL, title = title) +
    fig_theme
}


# 5. PLOT COLUMNS --------------------------------------------------------------

# Left column: shared y axis; land-cover names on the left of the panels
fig_presence <- pred_panel(pred_presence,
                           "Probability of Containing Any SOR",
                           scales = "fixed") +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  theme(strip.placement  = "outside",
        strip.background = element_blank(),
        # let the names extend past the strip, with room on the left for them
        strip.clip  = "off",
        plot.margin = margin(5.5, 5.5, 5.5, 25),
        strip.text.y.left = element_text(size = 15, face = "bold", angle = 0,
                                         hjust = 1))

# Right column: free y axis per row; no land-cover names (they are on the left)
fig_richness <- pred_panel(pred_richness,
                           "Predicted Species Richness",
                           scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  theme(strip.text = element_blank(),
        strip.background = element_blank())

# 6. COMBINE INTO ONE FIGURE ---------------------------------------------------

# One legend for the whole figure, taken from the left column
fig_legend <- get_legend(fig_presence +
                           theme(legend.position  = "bottom",
                                 legend.direction = "horizontal",
                                 legend.text = element_text(size = 16,
                                                            margin = margin(l = 6, r = 20)),
                                 legend.key.size = grid::unit(1, "cm")))

# Left column is wider because it carries the land-cover names
columns <- plot_grid(fig_presence, fig_richness,
                     nrow = 1, align = "h", axis = "tb",
                     rel_widths = c(1.25, 1))

figure_combined <- plot_grid(columns, fig_legend,
                             ncol = 1, rel_heights = c(1, 0.03))

# 7. SAVE FIGURE ---------------------------------------------------------------

# Save figure as .png
ggsave(filename = here("figures", "Figure_presence_richness_by_landcover.png"),
       plot = figure_combined,
       width = 15,
       height = 20,
       dpi = 600)

# Save figure as .pdf
ggsave(filename = here("figures", "Figure_presence_richness_by_landcover.pdf"),
       plot = figure_combined,
       width = 13,
       height = 20)

# END OF SCRIPT ----------------------------------------------------------------