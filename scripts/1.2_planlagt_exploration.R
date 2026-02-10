##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 1.2_planlagt_exploration
# This script contains code which explores the the "Planned 
# development area in Norway" dataset from NINA
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source setup file
library(here)
source(here("scripts", "0_setup.R"))

# Load development polygons
development_polygons <- here("data", "raw_data", "nina_planagt.gpkg")

# 2. CREATE SUMMARY STATISTICS TABLE  ------------------------------------------

## 2.1. Quick initial data exploration -----------------------------------------

# Check structure
nrow(development_polygons) #133 644 polygons

# Check CRS
st_crs(development_polygons)$input #ETRS89 / UTM zone 33N

# Check column names
colnames(development_polygons) # I guess arealformalsgruppe is the column with the planned category?

# Check which categories
unique(development_polygons$arealformalsgruppe) # Looks like the ones on NINA's page

# Check if there are any NA values for group
sum(is.na(development_polygons$arealformalsgruppe)) #0

## 2.2. Create summary statistics table ----------------------------------------

# Alredy have a colum givin area in m2 (planlagt_areal_m2) - but it is detected as <chr> by R, convert to numeric
development_polygons$area_m2_numeric <- as.numeric(development_polygons$planlagt_areal_m2)

# Calculate summary statistics by category
summary_stats <- development_polygons |>
  st_drop_geometry() |>
  group_by(arealformalsgruppe) |>
  summarise(n_polygons = n(),
            total_area_m2 = sum(area_m2_numeric, na.rm = TRUE),
            mean_area_m2 = mean(area_m2_numeric, na.rm = TRUE),
            median_area_m2 = median(area_m2_numeric, na.rm = TRUE),
            .groups = "drop") |>
  arrange(arealformalsgruppe)

# Print summary statistics
print(summary_stats, n = Inf)

# Save summary statistics to csv
write.csv(summary_stats, 
          here("figures", "TableS1_development_polygons_summary_stats.csv"),
          row.names = FALSE)

# 3. CREATE MAP OF POLYGONS ----------------------------------------------------

## 3.1. Download Norway shapefile ----------------------------------------------

# Download Norway boundary
norway <- geodata::gadm(country = "NOR", level = 0, path = tempdir(), version = "latest")

# Convert to sf object
norway_sf <- st_as_sf(norway)

# Transform to match the CRS of development polygons
norway_sf33 <- st_transform(norway_sf, st_crs(development_polygons))

## 3.2. Create map -------------------------------------------------------------

# Translate category names
development_polygons <- development_polygons |>
  mutate(english_categories = case_when(arealformalsgruppe == "01 Bolig eller sentrumsformål" ~ "Residential",
                                        arealformalsgruppe == "02 Fritidsbebyggelse" ~ "Recreational",
                                        arealformalsgruppe == "03 Tjenesteyting" ~ "Services",
                                        arealformalsgruppe == "04 Handel" ~ "Retail",
                                        arealformalsgruppe == "05 Turistformål" ~ "Tourism",
                                        arealformalsgruppe == "06 Næringsvirksomhet" ~ "Commercial",
                                        arealformalsgruppe == "07 Råstoffutvinning" ~ "Mining",
                                        arealformalsgruppe == "08 Kombinerte formål" ~ "Combined",
                                        arealformalsgruppe == "13 Forsvaret" ~ "Defense",
                                        arealformalsgruppe == "16 Havner og småbåthavner" ~ "Ports"))

# Create factor for mapping to ensure consistent colours
development_polygons$category_factor <- factor(development_polygons$english_categories)

# Create colourblind-friendly colour palette
n_categories <- length(unique(development_polygons$english_categories))
color_palette <- viridis(n_categories, option = "turbo")

# Create map
polygon_map <- ggplot() +
   geom_sf(data = norway_sf33,
          fill = "grey95",
          color = "black",
          linewidth = 0.2) +
  geom_sf(data = development_polygons, 
          aes(fill = category_factor),
          color = NA,
          alpha = 0.7) +
  scale_fill_manual(values = color_palette,
                    name = "Development Category") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 8)) +
  annotation_north_arrow(location = "tl", 
                         which_north = "true",
                         pad_x = unit(0.2, "cm"),
                         pad_y = unit(0.2, "cm"),
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "bl", 
                   width_hint = 0.25,
                   pad_x = unit(0.5, "cm"),
                   pad_y = unit(0.5, "cm"))

# Save map as .png
ggsave(filename = here("figures", "FigureS1_Map_of_Planned_Developments.png"),
       plot = polygon_map,
       width = 12,
       height = 14,
       dpi = 600)

# Save map as .pdf
ggsave(filename = here("figures", "FigureS1_Map_of_Planned_Developments.pdf"),
       plot = polygon_map,
       width = 12,
       height = 14,
       dpi = 600)

# 4. CREATE VIOLIN PLOTS -------------------------------------------------------

## 4.1. Raw (non-logged) violin plot -------------------------------------------

# Create violin plot
violin_plot_linear <- ggplot(development_polygons, 
                             aes(x = english_categories, 
                                 y = area_m2_numeric,
                                 fill = english_categories)) +
  geom_violin(alpha = 0.7, draw_quantiles = c(0.5)) +
  scale_fill_manual(values = color_palette) +
  scale_y_continuous(labels = comma) +
  labs(x = "Development Category",
       y = expression(paste("Polygon Area (m"^2, ")"))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "none")

## 4.2. Logged violin plot -----------------------------------------------------

# Add small constant to avoid logging 0 values
development_polygons$area_m2_log <- development_polygons$area_m2_numeric + 1

# Create violin plot with log y-axis
violin_plot_log <- ggplot(development_polygons, 
                          aes(x = english_categories, 
                              y = area_m2_log,
                              fill = english_categories)) +
  geom_violin(alpha = 0.7, draw_quantiles = c(0.5)) +
  scale_fill_manual(values = color_palette) +
  scale_y_log10(labels = comma,
                breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000)) +
  labs(x = "Development Category",
       y = expression(paste("Polygon Area (m"^2, ") - log scale"))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        legend.position = "none")

# Save figure as .png
ggsave(filename = here("figures", "FigureS2_Violins_of_Planned_Developments_Area.png"),
       plot = violin_plot_log,
       width = 12,
       height = 14,
       dpi = 600)

# Save figure as .pdf
ggsave(filename = here("figures", "FigureS2_Violins_of_Planned_Developments_Area.pdf"),
       plot = violin_plot_log,
       width = 12,
       height = 14,
       dpi = 600)

# END OF SCRIPT ----------------------------------------------------------------