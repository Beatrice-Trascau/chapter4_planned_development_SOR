##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 3.2_municipality_GBIF_planlagt
# This script contains code which creates a map of Norway where each 
# municipality is coloured by the percentage of total species occurrence 
# records (SOR) that fall within planned development polygons
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source setup file
library(here)
source(here("scripts", "0_setup.R"))

# Load cleaned occurrence records
clean_occurrences_70m <- read.csv(here("data", "derived_data",
                                        "clean_occurrences_70m.txt"))

# Load polygon dataframe
polygon_all_data <- readRDS(here("data", "derived_data",
                                 "polygons_occurrences_all_data.rds"))

# Load municipality boundaries downloaded from GeoNorge
norway_municipalities_sf <- st_read(here("data", "raw_data",
                                         "Basisdata_0000_Norge_25833_Kommune_GeoJSON.geojson"))


# 2. PREPARE SPATIAL  DATA -----------------------------------------------------

## 2.1. Prepare municipality boundaries ----------------------------------------

# Define CRS to use throughout (ETRS89 / UTM zone 33N - matches development polygons)
project_crs <- 25833

# Check column names to confirm kommunenummer column name
colnames(norway_municipalities_sf)

# Verify CRS matches project CRS - transform if needed
if (st_crs(norway_municipalities_sf)$epsg != project_crs) {
  norway_municipalities_sf <- st_transform(norway_municipalities_sf, project_crs)
}

# Download Norway land boundary and clip municipalities to it
# this removes marine areas that extend offshore for some coastal municipalities
norway_land <- geodata::gadm(country = "NOR", level = 0,
                             path = tempdir(), version = "latest") |>
  st_as_sf() |>
  st_transform(project_crs)

# Clip municipality boundaries to land
norway_municipalities_sf <- st_intersection(norway_municipalities_sf, norway_land)

## 2.2. Convert occurrences to sf spatial points -------------------------------

# Convert occurrences to sf spatial points (GBIF uses WGS84)
occurrences_sf <- clean_occurrences_70m |>
  filter(!is.na(decimalLongitude),
         !is.na(decimalLatitude)) |>
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = 4326)

# Transform occurrences to project CRS
occurrences_sf <- st_transform(occurrences_sf, project_crs)

# Verify CRS match
stopifnot(st_crs(norway_municipalities_sf) == st_crs(occurrences_sf))

# 3. COMBINE OCCURRENCES & MUNICIPALITIES --------------------------------------

## 3.1. Calculate total occurrences per municiapality --------------------------

# Spatial join occurrences to municipality boundaries
# left = FALSE drops occurrences outside any municipality (e.g. offshore)
# Only keep kommunenummer for grouping - keeps object lightweight
occurrence_municipality_join <- st_join(occurrences_sf |> select(gbifID),
                                        norway_municipalities_sf |> select(kommunenummer),
                                        join = st_intersects,
                                        left = FALSE)

# Count total occurrences per municipality
total_sor_per_municipality <- occurrence_municipality_join |>
  st_drop_geometry() |>
  group_by(kommunenummer) |>
  summarise(total_sor = n(), .groups = "drop")

## 3.2. Calculate occurrences within development polygons per municipality -----

# Sum occurrences within development polygons per municipality
# using kommunenummer column in polygon_all_data
polygon_sor_per_municipality <- polygon_all_data |>
  group_by(kommunenummer) |>
  summarise(polygon_sor = sum(n_occurrences), .groups = "drop")

# Join total SOR and polygon SOR by kommunenummer
municipality_pct <- total_sor_per_municipality |>
  left_join(polygon_sor_per_municipality, by = "kommunenummer") |>
  # Replace NA polygon SOR with 0 (municipalities with no development polygons)
  mutate(polygon_sor     = ifelse(is.na(polygon_sor), 0, polygon_sor),
         pct_in_polygons = (polygon_sor / total_sor) * 100)

# Quick check
cat("Municipalities with total SOR data:  ", nrow(municipality_pct), "\n")
cat("Municipalities with polygon SOR > 0: ", sum(municipality_pct$polygon_sor > 0), "\n")
cat("Municipalities with zero total SOR:  ", sum(municipality_pct$total_sor == 0), "\n")

# 4. PLOTTING ------------------------------------------------------------------

# Calculate colour scale limits from data (1% to maximum observed %)
pct_max <- ceiling(max(municipality_pct$pct_in_polygons, na.rm = TRUE))

norway_map_data <- norway_municipalities_sf |>
  left_join(municipality_pct, by = "kommunenummer") |>
   # Set to NA if: no total SOR, or polygon SOR is 0 (no development polygons
    # or no occurrences within them) - these will display as grey
  mutate(pct_in_polygons = ifelse(is.na(total_sor) | polygon_sor == 0,
                                  NA, pct_in_polygons))

# Plot map 
figure3 <- ggplot(norway_map_data) +
  geom_sf(aes(fill = pct_in_polygons), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(name = "% of SOR within\ndevelopment polygons",
                       option = "viridis", na.value = "grey80",
                       limits = c(1, pct_max), 
                       breaks = round(seq(1, pct_max, length.out = 5)),
                       labels = paste0(round(seq(1, pct_max, length.out = 5)), "%")) +
  annotation_north_arrow(location = "tl",
                         which_north = "true",
                         pad_x = unit(0.2, "cm"),
                         pad_y = unit(0.2, "cm"),
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "bl",
                   width_hint = 0.25,
                   pad_x = unit(0.5, "cm"),
                   pad_y = unit(0.5, "cm")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))

# Save figure as .png
ggsave(filename = here("figures", "Figure3_municipality_map_pct_SOR.png"),
       plot     = figure3,
       width    = 12,
       height   = 14,
       dpi      = 600)

# Save figure as .pdf
ggsave(filename = here("figures", "Figure3_municipality_map_pct_SOR.pdf"),
       plot     = figure3,
       width    = 12,
       height   = 14,
       dpi      = 600)

# END OF SCRIPT ----------------------------------------------------------------