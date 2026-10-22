##----------------------------------------------------------------------------##
# THESIS
# CLC2018_level2_map
# This script creates a map of CORINE Land Cover (CLC) Status 2018 for mainland
# Norway, reclassified to CLC Level 2, for the Methods section of the thesis
##----------------------------------------------------------------------------##

# 1. SETUP ---------------------------------------------------------------------

# Source setup file 
library(here)
source(here("scripts", "0_setup.R"))

# Define CRS to use for the map (ETRS89 / UTM zone 33N - matches project data)
project_crs <- 25833

# 2. LOAD DATA -----------------------------------------------------------------

# Load CLC Status 2018 (2024 still not available :()
clc <- rast(here("data", "raw_data", "U2018_CLC2018_V2020_20u1.tif"))

# Drop the attached attribute table so we work with the raw cell values
levels(clc) <- NULL

# Download mainland Norway boundary excluding Svalbard and Jan Mayen
norway_sf <- geodata::gadm(country = "NOR", level = 0, path = tempdir(),
                           version = "latest") |>
  st_as_sf()

# 3. PREPARE THE CLC LAYER -----------------------------------------------------

## 3.1. Crop and mask to Norway ------------------------------------------------

# Transform Norway boundary to have the same CRS as CLC
norway_clc_crs <- st_transform(norway_sf, crs(clc))

# Crop and mask
clc_nor <- crop(clc, vect(norway_clc_crs), mask = TRUE)

## 3.2. Reclassify to CLC Level 2 ----------------------------------------------

# CLC Level 3 codes in the order of the GRID_CODE values (1-44) of the raster
clc_level3_codes <- c(111, 112, 121, 122, 123, 124, 131, 132, 133, 141, 142,
                      211, 212, 213, 221, 222, 223, 231, 241, 242, 243, 244,
                      311, 312, 313, 321, 322, 323, 324, 331, 332, 333, 334, 335,
                      411, 412, 421, 422, 423, 511, 512, 521, 522, 523)

# Check which coding the raster uses: GRID_CODE (1-44) or 3-digit CLC codes
clc_max <- global(clc_nor, "max", na.rm = TRUE)[1, 1]

if (clc_max <= 48) {
  # GRID_CODE version (48 = no data)
  rcl <- cbind(from = 1:44, to = clc_level3_codes %/% 10)
} else {
  # 3-digit CLC code version (e.g. 999/990 = no data / unclassified)
  rcl <- cbind(from = clc_level3_codes, to = clc_level3_codes %/% 10)
}

# Reclassify (anything not in the table, e.g. no-data codes, becomes NA)
clc_l2 <- classify(clc_nor, rcl, others = NA)
names(clc_l2) <- "clc_level2"

## 3.3. Area per Level 2 class (for the Methods text) ---------------------------

# Cells are 100 x 100 m (1 ha) in the equal-area EPSG:3035, so km2 = count / 100
clc_l2_names <- c("11" = "Urban Fabric",
                  "12" = "Industrial, Commercial, and Transport Units",
                  "13" = "Mine, Dump, and Construction Sites",
                  "14" = "Artificial, Non-agricultural Vegetated Areas",
                  "21" = "Arable Land",
                  "22" = "Permanent Crops",
                  "23" = "Pastures",
                  "24" = "Heterogeneous Agricultural Areas",
                  "31" = "Forests",
                  "32" = "Scrub and/or Herbaceous Vegetation Associations",
                  "33" = "Open Spaces with Little or No Vegetation",
                  "41" = "Inland Wetlands",
                  "42" = "Maritime Wetlands",
                  "51" = "Inland Waters",
                  "52" = "Marine Waters")

# Calculate area for each land-cover category
clc_l2_area <- freq(clc_l2) |>
  transmute(code = value,
            class = clc_l2_names[as.character(value)],
            area_km2 = count * prod(res(clc_l2)) / 1e6,
            percent = round(100 * area_km2 / sum(area_km2), 2))

# Check the values
print(clc_l2_area)

## 3.4. Project to the map CRS -------------------------------------------------

# USe nearest neighbour so class codes are not averaged
clc_l2_utm <- project(clc_l2, paste0("EPSG:", project_crs),
                      method = "near", res = 100)

# Convert to categorical and keep only the classes present in Norway
present_codes <- sort(clc_l2_area$code)
clc_l2_labels <- unname(clc_l2_names[as.character(present_codes)])

# Put into a dataframe
levels(clc_l2_utm) <- data.frame(value = present_codes,
                                 class = clc_l2_labels)

# Convert the Norway boundary ito the map CRS
norway_utm <- st_transform(norway_sf, project_crs)

# 4. PLOT MAP ------------------------------------------------------------

# Define colour pallete (attempred colour blind-friendly pallete)
clc_l2_palette <- c("11" = "#B2182B", # Urban fabric
                    "12" = "#7B3294", # Industrial, commercial, transport
                    "13" = "#C51B7D", # Mine, dump, construction
                    "14" = "#F1B6DA", # Artificial, non-agri. vegetated 
                    "21" = "#E6AB02", # Arable land
                    "22" = "#A6611A", # Permanent crops
                    "23" = "#FFF7BC", # Pastures 
                    "24" = "#BF812D", # Heterogeneous agricultural 
                    "31" = "#1B6B3A", # Forests 
                    "32" = "#A6D96A", # Scrub/herbaceous vegetation 
                    "33" = "#BDBDBD", # Open spaces, little/no vegetation 
                    "41" = "#9E9AC8", # Inland wetlands 
                    "42" = "#35978F", # Maritime wetlands
                    "51" = "#2B7BBA", # Inland waters 
                    "52" = "#0B3C6E")  # Marine waters  

# Name the colours by the legend labels of the classes present
fill_values <- setNames(clc_l2_palette[as.character(present_codes)],
                        clc_l2_labels)

# Plot figure
(clc_map <- ggplot() +
  geom_spatraster(data = clc_l2_utm, maxcell = 5e6) +
  geom_sf(data = norway_utm, fill = NA, colour = "grey30",
          linewidth = 0.15) +
  scale_fill_manual(values = fill_values,
                    na.value = "transparent",
                    na.translate = FALSE,
                    labels = function(x) str_wrap(x, width = 32),
                    name = "CLC 2018 (Level 2)") +
  # graticule and axis labels in degrees, map drawn in UTM 33N
  coord_sf(crs = project_crs, datum = st_crs(4326), expand = FALSE) +
  scale_x_continuous(breaks = seq(5, 30, by = 5)) +
  scale_y_continuous(breaks = seq(58, 71, by = 2)) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         height = unit(1.2, "cm"), width = unit(1.2, "cm"),
                         pad_x = unit(0.4, "cm"), pad_y = unit(0.4, "cm"),
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "br", width_hint = 0.25,
                   pad_x = unit(0.4, "cm"), pad_y = unit(12, "cm"),
                   text_cex = 0.8) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 10) +
  theme(panel.grid.major = element_line(colour = "grey85", linewidth = 0.2),
        panel.border = element_rect(colour = "black", linewidth = 0.4),
        axis.text = element_text(colour = "black", size = 14),
        legend.title = element_text(face = "bold", size = 13),
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.65, "cm"),
        legend.key = element_rect(colour = "grey40", linewidth = 0.2),
        legend.spacing.y = unit(0.1, "cm"),
        legend.position = "inside",
        legend.position.inside = c(0.98, 0.02),
        legend.justification = c(1, 0),
        legend.background = element_rect(fill = alpha("white", 0.85),
                                         colour = "grey40", linewidth = 0.3),
        legend.margin = margin(5, 6, 5, 6)))

# Save as .png
ggsave(filename = here("figures", "ThesisMethodsFigure_CLC2018_level2_Norway.png"),
       plot = clc_map, width = 240, height = 270, units = "mm", dpi = 600)

# Save as .pdf
ggsave(filename = here("figures", "ThesisMethodsFigure_CLC2018_level2_Norway.pdf"),
       plot = clc_map, width = 180, height = 190, units = "mm")

# END OF SCRIPT ----------------------------------------------------------------