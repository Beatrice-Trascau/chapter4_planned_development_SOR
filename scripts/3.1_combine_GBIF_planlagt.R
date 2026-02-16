##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 3.1_combine_GBIF_planlagt
# This script contains code which adds the species occurrence records to the
# polygons of planned development
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source setup file
library(here)
source(here("scripts", "0_setup.R"))

# Load development polygons
development_polygons <- st_read(here("data", "raw_data", "nina_planagt.gpkg"))

# Load cleaned occurrence records
clean_occurrences_70m <- read.csv(here("data", "derived_data",
                                        "clean_occurrences_70m.txt"))

# 2. JOIN OCCURRENCES TO POLYGONS ----------------------------------------------

## 2.1. Prepare development polygons -------------------------------------------

# Add unique polygon ID to use as grouping factor after the join
# Add english category names and numeric area column so they are available for
# all subsequent joins and operations without needing to recreate them
development_polygons <- development_polygons |>
  mutate(polygon_id = row_number(),
         area_m2_numeric = as.numeric(planlagt_areal_m2),
         english_categories = case_when(arealformalsgruppe == "01 Bolig eller sentrumsformål" ~ "Residential",
                                        arealformalsgruppe == "02 Fritidsbebyggelse" ~ "Recreational",
                                        arealformalsgruppe == "03 Tjenesteyting" ~ "Services",
                                        arealformalsgruppe == "04 Handel" ~ "Retail",
                                        arealformalsgruppe == "05 Turistformål" ~ "Tourism",
                                        arealformalsgruppe == "06 Næringsvirksomhet"~ "Commercial",
                                        arealformalsgruppe == "07 Råstoffutvinning" ~ "Mining",
                                        arealformalsgruppe == "08 Kombinerte formål" ~ "Combined",
                                        arealformalsgruppe == "13 Forsvaret" ~ "Defense",
                                        arealformalsgruppe == "16 Havner og småbåthavner" ~ "Ports"))

## 2.2. Prepare spatial data ---------------------------------------------------

# Convert occurrences to spatial points
occurrences_sf <- clean_occurrences_70m |>
  # remove occurrences with NA for either longitude or latitude
  filter(!is.na(decimalLongitude),
         !is.na(decimalLatitude)) |>
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = 4326)

# Transform occurrences to match the CRS of the polygons
occurrences_sf_crs <- st_transform(occurrences_sf, st_crs(development_polygons))

# Check that the CRSs match
stopifnot(st_crs(development_polygons) == st_crs(occurrences_sf_crs))

## 2.2. Spatial join -----------------------------------------------------------

# Spatial join occurrences to polygon
# each row in the result = one occurrence within one polygon
# left = TRUE retains polygons with no occurrences (as rows with NA occurrence columns)
polygon_occurrence_join <- st_join(development_polygons, occurrences_sf_crs,
                                   join = st_intersects,
                                   left = TRUE)

## 2.3. Convert to one big dataframe with all the information needed -----------

# Convert dataframe to have 1 row per polygon 
# retains key polygon metadata, occurrence counts, species counts and species identities
polygon_all_data <- polygon_occurrence_join |>
  st_drop_geometry() |>
  group_by(polygon_id,
           arealformalsgruppe,
           english_categories,
           kommunenummer,
           kommune,
           planlagt_areal_m2,
           area_m2_numeric) |>
  summarise(n_occurrences = sum(!is.na(gbifID)), # count non-NA rows = occurrences
            n_species     = n_distinct(species[!is.na(species)]),   # number of unique species
            species_list  = list(unique(species[!is.na(species)])), # identity of species present
            kingdoms      = list(unique(kingdom[!is.na(kingdom)])), # kingdoms represented
            classes       = list(unique(class[!is.na(class)])), 
            phyla         = list(unique(phylum[!is.na(phylum)])), 
            .groups = "drop")

# Quick check that it makes sense
cat("Total polygons in master df:  ", nrow(polygon_all_data), "\n")
cat("Polygons with occurrences:    ", sum(polygon_all_data$n_occurrences > 0), "\n")
cat("Polygons without occurrences: ", sum(polygon_all_data$n_occurrences == 0), "\n")

# Save df to file
saveRDS(polygon_all_data,
        here("data", "derived_data", "polygons_occurrences_all_data.rds"))

# 3. PLOT EXPLORATORY FIGURES --------------------------------------------------

## 3.1. Number of Species Occurrence Records (SOR) per polygon -----------------

# Calculate histogram counts for non-zero values
nonzero_hist <- hist(log10(polygon_all_data$n_occurrences[polygon_all_data$n_occurrences > 0]),
                     breaks = 50, plot = FALSE)

# Get maximum number of occurrences in non-zero polygons
max_nonzero_count <- max(nonzero_hist$counts)

# Calculate the number of 0s
n_zeros <- sum(polygon_all_data$n_occurrences == 0)

# Scaling factor between the two y axes
scale_factor <- n_zeros / max_nonzero_count

# Figure 1a - Histogram
fig1a <- ggplot() +
  # zero bar
  geom_col(aes(x = 0, y = n_zeros),
           fill = "steelblue", color = "white", width = 0.1) +
  # non-zero histogram, counts scaled up to match primary y axis
  geom_histogram(data = polygon_all_data |> filter(n_occurrences > 0),
                 aes(x = log10(n_occurrences), y = after_stat(count) * scale_factor),
                 bins = 50, fill = "steelblue", color = "white") +
  scale_y_continuous(name     = "Number of polygons (zero SOR)",
                     labels   = scales::comma,
                     expand   = expansion(mult = c(0, 0.05)),
                     sec.axis = sec_axis(~ . / scale_factor,
                                         name   = "Number of polygons (>0 SOR)",
                                         labels = scales::comma)) +
  scale_x_continuous(breaks = c(0, log10(c(2, 11, 101, 1001, 10001))),
                     labels = c("0", "1", "10", "100", "1,000", "10,000")) +
  labs(x = "Number of SOR") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 12),
        axis.text  = element_text(size = 10))

# Figure 1b - Occurrence "Accummulation Curve"
fig1b <- ggplot(polygon_all_data,
                aes(x = n_occurrences + 1,
                    y = area_m2_numeric)) +
  geom_point(alpha = 0.3, size  = 0.8, color = "steelblue") +
  geom_smooth(color     = "black", linewidth = 0.8, se = TRUE) +
  scale_x_log10(labels = scales::comma,
                breaks = c(1, 10, 100, 1000, 10000)) +
  scale_y_log10(labels = scales::comma,
                breaks = c(100, 1000, 10000, 100000, 1000000)) +
  labs(x = "Number of species occurrence records + 1 (log10 scale)",
       y = expression(paste("Polygon area (m"^2, ") - log10 scale"))) +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 12),
        axis.text  = element_text(size = 10))

# Combine the two plots into a single figure
figure1 <- plot_grid(fig1a, fig1b, labels = c("a)", "b)"))

# Save figure as .png
ggsave(filename = here("figures", "Figure1_SOR_per_polygon.png"),
       plot = figure1,
       width = 20,
       height = 16,
       dpi = 600)

# Save figure as .pdf
ggsave(filename = here("figures", "Figure1_SOR_per_polygon.pdf"),
       plot = figure1,
       width = 20,
       height = 16,
       dpi = 600)

## 3.2. Number of Species per polygon ------------------------------------------

# Recalculate zeros and scale factor for species
n_zeros_sp      <- sum(polygon_all_data$n_species == 0)
nonzero_hist_sp <- hist(log10(polygon_all_data$n_species[polygon_all_data$n_species > 0]),
                        breaks = 50, plot = FALSE)
max_nonzero_count_sp <- max(nonzero_hist_sp$counts)
scale_factor_sp      <- n_zeros_sp / max_nonzero_count_sp


# Figure 2a - Histogram
fig2a <- ggplot() +
  geom_col(aes(x = 0, y = n_zeros_sp),
           fill = "steelblue", color = "white", width = 0.1) +
  geom_histogram(data = polygon_all_data |> filter(n_species > 0),
                 aes(x = log10(n_species), y = after_stat(count) * scale_factor_sp),
                 bins = 50, fill = "steelblue", color = "white") +
  scale_y_continuous(name     = "Number of polygons (zero species)",
                     labels   = scales::comma,
                     expand   = expansion(mult = c(0, 0)),
                     limits   = c(0, n_zeros * 1.05),
                     sec.axis = sec_axis(~ . / scale_factor_sp,
                                         name   = "Number of polygons (>0 species)",
                                         labels = scales::comma)) +
  scale_x_continuous(breaks = c(0, log10(c(2, 11, 101, 1001, 10001))),
                     labels = c("0", "1", "10", "100", "1,000", "10,000")) +
  labs(x = "Number of species") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 12),
        axis.text  = element_text(size = 10))

# Figure 2b - Species area curve
fig2b <- ggplot(polygon_all_data,
                aes(x = n_species + 1,
                    y = area_m2_numeric)) +
  geom_point(alpha = 0.3, size = 0.8, color = "steelblue") +
  geom_smooth(color = "black", linewidth = 0.8, se = TRUE) +
  scale_x_log10(labels = scales::comma,
                breaks = c(1, 10, 100, 1000, 10000)) +
  scale_y_log10(labels = scales::comma,
                breaks = c(100, 1000, 10000, 100000, 1000000)) +
  labs(x = "Number of species + 1 (log10 scale)",
       y = expression(paste("Polygon area (m"^2, ") - log10 scale"))) +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 12),
        axis.text  = element_text(size = 10))

# Combine into single figure
figure2 <- plot_grid(fig2a, fig2b, labels = c("a)", "b)"))

# Save figure as .png
ggsave(filename = here("figures", "Figure2_species_per_polygon.png"),
       plot = figure2,
       width = 20,
       height = 16,
       dpi = 600)

# Save figure as .pdf
ggsave(filename = here("figures", "Figure2_species_per_polygon.pdf"),
       plot = figure2,
       width = 20,
       height = 16,
       dpi = 600)

## 3.3. Taxonomic breakdown of occurrences in development polygons -------------

# Classify occurrences into taxonomic groups
polygon_tax_join <- polygon_occurrence_join |>
  st_drop_geometry() |>
  filter(!is.na(gbifID)) |>  # remove polygons with no occurrences
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
tax_colours <- c("Plants" = "#2d9e2d",
                 "Birds" = "#4a90d9",
                 "Arthropods" = "#d4a017",
                 "Mammals" = "#d95f4a",
                 "Fungi" = "#9b59b6",
                 "Other" = "grey70")

# Plot stacked barplot of proportion of occurrences belongoing to each group
# within the planned development polygons
figure4 <- ggplot(tax_proportions, aes(x = english_categories, y = proportion,
                                       fill = taxonomic_group)) +
  geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.3) +
  scale_y_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = tax_colours,
                    name   = "Taxonomic group") +
  labs(x = "Development category",
       y = "Proportion of occurrences") +
  theme_classic() +
  theme(panel.grid   = element_blank(),
        axis.title   = element_text(size = 12),
        axis.text    = element_text(size = 10),
        axis.text.x  = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 11),
        legend.text  = element_text(size = 10))

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

# END OF SCRIPT ----------------------------------------------------------------