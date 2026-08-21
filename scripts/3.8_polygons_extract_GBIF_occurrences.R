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

## 9.1. Figure 1 - Number of SOR per polygon  ----------------------------------

# Calculate histogram counts for non-zero values
nonzero_hist <- hist(log10(dev_data$n_occurrences[dev_data$n_occurrences > 0]),
                     breaks = 50, plot = FALSE)

# Get maximum number of occurrences in non-zero polygons
max_nonzero_count <- max(nonzero_hist$counts)

# Calculate the number of 0s
n_zeros <- sum(dev_data$n_occurrences == 0)

# Scaling factor between the two y axes
scale_factor <- n_zeros / max_nonzero_count

# Figure 1a - Histogram
(fig1a <- ggplot() +
  # zero bar
  geom_col(aes(x = 0, y = n_zeros),
           fill = "#5E3C99", color = "white", width = 0.1) +
  # non-zero histogram, counts scaled up to match primary y axis
  geom_histogram(data = dev_data |> filter(n_occurrences > 0),
                 aes(x = log10(n_occurrences), y = after_stat(count) * scale_factor),
                 bins = 50, fill = "#5E3C99", color = "white") +
  scale_y_continuous(name     = "Number of Polygons (zero SOR)",
                     labels   = scales::comma,
                     expand   = expansion(mult = c(0, 0.05)),
                     sec.axis = sec_axis(~ . / scale_factor,
                                         name   = "Number of Polygons (>0 SOR)",
                                         labels = scales::comma)) +
  scale_x_continuous(breaks = c(0, log10(c(2, 11, 101, 1001, 10001))),
                     labels = c("0", "1", "10", "100", "1,000", "10,000")) +
  labs(x = "Number of SOR") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text  = element_text(size = 14)))

# Figure 1b - Occurrence "Accummulation Curve"
(fig1b <- ggplot(dev_data,
                aes(x = n_occurrences + 1,
                    y = area_m2_numeric)) +
  geom_point(alpha = 0.3, size  = 0.8, color = "#5E3C99") +
  geom_smooth(color     = "black", linewidth = 0.8, se = TRUE) +
  scale_x_log10(labels = scales::comma,
                breaks = c(1, 10, 100, 1000, 10000)) +
  scale_y_log10(labels = scales::comma,
                breaks = c(100, 1000, 10000, 100000, 1000000)) +
  labs(x = "log(Number of SOR)",
       y = expression(paste("log(Polygon Area(m"^2, "))"))) +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text  = element_text(size = 14)))

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

## 9.2. Figure 2 - Number of Species per polygon -------------------------------

# Recalculate zeros and scale factor for species
n_zeros_sp <- sum(dev_data$n_species == 0)
nonzero_hist_sp <- hist(log10(dev_data$n_species[dev_data$n_species > 0]),
                        breaks = 50, plot = FALSE)
max_nonzero_count_sp <- max(nonzero_hist_sp$counts)
scale_factor_sp <- n_zeros_sp / max_nonzero_count_sp

# Figure 2a - Histogram
(fig2a <- ggplot() +
  geom_col(aes(x = 0, y = n_zeros_sp),
           fill = "#5E3C99", color = "white", width = 0.1) +
  geom_histogram(data = dev_data |> filter(n_species > 0),
                 aes(x = log10(n_species), y = after_stat(count) * scale_factor_sp),
                 bins = 50, fill = "#5E3C99", color = "white") +
  scale_y_continuous(name = "Number of Polygons (0 Species)",
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0)),
                     limits = c(0, n_zeros * 1.05),
                     sec.axis = sec_axis(~ . / scale_factor_sp,
                                         name   = "Number of Polygons (>0 Species)",
                                         labels = scales::comma)) +
  scale_x_continuous(breaks = c(0, log10(c(2, 11, 101, 1001, 10001))),
                     labels = c("0", "1", "10", "100", "1,000", "10,000")) +
  labs(x = "Number of Species") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)))

# Figure 2b - Species area curve
(fig2b <- ggplot(dev_data,
                aes(x = n_species + 1,
                    y = area_m2_numeric)) +
  geom_point(alpha = 0.3, size = 0.8, color = "#5E3C99") +
  geom_smooth(color = "black", linewidth = 0.8, se = TRUE) +
  scale_x_log10(labels = scales::comma,
                breaks = c(1, 10, 100, 1000, 10000)) +
  scale_y_log10(labels = scales::comma,
                breaks = c(100, 1000, 10000, 100000, 1000000)) +
  labs(x = "log(Number of Species)",
       y = expression(paste("log(Polygon Area (m"^2, "))"))) +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)))

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

## 9.3. Figure 4 - Taxonomic breakdown of SOR in polygons ----------------------

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

## 9.4. Figure 5 - Number of SOR vs Number of Species per Polygon -------------

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

# Use occurrences_sf from section 3 (or reload it if it was deleted)
if (!exists("occurrences_sf")) {
  message("occurrences_sf not found - rebuilding from clean_occurrences_1km.txt")
  occurrences_sf <- read.csv(here("data", "derived_data",
                                  "clean_occurrences_1km.txt"))[,
                                                                c("gbifID", "decimalLongitude", "decimalLatitude")] |>
    filter(!is.na(decimalLongitude), !is.na(decimalLatitude)) |>
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) |>
    st_transform(project_crs)
}

## 10.1. Prepare municipality boundaries ---------------------------------------

# Load municiaplity boundaries downloaded from GeoNorge
norway_municipalities_sf <- st_read(here("data", "raw_data",
                                         "Basisdata_0000_Norge_25833_Kommune_GeoJSON.geojson"))

# Match CRS of occurrence point exactly
norway_municipalities_sf <- st_transform(norway_municipalities_sf,
                                         st_crs(occurrences_sf))

# Download Norway land boundary and clip municipalities to it - removes marine
# areas that extend offshore for some coastal municipalities
norway_land <- geodata::gadm(country = "NOR", level = 0,
                             path = tempdir(), version = "latest") |>
  st_as_sf() |>
  st_transform(st_crs(occurrences_sf))

# Clip municipality boundaries to land
norway_municipalities_sf <- st_intersection(norway_municipalities_sf, norway_land)

## 10.2. Total SOR per municipality --------------------------------------------

# Confirm CRS is the same before joining
stopifnot(st_crs(occurrences_sf) == st_crs(norway_municipalities_sf))

# Join occurrences to municipality boundaries (drop occurrences that are outside any municipality)
# only keep the columns you need to make the processing faster
occurrence_municipality_join <- st_join(occurrences_sf |> dplyr::select(gbifID),
                                        norway_municipalities_sf |> dplyr::select(kommunenummer),
                                        join = st_intersects,
                                        left = FALSE)

# Count total occurrences per municipality
total_sor_per_municipality <- occurrence_municipality_join |>
  st_drop_geometry() |>
  group_by(kommunenummer) |>
  summarise(total_sor = n(), .groups = "drop")

# Sum occurrences within development polygons per municipality
# use the per-polygon counts already computed in model data
stopifnot("kommunenummer" %in% names(model_data))
polygon_sor_per_municipality <- model_data |>
  filter(polygon_type == "Development") |>
  group_by(kommunenummer) |>
  summarise(polygon_sor = sum(n_occurrences), .groups = "drop")

# Join total SOR and polygon SOR by kommunenummer
municipality_pct <- total_sor_per_municipality |>
  left_join(polygon_sor_per_municipality, by = "kommunenummer") |>
  # replace NA polygon SOR with 0 (municipalities with no development polygons)
  mutate(polygon_sor     = ifelse(is.na(polygon_sor), 0, polygon_sor),
         pct_in_polygons = (polygon_sor / total_sor) * 100)

# Quick check
cat("Municipalities with total SOR data:", nrow(municipality_pct), "\n") # 357
cat("Municipalities with polygon SOR > 0:", sum(municipality_pct$polygon_sor > 0), "\n") # 238
cat("Municipalities with zero total SOR:", sum(municipality_pct$total_sor == 0), "\n") # 0

## 10.3. Plot municipality map -------------------------------------------------

# Get the max value for % of SOR within polygons to use in the legend
pct_max <- ceiling(max(municipality_pct$pct_in_polygons, na.rm = TRUE))

# Join percentages to boundaries
norway_map_data <- norway_municipalities_sf |>
  left_join(municipality_pct, by = "kommunenummer")

# Quick check of how much data we have
cat("Municipalities with a percentage:", sum(!is.na(norway_map_data$pct_in_polygons)),
    "| of which 0%:", sum(norway_map_data$pct_in_polygons == 0, na.rm = TRUE),
    "| no data (grey):", sum(is.na(norway_map_data$pct_in_polygons)), "\n")
# Municipalities with a percentage: 357 | of which 0%: 119 | no data (grey): 0 

# Set legend breaks from 0% to the max
grad_breaks <- round(seq(0, pct_max, length.out = 5))


# Plot map
(figure3 <- ggplot(norway_map_data) +
    geom_sf(aes(fill = pct_in_polygons), color = "white", linewidth = 0.1) +
    scale_fill_viridis_c(name = "% of SOR Within\nDevelopment Polygons",
                         option = "viridis",
                         na.value = "grey80",
                         limits = c(0, pct_max),
                         breaks = grad_breaks,
                         labels = paste0(grad_breaks, "%"),
                         guide = guide_colourbar(barheight = unit(4, "cm"),
                                                 barwidth  = unit(0.8, "cm"))) +
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
          legend.title = element_text(size = 14),
          legend.text  = element_text(size = 13)))


# Save figure as .png
ggsave(filename = here("figures", "Figure3_municipality_map_pct_SOR.png"),
       plot = figure3,
       width = 12,
       height = 14,
       dpi = 600)

# Save figure as .pdf
ggsave(filename = here("figures", "Figure3_municipality_map_pct_SOR.pdf"),
       plot = figure3,
       width = 12,
       height = 14,
       dpi = 600)

# END OF SCRIPT ----------------------------------------------------------------