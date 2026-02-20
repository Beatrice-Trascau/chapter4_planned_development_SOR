##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 3.3_redlist_GBIF_planlagt
# This script contains code which creates harmonises the taxonomy of the
# Norwegian red list and adds redlisted species to planned development polygons
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source setup file
library(here)
source(here("scripts", "0_setup.R"))

# Load red list
redlist_raw <- read_excel(here("data", "raw_data", "rødliste-2021.xlsx"))

# Load polygon occurrence join (one row per occurrence, needed for SOR-based figures)
polygon_all_data <- readRDS(here("data", "derived_data",
                                 "polygons_occurrences_all_data.rds"))

# 2. PREPARE RED LIST ----------------------------------------------------------

## 2.1. Filter and cleam red list ----------------------------------------------

# Keep only mainland Norway assessments and relevant columns
redlist_clean <- redlist_raw |>
  filter(`Vurderingsområde` == "Norge") |>
  select(scientific_name  = `Vitenskapelig navn`,
         redlist_category = `Kategori 2021`) |>
  # filter to only threatened categories (CR, EN, VU, NT, LC, DD)
  filter(redlist_category %in% c("CR", "EN", "VU", "NT", "LC", "DD")) |>
  # remove any duplicate species entries keeping first occurrence
  distinct(scientific_name, .keep_all = TRUE)

# Check how many species there are
print(table(redlist_clean$redlist_category))

## 2.2. Harmonise species names with GBIF backbone -----------------------------

# Look up all red list species names against the GBIF backbone taxonomy
# Results are cached to file so the API call is only made once
if (file.exists(here("data", "derived_data", "redlist_backbone_lookup.rds"))) {
  cat("\nLoading cached backbone lookup...\n")
  backbone_lookup <- readRDS(here("data", "derived_data",
                                  "redlist_backbone_lookup.rds"))
} else {
  cat("\nLooking up", nrow(redlist_clean), "species names against GBIF backbone...\n")
  cat("This may take several minutes - results will be cached for future use.\n")
  
  # Split into batches of 1000 to avoid API rate limits
  batch_size <- 1000
  batches <- split(redlist_clean$scientific_name,
                         ceiling(seq_along(redlist_clean$scientific_name) / batch_size))
  backbone_results <- list()
  
  for (i in seq_along(batches)) {
    cat("Processing batch", i, "of", length(batches), "\n")
    backbone_results[[i]] <- name_backbone_checklist(
      data.frame(name = batches[[i]])
    )
    Sys.sleep(2) # pause between batches to avoid overwhelming the API
  }
  
  backbone_lookup <- bind_rows(backbone_results)
  
  # aave to file so this never needs to be rerun
  saveRDS(backbone_lookup,
          here("data", "derived_data", "redlist_backbone_lookup.rds"))
  cat("Backbone lookup saved to derived_data.\n")
}

# Join backbone results back to red list
redlist_harmonised <- redlist_clean |>
  mutate(gbif_species = backbone_lookup$species,
         match_type   = backbone_lookup$matchType) |>
  # keep original name where GBIF match is unavailable
  mutate(gbif_species = ifelse(is.na(gbif_species),
                               scientific_name, gbif_species))

# Check match quality
cat("\nMatch type breakdown:\n")
print(table(redlist_harmonised$match_type))

# Check the records with no matches
redlist_no_match <- redlist_harmonised |>
  filter(match_type == "NONE") # manual check suggests most of the names are accepted in GBIF, keep most as they are

# Check the records with fuzzy matches
redlizy_fuzzy <- redlist_harmonised |>
  filter(match_type == "FUZZY")

# Check the records with higherrank matches
redlist_higherrank <- redlist_harmonised |>
  filter(match_type == "HIGHERRANK")

# Keep only exact matches and remove duplicate GBIF species
redlist_harmonised <- redlist_harmonised |>
  filter(match_type == "EXACT") |>
  distinct(gbif_species, .keep_all = TRUE)

# 3. JOIN RED LIST TO OCCURRENCE DATA ------------------------------------------

# Recreate polygon_tax_join from polygon_occurrence_join
# (one row per occurrence, only within development polygons, no NAs)
polygon_tax_join <- polygon_occurrence_join |>
  st_drop_geometry() |>
  filter(!is.na(gbifID))

# Join red list categories to occurrences by species name
polygon_redlist_join <- polygon_tax_join |>
  left_join(redlist_harmonised |> select(gbif_species, redlist_category),
            by = c("species" = "gbif_species")) |>
  # Label non-red-listed species
  mutate(redlist_category = ifelse(is.na(redlist_category),
                                   "Not listed", redlist_category))


# 4. CALCULATE PROPORTIONS -----------------------------------------------------

# Calculate proportion of occurrence records (SOR) per red list category per development category
sor_proportions <- polygon_redlist_join |>
  group_by(english_categories, redlist_category) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(english_categories) |>
  mutate(proportion = n / sum(n)) |>
  ungroup()

# Get unique species per polygon, then join red list
# Unnest species_list from polygon_all_data to get one row per species per polygon
species_per_polygon <- polygon_all_data |>
  filter(n_species > 0) |>
  select(polygon_id, english_categories, species_list) |>
  tidyr::unnest(cols = species_list) |>
  rename(species = species_list) |>
  distinct(english_categories, species) |>
  left_join(redlist_harmonised |> select(gbif_species, redlist_category),
            by = c("species" = "gbif_species")) |>
  mutate(redlist_category = ifelse(is.na(redlist_category),
                                   "Not listed", redlist_category))

# Proportion of unique species per red list category per development category
species_proportions <- species_per_polygon |>
  group_by(english_categories, redlist_category) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(english_categories) |>
  mutate(proportion = n / sum(n)) |>
  ungroup()

# 5. PLOT FIGURES --------------------------------------------------------------

# Define colour palette for red list categories - order from most to least threatened
category_order  <- c("CR", "EN", "VU", "NT", "LC", "DD", "Not listed")
category_labels <- c("Critically Endangered (CR)", "Endangered (EN)",
                     "Vulnerable (VU)", "Near Threatened (NT)",
                     "Least Concern (LC)", "Data Deficient (DD)", "Not listed")
category_colours <- c("CR" = "#d73027",
                      "EN" = "#f46d43",
                      "VU" = "#fdae61",
                      "NT" = "#fee090",
                      "LC" = "#74c476",
                      "DD" = "#969696",
                      "Not listed" = "grey90")

# Set factor order for consistent ordering across both figures
sor_proportions$redlist_category <- factor(sor_proportions$redlist_category,
                                           levels = category_order)
species_proportions$redlist_category <- factor(species_proportions$redlist_category,
                                               levels = category_order)

# Stacked barplot of redlisted status of proportion of occurrences
fig_redlist_sor <- ggplot(sor_proportions,
                          aes(x    = english_categories,
                              y    = proportion,
                              fill = redlist_category)) +
  geom_bar(stat      = "identity",
           position  = "stack",
           color     = "white",
           linewidth = 0.3) +
  scale_y_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = category_colours,
                    labels = category_labels,
                    name   = "Red list category") +
  labs(x = "Development category",
       y = "Proportion of occurrence records") +
  theme_classic() +
  theme(panel.grid   = element_blank(),
        axis.title   = element_text(size = 12),
        axis.text    = element_text(size = 10),
        axis.text.x  = element_text(angle = 45, hjust = 1),
        legend.position = "none")


# Stacked barplot of redlisted status of unique species
fig_redlist_sp <- ggplot(species_proportions,
                         aes(x    = english_categories,
                             y    = proportion,
                             fill = redlist_category)) +
  geom_bar(stat      = "identity",
           position  = "stack",
           color     = "white",
           linewidth = 0.3) +
  scale_y_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = category_colours,
                    labels = category_labels,
                    name   = "Red list category") +
  labs(x = "Development category",
       y = "Proportion of unique species") +
  theme_classic() +
  theme(panel.grid   = element_blank(),
        axis.title   = element_text(size = 12),
        axis.text    = element_text(size = 10),
        axis.text.x  = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 11),
        legend.text  = element_text(size = 10))

# Extract legend from the right panel
legend <- get_legend(fig_redlist_sp)

# Remove legend from right panel
fig_redlist_sp_no_legend <- fig_redlist_sp + theme(legend.position = "none")

# Combine plots with aligned sizes
plots <- plot_grid(fig_redlist_sor, fig_redlist_sp_no_legend,
                   labels = c("a)", "b)"),
                   align = "h",
                   axis = "tb")

# Add legend to the right
figure5 <- plot_grid(plots, legend, rel_widths = c(3, 0.4))

# Save figure
ggsave(filename = here("figures", "Figure5_redlist_species_and_occurrences_per_development_type.png"),
       plot     = figure5,
       width    = 12,
       height   = 8,
       dpi      = 600)

ggsave(filename = here("figures", "Figure5_redlist_species_and_occurrences_per_development_type.pdf"),
       plot     = figure5,
       width    = 12,
       height   = 8,
       dpi      = 600)

# END OF SCRIPT ----------------------------------------------------------------