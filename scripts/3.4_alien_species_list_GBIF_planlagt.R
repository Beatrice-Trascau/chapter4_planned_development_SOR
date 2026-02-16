##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 3.4_alien_species_list_GBIF_planlagt
# This script contains code which creates harmonises the taxonomy of the
# Norwegian alien species list and adds redlisted species to planned 
# development polygons
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source setup file
library(here)
source(here("scripts", "0_setup.R"))

# Load alien species list
alien_list_raw <- read_excel(here("data", "raw_data", "fremmedartslista-2023.xlsx"))

# Load polygon all data (needed for species-based figures)
polygon_all_data <- readRDS(here("data", "derived_data",
                                 "polygons_occurrences_all_data.rds"))

# 2. PREPARE ALIEN SPECIES LIST ------------------------------------------------

## 2.1. Filter and clean alien list --------------------------------------------

# Keep only mainland Norway assessments and relevant columns
# Filter to only assessed alien species (exclude if needed based on status)
alien_clean <- alien_list_raw |>
  filter(`Vurderingsområde` == "Fastlands-Norge med havområder") |>
  select(scientific_name  = `Vitenskapelig navn`,
         risk_category    = `Risikokategori 2023`) |>
  # Remove any duplicate species entries keeping first occurrence
  distinct(scientific_name, .keep_all = TRUE)

cat("Alien species (mainland Norway):", nrow(alien_clean), "\n")
cat("Risk category breakdown:\n")
print(table(alien_clean$risk_category))

## 2.2. Harmonise species names with GBIF backbone ----------------------------

# Look up all alien species names against the GBIF backbone taxonomy
# Results are cached to file so the API call is only made once
if (file.exists(here("data", "derived_data", "alien_backbone_lookup.rds"))) {
  cat("\nLoading cached backbone lookup...\n")
  backbone_lookup <- readRDS(here("data", "derived_data",
                                  "alien_backbone_lookup.rds"))
} else {
  cat("\nLooking up", nrow(alien_clean), "species names against GBIF backbone...\n")
  cat("This may take several minutes - results will be cached for future use.\n")
  
  # Split into batches of 1000 to avoid API rate limits
  batch_size       <- 1000
  batches          <- split(alien_clean$scientific_name,
                            ceiling(seq_along(alien_clean$scientific_name) / batch_size))
  backbone_results <- list()
  
  for (i in seq_along(batches)) {
    cat("Processing batch", i, "of", length(batches), "\n")
    backbone_results[[i]] <- name_backbone_checklist(
      data.frame(name = batches[[i]])
    )
    Sys.sleep(2) # pause between batches to avoid overwhelming the API
  }
  
  backbone_lookup <- bind_rows(backbone_results)
  
  # Save to file so this never needs to be rerun
  saveRDS(backbone_lookup,
          here("data", "derived_data", "alien_backbone_lookup.rds"))
  cat("Backbone lookup saved to derived_data.\n")
}

# Join backbone results back to alien list
alien_harmonised <- alien_clean |>
  mutate(gbif_species = backbone_lookup$species,
         match_type   = backbone_lookup$matchType) |>
  # Keep original name where GBIF match is unavailable
  mutate(gbif_species = ifelse(is.na(gbif_species),
                               scientific_name, gbif_species))

# Check match quality
cat("\nMatch type breakdown:\n")
print(table(alien_harmonised$match_type))

# Keep only exact matches and remove duplicate GBIF species for preliminary analysis
alien_harmonised <- alien_harmonised |>
  filter(match_type == "EXACT") |>
  distinct(gbif_species, .keep_all = TRUE)

cat("\nUsing", nrow(alien_harmonised), "alien species with exact GBIF matches.\n")

# 3. PREPARE OCCURRENCE DATA ---------------------------------------------------

# Recreate polygon_tax_join from polygon_occurrence_join
# (one row per occurrence, only within development polygons, no NAs)
polygon_tax_join <- polygon_occurrence_join |>
  st_drop_geometry() |>
  filter(!is.na(gbifID))

# 4. JOIN ALIEN LIST TO OCCURRENCE DATA ----------------------------------------

# Join alien status and risk categories to occurrences by species name
polygon_alien_join <- polygon_tax_join |>
  left_join(alien_harmonised |> select(gbif_species, risk_category),
            by = c("species" = "gbif_species")) |>
  # Create simple alien/non-alien classification
  mutate(alien_status  = ifelse(is.na(risk_category), "Native", "Alien"),
         risk_category = ifelse(is.na(risk_category), "Native", risk_category))

# Check counts
cat("\nAlien status breakdown (SOR):\n")
print(table(polygon_alien_join$alien_status))
cat("\nRisk category breakdown (SOR):\n")
print(table(polygon_alien_join$risk_category))

# 5. CALCULATE PROPORTIONS -----------------------------------------------------

## 5.1. SOR-based proportions --------------------------------------------------

# Simple alien vs native proportions
alien_simple_sor <- polygon_alien_join |>
  group_by(english_categories, alien_status) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(english_categories) |>
  mutate(proportion = n / sum(n)) |>
  ungroup()

# Detailed risk category proportions
alien_risk_sor <- polygon_alien_join |>
  group_by(english_categories, risk_category) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(english_categories) |>
  mutate(proportion = n / sum(n)) |>
  ungroup()

## 5.2. Species-based proportions ----------------------------------------------

# Get unique species per polygon, then join alien list
species_per_polygon <- polygon_all_data |>
  filter(n_species > 0) |>
  select(polygon_id, english_categories, species_list) |>
  tidyr::unnest(cols = species_list) |>
  rename(species = species_list) |>
  distinct(english_categories, species) |>
  left_join(alien_harmonised |> select(gbif_species, risk_category),
            by = c("species" = "gbif_species")) |>
  mutate(alien_status  = ifelse(is.na(risk_category), "Native", "Alien"),
         risk_category = ifelse(is.na(risk_category), "Native", risk_category))

# Check counts
cat("\nAlien status breakdown (species):\n")
print(table(species_per_polygon$alien_status))
cat("\nRisk category breakdown (species):\n")
print(table(species_per_polygon$risk_category))

# Simple alien vs native proportions
alien_simple_sp <- species_per_polygon |>
  group_by(english_categories, alien_status) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(english_categories) |>
  mutate(proportion = n / sum(n)) |>
  ungroup()

# Detailed risk category proportions
alien_risk_sp <- species_per_polygon |>
  group_by(english_categories, risk_category) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(english_categories) |>
  mutate(proportion = n / sum(n)) |>
  ungroup()

# 6. CREATE FIGURES ------------------------------------------------------------

## 6.1. Simple alien vs native figures ----------------------------------------

# Colour palette for simple classification
simple_colours <- c("Alien"  = "#d62728",
                    "Native" = "#2ca02c")

# Figure 6a: Alien vs native (SOR)
fig6a <- ggplot(alien_simple_sor,
                aes(x    = english_categories,
                    y    = proportion,
                    fill = alien_status)) +
  geom_bar(stat      = "identity",
           position  = "stack",
           color     = "white",
           linewidth = 0.3) +
  scale_y_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = simple_colours,
                    name   = "Status") +
  labs(x = "Development category",
       y = "Proportion of occurrence records") +
  theme_classic() +
  theme(panel.grid    = element_blank(),
        axis.title    = element_text(size = 12),
        axis.text     = element_text(size = 10),
        axis.text.x   = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Figure 6b: Alien vs native (species)
fig6b <- ggplot(alien_simple_sp,
                aes(x    = english_categories,
                    y    = proportion,
                    fill = alien_status)) +
  geom_bar(stat      = "identity",
           position  = "stack",
           color     = "white",
           linewidth = 0.3) +
  scale_y_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = simple_colours,
                    name   = "Status") +
  labs(x = "Development category",
       y = "Proportion of unique species") +
  theme_classic() +
  theme(panel.grid   = element_blank(),
        axis.title   = element_text(size = 12),
        axis.text    = element_text(size = 10),
        axis.text.x  = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 11),
        legend.text  = element_text(size = 10))

# Combine panels
figure6 <- plot_grid(fig6a, fig6b,
                     labels = c("a)", "b)"),
                     align  = "h",
                     axis   = "tb")

# Save figure
ggsave(filename = here("figures", "Figure6_alien_vs_native_in_polygons.png"),
       plot     = figure6,
       width    = 12,
       height   = 6,
       dpi      = 600)

ggsave(filename = here("figures", "Figure6_alien_vs_native_in_polygons.pdf"),
       plot     = figure6,
       width    = 12,
       height   = 6,
       dpi      = 600)

## 6.2. Risk category figures --------------------------------------------------

# Define colour palette for risk categories
# Order from highest to lowest risk
risk_order  <- c("SE", "HI", "PH", "LO", "NK", "NR", "Native")
risk_labels <- c("Severe impact (SE)", "High impact (HI)",
                 "Potentially high impact (PH)", "Low impact (LO)",
                 "No known impact (NK)", "Not assessed (NR)", "Native")
risk_colours <- c("SE"     = "#7f0000",
                  "HI"     = "#d62728",
                  "PH"     = "#ff7f0e",
                  "LO"     = "#ffbb78",
                  "NK"     = "#bcbd22",
                  "NR"     = "#969696",
                  "Native" = "#2ca02c")

# Set factor order
alien_risk_sor$risk_category <- factor(alien_risk_sor$risk_category,
                                       levels = risk_order)
alien_risk_sp$risk_category <- factor(alien_risk_sp$risk_category,
                                      levels = risk_order)

# Remove native species
alien_risk_sor <- alien_risk_sor |>
  filter(risk_category != "Native")
alien_risk_sp <- alien_risk_sp |>
  filter(risk_category != "Native")

# Figure 7a: Risk categories (SOR)
fig7a <- ggplot(alien_risk_sor,
                aes(x    = english_categories,
                    y    = proportion,
                    fill = risk_category)) +
  geom_bar(stat      = "identity",
           position  = "stack",
           color     = "white",
           linewidth = 0.3) +
  scale_y_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = risk_colours,
                    labels = risk_labels,
                    name   = "Alien species risk") +
  labs(x = "Development category",
       y = "Proportion of occurrence records") +
  theme_classic() +
  theme(panel.grid      = element_blank(),
        axis.title      = element_text(size = 12),
        axis.text       = element_text(size = 10),
        axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Figure 7b: Risk categories (species)
fig7b <- ggplot(alien_risk_sp,
                aes(x    = english_categories,
                    y    = proportion,
                    fill = risk_category)) +
  geom_bar(stat      = "identity",
           position  = "stack",
           color     = "white",
           linewidth = 0.3) +
  scale_y_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = risk_colours,
                    labels = risk_labels,
                    name   = "Alien species risk") +
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
legend <- get_legend(fig7b)

# Remove legend from right panel
fig7b_no_legend <- fig7b + theme(legend.position = "none")

# Combine plots with aligned sizes
plots <- plot_grid(fig7a, fig7b_no_legend,
                   labels = c("a)", "b)"),
                   align = "h",
                   axis = "tb")

# Add legend to the right
figure7 <- plot_grid(plots, legend, rel_widths = c(3, 0.4))

# Save figure
ggsave(filename = here("figures", "Figure7_alien_risk_categories_in_polygons.png"),
       plot     = figure7,
       width    = 12,
       height   = 6,
       dpi      = 600)

ggsave(filename = here("figures", "Figure7_alien_risk_categories_in_polygons.pdf"),
       plot     = figure7,
       width    = 12,
       height   = 6,
       dpi      = 600)

# END OF SCRIPT ----------------------------------------------------------------