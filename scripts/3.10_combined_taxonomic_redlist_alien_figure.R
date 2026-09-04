##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 3.10_combined_taxonomic_redlist_alien_figure
# This script contains code to plot the taxonomic breakdown, redlist breakdown,
# and alien species category into a single figure for manuscript
##----------------------------------------------------------------------------##

# N.B: The parts in this megafigure are also created in other scripts
# Taxonomic breakdown (i.e. old Figure 4) is created in script 3.8
# Red-list category breakdown (i.e. old Figure 5) is created in script 3.3
# Alien species threat level breakdown (old Figure 6 and 7) area created in script 3.4

# 1. LOAD DATA -----------------------------------------------------------------

library(here)
library(patchwork)
source(here("scripts", "0_setup.R"))   # provides tidyverse, ggplot2, readxl, scales, etc.

# Per-polygon/buffer data (one row per polygon; carries species_list for the
# species-based panels)
model_data <- readRDS(here("data", "derived_data", "h2_polygon_buffer_data.rds"))

# Occurrence-level join (one row per matched occurrence per polygon/buffer)
occurrence_join <- readRDS(here("data", "derived_data",
                                "h2d_polygon_buffer_occurrence_join.rds"))

# Resolved alien species list (produced by 3.4): gbif_species + risk_category
alien_cleaned <- readRDS(here("data", "derived_data",
                              "alien_species_list_resolved.rds"))

# Red list (raw) + cached GBIF backbone lookup (produced by 3.3)
redlist_raw <- read_excel(here("data", "raw_data", "rødliste-2021.xlsx"))
backbone_lookup <- readRDS(here("data", "derived_data",
                                "redlist_backbone_lookup.rds"))

# Quick input checks
stopifnot(all(c("english_categories", "polygon_type", "species_list",
                "n_species") %in% names(model_data)),
          all(c("gbifID", "species", "polygon_type",
                "english_categories") %in% names(occurrence_join)),
          all(c("gbif_species", "risk_category") %in% names(alien_cleaned)))

# 2. RE-CREATE DATA FOR PLOTTING -----------------------------------------------

## 2.1. SOR Taxonomic breakdown for panel a) -----------------------------------

# Development-polygon occurrences only
dev_occurrences <- occurrence_join |>
  filter(polygon_type == "Development", !is.na(gbifID))

# The occurrence-level join does not carry taxonomy - rejoin kingdom/phylum/class
# from the cleaned occurrence file by gbifID (as done in 3.8). Only the taxonomy
# for gbifIDs present in the development occurrences is kept, to limit memory.
tax_lookup <- read.csv(here("data", "derived_data",
                            "clean_occurrences_1km.txt"))[,
                                                          c("gbifID", "kingdom", "phylum", "class")]
tax_lookup <- tax_lookup[tax_lookup$gbifID %in% dev_occurrences$gbifID, ]

# Add this to the development polygon occurrence only df
dev_occurrences <- dev_occurrences |>
  left_join(tax_lookup, by = "gbifID")
rm(tax_lookup); gc()
stopifnot(all(c("kingdom", "phylum", "class") %in% names(dev_occurrences)))

# Classify into taxonomic groups and compute proportions per development category
tax_proportions <- dev_occurrences |>
  mutate(taxonomic_group = case_when(kingdom == "Plantae" ~ "Plants",
                                     class == "Aves" ~ "Birds",
                                     phylum  == "Arthropoda" ~ "Arthropods",
                                     class == "Mammalia" ~ "Mammals",
                                     kingdom == "Fungi" ~ "Fungi",
                                     TRUE ~ "Other")) |>
  group_by(english_categories, taxonomic_group) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(english_categories) |>
  mutate(proportion = n / sum(n)) |>
  ungroup()

## 2.2. SOR and Species Red-list category for panels b) and c) -----------------

# Reproduce the cleaned red list EXACTLY as in 3.3 so it stays row-aligned with
# the cached backbone lookup
redlist_clean <- redlist_raw |>
  filter(`Vurderingsområde` == "Norge") |>
  select(scientific_name = `Vitenskapelig navn`,
         redlist_category = `Kategori 2021`) |>
  filter(redlist_category %in% c("CR", "EN", "VU", "NT", "LC", "DD")) |>
  distinct(scientific_name, .keep_all = TRUE)

# The cached backbone lookup must line up row-for-row with redlist_clean
stopifnot(nrow(backbone_lookup) == nrow(redlist_clean))

# Harmonise names, keep EXACT matches only, drop duplicate GBIF species
redlist_harmonised <- redlist_clean |>
  mutate(gbif_species = backbone_lookup$species,
         match_type = backbone_lookup$matchType,
         gbif_species = ifelse(is.na(gbif_species), scientific_name, gbif_species)) |>
  filter(match_type == "EXACT") |>
  distinct(gbif_species, .keep_all = TRUE)

# SOR-based: join red list category to development occurrences
redlist_sor_proportions <- occurrence_join |>
  filter(polygon_type == "Development", !is.na(gbifID)) |>
  left_join(redlist_harmonised |> select(gbif_species, redlist_category),
            by = c("species" = "gbif_species")) |>
  mutate(redlist_category = ifelse(is.na(redlist_category),
                                   "Not listed", redlist_category)) |>
  group_by(english_categories, redlist_category) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(english_categories) |>
  mutate(proportion = n / sum(n)) |>
  ungroup()

# Species-based: unique species per development category, then join red list
redlist_species_proportions <- model_data |>
  filter(polygon_type == "Development", n_species > 0) |>
  select(english_categories, species_list) |>
  tidyr::unnest(cols = species_list) |>
  rename(species = species_list) |>
  distinct(english_categories, species) |>
  left_join(redlist_harmonised |> select(gbif_species, redlist_category),
            by = c("species" = "gbif_species")) |>
  mutate(redlist_category = ifelse(is.na(redlist_category),
                                   "Not listed", redlist_category)) |>
  group_by(english_categories, redlist_category) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(english_categories) |>
  mutate(proportion = n / sum(n)) |>
  ungroup()

## 2.3. Alien species breakdown for panels d) e) f) g) -------------------------

# SOR-level alien join (development occurrences), reused by panels d and f
polygon_alien_join <- occurrence_join |>
  filter(polygon_type == "Development", !is.na(gbifID)) |>
  left_join(alien_cleaned |> select(gbif_species, risk_category),
            by = c("species" = "gbif_species")) |>
  mutate(alien_status = ifelse(is.na(risk_category), "Native", "Alien"),
         risk_category = ifelse(is.na(risk_category), "Native", risk_category))

# Species-level alien join (unique species per development category), panels e & g
alien_species_per_polygon <- model_data |>
  filter(polygon_type == "Development", n_species > 0) |>
  select(english_categories, species_list) |>
  tidyr::unnest(cols = species_list) |>
  rename(species = species_list) |>
  distinct(english_categories, species) |>
  left_join(alien_cleaned |> select(gbif_species, risk_category),
            by = c("species" = "gbif_species")) |>
  mutate(alien_status  = ifelse(is.na(risk_category), "Native", "Alien"),
         risk_category = ifelse(is.na(risk_category), "Native", risk_category))

# Panel d - alien vs native (SOR)
alien_simple_sor <- polygon_alien_join |>
  group_by(english_categories, alien_status) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(english_categories) |>
  mutate(proportion = n / sum(n)) |>
  ungroup()

# Panel e - alien vs native (species)
alien_simple_sp <- alien_species_per_polygon |>
  group_by(english_categories, alien_status) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(english_categories) |>
  mutate(proportion = n / sum(n)) |>
  ungroup()

# Panel f - alien risk categories (SOR). Native is kept in the denominator when
# computing proportions (so bars show the alien fraction of all SOR), then
# dropped for display - matching 3.4.
alien_risk_sor <- polygon_alien_join |>
  group_by(english_categories, risk_category) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(english_categories) |>
  mutate(proportion = n / sum(n)) |>
  ungroup() |>
  filter(risk_category != "Native")

# Panel g - alien risk categories (species)
alien_risk_sp <- alien_species_per_polygon |>
  group_by(english_categories, risk_category) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(english_categories) |>
  mutate(proportion = n / sum(n)) |>
  ungroup() |>
  filter(risk_category != "Native")

# 3. DEFINE COLOUR PALLETTES, ORDERS, AND SHARED THEME -------------------------

# All panels use the Okabe-Ito colour-blind-safe palette.

# Panel a - taxonomic groups (same as 3.8)
tax_group_order <- c("Plants", "Birds", "Arthropods", "Mammals", "Fungi", "Other")
tax_colours <- c("Plants" = "#009E73",
                 "Birds" = "#0072B2",
                 "Arthropods" = "#E69F00",
                 "Mammals" = "#D55E00",
                 "Fungi" = "#CC79A7",
                 "Other" = "#F0E442")

# Panels b & c - red list, REMAPPED onto Okabe-Ito (warm = more threatened),
# with "Not listed" kept neutral grey. Change the mapping here if preferred.
redlist_order  <- c("CR", "EN", "VU", "NT", "LC", "DD", "Not listed")
redlist_labels <- c("Critically Endangered (CR)", "Endangered (EN)",
                    "Vulnerable (VU)", "Near Threatened (NT)",
                    "Least Concern (LC)", "Data Deficient (DD)", "Not listed")
redlist_colours <- c("CR" = "#D55E00",   # vermillion
                     "EN" = "#E69F00",   # orange
                     "VU" = "#F0E442",   # yellow
                     "NT" = "#56B4E9",   # sky blue
                     "LC" = "#009E73",   # bluish green
                     "DD" = "#CC79A7",   # reddish purple
                     "Not listed" = "grey80")    # neutral

# Panels d & e - alien vs native (same as in 3.4)
simple_colours <- c("Alien" = "#E69F00", "Native" = "#CC79A7")

# Panels f & g - alien risk categories (unchanged from 3.4), Native not shown
risk_order  <- c("SE", "HI", "PH", "LO", "NK", "NR")
risk_labels <- c("Severe Impact (SE)", "High Impact (HI)",
                 "Potentially High Impact (PH)", "Low Impact (LO)",
                 "No Known Impact (NK)", "Not Assessed (NR)")
risk_colours <- c("SE" = "#E69F00", "HI" = "#56B4E9", "PH" = "#009E73",
                  "LO" = "#F0E442", "NK" = "#0072B2", "NR" = "#CC79A7")

# Apply factor orders so stacking and legends are consistent across paired panels
tax_proportions$taxonomic_group <-
  factor(tax_proportions$taxonomic_group, levels = tax_group_order)
redlist_sor_proportions$redlist_category <-
  factor(redlist_sor_proportions$redlist_category, levels = redlist_order)
redlist_species_proportions$redlist_category <-
  factor(redlist_species_proportions$redlist_category, levels = redlist_order)
alien_risk_sor$risk_category <-
  factor(alien_risk_sor$risk_category, levels = risk_order)
alien_risk_sp$risk_category <-
  factor(alien_risk_sp$risk_category, levels = risk_order)

# Shared theme so all seven panels match visually
base_theme <- theme_classic() +
  theme(panel.grid= element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

# Only label x-axis at the bottom
drop_x <- theme(axis.title.x = element_blank(),
                axis.text.x  = element_blank(),
                axis.ticks.x = element_blank())

# Shared y-axis for the two alien-risk panels (as in 3.4) so f) and g) are
# directly comparable and the small alien fractions stay legible
risk_y_max <- max(alien_risk_sor |> group_by(english_categories) |>
                    summarise(t = sum(proportion), .groups = "drop") |> pull(t),
                  alien_risk_sp  |> group_by(english_categories) |>
                    summarise(t = sum(proportion), .groups = "drop") |> pull(t))
risk_pretty <- scales::breaks_pretty()(c(0, risk_y_max))
risk_pretty <- risk_pretty[risk_pretty < risk_y_max * 0.85]
risk_breaks <- sort(unique(c(risk_pretty, risk_y_max)))
risk_shared_y <- list(scale_y_continuous(labels = scales::percent, breaks = risk_breaks,
                     expand = expansion(mult = c(0, 0.02))),
                     coord_cartesian(ylim = c(0, risk_y_max)))

# 4. BUILD PANELS --------------------------------------------------------------

# Panel a - taxonomic breakdown of SOR
(panel_a <- ggplot(tax_proportions,
                   aes(x = english_categories, y = proportion,
                       fill = taxonomic_group)) +
   geom_bar(stat = "identity", position = "stack",
            color = "white", linewidth = 0.3) +
   scale_y_continuous(labels = scales::percent,
                      expand = expansion(mult = c(0, 0.02))) +
   scale_fill_manual(values = tax_colours, name = "Taxonomic Group") +
   labs(x = "Development Category", y = "Proportion of SOR") +
   base_theme)

# Panel b - red list, SOR
(panel_b <- ggplot(redlist_sor_proportions,
                   aes(x = english_categories, y = proportion,
                       fill = redlist_category)) +
    geom_bar(stat = "identity", position = "stack",
             color = "white", linewidth = 0.3) +
    scale_y_continuous(labels = scales::percent,
                       expand = expansion(mult = c(0, 0.02))) +
    scale_fill_manual(values = redlist_colours, labels = redlist_labels,
                      name = "Red-list Category", drop = FALSE) +
    labs(x = "Development Category", y = "Proportion of SOR") +
    base_theme +
    drop_x)

# Panel c - red list, unique species (identical fill scale to panel b so the
# legend collects to a single shared guide)
(panel_c <- ggplot(redlist_species_proportions,
                   aes(x = english_categories, y = proportion,
                       fill = redlist_category)) +
    geom_bar(stat = "identity", position = "stack",
             color = "white", linewidth = 0.3) +
    scale_y_continuous(labels = scales::percent,
                       expand = expansion(mult = c(0, 0.02))) +
    scale_fill_manual(values = redlist_colours, labels = redlist_labels,
                      name = "Red-list Category", drop = FALSE) +
    labs(x = "Development Category", y = "Proportion of Unique Species") +
    base_theme +
    drop_x)

# Panel d - alien vs native, SOR
(panel_d <- ggplot(alien_simple_sor,
                   aes(x = english_categories, y = proportion,
                       fill = alien_status)) +
    geom_bar(stat = "identity", position = "stack",
             color = "white", linewidth = 0.3) +
    scale_y_continuous(labels = scales::percent,
                       expand = expansion(mult = c(0, 0.02))) +
    scale_fill_manual(values = simple_colours, name = "Status") +
    labs(x = "Development Category", y = "Proportion of SOR") +
    base_theme +
    drop_x)

# Panel e - alien vs native, species
(panel_e <- ggplot(alien_simple_sp,
                   aes(x = english_categories, y = proportion,
                       fill = alien_status)) +
    geom_bar(stat = "identity", position = "stack",
             color = "white", linewidth = 0.3) +
    scale_y_continuous(labels = scales::percent,
                       expand = expansion(mult = c(0, 0.02))) +
    scale_fill_manual(values = simple_colours, name = "Status") +
    labs(x = "Development Category", y = "Proportion of Unique Species") +
    base_theme +
    drop_x)

# Panel f - alien risk, SOR
(panel_f <- ggplot(alien_risk_sor,
                   aes(x = english_categories, y = proportion,
                       fill = risk_category)) +
    geom_bar(stat = "identity", position = "stack",
             color = "white", linewidth = 0.3) +
    risk_shared_y +
    scale_fill_manual(values = risk_colours, labels = risk_labels,
                      name = "Alien Species Risk", drop = FALSE) +
    labs(x = "Development Category", y = "Proportion of SOR") +
    base_theme)

# Panel g - alien risk, species
(panel_g <- ggplot(alien_risk_sp,
                   aes(x = english_categories, y = proportion,
                       fill = risk_category)) +
    geom_bar(stat = "identity", position = "stack",
             color = "white", linewidth = 0.3) +
    risk_shared_y +
    scale_fill_manual(values = risk_colours, labels = risk_labels,
                      name = "Alien Species Risk", drop = FALSE) +
    labs(x = "Development Category", y = "Proportion of Unique Species") +
    base_theme)

# 5. COMBINE WITH PATCHWORK ----------------------------------------------------

# Use fixed guide_area to have the same relative width for the legend across plots
legend_w <- 0.7
row_taxo  <- (panel_a | guide_area()) +
  plot_layout(widths = c(2, legend_w), guides = "collect")
row_red   <- (panel_b | panel_c | guide_area()) +
  plot_layout(widths = c(1, 1, legend_w), guides = "collect")
row_alien <- (panel_d | panel_e | guide_area()) +
  plot_layout(widths = c(1, 1, legend_w), guides = "collect")
row_risk  <- (panel_f | panel_g | guide_area()) +
  plot_layout(widths = c(1, 1, legend_w), guides = "collect")

# Add labels
combined_figure <- (row_taxo / row_red / row_alien / row_risk) +
  plot_layout(heights = c(1, 1, 1, 1)) +
  plot_annotation(tag_levels = "a", tag_suffix = ")") &
  theme(plot.tag = element_text(face = "bold", size = 16),
        legend.justification = "left")

# Chek if looks ok
combined_figure

# 6. SAVE FIGURE ---------------------------------------------------------------

ggsave(here("figures", "Figure_combined_taxo_redlist_alien.png"),
       plot = combined_figure, width = 15, height = 26, dpi = 600)

ggsave(here("figures", "Figure_combined_taxo_redlist_alien.pdf"),
       plot = combined_figure, width = 15, height = 26)

# END OF SCRIPT ----------------------------------------------------------------