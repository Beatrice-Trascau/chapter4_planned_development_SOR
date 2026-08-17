##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 7.2_H5c_models
# This script contains code to test H5c: Polygons will have higher richness of 
# alien species than buffers than areas outside of the planned developments
##-----------------------------------------------------------------------------#

# 1. LOAD DATA -----------------------------------------------------------------

# Source code
library(here)
source(here("scripts", "0_setup.R"))

# Load the per-side data built in 7.1
model_data <- readRDS(here("data", "derived_data", "h5_polygon_buffer_data.rds"))

# Load the impact-split per-side datasets that is also built in 7.1
model_data_high  <- readRDS(here("data", "derived_data",
                                 "h5_highimpact_polygon_buffer_data.rds"))
model_data_lower <- readRDS(here("data", "derived_data",
                                 "h5_lowerimpact_polygon_buffer_data.rds"))

# Quick summary
cat("Rows loaded:", nrow(model_data), "\n") # 259762 (2 per pair)
print(table(model_data$polygon_type))
# Buffer  Development 
# 129881      129881 

# 2. PREPARE DATA FOR MODELLING ------------------------------------------------

# Create the dataset for modelling
richness_data <- model_data |>
  mutate(polygon_type = factor(polygon_type, levels = c("Buffer", "Development")),
         log_area_c = as.numeric(scale(log(area_m2_numeric), scale = FALSE)),
         land_cover_name = factor(land_cover_name),
         kommune_factor = factor(kommune),
         pair_id_factor = factor(pair_id))

# Summarise the response
cat("\n=== Alien species richness summary ===\n")
print(summary(richness_data$n_species))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.0000   0.0000   0.0000   0.1009   0.0000 121.0000 
cat("Proportion of sides with zero alien species:",
    round(mean(richness_data$n_species == 0), 3), "\n")   # expect ~0.956
cat("\nRichness distribution among record-bearing sides (note the dominance of 1):\n")
print(table(richness_data$n_species[richness_data$n_species > 0]))
cat("\nMean alien richness by side:\n")
print(tapply(richness_data$n_species, richness_data$polygon_type, mean))
# Buffer     Development 
# 0.12697007  0.07493013 


















