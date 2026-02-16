##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 4.3_H3_GLMM
# This script contains code to test Hypothesis 3: Urban and near-urban polygons
# will have more SOR than othe planned development types
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source setup file
library(here)
source(here("scripts", "0_setup.R"))

# Load polygon all data
polygon_all_data <- readRDS(here("data", "derived_data",
                                 "polygons_occurrences_all_data.rds"))

# 2. PREPARE DATA FOR MODELING -------------------------------------------------

# Filter out Ports & Marinas from planned development category
model_data <- polygon_all_data |>
  filter(english_categories != "Ports") |>
  # log-transform area
  mutat(log_area = log(area_m2_numeric),
        # convert development category and kommune to factors
        development_category = as.factor(english_categories),
        kommune_factor = as.factor(kommune))

# Check if there are any NAs in key variables
cat("n_occurrences:", sum(is.na(model_data$n_occurrences)), "\n")
cat("log_area:", sum(is.na(model_data$log_area)), "\n")
cat("development_category:", sum(is.na(model_data$development_category)), "\n")
cat("kommune_factor:", sum(is.na(model_data$kommune_factor)), "\n")

# Remove rows with NAs in model variables
model_data <- model_data |>
  filter(!is.na(n_occurrences),
         !is.na(log_area),
         !is.na(development_category),
         !is.na(kommune_factor))

# Check sample size for model
cat("\nFinal sample size for model:", nrow(model_data), "polygons\n")
cat("Number of municipalities:", n_distinct(model_data$kommune_factor), "\n")
cat("Development categories:", paste(levels(model_data$development_category), collapse = ", "), "\n")
