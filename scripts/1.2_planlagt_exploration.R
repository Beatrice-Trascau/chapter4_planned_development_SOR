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

# 3. CREATE SUMMARY STATISTICS TABLE  ------------------------------------------

## 3.1. Quick initial data exploration -----------------------------------------

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

## 3.2. Calculate polygon areas ------------------------------------------------

# Alredy have a colum givin area in m2 (planlagt_areal_m2) - but it is detected as <chr> by R, convert to numeric
development_polygons$area_m2_numeric <- as.numeric(development_polygons$planlagt_areal_m2)

# Convert to km2
development_polygons$area_km2 <- development_polygons$area_m2 / 1e6

# Check if NA values were created
sum(is.na(development_polygons$area_m2)) #0!
