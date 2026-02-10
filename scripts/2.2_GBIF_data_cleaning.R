##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 2.2_GBIF_data_cleaning
# This script contains code which cleans the downloaded GBIF occurrence records
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source setup file
library(here)
source(here("scripts", "0_setup.R"))

# Load GBIF occurrence records
load(here("data","raw_data", "occurrences_chapter4_October2025.txt"))

# 2. CLEAN RECORDS -------------------------------------------------------------

# Original number of occurrences in raw dataframe: 
nrow(occurrence) 

# 2.1. Remove material citations, fossil specimens, and living specimens -------

clean_occurrences1 <- occurrence |>
  filter(!basisOfRecord %in% c("MATERIAL_CITATION", "FOSSIL_SPECIMEN",
                               "LIVING_SPECIMEN"))

# Occurrences left after material citations, fossil specimens and living specimens removed
nrow(clean_occurrences1) 

# 2.2. Remove records that are not Animalia, Plantae or Fungi ------------------

clean_occurrences2 <- clean_occurrences1 |>
  filter(kingdom %in% c("Animalia", "Plantae", "Fungi"))

# Check how many records are left
nrow(clean_occurrences2) 

# 2.3. Remove records with no registered species-level information -------------
clean_occurrences3 <- clean_occurrences2 |>
  filter(specificEpithet != "")

# Check how many records are left
nrow(clean_occurrences3) 

# 2.4. Remove duplicate records ------------------------------------------------
clean_occurrences4 <- clean_occurrences3 |>
  distinct()

# Check how many records are left
nrow(clean_occurrences4) # 28 269 295 -> there were no duplicated occurrences

# 2.5. Remove flagged records --------------------------------------------------

# Identify flagged records
coordinate_flags <- clean_coordinates(x = clean_occurrences4,
                                      lon = "decimalLongitude",
                                      lat = "decimalLatitude",
                                      species = "species",
                                      tests = c("equal", "gbif", "zeros"))

# Get a summary of the records
summary(coordinate_flags) # no flagged records

# 3. PREP DF FOR ANALYSIS ------------------------------------------------------

# Check column names
colnames(clean_occurrences4)

# Remove unnecessary columns and add 1 more column
clean_occurrences <- clean_occurrences4 |>
  select(gbifID, identifiedBy, basisOfRecord, occurrenceStatus,
         eventDate, year, countryCode, stateProvince, county, municipality,
         locality, decimalLatitude, decimalLongitude, 
         coordinateUncertaintyInMeters, kingdom, phylum, class, order, family,
         genus, specificEpithet, speciesKey, species, organismQuantity,
         occurrenceStatus, scientificName, eventID, parentEventID, samplingEffort) 

# 4. REMOVE RECORDS BASED ON COORDINATE UNCERTAINTY ----------------------------

# Largest polygon in planned development = 18 km2 => Only keep records with coordinate uncertainty lower than 15km (15000m)
# Remove records with coord uncertainty >15000m
clean_occurrences_15km <- clean_occurrences |>
  filter(coordinateUncertaintyInMeters < 15000 &
           !is.na(coordinateUncertaintyInMeters))

# Check how many records are left in the cleaned df
nrow(clean_occurrences_15km) # 

# 5. REMOVE RECORDS WITH OCCURRENCESTATUS = ABSENT -----------------------------

# Only keep records with occurrenceStatus = "PRESENT" 
clean_occurrences_15km <- clean_occurrences_15km |>
  filter(occurrenceStatus == "PRESENT")

# Check how many records are left in the cleaned df
nrow(clean_occurrences_15km) #

# Save cleaned occurrences
write.csv(clean_occurrences_15km,
          here("data", "derived_data", "clean_occurrences_15km.txt"))

# END OF SCRIPT ----------------------------------------------------------------