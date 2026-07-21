# Load here package
library(segmented)

# Get citation info
cit <- citation("segmented")

# Extract authors
authors <- paste(
  sapply(cit$author, function(a) {
    paste(a$family, paste(a$given, collapse = " "), sep = ", ")
  }),
  collapse = "\nAU  - "
)

# Extract year
year <- cit$year

# Extract title
title <- cit$title

# Extract version
version <- as.character(packageVersion("segmented"))

# Extract URL
url <- cit$url

# Build RIS entry
ris <- c("TY  - COMP",
         paste0("TI  - ", title),
         paste0("AU  - ", authors),
         paste0("PY  - ", year),
         paste0("N1  - R package version ", version),
         paste0("UR  - ", url),
         "ER  -")

# Write .RIS reference to file
writeLines(ris, here("references", "segmented.ris"))
