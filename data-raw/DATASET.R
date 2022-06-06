## code to prepare `nems_sample` dataset goes here
# This is an excerpt of ten stores from the raw Qualtrics data.
library(haven)

# Read the file, and then save the cleaned version in the package
nems_sample <- read_sav("data-raw/example_data.sav") |>
  clean_nemss()
usethis::use_data(nems_sample, overwrite = TRUE)
