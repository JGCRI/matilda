## Source observed data to be stored internally
source("prep_observed_gmst_data.R")
source("prep_observed_co2_data.R")
source("prep_observed_ocean_c_uptake_data.R")

# Add observed gmst data (adjusted) for internal use
usethis::use_data(observed_gmst_data,
                  observed_co2_data,
                  observed_ocean_c_uptake_data,
                  overwrite = TRUE, internal = TRUE)

