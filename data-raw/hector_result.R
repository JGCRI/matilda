## code to prepare `obs_gmst` dataset

## computing Hector result (single run) for use in observed temperature
## data adjustment.

# initiating a new core with the SSP 245 ini file
ini <- system.file("input/hector_ssp245.ini", package = "hector")
core <- newcore(ini)

# Run Hector with initiated core
hector_run <- run(core)

# Fetch some results
hector_result <- fetchvars(core, dates = NA, vars = GMST())

# Save data
usethis::use_data(hector_result, overwrite = TRUE)
