# initiating a new core with the SSP 245 ini file
ini <- system.file("input/hector_ssp245.ini", package = "hector")
core <- newcore(ini)

# generating params, creating a new metric, and running Hector with
# param uncertainty
# the new metric includes all variables in the output (var = NULL)
params <- generate_params(10)
metric_example <- new_metric(var = c(GLOBAL_TAS(), ATMOSPHERIC_CO2()),
                             years = 1960:2100,
                             op = mean)
hector_result <- iterate_hector(core, metric, params)

# Adding the new example Hector output as a data set in data/
usethis::use_data(hector_result, overwrite = TRUE)

