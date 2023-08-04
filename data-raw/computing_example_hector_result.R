## Computing Matilda results for use in examples

# initiating a new core with the SSP 245 ini file
ini <- system.file("input/hector_ssp245.ini", package = "hector")
core <- newcore(ini)

# generating params and running Hector with param uncertainty
params <- generate_params(core, 10)
matilda_result <- iterate_hector(core, params)

# Adding the new example Hector output as a data set in data/
usethis::use_data(matilda_result, overwrite = TRUE)
