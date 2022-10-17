# Example of interative_hector usage in hectorpractice package

# load package
library(hectorpractice)

# Ini file
ini_file <- system.file("input/hector_ssp245.ini", package = "hector")

# Initiate new core
ssp245 <- hector::newcore(ini_file)

# Run iterative_hector
result <- iterative_hector(ssp245,
                           op = median,
                           var = "global_tas",
                           years = 2000:2100,
                           runs = 10)

# view results
View(result)
