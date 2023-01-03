## Mauna Loa data as the observed CO2 values for scoring Hector runs.
## Mauna Loa CO2 data is recorded in ppm.
## This script adds a column for Pg C (ppm * 2.13), renames the 'mean' column to
## represent the units of the original CO2 measurements (co2_ppm), and removes the
## 'unc' column which represents STD for annual mean CO2.

# Reading in Mauna Loa annual mean CO2 data
metricdata_co2 <- read.csv("co2_annmean_maunaloa.csv")

# converting ppm to PgC for comparison to atmos_co2 in Hector
metricdata_co2$co2_PgC <- metricdata_co2$mean * 2.13

# rename 'mean' to co2_ppm to represent unit of original CO2 measurements
colnames(metricdata_co2)[colnames(metricdata_co2) == "mean"] = "co2_ppm"

# remove 'unc' (standard deviation) column
metricdata_co2 <- metricdata_co2[, -3]

## HADCRUT data as the obserevd temperature values for scoring Hector runs.
##


# Add metric data for internal use
#
usethis::use_data(metricdata_co2, overwrite = TRUE, internal = TRUE)
