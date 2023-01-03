## Mauna Loa data as the observed CO2 values for scoring Hector runs.

## Co2 data were download on DATE from (add URL)

# Reading in Mauna Loa annual mean CO2 data
metricdata_co2 <- read.csv("co2_annmean_maunaloa.csv")

# rename 'mean' to co2_ppm to represent unit of original CO2 measurements
colnames(metricdata_co2)[colnames(metricdata_co2) == "mean"] = "co2_ppm"

# remove 'unc' (standard deviation) column
metricdata_co2$unc <- NULL

## HADCRUT data as the obserevd temperature values for scoring Hector runs.
##


# Add metric data for internal use
#
usethis::use_data(metricdata_co2, overwrite = TRUE, internal = TRUE)
