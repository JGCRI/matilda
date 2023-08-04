## Mauna Loa data as the observed CO2 values for scoring Hector runs.

## Co2 data were download on 01/03/2023 from
## https://gml.noaa.gov/ccgg/trends/data.html}{https://gml.noaa.gov/ccgg/trends/data.html

# Reading in Mauna Loa annual mean CO2 data
observed_data_co2 <- read.csv("data-raw/co2_annmean_maunaloa.csv")

# rename 'mean' to co2_ppm to represent unit of original CO2 measurements
colnames(observed_data_co2)[colnames(observed_data_co2) == "mean"] = "co2_ppm"

# remove 'unc' (standard deviation) column
observed_data_co2$unc <- NULL

