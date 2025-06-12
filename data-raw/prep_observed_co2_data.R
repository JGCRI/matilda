## code to prepare 'observed_co2_data'

## Mauna Loa data as the observed CO2 values for scoring Hector runs.
## Lan et al. 2023. Trends in globally-averaged CO2 determined from NOAA global monitoring laboratory measurements.
## Downloaded 01/03/2023
## Accessed from: https://gml.noaa.gov/ccgg/trends/data.html}{https://gml.noaa.gov/ccgg/trends/data.html

# Reading in Mauna Loa annual mean CO2 data
observed_data_co2 <- read.csv("data-raw/co2_maunaloa_raw.csv")

# rename 'mean' to co2_ppm to represent unit of original CO2 measurements
colnames(observed_data_co2)[colnames(observed_data_co2) == "mean"] <- "co2_ppm"

# remove 'unc' (standard deviation) column
observed_co2_data$unc <- NULL
