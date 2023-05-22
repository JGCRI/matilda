## Mauna Loa data as the observed CO2 values for scoring Hector runs.

## Co2 data were download on DATE from (add URL)

# Reading in Mauna Loa annual mean CO2 data
metricdata_co2 <- read.csv("data-raw/co2_annmean_maunaloa.csv")

# rename 'mean' to co2_ppm to represent unit of original CO2 measurements
colnames(metricdata_co2)[colnames(metricdata_co2) == "mean"] = "co2_ppm"

# remove 'unc' (standard deviation) column
metricdata_co2$unc <- NULL

## HADCRUT data as observed temperature anomaly values for scoring Hector runs.

## Temperature anomaly data were downloaded on 05/22/2023 from
## https://www.metoffice.gov.uk/hadobs/hadcrut5/data/current/download.html

# Reading in HADCRUT5 global temmperature anomaly data
metricdata_tas <- read.csv("data-raw/temp_anomaly_annmean_hadcrut5.csv")

# rename 'anomaly' to anomaly_C' to give unit information
colnames(metricdata_tas)[colnames(metricdata_tas) == 'anomaly'] = "anomaly_C"

# remove confidece bounds in df - leaving only year and mean anomaly cols
metricdata_tas$Lower.confidence.limit..2.5.. <- NULL
metricdata_tas$Upper.confidence.limit..97.5.. <- NULL

# Add metric data for internal use
usethis::use_data(metricdata_co2, metricdata_tas, overwrite = TRUE, internal = TRUE)
