## Mauna Loa data as the observed CO2 values for scoring Hector runs.

## Co2 data were download on 01/03/2023 from
## https://gml.noaa.gov/ccgg/trends/data.html}{https://gml.noaa.gov/ccgg/trends/data.html

# Reading in Mauna Loa annual mean CO2 data
metricdata_co2 <- read.csv("data-raw/co2_annmean_maunaloa.csv")

# rename 'mean' to co2_ppm to represent unit of original CO2 measurements
colnames(metricdata_co2)[colnames(metricdata_co2) == "mean"] = "co2_ppm"

# remove 'unc' (standard deviation) column
metricdata_co2$unc <- NULL

## HADCRUT data as observed temperature anomaly values for scoring Hector runs.

## Temperature anomaly data were downloaded on 05/22/2023 from
## https://www.metoffice.gov.uk/hadobs/hadcrut5/data/current/download.html

# Reading in HADCRUT5 global temperature anomaly data
metricdata_gmst <- read.csv("data-raw/temp_anomaly_annmean_hadcrut5.csv")

# rename 'anomaly' to anomaly_C' to give unit information
colnames(metricdata_gmst)[colnames(metricdata_gmst) == 'anomaly'] = "temperature_C"

# remove confidence bounds in df - leaving only year and mean anomaly cols
metricdata_gmst$Lower.confidence.limit..2.5.. <- NULL
metricdata_gmst$Upper.confidence.limit..97.5.. <- NULL

#' Normalizing variables to reflect specific reference period
#'
#' @param observed_data Observed data frame. This function is specifically for adjusting observed temperature.
#' @param modeled_data Data from a Hector result that contains the global mean surface temperature.
#' @param reference_start_year Start year of reference period
#' @param reference_end_year End year of reference period
#'
#' @return A data frame of normalized temperature values
#'
normalize_temperature <- function(observed_data, modeled_data, reference_start_year, reference_end_year) {

  # Filter modeled data for the reference period
  modeled_reference_period <- subset(modeled_data,
                                     year >= reference_start_year &
                                       year <= reference_end_year)

  # Calculate the mean anomaly_C for the modeled reference period
  mean_modeled_anomaly <- mean(modeled_reference_period$value)

  # Calculate normalized anomaly_C for each year in the observed data
  normalized_anomaly <- observed_data$anomaly_C - mean_modeled_anomaly

  # Create a new data frame with the normalized data
  normalized_data <- data.frame(year = observed_data$year, anomaly_C = normalized_anomaly)

  return(normalized_data)
}

metricdata_gmst <-

# Add metric data for internal use
usethis::use_data(metricdata_co2, metricdata_tas, overwrite = TRUE, internal = TRUE)
