## code to prepare 'adjusted_gmst_data'

## HADCRUT data as observed temperature anomaly values for scoring Hector runs.
## Temperature anomaly data were downloaded on 05/22/2023 from
## https://www.metoffice.gov.uk/hadobs/hadcrut5/data/current/download.html

# Reading in HADCRUT5 global temperature anomaly data
observed_data_gmst <- read.csv("data-raw/temp_anomaly_annmean_hadcrut5.csv")

# rename 'anomaly' to anomaly_C' to give unit information
colnames(observed_data_gmst)[colnames(observed_data_gmst) == 'anomaly'] = "temperature_C"

# remove confidence bounds in df - leaving only year and mean anomaly cols
observed_data_gmst$Lower.confidence.limit..2.5.. <- NULL
observed_data_gmst$Upper.confidence.limit..97.5.. <- NULL

# Subset temperature data to include data from 1959-2023
subset_observed_gmst <- subset(observed_data_gmst,
                               year >= 1850,
                               year <= 2023)

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
  normalized_anomaly <- observed_data$temperature_C + mean_modeled_anomaly

  # Create a new data frame with the normalized data
  normalized_data <- data.frame(year = observed_data$year, anomaly_C = normalized_anomaly)

  return(normalized_data)
}

# producing adjusted gmst values using a hector result
adjusted_gmst_data <- normalize_temperature(subset_observed_gmst, hector_result, 1961, 1990)

# Add observed gmst data (adjusted) for internal use
usethis::use_data(adjusted_gmst_data, observed_co2_data, overwrite = TRUE, internal = TRUE)
