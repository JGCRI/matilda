## Ocean Carbon Uptake

## code to prepare 'observed_ocean_c_uptake_data'

## Ocean C uptake data from The Global Carbon Project's 2023 Global Carbon Budget.
## Friedlingstein et al. Global Carbon Budget 2023, Earth Sys. Sci. Data, 15, 5301-5369.
## Downloaded 03/26/2024
## Accessed from: https://globalcarbonbudgetdata.org/latest-data.html


# Reading in ocean carbon uptake data
observed_data_ocean <- read.csv("data-raw/csv/gcp_data.csv", stringsAsFactors = FALSE)

# Rename columns for consistency with internal package naming
names(observed_data_ocean)[names(observed_data_ocean) == "Year"] <- "year"
names(observed_data_ocean)[names(observed_data_ocean) == "ocean_sink"] <- "ocean_uptake"

# Subset to retain only relevant columns
observed_ocean_c_uptake_data <- observed_data_ocean[,c("year", "ocean_uptake")]

# Remove any rows with missing data
observed_ocean_c_uptake_data <- na.omit(observed_ocean_c_uptake_data)


#### GMST

## code to prepare 'observed_gmst_data'

## HADCRUT data as observed temperature anomaly values for scoring Hector runs.
## Morice et al. 2021. An updated assessment of near-surface temperature change from 1850: the HadCRUT5 data set. Journal of Geophysical Research.
## Downloaded 05/22/2023
## Accessed from: https://www.metoffice.gov.uk/hadobs/hadcrut5/data/current/download.html

# Reading in HADCRUT5 global temperature anomaly data
observed_data_gmst <- read.csv("data-raw/csv/gmst_hadcrut5_raw.csv")

# rename 'anomaly' to anomaly_C' to give unit information
colnames(observed_data_gmst)[colnames(observed_data_gmst) == "anomaly"] <- "temperature_C"

# remove confidence bounds in df - leaving only year and mean anomaly cols
observed_data_gmst$Lower.confidence.limit..2.5.. <- NULL
observed_data_gmst$Upper.confidence.limit..97.5.. <- NULL

# Subset temperature data to include data from 1959-2023
subset_observed_gmst <- subset(
  observed_data_gmst,
  year >= 1950,
  year <= 2023
)

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
  modeled_reference_period <- subset(
    modeled_data,
    year >= reference_start_year &
      year <= reference_end_year
  )

  # Calculate the mean anomaly_C for the modeled reference period
  mean_modeled_anomaly <- mean(modeled_reference_period$value)

  # Calculate normalized anomaly_C for each year in the observed data
  normalized_anomaly <- observed_data$temperature_C + mean_modeled_anomaly

  # Create a new data frame with the normalized data
  normalized_data <- data.frame(year = observed_data$year, anomaly_C = normalized_anomaly)

  return(normalized_data)
}

# producing adjusted gmst values using a hector result

observed_gmst_data <- normalize_temperature(subset_observed_gmst, matilda::hector_result, 1961, 1990)


#### CO2 Concentrations

## code to prepare 'observed_co2_data'

## Mauna Loa data as the observed CO2 values for scoring Hector runs.
## Lan et al. 2023. Trends in globally-averaged CO2 determined from NOAA global monitoring laboratory measurements.
## Downloaded 01/03/2023
## Accessed from: https://gml.noaa.gov/ccgg/trends/data.html}{https://gml.noaa.gov/ccgg/trends/data.html

# Reading in Mauna Loa annual mean CO2 data
observed_co2_data <- read.csv("data-raw/csv/co2_maunaloa_raw.csv")

# rename 'mean' to co2_ppm to represent unit of original CO2 measurements
colnames(observed_co2_data)[colnames(observed_co2_data) == "mean"] <- "co2_ppm"

# remove 'unc' (standard deviation) column
observed_co2_data$unc <- NULL


#### Add to package data

# Add observed gmst data (adjusted) for internal use
usethis::use_data(observed_gmst_data,
                  observed_co2_data,
                  observed_ocean_c_uptake_data,
                  overwrite = TRUE, internal = TRUE)

