## code to prepare 'observed_ocean_c_uptake_data'

## Ocean C upatake data from The Global Carbon Project's 2023 Global Carbon Budget.
## Friedlingstein et al. Global Carbon Budget 2023, Earth Sys. Sci. Data, 15, 5301-5369.
## Downloaded 03/26/2024
## Accessed from: https://globalcarbonbudgetdata.org/latest-data.html


# Reading in ocean carbon uptake data
observed_data_ocean <- read.csv("data-raw/gcp_data.csv", stringsAsFactors = FALSE)

# Rename columns for consistency with internal package naming
names(observed_data_ocean)[names(observed_data_ocean) == "Year"] <- "year"
names(observed_data_ocean)[names(observed_data_ocean) == "ocean_sink"] <- "ocean_uptake"

# Subset to retain only relevant columns
observed_ocean_c_uptake_data <- observed_data_ocean[,c("year", "ocean_uptake")]

# Remove any rows with missing data
observed_ocean_c_uptake_data <- na.omit(observed_ocean_c_uptake_data)
