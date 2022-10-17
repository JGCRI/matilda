
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hectorpractice

<!-- badges: start -->
<!-- badges: end -->

The `hectorpractice` package can be used to run Hector in an interative
process with the capability of computing probabilistic outcomes of
climatic variables in provided year ranges for different climate impact
scenarios.

## Installation

You can install the development version of `hectorpractice` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ecolo-joe/hectorpractice")
```

## Example

This is a basic example which shows you how to use the iterative_hector
function to run Hector 10 times for a provided emission scenario to
produce a data frame of median global average temperature increase
(variable = global_tas) for the years 2000-2100:

``` r
library(hectorpractice)
```

First, an emission scenario must be loaded and stored in a core to
initialize Hector. Scenarios are stored in the `hector` package in INI
config format.

``` r
# Load ini file 
ini_file <- system.file("input/hector_ssp245.ini", package = "hector")

# Build core with ini file
ssp245 <- hector::newcore(ini_file)
```

Once a core is constructed it can be passed to the `iterative_hector`
function. The user will also need to supply other required arguments.

``` r
# Identify climatic variables and years of interest
variable <- c("global_tas")
year_range <- 2000:2100

# Use core to run iterative_hector
results <- iterative_hector(ssp245,
                            op = median,
                            var = variable,
                            years = year_range,
                            runs = 10)

# View results 
head(results)
#>              scenario year   variable     value units      beta      q10
#> 1 Unnamed Hector core 2000 global_tas 0.4067987  degC 0.6459486 1.476804
#> 2 Unnamed Hector core 2001 global_tas 0.4387894  degC 0.6459486 1.476804
#> 3 Unnamed Hector core 2002 global_tas 0.4606619  degC 0.6459486 1.476804
#> 4 Unnamed Hector core 2003 global_tas 0.4767116  degC 0.6459486 1.476804
#> 5 Unnamed Hector core 2004 global_tas 0.4953987  degC 0.6459486 1.476804
#> 6 Unnamed Hector core 2005 global_tas 0.5099142  degC 0.6459486 1.476804
#>   npp_flux aero_scale  metric run_number
#> 1 41.91971   1.081534 1.46416          1
#> 2 41.91971   1.081534 1.46416          1
#> 3 41.91971   1.081534 1.46416          1
#> 4 41.91971   1.081534 1.46416          1
#> 5 41.91971   1.081534 1.46416          1
#> 6 41.91971   1.081534 1.46416          1
```

The result of `iterative_hector` is a data frame filtered to only
include the year range specified by the user for the variables specified
by the user. The data frame also includes the parameter values used for
each hector run (based on random draws from normal distribution).
Parameter values are re-drawn from the normal distribution for each run.
Finally, the `metric` column provides the median `global_tas` for each
run and the `run_number` informs the user of which hector run they are
observing values and metrics from.
