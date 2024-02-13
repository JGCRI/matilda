
<!-- README.md is generated from README.Rmd. Please edit that file -->

# matilda

<!-- badges: start -->

[![R-CMD-check](https://github.com/jk-brown/matilda/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jk-brown/matilda/actions/workflows/R-CMD-check.yaml) [![codecov](https://codecov.io/gh/jk-brown/matilda/branch/main/graph/badge.svg?token=4XiWRQQypv)](https://codecov.io/gh/jk-brown/matilda)

<!-- badges: end -->

The `matilda` package is a probabilistic analysis framework for the Hector simple climate model. `matilda` adds the capability to include parameter uncertainty for multiple Hector model iterations. Additionally, functions in `matilda` give users to ability to score Hector runs by comparing results to observed historical data and  compute probabilitistic projections of climate variable metrics.  

# Introduction

`matilda` is a package that provides a probabilistic framework to the Hector simple climate model. 
This vignette works through the basic functionality of `matilda`. 
First, it shows how to generate a random sample of important model parameters from arbitrary (most commonly, normal or lognormal) distributions. 
It then shows how to run Hector using the generated parameters, score model outputs for realism, and compute probabilities of climate variable projections.
Finally, this vignette will show how to calculate probabilistic projections of future climate change using Shared Socio-economic Pathways (SSPs).

#### 

## Configure Hector core

First, load the `matilda` package

```{r setup}
library(matilda)
```

Next, we will initialize a "core" for a new Hector instance. 
More information about establishing a new core for running Hector can be found in the tutorial for using the [Hector R interface](https://jgcri.github.io/hector/articles/intro-to-hector.html).

```{r}
# Read INI file with emission scenario of interest and create a new core
ini <- system.file("input/hector_ssp245.ini", package = "hector")
c_ssp245 <- newcore(ini)
```

This new Hector core is a self-contained object with information about Hector inputs and outputs.

## Generate parameter values

We generate parameter values using `generate_params()`. 
This function uses parameter values from the established Hector core to set the mean of a distribution (typically normal or lognormal) for each parameter, and provides parameter standard deviations based on the science literature.
It then uses the number of `draws` (supplied by the user) to generate randomly sampled values from the parameters' respective distributions.

```{r}
# Generate a data frame of random parameter values
set.seed(1)
param_values <- generate_params(c_ssp245, draws = 10)
```

We now have a data frame object with 10 random values sampled for each parameter.

It may be of interest to the user to only apply changes to a few model parameters while keeping the other static. We can easily enforce this type of control by manipulating which columns in our `param_values` data frame. For example, if we are only interested in perturbing CO2 fertilization and ocean heat diffusivity (parameters `BETA()` and `DIFFUSIVITY()`, respectively), we can simply drop all other columns in the `param_values` data frame.

In these cases, only the parameters included in the subsetted data frame will be altered during Hector runs. All other parameters will follow Hector defaults.

# Running Hector iteratively

The `iterate_model()` runs Hector multiple times, setting new parameter values with each iteration, and combining the results for easy analysis.

Running `iterate_model()` requires a Hector core and a data frame of parameter values.

```{r}
# Run Hector repeatedly over all parameter values
results <- iterate_model(
  core = c_ssp245,
  params = param_values
)
```

The resulting data frame has `r nrow(results)` rows: 10 samples * 556 years * 4 variables; i.e., it returns 10 separate runs (as indicated by `run_number`) for the major climate variables of a Hector output for the years `1745:2300`.

# Screening Hector results

Before further analysis, we frequently want to score the model results against current observations and/or future climate from Earth System Models (see e.g. [Goodwin and Cael 2021](https://esd.copernicus.org/articles/12/709/2021/)).
This score is then used to weight the runs in the final probabilistic calculation.

Scoring Hector runs requires a **scoring criterion**. Scoring criterion are objects used to aid in scoring model runs. The criterion defines information that can be filtered from the Hector result and used to score Hector runs against observations. The `matilda` package provides several pre-defined criteria for easy use, but also gives the user the ability to create their own arbitrary criteria using the `new_criterion()` function. For example:

```{r}
# Create a new criterion that can be used to screen Hector runs
# In this case, we set up a global temperature criterion, with the 'observations'
# running from 1951 to 2000
my_criterion <- new_criterion(GLOBAL_TAS(),
  years = 1951:2000,
  obs_values = seq(0.4, 1.0, length.out = 50)
)

```

For this example we will take advantage of an internal criterion - `criterion_co2_obs()`, which uses [Mauna Loa atmospheric CO2 observations](https://gml.noaa.gov/ccgg/trends/) to score Hector results.

We can score Hector runs with the function `score_runs()` which uses any **scoring function** to screen Hector results. A scoring function is a mathematical function that weights model realizations based on proximity to observed dat (i.e., to a criterion).

Here we use a `matilda`-provided scoring function called `score_ramp` that linearly ramps down a model run's score as it gets farther away from observations: specifically, model runs with differences $\le$ the `w1` parameter will score 1.0, while runs with differences $\ge$ `w2` will score 0.0, and model run scores decrease linearly from `w1` to `w2`.

```{r}
# Score Hector runs with observed CO2 data
scores <- score_runs(results, criterion_co2_obs(), score_ramp, w1 = 2, w2 = 20)

```
#### 

# Defining and calculating output metrics

**Metric objects** define what data the user is most interested in from a Hector core. For example, we can use a metric object defined as:

`Probabilistic Hector Metric:  mean global_tas 2000  to  2100`

to estimate mean global air temperature anomaly (`global_tas`) in years 2000-2100 from each Hector model iteration.

The `new_metric()` function can be used to define any arbitrary metric.

We create a metric indicating that we want to filter the Hector results to include global average air temperature anomaly (`global_tas`) for the years 2000 to 2100. We also specify that we are interested in computing the _mean_ of the temperature for these years

```{r}
# Define a new_metric object for Hector analysis
my_metric <- new_metric(GLOBAL_TAS(), 2000:2100, mean)

```

After defining a new metric of interest, we can use `metric_calc()` to compute metric values for each run:

```{r}
# Compute metric values
metric_values <- metric_calc(results, my_metric)

```

This result shows the mean global air temperature anomaly by the end of 2100 for each of our ten model runs.

#### 

# Weighted probabilistic projections

We now have the (1) calculated metric values and (2) scored model runs, and so can compute the likelihood of different climatic outcomes. The `prob_calc()` function allows the user to group metric values into bins and calculates probabilities of each outcome, weighted by the scores of the Hector runs.

For example, we may be interested in the probability that mean `global_tas` will remain below 2.0$^\circ$ C by the end of 2100:

```{r}
# Establish bin limits for grouping metric values
bins <- c(0, 1, 2, 3)

# Calculate probabilities for global_tas under 2C
prob_calc(metric_values$metric_result,
  bins = bins,
  scores = scores$weights
)
```

The `prob_calc()` function bins each metric and sums the scores of each Hector run as it is binned. Then, it uses the summed score of each bin to compute the probability of each climate variable outcome.

The output above shows the likelihood that the mean rise in global air temperature anomaly will fall within each of the temperature ranges we defined (0-1 C, 1-2 C, and 2-3 C) by the end of the 21st century, when we use our weighted Hector simulations to compute temperature probabilities.
