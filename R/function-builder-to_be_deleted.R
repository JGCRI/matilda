## goal: write a loop that will use values from the generate_params df for
## running each of the hector runs.
## Take a look at Leeyas code. "common_date.R" and "01_generate_data.Rmd"
##


param_name <- c("BETA" = BETA(),
                "Q_10" = Q10_RH(),
                "NPP_FLUX0" = NPP_FLUX0(),
                "AERO_SCALE" = AERO_SCALE())

param_units <- c("BETA" = "(unitless)",
                 "Q_10" = "(unitless)",
                 "NPP_FLUX0" = "PgC/yr",
                 "AERO_SCALE" = "(unitless)")


## Loop: for each index in the gen_params df setvars using those values and run hector
## for as many values have been produced for each parameter.
ini <- system.file("input/hector_ssp245.ini", package = "hector")
core <- newcore(ini)

iterative_hector <- function(core, var, years, params) {

  # store results
  result_list <- list()

  # set number of model iterations
  for(i in colnames(params)) {

    # set var
    setvar(core, NA, do.call(i, list()), params[i][[1]], unit = param_units[i])

    # resets model after each run
    reset(core, date = 0)

    # run the model
    run(core)

    # fetch model results based on function arguments provided by the user
    # Stores in object 'dat'
    dat <- fetchvars(core = core, dates = years, vars = var)

    # stores resulting dfs (dat) from each run in result_list()
    result_list[[i]] <- dat

  }

  # binds rows from result_list into df
  df = as.data.frame(do.call("rbind", result_list))

  # what to return?
  return(df)
}
