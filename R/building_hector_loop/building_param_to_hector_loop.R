# set-up
ini <- system.file("input/hector_ssp245.ini", package = "hector")

core <- newcore(ini)

par <- generate_param(10)
param_values <- unlist(par [1,])

# looping hector using generated param values
## Function and test - test error and are vars set correctly for core
stopifnot(is.numeric(param_values))
for (i in seq_along(param_values)) {
print(i)
  fn_name <- names(param_values)[i]
  var <- do.call(fn_name, list())
  var_units <- getunits(var)
  message("setting ", var, " to ", param_values[i])
  setvar(core, NA, var = var, values = param_values[i], unit = var_units)
}

  reset(core)

  run(core)

  dat <- fetchvars(core)

  result <- list()

  result[[i]] <- dat

}

# checking what do.call output produces
for (i in colnames(par)) {

  print(do.call(i, list()))

  }
# does not match the colnames in par

# checking if list() gives correct colnames
for (i in colnames(par)) {

  print(list(i))

  }
# adding [] or [[]] to list() doesn't change the output as I would expect
# gives same result as list(i)

# checking this lists parameter values
for (i in colnames(par)){
  print(par [i] [[1]])
}
