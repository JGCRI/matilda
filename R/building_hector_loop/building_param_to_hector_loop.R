# set-up
ini <- system.file("input/hector_ssp245.ini", package = "hector")

core <- newcore(ini)

par <- generate_param(10)


# looping hector using generated param values

for (i in colnames(par)) {

  setvar(core, NA, list(i), par[i] [[1]], unit = c( "BETA" = "(unitless)",
                                                         "Q10_RH" = "(unitless)",
                                                         "NPP_FLUX0" = "PgC/yr",
                                                         "AERO_SCALE" = "(unitless)")[i])
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
