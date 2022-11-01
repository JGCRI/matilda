# set-up
ini <- system.file("input/hector_ssp245.ini", package = "hector")

core <- newcore(ini)

par <- gen_param(10)


# looping hector using generated param values

for (i in colnames(par)) {

  params <- list(i)

  setvar(core, NA, list(i[[1]]), par[i] [[1]], unit = c( "BETA" = "(unitless)",
                                                         "Q10_RH" = "(unitless)",
                                                         "NPP_FLUX0" = "PgC/yr",
                                                         "AERO_SCALE" = "(unitless)")[i])
  reset(core)

  run(core)

  dat <- fetchvars(core)

  result <- list()

  result[[i]] <- dat

}

for (i in colnames(par)){
  print(par [i] [[1]])
}
