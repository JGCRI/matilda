## goal: write a loop that will use values from the generate_params df for
## running each of the hector runs.
## Take a look at Leeyas code. "common_date.R" and "01_generate_data.Rmd"
##

param_name <- c("BETA" = BETA(),
                "Q10_RH" = Q10_RH(),
                "NPP_FLUX0" = NPP_FLUX0(),
                "AERO_SCALE" = AERO_SCALE())

param_units <- c("BETA" = "(unitless)",
                 "Q10_RH" = "(unitless)",
                 "NPP_FLUX0" = "PgC/yr",
                 "AERO_SCALE" = "(unitless)")
