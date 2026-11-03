# Packages and sourcing -- nothing else happens here.
suppressPackageStartupMessages({
  library(dplyr); library(readxl); library(MASS); library(MCMCpack)
})

# Make sure select() is dplyr's, not MASS's
select <- dplyr::select

for (f in c("01_data.R", "02_checks.R", "03_diagnostics.R",
            "04_bvar.R", "05_irf_fevd.R", "06_plots.R", "07_run.R"))
  source(file.path("R", f))
