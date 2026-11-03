# Quick run of a single model:  edit the id below.
source("config.R"); source("R/00_setup.R")
res <- run_spec(SPECS[["unrate_gvt_2007_2025"]])
print(res$band_tab); print(res$diagnostics)
