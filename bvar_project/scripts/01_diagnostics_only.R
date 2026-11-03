# Stationarity + VAR residual checks for every spec, no estimation.
# Run from the project root:  source("scripts/01_diagnostics_only.R")
source("config.R"); source("R/00_setup.R")
tabs <- lapply(SPECS, function(s) {
  pm <- prepare_model(s)                       # resolves p by BIC
  list(stat = stationarity_table(pm$d, pm$spec),
       var  = var_diagnostics(pm$y, pm$spec, pm$x))
})
stat <- do.call(rbind, lapply(tabs, `[[`, "stat"))
vard <- do.call(rbind, lapply(tabs, `[[`, "var"))
dir.create(CFG$paths$tab, recursive = TRUE, showWarnings = FALSE)
write.csv(stat, file.path(CFG$paths$tab, "DIAG_stationarity.csv"), row.names = FALSE)
write.csv(vard, file.path(CFG$paths$tab, "DIAG_var_residuals.csv"), row.names = FALSE)
print(stat[, c("model", "series", "adf_stat", "adf_cv5", "kpss_p", "verdict", "lb_warning")])
print(vard)
