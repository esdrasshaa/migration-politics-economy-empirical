# Estimate every model in config.R. Run from the project root.
source("config.R"); source("R/00_setup.R")
results <- run_all()
print(read.csv(file.path(CFG$paths$tab, "SUMMARY_all_models.csv")))
