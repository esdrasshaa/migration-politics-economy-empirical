# Immigration sentiment BVAR: restructured project

Run everything from the project root (the folder containing `config.R`).

    source("scripts/01_diagnostics_only.R")   # stationarity + residual checks, no estimation
    source("scripts/02_run_all.R")            # all models in config.R -> outputs/
    source("scripts/03_run_one.R")            # one model

Needs: dplyr, readxl, MASS, MCMCpack, urca, tseries.

## Layout

    config.R            ALL choices: windows, lags, draws, seed, file names, model specs
    R/01_data.R         load + build panel, merged BY DATE
    R/02_checks.R       hard stops (gaps, NA, window coverage, #obs vs #coefficients)
    R/03_diagnostics.R  ADF + KPSS battery with one verdict per series; VAR residual checks
    R/04_bvar.R         diffuse-prior BVAR sampler (rejects unstable draws, reports acceptance rate)
    R/05_irf_fevd.R     IRF and FEVD per draw, summaries, tidy tables
    R/06_plots.R        plots; titles are generated from the sample actually used
    R/07_run.R          run_spec(), run_all()

Each model writes to outputs/: IRF pdf, FEVD pdf, IRF numbers (csv), a band summary
(at which horizons the band excludes 0), the stationarity table, and an .rds.
`SUMMARY_all_models.csv` has one row per model with the trust flags.

GPT-4.1-nano sentiment models (`rob_*_gpt41_*`) exist for 2007-2014, 2007-2019 and 2007-2025,
government and opposition. Each matches the window of the doc2vec model of the same name, so
the only difference between the pair is the sentiment measure. Note the GPT and doc2vec series
are on different scales, so compare the SHAPE and significance of IRFs, not their magnitudes
(IRFs are per one standard deviation of each measure).

To add a model, add one `make_spec(...)` line in config.R. Nothing else changes.


