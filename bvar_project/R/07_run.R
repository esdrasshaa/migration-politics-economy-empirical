############################################################
# 07_run.R -- run one spec end-to-end; run many.
############################################################

save_pdf <- function(file, expr, w = CFG$plot$w, h = CFG$plot$h) {
  pdf(file, width = w, height = h, pointsize = CFG$plot$pointsize)
  on.exit(dev.off()); force(expr)
}

# Build data and resolve the lag length (BIC or fixed). Single entry point so
# diagnostics and estimation always use the same p.
prepare_model <- function(spec) {
  d <- build_dataset(spec)
  y <- endog_matrix(d, spec)
  x <- if (spec$covid) matrix(d$covid, ncol = 1, dimnames = list(NULL, "covid")) else NULL
  lag_tab <- NULL
  if (identical(spec$p, "bic")) {
    sel <- select_lags(y, x, CFG$bvar$p_max)
    spec$p <- sel$p; lag_tab <- sel$table
    if (spec$p == CFG$bvar$p_max)
      warning(spec$id, ": BIC picked the upper bound p = ", spec$p,
              ". Consider raising CFG$bvar$p_max.")
  }
  stopifnot(is.numeric(spec$p), spec$p >= 1)
  list(spec = spec, d = d, y = y, x = x, lag_tab = lag_tab)
}

run_spec <- function(spec, save = TRUE) {
  message("== ", spec$id)
  set.seed(CFG$seed)                      # same seed per model => order-independent
  B <- CFG$bvar

  pm <- prepare_model(spec)
  spec <- pm$spec; d <- pm$d; y <- pm$y; x <- pm$x
  message("   lags p = ", spec$p, if (!is.null(pm$lag_tab)) " (BIC)" else " (fixed)")

  diag_var  <- var_diagnostics(y, spec, x)
  stat_tab  <- stationarity_table(d, spec)

  post <- sample_bvar(y, spec$p, x, draws = B$draws, max_tries = B$max_tries)
  dr   <- irf_fevd_draws(post, B$hor)

  res <- list(
    spec = spec, meta = sample_summary(d, spec),
    varNames = var_labels(spec), shock = shock_index(spec), ci = B$ci,
    irf_sm  = summarise_draws(dr$irf,  B$ci),
    fevd_sm = summarise_draws(dr$fevd, 0),       # median only is plotted
    acceptance_rate = post$acceptance_rate,
    n_obs_used = post$n_obs_used, n_coef = post$n_coef,
    diagnostics = diag_var, stationarity = stat_tab, lag_selection = pm$lag_tab
  )
  res$irf_tab  <- irf_table(res$irf_sm, res$shock, res$varNames, B$ci)
  res$band_tab <- band_summary(res$irf_tab, B$ci)

  if (post$acceptance_rate < 0.5)
    warning(spec$id, ": only ", round(100 * post$acceptance_rate),
            "% of posterior draws were stable -> IRFs conditional on stability, read with care")

  if (save) {
    f <- function(ext, tag) file.path(CFG$paths[[ext]], paste0(spec$id, tag))
    save_pdf(f("fig", "_irf.pdf"),  plot_irf(res))
    save_pdf(f("fig", "_fevd.pdf"), plot_fevd(res))
    write.csv(res$irf_tab,  f("tab", "_irf.csv"),        row.names = FALSE)
    write.csv(res$band_tab, f("tab", "_irf_bands.csv"),  row.names = FALSE)
    write.csv(stat_tab,     f("tab", "_stationarity.csv"), row.names = FALSE)
    if (!is.null(pm$lag_tab)) write.csv(pm$lag_tab, f("tab", "_lag_selection.csv"), row.names = FALSE)
    saveRDS(res, f("tab", "_result.rds"))
  }
  res
}

# One row per model: what was estimated and whether it is trustworthy
run_all <- function(specs = SPECS, ...) {
  dir.create(CFG$paths$fig, recursive = TRUE, showWarnings = FALSE)
  dir.create(CFG$paths$tab, recursive = TRUE, showWarnings = FALSE)
  res <- lapply(specs, run_spec, ...)
  summ <- do.call(rbind, lapply(res, function(r)
    cbind(r$meta, accept_rate = round(r$acceptance_rate, 3),
          r$diagnostics[, c("stable_OLS", "resid_autocorr_flag")],
          n_nonstationary = sum(grepl("unit root", r$stationarity$verdict)),
          n_conflicting_or_inconclusive =
            sum(grepl("conflicting|inconclusive", r$stationarity$verdict)))))
  write.csv(summ, file.path(CFG$paths$tab, "SUMMARY_all_models.csv"), row.names = FALSE)
  invisible(res)
}
