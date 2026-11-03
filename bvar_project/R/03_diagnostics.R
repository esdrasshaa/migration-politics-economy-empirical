############################################################
# 03_diagnostics.R
# ONE test battery, applied identically to every series.
# Output: one tidy table with an explicit verdict per series,
# so nobody has to read 15 raw ur.df summaries by eye.
############################################################

# --- ADF with BIC lag choice + residual check -------------------------
adf_one <- function(x, type = "drift", maxlag = NULL) {
  if (is.null(maxlag)) maxlag <- floor(12 * (length(x) / 100)^0.25)  # Schwert rule
  ur  <- urca::ur.df(x, type = type, lags = maxlag, selectlags = "BIC")
  tau <- switch(type, none = "tau1", drift = "tau2", trend = "tau3")
  res <- ur@testreg$residuals
  data.frame(
    adf_stat  = unname(ur@teststat[1, tau]),
    adf_cv5   = unname(ur@cval[tau, "5pct"]),
    adf_rejects_unit_root = unname(ur@teststat[1, tau] < ur@cval[tau, "5pct"]),
    adf_lags_used = nrow(ur@testreg$coefficients) -
                    c(none = 1, drift = 2, trend = 3)[[type]],
    adf_resid_LB_p = Box.test(res, lag = 12, type = "Ljung-Box")$p.value
  )
}

# --- KPSS (null = stationarity) --------------------------------------
kpss_one <- function(x, null = "Level") {
  k <- suppressWarnings(tseries::kpss.test(x, null = null))
  data.frame(kpss_stat = unname(k$statistic),
             kpss_p = k$p.value,                # tseries truncates to [0.01, 0.10]
             kpss_rejects_stationarity = k$p.value < 0.05)
}

# --- combine into a verdict that cannot be misread -------------------
verdict <- function(adf_rej, kpss_rej) {
  ifelse(adf_rej & !kpss_rej,  "stationary",
  ifelse(!adf_rej & kpss_rej,  "unit root (non-stationary)",
  ifelse(adf_rej & kpss_rej,   "conflicting (ADF: stationary / KPSS: not)",
                               "inconclusive (low power)")))
}

stationarity_table <- function(d, spec, type = "drift") {
  vars <- c(CFG$macro_order, "sentiment")
  out <- do.call(rbind, lapply(vars, function(v) {
    x <- d[[v]]
    cbind(model = spec$id, series = v, n = length(x),
          adf_one(x, type), kpss_one(x))
  }))
  out$verdict <- verdict(out$adf_rejects_unit_root, out$kpss_rejects_stationarity)
  out$lb_warning <- out$adf_resid_LB_p < 0.05   # TRUE => ADF lags too few, ADF unreliable
  out
}

# --- diagnostics on the fitted VAR (OLS), run before trusting IRFs ----
var_diagnostics <- function(y, spec, x = NULL) {
  xy  <- build_xy(y, spec$p, x)
  fit <- ols_fit(xy)
  A   <- companion(fit$pi_hat, ncol(y), spec$p, n_exog = if (is.null(x)) 0 else ncol(x))
  eig <- Mod(eigen(A, only.values = TRUE)$values)

  lb <- vapply(seq_len(ncol(y)), function(j)
          Box.test(fit$resid[, j], lag = 12, type = "Ljung-Box",
                   fitdf = 0)$p.value, numeric(1))
  names(lb) <- colnames(y)

  data.frame(model = spec$id,
             max_eigenvalue_OLS = max(eig),
             stable_OLS = max(eig) < 1,
             min_LB_p = min(lb),
             worst_equation = names(lb)[which.min(lb)],
             resid_autocorr_flag = min(lb) < 0.05,
             stringsAsFactors = FALSE)
}
