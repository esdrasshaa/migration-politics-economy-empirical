############################################################
# 05_irf_fevd.R
# Everything is stored as arrays [horizon, response, shock, draw]
# -> no more flattened "col = shock + n*(var-1)" indexing.
# FEVD is computed PER DRAW and summarised afterwards
# (the old code computed it from the median IRF, which is not
#  a posterior summary of the FEVD).
############################################################

# MA coefficients Phi_0..Phi_{hor-1} by recursion (no matrix powers)
ma_coefs <- function(BigA, n, p, hor) {
  Acoef <- BigA[1:n, , drop = FALSE]
  Ai <- lapply(1:p, function(i) Acoef[, ((i - 1) * n + 1):(i * n), drop = FALSE])
  Phi <- array(0, c(n, n, hor)); Phi[, , 1] <- diag(n)
  if (hor > 1) for (h in 2:hor)
    for (i in 1:min(p, h - 1)) Phi[, , h] <- Phi[, , h] + Ai[[i]] %*% Phi[, , h - i]
  Phi
}

# Orthogonalised IRFs (lower Cholesky, 1 s.d. shocks) and FEVD for all draws
irf_fevd_draws <- function(post, hor) {
  n <- post$n; p <- post$p; D <- dim(post$A)[3]
  irf  <- array(0, c(hor, n, n, D))
  fevd <- array(0, c(hor, n, n, D))
  for (d in seq_len(D)) {
    Phi <- ma_coefs(post$A[, , d], n, p, hor)
    S   <- t(chol(post$Sigma[, , d]))
    for (h in 1:hor) irf[h, , , d] <- Phi[, , h] %*% S      # [response, shock]
    sq  <- irf[, , , d]^2
    cum <- apply(sq, c(2, 3), cumsum)                       # [h, resp, shock]
    tot <- apply(cum, c(1, 2), sum)                         # total over shocks
    for (s in 1:n) fevd[, , s, d] <- cum[, , s] / tot
  }
  list(irf = irf, fevd = fevd)
}

# Posterior summary: median + bands for each requested credible level
summarise_draws <- function(arr, ci) {
  q <- function(pr) apply(arr, 1:3, quantile, probs = pr, names = FALSE)
  list(median = q(0.5),
       lo = setNames(lapply(ci, function(c) q((100 - c) / 200)), ci),
       hi = setNames(lapply(ci, function(c) q((100 + c) / 200)), ci))
}

# Tidy table for ONE shock: so the numbers behind every figure are on disk
irf_table <- function(sm, shock, varNames, ci) {
  hor <- dim(sm$median)[1]; n <- dim(sm$median)[2]
  do.call(rbind, lapply(1:n, function(v) {
    out <- data.frame(horizon = 0:(hor - 1), response = varNames[v],
                      median = sm$median[, v, shock])
    for (c in ci) {
      out[[paste0("lo", c)]] <- sm$lo[[as.character(c)]][, v, shock]
      out[[paste0("hi", c)]] <- sm$hi[[as.character(c)]][, v, shock]
    }
    out
  }))
}

# Reading aid: at which horizons does each band exclude zero?
# (prevents "it looks significant" misreadings of the figure)
band_summary <- function(tab, ci) {
  do.call(rbind, lapply(split(tab, tab$response), function(t) {
    row <- data.frame(response = t$response[1])
    for (c in ci) {
      ex <- t[[paste0("lo", c)]] > 0 | t[[paste0("hi", c)]] < 0
      row[[paste0("horizons_excl_0_", c)]] <- sum(ex)
      row[[paste0("first_excl_0_", c)]] <- if (any(ex)) t$horizon[which(ex)[1]] else NA
    }
    row
  }))
}
