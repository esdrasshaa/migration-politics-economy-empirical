############################################################
# 04_bvar.R -- Bayesian VAR with diffuse (Jeffreys) prior.
# Posterior: Sigma | Y ~ IW(S, T-k);  vec(PI) | Sigma ~ N(vec(PI_ols), Sigma (x) (X'X)^-1)
# Regressor order in X is ALWAYS: [constant, exogenous..., lag1, ..., lagp]
############################################################

build_xy <- function(y, p, x = NULL) {
  Traw <- nrow(y); n <- ncol(y)
  Y <- y[(p + 1):Traw, , drop = FALSE]
  lags <- do.call(cbind, lapply(1:p, function(i) y[(p + 1 - i):(Traw - i), , drop = FALSE]))
  parts <- list(matrix(1, Traw - p, 1, dimnames = list(NULL, "constant")))
  if (!is.null(x)) parts[[2]] <- as.matrix(x)[(p + 1):Traw, , drop = FALSE]
  X <- do.call(cbind, c(parts, list(lags)))
  list(Y = Y, X = X, n = n, p = p, n_exog = if (is.null(x)) 0 else ncol(as.matrix(x)))
}

ols_fit <- function(xy) {
  XtXinv <- solve(crossprod(xy$X))
  pi_hat <- XtXinv %*% crossprod(xy$X, xy$Y)
  list(pi_hat = pi_hat, resid = xy$Y - xy$X %*% pi_hat, XtXinv = XtXinv)
}

# companion matrix from a coefficient matrix laid out as above
companion <- function(PI, n, p, n_exog = 0) {
  first <- 1 + 1 + n_exog                       # after constant + exogenous
  Acoef <- t(PI[first:(first + n * p - 1), , drop = FALSE])   # n x (n*p)
  if (p == 1) return(Acoef)
  rbind(Acoef, cbind(diag(n * (p - 1)), matrix(0, n * (p - 1), n)))
}

# One posterior draw
draw_once <- function(xy, fit) {
  Tn <- nrow(xy$Y); k <- ncol(xy$X)
  S     <- crossprod(fit$resid)
  Sigma <- MCMCpack::riwish(Tn - k, S)
  vcv   <- kronecker(Sigma, fit$XtXinv)
  PI    <- matrix(MASS::mvrnorm(1, as.vector(fit$pi_hat), vcv), nrow = k, ncol = xy$n)
  list(PI = PI, Sigma = Sigma)
}

# Draw `draws` STABLE posterior draws (rejection sampling), keep acceptance rate.
sample_bvar <- function(y, p, x = NULL, draws = 1000, max_tries = 500) {
  xy  <- build_xy(y, p, x)
  fit <- ols_fit(xy)
  n   <- xy$n
  A     <- array(0, c(n * p, n * p, draws))
  Sigma <- array(0, c(n, n, draws))
  tries <- integer(draws)

  for (i in seq_len(draws)) {
    for (t in seq_len(max_tries)) {
      dr <- draw_once(xy, fit)
      BigA <- companion(dr$PI, n, p, xy$n_exog)
      if (all(Mod(eigen(BigA, only.values = TRUE)$values) < 1)) break
      if (t == max_tries)
        stop("No stable draw after ", max_tries, " tries (draw ", i,
             "). The posterior is mostly explosive: check data/lags.")
    }
    tries[i] <- t
    A[, , i] <- BigA
    Sigma[, , i] <- dr$Sigma
  }
  list(A = A, Sigma = Sigma, n = n, p = p,
       acceptance_rate = draws / sum(tries),     # share of raw draws that were stable
       n_obs_used = nrow(xy$Y), n_coef = ncol(xy$X))
}


############################################################
# Lag selection by BIC
# All candidate p are compared on the SAME observations (the last
# T - p_max rows). Otherwise a larger p is fitted on fewer data points
# and the criteria are not comparable.
# BIC(p) = log det(Sigma_hat) + m * log(T) / T,   m = n * (n*p + 1 + n_exog)
############################################################
select_lags <- function(y, x = NULL, p_max = 4) {
  Traw <- nrow(y); n <- ncol(y)
  rows <- do.call(rbind, lapply(1:p_max, function(p) {
    xy  <- build_xy(y, p, x)
    keep <- (nrow(xy$Y) - (Traw - p_max) + 1):nrow(xy$Y)     # common sample
    xy$Y <- xy$Y[keep, , drop = FALSE]; xy$X <- xy$X[keep, , drop = FALSE]
    fit <- ols_fit(xy)
    Tn  <- nrow(xy$Y)
    ld  <- as.numeric(determinant(crossprod(fit$resid) / Tn, logarithm = TRUE)$modulus)
    m   <- n * ncol(xy$X)
    data.frame(p = p, n_obs = Tn, n_coef_per_eq = ncol(xy$X),
               AIC = ld + 2 * m / Tn,
               HQ  = ld + 2 * m * log(log(Tn)) / Tn,
               BIC = ld + m * log(Tn) / Tn)
  }))
  rows$chosen_BIC <- rows$p == rows$p[which.min(rows$BIC)]
  list(table = rows, p = rows$p[which.min(rows$BIC)])
}
