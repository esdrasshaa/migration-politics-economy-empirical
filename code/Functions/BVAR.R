BVAR <- function(
    y,
    p,
    c = 1,
    trend = 0,
    x = NULL
){
  
  library(MASS)
  library(MCMCpack)
  
  source("Functions/VAR.R")
  
  ## =========================================================
  ## Dimensions
  ## =========================================================
  
  Traw <- nrow(y)
  n <- ncol(y)
  T <- Traw - p
  
  ## =========================================================
  ## Exogenous variables
  ## =========================================================
  
  if(is.null(x)){
    
    k <- 0
    
  } else {
    
    x <- as.matrix(x)
    
    if(nrow(x) != Traw){
      stop("x must have the same number of rows as y.")
    }
    
    k <- ncol(x)
    
  }
  
  ## =========================================================
  ## OLS estimate
  ## =========================================================
  
  var_fit <- VAR(
    data = y,
    p = p,
    c = c,
    trend = trend,
    x = x
  )
  
  pi_hat <- var_fit$pi_hat
  Y <- var_fit$Y
  X <- var_fit$X
  err <- var_fit$err
  
  ## =========================================================
  ## Number of coefficients
  ##
  ## trend + constant + exogenous + lagged endogenous
  ## =========================================================
  
  ncoef <- n*p + c + trend + k
  
  if(nrow(pi_hat) != ncoef){
    stop(
      paste(
        "Unexpected number of coefficients.",
        "Expected:", ncoef,
        "Found:", nrow(pi_hat)
      )
    )
  }
  
  ## =========================================================
  ## SSE
  ## =========================================================
  
  S <- t(err) %*% err
  
  ## =========================================================
  ## Draw Sigma ~ IW(S, v)
  ## =========================================================
  
  v <- T - ncol(X)
  
  Sigma <- MCMCpack::riwish(
    v,
    S
  )
  
  ## =========================================================
  ## Posterior covariance of beta
  ## =========================================================
  
  XX <- kronecker(
    Sigma,
    solve(
      t(X) %*% X
    )
  )
  
  ## =========================================================
  ## vec(pi_hat)
  ## =========================================================
  
  vec_pi_hat <- as.vector(
    pi_hat
  )
  
  ## =========================================================
  ## Draw beta
  ## =========================================================
  
  PI_vec <- MASS::mvrnorm(
    n = 1,
    mu = vec_pi_hat,
    Sigma = XX
  )
  
  ## =========================================================
  ## Reshape coefficient matrix
  ## =========================================================
  
  PI <- matrix(
    PI_vec,
    nrow = ncoef,
    ncol = n
  )
  
  ## =========================================================
  ## Companion matrix
  ##
  ## IMPORTANT:
  ##
  ## PI contains:
  ##
  ## [trend]
  ## [constant]
  ## [exogenous variables]
  ## [lagged endogenous variables]
  ##
  ## BigA must contain ONLY the lagged endogenous
  ## coefficients.
  ## =========================================================
  
  first_lag <- trend + c + k + 1
  
  last_lag <- trend + c + k + n*p
  
  Acoef <- t(
    PI[
      first_lag:last_lag,
      ,
      drop = FALSE
    ]
  )
  
  ## =========================================================
  ## Companion matrix
  ## =========================================================
  
  if(n*p > n){
    
    BigA <- rbind(
      
      Acoef,
      
      cbind(
        diag(n*p - n),
        matrix(
          0,
          n*p - n,
          n
        )
      )
      
    )
    
  } else {
    
    BigA <- Acoef
    
  }
  
  ## =========================================================
  ## Fitted values
  ## =========================================================
  
  fittednorm <- X %*% PI
  
  ## =========================================================
  ## Residuals
  ## =========================================================
  
  errornorm <- Y - fittednorm
  
  ## =========================================================
  ## Return
  ## =========================================================
  
  list(
    
    PI = PI,
    
    BigA = BigA,
    
    Sigma = Sigma,
    
    errornorm = errornorm,
    
    fittednorm = fittednorm
    
  )
}