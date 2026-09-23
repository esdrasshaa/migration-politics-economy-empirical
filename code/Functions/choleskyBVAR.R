CholeskyBVAR <- function(
    y,
    p,
    c = 1,
    trend = 0,
    x = NULL,
    drawfin = 10000,
    hor = 49,
    conf = 68
){
  
  library(expm)
  
  source("./Functions/BVAR.R")
  
  
  ## =========================================================
  ## Dimensions
  ## =========================================================
  
  T <- nrow(y)
  n <- ncol(y)
  
  ## =========================================================
  ## Exogenous variables
  ## =========================================================
  
  if(is.null(x)){
    
    k <- 0
    
  } else {
    
    x <- as.matrix(x)
    
    if(nrow(x) != T){
      stop(
        "x must have the same number of rows as y."
      )
    }
    
    k <- ncol(x)
    
  }
  
  ## Total number of coefficients per equation
  ncoef <- n*p + c + trend + k
  
  ###########################################
  ## Allocate memory
  ###########################################
  
  PI <- array(
    0,
    dim = c(
      ncoef,
      n,
      drawfin
    )
  )
  
  BigA <- array(
    0,
    dim = c(
      n*p,
      n*p,
      drawfin
    )
  )
  
  Sigma <- array(
    0,
    dim = c(
      n,
      n,
      drawfin
    )
  )
  
  errornorm <- array(
    0,
    dim = c(
      T-p,
      n,
      drawfin
    )
  )
  
  fittednorm <- array(
    0,
    dim = c(
      T-p,
      n,
      drawfin
    )
  )
  
  ###########################################
  ## Gibbs sampler
  ###########################################
  
  pb <- txtProgressBar(
    min = 0,
    max = drawfin,
    style = 3
  )
  
  for(i in 1:drawfin){
    
    stable <- FALSE
    
    while(!stable){
      
      ## ------------------------------------
      ## Draw BVAR
      ## ------------------------------------
      
      fit <- BVAR(
        y = y,
        p = p,
        c = c,
        trend = trend,
        x = x
      )
      
      ## ------------------------------------
      ## Store results
      ## ------------------------------------
      
      PI[,,i] <- fit$PI
      
      BigA[,,i] <- fit$BigA
      
      Sigma[,,i] <- fit$Sigma
      
      errornorm[,,i] <- fit$errornorm
      
      fittednorm[,,i] <- fit$fittednorm
      
      ## ------------------------------------
      ## Stability check
      ## ------------------------------------
      
      eig <- eigen(
        BigA[,,i]
      )$values
      
      if(all(Mod(eig) < 1)){
        
        stable <- TRUE
        
      }
      
    }
    
    setTxtProgressBar(
      pb,
      i
    )
    
  }
  
  close(pb)
  
  ###########################################
  ## Allocate IRF objects
  ###########################################
  
  C <- array(
    0,
    dim = c(
      n,
      n,
      hor,
      drawfin
    )
  )
  
  D <- array(
    0,
    dim = c(
      n,
      n,
      hor,
      drawfin
    )
  )
  
  eta <- array(
    0,
    dim = c(
      T-p,
      n,
      drawfin
    )
  )
  
  ###########################################
  ## Cholesky IRFs
  ###########################################
  
  for(k_draw in 1:drawfin){
    
    ## --------------------------------------
    ## Moving-average representation
    ## --------------------------------------
    
    for(j in 1:hor){
      
      BigC <- BigA[,,k_draw] %^% (j-1)
      
      C[,,j,k_draw] <-
        BigC[
          1:n,
          1:n
        ]
      
    }
    
    ## --------------------------------------
    ## Cholesky decomposition
    ## --------------------------------------
    
    S <- t(
      chol(
        Sigma[,,k_draw]
      )
    )
    
    ## --------------------------------------
    ## Structural IRFs
    ## --------------------------------------
    
    for(j in 1:hor){
      
      D[,,j,k_draw] <-
        C[,,j,k_draw] %*% S
      
    }
    
    ## --------------------------------------
    ## Structural residuals
    ## --------------------------------------
    
    eta[,,k_draw] <-
      t(
        solve(
          S,
          t(errornorm[,,k_draw])
        )
      )
    
  }
  
  ###########################################
  ## Store IRFs
  ###########################################
  
  candidateirf_wold <- array(
    0,
    dim = c(
      hor,
      n*n,
      drawfin
    )
  )
  
  for(k_draw in 1:drawfin){
    
    tmp <- aperm(
      D[,,,k_draw],
      c(3, 2, 1)
    )
    
    candidateirf_wold[,,k_draw] <-
      matrix(
        tmp,
        nrow = hor,
        ncol = n*n
      )
    
  }
  
  ###########################################
  ## Credible intervals
  ###########################################
  
  LowD <- matrix(
    0,
    hor,
    n*n
  )
  
  MiddleD <- matrix(
    0,
    hor,
    n*n
  )
  
  HighD <- matrix(
    0,
    hor,
    n*n
  )
  
  for(k_var in 1:n){
    
    for(j_var in 1:n){
      
      col <- j_var + n*k_var - n
      
      x_irf <- candidateirf_wold[
        ,
        col,
        ,
        drop = FALSE
      ]
      
      x_irf <- matrix(
        x_irf,
        nrow = hor,
        ncol = drawfin
      )
      
      LowD[,col] <-
        apply(
          x_irf,
          1,
          quantile,
          probs = (100-conf)/200
        )
      
      MiddleD[,col] <-
        apply(
          x_irf,
          1,
          quantile,
          probs = .50
        )
      
      HighD[,col] <-
        apply(
          x_irf,
          1,
          quantile,
          probs = (100+conf)/200
        )
      
    }
    
  }
  
  ###########################################
  ## FEVD
  ###########################################
  
  vardec <- vdec(
    MiddleD,
    hor,
    n
  )
  
  ###########################################
  ## Return
  ###########################################
  
  list(
    
    PI = PI,
    
    BigA = BigA,
    
    Sigma = Sigma,
    
    errornorm = errornorm,
    
    fittednorm = fittednorm,
    
    C = C,
    
    D = D,
    
    eta = eta,
    
    candidateirf_wold = candidateirf_wold,
    
    LowD = LowD,
    
    MiddleD = MiddleD,
    
    HighD = HighD,
    
    vardec = vardec
    
  )
}