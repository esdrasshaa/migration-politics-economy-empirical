SUR <- function(data, p, c = 1, trend = 0, x = NULL){
  
  ## =========================================================
  ## Dimensions
  ## =========================================================
  
  Traw <- nrow(data)
  n <- ncol(data)
  
  ## =========================================================
  ## Check exogenous variables
  ## =========================================================
  
  if(!is.null(x)){
    
    x <- as.matrix(x)
    
    if(nrow(x) != Traw){
      stop("x must have the same number of rows as data.")
    }
    
  }
  
  ## Number of exogenous variables
  k <- if(is.null(x)) 0 else ncol(x)
  
  ## =========================================================
  ## Dependent variables
  ## =========================================================
  
  Y <- data[
    (p + 1):Traw,
    ,
    drop = FALSE
  ]
  
  ## =========================================================
  ## Exogenous variables
  ##
  ## We align x_t with Y_t.
  ## =========================================================
  
  if(k > 0){
    
    x_used <- x[
      (p + 1):Traw,
      ,
      drop = FALSE
    ]
    
  }
  
  ## =========================================================
  ## Lagged endogenous variables
  ## =========================================================
  
  lagged <- do.call(
    cbind,
    lapply(
      1:p,
      function(i)
        data[
          (p + 1 - i):(Traw - i),
          ,
          drop = FALSE
        ]
    )
  )
  
  ## =========================================================
  ## Construct X
  ##
  ## IMPORTANT ORDER:
  ##
  ## [trend] [constant] [exogenous] [lagged endogenous]
  ##
  ## This makes the coefficient indexing in BVAR explicit.
  ## =========================================================
  
  X_parts <- list()
  
  ## Trend
  if(trend == 1){
    
    X_parts[[length(X_parts) + 1]] <-
      matrix(
        seq_len(Traw - p),
        ncol = 1,
        dimnames = list(
          NULL,
          "trend"
        )
      )
    
  }
  
  ## Constant
  if(c == 1){
    
    X_parts[[length(X_parts) + 1]] <-
      matrix(
        1,
        nrow = Traw - p,
        ncol = 1,
        dimnames = list(
          NULL,
          "constant"
        )
      )
    
  }
  
  ## Exogenous variables
  if(k > 0){
    
    X_parts[[length(X_parts) + 1]] <-
      x_used
    
  }
  
  ## Lagged endogenous variables
  X_parts[[length(X_parts) + 1]] <-
    lagged
  
  ## Combine all regressors
  X <- do.call(
    cbind,
    X_parts
  )
  
  ## =========================================================
  ## Initial observations
  ## =========================================================
  
  Y_initial <- data[
    1:p,
    ,
    drop = FALSE
  ]
  
  ## =========================================================
  ## Return
  ## =========================================================
  
  list(
    Y = Y,
    X = X,
    Y_initial = Y_initial
  )
}