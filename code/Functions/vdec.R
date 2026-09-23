vdec <- function(D, hor, n){
  
  # Square of IRFs
  MiddleDsquare <- D^2
  
  
  ############################################
  # Denominator:
  # total forecast error variance per variable
  ############################################
  
  denom <- matrix(0, nrow = hor, ncol = n)
  
  
  for(k in 1:n){
    
    # start column for variable k
    start <- 1 + (k-1)*n
    
    # n shocks belonging to variable k
    cols <- start:(start+n-1)
    
    
    # Sum over all shocks
    # and cumulative over horizon
    denom[,k] <-
      cumsum(rowSums(MiddleDsquare[,cols]))
    
  }
  
  
  
  ############################################
  # Repeat denominator for every shock
  ############################################
  
  denomtot <- matrix(0, nrow = hor, ncol = n*n)
  
  
  for(k in 1:n){
    
    start <- 1 + (k-1)*n
    cols <- start:(start+n-1)
    
    
    denomtot[,cols] <-
      denom[,k]
    
  }
  
  
  
  ############################################
  # FEVD
  ############################################
  
  
  vardec <- matrix(0, nrow = hor, ncol = n*n)
  
  
  for(j in 1:(n*n)){
    
    
    vardec[,j] <-
      cumsum(MiddleDsquare[,j]) /
      denomtot[,j]
    
  }
  
  
  return(vardec)
  
}
