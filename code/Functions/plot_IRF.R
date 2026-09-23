plot_IRF <- function(MiddleD,
                     LowD,
                     HighD,
                     hor,
                     n,
                     shock = 1,
                     varNames = NULL,
                     main_title = NULL) { 
  
  if(is.null(varNames)){
    varNames <- paste0("Variable ", 1:n)
  }
  
  # Set up the grid layout (outer margin 'oma' added at the top for the main title)
  par(
    mfrow = c(ceiling(n/2), 2),
    mar = c(4, 4, 3, 1),
    oma = c(0, 0, 5, 0)
  )
  
  for(k in 1:n){
    
    idx <- shock + n * (k - 1)
    
    median <- MiddleD[, idx]
    lower  <- LowD[, idx]
    upper  <- HighD[, idx]
    
    # Individual subplot (already handles each variable's title via 'main')
    plot(1:hor,
         median,
         type = "l",
         cex.axis = 1.3,   # size of axis numbers
         cex.lab  = 1.2,   # size of x/y axis labels
         cex.main = 1.5,   # size of title
         font = 2,
         lwd = 3,
         col = "blue",
         ylim = c(min(lower), max(upper)),
         xlab = "Horizon",
         ylab = "Response",
         main = paste(varNames[k], "\nShock:", varNames[shock]))
    
    # Confidence band
    polygon(
      c(1:hor, rev(1:hor)),
      c(upper, rev(lower)),
      col = rgb(0.5, 0.5, 0.5, 0.3),
      border = NA
    )
    
    # Median line on top
    lines(1:hor, median, col = "blue", lwd = 3)
    
    # Zero line
    abline(h = 0, col = "red", lty = 2)
  }
  
  # Add the overall main title for the entire multi-panel canvas
  mtext(main_title, outer = TRUE, cex = 1.3, font = 2, line = 1)
}