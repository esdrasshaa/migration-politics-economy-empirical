plot_FEVD <- function(vardec,
                      hor,
                      n,
                      shock = 1,
                      varNames = NULL,
                      main_title = "Forecast Error Variance Decomposition") {
  
  if(is.null(varNames)){
    varNames <- paste0("Variable ", 1:n)
  } else if(length(varNames) != n){
    stop(sprintf(
      "varNames hat %d Eintraege, aber n = %d. Bitte genau %d Namen angeben.",
      length(varNames), n, n
    ))
  }
  
  # Set up grid layout with outer margin space (oma) at the top for the main title
  par(mfrow = c(ceiling(n/2), 2),
      mar = c(4, 4, 3, 1),
      oma = c(0, 0, 3, 0))  # 3 lines of space reserved at the top
  
  for(i in 1:n){
    
    # Select the column of the shock
    idx <- shock + n * (i - 1)
    selected <- vardec[, idx]
    
    # All shocks affecting this variable
    start <- 1 + n * (i - 1)
    end <- n + n * (i - 1)
    all_shocks <- vardec[, start:end]
    
    # Remaining shocks
    other <- rowSums(all_shocks) - selected
    
    # Plot stacked FEVD
    mat <- cbind(selected, other)
    
    bp <- barplot(
      t(mat),
      beside = FALSE,
      col = c("steelblue", "orange"),
      border = NA,
      space = 0,
      ylim = c(0, 1),
      names.arg = rep("", hor),
      cex.axis = 1.3,   # size of axis numbers
      cex.lab  = 1.4,   # size of x/y axis labels
      cex.main = 1.5,   # size of title
      xlab = "Horizon",
      ylab = "Variance share",
      main = paste(
        varNames[i],
        "\nShock:",
        varNames[shock]
      )
    )
    
    # Eigene Achsenbeschriftung: nur alle 5 Perioden
    ticks <- seq(1, hor, 5)
    axis(1, at = bp[ticks], labels = (ticks - 1))
  }
  
  legend(
    "topright",
    legend = c(
      paste("Shock", shock),
      "Other shocks"
    ),
    fill = c("steelblue", "orange"),
    bty = "n"
  )
  
  # Add the overall main title centered across the entire canvas
  mtext(main_title, outer = TRUE, cex = 1.3, font = 2, line = 1)
}