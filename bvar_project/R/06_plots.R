############################################################
# 06_plots.R -- plots read EVERYTHING from the result object.
# The figures carry NO title; sample details (dates, n, source,
# ordering, COVID dummy) are available from plot_title() so you
# can paste them into the report caption.
############################################################
message(">>> 06_plots.R (no title, legend strip) loaded")


plot_title <- function(res, what) {
  m <- res$meta
  paste0(what, " to a one-s.d. ", res$varNames[res$shock], " shock. ",
         m$first, " to ", m$last, " (n = ", m$n_obs, "), ",
         m$source, ", ", m$order, if (m$covid_dummy) ", COVID dummy" else "")
}

# Shared layout: 3 x 2 panels + one full-width strip for the legend
setup_layout <- function() {
  layout(matrix(c(1:6, 7, 7), nrow = 4, byrow = TRUE),
         heights = c(1, 1, 1, CFG$plot$legend_h))
  par(mar = c(4.2, 4.5, 2.5, 0.8), oma = c(0, 0, 0, 0),
      cex.axis = CFG$plot$cex_axis, cex.lab = 1.0)
}

plot_irf <- function(res, band_cols = NULL) {
  sm <- res$irf_sm; ci <- res$ci; n <- length(res$varNames); shock <- res$shock
  o  <- order(ci, decreasing = TRUE)               # widest band drawn first
  ci <- ci[o]
  if (is.null(band_cols)) band_cols <- gray(seq(0.88, 0.60, length.out = length(ci)))
  hor <- dim(sm$median)[1]; x <- 0:(hor - 1)
  
  op <- par(no.readonly = TRUE)
  on.exit({ layout(1); par(op) })
  setup_layout()
  
  for (v in 1:n) {
    lo <- lapply(as.character(ci), function(c) sm$lo[[c]][, v, shock])
    hi <- lapply(as.character(ci), function(c) sm$hi[[c]][, v, shock])
    plot(x, sm$median[, v, shock], type = "n", ylim = range(unlist(lo), unlist(hi)),
         xlab = "Horizon (months)", ylab = "Response", font = 2,
         cex.main = 1.0, main = res$varNames[v])
    for (b in seq_along(ci))
      polygon(c(x, rev(x)), c(hi[[b]], rev(lo[[b]])), col = band_cols[b], border = NA)
    lines(x, sm$median[, v, shock], col = "blue", lwd = 3)
    abline(h = 0, col = "red", lty = 2)
  }
  
  # legend strip (7th cell of the layout)
  par(mar = c(0, 0, 0, 0)); plot.new()
  legend("center", horiz = TRUE, bty = "n", cex = CFG$plot$cex_legend, pt.cex = 2.5,
         legend = c("Posterior median", paste0(ci, "% credible band")),
         col = c("blue", band_cols), lwd = c(3, rep(NA, length(ci))),
         pch = c(NA, rep(15, length(ci))), x.intersp = 0.8, text.font = 2)
}

plot_fevd <- function(res) {
  sm <- res$fevd_sm; n <- length(res$varNames); shock <- res$shock
  hor <- dim(sm$median)[1]
  
  op <- par(no.readonly = TRUE)
  on.exit({ layout(1); par(op) })
  setup_layout()
  
  for (v in 1:n) {
    share <- pmin(pmax(sm$median[, v, shock], 0), 1)   # posterior median share
    bp <- barplot(t(cbind(share, 1 - share)), space = 0, border = NA, ylim = c(0, 1),
                  col = c("steelblue", "orange"), names.arg = rep("", hor),
                  cex.main = 1.0, cex.axis = CFG$plot$cex_axis,
                  xlab = "Horizon (months)", ylab = "Variance share",
                  main = res$varNames[v])
    ticks <- seq(1, hor, 6)
    axis(1, at = bp[ticks], labels = ticks - 1, cex.axis = CFG$plot$cex_axis)
  }
  
  # legend strip
  par(mar = c(0, 0, 0, 0)); plot.new()
  legend("center", horiz = TRUE, bty = "n", cex = CFG$plot$cex_legend, pt.cex = 2.5,
         legend = c(paste(res$varNames[shock], "shock"), "All other shocks"),
         fill = c("steelblue", "orange"), border = NA, text.font = 2)
}

