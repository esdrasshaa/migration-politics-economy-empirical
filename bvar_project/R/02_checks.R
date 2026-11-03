############################################################
# 02_checks.R  --  hard stops before any estimation.
# If an assumption is violated the run STOPS, instead of
# silently producing a plausible-looking but wrong figure.
############################################################

check_dataset <- function(d, spec) {
  id <- spec$id
  if (nrow(d) == 0) stop(id, ": empty sample after merging/filtering")

  # 1. window actually covered by data
  if (min(d$date) > spec$start + 31 || max(d$date) < spec$end - 31)
    warning(id, ": data cover ", min(d$date), " to ", max(d$date),
            " but spec asks ", spec$start, " to ", spec$end)

  # 2. strictly monthly, no gaps, no duplicates
  gaps <- as.numeric(diff(d$date))
  if (any(gaps < 28 | gaps > 31)) stop(id, ": dates are not a gap-free monthly grid")

  # 3. no missing values / infinities in any model column
  cols <- c(CFG$macro_order, "sentiment")
  bad  <- cols[!vapply(cols, function(v) all(is.finite(d[[v]])), logical(1))]
  if (length(bad)) stop(id, ": NA or non-finite values in: ", paste(bad, collapse = ", "))

  # 4. enough observations for the parameter count
  n <- length(cols)
  p_chk <- if (is.numeric(spec$p)) spec$p else CFG$bvar$p_max   # worst case
  k <- n * p_chk + 1 + spec$covid
  if (nrow(d) - p_chk < 3 * k)
    warning(id, ": only ", nrow(d) - p_chk, " usable obs for ", k,
            " coefficients per equation at p = ", p_chk)

  # 5. no constant column (would make X'X singular)
  if (any(vapply(cols, function(v) sd(d[[v]]), numeric(1)) == 0))
    stop(id, ": a variable is constant in this window")

  invisible(TRUE)
}

# Record of what was ACTUALLY estimated (saved next to every figure)
sample_summary <- function(d, spec) {
  data.frame(id = spec$id, panel = spec$panel, source = spec$source,
             sentiment = spec$sentiment, order = spec$order,
             first = format(min(d$date)), last = format(max(d$date)),
             n_obs = nrow(d), p = spec$p, covid_dummy = spec$covid)
}
