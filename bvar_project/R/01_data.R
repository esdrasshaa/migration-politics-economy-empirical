############################################################
# 01_data.R  --  load raw files, build ONE aligned panel.
# Alignment is by DATE (inner join), never by row position.
############################################################

de_months <- c(Januar = 1, Februar = 2, "M\u00e4rz" = 3, April = 4, Mai = 5,
               Juni = 6, Juli = 7, August = 8, September = 9,
               Oktober = 10, November = 11, Dezember = 12)

# Locale-independent parser for "Juni 2007" (old code needed a German locale)
parse_de_month <- function(x) {
  x     <- trimws(x)
  parts <- strsplit(x, " ")
  m <- vapply(parts, function(s) de_months[[s[1]]], numeric(1))
  y <- vapply(parts, function(s) as.integer(s[2]), integer(1))
  as.Date(sprintf("%d-%02d-01", y, m))
}

load_sentiment <- function(source, variable) {
  f   <- file.path(CFG$paths$data, CFG$files$sentiment[[source]])
  raw <- read.csv(f, stringsAsFactors = FALSE)
  dcol <- CFG$sentiment_date_col[[source]]
  vcol <- CFG$sentiment_cols[[source]][[variable]]
  stopifnot(dcol %in% names(raw), vcol %in% names(raw))
  data.frame(date = as.Date(raw[[dcol]]), sentiment = raw[[vcol]])
}

# Original study panel: Excel, seasonally adjusted (X13)
load_panel_baseline <- function() {
  x <- read_excel(file.path(CFG$paths$data, CFG$files$baseline_excel),
                  sheet = CFG$files$baseline_sheet)
  need <- c("DATES", "Total net migration", "Consumer confidence",
            "Business expectations", "Industrial Production",
            "Persons in employment", "Registered unemployed",
            "Population (interpolated)")
  stopifnot(all(need %in% names(x)))     # fail loudly if columns moved
  active <- x[["Persons in employment"]] + x[["Registered unemployed"]]
  data.frame(
    date     = as.Date(x$DATES),
    arrivals = x[["Total net migration"]] / x[["Population (interpolated)"]],
    busexp   = log(x[["Business expectations"]]) * 100,
    consconf = log(x[["Consumer confidence"]])   * 100,
    indpro   = log(x[["Industrial Production"]]) * 100,
    unrate   = x[["Registered unemployed"]] / active * 100
  )
}

# New panel: CSV macro data + official BA unemployment rate
load_panel_new <- function() {
  m <- read.csv(file.path(CFG$paths$data, CFG$files$new_macro),
                stringsAsFactors = FALSE)
  m$Date <- as.Date(m$Date)

  ba <- read.csv(file.path(CFG$paths$data, CFG$files$ba_unemp),
                 skip = 7, sep = ";", stringsAsFactors = FALSE,
                 check.names = FALSE, fileEncoding = "UTF-8-BOM")
  names(ba) <- trimws(names(ba))
  ba <- data.frame(
    date   = parse_de_month(ba[["Berichtsmonat"]]),
    unrate = as.numeric(gsub(",", ".", ba[["Arbeitslosenquote"]]))
  )

  out <- data.frame(
    date     = m$Date,
    arrivals = m$Arrivals,
    busexp   = log(m$BusinessExpectation)  * 100,
    consconf = log(m$ConsumerConfidence)   * 100,
    indpro   = log(m$IndustrialProduction) * 100
  )
  # join by date: the old script overwrote a column by row position
  inner_join(out, ba, by = "date")
}

# Build the estimation dataset for one spec
build_dataset <- function(spec) {
  macro <- switch(spec$panel,
                  baseline = load_panel_baseline(),
                  new      = load_panel_new(),
                  stop("unknown panel: ", spec$panel))
  sent  <- load_sentiment(spec$source, spec$sentiment)

  d <- inner_join(macro, sent, by = "date") %>%
    filter(date >= spec$start, date <= spec$end) %>%
    arrange(date)

  d$covid <- as.numeric(spec$covid &
                        d$date >= as.Date(CFG$covid$start) &
                        d$date <= as.Date(CFG$covid$end))

  check_dataset(d, spec)       # see 02_checks.R
  d
}

# Column order = Cholesky ordering
endog_cols <- function(spec) {
  if (spec$order == "sentiment_first") c("sentiment", CFG$macro_order)
  else c(CFG$macro_order, "sentiment")
}

endog_matrix <- function(d, spec) as.matrix(d[, endog_cols(spec)])

shock_index <- function(spec) match(spec$shock, endog_cols(spec))

var_labels <- function(spec) {
  sent_lab <- paste(tools::toTitleCase(gsub("_", " ", spec$sentiment)), "Immigration Sentiment")
  if (spec$order == "sentiment_first") c(sent_lab, CFG$macro_labels)
  else c(CFG$macro_labels, sent_lab)
}
