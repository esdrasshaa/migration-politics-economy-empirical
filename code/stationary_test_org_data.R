
library(ggplot2)
library(ggfortify)
library(urca)
library(aTSA)
library(vars)
library(readxl)
library(tsDyn)
library(car)
library(tseries)
library(dplyr)


#  DATA LOADING


# Sentiment Data ----
sentiment <- read.csv("./data/immigration_sentiment_monthly_doc2vec.csv")
sentiment$Date <- as.Date( sentiment$Date )

sentiment <- sentiment %>%
  filter(
    Date >= as.Date("2006-01-01"),
    Date <= as.Date("2019-10-01")
  )

# Macroeconomic Data ----
data <- read_excel(
  "data/immigration_database.xlsx",
  sheet = "Monthly data (SA X13+)"
)

# Extract individual series
NM_SA         <- data[, 3]   # Net migration
consconf      <- data[, 12]  # Consumer confidence
busexp        <- data[, 13]  # Business expectations
indpro        <- data[, 15]  # Industrial production
employed      <- data[, 17]  # Employed persons
pop           <- data[, 19]  # Population
unemployed    <- data[, 21]  # Unemployed total
unemployed_f  <- data[, 22]  # Unemployed female
unemployed_n  <- data[, 23]  # Unemployed male

# Derived Variables ----
participants <- employed + unemployed              # Labor force
unrate       <- (unemployed / participants) * 100  # Unemployment rate (%)


# STATIONARITY ANALYSIS

# ACF & PACF Plots ----
# Purpose: Visual inspection of autocorrelation structure

# Government Sentiment
par(mfrow = c(2, 1))
acf(sentiment$sentiment_government, lag.max = 25, main = "ACF: Government Sentiment")
pacf(sentiment$sentiment_government, lag.max = 25, main = "PACF: Government Sentiment")

# Opposition Sentiment
par(mfrow = c(2, 1))
acf(sentiment$sentiment_opposition, lag.max = 25, main = "ACF: Opposition Sentiment")
pacf(sentiment$sentiment_opposition, lag.max = 25, main = "PACF: Opposition Sentiment")

# Total Sentiment
par(mfrow = c(2, 1))
acf(sentiment$sentiment_gesamt, lag.max = 25, main = "ACF: Total Sentiment")
pacf(sentiment$sentiment_gesamt, lag.max = 25, main = "PACF: Total Sentiment")

# Unit Root Tests (ADF) ----
# Purpose: Formal stationarity testing
# Note: Testing multiple specifications (none, drift, trend) for robustness

# Sentiment Variables
summary(ur.df(sentiment$sentiment_government, 
              type = 'none', lags = 24, selectlags = c("BIC")))
summary(ur.df(sentiment$sentiment_government, 
              type = 'drift', lags = 24, selectlags = c("BIC")))
summary(ur.df(sentiment$sentiment_government, 
              type = 'trend', lags = 24, selectlags = c("BIC")))

summary(ur.df(sentiment$sentiment_opposition, 
              type = 'trend', lags = 24, selectlags = c("BIC")))

# Macroeconomic Variables (Trend specification)
summary(ur.df(NM_SA$`Total net migration`, 
              type = 'trend', lags = 24, selectlags = c("BIC")))
summary(ur.df(consconf$`Consumer confidence`, 
              type = 'trend', lags = 24, selectlags = c("BIC")))
summary(ur.df(busexp$`Business expectations`, 
              type = 'trend', lags = 24, selectlags = c("BIC")))
summary(ur.df(indpro$`Industrial Production`, 
              type = 'trend', lags = 24, selectlags = c("BIC")))
summary(ur.df(unrate$`Registered unemployed`, 
              type = 'trend', lags = 24, selectlags = c("BIC")))

# Macroeconomic Variables (Drift specification)
summary(ur.df(consconf$`Consumer confidence`, 
              type = 'drift', lags = 24, selectlags = c("BIC")))
summary(ur.df(busexp$`Business expectations`, 
              type = 'drift', lags = 24, selectlags = c("BIC")))
summary(ur.df(indpro$`Industrial Production`, 
              type = 'drift', lags = 24, selectlags = c("BIC")))
summary(ur.df(unrate$`Registered unemployed`, 
              type = 'drift', lags = 24, selectlags = c("BIC")))

# Migration (all specifications)
summary(ur.df(NM_SA$`Total net migration`, 
              type = 'none', lags = 24, selectlags = c("BIC")))
summary(ur.df(NM_SA$`Total net migration`, 
              type = 'drift', lags = 24, selectlags = c("BIC")))
summary(ur.df(busexp$`Business expectations`, 
              type = 'drift', lags = 24, selectlags = c("BIC")))
summary(ur.df(unrate$`Registered unemployed`, 
              type = 'drift', lags = 24, selectlags = c("BIC")))

# KPSS Test (Trend Stationarity) ----
# Purpose: Confirmatory test for trend stationarity
kpss.test(sentiment$expansive_government, null = "Level")
kpss.test(sentiment$restrictive_government, null = "Level")
kpss.test(sentiment$expansive_opposition, null = "Level")
kpss.test(sentiment$restrictive_opposition, null = "Level")

kpss.test(sentiment$sentiment_government, null = "Level")
kpss.test(sentiment$sentiment_opposition, null = "Level")
kpss.test(consconf$`Consumer confidence`, null = "Level")
kpss.test(busexp$`Business expectations`, null = "Level")
kpss.test(indpro$`Industrial Production`, null = "Level")
kpss.test(unrate$`Registered unemployed`, null = "Level")
kpss.test(NM_SA$`Total net migration`, null = "Level")



# ============================================================================
# LAG LENGTH SELECTION
# ============================================================================

dset <- cbind(
  NM_SA$`Total net migration`/pop,
  log(busexp$`Business expectations`) * 100,
  log(consconf$`Consumer confidence`) * 100,
  log(indpro$`Industrial Production`) * 100,
  unrate$`Registered unemployed`,
  sentiment$sentiment_government,
  sentiment$sentiment_opposition,
  sentiment$sentiment_gesamt
)


dset <- na.omit(dset)

lag_selection <- VARselect(
  dset,
  lag.max = 10,
  type = "const"
)

# Print selected lag order by different criteria
print("for the data set from original study")
print(lag_selection$selection)

# ============================================================================
# 6. COINTEGRATION ANALYSIS (Johansen Procedure)
# ============================================================================

jo <- ca.jo(
  dset,
  type = "trace",          # Trace test statistic
  K = 3,                   # Lag order (based on VARselect)
  spec = "longrun",        # Long-run specification
  ecdet = "const"          # Constant in cointegration space
)

summary(jo)

# ============================================================================
# 7. VECTOR AUTOREGRESSION (VAR) MODEL
# ============================================================================

# ---- 7.1 Estimate VAR ----
var_model <- VAR(
  dset,
  p = 3,                   # Lag order
  type = "const"           # Intercept
)

summary(var_model)

# ---- 7.2 Impulse Response Functions (IRF) ----
# Purpose: Trace effect of a shock to migration on all variables

variables <- colnames(dset)
impulse <- "sentiment_opposition"

irf_result <- irf(
  var_model,
  impulse = impulse,
  response = variables,
  n.ahead = 40,
  boot = TRUE,
  runs = 1000,
  ci = 0.65
)

# Extract IRF values and confidence bands
irf_values <- irf_result$irf[[impulse]]
lower <- irf_result$Lower[[impulse]]
upper <- irf_result$Upper[[impulse]]

# Plot IRF
par(mfrow = c(2, 3))

for (i in seq_along(variables)) {
  plot(
    irf_values[, i],
    type = "l",
    lwd = 2,
    ylim = range(lower[, i], upper[, i]),
    main = paste(variables[i], "\nresponse to", impulse),
    xlab = "Periods",
    ylab = "Response"
  )
  lines(lower[, i], lty = 2)
  lines(upper[, i], lty = 2)
  abline(h = 0, lty = 3)
}

# ---- 7.3 Forecast Error Variance Decomposition (FEVD) ----
# Purpose: Proportion of forecast variance explained by migration shocks

n.ahead <- 40
impulse <- "sentiment_opposition"

fevd_result <- fevd(
  var_model,
  n.ahead = n.ahead
)

variables <- colnames(dset)
impulse_index <- which(variables == impulse)

# Plot FEVD (contribution of migration to each variable)
par(mfrow = c(2, 3))

for (i in seq_along(variables)) {
  impulse_contribution <- fevd_result[[i]][, impulse_index]
  
  plot(
    1:n.ahead,
    impulse_contribution,
    type = "l",
    lty = 1,
    lwd = 2,
    ylim = c(0, 1),
    main = variables[i],
    xlab = "Forecast horizon",
    ylab = impulse
  )
  
  axis(
    2,
    at = seq(0, 1, by = 0.1),
    labels = paste0(seq(0, 100, by = 10), "%")
  )
}

# ============================================================================
# END OF SCRIPT
# ============================================================================