# ============================================================================
# 1. LIBRARIES
# ============================================================================

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
library(lubridate)



# ============================================================================
# 2. DATA LOADING
# ============================================================================

# ---- 2.1 Sentiment Data ----
sentiment <- read.csv("./data/immigration_sentiment_monthly_2006_2025.csv")
data <- read.csv("./data/all_data_2006Q1_2025.csv")

sentiment <- sentiment %>%
  filter(
    Date >= as.Date("2006-01-01"),
    Date <= as.Date("2019-10-01")
  )

data <- data %>%
  filter(
    Date >= as.Date("2006-01-01"),
    Date <= as.Date("2019-10-01")
  )

# ============================================================================
# 3. STATIONARITY ANALYSIS
# ============================================================================

# ---- 3.1 ACF & PACF Plots ----
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

# ---- 3.2 Unit Root Tests (ADF) ----
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
summary(ur.df(data$Arrivals, 
              type = 'drift', lags = 24, selectlags = c("BIC")))
summary(ur.df(data$ConsumerConfidence, 
              type = 'drift', lags = 24, selectlags = c("BIC")))
summary(ur.df(data$BusinessExpectation, 
              type = 'drift', lags = 24, selectlags = c("BIC")))
summary(ur.df(data$IndustrialProduction, 
              type = 'drift', lags = 24, selectlags = c("BIC")))
summary(ur.df(data$unemploymentrate, 
              type = 'drift', lags = 24, selectlags = c("BIC")))


# Migration (all specifications)
summary(ur.df(data$Arrivals, 
              type = 'none', lags = 24, selectlags = c("BIC")))
summary(ur.df(data$Arrivals, 
              type = 'drift', lags = 24, selectlags = c("BIC")))
summary(ur.df(data$Arrivals, 
              type = 'drift', lags = 24, selectlags = c("BIC")))


# ---- 3.3 KPSS Test (Trend Stationarity) ----
# Purpose: Confirmatory test for trend stationarity
kpss.test(sentiment$sentiment_government, null = "Level")
kpss.test(sentiment$sentiment_opposition, null = "Level")
kpss.test(sentiment$sentiment_gesamt, null = "Level")
kpss.test(data$ConsumerConfidence, null = "Level")
kpss.test(data$BusinessExpectation, null = "Level")
kpss.test(data$IndustrialProduction, null = "Level")
kpss.test(data$Arrivals, null = "Level")
kpss.test(data$unemploymentrate, null = "Level")

# ============================================================================
# 4. DATA PREPARATION FOR MULTIVARIATE ANALYSIS
# ============================================================================

# ---- 4.1 Create Dataset ----
# Transformations: Log transformation for scale adjustment, multiplied by 100

dset <- cbind(
  data$Arrivals*10,
  log(data$BusinessExpectation) * 100,
  log(data$ConsumerConfidence) * 100,
  log(data$IndustrialProduction) * 100,
  data$unemploymentrate,
  sentiment$restrictive_opposition
)





# ---- 4.2 Assign Column Names ----
colnames(dset) <- c(
  "migration",
  "business_expectations",
  "consumer_confidence",
  "industrial_production",
  "unemployment",
  "restrictive_opposition"
)

# ---- 4.3 Remove Missing Values ----
dset <- na.omit(dset)

# ---- 4.4 Select I(1) Variables for Cointegration ----
# Excluding sentiment_government (which is trend-stationary, not I(1))
dset_I1 <- dset[, c(
  "migration",
  #"business_expectations",
  "consumer_confidence",
  "industrial_production",
  "unemployment",
  "sentiment_opposition"
)]

# ============================================================================
# 5. LAG LENGTH SELECTION
# ============================================================================

lag_selection <- VARselect(
  dset,
  lag.max = 10,
  type = "const"
)

# Print selected lag order by different criteria
print(lag_selection$selection)

# ============================================================================
# 6. COINTEGRATION ANALYSIS (Johansen Procedure)
# ============================================================================

jo <- ca.jo(
  dset_I1,
  type = "trace",          # Trace test statistic
  K = 2,                   # Lag order (based on VARselect)
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
impulse <- "restrictive_opposition"

irf_result <- irf(
  var_model,
  impulse = impulse,
  response = variables,
  n.ahead = 49,
  boot = TRUE,
  runs = 1000,
  ortho = TRUE,
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
impulse <- "sentiment_government"

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