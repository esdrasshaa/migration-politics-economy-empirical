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

############################################################
# Load Data
############################################################

sentiment <- read.csv("./data/immigration_sentiment_monthly_doc2vec.csv")
data <- read.csv("./data/all_data_2006Q1_2025.csv")
employmentTotal <- read.csv("./data/employmentTotal.csv" , sep = ";")
arbeitlos_number = read.csv("./data/statistik_lzr_20260818203025.csv", skip = 7 ,
                            sep = ";")
date_strings <- arbeitlos_number$Berichtsmonat.
original_locale <- Sys.getlocale("LC_TIME")
parsed_dates <- as.Date(paste0("1. ", date_strings), format = "%d. %B %Y")

formatted_dates <- format(parsed_dates, "%Y-%m-%d")
arbeitlos_number$Berichtsmonat. <-formatted_dates
arbeitlos_number <- arbeitlos_number[nrow(arbeitlos_number):1, ]

arbeitlos_number <- arbeitlos_number %>%
  filter(
    Berichtsmonat. >= as.Date("2007-05-01"),
    Berichtsmonat. <= as.Date("2025-06-01")
  )


job_less<- as.numeric(gsub("\\.", "", arbeitlos_number$Bestand.Arbeitslose.))
rate_um <-job_less / 
  ((employmentTotal[1:218,]$Persons_employmentH * 1000) + job_less) * 100


data$Date <- as.Date( data$Date)
sentiment$Date <- as.Date( sentiment$Date )




# ============================================================================
# STATIONARITY ANALYSIS
# ============================================================================

# ---- ACF & PACF Plots ----
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

# ---- Unit Root Tests (ADF) ----
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
              type = 'drift', lags = 24, selectlags = c("BIC")))

summary(ur.df(sentiment$expansive_government, 
              type = 'drift', lags = 24, selectlags = c("BIC")))

summary(ur.df(sentiment$sentiment_gesamt, 
              type = 'drift', lags = 24, selectlags = c("BIC")))

# Macroeconomic Variables (Trend specification)
summary(ur.df( data$Arrivals, 
              type = 'drift', lags = 24, selectlags = c("BIC")))

summary(ur.df( data$BusinessExpectation, 
              type = 'drift', lags = 24, selectlags = c("BIC")))

summary(ur.df(data$ConsumerConfidence, 
              type = 'drift', lags = 24, selectlags = c("BIC")))

summary(ur.df(data$IndustrialProduction, 
              type = 'drift', lags = 24, selectlags = c("BIC")))

summary(ur.df(rate_um, 
              type = 'drift', lags = 24, selectlags = c("BIC")))


# ---- KPSS Test (Trend Stationarity) ----
# Purpose: Confirmatory test for trend stationarity

kpss.test(sentiment$expansive_government, null = "Level")
kpss.test(sentiment$restrictive_government, null = "Level")
kpss.test(sentiment$expansive_opposition, null = "Level")
kpss.test(sentiment$restrictive_opposition, null = "Level")

kpss.test(data$ConsumerConfidence, null = "Level")

kpss.test(data$BusinessExpectation, null = "Level")

kpss.test(data$IndustrialProduction, null = "Level")

kpss.test(rate_um, null = "Level")

kpss.test(data$Arrivals, null = "Level")


# LAG LENGTH SELECTION sentiment compute with Doc2vec

# 2007-2025
sentiment <- sentiment %>%
  filter(
    Date >= as.Date("2007-05-01"),
    Date <= as.Date("2025-06-01")
  )

data <- data %>%
  filter(
    Date >= as.Date("2007-05-01"),
    Date <= as.Date("2025-06-01")
  )
data$unemploymentrate <- rate_um

dset <- cbind(
  data$Arrivals,
  log(data$BusinessExpectation) * 100,
  log(data$ConsumerConfidence) * 100,
  log(data$IndustrialProduction) * 100,
  data$unemploymentrate,
  sentiment$sentiment_opposition,
  sentiment$sentiment_gesamt,
  sentiment$sentiment_government
)

# Remove Missing Values ----
dset <- na.omit(dset)

lag_selection <- VARselect(
  dset,
  lag.max = 10,
  type = "const"
)

# Print selected lag order by different criteria
print("for the reconstructed data set window 2007-2025")
print(lag_selection$selection)

# 2007-2019
sentiment <- sentiment %>%
  filter(
    Date >= as.Date("2007-05-01"),
    Date <= as.Date("2019-10-01")
  )

data <- data %>%
  filter(
    Date >= as.Date("2007-05-01"),
    Date <= as.Date("2019-10-01")
  )

dset <- cbind(
  data$Arrivals,
  log(data$BusinessExpectation) * 100,
  log(data$ConsumerConfidence) * 100,
  log(data$IndustrialProduction) * 100,
  data$unemploymentrate,
  sentiment$sentiment_opposition,
  sentiment$sentiment_gesamt,
  sentiment$sentiment_government
)

#  Remove Missing Values ----
dset <- na.omit(dset)

lag_selection <- VARselect(
  dset,
  lag.max = 10,
  type = "const"
)

# Print selected lag order by different criteria
print("for the reconstructed data set window 2007-2019")
print(lag_selection$selection)


# 2007-2014

sentiment <- sentiment %>%
  filter(
    Date >= as.Date("2007-05-01"),
    Date <= as.Date("2014-12-01")
  )

data <- data %>%
  filter(
    Date >= as.Date("2007-05-01"),
    Date <= as.Date("2014-12-01")
  )

dset <- cbind(
  data$Arrivals,
  log(data$BusinessExpectation) * 100,
  log(data$ConsumerConfidence) * 100,
  log(data$IndustrialProduction) * 100,
  data$unemploymentrate,
  sentiment$sentiment_opposition,
  sentiment$sentiment_gesamt,
  sentiment$sentiment_government
)

#  Remove Missing Values ----
dset <- na.omit(dset)

lag_selection <- VARselect(
  dset,
  lag.max = 5,
  type = "const"
)

# Print selected lag order by different criteria
print("for the reconstructed data set window 2007-2014")
print(lag_selection$selection)








# LAG LENGTH SELECTION sentiment compute with gpt-4-1-nano

sentiment <- read.csv("./data/GptSentimentMonthlyVargptNano.csv")

sentiment$month <- as.Date(sentiment$month)

# 2007-2025
sentiment <- sentiment %>%
  filter(
    month >= as.Date("2007-05-01"),
    month <= as.Date("2025-06-01")
  )

data <- data %>%
  filter(
    Date >= as.Date("2007-05-01"),
    Date <= as.Date("2025-06-01")
  )

data$unemploymentrate <- rate_um

dset <- cbind(
  data$Arrivals,
  log(data$BusinessExpectation) * 100,
  log(data$ConsumerConfidence) * 100,
  log(data$IndustrialProduction) * 100,
  data$unemploymentrate,
  sentiment$gpt_sentiment_government ,
  sentiment$gpt_sentiment_opposition,
  sentiment$gtp_overall_sentiment
)

# Remove Missing Values ----
dset <- na.omit(dset)

lag_selection <- VARselect(
  dset,
  lag.max = 10,
  type = "const"
)

# Print selected lag order by different criteria
print("for the reconstructed data set window 2007-2025")
print(lag_selection$selection)

# 2007-2019
sentiment <- sentiment %>%
  filter(
    month >= as.Date("2007-05-01"),
    month <= as.Date("2019-10-01")
  )

data <- data %>%
  filter(
    Date >= as.Date("2007-05-01"),
    Date <= as.Date("2019-10-01")
  )

dset <- cbind(
  data$Arrivals,
  log(data$BusinessExpectation) * 100,
  log(data$ConsumerConfidence) * 100,
  log(data$IndustrialProduction) * 100,
  data$unemploymentrate,
  sentiment$gpt_sentiment_government ,
  sentiment$gpt_sentiment_opposition,
  sentiment$gtp_overall_sentiment
)

#  Remove Missing Values ----
dset <- na.omit(dset)

lag_selection <- VARselect(
  dset,
  lag.max = 10,
  type = "const"
)

# Print selected lag order by different criteria
print("for the reconstructed data set window 2007-2019")
print(lag_selection$selection)


# 2007-2014

sentiment <- sentiment %>%
  filter(
    month >= as.Date("2007-05-01"),
    month <= as.Date("2014-12-01")
  )

data <- data %>%
  filter(
    Date >= as.Date("2007-05-01"),
    Date <= as.Date("2014-12-01")
  )

dset <- cbind(
  data$Arrivals,
  log(data$BusinessExpectation) * 100,
  log(data$ConsumerConfidence) * 100,
  log(data$IndustrialProduction) * 100,
  data$unemploymentrate,
  sentiment$gpt_sentiment_government ,
  sentiment$gpt_sentiment_opposition,
  sentiment$gtp_overall_sentiment
)

#  Remove Missing Values ----
dset <- na.omit(dset)

lag_selection <- VARselect(
  dset,
  lag.max = 5,
  type = "const"
)

# Print selected lag order by different criteria
print("for the reconstructed data set window 2007-2014")
print(lag_selection$selection)










# ============================================================================
# COINTEGRATION ANALYSIS (Johansen Procedure)
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
# VECTOR AUTOREGRESSION (VAR) MODEL
# ============================================================================

# ---- 7.1 Estimate VAR ----
var_model <- VAR(
  dset,
  p = 3,                   # Lag order
  type = "const"           # Intercept
)

summary(var_model)

# Impulse Response Functions (IRF) 
# Purpose: Trace effect of a shock to migration on all variables

variables <- colnames(dset)
impulse <- "migration"

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

# Forecast Error Variance Decomposition (FEVD) 
# Purpose: Proportion of forecast variance explained by migration shocks

n.ahead <- 40
impulse <- "migration"

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

