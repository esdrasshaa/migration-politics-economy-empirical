rm(list = ls())
library(readxl)
graphics.off()
library(MASS)
library(dplyr)
library(expm)

library(corrplot)
library(Hmisc)     # for correlation matrix with p-values
library(ggplot2)
library(reshape2)


############################################################
# Load Functions
############################################################

source("Functions/SUR.R")
source("Functions/VAR.R")
source("Functions/BVAR.R")
source("Functions/vdec.R")
source("Functions/choleskyBVAR.R")
source("Functions/plot_IRF.R")
source("Functions/plot_FEVD.R")

############################################################
# Reproducibility
############################################################

set.seed(0)


# Load Data (Sentiment Doc2vec )


sentiment <- read.csv("./data/immigration_sentiment_monthly_doc2vec.csv")
sentiment <- sentiment %>%
  filter(
    Date >= as.Date("2007-05-01"),
    Date <= as.Date("2019-10-01")
  )

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
    Berichtsmonat. <= as.Date("2019-10-01")
  )


job_less<- as.numeric(gsub("\\.", "", arbeitlos_number$Bestand.Arbeitslose.))
rate_um <-job_less / 
  ((employmentTotal[1:150,]$Persons_employmentH * 1000) + job_less) * 100


data$Date <- as.Date( data$Date)
sentiment$Date <- as.Date( sentiment$Date)
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

data$unemploymentrate <-rate_um


# 2007 - 2014
data <- data %>%
  filter(
    Date >= as.Date("2007-05-01"),
    Date <= as.Date("2014-12-01")
  )


sentiment <- sentiment %>%
  filter(
    Date >= as.Date("2007-05-01"),
    Date <= as.Date("2014-12-01")#
  )

# colnames(data)
# colnames(sentiment)
# 
# 
# #  Merge datasets on Date 
# merged_data <- inner_join(data, sentiment, by = "Date")
# 
# # Quick check
# str(merged_data)
# summary(merged_data)
# 
# 
# # Select only numeric variables of interest 
# vars_of_interest <- merged_data %>%
#   select(BusinessExpectation, ConsumerConfidence, IndustrialProduction,
#          Arrivals, unemploymentrate,
#          sentiment_gesamt, sentiment_government, expansive_government,
#          restrictive_government, sentiment_opposition, expansive_opposition,
#          restrictive_opposition)
# 
# # Compute correlation matrix 
# cor_matrix <- cor(vars_of_interest, use = "pairwise.complete.obs", method = "pearson")
# print(round(cor_matrix, 2))
# 
# #Compute correlation matrix with p-values
# cor_results <- rcorr(as.matrix(vars_of_interest), type = "pearson")
# cor_coeffs <- cor_results$r
# p_values   <- cor_results$P
# 
# print(round(cor_coeffs, 2))
# print(round(p_values, 3))
# 
# 
# # Focus specifically on sentiment vs. economic variables
# econ_vars <- c("BusinessExpectation", "ConsumerConfidence", "IndustrialProduction",
#                "Arrivals", "unemploymentrate")
# sentiment_vars <- c("sentiment_gesamt", "sentiment_government", "expansive_government",
#                     "restrictive_government", "sentiment_opposition",
#                     "expansive_opposition", "restrictive_opposition")
# 
# cross_cor <- cor(merged_data[, econ_vars], merged_data[, sentiment_vars],
#                  use = "pairwise.complete.obs")
# print(round(cross_cor, 2))
# 
# # Visualize just this cross-correlation block
# corrplot(cross_cor,
#          method = "color",
#          addCoef.col = "black",
#          tl.col = "black",
#          tl.srt = 45,
#          number.cex = 0.7,
#          title = "Economic Variables vs. Sentiment Variables",
#          mar = c(0, 0, 2, 0))


# endogenoeous variable

#government
y_gvt <- as.matrix(
  cbind(
    data$Arrivals,
    log(data$BusinessExpectation) * 100,
    log(data$ConsumerConfidence) * 100,
    log(data$IndustrialProduction) * 100,
    data$unemploymentrate,
    sentiment$sentiment_government
    )
)



T <- nrow(y_gvt)
n <- ncol(y_gvt)

p <- 1         # VAR lags
c <- 1          # Constant
trend <- 0      # No deterministic trend
drawfin <- 1000
hor <- 49       # IRF horizon



# Estimate Bayesian VAR

results_gvt <- CholeskyBVAR(
  y       = y_gvt,
  p       = p,
  c       = c,
  trend   = trend,
  drawfin = drawfin,
  hor     = hor,
  conf    = 68
)


# Extract Results
LowD_gvt   <- results_gvt$LowD
MiddleD_gvt <- results_gvt$MiddleD
HighD_gvt   <- results_gvt$HighD
vardec_gvt  <- results_gvt$vardec

# Variable Names
varNames_gvt <- c(
  "Arrivals",
  "Business Expectations",
  "Consumer Confidence",
  "Industrial Production",
  "Unemployment",
  "Government Immigration Sentiment"
)

# Plot Impulse Responses
plot_IRF(
  MiddleD = MiddleD_gvt,
  LowD    = LowD_gvt,
  HighD   = HighD_gvt,
  hor      = hor,
  n        = n,
  shock    = 6,
  varNames = varNames_gvt,
  main_title = "Macroeconomic and Sentiment Dynamics\nNew Sample: 2007–2019"
)


 # Plot Forecast Error Variance Decomposition

plot_FEVD(
  vardec   = vardec,
  hor      = hor,
  n        = n,
  shock    = 1,
  varNames = varNames,
  main_title = "Forecast Error Variance Decomposition (2006–2025)"
)



#Opposition
y_opp <- as.matrix(
  cbind(
    data$Arrivals,
    log(data$BusinessExpectation) * 100,
    log(data$ConsumerConfidence) * 100,
    log(data$IndustrialProduction) * 100,
    data$unemploymentrate,
    sentiment$sentiment_opposition
  )
)



T <- nrow(y_opp)
n <- ncol(y_opp)

p <- 1         # VAR lags
c <- 1          # Constant
trend <- 0      # No deterministic trend
drawfin <- 1000
hor <- 49       # IRF horizon



# Estimate Bayesian VAR

results_opp <- CholeskyBVAR(
  y       = y_opp,
  p       = p,
  c       = c,
  trend   = trend,
  drawfin = drawfin,
  hor     = hor,
  conf    = 68
)


# Extract Results
LowD_opp   <- results_opp$LowD
MiddleD_opp <- results_opp$MiddleD
HighD_opp   <- results_opp$HighD
vardec_opp  <- results_opp$vardec

# Variable Names
varNames_opp <- c(
  "Arrivals",
  "Business Expectations",
  "Consumer Confidence",
  "Industrial Production",
  "Unemployment",
  "Opposition Immigration Sentiment"
)

# Plot Impulse Responses
plot_IRF(
  MiddleD = MiddleD_opp,
  LowD    = LowD_opp,
  HighD   = HighD_opp,
  hor      = hor,
  n        = n,
  shock    = 6,
  varNames = varNames_opp,
  main_title = "Macroeconomic and Sentiment Dynamics\nNew Sample: 2007–2014"
)


# Plot Forecast Error Variance Decomposition

plot_FEVD(
  vardec   = vardec,
  hor      = hor,
  n        = n,
  shock    = 1,
  varNames = varNames,
  main_title = "Forecast Error Variance Decomposition (2006–2019)"
)


#Bundestag
y_bu <- as.matrix(
  cbind(
    data$Arrivals,
    log(data$BusinessExpectation) * 100,
    log(data$ConsumerConfidence) * 100,
    log(data$IndustrialProduction) * 100,
    data$unemploymentrate,
    sentiment$sentiment_gesamt
  )
)



T <- nrow(y_bu)
n <- ncol(y_bu)

p <- 1         # VAR lags
c <- 1          # Constant
trend <- 0      # No deterministic trend
drawfin <- 1000
hor <- 49       # IRF horizon



# Estimate Bayesian VAR

results_bu <- CholeskyBVAR(
  y       = y_bu,
  p       = p,
  c       = c,
  trend   = trend,
  drawfin = drawfin,
  hor     = hor,
  conf    = 68
)


# Extract Results
LowD_bu    <- results_bu$LowD
MiddleD_bu <- results_bu$MiddleD
HighD_bu   <- results_bu$HighD
vardec_bu  <- results_bu$vardec

# Variable Names
varNames_bu <- c(
  "Arrivals",
  "Business Expectations",
  "Consumer Confidence",
  "Industrial Production",
  "Unemployment",
  "Bundestag Immigration Sentiment"
)

# Plot Impulse Responses
plot_IRF(
  MiddleD = MiddleD_bu,
  LowD    = LowD_bu,
  HighD   = HighD_bu,
  hor      = hor,
  n        = n,
  shock    = 6,
  varNames = varNames_bu,
  main_title = "Macroeconomic and Sentiment Dynamics\nNew Sample: 2007–2019"
)


# Plot Forecast Error Variance Decomposition

plot_FEVD(
  vardec   = vardec,
  hor      = hor,
  n        = n,
  shock    = 1,
  varNames = varNames,
  main_title = "Forecast Error Variance Decomposition (2006–2019)"
)

# gpt-4.1-nano 2007-2019


# Load Data


data <- data %>%
  filter(
    Date >= as.Date("2007-05-01"),
    Date <= as.Date("2019-10-01")
  )

sentiment <- read.csv("./data/GptSentimentMonthlyVargptNano.csv")

sentiment$month <- as.Date(sentiment$month)
sentiment <- sentiment %>%
  filter(
    month >= as.Date("2007-05-01"),
    month <= as.Date("2019-10-01")
  )

# endogenoeous variable

y_gpt <- as.matrix(
  cbind(
    data$Arrivals,
    log(data$BusinessExpectation) * 100,
    log(data$ConsumerConfidence) * 100,
    log(data$IndustrialProduction) * 100,
    data$unemploymentrate,
    sentiment$gpt_sentiment_opposition
  )
)


T <- nrow(y_gpt)
n <- ncol(y_gpt)

p <- 2         # VAR lags
c <- 1          # Constant
trend <- 0      # No deterministic trend
drawfin <- 1000
hor <- 49       # IRF horizon



# Estimate Bayesian VAR

results_gpt <- CholeskyBVAR(
  y       = y_gpt,
  p       = p,
  c       = c,
  trend   = trend,
  drawfin = drawfin,
  hor     = hor,
  conf    = 68
)


# Extract Results
LowD_gpt    <- results_gpt$LowD
MiddleD_gpt <- results_gpt$MiddleD
HighD_gpt   <- results_gpt$HighD
vardec_gpt  <- results_gpt$vardec

# Variable Names
varNames_gpt <- c(
  "Arrivals",
  "Business Expectations",
  "Consumer Confidence",
  "Industrial Production",
  "Unemployment",
  "Opposition Immigration Sentiment"
)

# Plot Impulse Responses
plot_IRF(
  MiddleD = MiddleD_gpt,
  LowD    = LowD_gpt,
  HighD   = HighD_gpt,
  hor      = hor,
  n        = n,
  shock    = 6,
  varNames = varNames_gpt,
  main_title = "Macroeconomic and Sentiment Dynamics (Gpt-4-1-nano) \nNew Sample: 2007–2019"
)


# Plot Forecast Error Variance Decomposition

plot_FEVD(
  vardec   = vardec_gpt,
  hor      = hor,
  n        = n,
  shock    = 6,
  varNames = varNames,
  main_title = "Forecast Error Variance Decomposition (2006–2025)"
)


#gpt-4.1-nano 2007-2014


# Load Data
data <- data %>%
  filter(
    Date >= as.Date("2007-05-01"),
    Date <= as.Date("2014-12-01")
  )

sentiment <- read.csv("./data/GptSentimentMonthlyVargptNano.csv")

sentiment$month <- as.Date(sentiment$month)
sentiment <- sentiment %>%
  filter(
    month >= as.Date("2007-05-01"),
    month <= as.Date("2014-12-01")
  )

# endogenoeous variable

y <- as.matrix(
  cbind(
    data$Arrivals,
    log(data$BusinessExpectation) * 100,
    log(data$ConsumerConfidence) * 100,
    log(data$IndustrialProduction) * 100,
    data$unemploymentrate,
    sentiment$gpt_sentiment_opposition
  )
)


T <- nrow(y)
n <- ncol(y)

p <- 2         # VAR lags
c <- 1          # Constant
trend <- 0      # No deterministic trend
drawfin <- 1000
hor <- 49       # IRF horizon



# Estimate Bayesian VAR

results <- CholeskyBVAR(
  y       = y,
  p       = p,
  c       = c,
  trend   = trend,
  drawfin = drawfin,
  hor     = hor,
  conf    = 68
)


# Extract Results
LowD    <- results$LowD
MiddleD <- results$MiddleD
HighD   <- results$HighD
vardec  <- results$vardec

# Variable Names
varNames <- c(
  "Arrivals",
  "Business Expectations",
  "Consumer Confidence",
  "Industrial Production",
  "Unemployment",
  "Opposition Immigration Sentiment"
)

# Plot Impulse Responses
plot_IRF(
  MiddleD = MiddleD,
  LowD    = LowD,
  HighD   = HighD,
  hor      = hor,
  n        = n,
  shock    = 6,
  varNames = varNames,
  main_title = "Macroeconomic and Sentiment Dynamics (Gpt-4-1-nano) \nNew Sample: 2007–2014"
)


# Plot Forecast Error Variance Decomposition

plot_FEVD(
  vardec   = vardec,
  hor      = hor,
  n        = n,
  shock    = 6,
  varNames = varNames,
  main_title = "Forecast Error Variance Decomposition (2007–2014 )"
)



#gpt-5-nano

# Load Data


data <- data %>%
  filter(
    Date >= as.Date("2007-05-01"),
    Date <= as.Date("2014-12-01")
  )

sentiment <- read.csv("./data/GptSentimentMonthlyVarGpt5Nano.csv")

sentiment$month <- as.Date(sentiment$month)
sentiment <- sentiment %>%
  filter(
    month >= as.Date("2007-05-01"),
    month <= as.Date("2014-12-01")
  )

# endogenoeous variable

y_gpt5 <- as.matrix(
  cbind(
    data$Arrivals,
    log(data$BusinessExpectation) * 100,
    log(data$ConsumerConfidence) * 100,
    log(data$IndustrialProduction) * 100,
    data$unemploymentrate,
    sentiment$gpt_sentiment_opposition
  )
)



T <- nrow(y_gpt5)
n <- ncol(y_gpt5)

p <- 3         # VAR lags
c <- 1          # Constant
trend <- 0      # No deterministic trend
drawfin <- 1000
hor <- 49       # IRF horizon



# Estimate Bayesian VAR

results_gpt5 <- CholeskyBVAR(
  y       = y_gpt5,
  p       = p,
  c       = c,
  trend   = trend,
  drawfin = drawfin,
  hor     = hor,
  conf    = 68
)


# Extract Results
LowD_gpt5    <- results_gpt5$LowD
MiddleD_gpt5 <- results_gpt5$MiddleD
HighD_gpt5   <- results_gpt5$HighD
vardec_gpt5  <- results_gpt5$vardec

# Variable Names
varNames_gpt5 <- c(
  "Arrivals",
  "Business Expectations",
  "Consumer Confidence",
  "Industrial Production",
  "Unemployment",
  "Opposition Immigration Sentiment"
)

# Plot Impulse Responses
plot_IRF(
  MiddleD = MiddleD_gpt5,
  LowD    = LowD_gpt5,
  HighD   = HighD_gpt5,
  hor      = hor,
  n        = n,
  shock    = 6,
  varNames = varNames_gpt5,
  main_title = "Macroeconomic and Sentiment Dynamics\nNew Sample: 2007–2014"
)


# Plot Forecast Error Variance Decomposition

plot_FEVD(
  vardec   = vardec_gpt5,
  hor      = hor,
  n        = n,
  shock    = 1,
  varNames = varNames,
  main_title = "Forecast Error Variance Decomposition (2007–2014)"
)


#gpt-5-nano













