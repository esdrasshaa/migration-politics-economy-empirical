############################################################
# Does Immigration Grow the Pie?
# Bayesian VAR (Diffuse Prior)
# Main Script
############################################################

rm(list = ls())
library(readxl)
graphics.off()


library(corrplot)
library(Hmisc)     # for correlation matrix with p-values
library(ggplot2)
library(reshape2)

############################################################
# Packages
############################################################

library(MASS)
library(expm)
library(dplyr)
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

############################################################
# Load Data
############################################################


sentiment <- read.csv("./data/immigration_sentiment_monthly_doc2vec.csv")
sentiment$Date <- as.Date(sentiment$Date)
sentiment <- sentiment %>%
  filter(
    Date >= as.Date("2006-01-01"),
    Date <= as.Date("2019-10-01")
  )



data <- read_excel(
  "data/immigration_database.xlsx",
  sheet = "Monthly data (SA X13+)"
)

data$DATES <- as.Date(data$DATES)

NM_SA         <- data[, 3]
consconf      <- data[,12]
busexp        <- data[,13]
indpro        <- data[,15]
employed      <- data[,17]
pop           <- data[,19]
unemployed    <- data[,21]
unemployed_f  <- data[,22]
unemployed_n  <- data[,23]
participants <- employed+unemployed
unrate <- (unemployed / participants) * 100

# Create calculated variables first
participants <- data[, 17] + data[, 21]
unrate <- (data[, 21] / participants) * 100



# # Combine columns into a new DataFrame
# new_df <- data.frame(
#   Date         = data[, 1],
#   Arrivals       = data[, 3],
#   consconf     = data[, 12],
#   busexp       = data[, 13],
#   indpro       = data[, 15],
#   unrate       = unrate
# )
# 
# new_df <- data.frame(
#   Date         = data[, 1],
#   Arrivals       = data[, 3],
#   consconf     = log(data[, 12])*100,
#   busexp       = log(data[, 13])*100,
#   indpro       = log(data[, 15])*100,
#   unrate       = unrate
# )
# 
# new_df <- new_df %>%
#   rename(Date = DATES)
# 
# # Merge datasets on Date 
# merged_data <- inner_join(new_df, sentiment, by = "Date")
# 
# # Quick check
# str(merged_data)
# summary(merged_data)
# 
# 
# 
# colnames(merged_data)
# 
# 
# 
# vars_of_interest <- merged_data %>%
#   dplyr::select(
#     Total.net.migration,
#     Consumer.confidence,
#     Business.expectations,
#     Industrial.Production,
#     Registered.unemployed,
#     sentiment_gesamt,
#     sentiment_government,
#     expansive_government,
#     restrictive_government,
#     sentiment_opposition,
#     expansive_opposition,
#     restrictive_opposition
#   )
# 
# Compute correlation matrix 
# cor_matrix <- cor(vars_of_interest, use = "pairwise.complete.obs", method = "pearson")
# print(round(cor_matrix, 2))
# 
# Compute correlation matrix with p-values 
# cor_results <- rcorr(as.matrix(vars_of_interest), type = "pearson")
# cor_coeffs <- cor_results$r
# p_values   <- cor_results$P
# 
# print(round(cor_coeffs, 2))
# print(round(p_values, 3))
# 
# Focus specifically on sentiment vs. economic variables 
# econ_vars <- c("Business.expectations", "Consumer.confidence", "Industrial.Production",
#                "Total.net.migration", "Registered.unemployed")
# 
# sentiment_vars <- c("sentiment_gesamt", "sentiment_government", "expansive_government",
#                     "restrictive_government", "sentiment_opposition",
#                     "expansive_opposition", "restrictive_opposition")
# 
# cross_cor <- cor(merged_data[, econ_vars], merged_data[, sentiment_vars],
#                  use = "pairwise.complete.obs")
# # Labels corresponding to the mathematical notation
# colnames(cross_cor) <- c(
#   "Overall Sentiment",
#   "Government Sentiment",
#   "Similarity Welcoming (Government)",
#   "Similarity Restrictive (Government)",
#   "Opposition Sentiment",
#   "Similarity Welcoming (Opposition)",
#   "Similarity Restrictive (Opposition)"
# )
# 
# print(round(cross_cor, 2))
# 
# corrplot(
#   cross_cor,
#   method = "color",
#   addCoef.col = "black",
#   tl.col = "black",
#   tl.srt = 65,
#   number.cex = 0.7,
#   title = "Economic Variables vs. Political Sentiment Indicators",
#   mar = c(0, 0,2, 0)
# )






#Government Pro-Immigration Sentiment
# endogenoeus variable 

y <- as.matrix(
  cbind(
    NM_SA / pop,
    log(busexp) * 100,
    log(consconf) * 100,
    log(indpro) * 100,
    unrate,
    sentiment$sentiment_government
  )
)

# BVAR Specification

T <- nrow(y)
n <- ncol(y)

p <- 1        # VAR lags
c <- 1          # Constant
trend <- 0     # No deterministic trend
drawfin <- 1000
hor <- 49       # IRF horizon


# Estimate Bayesian VAR


results_Gvt <- CholeskyBVAR(
  y       = y,
  p       = p,
  c       = c,
  trend   = trend,
  drawfin = drawfin,
  hor     = hor,
  conf    = 68
)


# Extract Results


LowD_Gvt    <- results_Gvt$LowD
MiddleD_Gvt <- results_Gvt$MiddleD
HighD_Gvt   <- results_Gvt$HighD

vardec_Gvt  <- results_Gvt$vardec


# Variable Names


varNames_Gvt <- c(
  "Arrivals",
  "Business Expectations",
  "Consumer Confidence",
  "Industrial Production",
  "Unemployment",
  "Government Immigration Sentiment"
)

# Plot Impulse Responses

plot_IRF(
  MiddleD = MiddleD_Gvt,
  LowD    = LowD_Gvt,
  HighD   = HighD_Gvt,
  hor      = hor,
  n        = n,
  shock    = 6,
  varNames = varNames_Gvt,
  main_title = "Macroeconomic and Sentiment Dynamics (2006–2019)"
)

# Plot Forecast Error Variance Decomposition

plot_FEVD(
  vardec   = vardec_Gvt,
  hor      = hor,
  n        = n,
  shock    = 6,
  varNames = varNames,
  main_title = "Forecast Error Variance Decomposition (2006–2019)"
)

# Opposition Immigration Sentiment
# endogenoeus variable 

y_opp <- as.matrix(
  cbind(
    NM_SA / pop,
    log(busexp) * 100,
    log(consconf) * 100,
    log(indpro) * 100,
    unrate,
    sentiment$sentiment_opposition
  )
)

# BVAR Specification

T <- nrow(y_opp)
n <- ncol(y_opp)

p <- 1        # VAR lags
c <- 1          # Constant
trend <- 0     # No deterministic trend
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


LowD_opp    <- results_opp$LowD
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
  main_title = "Macroeconomic and Sentiment Dynamics (2006–2019)"
)

# Plot Forecast Error Variance Decomposition

plot_FEVD(
  vardec   = vardec,
  hor      = hor,
  n        = n,
  shock    = 6,
  varNames = varNames,
  main_title = "Forecast Error Variance Decomposition (2006–2019)"
)


# Bundestag Immigration Sentiment
# endogenoeus variable 

y_B <- as.matrix(
  cbind(
    NM_SA / pop,
    log(busexp) * 100,
    log(consconf) * 100,
    log(indpro) * 100,
    unrate,
    sentiment$sentiment_gesamt
  )
)

# BVAR Specification

T <- nrow(y_B)
n <- ncol(y_B)

p <- 1        # VAR lags
c <- 1          # Constant
trend <- 0     # No deterministic trend
drawfin <- 1000
hor <- 49       # IRF horizon


# Estimate Bayesian VAR


results_B <- CholeskyBVAR(
  y       = y_B,
  p       = p,
  c       = c,
  trend   = trend,
  drawfin = drawfin,
  hor     = hor,
  conf    = 68
)


# Extract Results


LowD_B   <- results_B$LowD
MiddleD_B <- results_B$MiddleD
HighD_B   <- results_B$HighD

vardec_B  <- results_B$vardec


# Variable Names


varNames_B <- c(
  "Arrivals",
  "Business Expectations",
  "Consumer Confidence",
  "Industrial Production",
  "Unemployment",
  "Bundestag Immigration Sentiment"
)

# Plot Impulse Responses

plot_IRF(
  MiddleD = MiddleD_B,
  LowD    = LowD_B,
  HighD   = HighD_B,
  hor      = hor,
  n        = n,
  shock    = 6,
  varNames = varNames_B,
  main_title = "Macroeconomic and Sentiment Dynamics (2006–2019)"
)

# Plot Forecast Error Variance Decomposition

plot_FEVD(
  vardec   = vardec,
  hor      = hor,
  n        = n,
  shock    = 6,
  varNames = varNames,
  main_title = "Forecast Error Variance Decomposition (2006–2019)"
)






#gpt-4.1-nano

sentiment <- read.csv("./data/gpt_sentiment_monthly_for_var.csv")
sentiment$month <- as.Date(sentiment$month)
sentiment <- sentiment %>%
  filter(
    month >= as.Date("2006-01-01"),
    month <= as.Date("2019-10-01")
  )

y_gtp <- as.matrix(
  cbind(
    NM_SA / pop,
    log(busexp) * 100,
    log(consconf) * 100,
    log(indpro) * 100,
    unrate,
    sentiment$gpt_sentiment_opposition
  )
)




# BVAR Specification


T <- nrow(y_gtp)
n <- ncol(y_gtp)

p <- 1          # VAR lags
c <- 1          # Constant
trend <- 0     # No deterministic trend
drawfin <- 1000
hor <- 49       # IRF horizon



# Estimate Bayesian VAR


results_gtp <- CholeskyBVAR(
  y       = y_gtp,
  p       = p,
  c       = c,
  trend   = trend,
  drawfin = drawfin,
  hor     = hor,
  conf    = 68
)


# Extract Results


LowD_gtp    <- results_gtp$LowD
MiddleD_gtp <- results_gtp$MiddleD
HighD_gtp   <- results_gtp$HighD

vardec_gtp  <- results_gtp$vardec


# Variable Names


varNames_gtp <- c(
  "Arrivals",
  "Business Expectations",
  "Consumer Confidence",
  "Industrial Production",
  "Unemployment",
  "Opposition Immigration Sentiment"
)

#Government Pro-Immigration Sentiment

# Plot Impulse Responses

plot_IRF(
  MiddleD = MiddleD_gtp,
  LowD    = LowD_gtp,
  HighD   = HighD_gtp,
  hor      = hor,
  n        = n,
  shock    = 1,
  varNames = varNames_gtp,
  main_title = "Macroeconomic and Sentiment Dynamics (2006–2019)"
)

# Plot Forecast Error Variance Decomposition

plot_FEVD(
  vardec   = vardec_gtp,
  hor      = hor,
  n        = n,
  shock    = 1,
  varNames = varNames,
  main_title = "Forecast Error Variance Decomposition (2006–2019)"
)

