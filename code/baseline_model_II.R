rm(list = ls())
library(readxl)
graphics.off()
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

sentiment <- read.csv("./data/immigration_sentiment_monthly_2006_2025.csv")
data <- read.csv("./data/all_data_2006Q1_2025.csv")

sentiment <- sentiment %>%
  filter(
    Date >= as.Date("2022-01-01"),
    Date <= as.Date("2025-06-01")
  )

data <- data %>%
  filter(
    Date >= as.Date("2022-01-01"),
    Date <= as.Date("2025-06-01")
  )

#  visualisation 

par(mfrow = c(2, 1))

plot(as.Date(sentiment$Date) , sentiment$sentiment_gesamt , type = "l")
plot(as.Date(sentiment$Date) , data$Arrivals , type = "l")

# endogenoeous variable

y <- as.matrix(
  cbind(
    data$Arrivals,
    log(data$BusinessExpectation) * 100,
    log(data$ConsumerConfidence) * 100,
    log(data$IndustrialProduction) * 100,
    data$unemploymentrate,
    sentiment$restrictive_opposition
    
  )
)



T <- nrow(y)
n <- ncol(y)

p <- 3          # VAR lags
c <- 1          # Constant
trend <- 0      # No deterministic trend
drawfin <- 100
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
  "restrictive_opposition"
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
  main_title = "Macroeconomic and Sentiment Dynamics (2022–2025)"
)


# Plot Forecast Error Variance Decomposition

plot_FEVD(
  vardec   = vardec,
  hor      = hor,
  n        = n,
  shock    = 6,
  varNames = varNames,
  main_title = "Forecast Error Variance Decomposition (2022–2025)"
)









