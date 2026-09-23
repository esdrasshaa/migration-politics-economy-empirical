
rm(list = ls())
library(readxl)
graphics.off()

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

sentiment <- read.csv("./data/immigration_sentiment_monthly_2006_2025.csv")

data <- read_excel(
  "data/immigration_database.xlsx",
  sheet = "Monthly data (SA X13+)"
)

data$DATES <- as.Date( data$DATES)

sentiment <- sentiment %>%
  filter(
    Date >= as.Date("2006-01-01"),
    Date <= as.Date("2015-01-01")
  )

data <- data %>%
  filter(
    DATES >= as.Date("2006-01-01"),
    DATES <= as.Date("2015-01-01")
  )





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


#  visualisation 

par(mfrow = c(2, 1))

plot(as.Date(sentiment$Date) , sentiment$sentiment_opposition , type = "l")
plot(as.Date(sentiment$Date) , NM_SA$`Total net migration`/pop$`Population (interpolated)` , type = "l")


# define the endogeneous variables 

y <- as.matrix(
  cbind(
    NM_SA / pop,
    log(busexp) * 100,
    log(consconf) * 100,
    log(indpro) * 100,
    unrate,
    sentiment$restrictive_opposition
  )
)



############################################################
# BVAR Specification
############################################################

T <- nrow(y)
n <- ncol(y)

p <- 3          # VAR lags
c <- 1          # Constant
trend <- 0      # No deterministic trend
drawfin <- 100
hor <- 49       # IRF horizon


############################################################
# Estimate Bayesian VAR
############################################################

results <- CholeskyBVAR(
  y       = y,
  p       = p,
  c       = c,
  trend   = trend,
  drawfin = drawfin,
  hor     = hor,
  conf    = 68
)

############################################################
# Extract Results
############################################################

LowD    <- results$LowD
MiddleD <- results$MiddleD
HighD   <- results$HighD

vardec  <- results$vardec

############################################################
# Variable Names
############################################################

varNames <- c(
  "Arrivals",
  "Business Expectations",
  "Consumer Confidence",
  "Industrial Production",
  "Unemployment",
  "restrictive_oppositionn"
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
  main_title = "Macroeconomic and Sentiment Dynamics (2006–2015)"
)

# Plot Forecast Error Variance Decomposition

plot_FEVD(
  vardec   = vardec,
  hor      = hor,
  n        = n,
  shock    = 1,
  varNames = varNames,
  main_title = "Forecast Error Variance Decomposition (2006–2015)"
)
