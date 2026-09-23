############################################################
# Does Immigration Grow the Pie?
# Bayesian VAR (Diffuse Prior)
# Main Script
############################################################

rm(list = ls())
library(readxl)
graphics.off()
library(dplyr)

############################################################
# Packages
############################################################

library(MASS)
library(expm)

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


set.seed(0)

############################################################
# Load Data
############################################################

sentiment <- read.csv("./data/immigration_sentiment_monthly.csv")


sentiment <- sentiment %>%
  filter(
    Date >= as.Date("2006-01-01"),
    Date <= as.Date("2019-10-01")
  )

par(mfrow = c(1,1))
plot(as.Date(sentiment$Date) , sentiment$expansive_government , type ="l")

data <- read_excel(
  "data/immigration_database.xlsx",
  sheet = "Monthly data (SA X13+)"
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

# endogenoeus variable 

dset <- cbind(
  NM_SA / pop,
  log(busexp) * 100,
  log(consconf) * 100,
  log(indpro) * 100,
  unrate,
  sentiment$restrictive_government
)

# ---- 4.2 Assign Column Names ----
colnames(dset) <- c(
  "migration",
  "business_expectations",
  "consumer_confidence",
  "industrial_production",
  "unemployment",
  "expansive_government"
)


# ---- 4.3 Remove Missing Values ----
dset <- na.omit(dset)


var_model <- VAR(
  dset,
  p = 3,                   # Lag order
  type = "const"           # Intercept
)


summary(var_model)



variables <- colnames(dset)
impulse <- "expansive_government"

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






