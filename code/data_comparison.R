# Does Immigration Grow the Pie?
# Bayesian VAR (Diffuse Prior)
# Main Script


rm(list = ls())
library(readxl)
graphics.off()
library(dplyr)

############################################################
# Packages
############################################################

library(MASS)
library(expm)


set.seed(0)

############################################################
# Load Data
############################################################

employmentTotal <- read.csv("./data/employmentTotal.csv" , sep = ";")

arbeitlos_number = read.csv("./data/statistik_lzr_20260818203025.csv", skip = 7 ,
                            sep = ";")

# 1. Your vector of German date strings
date_strings <- arbeitlos_number$Berichtsmonat.

# 2. Temporarily set locale to German so R understands "Juli", "März", etc.
# Note: Use "de_DE.UTF-8" for Mac/Linux or "German" for Windows
original_locale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "de_DE.UTF-8") # Try "German" if on Windows

# 3. Parse strings into Date objects (assuming the 1st of each month)
# %B matches full month name, %Y matches 4-digit year
parsed_dates <- as.Date(paste0("1. ", date_strings), format = "%d. %B %Y")

# 4. Restore original locale
Sys.setlocale("LC_TIME", original_locale)

# 5. Sort in ascending (growing) order and format as character strings
sorted_dates <- sort(parsed_dates)
formatted_dates <- format(sorted_dates, "%Y-%m-%d")

print(formatted_dates)


sentiment <- read.csv("./data/immigration_sentiment_monthly_2006_2025.csv")


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
poprate <- NM_SA/pop

data_extend <- read.csv("./data/all_data_2006Q1_2025.csv")




# plot 
par(mfrow = c(2, 3), oma = c(0, 0, 3, 0), mar = c(4, 4, 3, 1))

# 1. Migration
plot(
  x = as.Date(sentiment$Date), 
  y = data_extend$Arrivals * 10, 
  type = "l",
  col = "blue",
  ylim = range(c(poprate$`Total net migration`, data_extend$Arrivals * 10), na.rm = TRUE),
  ylab = "Count",     
  xlab = "Date",
  main = "Migration",
  cex.axis = 0.8              
)
lines(
  x = as.Date(data$DATES),
  y = poprate$`Total net migration`,
  col = "red",
  type = "l"
)

# 2. Business Expectations
plot(
  x = as.Date(sentiment$Date), 
  y = data_extend$BusinessExpectation, 
  type = "l",
  col = "blue",
  ylim = range(c(busexp$`Business expectations`, data_extend$BusinessExpectation), na.rm = TRUE),
  ylab = "Count",     
  xlab = "Date",
  main = "Business expectations",
  cex.axis = 0.8              
)
lines(
  x = as.Date(data$DATES),
  y = busexp$`Business expectations`,
  col = "red",
  type = "l"
)

# 3. Consumer Confidence
plot(
  x = as.Date(sentiment$Date), 
  y = data_extend$ConsumerConfidence, 
  type = "l",
  col = "blue",
  ylim = range(c(consconf$`Consumer confidence`, data_extend$ConsumerConfidence), na.rm = TRUE),
  ylab = "Count",     
  xlab = "Date",
  main = "Consumer confidence",
  cex.axis = 0.8              
)
lines(
  x = as.Date(data$DATES),
  y = consconf$`Consumer confidence`,
  col = "red",
  type = "l"
)

# 4. Industrial Production
plot(
  x = as.Date(sentiment$Date), 
  y = data_extend$IndustrialProduction, 
  type = "l",
  col = "blue",
  ylim = range(c(indpro$`Industrial Production`, data_extend$IndustrialProduction), na.rm = TRUE),
  ylab = "Count",     
  xlab = "Date",
  main = "Industrial Production",
  cex.axis = 0.8              
)
lines(
  x = as.Date(data$DATES),
  y = indpro$`Industrial Production`,
  col = "red",
  type = "l"
)

# 5. Unemployment
plot(
  x = as.Date(sentiment$Date), 
  y = data_extend$unemploymentrate, 
  type = "l",
  col = "blue",
  ylim = range(c(unrate$`Registered unemployed`, data_extend$unemploymentrate), na.rm = TRUE),
  ylab = "Count",     
  xlab = "Date",
  main = "Registered unemployed",
  cex.axis = 0.8              
)
lines(
  x = as.Date(data$DATES),
  y = unrate$`Registered unemployed`,
  col = "red",
  type = "l"
)

plot(
  x = as.Date(arbeitlos_number$Berichtsmonat.) ,
  y = as.numeric(gsub("\\.", "", arbeitlos_number$Bestand.Arbeitslose.)),
  col = "green",
  type = "l"
)


