library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readxl)

# Read the file
migration <- read_delim(
  "./../Data/migration.csv",
  delim = ";",
  skip = 9,          # Skip metadata rows
  col_names = FALSE,
  trim_ws = TRUE
)

# Rename columns
colnames(migration) <- c(
  "Year",
  "Month",
  "Foreign_Male",
  "Foreign_Female",
  "Total"
)

# Convert to numeric
migration <- migration %>%
  mutate(
    Year = as.integer(Year),
    across(Foreign_Male:Total, as.numeric)
  )

# Convert month names to dates
migration <- migration %>%
  mutate(
    Date = dmy(paste("01", Month, Year))
  )

# Arrange by date
migration <- migration %>%
  arrange(Date)

# Display
head(migration)


ggplot(migration, aes(x = Date, y = Total)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  labs(
    title = "Monthly Net Migration Balance in Germany",
    subtitle = "2008–2025",
    x = "Year",
    y = "Net Migration"
  ) +
  theme_minimal(base_size = 14)

### population 


population <- read_delim(
  "./../Data/population.csv",
  delim = ";",
  skip = 6,          # Skip metadata rows
  col_names = FALSE,
  trim_ws = TRUE
)

# Rename columns
colnames(population) <- c(
  "Year",
  "Population"
)

population <- population %>%
  mutate(
    Year = year(as.Date(Year))
  )

migration <- migration %>%
  left_join(population, by = "Year")

migration <- migration %>%
  mutate(
    MigrationRate = Total / Population
  )

migration <- migration %>%
  mutate(
    MigrationRate100 = Total / Population * 100
  )


ggplot(migration, aes(x = Date, y = MigrationRate100)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  labs(
    title = "Monthly Net Migration Balance in Germany",
    subtitle = "2008–2025",
    x = "Year",
    y = "% of population"
  ) +
  theme_minimal(base_size = 14)

migration_filtered <- migration %>%
  filter(
    Date >= as.Date("2006-01-01"),
    Date <= as.Date("2025-06-01")
  )

write.csv(
  migration_filtered,
  "./../final_sample/migration_2006Q1_2025Q1.csv",
  row.names = FALSE
)


### unemploymentrate

# Read the Bundesbank file
unemployment <- read_delim(
  "./../Data/unemploymentRate.csv",   # <-- replace with your filename
  delim = ",",
  show_col_types = FALSE
)

# Rename and clean
unemployment <- unemployment %>%
  rename(
    Date = observation_date,
    unemploymentrate = LRHUTTTTDEM156S
  ) %>%
  mutate(
    Date = ymd(Date),
    unemploymentrate = as.numeric(unemploymentrate)
  )


ggplot(unemployment, aes(Date,unemploymentrate)) +
  geom_line(
    colour = "#0072B2",
    linewidth = 0.8
  ) +
  labs(
    title = "Registered Unemployment Rate in Germany",
    subtitle = "Calendar and seasonally adjusted",
    x = "Year",
    y = "Percent"
  ) +
  theme_bw(base_size = 14)


unemployment <- unemployment %>%
  filter(
    Date >= as.Date("2006-01-01"),
    Date <= as.Date("2025-06-01")
  )

write.csv(
  unemployment,
  "./../final_sample/unemployment_2006Q1_2025.csv",
  row.names = FALSE
)


## industrialProductionindex

### Industrial Production
production <- read_delim(
  "./../Data/IndustryProduction.csv",
  delim = ",",
  show_col_types = FALSE
)

# Rename and clean
production <- production %>%
  rename(
    Date = observation_date,
    IndustrialProduction = DEUPROINDMISMEI
  ) %>%
  mutate(
    # Add "-01" to make it a full date (first day of the month)
    Date = ymd(paste0(Date, "-01")),
    IndustrialProduction = as.numeric(IndustrialProduction)
  )

# Plot
ggplot(production, aes(Date, IndustrialProduction)) +
  geom_line(
    colour = "#0072B2",
    linewidth = 0.8
  ) +
  labs(
    title = "Industrial Production Index in Germany",
    x = "Year",
    y = "Index "
  ) +
  theme_bw(base_size = 14)

# Keep the sample used in the thesis
production <- production %>%
  filter(
    Date >= as.Date("2006-01-01"),
    Date <= as.Date("2025-06-01")
  )

# Save
write.csv(
  production,
  "./../final_sample/industrialProduction_2006Q1_2025.csv",
  row.names = FALSE
)


### Consumer Confidence
consumerConfidence <- read_csv(
  "./../Data/consumerconfidence2.csv",
  skip = 2,
  show_col_types = FALSE
)
colnames(consumerConfidence)
# Rename and clean
consumerConfidence <- consumerConfidence %>%
  rename(
    Date =Category,
    ConsumerConfidence = Germany
  ) %>%
  mutate(
    Date = ymd(Date),
    ConsumerConfidence = as.numeric(ConsumerConfidence)
  )

# Plot
ggplot(consumerConfidence, aes(Date, ConsumerConfidence)) +
  geom_line(
    colour = "#0072B2",
    linewidth = 0.8
  ) +
  labs(
    title = "Consumer Confidence in Germany",
    subtitle = "OECD via FRED (Seasonally Adjusted)",
    x = "Year",
    y = "Percentage Balance"
  ) +
  theme_bw(base_size = 14)

# Keep thesis sample
consumerConfidence <- consumerConfidence %>%
  filter(
    Date >= as.Date("2006-01-01"),
    Date <= as.Date("2025-06-01")
  )

# Save
write.csv(
  consumerConfidence,
  "./../final_sample/consumerConfidence_2006Q1_2025.csv",
  row.names = FALSE
)




### Business Expectations

# Read the Excel file
businessExpectation <- read_excel(
  "./../Data/businessExpectation.xlsx",
  skip = 7   # Adjust if necessary
)

# Keep only the month and Business Expectations index
businessExpectation <- businessExpectation %>%
  select(
    Date = `Monthyear`,
    BusinessExpectation = `BusinessExpectations...4`
  ) %>%
  mutate(
    Date = dmy(paste0("01/", Date)),   # "01/2005" -> "2005-01-01"
    BusinessExpectation = as.numeric(BusinessExpectation)
  )

# Plot
ggplot(businessExpectation,
       aes(Date, BusinessExpectation)) +
  geom_line(
    colour = "#0072B2",
    linewidth = 0.8
  ) +
  labs(
    title = "ifo Business Expectations Index",
    subtitle = "Germany (2015 = 100, seasonally adjusted)",
    x = "Year",
    y = "Index (2015 = 100)"
  ) +
  theme_bw(base_size = 14)

# Keep thesis sample
businessExpectation <- businessExpectation %>%
  filter(
    Date >= as.Date("2006-01-01"),
    Date <= as.Date("2025-06-01")
  )

# Save
write.csv(
  businessExpectation,
  "./../final_sample/businessExpectation_2006Q1_2025.csv",
  row.names = FALSE
)




