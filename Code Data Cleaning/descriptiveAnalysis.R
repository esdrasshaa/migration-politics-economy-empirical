# ============================================================
# MASTER THESIS — Esdras Shaa Prombo
# Combined Dataset: Descriptive Statistics & Interaction Plots
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)      # for ggpairs correlation matrix
library(gridExtra)   # for grid.arrange
library(scales)      # for axis formatting
library(corrplot)    # for correlation heatmap
library(readxl)

### Business Expectations
data_busexp <- read.csv(
  "./../final_sample/businessExpectation_2006Q1_2025.csv",
  stringsAsFactors = FALSE
)
data_busexp$Date <- as.Date(data_busexp$Date)


### Consumer Confidence
data_conconf <- read.csv(
  "./../final_sample/consumerConfidence_2006Q1_2025.csv",
  stringsAsFactors = FALSE
)
data_conconf$Date <- as.Date(data_conconf$Date)

## industrialProductionindex
data_indpr <- read.csv(
  "./../final_sample/industrialProduction_2006Q1_2025.csv",
  stringsAsFactors = FALSE
)
data_indpr$Date <- as.Date(data_indpr$Date)

### migration

#-------------------------------------------------------
# Read the replication database
#-------------------------------------------------------

migration_old <- read_excel(
  "./../final_sample/immigration_database.xlsx",
  sheet = "Monthly data (SA X13+)"
)

#-------------------------------------------------------
# Keep only Date and Total net migration
#-------------------------------------------------------

migration_old <- data.frame(
  Date = as.Date(migration_old$DATES),
  Total = migration_old$`Total net migration`
)

#-------------------------------------------------------
# Keep only observations before 2008-01-01
#-------------------------------------------------------

migration_old <- migration_old %>%
  filter(Date < as.Date("2008-01-01"))



migration_old <- migration_old%>%
  mutate(
    Year = year(as.Date(Date))
  )



data_mig <- read.csv(
  "./../final_sample/migration_2006Q1_2025.csv",
  stringsAsFactors = FALSE
)
data_mig$Date <- as.Date(data_mig$Date)

data_mig<- bind_rows(
  migration_old,
  data_mig
) %>%
  arrange(Year)


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

data_mig <- data_mig %>%
  left_join(population, by = "Year")

data_mig <- data_mig %>%
  mutate(
    MigrationRate = Total / Population.y
  )

data_mig <- data_mig %>%
  mutate(
    MigrationRate100 = Total / Population.y * 100
  )



### unemploymentrate
data_unemp <- read.csv(
  "./../final_sample/unemployment_2006Q1_2025.csv",
  stringsAsFactors = FALSE
)
data_unemp$Date <- as.Date(data_unemp$Date)


#  MERGE ALL DATASETS INTO ONE 

data_all <- data_busexp %>%
  select(Date, BusinessExpectation
) %>%
  
  full_join(
    data_conconf %>% select(Date, ConsumerConfidence),
    by = "Date"
  ) %>%
  
  full_join(
    data_indpr %>% select(Date, IndustrialProduction),
    by = "Date"
  ) %>%
  
  full_join(
    data_mig %>% select(Date, Arrivals = MigrationRate100),
    by = "Date"
  ) %>%
  
  full_join(
    data_unemp %>% select(Date,unemploymentrate),
    by = "Date"
  ) %>%
  
  # full_join(
  #   dataSen %>% select(Date,
  #                      Sentiment_Total      = sentiment_gesamt,
  #                      Sentiment_Government = sentiment_government,
  #                      Sentiment_Opposition = sentiment_opposition),
  #   by = "Date"
  # ) %>%
  # 
  
  arrange(Date)

# Quick check
cat("── Combined dataset ──\n")
cat("Rows:", nrow(data_all), "\n")
cat("Columns:", ncol(data_all), "\n")
cat("Date range:", as.character(min(data_all$Date, na.rm = TRUE)),
    "→", as.character(max(data_all$Date, na.rm = TRUE)), "\n\n")

data_all

vars_numeric <- c("BusinessExpectation", "ConsumerConfidence","IndustrialProduction", "Arrivals",
                  "unemploymentrate")

desc_stats <- data_all %>%
  select(all_of(vars_numeric)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    N    = sum(!is.na(Value)),
    Mean = mean(Value, na.rm = TRUE),
    SD   = sd(Value,   na.rm = TRUE),
    Min  = min(Value,  na.rm = TRUE),
    Max  = max(Value,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 3)))

cat("── Descriptive Statistics ──\n")
print(desc_stats, n = Inf)
cat("\n")

#  TIME SERIES OVERVIEW — all variables

# Pivot to long format for ggplot faceting
data_long <- data_all %>%
  pivot_longer(cols = all_of(vars_numeric),
               names_to  = "Variable",
               values_to = "Value") %>%
  mutate(Variable = factor(Variable, levels = vars_numeric))


# labels for facet titles
var_labels <- c(
  BusinessExpectation  = "ifo Business Expectations Index",
  ConsumerConfidence   = "Consumer Confidence in Germany",
  IndustrialProduction = "Industrial Production Index in Germany",
  Arrivals       = "Monthly Net Migration Balance in Germany",
  unemploymentrate      = "Registered Unemployment Rate in Germany"
  )

levels(data_long$Variable) <- var_labels[levels(data_long$Variable)]

p_timeseries <- ggplot(data_long, aes(x = Date, y = Value)) +
  geom_line(colour = "#19376D", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "gray60", linewidth = 0.4) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
  labs(
    title    = "Time Series Overview — All VAR Variables",
    subtitle = "monthly data | Master Thesis: Political Discourse on and Economic Impact of Migration",
    x = NULL, y = NULL,
    caption  = "Sources: Bundesbank, Destatis GENESIS,FRED,ifo Institut"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", colour = "#19376D", size = 13),
    plot.subtitle = element_text(colour = "gray50", size = 9),
    plot.caption  = element_text(colour = "gray60", size = 7, hjust = 0),
    strip.text    = element_text(face = "bold", colour = "#19376D", size = 9),
    panel.grid.minor = element_blank(),
    axis.text.x   = element_text(size = 7),
    axis.text.y   = element_text(size = 7)
  )

print(p_timeseries)


# Save
write.csv(
  data_all, "./../final_sample/all_data_2006Q1_2025.csv",
  row.names = FALSE
)

