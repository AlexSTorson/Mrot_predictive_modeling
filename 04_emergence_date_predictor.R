################################################################################
#
# Title: Three-Phase Temperature Transfer Predictor for Bee Emergence Timing
#
# Author: Alex Torson
# Institution: USDA-ARS
# Email: Alex.Torson@usda.gov
#
# Description: Predicts emergence dates for a user-specified three-phase
#              protocol. Returns predictions for combined, female, and male
#              models.
#
# Dependencies: 01_dev_rate_estimation.R must be run first.
#
# Input files:
#   - 01_dev_rate_estimation/dev_rate_summary_combined.csv
#   - 01_dev_rate_estimation/dev_rate_summary_by_sex.csv
#
# Output files:
#   - 04_emergence_date_predictor/predictor_output.csv
#   - 04_emergence_date_predictor/04_session_info.txt
#
################################################################################

# Package Loading -------------------------------------------------------------

library(tidyverse)
library(lubridate)

# Working Directory -----------------------------------------------------------

setwd(
  "/Users/alextorson/Library/CloudStorage/OneDrive-USDA/Torson_Lab/Mrot_Predictive_Modeling/"
)

if (!dir.exists("./04_emergence_date_predictor/")) {
  dir.create("./04_emergence_date_predictor/", recursive = TRUE)
}

# User Input ------------------------------------------------------------------

startTemp <- 29
transferDownTemp <- 23
transferUpTemp <- 31

startDate <- "5/1/2022"
transferDownDate <- "5/20/2022"
transferUpDate <- "6/01/2022"

# Load Developmental Rate Summaries -------------------------------------------

summary_combined <- read_csv("./01_dev_rate_estimation/dev_rate_summary_combined.csv") %>%
  select(temperature, medianDevRate) %>%
  rename(devRate = medianDevRate)

summary_by_sex <- read_csv("./01_dev_rate_estimation/dev_rate_summary_by_sex.csv") %>%
  select(temperature, sex, medianDevRate) %>%
  rename(devRate = medianDevRate)

summary_female <- summary_by_sex %>% filter(sex == "Female") %>% select(-sex)
summary_male <- summary_by_sex %>% filter(sex == "Male") %>% select(-sex)

# Parse Dates -----------------------------------------------------------------

start_date <- mdy(startDate)
transfer_down_date <- mdy(transferDownDate)
transfer_up_date <- mdy(transferUpDate)

duration_first_thermo <- as.numeric(transfer_down_date - start_date)
duration_cryo <- as.numeric(transfer_up_date   - transfer_down_date)

# Prediction Function ---------------------------------------------------------

predict_emergence <- function(input_data, group_label) {
  
  rate_first_thermo <- input_data %>% filter(temperature == startTemp) %>% pull(devRate)
  rate_cryo <- input_data %>% filter(temperature == transferDownTemp) %>% pull(devRate)
  rate_final <- input_data %>% filter(temperature == transferUpTemp) %>% pull(devRate)
  
  dev_first_thermo <- duration_first_thermo * rate_first_thermo
  dev_cryo <- duration_cryo * rate_cryo
  total_dev <- dev_first_thermo + dev_cryo
  remaining_dev <- 1 - total_dev
  
  final_days <- round(remaining_dev / rate_final, digits = 0)
  predicted_date <- transfer_up_date + days(final_days)
  
  tibble(
    group = group_label,
    startDate = startDate,
    startTemp = startTemp,
    transferDownDate = transferDownDate,
    transferDownTemp = transferDownTemp,
    transferUpDate = transferUpDate,
    transferUpTemp = transferUpTemp,
    durationFirstThermo = duration_first_thermo,
    durationCryo = duration_cryo,
    devFirstThermo = round(dev_first_thermo, 4),
    devCryo = round(dev_cryo, 4),
    totalDev = round(total_dev, 4),
    remainingDev = round(remaining_dev, 4),
    finalDays = final_days,
    predictedDate = predicted_date
  )
}

# Run Predictions -------------------------------------------------------------

output_table <- bind_rows(
  predict_emergence(summary_combined, "combined"),
  predict_emergence(summary_female, "female"),
  predict_emergence(summary_male, "male")
)

print(output_table)

# Save Output -----------------------------------------------------------------

write_csv(output_table, file = "./04_emergence_date_predictor/predictor_output.csv")

# Session Info ----------------------------------------------------------------

writeLines(capture.output(sessionInfo()), "./04_emergence_date_predictor/04_session_info.txt")