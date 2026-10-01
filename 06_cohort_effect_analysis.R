################################################################################
#
# Title: Cohort (April vs. June) Effect on Developmental Rate and Its
#        Consequence for Validation-Set Predictive Performance
#
# Author: Alex Torson
# Institution: USDA-ARS
# Email: Alex.Torson@usda.gov
#
# Description: Tests whether cohort (April vs. June 2021, a proxy for age at
#              induction of development / overwintering duration) affects
#              developmental rate in the model-development experiments, and
#              evaluates whether restricting the training data to the April
#              cohort (rather than pooling April + June, as used in the
#              reported model) changes predictive performance against
#              independent validation data (experiments 3-4, 2022-2023).
#
# Dependencies: none
#
# Input files:
#   - degree_day_dataset.csv
#
# Output files:
#   - Table_S7_cohort_anova.csv
#   - Table_S8_cohort_predicted_days.csv
#   - cohort_dev_days_combined.png/pdf
#   - Table_S9_cohort_validation_comparison.csv
#   - 06_session_info.txt
#
################################################################################

# Package Loading -------------------------------------------------------------

library(tidyverse)
library(ggsci)
library(patchwork)
library(quantreg)

# Plotting Theme Setup --------------------------------------------------------

alex_theme <- theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", size = 12),
    axis.text = element_text(face = "plain", size = 10),
    axis.title = element_text(face = "plain", size = 10),
    axis.title.x = element_text(margin = margin(t = 8, r = 20, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, r = 6, b = 0, l = 0)),
    panel.border = element_rect(fill = NA, colour = "black", linewidth = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank()
  )

# Working Directory and Output Setup ------------------------------------------

setwd(
  "/Users/alex.torson/Library/CloudStorage/OneDrive-USDA/Torson_Lab/Mrot_Predictive_Modeling/"
)

if (!dir.exists("./06_cohort_effect_analysis/")) {
  dir.create("./06_cohort_effect_analysis/", recursive = TRUE)
}

# Data Loading and Cleaning ---------------------------------------------------

raw_data <- read_csv("./degree_day_dataset.csv")

# Model-development data (experiments 1-2), with cohort added.
emergence_data <- raw_data %>%
  filter(
    experiment %in% c(1, 2),
    !is.na(daysToEmergence),
    sex %in% c("M", "F")
  ) %>%
  mutate(
    treatment = as.integer(treatment),
    devRate = 1 / daysToEmergence,
    sex = recode(sex, "M" = "Male", "F" = "Female"),
    cohort = recode(as.character(experiment), "1" = "April", "2" = "June")
  ) %>%
  rename(temperature = treatment)

# Cohort Effect on Developmental Rate ------------------------------------------
# temperature * cohort * sex tests whether cohort affects development rate,
# and whether that effect is separable from the sex ratio difference between
# cohorts (i.e. the effect should hold even after accounting for sex).

lm_cohort <- lm(devRate ~ temperature * cohort * sex, data = emergence_data)
anova_cohort <- anova(lm_cohort)
print(anova_cohort)

table_s7 <- anova_cohort %>%
  as.data.frame() %>%
  rownames_to_column("term")
write_csv(table_s7, "./06_cohort_effect_analysis/Table_S7_cohort_anova.csv")

# Practical effect size: predicted days to emergence by cohort and sex --------

cohort_predictions <- expand_grid(
  temperature = seq(21, 31, by = 2),
  cohort = c("April", "June"),
  sex = c("Female", "Male")
) %>%
  mutate(
    predictedDevRate = predict(lm_cohort, newdata = .),
    predictedDays = 1 / predictedDevRate
  )

print(cohort_predictions)
write_csv(cohort_predictions, "./06_cohort_effect_analysis/Table_S8_cohort_predicted_days.csv")

# Plot: Developmental Rate by Cohort and Sex -----------------------------------

cohort_predictions_fine <- expand_grid(
  temperature = seq(21, 31, 0.1),
  cohort = c("April", "June"),
  sex = c("Female", "Male")
) %>%
  mutate(
    predictedDevRate = predict(lm_cohort, newdata = .),
    predictedDays = 1 / predictedDevRate
  )

(cohort_plot <- ggplot() +
    geom_jitter(
      data = emergence_data,
      aes(x = temperature, y = devRate, color = cohort),
      width = 0.3, size = 0.8, alpha = 0.3
    ) +
    geom_line(
      data = cohort_predictions_fine,
      aes(x = temperature, y = predictedDevRate, color = cohort, linetype = sex),
      linewidth = 0.8
    ) +
    scale_color_jco() +
    scale_x_continuous(breaks = seq(21, 31, 2)) +
    alex_theme +
    theme(legend.position = c(0.2, 0.7),
          legend.background = element_rect(colour = "black", linewidth = 0.25)) +
    xlab("Temperature (°C)") +
    ylab("Developmental rate (day⁻¹)") +
    ggtitle(NULL)
)

# Plot: Days to Emergence by Cohort and Sex ------------------------------------

(cohort_days_plot <- ggplot() +
   geom_jitter(
     data = emergence_data,
     aes(x = temperature, y = daysToEmergence, color = cohort),
     width = 0.3, size = 0.8, alpha = 0.3
   ) +
   geom_line(
     data = cohort_predictions_fine,
     aes(x = temperature, y = predictedDays, color = cohort, linetype = sex),
     linewidth = 0.8
   ) +
   scale_color_jco() +
   scale_x_continuous(breaks = seq(21, 31, 2)) +
   alex_theme +
   theme(legend.position = "none") +
   xlab("Temperature (°C)") +
   ylab("Days to emergence") +
   ggtitle(NULL)
)

# Panel: Developmental Rate + Days to Emergence, Combined ---------------------

cohort_specific_figure <- (cohort_plot | cohort_days_plot) +
  plot_annotation(tag_levels = list(c("A.", "B."))) &
  theme(plot.tag = element_text(size = 12, face = "bold"))

ggsave(plot = cohort_specific_figure,
       filename = "./06_cohort_effect_analysis/cohort_dev_days_combined.png",
       width = 180, height = 90, units = "mm", dpi = 300)
ggsave(plot = cohort_specific_figure,
       filename = "./06_cohort_effect_analysis/cohort_dev_days_combined.pdf",
       width = 180, height = 90, units = "mm")

# Validation-Performance Comparison: April-only vs. Pooled --------------------
# Tests whether restricting the training data to the April cohort (matching
# the approximate timing of the validation experiments) improves predictive
# performance relative to the pooled (April+June) model used in the reported
# three-phase model, against independent data from experiments 3-4.

fit_dev_threshold <- function(data) {
  summary_tbl <- data %>%
    group_by(temperature) %>%
    summarise(medianDevRate = median(devRate), .groups = "drop")
  lm_obj <- lm(medianDevRate ~ temperature, data = summary_tbl)
  -coef(lm_obj)["(Intercept)"] / coef(lm_obj)["temperature"]
}

fit_rate_table <- function(data, base_temp, temps = c(21, 23, 25, 27, 29, 31)) {
  data <- data %>% mutate(inv_temp = 1 / (temperature - base_temp))
  newdata <- tibble(inv_temp = 1 / (temps - base_temp))
  tibble(
    temperature = temps,
    p10 = 1 / predict(rq(daysToEmergence ~ inv_temp, tau = 0.10, data = data), newdata = newdata),
    p50 = 1 / predict(rq(daysToEmergence ~ inv_temp, tau = 0.50, data = data), newdata = newdata),
    p90 = 1 / predict(rq(daysToEmergence ~ inv_temp, tau = 0.90, data = data), newdata = newdata)
  )
}

fit_group <- function(data) {
  fit_rate_table(data, fit_dev_threshold(data))
}

april_data <- filter(emergence_data, cohort == "April")

rates_april_combined  <- fit_group(april_data)
rates_pooled_combined <- fit_group(emergence_data)
rates_april_female    <- fit_group(filter(april_data, sex == "Female"))
rates_pooled_female   <- fit_group(filter(emergence_data, sex == "Female"))
rates_april_male      <- fit_group(filter(april_data, sex == "Male"))
rates_pooled_male     <- fit_group(filter(emergence_data, sex == "Male"))

# Observed validation percentiles (experiments 3-4, interrupted development
# treatments only; treatment strings encode phases as temp_weeks_temp_..._temp,
# e.g. "29_2_25_2_29" = 29C for 2wk, 25C for 2wk, back to 29C until emergence)

validation_raw <- raw_data %>%
  filter(experiment %in% c(3, 4), !is.na(daysToEmergence), sex %in% c("M", "F"),
         str_detect(treatment, "_")) %>%
  mutate(sex = recode(sex, "M" = "Male", "F" = "Female"))

validation_percentiles <- validation_raw %>%
  group_by(treatment) %>%
  summarise(
    n = n(),
    obs_p10 = quantile(daysToEmergence, 0.10),
    obs_p50 = quantile(daysToEmergence, 0.50),
    obs_p90 = quantile(daysToEmergence, 0.90),
    .groups = "drop"
  )

validation_percentiles_sex <- validation_raw %>%
  group_by(treatment, sex) %>%
  summarise(
    n = n(),
    obs_p10 = quantile(daysToEmergence, 0.10),
    obs_p50 = quantile(daysToEmergence, 0.50),
    obs_p90 = quantile(daysToEmergence, 0.90),
    .groups = "drop"
  )

# Predicted development/days for a treatment, given a rate table --------------

parse_treatment <- function(treatment_str) {
  parts <- as.numeric(str_split(treatment_str, "_")[[1]])
  n <- length(parts)
  list(temps = parts[seq(1, n, by = 2)], weeks = parts[seq(2, n - 1, by = 2)])
}

predict_treatment <- function(treatment_str, rate_table, quantile_col) {
  parsed <- parse_treatment(treatment_str)
  rate_lookup <- function(temp) rate_table[[quantile_col]][rate_table$temperature == temp]
  
  pre_final_dev  <- 0
  pre_final_days <- 0
  for (i in seq_along(parsed$weeks)) {
    days_i <- parsed$weeks[i] * 7
    pre_final_dev  <- pre_final_dev + rate_lookup(parsed$temps[i]) * days_i
    pre_final_days <- pre_final_days + days_i
  }
  
  final_rate <- rate_lookup(parsed$temps[length(parsed$temps)])
  remaining_days <- (1 - pre_final_dev) / final_rate
  
  tibble(total_pre_final_dev = pre_final_dev,
         total_days = pre_final_days + remaining_days)
}

# Generate predictions from rate_table_score, with a matched exclusion flag:
# a treatment is excluded from BOTH models' error calculations if EITHER
# model predicts (at the median rate) that development would complete before
# the final phase is ever reached (total_pre_final_dev >= 1).

predict_window <- function(observed_df, rate_table_score, rate_table_other) {
  excl_score <- map_dbl(observed_df$treatment,
                        ~ predict_treatment(.x, rate_table_score, "p50")$total_pre_final_dev) >= 1
  excl_other <- map_dbl(observed_df$treatment,
                        ~ predict_treatment(.x, rate_table_other, "p50")$total_pre_final_dev) >= 1
  
  observed_df %>%
    mutate(
      pred_p10 = map_dbl(treatment, ~ predict_treatment(.x, rate_table_score, "p10")$total_days),
      pred_p50 = map_dbl(treatment, ~ predict_treatment(.x, rate_table_score, "p50")$total_days),
      pred_p90 = map_dbl(treatment, ~ predict_treatment(.x, rate_table_score, "p90")$total_days),
      excluded = excl_score | excl_other
    )
}

compute_rmse_mae <- function(preds, model_label, group_label) {
  d <- filter(preds, !excluded)
  tibble(
    model = model_label,
    group = group_label,
    n = nrow(d),
    rmse_p10 = sqrt(mean((d$pred_p10 - d$obs_p10)^2)),
    rmse_p50 = sqrt(mean((d$pred_p50 - d$obs_p50)^2)),
    mae_p50  = mean(abs(d$pred_p50 - d$obs_p50)),
    rmse_p90 = sqrt(mean((d$pred_p90 - d$obs_p90)^2))
  )
}

cohort_validation_comparison <- bind_rows(
  compute_rmse_mae(
    predict_window(validation_percentiles, rates_april_combined, rates_pooled_combined),
    "April-only", "Combined"),
  compute_rmse_mae(
    predict_window(validation_percentiles, rates_pooled_combined, rates_april_combined),
    "Pooled", "Combined"),
  compute_rmse_mae(
    predict_window(filter(validation_percentiles_sex, sex == "Female"),
                   rates_april_female, rates_pooled_female),
    "April-only", "Female"),
  compute_rmse_mae(
    predict_window(filter(validation_percentiles_sex, sex == "Female"),
                   rates_pooled_female, rates_april_female),
    "Pooled", "Female"),
  compute_rmse_mae(
    predict_window(filter(validation_percentiles_sex, sex == "Male"),
                   rates_april_male, rates_pooled_male),
    "April-only", "Male"),
  compute_rmse_mae(
    predict_window(filter(validation_percentiles_sex, sex == "Male"),
                   rates_pooled_male, rates_april_male),
    "Pooled", "Male")
)

print(cohort_validation_comparison)
write_csv(cohort_validation_comparison, "./06_cohort_effect_analysis/Table_S9_cohort_validation_comparison.csv")

# Session Info ------------------------------------------------------------------

writeLines(capture.output(sessionInfo()), "./06_cohort_effect_analysis/06_session_info.txt")