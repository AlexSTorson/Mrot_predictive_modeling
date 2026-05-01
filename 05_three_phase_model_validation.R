################################################################################
#
# Title: Three-Phase Temperature Transfer Model Validation
#
# Author: Alex Torson
# Institution: USDA-ARS
# Email: Alex.Torson@usda.gov
#
# Description: Validates the three-phase model against observed emergence
#              from Experiments 3 and 4 using RMSE and MAE. One treatment
#              (29_2_25_2_29) is excluded where total development >= 1.0
#              before the final thermophase. Includes sex-specific validation
#              and emergence window validation (Tables S5-S6).
#
# Dependencies: 01_dev_rate_estimation.R must be run first.
#
# Input files:
#   - 01_dev_rate_estimation/dev_rate_summary_combined.csv
#   - 01_dev_rate_estimation/dev_rate_summary_by_sex.csv
#   - 01_dev_rate_estimation/quantile_predictions_combined.csv
#   - 01_dev_rate_estimation/quantile_predictions_female.csv
#   - 01_dev_rate_estimation/quantile_predictions_male.csv
#   - degree_day_dataset.csv
#
# Output files:
#   - 05_three_phase_model_validation/three_phase_model_validation_predictions.csv
#   - 05_three_phase_model_validation/three_phase_model_validation_predictions_all.csv
#   - 05_three_phase_model_validation/three_phase_model_validation_plot.png/pdf
#   - 05_three_phase_model_validation/three_phase_model_validation_plot_by_sex.png/pdf
#   - 05_three_phase_model_validation/three_phase_model_validation_window_predictions.csv
#   - 05_three_phase_model_validation/three_phase_model_validation_window_predictions_by_sex.csv
#   - 05_three_phase_model_validation/three_phase_model_validation_window_accuracy.csv
#   - 05_three_phase_model_validation/three_phase_model_validation_window_plot_facet.png/pdf
#   - 05_three_phase_model_validation/three_phase_model_validation_window_plot_dumbbell.png/pdf
#   - 05_three_phase_model_validation/three_phase_model_validation_window_plot_facet_by_sex.png/pdf
#   - 05_three_phase_model_validation/three_phase_model_validation_window_plot_dumbbell_by_sex.png/pdf
#   - 05_three_phase_model_validation/Table_S5_int_dev_model_performance.csv
#   - 05_three_phase_model_validation/Table_S6_validation_window_predictions.csv
#   - 05_three_phase_model_validation/05_session_info.txt
#
################################################################################

# Package Loading -------------------------------------------------------------

library(tidyverse)
library(ggsci)
library(ggforce)
library(patchwork)
library(cowplot)

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

# Working Directory -----------------------------------------------------------

setwd(
  "/Users/alextorson/Library/CloudStorage/OneDrive-USDA/Torson_Lab/Mrot_Predictive_Modeling/"
)

if (!dir.exists("./05_three_phase_model_validation/")) {
  dir.create("./05_three_phase_model_validation/", recursive = TRUE)
}

# Load Developmental Rate Summary ---------------------------------------------

input_data <- read_csv("./01_dev_rate_estimation/dev_rate_summary_combined.csv") %>%
  select(temperature, medianDays, medianDevRate) %>%
  rename(
    medianEmergence = medianDays,
    devRate = medianDevRate
  )

# Load and Summarise Validation Data ------------------------------------------
#
# Retain only Experiments 3 and 4, which used multi-phase transfer protocols.
# Compute observed median and SD of days to emergence per treatment.
# Control treatments (single integer, e.g., "29") are retained separately
# for reference but excluded from model validation metrics.

raw_data <- read_csv("./degree_day_dataset.csv")

validation_observed <- raw_data %>%
  filter(
    experiment %in% c(3, 4),
    !is.na(daysToEmergence)
  ) %>%
  group_by(treatment) %>%
  summarise(
    n_obs = n(),
    obs_median = median(daysToEmergence),
    obs_sd = sd(daysToEmergence),
    .groups = "drop"
  ) %>%
  mutate(is_control = !str_detect(treatment, "_"))

# Parse Treatment Strings and Run Model ---------------------------------------
#
# Multi-phase treatment strings are split on "_" to extract:
#   startTemp, weeksFirstThermo, transferTemp, weeksCryo, finalTemp
# Weeks are converted to days (* 7).

model_predictions <- validation_observed %>%
  filter(!is_control) %>%
  mutate(
    parts = str_split(treatment, "_"),
    startTemp = map_int(parts, ~ as.integer(.x[1])),
    weeksFirstThermo = map_int(parts, ~ as.integer(.x[2])),
    transferDownTemp = map_int(parts, ~ as.integer(.x[3])),
    weeksCryo = map_int(parts, ~ as.integer(.x[4])),
    transferUpTemp = map_int(parts, ~ as.integer(.x[5])),
    d1 = weeksFirstThermo * 7L,
    d2 = weeksCryo * 7L
  ) %>%
  rowwise() %>%
  mutate(
    r1 = input_data %>% filter(temperature == startTemp) %>% pull(devRate),
    r2 = input_data %>% filter(temperature == transferDownTemp) %>% pull(devRate),
    r3 = input_data %>% filter(temperature == transferUpTemp) %>% pull(devRate),
    
    dev1 = d1 * r1,
    dev2 = d2 * r2,
    total_dev = dev1 + dev2,
    rem_dev = 1 - total_dev,
    days_final = round(rem_dev / r3, digits = 0),
    
    pred_days = d1 + d2 + days_final,
    
    # Residual: positive = model over-predicts
    residual = pred_days - obs_median
    
  ) %>%
  ungroup() %>%
  select(treatment, transferDownTemp, weeksFirstThermo, weeksCryo,
         n_obs, obs_median, obs_sd,
         pred_days, residual, total_dev)

model_predictions

# Protocol Violation Exclusion ------------------------------------------------
#
# One treatment (29_2_25_2_29) accumulated total_dev = 1.01 across the initial
# thermophase and cryophase alone, leaving no remaining development for the
# final thermophase. The model prediction for this treatment is undefined.
# It is excluded from RMSE and MAE calculations but retained in the full
# predictions CSV for the sake of transparency.

model_predictions_valid <- model_predictions %>%
  filter(total_dev < 1)

violations <- model_predictions %>%
  filter(total_dev >= 1)

if (nrow(violations) > 0) {
  cat(sprintf(
    "NOTE: %d treatment(s) excluded from accuracy metrics (total_dev >= 1):\n",
    nrow(violations)
  ))
  print(violations %>% select(treatment, total_dev, pred_days, obs_median))
}

# Validation Summary Statistics -----------------------------------------------

rmse <- sqrt(mean(model_predictions_valid$residual^2))
mae <- mean(abs(model_predictions_valid$residual))

# Validation Plot: Predicted vs. Observed -------------------------------------
#
# Each point is one treatment combination. Horizontal error bars show observed
# SD. Points are colored by transfer temperature and shaped by protocol
# (thermophase/cryophase weeks). A 1:1 reference line indicates perfect
# prediction.

axis_range <- range(c(model_predictions_valid$obs_median - model_predictions_valid$obs_sd,
                      model_predictions_valid$obs_median + model_predictions_valid$obs_sd,
                      model_predictions_valid$pred_days))

axis_limits <- c(floor(axis_range[1]   / 5) * 5,
                 ceiling(axis_range[2] / 5) * 5)

(validation_plot <- ggplot(model_predictions_valid,
                           aes(x = obs_median,
                               y = pred_days,
                               color = factor(transferDownTemp),
                               shape = interaction(factor(weeksFirstThermo),
                                                   factor(weeksCryo)))) +
    geom_mark_ellipse(
      data = filter(model_predictions_valid, weeksFirstThermo == 1 & weeksCryo == 2),
      aes(x = obs_median, y = pred_days, label = "1 wk / 2 wk cryophase"),
      color = "grey60",
      fill = NA,
      label.fill = NA,
      label.colour = "grey60",
      con.colour = "grey60",
      label.fontsize = 8,
      label.buffer = unit(2, "mm"),
      con.type = "elbow",
      expand = unit(3, "mm"),
      inherit.aes = FALSE
    ) +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      linewidth = 0.5,
      color = "grey50"
    ) +
    geom_errorbarh(
      aes(xmin = obs_median - obs_sd,
          xmax = obs_median + obs_sd),
      height = 0.4,
      linewidth = 0.5
    ) +
    geom_point(size = 3) +
    scale_color_jco(name = "Transfer Temp. (\u00b0C)") +
    scale_shape_manual(
      name = "Protocol (thermo/cryo weeks)",
      values = c("1.1" = 1,
                 "1.2" = 2,
                 "2.1" = 16,
                 "2.2" = 17),
      labels = c("1.1" = "1 wk / 1 wk",
                 "1.2" = "1 wk / 2 wk",
                 "2.1" = "2 wk / 1 wk",
                 "2.2" = "2 wk / 2 wk")
    ) +
    scale_x_continuous(limits = axis_limits,
                       breaks = seq(axis_limits[1], axis_limits[2], 5)) +
    scale_y_continuous(limits = axis_limits,
                       breaks = seq(axis_limits[1], axis_limits[2], 5)) +
    coord_equal() +
    alex_theme +
    theme(legend.position = c(0.22, 0.72),
          legend.background = element_rect(colour = "black", linewidth = 0.25),
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 8),
          legend.spacing.y = unit(0.1, "cm")) +
    xlab("Observed days to emergence (median \u00b1 SD)") +
    ylab("Predicted days to emergence") +
    ggtitle(NULL)
)

ggsave(
  plot = validation_plot,
  filename = "./05_three_phase_model_validation/three_phase_model_validation_plot.png",
  width = 130,
  height = 130,
  units = "mm",
  dpi = 300
)

ggsave(
  plot = validation_plot,
  filename = "./05_three_phase_model_validation/three_phase_model_validation_plot.pdf",
  width = 130,
  height = 130,
  units = "mm"
)

# Save full predictions (all treatments, including the excluded violation)
# and the validated subset (used for RMSE/MAE) separately.
write_csv(model_predictions, file = "./05_three_phase_model_validation/three_phase_model_validation_predictions_all.csv")
write_csv(model_predictions_valid, file = "./05_three_phase_model_validation/three_phase_model_validation_predictions.csv")

# Sex-Specific Validation -----------------------------------------------------
#
# Repeats the validation using sex-specific developmental rate tables and
# sex-specific observed medians from Experiments 3 and 4. 

input_female <- read_csv("./01_dev_rate_estimation/dev_rate_summary_by_sex.csv") %>%
  filter(sex == "Female") %>%
  select(temperature, medianDays, medianDevRate) %>%
  rename(medianEmergence = medianDays, devRate = medianDevRate)

input_male <- read_csv("./01_dev_rate_estimation/dev_rate_summary_by_sex.csv") %>%
  filter(sex == "Male") %>%
  select(temperature, medianDays, medianDevRate) %>%
  rename(medianEmergence = medianDays, devRate = medianDevRate)

validation_observed_sex <- raw_data %>%
  filter(
    experiment %in% c(3, 4),
    !is.na(daysToEmergence),
    sex %in% c("M", "F")
  ) %>%
  mutate(sex = recode(sex, "M" = "Male", "F" = "Female")) %>%
  group_by(treatment, sex) %>%
  summarise(
    n_obs = n(),
    obs_median = median(daysToEmergence),
    obs_sd = sd(daysToEmergence),
    .groups = "drop"
  ) %>%
  mutate(is_control = !str_detect(treatment, "_"))

run_sex_validation <- function(input_data, observed_data) {
  
  observed_data %>%
    filter(!is_control) %>%
    mutate(
      parts = str_split(treatment, "_"),
      startTemp = map_int(parts, ~ as.integer(.x[1])),
      weeksFirstThermo = map_int(parts, ~ as.integer(.x[2])),
      transferDownTemp = map_int(parts, ~ as.integer(.x[3])),
      weeksCryo = map_int(parts, ~ as.integer(.x[4])),
      transferUpTemp = map_int(parts, ~ as.integer(.x[5])),
      d1 = weeksFirstThermo * 7L,
      d2 = weeksCryo * 7L
    ) %>%
    rowwise() %>%
    mutate(
      r1 = input_data %>% filter(temperature == startTemp) %>% pull(devRate),
      r2 = input_data %>% filter(temperature == transferDownTemp) %>% pull(devRate),
      r3 = input_data %>% filter(temperature == transferUpTemp) %>% pull(devRate),
      
      dev1 = d1 * r1,
      dev2 = d2 * r2,
      total_dev = dev1 + dev2,
      rem_dev = 1 - total_dev,
      days_final = round(rem_dev / r3, digits = 0),
      pred_days = d1 + d2 + days_final,
      residual = pred_days - obs_median
    ) %>%
    ungroup() %>%
    select(treatment, sex, transferDownTemp, weeksFirstThermo, weeksCryo,
           n_obs, obs_median, obs_sd,
           pred_days, residual, total_dev)
}

preds_female_all <- run_sex_validation(
  input_female,
  filter(validation_observed_sex, sex == "Female")
)

preds_male_all <- run_sex_validation(
  input_male,
  filter(validation_observed_sex, sex == "Male")
)

# Exclude protocol violations consistent with the combined-sex analysis above.
preds_female <- preds_female_all %>% filter(total_dev < 1)
preds_male <- preds_male_all %>% filter(total_dev < 1)

write_csv(
  bind_rows(preds_female_all, preds_male_all),
  "./05_three_phase_model_validation/three_phase_model_validation_predictions_by_sex_all.csv"
)
write_csv(
  bind_rows(preds_female, preds_male),
  "./05_three_phase_model_validation/three_phase_model_validation_predictions_by_sex.csv"
)

make_sex_validation_panel <- function(preds, axis_limits, ellipse_expand = 5) {
  
  ggplot(preds,
         aes(x = obs_median,
             y = pred_days,
             color = factor(transferDownTemp),
             shape = interaction(factor(weeksFirstThermo),
                                 factor(weeksCryo)))) +
    geom_mark_ellipse(
      data = filter(preds, weeksFirstThermo == 1 & weeksCryo == 2),
      aes(x = obs_median, y = pred_days, label = "1 wk / 2 wk cryophase"),
      color = "grey60",
      fill = NA,
      label.fill = NA,
      label.colour = "grey60",
      con.colour = "grey60",
      label.fontsize = 8,
      label.buffer = unit(2, "mm"),
      con.type = "elbow",
      expand = unit(ellipse_expand, "mm"),
      inherit.aes = FALSE
    ) +
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", linewidth = 0.5, color = "grey50") +
    geom_errorbarh(
      aes(xmin = obs_median - obs_sd,
          xmax = obs_median + obs_sd),
      height = 0.4, linewidth = 0.5
    ) +
    geom_point(size = 3) +
    scale_color_jco(name = "Transfer Temp. (\u00b0C)") +
    scale_shape_manual(
      name = "Protocol (thermo/cryo weeks)",
      values = c("1.1" = 1, "1.2" = 2, "2.1" = 16, "2.2" = 17),
      labels = c("1.1" = "1 wk / 1 wk", "1.2" = "1 wk / 2 wk",
                 "2.1" = "2 wk / 1 wk", "2.2" = "2 wk / 2 wk")
    ) +
    scale_x_continuous(limits = axis_limits,
                       breaks = seq(axis_limits[1], axis_limits[2], 5)) +
    scale_y_continuous(limits = axis_limits,
                       breaks = seq(axis_limits[1], axis_limits[2], 5)) +
    coord_equal(clip = "off") +
    alex_theme +
    theme(legend.position = c(0.22, 0.72),
          legend.background = element_rect(colour = "black", linewidth = 0.25),
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 8),
          legend.spacing.y = unit(0.1, "cm")) +
    xlab("Observed days to emergence (median \u00b1 SD)") +
    ylab("Predicted days to emergence") +
    ggtitle(NULL)
}

sex_axis_range <- range(c(
  preds_female$obs_median - preds_female$obs_sd,
  preds_female$obs_median + preds_female$obs_sd,
  preds_female$pred_days,
  preds_male$obs_median - preds_male$obs_sd,
  preds_male$obs_median + preds_male$obs_sd,
  preds_male$pred_days
))

sex_axis_limits <- c(floor(sex_axis_range[1]   / 5) * 5,
                     ceiling(sex_axis_range[2] / 5) * 5)

panel_female <- make_sex_validation_panel(preds_female, sex_axis_limits)
panel_male <- make_sex_validation_panel(preds_male, sex_axis_limits, ellipse_expand = 10)

sex_validation_figure <- plot_grid(
  panel_female, panel_male,
  ncol = 1,
  labels = c("A.", "B."),
  label_size = 12
)

ggsave(
  plot = sex_validation_figure,
  filename = "./05_three_phase_model_validation/three_phase_model_validation_plot_by_sex.png",
  width = 130,
  height = 280,
  units = "mm",
  dpi = 300
)

ggsave(
  plot = sex_validation_figure,
  filename = "./05_three_phase_model_validation/three_phase_model_validation_plot_by_sex.pdf",
  width = 130,
  height = 280,
  units = "mm"
)

# Emergence Window Validation (Table S6) --------------------------------------
#
# Applies percentile-specific developmental rates from the quantile regression
# models to the three-phase framework to predict 10th, 50th, and 90th
# percentile emergence times for each validation treatment. Quantile rates
# are derived from the prediction lines saved by script 01 by taking 1/fit
# at each temperature.

load_quantile_rates <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    filter(temperature %in% c(21, 23, 25, 27, 29, 31)) %>%
    mutate(
      tau = case_when(
        tau == "10th percentile" ~ "p10",
        tau == "50th percentile" ~ "p50",
        tau == "90th percentile" ~ "p90"
      ),
      rate = 1 / fit
    ) %>%
    select(temperature, tau, rate)
}

qrates_combined <- load_quantile_rates("./01_dev_rate_estimation/quantile_predictions_combined.csv")
qrates_female <- load_quantile_rates("./01_dev_rate_estimation/quantile_predictions_female.csv")
qrates_male <- load_quantile_rates("./01_dev_rate_estimation/quantile_predictions_male.csv")

# Observed percentiles from individual-level validation data
validation_percentiles <- raw_data %>%
  filter(experiment %in% c(3, 4), !is.na(daysToEmergence), sex %in% c("M", "F")) %>%
  mutate(sex = recode(sex, "M" = "Male", "F" = "Female")) %>%
  filter(str_detect(treatment, "_")) %>%
  group_by(treatment) %>%
  summarise(
    n_obs = n(),
    obs_p10 = quantile(daysToEmergence, 0.10),
    obs_p50 = median(daysToEmergence),
    obs_p90 = quantile(daysToEmergence, 0.90),
    .groups = "drop"
  )

validation_percentiles_sex <- raw_data %>%
  filter(experiment %in% c(3, 4), !is.na(daysToEmergence), sex %in% c("M", "F")) %>%
  mutate(sex = recode(sex, "M" = "Male", "F" = "Female")) %>%
  filter(str_detect(treatment, "_")) %>%
  group_by(treatment, sex) %>%
  summarise(
    n_obs = n(),
    obs_p10 = quantile(daysToEmergence, 0.10),
    obs_p50 = median(daysToEmergence),
    obs_p90 = quantile(daysToEmergence, 0.90),
    .groups = "drop"
  )

predict_window <- function(observed_df, qrates) {
  observed_df %>%
    mutate(
      parts = str_split(treatment, "_"),
      startTemp = map_int(parts, ~ as.integer(.x[1])),
      weeksFirstThermo = map_int(parts, ~ as.integer(.x[2])),
      transferDownTemp = map_int(parts, ~ as.integer(.x[3])),
      weeksCryo = map_int(parts, ~ as.integer(.x[4])),
      transferUpTemp = map_int(parts, ~ as.integer(.x[5])),
      d1 = weeksFirstThermo * 7L,
      d2 = weeksCryo * 7L
    ) %>%
    rowwise() %>%
    mutate(
      across(c(pred_p10, pred_p50, pred_p90), ~ {
        tau_label <- cur_column() %>% str_replace("pred_", "")
        r1 <- qrates %>% filter(temperature == startTemp, tau == tau_label) %>% pull(rate)
        r2 <- qrates %>% filter(temperature == transferDownTemp, tau == tau_label) %>% pull(rate)
        r3 <- qrates %>% filter(temperature == transferUpTemp, tau == tau_label) %>% pull(rate)
        dev1 <- d1 * r1
        dev2 <- d2 * r2
        total <- dev1 + dev2
        rem <- 1 - total
        days_fin <- round(rem / r3, digits = 0)
        as.integer(d1 + d2 + days_fin)
      }, .names = "{.col}"),
      total_dev = d1 * (qrates %>% filter(temperature == startTemp, tau == "p50") %>% pull(rate)) +
        d2 * (qrates %>% filter(temperature == transferDownTemp, tau == "p50") %>% pull(rate))
    ) %>%
    ungroup() %>%
    mutate(excluded = total_dev >= 1) %>%
    select(treatment, transferDownTemp, weeksFirstThermo, weeksCryo,
           n_obs, obs_p10, obs_p50, obs_p90,
           pred_p10, pred_p50, pred_p90, excluded)
}

# Add placeholder columns before rowwise mutate
window_preds_combined <- validation_percentiles %>%
  mutate(pred_p10 = NA_integer_, pred_p50 = NA_integer_, pred_p90 = NA_integer_) %>%
  predict_window(qrates_combined)

window_preds_female <- validation_percentiles_sex %>%
  filter(sex == "Female") %>%
  mutate(pred_p10 = NA_integer_, pred_p50 = NA_integer_, pred_p90 = NA_integer_) %>%
  predict_window(qrates_female) %>%
  mutate(sex = "Female")

window_preds_male <- validation_percentiles_sex %>%
  filter(sex == "Male") %>%
  mutate(pred_p10 = NA_integer_, pred_p50 = NA_integer_, pred_p90 = NA_integer_) %>%
  predict_window(qrates_male) %>%
  mutate(sex = "Male")

compute_window_rmse <- function(preds) {
  preds_valid <- preds %>% filter(!excluded)
  tibble(
    quantile = c("10th percentile", "50th percentile", "90th percentile"),
    n = nrow(preds_valid),
    rmse = c(
      sqrt(mean((preds_valid$pred_p10 - preds_valid$obs_p10)^2)),
      sqrt(mean((preds_valid$pred_p50 - preds_valid$obs_p50)^2)),
      sqrt(mean((preds_valid$pred_p90 - preds_valid$obs_p90)^2))
    ),
    mae = c(
      mean(abs(preds_valid$pred_p10 - preds_valid$obs_p10)),
      mean(abs(preds_valid$pred_p50 - preds_valid$obs_p50)),
      mean(abs(preds_valid$pred_p90 - preds_valid$obs_p90))
    )
  )
}

window_accuracy_combined <- compute_window_rmse(window_preds_combined) %>% mutate(model = "Combined")
window_accuracy_female <- compute_window_rmse(window_preds_female) %>% mutate(model = "Female")
window_accuracy_male <- compute_window_rmse(window_preds_male) %>% mutate(model = "Male")

window_accuracy <- bind_rows(window_accuracy_combined, window_accuracy_female, window_accuracy_male) %>%
  select(model, quantile, n, rmse, mae)

write_csv(window_preds_combined, "./05_three_phase_model_validation/three_phase_model_validation_window_predictions.csv")
write_csv(bind_rows(window_preds_female, window_preds_male), "./05_three_phase_model_validation/three_phase_model_validation_window_predictions_by_sex.csv")
write_csv(window_accuracy, "./05_three_phase_model_validation/three_phase_model_validation_window_accuracy.csv")

# Emergence Window Validation Plots ------------------------------------------
#
# Three plot types: (1) combined scatter with shape = percentile and
# color = transfer temp; (2) faceted by treatment with rug showing individual-
# level observed distribution; (3) dumbbell plot with observed and predicted
# connected by segment, faceted by percentile.

window_long <- window_preds_combined %>%
  filter(!excluded) %>%
  mutate(
    protocol = paste0(weeksFirstThermo, "wk / ", weeksCryo, "wk"),
    treatment_label = paste0(transferDownTemp, "°C\n", protocol),
    protocol = factor(protocol,
                      levels = c("1wk / 1wk", "1wk / 2wk",
                                 "2wk / 1wk", "2wk / 2wk"))
  ) %>%
  pivot_longer(
    cols = c(obs_p10, obs_p50, obs_p90, pred_p10, pred_p50, pred_p90),
    names_to = c("type", "quantile"),
    names_pattern = "(obs|pred)_(p\\d+)"
  ) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  mutate(
    quantile = factor(quantile,
                      levels = c("p10", "p50", "p90"),
                      labels = c("10th percentile", "50th percentile", "90th percentile"))
  )

window_axis_range <- range(c(window_long$obs, window_long$pred), na.rm = TRUE)
window_axis_limits <- c(floor(window_axis_range[1]   / 5) * 5,
                        ceiling(window_axis_range[2] / 5) * 5)

# Plot 1: Combined scatter ----------------------------------------------------

window_validation_plot <- ggplot(
  window_long,
  aes(x = obs,
      y = pred,
      color = factor(transferDownTemp),
      shape = quantile)
) +
  geom_mark_ellipse(
    data = filter(window_long, weeksFirstThermo == 1 & weeksCryo == 2),
    aes(x = obs, y = pred, label = "1 wk / 2 wk cryophase"),
    color = "grey60",
    fill = NA,
    label.fill = NA,
    label.colour = "grey60",
    con.colour = "grey60",
    label.fontsize = 8,
    label.buffer = unit(2, "mm"),
    con.type = "elbow",
    expand = unit(3, "mm"),
    inherit.aes = FALSE
  ) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", linewidth = 0.5, color = "grey50") +
  geom_point(size = 3) +
  scale_color_jco(name = "Transfer Temp. (°C)") +
  scale_shape_manual(
    name = "Percentile",
    values = c("10th percentile" = 1,
               "50th percentile" = 16,
               "90th percentile" = 2)
  ) +
  scale_x_continuous(limits = window_axis_limits,
                     breaks = seq(window_axis_limits[1], window_axis_limits[2], 5)) +
  scale_y_continuous(limits = window_axis_limits,
                     breaks = seq(window_axis_limits[1], window_axis_limits[2], 5)) +
  coord_equal() +
  alex_theme +
  theme(legend.position = c(0.22, 0.72),
        legend.background = element_rect(colour = "black", linewidth = 0.25),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        legend.spacing.y = unit(0.1, "cm")) +
  xlab("Observed days to emergence") +
  ylab("Predicted days to emergence") +
  ggtitle(NULL)

ggsave(
  plot = window_validation_plot,
  filename = "./05_three_phase_model_validation/three_phase_model_validation_window_plot.png",
  width = 130, height = 130, units = "mm", dpi = 300
)
ggsave(
  plot = window_validation_plot,
  filename = "./05_three_phase_model_validation/three_phase_model_validation_window_plot.pdf",
  width = 130, height = 130, units = "mm"
)

# Window accuracy summary -----------------------------------------------------

print(window_accuracy)

# Plot 2: Facet by treatment, with rug ----------------------------------------

window_individuals <- raw_data %>%
  filter(
    experiment %in% c(3, 4),
    !is.na(daysToEmergence),
    str_detect(treatment, "_"),
    treatment != "29_2_25_2_29"
  ) %>%
  mutate(
    parts = str_split(treatment, "_"),
    weeksFirstThermo = map_int(parts, ~ as.integer(.x[2])),
    transferDownTemp = map_int(parts, ~ as.integer(.x[3])),
    weeksCryo = map_int(parts, ~ as.integer(.x[4])),
    protocol = paste0(weeksFirstThermo, "wk / ", weeksCryo, "wk"),
    treatment_label = paste0(transferDownTemp, "°C\n", protocol)
  )

window_axis_range_facet <- range(c(window_long$obs, window_long$pred), na.rm = TRUE)
window_axis_limits_facet <- c(floor(window_axis_range_facet[1]   / 5) * 5,
                              ceiling(window_axis_range_facet[2] / 5) * 5)

window_plot_facet <- ggplot(
  window_long,
  aes(x = obs, y = pred, color = quantile, shape = quantile)
) +
  geom_rug(
    data = window_individuals,
    aes(x = daysToEmergence),
    color = "grey60",
    linewidth = 0.3,
    alpha = 0.5,
    sides = "b",
    inherit.aes = FALSE
  ) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", linewidth = 0.5, color = "grey50") +
  geom_point(size = 2.5) +
  scale_color_jco(name = "Percentile") +
  scale_shape_manual(
    name = "Percentile",
    values = c("10th percentile" = 1,
               "50th percentile" = 16,
               "90th percentile" = 2)
  ) +
  scale_x_continuous(limits = window_axis_limits_facet,
                     breaks = seq(window_axis_limits_facet[1],
                                  window_axis_limits_facet[2], 10)) +
  scale_y_continuous(limits = window_axis_limits_facet,
                     breaks = seq(window_axis_limits_facet[1],
                                  window_axis_limits_facet[2], 10)) +
  coord_equal() +
  facet_wrap(~ treatment_label, ncol = 4) +
  alex_theme +
  theme(
    strip.background = element_rect(fill = "grey92", colour = "black",
                                    linewidth = 0.5),
    strip.text = element_text(size = 9),
    legend.position = "bottom",
    legend.background = element_rect(colour = "black", linewidth = 0.25),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  ) +
  xlab("Observed days to emergence") +
  ylab("Predicted days to emergence")

ggsave(
  plot = window_plot_facet,
  filename = "./05_three_phase_model_validation/three_phase_model_validation_window_plot_facet.png",
  width = 220, height = 160, units = "mm", dpi = 300
)
ggsave(
  plot = window_plot_facet,
  filename = "./05_three_phase_model_validation/three_phase_model_validation_window_plot_facet.pdf",
  width = 220, height = 160, units = "mm"
)

# Plot 3: Dumbbell by treatment, faceted by percentile ------------------------

window_plot_dumbbell <- ggplot(
  window_long,
  aes(y = treatment_label, color = factor(transferDownTemp))
) +
  geom_segment(
    aes(x = obs, xend = pred, yend = treatment_label),
    linewidth = 0.8, alpha = 0.6
  ) +
  geom_point(aes(x = obs, shape = "Observed"), size = 3) +
  geom_point(aes(x = pred, shape = "Predicted"), size = 3) +
  scale_color_jco(name = "Transfer Temp. (°C)") +
  scale_shape_manual(
    name = NULL,
    values = c("Observed" = 16, "Predicted" = 1)
  ) +
  scale_x_continuous(breaks = seq(20, 60, 10)) +
  facet_wrap(~ quantile, ncol = 3) +
  alex_theme +
  theme(
    strip.background = element_rect(fill = "grey92", colour = "black",
                                    linewidth = 0.5),
    strip.text = element_text(size = 9),
    axis.text.y = element_text(size = 7),
    legend.position = "bottom",
    legend.background = element_rect(colour = "black", linewidth = 0.25)
  ) +
  xlab("Days to emergence") +
  ylab(NULL)

ggsave(
  plot = window_plot_dumbbell,
  filename = "./05_three_phase_model_validation/three_phase_model_validation_window_plot_dumbbell.png",
  width = 220, height = 100, units = "mm", dpi = 300
)
ggsave(
  plot = window_plot_dumbbell,
  filename = "./05_three_phase_model_validation/three_phase_model_validation_window_plot_dumbbell.pdf",
  width = 220, height = 100, units = "mm"
)

# Sex-Specific Emergence Window Validation Plots ------------------------------
#
# Repeats window plots using sex-specific quantile rate objects.

make_window_long_sex <- function(preds, sex_label) {
  preds %>%
    filter(!excluded) %>%
    mutate(
      sex = sex_label,
      protocol = paste0(weeksFirstThermo, "wk / ", weeksCryo, "wk"),
      treatment_label = paste0(transferDownTemp, "°C\n", protocol),
      protocol = factor(protocol,
                        levels = c("1wk / 1wk", "1wk / 2wk",
                                   "2wk / 1wk", "2wk / 2wk"))
    ) %>%
    pivot_longer(
      cols = c(obs_p10, obs_p50, obs_p90, pred_p10, pred_p50, pred_p90),
      names_to = c("type", "quantile"),
      names_pattern = "(obs|pred)_(p\\d+)"
    ) %>%
    pivot_wider(names_from = type, values_from = value) %>%
    mutate(
      quantile = factor(quantile,
                        levels = c("p10", "p50", "p90"),
                        labels = c("10th percentile", "50th percentile", "90th percentile"))
    )
}

window_long_female <- make_window_long_sex(window_preds_female, "Female")
window_long_male <- make_window_long_sex(window_preds_male, "Male")
window_long_sex <- bind_rows(window_long_female, window_long_male)

make_window_facet_sex <- function(window_long_data, sex_label) {
  ax_range <- range(c(window_long_data$obs, window_long_data$pred), na.rm = TRUE)
  ax_limits <- c(floor(ax_range[1]   / 5) * 5,
                 ceiling(ax_range[2] / 5) * 5)
  
  ggplot(window_long_data,
         aes(x = obs, y = pred, color = quantile, shape = quantile)) +
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", linewidth = 0.5, color = "grey50") +
    geom_point(size = 2.5) +
    scale_color_jco(name = "Percentile") +
    scale_shape_manual(
      name = "Percentile",
      values = c("10th percentile" = 1,
                 "50th percentile" = 16,
                 "90th percentile" = 2)
    ) +
    scale_x_continuous(limits = ax_limits,
                       breaks = seq(ax_limits[1], ax_limits[2], 10)) +
    scale_y_continuous(limits = ax_limits,
                       breaks = seq(ax_limits[1], ax_limits[2], 10)) +
    coord_equal() +
    facet_wrap(~ treatment_label, ncol = 4) +
    alex_theme +
    theme(
      strip.background = element_rect(fill = "grey92", colour = "black",
                                      linewidth = 0.5),
      strip.text = element_text(size = 9),
      legend.position = "bottom",
      legend.background = element_rect(colour = "black", linewidth = 0.25),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    ) +
    xlab("Observed days to emergence") +
    ylab("Predicted days to emergence") +
    ggtitle(sex_label)
}

make_window_dumbbell_sex <- function(window_long_data, sex_label) {
  ggplot(window_long_data,
         aes(y = treatment_label, color = factor(transferDownTemp))) +
    geom_segment(
      aes(x = obs, xend = pred, yend = treatment_label),
      linewidth = 0.8, alpha = 0.6
    ) +
    geom_point(aes(x = obs, shape = "Observed"), size = 3) +
    geom_point(aes(x = pred, shape = "Predicted"), size = 3) +
    scale_color_jco(name = "Transfer Temp. (°C)") +
    scale_shape_manual(
      name = NULL,
      values = c("Observed" = 16, "Predicted" = 1)
    ) +
    scale_x_continuous(limits = c(20, 60), breaks = seq(20, 60, 10)) +
    facet_wrap(~ quantile, ncol = 3) +
    alex_theme +
    theme(
      strip.background = element_rect(fill = "grey92", colour = "black",
                                      linewidth = 0.5),
      strip.text = element_text(size = 9),
      axis.text.y = element_text(size = 7),
      legend.position = "bottom",
      legend.background = element_rect(colour = "black", linewidth = 0.25)
    ) +
    xlab("Days to emergence") +
    ylab(NULL) +
    ggtitle(sex_label)
}

facet_female <- make_window_facet_sex(window_long_female, "Female")
facet_male <- make_window_facet_sex(window_long_male, "Male")
dumbbell_female <- make_window_dumbbell_sex(window_long_female, "Female")
dumbbell_male <- make_window_dumbbell_sex(window_long_male, "Male")

sex_window_facet <- plot_grid(
  facet_female, facet_male,
  ncol = 1, labels = c("A.", "B."), label_size = 12
)

sex_window_dumbbell <- plot_grid(
  dumbbell_female, dumbbell_male,
  ncol = 1, labels = c("A.", "B."), label_size = 12
)

ggsave(
  plot = sex_window_facet,
  filename = "./05_three_phase_model_validation/three_phase_model_validation_window_plot_facet_by_sex.png",
  width = 220, height = 320, units = "mm", dpi = 300
)
ggsave(
  plot = sex_window_facet,
  filename = "./05_three_phase_model_validation/three_phase_model_validation_window_plot_facet_by_sex.pdf",
  width = 220, height = 320, units = "mm"
)
ggsave(
  plot = sex_window_dumbbell,
  filename = "./05_three_phase_model_validation/three_phase_model_validation_window_plot_dumbbell_by_sex.png",
  width = 220, height = 320, units = "mm", dpi = 300
)
ggsave(
  plot = sex_window_dumbbell,
  filename = "./05_three_phase_model_validation/three_phase_model_validation_window_plot_dumbbell_by_sex.pdf",
  width = 220, height = 320, units = "mm"
)

# Sex-specific window accuracy summary ----------------------------------------

window_accuracy_sex <- window_long_sex %>%
  group_by(sex, quantile) %>%
  summarise(
    n = n(),
    rmse = sqrt(mean((pred - obs)^2, na.rm = TRUE)),
    mae = mean(abs(pred - obs), na.rm = TRUE),
    .groups = "drop"
  )

print(window_accuracy_sex)

# Table S5: Merged Interrupted Development Model Validation ------------------
#
# Stacks combined and sex-specific predictions. The excluded flag marks
# treatments where total_dev >= 1.

table_s5 <- bind_rows(
  model_predictions %>%
    mutate(sex = "Combined", excluded = total_dev >= 1),
  bind_rows(preds_female_all, preds_male_all) %>%
    mutate(excluded = total_dev >= 1)
) %>%
  arrange(treatment, factor(sex, levels = c("Combined", "Female", "Male"))) %>%
  select(treatment, sex, transferDownTemp, weeksFirstThermo, weeksCryo,
         n_obs, obs_median, obs_sd, pred_days, residual, total_dev, excluded)

write_csv(table_s5, "./05_three_phase_model_validation/Table_S5_int_dev_model_performance.csv")

# Table S6: Merged Emergence Window Validation --------------------------------
#
# Same structure as S5 for window predictions.

table_s6 <- bind_rows(
  window_preds_combined %>% mutate(sex = "Combined"),
  bind_rows(window_preds_female, window_preds_male)
) %>%
  arrange(treatment, factor(sex, levels = c("Combined", "Female", "Male"))) %>%
  select(treatment, sex, transferDownTemp, weeksFirstThermo, weeksCryo,
         n_obs, obs_p10, obs_p50, obs_p90, pred_p10, pred_p50, pred_p90, excluded)

write_csv(table_s6, "./05_three_phase_model_validation/Table_S6_validation_window_predictions.csv")

# Session Info ----------------------------------------------------------------

writeLines(capture.output(sessionInfo()), "./05_three_phase_model_validation/05_session_info.txt")