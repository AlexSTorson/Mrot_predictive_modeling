################################################################################
#
# Title: Developmental Rate Calculation from Raw Emergence Data
#
# Author: Alex Torson
# Institution: USDA-ARS
# Email: Alex.Torson@usda.gov
#
# Description: Estimates developmental rates and degree-day parameters from
#              constant-temperature emergence data. Fits linear degree-day and
#              quantile regression models; exports summary tables used by all
#              downstream scripts.
#
# Dependencies: none
#
# Input files:
#   - degree_day_dataset.csv
#
# Output files:
#   - dev_rate_summary_combined.csv
#   - dev_rate_summary_by_sex.csv
#   - dev_rate_plot_avg.png/pdf
#   - dev_rate_density_by_temperature.png/pdf
#   - dev_rate_by_sex_combined.png/pdf
#   - emergence_window_quantile_plot.png/pdf
#   - emergence_window_quantile_plot_by_sex.png/pdf
#   - emergence_window_summary.csv
#   - emergence_window_summary_by_sex.csv
#   - Table_S1_Emergence_Rates.csv
#   - Table_S2_dd_model_parameters.csv
#   - Table_S3_quantile_regression_parameters.csv
#   - Table_S4_emergence_window_summary.csv
#   - quantile_predictions_combined.csv
#   - quantile_predictions_female.csv
#   - quantile_predictions_male.csv
#   - 01_session_info.txt
#
################################################################################

# Package Loading -------------------------------------------------------------

library(tidyverse)
library(ggsci)
library(patchwork)
library(cowplot)
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
  "/Users/alextorson/Library/CloudStorage/OneDrive-USDA/Torson_Lab/Mrot_Predictive_Modeling/"
)

if (!dir.exists("./01_dev_rate_estimation/")) {
  dir.create("./01_dev_rate_estimation/", recursive = TRUE)
}

# Data Loading and Cleaning ---------------------------------------------------

raw_data <- read_csv("./degree_day_dataset.csv")

# Retain only constant-temperature model development experiments (1 and 2).
# Remove individuals with missing emergence dates or unrecorded sex.
emergence_data <- raw_data %>%
  filter(
    experiment %in% c(1, 2),
    !is.na(daysToEmergence),
    sex %in% c("M", "F")
  ) %>%
  mutate(
    treatment = as.integer(treatment),
    devRate = 1 / daysToEmergence,
    sex = recode(sex, "M" = "Male", "F" = "Female")
  ) %>%
  rename(temperature = treatment)

# Summary of sample sizes after cleaning
emergence_data %>%
  count(experiment, temperature, sex) %>%
  print(n = Inf)

# Temperature-level summary: Both sexes combined ------------------------------

devrate_summary_combined <- emergence_data %>%
  group_by(temperature) %>%
  summarise(
    n = n(),
    medianDays = median(daysToEmergence),
    meanDays = mean(daysToEmergence),
    medianDevRate = median(devRate),
    meanDevRate = mean(devRate),
    seDevRate = sd(devRate) / sqrt(n()),
    sdDevRate = sd(devRate),
    .groups = "drop"
  )

devrate_summary_combined

# Temperature-Level Summary: By Sex -------------------------------------------

devrate_summary_by_sex <- emergence_data %>%
  group_by(temperature, sex) %>%
  summarise(
    n = n(),
    medianDays = median(daysToEmergence),
    meanDays = mean(daysToEmergence),
    medianDevRate = median(devRate),
    meanDevRate = mean(devRate),
    seDevRate = sd(devRate) / sqrt(n()),
    sdDevRate = sd(devRate),
    .groups = "drop"
  )

devrate_summary_by_sex

# Linear Model: devRate ~ Temperature (Combined) ------------------------------

lm_combined <- lm(medianDevRate ~ temperature, data = devrate_summary_combined)
summary(lm_combined)

dev_threshold <- -coef(lm_combined)["(Intercept)"] / coef(lm_combined)["temperature"]
thermal_constant <- 1 / coef(lm_combined)["temperature"]

# Linear Models: devRate ~ Temperature (By Sex) -------------------------------

lm_female <- lm(medianDevRate ~ temperature,
                data = filter(devrate_summary_by_sex, sex == "Female"))
lm_male <- lm(medianDevRate ~ temperature,
              data = filter(devrate_summary_by_sex, sex == "Male"))

dev_threshold_female <- -coef(lm_female)["(Intercept)"] / coef(lm_female)["temperature"]
thermal_constant_female <- 1 / coef(lm_female)["temperature"]
dev_threshold_male <- -coef(lm_male)["(Intercept)"] / coef(lm_male)["temperature"]
thermal_constant_male <- 1 / coef(lm_male)["temperature"]

# Table S2: Degree-Day Model Parameters with Propagated SE -------------------
# LDT = -b0/b1, DD = 1/b1. SE propagated via delta method.

compute_dd_params <- function(lm_obj, model_label) {
  vc <- vcov(lm_obj)
  b0 <- coef(lm_obj)["(Intercept)"]
  b1 <- coef(lm_obj)["temperature"]
  ldt <- -b0 / b1
  dd <- 1 / b1
  var_ldt <- (1/b1)^2 * vc["(Intercept)","(Intercept)"] +
    (b0/b1^2)^2 * vc["temperature","temperature"] -
    2 * (1/b1) * (b0/b1^2) * vc["(Intercept)","temperature"]
  var_dd <- (1/b1^2)^2 * vc["temperature","temperature"]
  tibble(
    model = model_label,
    LDT = round(unname(ldt), 2),
    LDT_SE = round(sqrt(unname(var_ldt)), 2),
    DD = round(unname(dd), 1),
    DD_SE = round(sqrt(unname(var_dd)), 1),
    R2 = round(summary(lm_obj)$r.squared, 3),
    F_stat = round(summary(lm_obj)$fstatistic["value"], 1),
    df1 = summary(lm_obj)$fstatistic["numdf"],
    df2 = summary(lm_obj)$fstatistic["dendf"],
    p_value = formatC(pf(summary(lm_obj)$fstatistic["value"],
                         summary(lm_obj)$fstatistic["numdf"],
                         summary(lm_obj)$fstatistic["dendf"],
                         lower.tail = FALSE),
                      format = "e", digits = 2)
  )
}

table_s2 <- bind_rows(
  compute_dd_params(lm_combined, "Combined"),
  compute_dd_params(lm_female, "Female"),
  compute_dd_params(lm_male, "Male")
)

print(table_s2)
write_csv(table_s2, "./01_dev_rate_estimation/Table_S2_dd_model_parameters.csv")

# Plot: Individual DevRates with Linear Fit (Combined) -----------------------

fit_line <- tibble(
  temperature = seq(min(emergence_data$temperature),
                    max(emergence_data$temperature), 0.1)
) %>%
  mutate(medianDevRate = predict(lm_combined, newdata = .))

(individual_plot <- ggplot() +
    geom_jitter(
      data = emergence_data,
      aes(x = temperature, y = devRate, color = sex),
      width = 0.3, size = 0.8, alpha = 0.4
    ) +
    geom_line(
      data = fit_line,
      aes(x = temperature, y = medianDevRate),
      linewidth = 0.8, linetype = "dashed"
    ) +
    geom_errorbar(
      data = devrate_summary_combined,
      aes(x = temperature,
          ymin = medianDevRate - sdDevRate,
          ymax = medianDevRate + sdDevRate),
      width = 0.4, linewidth = 0.5
    ) +
    geom_point(
      data = devrate_summary_combined,
      aes(x = temperature, y = medianDevRate),
      size = 2, shape = 21, fill = "white", color = "black", stroke = 0.8
    ) +
    scale_color_jco() +
    scale_x_continuous(breaks = seq(21, 31, 2)) +
    alex_theme +
    theme(legend.position = c(0.2, 0.85),
          legend.background = element_rect(colour = "black", linewidth = 0.25)) +
    guides(color = guide_legend(override.aes = list(size = 2))) +
    xlab("Temperature (°C)") +
    ylab("Developmental rate (day⁻¹)") +
    ggtitle(NULL)
)

ggsave(plot = individual_plot,
       filename = "./01_dev_rate_estimation/dev_rate_plot_avg.png",
       width = 90, height = 90, units = "mm", dpi = 300)
ggsave(plot = individual_plot,
       filename = "./01_dev_rate_estimation/dev_rate_plot_avg.pdf",
       width = 90, height = 90, units = "mm")

# Plot: Emergence Density by Temperature and Sex ------------------------------

(density_plot <- ggplot(emergence_data,
                        aes(x = daysToEmergence, fill = sex, color = sex)) +
   geom_density(alpha = 0.4, linewidth = 0.5) +
   scale_fill_jco() +
   scale_color_jco() +
   facet_wrap(~ temperature,
              labeller = labeller(temperature = function(x) paste0(x, "C"))) +
   alex_theme +
   theme(legend.position = "bottom",
         legend.background = element_rect(colour = "black", linewidth = 0.25),
         strip.background = element_rect(fill = "grey92", colour = "black",
                                         linewidth = 0.5),
         strip.text = element_text(size = 9)) +
   xlab("Days to emergence") +
   ylab("Density") +
   ggtitle(NULL)
)

ggsave(plot = density_plot,
       filename = "./01_dev_rate_estimation/dev_rate_density_by_temperature.png",
       width = 180, height = 120, units = "mm", dpi = 300)
ggsave(plot = density_plot,
       filename = "./01_dev_rate_estimation/dev_rate_density_by_temperature.pdf",
       width = 180, height = 120, units = "mm")


# Sex-Specific Figures ----------------------------------------------------

fit_line_by_sex <- bind_rows(
  tibble(temperature = seq(21, 31, 0.1), sex = "Female") %>%
    mutate(medianDevRate = predict(lm_female, newdata = .)),
  tibble(temperature = seq(21, 31, 0.1), sex = "Male") %>%
    mutate(medianDevRate = predict(lm_male, newdata = .))
)

# Panel A: Sex-specific developmental rates
(sex_devrate_plot <- ggplot() +
    geom_jitter(
      data = emergence_data,
      aes(x = temperature, y = devRate, color = sex),
      width = 0.3, size = 0.8, alpha = 0.3
    ) +
    geom_line(
      data = fit_line_by_sex,
      aes(x = temperature, y = medianDevRate, color = sex),
      linewidth = 0.8, linetype = "dashed"
    ) +
    geom_errorbar(
      data = devrate_summary_by_sex,
      aes(x = temperature, ymin = medianDevRate - sdDevRate,
          ymax = medianDevRate + sdDevRate,
          group = sex),
      color = "black",
      width = 0.4, linewidth = 0.5,
      position = position_dodge(width = 0.5)
    ) +
    geom_point(
      data = devrate_summary_by_sex,
      aes(x = temperature, y = medianDevRate, fill = sex),
      size = 2, shape = 21, color = "black", stroke = 0.8,
      position = position_dodge(width = 0.5)
    ) +
    scale_color_jco() +
    scale_fill_jco() +
    scale_x_continuous(breaks = seq(21, 31, 2)) +
    alex_theme +
    theme(legend.position = c(0.2, 0.85),
          legend.background = element_rect(colour = "black", linewidth = 0.25)) +
    guides(color = guide_legend(override.aes = list(size = 2, linetype = "solid")),
           fill = "none") +
    xlab("Temperature (°C)") +
    ylab("Developmental rate (day⁻¹)") +
    ggtitle(NULL)
)

# Panel B: Sex-specific days to emergence
(sex_days_plot <- ggplot(emergence_data,
                         aes(x = factor(temperature), y = daysToEmergence,
                             fill = sex, color = sex)) +
    geom_boxplot(alpha = 0.4, outlier.size = 0.8, linewidth = 0.5,
                 position = position_dodge(width = 0.8)) +
    scale_fill_jco() +
    scale_color_jco() +
    scale_y_continuous(breaks = seq(0, 100, 20)) +
    alex_theme +
    theme(legend.position = c(0.8, 0.85),
          legend.background = element_rect(colour = "black", linewidth = 0.25)) +
    xlab("Temperature (°C)") +
    ylab("Days to emergence") +
    ggtitle(NULL)
)

sex_specific_figure <- (sex_devrate_plot | sex_days_plot) +
  plot_annotation(tag_levels = list(c("A.", "B."))) &
  theme(plot.tag = element_text(size = 12, face = "bold"))

ggsave(plot = sex_specific_figure,
       filename = "./01_dev_rate_estimation/dev_rate_by_sex_combined.png",
       width = 180, height = 90, units = "mm", dpi = 300)
ggsave(plot = sex_specific_figure,
       filename = "./01_dev_rate_estimation/dev_rate_by_sex_combined.pdf",
       width = 180, height = 90, units = "mm")

# Emergence Window Analysis ---------------------------------------------------
# Three-parts: Fligner-Killeen (variance homogeneity), Spearman
# correlations (IQR and window width vs. temperature), and quantile
# regression (10th, 50th, 90th percentile; Cade & Noon 2003).

# Fligner-Killeen test ----------------------------------------------------

fk_test <- fligner.test(daysToEmergence ~ factor(temperature),
                        data = emergence_data)

# Window metrics by temperature -------------------------------------------

window_summary <- emergence_data %>%
  group_by(temperature) %>%
  summarise(
    n = n(),
    mean = mean(daysToEmergence),
    sd = sd(daysToEmergence),
    iqr = IQR(daysToEmergence),
    p10 = quantile(daysToEmergence, 0.10),
    p90 = quantile(daysToEmergence, 0.90),
    window = p90 - p10,
    .groups = "drop"
  )

sp_iqr <- cor.test(window_summary$temperature, window_summary$iqr,
                   method = "spearman")
sp_window <- cor.test(window_summary$temperature, window_summary$window,
                      method = "spearman")

# Sex-specific window metrics by temperature
window_summary_by_sex <- emergence_data %>%
  group_by(temperature, sex) %>%
  summarise(
    n = n(),
    mean = mean(daysToEmergence),
    sd = sd(daysToEmergence),
    iqr = IQR(daysToEmergence),
    p10 = quantile(daysToEmergence, 0.10),
    p90 = quantile(daysToEmergence, 0.90),
    window = p90 - p10,
    .groups = "drop"
  )


# Quantile regression -----------------------------------------------------

# Hyperbolic predictor: 1/(temperature - base_temp), consistent with the
# degree-day framework. Fit separately for combined, female, and male groups.

fit_quantile_models <- function(data, base_temp) {
  data <- data %>%
    mutate(inv_temp = 1 / (temperature - base_temp))
  list(
    rq_10 = rq(daysToEmergence ~ inv_temp, tau = 0.10, data = data),
    rq_50 = rq(daysToEmergence ~ inv_temp, tau = 0.50, data = data),
    rq_90 = rq(daysToEmergence ~ inv_temp, tau = 0.90, data = data)
  )
}

qm_combined <- fit_quantile_models(emergence_data, dev_threshold)
qm_female <- fit_quantile_models(filter(emergence_data, sex == "Female"), dev_threshold_female)
qm_male <- fit_quantile_models(filter(emergence_data, sex == "Male"), dev_threshold_male)

# Table S3: Quantile Regression Parameters --------------------------------

# NID SEs used given heterogeneous variance (Fligner-Killeen test above).
# Tb is the sex-specific LDT used in 1/(temperature - Tb).

extract_rq_params <- function(rq_obj, model_label, tau_label, base_temp) {
  s <- summary(rq_obj, se = "nid")$coefficients
  tibble(
    model = model_label,
    quantile = tau_label,
    Tb = round(base_temp, 2),
    intercept = round(s["(Intercept)", "Value"], 2),
    intercept_SE = round(s["(Intercept)", "Std. Error"], 2),
    slope = round(s["inv_temp", "Value"], 2),
    slope_SE = round(s["inv_temp", "Std. Error"], 2)
  )
}

table_s3 <- bind_rows(
  extract_rq_params(qm_combined$rq_10, "Combined", "10th", dev_threshold),
  extract_rq_params(qm_combined$rq_50, "Combined", "50th", dev_threshold),
  extract_rq_params(qm_combined$rq_90, "Combined", "90th", dev_threshold),
  extract_rq_params(qm_female$rq_10, "Female", "10th", dev_threshold_female),
  extract_rq_params(qm_female$rq_50, "Female", "50th", dev_threshold_female),
  extract_rq_params(qm_female$rq_90, "Female", "90th", dev_threshold_female),
  extract_rq_params(qm_male$rq_10, "Male", "10th", dev_threshold_male),
  extract_rq_params(qm_male$rq_50, "Male", "50th", dev_threshold_male),
  extract_rq_params(qm_male$rq_90, "Male", "90th", dev_threshold_male)
)

print(table_s3)
write_csv(table_s3, "./01_dev_rate_estimation/Table_S3_quantile_regression_parameters.csv")

# Build quantile prediction lines ---------------------------------------------

build_qr_lines <- function(qm, base_temp, temp_seq) {
  newdata <- tibble(inv_temp = 1 / (temp_seq - base_temp))
  bind_rows(
    tibble(temperature = temp_seq, tau = "10th percentile",
           fit = predict(qm$rq_10, newdata = newdata)),
    tibble(temperature = temp_seq, tau = "50th percentile",
           fit = predict(qm$rq_50, newdata = newdata)),
    tibble(temperature = temp_seq, tau = "90th percentile",
           fit = predict(qm$rq_90, newdata = newdata))
  ) %>%
    mutate(tau = factor(tau, levels = c("10th percentile",
                                        "50th percentile",
                                        "90th percentile")))
}

temp_seq <- seq(21, 31, 0.1)

qr_lines_combined <- build_qr_lines(qm_combined, dev_threshold, temp_seq)
qr_lines_female <- build_qr_lines(qm_female, dev_threshold_female, temp_seq)
qr_lines_male <- build_qr_lines(qm_male, dev_threshold_male, temp_seq)

# Save quantile predictions for use in app ------------------------------------

write_csv(qr_lines_combined, "./01_dev_rate_estimation/quantile_predictions_combined.csv")
write_csv(qr_lines_female, "./01_dev_rate_estimation/quantile_predictions_female.csv")
write_csv(qr_lines_male, "./01_dev_rate_estimation/quantile_predictions_male.csv")

# Plot helper -----------------------------------------------------------------

make_quantile_plot <- function(data, qr_lines) {
  ggplot() +
    geom_jitter(
      data = data,
      aes(x = temperature, y = daysToEmergence),
      width = 0.3, size = 0.6, alpha = 0.15, color = "grey50"
    ) +
    geom_line(
      data = qr_lines,
      aes(x = temperature, y = fit, color = tau),
      linewidth = 0.9
    ) +
    scale_color_jco() +
    scale_x_continuous(breaks = seq(21, 31, 2)) +
    scale_y_continuous(breaks = seq(0, 100, 20)) +
    alex_theme +
    theme(legend.position = c(0.75, 0.825),
          legend.background = element_rect(colour = "black", linewidth = 0.25)) +
    xlab("Temperature (°C)") +
    ylab("Days to emergence") +
    ggtitle(NULL)
}

# Plot: Combined ----------------------------------------------------------

quantile_plot_combined <- make_quantile_plot(emergence_data, qr_lines_combined)

ggsave(plot = quantile_plot_combined,
       filename = "./01_dev_rate_estimation/emergence_window_quantile_plot.png",
       width = 90, height = 90, units = "mm", dpi = 300)
ggsave(plot = quantile_plot_combined,
       filename = "./01_dev_rate_estimation/emergence_window_quantile_plot.pdf",
       width = 90, height = 90, units = "mm")

# Plot: By sex ------------------------------------------------------------

quantile_plot_female <- make_quantile_plot(
  filter(emergence_data, sex == "Female"), qr_lines_female)
quantile_plot_male <- make_quantile_plot(
  filter(emergence_data, sex == "Male"), qr_lines_male)

quantile_plot_by_sex <- plot_grid(
  quantile_plot_female, quantile_plot_male,
  ncol = 1,
  labels = c("A.", "B."),
  label_size = 12
)

ggsave(plot = quantile_plot_by_sex,
       filename = "./01_dev_rate_estimation/emergence_window_quantile_plot_by_sex.png",
       width = 90, height = 180, units = "mm", dpi = 300)
ggsave(plot = quantile_plot_by_sex,
       filename = "./01_dev_rate_estimation/emergence_window_quantile_plot_by_sex.pdf",
       width = 90, height = 180, units = "mm")

# Save Summary Tables ---------------------------------------------------------

write_csv(window_summary, "./01_dev_rate_estimation/emergence_window_summary.csv")
write_csv(window_summary_by_sex, "./01_dev_rate_estimation/emergence_window_summary_by_sex.csv")

write_csv(devrate_summary_combined, "./01_dev_rate_estimation/dev_rate_summary_combined.csv")
write_csv(devrate_summary_by_sex, "./01_dev_rate_estimation/dev_rate_summary_by_sex.csv")


# Table S1: Merged developmental rate summary -----------------------------

table_s1 <- bind_rows(
  devrate_summary_combined %>% mutate(sex = "Combined"),
  devrate_summary_by_sex
) %>%
  arrange(temperature, factor(sex, levels = c("Combined", "Female", "Male"))) %>%
  select(temperature, sex, n, medianDays, meanDays,
         medianDevRate, meanDevRate, seDevRate, sdDevRate)

write_csv(table_s1, "./01_dev_rate_estimation/Table_S1_Emergence_Rates.csv")


# Table S4: Merged emergence window summary -------------------------------

table_s4 <- bind_rows(
  window_summary %>% mutate(sex = "Combined"),
  window_summary_by_sex
) %>%
  arrange(temperature, factor(sex, levels = c("Combined", "Female", "Male"))) %>%
  select(temperature, sex, n, mean, sd, iqr, p10, p90, window)

write_csv(table_s4, "./01_dev_rate_estimation/Table_S4_emergence_window_summary.csv")

# Session Info ----------------------------------------------------------------

writeLines(capture.output(sessionInfo()), "./01_dev_rate_estimation/01_session_info.txt")