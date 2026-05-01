################################################################################
#
# Title: Two-Phase Temperature Transfer Model for Bee Emergence Timing
#
# Author: Alex Torson
# Institution: USDA-ARS
# Email: Alex.Torson@usda.gov
#
# Description: Predicts emergence timing under two-phase protocols as a
#              function of days at the starting temperature. Emergence window
#              ribbon derived from quantile models in script 01.
#
# Dependencies: 01_dev_rate_estimation.R must be run first.
#
# Input files:
#   - 01_dev_rate_estimation/dev_rate_summary_combined.csv
#   - 01_dev_rate_estimation/dev_rate_summary_by_sex.csv
#   - 01_dev_rate_estimation/Table_S3_quantile_regression_parameters.csv
#
# Output files:
#   - 02_two_phase_model/two_phase_model_combined.png/pdf
#   - 02_two_phase_model/two_phase_model_combined_ribbon.png/pdf
#   - 02_two_phase_model/two_phase_model_by_sex_ribbon.png/pdf
#   - 02_two_phase_model/two_phase_model_by_sex.png/pdf
#   - 02_two_phase_model/two_phase_model_combined.csv
#   - 02_two_phase_model/two_phase_model_by_sex.csv
#   - 02_two_phase_model/02_session_info.txt
#
################################################################################

# Package Loading -------------------------------------------------------------

library(tidyverse)
library(ggsci)
library(patchwork)

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

if (!dir.exists("./02_two_phase_model/")) {
  dir.create("./02_two_phase_model/", recursive = TRUE)
}

# User Input ------------------------------------------------------------------

startTemp <- 29

# Load Developmental Rate Summaries -------------------------------------------

summary_combined <- read_csv("./01_dev_rate_estimation/dev_rate_summary_combined.csv") %>%
  select(temperature, medianDays, medianDevRate) %>%
  rename(medianEmergence = medianDays, devRate = medianDevRate)

summary_by_sex <- read_csv("./01_dev_rate_estimation/dev_rate_summary_by_sex.csv") %>%
  select(temperature, sex, medianDays, medianDevRate) %>%
  rename(medianEmergence = medianDays, devRate = medianDevRate)

summary_female <- summary_by_sex %>% filter(sex == "Female") %>% select(-sex)
summary_male <- summary_by_sex %>% filter(sex == "Male") %>% select(-sex)

qr_params <- read_csv("./01_dev_rate_estimation/Table_S3_quantile_regression_parameters.csv")

# Model Function --------------------------------------------------------------
#
# For a given input_data table, builds the full predictions table for all
# transfer temperatures and days-at-start combinations.

build_predictions <- function(input_data) {
  
  variable_table <- input_data %>% filter(temperature == startTemp)
  
  max_day <- variable_table$medianEmergence
  plot_days <- tibble(plotDays = seq(1, max_day - 1, 1)) %>%
    mutate(
      devRate = variable_table$devRate,
      percentDev = plotDays * devRate,
      remainder = 1 - percentDev
    )
  
  transfer_temps <- seq(21, startTemp - 2, 2)
  
  predictions <- map_dfr(transfer_temps, function(d) {
    devRate_t <- input_data %>% filter(temperature == d) %>% pull(devRate)
    days_est <- round(plot_days$remainder / devRate_t, digits = 0)
    tibble(
      plotDays = plot_days$plotDays,
      Temperatures = paste0(d, "\u00b0C"),
      daysToEmergence = days_est
    )
  })
  
  list(predictions = predictions,
       uninterrupted = variable_table$medianEmergence,
       max_day = max_day)
}

# Window Prediction Function --------------------------------------------------
#
# Uses quantile regression parameters to compute p10 and p90 remaining days
# at each transfer temperature, given the median developmental progress at
# the starting temperature. Model: days(T, tau) = intercept + slope / (T - Tb)

build_window_predictions <- function(input_data, qr_params, model_label = "Combined") {
  
  qr <- qr_params %>% filter(model == model_label)
  Tb <- qr$Tb[1]
  
  get_days_quantile <- function(temp, tau_label) {
    row <- qr %>% filter(quantile == tau_label)
    row$intercept + row$slope / (temp - Tb)
  }
  
  variable_table <- input_data %>% filter(temperature == startTemp)
  max_day <- variable_table$medianEmergence
  
  plot_days <- tibble(plotDays = seq(1, max_day - 1, 1)) %>%
    mutate(
      devRate = variable_table$devRate,
      percentDev = plotDays * devRate,
      remainder = 1 - percentDev
    )
  
  transfer_temps <- seq(21, startTemp - 2, 2)
  
  window_preds <- map_dfr(transfer_temps, function(d) {
    days_p10 <- get_days_quantile(d, "10th")
    days_p90 <- get_days_quantile(d, "90th")
    
    tibble(
      plotDays = plot_days$plotDays,
      Temperatures = paste0(d, "\u00b0C"),
      window_lo = plot_days$remainder * days_p10,
      window_hi = plot_days$remainder * days_p90,
      window_width = plot_days$remainder * (days_p90 - days_p10)
    )
  })
  
  # Uninterrupted window reference at startTemp
  ref_p10 <- get_days_quantile(startTemp, "10th")
  ref_p90 <- get_days_quantile(startTemp, "90th")
  
  list(window_preds = window_preds,
       ref_window_lo = ref_p10,
       ref_window_hi = ref_p90)
}

# Run Model -------------------------------------------------------------------

out_combined <- build_predictions(summary_combined)
out_female <- build_predictions(summary_female)
out_male <- build_predictions(summary_male)

win_combined <- build_window_predictions(summary_combined, qr_params, "Combined")
win_female <- build_window_predictions(summary_female, qr_params, "Female")
win_male <- build_window_predictions(summary_male, qr_params, "Male")

by_sex_table <- bind_rows(
  out_female$predictions %>% mutate(sex = "Female"),
  out_male$predictions %>% mutate(sex = "Male")
)

x_axis_lab <- paste0("Days at ", startTemp, "\u00b0C")
x_axis_scale <- out_combined$max_day - 1


# Plot: Combined ----------------------------------------------------------

(combined_plot <- ggplot(out_combined$predictions,
                         aes(x = plotDays, y = daysToEmergence,
                             color = Temperatures, fill = Temperatures)) +
   geom_hline(yintercept = out_combined$uninterrupted,
              linetype = "dotted", linewidth = 0.6, color = "grey40") +
   annotate("segment",
            x = 3, xend = 3,
            y = out_combined$uninterrupted - 7,
            yend = out_combined$uninterrupted - 0.5,
            color = "grey40", linewidth = 0.4,
            arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
   annotate("text",
            x = 3, y = out_combined$uninterrupted - 6,
            label = paste0("Uninterrupted (", startTemp, "\u00b0C)"),
            hjust = 0.25, vjust = 2, size = 3.0, color = "grey40") +
   geom_line(linewidth = 0.8) +
   scale_color_jco(name = "Transfer Temp.") +
   scale_fill_jco(name = "Transfer Temp.") +
   alex_theme +
   theme(legend.position = c(0.80, 0.8),
         legend.background = element_rect(colour = "black", linewidth = 0.25),
         legend.title = element_text(size = 9)) +
   ylab("Days to Emergence") +
   xlab(x_axis_lab) +
   scale_x_continuous(breaks = seq(1, x_axis_scale, 2)) +
   scale_y_continuous(breaks = seq(0, 70, 10))
)

ggsave(plot = combined_plot,
       filename = "./02_two_phase_model/two_phase_model_combined.png",
       width = 90, height = 110, units = "mm", dpi = 300)
ggsave(plot = combined_plot,
       filename = "./02_two_phase_model/two_phase_model_combined.pdf",
       width = 90, height = 110, units = "mm")

# Plot: Combined + Window Ribbon ----------------------------------------------
#
# Adds p10–p90 emergence window as a ribbon behind each median line.
# The startTemp uninterrupted window is shown as a horizontal shaded band.

ribbon_data <- out_combined$predictions %>%
  left_join(win_combined$window_preds, by = c("plotDays", "Temperatures"))

(ribbon_plot <- ggplot(ribbon_data,
                       aes(x = plotDays, y = daysToEmergence,
                           color = Temperatures, fill = Temperatures)) +
    annotate("rect",
             xmin = -Inf, xmax = Inf,
             ymin = win_combined$ref_window_lo,
             ymax = win_combined$ref_window_hi,
             fill = "grey70", alpha = 0.25) +
    geom_hline(yintercept = out_combined$uninterrupted,
               linetype = "dotted", linewidth = 0.6, color = "grey40") +
    annotate("text",
             x = out_combined$max_day - 1,
             y = (win_combined$ref_window_lo + win_combined$ref_window_hi) / 2,
             label = paste0("Uninterrupted (", startTemp, "\u00b0C)"),
             hjust = 1, vjust = -0.4, size = 2.8, color = "grey40") +
    geom_ribbon(aes(ymin = window_lo, ymax = window_hi),
                alpha = 0.20, color = NA) +
    geom_line(linewidth = 0.8) +
    scale_color_jco(name = "Transfer Temp.") +
    scale_fill_jco(name = "Transfer Temp.") +
    alex_theme +
    theme(legend.position = c(0.80, 0.8),
          legend.background = element_rect(colour = "black", linewidth = 0.25),
          legend.title = element_text(size = 9)) +
    ylab("Days to Emergence") +
    xlab(x_axis_lab) +
    scale_x_continuous(breaks = seq(1, x_axis_scale, 2)) +
    scale_y_continuous(breaks = seq(0, 70, 10))
)

ggsave(plot = ribbon_plot,
       filename = "./02_two_phase_model/two_phase_model_combined_ribbon.png",
       width = 90, height = 110, units = "mm", dpi = 300)
ggsave(plot = ribbon_plot,
       filename = "./02_two_phase_model/two_phase_model_combined_ribbon.pdf",
       width = 90, height = 110, units = "mm")

# Plot: Female + Window Ribbon ------------------------------------------------

ribbon_data_female <- out_female$predictions %>%
  left_join(win_female$window_preds, by = c("plotDays", "Temperatures"))

(ribbon_female_plot <- ggplot(ribbon_data_female,
                              aes(x = plotDays, y = daysToEmergence,
                                  color = Temperatures, fill = Temperatures)) +
    annotate("rect",
             xmin = -Inf, xmax = Inf,
             ymin = win_female$ref_window_lo,
             ymax = win_female$ref_window_hi,
             fill = "grey70", alpha = 0.25) +
    geom_hline(yintercept = out_female$uninterrupted,
               linetype = "dotted", linewidth = 0.6, color = "grey40") +
    annotate("text",
             x = out_female$max_day - 1,
             y = (win_female$ref_window_lo + win_female$ref_window_hi) / 2,
             label = paste0("Uninterrupted (", startTemp, "\u00b0C)"),
             hjust = 1, vjust = -0.4, size = 2.8, color = "grey40") +
    geom_ribbon(aes(ymin = window_lo, ymax = window_hi),
                alpha = 0.20, color = NA) +
    geom_line(linewidth = 0.8) +
    scale_color_jco(name = "Transfer Temp.") +
    scale_fill_jco(name = "Transfer Temp.") +
    alex_theme +
    theme(legend.position = c(0.80, 0.8),
          legend.background = element_rect(colour = "black", linewidth = 0.25),
          legend.title = element_text(size = 9)) +
    ylab("Days to Emergence") +
    xlab(x_axis_lab) +
    scale_x_continuous(breaks = seq(1, x_axis_scale, 2)) +
    scale_y_continuous(breaks = seq(0, 70, 10))
)

# Plot: Male + Window Ribbon --------------------------------------------------

ribbon_data_male <- out_male$predictions %>%
  left_join(win_male$window_preds, by = c("plotDays", "Temperatures"))

(ribbon_male_plot <- ggplot(ribbon_data_male,
                            aes(x = plotDays, y = daysToEmergence,
                                color = Temperatures, fill = Temperatures)) +
    annotate("rect",
             xmin = -Inf, xmax = Inf,
             ymin = win_male$ref_window_lo,
             ymax = win_male$ref_window_hi,
             fill = "grey70", alpha = 0.25) +
    geom_hline(yintercept = out_male$uninterrupted,
               linetype = "dotted", linewidth = 0.6, color = "grey40") +
    annotate("text",
             x = out_male$max_day - 1,
             y = (win_male$ref_window_lo + win_male$ref_window_hi) / 2,
             label = paste0("Uninterrupted (", startTemp, "\u00b0C)"),
             hjust = 1, vjust = -0.4, size = 2.8, color = "grey40") +
    geom_ribbon(aes(ymin = window_lo, ymax = window_hi),
                alpha = 0.20, color = NA) +
    geom_line(linewidth = 0.8) +
    scale_color_jco(name = "Transfer Temp.") +
    scale_fill_jco(name = "Transfer Temp.") +
    alex_theme +
    theme(legend.position = c(0.80, 0.8),
          legend.background = element_rect(colour = "black", linewidth = 0.25),
          legend.title = element_text(size = 9)) +
    ylab("Days to Emergence") +
    xlab(x_axis_lab) +
    scale_x_continuous(breaks = seq(1, x_axis_scale, 2)) +
    scale_y_continuous(breaks = seq(0, 70, 10))
)

# Plot: By Sex Ribbon (Paneled) -----------------------------------------------

ribbon_by_sex_plot <- (ribbon_female_plot | ribbon_male_plot) +
  plot_annotation(tag_levels = list(c("A.", "B."))) &
  theme(plot.tag = element_text(size = 12, face = "bold"))

ggsave(plot = ribbon_by_sex_plot,
       filename = "./02_two_phase_model/two_phase_model_by_sex_ribbon.png",
       width = 180, height = 110, units = "mm", dpi = 300)
ggsave(plot = ribbon_by_sex_plot,
       filename = "./02_two_phase_model/two_phase_model_by_sex_ribbon.pdf",
       width = 180, height = 110, units = "mm")


# Plot: By Sex ------------------------------------------------------------

# I removed the window ribbons for clarity given the doubled number of lines.

(sex_plot <- ggplot(by_sex_table,
                    aes(x = plotDays, y = daysToEmergence,
                        color = Temperatures,
                        linetype = sex,
                        group = interaction(Temperatures, sex))) +
   geom_hline(yintercept = out_female$uninterrupted,
              linetype = "dashed", linewidth = 0.6, color = "grey40", alpha = 0.4) +
   geom_hline(yintercept = out_male$uninterrupted,
              linetype = "solid", linewidth = 0.6, color = "grey40", alpha = 0.4) +
   geom_line(linewidth = 0.8) +
   scale_color_jco(name = "Transfer Temp.") +
   scale_linetype_manual(name = NULL,
                         values = c("Female" = "dashed", "Male" = "solid")) +
   alex_theme +
   theme(legend.position = c(0.80, 0.70),
         legend.background = element_rect(colour = "black", linewidth = 0.25),
         legend.title = element_text(size = 9)) +
   ylab("Days to Emergence") +
   xlab(x_axis_lab) +
   scale_x_continuous(breaks = seq(1, x_axis_scale, 2)) +
   scale_y_continuous(breaks = seq(0, 70, 10))
)

ggsave(plot = sex_plot,
       filename = "./02_two_phase_model/two_phase_model_by_sex.png",
       width = 90, height = 110, units = "mm", dpi = 300)
ggsave(plot = sex_plot,
       filename = "./02_two_phase_model/two_phase_model_by_sex.pdf",
       width = 90, height = 110, units = "mm")

# Save CSVs -------------------------------------------------------------------

write_csv(out_combined$predictions, "./02_two_phase_model/two_phase_model_combined.csv")
write_csv(by_sex_table, "./02_two_phase_model/two_phase_model_by_sex.csv")

# Session Info ----------------------------------------------------------------

writeLines(capture.output(sessionInfo()), "./02_two_phase_model/02_session_info.txt")