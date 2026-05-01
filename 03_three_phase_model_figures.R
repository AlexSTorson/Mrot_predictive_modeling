################################################################################
#
# Title: Three-Phase Temperature Transfer Model Figures
#
# Author: Alex Torson
# Institution: USDA-ARS
# Email: Alex.Torson@usda.gov
#
# Description: Generates protocol timeline, cumulative development, and
#              cryophase sensitivity figures for the three-phase model.
#              Panels assembled into a single combined figure.
#
# Dependencies: 01_dev_rate_estimation.R must be run first.
#
# Input files:
#   - 01_dev_rate_estimation/dev_rate_summary_combined.csv
#
# Output files:
#   - 03_three_phase_model_figures/protocol_timeline.png/.pdf
#   - 03_three_phase_model_figures/cumulative_development.png/.pdf
#   - 03_three_phase_model_figures/sensitivity_cryophase.png/.pdf
#   - 03_three_phase_model_figures/combined_manuscript_figure.png/.pdf
#   - 03_three_phase_model_figures/03_session_info.txt
#
################################################################################

# Package Loading -------------------------------------------------------------

library(tidyverse)
library(lubridate)
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

# Working Directory and Output Setup ------------------------------------------

setwd(
  "/Users/alextorson/Library/CloudStorage/OneDrive-USDA/Torson_Lab/Mrot_Predictive_Modeling/"
)

if (!dir.exists("./03_three_phase_model_figures/")) {
  dir.create("./03_three_phase_model_figures/", recursive = TRUE)
}

# User Input ------------------------------------------------------------------

startTemp <- 29
transferDownTemp <- 23
transferUpTemp <- 31

startDate <- "5/1/2022"
transferDownDate <- "5/9/2022"
transferUpDate <- "5/16/2022"

# Load Developmental Rate Summary ---------------------------------------------

input_data <- read_csv("./01_dev_rate_estimation/dev_rate_summary_combined.csv") %>%
  select(temperature, medianDays, medianDevRate) %>%
  rename(
    medianEmergence = medianDays,
    devRate = medianDevRate
  )

# Parse Dates and Compute Core Model ------------------------------------------

start_date <- mdy(startDate)
transfer_down_date <- mdy(transferDownDate)
transfer_up_date <- mdy(transferUpDate)

duration_first_thermo <- as.numeric(transfer_down_date - start_date)
duration_cryo <- as.numeric(transfer_up_date   - transfer_down_date)

rate_first_thermo <- input_data %>% filter(temperature == startTemp) %>% pull(devRate)
rate_cryo <- input_data %>% filter(temperature == transferDownTemp) %>% pull(devRate)
rate_final <- input_data %>% filter(temperature == transferUpTemp) %>% pull(devRate)

dev_first_thermo <- duration_first_thermo * rate_first_thermo
dev_cryo <- duration_cryo         * rate_cryo
total_dev <- dev_first_thermo + dev_cryo
remaining_dev <- 1 - total_dev

final_days <- round(remaining_dev / rate_final, digits = 0)
predicted_date <- transfer_up_date + days(final_days)

# Protocol Timeline Panel -------------------------------------------------
#
# Displays incubation temperature as a step function across time. Each phase
# is drawn as a horizontal segment at its temperature, with vertical drops at
# transitions. A dashed vertical line marks the predicted emergence date.

timeline_data <- tibble(
  elapsed = c(0, duration_first_thermo,
              duration_first_thermo, duration_first_thermo + duration_cryo,
              duration_first_thermo + duration_cryo,
              duration_first_thermo + duration_cryo + final_days),
  temp = c(startTemp, startTemp,
           transferDownTemp, transferDownTemp,
           transferUpTemp, transferUpTemp),
  phase = c("Thermophase 1", "Thermophase 1",
            "Cryophase", "Cryophase",
            "Thermophase 2", "Thermophase 2")
)

emergence_elapsed <- duration_first_thermo + duration_cryo + final_days

phase_labels <- tibble(
  elapsed = c(duration_first_thermo / 2,
              duration_first_thermo + duration_cryo / 2,
              duration_first_thermo + duration_cryo + final_days / 2),
  temp = c(startTemp + 0.5, transferDownTemp + 0.5, transferUpTemp + 0.5),
  label = c(paste0(startTemp, "°C"),
            paste0(transferDownTemp, "°C"),
            paste0(transferUpTemp, "°C"))
)

(timeline_plot <- ggplot(timeline_data,
                         aes(x = elapsed, y = temp,
                             color = phase, group = phase)) +
    geom_line(linewidth = 1.2) +
    geom_vline(xintercept = emergence_elapsed,
               linetype = "dashed", linewidth = 0.6, color = "grey40") +
    geom_text(data = phase_labels,
              aes(x = elapsed, y = temp, label = label),
              inherit.aes = FALSE,
              size = 3.2, vjust = -0.4) +
    annotate("text",
             x = emergence_elapsed, y = min(timeline_data$temp) + 1,
             label = "Emergence",
             hjust = 1.1, size = 3.2, color = "grey60") +
    scale_color_jco() +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = seq(20, 34, 2),
                       limits = c(NA, transferUpTemp + 3)) +
    alex_theme +
    theme(legend.position = "none") +
    xlab("Time") +
    ylab("Temperature (°C)") +
    ggtitle(NULL)
)

ggsave(plot = timeline_plot, filename = "./03_three_phase_model_figures/protocol_timeline.png",
       width = 8, height = 5, dpi = 300)
ggsave(plot = timeline_plot, filename = "./03_three_phase_model_figures/protocol_timeline.pdf",
       width = 8, height = 5)


# Cumulative Development Curve Panel --------------------------------------
#
# Plots cumulative relative developmental progress (0 to 1) against time.
# Progress accumulates linearly within each phase at a rate equal to the
# devRate for that temperature. 

days_phase1 <- seq(0, duration_first_thermo, 1)
days_phase2 <- seq(0, duration_cryo, 1)
days_phase3 <- seq(0, final_days, 1)

offset_phase2 <- duration_first_thermo
offset_phase3 <- duration_first_thermo + duration_cryo

cumdev_data <- bind_rows(
  tibble(
    elapsed = days_phase1,
    cumDev = days_phase1 * rate_first_thermo,
    phase = "Thermophase 1"
  ),
  tibble(
    elapsed = offset_phase2 + days_phase2,
    cumDev = dev_first_thermo + days_phase2 * rate_cryo,
    phase = "Cryophase"
  ),
  tibble(
    elapsed = offset_phase3 + days_phase3,
    cumDev = total_dev + days_phase3 * rate_final,
    phase = "Thermophase 2"
  )
)

boundary_x <- c(offset_phase2, offset_phase3)
emergence_x <- offset_phase3 + final_days

(cumdev_plot <- ggplot(cumdev_data, aes(x = elapsed, y = cumDev,
                                        color = phase, group = phase)) +
    geom_line(linewidth = 1.2) +
    geom_hline(yintercept = 1, linetype = "dotted",
               linewidth = 0.6, color = "grey40") +
    geom_vline(xintercept = boundary_x,
               linetype = "dashed", linewidth = 0.5, color = "grey60") +
    geom_vline(xintercept = emergence_x,
               linetype = "dashed", linewidth = 0.6, color = "grey40") +
    annotate("text",
             x = emergence_x, y = 0.05,
             label = "Emergence",
             hjust = 1.1, size = 3.2, color = "grey60") +
    annotate("text", x = boundary_x[1], y = 0.05,
             label = "Trans. 1", hjust = 1.1, size = 3.0, color = "grey60") +
    annotate("text", x = boundary_x[2], y = 0.05,
             label = "Trans. 2", hjust = 1.1, size = 3.0, color = "grey60") +
    scale_color_jco() +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(limits = c(0, 1.05),
                       breaks = seq(0, 1, 0.2),
                       labels = seq(0, 100, 20)) +
    alex_theme +
    theme(legend.position = "none") +
    xlab("Time") +
    ylab("Percent Developed") +
    ggtitle(NULL)
)

ggsave(plot = cumdev_plot, filename = "./03_three_phase_model_figures/cumulative_development.png",
       width = 8, height = 5, dpi = 300)
ggsave(plot = cumdev_plot, filename = "./03_three_phase_model_figures/cumulative_development.pdf",
       width = 8, height = 5)


# Cryophase Duration Sensitivity Panel ------------------------------------
#
# Holds all inputs fixed except cryophase duration, which is swept across a
# range of plausible values. Illustrates how sensitively the predicted
# emergence date responds to decisions about cold-phase length.
#
# Note: transfer_up_date is held fixed, so varying cryophase duration
# implicitly changes transfer_down_date.

cryo_range <- seq(1, 14, 1)

sensitivity_data <- map_dfr(cryo_range, function(cryo_days) {
  dev_1 <- duration_first_thermo * rate_first_thermo
  dev_c <- cryo_days * rate_cryo
  tot_dev <- dev_1 + dev_c
  rem_dev <- 1 - tot_dev
  fin_days <- round(rem_dev / rate_final, digits = 0)
  tibble(cryoDays = cryo_days, finalDays = fin_days)
})

user_point <- sensitivity_data %>% filter(cryoDays == duration_cryo)

(sensitivity_plot <- ggplot(sensitivity_data,
                            aes(x = cryoDays, y = finalDays)) +
    geom_line(linewidth = 0.9, color = pal_jco()(1)) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.6,
                linetype = "dashed", color = "grey40") +
    geom_point(data = user_point, aes(x = cryoDays, y = finalDays),
               size = 3, shape = 21,
               fill = pal_jco()(1), color = "white", stroke = 1.2) +
    geom_vline(xintercept = duration_cryo,
               linetype = "dashed", linewidth = 0.5, color = "grey50") +
    annotate("text",
             x = duration_cryo, y = min(sensitivity_data$finalDays),
             label = paste0("Ex. protocol\n(", duration_cryo, " days)"),
             hjust = 1.2, vjust = -0.1, size = 3.2, color = "grey60") +
    scale_x_continuous(breaks = seq(1, 14, 2)) +
    alex_theme +
    xlab("Cryophase Duration (days)") +
    ylab("Days to Emergence") +
    ggtitle(NULL)
)

ggsave(plot = sensitivity_plot, filename = "./03_three_phase_model_figures/sensitivity_cryophase.png",
       width = 8, height = 5, dpi = 300)
ggsave(plot = sensitivity_plot, filename = "./03_three_phase_model_figures/sensitivity_cryophase.pdf",
       width = 8, height = 5)

# Combined Manuscript Figure --------------------------------------------------

combined_plot <- (timeline_plot / cumdev_plot / sensitivity_plot) +
  plot_annotation(tag_levels = list(c("A.", "B.", "C."))) &
  theme(legend.position = "none",
        plot.tag = element_text(size = 12, face = "bold"))

ggsave(
  plot = combined_plot,
  filename = "./03_three_phase_model_figures/combined_manuscript_figure.png",
  width = 90,
  height = 180,
  units = "mm",
  dpi = 300
)

ggsave(
  plot = combined_plot,
  filename = "./03_three_phase_model_figures/combined_manuscript_figure.pdf",
  width = 90,
  height = 180,
  units = "mm"
)

# Session Info ----------------------------------------------------------------

writeLines(capture.output(sessionInfo()), "./03_three_phase_model_figures/03_session_info.txt")