################################################################################
#
# Title: Megachile rotundata Emergence Date Predictor — Shiny Application
#
# Author: Alex Torson
# Institution: USDA-ARS
# Email: Alex.Torson@usda.gov
#
# Description: Shiny application for predicting M. rotundata emergence timing
#              under standard, three-phase, and two-phase incubation protocols.
#              Supports combined, female, and male developmental rate models.
#
################################################################################

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(ggsci)
library(patchwork) 

# Embedded Developmental Rate Data --------------------------------------------
# These tables are embedded directly rather than read from a file so the app
# is self-contained and deployable without external data dependencies.
#
# Each table covers integer temperatures 21-31°C. devRate = 1/medianDays,
# i.e. the proportion of total development completed per day. seDevRate and n
# are non-NA only for the six experimentally validated temperatures (21, 23,
# 25, 27, 29, 31°C); even-numbered intermediate temperatures are interpolated
# from the degree-day model and have NA for seDevRate and n.
#
# Source: dev_rate_summary_combined.csv from script 01_dev_rate_estimation.R

dev_rates_combined <- tibble(
  temperature = 21:31,
  medianDays = c(70, 58, 50, 41, 35, 31, 29, 26, 23, 22, 20),
  devRate = c(0.014286, 0.017225, 0.020000, 0.024508, 0.028571, 0.031792, 0.034483, 0.039075, 0.043478, 0.046358, 0.050000),
  seDevRate = c(0.000113, NA, 0.000173, NA, 0.000206, NA, 0.000228, NA, 0.000323, NA, 0.000293),
  n = c(205, NA, 205, NA, 207, NA, 209, NA, 202, NA, 205)
)

dev_rates_female <- tibble(
  temperature = 21:31,
  medianDays = c(77, 63, 54.5, 44, 38, 34, 31, 28, 24, 23, 22),
  devRate = c(0.012987, 0.015895, 0.018350, 0.022703, 0.026316, 0.029510, 0.032258, 0.036317, 0.041667, 0.043125, 0.045455),
  seDevRate = c(0.000126, NA, 0.000228, NA, 0.000296, NA, 0.000290, NA, 0.000399, NA, 0.000359),
  n = c(91, NA, 70, NA, 85, NA, 76, NA, 82, NA, 81)
)

dev_rates_male <- tibble(
  temperature = 21:31,
  medianDays = c(65, 53, 46, 38, 33, 30, 27, 25, 22, 21, 20),
  devRate = c(0.015385, 0.018967, 0.021739, 0.026138, 0.030303, 0.033309, 0.037037, 0.040480, 0.045455, 0.047651, 0.050000),
  seDevRate = c(0.000107, NA, 0.000181, NA, 0.000177, NA, 0.000237, NA, 0.000372, NA, 0.000308),
  n = c(114, NA, 135, NA, 122, NA, 133, NA, 120, NA, 124)
)

# Embedded Quantile Predictions -----------------------------------------------
# Predicted days to emergence at the 10th, 50th, and 90th percentiles for
# integer temperatures 21-31°C, derived from hyperbolic quantile regression
# models fit in script 01_dev_rate_estimation.R. These are used to display
# emergence windows and to propagate percentile-specific developmental rates
# through the three-phase and two-phase models.
#
# Values at all 11 integer temperatures are stored in long format (tau column
# identifies the percentile). Source: quantile_predictions_*.csv from script 01.

quantile_combined <- tibble(
  temperature = rep(21:31, 3),
  tau = rep(c("p10", "p50", "p90"), each = 11),
  days = c(
    63.05, 50.02, 41.53, 35.57, 31.15, 27.74, 25.03, 22.83, 21.00, 19.46, 18.15, # p10
    71.77, 56.81, 47.03, 40.15, 35.06, 31.13, 28.01, 25.47, 23.37, 21.59, 20.07, # p50
    88.00, 69.26, 57.01, 48.40, 42.02, 37.10, 33.19, 30.01, 27.37, 25.15, 23.25   # p90
  )
)

quantile_female <- tibble(
  temperature = rep(21:31, 3),
  tau = rep(c("p10", "p50", "p90"), each = 11),
  days = c(
    69.99, 55.31, 45.81, 39.16, 34.24, 30.46, 27.46, 25.02, 23.00, 21.30, 19.84, # p10
    78.99, 62.31, 51.51, 43.95, 38.36, 34.06, 30.65, 27.88, 25.58, 23.65, 22.00, # p50
    94.91, 74.32, 60.99, 51.66, 44.77, 39.46, 35.25, 31.83, 29.00, 26.61, 24.57   # p90
  )
)

quantile_male <- tibble(
  temperature = rep(21:31, 3),
  tau = rep(c("p10", "p50", "p90"), each = 11),
  days = c(
    61.06, 49.43, 41.49, 35.74, 31.37, 27.94, 25.18, 22.91, 21.01, 19.39, 18.00, # p10
    66.58, 53.79, 45.03, 38.67, 33.85, 30.06, 27.01, 24.50, 22.40, 20.62, 19.08, # p50
    77.11, 62.23, 52.03, 44.63, 39.02, 34.61, 31.06, 28.14, 25.70, 23.62, 21.83   # p90
  )
)

# Named vector used to populate temperature dropdown menus throughout the UI.
# setNames() pairs each integer value (21:31) with a display label ("21°C" etc.).
temp_choices <- setNames(21:31, paste0(21:31, "°C"))

# JCO Palette Colors ----------------------------------------------------------

jco1 <- "#0073C2"
jco2 <- "#EFC000"
jco3 <- "#868686"
jco4 <- "#CD534C"
jco5 <- "#7AA6DC"

# Plotting Theme --------------------------------------------------------------

alex_theme <- theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
    axis.text = element_text(face = "plain", size = 10),
    axis.title = element_text(face = "bold", size = 10),
    axis.title.x = element_text(margin = margin(t = 8, r = 20, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, r = 6, b = 0, l = 0)),
    panel.border = element_rect(fill = NA, colour = "black", linewidth = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank()
  )

# Model Functions -------------------------------------------------------------
#
# These functions implement the degree-day accumulation logic. Development is
# expressed as a proportion from 0 to 1 (i.e. fraction of thermal requirement
# completed). A value of 1 means the bee has accumulated enough degree-days to
# emerge. Rates r1/r2/r3 are the devRate values from the embedded tables above
# (units: proportion of development per day), and d1/d2 are phase durations in
# days.

# Three-phase model: thermophase -> cryophase -> final thermophase.
# dev1 = development accumulated in phase 1 (r1 * d1)
# dev2 = development accumulated in phase 2 (r2 * d2)
# rem_dev = remaining development to complete at phase 3 rate (1 - dev1 - dev2)
# days_final = days needed in final thermophase (rem_dev / r3, rounded)
# valid = TRUE if development is not already complete before the final phase
predict_emergence <- function(r1, r2, r3, d1, d2) {
  dev1 <- d1 * r1
  dev2 <- d2 * r2
  total_dev <- dev1 + dev2
  rem_dev <- 1 - total_dev
  days_final <- round(rem_dev / r3, 0)
  list(
    dev1 = dev1,
    dev2 = dev2,
    total_dev = total_dev,
    rem_dev = rem_dev,
    days_final = max(days_final, 0),
    pred_total = d1 + d2 + max(days_final, 0),
    valid = rem_dev > 0
  )
}

# Two-phase model: thermophase -> cryophase only (no final thermophase).
# Development completes during the cryophase. Same logic but simpler:
# days_cryo = days needed at r2 to finish the remaining development after phase 1.
predict_emergence_2phase <- function(r1, r2, d1) {
  dev1 <- d1 * r1
  rem_dev <- 1 - dev1
  days_cryo <- round(rem_dev / r2, 0)
  list(
    dev1 = dev1,
    rem_dev = rem_dev,
    days_cryo = max(days_cryo, 0),
    pred_total = d1 + max(days_cryo, 0),
    valid = rem_dev > 0
  )
}

# Reverse (bloom date) calculator for the three-phase model.
# Given how much development is already done (dev_already) and how many days
# remain until bloom (days_remaining), this function searches all combinations
# of cryophase temperature (t2) and duration (1-14 days) and final thermophase
# temperature (t3) to find protocols that produce emergence exactly on the target
# date. A protocol is valid when the predicted days in the final phase equals the
# days remaining after the cryophase.
reverse_calculate <- function(dev_already, days_remaining, temp_choices, rates) {
  results <- list()
  for (t2 in temp_choices) {
    r2 <- rates %>% filter(temperature == t2) %>% pull(devRate)
    for (cryo_days in 1:14) {
      dev_after_cryo <- dev_already + cryo_days * r2
      if (dev_after_cryo >= 1) next
      rem_dev <- 1 - dev_after_cryo
      days_left_for_final <- days_remaining - cryo_days
      if (days_left_for_final <= 0) next
      for (t3 in temp_choices) {
        r3 <- rates %>% filter(temperature == t3) %>% pull(devRate)
        days_final_needed <- round(rem_dev / r3, 0)
        if (days_final_needed == days_left_for_final) {
          results[[length(results) + 1]] <- tibble(
            cryo_temp = t2,
            cryo_days = cryo_days,
            final_temp = t3,
            final_days = days_final_needed,
            dev_at_final = round(dev_after_cryo * 100, 1)
          )
        }
      }
    }
  }
  if (length(results) == 0) return(NULL)
  bind_rows(results) %>% arrange(cryo_days, desc(final_temp))
}

# CSS -------------------------------------------------------------------------
# Custom CSS injected into the app to override the default shinydashboard
# "skin-blue" theme colors with the JCO palette used in the manuscript figures.
# This is injected via tags$head(tags$style(HTML(custom_css))) in the UI body.
# The .recommended-row class is applied via JavaScript callback to highlight
# the recommended protocol row in the results tables.

custom_css <- "
  .skin-blue .main-header .logo {
    background-color: #0073C2;
    color: #ffffff;
    font-weight: bold;
  }
  .skin-blue .main-header .logo:hover { background-color: #003C67; }
  .skin-blue .main-header .navbar { background-color: #0073C2; }
  .skin-blue .main-sidebar { background-color: #003C67; }
  .skin-blue .sidebar a { color: #cce0f5; }
  .skin-blue .sidebar-menu > li.active > a,
  .skin-blue .sidebar-menu > li:hover > a {
    background-color: #0073C2;
    border-left-color: #EFC000;
    color: #ffffff;
  }
  .skin-blue .sidebar-menu > li > a { border-left: 3px solid transparent; }
  .skin-blue .sidebar-menu .header {
    color: #EFC000 !important;
    font-size: 11px;
    font-weight: bold;
    letter-spacing: 0.08em;
    padding: 10px 15px 4px 15px;
  }
  .bg-green { background-color: #0073C2 !important; }
  .bg-red { background-color: #CD534C !important; }
  .bg-yellow {
    background-color: #EFC000 !important;
    color: #003C67 !important;
  }
  .box.box-solid.box-success > .box-header {
    background-color: #0073C2;
    border-color: #0073C2;
  }
  .box.box-solid.box-success { border-color: #0073C2; }
  .box.box-solid.box-primary > .box-header {
    background-color: #003C67;
    border-color: #003C67;
  }
  .box.box-solid.box-primary { border-color: #003C67; }
  .box.box-solid.box-warning > .box-header {
    background-color: #EFC000;
    border-color: #EFC000;
    color: #003C67;
  }
  .box.box-solid.box-warning { border-color: #EFC000; }
  .recommended-row {
    background-color: #CCE0F5 !important;
    font-weight: bold;
  }
  h4 {
    color: #0073C2;
    border-bottom: 2px solid #EFC000;
    padding-bottom: 4px;
  }
  hr { border-top: 1px solid #EFC000; opacity: 0.4; }
  a { color: #0073C2; }
  a:hover { color: #003C67; }
  dt {
    font-weight: bold;
    color: #003C67;
    margin-top: 6px;
  }
  dd { margin-left: 12px; margin-bottom: 2px; color: #333; }
"

# UI --------------------------------------------------------------------------

ui <- dashboardPage(
  skin = "blue",
  title = "AlfalfaBeeTools",
  
  dashboardHeader(
    title = tagList(
      tags$head(
        tags$link(rel = "icon", type = "image/png", href = "bee01.png")
      ),
      tags$img(src = "bee01.png", height = "50px",
               style = "vertical-align: middle; margin-right: 8px;"),
      "AlfalfaBeeTools"
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("About & How-To", tabName = "about", icon = icon("info-circle")),
      tags$li(class = "header", "CHOOSE YOUR MODEL"),
      tags$li(
        style = "padding: 4px 15px 8px 15px;",
        radioButtons(
          inputId = "sex_model",
          label = NULL,
          choices = c("Combined" = "combined",
                      "Female"   = "female",
                      "Male"     = "male"),
          selected = "combined"
        )
      ),
      tags$li(class = "header", "STANDARD DEVELOPMENT"),
      menuItem("Degree-Day Predictor", tabName = "standard", icon = icon("thermometer-half")),
      tags$li(class = "header", "INTERRUPTED DEVELOPMENT"),
      menuItem("Three-Phase Model", tabName = "three_phase", icon = icon("calendar")),
      menuItem("Two-Phase Model", tabName = "two_phase", icon = icon("exchange-alt"))
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    
    tabItems(
      
      # ---- About Tab ----
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "Welcome to AlfalfaBeeTools!", width = 12,
                  status = "success", solidHeader = TRUE,
                  p("This tool helps alfalfa leafcutting bee (", em("Megachile rotundata"),
                    ") managers predict and plan emergence timing. Three approaches are
               available: a standard, single-temperature degree-day model for
               conventional incubation, a three-phase interrupted-development
               model for fine-tuning emergence synchrony with peak floral bloom,
               and a two-phase model for situations where a final thermophase is
               not practical."),
                  p("Use the ", strong("CHOOSE YOUR MODEL"), " selector in the sidebar to switch between
               combined (sexes pooled), female-specific, and male-specific developmental
               rate models. The selected model applies across all tools."),
                  hr(),
                  p(icon("flask"), strong(" Coming soon: "),
                    "Future versions of this tool will include models for managing",
                    em("Melittobia"), "and other parasites that affect", em("M. rotundata"),
                    "nest cells during incubation.",
                    style = "color: #555; font-size: 13px;")
                )
              ),
              fluidRow(
                box(
                  title = "Standard Development Tools", width = 6,
                  status = "primary", solidHeader = TRUE,
                  h4("Degree-Day Predictor"),
                  p("Use this tab to predict emergence timing under standard
               single-temperature incubation."),
                  tags$ol(
                    tags$li("Select your incubation temperature."),
                    tags$li("Enter either the start date or the number of days
                       already elapsed at that temperature."),
                    tags$li("Predicted emergence dates are shown for the 10th, 50th,
                       and 90th percentiles, giving you the expected emergence
                       window under the selected conditions."),
                    tags$li("A reference table shows median days to emergence and
                       developmental rates for all available temperatures.")
                  )
                ),
                box(
                  title = "Interrupted Development Tools", width = 6,
                  status = "primary", solidHeader = TRUE,
                  h4("Three-Phase Model"),
                  p("Use this tab when you are using a full three-phase protocol:
               an initial thermophase, a cryophase to delay development, and
               a final thermophase to complete development before your target bloom date.
               This is the preferred approach — the final thermophase acts to synchronize
               emergence across individuals, producing a tighter, more predictable
               emergence window than protocols that end at the cryophase temperature."),
                  p(strong("Emergence Predictor: "), "Select the temperature and duration for each
               phase (by calendar dates or days). The tool returns the predicted emergence
               date, a protocol schematic, an estimated emergence window, and a
               phase-by-phase summary."),
                  p(strong("Bloom Date Planner: "), "Enter a target bloom date and your bees'
               current status. The tool identifies all cryophase and final thermophase
               combinations that produce emergence on the target date, with the
               recommended protocol (shortest cryophase) highlighted."),
                  hr(),
                  h4("Two-Phase Model"),
                  p("Use this tab when a cryophase is needed but a final thermophase
               is not practical — for example, when the required delay is short
               and development can complete at the cryophase temperature."),
                  p(strong("Emergence Predictor: "), "Specify a starting temperature and duration
               and a cryophase temperature. The tool returns the predicted emergence
               date, a protocol schematic, and an estimated emergence window."),
                  p(strong("Bloom Date Planner: "), "Enter a target bloom date and your bees'
               current status. The tool identifies which cryophase temperatures produce
               emergence on the target date."),
                  p("Note: cryophase temperature options are automatically filtered to
               temperatures below the starting temperature.",
                    style = "color: #666; font-size: 12px; font-style: italic;")
                )
              ),
              fluidRow(
                box(
                  title = "Important Limitations", width = 6,
                  status = "warning", solidHeader = TRUE,
                  tags$ul(
                    tags$li("Predictions are available for integer incubation
                       temperatures from 21 to 31°C. Values 21, 23, 25, 27, 29,
                       and 31°C are experimentally validated; intermediate
                       temperatures are interpolated from the model."),
                    tags$li("The interrupted development model is less reliable when
                       the initial thermophase is less than two weeks and is
                       followed by an extended cryophase (≥2 weeks) at
                       cooler temperatures (21–25°C). Under these
                       conditions the model tends to underpredict emergence
                       time by approximately 11–13 days."),
                    tags$li("Window estimates are also provided for interrupted
                       development protocols but have currently only been
                       validated under a subset of conditions."),
                    tags$li("These are planning tools. Actual emergence timing
                       may vary due to uncontrolled factors such as incubator
                       precision."),
                    tags$li("The two-phase model is not independently validated in this
                       study. The 2022 and 2023 validation experiments used three-phase
                       protocols in which development was completed at a final
                       thermophase temperature, not at a suboptimal cryophase
                       temperature.")
                  )
                ),
                box(
                  title = "Glossary", width = 6,
                  status = "primary", solidHeader = TRUE,
                  tags$dl(
                    tags$dt("Thermophase"),
                    tags$dd("A warm incubation period that promotes development."),
                    tags$dt("Cryophase"),
                    tags$dd("A cool incubation period that slows or pauses development."),
                    tags$dt("Interrupted development"),
                    tags$dd("A multi-phase incubation protocol that alternates thermophase
                            and cryophase periods to control emergence timing."),
                    tags$dt("Degree-day"),
                    tags$dd("A unit of heat accumulation above the lower development threshold.
                            One degree-day is accumulated for each degree above the threshold
                            per day of incubation."),
                    tags$dt("Developmental rate"),
                    tags$dd("The proportion of total development completed per day at a
                            given temperature."),
                    tags$dt("Emergence window"),
                    tags$dd("The range of dates spanning the 10th to 90th percentile of
                            emergence — i.e., the period over which the majority of bees
                            are expected to emerge.")
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Citation & Contact", width = 12,
                  status = "primary", solidHeader = TRUE,
                  p("If you use this tool in your management decisions or research,
               please cite the underlying article:"),
                  p(em("Torson, A.S., Tischleder, M., Yocum, G.D., and Rinehart, J.P.
                  (in prep). A three-phase interrupted development model and web application
                  for predicting timing of Megachile rotundata emergence for synchronization
                  with peak floral bloom."),
                    style = "padding-left: 15px; border-left: 3px solid #EFC000;
                       margin: 10px 0;"),
                  p("Article link: ", em("Available once published."),
                    style = "padding-left: 15px; color: #888;"),
                  hr(),
                  p("For questions or concerns, please contact:"),
                  tags$address(
                    strong("Alex Torson"), " — USDA Agricultural Research Service",
                    tags$br(),
                    tags$a(href = "mailto:Alex.Torson@usda.gov", "Alex.Torson@usda.gov")
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Raw Data", width = 6,
                  status = "primary", solidHeader = TRUE,
                  p("The experimental data underlying the developmental rate models
               in this tool are publicly available through Ag Data Commons,
               the USDA's research data repository."),
                  p(tags$a(href = "#", "Ag Data Commons dataset — link available upon publication.",
                           style = "color: #888; font-style: italic;"))
                ),
                box(
                  title = "Share Your Feedback", width = 6,
                  status = "primary", solidHeader = TRUE,
                  p("Have a suggestion, found a bug, or want to request a new feature?
               We'd love to hear from you. Feedback is reviewed by the development
               team and helps prioritize future improvements."),
                  p(tags$a(href = "#",
                           icon("external-link-alt"), " Open feedback form",
                           style = "color: #888; font-style: italic;"),
                    " — link coming soon.")
                )
              )
      ),
      
      # ---- Standard Degree-Day Predictor Tab ----
      tabItem(tabName = "standard",
              fluidRow(
                box(
                  title = "Standard Development Settings", width = 4,
                  status = "success", solidHeader = TRUE,
                  p("Predict emergence timing for bees held at a single constant
               incubation temperature throughout development. The plot shows
               estimated cumulative emergence (%) over time; predicted dates
               are shown for the 10%, 50%, and 90% emergence milestones."),
                  hr(),
                  h4("Incubation Settings"),
                  selectInput("std_temp", "Incubation Temperature",
                              choices = temp_choices, selected = 29),
                  radioButtons("std_input_mode", "Specify start by:",
                               choices = c("Calendar date" = "date",
                                           "Days from today" = "days"),
                               selected = "date", inline = TRUE),
                  conditionalPanel(
                    condition = "input.std_input_mode == 'date'",
                    dateInput("std_start_date", "Start date", value = Sys.Date())
                  ),
                  conditionalPanel(
                    condition = "input.std_input_mode == 'days'",
                    numericInput("std_days_elapsed",
                                 "Days already at this temperature",
                                 value = 0, min = 0, max = 100)
                  )
                ),
                
                column(width = 8,
                       fluidRow(
                         valueBoxOutput("std_box_p10_date", width = 4),
                         valueBoxOutput("std_box_p50_date", width = 4),
                         valueBoxOutput("std_box_p90_date", width = 4)
                       ),
                       fluidRow(
                         box(
                           title = "Estimated Emergence Window",
                           width = 12, status = "success", solidHeader = TRUE,
                           plotOutput("std_dev_plot", height = "320px")
                         )
                       ),
                       fluidRow(
                         box(
                           title = "Reference: Days to Emergence at Each Temperature",
                           width = 12, status = "primary", solidHeader = TRUE,
                           p("Days to emergence from constant-temperature experiments (Experiments 1 and 2) for the selected model. 10%, 50%, and 90% refer to the proportion of bees emerged at that time point; 50% corresponds to the median. NA in the sample size (n) column indicates temperatures that were not experimentally validated; developmental rates at these temperatures are interpolated from the fitted degree-day model."),
                           tableOutput("std_reference_table")
                         )
                       )
                )
              )
      ),
      
      # ---- Three-Phase Model Tab ----
      tabItem(tabName = "three_phase",
              tabsetPanel(
                
                # ---- Three-Phase Emergence Predictor ----
                tabPanel("Emergence Predictor",
                         fluidRow(
                           box(
                             title = "Protocol Settings", width = 4,
                             status = "success", solidHeader = TRUE,
                             
                             p(icon("info-circle"),
                               "Estimated emergence window (10%, 50%, 90% emerged) is shown below.
               Window estimates for interrupted protocols are derived from
               single-temperature quantile models and are pending independent
               validation.",
                               style = "color: #666; font-size: 12px; font-style: italic;"),
                             hr(),
                             
                             radioButtons("input_mode", "Specify phases by:",
                                          choices = c("Calendar dates"    = "dates",
                                                      "Days in each phase" = "days"),
                                          selected = "dates", inline = TRUE),
                             hr(),
                             
                             h4("Phase 1 — Initial Thermophase"),
                             selectInput("temp1", "Temperature",
                                         choices = temp_choices, selected = 29),
                             conditionalPanel(
                               condition = "input.input_mode == 'dates'",
                               dateInput("start_date", "Start date",
                                         value = Sys.Date()),
                               dateInput("transfer1_date", "Transfer to cryophase",
                                         value = Sys.Date() + 7)
                             ),
                             conditionalPanel(
                               condition = "input.input_mode == 'days'",
                               numericInput("d1_days", "Duration (days)", value = 7, min = 1, max = 60)
                             ),
                             hr(),
                             
                             h4("Phase 2 — Cryophase"),
                             selectInput("temp2", "Temperature",
                                         choices = temp_choices, selected = 25),
                             conditionalPanel(
                               condition = "input.input_mode == 'dates'",
                               dateInput("transfer2_date", "Transfer to final thermophase",
                                         value = Sys.Date() + 14)
                             ),
                             conditionalPanel(
                               condition = "input.input_mode == 'days'",
                               numericInput("d2_days", "Duration (days)", value = 7, min = 1, max = 14)
                             ),
                             hr(),
                             
                             h4("Phase 3 — Final Thermophase"),
                             selectInput("temp3", "Temperature",
                                         choices = temp_choices, selected = 29)
                           ),
                           
                           column(width = 8,
                                  fluidRow(
                                    valueBoxOutput("box_emergence_date", width = 6),
                                    valueBoxOutput("box_days_total", width = 6)
                                  ),
                                  fluidRow(uiOutput("warning_box")),
                                  fluidRow(
                                    box(title = "Protocol Schematic", width = 12,
                                        status = "success", solidHeader = TRUE,
                                        plotOutput("schematic_plot", height = "640px"))
                                  ),
                                  fluidRow(
                                    valueBoxOutput("box_win_p10_date", width = 4),
                                    valueBoxOutput("box_win_p50_date", width = 4),
                                    valueBoxOutput("box_win_p90_date", width = 4)
                                  ),
                                  fluidRow(
                                    box(title = "Estimated Emergence Window", width = 12,
                                        status = "success", solidHeader = TRUE,
                                        plotOutput("pred_window_plot", height = "300px"))
                                  ),
                                  fluidRow(
                                    box(title = "Summary", width = 12,
                                        status = "primary", solidHeader = TRUE,
                                        tableOutput("summary_table"))
                                  )
                           )
                         )
                ),
                
                # ---- Three-Phase Bloom Date Planner ----
                tabPanel("Bloom Date Planner",
                         fluidRow(
                           box(
                             title = "Target Settings", width = 4,
                             status = "success", solidHeader = TRUE,
                             
                             h4("Target Bloom Date"),
                             dateInput("bloom_date", "Peak bloom date", value = Sys.Date() + 21),
                             dateInput("today_date", "Today's date", value = Sys.Date()),
                             hr(),
                             
                             h4("Current Bee Status"),
                             p("Enter the temperature your bees are currently incubating at
               and how many days they have been there."),
                             selectInput("rev_temp1", "Current incubation temperature",
                                         choices = temp_choices, selected = 29),
                             numericInput("rev_d1", "Days already at this temperature",
                                          value = 7, min = 0, max = 60),
                             hr(),
                             
                             h4("Cryophase Temperature Options"),
                             uiOutput("rev_cryo_temp_ui"),
                             hr(),
                             
                             h4("Final Thermophase Temperature Options"),
                             checkboxGroupInput("rev_temp3_options", label = NULL,
                                                choices = temp_choices,
                                                selected = c(27, 29, 31))
                           ),
                           
                           column(width = 8,
                                  fluidRow(
                                    valueBoxOutput("rev_box_days_remaining", width = 6),
                                    valueBoxOutput("rev_box_dev_completed", width = 6)
                                  ),
                                  fluidRow(uiOutput("rev_warning")),
                                  fluidRow(
                                    box(
                                      title = "Recommended Protocol", width = 12,
                                      status = "warning", solidHeader = TRUE,
                                      uiOutput("recommended_protocol")
                                    )
                                  ),
                                  fluidRow(
                                    valueBoxOutput("rev_box_win_p10_date", width = 4),
                                    valueBoxOutput("rev_box_win_p50_date", width = 4),
                                    valueBoxOutput("rev_box_win_p90_date", width = 4)
                                  ),
                                  fluidRow(
                                    box(
                                      title = "All Valid Protocols", width = 12,
                                      status = "primary", solidHeader = TRUE,
                                      p("All cryophase and final thermophase combinations that produce
                   emergence on your target date. The recommended protocol
                   (shortest cryophase) is highlighted."),
                                      tableOutput("reverse_table")
                                    )
                                  )
                           )
                         )
                )
                
              )
      ),
      
      # ---- Two-Phase Model Tab ----
      tabItem(tabName = "two_phase",
              tabsetPanel(
                
                # ---- Two-Phase Emergence Predictor ----
                tabPanel("Emergence Predictor",
                         fluidRow(
                           box(
                             title = "Protocol Settings", width = 4,
                             status = "success", solidHeader = TRUE,
                             
                             p(icon("info-circle"),
                               "This two-phase model is not independently validated in this study.",
                               style = "color: #666; font-size: 12px; font-style: italic;"),
                             hr(),
                             
                             radioButtons("tp_input_mode", "Specify phases by:",
                                          choices = c("Calendar dates"     = "dates",
                                                      "Days in each phase" = "days"),
                                          selected = "dates", inline = TRUE),
                             hr(),
                             
                             h4("Phase 1 \u2014 Initial Thermophase"),
                             selectInput("tp_temp1", "Temperature",
                                         choices = temp_choices, selected = 29),
                             conditionalPanel(
                               condition = "input.tp_input_mode == 'dates'",
                               dateInput("tp_start_date", "Start date",
                                         value = Sys.Date()),
                               dateInput("tp_transfer_date", "Transfer to cryophase",
                                         value = Sys.Date() + 7)
                             ),
                             conditionalPanel(
                               condition = "input.tp_input_mode == 'days'",
                               numericInput("tp_d1_days", "Duration (days)",
                                            value = 7, min = 1, max = 60)
                             ),
                             hr(),
                             
                             h4("Phase 2 \u2014 Cryophase"),
                             uiOutput("tp_temp2_ui")
                           ),
                           
                           column(width = 8,
                                  fluidRow(
                                    valueBoxOutput("tp_box_emergence_date", width = 6),
                                    valueBoxOutput("tp_box_days_total", width = 6)
                                  ),
                                  fluidRow(uiOutput("tp_warning_box")),
                                  fluidRow(
                                    box(title = "Protocol Schematic", width = 12,
                                        status = "success", solidHeader = TRUE,
                                        plotOutput("tp_schematic_plot", height = "640px"))
                                  ),
                                  fluidRow(
                                    valueBoxOutput("tp_box_win_p10_date", width = 4),
                                    valueBoxOutput("tp_box_win_p50_date", width = 4),
                                    valueBoxOutput("tp_box_win_p90_date", width = 4)
                                  ),
                                  fluidRow(
                                    box(title = "Estimated Emergence Window", width = 12,
                                        status = "success", solidHeader = TRUE,
                                        plotOutput("tp_window_plot", height = "300px"))
                                  ),
                                  fluidRow(
                                    box(title = "Summary", width = 12,
                                        status = "primary", solidHeader = TRUE,
                                        tableOutput("tp_summary_table"))
                                  )
                           )
                         )
                ),
                
                # ---- Two-Phase Bloom Date Planner ----
                tabPanel("Bloom Date Planner",
                         fluidRow(
                           box(
                             title = "Target Settings", width = 4,
                             status = "success", solidHeader = TRUE,
                             
                             p(icon("info-circle"),
                               "This two-phase model is not independently validated in this study.",
                               style = "color: #666; font-size: 12px; font-style: italic;"),
                             hr(),
                             
                             h4("Target Bloom Date"),
                             dateInput("tp_bloom_date", "Peak bloom date",
                                       value = Sys.Date() + 21),
                             dateInput("tp_today_date", "Today's date",
                                       value = Sys.Date()),
                             hr(),
                             
                             h4("Current Bee Status"),
                             p("Enter the temperature your bees are currently incubating at
                          and how many days they have been there."),
                             selectInput("tp_rev_temp1", "Current incubation temperature",
                                         choices = temp_choices, selected = 29),
                             numericInput("tp_rev_d1", "Days already at this temperature",
                                          value = 7, min = 0, max = 60),
                             hr(),
                             
                             h4("Cryophase Temperature Options"),
                             uiOutput("tp_rev_cryo_ui")
                           ),
                           
                           column(width = 8,
                                  fluidRow(
                                    valueBoxOutput("tp_rev_box_days_remaining", width = 6),
                                    valueBoxOutput("tp_rev_box_dev_completed", width = 6)
                                  ),
                                  fluidRow(uiOutput("tp_rev_warning")),
                                  fluidRow(
                                    box(
                                      title = "Recommended Protocol", width = 12,
                                      status = "warning", solidHeader = TRUE,
                                      uiOutput("tp_rev_recommended")
                                    )
                                  ),
                                  fluidRow(
                                    box(
                                      title = "All Valid Protocols", width = 12,
                                      status = "primary", solidHeader = TRUE,
                                      p("All cryophase temperatures and durations that produce
                               emergence on your target date. The recommended protocol
                               (warmest cryophase temperature) is highlighted."),
                                      tableOutput("tp_rev_table")
                                    )
                                  )
                           )
                         )
                )
                
              )
      )
      
    )
  )
)

# Server ----------------------------------------------------------------------
# The server function contains all the reactive logic. In Shiny, reactive()
# creates a lazily-evaluated expression that re-runs automatically whenever
# any of its input dependencies change. render*() functions (renderPlot,
# renderTable, renderValueBox, renderUI) populate named outputs defined in the
# UI. req() is used to prevent computation (and error messages) when required
# inputs are not yet set or are invalid.

server <- function(input, output, session) {
  
  # The sex model selector (input$sex_model) is global — it applies to all tools.
  # These two reactives select the correct dev rate and quantile tables based on
  # the user's selection. switch() acts like a lookup: "combined" -> dev_rates_combined,
  # "female" -> dev_rates_female, etc. All downstream reactives call active_dev_rates()
  # or active_quantiles() so they automatically update when the sex model changes.
  active_dev_rates <- reactive({
    switch(input$sex_model,
           combined = dev_rates_combined,
           female = dev_rates_female,
           male = dev_rates_male)
  })
  
  active_quantiles <- reactive({
    switch(input$sex_model,
           combined = quantile_combined,
           female = quantile_female,
           male = quantile_male)
  })
  
  # ---- Standard Degree-Day Reactives ----
  # These reactives drive the standard single-temperature tool.
  
  # Look up the developmental rate for the selected temperature.
  std_rate <- reactive(
    active_dev_rates() %>%
      filter(temperature == as.integer(input$std_temp)) %>%
      pull(devRate)
  )
  
  # Look up the median days to emergence for the selected temperature.
  std_median_days <- reactive(
    active_dev_rates() %>%
      filter(temperature == as.integer(input$std_temp)) %>%
      pull(medianDays)
  )
  
  # Days elapsed since incubation started. The user can specify either a
  # calendar start date (in which case we compute days from start to today)
  # or enter the number of elapsed days directly.
  std_days_elapsed <- reactive({
    if (input$std_input_mode == "date")
      as.numeric(Sys.Date() - input$std_start_date)
    else
      input$std_days_elapsed
  })
  
  # Fraction of development already completed. Capped at 1 (100%) so the
  # "today" marker on the plot doesn't exceed the emergence point.
  std_dev_completed <- reactive({
    min(std_days_elapsed() * std_rate(), 1)
  })
  
  # Look up percentile-specific days to emergence from the quantile table
  # for the selected temperature. These are the predicted total days from
  # incubation start to the 10th, 50th, and 90th percentile of emergence.
  std_p10_days <- reactive(
    active_quantiles() %>%
      filter(temperature == as.integer(input$std_temp), tau == "p10") %>%
      pull(days)
  )
  std_p50_days <- reactive(
    active_quantiles() %>%
      filter(temperature == as.integer(input$std_temp), tau == "p50") %>%
      pull(days)
  )
  std_p90_days <- reactive(
    active_quantiles() %>%
      filter(temperature == as.integer(input$std_temp), tau == "p90") %>%
      pull(days)
  )
  
  # Reconstruct the incubation start date. If the user entered a start date
  # directly, use it. If they entered elapsed days, back-calculate from today.
  std_start_dt <- reactive({
    if (input$std_input_mode == "date") input$std_start_date
    else Sys.Date() - std_days_elapsed()
  })
  
  # Add the percentile-specific days (rounded to whole days) to the start date
  # to get the predicted calendar date for each emergence milestone.
  std_p10_date <- reactive({ std_start_dt() + round(std_p10_days()) })
  std_p50_date <- reactive({ std_start_dt() + round(std_p50_days()) })
  std_p90_date <- reactive({ std_start_dt() + round(std_p90_days()) })
  
  output$std_box_p10_date <- renderValueBox({
    valueBox(
      value = format(std_p10_date(), "%b %d, %Y"),
      subtitle = "First 10% Emerged",
      icon = icon("calendar"),
      color = "green"
    )
  })
  
  output$std_box_p50_date <- renderValueBox({
    valueBox(
      value = format(std_p50_date(), "%b %d, %Y"),
      subtitle = "Median Emergence (50th %ile)",
      icon = icon("calendar-check"),
      color = "green"
    )
  })
  
  output$std_box_p90_date <- renderValueBox({
    valueBox(
      value = format(std_p90_date(), "%b %d, %Y"),
      subtitle = "90% Emerged",
      icon = icon("calendar"),
      color = "yellow"
    )
  })
  
  output$std_dev_plot <- renderPlot(res = 120, {
    p10d <- round(std_p10_days())
    p50d <- round(std_p50_days())
    p90d <- round(std_p90_days())
    elapsed <- std_days_elapsed()
    
    # The emergence window is displayed as a logistic CDF curve. 
    # We fit a logistic curve through the three known percentile points
    # (p10, p50, p90). The logistic CDF is: P(t) = 1 / (1 + exp(-(t-mu)/s)).
    # mu = p50 (the median, i.e. the inflection point of the curve).
    # s = scale parameter estimated from the distances between p50 and the tails,
    # using the identity that for a logistic, p90 - p50 = s * log(9).
    # We average the two tail-based s estimates.
    mu <- p50d
    s <- mean(c((p50d - p10d) / log(9), (p90d - p50d) / log(9)))
    
    day_seq <- seq(0, p90d + 5, 0.5)
    df_cdf <- tibble(
      day = day_seq,
      pct = 1 / (1 + exp(-(day_seq - mu) / s)) * 100
    )
    
    # Cumulative emergence at elapsed days from logistic CDF
    pct_today <- 1 / (1 + exp(-(elapsed - mu) / s)) * 100
    
    ggplot() +
      annotate("rect",
               xmin = p10d, xmax = p90d, ymin = -Inf, ymax = Inf,
               fill = "grey60", alpha = 0.15) +
      geom_vline(xintercept = c(p10d, p50d, p90d),
                 linetype = c("dashed", "solid", "dashed"),
                 linewidth = 0.6,
                 color = c("#444444", jco1, "#444444")) +
      annotate("text", x = p10d - 0.25, y = 107,
               label = "10%", hjust = 1, size = 3, color = "black") +
      annotate("text", x = p50d + 0.25, y = 107,
               label = "50%", hjust = 0, size = 3, color = "black") +
      annotate("text", x = p90d + 0.25, y = 107,
               label = "90%", hjust = 0, size = 3, color = "black") +
      geom_line(data = df_cdf, aes(x = day, y = pct),
                color = jco1, linewidth = 1.2) +
      { if (elapsed > 0 && elapsed < p90d)
        list(
          annotate("point", x = elapsed, y = pct_today,
                   size = 4, shape = 21,
                   fill = jco4, color = "black", stroke = 0.8),
          annotate("text",
                   x = elapsed + (p90d + 5) * 0.02,
                   y = pct_today,
                   label = paste0("Today\n(", round(pct_today, 1), "%)"),
                   hjust = 0, color = jco4, size = 3.5)
        )
      } +
      scale_x_continuous(limits = c(0, p90d + 6),
                         breaks = pretty(0:(p90d + 5), n = 8)) +
      scale_y_continuous(limits = c(0, 112),
                         breaks = seq(0, 100, 25),
                         labels = paste0(seq(0, 100, 25), "%")) +
      alex_theme +
      xlab("Days from Start") +
      ylab("Cumulative Emergence") +
      labs(title = paste0("Estimated Emergence Window at Constant ", input$std_temp, "°C"))
  })
  
  output$std_reference_table <- renderTable({
    qs <- active_quantiles()
    active_dev_rates() %>%
      left_join(qs %>% filter(tau == "p10") %>% select(temperature, p10 = days),
                by = "temperature") %>%
      left_join(qs %>% filter(tau == "p50") %>% select(temperature, p50 = days),
                by = "temperature") %>%
      left_join(qs %>% filter(tau == "p90") %>% select(temperature, p90 = days),
                by = "temperature") %>%
      transmute(
        `Temperature (°C)` = temperature,
        `Median Days` = medianDays,
        `10%` = round(p10, 1),
        `50%` = round(p50, 1),
        `90%` = round(p90, 1),
        `Dev. Rate (day⁻¹)` = round(devRate, 6),
        `n` = n
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # ---- Predictor Reactives ----
  # Compute phase durations d1 and d2 from the user inputs. The user can enter
  # either calendar dates for each phase transition (in which case we compute
  # duration as the difference between dates) or the number of days directly.
  
  d1 <- reactive({
    if (input$input_mode == "dates")
      as.numeric(input$transfer1_date - input$start_date)
    else input$d1_days
  })
  
  d2 <- reactive({
    if (input$input_mode == "dates")
      as.numeric(input$transfer2_date - input$transfer1_date)
    else input$d2_days
  })
  
  # Incubation start date, used to convert predicted total days to a calendar date.
  start_dt <- reactive({
    if (input$input_mode == "dates") input$start_date else Sys.Date()
  })
  
  # Developmental rates for each of the three phases, looked up from the active
  # table by the selected temperature for that phase.
  r1 <- reactive(
    active_dev_rates() %>% filter(temperature == as.integer(input$temp1)) %>% pull(devRate)
  )
  r2 <- reactive(
    active_dev_rates() %>% filter(temperature == as.integer(input$temp2)) %>% pull(devRate)
  )
  r3 <- reactive(
    active_dev_rates() %>% filter(temperature == as.integer(input$temp3)) %>% pull(devRate)
  )
  
  # Percentile-specific rates for emergence window propagation through three-phase model.
  # The quantile table stores predicted days at each percentile and temperature.
  # Converting to a rate (1/days) allows the same predict_emergence() function to
  # be applied at each percentile, propagating the window through all three phases.
  get_prate <- function(temp, tau_val) {
    days <- active_quantiles() %>%
      filter(temperature == as.integer(temp), tau == tau_val) %>%
      pull(days)
    1 / days
  }
  
  r1_p10 <- reactive({ get_prate(input$temp1, "p10") })
  r1_p50 <- reactive({ get_prate(input$temp1, "p50") })
  r1_p90 <- reactive({ get_prate(input$temp1, "p90") })
  
  r2_p10 <- reactive({ get_prate(input$temp2, "p10") })
  r2_p50 <- reactive({ get_prate(input$temp2, "p50") })
  r2_p90 <- reactive({ get_prate(input$temp2, "p90") })
  
  r3_p10 <- reactive({ get_prate(input$temp3, "p10") })
  r3_p50 <- reactive({ get_prate(input$temp3, "p50") })
  r3_p90 <- reactive({ get_prate(input$temp3, "p90") })
  
  # Run predict_emergence() at each percentile to produce the emergence window.
  # req() prevents execution if d1 or d2 are zero or missing (e.g., if the user
  # has not yet entered all phase dates).
  win_p10 <- reactive({
    req(d1() > 0, d2() > 0)
    predict_emergence(r1_p10(), r2_p10(), r3_p10(), d1(), d2())
  })
  win_p50 <- reactive({
    req(d1() > 0, d2() > 0)
    predict_emergence(r1_p50(), r2_p50(), r3_p50(), d1(), d2())
  })
  win_p90 <- reactive({
    req(d1() > 0, d2() > 0)
    predict_emergence(r1_p90(), r2_p90(), r3_p90(), d1(), d2())
  })
  
  # Convert predicted total days (from start) to calendar dates for each
  # percentile. days() is from lubridate and handles date arithmetic correctly.
  win_p10_date <- reactive({ start_dt() + days(win_p10()$pred_total) })
  win_p50_date <- reactive({ start_dt() + days(win_p50()$pred_total) })
  win_p90_date <- reactive({ start_dt() + days(win_p90()$pred_total) })
  
  # Run the model at the median (combined/sex-specific) rate to get the main
  # point prediction, which populates the primary emergence date value box.
  result <- reactive({
    req(d1() > 0, d2() > 0)
    predict_emergence(r1(), r2(), r3(), d1(), d2())
  })
  
  emergence_date <- reactive({ start_dt() + days(result()$pred_total) })
  
  output$box_emergence_date <- renderValueBox({
    valueBox(
      value = format(emergence_date(), "%B %d, %Y"),
      subtitle = "Predicted Emergence Date",
      icon = icon("calendar-check"),
      color = if (result()$valid) "green" else "red"
    )
  })
  
  output$box_days_total <- renderValueBox({
    valueBox(
      value = paste0(result()$pred_total, " days"),
      subtitle = "Total Days to Emergence",
      icon = icon("clock"),
      color = if (result()$valid) "green" else "red"
    )
  })
  
  output$box_win_p10_date <- renderValueBox({
    valueBox(
      value = format(win_p10_date(), "%b %d, %Y"),
      subtitle = "10% Emerged (est. window)",
      icon = icon("calendar"),
      color = "green"
    )
  })
  
  output$box_win_p50_date <- renderValueBox({
    valueBox(
      value = format(win_p50_date(), "%b %d, %Y"),
      subtitle = "50% Emerged (est. window)",
      icon = icon("calendar-check"),
      color = "green"
    )
  })
  
  output$box_win_p90_date <- renderValueBox({
    valueBox(
      value = format(win_p90_date(), "%b %d, %Y"),
      subtitle = "90% Emerged (est. window)",
      icon = icon("calendar"),
      color = "yellow"
    )
  })
  
  # renderUI() is used here instead of a static UI element because the warning
  # box should only appear when the model is invalid. Returning nothing (no
  # output) from renderUI() removes the element from the page entirely.
  output$warning_box <- renderUI({
    if (!result()$valid)
      fluidRow(box(width = 12, background = "red",
                   icon("exclamation-triangle"), strong(" Warning: "),
                   "Development exceeds 100% before the final thermophase.
         Reduce Phase 1 or Phase 2 duration, or lower the temperatures."))
  })
  
  output$summary_table <- renderTable({
    tibble(
      Phase = c("Phase 1 — Initial Thermophase",
                "Phase 2 — Cryophase",
                "Phase 3 — Final Thermophase",
                "Total"),
      `Temperature (°C)` = c(input$temp1, input$temp2, input$temp3, "—"),
      `Duration (days)` = c(d1(), d2(), result()$days_final, result()$pred_total),
      `Development Accumulated` = c(
        sprintf("%.1f%%", result()$dev1    * 100),
        sprintf("%.1f%%", result()$dev2    * 100),
        sprintf("%.1f%%", result()$rem_dev * 100),
        "100.0%"
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$schematic_plot <- renderPlot(res = 120, {
    d1v <- d1(); d2v <- d2(); d3v <- result()$days_final
    t1 <- as.integer(input$temp1)
    t2 <- as.integer(input$temp2)
    t3 <- as.integer(input$temp3)
    total <- d1v + d2v + d3v
    
    # Step-function data for the temperature-vs-time panel. Each phase transition
    # requires two rows at the same x (time) to create a vertical step rather
    # than a diagonal line between the two temperatures.
    df_temp <- tibble(
      time = c(0, d1v, d1v, d1v + d2v, d1v + d2v, total),
      temp = c(t1, t1, t2, t2, t3, t3)
    )
    # Same step structure for the cumulative development panel. dev values are
    # scaled to percentages (0-100) for readability.
    df_dev <- tibble(
      time = c(0, d1v, d1v, d1v + d2v, d1v + d2v, total),
      dev = c(0, result()$dev1, result()$dev1,
              result()$total_dev, result()$total_dev, 1) * 100
    )
    
    phase_bands <- tibble(
      xmin = c(0, d1v, d1v + d2v),
      xmax = c(d1v, d1v + d2v, total),
      label = c("Phase 1", "Phase 2", "Phase 3"),
      fill = c("#CCE0F5", "#FDEFC3", "#F5CCCA")
    )
    
    p_temp <- ggplot() +
      geom_rect(data = phase_bands,
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf,
                    fill = label),
                alpha = 0.3, inherit.aes = FALSE) +
      scale_fill_manual(values = setNames(phase_bands$fill, phase_bands$label)) +
      geom_line(data = df_temp, aes(x = time, y = temp),
                linewidth = 1, color = jco4) +
      geom_vline(xintercept = c(d1v, d1v + d2v),
                 linetype = "dashed", color = "grey60", linewidth = 0.5) +
      scale_x_continuous(limits = c(0, total),
                         breaks = c(0, d1v, d1v + d2v, total)) +
      alex_theme +
      theme(legend.position = "none",
            plot.margin = margin(t = 5, r = 5, b = 0, l = 5)) +
      xlab(NULL) + ylab("Temperature (°C)") +
      labs(title = "Three-Phase Incubation Protocol")
    
    p_dev <- ggplot() +
      geom_rect(data = phase_bands,
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf,
                    fill = label),
                alpha = 0.3, inherit.aes = FALSE) +
      scale_fill_manual(values = setNames(phase_bands$fill, phase_bands$label),
                        name = "Phase") +
      geom_hline(yintercept = 100, linetype = "dotted",
                 color = "grey40", linewidth = 0.5) +
      geom_line(data = df_dev, aes(x = time, y = dev),
                linewidth = 1, color = jco1) +
      geom_vline(xintercept = c(d1v, d1v + d2v),
                 linetype = "dashed", color = "grey60", linewidth = 0.5) +
      scale_x_continuous(limits = c(0, total),
                         breaks = c(0, d1v, d1v + d2v, total)) +
      scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 25)) +
      alex_theme +
      theme(legend.position = "right",
            plot.margin = margin(t = 0, r = 5, b = 5, l = 5)) +
      xlab("Days from Start") + ylab("Development Complete (%)")
    
    p_temp / p_dev
  })
  
  output$pred_window_plot <- renderPlot(res = 120, {
    p10d <- win_p10()$pred_total
    p50d <- win_p50()$pred_total
    p90d <- win_p90()$pred_total
    
    mu <- p50d
    s <- mean(c((p50d - p10d) / log(9), (p90d - p50d) / log(9)))
    
    day_seq <- seq(0, p90d + 5, 0.5)
    df_cdf <- tibble(
      day = day_seq,
      pct = 1 / (1 + exp(-(day_seq - mu) / s)) * 100
    )
    
    ggplot() +
      annotate("rect",
               xmin = p10d, xmax = p90d, ymin = -Inf, ymax = Inf,
               fill = "grey60", alpha = 0.15) +
      geom_vline(xintercept = c(p10d, p50d, p90d),
                 linetype = c("dashed", "solid", "dashed"),
                 linewidth = 0.6,
                 color = c("#444444", jco1, "#444444")) +
      annotate("text", x = p10d - 0.25, y = 107,
               label = "10%", hjust = 1, size = 3, color = "black") +
      annotate("text", x = p50d + 0.25, y = 107,
               label = "50%", hjust = 0, size = 3, color = "black") +
      annotate("text", x = p90d + 0.25, y = 107,
               label = "90%", hjust = 0, size = 3, color = "black") +
      geom_line(data = df_cdf, aes(x = day, y = pct),
                color = jco1, linewidth = 1.2) +
      scale_x_continuous(limits = c(0, p90d + 6),
                         breaks = pretty(0:(p90d + 5), n = 8)) +
      scale_y_continuous(limits = c(0, 112),
                         breaks = seq(0, 100, 25),
                         labels = paste0(seq(0, 100, 25), "%")) +
      alex_theme +
      xlab("Days from Start") +
      ylab("Cumulative Emergence") +
      labs(title = "Estimated Emergence Window")
  })
  
  # ---- Reverse Calculator Reactives ----
  # The bloom date planner works in reverse: given a target bloom date and the
  # bees' current incubation status, find all three-phase protocols that produce
  # emergence exactly on the target date.
  
  # Developmental rate at the current (Phase 1) temperature.
  rev_r1 <- reactive(
    active_dev_rates() %>%
      filter(temperature == as.integer(input$rev_temp1)) %>%
      pull(devRate)
  )
  
  # Fraction of development already completed at Phase 1 (days * rate).
  rev_dev_completed <- reactive({ input$rev_d1 * rev_r1() })
  
  # Days remaining until the target bloom date.
  rev_days_remaining <- reactive({ as.numeric(input$bloom_date - input$today_date) })
  
  # Call reverse_calculate() with the current state and temperature options.
  # The result is then filtered to only include the cryophase and final
  # thermophase temperatures the user has selected via the checkboxes.
  rev_results <- reactive({
    req(length(input$rev_temp2_options) > 0,
        length(input$rev_temp3_options) > 0)
    t2_opts <- as.integer(input$rev_temp2_options)
    t3_opts <- as.integer(input$rev_temp3_options)
    all_temps <- sort(unique(c(t2_opts, t3_opts)))
    reverse_calculate(
      dev_already = rev_dev_completed(),
      days_remaining = rev_days_remaining(),
      temp_choices = all_temps,
      rates = active_dev_rates()
    ) %>%
      filter(cryo_temp %in% t2_opts, final_temp %in% t3_opts)
  })
  
  output$rev_box_days_remaining <- renderValueBox({
    valueBox(
      value = paste0(rev_days_remaining(), " days"),
      subtitle = paste0("Days Until Bloom (", format(input$bloom_date, "%b %d"), ")"),
      icon = icon("seedling"),
      color = if (rev_days_remaining() > 0) "green" else "red"
    )
  })
  
  output$rev_box_dev_completed <- renderValueBox({
    valueBox(
      value = sprintf("%.1f%%", rev_dev_completed() * 100),
      subtitle = paste0("Development Completed at ", input$rev_temp1, "°C"),
      icon = icon("thermometer-half"),
      color = "yellow"
    )
  })
  
  output$rev_warning <- renderUI({
    if (rev_days_remaining() <= 0)
      fluidRow(box(width = 12, background = "red",
                   icon("exclamation-triangle"), strong(" Warning: "),
                   "Bloom date is in the past or today. Please update your target date."))
    else if (rev_dev_completed() >= 1)
      fluidRow(box(width = 12, background = "red",
                   icon("exclamation-triangle"), strong(" Warning: "),
                   "Bees have already completed 100% development at Phase 1 temperature."))
    else if (is.null(rev_results()) || nrow(rev_results()) == 0)
      fluidRow(box(width = 12, background = "yellow",
                   icon("exclamation-triangle"), strong(" No valid protocols found. "),
                   "No combination of the selected temperatures produces emergence exactly
         on your target date. Try adjusting the bloom date, Phase 1 duration,
         or temperature options."))
  })
  
  output$recommended_protocol <- renderUI({
    res <- rev_results()
    if (is.null(res) || nrow(res) == 0) {
      p("No valid protocols found with the current settings.")
    } else {
      best <- res %>% arrange(cryo_days, desc(final_temp)) %>% slice(1)
      tagList(
        p(strong("Recommended protocol (shortest cryophase):"),
          style = "font-size: 15px;"),
        tags$ul(
          tags$li(strong("Cryophase: "), best$cryo_days, " days at ",
                  best$cryo_temp, "°C"),
          tags$li(strong("Final thermophase: "), best$final_days, " days at ",
                  best$final_temp, "°C"),
          tags$li(strong("Development at start of final phase: "),
                  sprintf("%.1f%%", best$dev_at_final))
        ),
        p("Transfer bees to the cryophase temperature today, then to the final
           thermophase temperature after ", strong(best$cryo_days, " days"), ".",
          style = "color: #003C67;")
      )
    }
  })
  
  # Emergence window for recommended protocol ----------------------------------
  # The recommended protocol is the valid result with the shortest cryophase
  # (ties broken by highest final thermophase temperature). Once identified,
  # we re-run predict_emergence() at all three percentile rates using that
  # protocol's specific cryo_temp and cryo_days to estimate the emergence window.
  
  rev_best <- reactive({
    res <- rev_results()
    if (is.null(res) || nrow(res) == 0) return(NULL)
    res %>% arrange(cryo_days, desc(final_temp)) %>% slice(1)
  })
  
  rev_win <- reactive({
    best <- rev_best()
    req(!is.null(best))
    # Helper to convert quantile days to rate for a given temp and percentile.
    get_prate_rev <- function(temp, tau_val) {
      days <- active_quantiles() %>%
        filter(temperature == as.integer(temp), tau == tau_val) %>%
        pull(days)
      1 / days
    }
    list(
      p10 = predict_emergence(
        get_prate_rev(input$rev_temp1, "p10"),
        get_prate_rev(best$cryo_temp, "p10"),
        get_prate_rev(best$final_temp, "p10"),
        input$rev_d1, best$cryo_days
      ),
      p50 = predict_emergence(
        get_prate_rev(input$rev_temp1, "p50"),
        get_prate_rev(best$cryo_temp, "p50"),
        get_prate_rev(best$final_temp, "p50"),
        input$rev_d1, best$cryo_days
      ),
      p90 = predict_emergence(
        get_prate_rev(input$rev_temp1, "p90"),
        get_prate_rev(best$cryo_temp, "p90"),
        get_prate_rev(best$final_temp, "p90"),
        input$rev_d1, best$cryo_days
      )
    )
  })
  
  rev_start_dt <- reactive({ input$today_date - input$rev_d1 })
  
  output$rev_box_win_p10_date <- renderValueBox({
    w <- rev_win()
    req(!is.null(w))
    valueBox(
      value = format(rev_start_dt() + w$p10$pred_total, "%b %d, %Y"),
      subtitle = "10% Emerged (est. window)",
      icon = icon("calendar"),
      color = "green"
    )
  })
  
  output$rev_box_win_p50_date <- renderValueBox({
    w <- rev_win()
    req(!is.null(w))
    valueBox(
      value = format(rev_start_dt() + w$p50$pred_total, "%b %d, %Y"),
      subtitle = "50% Emerged (est. window)",
      icon = icon("calendar-check"),
      color = "green"
    )
  })
  
  output$rev_box_win_p90_date <- renderValueBox({
    w <- rev_win()
    req(!is.null(w))
    valueBox(
      value = format(rev_start_dt() + w$p90$pred_total, "%b %d, %Y"),
      subtitle = "90% Emerged (est. window)",
      icon = icon("calendar"),
      color = "yellow"
    )
  })
  
  output$reverse_table <- renderTable({
    res <- rev_results()
    if (is.null(res) || nrow(res) == 0) return(NULL)
    best <- res %>% arrange(cryo_days, desc(final_temp)) %>% slice(1)
    res %>%
      arrange(cryo_days, desc(final_temp)) %>%
      mutate(
        Recommended = if_else(
          cryo_temp == best$cryo_temp &
            cryo_days == best$cryo_days &
            final_temp == best$final_temp,
          "★ Recommended", ""
        )
      ) %>%
      rename(
        `Cryo Temp (°C)` = cryo_temp,
        `Cryo Duration (days)` = cryo_days,
        `Final Temp (°C)` = final_temp,
        `Final Duration (days)` = final_days,
        `Dev. at Final Phase` = dev_at_final
      ) %>%
      mutate(`Dev. at Final Phase` = paste0(`Dev. at Final Phase`, "%"))
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # ---- Reactive cryo temp UI: Three-Phase Bloom Date Planner ----
  # The cryophase temperature options are rendered dynamically (renderUI) rather
  # than as a static UI element because the valid choices depend on the current
  # Phase 1 temperature: cryophase must be strictly below the starting temperature.
  # When the user changes rev_temp1, this output re-renders with the updated list.
  # Default selection is temperatures <= 25°C (the experimentally validated range).
  
  output$rev_cryo_temp_ui <- renderUI({
    max_cryo <- as.integer(input$rev_temp1) - 1
    valid <- temp_choices[temp_choices < max_cryo + 1]
    selected <- valid[valid <= 25]
    checkboxGroupInput("rev_temp2_options", label = NULL,
                       choices = valid,
                       selected = selected)
  })
  
  # ---- Two-Phase Reactives ----
  
  # Dynamic cryo temp dropdowns for the two-phase tools. Same logic as the three-
  # phase planner: cryophase must be below the starting temperature. tp_temp2_ui
  # is a single selectInput (not a checkbox group) since the emergence predictor
  # uses one specific cryophase temperature at a time.
  
  output$tp_temp2_ui <- renderUI({
    max_cryo <- as.integer(input$tp_temp1) - 1
    valid <- temp_choices[temp_choices <= max_cryo]
    selected <- if (27 %in% valid) 27 else max(valid)
    selectInput("tp_temp2", "Temperature", choices = valid, selected = selected)
  })
  
  output$tp_rev_cryo_ui <- renderUI({
    max_cryo <- as.integer(input$tp_rev_temp1) - 1
    valid <- temp_choices[temp_choices <= max_cryo]
    selected <- valid[valid <= 25]
    checkboxGroupInput("tp_rev_cryo_opts", label = NULL,
                       choices = valid,
                       selected = selected)
  })
  
  # -- Emergence Predictor --
  
  tp_r1 <- reactive({
    active_dev_rates() %>%
      filter(temperature == as.integer(input$tp_temp1)) %>%
      pull(devRate)
  })
  
  tp_r2 <- reactive({
    req(input$tp_temp2)
    active_dev_rates() %>%
      filter(temperature == as.integer(input$tp_temp2)) %>%
      pull(devRate)
  })
  
  tp_d1 <- reactive({
    if (input$tp_input_mode == "dates")
      as.numeric(input$tp_transfer_date - input$tp_start_date)
    else
      input$tp_d1_days
  })
  
  tp_start_dt <- reactive({
    if (input$tp_input_mode == "dates") input$tp_start_date else Sys.Date()
  })
  
  tp_result <- reactive({
    req(tp_d1() > 0, input$tp_temp2)
    predict_emergence_2phase(tp_r1(), tp_r2(), tp_d1())
  })
  
  tp_emergence_date <- reactive({ tp_start_dt() + tp_result()$pred_total })
  
  # Percentile window rates
  tp_get_prate <- function(temp, tau_val) {
    req(temp)
    days <- active_quantiles() %>%
      filter(temperature == as.integer(temp), tau == tau_val) %>%
      pull(days)
    1 / days
  }
  
  tp_win_p10 <- reactive({
    req(input$tp_temp2)
    predict_emergence_2phase(
      tp_get_prate(input$tp_temp1, "p10"),
      tp_get_prate(input$tp_temp2, "p10"),
      tp_d1()
    )
  })
  tp_win_p50 <- reactive({
    req(input$tp_temp2)
    predict_emergence_2phase(
      tp_get_prate(input$tp_temp1, "p50"),
      tp_get_prate(input$tp_temp2, "p50"),
      tp_d1()
    )
  })
  tp_win_p90 <- reactive({
    req(input$tp_temp2)
    predict_emergence_2phase(
      tp_get_prate(input$tp_temp1, "p90"),
      tp_get_prate(input$tp_temp2, "p90"),
      tp_d1()
    )
  })
  
  output$tp_box_emergence_date <- renderValueBox({
    valueBox(
      value = format(tp_emergence_date(), "%B %d, %Y"),
      subtitle = "Predicted Emergence Date",
      icon = icon("calendar-check"),
      color = if (tp_result()$valid) "green" else "red"
    )
  })
  
  output$tp_box_days_total <- renderValueBox({
    valueBox(
      value = paste0(tp_result()$pred_total, " days"),
      subtitle = "Total Days to Emergence",
      icon = icon("clock"),
      color = if (tp_result()$valid) "green" else "red"
    )
  })
  
  output$tp_box_win_p10_date <- renderValueBox({
    valueBox(
      value = format(tp_start_dt() + tp_win_p10()$pred_total, "%b %d, %Y"),
      subtitle = "10% Emerged (est. window)",
      icon = icon("calendar"),
      color = "green"
    )
  })
  
  output$tp_box_win_p50_date <- renderValueBox({
    valueBox(
      value = format(tp_start_dt() + tp_win_p50()$pred_total, "%b %d, %Y"),
      subtitle = "50% Emerged (est. window)",
      icon = icon("calendar-check"),
      color = "green"
    )
  })
  
  output$tp_box_win_p90_date <- renderValueBox({
    valueBox(
      value = format(tp_start_dt() + tp_win_p90()$pred_total, "%b %d, %Y"),
      subtitle = "90% Emerged (est. window)",
      icon = icon("calendar"),
      color = "yellow"
    )
  })
  
  output$tp_warning_box <- renderUI({
    if (!tp_result()$valid)
      fluidRow(box(width = 12, background = "red",
                   icon("exclamation-triangle"), strong(" Warning: "),
                   "Development exceeds 100% before the cryophase.
           Reduce Phase 1 duration or lower the starting temperature."))
  })
  
  output$tp_summary_table <- renderTable({
    tibble(
      Phase = c("Phase 1 \u2014 Initial Thermophase",
                "Phase 2 \u2014 Cryophase",
                "Total"),
      `Temperature (°C)` = c(input$tp_temp1, input$tp_temp2, "\u2014"),
      `Duration (days)` = c(tp_d1(), tp_result()$days_cryo, tp_result()$pred_total),
      `Development Accumulated` = c(
        sprintf("%.1f%%", tp_result()$dev1    * 100),
        sprintf("%.1f%%", tp_result()$rem_dev * 100),
        "100.0%"
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$tp_schematic_plot <- renderPlot(res = 120, {
    req(input$tp_temp2)
    d1v <- tp_d1()
    d2v <- tp_result()$days_cryo
    t1 <- as.integer(input$tp_temp1)
    t2 <- as.integer(input$tp_temp2)
    total <- d1v + d2v
    
    df_temp <- tibble(
      time = c(0, d1v, d1v, total),
      temp = c(t1, t1, t2, t2)
    )
    df_dev <- tibble(
      time = c(0, d1v, d1v, total),
      dev = c(0, tp_result()$dev1, tp_result()$dev1, 1) * 100
    )
    
    phase_bands <- tibble(
      xmin = c(0, d1v),
      xmax = c(d1v, total),
      label = c("Phase 1", "Phase 2"),
      fill = c("#CCE0F5", "#FDEFC3")
    )
    
    p_temp <- ggplot() +
      geom_rect(data = phase_bands,
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf,
                    fill = label),
                alpha = 0.3, inherit.aes = FALSE) +
      scale_fill_manual(values = setNames(phase_bands$fill, phase_bands$label)) +
      geom_line(data = df_temp, aes(x = time, y = temp),
                linewidth = 1, color = jco4) +
      geom_vline(xintercept = d1v,
                 linetype = "dashed", color = "grey60", linewidth = 0.5) +
      scale_x_continuous(limits = c(0, total),
                         breaks = c(0, d1v, total)) +
      alex_theme +
      theme(legend.position = "none",
            plot.margin = margin(t = 5, r = 5, b = 0, l = 5)) +
      xlab(NULL) + ylab("Temperature (\u00b0C)") +
      labs(title = "Two-Phase Incubation Protocol")
    
    p_dev <- ggplot() +
      geom_rect(data = phase_bands,
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf,
                    fill = label),
                alpha = 0.3, inherit.aes = FALSE) +
      scale_fill_manual(values = setNames(phase_bands$fill, phase_bands$label),
                        name = "Phase") +
      geom_hline(yintercept = 100, linetype = "dotted",
                 color = "grey40", linewidth = 0.5) +
      geom_line(data = df_dev, aes(x = time, y = dev),
                linewidth = 1, color = jco1) +
      geom_vline(xintercept = d1v,
                 linetype = "dashed", color = "grey60", linewidth = 0.5) +
      scale_x_continuous(limits = c(0, total),
                         breaks = c(0, d1v, total)) +
      scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 25)) +
      alex_theme +
      theme(legend.position = "right",
            plot.margin = margin(t = 0, r = 5, b = 5, l = 5)) +
      xlab("Days from Start") + ylab("Development Complete (%)")
    
    p_temp / p_dev
  })
  
  output$tp_window_plot <- renderPlot(res = 120, {
    p10d <- tp_win_p10()$pred_total
    p50d <- tp_win_p50()$pred_total
    p90d <- tp_win_p90()$pred_total
    
    mu <- p50d
    s <- mean(c((p50d - p10d) / log(9), (p90d - p50d) / log(9)))
    
    day_seq <- seq(0, p90d + 5, 0.5)
    df_cdf <- tibble(
      day = day_seq,
      pct = 1 / (1 + exp(-(day_seq - mu) / s)) * 100
    )
    
    ggplot() +
      annotate("rect",
               xmin = p10d, xmax = p90d, ymin = -Inf, ymax = Inf,
               fill = "grey60", alpha = 0.15) +
      geom_vline(xintercept = c(p10d, p50d, p90d),
                 linetype = c("dashed", "solid", "dashed"),
                 linewidth = 0.6,
                 color = c("#444444", jco1, "#444444")) +
      annotate("text", x = p10d - 0.25, y = 107,
               label = "10%", hjust = 1, size = 3, color = "black") +
      annotate("text", x = p50d + 0.25, y = 107,
               label = "50%", hjust = 0, size = 3, color = "black") +
      annotate("text", x = p90d + 0.25, y = 107,
               label = "90%", hjust = 0, size = 3, color = "black") +
      geom_line(data = df_cdf, aes(x = day, y = pct),
                color = jco1, linewidth = 1.2) +
      scale_x_continuous(limits = c(0, p90d + 6),
                         breaks = pretty(0:(p90d + 5), n = 8)) +
      scale_y_continuous(limits = c(0, 112),
                         breaks = seq(0, 100, 25),
                         labels = paste0(seq(0, 100, 25), "%")) +
      alex_theme +
      xlab("Days from Start") +
      ylab("Cumulative Emergence") +
      labs(title = "Estimated Emergence Window")
  })
  
  # -- Bloom Date Planner --
  # Two-phase reverse calculator. Simpler than the three-phase version: for each
  # selected cryophase temperature, compute how many days at that rate are needed
  # to complete the remaining development after Phase 1. If that equals the number
  # of days until bloom, the protocol is valid. Results are sorted by cryophase
  # temperature (warmest first, as this is the least stressful option).
  
  tp_rev_r1 <- reactive({
    active_dev_rates() %>%
      filter(temperature == as.integer(input$tp_rev_temp1)) %>%
      pull(devRate)
  })
  
  tp_rev_dev_completed <- reactive({ input$tp_rev_d1 * tp_rev_r1() })
  
  tp_rev_days_remaining <- reactive({
    as.numeric(input$tp_bloom_date - input$tp_today_date)
  })
  
  tp_rev_results <- reactive({
    req(length(input$tp_rev_cryo_opts) > 0)
    dev_already <- tp_rev_dev_completed()
    days_remaining <- tp_rev_days_remaining()
    if (days_remaining <= 0 || dev_already >= 1) return(NULL)
    
    cryo_temps <- as.integer(input$tp_rev_cryo_opts)
    results <- map_dfr(cryo_temps, function(ct) {
      r2 <- active_dev_rates() %>% filter(temperature == ct) %>% pull(devRate)
      days_needed <- round((1 - dev_already) / r2, 0)
      if (days_needed == days_remaining) {
        tibble(
          cryo_temp = ct,
          cryo_days = days_needed,
          dev_at_cryo = round((dev_already + days_needed * r2) * 100, 1)
        )
      }
    })
    if (is.null(results) || nrow(results) == 0) return(NULL)
    results %>% arrange(desc(cryo_temp))
  })
  
  output$tp_rev_box_days_remaining <- renderValueBox({
    valueBox(
      value = paste0(tp_rev_days_remaining(), " days"),
      subtitle = paste0("Days Until Bloom (", format(input$tp_bloom_date, "%b %d"), ")"),
      icon = icon("seedling"),
      color = if (tp_rev_days_remaining() > 0) "green" else "red"
    )
  })
  
  output$tp_rev_box_dev_completed <- renderValueBox({
    valueBox(
      value = sprintf("%.1f%%", tp_rev_dev_completed() * 100),
      subtitle = paste0("Development Completed at ", input$tp_rev_temp1, "\u00b0C"),
      icon = icon("thermometer-half"),
      color = "yellow"
    )
  })
  
  output$tp_rev_warning <- renderUI({
    if (tp_rev_days_remaining() <= 0)
      fluidRow(box(width = 12, background = "red",
                   icon("exclamation-triangle"), strong(" Warning: "),
                   "Bloom date is in the past or today. Please update your target date."))
    else if (tp_rev_dev_completed() >= 1)
      fluidRow(box(width = 12, background = "red",
                   icon("exclamation-triangle"), strong(" Warning: "),
                   "Bees have already completed 100% development at Phase 1 temperature."))
    else if (is.null(tp_rev_results()) || nrow(tp_rev_results()) == 0)
      fluidRow(box(width = 12, background = "yellow",
                   icon("exclamation-triangle"), strong(" No valid protocols found. "),
                   "No cryophase temperature produces emergence exactly on your target date.
            Try adjusting the bloom date, Phase 1 duration, or temperature options."))
  })
  
  output$tp_rev_recommended <- renderUI({
    res <- tp_rev_results()
    if (is.null(res) || nrow(res) == 0) {
      p("No valid protocols found with the current settings.")
    } else {
      best <- res %>% arrange(desc(cryo_temp)) %>% slice(1)
      tagList(
        p(strong("Recommended protocol (warmest cryophase temperature):"),
          style = "font-size: 15px;"),
        tags$ul(
          tags$li(strong("Cryophase: "), best$cryo_days, " days at ",
                  best$cryo_temp, "\u00b0C"),
          tags$li(strong("Development at end of cryophase: "),
                  sprintf("%.1f%%", best$dev_at_cryo))
        ),
        p("Transfer bees to the cryophase temperature today.",
          style = "color: #003C67;")
      )
    }
  })
  
  output$tp_rev_table <- renderTable({
    res <- tp_rev_results()
    if (is.null(res) || nrow(res) == 0) return(NULL)
    best <- res %>% arrange(desc(cryo_temp)) %>% slice(1)
    res %>%
      mutate(
        Recommended = if_else(cryo_temp == best$cryo_temp, "\u2605 Recommended", "")
      ) %>%
      rename(
        `Cryo Temp. (°C)` = cryo_temp,
        `Cryo Duration (days)` = cryo_days,
        `Dev. at End of Cryo` = dev_at_cryo
      ) %>%
      mutate(`Dev. at End of Cryo` = paste0(`Dev. at End of Cryo`, "%"))
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
}

# Run -------------------------------------------------------------------------

shinyApp(ui = ui, server = server)