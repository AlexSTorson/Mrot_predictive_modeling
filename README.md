# Mrot_Predictive_Modeling

R scripts and Shiny application for Torson et al. (*Journal of Economic Entomology*, in review):  
*A three-phase interrupted development model and web application for predicting timing of* Megachile rotundata *emergence for synchronization with peak floral bloom*

AlfalfaBeeTools web application: https://torsonlab.shinyapps.io/AlfalfaBeeTools/  
Dataset (Ag Data Commons): https://doi.org/XXXXX

---

## Repository contents

| File | Description |
|------|-------------|
| `01_dev_rate_estimation.R` | Estimates developmental rates and degree-day parameters from constant-temperature emergence data; fits linear and sex-specific degree-day models; fits quantile regression models for emergence window estimation. Outputs summary tables used by all downstream scripts. |
| `02_two_phase_model.R` | Predicts emergence timing under two-phase (starting temperature → cryophase) protocols; generates emergence window ribbon figures. |
| `03_three_phase_model_figures.R` | Generates the protocol timeline, cumulative development curve, and cryophase sensitivity figures for the three-phase interrupted development model. |
| `04_emergence_date_predictor.R` | Predicts emergence dates for a user-specified three-phase protocol (thermophase / cryophase / thermophase) for combined, female, and male models. |
| `05_three_phase_model_validation.R` | Validates three-phase model predictions against observed emergence from 2022–2023 experiments; computes RMSE and MAE for median and percentile emergence predictions; generates all validation figures and supplemental tables. |
| `app.R` | AlfalfaBeeTools Shiny application source code. |

---

## Data

The input dataset (`degree_day_dataset.csv`) is available via USDA Ag Data Commons (https://doi.org/XXXXX).  
Place the file in the project root directory before running any scripts.

---

## Running the pipeline

Scripts must be run in order, as downstream scripts depend on outputs from script 01:

```r
source("01_dev_rate_estimation.R")
source("02_two_phase_model.R")
source("03_three_phase_model_figures.R")
source("04_emergence_date_predictor.R")
source("05_three_phase_model_validation.R")
```

Each script contains a `setwd()` call pointing to the original author's local directory. Update this path to your own working directory before running. Each script creates its own output directory on first run. Before running script 04, edit the user input block near the top of the file to specify your protocol temperatures and durations.

---

## Dependencies

```r
install.packages(c("tidyverse", "quantreg", "ggsci", "patchwork", "cowplot", "lubridate", "ggforce", "shiny", "shinydashboard"))
```

R version 4.x or later recommended.

---

## Contact

Alex Torson — Alex.Torson@usda.gov  
USDA-ARS Edward T. Schafer Agricultural Research Center, Fargo, ND
