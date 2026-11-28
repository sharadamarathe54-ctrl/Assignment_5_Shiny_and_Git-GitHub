# Assignment_5_Shiny_and_Git-GitHub

## Project Overview

An interactive R Shiny dashboard for exploring, analysing, and visualizing clinical data from the Digitalis Investigation Group (DIG) trial, a randomized clinical trial comparing Digoxin and Placebo in patients with heart failure.

The dashboard incorporates data cleaning, descriptive statistics, interactive visualizations, hypothesis testing, and Kaplan–Meier-based survival analysis to explore patient characteristics and clinical outcomes across treatment groups.

## Dashboard Features

- **Overview:** Summary of patient counts, baseline characteristics, mortality, and hospitalization by treatment group.
- **Baseline Characteristics:** Comparative boxplots of age, BMI, serum potassium, creatinine, and blood pressure.
- **CVD and Mortality:** Exploration of the relationship between cardiovascular hospitalization and mortality using counts, percentages, summary tables, and a chi-square test.
- **Hospitalization:** Comparison of hospitalization proportions between treatment groups.
- **Monthly Risk:** Estimation of conditional monthly mortality risk using Kaplan–Meier survival estimates.
- **Monthly Risk by Treatment:** Comparison of monthly mortality risk between Digoxin and Placebo.
- **All Variables:** Interactive exploration of individual variables, descriptive statistics, patient-level data, and CSV export.

## Interactive Patient Filters

Users can dynamically filter the dataset based on:

- Treatment group
- Sex and race
- NYHA functional classification
- Diabetes, hypertension, and previous myocardial infarction
- Age and BMI
- Ejection fraction
- Serum potassium and creatinine

All dashboard tabs respond to a common filtered patient cohort. A reset button restores the original patient selection.

## Run the Dashboard

1. Clone or download this repository.
2. Download the DIG Trial CSV from the course's Canvas page.
3. Save it as `DIG-1.csv` in the same folder as `app.R`.
4. Open an R session in the project folder.
5. Install the required packages once and launch the dashboard.

```r
install.packages(c("shiny", "tidyverse", "survival"))
shiny::runApp(".")
```

## Check the Filters

1. Start with both treatments selected and all patient filters at their default values.
2. The course dataset should display 6,800 participants.
3. Select Female. The patient count should decrease to 1,519, and the relevant dashboard outputs should update.
4. Apply additional demographic or clinical filters to explore specific patient subgroups.
5. Clear both treatment selections. The patient count should drop to zero, and analytical plots should display appropriate validation messages.
6. Click Reset filters to restore the original patient cohort.

## Technologies Used

- R
- R Shiny
- Tidyverse
- ggplot2
- Survival


