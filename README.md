# Assignment_5_Shiny_and_Git-GitHub
Interactive R Shiny dashboard for exploring and visualizing DIG Trial clinical data, with dynamic visualizations and analysis of relationships between variables.

## Run the dashboard

1. Clone or download this repository. Open an R session in the project folder.
2. Download the DIG Trial CSV from the course's Canvas page. Save it as `DIG-1.csv` in the same folder as `app.R`.
3. Install the required packages once, then launch the dashboard.

```r
install.packages(c("shiny", "tidyverse", "survival"))
shiny::runApp(".")
```

The cohort filters update the baseline and hospitalization charts. If you see a message about a missing CSV, check the filename and confirm that it is beside `app.R`.
