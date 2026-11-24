library(shiny)
library(tidyverse)
library(survival)

# Read the complete dataset
dig.df <- read.csv("C:/Users/Sharada/Downloads/DIG-1.csv")
names(dig.df) <- trimws(names(dig.df))

# Data cleaning
dig.df <- dig.df %>%
  mutate(
    ID = as.character(ID),
    across(c(AGE, BMI, KLEVEL, CREAT, DIABP, SYSBP,
             HOSPDAYS, DEATHDAY, EJF_PER), as.numeric)
  )
# Check coding before relabeling (same coding as the original assignment).
for (v in c("TRTMT", "HYPERTEN", "CVD", "WHF", "DIG", "HOSP", "DEATH", "SEX", "RACE", "FUNCTCLS", "DIABETES", "PREVMI")) {
  allowed <- if (v %in% c("SEX", "RACE")) c("1", "2") else if (v == "FUNCTCLS") as.character(1:4) else c("0", "1")
  unexpected <- setdiff(as.character(na.omit(dig.df[[v]])), allowed)
  if (length(unexpected)) stop("Unexpected codes in ", v, ": ", paste(unexpected, collapse = ", "))
}

# Factor labels and follow-up month
dig.df <- dig.df %>%
  mutate(
    TRTMT = factor(TRTMT, levels = c(0, 1), labels = c("Placebo", "Digoxin")),
    SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
    RACE = factor(RACE, levels = c(1, 2), labels = c("White", "Nonwhite")),
    FUNCTCLS = factor(FUNCTCLS, levels = 1:4, labels = paste("Class", c("I", "II", "III", "IV"))),
    DIABETES = factor(DIABETES, levels = c(0, 1), labels = c("No", "Yes")),
    PREVMI = factor(PREVMI, levels = c(0, 1), labels = c("No", "Yes")),
    HYPERTEN = factor(HYPERTEN, levels = c(0, 1), labels = c("No", "Yes")),
    CVD = factor(CVD, levels = c(0, 1), labels = c("No", "Yes")),
    WHF = factor(WHF, levels = c(0, 1), labels = c("No", "Yes")),
    DIG = factor(DIG, levels = c(0, 1), labels = c("No", "Yes")),
    HOSP = factor(HOSP, levels = c(0, 1), labels = c("No", "Yes")),
    DEATH = factor(DEATH, levels = c(0, 1), labels = c("Alive", "Death")),
    Month = round(DEATHDAY / 30)
  )
arm_colors <- c(Placebo = "#235a9f", Digoxin = "#be5912")
arm_labels <- c(Placebo = "Placebo", Digoxin = "Digoxin")

# Keep all original columns; classify coded variables for exploration.
# Unlabelled codes remain as recorded in the CSV.
code_vars <- c("EJFMETH", "RALES", "ELEVJVP", "PEDEMA", "RESTDYS",
               "EXERTDYS", "ACTLIMIT", "S3", "PULCONG", "CHFETIOL",
               "ANGINA", "DIGUSE", "DIURETK", "DIURET", "KSUPP", "ACEINHIB",
               "NITRATES", "HYDRAL", "VASOD", "MI", "UANG", "STRK", "SVA",
               "VENA", "CREV", "OCVD", "RINF", "OTH", "REASON", "DWHF")
dig.df <- dig.df %>% mutate(across(any_of(code_vars), as.factor))


# Use the same limits for the sliders and the filtering code
numeric_vars <- c("AGE", "BMI", "EJF_PER", "KLEVEL", "CREAT")

slider_limits <- function(var) {
  x <- dig.df[[var]]
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NULL)
  r <- c(floor(min(x)), ceiling(max(x)))
  if (r[1] == r[2]) r[2] <- r[2] + 1
  r
}

range_filter <- function(var, label) {
  r <- slider_limits(var)
  if (!is.null(r)) {
    sliderInput(var, label, min = r[1], max = r[2],
                value = r, step = if (var == "AGE") 1 else 0.01)
  }
}

monthly_risk <- function(d) {
  empty <- data.frame(Month = numeric(0), Risk = numeric(0))
  if (!nrow(d)) return(empty)
  times <- seq(30, 1770, 30)
  times <- times[times <= max(d$DEATHDAY)]
  if (!length(times)) return(empty)
  fit <- survival::survfit(survival::Surv(DEATHDAY, DEATH == "Death") ~ 1, data = d)
  z <- summary(fit, times = times, extend = FALSE)
  previous <- lag(z$surv, default = 1)
  risk <- ifelse(previous > 0, 100 * (1 - z$surv / previous), NA_real_)
  data.frame(Month = z$time / 30, Risk = risk)
}

# User interface
ui <- fluidPage(
  titlePanel("DIG Trial Dashboard"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 h4("Patient filters"),
                 checkboxGroupInput("TRTMT", "Treatment", c("Placebo", "Digoxin"),
                                    selected = c("Placebo", "Digoxin")),
                 radioButtons("SEX", "Sex", c("All", "Male", "Female")),
                 selectInput("RACE", "Race", c("All", levels(dig.df$RACE))),
                 selectInput("FUNCTCLS", "NYHA class", c("All", levels(dig.df$FUNCTCLS))),
                 selectInput("DIABETES", "Diabetes", c("All", "No", "Yes")),
                 selectInput("HYPERTEN", "Hypertension", c("All", "No", "Yes")),
                 selectInput("PREVMI", "Previous MI", c("All", "No", "Yes")),
                 range_filter("AGE", "Age"),
                 range_filter("BMI", "BMI"),
                 range_filter("EJF_PER", "Ejection fraction (%)"),
                 range_filter("KLEVEL", "Potassium"),
                 range_filter("CREAT", "Creatinine"),
                 actionButton("reset_filters", "Reset filters"),
                 textOutput("patient_n")
    ),
    mainPanel(width = 9,
              tabsetPanel(
                tabPanel("Overview",
                         h3("DIG Trial Overview"),
                         p("Randomized trial comparing digoxin and placebo in heart failure."),
                         p("Teaching data were permuted within treatment groups. Subgroup results are not validated clinical findings."),
                         p("The reported potassium value of 434 is retained pending verification."),
                         textOutput("overview_n"),
                         tableOutput("overview"),
                         plotOutput("plot1")),
                tabPanel("Baseline",
                         # Add Q2 UI here
                         h3("Question 2"),
                         p("To be added")),
                tabPanel("CVD and Mortality",
                         radioButtons("q4_display", "Display", c("Percent", "Count")),
                         plotOutput("q4_plot"), tableOutput("q4_table"), textOutput("q4_p")),
                tabPanel("Hospitalization",
                         # Add Q5 UI here
                         h3("Question 5"),
                         p("To be added")),
                tabPanel("Monthly Risk",
                         sliderInput("q9_months", "Months", 1, 59, c(1, 59)),
                         plotOutput("q9_plot")),
                tabPanel("Monthly Risk by Treatment",
                         sliderInput("q10_months", "Months", 1, 59, c(1, 59)),
                         plotOutput("q10_plot"),
                         helpText("Late follow-up estimates can be unstable.")),
                tabPanel("All Variables",
                         selectInput("variable", "Choose variable", names(dig.df), selected = "AGE"),
                         plotOutput("variable_plot"), verbatimTextOutput("variable_summary"),
                         selectInput("columns", "Table columns", names(dig.df),
                                     selected = c("ID", "TRTMT", "AGE", "SEX"), multiple = TRUE),
                         tableOutput("patient_data"),
                         downloadButton("download_data", "Download selected patients"))
              )
    )
  )
)

server <- function(input, output, session) {
  # One filtered dataset for all tabs
  cohort <- reactive({
    d <- dig.df %>% filter(TRTMT %in% input$TRTMT)
    if (input$SEX != "All") d <- filter(d, SEX == input$SEX)
    if (input$RACE != "All") d <- filter(d, RACE == input$RACE)
    if (input$FUNCTCLS != "All") d <- filter(d, FUNCTCLS == input$FUNCTCLS)
    if (input$DIABETES != "All") d <- filter(d, DIABETES == input$DIABETES)
    if (input$HYPERTEN != "All") d <- filter(d, HYPERTEN == input$HYPERTEN)
    if (input$PREVMI != "All") d <- filter(d, PREVMI == input$PREVMI)
    # Keep missing measurements when sliders are unchanged
    for (var in numeric_vars) {
      r <- slider_limits(var)
      selected <- input[[var]]
      if (!is.null(r) && !is.null(selected)) {
        if (selected[1] > r[1] + 1e-8 || selected[2] < r[2] - 1e-8) {
          d <- filter(d, between(.data[[var]], selected[1], selected[2]))
        }
      }
    }
    d
  })
  