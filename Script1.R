data_file <- "DIG-1.csv"
if (!file.exists(data_file)) {
  stop("Place the course CSV in the project folder as DIG-1.csv, then restart the app.",
       call. = FALSE)
}

library(shiny)
library(tidyverse)
library(survival)

# Read the complete dataset
dig.df <- read.csv(data_file)
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
                         h3("What participants were like at the start"),
                         plotOutput("baseline_plot", height = "550px")),
                tabPanel("CVD and Mortality",
                         radioButtons("q4_display", "Display", c("Percent", "Count")),
                         plotOutput("q4_plot"), tableOutput("q4_table"), textOutput("q4_p")),
                tabPanel("Hospitalization",
                         h3("Who was hospitalized during follow-up?"),
                         plotOutput("hospitalization_plot", height = "340px")),
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
  
  # Reset all global patient filters
  observeEvent(input$reset_filters, {
    updateCheckboxGroupInput(session, "TRTMT", selected = c("Placebo", "Digoxin"))
    updateRadioButtons(session, "SEX", selected = "All")
    for (var in c("RACE", "FUNCTCLS", "DIABETES", "HYPERTEN", "PREVMI")) {
      updateSelectInput(session, var, selected = "All")
    }
    for (var in numeric_vars) {
      r <- slider_limits(var)
      if (!is.null(r)) updateSliderInput(session, var, value = r)
    }
  })
  
  output$patient_n <- renderText({ paste("Selected patients:", nrow(cohort())) })
  
  output$overview_n <- renderText({
    paste("Selected patients:", nrow(cohort()), "of", nrow(dig.df))
  })
  
  output$overview <- renderTable({
    d <- cohort()
    validate(need(nrow(d) > 0, "No patients match the selected filters."))
    d %>% group_by(TRTMT) %>%
      summarise(Patients = n(), Mean_age = mean(AGE, na.rm = TRUE),
                Mean_BMI = mean(BMI, na.rm = TRUE),
                Mean_EF = mean(EJF_PER, na.rm = TRUE),
                Mortality_pct = 100 * mean(DEATH == "Death", na.rm = TRUE),
                Hospitalization_pct = 100 * mean(HOSP == "Yes", na.rm = TRUE),
                .groups = "drop")
  }, digits = 1)
  
  output$plot1 <- renderPlot({
    d <- cohort()
    validate(need(nrow(d) > 0, "No patients match the selected filters."))
    ggplot(d, aes(TRTMT, fill = TRTMT)) + geom_bar() +
      scale_fill_manual(values = arm_colors) +
      scale_x_discrete(drop = FALSE) + theme_minimal() +
      labs(x = "Treatment", y = "Patients")
  })
  
  # Baseline
  output$baseline_plot <- renderPlot({
    measures <- c(
      AGE = "Age (years)", BMI = "BMI (kg/m^2)",
      KLEVEL = "Serum potassium", CREAT = "Serum creatinine (mg/dL)",
      DIABP = "Diastolic BP (mmHg)", SYSBP = "Systolic BP (mmHg)"
    )
    baseline <- cohort() %>%
      dplyr::select(TRTMT, all_of(names(measures))) %>%
      pivot_longer(-TRTMT, names_to = "Characteristic", values_to = "Value") %>%
      filter(is.finite(Value)) %>%
      mutate(Characteristic = factor(Characteristic, levels = names(measures),
                                     labels = unname(measures)))
    validate(need(nrow(baseline) > 0, "No baseline measurements match this selection."))
    
    ggplot(baseline, aes(TRTMT, Value, fill = TRTMT)) +
      geom_boxplot(alpha = 0.8, outlier.colour = "red") +
      facet_wrap(~Characteristic, scales = "free_y", ncol = 2) +
      scale_x_discrete(labels = arm_labels) +
      scale_fill_manual(values = arm_colors) +
      labs(x = "Randomized treatment", y = NULL,
           caption = "Compare the two groups within each panel. The line in each box marks the median.") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  
  
  # CVD and mortality
  output$q4_plot <- renderPlot({
    d <- cohort() %>% filter(!is.na(CVD), !is.na(DEATH), !is.na(TRTMT))
    validate(need(nrow(d) > 0, "No records match these filters."))
    pos <- if (input$q4_display == "Percent") "fill" else "stack"
    p <- ggplot(d, aes(CVD, fill = DEATH)) +
      geom_bar(position = pos, width = 0.65) +
      facet_wrap(~ TRTMT, labeller = labeller(TRTMT = arm_labels)) +
      scale_fill_manual(values = c(Alive = "#235a9f", Death = "#be5912")) +
      labs(x = "Cardiovascular hospitalization", y = input$q4_display, fill = "Vital status") +
      theme_minimal()
    if (input$q4_display == "Percent") {
      p <- p + scale_y_continuous(labels = scales::label_percent())
    }
    p
  })
  output$q4_table <- renderTable({
    d <- cohort() %>% filter(!is.na(CVD), !is.na(DEATH), !is.na(TRTMT))
    validate(need(nrow(d) > 0, "No records match these filters."))
    d %>% group_by(TRTMT, CVD) %>%
      summarise(Patients = n(), Deaths = sum(DEATH == "Death"),
                Mortality_pct = round(100 * Deaths / Patients, 1),
                .groups = "drop") %>%
      mutate(TRTMT = as.character(TRTMT))
  }, striped = TRUE, spacing = "s", rownames = FALSE)
  output$q4_p <- renderText({
    d <- cohort() %>% filter(!is.na(CVD), !is.na(DEATH))
    tab <- table(d$CVD, d$DEATH)
    if (nrow(d) == 0 || any(rowSums(tab) == 0) || any(colSums(tab) == 0)) {
      return("Chi-square test unavailable for this selection.")
    }
    paste("Overall cardiovascular hospitalization-mortality chi-square p-value for the selected patients:",
          format.pval(suppressWarnings(chisq.test(tab)$p.value)))
  })
  
  # Hospitalization
  output$hospitalization_plot <- renderPlot({
    hospital_summary <- cohort() %>%
      filter(!is.na(HOSP)) %>%
      group_by(TRTMT) %>%
      summarise(Proportion = mean(HOSP == "Yes"), .groups = "drop")
    validate(need(nrow(hospital_summary) > 0, "No hospitalization records match this selection."))
    
    ggplot(hospital_summary, aes(TRTMT, Proportion, fill = TRTMT)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = scales::percent(Proportion, accuracy = 0.01)), vjust = -0.5) +
      scale_x_discrete(labels = arm_labels) +
      scale_y_continuous(labels = scales::label_percent(),
                         limits = c(0, 1), expand = expansion(mult = c(0, 0.07))) +
      scale_fill_manual(values = arm_colors) +
      labs(x = "Randomized treatment", y = "Patients hospitalized",
           caption = "Each bar counts patients with at least one hospital stay as a share of those with a recorded outcome.") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  
  
  # monthly mortality risk
  # Same survival cohort for both risk plots
  q9_data <- reactive({
    cohort() %>% filter(is.finite(DEATHDAY), DEATHDAY >= 0, !is.na(DEATH))
  })
  output$q9_plot <- renderPlot({
    d <- q9_data()
    validate(need(nrow(d) > 1, "Not enough patients for survival analysis."))
    monthly <- monthly_risk(d) %>%
      filter(between(Month, input$q9_months[1], input$q9_months[2]), is.finite(Risk))
    validate(need(nrow(monthly) > 0, "No monthly results match this selection."))
    ggplot(monthly, aes(Month, Risk)) +
      geom_line(color = "#235a9f", linewidth = 0.9) +
      geom_point(color = "#235a9f", size = 1.5) +
      labs(x = "Month", y = "Mortality risk (%)") +
      theme_minimal()
  })
  
  # monthly mortality risk by treatment
  q10_data <- reactive({ q9_data() %>% filter(!is.na(TRTMT)) })
  output$q10_plot <- renderPlot({
    d <- q10_data()
    validate(need(nrow(d) > 1, "Not enough patients for survival analysis."))
    monthly <- bind_rows(lapply(unique(as.character(d$TRTMT)), function(arm) {
      m <- monthly_risk(d[d$TRTMT == arm, , drop = FALSE])
      m$Treatment <- rep(arm, nrow(m))
      m
    })) %>%
      filter(between(Month, input$q10_months[1], input$q10_months[2]), is.finite(Risk))
    validate(need(nrow(monthly) > 0, "No monthly results match this selection."))
    ggplot(monthly, aes(Month, Risk, color = Treatment)) +
      geom_line(linewidth = 0.9) + geom_point(size = 1.5) +
      scale_color_manual(values = arm_colors, labels = arm_labels) +
      labs(x = "Month", y = "Mortality risk (%)", color = "Treatment group") +
      theme_minimal()
  })
  
  # Explore every variable in the dataset
  output$variable_plot <- renderPlot({
    var <- input$variable
    validate(need(var != "ID", "Patient IDs are shown in the table."))
    d <- cohort() %>% filter(!is.na(.data[[var]]))
    validate(need(nrow(d) > 0, "No available data."))
    if (is.numeric(d[[var]])) {
      ggplot(d, aes(TRTMT, .data[[var]], fill = TRTMT)) +
        geom_boxplot() + scale_fill_manual(values = arm_colors) + theme_minimal()
    } else {
      ggplot(d, aes(.data[[var]], fill = TRTMT)) +
        geom_bar(position = "dodge") +
        scale_fill_manual(values = arm_colors) + theme_minimal()
    }
  })
  
  output$variable_summary <- renderPrint({
    if (input$variable == "ID") {
      print(n_distinct(cohort()$ID))
    } else {
      print(summary(cohort()[[input$variable]]))
    }
  })
  
  output$patient_data <- renderTable({
    req(input$columns)
    head(cohort()[, input$columns, drop = FALSE], 20)
  })
  
  output$download_data <- downloadHandler(
    filename = function() "DIG_selected.csv",
    content = function(file) write.csv(cohort(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)

