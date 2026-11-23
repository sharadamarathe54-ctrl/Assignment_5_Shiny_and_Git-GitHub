library(shiny)
library(shinydashboard)
library(tidyverse)

# Read data
dig.df <- read.csv("C:/Users/Sharada/Downloads/DIG-1.csv")

# Data cleaning
dig.df <- dig.df %>%
  dplyr::select(ID, TRTMT, AGE, SEX, BMI, KLEVEL, CREAT,
                DIABP, SYSBP, HYPERTEN, CVD, WHF, DIG,
                HOSP, HOSPDAYS, DEATH, DEATHDAY) %>%
  mutate(
    ID = as.character(ID),
    across(c(AGE, BMI, KLEVEL, CREAT, DIABP, SYSBP,
             HOSPDAYS, DEATHDAY), as.numeric),
    across(c(TRTMT, SEX, HYPERTEN, CVD, WHF, DIG,
             HOSP, DEATH), as.factor)
  )

# Factor labels
dig.df <- dig.df %>%
  mutate(
    TRTMT = factor(TRTMT,
                   levels = c(0, 1),
                   labels = c("Placebo", "Treatment")),
    
    SEX = factor(SEX,
                 levels = c(1, 2),
                 labels = c("Male", "Female")),
    
    HYPERTEN = factor(HYPERTEN,
                      levels = c(0, 1),
                      labels = c("No", "Yes")),
    
    CVD = factor(CVD,
                 levels = c(0, 1),
                 labels = c("No", "Yes")),
    
    WHF = factor(WHF,
                 levels = c(0, 1),
                 labels = c("No", "Yes")),
    
    DIG = factor(DIG,
                 levels = c(0, 1),
                 labels = c("No", "Yes")),
    
    HOSP = factor(HOSP,
                  levels = c(0, 1),
                  labels = c("No", "Yes")),
    
    DEATH = factor(DEATH,
                   levels = c(0, 1),
                   labels = c("Alive", "Death"))
  )

# UI
ui <- dashboardPage(
  
  dashboardHeader(title = "DIG Trial"),
  
  dashboardSidebar(
    radioButtons(
      inputId = "SEX",
      label = "Filter by sex:",
      choices = c("All", "Male", "Female"),
      selected = "All"
    )
  ),
  
  dashboardBody(
    
    h2("DIG Trial Dashboard"),
    
    fluidRow(
      valueBoxOutput("total", width = 4),
      valueBoxOutput("digoxin", width = 4),
      valueBoxOutput("placebo", width = 4)
    ),
    
    fluidRow(
      box(
        title = "Treatment Group Distribution",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        plotOutput("plot1")
      )
    ),

    fluidRow(
      box(
        title = "What participants were like at the start",
        width = 7,
        status = "primary",
        solidHeader = TRUE,
        plotOutput("baseline_plot", height = "550px",
                   alt = "Six boxplots compare baseline measurements for the digoxin and placebo groups.")
      ),
      box(
        title = "Who was hospitalized during follow-up?",
        width = 5,
        status = "primary",
        solidHeader = TRUE,
        plotOutput("hospitalization_plot", height = "340px",
                   alt = "Two bars show the share of each treatment group hospitalized at least once.")
      )
    )
  )
)

# Server
server <- function(input, output) {
  cohort <- reactive({
    req(input$SEX)
    if (input$SEX == "All") return(dig.df)
    dplyr::filter(dig.df, SEX == input$SEX)
  })
  
  arm_labels <- c(Placebo = "Placebo", Treatment = "Digoxin")
  arm_colors <- c(Placebo = "#235a9f", Treatment = "#be5912")

  # Total patients
  output$total <- renderValueBox({
    valueBox(
      nrow(cohort()),
      "Patients in selected cohort",
      icon = icon("users"),
      color = "aqua"
    )
  })
  
  # Digoxin treatment patients
  output$digoxin <- renderValueBox({
    valueBox(
      sum(cohort()$TRTMT == "Treatment", na.rm = TRUE),
      "Digoxin",
      icon = icon("medkit"),
      color = "blue"
    )
  })
  
  # Placebo patients
  output$placebo <- renderValueBox({
    valueBox(
      sum(cohort()$TRTMT == "Placebo", na.rm = TRUE),
      "Placebo",
      icon = icon("user"),
      color = "green"
    )
  })
  
  # Chart topics follow Questions 2 and 5 in the supplied Assignment 4 report.
  output$baseline_plot <- renderPlot({
    measures <- c(
      AGE = "Age (years)", BMI = "BMI (kg/m^2)",
      KLEVEL = "Serum potassium", CREAT = "Serum creatinine (mg/dL)",
      DIABP = "Diastolic BP (mmHg)", SYSBP = "Systolic BP (mmHg)"
    )
    baseline <- cohort() %>%
      select(TRTMT, all_of(names(measures))) %>%
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

  # Treatment bar chart
  output$plot1 <- renderPlot({
    
    patients <- cohort()
    validate(need(nrow(patients) > 0, "No patients match this selection."))
    patients %>%
      ggplot(aes(x = TRTMT, fill = TRTMT)) +
      geom_bar(alpha = 0.4) +
      labs(
        x = "Randomized treatment",
        y = "Patients in selected cohort",
        fill = "Randomized treatment"
      ) +
      scale_fill_manual(
        values = c(Placebo = "purple", Treatment = "darkgreen"),
        breaks = c("Placebo", "Treatment"),
        labels = c("Placebo", "Digoxin")
      ) +
      scale_x_discrete(labels = c(Placebo = "Placebo", Treatment = "Digoxin"))
    
  })
  
}

# Run app
shinyApp(ui, server)
