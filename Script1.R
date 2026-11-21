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
      label = "Select Sex:",
      choices = c("Male", "Female")
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
    )
  )
)

# Server
server <- function(input, output) {
  
  # Total patients
  output$total <- renderValueBox({
    valueBox(
      nrow(dig.df),
      "Total Patients",
      icon = icon("users"),
      color = "aqua"
    )
  })
  
  # Digoxin treatment patients
  output$digoxin <- renderValueBox({
    valueBox(
      sum(dig.df$TRTMT == "Treatment", na.rm = TRUE),
      "Digoxin",
      icon = icon("medkit"),
      color = "blue"
    )
  })
  
  # Placebo patients
  output$placebo <- renderValueBox({
    valueBox(
      sum(dig.df$TRTMT == "Placebo", na.rm = TRUE),
      "Placebo",
      icon = icon("user"),
      color = "green"
    )
  })
  
  # Treatment bar chart
  output$plot1 <- renderPlot({
    
    dig.df %>%
      filter(SEX == input$SEX) %>%
      ggplot(aes(x = TRTMT, fill = TRTMT)) +
      geom_bar(alpha = 0.4) +
      labs(
        title = "Barplot of Treatment Groups",
        x = "Treatment Group",
        y = "Frequency",
        fill = "Treatment Group",
        caption = "Figure 1: Distribution of patients by treatment group"
      ) +
      scale_fill_manual(values = c("purple", "darkgreen"))
    
  })
  
}

# Run app
shinyApp(ui, server)