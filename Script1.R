library(shiny)
library(tidyverse)
library(tidyverse)

dig.df <- read.csv("C:/Users/Sharada/Downloads/DIG-1.csv")
dig.df
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

str(dig.df)
library(dplyr)

dig.df <-   dig.df %>%
  mutate(
    ID = as.character(ID),
    
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
str(dig.df)

dig.df

ui <- fluidPage(
  titlePanel("Palmer Penguins Explorer"),
  sidebarLayout(
    sidebarPanel(
 
      radioButtons(inputId = "SEX", label = "Select Penguin Sex:", choices = c("Male", "Female"))
  
    ),
    mainPanel(
      plotOutput("plot1")
    )
  )
  
)

server <- function(input, output) {
  
  output$plot1 <- renderPlot({ 
    dig.df %>%  filter(SEX == input$SEX) %>% ggplot(mapping = aes(x = TRTMT,
                     fill = TRTMT)) +
  geom_bar(alpha = 0.4) +
  labs(title = "Barplot of Treatment Groups",
       x = "Treatment Group",
       y = "Frequency",
       fill = "Treatment Group",
       caption = "Figure 1: Distribution of patients by treatment group") +
  scale_fill_manual(values = c("purple", "darkgreen"))
   
  })
}

shinyApp(ui, server)