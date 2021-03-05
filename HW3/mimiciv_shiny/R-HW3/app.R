#install.packages("tableone")
library(shiny)
library(tableone)
library(tidyverse)
icu <- readRDS("./data/icu_cohort.rds")
# ggplot(data = icu, aes_string(x = ")) + 
  #     geom_bar()
  
  # Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("MIMIC-IV ICU Stays Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select a item to show the summary of the data and number of
               missing values.",
               "Bar charts for characteristic variables.",
               "Boxplot and histograms with a red line indicated for the mean 
               and analysis table for numeric and non-characteristic variables."
               ,
               "Select bins for histograms."
               ),
      selectInput("var", label = "Choose a variable type:",
                  choices = c("Demographic", "Lab measurement", "Vitals")), 
      
      sliderInput("bins",
                  "Number of bins for histograms:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      uiOutput("typeChanged")
      
    ),
    
    mainPanel(
      verbatimTextOutput("summary"),
      plotOutput("plot"),
      plotOutput("boxplot")
     
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$typeChanged <- renderUI({
    
    if(input$var == "Demographic") {
      selectInput("varselected", label = "Choose a variable to display:",
                  choices = c("insurance", "language", "marital_status", 
                              "ethnicity", "gender", "age_at_adm",
                              "first_careunit", "last_careunit", "los",
                              "intime", "outtime", "admittime", "dischtime", 
                              "deathtime", "admission_type", 
                              "admission_location", "discharge_location"))
    } else if(input$var == "Lab measurement") {
      selectInput("varselected", label = "Choose a variable to display:",
                  choices = c("bicarbonate", "calcium", "chloride", 
                              "creatinine", "glucose", "magnesium", "potassium",
                              "sodium", "hematocrit", "wbc", "lactate"
                              
                  ))
    } else { # Vitals
      selectInput("varselected", label = "Choose a variable to display:",
                  choices = c("heart_rate", "arterial_blood_pressure_systolic",
                              "arterial_blood_pressure_mean","respiratory_rate",
                              "non_invasive_blood_pressure_systolic", 
                              "non_invasive_blood_pressure_mean"))
    }
    
  })
  
  output$plot <- renderPlot({
    
    var <- input$varselected
    if(is.null(var)) return(NULL)
    varClass <- eval(substitute(class(icu$var)))
    
    if(varClass == "character") {
      ggplot(data = icu, mapping = aes_string(x = var)) + 
        geom_bar(aes_string(fill = var)) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    } else if (varClass != "character") {
      bins <- seq(eval(substitute(min(icu$var, na.rm = TRUE))), 
                  eval(substitute(max(icu$var, na.rm = TRUE))), 
                  length.out = input$bins + 1)
      mean <- eval(substitute(mean(icu$var, na.rm = TRUE)))
      ggplot(data = icu, mapping = aes_string(x = var)) + 
        geom_histogram(breaks = bins) +
        geom_vline(xintercept = mean, color = "red") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    } else {
      NULL
    }
      
  })
  
  output$boxplot <- renderPlot({
    
    var <- input$varselected
    if(is.null(var)) return(NULL)
    varClass = eval(substitute(class(icu$var)))
    
    if (varClass == "numeric") {
      ggplot(data = icu, mapping = aes_string(x = var)) + 
        geom_boxplot() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    } else {
      NULL
    }
    
  })


    output$summary <- renderPrint({
      
      
      var = input$varselected
      
      
      if(is.null(var)) return(NULL)
      varClass = eval(substitute(class(icu$var)))
      
      
      tableOne<- CreateTableOne(vars = var,   data = icu)
      summary(tableOne, showAllLevels = TRUE, missing = TRUE)
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
