library(shiny)

library(tidyverse)

icu <- readRDS("/home/haoyunj/biostat-203b-2021-winter/HW3/mimiciv_shiny/icu_cohort.rds")
# ggplot(data = icu, aes_string(x = ")) + 
#     geom_bar()

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("ICU_cohort"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select a item to show the graphic and numeraic information"),
      selectInput("var",label = "Choose a variable type:",
                  choices = c("Demographic", "Lab measurement","Vitals")), 
      
      uiOutput("typeChanged")
      
    ),
    
    mainPanel(
      plotOutput("plot")
      
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$typeChanged <- renderUI({
    
    if(input$var == "Demographic") {
      selectInput("varselected",label = "Choose a variable to display:",
                  choices = c('insurance', 'language', 'marital_status', 
                              'ethnicity', 'gender', 'age_at_adm',
                              'first_careunit ', 'last_careunit '))
    } else if(input$var == "Lab measurement") {
      selectInput("varselected",label = "Choose a variable to display:",
                  choices = c("bicarbonate", "calcium", " chloride creatinine",
                              "glucose",  "magnesium", "potassium",
                              "sodium", "hematocrit", "wbc", "lactate",
                    "heart_rate", "arterial_blood_pressure_systolic",
                              "arterial_blood_pressure_mean",
                              "non_invasive_blood_pressure_systolic", 
                              "non_invasive_blood_pressure_mean"
                              
                              
                              
                              
                  ))
    } else { # Vitals
      selectInput("varselected",label = "Choose a variable to display:",
                  choices = c("deathin30"))
    }
    
  })
  
  output$plot <- renderPlot({
    
    var = input$varselected
    
    ggplot(data = icu, mapping = aes_string(x = var, fill = var)) + 
      geom_bar()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
