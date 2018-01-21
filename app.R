# This script runs a Shiny app which aims to explore the rate of violent crimes over recent years in 68 US cities.
#
# Ying Dong, Jan 2017

library(shiny)
library(tidyverse)
library(ggplot2)

dataset <- read_csv("data/ucr_crime_1975_2015.csv") 

ui <- fluidPage(
  titlePanel("US Violent Crimes Shiny Application"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("citiesInput", "Cities", choices = unique(dataset$department_name),
                         inline = TRUE, selected = "Albuquerque, N.M."),
      sliderInput("yearInput", "Year Range", min = 1975, max = 2015, step = 1, value = c(1975, 2015))
    ),
    mainPanel(
      plotOutput("linePlot"),
      br(), br(),
      tableOutput("resultsTable")
    )
  )
)

server <- function(input, output) {
  filtered <- reactive({
    dataset %>%
      filter(department_name %in% input$citiesInput,
             year >= input$yearInput[1],
             year <= input$yearInput[2]
      )
  })  
  
  output$linePlot <- renderPlot({
    ggplot(filtered(), aes(x = year, y = violent_per_100k, colour = department_name)) +
      geom_line() +
      geom_point() +
      labs(x = "Year", y = "Violent Crimes Per 100,000 People", colour = "City") +
      ggtitle("The Rate Of Violent Crime Over Years") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
   output$resultsTable <- renderTable({
    filtered2 <- filtered() %>%
      select(year, department_name, total_pop, violent_per_100k, homs_per_100k, rape_per_100k, rob_per_100k, agg_ass_per_100k) %>% 
      rename(Year = year,
             City = department_name,
             Population = total_pop,
             Total_crimes_rate = violent_per_100k,
             Homicides_rate = homs_per_100k,
             Rapes_rate = rape_per_100k,
             Robberies_rate = rob_per_100k,
             Aggravated_assaults_rate = agg_ass_per_100k) 
    
    filtered2
  })
}

shinyApp(ui = ui, server = server)
