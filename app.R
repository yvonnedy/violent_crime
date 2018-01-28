# This script runs a Shiny app which aims to explore the rate of violent crimes over recent years in 68 US cities.
#
# Ying Dong, Jan 2017

library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)

dataset <- read_csv("data/ucr_crime_1975_2015.csv") 

ui <- fluidPage(
  
  titlePanel("US Violent Crimes Shiny Application"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("citiesInput", "City", choices = sort(unique(dataset$department_name)), 
                  selected = "Albuquerque, N.M.", multiple = TRUE),
      
      sliderInput("yearInput", "Year Range", min = 1975, max = 2015, step = 1, value = c(1975, 2015)),
      
      radioButtons("typeInput", "Crime Type",
                   choices = c("Total Crime" = "Crimes", "Homicide" = "Homicides", "Rape" = "Rapes", 
                               "Robbery" = "Robberies", "Aggravated Assault" = "Aggravated_Assaults"),
                   selected = "Crimes")
    ),
    
    mainPanel(
      h4(textOutput("text")),
      
      br(), br(),
      
      plotlyOutput("linePlot"),
      
      br(), br(),
      
      plotlyOutput("scatterPlot"),
      
      br(), br(),
      
      dataTableOutput("resultsTable")
    )
  )
)

server <- function(input, output) {
  
  filtered <- reactive({
    if (is.null(input$citiesInput)) {
      return(NULL)
    } 
    
    dataset %>%
      rename(City = department_name,
             Year = year,
             Population = total_pop,
             Crimes = violent_crime,
             Homicides = homs_sum,
             Rapes = rape_sum,
             Robberies = rob_sum,
             Aggravated_Assaults = agg_ass_sum) %>% 
      filter(City %in% input$citiesInput,
             Year >= input$yearInput[1],
             Year <= input$yearInput[2])
  })
  
  output$text <- renderText({
    rows <- nrow(filtered())
    if(is.null(rows)){
      rows <- 0
    }
    paste0("There are ",rows," records selected:")
  })
  
  output$linePlot <- renderPlotly({
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes_string(x = "Year", y = input$typeInput, colour = "City")) +
      geom_line() +
      geom_point() +
      labs(x = "Year", y = input$typeInput, colour = "City") +
      ggtitle(paste0("The Number Of Total ", input$typeInput, " Over Years")) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$scatterPlot <- renderPlotly({
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes_string(x = "Population", y = input$typeInput, colour = "City")) +
      geom_point() +
      labs(x = "Population", y = input$typeInput, colour = "City") +
      ggtitle(paste0("The Number Of Total ", input$typeInput, " Over Population")) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$resultsTable <- renderDataTable({
    if (is.null(filtered())) {
      return()
    }

    filtered2 <- filtered() %>%
      select(Year, City, violent_per_100k, homs_per_100k, rape_per_100k, rob_per_100k, agg_ass_per_100k) %>%
      rename("Total Crime Rate" = "violent_per_100k",
             "Homicide Rate" = "homs_per_100k",
             "Rape Rate" = "rape_per_100k",
             "Robbery Rate" = "rob_per_100k",
             "Aggravated Assault Rate" = "agg_ass_per_100k")

    filtered2
  })

}

shinyApp(ui = ui, server = server)
