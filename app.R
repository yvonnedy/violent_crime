library(shiny)
library(dplyr)
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


}

shinyApp(ui = ui, server = server)
