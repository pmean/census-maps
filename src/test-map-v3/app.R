# Test shiny app, v3
# Written by Steve Simon
# Created on 2021-10-20
# Purpose: to test some simple maps in Shiny
# License: Public domain

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

  titlePanel("Test maps"),

  
  sidebarLayout(
    mainPanel(plotOutput("distPlot")), 
    sidebarPanel(
      sliderInput(
        "bins",
        "Number of bins:",
        min = 1,
        max = 150,
        value = 30)
      )
    )
  )

server <- function(input, output) {
  x <- faithful[, 2]
  output$distPlot <- 
  renderPlot(
    {
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      hist(x, breaks = bins, col = 'orange', border = 'black')
    }
  )
}

shinyApp(ui = ui, server = server)
