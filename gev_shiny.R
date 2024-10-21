library(devtools)
devtools::install_github("ropensci/plotly")
library(extRemes)
library(tidyverse)
library(shiny)

#| context: server

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      sliderInput("loc",
                  "Location:",
                  min = -5,
                  max = 5,
                  value = 0,
                  step = 0.025,
                  animate = animationOptions(interval = 10)),
      
      sliderInput("scale",
                  "Scale:",
                  min = 0.5,
                  max = 3,
                  value = 1,
                  step = 0.025,
                  animate = animationOptions(interval = 10)),
      
      sliderInput("shape",
                  "Shape:",
                  min = -0.5,
                  max = 1,
                  value = 0,
                  step = 0.01)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("gevPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$gevPlot <- renderPlot({
    
    x = seq(-10, 20, length.out = 500)
    y_orig = devd(x, 0, 1, 0)
    y = devd(x, input$loc, input$scale, input$shape)
    y_p = pevd(x, input$loc, input$scale, input$shape)
    
    df = data.frame(x, y, y_orig, y_p)
    
    gev_plot <- ggplot(data = df) + 
      geom_line(aes(x=x, y=y_orig), linetype = "dotted") + 
      geom_line(aes(x=x, y=y), col = "blue") + 
      scale_x_continuous(limits = c(-10, 20)) +
      scale_y_continuous(limits = c(0, 0.6)) + 
      xlab("x") +
      ylab("y") +
      theme_bw()
    
    # ggplotly(gev_plot)
    gev_plot
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
