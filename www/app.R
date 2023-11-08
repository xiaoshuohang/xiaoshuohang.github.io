#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)

df <- read_csv("marine1.csv")
df1 <- df %>% select(Latitude, `Species Count`)



library(shiny)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Relationship between latitude and marine species"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("df1",
                  "n",
                  min = 1,
                  max = 500,
                  value =200)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      img(src="intro_species",height=140,width=400),
      
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$df1 + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'blue', border = 'black',
         xlab = 'latitude',
         main = 'no. of marine species')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



