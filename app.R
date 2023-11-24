library(tidyverse)
library(hrbrthemes)
library(viridis)
library(shiny)
library(shinyWidgets)
library(shinyjs)  


df <- read_csv("marine1.csv")

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel(div("Marine Species Count by Location",style = "color:#c4deec;")),
  chooseSliderSkin("Sharp"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("minxres", "Minimum Latitude", min = -120, max = 0, value = 2),
      sliderInput("maxxres", "Maximum Latitude", min = 0, max = 120, value = 2),
      sliderInput("minyres", "Minimum Longitude", min = -200, max = 0, value = 2),
      sliderInput("maxyres", "Maximum Longitude", min = 0, max = 200, value = 2),
      style="color:#fbe4d8 ;background-color:#0e32a1;" ),
    
    
    mainPanel(
      em("Drag the sliders to see the species count in different longitude and latitude", 
         align = "left",
         style = "color:white;"),  
      plotOutput("distPlot", width = "100%"),
      setBackgroundColor(color = c("#02407d"))
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$distPlot <- renderPlot({
    ggplot(df, aes(x = Longitude, y = Latitude, fill = `Species Count`)) +
      geom_tile() +
      scale_x_continuous(limits = c(input$minyres, input$maxyres)) +
      scale_y_continuous(limits = c(input$minxres, input$maxxres)) +
      scale_fill_viridis(discrete = FALSE) +
      theme_ipsum()
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
