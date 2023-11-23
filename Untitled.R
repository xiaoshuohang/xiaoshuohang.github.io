library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
pp <- read.csv("pp.csv")
pp2<- pp1[c("Entity", "Mismanaged.plastic.waste.to.ocean.per.capita..kg.per.year.")]

ui <- fluidPage(
  titlePanel("Mismanaged Plastic Waste to Ocean per Capita by Country"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("timeSlider", "Simulation Time", min = 0, max = 100, value = 0),
      sliderInput("pollutionSlider", "Pollution Source Strength", min = 0, max = 100, value = 50)
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet(data = pp2) %>%
      addTiles() %>%
      addCircleMarkers(
        lat = ~Lat,
        lng = ~Lon,
        radius = ~`Mismanaged plastic waste to ocean per capita`,
        fill = TRUE,
        fillOpacity = 0.8,
        color = "red",
        popup = paste("Country: ", pollution_data$Country, "<br>",
                      "Pollution Rate: ", pollution_data$`Mismanaged plastic waste to ocean per capita`)
      )
  })
  
  observe({
    time_value <- input$timeSlider
    pollution_value <- input$pollutionSlider
    
    # Calculate the spread of pollution based on user input
    updated_data <- simulatePollutionSpread(pollution_data, time_value, pollution_value)
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(
        lat = ~Lat,
        lng = ~Lon,
        radius = ~`Mismanaged plastic waste to ocean per capita`,
        fill = TRUE,
        fillOpacity = 0.8,
        color = "red",
        popup = paste("Country: ", updated_data$Country, "<br>",
                      "Pollution Rate: ", updated_data$`Mismanaged plastic waste to ocean per capita`)
      )
  })
}

# Function to simulate the spread of pollution
simulatePollutionSpread <- function(data, time, pollution) {
  # Simulate pollution spread using a simple linear model for illustration
  spread_factor <- 0.1  # Adjust this factor for more realistic simulation
  
  updated_data <- data %>%
    mutate(
      `Mismanaged plastic waste to ocean per capita` = `Mismanaged plastic waste to ocean per capita` + 
        (time * spread_factor * pollution)
    )
  
  return(updated_data)
}

shinyApp(ui, server)