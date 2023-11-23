pp <- read.csv("pp.csv")
pp2<- pp1[c("Entity", "Mismanaged.plastic.waste.to.ocean.per.capita..kg.per.year.")]
cc<-read.csv("country c.csv")
cc2<-cc[c("Country","Latitude","Longitude")]
print(colnames(cc2))
colnames(cc2)[colnames(cc2) == "Country"] <- "Entity"
pc<- merge(pp2, cc2, by = "Entity", all.x = TRUE)


library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)

# Assuming pc.csv contains your data
ui <- fluidPage(
  titlePanel("Mismanaged Plastic Waste to Ocean per Capita by Country"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("timeSlider", "Simulation Time", min = 0, max = 100, value = 2),
      sliderInput("pollutionSlider", "Pollution Source Strength", min = 0, max = 100, value = 50)
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    # Filter out rows with missing or invalid coordinates
    valid_data <- pc %>% filter(!is.na(Latitude), !is.na(Longitude))
    
    leaflet(valid_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lat = ~Latitude,
        lng = ~Longitude,
        radius = ~Mismanaged_plastic,
        fill = TRUE,
        fillOpacity = 0.8,
        color = "red",
        popup = paste("Country: ", valid_data$Entity, "<br>",
                      "Pollution Rate: ", valid_data$Mismanaged_plastic)
      )
  })
  
  observe({
    time_value <- input$timeSlider
    pollution_value <- input$pollutionSlider
    
    # Calculate the spread of pollution based on user input
    updated_data <- simulatePollutionSpread(pc, time_value, pollution_value)
    
    if (!is.null(input$map)) {
      # If yes, update the existing map
      leafletProxy("map") %>%
        clearMarkers() %>%
        addCircleMarkers(
          lat = ~Latitude,
          lng = ~Longitude,
          radius = ~`Mismanaged.plastic.waste.to.ocean.per.capita..kg.per.year1`,
          fill = TRUE,
          fillOpacity = 0.8,
          color = "red",
          popup = paste("Country: ", updated_data$Entity, "<br>",
                        "Pollution Rate: ", updated_data$Mismanaged_plastic)
        )
    }
  })
}

# Function to simulate the spread of pollution
simulatePollutionSpread <- function(data, time, pollution) {
  # Simulate pollution spread using a simple linear model for illustration
  spread_factor <- 0.1  # Adjust this factor for more realistic simulation
  
  updated_data <- data %>%
    mutate(
      Mismanaged_plastic_2050 = Mismanaged_plastic + (time * spread_factor * pollution)
    )
  
  return(updated_data)
}

# Run the app
shinyApp(ui, server)

