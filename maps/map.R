pp <- read.csv("pp.csv")
pp2 <- pp[c("Entity", "Mismanaged.plastic.waste.to.ocean.per.capita..kg.per.year.")]

cc <- read.csv("country c.csv")
cc2 <- cc[c("Country", "Latitude", "Longitude")]
colnames(cc2)[colnames(cc2) == "Country"] <- "Entity"

pc <- merge(pp2, cc2, by = "Entity", all.x = TRUE)
colnames(pc)[colnames(pc)=="Mismanaged.plastic.waste.to.ocean.per.capita..kg.per.year."]<-"plastic_pollution"


# Load required libraries
library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(shinyWidgets)

ui <- fluidPage(
  mainPanel(align="center",
   radioButtons("timeSelector", "Select Time Point", choices = c("Current", "2040")) ,      
    leafletOutput("map",width="150%",height="300px"), setBackgroundColor(color = c("#377ab9"))
   
  ),
)

# Define server
server <- function(input, output, session) {
  simulateFuturePollution <- function(data) {
    data %>% mutate(plastic_pollution_future = plastic_pollution * 3)
  }
  
  # Filter out rows with missing or invalid coordinates
  valid_data <- pc %>% filter(!is.na(Latitude), !is.na(Longitude))
  
  # Render the map based on the selected time point
  output$map <- renderLeaflet({
    time_point <- input$timeSelector
    
    # Customize the markers based on the selected time point
    if (time_point == "Current") {
      markers_data <- valid_data %>% mutate(radius = plastic_pollution,
                                            color = "red",
                                            popup = paste("Country: ", Entity, "<br>",
                                                          "Plastic Pollution Rate: ", plastic_pollution))
    } else {
      future_data <- simulateFuturePollution(valid_data)
      markers_data <- future_data %>% mutate(radius = plastic_pollution_future,
                                             color = "blue",
                                             popup = paste("Country: ", Entity, "<br>",
                                                           "Future Plastic Pollution Rate: ", plastic_pollution_future))
    }
    
    
    leaflet(data = markers_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lat = ~Latitude,
        lng = ~Longitude,
        radius = ~radius,
        fill = TRUE,
        fillOpacity = 0.02,
        stroke=TRUE,
        weight = 10,
        color = ~color,
        popup = ~popup
      )
  })
}

# Run the app
shinyApp(ui, server)