# Load required libraries
library(shiny)
library(ggplot2)
library(tidyverse)

# Read data 
data <- read.csv("app4.csv")

# Define the UI
ui <- fluidPage(
  titlePanel("The most popular types of wastes"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", choices = unique(data$Countries))
    ),
    
    mainPanel(
      plotOutput("wastagePlot")
    )
  )
)

# Define the server
server <- function(input, output) {
  output$wastagePlot <- renderPlot({
    selected_country <- filter(data, Countries == input$country)
    
    # Reshape the data for ggplot
    data_long <- pivot_longer(selected_country, cols = -Countries, 
                              names_to = "Category", values_to = "WastageCount")
    
    ggplot(data_long, aes(x = Category, y = WastageCount, group = Countries, color = Countries)) +
      geom_line() +
      labs(title = paste("Wastage Trends in", input$country),
           x = "Categories",
           y = "Wastage Count") +
      scale_x_discrete() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  })
}



# Run the application
shinyApp(ui, server)
