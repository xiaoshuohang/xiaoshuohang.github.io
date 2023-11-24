#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
## Load required libraries
library(shiny)
library(ggplot2)
library(tidyverse)
library(shinyWidgets)
# Read data 
data <- read.csv("app4.csv")

# Define the UI
ui <- fluidPage(
  titlePanel(div("The most popular types of wastes",style = "color:#cfd89f;")),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Locations:", choices = unique(data$Countries)),
      style="color:#cfd89f;background-color:#3e75ad"
     ),
    
    mainPanel(
      plotOutput("wastagePlot"),setBackgroundColor(color = c("#3e75ad"))
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


