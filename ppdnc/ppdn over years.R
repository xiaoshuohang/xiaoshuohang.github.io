ppro <- read.csv("pproduction.csv")
plastic1 <- ppro[c("Year", "Annual.plastic.production.between.1950.and.2019")]
colnames(plastic1)[colnames(plastic1) == "Annual.plastic.production.between.1950.and.2019"] <- "plastic_production"

library(shiny)
library(ggplot2)
library(shinyWidgets)

# Define UI
ui <- fluidPage(
  titlePanel(div("Plastic Production Over the Years",style = "color:#feb170;")),
  chooseSliderSkin("Square"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearRange", "Select Year Range", 
                  min = min(plastic1$Year), max = max(plastic1$Year), 
                  value = c(min(plastic1$Year), max(plastic1$Year)), step = 1),
      style="color:#feb170 ;background-color:#377ab9;" ),
   
    mainPanel(
      plotOutput("lineChart"),
      setBackgroundColor(color = c("#377ab9"))
    )
  )
)


# Define server
server <- function(input, output) {
  filtered_data <- reactive({
    subset(plastic1, Year >= input$yearRange[1] & Year <= input$yearRange[2])
  })
  output$lineChart <- renderPlot({
    ggplot(filtered_data(), aes(x = Year, y = plastic_production)) +
      geom_line() +
      labs(title = "Plastic Production Over the Years", x = "Year", y = "Plastic Production")
  })
}

# Run the app
shinyApp(ui, server)
