library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyWidgets)

# Sample data
data <- data.frame(
  Category = c("Polystyrene Foam", "Other Plastic Items", "Plastic Food Packaging", 
               "Glass Pieces", "Others - Shoes, Foil, Medical Waste, Rubber, Fabric, Fishing Line",
               "Recyclables - PET Bottles", "Recyclables (Al Cans, Tin Cans, Glass Bottles)",
               "Paper", "Plastic Bags & Straws"),
  Values = c(4.18, 1.72, 1.20, 0.88, 0.619, 0.50, 0.47, 0.29, 0.02)
)

# Shiny app
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("selected_categories", "Select the waste category",
                         choices = data$Category,
                         selected = data$Category),
      style="color:#cfd89f;background-color:#3e75ad"
    ),
    mainPanel(
      plotlyOutput("bar_plot"),setBackgroundColor(color = c("#d3c3fc","#9ac5fc"))
    )
  )
)

server <- function(input, output, session) {
  # Reactive expression for filtered data based on the checkbox input
  filtered_data <- reactive({
    data %>%
      filter(Category %in% input$selected_categories)
  })
  
  # Render the interactive bar plot
  output$bar_plot <- renderPlotly({
    ggplot(filtered_data(), aes(x = reorder(Category, Values), y = Values)) +
      geom_bar(stat = "identity", fill = "#cfd89e") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1.5)) +
      labs(title = "Selected Categories by Values", x = "Category", y = "Values") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 2)) +
      geom_col(fill = "#cfd89e") +
      scale_y_continuous(labels = scales::percent_format(scale = 100)
      )
  })
}

shinyApp(ui, server)
