library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

# Sample data
data <- data.frame(
  Category = c("Polysterin, Foam", "Other Plastic items", "Plastic food packaging", 
               "Glass pieces", "Others - Shoes, Foil, medical waste, rubber, fabric, fishing line",
               "Recyclables - PET Bottles", "Recyclables (Al Cans, Tin Cans, Glass bottles)",
               "Paper", "Plastic bags & Straws"),
  Values = c(4.18, 1.72, 1.20, 0.88, 0.619, 0.50, 0.47, 0.29, 0.02)
)

# Shiny app
ui <- fluidPage(
  titlePanel("Category Values Visualization"),
  sidebarLayout(
    sidebarPanel(
    
      sliderInput("num_categories","Number of Categories:", 
                  min = 1, max = nrow(data), value = 1)
    ),
    mainPanel(
      plotlyOutput("bar_plot")
    )
  )
)

server <- function(input, output, session) {
  # Reactive expression for filtered data based on the slider input
  filtered_data <- reactive({
    head(data[order(data$Values), ], input$num_categories)
  })
  
  # Render the interactive bar plot
  output$bar_plot <- renderPlotly({
    ggplot(filtered_data(), aes(x = reorder(Category, Values), y = Values)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1.5)) +
      labs(title = "Top Categories by Values", x = "Category", y = "Values") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 2)) +
      geom_col( fill = "skyblue") +
      scale_y_continuous(labels = scales::percent_format(scale = 100))
  })
}

shinyApp(ui, server)



