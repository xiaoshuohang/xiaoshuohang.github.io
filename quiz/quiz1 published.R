#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load libraries
library(shiny)
library(shinyjs)

# Define UI
ui <- fluidPage(
  titlePanel("Test your knowledge about marine life!"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("question", "Select a Question:", choices = c("Q1", "Q2")),
      actionButton("submitBtn", "Submit")
    ),
    
    mainPanel(
      uiOutput("questionUI"),
      textOutput("result"),
      textOutput("timer")
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Define questions
  questions <- list(
    Q1 = list(
      prompt = "How long can Mantis Shrimp grow to?",
      options = c("10cm", "10mm", "10m"),
      correct = "10m"
    ),
    Q2 = list(
      prompt = "Which species is known for its ability to change colors and patterns rapidly?",
      options = c("Clownfish", "Cuttlefish", "Seahorse"),
      correct = "Cuttlefish"
    )
  )
  
  # Render question based on user selection
  output$questionUI <- renderUI({
    question <- input$question
    question_obj <- questions[[question]]
    
    radioButtons(
      "answer",
      question_obj$prompt,
      choices = question_obj$options,
      selected = NULL
    )
  })
  
  # Timer
  timer <- reactiveVal(60)  
  
  observe({
    invalidateLater(1000)  
    
    timer_value <- isolate(timer())
    
    if (timer_value > 0) {
      timer(timer_value - 1)
    }
  })
  
  # Update timer output
  output$timer <- renderText({
    timer_value <- timer()
    paste("Time remaining: ", timer_value, " seconds")
  })
  
  # Evaluate user's answer on button click
  observeEvent(input$submitBtn, {
    question <- input$question
    question_obj <- questions[[question]]
    
    answer <- input$answer
    
    if (!is.null(answer)) {
      if (answer == question_obj$correct) {
        output$result <- renderText("Correct!")
      } else {
        output$result <- renderText("Incorrect. Try again!")
      }
    }
  })
  
  # Stop the quiz after the timer reaches 0
  observe({
    invalidateLater(1000)  # Check every second
    
    if (timer() == 0) {
      showModal(modalDialog(
        title = "Quiz Over",
        "Time's up! Quiz has ended.",
        footer = NULL
      ))
      stopApp()
    }
  })
}

# Run the app
shinyApp(ui, server)
