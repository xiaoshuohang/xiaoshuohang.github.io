
# Load libraries
library(shiny)
library(shinyjs)

# Define UI
ui <- fluidPage(
  titlePanel("Quiz App"),
  
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
      prompt = "What is the largest living species of whale?",
      options = c("Humpback Whale", "Blue Whale", "Killer Whale (Orca)"),
      correct = "Blue Whale"
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
  timer <- reactiveVal(0)  # Initialize timer value
  
  observe({
    invalidateLater(1000)  # Update every second
    
    timer_value <- isolate(timer())
    
    if (timer_value < 60) {
      timer(timer_value + 1)
    }
  })
  
  # Update timer output
  output$timer <- renderText({
    timer_value <- timer()
    paste("Time remaining: ", 60 - timer_value, " seconds")
  })
  
  # Update timer output
  output$timer <- renderText({
    invalidateLater(1000)  # Update every second
    timer()
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
  
  # Stop the quiz after 2 minutes
  observe({
    invalidateLater(1000)  # Check every second
    
    if (timer() <= 0) {
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
