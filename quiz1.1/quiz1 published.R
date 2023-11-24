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
library(shinyWidgets)


# Define UI
ui <- fluidPage(
  titlePanel(div("Test your knowledge about marine life!",style="color:#524e88;")),

  sidebarLayout(
    sidebarPanel(
      selectInput("question", "Select a Question:", choices = c("Q1", "Q2","Q3","Q4")),
      actionButton("submitBtn", "Submit"),
      style="color:#524e88;background-color:#b4dde3;" 
    ),
    
    mainPanel(
      uiOutput("questionUI"),
      textOutput("result"),
      textOutput("timer"),
      setBackgroundColor(color = "#77afc5")
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
      correct = "10cm"
    ),
    Q2 = list(
      prompt = "What is Nautilus?",
      options = c("A type of fish", "A marine cephalopod", "A species of seaweed"),
      correct = "A marine cephalopod"
    ),
    Q3 = list(
      prompt = "Which ocean is the primary habitat for the Southern bluefin tuna?",
      options = c("Atlantic Ocean", "Southern Ocean", "Indian Ocean"),
      correct = "Indian Ocean"
    ),
    
    Q4 = list(
      prompt = "What do Hawksbill sea turtles primarily eat?",
      options = c("Jellyfish", "Seagrasses", "Small fish"),
      correct = "Jellyfish"
    ))
    
  
  
  
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
    invalidateLater(1000)  
    
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
