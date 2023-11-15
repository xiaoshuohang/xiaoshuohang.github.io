# Define UI
ui <- fluidPage(
  titlePanel("Test your knowledge about marine life!"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("question", "Select a Question:", choices = c("Q1", "Q2","Q3")),
      actionButton("submitBtn", "Submit")
    ),
    
    mainPanel(
      uiOutput("questionUI"),
      textOutput("result"),
      textOutput("timer"),
      
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
    ),
    Q3=list(
      prompt = "Where can you find the leafy dragon in Australia?",
      options = c("Northern and eastern coasts", "Southern and western coasts", " Central inland regions"),
      correct = "Southern and western coasts"
    )
  )
  
  # Initialize reactive values
  timer <- reactiveVal(60)
  correct_answers <- reactiveVal(0)
  
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
  observe({
    invalidateLater(1000)  # Update every second
    
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
        correct_answers(correct_answers() + 1)
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
  
  # Show picture when all answers are correct
  output.pictureShown <- reactive({
    correct_answers() == length(questions)
  })
}

# Run the app
shinyApp(ui, server)
