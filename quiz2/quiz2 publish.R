#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
library(shiny)
library(shinyWidgets)
ui <- fluidPage(
  titlePanel("Marine Pollution Quiz"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("question", "Select a Question:", choices = c("Q1", "Q2","Q3")),
      actionButton("submitBtn", "Submit"),
      style="background-color:#2772fa ;color:#071952;"
    ),
    
    mainPanel(
      uiOutput("questionUI"),
      textOutput("result"),
      textOutput("timer"),
      setBackgroundColor(color = c("#7180d7","#daa0fe"))
    )
  )
)


# Define server
server <- function(input, output, session) {
  
  questions <- list(
    Q1 = list(
      prompt = "What is a major contributor to marine pollution?",
      options = c("Deforestation", "Air pollution", "Plastic waste"),
      correct = "Plastic waste"
    ),
    Q2 = list(
      prompt = "What percentage of marine debris is estimated to sink into the ocean's ecosystem?",
      options = c("30%", "70%", "50%"),
      correct = "70%"
    ),
    Q3=list(
      prompt = "What is the primary source of microplastics in the oceans?",
      options = c("Volcanic eruptions", "Industrial emissions", "Plastic bottles"),
      correct = "Plastic bottles"
    )
  )
  
  
 #Timer
  timer <- reactiveVal(60)
  correct_answers <- reactiveVal(0)
  
  # User interactive
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
  
  # Update timer 
  output$timer <- renderText({
    timer_value <- timer()
    paste("Time remaining: ", timer_value, " seconds")
  })
  
  # Evaluate user's answer
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

shinyApp(ui, server)
