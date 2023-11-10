#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(hrbrthemes)
library(viridis)

df <- read_csv("marine1.csv")
df1 <- df %>% select(Latitude, `Species Count`)



library(shiny)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Different locations have different marine species densities"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("minxres",
                  "Minimum Latitude",
                  min = -120,
                  max = 0,
                  value =2),
      sliderInput("maxxres",
                "Maximum Latitude",
                min = 0,
                max = 120,
                value =2),
      sliderInput("minyres",
                "Minimum Longitude",
                min = -120,
                max = 0,
                value =2),
      sliderInput("maxyres",
            "Maximum Longitude",
            min = 0,
            max = 200,
            value =2)
            ),
    
    # Show a plot of the generated distribution
    mainPanel( h1("Drag the sliderbars to see the species count in different longtitude and latitude",align = "center"),
               em("This a marine life distribution map of a specific area on Earth,explore the marine life in this particular region by use the slider bar beside to set the ranges of the longtitude and latitude.",align="left"),
                plotOutput("distPlot"),
     
      
          
       )
    
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
   
    ggplot(df,aes(x=Latitude,y=Longitude,fill=`Species Count`)) + geom_tile() +
      scale_x_continuous(limits=c(input$minxres,input$maxxres))+
      scale_y_continuous(limits=c(input$minyres,input$maxyres))+ 
      scale_fill_viridis(discrete=FALSE) +
      theme_ipsum()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



