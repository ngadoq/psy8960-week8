library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Gender and Scores"),
  
  # Sidebar to separate from the main panel
  sidebarLayout(
    # All inputs are placed in the side panel to direct user attention
    sidebarPanel(
      # Add select input so users can filter based on gender
      selectInput('gender', "Select Gender", choices=c("Male", "Female", "All"), selected="All"),
      # Add select input so users can choose to display the error band in the plot
      selectInput('error_band', "Display Error Band?", choices=list("Display Error Band", "Suppress Error Band"), selected="Display Error Band"),
      # Add select input so users can choose whether to exclude participants who completed the assessment before August 1 2017
      selectInput('timeEnd', "Include participants who completed before August 1, 2017?", choices = c("Include", "Exclude"), selected = "Include"))
    ,
    
    # Show the plot in the main panel
    mainPanel(plotOutput("scatterplot"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Read data 
  shiny_tbl <- readRDS("shiny.rds")
  # Create reactive expression to use later for plot
  df <- reactive({
    shiny_tbl %>% 
      # Filter original data based on users' gender input and don't filter if user chooses "All"
      filter(if(input$gender != "All") {gender == input$gender} else TRUE,
             # Filter based on user's time completion input and don't filter if user chooses Include
             if(input$timeEnd == "Exclude") {timeEnd >= 2017-08-01} else TRUE)    
    })
  # Create scatter plot
  output$scatterplot <- renderPlot({
  # Recode user's error band input to since se from geom_smooth only takes logical vector
  error_band <-  ifelse (input$error_band == "Display Error Band", TRUE, FALSE)
  # Create scatterplot using ggplot to show relationship between two mean scores 
    ggplot(df(), aes(x=mean_q1_q6, y=mean_q8_q10)) + 
      # Use geom_jitter for better visualization
      geom_jitter() + 
      # Add regression line and error band based on user input
      geom_smooth(method = "lm", color = "purple", se = error_band) +
      # Add label for xasix, yxis and plot
      labs(x="Mean q1-q6",
           y="Mean q8-q10",
           title = "Scatterplot of mean q1-q6 and mean q8-q10")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

rsconnect::setAccountInfo(name='axf4o3-nga-do', token='D59DC787825A7BFA1E2A9F9917103A1A', secret='KXY7Y+hdstC0p+nLzDdzjMQreLvnCNgMXUwNDzas')

library(rsconnect)
deployApp("~/psy8960-week8/shiny/shiny_week8")
