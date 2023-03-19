library(shiny)
saveRDS(file="../data/shiny.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput('gender', "Select Gender", choices=c("Male", "Female", selected="All")),
            selectInput('error_bar', "Display Error Bar?", choices=c("Display Error Band", "Suppress Error Band"), selected="Display Error Band"),
            selectInput('completion_date', "Include participants who completed before August 1, 2017?", choices = c("Include", "Exclude"), selected = "Include"))
        ,

        # Show a plot of the generated distribution
        mainPanel(plotOutput("scatterplot"))
        )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  error_bar <- reactive(if (input$error_bar == "Display Error Band") TRUE else FALSE)

  output$scatterplot <- renderPlot({
        
        # draw the histogram with the specified number of bins
        shiny_tbl %>% 
          filter(
            gender == input$gender,
            timeEnd <= input$completion_date
          ) %>% 
          ggplot(aes(x=mean_q1_q6, y=mean_q8_q10)) + 
          geom_jitter() + 
          geom_smooth(method = "lm", color = "purple", se = error_bar ) +
          labs(x="Mean q1-q6",
               y="Mean q8-q10",
               title = "Scatterplot of mean q1-q6 and mean q8-q10")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
