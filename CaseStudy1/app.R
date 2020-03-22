#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Breweries and Beers"),

    # Sidebar with a slider input for number of bins 
    sliderInput("bins",
                "Number of bins:",
                min = 1,
                max = 50,
                value = 30),
    
    textOutput("bincount")
    
)
# Define server logic required to draw a histogram
server <- function(input, output) {

    data <- reactive({
        input$bins
    })
    
    output$bincount <- renderText(input$bins)

}

# Run the application 
shinyApp(ui = ui, server = server)
