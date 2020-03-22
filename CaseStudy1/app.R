#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(openintro)


beers <- read.csv("data/Beers.csv")
breweries <- read.csv("data/Breweries.csv")

beer_data <- merge(beers,breweries,by.x = "Brewery_id", by.y = "Brew_ID")

# clean up data
beer_data <- beer_data %>% 
  mutate(fullname = abbr2state(str_trim(as.character(State)))) %>% 
  rename(Beer = Name.x) %>%           # clean up munged names from merge
  rename(Brewery = Name.y) %>%        # clean up munged names from merge
  rename(Brewery_ID = Brewery_id) %>% # Make anything with _ID the same
  filter(!is.na(IBU)) %>% 
  filter(!is.na(ABV))

states <- sort(abbr2state(str_trim(as.character(levels(beer_data$State)))))

# select out ipas
ipas <- beer_data  %>%
  filter(!is.na(IBU) & !is.na(ABV)) %>%
  filter(str_detect(Style, regex("\\bipa\\b", ignore_case = TRUE, multiline = FALSE))) %>%
  mutate(ipa=TRUE)

# select out other ales
otherAles <- beer_data %>%
  filter(!is.na(IBU) & !is.na(ABV)) %>%
  filter(str_detect(Style, regex("\\bale\\b", ignore_case = TRUE, multiline = FALSE))) %>%
  filter(!str_detect(Style, regex("\\bipa\\b", ignore_case = TRUE, multiline = FALSE))) %>%
  mutate(ipa=FALSE)

allAles <- rbind(ipas, otherAles)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Selected Beers from the US"),
  
  
  sidebarLayout(
    sidebarPanel("Alcohol By Volume (ABV)",
                 sliderInput("abv_bins",
                             "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30),
                 checkboxInput("abv_state_filter", "Filter by State?"),
                 selectInput("abv_state",
                             "Choose State to display",
                             states,
                             selectize = TRUE,
                             multiple = FALSE),
                 radioButtons("abv_type", 
                              "Display Type",
                              c("Histogram"="hist","Boxplot"="box"),
                              selected = "hist")
    ),
    
    mainPanel(plotOutput("abv_plot", width = 500, height = 400))
  ),
  
  sidebarLayout(
    sidebarPanel("International Bitterness Units (IBU)",
                 sliderInput("ibu_bins",
                             "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30),
                 checkboxInput("ibu_state_filter", "Filter by State?"),
                 selectInput("ibu_state",
                             "Choose State to display",
                             states,
                             selectize = TRUE,
                             multiple = FALSE),
                 radioButtons("ibu_type", 
                              "Display Type",
                              c("Histogram"="hist","Boxplot"="box"),
                              selected = "hist")
    ),
    mainPanel(plotOutput("ibu_plot", width = 500, height = 400))
  ),
  sidebarLayout(
    sidebarPanel("ABV vs IBU",
                 checkboxInput("scatter_regression",
                               "See the regression line?"),
                 checkboxInput("scatter_state_filter", "Filter by State?"),
                 selectInput("scatter_state",
                             "Choose State to display",
                             states,
                             selectize = TRUE,
                             multiple = FALSE)
    ),
    mainPanel(
      plotOutput("scatter_plot",
                 width = 500, height = 400)
    )
  ),
  sidebarLayout(
    sidebarPanel("Correlation of ABV vs. IBU",
                 checkboxInput("duck_plot_check",
                               "See the Correlation Contours?")
                 ),
    mainPanel(
      plotOutput("duck_plot",
                 width = 500,
                 height = 400),
      verbatimTextOutput("duck_explain")
    )
  )
) 

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  abv_filtered <- reactive({
    if(input$abv_state_filter == TRUE) {
      dplyr::filter(beer_data, fullname == input$abv_state)
    } else {
      beer_data
    }
  })
  ibu_filtered <- reactive({
    if(input$ibu_state_filter == TRUE) {
      dplyr::filter(beer_data, fullname == input$ibu_state)
    } else {
      beer_data
    }
  })
  scatter_filtered <- reactive({
    if(input$scatter_state_filter == TRUE) {
      dplyr::filter(beer_data, fullname == input$scatter_state)
    } else {
      beer_data
    }
  })
  
  
  # Build the ABV Plots
  output$abv_plot <- renderPlot({
    
    if(input$abv_type == "hist") {
      abv_filtered() %>% 
        filter(!is.na(ABV)) %>% 
        ggplot(aes(x=ABV)) + 
        geom_histogram(bins = input$abv_bins) +
        labs(title = "Alcohol Histogram",
             x = "Alcohol by Volume"
        )
    } else {
      abv_filtered() %>% 
        filter(!is.na(ABV)) %>% 
        ggplot(aes(y=ABV)) + 
        geom_boxplot() +
        scale_x_discrete(0.5) +
        labs(title = "Alcohol Boxplot",
             y = "Alcohol by Volume"
        )
    }
  })
  
  # Build the IBU plots
  output$ibu_plot <- renderPlot({
    if(input$ibu_type == "hist") {
      ibu_filtered() %>% 
        filter(!is.na(IBU)) %>% 
        ggplot(aes(x=IBU)) + 
        geom_histogram(bins = input$ibu_bins) +
        labs(title = "Bitterness Histogram",
             x = "International Bitterness Unit",
             y = "Count"
        )
    } else {
      ibu_filtered() %>% 
        filter(!is.na(IBU)) %>% 
        ggplot(aes(y=IBU)) + 
        geom_boxplot() +
        scale_x_discrete(0.5) +
        labs(title = "Bitterness Boxplot",
             y = "International Bitterness Unit"
        )
    }
  })
  
  # build the ABV vs IBU Scatter plot
  output$scatter_plot <- renderPlot({
    if(input$scatter_regression == TRUE) {
      scatter_filtered() %>% 
        ggplot(aes(x=IBU, y=ABV)) +
        geom_point(position = "jitter") +
        geom_smooth(method="lm") +
        labs(title = "ABV vs. IBU With Regression",
             x = "IBU",
             y = "ABV"
        )
      
      
    }else {
      scatter_filtered() %>% 
        ggplot(aes(x=IBU, y=ABV)) +
        geom_point(position = "jitter") +
        labs(title = "ABV vs. IBU Without Regression",
             x = "IBU",
             y = "ABV"
        )

    }
  })
  
  # build the duck plot
  output$duck_plot <- renderPlot({
    if(input$duck_plot_check == TRUE){
      allAles %>% ggplot(aes(x=IBU, y=ABV)) +
        geom_point(aes(color = ipa)) +
        geom_density2d(aes(color=ipa)) +
        labs(title = "IBU x ABV for IPAs and Other Ales", color = "IPA?")
    } else {
      allAles %>% ggplot(aes(x=IBU, y=ABV)) +
        geom_point(aes(color = ipa)) +
        labs(title = "IBU x ABV for IPAs and Other Ales", color = "IPA?")

    }
  })
  
  output$duck_explain <- renderText({
    "This graph shows concentric lines that represent\nPearson's Correlation Coefficient for the groups \n'India Pale Ale' and 'All Other Ales'"
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)
