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
# library(here)
# load(here("TidyTuesdayDiseases-Week50", "measles.rda"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Measles in the United States by Year"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput(inputId = "year",
                        "Year",
                        min = 1928,
                        max = 2002,
                        value = 1928,
                        step = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate data set for selected year based on input$year from ui.R
        measles.byyear <- filter(measles, year == input$year)

        # draw the histogram with the specified number of bins
        ggplot(measles.byyear, aes(x = state, y = percentage)) +
            theme(axis.text.x=element_text(angle=90,hjust=1)) +
            geom_col()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
