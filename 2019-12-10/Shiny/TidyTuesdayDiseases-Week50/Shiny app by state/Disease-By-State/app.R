library(shiny)
library(tidyverse)

load("bystate.rda")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Disease by State"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "state",
                         "State",
                         levels(as.factor(bystate$state)))
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
        # generate data set for selected year based on input$state from ui.R
        bystate.bystate <- filter(bystate, state == input$state)
        
        # draw the histogram with the specified number of bins
        ggplot(bystate.bystate, aes(x = year, y = count, col = disease)) +
            theme(axis.text.x=element_text(angle=90,hjust=1)) +
            geom_point() +
            geom_line() +
            labs(col = "Disease", x = "Count", y = "Year")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
