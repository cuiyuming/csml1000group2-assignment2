#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(htmltools)
library(ellipse)
library(shinyWidgets)

alignCenter <- function(el) {
    htmltools::tagAppendAttributes(el,
                                   style="margin-left:auto;margin-right:auto;"
    )
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Wine Data Analysis"),

    # Sidebar with a slider input for number of bins 
    
   
    # Sidebar panel for inputs ----
    sidebarPanel(
        prettyRadioButtons("disp", "Classification:",
                           choices = c("K-means clustering",
                                       "Gaussian Mixture Model",
                                       "Hierarchical Clustering"),
                           animation = "pulse"),
        br(),
        prettyRadioButtons("x", "X-axis",
                           choices = c("Fixed Acidity" = "1",
                                       "Volatile Acidity" = "2",
                                       "Residual Sugar" = "3",
                                       "Chlorides" = "4",
                                       "Total Sulfur Dioxide" = "5",
                                       "Density" = "6",
                                       "pH" = "7",
                                       "Sulphates" = "8",
                                       "Alcohol" = "9"
                           ), 
                           animation = "pulse", 
                           selected = "2"),
        br(),
        prettyRadioButtons("y", "Y-axis",
                           choices = c("Fixed Acidity" = "1",
                                       "Volatile Acidity" = "2",
                                       "Residual Sugar" = "3",
                                       "Chlorides" = "4",
                                       "Total Sulfur Dioxide" = "5",
                                       "Density" = "6",
                                       "pH" = "7",
                                       "Sulphates" = "8",
                                       "Alcohol" = "9"
                           ), 
                           animation = "pulse", 
                           selected = "4"),
        br(),
        prettyRadioButtons("factor", "Factor",
                           choices = c("Cluster" = "1",
                                       "Quality" = "2"
                           ), 
                           animation = "pulse", 
                           selected = "1")
    ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
