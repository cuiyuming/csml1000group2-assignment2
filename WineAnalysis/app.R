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
                           choices = c("Original(Quality)",
                                        "K-means clustering",
                                       "Hierarchical Clustering"),
                           animation = "pulse"),
        br(),
        prettyRadioButtons("x", "X-axis",
                           choices = c("Fixed Acidity" = "2",
                                       "Volatile Acidity" = "3",
                                       "Critric Acid" = "4",
                                       "Residual Sugar" = "5",
                                       "Chlorides" = "6",
                                       "Free Sulfur Dioxide" = "7",
                                       "Total Sulfur Dioxide" = "8",
                                       "Density" = "9",
                                       "pH" = "10",
                                       "Sulphates" = "11",
                                       "Alcohol" = "12"
                           ), 
                           animation = "pulse", 
                           selected = "2"),
        br(),
        prettyRadioButtons("y", "Y-axis",
                           choices = c("Fixed Acidity" = "2",
                                       "Volatile Acidity" = "3",
                                       "Critric Acid" = "4",
                                       "Residual Sugar" = "5",
                                       "Chlorides" = "6",
                                       "Free Sulfur Dioxide" = "7",
                                       "Total Sulfur Dioxide" = "8",
                                       "Density" = "9",
                                       "pH" = "10",
                                       "Sulphates" = "11",
                                       "Alcohol" = "12"
                           ), 
                           animation = "pulse", 
                           selected = "4"),
        br(),
        checkboxInput('ellipseGroup', label = "Ellipses", value=TRUE),
        sliderInput("alpha", "Level (Ellipses)", value = 0.8, min = 0.25, max = 0.95, step=0.025)
    ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                tabPanel("Visual Output", plotOutput("plot1", height="500px")),
                tabPanel("Numeric Output", tableOutput("table"))
            )
        )
    
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    load("../data/shiny.RData")

    r <- reactive({
        alpha <- input$alpha
        j1 <- as.numeric(input$x)
        j2 <- as.numeric(input$y)
        disp <- input$disp
        
        class.km <- as.factor(raw$cluster_kmeans)
        class.hc <- as.factor(raw$cluster_hclust)
        class.or <- as.factor(raw$quality)
        raw$quality <- class.or
        raw[,22] <- class.km
        raw[,23] <- class.hc

        df_ell <- data.frame()
        d <-raw[,  c(j1, j2, 13)]
         if (disp=="K-means clustering") {
             d <- cbind(raw[,c(j1,j2)],class.km)
         } else if (disp=="Hierarchical Clustering") {
             d <- cbind(raw[,c(j1,j2)],class.hc)
         } 
    
        df_ell <- raw[,  c(13:16, 18, 19)]
        
        r <- list(df_ell=df_ell, d=d)
        
        return(r)
    })
    
    output$plot1 <- renderPlot({
        df_ell <- r()$df_ell
        d <- r()$d
        # col <- c("#FF1BB3","#A7FF5B","#99554D","#839AD3", "#007B57")
        col <- c("#7DB0DD","#86B875","#E495A5","#208575")
        
        pl <- ggplot(data=d, aes_string(x=d[,1],y=d[,2], colour=d[,3])) + geom_point(size=2) +
            xlab(names(d)[1]) + ylab(names(d)[2]) + theme_bw() 
   
        if (input$disp=="Original(Quality)") {
            pl <- ggplot(data=d, aes_string(x=d[,1],y=d[,2], colour=d[,3])) + geom_point(size=2) +
                xlab(names(d)[1]) + ylab(names(d)[2]) + theme_light()
        }else{
            pl <- ggplot(data=d, aes_string(x=d[,1],y=d[,2], colour=d[,3])) + geom_point(size=2) +
                xlab(names(d)[1]) + ylab(names(d)[2]) + theme_bw() 
        }
        

        if (input$ellipseGroup==TRUE) {
            pl <- pl + stat_ellipse(level=input$alpha, size=1) 
        }
        return(pl)
    })
    
    output$table <- renderTable({
        r()
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
