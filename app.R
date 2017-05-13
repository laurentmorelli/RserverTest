#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#Yay !!!! let's put the libs here !!!!
#Package dependencies injection
pacman::p_load("gridExtra")
pacman::p_load("grid")
pacman::p_load("ggplot2")
pacman::p_load("lattice")
pacman::p_load("tidyverse")
pacman::p_load("fpc")

#We load some server content
Clusters <- read.csv("donclassif.txt", sep=";")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("cluster out of it"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "K Means : Number of K:",
                     min = 1,
                     max = 20,
                     value = 5)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("kmeansPlot")
      )
   ),
   
   
   sidebarLayout(
     sidebarPanel(
       sliderInput("delta",
                   "DB Scan : let's delta:",
                   min = 1,
                   max = 100,
                   value = 5)
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       plotOutput("dbscanPlot")
     )
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
   
   
   #OK OK OK
   
   #ok we load
   
   Clusters <- Clusters[sample(1:nrow(Clusters),nrow(Clusters)), ]
   
   #smart to encapsulate visualization fonc to avoir rework
   PlotClusters <- function(CoordPoints, Col = NULL) {
     if (is.null(Col)) {
       Col = 1:nrow(Clusters)
     }
     ggplot(data = data.frame( X = CoordPoints[,1], Y = CoordPoints[,2], Col = Col),
            aes(x = X, y = Y)) + 
       geom_point(aes(colour = Col), size = 5) + guides(color = FALSE)
   }
   
   #end
   
   #ok here we go
   output$kmeansPlot <- renderPlot({
     PlotClusters(Clusters, Col = factor(kmeans(Clusters, input$bins)$cluster)) + coord_fixed()
   
   })
   
   
   output$dbscanPlot <- renderPlot({
     PlotClusters(Clusters, Col = factor(dbscan(Clusters, input$delta / 500)$cluster)) + coord_fixed()
     
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

