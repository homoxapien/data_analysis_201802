#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(plyr)
library(ggplot2)

### Set working directory
setwd("~/documents/data_science_R/happy_country")

### Import data from local device
social <- read.csv("data/2017_SPI.csv")#select year: 2014-2017

### Select key components for clustering
social <- social[,c(1,4,5,6)]
### Median imputation
social[is.na(social[,2]),2] <- median(social[,2], na.rm=TRUE)
social[is.na(social[,3]),3] <- median(social[,3], na.rm=TRUE)
social[is.na(social[,4]),4] <- median(social[,4], na.rm=TRUE)
### Country name
library(plyr)
social$Country <- as.character(mapvalues(social$Country, 
                                         from = c("United States", "United Kingdom", "Trinidad and Tobago"),
                                         to=c("USA", "UK", "Trinidad")))
library(useful)
map.world <- map_data("world")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("SPI Clustering"),
   
   # Sidebar with a slider input for number of centers
   sidebarLayout(
      sidebarPanel(
         sliderInput(inputId="centers",
                     label="Number of clusters:",
                     min = 1,
                     max = 5,
                     value = 3)
      ),
      
      # Show a plot of the world
      mainPanel(
         plotOutput("worldPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$worldPlot <- renderPlot({
     km.out <- kmeans(social[,-1], centers=input$centers, nstart=20, iter.max=50)
     social$cluster <- km.out$cluster
     
     #library(dplyr)
     map.world_joined <- left_join(map.world, social, by = c('region' = 'Country'))
     #library(ggplot2)
     ggplot() +
       geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill=cluster, color=cluster)) +
       labs(title = "Applied Clustering Social Progress Index",
            x=NULL, y=NULL) +
       coord_equal() +
       theme(panel.grid.major=element_blank(),
             panel.grid.minor=element_blank(),
             axis.text.x=element_blank(),
             axis.text.y=element_blank(),
             axis.ticks=element_blank(),
             panel.background=element_blank()
       )
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

