#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plyr)
library(dplyr)
library(rvest)
library(magrittr)
library(ggplot2)
library(ggmap)
#library(map_data) #Currently not available, so I use ggmap instead

### Set working directory (when testing on local machine)
#setwd("~/documents/data_science_R/happy_country/MeasureSocialProgress")

### Import data from local device
social <- read.csv("data/2017_SPI.csv")#select year: 2014-2017

### Preprocessing
social <- social[,c(1,4,5,6)]
### rename column headers by replacing space with underscore
colnames(social) <- gsub("\\.", "_", colnames(social), perl=TRUE)
colnames(social) <- c("Country", "Basic_human_needs", "Foundation_of_wellbeing", "Opportunity")
### Median imputation
social[is.na(social[,2]),2] <- median(social[,2], na.rm=TRUE)
social[is.na(social[,3]),3] <- median(social[,3], na.rm=TRUE)
social[is.na(social[,4]),4] <- median(social[,4], na.rm=TRUE)
### Country name (according to map_data("world") later on)
library(plyr)
social$Country <- as.character(mapvalues(social$Country, 
                                         from = c("United States", "United Kingdom", "Trinidad and Tobago"),
                                         to=c("USA", "UK", "Trinidad")))

### Import the data set from the happiness report of 2017 published by UN
url <- "https://en.wikipedia.org/wiki/World_Happiness_Report"
library(rvest)
library(magrittr)
happy <- read_html(url) %>% 
  html_nodes("table") %>% 
  extract2(1) %>% 
  html_table()

### Preprocessing
happy <- happy[c(3,6:11)]
### Rename column headers by replacing space with underscore
colnames(happy) <- gsub(" ", "_", colnames(happy), perl=TRUE)
### Coerce character data columns to numeric
happy[, 2:7] <- sapply(happy[, 2:7], as.numeric)
### Median imputation
for(i in 1:ncol(happy)){
  happy[is.na(happy[,i]),i] <- median(happy[,i], na.rm=TRUE)
}
### Country name (according to map_data("world") later on)
library(plyr)
happy$Country <- as.character(mapvalues(happy$Country, 
                                         from = c("United States", "United Kingdom", "Trinidad and Tobago"),
                                         to=c("USA", "UK", "Trinidad")))
###combine social and happy to measure SocialProgress
SocialProgress <- inner_join(happy, social, by = c('Country' = 'Country'))

library(useful)
map.world <- map_data("world")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Measuring Social Progress in the World"),
   
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
     km.out <- kmeans(SocialProgress[,-1], centers=input$centers, nstart=20, iter.max=50)
     SocialProgress$cluster <- as.factor(km.out$cluster)
     
     #library(dplyr)
     map.world_joined <- left_join(map.world, SocialProgress, by = c('region' = 'Country'))
     #library(ggplot2)
     ggplot() +
       geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill=cluster, color=cluster)) +
       labs(title = "Clustering Countries by Social Progress ",
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

