#
# Приложение "Кафе Москвы"
# основано на открытых данных Правительства Москвы
#
# Автор Андрей Макеев suitedm@gmail.com
#

library(shiny)
library(tidyverse)
library(leaflet)
library(readxl)
library(RColorBrewer)

cafe_msk <- read_excel("cafe.xlsx", sheet = "Sheet0") %>% mutate(Longitude_WGS84 = as.numeric(Longitude_WGS84), Latitude_WGS84 = as.numeric(Latitude_WGS84))


# Define UI for application that draws a histogram
ui <- bootstrapPage(
      tags$head(
        # Include our custom CSS
        includeCSS("style.css")
      ),
      
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto",
                
                h2("Общепит Москвы"),
                
                selectInput("color", "Color", "vars"),
                selectInput("size", "Size", "vars", selected = "adultpop"),
                conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                 # Only prompt for threshold when coloring or sizing by superzip
                                 numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                ),
                
                plotOutput("histCentile", height = 200),
                plotOutput("scatterCollegeIncome", height = 250)
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  #browser()
  
   filtered <- reactiveValues(data = cafe_msk)
   
   output$map <- renderLeaflet({
     map <- NA
     
     map <- leaflet() %>% addTiles()
     map <- addMarkers(map, 
                       lng = filtered$data$Longitude_WGS84, 
                       lat = filtered$data$Latitude_WGS84, 
                       clusterOptions = markerClusterOptions(), 
                       popup = paste(paste("<b>",filtered$data$Name, "</b>", sep=""),
                                     filtered$data$TypeObject,
                                     filtered$data$Address,
                                     filtered$data$PublicPhone,
                                     sep = "<br>"),
                       label = filtered$data$Name)
     map <- addProviderTiles(map, providers$CartoDB.Positron)
     #map <- addPopups(map, lng = cafe_msk$Longitude_WGS84, lat = cafe_msk$Latitude_WGS84, popup = paste(paste("<b>",cafe_msk$Name, "</b>", sep=""), cafe_msk$TypeObject, cafe_msk$Address, cafe_msk$PublicPhone, sep = "<br>"))
     
     map
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

