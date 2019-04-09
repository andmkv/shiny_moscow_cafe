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

cafe_msk <- read_excel("cafe.xlsx", sheet = "Sheet0") %>% mutate(Longitude_WGS84 = as.numeric(Longitude_WGS84), Latitude_WGS84 = as.numeric(Latitude_WGS84), TypeObject = as.factor(TypeObject), SeatsCount = as.numeric(SeatsCount)) #%>% head(100)
object_types <- levels(unique(cafe_msk$TypeObject))


# Define UI for application that draws a histogram
ui <- bootstrapPage(
      tags$head(
        # Include our custom CSS
        includeCSS("style.css")
      ),
      
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto",
                
                h2("Весь общепит Москвы"),
                
                selectInput("select_type", "Фильтр по типу", c("Все типы", object_types)),
                
                sliderInput("slider_seats", "Количество мест", min = min(cafe_msk$SeatsCount), max = max(cafe_msk$SeatsCount), value = c(0, max(cafe_msk$SeatsCount))),
                
                actionButton("btn_apply", "Применить фильтр")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
  #browser()
  
   filtered <- reactiveValues(data = cafe_msk)
   
   #browser()
   
   observeEvent(input$btn_apply, {
     if (input$select_type == "Все типы") {
       #browser()
       filtered_data <- cafe_msk %>% filter(SeatsCount >= input$slider_seats[1], SeatsCount <= input$slider_seats[2])
     } else {
       filtered_data <- cafe_msk %>% filter(TypeObject == input$select_type, SeatsCount >= input$slider_seats[1], SeatsCount <= input$slider_seats[2])
     }
     
     if(nrow(filtered_data) == 0) {
       filtered$data <- cafe_msk
       updateSliderInput(session, "slider_seats", value = c(0, max(cafe_msk$SeatsCount)))
       updateSelectInput(session, "select_type", selected = "Все типы")
       
       sendSweetAlert(
         session = session,
         title = "Ошибка!",
         text = "Ничего не найдено.",
         type = "error"
       )
     } else {
       filtered$data <- filtered_data
     }
   })
   
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

