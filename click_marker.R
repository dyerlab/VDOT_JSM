library(shiny)
library(leaflet)

ui <- fluidPage(
  leafletOutput('map')
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({leaflet()%>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(lng = -79, lat = 38, zoom = 8)})
  
  observeEvent(input$map_click, {
    click = input$map_click
    leafletProxy('map')%>%addMarkers(lng = click$lng, lat = click$lat)
  })
}

shinyApp(ui, server)

