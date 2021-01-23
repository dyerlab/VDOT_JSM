require(spatstat)
require(tidyverse)
require(shiny)
require(shinydashboard)
require(leaflet)
library(rgdal)
library(sf)
library(units)


streams <- readRDS("streams2.rds")
hsm <- readRDS("hsm_union.rds")
counties <- readRDS("counties.rds")
roads <- readRDS("roads.rds")
segments <- readRDS("segments.rds")

pal <- colorBin(
  palette = c("#b2d8d8", "#66b2b2", "#008080", "#006666", "#004c4c"),
  domain = segments$suit,
  bins = 5)


ui <- fluidPage(
  
  # Application title
  titlePanel("Coordinates as Inputs"),
  
  # Input from User
  fluidRow (
    column(4, numericInput("num", "Buff Distance", value = 5000, step  =1)),
    column(3, verbatimTextOutput("Coords"))
  ),
  
  # Where leaflet map will be rendered
  fluidRow(
    leafletOutput("map", height= 500)
  ),
  fluidRow(
    textOutput("coords")
    
  )
)

server <- function(input, output) {
  
  #making reactive object of input location coordinates
  input_pt = reactive({matrix(c(input$y, input$x), nrow = 1, ncol=2, dimnames = list(c("r1"), c("X", "Y")))})
  #rendering the output map showing the input coordinates
  output$map = renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldGrayCanvas)%>%
      setView(lng = -79.6, 37, zoom = 8) %>%
      addPolygons(
        data = counties,
        color = "grey",
        weight = 1.5,
        fill = NA,
        group = "County Boundaries"
      ) %>%
      # Add all streams as one line
      addPolylines(
        data = hsm,
        color = "grey", 
        weight = .5, 
        opacity = 1.0,
        group = "Streams"
      ) %>%
      # Set zoom levels for all streams layer
      groupOptions(
        "Streams" , zoomLevels = 7:18
      ) %>%
      # Add layer control options using group assignments
      addLayersControl(
        overlayGroups = c("County Boundaries", "Streams", "Legend", "Circle","Segments"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addPolylines(
        data = segments,
        color = ~pal(suit),
        weight = 3,
        opacity = 1,
        stroke = TRUE,
        highlightOptions = highlightOptions(color="orange", weight=4),
        group = "Segments",
        popup = paste0(
          "JSM Suitability: ", segments$suit, "<br>",
          "Name: ", segments$GNIS_NAME, "<br>",
          "COM ID: ", segments$COMID, "<br>",
          "Latitude:", segments$lat, "<br>",
          "Longitude:", segments$long)
      ) %>%
      groupOptions(
        "Segments", 
        zoomLevels = 10:18
      ) %>%
      addLegend(data = segments,
                pal = pal,
                values = ~suit,
                opacity = 0.7,
                title = "Suitability Score",
                position = "bottomright",
                group = "Legend"
      ) %>%
      groupOptions(
        "Legend",
        zoomLevels = 10:18
      )
  })
  
  
  observeEvent(input$map_click, {
    
    click = input$map_click
    if(is.null(click))
      return()
    
    text<-paste("Longtitude:", round(click$lng, digits=5),
                "Latitude:", round(click$lat, digits = 5)  
    )
    text2 <- paste(" ", text)
    
    
    proxy <- leafletProxy("map")
    
    ## This displays the pin drop circle
    proxy %>% 
      clearGroup("Circle") %>%
      addCircles(click$lng, 
                 click$lat, 
                 radius=input$num, 
                 color="red",
                 fillColor = "transparent",
                 group = "Circle")
    
    
    
    output$coords <- renderText({
      (text2)
    })
  })
  
}
# Run the application
shinyApp(ui = ui, server = server)
