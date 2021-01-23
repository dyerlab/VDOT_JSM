require(spatstat)
require(tidyverse)
require(shiny)
require(shinydashboard)
require(leaflet)
library(rgdal)
library(sf)
library(units)
require(DT)

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
  titlePanel("JSM Suitability"),
  
  # Input from User
  
  # Where leaflet map will be rendered
  fluidRow(
    leafletOutput("map", height= 500)
  ),
  fluidRow(
    textOutput("coords")
    
  ),
  
  fluidRow(
    box(
      width = 12,
      solidHeader = TRUE,
      DTOutput("my_datatable"))
  )
)

server <- function(session, input, output) {
  
  segments_r <- reactive({ as_tibble(segments) })
  
  output$my_datatable <- renderDT({
    
    segments_r() %>%
      datatable()
  })
  
  ###
  observeEvent(input$my_datatable_rows_selected, {
    
    selected_lats <- eventReactive(input$my_datatable_rows_selected, {
      as.list(segments_r()$lat[c(unique(input$my_datatable_rows_selected))])
    })
    
    selected_longs <- eventReactive(input$my_datatable_rows_selected, {
      as.list(segments_r()$long[c(unique(input$my_datatable_rows_selected))])
    })
    
    selected_suit <- eventReactive(input$my_datatable_rows_selected, {
      as.list(segments_r()$suit[c(unique(input$my_datatable_rows_selected))])
    })
    
    selected_names <- eventReactive(input$my_datatable_rows_selected, {
      as.list(segments_r()$GNIS_NAME[c(unique(input$my_datatable_rows_selected))])
    })
    
    selected_COMID <- eventReactive(input$my_datatable_rows_selected, {
      as.list(segments_r()$COMID[c(unique(input$my_datatable_rows_selected))])
    })
    
    # this is the data that will be passed to the leaflet in the addCircleMarkers argument,
    # as well as the popups when the points are hovered over
    map_segments <- reactive({
      tibble(lat = unlist(selected_lats()),
             lng = unlist(selected_longs()),
             suit = unlist(selected_suit()),
             GNIS_NAME = unlist(selected_names()),
             COMID = unlist(selected_COMID()))
      
    })
    
    leafletProxy("map", session) %>% 
      clearMarkers() %>% 
      addCircleMarkers(
        data = map_segments(),
        lng = ~lng,
        lat = ~lat,
        fillColor = "#611708",
        stroke = TRUE,
        color = "white",
        radius = 3,
        weight = 1,
        fillOpacity = 1.0,
        popup = paste0("lat: ", map_segments()$lat, "<br>",
                       "lng: ", map_segments()$lng, "<br>",
                       "JSM Suitability: ", map_segments()$suit, "<br>",
                       "Name: ", map_segments()$GNIS_NAME, "<br>",
                       "COM ID: ", map_segments()$COMID)
      )
    
  })
  ###
  
  
  #making reactive object of input location coordinates
  # input_pt = reactive({matrix(c(input$y, input$x), nrow = 1, ncol=2, dimnames = list(c("r1"), c("X", "Y")))})
  labs <- as.list(segments$GNIS_NAME)
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
        overlayGroups = c("County Boundaries", "Streams", "Legend","Segments"),
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
        label = lapply(labs, HTML),
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
 
    output$coords <- renderText({
      (text2)
    })
  })
  
}
# Run the application
shinyApp(ui = ui, server = server)
