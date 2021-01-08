require(spatstat)
require(tidyverse)
require(shiny)
require(shinydashboard)
require(leaflet)
library(rgdal)
library(sf)
library(units)

df <- readRDS("df.rds")
st_crs(df)
df
streams <- readRDS("streams.rds")
streams2 <- readRDS("streams2.rds")
segments <- st_as_sf(streams2)

ui <- fluidPage(
  
  # Application title
  titlePanel("Coordinates as Inputs"),
  
  # Numerio Input from User
  fluidRow (
    column(3,numericInput("x", ("Enter x-coordinate of your location"), value = 37.541290, step = 0.0000001)),
    column(3, numericInput("y", ("Enter y-coordinate of your location"), value = -77.434769 , step = 0.0000001)),
    column(4, numericInput("num", "Buff Distance", value  = 5000, step  =1))
  ),
  
  # Where leaflet map will be rendered
  fluidRow(
    leafletOutput("map", height= 500)
  )
)

server <- function(input, output) {

  #making reactive object of input location coordinates
  input_pt = reactive({matrix(c(input$y, input$x), nrow = 1, ncol=2, dimnames = list(c("r1"), c("X", "Y")))})
  #rendering the output map showing the input coordinates
  output$map = renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldGrayCanvas)%>%
      setView(lng = input_pt()[, "X"], input_pt()[, "Y"], zoom = 8) %>%
      addMarkers(data = streams)
  })

  data = reactive({
    streams$dist = st_point(input_pt()) %>%  
      st_sfc() %>% 
      st_set_crs(4326) %>% 
      st_distance(streams$geometry) %>%
      t() %>%
      set_units("m") %>%
      as.numeric()
    
    streams[order(streams$dist),]
  })
  
  #Filtering the distance data from above 
  filteredData = reactive({
    filter(data(), dist <= input$num) %>%
      mutate(popup = str_c(str_c("Name:", GNIS_NAME, sep=" "),
                           str_c("Distance:", dist, sep=" "), sep = "<br/>"))
    
  })

  icons = awesomeIcons(icon = "tint", library = "fa", squareMarker = TRUE, markerColor = "blue")
  
  observe({
    proxy = leafletProxy("map", data =filteredData()) %>% clearMarkers()
    
    proxy %>%
      clearMarkers() %>% 
      addAwesomeMarkers(icon = icons, popup = ~popup) %>% 
      addMarkers(lng = input_pt()[, "X"], input_pt()[, "Y"], label = "Your Location")
    
  })
}
# Run the application
shinyApp(ui = ui, server = server) 
