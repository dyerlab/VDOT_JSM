require(tidyverse)
require(shiny)
require(shinydashboard)
require(datasets)
require(DT)
require(leaflet)
library(rgdal)
library(raster)
library(plotKML)
library(sf)
library(colorspace)

counties <- readRDS("counties.rds")
roads <- readRDS("roads.rds")
hsm <- readRDS("hsm_union.rds")
df <- readRDS("df.rds")

groupColors = colorFactor(palette = c("#7D0025", "#F39300", "#FDCC36"), domain = roads$ROAD_TYPE)

ui <- dashboardPage(
  
  dashboardHeader(
    title = "JSM Project"
    ),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "menu_1",
      br(),
      actionButton(
        "select_all_rows_button",
        "Select All Table Rows"
      ),
      br(),
      actionButton(
        "clear_rows_button",
        "Clear Table Selections"
      ),
      br(),
      sliderInput("bins",
                  "JSM Habitat Suitability:",
                  min = min(df$JSM_suitab),
                  max = max(df$JSM_suitab),
                  value = min(df$JSM_suitab))
    )
  ),
  
  dashboardBody(
    
    fluidRow(
      box(
        width = 12,
        solidHeader = TRUE,
        leafletOutput(
          "my_leaflet"
        )
      )
    ),
    fluidRow(
      box(
        width = 12,
        solidHeader = TRUE,
        DTOutput(
          "my_datatable"
        )
      )
    )
  )

)



my_server <- function(session, input, output) {
  
  df_r <- reactive({ as_tibble(df) })
  
  output$my_datatable <- renderDT({
    
    df_r() %>% 
      filter(JSM_suitab >= input$bins) %>%
      datatable()
  })
  
  # base map that we will add points to with leafletProxy()
  output$my_leaflet <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(lng = -79, lat = 38, zoom = 8) %>%
      addPolygons(
        data = counties,
        color = "grey",
        weight = 1.5,
        fill = NA,
        group = "County Boundaries"
        ) %>%
      addPolylines(
        data = roads,
        weight = 1,
        opacity = 1,
        color = ~groupColors(ROAD_TYPE),
        group = "Major Roads"
      ) %>%
      addPolylines(
        data = hsm,
        color = "#098F92", 
        weight = .5, 
        opacity = 1.0,
        group = "Streams") %>%
      addLayersControl(
        overlayGroups = c("County Boundaries", "Major Roads", "Streams"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    
  })
  
  observeEvent(input$my_datatable_rows_selected, {
    
    selected_lats <- eventReactive(input$my_datatable_rows_selected, {
      as.list(df_r()$Latitude[c(unique(input$my_datatable_rows_selected))])
    })
    
    selected_longs <- eventReactive(input$my_datatable_rows_selected, {
      as.list(df_r()$Longitude[c(unique(input$my_datatable_rows_selected))])
    })
    
    selected_JSM_suitab <- eventReactive(input$my_datatable_rows_selected, {
      as.list(df_r()$JSM_suitab[c(unique(input$my_datatable_rows_selected))])
    })
    
    selected_names <- eventReactive(input$my_datatable_rows_selected, {
      as.list(df_r()$GNIS_NAME[c(unique(input$my_datatable_rows_selected))])
    })
    
    selected_COMID <- eventReactive(input$my_datatable_rows_selected, {
      as.list(df_r()$COMID[c(unique(input$my_datatable_rows_selected))])
    })
    
    # this is the data that will be passed to the leaflet in the addCircleMarkers argument,
    # as well as the popups when the points are hovered over
    map_df <- reactive({
      tibble(lat = unlist(selected_lats()),
             lng = unlist(selected_longs()),
             JSM_suitab = unlist(selected_JSM_suitab()),
             GNIS_NAME = unlist(selected_names()),
             COMID = unlist(selected_COMID()) )
      
    })
    
    leafletProxy("my_leaflet", session) %>% 
      clearMarkers() %>% 
      addCircleMarkers(
        data = map_df(),
        lng = ~lng,
        lat = ~lat,
        fillColor = "#611708",
        stroke = TRUE,
        color = "white",
        radius = 3,
        weight = 1,
        fillOpacity = 1.0,
        popup = paste0("lat: ", map_df()$lat, "<br>",
                       "lng: ", map_df()$lng, "<br>",
                       "JSM Suitability: ", map_df()$JSM_suitab, "<br>",
                       "Name: ", map_df()$GNIS_NAME, "<br>",
                       "COM ID: ", map_df()$COMID)
      )
    
  })
  
  
  # create a proxy to modify datatable without recreating it completely
  DT_proxy <- dataTableProxy("my_datatable")
  
  # clear row selections when clear_rows_button is clicked
  observeEvent(input$clear_rows_button, {
    selectRows(DT_proxy, NULL)
  })
  
  # clear markers from leaflet when clear_rows_button is clicked
  observeEvent(input$clear_rows_button, {
    clearMarkers(leafletProxy("my_leaflet", session))
  })
  
  # select all rows when select_all_rows_button is clicked
  observeEvent(input$select_all_rows_button, {
    selectRows(DT_proxy, input$my_datatable_rows_all)
  })
  
}

shinyApp( ui, server
)

