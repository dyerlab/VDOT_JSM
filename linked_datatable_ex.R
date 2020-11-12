require(tidyverse)
require(shiny)
require(shinydashboard)
require(datasets)
require(DT)
require(leaflet)

load_data()


### In the chunk below we have defined our dataset as a reactive object
#even though we will not be modifying the dataset itself,
#and defined the DT::datatable from which we will drive the interactivity of our application

my_server <- function(session, input, output {
  df_r <- reactive({as_tibble(df) })
  
  output$my_datatable <- renderDT({
    df_r() %>%
      datatable()
  })
  
  #other server code goes here#
  
})

  
# The next step is to set up two action buttons linked to the datatable by a datatableProxy
  #The datatableProxy will allow us ot modify the datatable without regenerating the table
  
  #In the chunk below, we establish the leaflet onto which we will eventually plot our selected
  #observations
  
output$my_leaflet <- renderLeaflet({
  
  leaflet() %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
    setView(lng = -79, lat = 38, zoom = 8) %>%
    
    
})

# Now we need to make a reactive tibble (or dataframe) that will contain the coordinates
# and other values from the rows that are selected in my_datatable

# To accomplish this, we will make use of the observeEvent and eventReactive functions


# In the next chunk we use sever instances of eventReactive, nested within an observeEvent that
# only exectures when rows are selected or deselected in the datatable, to capture the lat, lng
# ect

# We then make, within the same observeEvent, a reactive tibble (or dataframe) that contains the
# aforementioned lat, lng, ect. This is the data that will be passed to a leafletProxy, which
# will plot the selected df_r() observaations onto the leaflet map without needing to completely
#regenerate the map


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
  
  selected_COMIDs <- eventReactive(input$my_datatable_rows_selected, {
    as.list(df_r()$COMID[c(unique(input$my_datatable_rows_selected))])
  })
  
  #This is the data that will be passed to the leaflet in the addCircleMarkers argument in the
  #leafletProxy, as well as the popups when the points are hovered over
  
  map_df <- reactive({
    tibble(lat = unlisted(selected_lats()),
           lng = unlisted(selected_longs()),
           JSM_suitab = unlisted(selected_JSM_suitab()),
           name = unlisted(selected_names()),
           COMID = unlisted(selected_COMIDs())
  })
  
    
   leafletProxy("my_leaflet", session) %>%
     clearMarkers() %>%
     addCircleMarkers(
       data = map_df(),
       lng = ~lng,
       lat = ~lat,
       fillColor = "#098F92",
       stroke = TRUE,
       color = "white",
       radius = 3,
       weight = 1,
       fillOpacity = 0.4,
       popup = paste0("lat: ", map_df()$lat, "<br>",
                      "lng: ", map_df()$lng, "<br>",
                      "JSM_suitab: ", map_df()$JSM_suitab, "<br>",
                      "name: ", map_df()$GNIS_NAME, "<br>",
                      "COM_ID: ", map_df()$COM_ID, "<br>")
     )
  
    
})

# Note: the DT package enables R to know that "input$my_datatable_rows_selected" is,
  #and that it refers specifically to the selected rows of my_datatable.
  #It is not a variable we created somewhere else
  
# In the above chunk, the calls to eventReactive serve as a way of subsetting out reactive
  #df_r() dataset by the index returned by the clicked rows from my_datatable 
  #(input$my_datatable_rows_selected). eventReactive only executes the desired expression when
  #the evenExpr argument, "input$my_datatable_rows_selected" in our case, is noticed or 
  #observed. This prevents any other reactive expressions we might have inside of the eventReactive
  #from re-evaluating in the absence of the eventExpr being noticed
  
# We now have a way for the user to select df observations from the datatable (and consequently
  #the plotting of all observations on the leaflet), as well as the clearing of all selected rows
  #in the datatable (and consequently the removeal of all plotted points from my_leaflet).
  
  
# Create a proxy to modify datatable without recreating it completely
  
  DT_proxy <- dataTableProxy("my_datatable")
  
# Clear row selection when clear_rows_button is clicked
  
  observeEvent(input$clear_rows_button, {
    clearMarkers(leafletProxy("my_leaflet", session))
  })
  
# Select all rows when select_all_rows_button is clicked
  
  observeEvent(input$select_all_rows_button, {
    selectRows(DT_proxy, input$my_datatable_rows_all)
  })

  
#######################################################################


shiny::shinyApp(
  ui = fluidPage(
    column(
      width = 3,
      br(),
      actionButton(
        "select_all_rows_button",
        "Select All Table Rows"
      ),
      br(),
      actionButton(
        "clear_rows_button",
        "Clear Table Selections"
      )
    ),
    column(
      width = 9,
      fluidRow(
        column(
          width = 12,
          solidHeader = TRUE,
          leafletOutput(
            "my_leaflet"
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          solidHeader = TRUE,
          DTOutput(
            "my_datatable"
          )
        )
      )
    )
  ),
  
  server = function(session, input, output {
    df_r <- reactive({as_tibble(df) })
    
    output$my_datatable <- renderDT({
      df_r() %>%
        datatable()
    })
    
    # base map that we will add points to with leafletProxy()
  
    output$my_leaflet <- renderLeaflet({
      
      leaflet() %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
        setView(lng = -79, lat = 38, zoom = 8)
        
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
      
      selected_COMIDs <- eventReactive(input$my_datatable_rows_selected, {
        as.list(df_r()$COMID[c(unique(input$my_datatable_rows_selected))])
      }) 
      
      # This is the data that will be passed to the leaflet in the addCircleMarkers argument,
      # as well as the popups when the points are hovered over
      
      map_df <- reactive({
        tibble(lat = unlisted(selected_lats()),
               lng = unlisted(selected_longs()),
               JSM_suitab = unlisted(selected_JSM_suitab()),
               name = unlisted(selected_names()),
               COMID = unlisted(selected_COMIDs())
      })
        
        
        leafletProxy("my_leaflet", session) %>%
          clearMarkers() %>%
          addCircleMarkers(
            data = map_df(),
            lng = ~lng,
            lat = ~lat,
            fillColor = "#098F92",
            stroke = TRUE,
            color = "white",
            radius = 3,
            weight = 1,
            fillOpacity = 0.4,
            popup = paste0("lat: ", map_df()$lat, "<br>",
                           "lng: ", map_df()$lng, "<br>",
                           "JSM_suitab: ", map_df()$JSM_suitab, "<br>",
                           "name: ", map_df()$GNIS_NAME, "<br>",
                           "COM_ID: ", map_df()$COM_ID, "<br>")
          )
        
        
    }) 
    
    # create a proxy to modify datatable without recreating it completely
    DT_proxy <- dataTableProxy("my_datatable")  
    
    # clear row selections when clear_rows_button is clicked
    observeEvent(input$clear_rows_button, {
      selectedRows(DT_proxy, NULL)
    })
  
    # clear markers from leaflet when clear_rows_button is clicked
    observeEvent(input$clear_rows_button, {
      clearMarkers(leafletProxy("my_leaflet", session))
    })
    
    # select all rows when select_all_rows_button is clicked
    observeEvent(input$select_all_rows_button, {
      selectRows(DT_proxy, input$my_datatable_rows_all)
    })
      
)























