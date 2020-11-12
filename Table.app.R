library(leaflet)
library(shiny)
library(dplyr)
library(sp)
library(maptools)
library(tidyverse)

load("streams2.rda")
load("df.rda")
load("pts.rda")

###################

ui <- fluidPage(
    fluidRow(
        column(12,
               dataTableOutput('table')
        )
    )
)

server <- function(input, output) {
    output$table <- renderDataTable(df)
}


shinyApp(ui = ui, server = server)

##### Module ####














