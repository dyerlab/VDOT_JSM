

library(rgdal)
library(raster)
library(mapview)
library(leaflet)
library(plotKML)
library(sf)
library(tidyverse)

make_map <- function(){
  
  streams <- readOGR("data/HSM_selection.shp")
  streams2 <- readOGR("data/HSM_selection.shp")
  pts <- read_csv("data/JSM_presence_coords.csv")
  
  streams %>%
    reproject() %>%
    st_as_sf() %>%
    st_centroid() -> df
  
  cbind(df,st_coordinates(df$geometry)) %>%
    select (COMID,
            Longitude = X,
            Latitude = Y,
            JSM_suitab,
            fish_suita,
            JSM_fish_s,
            GNIS_NAME) -> df
  
  df2 <- st_as_sf(streams2)
  df.latlng <- st_transform(df2, crs="+init=epsg:3857")
  
  streams2 <- reproject(streams2)
  
  m <- leaflet() %>%
    setView(lng = -79, lat = 38, zoom = 8) %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas)%>%
    addMarkers(data = pts, lng = ~Longitude, lat = ~Latitude) %>%
    addPolylines(data = streams2, color = "#098F92", weight = 1.5, opacity = 1.0 ) %>%
    addCircleMarkers(data = df, label = ~JSM_suitab, opacity = 1, color = "#611708", weight = 2, fillOpacity = 0, popup = ~GNIS_NAME, group = "scores_close") %>%
    groupOptions("scores_close", zoomLevels = 12:18) %>%
    addCircleMarkers(data = df, label = ~JSM_suitab, opacity = 0, fillOpacity = 0, popup = ~GNIS_NAME, group = "scores_far") %>%
    groupOptions("scores_far", zoomLevels = 1:11)
  
  return(m)
  
}

make_map()







