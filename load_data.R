
library(rgdal)
library(raster)
library(mapview)
library(leaflet)
library(plotKML)
library(sf)
library(tidyverse)
load_data <- function(){
  
  streams <- readOGR("data/HSM_selection.shp")
  streams2 <- streams
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
  
  return(df)
}

load_data()

crs.df <- crs(df)
crs(df)

counties <- st_read("VA_County_lam83")

counties <- st_transform(counties, "+proj=longlat +datum=WGS84 +no_defs" )
crs(counties)

st_write(counties, "VA_County_WGS84.shp")

coord <- st_point_on_surface(st_zm(counties))
coord <- st_coordinates(coord)

counties <- cbind(counties, coord)
counties

roads <- st_read("VDOT_Main_Roads")
roads <- st_transform(roads, "+proj=longlat +datum=WGS84 +no_defs")
st_write(roads, "VDOT_Main_Roads_WGS84.shp")


saveRDS(streams2, file = "streams2.rds")
streams2 <- readRDS("streams2.rds")

saveRDS(counties, file = "counties.rds")
saveRDS(roads, file = "roads.rds")

saveRDS(df, file = "streams.rds")

streams3 <- readRDS("streams.rds")
streams3


hsm <- st_read("data/HabitatSuitabilityModel")
hsm <- st_transform(hsm, "+proj=longlat +datum=WGS84 +no_defs")


names(hsm)
plot(hsm["JSM_suitab"])

hsm_union <- st_union(hsm)
plot(hsm_union)

saveRDS(hsm_union, file="hsm_union.rds")


names(roads)
roads$ROAD_TYPE <- as.factor(roads$ROAD_TYPE)
class(roads$ROAD_TYPE)
levels(roads$ROAD_TYPE)

roads <- roads %>%
  group_by(ROAD_TYPE) %>%
  summarise()

plot(roads["ROAD_TYPE"])

saveRDS(roads, file = "roads.rds")


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
    data = streams2,
    color = "#098F92", 
    weight = 1.5, 
    opacity = 1.0 ) %>%
  addPolylines(
    data = roads
  )
addLayersControl(
  overlayGroups = c("County Boundaries", "Major Roads"),
  options = layersControlOptions(collapsed = FALSE)
)



























