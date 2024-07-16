# This group task requires the team to do the following;
# Identify a Nigerian company that has at least 100 branch locations
# Map the longitude and latitude of the street of each location
# Geo plot this location to display on map

# The team started by researching the a Nigerian organisation with 100 branches. 
# The result was First City Monument Bank (FCMB). It has 124 branch locations 

# The next step are as follows;

#load the library
library(UsingR)
library(janitor)
library(dplyr)
library(mapview)
library(plotly)
library(tidygeocoder)
library(sf)
library(leaflet)
library(tidyverse)
library(leaflet)

#load the data set
fcmb_branches <- read.csv("D:/DDrive - Document/DS_Project/Datasets/FCMB_Locations_Dataset_UTF8.csv")
view(fcmb_branches) # view in tabular form

#clean data
fcmb_br_cl <- fcmb_branches %>% 
  clean_names()
view(fcmb_br_cl)

dim(fcmb_br_cl) # view data's dimension

#Google Geocoding
Sys.setenv(GOOGLEGEOCODE_API_KEY= Insert Key)

#Geocode Fcmb Branch
fcmb_code_tbl <- fcmb_br_cl %>%
  tidygeocoder::geocode(
    address = location,
    method ="google"
  )
View(fcmb_code_tbl)

fcmb_na <- na.omit(fcmb_code_tbl) # omit na
view(fcmb_na)

#convert to sf format
fcmb_map <- fcmb_na
fcmb_map_sf <- fcmb_na %>%
  st_as_sf(
    coords = c("long", "lat"),
    crs    = 4326
  )

#mapview(fcmb_branches_sf)
fcmb_map_sf %>%
  leaflet() %>%
  #addProviderTiles (providers$Esri.worldImagery, group = "world Imagery") %>%
  addProviderTiles (providers$OpenStreetMap, group = "OpenStreetMap") %>%
  #addLayersControl (baseGroups = c("Toner Lite", "world Imagery")) %>%
  addMarkers (label = fcmb_map_sf$location,
              clusterOptions = markerClusterOptions(),
              popup = ifelse(!is.na(fcmb_map_sf$location),
                             fcmb_map_sf$location,
                             "Not sure of branch location"))

mapview(fcmb_map_sf) # map displays in web browser as an html file.

