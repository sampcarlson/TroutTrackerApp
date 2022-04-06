#sources and functions for app
require(shiny)
require(sf)
require(leaflet)
require(leaflet.extras2)
require(magrittr)

source("movingMarkerFunctions.R")

fishData=st_read("fishLocationTimeseries.gpkg")

fishData$Name=as.character(fishData$fishID)


#rsconnect::deployApp()
