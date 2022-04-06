library(shiny)


source("init.R")

#user interface
#depends on server side to provide current database


ui=fluidPage(
  titlePanel("Silver Creek Fish Tracker Test"),
  
  sidebarLayout(
    selectInput(
      "fishID",
      label="fish",
      choices=c(1:10)
    ),
    
    mainPanel(
      leafletOutput("scMap",width=700,height=400)
    )
    
    
  )
)

#server side - update, manage, backup, and serve database

server = function(input,output,session){
  source("init.R")
  #readRenviron(".Renviron")
  network=readRDS("network.rds")
  
  fishIcon=makeIcon(iconUrl="https://www.silvercreekatlas.com/wp-content/uploads/2022/04/TroutIcon.png",
                    iconWidth = 60, iconHeight = 60,
                    iconAnchorX=30,iconAnchorY = 30)
  
  scMap =leaflet( leafletOptions(leafletCRS(crsClass="L.CRS.EPSG4326")) )
  scMap=setView(map=scMap,lng=-114.15,lat=43.33,zoom=12)
  
  scMap=addWMSTiles(map=scMap,
                    baseUrl="https://basemap.nationalmap.gov/arcgis/services/USGSImageryOnly/MapServer/WmsServer",
                    layers=0,
                    attribution = 'Tiles courtesy of the <a href="https://usgs.gov/">U.S. Geological Survey</a>',
                    tileOptions(zIndex=1))
  
  scMap=addPolylines(map=scMap,opacity=1,data=network,weight=3,smoothFactor = 2,label=network$GNIS_Name)
  
  for(fishID in unique(fishData$fishID)){
    
    
    scMap=addMovingMarker(map=scMap,data=fishData[fishData$fishID==fishID,],
                          layerId=fishID,
                          icon=fishIcon,
                          label=paste0("Fish ",fishID),
                          duration=2*60*1000,
                          movingOptions = movingMarkerOptions(autostart = TRUE, loop = TRUE)
    )
  }
  
  output$scMap = renderLeaflet(scMap)
  
}
# Run the application 
shinyApp(ui = ui, server = server)
