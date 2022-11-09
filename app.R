library(shiny)


source("init.R")

#user interface
#depends on server side to provide current database
secPerDay=.25



ui=fluidPage(
  titlePanel("Silver Creek Fish Tracker Test"),
  fluidRow(
    column(width=3,
           selectInput(
             inputId="fishHID",
             label="fish",
             choices=availableFish$l4hex,
             selected=NULL,
             multiple = TRUE
           ),
           offset=1
    ),
    column(width=5,
           sliderInput(inputId = "dateTime",
                       label = "Date Range:",
                       min = minDateTime,
                       max = maxDateTime,
                       value = c(minDateTime,maxDateTime),
                       step=1
                       #animate = animationOptions(interval=msPerDay*daysPerStep)
           ),
    ),
    column(width=1,
           actionButton("startFish","Start"),
    ),
    column(width=2,
           textOutput("simTime"),
           
    )
  ),
  
  fluidRow(
    column(12,align="center",
           leafletOutput("scMap",width="100%",height=450)
    )
  )
)


#server side - update, manage, backup, and serve database

server = function(input,output,session){
  source("init.R")
  #readRenviron(".Renviron")
  network=readRDS("network.rds")
  
  #fishData=reactive(getFishData_interval(input$dateTime,daysPerStep,whichFish=input$fishID))
  #fishData=reactive(getFishData(input$dateTime[1],input$dateTime[2],fishHIDs=input$fishHID))
  
  
  #clockStartTime = reactiveVal(Sys.time())
  #simTime=reactive(getSimTime(clockStartTime,simStartTime=input$dateTime[1],simEndTime=input$dateTime[2], secPerDay = secPerDay))
  
  
  #https://trackatrout.com/Images/TroutIcon.png
  fishIcon=makeIcon(iconUrl="https://trackatrout.com/Images/TroutIcon.png",
                    iconWidth = 60, iconHeight = 60,
                    iconAnchorX=30,iconAnchorY = 30)
  
  
  scMap = leaflet( leafletOptions(leafletCRS(crsClass="L.CRS.EPSG4326")) )
  
  scMap = setView(map=scMap,lng=-114.15,lat=43.33,zoom=12)
  
  scMap = addWMSTiles(map=scMap,baseUrl="https://basemap.nationalmap.gov/arcgis/services/USGSImageryOnly/MapServer/WmsServer",
                      layers=0,
                      attribution = 'Tiles courtesy of the <a href="https://usgs.gov/">U.S. Geological Survey</a>',
                      tileOptions(zIndex=1))
  
  scMap = addPolylines(map=scMap,opacity=1,data=network,weight=1,smoothFactor = 2,label=network$GNIS_Name)
  
  output$scMap = renderLeaflet(scMap)
  
  
  observeEvent(input$startFish,{
    
    #observe({
    clockStartTime <<- Sys.time()
    
    fishData=getFishData(input$dateTime[1],input$dateTime[2],fishHIDs=input$fishHID)
    print(fishData)
    showFish=as.character(unique(fishData$fishid))
    scMap= leafletProxy("scMap",deferUntilFlush = T)
    scMap = clearGroup(map=scMap,"activeFish")
    days=input$dateTime[2]-input$dateTime[1]
    
    for(fish in showFish){
      scMap = addMovingMarker(map=scMap,data=fishData[fishData$fishid==fish,],
                              layerId=showFish,
                              group="activeFish",
                              icon=fishIcon,
                              label=getFishLabel(fish),
                              duration=(secPerDay*1000)*days,
                              movingOptions = movingMarkerOptions(autostart = T, loop = FALSE)
      ) 
    }
    
  })
  
  observe({
    #req(clockStartTime)
    #req(input$dateTime)
    
    #Sys.sleep(1)
    
    simTime=reactive(getSimTime(clockStartTime,simStartTime=input$dateTime[1],simEndTime=input$dateTime[2], secPerDay = secPerDay))
    
    #thisSimTime=getSimTime(clockStartTime,simStartTime=input$dateTime[1],simEndTime=input$dateTime[2], secPerDay = secPerDay)
    
    invalidateLater(500)
    
    thisSimTime=simTime()
    #print(thisSimTime)
    output$simTime=renderText({thisSimTime})
    
    
    
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
