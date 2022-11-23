library(shiny)

source("init.R")

#todo:
# check that getting species info from other data sources -< fgdata species
# kml w/ links in popups (and select w/ kml)
# pause button - click on map or fish to pause
# use DT selectable table
# change date range to reflect selected fish
# cold phone gps location bug - snap tolerance - snap to next point instead

#addmovingMarker allows numeric layerId, while removeMarker suggests(requires?) character


#user interface
#depends on server side to provide current database
secPerDay=.25



ui=fluidPage(
  #titlePanel("Silver Creek Fish Tracker"),
  fluidRow(
    column(width=2,
           textInput(inputId="userName",
                     label="User Name",
                     placeholder = "Enter your last name")
           
    ),
    column(width=3,
           selectInput(
             inputId="selectFish",
             label="fish",
             choices=availableFish$l4hex,
             selected=NULL,
             multiple = TRUE
           )
    ),
    column(width=3,
           dateRangeInput(inputId = 'dateTime',
                          label = "Date Range",
                          start = minDateTime,
                          end = maxDateTime,
                          min = minDateTime,
                          max = maxDateTime,
                          startview = "year",
                          separator = "to ",
                          autoclose = T
           )
           
           # sliderInput(inputId = "dateTime",
           #             label = "Date Range:",
           #             min = minDateTime,
           #             max = maxDateTime,
           #             value = c(minDateTime,maxDateTime),
           #             step=1
           #             #animate = animationOptions(interval=msPerDay*daysPerStep)
           # ),
    ),
    column(width=3,
           actionButton("startFish","Start",style="position: relative; top: 22px; right: 20px;"),
           div(style="position: relative; top: -2px; left: 50px;",
               textOutput("simTime"),
           ) 
           
    ),
    # column(width=2,
    #        div(style="position: relative; top: 25px; right: 30px;",
    #            textOutput("simTime"),
    #        )     
    # )
  ),
  fluidRow(
    column(3,textOutput("nameReport")),
    column(4,checkboxInput("temperatureLoggers","Display Temperature Loggers"))
  ),
  
  fluidRow(
    column(12,align="center",
           leafletOutput("scMap",width="100%",height=500)
    )
  ),
  fluidRow(
    column(12,
           div(style="height:300px; overflow: scroll",
               tableOutput("availableFishTable")
           )
    )
  )
)

#server side - update, manage, backup, and serve database

server = function(input,output,session){
  source("init.R")
  #readRenviron(".Renviron")
  #network=readRDS("network.rds")
  
  #availableFish=reactive({ getAvailableFish(input$userName) })
  
  observeEvent(input$userName,{
    
    
    name=tolower(input$userName)
    if(name %in% c(unique(clientFish$Client),"","superuser","contest")){
      validName=T
      availableFish <<- getAvailableFish(name)
      if(name == ""){
        output$nameReport=renderText("")
      } else {
        output$nameReport=renderText(paste(length(unique(availableFish$idx)),"fish found"))
      } 
    } else {
      validName=F
      output$nameReport=renderText("Name not recognized")
      availableFish <<- getAvailableFish("")
    }
    printFish=availableFish[,c("l4hex","firstObserved","lastObserved","obsCount","Species","Length.Inches","routeLength_km")]
    names(printFish)=c("Fish ID","First Observed", "Last Observed", "# of Observations", "Species", "Length (in)", "Travel Distance (km)")
    printFish$`# of Observations`=as.character( printFish$`# of Observations` )
    printFish$`Length (in)` = as.character(printFish$`Length (in)`)
    printFish$`Travel Distance (km)`=round(printFish$`Travel Distance (km)`,1)
    
    output$availableFishTable=renderTable({printFish},sanitize.text.function=function(x){x})
    
    updateSelectInput(inputId="selectFish",choices=unique(availableFish$l4hex))
    #print(unique(availableFish$l4hex))
    print(paste("# of availFish:",nrow(availableFish)))
    
    
  })
  
  observeEvent(input$selectFish,{
    
    ############# ne workez pas -----------
    minFishDate=fishDetails$firstObserved[fishDetails$hexID %in% input$selectFish]
    maxFishDate=fishDetails$lastObserved[fishDetails$hexID %in% input$selectFish]
    updateDateRangeInput(inputId="dateTime",
                         min=minFishDate,
                         max=maxFishDate)
  })
  
  #fishData=reactive(getFishData_interval(input$dateTime,daysPerStep,whichFish=input$fishID))
  #fishData=reactive(getFishData(input$dateTime[1],input$dateTime[2],selectFishs=input$selectFish))
  
  
  #clockStartTime = reactiveVal(Sys.time())
  #simTime=reactive(getSimTime(clockStartTime,simStartTime=input$dateTime[1],simEndTime=input$dateTime[2], secPerDay = secPerDay))
  
  
  #https://trackatrout.com/Images/TroutIcon.png
  
  
  scMap = leaflet( leafletOptions(leafletCRS(crsClass="L.CRS.EPSG4326")) )
  
  scMap = setView(map=scMap,lng=-114.15,lat=43.33,zoom=12)
  
  # scMap = addWMSTiles(map=scMap,baseUrl="https://basemap.nationalmap.gov/arcgis/services/USGSImageryOnly/MapServer/WmsServer",
  #                     layers=0,
  #                     attribution = 'Tiles courtesy of the <a href="https://usgs.gov/">U.S. Geological Survey</a>',
  #                     tileOptions(zIndex=1)) #can set maxZoom in tileOptions
  
  scMap=addProviderTiles(map=scMap,provider = providers$Esri.WorldImagery)
  
  #scMap = addPolylines(map=scMap,opacity=1,data=network,weight=1,smoothFactor = 2,label=network$GNIS_Name)
  
  # scMap = addMarkers(map=scMap,lng=st_coordinates(lng=add_loggers[,"X"]),lat= )
  # scMap=addAwesomeMarkers(map=scMap,group="temperatureMarkers",
  #                         data=add_loggers,
  #                         icon=loggerIcon,
  #                         #popup="testPopup",
  #                         popup=as.character(add_loggers$Description)
  #                         #options=markerOptions(zIntexOffset=0),
  #                         #clusterOptions = markerClusterOptions()
  #                         )
  
  output$scMap = renderLeaflet(scMap)
  
  
  observeEvent(input$startFish,{
    
    #observe({
    # clockStartTime <<- Sys.time()
    
    fishData <<- getFishData(input$dateTime[1],input$dateTime[2],fishHIDs=input$selectFish)
    
    addRemoveFishKey <<- fishDetails[fishDetails$idx %in% unique(fishData$fishid),c("idx","l4hex","firstObserved","lastObserved","Species","Length.Inches")]
    addRemoveFishKey$firstDay <<- as.Date(addRemoveFishKey$firstObserved)
    addRemoveFishKey$lastDay <<- as.Date(addRemoveFishKey$lastObserved)
    addRemoveFishKey$firstObserved <<- NULL
    addRemoveFishKey$lastObserved <<- NULL
    if(nrow(addRemoveFishKey>=1)){
      addRemoveFishKey$days <<- mapply(FUN=getFishDuration,
                                       fishMin=addRemoveFishKey$firstDay,
                                       fishMax=addRemoveFishKey$lastDay,
                                       calMin=input$dateTime[1],
                                       calMax=input$dateTime[2])
    } else {addRemoveFishKey$days=numeric(0)}
    # 
    scMap= leafletProxy("scMap",deferUntilFlush = T)
    scMap = clearGroup(map=scMap,"activeFish")
    
    alreadyPresentFish=addRemoveFishKey[addRemoveFishKey$firstDay<input$dateTime[1] & addRemoveFishKey$lastDay>input$dateTime[1],]
    
    for(fish in alreadyPresentFish$idx){
      scMap = addMovingMarker(map=scMap,data=fishData[fishData$fishid==fish,],
                              layerId=as.character(fish),
                              group="activeFish",
                              icon=fishIcon,
                              label=getFishLabel(fish),
                              duration=(secPerDay*1000)*alreadyPresentFish$days[alreadyPresentFish$idx==fish],
                              movingOptions = movingMarkerOptions(autostart = T, loop = FALSE),
                              options = markerOptions(zIndexOffset = 1000)
      )
    }
    
    clockStartTime <<- Sys.time()
    
  })
  
  observe({
    simTime=reactive(getSimTime(clockStartTime,simStartTime=input$dateTime[1],simEndTime=input$dateTime[2], secPerDay = secPerDay))
    
    #thisSimTime=getSimTime(clockStartTime,simStartTime=input$dateTime[1],simEndTime=input$dateTime[2], secPerDay = secPerDay)
    
    invalidateLater(250)
    
    
    thisSimTime=simTime()
    output$simTime=renderText({thisSimTime})
    
    
    if(thisSimTime %in% as.character(addRemoveFishKey$firstDay)){
      
      thisAddFish=addRemoveFishKey[as.character(addRemoveFishKey$firstDay)==thisSimTime,]
      scMap= leafletProxy("scMap",deferUntilFlush = T)
      
      for(fish in thisAddFish$idx){
        scMap = addMovingMarker(map=scMap,data=fishData[fishData$fishid==fish,],
                                layerId=as.character(fish),
                                group="activeFish",
                                icon=fishIcon,
                                label=getFishLabel(fish),
                                duration=(secPerDay*1000)*thisAddFish$days[thisAddFish$idx==fish],
                                movingOptions = movingMarkerOptions(autostart = T, loop = FALSE),
                                options = markerOptions(zIndexOffset = 1000)
        ) 
      }
    }
    
    #end of a fish movement
    if(thisSimTime %in% as.character(addRemoveFishKey$lastDay)){
      
      thisRemoveFish=addRemoveFishKey[as.character(addRemoveFishKey$lastDay)==thisSimTime,]
      scMap= leafletProxy("scMap",deferUntilFlush = T)
      
      removeFishData=fishData[order(fishData$datetime,decreasing = T),]
      removeFishData=removeFishData[!duplicated(removeFishData$fishid),]
      removeFishData=removeFishData[removeFishData$fishid %in% thisRemoveFish$idx,]
      
      removeMarker(map=scMap,layerId = as.character(removeFishData$fishid))
      
      addMarkers(map=scMap,
                 data=removeFishData,
                 layerId=removeFishData$fishid,
                 group="activeFish",
                 icon=fishIcon,
                 popup=getFishLabel(removeFishData$fishid),
                 options=markerOptions(opacity=.65))
      
      
      
    }
  })
  
  
} 
# Run the application 
shinyApp(ui = ui, server = server)
