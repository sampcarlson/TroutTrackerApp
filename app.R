library(shiny)
library(DT)
source("init.R")

#todo:
# check that getting species info from other data sources -< fgdata species

# check cold phone gps location bug - snap tolerance - snap to next point instead
# Data Import side:  add single day locations for single observation fish
#bug: addmovingMarker allows numeric layerId, while removeMarker suggests(requires?) character
#usgs logger link
#timer issues


#user interface
#depends on server side to provide current database
secPerDay=.25



ui=fluidPage(
  #titlePanel("Silver Creek Fish Tracker"),
  fluidRow(
    column(width=2,
           textInput(inputId="userName",
                     label=NULL,
                     placeholder = "User Name or Last Name")
           
    ),
    # column(width=3,
    #        selectInput(
    #          inputId="selectFish",
    #          label="fish",
    #          choices=availableFish$l4hex,
    #          selected=NULL,
    #          multiple = TRUE
    #        )
    # ),
    column(width=3,
           dateRangeInput(inputId = 'dateTime',
                          label = NULL,
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
    # column(width=2,
    #        actionButton("startFish","Start",style="position: relative; top: 22px; right: 20px;"),
    #        div(style="position: relative; top: -2px; left: 50px;",
    #            textOutput("simTime"),
    #        ) 
    #),
    column(width=1, actionButton("startFish","Start")),
    column(width=2,div(style="position: relative; top: 6px;",textOutput("simTime"))),
    
    
    # column(width=2,
    #        div(style="position: relative; top: 25px; right: 30px;",
    #            textOutput("simTime"),
    #        )     
    # )
    column(2,checkboxInput("temperatureLoggers","Temperature Sites")),
    column(2,checkboxInput("DOLoggers","Dissolved Oxygen Sites"))
  ),
  fluidRow(
    column(3,textOutput("nameReport"))
    
    
  ),
  
  fluidRow(
    column(12,align="center",
           leafletOutput("scMap",width="90%",height=500)
    )
  ),
  br(),
  fluidRow(
    column(12,
           div(style="height:350px; overflow: scroll",
               DTOutput("fishTable")
           )
    )
  )
)

#server side - update, manage, backup, and serve database

server = function(input,output,session){
  source("init.R")
  
  clockStartTime=""
  paused=F
  
  startPauseTime=Sys.time()
  endPauseTime=Sys.time()
  pausedTime=0
  
  
  ########---------- get available fish from username-----------
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
    #printFish$`# of Observations`=as.character( printFish$`# of Observations` )
    #printFish$`Length (in)` = as.character(printFish$`Length (in)`)
    printFish$`Travel Distance (km)`=round(printFish$`Travel Distance (km)`,1)
    
    output$fishTable=renderDT({printFish},rownames=F,options=list(paging=F,searching=F))
    
    updateSelectInput(inputId="selectFish",choices=unique(availableFish$l4hex))
    #print(unique(availableFish$l4hex))
    print(paste("# of availFish:",nrow(availableFish)))
    
    
  })
  
  #############-------- build map --------------------
  scMap = leaflet( leafletOptions(leafletCRS(crsClass="L.CRS.EPSG4326")) )
  scMap = setView(map=scMap,lng=-114.15,lat=43.33,zoom=12)
  scMap=addProviderTiles(map=scMap,provider = providers$Esri.WorldImagery)
  scMap=addAwesomeMarkers(scMap,lng=-114.1087,lat=43.32217,
                          icon=DOLoggerIcon,
                          popup='<a href="https://waterdata.usgs.gov/monitoring-location/13150430/#parameterCode=00065&period=P30D"> USGS Streamflow Data')
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
  
  ###########----------------start fish animation------------------
  observeEvent(input$startFish,{
    pausedTime <<- 0
    
    thisFishIdx=availableFish[input$fishTable_rows_selected,"idx"]
    try({
      if(length(thisFishIdx)>10){
        thisFishIdx=thisFishIdx[1:10]
      }
      
      fishData <<- getFishData(input$dateTime[1],input$dateTime[2],fishIDs=thisFishIdx)
      
      if(nrow(fishData)>1){
        startTime=as.Date(min(fishData$datetime))
        endTime=as.Date(max(fishData$datetime))
        updateDateRangeInput(inputId="dateTime",
                             start=startTime,
                             end=endTime+1)
      }
    })
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
  
  ###########-------- timekeeper, including adding and removing fish due to first/last observation ----------------
  observe({
    simTime=reactive(getSimTime(clockStartTime,simStartTime=input$dateTime[1],simEndTime=input$dateTime[2], secPerDay = secPerDay))
    
    invalidateLater(250)
    
    thisSimTime=simTime()
    if(paused==F){
      output$simTime=renderText({thisSimTime})
    }
    
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
                 label=sapply(removeFishData$fishid,getFishLabel),
                 options=markerOptions(opacity=.65))
      
      
      
    }
  })
  
  ###########-------pause or unpause on click------------------
  observeEvent(input$scMap_click,{
    scMap= leafletProxy("scMap",deferUntilFlush = T)
    if(paused){
      resumeMoving(scMap)
      paused <<- F
      endPauseTime <<- Sys.time()
      pausedTime <<- pausedTime + round(as.numeric(difftime(endPauseTime,startPauseTime,units="secs")),3)
    } else {
      pauseMoving(scMap) 
      paused <<- T
      startPauseTime <<- Sys.time()
    }
    
  })
  
  
  ############---------- add/remove Temperature sites--------
  observeEvent(input$temperatureLoggers,{
    scMap= leafletProxy("scMap",deferUntilFlush = T)
    if(input$temperatureLoggers==T){
      scMap=addAwesomeMarkers(map=scMap,
                              data=temperatureLoggers,
                              icon=TempLoggerIcon,
                              group = "loggerIcons",
                              layerId = temperatureLoggers$Name,
                              popup = as.character(temperatureLoggers$Description))
    } else {
      scMap=removeMarker(scMap,temperatureLoggers$Name)
    }
    #can set min/max zoom levels with groupOptions
  })
  
  ############---------- add/remove DO sites--------
  observeEvent(input$DOLoggers,{
    scMap= leafletProxy("scMap",deferUntilFlush = T)
    if(input$DOLoggers==T){
      scMap=addAwesomeMarkers(map=scMap,
                              data=DOLoggers,
                              icon=DOLoggerIcon,
                              group = "loggerIcons",
                              layerId = DOLoggers$Name,
                              popup = as.character(DOLoggers$Description))
    } else {
      scMap=removeMarker(scMap,DOLoggers$Name)
    }
    
  })
  
} 
# Run the application 
shinyApp(ui = ui, server = server)
