#functions for app


getFishData=function(startDateTime,stopDateTime,fishIDs=NULL,fishHIDs=NULL,availFish=availableFish){
  #print(fishIDs)
  #print(fishHIDs)
  #print("fish in availFish in getFishData:")
  #print(paste(unique(availFish$l4hex)))
  if(is.null(fishIDs) & !is.null(fishHIDs)){
    fishIDs=availFish[availFish$l4hex %in% fishHIDs,]
    #print(fishIDs)
  } else {
    fishIDs=availFish[availFish$idx %in% fishIDs,]
  }
  
  whichFish = fishIDs[fishIDs$idx %in% availFish$idx,"idx"]
  #print(whichFish)
  
  if(length(whichFish)>0){
    q=paste0("SELECT * FROM fishlocations WHERE fishlocations.datetime > '",startDateTime,
             "' AND fishlocations.datetime < '",stopDateTime,
             "' AND fishlocations.fishid IN ('",paste0(whichFish,collapse="', '"),"');")
    print(q)
    fishData=st_read(dsn=conn,query=q)
    return(fishData)
  } else { return(NULL) }
  
}


getSimTime=function(clockStartTime,simStartTime,simEndTime,secPerDay,pauseOffset=pausedTime){
  
  if(inherits(clockStartTime,"POSIXct")){
    
    duration=round(as.numeric(difftime(Sys.time(),clockStartTime,units="secs")),3)
    duration = duration - pauseOffset
    simDays=duration/secPerDay
    thisSimTime=as.Date(simStartTime+simDays)
    thisSimTime=min(thisSimTime,simEndTime)
    
    # print(paste0("cst:",clockStartTime,"  sst:",simStartTime,
    #              "  set:",simEndTime,"  po:",pauseOffset,
    #              "  st:",thisSimTime))
    # 
    return(as.character(thisSimTime))
    #return(as.character(simDays))
  }else{
    return(" ")
  }
}


getFishLabel=function(fishid,fishDeets=fishDetails){
  #allAttributes=dbGetQuery(conn,paste0("SELECT * FROM fishattributes WHERE fishid = '",fishid,"';"))
  allAttributes=fishDeets[fishDeets$idx==fishid,]
  
  allAttributes[is.na(allAttributes)]=" "
  attributeString=paste0("Fish ", allAttributes$l4hex,"\n",allAttributes$Length.Inches,'" ' ,allAttributes$Species )
  return(attributeString)
  
}

# make4bold=function(string){
#   n=nchar(string)
#   firstbit=substring(string,first=1,last=n-4)
#   lastbit=substring(string,first=n-3)
#   boldString=paste0(firstbit,"<strong>",lastbit,"</strong>")
#   return(boldString)
# }

getAvailableFish=function(name="",fishDeets=fishDetails){
  fishDeets=fishDeets[,c("idx","hexID","l4hex","firstObserved","lastObserved","obsCount","Species","Length.Inches","routeLength_km")]
  
  if(name=="superuser"){
    outdf=fishDeets
  } else if(name %in% unique(clientFish$Client)){
    fishIDs=clientFish$idx[clientFish$Client==name]
    #print(fishIDs)
    outdf=fishDeets[fishDeets$idx %in% fishIDs,]
  } else {
    outdf=fishDeets[order(fishDeets$routeLength_km,decreasing=T),]
    #fishDeets=fishDeets[order(fishDeets$lastObserved,decreasing=T),]
    outdf=outdf[1:10,]
  }
  outdf$firstObserved=as.character(as.Date(outdf$firstObserved))
  outdf$lastObserved=as.character(as.Date(outdf$lastObserved))
  #outdf$hexBold=make4bold(outdf$hexID)
  
  outdf=outdf[order(outdf$routeLength_km,decreasing = T),]
  
  #print(outdf)
  return(outdf)
  
}

getFishDuration=function(fishMin,fishMax,calMin,calMax){
  days=min(fishMax,calMax)-max(fishMin,calMin)
  return(days)
}



# printifyAvailFish=function(availFish){
#   printFish=availableFish[,c("l4hex","firstObserved","lastObserved","obsCount","Species","Length.Inches","routeLength_km")]
#   names(printFish)=c("Fish ID","First Observed", "Last Observed", "# of Observations", "Species", "Length (in)", "Travel Distance (km)")
#   return(printFish)
# }



#########-------- initialize variables-------------------

resetClock=function(){
  try({
    clockStartTime <<- Sys.time()
  })
  try({
    paused <<- T
  })
  try({
    startPauseTime <<- Sys.time()
  })
  try({
    endPauseTime <<- Sys.time()
  })
  try({
    pausedTime <<- 0
  })
}


#rsconnect::deployApp()

#gauge @ sportsmans:
#dbGetQuery(conn,"SELECT * FROM locations WHERE source_site_id = '13150430';")
# scFlow=st_read(dsn=conn,query="SELECT * FROM locations WHERE source_site_id = '13150430';")
# scFlow$geometry
# st_transform(scFlow,4326)
