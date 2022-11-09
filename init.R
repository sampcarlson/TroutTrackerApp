#sources and functions for app
require(shiny)
require(sf)
require(leaflet)
require(leaflet.extras2)
require(magrittr)
library(RPostgres)
library(DBI)

source("movingMarkerFunctions.R")

userRole="user"
#userRole="superuser"

scdbConnect=function(){
  #readRenviron(".Renviron")
  conn=dbConnect(RPostgres::Postgres(),
                 host="silvercreekdb-do-user-12108041-0.b.db.ondigitalocean.com",
                 port="25060",
                 dbname="silvercreekdb" ,
                 user="dbread",
                 password="dbread"
                 #password=Sys.getenv("scdb_readPass")
  )
  return(conn)
}


conn=scdbConnect()

#write temp data to fishlocations
#fishData=st_read("fishLocationTimeseries.gpkg")
#writeMe=data.frame(fishid=fishData$fishID,datetime=fishData$time,geometry=fishData$geom)
#st_write(writeMe,dsn=conn,"fishlocations")

minDateTime=as.Date(dbGetQuery(conn,"SELECT MIN (datetime) FROM fishlocations;")$min)
maxDateTime=as.Date(dbGetQuery(conn,"SELECT MAX (datetime) FROM fishlocations;")$max)
#startDateTime="2022-02-21 04:20"
#stopDateTime="2023-03-15"
#q=paste0("SELECT * FROM fishlocations WHERE fishlocations.datetime > '",startDateTime,"' AND fishlocations.datetime < '",stopDateTime,"';")
#fishData=st_read(dsn=conn,query=q)


getFishData=function(startDateTime,stopDateTime,fishIDs=NULL,fishHIDs=NULL,availFish=availableFish){
  print(fishIDs)
  print(fishHIDs)
  if(is.null(fishIDs) & !is.null(fishHIDs)){
    fishIDs=availFish[availFish$l4hex %in% fishHIDs,]
  }
  
  whichFish = fishIDs[fishIDs$idx %in% availFish$idx,"idx"]
  print(whichFish)
  
  if(length(whichFish)>0){
    q=paste0("SELECT * FROM fishlocations WHERE fishlocations.datetime > '",startDateTime,
             "' AND fishlocations.datetime < '",stopDateTime,
             "' AND fishlocations.fishid IN ('",paste0(whichFish,collapse="', '"),"');")
    print(q)
    fishData=st_read(dsn=conn,query=q)
    return(fishData)
  } else { return(NULL) }
  
}

getFishData_interval=function(startDateTime,intervalDays,whichFish){
  if(length(whichFish)>0){
    stopDateTime=startDateTime+intervalDays
    q=paste0("SELECT * FROM fishlocations WHERE fishlocations.datetime > '",startDateTime,
             "' AND fishlocations.datetime < '",stopDateTime,
             "' AND fishlocations.fishid IN ('",paste0(whichFish,collapse="', '"),"');")
    fishData=st_read(dsn=conn,query=q)
    return(fishData)
  } else { return(NULL) }
  
}

dbGetQuery(conn,"SELECT * FROM fishlocations LIMIT 5;")

#get fishdetails once and handle locally
fishDetails=dbGetQuery(conn,"SELECT * FROM fishdetails;")


#fishData=getFishData(minDateTime,maxDateTime,fishDetails$idx[1])

getSimTime=function(clockStartTime,simStartTime,simEndTime,secPerDay){
  
  if(inherits(clockStartTime,"POSIXct")){
    
    duration=round(as.numeric(difftime(Sys.time(),clockStartTime,units="secs")),3)
    simDays=duration/secPerDay
    thisSimTime=as.Date(simStartTime+simDays)
    thisSimTime=min(thisSimTime,simEndTime)
    
    return(as.character(thisSimTime))
    #return(as.character(simDays))
  }else{
    return(" ")
  }
}

clockStartTime=""

getFishLabel=function(fishid,fishDeets=fishDetails){
  #allAttributes=dbGetQuery(conn,paste0("SELECT * FROM fishattributes WHERE fishid = '",fishid,"';"))
  allAttributes=fishDeets[fishDeets$idx==fishid,]
  
  allAttributes[is.na(allAttributes)]=" "
  attributeString=paste0("Fish ", allAttributes$l4hex,"\n",allAttributes$Length.Inches,'" ' ,allAttributes$Species )
  return(attributeString)
  
}

getAvailableFish=function(role="user",fishDeets=fishDetails){
  if(role=="superuser"){
    return(fishDeets[,c("idx","hexID","l4hex")])
  }else {
    return(fishDeets[1:10,c("idx","hexID","l4hex")])
  }
}
availableFish=getAvailableFish(role=userRole)

#rsconnect::deployApp()
