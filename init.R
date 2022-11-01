#sources and functions for app
require(shiny)
require(sf)
require(leaflet)
require(leaflet.extras2)
require(magrittr)
library(RPostgres)
library(DBI)

source("movingMarkerFunctions.R")

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


getFishData=function(startDateTime,stopDateTime,whichFish){
  if(length(whichFish)>0){
    q=paste0("SELECT * FROM fishlocations WHERE fishlocations.datetime > '",startDateTime,
             "' AND fishlocations.datetime < '",stopDateTime,
             "' AND fishlocations.fishid IN ('",paste0(whichFish,collapse="', '"),"');")
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


allFish=dbGetQuery(conn,"SELECT DISTINCT fishid FROM fishlocations;")$fishid


#fishData=getFishData(minDateTime,maxDateTime,1:10)

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

getFishLabel=function(fishid){
  allAttributes=dbGetQuery(conn,paste0("SELECT * FROM fishattributes WHERE fishid = '",fishid,"';"))
  if(nrow(allAttributes)==0){
    return(paste0("Fish# ",fishid))
  }else if(sum(complete.cases(allAttributes))>=1){
    print(allAttributes)
    allAttributes=allAttributes[complete.cases(allAttributes),]
    allAttributes=allAttributes[1,]
  }else {
    allAttributes=allAttributes[1,]
  }
  allAttributes[is.na(allAttributes)]=" "
  attributeString=paste0("Fish ", allAttributes$fishid,"\n",allAttributes$length,'" ' ,allAttributes$species )
  return(attributeString)
}




#rsconnect::deployApp()
