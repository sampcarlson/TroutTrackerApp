#sources and functions for app
require(shiny)
require(sf)
require(leaflet)
require(leaflet.extras2)
require(magrittr)
library(RPostgres)
library(DBI)

source("movingMarkerFunctions.R")

validName=T

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



minDateTime=as.Date(dbGetQuery(conn,"SELECT MIN (datetime) FROM fishlocations;")$min)
maxDateTime=as.Date(dbGetQuery(conn,"SELECT MAX (datetime) FROM fishlocations;")$max)
#startDateTime="2022-02-21 04:20"
#stopDateTime="2023-03-15"
#q=paste0("SELECT * FROM fishlocations WHERE fishlocations.datetime > '",startDateTime,"' AND fishlocations.datetime < '",stopDateTime,"';")
#fishData=st_read(dsn=conn,query=q)


getFishData=function(startDateTime,stopDateTime,fishIDs=NULL,fishHIDs=NULL,availFish=availableFish){
  #print(fishIDs)
  #print(fishHIDs)
  #print("fish in availFish in getFishData:")
  #print(paste(unique(availFish$l4hex)))
  if(is.null(fishIDs) & !is.null(fishHIDs)){
    fishIDs=availFish[availFish$l4hex %in% fishHIDs,]
    #print(fishIDs)
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

# getFishData_interval=function(startDateTime,intervalDays,whichFish){
#   if(length(whichFish)>0){
#     stopDateTime=startDateTime+intervalDays
#     q=paste0("SELECT * FROM fishlocations WHERE fishlocations.datetime > '",startDateTime,
#              "' AND fishlocations.datetime < '",stopDateTime,
#              "' AND fishlocations.fishid IN ('",paste0(whichFish,collapse="', '"),"');")
#     fishData=st_read(dsn=conn,query=q)
#     return(fishData)
#   } else { return(NULL) }
#   
# }

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

make4bold=function(string){
  n=nchar(string)
  firstbit=substring(string,first=1,last=n-4)
  lastbit=substring(string,first=n-3)
  boldString=paste0(firstbit,"<strong>",lastbit,"</strong>")
  return(boldString)
}

getAvailableFish=function(name="",fishDeets=fishDetails){
  fishDeets=fishDeets[,c("idx","hexID","l4hex","firstObserved","lastObserved","obsCount","Species","Length.Inches","routeLength_km")]

    if(name=="superuser"){
      outdf=fishDeets
    } else if(name %in% unique(clientFish$Client)){
      fishIDs=clientFish$idx[clientFish$Client==name]
      #print(fishIDs)
      outdf=fishDeets[fishDeets$idx %in% fishIDs,]
    } else {
      #fishDeets=fishDeets[order(fishDeets$lastObserved,decreasing=T),]
      outdf=fishDeets[1:5,]
    }
    outdf$firstObserved=as.character(as.Date(outdf$firstObserved))
    outdf$lastObserved=as.character(as.Date(outdf$lastObserved))
    outdf$hexBold=make4bold(outdf$hexID)
    
    outdf=outdf[order(outdf$routeLength_km,decreasing = T),]
    #print(outdf)
    return(outdf)
  
}

getFishDuration=function(fishMin,fishMax,calMin,calMax){
  days=min(fishMax,calMax)-max(fishMin,calMin)
  return(days)
}

clientFish=dbGetQuery(conn,"select * from clientfish;")
clientFish$ncharClient=nchar(clientFish$Client)
clientFish=clientFish[clientFish$ncharClient>=1,c("idx","l4hex","Client")]
clientFish$Client=tolower(clientFish$Client)
#do_loggers=st_read("C:/Users/sam/Documents/SilverCreek/R/TroutTrackerApp/www/DO_Loggers_2020.kml")

add_loggers = st_read("https://savesilvercreek.org/2020_KML_Files/Temperature%20Data%202020.kml") 

#loggerIcon=makeAwesomeIcon(icon="thermometer",markerColor = "darkblue",library = "fa",iconColor = "black")

fixHtmlLink=function(sfObject){
  #inputFormat = "<!DOCTYPE html> <html> <body> <strong>Temperature Data for Lower Silver Creek at Highway 93 #1</strong> <br /> <a href=\"https://savesilvercreek.org/data-for-1-highway-93/\">Logger  Data Page</a> <br /> </body> </html>"
  #output format: 
  #"<a target=_'blank' href=\"https://savesilvercreek.org/data-for-1-highway-93/\">Logger  Data Page</a> <br />"
  for(i in 1:nrow(sfObject)){
    htmlLink=sfObject$Description[i]
    
    # linkName=substring(htmlLink,
    #                    first=regexpr("<strong>",htmlLink)[1]+8,
    #                    last=regexpr("</strong>",htmlLink)[1])
    # 
    htmlLink=htmltools::HTML("<a target='_blank' href=",
                             substring(htmlLink,
                                       first=regexpr('"https://',htmlLink)[1],
                                       last=regexpr('/">',htmlLink)[1]
                             ),'\"> Test Link Text </a>'
    )
    #htmlLink=sub("</body> </html>","",htmlLink)
    #htmlLink=sub('<img src=""> ',"",htmlLink)
    
    sfObject$Description[i]=htmlLink
  }
  return(sfObject)
}

#temperatureLoggers=fixHtmlLink(add_loggers)


availableFish=getAvailableFish("")
addRemoveFishKey = data.frame(idx=118,l4hex="80FB",Species="Brown Trout", Length.Inches=5.5, firstDay=as.Date("2022-04-28"),lastDay=as.Date("2022-05-11"),days=13)
addRemoveFishKey=addRemoveFishKey[0,]

fishIcon=makeIcon(iconUrl="https://trackatrout.com/Images/TroutIcon.png",
                  iconWidth = 60, iconHeight = 60,
                  iconAnchorX=30,iconAnchorY = 30)


#rsconnect::deployApp()
