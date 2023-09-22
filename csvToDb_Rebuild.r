#library(Rmpfr)
library(readxl)
library(DBI)
library(RPostgres)
library(curl)

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


#read ftp data, build fish IDS and routes
#all outside of shiny (for now)
defaultTime="11:00:00"

last4=function(ID){
  ID=gsub(" ", "", ID, fixed = TRUE)
  last4=substring(ID,nchar(ID)-3)
  return(last4)
}

#still breaks w/ spaces...
dlTroutData=function(){
  
  rebuildAll=T
  
  existingData=dbGetQuery(conn, "SELECT * FROM fishdatasources")
  
  destDir=getwd()
  
  destDir=paste0(destDir,"/troutDL_",Sys.Date())
  dir.create(destDir)
  
  ssc=new_handle(verbose=F,username="Tagging@savesilvercreek.com",password=Sys.getenv("SSC_PASS"))
  #for track a trout only:
  sscUrl="ftp://savesilvercreek.com/Track_A_Trout/"
  
  sscContent=data.table::fread(rawToChar(curl_fetch_memory(sscUrl,ssc)$content),blank.lines.skip = T,fill=T)
  
  dlFiles=function(fileNames,saveDir,baseUrl,sscHandle=ssc){
    for(fName in fileNames){
      this.url=paste0(baseUrl,fName)
      this.dest=paste0(saveDir,"/",fName)
      print(fName)
      if( any(!(fName %in% existingData$source), rebuildAll==T ) ){
        try(
          curl_download(url=this.url,destfile = this.dest,handle=sscHandle),
          silent=F
        )
        dbWriteTable(conn, "fishdatasources", data.frame(source=fName),append=T)
      }
    }
  }
  
  #download all top level files
  readDlDir=function(addDir=NULL,baseUrl=sscUrl,baseDir=destDir,sscHandle=ssc){
    if(!is.null(addDir)){
      dirUrl=paste0(baseUrl,addDir,"/")
      localDir=paste0(baseDir,"/",addDir)
      dir.create(localDir)
    } else {
      dirUrl = baseUrl
      localDir=baseDir
    }
    print(paste("dir:",dirUrl))
    
    allNames=data.table::fread(rawToChar(curl_fetch_memory(dirUrl,sscHandle)$content),blank.lines.skip = T)$V9
    dirNames=grep("\\.",allNames,invert=T,value=T)
    fileNames=grep("\\....",allNames,invert=F,value=T)
    
    #download files in this dir
    dlFiles(fileNames,saveDir=localDir,baseUrl=dirUrl)
    
    #apply dir function to dirs within this dir
    summary=sapply(dirNames,FUN=readDlDir,baseUrl=dirUrl,baseDir=localDir)
  }
  
  readDlDir()
  return(destDir)
}

dataDir=dlTroutData()

#dataDir="C:/Users/sam/Documents/SilverCreek/R/SilverCreekFishTracker/troutDL_2022-11-21"

#need: list of all fish ids, locationTimeseries(s), fish metadata (species, size, ,'owner', image, notes)
#also - keep track of all files included
includedFiles=NULL

updateData=function(subDir,baseDir=dataDir,headers,skipLines=0,renames=renameKey){
  
  renameDF=data.frame(old=names(renames), new=unlist(renames))
  
  
  files=list.files(paste0(dataDir,"/",subDir))
  files=paste0(subDir,"/",files)
  addFiles=files[!files%in%includedFiles]
  
  for(file in files){#change to 'in addFiles'
    print(file)
    try({
      if( grepl(".xls",file) ){
        thisData=data.frame(read_excel(paste0(baseDir,"/",file),skip=skipLines,.name_repair = "minimal"),stringsAsFactors = F)
      } 
      if( grepl(".csv",file) ){
        thisData=read.csv(paste0(baseDir,"/",file),skip=skipLines,stringsAsFactors = F)
      }
      
      
      if(exists("thisData")){
        for(n in 1:length(names(thisData))){
          if(names(thisData)[n] %in% renameDF$old ){
            names(thisData)[n] = renameDF$new[renameDF$old==names(thisData)[n]]
          }
        }
        
        
        if(all(c("obs2_location","obs2_date") %in% names(thisData))){
          addRows=thisData
          addRows$LATITUDE.LONGITUDE=addRows$obs2_location
          addRows$Scan.Date=addRows$obs2_date
          thisData=rbind(thisData,addRows)
        }
        
        if(all(c("obs3_location","obs3_date") %in% names(thisData))){
          addRows=thisData
          addRows$LATITUDE.LONGITUDE=addRows$obs3_location
          addRows$Scan.Date=addRows$obs3_date
          thisData=rbind(thisData,addRows)
        }
        
        
        print(paste('columns [',paste(names(thisData)[!names(thisData)%in% c(headers,renameDF$old,renameDF$new)],collapse=', '),'] dropped from',file))
        thisData=thisData[,names(thisData)%in% headers]
        
        
        thisData$sourceFile=file
        
        #to add
        addCols=headers[!(headers %in% names(thisData))]
        print(paste('columns [',paste(addCols,collapse=', '),'] added to',file))
        if(length(addCols)>0){
          for(i in 1:length(addCols)){
            thisData[,addCols[i]]=NA
            #print(thisData)
          }
        }
        if(!exists("allData")){
          allData=thisData
        } else {
          allData=rbind(allData,thisData)
        }
      }
    })
  }
  
  includedFiles=c(includedFiles,addFiles)
  assign("includedFiles",includedFiles,envir=.GlobalEnv)
  return(allData)
}

renameKey=list("Second.Detiction.location"="obs2_location",
               "Second.Detection.location"="obs2_location",
               "Secound.Detection.Date"="obs2_date",
               "Second.Detection.Date"="obs2_date",
               "S.A.Necound.Detection.Date"="obs2_date",
               "Third.Detection"="obs3_location",
               "Third.Detection.location"="obs3_location",
               "Third.Detection.Date"="obs3_date"
)


################from Fish&Game, metadata on initial capture -------------
fgData=updateData("FG_Data",headers=c("Fish.Number","PIT.Tag.Number","Species","Total.Length..mm.","Comments")) 

fgData$hexID=fgData$PIT.Tag.Number
fgData$PIT.Tag.Number=NULL

getFgDate=function(sourceFileName){
  date=tail(strsplit(sourceFileName,"[_.]")[[1]],n=2)[1]
  date=paste(as.POSIXct(paste(date,defaultTime),format="%m-%d-%Y %H:%M:%S"))
  return(date)
}


fgData$dateTime=as.POSIXct( sapply(fgData$sourceFile,FUN=getFgDate) )

#######################image metadata.  Includes location data. ---------------------------  
imgData=updateData("Image_Data",headers=c("PHOTO_TITLE","POINT_NOTE","PHOTO_NOTE","LATITUDE","LONGITUDE","POINT_CREATION_DATE"))

last4_img=function(pointNote){
  #print(pointNote)
  firstIdCharPos = regexpr("\\d{4}",pointNote)
  last4=substring(pointNote,first=firstIdCharPos,last=firstIdCharPos+3)
  #print(last4)
  return(last4)
}

imgData$last4Dec=sapply(imgData$POINT_NOTE,last4_img)

imgData=imgData[,c('PHOTO_TITLE','POINT_CREATION_DATE','LATITUDE',"LONGITUDE",'last4Dec','sourceFile')]

imgData$dateTime=as.POSIXlt(imgData$POINT_CREATION_DATE,format="%Y-%m-%dT%H:%M:") #drop seconds...
imgData$POINT_CREATION_DATE=NULL

imgData$lat=imgData$LATITUDE
imgData$LATITUDE=NULL

imgData$lon=imgData$LONGITUDE
imgData$LONGITUDE=NULL

#some Img data in mob data folder:
mobData_oops=updateData("Mobile_Data",headers=c("PHOTO_TITLE","POINT_NOTE","PHOTO_NOTE","LATITUDE","LONGITUDE","POINT_CREATION_DATE"))

mobData_oops$last4Dec=sapply(mobData_oops$POINT_NOTE,last4_img)

mobData_oops=mobData_oops[,c('PHOTO_TITLE','POINT_CREATION_DATE','LATITUDE',"LONGITUDE",'last4Dec','sourceFile')]

mobData_oops$dateTime=as.POSIXlt(mobData_oops$POINT_CREATION_DATE,format="%Y-%m-%dT%H:%M:") #drop seconds...
mobData_oops$POINT_CREATION_DATE=NULL

mobData_oops$lat=mobData_oops$LATITUDE
mobData_oops$LATITUDE=NULL

mobData_oops$lon=mobData_oops$LONGITUDE
mobData_oops$LONGITUDE=NULL

imgData=rbind(imgData,mobData_oops)

###############mobile array data -------------------
mobData=updateData("Mobile_Data",headers=c("Date.Time","Count","Tag_ID","GPS_Location"),skipLines = 6)


mobDataCoord=function(latLon,out="lat"){
  ll=strsplit(latLon,"[ ,]")
  lat=as.numeric(ll[[1]][1])
  lon=as.numeric(ll[[1]][2])
  if(out=="lat"){
    return(lat)
  } else {
    return(lon)
  }
}

mobData$lat=mobDataCoord(mobData$GPS_Location,out="lat")
mobData$lon=mobDataCoord(mobData$GPS_Location,out="lon")
mobData$dateTime=as.POSIXct(mobData$Date.Time,format="%m/%d/%Y %H:%M")



#################handheld reader data -------------------------------
readData=updateData("Reader_Data",headers=c("Scan.Date","Scan.Time","Reader.ID","HEX.Tag.ID","DEC.Tag.ID","LATITUDE.LONGITUDE","Species","Length.Inches","Client"))

readDataCoord=function(latLon,which="lat"){
  latLon=as.character(latLon)
  #print(latLon)
  if(!is.na(latLon)){
    oldstr=""
    newstr="n"
    while(!oldstr==newstr){
      oldstr=newstr
      try(expr={newstr=substring(latLon,0,nchar(oldstr)+1)},silent=T)
    }
    latLon=newstr
  }

#print(!is.na(latLon) & nchar(latLon)>=6)
if( !is.na(latLon) & nchar(latLon)>=6){
  latLon=sub(" ","",latLon)
  latLon=strsplit(latLon,",")[[1]]
  if(length(latLon)==2){
    lat=as.numeric(latLon[1])
    lon=as.numeric(latLon[2])
  } else {
    latLon=strsplit(latLon,"-")[[1]]
    if(length(latLon)==2){
      lat=as.numeric(latLon[1])
      lon=as.numeric(latLon[2])
      lon=-lon
    } else {
      lat=NA
      lon=NA
    }
  }
} else {
  lat=NA
  lon=NA
}
#print(paste0(lat,", ",lon))
if(which=="lat"){
  return(lat)
}
if(which=="lon"){
  return(lon)
}

}


readData$DEC.Tag.ID=as.character(readData$DEC.Tag.ID)

readData$lat=sapply(readData$LATITUDE.LONGITUDE,readDataCoord,"lat")
readData$lon=sapply(readData$LATITUDE.LONGITUDE,readDataCoord,"lon")

readData$dateTime=paste0(readData$Scan.Date,"-",readData$Scan.Time)

readData$dateTime=as.POSIXlt(readData$dateTime,format="%m/%d/%Y-%H:%M:%S")

readData$dateTime[is.na(readData$dateTime)]=as.POSIXlt(
  paste0(readData$Scan.Date[is.na(readData$dateTime)],"-",defaultTime)
  ,format="%m/%d/%Y-%H:%M:%S")


##########---    match fg data location to img data location ----
fgData$lat=NA
fgData$lon=NA
for(i in 1:nrow(fgData)){
  thisDate=fgData[i,'dateTime']
  d=difftime(thisDate,imgData$dateTime,units="hours")
  #print(d[which.min(d)])
  thisImgDataRow=which.min(d)
  if(abs(d[thisImgDataRow])<6){  # if there is a imgData record within 6 hours
    fgData$lat[i]=imgData$lat[thisImgDataRow]
    fgData$lon[i]=imgData$lon[thisImgDataRow]
    #for fish which are in both datasets, this creates a redundant record 
    #w/ a slightly different dateTime (as fg time is set to defaultTime)
    #take img dateTime to make it easy to resolve duplicate records later
    fgData$dateTime[i]=imgData$dateTime[thisImgDataRow]
  }
}
rm(i,d,thisDate)



############--------------generate all fish df ------------------

allFish=data.frame(hexID=unique(fgData$hexID),decID=NA,stringsAsFactors = F)

allFish=rbind(allFish,
              data.frame(hexID=readData$HEX.Tag.ID,
                         decID=readData$DEC.Tag.ID
              )
)

missingFish=allFish #for later...

allFish$hexID[which(nchar(allFish$hexID)<4)]=NA

#abriged decIDs cause problems......
allFish$decID[which(nchar(allFish$decID)<12)]=NA


#allFish[is.na(allFish$hexID),]#if hex id is missing, no fishy here!
allFish=allFish[complete.cases(allFish$hexID),]

allFish$decChar=nchar(allFish$decID)
allFish=allFish[order(allFish$decChar,decreasing=T),]

allFish=allFish[!duplicated(allFish$hexID),]

allFish$decChar=NULL
# some truncation of decID and hexID (less common) could cause info loss

allFish$l4hex=last4(allFish$hexID)
allFish$l4dec=last4(allFish$decID)

allFish$idx=1:nrow(allFish)

missingFish=missingFish[!missingFish$hexID %in% allFish$hexID,]

#########compile observations:

fishObs=merge(allFish,imgData[,c("last4Dec","dateTime","lat","lon","sourceFile")],by.x="l4dec",by.y="last4Dec",all=F,incomparables = NA)

#join to readData by dec OR hex ids, as records are inconsistent
fishObs=rbind(fishObs,
              #when merging by dec, fish w/ abbreviated decIDs can get falsely matched
              merge(allFish,readData[,c("DEC.Tag.ID","dateTime","lat","lon","sourceFile")],by.x="decID",by.y="DEC.Tag.ID",all=F,incomparables = NA),
              merge(allFish,readData[,c("HEX.Tag.ID","dateTime","lat","lon","sourceFile")],by.x="hexID",by.y="HEX.Tag.ID",all=F,incomparables = NA)
)

fishObs=rbind(fishObs,
              merge(allFish,fgData[,c('hexID','dateTime','lat','lon','sourceFile')],by="hexID",all=F,incomparables=NA))


fishObs=rbind(fishObs,
              merge(allFish,mobData[,c("Tag_ID",'dateTime','lat','lon','sourceFile')],by.x="hexID",by.y="Tag_ID",all=F,incomparables=NA))

fishObs$dateTime[as.Date(fishObs$dateTime)=="2022-04-27"] = as.POSIXct("2022-04-28 11:00:00")

allFishObs=fishObs

#
#if no complete observation data, then can't be used.
#complete cases can't handle dates apparently....
fishObs=fishObs[complete.cases(fishObs[,c("lat","lon")]),]
fishObs=fishObs[!is.na(fishObs$dateTime),]
fishObs=fishObs[which(fishObs$idx>=1),]
#remove duplicates from OR join
fishObs=fishObs[!duplicated(fishObs),]

#consider observations of the same fish on the same day duplicates
#fishObs$hour=format.Date(fishObs$dateTime,"%F:%H")

#fishObs$hour=round(as.numeric(format.Date(fishObs$dateTime,"%H"))/4)\

###############-------exclude fish observations within 2 days-----------------------

fishObs$hour=paste0(format.Date(fishObs$dateTime,"%F"),"|")# can add hour to this to allow multiple records per day
fishObs=fishObs[!duplicated(fishObs[,c("idx","hour")]),]
fishObs$hour=NULL

#Also remove same day same place as dups
#this could be sunstantially improved by creating a set of spatial points and calculating distance
#round to (1/10000) is a roughly 10 m y tolerance, 7 m x tolerance
fishObs$roughDayLocation=paste0(round(fishObs$lat,4),"|",
                                round(fishObs$lon,4),"|",
                                format.Date(fishObs$dateTime,"%F"))

fishObs=fishObs[ !duplicated(fishObs[,c("idx","roughDayLocation")]),]

fishObs$roughDayLocation=NULL

##############---------build fish details data w/ obs count, species, etc... -----------
fishDetails=allFish
fishDetails$obsCount=0
fishDetails$firstObserved=Sys.time()
fishDetails$lastObserved=Sys.time()

fishDetails=merge(fishDetails,
                  aggregate(fishObs$sourceFile,by=list(hexID=fishObs$hexID),FUN=paste0,collapse=", "),
                  by="hexID",
                  all=T,
                  incomparables=NA
)

names(fishDetails[names(fishDetails)=="x"])="sources"


for(i in 1:nrow(fishDetails)){
  thisFishIDX=fishDetails[i,"idx"]
  fishDetails$obsCount[i]=sum(fishObs$idx==thisFishIDX,na.rm = T)
  fishDetails$firstObserved[i]=min(fishObs$dateTime[which(fishObs$idx==thisFishIDX)],na.rm=T)
  fishDetails$lastObserved[i]=max(fishObs$dateTime[which(fishObs$idx==thisFishIDX)],na.rm=T)
  
}



getLongestRecord=function(allRecords){
  if(length(allRecords)>1){
    recordLengths=sapply(allRecords,nchar)
    return(allRecords[which.max(recordLengths)])
  } else {
    return (allRecords)
  }
}

#currently only looks to readData for details, more info potentially available from other sources?

fishDetails=merge(fishDetails,aggregate(readData[,c("HEX.Tag.ID","Species","Length.Inches")],
                                        by=list(hexID=readData$HEX.Tag.ID),
                                        FUN=getLongestRecord),
                  by="hexID",all.x=T,all.y=F)

#unlist mixed types:
fixMix=function(mixed,returnNumeric=F){
  ch=as.character(mixed)
  ch[ch %in% c("character(0)","NA")]=NA
  if(returnNumeric){
    return(as.numeric(ch))
  } else {
    return(ch)
  }
}




fishDetails$Species=fixMix(fishDetails$Species)
fishDetails$Length.Inches=fixMix(fishDetails$Length.Inches,returnNumeric=T)
#fishDetails$Client=fixMix(fishDetails$Client)
names(fishDetails)[names(fishDetails)=="x"]="sources"

fishDetails=fishDetails[fishDetails$obsCount>=1,]
fishDetails=fishDetails[order(fishDetails$obsCount,decreasing = T),]

###############client list:

clientList=readData[,c("HEX.Tag.ID","Client")]
clientList=clientList[complete.cases(clientList),]
clientList$idx=NA

for(i in 1:nrow(clientList)){
  thisHID=clientList$HEX.Tag.ID[i]
  if(thisHID %in% allFish$hexID){
    thisIDX=allFish$idx[allFish$hexID==thisHID]
  }
  if(thisHID %in% allFish$l4hex){
    thisIDX=allFish$idx[allFish$l4hex==thisHID]
  }
  
  if(thisHID %in% allFish$decID){
    thisIDX=allFish$idx[allFish$decID==thisHID]
  }
  if(thisHID %in% allFish$l4dec){
    thisIDX=allFish$idx[allFish$l4dec==thisHID]
  }
  clientList$idx[i]=thisIDX
}

clientList=merge(clientList[,c("Client","idx")],allFish)
clientList=unique(clientList)

clientList=clientList[,c("idx","l4hex","Client")]



write.csv(fishDetails,file=paste0("fishDetails_",Sys.Date(),".csv"))
write.csv(fishObs,file=paste0("fishObs_",Sys.Date(),".csv"))
write.csv(clientList,file=paste0("clientList_",Sys.Date(),".csv"))


##############-------------generate routes----------------------

library(sf)
library(terra)
library(gdistance)

library(raster) #still needed for type conversions within gdistance::transition

#sources
#windows
netDSN="C:/Users/sam/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/SilverCreekNet_revised.gpkg"
#linux
netDSN="/home/sam/Dropbox/SilverCreek/SilverCreekSpatial/StaticData/SilverCreekNet_revised.gpkg"

netName="SilverCreek"


#load data
network=st_read(dsn=netDSN,layer=netName)


obsPoints=fishObs


#limit obsPoints to those with multiple records
#to facilitate location fixing (when coord is bad, use next coord record), use fishObs which still has all records
obsPoints=obsPoints[
  obsPoints$idx %in% obsPoints$idx[
    duplicated(obsPoints$idx)]
  ,]



obsPoints=obsPoints[order(obsPoints$dateTime),]

obsPoints=st_as_sf(obsPoints,coords=c("lon","lat"),crs=4326)

obsPoints=st_transform(obsPoints,crs=26911)

nameByCount=function(id,df){
  count=sum(df$idx==id)
  df$locationName[df$idx==id]=paste0(id,"_",1:count)
  return(df)
}

obsPoints$locationName=""
for(id in unique(obsPoints$idx)){
  obsPoints=nameByCount(id,obsPoints)
}
#check geom and crs...
#st_geometry(obsPoints)
#st_geometry(network)
st_crs(obsPoints)==st_crs(network)

#rm M (order) for GEOS compatibility
network=st_zm(network)

snapPointsToLines=function(points_to_snap,target_lines){
  #First define points along lines to snap to...
  snapPoints=st_line_sample(st_cast(target_lines,"LINESTRING"),density=1)#'density' snap points per meter
  
  #snap array pts to pts along network lines:
  #st_snap to the line directly doesn't work, st_snap to all sampled points also fails.  
  
  for(i in 1:nrow(points_to_snap)){ #iterate through points
    near=st_nearest_points(points_to_snap[i,],snapPoints)  #find line to all snapPoints
    thisLocation=st_geometry( 
      st_cast(near[which.min(st_length(near))],"POINT")[2] #find geometry of nearest snap point. [2] returns 'to' point of st_nearest line
    )  
    print(paste("Snapped point",points_to_snap$locationName[i],"to target line at distance of", round(st_distance(points_to_snap[i,],thisLocation),1), "meters"))
    st_geometry( points_to_snap[i,] ) = st_geometry( thisLocation )
  }
  return(points_to_snap)
}

########todo - when snap distance exceeds threshold, assume cold start GPS and look up subsequent observation
obsPoints=snapPointsToLines(points_to_snap = obsPoints,target_lines = network)



#set up cost surface raster of network -----------
network$weight=1 
rastRes=5#reducing resolution vastly speeds up process
nr=terra::rast(vect(network),res=rastRes,crs="EPSG:26911",vals=NA) 

netBuff=st_buffer(network,dist=2*rastRes)
netBuff$weight=0.1

# not sure why, but only works w/ update = T

nr=terra::rasterize(vect(netBuff),nr,field="weight",update=T)
nr=terra::rasterize(vect(network),nr,field="weight",update=T)
#check...
#plot(nr)

costSurface=transition(raster::raster(nr),transitionFunction=prod,directions=8)
#throws 'error in function(x) : non function' errors, but works



#make df of all route ids
#arrays$ID=row.names(arrays)
#allRouteIDs=expand.grid(arrays$ID,arrays$ID)
#names(allRouteIDs)=c("from","to")

#########define all routes to be mapped----
fromTo=data.frame(idx=character(),from=character(),to=character())
for(fish in unique(obsPoints$idx)){
  obsCount=sum(obsPoints$idx==fish)
  addToFromTo=data.frame(idx=fish,from=paste0(fish,"_",1:(obsCount-1)),to=paste0(fish,"_",2:obsCount))
  fromTo=rbind(fromTo,addToFromTo)
}

row.names(obsPoints)=obsPoints$locationName

buildRoute=function(idx,from,to,costSurf,obsPoints){
  routeName=paste0(from,"_to_",to)
  print(routeName)
  print(st_distance(obsPoints[obsPoints$locationName==from,] , obsPoints[obsPoints$locationName==to,] )[1,1])
  isSamePt=as.numeric(st_distance(obsPoints[obsPoints$locationName==from,],obsPoints[obsPoints$locationName==to,])[1,1])<3 #3 meter difference required to form a route 
  if(isSamePt){ #return a 'linestring' from/to the same point
    coords=st_coordinates(obsPoints[obsPoints$locationName==from,])
    routePoint=st_point(coords,dim="XY")
    route=st_linestring(c(routePoint,routePoint))
    route=st_sfc(route,crs=st_crs(26911))
    route=st_as_sf(route)
    route$geometry=st_geometry(route)
    route$x=NULL
    route=st_as_sf(route)
    st_transform(route,st_crs(26911))
    #st_crs(route)=st_crs(26911)
  }else{
    
    route=shortestPath(costSurf, 
                       origin=st_coordinates(obsPoints[obsPoints$locationName==from,]),
                       goal=st_coordinates(obsPoints[obsPoints$locationName==to,]),
                       output="SpatialLines")
    
    route=st_as_sf(route)
    st_crs(route)=st_crs(obsPoints)
    route=st_transform(route,crs=st_crs(26911))
  }
  route$routeName=routeName
  route$idx=idx
  return(route)
}

#could apply, but then I get a list of sf objects rather than a 'multi' sf object
for(i in 1:nrow(fromTo)){
  thisRoute=buildRoute(idx=fromTo$idx[i], from=fromTo$from[i], to=fromTo$to[i],
                       costSurf = costSurface, obsPoints = obsPoints)
  if(i==1){
    allRoutes=thisRoute
  } else { allRoutes=rbind(allRoutes,thisRoute) }
}

row.names(allRoutes)=allRoutes$routeName
allRoutes=st_as_sf(allRoutes)
allRoutes$routeLength=st_length(allRoutes)

plot(st_geometry(network),col="blue")
plot(st_geometry(obsPoints),add=T)
plot(st_geometry(allRoutes),add=T,lwd=4)

routeLenDf=data.frame(allRoutes)[,c("idx","routeLength")]

fishDetails=merge(fishDetails,aggregate(routeLength~idx,data=routeLenDf,FUN=sum),all.x=T)
#plot(st_geometry(allRoutes[20,]),add=T,lwd=4)

#st_write(allRoutes,"allRoutes.gpkg",append=F,overwrite=T)

fishDetails$routeLength_km=as.numeric(fishDetails$routeLength/1000)
fishDetails$routeLength=NULL
fishDetails=fishDetails[order(fishDetails$obsCount,decreasing = T),]
############# routes to location timeseries-----------------

calculateRoutePoints=function(startLoc,endLoc,startTime,endTime,routes,timestep="hours"){
  thisTimes=round(
    seq(from=as.POSIXct(startTime),to=as.POSIXct(endTime),by=timestep),
    units=timestep  
  )
  
  thisRouteName=paste0(startLoc,"_to_",endLoc)
  thisRoute=routes[routes$routeName==thisRouteName,]
  
  if(startLoc == endLoc){
    #find coords of this location
    locationDF=data.frame(st_coordinates(thisRoute))[1,] #grab the first coord
    locationDF=locationDF[rep(1,length(thisTimes)),]
  }else{ #find coords along route
    location=st_line_sample(thisRoute,n=length(thisTimes))
    locationDF=data.frame(st_coordinates(location))
  }
  locationDF$time=thisTimes
  locationDF$L1=NULL
  locationDF$routeName=thisRouteName
  
  return(locationDF)
}


buildLocationTimeseries=function(observations,allRoutes){ #must have an 'idx' field, a 'locationName' field, and a 'dateTime' field
  #for debug:
  observations=obsPoints
  
  fish=unique(observations$idx)
  
  for(f in fish){
    print(paste("fish",f))
    thisFishObs=observations[observations$idx==f,]
    thisFishObs=thisFishObs[order(thisFishObs$dateTime),]
    for(o in 2:nrow(thisFishObs)){
      #print(o)
      thisStartLoc=data.frame(thisFishObs)[o-1,"locationName"]
      thisEndLoc=data.frame(thisFishObs)[o,"locationName"]
      thisStartTime=data.frame(thisFishObs)[o-1,"dateTime"]
      thisEndTime=data.frame(thisFishObs)[o,"dateTime"]
      
      thisRoutePoints=calculateRoutePoints(startLoc = thisStartLoc, endLoc = thisEndLoc, 
                                           startTime = thisStartTime, endTime = thisEndTime,
                                           routes=allRoutes, timestep = "hours")
      #print(paste0(thisStartLoc," to ",thisEndLoc))
      
      thisRoutePoints$idx=f
      if(exists("allLocations")){
        allLocations=rbind(allLocations,thisRoutePoints)
      }else{
        allLocations=thisRoutePoints
      }
    }
  }
  return(allLocations)
  
}

locationTimeseries=buildLocationTimeseries(observations=obsPoints,allRoutes=allRoutes)

#locationTimeseries$timeNumeric=as.numeric(locationTimeseries$time)

locationTimeseries=st_as_sf(locationTimeseries,coords=c("X","Y"),crs=st_crs(26911))


locationTimeseries=st_transform(locationTimeseries,crs=st_crs(4326))

format(object.size(locationTimeseries),units="Mb")

#st_write(locationTimeseries,"C:\\Users\\sam\\Documents\\SilverCreek\\R\\SilverCreekApp\\fishLocationTimeseries.gpkg",append=FALSE)

#plot(st_geometry(locationTimeseries))
#plot(st_geometry(locationTimeseries[locationTimeseries$idx==280,]))


fishObs[fishObs$idx==280,]
readData[readData$HEX.Tag.ID=="3DD.003E281A82",]$LATITUDE.LONGITUDE



hist(allFishObs$dateTime,breaks="months")

##########--------------write to db-------


#dbGetQuery(conn,"SELECT * FROM fishlocations LIMIT 10;")


writeMe=locationTimeseries[,c("idx","time")]

names(writeMe)=c("fishid","datetime","geometry")
dbWriteTable(conn,"fishlocations",writeMe,overwrite=T)


#write fish details table
dbWriteTable(conn,"fishdetails",fishDetails,overwrite=T)

dbWriteTable(conn,"clientfish",clientList,overwrite=T)

max(fishObs$dateTime)
