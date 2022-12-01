require(shiny)
require(sf)
require(leaflet)
require(leaflet.extras2)
require(magrittr)
require(DBI)
require(RPostgres)
require(DT)

source("movingMarkerFunctions.R")

fishIcon=makeIcon(iconUrl="https://trackatrout.com/Images/TroutIcon.png",
                  iconWidth = 60, iconHeight = 60,
                  iconAnchorX=30,iconAnchorY = 30)


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

#get fishdetails once and handle locally
fishDetails=dbGetQuery(conn,"SELECT * FROM fishdetails;")



clientFish=dbGetQuery(conn,"select * from clientfish;")
clientFish$ncharClient=nchar(clientFish$Client)
clientFish=clientFish[clientFish$ncharClient>=1,c("idx","l4hex","Client")]
clientFish$Client=tolower(clientFish$Client)



###############------------------get and prepare kml data------------------


TempLoggerIcon=makeAwesomeIcon(icon="thermometer",markerColor = "darkred",library = "fa",iconColor = "black")
DOLoggerIcon=makeAwesomeIcon(icon="leaf",markerColor = "darkgreen",library = "fa",iconColor = "black")
FlowIcon=makeAwesomeIcon(icon="signal",markerColor = "darkblue",library = "fa",iconColor = "black")




temperatureLoggers=st_read("./www/Temperature Data 2020.kml")
DOLoggers=st_read("./www/DO-Trackatrout.kml")
#restorationSites=st_read("./www/Silver Creek Restoration.kml")

# makeLinkNewWindow=function(htmlString){
#   endLink=regexpr('/\">',htmlString)[[1]]+1
#   htmltools::HTML(substring(htmlString,1,endLink),
#                   ' target="_blank"> ',
#                   substring(htmlString,endLink+2))
# }

#temperatureLoggers$Description=sapply(temperatureLoggers$Description,makeLinkNewWindow)
#DOLoggers$Description=sapply(DOLoggers$Description,makeLinkNewWindow)
