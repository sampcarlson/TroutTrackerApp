require(shiny)
require(sf)
require(leaflet)
require(leaflet.extras2)
require(magrittr)
require(DBI)
require(RPostgres)
require(DT)
require(htmltools)

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
#####################################-----------------------------------begin dput mess--------------------------------------

DODescription = c('Dissolved Oxygen Data #1<br> <a href="https://savesilvercreek.org/1-dissolved-oxygen-lower-silver-creek/", target="_blank"> Logger Data Page</a>', 
                'Dissolved Oxygen Data #2<br> <a href="https://savesilvercreek.org/2-dissolved-oxygen-butte-creek/", target="_blank"> Logger Data Page</a>', 
                'Dissolved Oxygen Data #3<br> <a href="https://savesilvercreek.org/3-dissolved-oxygen-lower-loving-creek/", target="_blank"> Logger Data Page</a>', 
                'Dissolved Oxygen Data #4<br> <a href="https://savesilvercreek.org/4-dissolved-oxygen-lower-grove-creek/", target="_blank"> Logger Data Page</a>', 
                'Dissolved Oxygen Data #5<br> <a href="https://savesilvercreek.org/5-dissolved-oxygen-hwy-20-bridge-2/", target="_blank"> Logger Data Page</a>', 
                'Dissolved Oxygen Data #6<br> <a href="https://savesilvercreek.org/6-dissolved-oxygen-lower-stalker-cr/", target="_blank"> Logger Data Page</a>', 
                'Dissolved Oxygen Data #6a<br> <a href="https://savesilvercreek.org/6a-dissolved-oxygen-lower-chaney-cr/", target="_blank"> Logger Data Page</a>', 
                'Dissolved Oxygen Data #7<br> <a href="https://savesilvercreek.org/7-dissolved-oxygen-upper-loving-cr/", target="_blank"> Logger Data Page</a>', 
                'Dissolved Oxygen Data #8<br> <a href="https://www.savesilvercreek.com/8-dissolved-oxygen-upper-stalker-cr/", target="_blank"> Logger Data Page</a>', 
                'Dissolved Oxygen Data #9<br> <a href=https://www.savesilvercreek.com/9-dissolved-oxygen-kilpatrick-bridge/", target="_blank"> Logger Data Page</a>')


TempDescription = c('Temperature Data for Lower Silver Creek at Highway 93 #1<br> <a href="https://savesilvercreek.org/data-for-1-highway-93/">Logger Data Page</a>', 
                'Temperature Data for Lower Silver Creek at Susie Q #2<br> <a href="https://savesilvercreek.org/2-temperature-data-for-susie-q/">Logger Data Page</a>', 
                'Temperature Data for Lower O Drain #3<br> <a href="https://savesilvercreek.org/3-temperature-data-for-o-drain/">Logger Data Page</a>', 
                'Temperature Data for Silver Creek Hwy 20 (TNC) #4<br> <a href="https://savesilvercreek.org/4-temperature-data-for-hwy-20-tnc/">Logger Data Page</a>', 
                'Temperature Data for Silver Creek RR Bridge #5<br> <a href="https://savesilvercreek.org/5-temperature-data-for-rr-bridge/">Logger Data Page</a>', 
                'Temperature Data for S Turns #6 <a href="https://savesilvercreek.org/6-temperature-data-for-s-turns-tnc/">Logger Data Page</a>', 
                'Temperature Data for Visitor Center #7<br> <a href="https://savesilvercreek.org/7-temperature-data-for-visitor-center-tnc/">Logger Data Page</a>', 
                'Temperature Data for Sullivans Pond #8</strong><br> <a href="https://savesilvercreek.org/8-temperature-data-for-sullivan-pond-tnc/">Logger Data Page</a>', 
                'Temperature Data for Lower Stalker #9</strong> <br> <a href="https://savesilvercreek.org/9-temperature-data-for-lower-stalker-tnc/">Logger Data Page</a>', 
                'Temperature Data for Upper Stalker Creek #10<br> <a href="https://savesilvercreek.org/10-temperature-data-for-upper-stalker/">Logger Data Page</a>', 
                'Temperature Data for Upper Stalker Cabin #11 <br> <a href="https://savesilvercreek.org/11-temperature-data-for-upper-stalker-tnc/">Logger Data Page</a>', 
                'Temperature Data for Upper Chaney Creek Above Pond #12 <br> <a href="https://savesilvercreek.org/12-temperature-data-for-lower-chaney-below-pond/">Logger Data Page</a>', 
                'Temperature Data for Upper Chaney Creek Above Pond #13 <br> <a href="https://savesilvercreek.org/13-temperature-data-for-chaney-above-pond/">Logger Data Page</a>', 
                'Temperature Data for Upper Chaney Creek #14 <br> <a href="https://savesilvercreek.org/14-temperature-data-for-upper-chaney/">Logger Data Page</a>', 
                'Temperature Data for Lower Cain Creek #15 <br> <a href="https://savesilvercreek.org/15-temperature-data-for-lower-cain/">Logger Data Page</a>', 
                'Temperature Data for North Fork Chaney Spring Head #16<br> <a href="https://savesilvercreek.org/16-temperature-data-for-nf-chaney-sp/">Logger Data Page</a>', 
                'Temperature Data for West Fork Chaney Spring Head #17<br> <a href="https://savesilvercreek.org/17-temperature-data-for-wf-chaney-sp/">Logger Data Page</a>', 
                'Temperature Data for Cain Creek Spring Head #18<br> <a href="https://savesilvercreek.org/18-temperature-data-for-cain-sp/">Logger Data Page</a>', 
                'Temperature Data for Lower Grove Creek #19 (TNC)<br> <a href="https://savesilvercreek.org/19-temperature-data-for-lower-grove-tnc/">Logger Data Page</a>', 
                'Temperature Data for Lower Grove Creek #20<br> <a href="https://savesilvercreek.org/20-temperature-data-for-lower-grove/">Logger Data Page</a>', 
                'Temperature Data for Wilson Creek #21<br> <a href="https://savesilvercreek.org/21-temperature-data-for-wilson/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Wilson Spring Head #22 <br> <a href="https://savesilvercreek.org/22-temperature-data-for-wilson-spring/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Lower Thompson Creek #23 <br> <a href="https://savesilvercreek.org/23-temperature-data-for-lower-thompson/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Thompson Spring Head #24 <br> <a href="https://savesilvercreek.org/24-temperature-data-for-thompson-sp/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Grove Creek at Pumpkin Center Road #25<br> <a href="https://savesilvercreek.org/25-temperature-data-for-grove-at-pmk-ct-rd/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for East Fork Grove Creek #3 Spring Head #26<br> <a href="https://savesilvercreek.org/26-temperature-data-for-ef-grove-sp-3/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for East Fork Grove Creek #2 Spring Head #27<br> <a href="https://savesilvercreek.org/27-temperature-data-for-east-fork-grove-sp-2/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for East Fork Grove Creek #1 Spring Head #28<br> <a href="https://savesilvercreek.org/28-temperature-data-for-east-fork-grove-sp-1/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for North Fork Grove Spring Head #29<br> <a href="https://savesilvercreek.org/29-temperature-data-for-north-fork-grove-sp/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Lower Mud Creek #30<br> <a href="https://savesilvercreek.org/30-temperature-data-for-lower-mud/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Lower Loving Creek #31<br> <a href="https://savesilvercreek.org/31-temperature-data-for-lower-loving-tnc/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Lower Loving Creek #32<br> <a href="https://savesilvercreek.org/32-temperature-data-for-lower-loving/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Upper Loving Creek #33<br> <a href="https://savesilvercreek.org/33-temperature-data-for-upper-loving/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for East Fork Loving Creek and Butte Creek #34<br> <a href="https://savesilvercreek.org/35-temperature-data-for-lower-north-fork-loving/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Lower North Fork Loving Creek #35<br> <a href="https://savesilvercreek.org/35-temperature-data-for-lower-north-fork-loving/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Lower West Fork Loving Creek #36<br> <a href="https://savesilvercreek.org/36-temperature-data-for-lower-west-fork-loving/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Upper West Fork Loving Creek (Rio) #37<br> <a href="https://savesilvercreek.org/37-temperature-data-for-upper-west-fork-loving/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Upper Main North Fork Loving Creek #38<br> <a href="https://savesilvercreek.org/38-temperature-data-for-upper-main-north-fork-loving/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for North Fork Loving Creek #39<br> <a href="https://savesilvercreek.org/39-temperature-data-for-north-fork-loving/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Middle Fork Loving Creek #40<br> <a href="https://savesilvercreek.org/40-temperature-data-for-middle-fork-loving/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for North Fork Loving #1 Spring Head #41<br> <a href="https://savesilvercreek.org/41-temperature-data-for-north-fork-loving-spring-1/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for North Fork Loving #2 Spring Head #42<br> <a href="https://savesilvercreek.org/42-temperature-data-for-north-fork-loving-spring-2/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for North Fork Loving Below Pond #43<br> <a href="https://savesilvercreek.org/43-temperature-data-for-north-fork-loving-below-pond/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Mud Creek Spring Head #44<br> <a href="https://savesilvercreek.org/44-temperature-data-for-mud-spring/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Kilpatrick Bridge #45<br> <a href="https://savesilvercreek.org/45-temperature-data-for-kilpatrick-bridge-tnc/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Mid Grove Creek #46<br> <a href="https://savesilvercreek.org/46-temperature-data-for-mid-grove/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Mid Thompson Creek #47<br> <a href="https://savesilvercreek.org/47-temperature-data-for-mid-thompson/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Mid Loving Creek (LCR) #48<br> <a href="https://savesilvercreek.org/48-temperature-data-for-mid-loving-lcr-1/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Mid Loving Creek (LCR) #49<br> <a href="https://savesilvercreek.org/49-temperature-data-for-mid-loving-lcr-2/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Mid Loving Creek (LCR) #50<br> <a href="https://savesilvercreek.org/50-temperature-data-for-mid-loving-lcr-3/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Silver Creek Priest Bridge #51<br> <a href="https://savesilvercreek.org/51-temperature-data-for-silver-creek-priest-bridge/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Silver Creek Upper French Ranch #52<br> <a href="https://savesilvercreek.org/52-temperature-data-for-silver-creek-upper-french/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Mid Mud Creek #53<br> <a href="https://savesilvercreek.org/53-temperature-data-for-mid-mud/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Silver Creek Lower French Ranch #54<br> <a href=https://savesilvercreek.org/54-temperature-data-for-silver-creek-lower-french/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Lower RR Ranch #55<br> <a href="https://savesilvercreek.org/55-temperature-data-for-silver-creek-lower-rr/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for East Fork Grove Creek #4 Spring Head #56<br> <a href="https://savesilvercreek.org/56-temperature-data-for-east-fork-grove-sp-4/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Silver Creek S-Turns #57<br> <a href="https://savesilvercreek.org/57-temperature-data-for-silver-creek-s-turns/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Lower Island Purdy Pond #58<br> <a href="https://savesilvercreek.org/58-temperature-data-for-pond-island/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Pond Dam Outlet #59<br> <a href="https://savesilvercreek.org/59-temperature-data-for-pond-dam-outlet/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Lower Willows #60<br> <a href="https://savesilvercreek.org/60-temperature-data-for-silver-creek-lower-willows/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Upper Oxbow #61<br> <a href="https://savesilvercreek.org/61-temperature-data-for-rr-upper-oxbow/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Lower Oxbow #62<br> <a href="https://savesilvercreek.org/62-temperature-data-for-rr-lower-oxbow/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for RR Well #63<br> <a href="https://savesilvercreek.org/63-temperature-data-for-rr-well/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Purdy Dam #64<br> <a href="https://savesilvercreek.org/64-temperature-data-for-pond-at-dam-lower-level/" target="_blank"> Logger Data Page </a>', 
                'Temperature Data for Lower Stalker #65<br> <a href="https://savesilvercreek.org/65-temperature-data-for-lower-stalker/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Upper Main Stem #66<br> <a href="https://savesilvercreek.org/66-temperature-data-for-upper-main-stem/" target="_blank"> Logger Data Page</a>', 
                'Temperature Data for Mid Stalker #67</strong><br> <a href="https://savesilvercreek.com/67-temperature-data-for-mid-stalker/" target="_blank"> Logger Data Page</a>')
#write.csv(temperatureLoggers,file="tempLog.csv")

#restorationSites=st_read("./www/Silver Creek Restoration.kml")

# makeLinkNewWindow=function(htmlString){
#   endLink=regexpr('/">',htmlString)[[1]]+1
#   htmltools::HTML(substring(htmlString,1,endLink),
#                   ' target="_blank"> ',
#                   substring(htmlString,endLink+2))
# }

#temperatureLoggers$Description=sapply(temperatureLoggers$Description,makeLinkNewWindow)
#DOLoggers$Description=sapply(DOLoggers$Description,makeLinkNewWindow)
