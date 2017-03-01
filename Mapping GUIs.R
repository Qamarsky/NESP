
############# Map the surveys #########

map(database="world", regions="Australia", interior=TRUE)
points(CUA$Longitude, CUA$Latitude, pch=16, col="black")
points(KAB$Longitude, KAB$Latitude, pch=20, col="red")
points(CSIRO$TrlStartlong, CSIRO$TrlStartlat, pch=24, col="blue")

## Let's just look at Brisbane area ##
quartz()
map(database="world", ylim = c(-28,-26), xlim = c(152,154))
points(CUA$Longitude, CUA$Latitude, pch=16, col="black")
points(KAB$Longitude, KAB$Latitude, pch=20, col="red")
points(CSIRO$TrlStartlong, CSIRO$TrlStartlat, pch=24, col="blue")

points(Covars2$Long, Covars2$Lat, col=as.factor(Covars$Source))
legend(-28,152)


# There are 2431 KAB surveys in the Brisbane area # 
# Let's look on Google maps

m1 <- get_map(location = 'Brisbane', zoom = 10, maptype="watercolor") ## very pretty :)
m2<- get_map(location = 'Brisbane', zoom = 10, maptype="satellite") 
mx<-get_map(location='Brisbane', zoom=9, maptype='satellite')

map <- ggmap(m2)
quartz()
map + geom_point(aes(x = Longitude, y = Latitude), data = KAB, alpha = .5) +
  geom_point(aes(x = Longitude, y = Latitude), data = CUA, alpha = .5, col="red") +
  geom_point(aes(x= TrlStartlong, y=TrlStartlat), data=CSIRO, alpha=.5, col="blue")


map$mapping$x<-Longitude

map2<-qmplot(mx)
map2

### use gglocator to find lat/lon of interest - not successful yet

ggmap.loc <- function(object){
  x <- grid.ls()[[1]][grep("panel-", grid.ls()[[1]])] #locate the panel
  seekViewport(x)
  y <-  as.numeric(grid.locator("npc"))
  locatedX <- min(object$data$long) + y[1]*diff(range(object$data$long))
  locatedy <- min(object$data$lat) + y[2]*diff(range(object$data$lat))
  return(c(locatedX, locatedy))
}


tarnTMap = leaflet(list(tarnTDat1, tarnTDat2), dest="maps/",
                   style=list(styTrack, styStops),
                   title="Tour du Tarn à Vélo - Juillet 2015", 
                   base.map=list("osm","tls"), 
                   popup=c("altitude", "halte", "dates", "hébergement",
                           "restauration"), incl.data=TRUE)


KAB_small<-KAB_sub[,c(1,191:199)]
KAB_small$LatLong<-paste(KAB_small$Latitude,KAB_small$Longitude, sep=":")
#Dat1 = toGeoJSON(data=KAB_small, name="map")

#Mapx<-leaflet(Dat1, base.map=("osm"))

M1 <- gvisMap(KAB_small, locationvar="LatLong", tipvar = "X.1", 
              options=list(showTip=TRUE, showLine=FALSE, enableScrollWheel=TRUE, 
                           mapType='satellite', useMapTypeControl=TRUE, width=800, height=700))
plot(M1)


data(Andrew)
Mx <- gvisMap(Andrew, "LatLong", "Tip", 
              options=list(showTip=TRUE, showLine=F, enableScrollWheel=TRUE, 
                           mapType='satellite', useMapTypeControl=TRUE, width=800,height=400))
plot(Mx)



(clicks <- clicks <- gglocator(2) )
expand.grid(lon = clicks$lon, lat = clicks$lat)

clicks<-gglocator(2)

map +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))
gglocator(1, xexpand = c(0,0), yexpand = c(0,0))


# try Sydney
m3<- get_map(location = 'Sydney', zoom = 10, maptype="satellite") 

map2 <- ggmap(m3)
map2 + geom_point(aes(x = Longitude, y = Latitude), data = KAB, alpha = .5) +
  geom_point(aes(x = Longitude, y = Latitude), data = CUA, alpha = .5, col="red") +
  geom_point(aes(x= TrlStartlong, y=TrlStartlat), data=CSIRO, alpha=.5, col="blue")
# Melbourne

m4<- get_map(location = 'Melbourne, Australia', zoom = 10, maptype="satellite") 

map3 <- ggmap(m4)
map3 + geom_point(aes(x = Longitude, y = Latitude), data = KAB, alpha = .5) +
  geom_point(aes(x = Longitude, y = Latitude), data = CUA, alpha = .5, col="red") +
  geom_point(aes(x= TrlStartlong, y=TrlStartlat), data=CSIRO, alpha=.5, col="blue")


#Perth
m5<- get_map(location = 'Perth', zoom = 10, maptype="satellite") 

map4 <- ggmap(m5)
map4 + geom_point(aes(x = Longitude, y = Latitude), data = KAB, alpha = .5) +
  geom_point(aes(x = Longitude, y = Latitude), data = CUA, alpha = .5, col="red")+
  geom_point(aes(x= TrlStartlong, y=TrlStartlat), data=CSIRO, alpha=.5, col="blue")

# Hobart

m6<- get_map(location = 'Hobart', zoom = 10, maptype="satellite") 

map5 <- ggmap(m6)
map5 + geom_point(aes(x = Longitude, y = Latitude), data = KAB, alpha = .5) +
  geom_point(aes(x = Longitude, y = Latitude), data = CUA, alpha = .5, col="red") +
  geom_point(aes(x= TrlStartlong, y=TrlStartlat), data=CSIRO, alpha=.5, col="blue")









###### Select upstream and downstream sites in Brisbane ##### 
### gvis only allows 400 sites to be displayed :( 
Covars2$LatLong<-paste(Covars2$Lat,Covars2$Long, sep=":")
Map <- gvisMap(Covars2, locationvar="LatLong", tipvar = "Global_ID", 
               options=list(showTip=TRUE, showLine=FALSE, enableScrollWheel=TRUE, 
                            mapType='satellite', useMapTypeControl=TRUE, width=800, height=700))

plot(Map)

KAB_small<-KAB_sub[,c(1,191:199)]
KAB_small$LatLong<-paste(KAB_small$Latitude,KAB_small$Longitude, sep=":")
#Dat1 = toGeoJSON(data=KAB_small, name="map")

#Mapx<-leaflet(Dat1, base.map=("osm"))

M1 <- gvisMap(KAB_small, locationvar="LatLong", tipvar = "X.1", 
              options=list(showTip=TRUE, showLine=FALSE, enableScrollWheel=TRUE, 
                           mapType='satellite', useMapTypeControl=TRUE, width=800, height=700))
plot(M1)


#### try leaflet ###

Covars2_Bris<-Covars2[Covars2$Long<154 & Covars2$Long>152 & Covars2$Lat<(-26) & Covars2$Lat > (-28),]


map2<-leaflet(Covars2_Bris)
map2<-addProviderTiles(map2, "Esri.WorldImagery", group = "Satellite")
map2<-addTiles(map2, urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}")
map2<-addMarkers(map2,~Long,~Lat,popup = ~as.character(Global_ID))


map2





### putting it onto shiny #### 
library(shiny)
library(leaflet)

map2<-leaflet(Covars2_Bris)
map2<-addProviderTiles(map2, "Esri.WorldImagery", group = "Satellite")
map2<-addMarkers(map2,~Long,~Lat,popup = ~as.character(Global_ID), layerId = ~as.vector(Covars2$Global_ID))




ui<-fluidPage(
  leafletOutput("map2"),
  textOutput("clicks")
)

server<-function(input, output, session){
  
  #logfilename<-"upstream"
  
  #observe({  obs<- cat(input$map2_marker_click, '\n', file = logfilename, append = TRUE)
  # })
  
  observe({clicks<-input$mapid_marker_click
           if (is.null(observe))
             return()
  })
  
  observe({
    event <- input$map2_marker_click
    if (is.null(event))
      return()
    print(event)      
  })
  
  output$map2 <- renderLeaflet({map2
  })
  
  
  output$clicks<-renderText ({clicks})
  #session$onSessionEnded(function() {
  #  obs$suspend()
  
  #    Also clean up the log file for this example
  #    unlink(logfilename)
  #  })
  
  # output$filetable <- renderTable(
  #isolate(input$tabs)
  #   obs
  #)
  
}

shinyApp(ui, server)



##### SAMPLE DATA ######

Lat<-runif (100, min=-28, max=-26)
Long<-runif(100, min=152, max=153)
Global_ID<-(1:100)
data<-as.data.frame(cbind(Lat, Long, Global_ID))

## create map

map3<-leaflet(data)
map3<-addProviderTiles(map3, "Esri.WorldImagery", group = "Satellite")
map3<-addMarkers(map3,~Long,~Lat,popup = ~as.character(Global_ID), layerId = ~as.vector(data$Global_ID))


##UI

ui<-fluidPage(
  leafletOutput("map3")
)

## server

server<-function(input, output, session){
  
  output$map3 <- renderLeaflet({map3
  })
  
  logfilename<-"upstream"
  
  obs <-observe({ 
    c(input$map3_marker_click)
  })
  
  session$onSessionEnded(function() {
    obs$suspend()
    
    
    #    Also clean up the log file for this example
    unlink(logfilename)
  })
  
  
}

shinyApp(ui, server)


#### finished shiny ####


drop_upload("~/Documents/R data/NESP/R scripts/Mapping GUIs.R", dest="/NESP/R scripts")

