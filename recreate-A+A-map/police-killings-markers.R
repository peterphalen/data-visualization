
#download needed packages you don't have 
wants <- c("magrittr", "leaflet", "jsonlite", "curl")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
require(jsonlite)
require(curl)
require(leaflet)
require(magrittr)


#pull data from json file embedded in the Guardian's The Counted website: http://www.theguardian.com/thecounted
thecounted <- fromJSON("https://interactive.guim.co.uk/2015/the-counted/v/1455138961531/files/skeleton.json")
thecounted <- subset(thecounted, name != "Unknown") #drop deaths with unknown names

#Use the leaflet htmlwidget to create an interactive online visualization of data
leaflet(data = thecounted) %>%   #data from the counted
  
  #add default open source map tiles from OpenStreetMap
  addProviderTiles("MapQuestOpen.Aerial")%>%  
  
  #fit bounds around the USA
  fitBounds(-150,35, -70,50) %>%  
  
  #dynamically add markers for people who were killed
  addMarkers(~long, ~lat, 
         #create pop-up windows with some information for each marker
         popup = ~ name   )


