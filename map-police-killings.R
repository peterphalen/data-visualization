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
thecounted <- subset(thecounted, name != "Unknown") #drop people not yet identified

#Color-code for whether the victim was armed
#  Red = Unarmed
unarmedC <-"#ff0000"
#  Blue = armed
armedC <-  "#ffa500"
#  Black = Don't know or ambiguous category like "Non-lethal firearm" or "vehicle"
idkC <- "#000000"
pal <- colorFactor(c(rep(armedC,3), unarmedC, rep(idkC,4)), domain= c("Disputed", 
                                                                      "Firearm", 
                                                                      "Knife", 
                                                                      "No", 
                                                                      "Non-lethal firearm",
                                                                      "Other",
                                                                      "Unknown",
                                                                      "Vehicle"))

# automatically set date range for pulled data
today <- Sys.Date()
today <- format(today, format="%b %Y")
dateRange <- paste0("(Jan 2015"," - ", today,")")

#Use the leaflet htmlwidget to create an interactive online visualization of data
leaflet(data = thecounted) %>%   #data from the counted
  
  #add default open source map tiles from OpenStreetMap
  addTiles() %>%  
  
  #fit bounds around the USA
  fitBounds(-150,35, -70,50) %>%  
  
  #add a map legend
  addLegend(
    title=paste(sep="<br/>","People killed by police",dateRange),
    position = 'bottomright',
    colors = c(unarmedC,armedC, idkC), 
    labels = c("Unarmed", "Armed", "Unknown / non-lethal / vehicle / other"))  %>%
  
  #dynamically add markers for people who were killed
  addCircleMarkers(~long, ~lat, radius = 5.5, stroke=FALSE, 
                   color = ~pal(armed), #color defined above
                   fillOpacity = ifelse(thecounted$armed=="No",0.9,0.3), #make unarmed dots more visible
                   
                   #create pop-up windows with some information for each marker
                   popup = ~ paste(sep="<br/>",name, 
                                   #include race if available
                                   ifelse(race == "B", "Black", 
                                          ifelse(race == "W" , "White",
                                                 ifelse(race =="H", "Hispanic",
                                                        ifelse(race == "A", "Asian",
                                                               ifelse(race == "N", "Native American",
                                                                      ifelse(race == "U", "Race unknown", "")))))),
                                   #include cause of death
                                   ifelse(classification == "Gunshot", "Killed by gunshot",
                                          ifelse(classification == "Death in custody", "Died in custody",
                                                 ifelse(classification == "Other", "",
                                                        ifelse(classification == "Taser", "Killed by taser",
                                                               "Struck by vehicle")))),
                                   #tell us whether they were unarmed or if unknown, else leave blank
                                   #because the categories for being armed are convoluted
                                   ifelse(armed=="No", "Unarmed", 
                                          ifelse(armed=="Unknown", "Unknown if armed", "")))   )