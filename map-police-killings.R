#download needed packages you don't have 
wants <- c("magrittr", "leaflet", "jsonlite", "curl", "httr")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

#load needed packages
sapply(wants, require, character.only = TRUE)

#pull data from json file
thecounted <- fromJSON("https://raw.githubusercontent.com/joshbegley/the-counted/master/skeleton.json")
thecounted <- thecounted[thecounted$lat != "", ] #remove single entry that has missing lat/lang values, avoid later glitches


#Color-code for whether the victim was armed
unarmedC <-"#ff0000"  # Red = Unarmed
armedC <-  "#008080"  # Teal = armed
idkC <- "#000000"  # Black = Don't know or ambiguous category like "Non-lethal firearm" or "vehicle"
pal <- colorFactor(c(idkC, rep(armedC,2), unarmedC, rep(idkC,4)), domain= c("Disputed", 
                                                                            "Firearm", 
                                                                            "Knife", 
                                                                            "No", 
                                                                            "Non-lethal firearm",
                                                                            "Other",
                                                                            "Unknown",
                                                                            "Vehicle"))


#Use the leaflet htmlwidget to create an interactive online visualization of data
leaflet(data = thecounted) %>%   #data from the counted
  
  #add default open source map tiles from OpenStreetMap
  addTiles() %>%  
    
  #fit bounds around the USA
  fitBounds(-125,25, -67,49) %>%  
  
  #add a map legend
  addLegend(
    title="People killed by police",
    position = 'bottomright',
    colors = c(unarmedC,armedC, idkC), 
    labels = c("Unarmed", "Armed", "Unknown / non-lethal / vehicle / other"))  %>%
  
  #dynamically add markers for people who were killed
  
  ## 2015 only group (to allow user to select years)
  addCircleMarkers(data=thecounted[as.Date(thecounted$date) <= as.Date("2015/12/31"),],~long, ~lat, stroke=FALSE, 
                   color = ~pal(armed), #color defined above
                   fillOpacity = ~ifelse(armed=="No",0.75,0.3), #make unarmed dots more visible
                   #create pop-up windows with some information for each marker
                   popup = ~ paste(name, "<br/>",
                                   "Age",age,"<br/>",
                                   #include race if available
                                   ifelse(race == "B", "Black", 
                                          ifelse(race == "W" , "White",
                                                 ifelse(race =="H", "Hispanic",
                                                        ifelse(race == "A", "Asian",
                                                               ifelse(race == "N", "Native American",
                                                                      ifelse(race == "U", "Race unknown", "")))))),"<br/>",
                                 
                                   #tell us about whether/how they were armed
                                   ifelse(armed=="No", "Unarmed<br/>", 
                                          ifelse(armed=="Unknown", "Unknown if armed<br/>",
                                                  ifelse(armed=="Vehicle", "Armed with 'vehicle'<br/>",
                                                    ifelse(armed=="Knife", "Had a knife<br/>",
                                                      ifelse(armed=="Disputed", "Disputed if armed<br/>", ""))))),
                   #include cause of death
                   ifelse(classification == "Gunshot", "Killed by gunshot",
                          ifelse(classification == "Death in custody", "Died in custody",
                                 ifelse(classification == "Other", "",
                                        ifelse(classification == "Taser", "Killed by taser",
                                               ifelse(classification == "Struck by vehicle", "Struck by vehicle", "")))))),              
                   group="2015")  %>%
  
  ## 2016 only group
  addCircleMarkers(data=thecounted[as.Date(thecounted$date) > as.Date("2015/12/31"),], ~long, ~lat, stroke=FALSE, 
                   color = ~pal(armed), #color defined above
                   fillOpacity = ~ifelse(armed=="No",0.75,0.3), #make unarmed dots more visible
                   #create pop-up windows with some information for each marker
                   popup = ~ paste(name, "<br/>",
                                   "Age",age,"<br/>",
                                   #include race if available
                                   ifelse(race == "B", "Black", 
                                          ifelse(race == "W" , "White",
                                                 ifelse(race =="H", "Hispanic",
                                                        ifelse(race == "A", "Asian",
                                                               ifelse(race == "N", "Native American",
                                                                      ifelse(race == "U", "Race unknown", "")))))),"<br/>",
                                   
                                   #tell us about whether/how they were armed
                                   ifelse(armed=="No", "Unarmed<br/>", 
                                          ifelse(armed=="Unknown", "Unknown if armed<br/>",
                                                 ifelse(armed=="Vehicle", "Armed with 'vehicle'<br/>",
                                                        ifelse(armed=="Knife", "Had a knife<br/>",
                                                               ifelse(armed=="Disputed", "Disputed if armed<br/>", ""))))),
                                   #include cause of death
                                   ifelse(classification == "Gunshot", "Killed by gunshot",
                                          ifelse(classification == "Death in custody", "Died in custody",
                                                 ifelse(classification == "Other", "",
                                                        ifelse(classification == "Taser", "Killed by taser",
                                                               ifelse(classification == "Struck by vehicle", "Struck by vehicle", "")))))),
                   group="2016") %>%
  
  #give user the option of selecting years manually
  addLayersControl(
    overlayGroups = c("2015","2016"),
    options = layersControlOptions(collapsed = FALSE)
  )
  
BROWSE("https://raw.githubusercontent.com/peterphalen/peterphalen.github.io/master/datavisualization/map-police-killings.html")

