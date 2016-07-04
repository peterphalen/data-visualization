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

#Color-code for whether the victim was armed
#  Red = Unarmed
unarmedC <-"#ff0000"
#  Teal = armed
armedC <-  "#008080"
#  Black = Don't know or ambiguous category like "Non-lethal firearm" or "vehicle"
idkC <- "#000000"
pal <- colorFactor(c(idkC, rep(armedC,2), unarmedC, rep(idkC,4)), domain= c("Disputed", 
                                                                      "Firearm", 
                                                                      "Knife", 
                                                                      "No", 
                                                                      "Non-lethal firearm",
                                                                      "Other",
                                                                      "Unknown",
                                                                      "Vehicle"))

# automatically set date range for pulled data
today <- Sys.Date()
today <- format(today, format="%b %d %Y")
dateRange <- paste0("(Jan 01 2015"," - ", today,")")

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
  
    addCircleMarkers(data=subset(thecounted, race=="B"), ~long, ~lat, stroke=FALSE, 
                   color = ~pal(armed), #color defined above
                   fillOpacity = ifelse(thecounted$armed=="No",0.85,0.35), #make unarmed dots more visible
                   group = "Black",
                   #create pop-up windows with some information for each marker
                  
                   popup = ~ paste(sep="<br/>",name, 
                                   "Black",
                                   #include cause of death
                                   ifelse(classification == "Gunshot", "Killed by gunshot",
                                          ifelse(classification == "Death in custody", "Died in custody",
                                                 ifelse(classification == "Other", "",
                                                        ifelse(classification == "Taser", "Killed by taser",
                                                               ifelse(classification == "Struck by vehicle", "Struck by vehicle", ""))))),
                                   #tell us whether they were unarmed or if unknown, else leave blank
                                   #because the categories for being armed are convoluted
                                   ifelse(armed=="No", "Unarmed", 
                                          ifelse(armed=="Unknown", "Unknown if armed", 
                                                 ifelse(armed=="Disputed", "Disputed if armed", ""))) ))   %>%
                   
                   #dynamically add markers for people who were killed
                   addCircleMarkers(data=subset(thecounted, race=="W"), ~long, ~lat, stroke=FALSE, 
                                    color = ~pal(armed), #color defined above
                                    fillOpacity = ifelse(thecounted$armed=="No",0.85,0.35), #make unarmed dots more visible
                                    group = "White",
                                    #create pop-up windows with some information for each marker
                                    popup = ~ paste(sep="<br/>",name, 
                                                    "White",
                                                    #include cause of death
                                                    ifelse(classification == "Gunshot", "Killed by gunshot",
                                                           ifelse(classification == "Death in custody", "Died in custody",
                                                                  ifelse(classification == "Other", "",
                                                                         ifelse(classification == "Taser", "Killed by taser",
                                                                                ifelse(classification == "Struck by vehicle", "Struck by vehicle", ""))))),
                                                    #tell us whether they were unarmed or if unknown, else leave blank
                                                    #because the categories for being armed are convoluted
                                                    ifelse(armed=="No", "Unarmed", 
                                                           ifelse(armed=="Unknown", "Unknown if armed", 
                                                                  ifelse(armed=="Disputed", "Disputed if armed", ""))) ))  %>%
                                      
                                      #dynamically add markers for people who were killed
                                      addCircleMarkers(data=subset(thecounted, race=="H"), ~long, ~lat, stroke=FALSE, 
                                                       color = ~pal(armed), #color defined above
                                                       fillOpacity = ifelse(thecounted$armed=="No",0.85,0.35), #make unarmed dots more visible
                                                       group = "Hispanic",
                                                       #create pop-up windows with some information for each marker
                                                       popup = ~ paste(sep="<br/>",name, 
                                                                       "Hispanic",
                                                                       #include cause of death
                                                                       ifelse(classification == "Gunshot", "Killed by gunshot",
                                                                              ifelse(classification == "Death in custody", "Died in custody",
                                                                                     ifelse(classification == "Other", "",
                                                                                            ifelse(classification == "Taser", "Killed by taser",
                                                                                                   ifelse(classification == "Struck by vehicle", "Struck by vehicle", ""))))),
                                                                       #tell us whether they were unarmed or if unknown, else leave blank
                                                                       #because the categories for being armed are convoluted
                                                                       ifelse(armed=="No", "Unarmed", 
                                                                              ifelse(armed=="Unknown", "Unknown if armed", 
                                                                                     ifelse(armed=="Disputed", "Disputed if armed", ""))) ))  %>% 
                                                         
                                                         #dynamically add markers for people who were killed
                                                         addCircleMarkers(data=subset(thecounted, race=="N"), ~long, ~lat, stroke=FALSE, 
                                                                          color = ~pal(armed), #color defined above
                                                                          fillOpacity = ifelse(thecounted$armed=="No",0.85,0.35), #make unarmed dots more visible
                                                                          group = "Native American",
                                                                          #create pop-up windows with some information for each marker
                                                                          popup = ~ paste(sep="<br/>",name, 
                                                                                          "Native American",
                                                                                          #include cause of death
                                                                                          ifelse(classification == "Gunshot", "Killed by gunshot",
                                                                                                 ifelse(classification == "Death in custody", "Died in custody",
                                                                                                        ifelse(classification == "Other", "",
                                                                                                               ifelse(classification == "Taser", "Killed by taser",
                                                                                                                      ifelse(classification == "Struck by vehicle", "Struck by vehicle", ""))))),
                                                                                          #tell us whether they were unarmed or if unknown, else leave blank
                                                                                          #because the categories for being armed are convoluted
                                                                                          ifelse(armed=="No", "Unarmed", 
                                                                                                 ifelse(armed=="Unknown", "Unknown if armed", 
                                                                                                        ifelse(armed=="Disputed", "Disputed if armed", ""))) ))  %>%
                                                                            
                                                                            #dynamically add markers for people who were killed
                                                                            addCircleMarkers(data=subset(thecounted, race=="A"), ~long, ~lat, stroke=FALSE, 
                                                                                             color = ~pal(armed), #color defined above
                                                                                             fillOpacity = ifelse(thecounted$armed=="No",0.85,0.35), #make unarmed dots more visible
                                                                                             group = "Asian",
                                                                                             #create pop-up windows with some information for each marker
                                                                                             popup = ~ paste(sep="<br/>",name, 
                                                                                                             "Asian",
                                                                                                             #include cause of death
                                                                                                             ifelse(classification == "Gunshot", "Killed by gunshot",
                                                                                                                    ifelse(classification == "Death in custody", "Died in custody",
                                                                                                                           ifelse(classification == "Other", "",
                                                                                                                                  ifelse(classification == "Taser", "Killed by taser",
                                                                                                                                         ifelse(classification == "Struck by vehicle", "Struck by vehicle", ""))))),
                                                                                                             #tell us whether they were unarmed or if unknown, else leave blank
                                                                                                             #because the categories for being armed are convoluted
                                                                                                             ifelse(armed=="No", "Unarmed", 
                                                                                                                    ifelse(armed=="Unknown", "Unknown if armed", 
                                                                                                                           ifelse(armed=="Disputed", "Disputed if armed", ""))) ))  %>% 
                                                                                             
                                                                                               #dynamically add markers for people who were killed
                                                                                               addCircleMarkers(data=subset(thecounted, (race=="O" | race=="U"), ~long, ~lat, stroke=FALSE, 
                                                                                                                color = ~pal(armed), #color defined above
                                                                                                                fillOpacity = ifelse(thecounted$armed=="No",0.85,0.35), #make unarmed dots more visible
                                                                                                                group = "Other/unknown",
                                                                                                                #create pop-up windows with some information for each marker
                                                                                                                popup = ~ paste(sep="<br/>",name, 
                                                                                                                                "Other/unknown",
                                                                                                                                #include cause of death
                                                                                                                                ifelse(classification == "Gunshot", "Killed by gunshot",
                                                                                                                                       ifelse(classification == "Death in custody", "Died in custody",
                                                                                                                                              ifelse(classification == "Other", "",
                                                                                                                                                     ifelse(classification == "Taser", "Killed by taser",
                                                                                                                                                            ifelse(classification == "Struck by vehicle", "Struck by vehicle", ""))))),
                                                                                                                                #tell us whether they were unarmed or if unknown, else leave blank
                                                                                                                                #because the categories for being armed are convoluted
                                                                                                                                ifelse(armed=="No", "Unarmed", 
                                                                                                                                       ifelse(armed=="Unknown", "Unknown if armed", 
                                                                                                                                              ifelse(armed=="Disputed", "Disputed if armed", ""))) ))  %>% 
                                                                                                                  # Layers control
                                                                                                                  addLayersControl(
                                                                                                                    overlayGroups = c("White", "Black", "Hispanic", "Native American","Asian", "Other/unknown"),
                                                                                                                    position = "topright",
                                                                                                                    options = layersControlOptions(collapsed = FALSE)
                                                                                                                  )
                   
                   
                   
