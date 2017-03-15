
  
  
  #download needed packages you don't have 
  wants <- c("magrittr", "leaflet", "jsonlite", "curl", "httr")
  has   <- wants %in% rownames(installed.packages())
  if(any(!has)) install.packages(wants[!has])
  
  #load needed packages
  sapply(wants, require, character.only = TRUE)
  
  #pull data from json file
  thecounted <- fromJSON("https://raw.githubusercontent.com/joshbegley/the-counted/master/skeleton.json")
  thecounted <- thecounted[thecounted$lat != "", ] #remove single entry that has missing lat/lang values, avoid later glitches
  
  marker.col <- grey(0.15)

  #Use the leaflet htmlwidget to create an interactive online visualization of data
leaflet(data = thecounted) %>%   #data from the counted

  #add CartoDB positron tiles (backing map, nice and B and W)
  addProviderTiles("CartoDB.Positron") %>%
  
  #add census-tract level indicator of income
    addTiles(
      urlTemplate = "http://www.justicemap.org/tile/{size}/{variant}/{z}/{x}/{y}.png",
      attribution = '<a href="http://www.justicemap.org/index.php?gsLayer=income&gfLon=-95.3&gfLat=39.6&giZoom=4&">Justice Map</a>',
      options=tileOptions(opacity=.55)
    ) %>%  
  
    #fit bounds around the USA
  fitBounds(-125,25, -67,49) %>%  
  
  #legend for income color codes
    addLegend(
      title="Income",
      position = 'bottomleft',
      colors = c("#67001F","#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"),
      labels = c("$0-28,000", "$28,000-35,000", "$35,000-40,000", "$40,000-45,000", "$45,000-51,000", "$51,000-56,000", "$56,000-64,000", "$64,000-75,000", "$75,000-93,000", "93,000-and more"))  %>%
   
  #legend reminding everyone that people are being killed
    addLegend(
      position = 'bottomright',
      colors = "black",
      opacity=.6,
      labels = "Killed by police")  %>%
    
    #dynamically add markers for people who were killed
        addCircleMarkers(data=thecounted,~long, ~lat, 
                     color=marker.col,
                     radius=7,
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
                                                                 ifelse(classification == "Struck by vehicle", "Struck by vehicle", ""))))))         
                     ) 


    
  
  

  


  