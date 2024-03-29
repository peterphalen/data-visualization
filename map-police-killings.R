
#download needed packages you don't have 
wants <- c("magrittr", "leaflet", "jsonlite", "curl", "httr", "lubridate", "RCurl", "geonames", "dplyr", "XML", "htmlwidgets", "shiny")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

#load needed packages
sapply(wants, require, character.only = TRUE)

#pull data from json file
thecounted <- fromJSON("https://raw.githubusercontent.com/joshbegley/the-counted/master/skeleton.json")
thecounted <- thecounted[complete.cases(as.numeric(thecounted$lat)),] #remove single entry that has missing lat/lang values, avoid later glitches
thecounted$lat <- as.numeric(thecounted$lat)
thecounted$long <- as.numeric(thecounted$long)
thecounted$date <- as.Date(thecounted$date)
  
#Color-code thecounted codes for whether the victim was armed
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


#The Guardian stopped keeping track of police killings in 2017
#use WaPost data for post-2017 killings


#download csv
wpost <- getURL("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv")
wpost <- read.csv(textConnection(wpost))

#restrict dataset to 2017-present
wpost$date <- as.POSIXct(wpost$date)
wpost <- wpost[wpost$date > "2016-12-31 EST",]

#rename variable to match thecounted scheme
wpost <- rename(wpost, classification = manner_of_death)
wpost <- rename(wpost, long = longitude)
wpost <- rename(wpost, lat = latitude)

wpost <- wpost[complete.cases(as.numeric(wpost$lat)),] #remove missing lat/lang
wpost <- wpost[complete.cases(as.numeric(wpost$long)),] 

#rm TBA names
wpost <- wpost[wpost$name != "TK TK",] 

if (sum(!complete.cases(wpost$age)) > 0){
  #if age is missing, label unknown
  wpost[!complete.cases(wpost$age),]$age <- "unknown"
}

wpost$date <- as.Date(wpost$date, format="yyyy-mm-dd")




#process some basic info on armed / unarmed for color coding
wpost$armed <- as.character(wpost$armed)
if ( ( sum(wpost$armed == "") > 0 ) ){
  wpost[wpost$armed == "",]$armed <- "undetermined"
}

#all wapo cats
(cats <- unique(wpost$armed))
#WaPo entries for people with guns
wapo.gun <- cats[grepl("gun", cats)]
non.lethal.or.questionable <- c("toy weapon", "undetermined", "vehicle","beer bottle")

other.cats <- cats[ !(cats %in% c("unarmed",non.lethal.or.questionable, wapo.gun)) ]


#Color-code thecounted codes for whether the victim was armed
unarmedC <-"#ff0000"  # Red = Unarmed
armedC <-  "#008080"  # Teal = armed
idkC <- "#000000"  # Black = Don't know or ambiguous category like "Non-lethal firearm" or "vehicle"

#assign colors to armed/unarmed/other for leaflet, specific to WaPo data
all.cats <- c("unarmed", non.lethal.or.questionable, wapo.gun, other.cats)


wpost$col <- ifelse(wpost$armed %in% "unarmed", unarmedC,
                 ifelse(wpost$armed %in% non.lethal.or.questionable, idkC,
                        ifelse(wpost$armed %in% c(wapo.gun, other.cats), armedC, idkC)))


#Use the leaflet htmlwidget to create an interactive online visualization of data
map <-  leaflet(data = thecounted) %>%   #data from the counted
  
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
  addCircleMarkers(data=thecounted[year(thecounted$date) == 2015,],~long, ~lat, stroke=FALSE, 
                   color = ~pal(armed), #color defined above
                   fillOpacity = ~ifelse(armed=="No",0.75,0.4), #make unarmed dots more visible
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
  addCircleMarkers(data=thecounted[year(thecounted$date) == 2016,], ~long, ~lat, stroke=FALSE, 
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
                   group="2016") 

### now we add the other years from WaPost

for (i in 2017:max(year(wpost$date))){
map <- map %>%
  ## 2017 only group from WaPost
  addCircleMarkers(data=wpost[year(wpost$date) == i,], ~long, ~lat, stroke=FALSE, 
                   color = wpost[year(wpost$date) == i,]$col, #color defined above
                   fillOpacity = ~ifelse(armed=="unarmed",0.75,0.3), #make unarmed dots more visible
                   #create pop-up windows with some information for each marker
                   popup = ~ paste(name, "<br/>",
                                   "Age",age,"<br/>",
                                   #include race if available
                                   ifelse(race == "B", "Black<br/>", 
                                          ifelse(race == "W" , "White<br/>",
                                                 ifelse(race =="H", "Hispanic<br/>",
                                                        ifelse(race == "A", "Asian<br/>",
                                                               ifelse(race == "N", "Native American<br/>",
                                                                      ifelse(race %in% c("U", ""), "Race unknown<br/>", "")))))),
                                   
                                   #tell us about whether/how they were armed
                                   ifelse(armed=="unarmed", "Unarmed<br/>", 
                                          ifelse(armed %in% c("undetermined", "Unknown", "unknown weapon"), "Unknown if armed<br/>",
                                                 ifelse(armed == "vehicle", "Armed with 'vehicle'<br/>",paste0("Had ",armed,"<br/>")))),
                                   
                                   #include cause of death
                                   ifelse(classification == "shot", "Killed by gunshot",
                                          classification)),
                   group=as.character(i)) 
}

map <- map %>%  
  #give user the option of selecting years manually
  addLayersControl(
    overlayGroups = 2015:max(year(wpost$date)),
    options = layersControlOptions(collapsed = FALSE)
  )

sav.file <- "/Users/peterphalen/Documents/peterphalen.github.io/datavisualization/map-police-killings.html"
saveWidget(map, file=sav.file, selfcontained = F)

html.file <- includeHTML(sav.file)

head.loc <- regexpr("<head>", html.file)
html.map <- HTML(substr(html.file, 1,head.loc+5), 
                 "<title>Mapping police killings - armed versus unarmed</title>
     <meta name=\"description\" content=\"Coordinates of police killings across the United States of America, colored according to whether the victim was armed or unarmed. Mouse-over to discover facts about the killing, such as the age, name, and race of the victim, and the cause of death.\">
    <meta charset=\"utf-8\"/>
    <meta name=\"author\" content=\"Peter Phalen\"/>
    <link rel=\"icon\" type=\"image/png\" href=\"../images/icon-code-fork-16x16.png\" sizes=\"16x16\">
    <link rel=\"icon\" type=\"image/png\" href=\"../images/icon-code-fork-32x32.png\" sizes=\"32x32\">
    <link rel=\"icon\" type=\"image/x-icon\" href=\"../images/favicon.ico\">
    <link rel=\"shortcut icon\" type=\"image/x-icon\" href=\"../images/favicon.ico\"/>
    <meta property=\"og:url\" content=\"https://peterphalen.github.io/datavisualization/map-police-killings.html\"/>
    <meta property=\"og:title\" content=\"Mapping police killings\"/>
    <meta property=\"og:description\" content=\"Interactive map of police killings in the United States. Generated using data from The Guardian's The Counted project.\"/>
    <meta property=\"og:image\" content=\"https://peterphalen.github.io/images/police-killings-map-img.png\"/>
    <meta property=\"og:type\" content=\"website\"/>
    <meta property=\"twitter:card\" content=\"summary_large_image\"/>
    <meta property=\"twitter:creator\" content=\"@peterphalen\"/>
    <meta property=\"twitter:description\" content=\"Interactive map of police killings in the United States. Generated using data from The Guardian's The Counted project.\"/>
    <meta name=\"twitter:image\" content=\"https://peterphalen.github.io/images/police-killings-map-img.png\"/>",
                 substr(html.file, (head.loc+6),nchar(html.file)))


write.table(as.character(html.map), file=sav.file, quote = FALSE, col.names = FALSE, row.names = FALSE)





# ***** NOW THE POVERTY MAP


# marker color dark grey
marker.col <- grey(0.15)


#Use the leaflet htmlwidget to create an interactive online visualization of data
map.pov <- leaflet(data = thecounted) %>%   #data from the counted
  
  #add CartoDB positron tiles (backing map, nice and B and W)
  addProviderTiles("CartoDB.Positron") %>%
  
  #add census-tract level indicator of income
  addProviderTiles("JusticeMap.income",
    options=tileOptions(opacity=.55, size="tract")
  ) %>%  
  
  #fit bounds around the USA
  setView(-75.165222, 39.952583, zoom = 9) %>%  
  
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
  ) %>%
  
  ## 2017+ group FROM WAPOST
  addCircleMarkers(data=wpost, ~long, ~lat,
                   color = marker.col, #color defined above
                   #create pop-up windows with some information for each marker
                   popup = ~ paste(name, "<br/>",
                                   "Age",age,"<br/>",
                                   #include race if available
                                   ifelse(race == "B", "Black<br/>", 
                                          ifelse(race == "W" , "White<br/>",
                                                 ifelse(race =="H", "Hispanic<br/>",
                                                        ifelse(race == "A", "Asian<br/>",
                                                               ifelse(race == "N", "Native American<br/>",
                                                                      ifelse(race %in% c("U", ""), "Race unknown<br/>", "")))))),
                                   
                                   #tell us about whether/how they were armed
                                   ifelse(armed=="unarmed", "Unarmed<br/>", 
                                          ifelse(armed %in% c("undetermined", "Unknown", "unknown weapon"), "Unknown if armed<br/>",
                                                 ifelse(armed == "vehicle", "Armed with 'vehicle'<br/>",paste0("Had ",armed,"<br/>")))),
                                   
                                   #include cause of death
                                   ifelse(classification == "shot", "Killed by gunshot",
                                          classification))
  )



sav.file.pov <- "/Users/peterphalen/Documents/peterphalen.github.io/datavisualization/poverty-police-killings.html"
saveWidget(map.pov, file=sav.file.pov)

html.pov <- includeHTML(sav.file.pov)

head.loc <- regexpr("<head>", html.pov)
html.pov <- HTML(substr(html.pov, 1,head.loc+5), 
                 "<title>Poverty and police killings</title>
                 <meta name=\"description\" content=\"Coordinates of police killings across the United States of America, with map colored according to the average income of the census tract. In populated areas such as major cities, it is clear that poorer neighborhoods have greater rates of police killings.\">
                 <meta charset=\"utf-8\"/>
                 <meta name=\"author\" content=\"Peter Phalen\"/>
                  <link href=\"http://peterphalen.github.io/datavisualization/poverty-police-killings\" rel=\"canonical\">

                  <link rel=\"icon\" type=\"image/png\" href=\"../images/icon-code-fork-16x16.png\" sizes=\"16x16\">
                  <link rel=\"icon\" type=\"image/png\" href=\"../images/icon-code-fork-32x32.png\" sizes=\"32x32\">
                  <link rel=\"icon\" type=\"image/x-icon\" href=\"../images/favicon.ico\">
                  <link rel=\"shortcut icon\" type=\"image/x-icon\" href=\"../images/favicon.ico\"/>
                  <meta property=\"og:url\" content=\"https://peterphalen.github.io/datavisualization/poverty-police-killings.html\"/>
                  <meta property=\"og:title\" content=\"Poverty and police killings\"/>
                  <meta property=\"og:description\" content=\"Mapping income against killings by police in the United States. Generated using data from Justice Map and The Guardian's The Counted project.\"/>
                  <meta property=\"og:image\" content=\"https://peterphalen.github.io/images/income-police-killings.png\"/>
                  <meta property=\"twitter:card\" content=\"summary_large_image\"/>
                  <meta property=\"twitter:creator\" content=\"@peterphalen\"/>
                  <meta property=\"twitter:description\" content=\"Mapping income against killings by police in the United States. Generated using data from Justice Map and The Guardian's The Counted project.\"/>
                  <meta name=\"twitter:image\" content=\"https://peterphalen.github.io/images/income-police-killings.png\"/>",
                 substr(html.pov, (head.loc+6),nchar(html.pov)))


write.table(as.character(html.pov), file=sav.file.pov, quote = FALSE, col.names = FALSE, row.names = FALSE)



