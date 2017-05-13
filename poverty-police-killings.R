
  

#download needed packages you don't have 
wants <- c("magrittr", "leaflet", "jsonlite", "curl", "httr", "RCurl", "geonames", "dplyr")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
  
  #load needed packages
  sapply(wants, require, character.only = TRUE)
  
  #pull data from json file
  thecounted <- fromJSON("https://raw.githubusercontent.com/joshbegley/the-counted/master/skeleton.json")
  thecounted <- thecounted[complete.cases(as.numeric(thecounted$lat)),]  #remove entries with missing lat/lang values, avoid later glitches
  
  
  
  
  
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
  #rm TBA names
  wpost <- wpost[wpost$name != "TK TK",] 
  #if age is missing, label unknown
  wpost[!complete.cases(wpost$age),]$age <- "unknown"
  
  
source("process-wapost-killings.R") # replace with process-wapost-killings.R path
  output <- processWaPoKillings(data = wpost, username = "peterphalen")
  wp.loc <- output$processedData

  wp.loc <- rename(wp.loc, long = lng)
  
  
  #marker color dark grey
  marker.col <- grey(0.15)
  
  
  #Use the leaflet htmlwidget to create an interactive online visualization of data
map.pov <- leaflet(data = thecounted) %>%   #data from the counted

  #add CartoDB positron tiles (backing map, nice and B and W)
  addProviderTiles("CartoDB.Positron") %>%
  
  #add census-tract level indicator of income
    addTiles(
      urlTemplate = "http://www.justicemap.org/tile/tract/income/{z}/{x}/{y}.png",
      attribution = '<a href="http://www.justicemap.org/index.php?gsLayer=income&gfLon=-95.3&gfLat=39.6&giZoom=4&">Justice Map</a>',
      options=tileOptions(opacity=.55)
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

## 2017 only group FROM WAPOST
addCircleMarkers(data=wp.loc, ~long, ~lat,
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
                                        ifelse(classification == "shot and Tasered", "Shot and tasered", "")))
)
  


sav.file.pov <- "/Users/PeterPhalen/Desktop/CodingProjects/personalwebsite/peterphalen.github.io/datavisualization/poverty-police-killings.html"
saveWidget(map.pov, file=sav.file.pov)

html.pov <- includeHTML(sav.file.pov)

head.loc <- regexpr("<head>", html.pov)
html.pov <- HTML(substr(html.pov, 1,head.loc+5), 
                  "<title>Poverty and police killings</title>
         <meta name=\"description\" content=\"Coordinates of police killings across the United States of America, with map colored according to the average income of the census tract. In populated areas such as major cities, it is clear that poorer neighborhoods have greater rates of police killings.\">
                  <meta charset=\"utf-8\"/>
                  <meta name=\"author\" content=\"Peter Phalen\"/>
                  <link rel=\"icon\" type=\"image/png\" href=\"../images/icon-code-fork-16x16.png\" sizes=\"16x16\">
                  <link rel=\"icon\" type=\"image/png\" href=\"../images/icon-code-fork-32x32.png\" sizes=\"32x32\">
                  <link rel=\"icon\" type=\"image/x-icon\" href=\"../images/favicon.ico\">
                  <link rel=\"shortcut icon\" type=\"image/x-icon\" href=\"../images/favicon.ico\"/>
                  <meta property=\"og:url\" content=\"https://www.peterphalen.com/datavisualization/poverty-police-killings.html\"/>
                  <meta property=\"og:title\" content=\"Poverty and police killings\"/>
                  <meta property=\"og:description\" content=\"Mapping income against killings by police in the United States. Generated using data from Justice Map and The Guardian's The Counted project.\"/>
                  <meta property=\"og:image\" content=\"https://www.peterphalen.com/images/income-police-killings.png\"/>
                  <meta property=\"twitter:card\" content=\"summary_large_image\"/>
                  <meta property=\"twitter:creator\" content=\"@peterphalen\"/>
                  <meta property=\"twitter:description\" content=\"Mapping income against killings by police in the United States. Generated using data from Justice Map and The Guardian's The Counted project.\"/>
                  <meta name=\"twitter:image\" content=\"https://www.peterphalen.com/images/income-police-killings.png\"/>",
                  substr(html.pov, (head.loc+6),nchar(html.pov)))


write.table(as.character(html.pov), file=sav.file.pov, quote = FALSE, col.names = FALSE, row.names = FALSE)




  