
#download needed packages you don't have 
wants <- c("magrittr", "leaflet", "jsonlite", "curl", "httr", "RCurl", "geonames", "dplyr")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

#load needed packages
sapply(wants, require, character.only = TRUE)

#pull data from json file
thecounted <- fromJSON("https://raw.githubusercontent.com/joshbegley/the-counted/master/skeleton.json")
thecounted <- thecounted[complete.cases(as.numeric(thecounted$lat)),] #remove single entry that has missing lat/lang values, avoid later glitches


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
#rm TBA names
wpost <- wpost[wpost$name != "TK TK",] 
#if age is missing, label unknown
wpost[!complete.cases(wpost$age),]$age <- "unknown"


#initialize empty data.frame to contain location data
#WaPost only gives city-state names, so we have to 
#determine GPS coordinates ourselves
wp.loc <- data.frame()

# loop over WaPost data
for (i in 1:nrow(wpost)){
  
  #get row
  x <- wpost[i,]
  
  #the geoname search function had trouble with the following names so
  #I looked up the coordinates of their deaths manually
  if (x$name %in% c("William Brigham", "Joshua Henry", "Frank W. Wratny", 
                    "James Owens", "Joshua Martino","Kadeem Torres",
                    "Kris Kristl", "Bradley Nelson", "Timothy Lionel Williams", 
                    "Joey J. Bridges", "Shawn M. Igers", "Nana Adomako")){
    
    if(x$name == "William Brigham"){
      res$lng <- "-82.9126001"#give exact coords. GNsearch goes haywire
      res$lat <- "39.8512624"
    }
    if ( x$name == "Joshua Henry" ){
      res$lng <- "-97.4037284"#give exact coords. GNsearch goes haywire
      res$lat <- "32.6970538"
    }
    if (x$name == "Frank W. Wratny"){
      res$lng <- "-80.364483"#give exact coords. GNsearch goes haywire
      res$lat <- "40.9930507"
    }
    if (x$name == "James Owens"){
      res$lng <- "-73.8994051"#give exact coords. GNsearch goes haywire
      res$lat <- "40.6430541"
    }  
    if (x$name == "Joshua Martino"){
      res$lng <- "-73.963353"#give exact coords. GNsearch goes haywire
      res$lat <- " 40.680229"
    }
    if (x$name == "Kadeem Torres"){
      res$lng <- "-73.8801562"#give exact coords. GNsearch goes haywire
      res$lat <- "40.6820163"
    }
    if (x$name == "Kris Kristl"){
      res$lng <- "-88.5227984" #give exact coords. GNsearch goes haywire
      res$lat <- "42.6427308"
    }
    if (x$name == "Bradley Nelson"){
      res$lng <- "-87.2204523"#give exact coords. GNsearch goes haywire
      res$lat <- "35.5436302"
    }
    if (x$name == "Timothy Lionel Williams"){
      res$lng <- "-76.986759"#give exact coords. GNsearch goes haywire
      res$lat <- "38.9015122"
    }
    if (x$name == "Joey J. Bridges"){
      res$lng <- "-81.5432721"#give exact coords. GNsearch goes haywire
      res$lat <- "35.2700086"
    }
    if (x$name == "Shawn M. Igers"){
      res$lng <- "-89.82778"#give exact coords. GNsearch goes haywire
      res$lat <- "45.1206507"
    }
    if (x$name == "Nana Adomako"){
      res$lng <- "-121.9990104"#give exact coords. GNsearch goes haywire
      res$lat <- "37.5384125"
    }
    ### end manual entry of lat-long death locations 
    
    #attach these coordinates to the wp.loc frame
    wp.loc <- rbind(wp.loc, cbind(x,res$lat, res$lng))
    
  }else{
    
    #search for GPS coords of exact city name
    res <- GNsearch(name_equals=paste0(x$city, "&username=demo"), country="US")
    
    if (ncol(res) == 0){ #if we can't get exact match then try searching again for less exact
      res <- GNsearch(name=paste0(x$city, "&username=demo"), country="US")
    }
    
    #restrict results to the state we want
    x.state <- state.name[grep(x$state, state.abb)]
    res <- res[res$adminName1 == x.state,]
    
    #if there are multiple possible entries
    #it can be enough to restrict the entries to those
    #with the word "city" in the fclName column
    city.mention <- grep("city", res$fclName)
    if (length(city.mention) == 1){
      res <- res[city.mention,]
    }
    
    #we can also rm false positives
    #by cutting out entries with population 0
    if (sum(res$population > 0) == 1){ #not super reliable so don't use this 
                                       #as a criteria unless it actually cuts our
                                       #possibilites down to 1
      res <- res[res$population >0, ]
    }
    
    #if after all that we still have multiple possible entries for the city-state..
    if (nrow(res) > 1){

      #Darrion Barnhill killed in Reagan TN 
      #has two slightly different entries
      if (x$name == "Darrion Barnhill"){
        res <- res[1,] 
      }
      
      #This clears up ambiguities in LA entry
      if (x$city == "Los Angeles"){
        res <- res[res$population >100000,]
      }
      
      
      #if after all this we still have multiple possible entries
      #stop script and throw informative error
      if(nrow(res ) > 1){  
        stop(paste("There are multiple cities named", res$name,"in the state:",unique(res$adminName1)))
      }
      
      
      
    }else{
      
      #add entry to main data.frame
      wp.loc <- rbind(wp.loc, cbind(x, res$lat, res$lng))
      
      #check to see if we put multiple people at identical lat-long
      #if so throw an informative warning
      dup <- (wp.loc[,"res$lat"] == res$lat) & (wp.loc[,"res$lng"] == res$long)
      if (TRUE %in% dup){ 
        warning( paste(res$name, res$city, res$state, "is being mapped to the same location as someone else") )
      }
      
      
    }
  }
  
  
  #throw a warning if we failed to come up with a legitimate lat-long for the location
  if (any(!complete.cases(res$lat), !complete.cases(res$lat))){
    warning(paste(i, x$name, "in", x$city, x$state, "lat long not found" ))
  }
  
  
  #initiate progress bar at first loop
  if (i == 1){
  progress <- 0
  cat("|Calculating GPS coordinates for",nrow(wpost),"WaPost killings\n|\n|0%                  100%|\n|==")
  }
  
  #incremement progress bar for each 5% we get through
  if ( (i %% as.integer(nrow(wpost)/20)) == 0){
  cat("=")
  }
  
}

#rename lat-long to match thecounted frame and leaflet
last.name <- length(names(wp.loc))
names(wp.loc)[[last.name - 1]] <- "lat"
names(wp.loc)[[last.name]] <- "long"

#process info on armed / unarmed
wp.loc$armed <- as.character(wp.loc$armed)
wp.loc[wp.loc$armed == "",]$armed <- "undetermined"

#WaPo entries for unambiguously armed person 
wapo.armed <- c("gun","gun and knife","knife", "pole and knife")
#other.cats for ambiguous armaments like vehicles
other.cats <- unique(wp.loc$armed)
other.cats <- other.cats[ !(other.cats %in% c("unarmed","unknown weapon","undetermined", wapo.armed)) ]

#data.frame for armed factors 
facs <- c("unarmed",
          "undetermined", 
          "unknown weapon",
          as.character(other.cats),
          as.character(wapo.armed)
)

#assign colors to armed/unarmed/other for leaflet, specific to WaPo data
wp.pal <- ifelse(wp.loc$armed %in% facs[1], unarmedC,
                    ifelse(wp.loc$armed %in% facs[2:13], idkC,
                           ifelse(wp.loc$armed %in% wapo.armed, armedC, idkC)))



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
  
  
  ## 2017 only group from WaPost
  addCircleMarkers(data=wp.loc, ~long, ~lat, stroke=FALSE, 
                   color = wp.pal, #color defined above
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
                                          ifelse(armed=="undetermined", "Unknown if armed<br/>",
                                                 ifelse(armed=="unknown weapon", "Unknown<br/>", paste("Armed with",armed,"<br/>")))),
                                   #include cause of death
                                   ifelse(classification == "shot", "Killed by gunshot",
                                          ifelse(classification == "shot and Tasered", "Shot and tasered", ""))),
                   group="2017") %>%
  
  #give user the option of selecting years manually
  addLayersControl(
    overlayGroups = c("2015","2016", "2017"),
    options = layersControlOptions(collapsed = FALSE)
  )
  
BROWSE("https://raw.githubusercontent.com/peterphalen/peterphalen.github.io/master/datavisualization/map-police-killings.html")

