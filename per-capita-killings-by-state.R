
##########
#
# This code generates a graph of police killings of black people per capita, by state. Mouse-over bars for detail.
# Output viewable here: https://www.peterphalen.com/datavisualization/police-killings-graph-viz.html
#
##########

#download needed packages you don't have 
wants <- c("magrittr", "leaflet", "jsonlite", "plyr", "httr")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

#load needed packages
sapply(wants, require, character.only = TRUE)

devtools::install_github("timelyportfolio/rcdimple") #for dataviz
require(rcdimple)

#pull data from json file embedded in the Guardian's The Counted website: http://www.theguardian.com/thecounted
thecounted_raw <- fromJSON("https://raw.githubusercontent.com/joshbegley/the-counted/master/skeleton.json")
thecounted <- thecounted_raw[complete.cases(as.numeric(thecounted_raw$lat)),]  #remove entries with missing lat/lang values, avoid later glitches

# make race a factor
thecounted$race <- as.factor(thecounted$race)

thecounted$state <- revalue(thecounted$state, c("AL"="Alabama", #rename all the counted state codes to match the census
                                                "AK"="Alaska", 
                                                "AZ"="Arizona", 
                                                "AR"="Arkansas", 
                                                "CA"="California",
                                                "CO"="Colorado",
                                                "CT"="Connecticut",
                                                "DE"="Delaware",
                                                "DC"="District of Columbia",
                                                "FL"="Florida",
                                                "GA"="Georgia",
                                                "HI"="Hawaii",
                                                "ID"="Idaho",
                                                "IL"="Illinois",
                                                "IN"="Indiana",
                                                "IA"="Iowa",
                                                "KS"="Kansas",
                                                "KY"="Kentucky",
                                                "LA"="Louisiana",
                                                "ME"="Maine",
                                                "MD"="Maryland",
                                                "MA"="Massachusetts",
                                                "MI"="Michigan",
                                                "MN"="Minnesota",
                                                "MS"="Mississippi",
                                                "MO"="Missouri",
                                                "MT"="Montana",
                                                "NE"="Nebraska",
                                                "NV"="Nevada",
                                                "NH"="New Hampshire",
                                                "NJ"="New Jersey",
                                                "NM"="New Mexico",
                                                "NY"="New York",
                                                "NC"="North Carolina",
                                                "ND"="North Dakota",
                                                "OH"="Ohio",
                                                "OK"="Oklahoma",
                                                "OR"="Oregon",
                                                "PA"="Pennsylvania",
                                                "RI"="Rhode Island",
                                                "SC"="South Carolina",
                                                "SD"="South Dakota",
                                                "TN"="Tennessee",
                                                "TX"="Texas",
                                                "UT"="Utah",
                                                "VT"="Vermont",
                                                "VA"="Virginia",
                                                "WA"="Washington",
                                                "WV"="West Virginia",
                                                "WI"="Wisconsin",
                                                "WY"="Wyoming"
) )


# read census data downloaded from http://factfinder.census.gov/
census_raw <- read.csv("/Users/PeterPhalen/Dropbox/Manuscripts/WFYI visualizations/PEP_2015_PEPSR5H/PEP_2015_PEPSR5H_with_ann.csv")

## CLEAN CENSUS DATA
# 2015 estimate only
census <- subset(census_raw, Year.id == "est72015") 
# both sexes 
census <- subset(census, Sex.id == "totsex") 
# non-hispanic only 
census <- subset(census, Hisp.id == "nhisp") 

# rename location variable to something that doesn't sound stupid
census["state"] <- census["GEO.display.label"] 

# delete the silly variable name
census["GEO.display.label"] <- NULL 

#create empty dataframe for black people killed per one hundred thousand
Bper <- data.frame(state=0, Per100k=0) 

#total number of black people killed in the USA, as per The Counted
totalBlackPeopleKilled <- nrow(thecounted[(thecounted$race == "B"),])


# set up "per x" value for modification, default to 100 thousand
per <- 100000

# total black people killed per 100k
Bper[1, ] <- c("United States", (totalBlackPeopleKilled / census[census$state == "United States","bac"]) * per )

#states-only data.frame
census_statesonly <- census[census$state != "United States",]  

# loop for each state 
for (i in unique(thecounted$state)[! "United States" %in% unique(thecounted$state)]) {  
  # number of black people killed in state i
  blackPeopleKilled <-  nrow(thecounted[(thecounted$race == "B") & (thecounted$state == i),]) 
  # if state has killed *any* black people, add a row to column blacks-killed-per-x for each state
  if ( blackPeopleKilled != 0 ){ 
    #number of black people killed per x, given census data on population by state
    Bper <- rbind(Bper, c(i, (blackPeopleKilled / census_statesonly[census_statesonly$state == i,"bac"] ) * per )) 
  # but if the state has killed no black people just give it a zero
  } else {
    Bper <- rbind( Bper, c(i, 0 ))  
  }
}

#order values by decreasing
Bper <- Bper[order(as.numeric(Bper$Per100k), decreasing=TRUE),] 

Bper["State"] <- Bper["state"] #rename "state" to capital letters

#create html bar graph of number of black people killed per capita, by state
#mouse-over to get the exact value
Bper %>% 
    dimple(
    y = "State", 
    x="Per100k",
    height= 700,
    width= 960,
    type = "bar") %>%
  xAxis(type = "addMeasureAxis", title="Black people killed by police (per 100,000)") %>%
  yAxis(type = "addCategoryAxis", orderRule="Per100k")


BROWSE("https://raw.githubusercontent.com/peterphalen/peterphalen.github.io/master/datavisualization/police-killings-graph-viz.html")

