



#download needed packages you don't have 
wants <- c("magrittr", "leaflet", "jsonlite", "dplyr", "httr", "dygraphs", "xts")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

#load needed packages
sapply(wants, require, character.only = TRUE)


#pull data from json file embedded in the Guardian's The Counted website: http://www.theguardian.com/thecounted
thecounted_raw <- fromJSON("https://raw.githubusercontent.com/joshbegley/the-counted/master/skeleton.json")
thecounted <- thecounted_raw[complete.cases(as.numeric(thecounted_raw$lat)),]  #remove entries with missing lat/lang values, avoid later glitches

thecounted$date <- as.Date(thecounted$date) 

t.series <- table(factor(format(thecounted$date,"%m/%y")))
t.series <- as.data.frame(t.series)
t.series$date <- as.yearmon(t.series$Var1, format="%m/%y")
t.series <- t.series[order(t.series$date),]
t.series <- t.series[-1]

full.ts <- xts(t.series$Freq, order.by = t.series$date)
colnames(full.ts) <- "Killings"
full.ts <- full.ts[-nrow(full.ts),] #drop most recent month

dygraph( full.ts, main = "Police killings over time") %>% 
  dyAxis("y", label="killings per month", valueRange=c(0,140)) %>% 
  dyRangeSelector()









