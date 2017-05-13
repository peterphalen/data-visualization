
processWaPoKillings <- function(data, #wapo data
                                username #register a username or you'll run out of download credits
                                ){
wpost <- data
#initialize empty data.frame to contain location data
#WaPost only gives city-state names, so we have to 
#determine GPS coordinates ourselves
wp.loc <- data.frame(matrix(ncol=(ncol(wpost) + 2)))[complete.cases(wpost),]
names(wp.loc) <- c(names(wpost), "lat", "lng")

#this feeds username to site in their format
user.suffix <- paste0("&username=",username)

# loop over WaPost data
for (i in 1:nrow(wpost)){
  
  #get row
  x <- wpost[i,]
  
  
  #the geoname search function had trouble with the following names so
  #I looked up the coordinates of their deaths manually
  exact.coords <- c("William Brigham", "Joshua Henry", "Frank W. Wratny", 
    "James Owens", "Joshua Martino","Kadeem Torres",
    "Kris Kristl", "Bradley Nelson", "Timothy Lionel Williams", 
    "Joey J. Bridges", "Shawn M. Igers", "Nana Adomako", 
    "JR Williams", "Herbert Johnson", "Chad Robertson", "Michelle Robey",
    "Ambroshia Fagre", "Kadhar Bailey", "Sergio Reyes","Jahlire Nicholson",
    "Davion Henderson", "Don Clark", "Morgan London Rankins",
    "Scott Laurance Gilpin", "Josue Javier Diaz", "Iaroslav Mosiiuk",
    "Daniel Scott Giberson", "William Dwayne Darby",
    "Rashad Opher", "Daniel D. Rogers", "James E. Lewis",
    "John McLaughlin", "Darrion Barnhill", "James Stephen McMullen",
    "Johnnie J. Harris", "Alonzo E. Ashley", "Michael Lee Morris",
    "Jimmie Patton", "Earl Riley", "Juan Carlos Cuadra", "Willard Eugene Scott",
    "Kenneth Lee Bailey", "Rafael Navarro-Garcia", "Steven Valenzuela",
    "Jamake Cason Thomas", "Christopher Apostolos", "Vincent Palma",
    "Peter Torres", "Steve Salgado", "Ronnie Johnson", "William Tucker Mathis",
    "Jesse Enjaian", "Jesus Alberto Geney", "Spencer Herckt", "Refugio Alvarez",
   "Quanice Derrick Hayes", "Lorenzo Antoine Cruz", "Brian Carreno",
   "Dexter Martin Dumarce", "Justin Burton", "Gerardo Vasquez",
   "Jeremy Lopez-Robledo", "Gilbert Zambronio-Lovato", "Rashad Daquan Opher",
   "Curtis Jamal Deal", "Todd Munson", "Elena \"Ebbie\" Mondragon",
   "Christopher Redding", "Jonathan David Sper", "Raynard Burton", "Thomas Scott Henry",
   "Mario Simoes", "Bradley James Sheets", "Destiny A. Moneyhun", "Fred Barragan",
   "Michael Rogers", "Francisco Valdez", "Michael Cisneros", "Marlon Joel Rodas-Sanchez",
   "Austin Tyler Macon", "Genevive Dawes", "Daniel Darnell Stroughter", "Joseph Tamayo",
   "Alejandro Valencia Mendez", "William Stokes", "Mi'Chance Dunlap-Gittens",
   "Benjamin Ailstock", "William Brigham", "Antonio Arturo Perez Garcia",
   "Rogelio Vidal", "Michael Maldonado", "Noel Aaron Russell", "Stephen Connard Ferry",
   "Austin Dakota Snyder", "Michael Hornibrook", "Reno Joseph Owens",
   "Zelalem Eshetu Ewnetu", "Michael Wilson-Salzl", "Keith Price", "Bruce Altenburger",
   "Ray Valdez", "Jordan Edwards", "Jacy Kevin McManus", "Kendell Wilson",
   "Jerome Allen", "Avery Richard", "Deaundre Phillips",
   "Medger Blake", "Joseph William Alain", "Rodney Henderson", "Daniel Loren Rucker",
   "Landon Nobles", "Scott Laurance Gilpin", "Jason Sebastian Roque", "Jacob Peterson")
  
  
  #function to add exact coordinates more simply
  ext.crds <- function(name, lng, lat, data=res){
    if (x$name == name){
      data$lng <- toString(lng)
      data$lat <- toString(lat)
    }
  }
  

  if (x$name %in% exact.coords){
    
    res <- data.frame(matrix(ncol=2))
    names(res) <- c("lat", "lng")
  
    
    ext.crds("Anthony David Soderberg", 
             -118.3076373,
             34.2745478)
    ext.crds("Terry Percy Campbell", 
             -81.520041,
             30.3245488)
    
    if(x$name == "Jacob Peterson"){
      res$lng <- "-117.2283824"#give exact coords. 
      res$lat <- "32.9574372"
    } 
    if(x$name == "Jason Sebastian Roque"){
      res$lng <- "-97.6555969"#give exact coords. 
      res$lat <- "30.3705714"
    } 
    if(x$name == "Scott Laurance Gilpin"){
      res$lng <- "-97.8088306"#give exact coords. 
      res$lat <- "30.4471695"
    } 
    if(x$name == "Landon Nobles"){
      res$lng <- "-97.7412653"#give exact coords. 
      res$lat <- "30.2673275"
    } 
    if(x$name == "Daniel Loren Rucker"){
      res$lng <- "-96.9445838"#give exact coords. 
      res$lat <- "32.8478519"
    } 
    if(x$name == "Rodney Henderson"){
      res$lng <- "-96.9791737"#give exact coords. 
      res$lat <- "32.8003101"
    } 
    if(x$name == "Joseph William Alain"){
      res$lng <- "-95.4641572"#give exact coords. 
      res$lat <- "30.3402039"
    } 
    if(x$name == "Medger Blake"){
      res$lng <- "-95.4947626"#give exact coords. 
      res$lat <- "30.2447195"
    } 
    if(x$name == "Deaundre Phillips"){
      res$lng <- "-84.50044"#give exact coords. 
      res$lat <- "33.7923138"
    } 
    if(x$name == "Avery Richard"){
      res$lng <- "-84.4696605"#give exact coords. 
      res$lat <- "33.7518882"
    } 
    if(x$name == "Jerome Allen"){
      res$lng <- "-81.688844"#give exact coords. 
      res$lat <- "30.3451245"
    } 
    if(x$name == "Kendell Wilson"){
      res$lng <- "-95.6458633"#give exact coords. 
      res$lat <- "29.878102"
    } 
    if(x$name == "Jacy Kevin McManus"){
      res$lng <- "-123.3985674"#give exact coords. 
      res$lat <- "42.6951021"
    } 
    if(x$name == "Jordan Edwards"){
      res$lng <- "-96.6171549"#give exact coords. 
      res$lat <- "32.7121063"
    } 
    if(x$name == "Ray Valdez"){
      res$lng <- "-98.5289968"#give exact coords. 
      res$lat <- "29.4290327"
    } 
    if(x$name == "Bruce Altenburger"){
      res$lng <- "-75.5379164"#give exact coords. 
      res$lat <- "39.7441052"
    } 
    if(x$name == "Keith Price"){
      res$lng <- "-75.539945"#give exact coords. 
      res$lat <- "39.7536355"
    } 
    if(x$name == "Zelalem Eshetu Ewnetu"){
      res$lng <- "-118.248916"#give exact coords. 
      res$lat <- "33.9544736"
    } 
    if(x$name == "Michael Wilson-Salzl"){
      res$lng <- "-84.5091264"#give exact coords. 
      res$lat <- "39.3734405"
    } 
    if(x$name == "Reno Joseph Owens"){
      res$lng <- "-76.6483867"#give exact coords. 
      res$lat <- "39.299979"
    } 
    if(x$name == "Michael Hornibrook"){
      res$lng <- "-92.2694357"#give exact coords. 
      res$lat <- "34.7474488"
    } 
    
    if(x$name == "Austin Dakota Snyder"){
      res$lng <- "-92.3972148"#give exact coords. 
      res$lat <- "34.7437644"
    } 
    if(x$name == "Stephen Connard Ferry"){
      res$lng <- "-122.317969"#give exact coords. 
      res$lat <- "38.3107036"
    }
    
    if(x$name == "Noel Aaron Russell"){
      res$lng <- "-122.2775642"#give exact coords. 
      res$lat <- "38.2847504"
    }
    
    if(x$name == "Noel Aaron Russell"){
      res$lng <- "-122.2775642"#give exact coords. 
      res$lat <- "38.2847504"
    }
    if(x$name == "Michael Maldonado"){
      res$lng <- "-95.4004836"#give exact coords. 
      res$lat <- "29.8963045"
    }
    
    if(x$name == "Rogelio Vidal"){
      res$lng <- "-119.0053476"#give exact coords. 
      res$lat <- "35.2668507"
    }
    if(x$name == "Antonio Arturo Perez Garcia"){
      res$lng <- "-118.9997988"#give exact coords. 
      res$lat <- "35.382442"
    }
    if(x$name == "William Brigham"){
      res$lng <- "-82.9126054"#give exact coords. 
      res$lat <- "39.8512624"
    }  
    if(x$name == "Benjamin Ailstock"){
      res$lng <- "-81.0536261"#give exact coords. 
      res$lat <- "41.8329659"
    }  
        if(x$name == "Mi'Chance Dunlap-Gittens"){
      res$lng <- "-122.298211"#give exact coords. 
    res$lat <- "47.4082129"
        } 

    if(x$name == "William Stokes"){
      res$lng <- "-86.7747764"#give exact coords. 
      res$lat <- "36.2476721"
    } 
    if(x$name == "Alejandro Valencia Mendez"){
      res$lng <- "-118.2600016"#give exact coords. 
      res$lat <- "34.0405703"
    } 
    
    if(x$name == "Joseph Tamayo"){
      res$lng <- "-98.5340369"#give exact coords. 
      res$lat <- "29.3664666"
    } 
    if(x$name == "Daniel Darnell Stroughter"){
      res$lng <- "-122.0157817"#give exact coords. 
      res$lat <- "38.2624659"
    } 
    
    if(x$name == "Genevive Dawes"){
      res$lng <- "-96.7664497"#give exact coords. 
      res$lat <- "32.7926343"
    }   
    if(x$name == "Austin Tyler Macon"){
      res$lng <- "-122.2869974"#give exact coords. 
      res$lat <- "40.3124194"
    }    
    if(x$name == "Marlon Joel Rodas-Sanchez"){
      res$lng <- "-121.6369909"#give exact coords. 
      res$lat <- "36.6785188"
    }    
    if(x$name == "Bradley James Sheets"){
      res$lng <- "-86.2406969"#give exact coords. 
      res$lat <- "39.7327456"
    }    
    if(x$name == "Destiny A. Moneyhun"){
      res$lng <- "-86.2352895"#give exact coords. 
      res$lat <- "39.7271348"
    }    
    if(x$name == "Fred Barragan"){
      res$lng <- "-118.2119839"#give exact coords. 
      res$lat <- "34.0437118"
    }
    if(x$name == "Michael Rogers"){
      res$lng <- "-118.26527"#give exact coords. 
      res$lat <- "34.0416883"
    }
    if(x$name == "Francisco Valdez"){
      res$lng <- "-112.2006637"#give exact coords. 
      res$lat <- "33.4526315"
    }
    if(x$name == "Michael Cisneros"){
      res$lng <- "-112.0342214"#give exact coords. 
      res$lat <- "33.4504707"
    }
    if(x$name == "Thomas Scott Henry"){
      res$lng <- "-81.412078"#give exact coords. 
      res$lat <- "28.9734887"
    }
    if(x$name == "Mario Simoes"){
      res$lng <- "-81.3080747"#give exact coords. 
      res$lat <- "29.0584178"
    }
    if(x$name == "Raynard Burton"){
      res$lng <- "-83.1314277"#give exact coords. 
      res$lat <- "42.3774269"
    }
    if(x$name == "Jonathan David Sper"){
      res$lng <- "-85.5723919"#give exact coords. 
      res$lat <- "43.1661468"
    }
    if(x$name == "Christopher Redding"){
      res$lng <- "-81.413793"#give exact coords. 
      res$lat <- "28.489765"
    }  
    if(x$name == "Elena \"Ebbie\" Mondragon"){
      res$lng <- "-122.067506"#give exact coords. 
      res$lat <- "37.6606681"
    }  
    if(x$name == "Todd Munson"){
      res$lng <- "-112.1434398"#give exact coords. 
      res$lat <- "33.6101707"
    }  
    if(x$name == "Curtis Jamal Deal"){
      res$lng <- "-76.6490336"#give exact coords. 
      res$lat <- "39.2868022"
    }
    if(x$name == "Rashad Daquan Opher"){
      res$lng <- "-76.7297393"#give exact coords. 
      res$lat <- "39.3212978"
    }
    if(x$name == "Gilbert Zambronio-Lovato"){
      res$lng <- "-106.6051881"#give exact coords. 
      res$lat <- "35.1232461"
    }
    if(x$name == "Jeremy Lopez-Robledo"){
      res$lng <- "-106.7578996"#give exact coords. 
      res$lat <- "32.2947359"
    }
    if(x$name == "Gerardo Vasquez"){
      res$lng <- "-118.4552638"#give exact coords. 
      res$lat <- "34.0268981"
    }
    if(x$name == "Justin Burton"){
      res$lng <- "-122.604864"#give exact coords. 
      res$lat <- "45.6544996"
    }
    if(x$name == "Dexter Martin Dumarce"){
      res$lng <- "-117.4340604"#give exact coords. 
      res$lat <- "47.6514985"
    }
    if(x$name == "Brian Carreno"){
      res$lng <- "-119.7524867"#give exact coords. 
      res$lat <- "34.4485565"
    }
    if(x$name == "Lorenzo Antoine Cruz"){
      res$lng <- "-121.2602086"#give exact coords. 
      res$lat <- "38.8272455"
    }
    if(x$name == "Quanice Derrick Hayes"){
      res$lng <- "-122.5801701"#give exact coords. 
      res$lat <- "45.5356419"
    }
    if(x$name == "Refugio Alvarez"){
      res$lng <- "-119.7700624"#give exact coords. 
      res$lat <- "36.7732265"
    } 
    if(x$name == "Spencer Herckt"){
      res$lng <- "-120.97981"#give exact coords. 
      res$lat <- "37.6935726"
    }  
    if(x$name == "Jesus Alberto Geney"){
      res$lng <- "-121.9618198"#give exact coords. 
      res$lat <- "37.36101"
    }  
    if(x$name == "Jesse Enjaian"){
      res$lng <- "-122.1639837"#give exact coords. 
      res$lat <- "37.754261"
    }     
    if(x$name == "William Tucker Mathis"){
      res$lng <- "-76.7788237"#give exact coords. 
      res$lat <- "39.1963526"
    }  
    if(x$name == "Ronnie Johnson"){
      res$lng <- "-92.9883419"#give exact coords. 
      res$lat <- "43.6593058"
    }  
    if(x$name == "Steve Salgado"){
      res$lng <- "-117.8731407"#give exact coords. 
      res$lat <- "33.7424444"
    }  
    if(x$name == "Peter Torres"){
      res$lng <- "-83.7735761"#give exact coords. 
      res$lat <- "31.0815981"
    }
    if(x$name == "Vincent Palma"){
      res$lng <- "-80.9140454"#give exact coords. 
      res$lat <- "41.8053823"
    }
    if(x$name == "Christopher Apostolos"){
      res$lng <- "-74.2143419"#give exact coords. 
      res$lat <- "39.9574761"
    }
    if(x$name == "Christopher Apostolos"){
      res$lng <- "-74.2143419"#give exact coords. 
      res$lat <- "39.9574761"
    }
    if(x$name == "Jamake Cason Thomas"){
      res$lng <- "-79.2037016"#give exact coords. 
      res$lat <- "34.5529097"
    }
    if(x$name == "Steven Valenzuela"){
      res$lng <- "-119.8143715"#give exact coords. 
      res$lat <- "39.529315"
    }
    if(x$name == "Rafael Navarro-Garcia"){
      res$lng <- "-119.793845"#give exact coords. 
      res$lat <- "39.485247"
    }
    if(x$name == "Kenneth Lee Bailey"){
      res$lng <- "-78.8794038"#give exact coords. 
      res$lat <- "36.0249124"
    }
    if(x$name == "Willard Eugene Scott"){
      res$lng <- "-78.9080938"#give exact coords. 
      res$lat <- "36.0228357"
    }
    if(x$name == "Juan Carlos Cuadra"){
      res$lng <- "-95.4303331"#give exact coords. 
      res$lat <- "29.6026297"
    }
    
    if(x$name == "Earl Riley"){
      res$lng <- "-95.5402252"#give exact coords. 
      res$lat <- "29.6509179"
    }
    if(x$name == "Jimmie Patton"){
      res$lng <- "-82.9872448"#give exact coords. 
      res$lat <- "39.9319575"
    }
    if(x$name == "Michael Lee Morris"){
      res$lng <- "-83.0952457"#give exact coords. 
      res$lat <- "39.943563"
    }     
    if(x$name == "Alonzo E. Ashley"){
      res$lng <- "-94.5091687"#give exact coords. 
      res$lat <- "38.9178489"
    } 
    if(x$name == "Johnnie J. Harris"){
      res$lng <- "-94.5713833"#give exact coords. 
      res$lat <- "38.9829113"
    }
    if(x$name == "James Stephen McMullen"){
      res$lng <- "-97.6656294"#give exact coords. 
      res$lat <- "35.5077306"
    }
    if(x$name == "Darrion Barnhill"){
      res$lng <- "-88.3570759"#give exact coords. 
      res$lat <- "35.5219974"
    }
    if(x$name == "John McLaughlin"){
      res$lng <- "-84.5250777"#give exact coords. 
      res$lat <- "39.2321695"
    }
    if(x$name == "James E. Lewis"){
      res$lng <- "-93.2778303"#give exact coords. 
      res$lat <- "37.2216573"
    }
    if(x$name == "Daniel D. Rogers"){
      res$lng <- "-89.6656992"#give exact coords. 
      res$lat <- "39.8076124"
    }
    if(x$name == "William Brigham"){
      res$lng <- "-82.9126001"#give exact coords. 
      res$lat <- "39.8512624"
    }
    if ( x$name == "Joshua Henry" ){
      res$lng <- "-97.4037284"#give exact coords. 
      res$lat <- "32.6970538"
    }
    if (x$name == "Frank W. Wratny"){
      res$lng <- "-80.364483"#give exact coords. 
      res$lat <- "40.9930507"
    }
    if (x$name == "James Owens"){
      res$lng <- "-73.8994051"#give exact coords. 
      res$lat <- "40.6430541"
    }  
    if (x$name == "Joshua Martino"){
      res$lng <- "-73.963353"#give exact coords. 
      res$lat <- " 40.680229"
    }
    if (x$name == "Kadeem Torres"){
      res$lng <- "-73.8801562"#give exact coords. 
      res$lat <- "40.6820163"
    }
    if (x$name == "Kris Kristl"){
      res$lng <- "-88.5227984" #give exact coords. 
      res$lat <- "42.6427308"
    }
    if (x$name == "Bradley Nelson"){
      res$lng <- "-87.2204523"#give exact coords. 
      res$lat <- "35.5436302"
    }
    if (x$name == "Timothy Lionel Williams"){
      res$lng <- "-76.986759"#give exact coords. 
      res$lat <- "38.9015122"
    }
    if (x$name == "Joey J. Bridges"){
      res$lng <- "-81.5432721"#give exact coords. 
      res$lat <- "35.2700086"
    }
    if (x$name == "Shawn M. Igers"){
      res$lng <- "-89.82778"#give exact coords. 
      res$lat <- "45.1206507"
    }
    if (x$name == "Nana Adomako"){
      res$lng <- "-121.9990104"#give exact coords. 
      res$lat <- "37.5384125"
    }
    if (x$name == "JR Williams"){
      res$lng <- "-112.134161"#give exact coords. 
      res$lat <- "33.56756"
    }
    if (x$name == "Herbert Johnson"){
      res$lng <- "-87.7109127"#give exact coords. 
      res$lat <- "41.879988"
    }
    if (x$name == "Chad Robertson"){
      res$lng <- "-87.6401182"#give exact coords. 
      res$lat <- "41.879988"
    }
    if (x$name == "Michelle Robey"){
      res$lng <- "-87.6967298"#give exact coords. 
      res$lat <- "41.9528976"
    }
    if (x$name == "Kadhar Bailey"){
      res$lng <- "-69.6671544"#give exact coords. 
      res$lat <- "44.4166166"
    }
    if (x$name == "Ambroshia Fagre"){
      res$lng <- "-69.666389"#give exact coords. 
      res$lat <- "44.416479"
    }
    if (x$name == "Sergio Reyes"){
      res$lng <- "-73.9269128"#give exact coords. 
      res$lat <- "40.7034323"
    }
    if (x$name == "Jahlire Nicholson"){
      res$lng <- "-73.761866"#give exact coords. 
      res$lat <- "40.672705"
    }
    if (x$name == "Davion Henderson"){
      res$lng <- "-90.3193157"#give exact coords. 
      res$lat <- "38.767247"
    }
    if (x$name == "Don Clark"){
      res$lng <- "-90.232233"#give exact coords. 
      res$lat <- "38.583107"
    }
    if (x$name == "Morgan London Rankins"){
      res$lng <- "-97.8084238"#give exact coords. 
      res$lat <- "30.2019399"
    }
    if (x$name == "Scott Laurance Gilpin"){
      res$lng <- "-97.8083145"#give exact coords. 
      res$lat <- "30.4451322"
    }
    if (x$name == "Josue Javier Diaz"){
      res$lng <- "-80.7447278"#give exact coords. 
      res$lat <- "35.2027447"
    }
    if (x$name == "Iaroslav Mosiiuk"){
      res$lng <- "-80.8349917"#give exact coords. 
      res$lat <- "35.2603674"
    }
    if (x$name == "Daniel Scott Giberson"){
      res$lng <- "-81.0561785"#give exact coords. 
      res$lat <- "37.3614106"
    }
    if (x$name == "William Dwayne Darby"){
      res$lng <- "-84.7965679"#give exact coords. 
      res$lat <- "32.069971"
    }
    if (x$name == "Rashad Opher"){
      res$lng <- "-76.7287234"#give exact coords. 
      res$lat <- "39.3221064"
    }
    
    ### end manual entry of lat-long death locations 
    
    #attach the manually idenitified coordinates to the wp.loc frame
    wp.loc <- rbind(wp.loc, cbind(x,res))
    
        }else{ #start trying GIS look-up
    
    #search for GPS coords of exact city name
    city.user <- paste0(x$city, user.suffix)
    res <- GNsearch(name_equals=city.user, country="US")
    
    
    ## If there isn't an obvious unique match we need to be more creative:
    
    if (ncol(res) == 0){ #Try searching again for non-exact name
      res <- GNsearch(name=city.user, country="US")
    }
    
    #restrict results to the state we're looking for
    x.state <- state.name[grep(x$state, state.abb)]
    res <- res[res$adminName1 == x.state,]
    
    #if there are multiple possible entries for a city
    #it's sometimes enough to restrict to entries with
    #the word "city" in the fclName column
    city.mention <- grep("city", res$fclName)
    if (length(city.mention) == 1){
      res <- res[city.mention,]
    }
    
    #we can also rm false positives
    #by cutting out entries with population 0
    if (sum(res$population > 0) == 1){ #but not super reliable so 
      #don't use this as a criteria unless it actually cuts our
      #possibilites down to 1
      res <- res[res$population >0, ]
    }
    
    if (nrow(res) > 1){ #if we still have multiple 
                        # possible entries for the city-state...

      
      #(This block clears up an ambiguity in the LA entry)
      if (x$city == "Los Angeles"){
        res <- res[res$population >100000,]
      }
      
      
      #if after all this we still have multiple possible entries
      #stop script and throw informative error
      if(nrow(res ) > 1){  
        stop(paste0("Err on",x$name,". There are multiple indistinguishable entries for ", res$name," in the state of ",x$state,"\n"))
      }
      
      
      
    }
    
    
    
    if (i != 1){ 
      
      #check to see if we put multiple people at identical lat-long
      #if so throw an informative warning and warn user if so...
      dup <- (wp.loc[,"lat"] == res$lat) & (wp.loc[,"lng"] == res$lng)
      if (TRUE %in% dup){ 
        warning( paste(x$name, "(", x$city,",", x$state,")", "is being mapped to the same location (",res$lat,res$lng ,") as someone else") )
      }
      if (as.numeric(res$population) > 1e6){
        warning( paste(x$name, "is being mapped to the center of",res$toponymName,", a city of >1 million people","in",res$adminName1,"...") )
      }
      
      
    }
    
    
    #add resulting entry to main data.frame...
    wp.loc <- rbind(wp.loc, cbind(x, res[c("lng","lat")]))
    
    
  }
  
  
  #throw a warning if we failed to come up with a legitimate lat-long for the person's location
  if (any(!complete.cases(res$lat), !complete.cases(res$lat))){
    warning(paste(i, x$name, "in", x$city, x$state, "lat long not found" ))
  }
  
  
  
  progress.interval <- 50
  length.p.bar <- as.character(rep(x = "", progress.interval + 1) )

  #initiate progress bar at first loop
  if (i == 1){
    progress <- 0
    cat("|Calculating GPS coordinates for",nrow(wpost),"WaPost killings\n|",length.p.bar,"|\n|0%" ,length.p.bar[1:(length(length.p.bar)-6)] , "100%|\n|==")
  }
  
  #incremement progress bar
  if ( (i %% as.integer(nrow(wpost)/progress.interval)) == 0){
    cat("=")
  }
  
}

#drop cases that didn't yield a lat/lang pair
wp.loc <- wp.loc[complete.cases(wp.loc$lat),]

#process some basic info on armed / unarmed for color coding
wp.loc$armed <- as.character(wp.loc$armed)
if ( ( sum(wp.loc$armed == "") > 0 ) ){
  wp.loc[wp.loc$armed == "",]$armed <- "undetermined"
}

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


#Color-code thecounted codes for whether the victim was armed
unarmedC <-"#ff0000"  # Red = Unarmed
armedC <-  "#008080"  # Teal = armed
idkC <- "#000000"  # Black = Don't know or ambiguous category like "Non-lethal firearm" or "vehicle"


#assign colors to armed/unarmed/other for leaflet, specific to WaPo data
wp.pal <- ifelse(wp.loc$armed %in% facs[1], unarmedC,
                 ifelse(wp.loc$armed %in% facs[2:13], idkC,
                        ifelse(wp.loc$armed %in% wapo.armed, armedC, idkC)))

#return data with locations and a color-coding scheme
return (list(processedData = wp.loc, colorCodeArmed = wp.pal))

}
