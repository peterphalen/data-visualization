
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
  
  
  # I looked up the coordinates of the following deaths manually, 
  # either because the geoname search function had trouble with them
  # or because the cities they occurred in were too big for it to be reasonable
  # to map deaths to their center
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
   "Landon Nobles", "Scott Laurance Gilpin", "Jason Sebastian Roque", "Jacob Peterson","Matthew Zank", 
   "Brandon Pequeno", "Sariah Marie Lane", "Jorge Alberto Fuentes", "Patrick Sanchez", 
   "Andrew James Lucero", "Anthony David Soderberg", "Terry Percy Campbell", "Selwyn Aubrey Hall",
   "Ronald Singletary", "Jonie Block", "Isabelle Duval", "Daniel George Boak", "Anthony Paul Ardo",
   "John Eno", "Nicholas Flusche", "Brandon S. Lambert", "Albert Gagnier",
   "Jamie J. Robinson", "Isaiah M. Hammett", "Robin White", "Joseph Zimmerman",
   "Manuel Encinas", "Corsean Lewis", "Mirza Tatlic", "Francis de la Cruz",
   "Francisco Suarez-Madonado","Steven Price","David English","Naway Willy","Joshua Barre",
   "Jimmie Bevenue", "Philando Castile", "Donald Lee Cramer", "Erik Pamias",
   "Quentin Louis Case", "Santino Trevino", "Arties Manning", "Alexander Bonds",
   "Nicholas Johnston", "Jesus Ramon Deltoro", "Alton Folmar", "Charleena Lyles",
   "Damarius Butts", "David Jones", "Adrian Maurice Hardeman", "Refugio Alvarez",
   "Isaiah Murrietta-Golding", "Salvadro Alfredo Pablo Lopez", "Sinuon Pream",
   "Michele Rice", "Theodore Brendecke", "John Spaulding", "Jermaine Claybrooks",
   "Terry Williams", "Shawn Buck","Matthew Colton Stover", "William Tucker Mathis",
   "Rogelio Vidal Landa", "Oscar Junior", "Rodney L. Cole","Tim A. Holmgren",
   "Jocques Scott Clemmons", "George Lee Seeton","Deveonte Johnson",
   "Joseph Paul Hogan","Robert J. Berube","Jamie Dougan","Daniel Donarski",
   "Jose Guillermo Flores Colon", "Stephen Rich", "Chet Knuppel", "Alexander Meltz",
   "Brett Rodriguez", "Aaron Bailey", "Terrell Kyreem Johnson", "James Edward Ray",
   "Jason Thomas Christian", "Fred Cardenas", "Christopher Thompkins",
   "Misael Macias Cano", "Tavis Crane", "Kevin C. Perry", "Chazz Brown",
   "Jeffrey Findlay"
   
   )
  

  if (x$name %in% exact.coords){
    
    res <- data.frame(matrix(ncol=2))
    names(res) <- c("lat", "lng")

    

    if(x$name == "Jeffrey Findlay"){
      res$lng <- "-81.7121359"#give exact coords. 
      res$lat <- "41.4545678"
    }
    if(x$name == "Chazz Brown"){
      res$lng <- "-90.2186905"#give exact coords. 
      res$lat <- "38.5967371"
    }
    if(x$name == "Kevin C. Perry"){
      res$lng <- "-97.3118014"#give exact coords. 
      res$lat <- "37.6238853"
    }
    if(x$name == "Tavis Crane"){
      res$lng <- "-97.1341163"#give exact coords. 
      res$lat <- "32.7570355"
    }
    if(x$name == "Misael Macias Cano"){
      res$lng <- "-104.7757969"#give exact coords. 
      res$lat <- "38.8542723"
    }
    if(x$name == "Christopher Thompkins"){
      res$lng <- "-79.9117396"#give exact coords. 
      res$lat <- "40.4599276"
    }  
    if(x$name == "Fred Cardenas"){
      res$lng <- "-97.267994"#give exact coords. 
      res$lat <- "32.7404268"
    }   
    if(x$name == "Fred Cardenas"){
      res$lng <- "-97.267994"#give exact coords. 
      res$lat <- "32.7404268"
    }  
    if(x$name == "Jason Thomas Christian"){
      res$lng <- "-82.966608"#give exact coords. 
      res$lat <- "39.9345941"
    }  
    if(x$name == "James Edward Ray"){
      res$lng <- "-83.1601277"#give exact coords. 
      res$lat <- "42.3590168"
    }  
    if(x$name == "James Edward Ray"){
      res$lng <- "-83.1601277"#give exact coords. 
      res$lat <- "42.3590168"
    } 
    if(x$name == "Terrell Kyreem Johnson"){
      res$lng <- "-122.5687308"#give exact coords. 
      res$lat <- "45.468068"
    } 
    if(x$name == "Aaron Bailey"){
      res$lng <- "-86.1924803"#give exact coords. 
      res$lat <- "39.8007405"
    } 
    if(x$name == "Brett Rodriguez"){
      res$lng <- "-105.0602889"#give exact coords. 
      res$lat <- "39.8201176"
    } 
    if(x$name == "Alexander Meltz"){
      res$lng <- "-105.04399"#give exact coords. 
      res$lat <- "39.8977021"
    } 
    if(x$name == "Chet Knuppel"){
      res$lng <- "-105.0223897"#give exact coords. 
      res$lat <- "40.4805557"
    } 
    if(x$name == "Stephen Rich"){
      res$lng <- "-105.1262139"#give exact coords. 
      res$lat <- "40.3992228"
    }  
    if(x$name == "Jose Guillermo Flores Colon"){
      res$lng <- "-81.2664023"#give exact coords. 
      res$lat <- "28.9114227"
    }  
    if(x$name == "Daniel Donarski"){
      res$lng <- "-81.1626474"#give exact coords. 
      res$lat <- "28.8821946"
    }   
    if(x$name == "Robert J. Berube"){
      res$lng <- "-89.6263004"#give exact coords. 
      res$lat <- "34.1404612"
    } 
    if(x$name == "Jamie Dougan"){
      res$lng <- "-89.6281685"#give exact coords. 
      res$lat <- "34.1405366"
    } 
    if(x$name == "Joseph Paul Hogan"){
      res$lng <- "-80.8729777"#give exact coords. 
      res$lat <- "34.112632"
    } 
    if(x$name == "Tim A. Holmgren"){
      res$lng <- "-103.2507667"#give exact coords. 
      res$lat <- "44.102117"
    } 
    if(x$name == "Deveonte Johnson"){
      res$lng <- "-97.5183415"#give exact coords. 
      res$lat <- "35.4497174"
    } 
    if(x$name == "George Lee Seeton"){
      res$lng <- "-97.4980065"#give exact coords. 
      res$lat <- "35.4316821"
    } 
    if(x$name == "Rodney L. Cole"){
      res$lng <- "-86.7335658"#give exact coords. 
      res$lat <- "36.112096"
    } 
    if(x$name == "Jocques Scott Clemmons"){
      res$lng <- "-86.7618719"#give exact coords. 
      res$lat <- "36.1672424"
    }  
    if(x$name == "Oscar Junior"){
      res$lng <- "-117.68114"#give exact coords. 
      res$lat <- "35.6152533"
    }   
    if(x$name == "Rogelio Vidal Landa"){
      res$lng <- "-119.0107207"#give exact coords. 
      res$lat <- "35.2778013"
    }
    if(x$name == "Isaiah Murrietta-Golding"){
      res$lng <- "-119.7745632"#give exact coords. 
      res$lat <- "36.8015481"
    }
    if(x$name == "William Tucker Mathis"){
      res$lng <- "-76.7788184"#give exact coords. 
      res$lat <- "39.1963485"
    }
    if(x$name == "Matthew Colton Stover"){
      res$lng <- "-147.8152625"#give exact coords. 
      res$lat <- "64.8324591"
    }
    if(x$name == "Shawn Buck"){
      res$lng <- "-147.7212201"#give exact coords. 
      res$lat <- "64.8226729"
    }
    if(x$name == "Terry Williams"){
      res$lng <- "-87.8794666"#give exact coords. 
      res$lat <- "43.0578217"
    }
    if(x$name == "Jermaine Claybrooks"){
      res$lng <- "-87.9368689"#give exact coords. 
      res$lat <- "43.1064722"
    }
    if(x$name == "John Spaulding"){
      res$lng <- "-80.3138983"#give exact coords. 
      res$lat <- "25.8105769"
    }
    if(x$name == "Theodore Brendecke"){
      res$lng <- "-80.3286883"#give exact coords. 
      res$lat <- "25.6522132"
    }
    if(x$name == "Sinuon Pream"){
      res$lng <- "-118.1233814"#give exact coords. 
      res$lat <- "33.775248"
    }
    if(x$name == "Michele Rice"){
      res$lng <- "-118.1013132"#give exact coords. 
      res$lat <- "33.8103804"
    }
    if(x$name == "Salvadro Alfredo Pablo Lopez"){
      res$lng <- "-119.8174674"#give exact coords. 
      res$lat <- "36.7866403"
    }
    if(x$name == "Refugio Alvarez"){
      res$lng <- "-119.7700571"#give exact coords. 
      res$lat <- "36.7732222"
    }
    if(x$name == "Adrian Maurice Hardeman"){
      res$lng <- "-97.2652917"#give exact coords. 
      res$lat <- "29.4574588"
    }
    if(x$name == "David Jones"){
      res$lng <- "-75.1665767"#give exact coords. 
      res$lat <- "40.047812"
    }
    if(x$name == "Damarius Butts"){
      res$lng <- "-122.3356692"#give exact coords. 
      res$lat <- "47.6022011"
    }
    if(x$name == "Charleena Lyles"){
      res$lng <- "-122.2636216"#give exact coords. 
      res$lat <- "47.6785736"
    }
    if(x$name == "Alton Folmar"){
      res$lng <- "-96.644218"#give exact coords. 
      res$lat <- "32.7461663"
    }
    if(x$name == "Jesus Ramon Deltoro"){
      res$lng <- "-112.0237407"#give exact coords. 
      res$lat <- "33.450308"
    }
    if(x$name == "Nicholas Johnston"){
      res$lng <- "-111.995061"#give exact coords. 
      res$lat <- "33.7670024"
    }
    if(x$name == "Alexander Bonds"){
      res$lng <- "-73.9041131"#give exact coords. 
      res$lat <- "40.8580946"
    }    
    if(x$name == "Arties Manning"){
      res$lng <- "-90.0148624"#give exact coords. 
      res$lat <- "30.0451766"
    }
    if(x$name == "Santino Trevino"){
      res$lng <- "-118.348368"#give exact coords. 
      res$lat <- "34.1046192"
    } 
    if(x$name == "Quentin Louis Case"){
      res$lng <- "-81.4675884"#give exact coords. 
      res$lat <- "27.8723868"
    }  
    if(x$name == "Donald Lee Cramer"){
      res$lng <- "-112.2920843"#give exact coords. 
      res$lat <- "33.4789372"
    }  
    
    if(x$name == "Erik Pamias"){
      res$lng <- "-112.3405328"#give exact coords. 
      res$lat <- "33.4576654"
    } 
    if(x$name == "Philando Castile"){
      res$lng <- "-93.1723578"#give exact coords. 
      res$lat <- "44.991748"
    }  
    if(x$name == "Naway Willy"){
      res$lng <- "-95.8600529"#give exact coords. 
      res$lat <- "36.1479271"
    }
  if(x$name == "Jimmie Bevenue"){
      res$lng <- "-95.853367"#give exact coords. 
      res$lat <- "36.1552932"
    }
   if(x$name == "Joshua Barre"){
      res$lng <- "-95.9982679"#give exact coords. 
      res$lat <- "36.2278797"
    }
  if(x$name == "Francisco Suarez-Madonado"){
      res$lng <- "-115.04007"#give exact coords. 
      res$lat <- "36.1906263"
    }
    if(x$name == "Steven Price"){
      res$lng <- "-115.062137"#give exact coords. 
      res$lat <- "36.2002761"
    }
    if(x$name == "David English"){
      res$lng <- "-95.8708093"#give exact coords. 
      res$lat <- "36.0947635"
    }
    if(x$name == "Mirza Tatlic"){
      res$lng <- "-121.8958799"#give exact coords. 
      res$lat <- "37.3015984"
    }
    if(x$name == "Francis de la Cruz"){
      res$lng <- "-121.8917754"#give exact coords. 
      res$lat <- "37.3386625"
    }
    if(x$name == "Corsean Lewis"){
      res$lng <- "-87.6257317"#give exact coords. 
      res$lat <- "41.759946"
    }
    if(x$name == "Manuel Encinas"){
      res$lng <- "-110.8338337"#give exact coords. 
      res$lat <- "32.177039"
    }
    if(x$name == "Joseph Zimmerman"){
      res$lng <- "-110.9532451"#give exact coords. 
      res$lat <- "32.1528979"
    }
    if(x$name == "Robin White"){
      res$lng <- "-90.2588104"#give exact coords. 
      res$lat <- "38.5660091"
    }
    if(x$name == "Isaiah M. Hammett"){
      res$lng <- "-90.2808615"#give exact coords. 
      res$lat <- "38.5806641"
    }
    if(x$name == "Jamie J. Robinson"){
      res$lng <- "-90.2962382"#give exact coords. 
      res$lat <- "38.5606937"
    }
    if(x$name == "Albert Gagnier"){
      res$lng <- "-83.9026136"#give exact coords. 
      res$lat <- "35.8811939"
    }
    if(x$name == "Brandon S. Lambert"){
      res$lng <- "-84.1556785"#give exact coords. 
      res$lat <- "35.9005618"
    }
    if(x$name == "Nicholas Flusche"){
      res$lng <- "-122.4104125"#give exact coords. 
      res$lat <- "37.7840296"
    }
    if(x$name == "John Eno"){
      res$lng <- "-122.4692547"#give exact coords. 
      res$lat <- "37.6470768"
    }
    if(x$name == "Anthony Paul Ardo"){
      res$lng <- "-75.2028438"#give exact coords. 
      res$lat <- "40.8088184"
    }
    if(x$name == "Daniel George Boak"){
      res$lng <- "-77.0843054"#give exact coords. 
      res$lat <- "38.8460961"
    }
    if(x$name == "Isabelle Duval"){
      res$lng <- "-77.0081357"#give exact coords. 
      res$lat <- "38.8216036"
    }
    if(x$name == "Jonie Block"){
      res$lng <- "-112.0790317"#give exact coords. 
      res$lat <- "33.4609849"
    }
    if(x$name == "Ronald Singletary"){
      res$lng <- "-75.2212043"#give exact coords. 
      res$lat <- "39.9423937"
    }
    if(x$name == "Selwyn Aubrey Hall"){
      res$lng <- "-81.6510632"#give exact coords. 
      res$lat <- "30.3557386"
    }
    if(x$name == "Terry Percy Campbell"){
      res$lng <- "-81.5215805"#give exact coords. 
      res$lat <- "30.3220048"
    }
    if(x$name == "Anthony David Soderberg"){
      res$lng <- "-118.307262"#give exact coords. 
      res$lat <- "34.2745553"
    } 
    
    if(x$name == "Andrew James Lucero"){
      res$lng <- "-105.8903967"#give exact coords. 
      res$lat <- "35.519732"
    } 
    if(x$name == "Jorge Alberto Fuentes"){
      res$lng <- "-112.1537635"#give exact coords. 
      res$lat <- "33.5531156"
    } 
    if(x$name == "Patrick Sanchez"){
      res$lng <- "-112.2366017"#give exact coords. 
      res$lat <- "33.5376852"
    } 
    if(x$name == "Sariah Marie Lane"){
      res$lng <- "-112.173"#give exact coords (imprecise here)
      res$lat <- "33.551"
    } 
    if(x$name == "Brandon Pequeno"){
      res$lng <- "-112.1739539"#give exact coords. 
      res$lat <- "33.5511412"
    } 
    if(x$name == "Matthew Zank"){
      res$lng <- "-91.5196145"#give exact coords. 
      res$lat <- "44.7879595"
    } 
    if(x$name == "Gregory Kever"){
      res$lng <- "-91.5423924"#give exact coords. 
      res$lat <- "44.8258217"
    } 
    if(x$name == "Anthony David Soderberg"){
      res$lng <- "-118.3076373"#give exact coords. 
      res$lat <- "34.2745478"
    } 
    if(x$name == "Terry Percy Campbell"){
      res$lng <- "-81.520041"#give exact coords. 
      res$lat <- "30.3245488"
    } 
    
    
    
    
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
    
        }else{ #start trying GIS look-up...
          # the goal here is to narrow down all the possible matches to a unique
          # match for the city given by WaPost
    
    #search for GPS coords of exact city name
    city.user <- paste0(x$city, user.suffix)
    city.user <- gsub(" ", "-",city.user) #replace spaces with hyphens
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
    

      #if after all this we still have multiple possible entries
      #stop script and throw informative error
      if(nrow(res ) > 1){  
        stop(paste0("Err on",unique(x$name),". There are multiple indistinguishable entries for ", unique(res$name)," in the state of ",unique(x$state),"\n"))
      }
      
    
    
    
    if (i != 1){ #unless we're on the first entry...
      
      #check to see if we put multiple people at identical lat-long and
      #if so throw an informative warning and warn user...
      dup <- (wp.loc[,"lat"] == res$lat) & (wp.loc[,"lng"] == res$lng) # vector showing rows with lat-lng pairs that match the current entry
      if (TRUE %in% dup){ 
        warning( paste(x$name, "(", x$city,",", x$state,")", "is being mapped to the same location (",res$lat,res$lng ,") as someone else") )
      }
      
      #Also, et user know if they're placing a marker in the center of a large city (>300k) rather than a more exact location
      if (as.numeric(res$population) > 3e5){
        warning( paste(x$name, "is being mapped to the center of",res$toponymName,", a city of >300 thousand people","in",res$adminName1,"...") )
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
