install.packages("stringr")
install.packages("curl")
install.packages("httr")
install.packages("rvest")
install.packages("dplyr")
install.packages("purrr")
install.packages("lubridate")


library(stringr)
library(curl)
library(rvest)
library(dplyr)
library(purrr)
library(lubridate)

times<-strptime(with_tz(Sys.time(),tzone="US/Eastern"),"%Y-%m-%d %H:%M:%S") # Extract the system date and time
y<-as.character(format(times,"%Y")) # Extract and store the year value
m<-as.character(format(times,"%m")) # Extract and store the month value
d<-as.character(format(times,"%d")) # Extract and store the day value
h<-as.character(format(times,"%H")) # Extract and store the hour value

# 1, webscraping processes
# webscraping with rvest package, some regular expressions are used.
# website 1
# For website 1, AQI information(today, tomorrow) for Pittsburgh area and Liberty-Clairton area are scarped.
# The content of the website would be updated at ___ everyday, and does not change until next update.
# The source of the websites is the api of airnow.
#
webpage1_1 <- tryCatch(read_html("http://feeds.airnowapi.org/rss/forecast/113.xml"),error=function(y){return(c())})
if (is_empty(webpage1_1)==FALSE){
  pitt = webpage1_1 %>%
    html_nodes("body") %>%
    html_text()
} else {
  pitt<-c()
}
if (is_empty(pitt)==FALSE){
  todaypittpollutant=strsplit(str_extract(pitt,"(?<=Today).*?(?=<br />)"),"-")[[1]][3]
  todaypittpollutant=gsub(" ","",todaypittpollutant)
  if(todaypittpollutant != "Ozone")
  {
    todaypittpollutant="PM 2.5"
  }
  todaypitt=str_extract(pitt,"(?<=Today).*?(?=AQI)")
  todaypitt=strsplit(todaypitt,"-")
  AQIDateToday = substr(todaypitt[[1]][1],3,12)
  AQIWeekToday = weekdays(as.Date(AQIDateToday,"%m/%d/%Y"))
  todaypitt=strsplit(todaypitt[[1]][2]," ")
  todaypitt=todaypitt[[1]][2]
  tomorrowpittpollutant=strsplit(str_extract(pitt,"(?<=Tomorrow).*?(?=<br />)"),"-")[[1]][3]
  tomorrowpittpollutant=gsub(" ","",tomorrowpittpollutant)
  if(tomorrowpittpollutant != "Ozone")
  {
    tomorrowpittpollutant="PM 2.5"
  }
  tomorrowpitt=str_extract(pitt,"(?<=Tomorrow).*?(?=AQI)")
  tomorrowpitt=strsplit(tomorrowpitt,"-")
  AQIDateTom = substr(tomorrowpitt[[1]][1],3,12)
  AQIWeekTom = weekdays(as.Date(AQIDateTom,"%m/%d/%Y"))
  tomorrowpitt=strsplit(tomorrowpitt[[1]][2]," ")
  tomorrowpitt=tomorrowpitt[[1]][2]
} else{
  todaypitt<-"--"
  tomorrowpitt<-"--"
}
webpage1_2 <- tryCatch(read_html("http://feeds.airnowapi.org/rss/forecast/352.xml"),error=function(y){return(c())})
if (is_empty(webpage1_2)==FALSE){
  LC = webpage1_2 %>%
    html_nodes("body") %>%
    html_text()
} else{
  LC<-c()
}
if(is_empty(LC)==FALSE){
  todayLCpollutant=strsplit(str_extract(LC,"(?<=Today).*?(?=<br />)"),"-")[[1]][3]
  todayLCpollutant=gsub(" ","",todayLCpollutant)
  if(todayLCpollutant != "Ozone")
  {
    todayLCpollutant="PM 2.5"
  }
  todayLC=str_extract(LC,"(?<=Today).*?(?=AQI)")
  todayLC=strsplit(todayLC,"-")
  todayLC=strsplit(todayLC[[1]][2]," ")
  todayLC=todayLC[[1]][2]
  tomorrowLCpollutant=strsplit(str_extract(LC,"(?<=Tomorrow).*?(?=<br />)"),"-")[[1]][3]
  tomorrowLCpollutant=gsub(" ","",tomorrowLCpollutant)
  if(tomorrowLCpollutant != "Ozone")
  {
    tomorrowLCpollutant="PM 2.5"
  }
  tomorrowLC=str_extract(LC,"(?<=Tomorrow).*?(?=AQI)")
  tomorrowLC=strsplit(tomorrowLC,"-")
  tomorrowLC=strsplit(tomorrowLC[[1]][2]," ")
  tomorrowLC=tomorrowLC[[1]][2]
} else{
  todayLC<-"--"
  tomorrowLC<-"--"
}

# color code function of AQI
aqi_color<-function(x){
  if (x == "Good"){
    "6AFE19"
  } else if (x == "Moderate"){
    "FFF421"
  } else if (x == "Unhealthy for Sensitive Groups"){
    "FF6A20"
  } else "FF2121" 
}


# website 2
#
# For this website, we scraped the Discussion part from the report "Southwest Ozone/PM2.5 Forecast", in our report
# it is the content in "Today's Forecast" section.
# The content of the website would be updated at ___ everyday, and does not change until next update.
# The source of the website is the Pennsylvania Department of Environmental Protection.
# 
webpage2 <- tryCatch(read_html("https://www.ahs.dep.pa.gov/AQPartnersWeb/forecast.aspx?vargroup=sw"),error=function(y){return(c())})
if (is_empty(webpage2)==FALSE){
  todayforecast <- webpage2 %>%
    html_nodes("body form div div div div div div") %>%
    html_text()
} else {
  todayforecast<-c()
}
if(is_empty(todayforecast)==FALSE){
  head(todayforecast)
  discriptions = todayforecast[1]
  todayforecast = todayforecast[2]
  todayforecast = strsplit(todayforecast, "\r\n")
  todayforecast = todayforecast[[1]][2]
  todayforecast = gsub("  ","",todayforecast)
} else{
  todayforecast<-"--"
}

# website 3
# 
# The wind direction and speed information is obtained from this website.
# The source is National Weather Service Forecast office.
# The content in this website updates every hour, and only contains the information in the future.
# Better be scraped in a fixed time daily(e.g.: 8:30 am)
# 
webpage4 <- tryCatch(read_html("https://forecast.weather.gov/MapClick.php?lat=40.427&lon=-80.0107&lg=english&&FcstType=digital"),error=function(y){return(c())})
date = as.character(Sys.Date())
date = strsplit(date,"-")
date = paste(date[[1]][2],date[[1]][3],sep="/")
if(is_empty(webpage4)==FALSE){
  p2 <- webpage4 %>%
    html_nodes(xpath="/html/body/table[6]/tr/td") %>%
    html_text()
} else {
  p2<-c()
}
if(is_empty(p2)==FALSE){
  p2 = p2[-c(1,402)]
  dim(p2) = c(25,32)
  p2 = t(p2)
  booldate = FALSE
  if(p2[1,2]==date){
    booldate = TRUE
  }
  time = as.numeric(p2[2,2])# this time need to be <=9, which means this code should be run before 9am everyday
  todaymorningwind = "--"
  todayafternoonwind = "--"
  todayeveningwind = "--"
  todayovernightwind = "--"
  tomorrowmorningwind = "--"
  tomorrowafternoonwind = "--"
} else{
  todaymorningwind = "--"
  todayafternoonwind = "--"
  todayeveningwind = "--"
  todayovernightwind = "--"
  tomorrowmorningwind = "--"
  tomorrowafternoonwind = "--"
}


web4_1 = paste("https://forecast.weather.gov/MapClick.php?w0=t&w1=td&w2=wc&w3=sfcwind&w3u=1&w4=sky&w5=pop&w6=rh&w7=rain&w8=thunder&w9=snow&w10=fzg&w11=sleet&w13u=0&w16u=1&w17u=1&AheadHour=",9-time,"&Submit=Submit&FcstType=digital&textField1=40.427&textField2=-80.0107&site=all&unit=0&dd=&bw=",sep="")
web4_2 = paste("https://forecast.weather.gov/MapClick.php?w0=t&w1=td&w2=wc&w3=sfcwind&w3u=1&w4=sky&w5=pop&w6=rh&w7=rain&w8=thunder&w9=snow&w10=fzg&w11=sleet&w13u=0&w16u=1&w17u=1&AheadHour=",9-time+24,"&Submit=Submit&FcstType=digital&textField1=40.427&textField2=-80.0107&site=all&unit=0&dd=&bw=",sep="")
webpage4_1=tryCatch(read_html(web4_1),error=function(y){return(c())})
if (is_empty(webpage4_1)==FALSE){
  p4_1 <- webpage4_1 %>%
    html_nodes(xpath="/html/body/table[6]/tr/td") %>%
    html_text()
} else {
  p4_1<-c()
}

if(is_empty(p4_1)==FALSE){
  p4_1 = p4_1[-c(1,402)]
  dim(p4_1) = c(25,32)
  p4_1 = t(p4_1)
  
  todaymorningwind = paste(p4_1[7,1+which.max(as.numeric(p4_1[6,2:5]))],p4_1[6,1+which.max(as.numeric(p4_1[6,2:5]))],sep=" - ")
  todayafternoonwind = paste(p4_1[7,5+which.max(as.numeric(p4_1[6,6:11]))],p4_1[6,5+which.max(as.numeric(p4_1[6,6:11]))],sep=" - ")
  todayeveningwind = paste(p4_1[7,11+which.max(as.numeric(p4_1[6,12:17]))],p4_1[6,11+which.max(as.numeric(p4_1[6,12:17]))],sep=" - ")
  todayovernightwind = paste(p4_1[7,17+which.max(as.numeric(p4_1[6,18:23]))],p4_1[6,17+which.max(as.numeric(p4_1[6,18:23]))],sep=" - ")
} else{
  todaymorningwind<-"--"
  todayafternoonwind<-"--"
  todayeveningwind<-"--"
  todayovernightwind<-"--"
}
webpage4_2=tryCatch(read_html(web4_2),error=function(y){return(c())})
if (is_empty(webpage4_2)==FALSE){
  p4_2 <- webpage4_2 %>%
    html_nodes(xpath="/html/body/table[6]/tr/td") %>%
    html_text()
} else {
  p4_2<-c()
}

if(is_empty(p4_2)==FALSE){
  p4_2 = p4_2[-c(1,402)]
  dim(p4_2) = c(25,32)
  p4_2 = t(p4_2)
  if(max(as.numeric(p4_2[6,2:5])>=max(as.numeric(p4_1[6,24:25])))){
  tomorrowmorningwind = paste(p4_2[7,1+which.max(as.numeric(p4_2[6,2:5]))],p4_2[6,1+which.max(as.numeric(p4_2[6,2:5]))],sep=" - ")
  }
  else{
    tomorrowmorningwind = paste(p4_1[7,23+which.max(as.numeric(p4_1[6,24:25]))],p4_1[6,23+which.max(as.numeric(p4_1[6,24:25]))],sep=" - ")
  }
  
  tomorrowafternoonwind = paste(p4_2[7,5+which.max(as.numeric(p4_2[6,6:11]))],p4_2[6,5+which.max(as.numeric(p4_2[6,6:11]))],sep=" - ")
} else {
  tomorrowmorningwind<-"--"
  tomorrowafternoonwind<-"--"
}
# Website 4 - Air Dispersion Index
# For this website, the numbers in the first three columns of the first table after the "ADI Early" and "ADI Late" rows will be scraped
# These consist of a number and a description

if (as.numeric(h)>=4 & as.numeric(h)<12){
  link4<-"https://forecast.weather.gov/product.php?site=NWS&product=FWF&issuedby=PBZ"
} else {
  link4<-"https://forecast.weather.gov/product.php?site=NWS&issuedby=PBZ&product=FWF&format=CI&version=2&glossary=0"
}


page4<-tryCatch(read_html(link4),error=function(y){return(c())}) # Read in correct website link, return a blank vector if the website is down
if (is_empty(page4)==FALSE){
  table4<-page4 %>% # Select the node containing the data, in  this case, all of the tables on the site will be scraped at once
    html_nodes(".glossaryProduct") %>%
    html_text()
} else {
  table4<-c()
}

if(is_empty(table4)==FALSE){
  adiearly<-str_extract(table4,'ADI\\searly.{1,}') # Extract ADI Early row from the first table
  adilate<-str_extract(table4,'ADI\\slate.{1,}') # Extract ADI Late row from the first table
  # ADI Early
  adiearlysplit<-str_extract_all(adiearly,'\\d{1,}\\s.{1,9}') # Split the row into each number-description pair
  adiearlytoday<-trimws(adiearlysplit[[1]][1],whitespace=" ") # First pair is the "today" column, take the pair and remove any spaces before and after the text
  aetodvalue<-str_extract(adiearlytoday,'\\d{1,}') # Extracts just the number from the today pair
  aetoddesc<-str_extract(adiearlytoday,regex('[:alpha:]{1,}[:space:]{1}[:alpha:]{1,}|[:alpha:]{1,}')) # Extracts the description after the today pair
  adiearlytonight<-trimws(adiearlysplit[[1]][2],whitespace=" ") # Second pair is the "tonight" column, take the pair and remove any spaces before and after the text
  aetonvalue<-str_extract(adiearlytonight,'\\d{1,}') # Extracts just the number from the tonight pair
  aetondesc<-str_extract(adiearlytonight,regex('[:alpha:]{1,}[:space:]{1}[:alpha:]{1,}|[:alpha:]{1,}')) # Extracts the description after the tonight pair
  adiearlytomorrow<-trimws(adiearlysplit[[1]][3],whitespace=" ") # Third pair is the "tomorrow" column, take the pair and remove any spaces before and after the text
  aetomvalue<-str_extract(adiearlytomorrow,'\\d{1,}') # Extracts just the number from the tonight pair
  aetomdesc<-str_extract(adiearlytomorrow,regex('[:alpha:]{1,}[:space:]{1}[:alpha:]{1,}|[:alpha:]{1,}')) # Extracts the description after the tonight pair
  
  # ADI Late
  adilatesplit<-str_extract_all(adilate,'\\d{1,}\\s.{1,9}') # Code here is identical to the ADI Early code, process is the same as above
  adilatetoday<-trimws(adilatesplit[[1]][1],whitespace=" ")
  altodvalue<-str_extract(adilatetoday,'\\d{1,}')
  altoddesc<-str_extract(adilatetoday,regex('[:alpha:]{1,}[:space:]{1}[:alpha:]{1,}|[:alpha:]{1,}'))
  adilatetonight<-trimws(adilatesplit[[1]][2],whitespace=" ")
  altonvalue<-str_extract(adilatetonight,'\\d{1,}')
  altondesc<-str_extract(adilatetonight,regex('[:alpha:]{1,}[:space:]{1}[:alpha:]{1,}|[:alpha:]{1,}'))
  adilatetomorrow<-trimws(adilatesplit[[1]][3],whitespace=" ")
  altomvalue<-str_extract(adilatetomorrow,'\\d{1,}')
  altomdesc<-str_extract(adilatetomorrow,regex('[:alpha:]{1,}[:space:]{1}[:alpha:]{1,}|[:alpha:]{1,}'))
  
  if(is.na(adiearlytomorrow)==TRUE){
    aetomvalue<-0
    aetomdesc<-"Very Poor"
  }
  
  if(is.na(adilatetomorrow)==TRUE){
    altomvalue<-0
    altomdesc<-"Very Poor"
  }
  
  if(is.na(adiearlytonight)==TRUE){
    aetonvalue<-0
    aetondesc<-"Very Poor"
  }
  
  if(is.na(adilatetonight)==TRUE){
    altonvalue<-0
    altondesc<-"Very Poor"
  }
  
  if(is.na(adiearlytoday)==TRUE){
    aetodvalue<-0
    aetoddesc<-"Very Poor"
  }
  
  if(is.na(adilatetoday)==TRUE){
    altodvalue<-0
    altoddesc<-"Very Poor"
  }
  
  # These if-else statements change the descriptions of the ADIs if 
  # they are "Gen Poor" or "Gen Good" to "Generally Poor" or "Generally Good"
  
  if(aetoddesc=="Gen Poor"){
    aetoddesc<-"Generally Poor"
  } else if (aetoddesc=="Gen Good"){
    aetoddesc<-"Generally Good"
  }
  if(altoddesc=="Gen Poor"){
    altoddesc<-"Generally Poor"
  } else if (altoddesc=="Gen Good"){
    altoddesc<-"Generally Good"
  }
  
  if(aetondesc=="Gen Poor"){
    aetondesc<-"Generally Poor"
  } else if (aetondesc=="Gen Good"){
    aetondesc<-"Generally Good"
  }
  
  if(altondesc=="Gen Poor"){
    altondesc<-"Generally Poor"
  } else if (altondesc=="Gen Good"){
    altondesc<-"Generally Good"
  }
  
  if(aetomdesc=="Gen Poor"){
    aetomdesc<-"Generally Poor"
  } else if (aetomdesc=="Gen Good"){
    aetomdesc<-"Generally Good"
  }
  
  if(altomdesc=="Gen Poor"){
    altomdesc<-"Generally Poor"
  } else if (altomdesc=="Gen Good"){
    altomdesc<-"Generally Good"
  }
  
  # These lines combine the number and description into what will be shown on the report
  todaymorning<-paste(aetoddesc,"-",aetodvalue)
  todayafternoon<-paste(altoddesc,"-",altodvalue)
  tonightevening<-paste(aetondesc,"-",aetonvalue)
  tonightovernight<-paste(altondesc,"-",altonvalue)
  tomorrowmorning<-paste(aetomdesc,"-",aetomvalue)
  tomorrowafternoon<-paste(altomdesc,"-",altomvalue)
} else {
  todaymorning<-"--"
  todayafternoon<-"--"
  tonightevening<-"--"
  tonightovernight<-"--"
  tomorrowmorning<-"--"
  tomorrowafternoon<-"--"
}
# Website 5
# Data is scraped here to calculate the Inversion Strength and Inversion Depths for the day
# Website is updated at 7AM every day

# if the hour is before 8 am
if (as.numeric(h)<8){
  yesterday<-strptime(with_tz(Sys.Date()-1,tzone="US/Eastern"),"%Y-%m-%d") # Using yesterday's date if website hasn't updated yet
  y5<-as.character(format(yesterday,"%Y"))
  m5<-as.character(format(yesterday,"%m"))
  d5<-as.character(format(yesterday,"%d"))
  # Place the year, month, and day values into the link to get the data for the day
  link5<-paste("http://weather.uwyo.edu/cgi-bin/sounding?region=naconf&TYPE=TEXT%3ALIST&YEAR=",y5,
               "&MONTH=",m5,"&FROM=",d5,"12&TO=",d5,"12&STNM=72520",sep="") 
} else {
  link5<-paste("http://weather.uwyo.edu/cgi-bin/sounding?region=naconf&TYPE=TEXT%3ALIST&YEAR=",y,
               "&MONTH=",m,"&FROM=",d,"12&TO=",d,"12&STNM=72520",sep="")
}

page5<-tryCatch(read_html(link5),error=function(y){return(c())})# Read link, return a blank vector if the website is down
if (is_empty(page5)==FALSE){
  table5<-page5 %>% # Extract the node containing the data, which is the whole table in this case
    html_nodes("pre") %>%
    html_text()
} else {
  table5<-c()
}

fivestrength<-function(x){ # Create a description value for how strong the Surface Inversion Strength is based on its value
  if (x==0){
    return("None")
  } else if (x>0 & x<1){
    return("Slight")
  } else if (x>=1 & x<3){
    return("Weak")
  } else if (x>=3 & x<5){
    return("Moderate")
  } else return("Strong")
}

# if the table is not empty
if (is_empty(table5)==FALSE){
  fiveextract<-str_extract_all(table5,'\\n.{22}') # Extracts and splits each row of data
  fivenum<-fiveextract[[1]][-c(1:4)] # Removes the first four rows
  digits<-trimws(substr(fivenum,3,22),which=c("left")) # Removes the new line characters at the start and any whitespace characters
  digitssplit<-str_extract_all(digits,'.{1}\\d{1,}.{1}\\d|\\d{1,}.{1}\\d') # Splits the numbers in each row
  pressure<-lapply(digitssplit,`[[`,1) # Label first column values as "pressure"
  height<-lapply(digitssplit,`[[`,2) # Label second column values as "height"
  digitssplit[[1]][3]<-"" # Add an empty string for the third entry in third column since it's not present
  temperature<-lapply(digitssplit,`[[`,3) # Label third column values as "temperature"
  five<-cbind(as.numeric(pressure),as.numeric(height),as.numeric(temperature)) # Combine these variables into a new data frame
  colnames(five)<-c("Pressure (hPa)","Height (m)","Temperature (C)") # Add names to each column with their units of measurement
  five<-five[-1,] # Remove first row, since it has no temperature value
  
  # Calculation for Surface Inversion Strength and Inversion Depth
  tempdiff<-diff(five[,3]) # Create a differenced list of the temperature column, each entry subtracted from the next
  peaktemp<-five[which(tempdiff<0),3][1]
  mintempheight<-as.numeric(five[which(five[,3]==peaktemp[1]),2])[1]
  surfaceinversion<-as.numeric(peaktemp-five[1,3]) # If the temperature increases as height increases, take the peak temperature and subtract it from the surface temperature to get Surface Inversion Strength
  inversiondepth<-as.numeric(mintempheight-five[1,2])
  
  
  ## To Calculate Break Time
  breaktemp<-(((inversiondepth/100)+five[which(tempdiff<0),3][1])*9/5)+32 # Take this number and match it to the weather forecast. The time of day when this temperature is reached is the break time.
  
  ## Calculations for Surface Inversion Breaks
  temp5 <- paste(round(surfaceinversion,1),"°C")
  depth5 <- paste((inversiondepth),"m")
  time5 <- "--"
  scale5<- fivestrength(surfaceinversion)
  mode<-"observations"
  
  # Determining if there are any upper inversions
  sentence<-five[which(five[,2]<1000),] # Take all temperature values below 1000 m
  tempdiffunder1k<-diff(rle(sentence[,3])$values) # Make differenced list
  e<-which(tempdiffunder1k<0) # Find any differences less than 0
  f<-diff(e) # Make second differenced list
  g<-which(f>1) # Find any values greater than 1
  
  upperinversion<-function(){ # Function to detect any inversion that is not a surface inversion and print "Yes" or "No"
    if (is.na(tempdiffunder1k[e[g[1]+1]])){
      return("No upper inversion starting below ~1000 m is reported")
    } else return("Yes, an upper inversion starting below ~1000 m is reported")
  }
  
  inversion5 <- upperinversion()
  
} else {
  link5<-paste("https://rucsoundings.noaa.gov/get_soundings.cgi?data_source=GFS&latest=latest&start_year=",y,"&start_month_name=",month.abb[as.numeric(m)],"&start_mday=",as.numeric(d),"&start_hour=",h,"&start_min=0&n_hrs=1.0&fcst_len=shortest&airport=PIT&text=Ascii%20text%20%28GSL%20format%29&hydrometeors=false&start=latest",sep="")
  page5<-read_html(link5) # Read link
  table5<-page5 %>% # Extract the node containing the data, which is the whole table in this case
    html_nodes("p") %>%
    html_text()
  if(is_empty(table5)==FALSE){
    fiveextract<-str_extract_all(table5,'\\d.{1,}') # Remove all new line characters
    fiverows<-fiveextract[[1]][-c(1:6)] # Remove first 6 rows since important data starts on row 7
    splitfive<-str_extract_all(fiverows,'-\\d{1,}|\\d{1,}') # Separate all numbers into their own entries
    type<-lapply(splitfive,`[[`,1) # Label first column as "Type"
    pressure<-lapply(splitfive,`[[`,2) # Label second column as "Pressure"
    height<-lapply(splitfive,`[[`,3) # Label third column as "Height"
    temp<-lapply(splitfive,`[[`,4) # Label fourth column as "temp"
    dewpoint<-lapply(splitfive,`[[`,5) # Label fifth column as "Dewpoint"
    winddirection<-lapply(splitfive,`[[`,6) # Label sixth column as "Wind Direction"
    windspeed<-lapply(splitfive,`[[`,7) # Label seventh column as "Wind Speed"
    
    # Create data frame with all the columns combined. Some columns need their data modified by dividing by 10
    five<-as.data.frame(cbind(type,as.numeric(pressure)/10,height,as.numeric(temp)/10,as.numeric(dewpoint)/10,winddirection,windspeed)) 
    
    colnames(five)<-c("Type","Pressure (mb)","Height (m)","Temperature (C)","Dew Point (C)","Wind Direction (Degrees)","Wind Speed (Knots)") # Give each column their names and units
    tempdifffive<-diff(unlist(five[,4])) # Create a differenced list of the temperature column, each entry subtracted from the next
    peaktempalt<-five[which(tempdifffive<0),4][1] 
    mintempheightalt<-as.numeric(five[which(five[,4]==as.numeric(peaktempalt[1])),3])[1]
    surfaceinversion<-as.numeric(peaktempalt[[1]]-five[1,4][[1]]) # If the temperature increases as height increases, take the peak temperature and subtract it from the surface temperature to get Surface Inversion Strength
    inversiondepth<-as.numeric(mintempheightalt[[1]]-as.numeric(five[1,3][[1]])) # Take the height of the peak temperature and subtract the surface height (359 m) to get Inversion Depth
    
    breaktemp<-(((inversiondepth/100)+five[which(tempdifffive<0),4][[1]])*9/5)+32 # Take this number and match it to the weather forecast. The time of day when this temperature is reached is the break time.
    
    # Determining if there are any upper inversions
    sentence<-five[which(five[,3]<1000),] # Take all temperature values below 1000 m
    tempdiffunder1k<-diff(rle(unlist(sentence[,4]))$values) # Make differenced list
    e<-which(tempdiffunder1k<0) # Find any differences less than 0
    f<-diff(e) # Make second differenced list
    g<-which(f>1) # Find any values greater than 1
    upperinversion<-function(){ # Function to detect any inversion that is not a surface inversion and print "Yes" or "No"
      if (is.na(tempdiffunder1k[e[g[1]+1]])){
        return("No upper inversion starting below ~1000 m is reported")
      } else return("Yes, an upper inversion starting below ~1000 m is reported")
    }
    temp5 <- paste(round(surfaceinversion,1),"°C")
    depth5 <- paste((inversiondepth),"m")
    time5 <- "--"
    scale5<- fivestrength(surfaceinversion)
    mode<-"forecast"
    inversion5<-upperinversion()
    if(is.na(breaktemp)==FALSE){
      for(i in 2:25){
        if(as.numeric(p4_1[2,i])!=23){
          if(abs(as.numeric(p4_1[3,i])-breaktemp)<=2){
            if(as.numeric(p4_1[2,i])<12){
              time5=as.numeric(p4_1[2,i])
              time5=paste(time5," am",sep="")}
            break
          }
          else{break}
          
        }
        
      }
    }
    
  } else{
    surfaceinversion<-"--"
    inversiondepth<-"--"
    inversion5<-"--"
    temp5<-paste("--","°C")
    depth5<-paste("--","m")
    time5<-"--"
    scale5<-"--"
    mode<-"N/A because data could not be collected today"
  }
}

# --------------------------------------------------------------------------------------------------------------------------
# Website 6
# Used to calculate the Inversion Strength for the next day
# Website is updated at 7AM every day 
# --------------------------------------------------------------------------------------------------------------------------

if (as.numeric(h)<8){
  link6<-paste("https://rucsoundings.noaa.gov/get_soundings.cgi?data_source=GFS&start_year=",y,
               "&start_month_name=",month.abb[as.numeric(m)],"&start_mday=",as.numeric(d),
               "&start_hour=12&start_min=0&n_hrs=1&fcst_len=shortest&airport=PIT&text=Ascii%20text%20%28GSL%20format%29&hydrometeors=false&startSecs=",
               as.numeric(as.POSIXct(Sys.Date()))+30000,"&endSecs=",as.numeric(as.POSIXct(Sys.Date()))+33600,sep="") # Link with the system's year, month, day, and epoch times
} else {
  link6<-paste("https://rucsoundings.noaa.gov/get_soundings.cgi?data_source=GFS&start_year=",y,
               "&start_month_name=",month.abb[as.numeric(m)],"&start_mday=",as.numeric(d)+1,
               "&start_hour=12&start_min=0&n_hrs=1&fcst_len=shortest&airport=PIT&text=Ascii%20text%20%28GSL%20format%29&hydrometeors=false&startSecs=",
               as.numeric(as.POSIXct(Sys.Date()+1))+43200,"&endSecs=",as.numeric(as.POSIXct(Sys.Date()+1))+46800,sep="")
}

page6<-tryCatch(read_html(link6),error = function(y){return(c())}) # Read link, return a blank vector if the website is down
if (is_empty(page6)==FALSE){
  table6<-page6 %>% # Select nodes with the needed data which is the full table
    html_nodes("p") %>%
    html_text()
} else {
  table6<-c()
}

if(is_empty(table6)==FALSE){
  sixextract<-str_extract_all(table6,'\\d.{1,}') # Remove all new line characters
  sixrows<-sixextract[[1]][-c(1:6)] # Remove first 6 rows since important data starts on row 7
  splitsix<-str_extract_all(sixrows,'-\\d{1,}|\\d{1,}') # Separate all numbers into their own entries
  type<-lapply(splitsix,`[[`,1) # Label first column as "Type"
  pressure<-lapply(splitsix,`[[`,2) # Label second column as "Pressure"
  height<-lapply(splitsix,`[[`,3) # Label third column as "Height"
  temp<-lapply(splitsix,`[[`,4) # Label fourth column as "temp"
  dewpoint<-lapply(splitsix,`[[`,5) # Label fifth column as "Dewpoint"
  winddirection<-lapply(splitsix,`[[`,6) # Label sixth column as "Wind Direction"
  windspeed<-lapply(splitsix,`[[`,7) # Label seventh column as "Wind Speed"
  
  # Create data frame with all the columns combined. Some columns need their data modified by dividing by 10
  six<-as.data.frame(cbind(type,as.numeric(pressure)/10,height,as.numeric(temp)/10,as.numeric(dewpoint)/10,winddirection,windspeed)) 
  
  colnames(six)<-c("Type","Pressure (mb)","Height (m)","Temperature (C)","Dew Point (C)","Wind Direction (Degrees)","Wind Speed (Knots)") # Give each column their names and units
  tempdiffsix<-diff(unlist(six[,4])) # Create a differenced list of the temperature column, each entry subtracted from the next
  peaktempalt<-six[which(tempdiffsix<0),4][1] 
  mintempheightalt<-as.numeric(six[which(six[,4]==as.numeric(peaktempalt[1])),3])[1]
  surfaceinversion2<-as.numeric(peaktempalt[[1]]-six[1,4][[1]]) # If the temperature increases as height increases, take the peak temperature and subtract it from the surface temperature to get Surface Inversion Strength
  inversiondepth<-as.numeric(mintempheightalt[[1]]-as.numeric(six[1,3][[1]])) # Take the height of the peak temperature and subtract the surface height (359 m) to get Inversion Depth
  
  sixout<-function(x){ # Create a description value for how strong the Surface Inversion Strength is based on its value
    if (x==0){
      return("None")
    } else if (x>0 & x<1){
      return("Slight")
    } else if (x>=1 & x<3){
      return("Weak")
    } else if (x>=3 & x<5){
      return("Moderate")
    } else return("Strong")
  }
} else{
  sixout<-function(x){
    return("N/A")
  }
}



# --------------------------------------------------------------------------------------------------------------------------
# generate current system time for report's title
currentDate <- Sys.Date()
title <- paste("Air Quality Forecast and Dispersion Outlook \\\\of Allegheny County, Pennsylvania for", as.character(currentDate))

# AQI
# define the conditions for AQI
aqi_index<-function(x){
  x = as.numeric(x)
  if (x>=0 & x<=50){
    "Good"
  } else if (x>=51 & x<=100){
    "Moderate"
  } else if (x>=101 & x<=150){
    "Unhealthy for Sensitive Groups"
  } else "Unhealthy" 
}

# integrate the scraped data for Air Quality Forecast table
aqi <- data.frame(
  "Forecast Period"= c("Today","Tomorrow"),
  "Pittsburgh Area"=c(aqi_index(todaypitt),aqi_index(tomorrowpitt)),
  "Liberty-Clairton Area"=c(aqi_index(todayLC),aqi_index(tomorrowLC))
)

# the paragraph on the righ side of Air Quality Forecast table
aqi_forecast <- todayforecast[1]

# ADI
# integrate the data scraped into ACHD Air Dispersion 36-Hour Forecast table
adi <- data.frame(
  "Forecast Period" = c("Today Morning","Today Afternoon", "Tonight Evening", 
                        "Tonight Overnight", "Tomorrow Morning", "Tomorrow Afternoon"),
  "Atmospheric Dispersion Index" = c(todaymorning,todayafternoon,tonightevening,tonightovernight,tomorrowmorning,tomorrowafternoon),
  "Surface Inversion Strength" = c(fivestrength(surfaceinversion),"--","--","--",sixout(surfaceinversion2),"--"),
  "Wind(dir,mph)"=c(todaymorningwind,todayafternoonwind,todayeveningwind,todayovernightwind,tomorrowmorningwind,tomorrowafternoonwind))

result = c(aqi,aqi_forecast,adi,temp5,depth5,time5,scale5,inversion5,title)

# --------------------------------------------------------------------------------------------------------------------------
# Use to generate the variables in the .tex files
output = ""

# From here, we are generating the .tex file, you can add any varibles you like by using the format below.

AQI_pitt_today = paste("\\newcommand\\AQIPittToday{",todaypitt,"}",sep="")
output = paste(output,AQI_pitt_today,sep="\n")

AQI_pitt_tom = paste("\\newcommand\\AQIPittTom{",tomorrowpitt,"}",sep="")
output = paste(output,AQI_pitt_tom,sep="\n")

AQI_LC_today = paste("\\newcommand\\AQILCToday{",todayLC,"}",sep="")
output = paste(output,AQI_LC_today,sep="\n")

AQI_LC_tom = paste("\\newcommand\\AQILCTom{",tomorrowLC,"}",sep="")
output = paste(output,AQI_LC_tom,sep="\n")


AQI_pitt_today_cate = paste("\\newcommand\\AQIPittTodayCate{",result$Pittsburgh.Area[1],"}",sep="")
output = paste(output,AQI_pitt_today_cate,sep="\n")

AQI_pitt_tom_cate = paste("\\newcommand\\AQIPittTomCate{",result$Pittsburgh.Area[2],"}",sep="")
output = paste(output,AQI_pitt_tom_cate,sep="\n")

AQI_LC_today_cate = paste("\\newcommand\\AQILCTodayCate{",result$Liberty.Clairton.Area[1],"}",sep="")
output = paste(output,AQI_LC_today_cate,sep="\n")

AQI_LC_tom_cate = paste("\\newcommand\\AQILCTomCate{",result$Liberty.Clairton.Area[2],"}",sep="")
output = paste(output,AQI_LC_tom_cate,sep="\n")

AQI_pitt_today_color <- aqi_color(result$Pittsburgh.Area[1])

AQI_pitt_tom_color <- aqi_color(result$Pittsburgh.Area[2])

AQI_LC_today_color <- aqi_color(result$Liberty.Clairton.Area[1])

AQI_LC_tom_color <- aqi_color(result$Liberty.Clairton.Area[2])

AQI_pitt_today_pollutant = paste("\\newcommand\\AQIpitttodaypolutant{",todaypittpollutant,"}",sep="")
output = paste(output,AQI_pitt_today_pollutant,sep="\n")

AQI_pitt_tomorrow_pollutant= paste("\\newcommand\\AQIpitttomorrowpolutant{",tomorrowpittpollutant,"}",sep="")
output = paste(output,AQI_pitt_tomorrow_pollutant,sep="\n")

AQI_LC_today_pollutant= paste("\\newcommand\\AQILCtodaypolutant{",todayLCpollutant,"}",sep="")
output = paste(output,AQI_LC_today_pollutant,sep="\n")

AQI_LC_tomorrow_pollutant= paste("\\newcommand\\AQILCtomorrowpolutant{",tomorrowLCpollutant,"}",sep="")
output = paste(output,AQI_LC_tomorrow_pollutant,sep="\n")

discription = paste("\\newcommand\\Discriptions{",result[[4]][1],"}",sep="")
output = paste(output,discription,sep="\n")

ADIone = paste("\\newcommand\\ADIone{",result$Atmospheric.Dispersion.Index[1],"}",sep="")
output = paste(output,ADIone,sep="\n")

ADItwo = paste("\\newcommand\\ADItwo{",result$Atmospheric.Dispersion.Index[2],"}",sep="")
output = paste(output,ADItwo,sep="\n")

ADIthree = paste("\\newcommand\\ADIthree{",result$Atmospheric.Dispersion.Index[3],"}",sep="")
output = paste(output,ADIthree,sep="\n")

ADIfour = paste("\\newcommand\\ADIfour{",result$Atmospheric.Dispersion.Index[4],"}",sep="")
output = paste(output,ADIfour,sep="\n")

ADIfive = paste("\\newcommand\\ADIfive{",result$Atmospheric.Dispersion.Index[5],"}",sep="")
output = paste(output,ADIfive,sep="\n")

ADIsix = paste("\\newcommand\\ADIsix{",result$Atmospheric.Dispersion.Index[6],"}",sep="")
output = paste(output,ADIsix,sep="\n")

SISone = paste("\\newcommand\\SISone{",result$Surface.Inversion.Strength[1],"}",sep="")
output = paste(output,SISone,sep="\n")

SIStwo = paste("\\newcommand\\SIStwo{",result$Surface.Inversion.Strength[2],"}",sep="")
output = paste(output,SIStwo,sep="\n")

SISthree = paste("\\newcommand\\SISthree{",result$Surface.Inversion.Strength[3],"}",sep="")
output = paste(output,SISthree,sep="\n")

SISfour = paste("\\newcommand\\SISfour{",result$Surface.Inversion.Strength[4],"}",sep="")
output = paste(output,SISfour,sep="\n")

SISfive = paste("\\newcommand\\SISfive{",result$Surface.Inversion.Strength[5],"}",sep="")
output = paste(output,SISfive,sep="\n")

SISsix = paste("\\newcommand\\SISsix{",result$Surface.Inversion.Strength[6],"}",sep="")
output = paste(output,SISsix,sep="\n")

Windone = paste("\\newcommand\\Windone{",result$Wind.dir.mph.[1],"}",sep="")
output = paste(output,Windone,sep="\n")

Windtwo = paste("\\newcommand\\Windtwo{",result$Wind.dir.mph.[2],"}",sep="")
output = paste(output,Windtwo,sep="\n")

Windthree = paste("\\newcommand\\Windthree{",result$Wind.dir.mph.[3],"}",sep="")
output = paste(output,Windthree,sep="\n")

Windfour = paste("\\newcommand\\Windfour{",result$Wind.dir.mph.[4],"}",sep="")
output = paste(output,Windfour,sep="\n")

Windfive = paste("\\newcommand\\Windfive{",result$Wind.dir.mph.[5],"}",sep="")
output = paste(output,Windfive,sep="\n")

Windsix = paste("\\newcommand\\Windsix{",result$Wind.dir.mph.[6],"}",sep="")
output = paste(output,Windsix,sep="\n")

Temp = paste("\\newcommand\\Temp{",result[[9]][1],"}",sep="")
output = paste(output,Temp,sep="\n")

Depth = paste("\\newcommand\\Depth{",result[[10]][1],"}",sep="")
output = paste(output,Depth,sep="\n")

Time = paste("\\newcommand\\Time{",result[[11]][1],"}",sep="")
output = paste(output,Time,sep="\n")

Scale = paste("\\newcommand\\Scale{",result[[12]][1],"}",sep="")
output = paste(output,Scale,sep="\n")

Inversion = paste("\\newcommand\\Inversion{",result[[13]][1],"}",sep="")
output = paste(output,Inversion,sep="\n")

Title = paste("\\newcommand\\Title{",result[[14]][1],"}",sep="")
output = paste(output,Title,sep="\n")

AQI_pitt_today_color = paste("\\newcommand\\AQIPittTodayColor{",AQI_pitt_today_color,"}",sep="")
output = paste(output,AQI_pitt_today_color,sep="\n")

AQI_pitt_tom_color = paste("\\newcommand\\AQIPittTomColor{",AQI_pitt_tom_color,"}",sep="")
output = paste(output,AQI_pitt_tom_color,sep="\n")

AQI_LC_today_color = paste("\\newcommand\\AQILCTodayColor{",AQI_LC_today_color,"}",sep="")
output = paste(output,AQI_LC_today_color,sep="\n")

AQI_LC_tom_color = paste("\\newcommand\\AQILCTomColor{",AQI_LC_tom_color,"}",sep="")
output = paste(output,AQI_LC_tom_color,sep="\n")


AQI_pitt_today_Date = paste("\\newcommand\\AQIDateToday{",AQIDateToday,"}",sep="")
output = paste(output,AQI_pitt_today_Date,sep="\n")

AQI_pitt_tom_Date = paste("\\newcommand\\AQIDateTom{",AQIDateTom,"}",sep="")
output = paste(output,AQI_pitt_tom_Date,sep="\n")

AQI_LC_today_Week = paste("\\newcommand\\AQIWeekToday{",AQIWeekToday,"}",sep="")
output = paste(output,AQI_LC_today_Week,sep="\n")

AQI_LC_tom_Week = paste("\\newcommand\\AQIWeekTom{",AQIWeekTom,"}",sep="")
output = paste(output,AQI_LC_tom_Week,sep="\n")

# write the output to the folder data-raw
writeLines(output,paste0("data-raw/data_", make.names(h), ".tex"))












