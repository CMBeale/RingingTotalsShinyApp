# Extracting information from BTO DemOn .html recovery reports #
# Stephen Vickers - 08/07/2022

if (!require('rvest')) install.packages('rvest'); library(rvest)
if (!require('tidyr')) install.packages('tidyr'); library(tidyr)
if (!require('stringr')) install.packages('stringr'); library(stringr)
if (!require('lubridate')) install.packages('lubridate'); library(lubridate)
if (!require('geosphere')) install.packages('geosphere'); library(geosphere)
if (!require('measurements')) install.packages('measurements'); library(measurements)

read_recovery <- function(bto_html_file) {
  df <- read_html(bto_html_file) # Read in in .html recovery form
  
  
  # Create data frame for data
  data <- data.frame(matrix(nrow=2, ncol=15))
  colnames(data) <- c('Species','Latin','Event', 'Lat', 'Lon', 'Distance', 'Ring_no', 'Date', 'Site','Age', 'Sex', 'Duration', 'Direction', 'Remarks', 'Recovery_type')
  
  
  
  # General framework for pulling a section of the .html file into a character string
  
  myurl <- df %>%
    html_nodes(".ringingPlaceSection.spanRow") %>%
    html_text2()
  
  # To find .html section names I open up a .html recovery form in Chrome and right click on the part of the form you're interested in and select 'Inspect'. It will bring up the html viewer screen. It should take you to the html element for that section and as you hover over the code it will highlight the sections of the page it refers to.
  
  # We can ther split that string based on \r which is the seperator used between sections  
  parts <- str_split(myurl, "\r ", n = 5)
  
  
  # This format is repeated for all information needed
  
  northing <- parts[[1]][4]
  northing <- str_split(northing, ": ")
  northing <- northing[[1]][2]
  northing = gsub('deg', '', northing)
  northing = gsub('min ', ' 0', northing)
  
  easting <- parts[[1]][5]
  easting <- str_split(easting, " Accu", n = 2)
  easting <- easting[[1]][1]
  easting = gsub('deg', '', easting)
  easting = gsub('min ', ' 0', easting)
  
  ewns <- ifelse((str_extract(northing,"\\(?[EWNS,.]+\\)?") %in% c("E","N")) | (length(grep("-", northing))==0),"+","-")
  northing <- gsub('-', '', northing)
  dms <- str_sub(northing,1,str_length(northing)-1)
  df2 <- paste0(ewns,dms)
  if(substr(df2, 1, 2)=="--") df2 <- gsub('--', '-', df2)
  df_dec <- measurements::conv_unit(df2,from = 'deg_min_sec', to = 'dec_deg')
  
  lat <- as.numeric(df_dec)
  
  ewns <- ifelse((str_extract(easting,"\\(?[EWNS,.]+\\)?") %in% c("E","N")) | (length(grep("-", easting))==0),"+","-")
  easting <- gsub('-', '', easting)
  dms <- str_sub(easting,1,str_length(easting)-1)
  df2 <- paste0(ewns,dms)
  if(substr(df2, 1, 2)=="--") df2 <- gsub('--', '-', df2)
  df_dec <- measurements::conv_unit(df2,from = 'deg_min_sec', to = 'dec_deg')
  
  lon <- as.numeric(df_dec)
  
  data$Lat[1] <- lat
  data$Lon[1] <- lon
  data$Event[1] <- 'Ringed'
  
  myurl <- df %>%
    html_nodes(".quickSummarySection") %>%
    html_text2()
  
  parts <- str_split(myurl, "\r ", n = 20)
  spec <- parts[[1]][6]
  spec <- str_split(spec, " [(]", n = 2)
  spec <- spec[[1]][1]
  
  data$Species <- spec
  
  
  latin <- parts[[1]][6]
  latin <- str_split(latin, " [(]", n = 2)
  latin <- latin[[1]][2]
  latin <- substr(latin,1,nchar(latin)-1)
  data$Latin <- latin
  
  ring <- parts[[1]][18]
  data$Ring_no <- ring
  
  myurl <- df %>%
    html_nodes(".ringingDateSection.spanRow") %>%
    html_text2()
  parts <- str_split(myurl, " ", n = 20)
  date <- parts[[1]][4]
  data$Date[1] <- as.character(dmy(date))
  
  myurl <- df %>%
    html_nodes(".spanRow") %>%
    html_text2()
  myurl <- myurl[9]
  parts <- str_split(myurl, " ", n = 20)
  date <- parts[[1]][4]
  data$Date[2] <- as.character(dmy(date))
  data$Date <- ymd(data$Date)
  
  myurl2 <- df %>%
    html_nodes(".findingCountyAndCoordsSection.spanRow") %>%
    html_text2() 
  
  parts <- str_split(myurl2, "\r ", n = 5)
  
  northing <- parts[[1]][4]
  northing <- str_split(northing, ": ")
  northing <- northing[[1]][2]
  northing = gsub('deg', '', northing)
  northing = gsub('min ', ' 0', northing)
  
  easting <- parts[[1]][5]
  easting <- str_split(easting, " Accu", n = 2)
  easting <- easting[[1]][1]
  easting = gsub('deg', '', easting)
  easting = gsub('min ', ' 0', easting)
  
  ewns <- ifelse((str_extract(northing,"\\(?[EWNS,.]+\\)?") %in% c("E","N")) | (length(grep("-", northing))==0),"+","-")
  northing <- gsub('-', '', northing)
  dms <- str_sub(northing,1,str_length(northing)-1)
  df2 <- paste0(ewns,dms)
  if(substr(df2, 1, 2)=="--") df2 <- gsub('--', '-', df2)
  
  df_dec <- measurements::conv_unit(df2,from = 'deg_min_sec', to = 'dec_deg')
  
  lat <- as.numeric(df_dec)
  
  ewns <- ifelse((str_extract(easting,"\\(?[EWNS,.]+\\)?") %in% c("E","N")) | (length(grep("-", easting))==0),"+","-")
  easting <- gsub('-', '', easting)
  dms <- str_sub(easting,1,str_length(easting)-1)
  df2 <- paste0(ewns,dms)
  if(substr(df2, 1, 2)=="--") df2 <- gsub('--', '-', df2)
  
  df_dec <- measurements::conv_unit(df2,from = 'deg_min_sec', to = 'dec_deg')
  
  lon <- as.numeric(df_dec)
  
  data$Lat[2] <- lat
  data$Lon[2] <- lon
  data$Event[2] <- 'Re-encountered'
  
  
  myurl <- df %>%
    html_nodes(".regPlaceCodeSection") %>%
    html_text2()
  
  parts <- str_split(myurl, "\r ", n = 20)
  loc <- parts[[1]][4]
  loc <- str_split(loc, "name: ", n = 2)
  loc <- loc[[1]][2]
  loc <- str_split(loc, "\r", n = 2)
  loc <- loc[[1]][1]
  
  myurl <- df %>%
    html_nodes(".spanRow") %>%
    html_text2()
  
  myurl <- myurl[10]
  parts <- str_split(myurl, "\r ", n = 20)
  loc2 <- parts[[1]][4]
  loc2 <- str_split(loc2, "name: ", n = 2)
  loc2 <- loc2[[1]][2]
  loc2 <- str_split(loc2, "\r", n = 2)
  loc2 <- loc2[[1]][1]
  
  data$Site <- c(loc,loc2)
  
  
  data$Distance <- NA
  data$Distance[1] <- '0km'
  
  data$Distance[2] <- paste0(round(distHaversine(cbind(data$Lon, data$Lat))/1000,1),'km')
  
  myurl <- df %>%
    html_nodes(".ageSexSection.spanRow") %>%
    html_text2()
  
  myurl2 <- df %>%
    html_nodes(".ringNotVerifiedSection.spanRow") %>%
    html_text2()
  
  parts <- str_split(myurl, "\r ", n = 20)
  age1 <- parts[[1]][2]
  age1 <- str_split(age1, ": ", n = 20)
  age1 <- age1[[1]][2]
  
  sex1 <- parts[[1]][3]
  sex1 <- str_split(sex1, ": ", n = 20)
  sex1 <- sex1[[1]][2]
  
  parts <- str_split(myurl2, "\r ", n = 20)
  age2 <- parts[[1]][3]
  age2 <- str_split(age2, ": ", n = 20)
  age2 <- age2[[1]][2]
  sex2 <- parts[[1]][4]
  sex2 <- str_split(sex2, ": ", n = 20)
  sex2 <- sex2[[1]][2]
  
  data$Age[1] <- age1
  data$Age[2] <- age2
  
  data$Sex[1] <- sex1
  data$Sex[2] <- sex2
  
  myurl <- df %>%
    html_nodes(".distanceDurationDirectionSection.spanRow") %>%
    html_text2()
  
  parts <- str_split(myurl, "\r ", n = 20)
  duration2 <- parts[[1]][2]
  duration2 <- str_split(duration2, ": ", n = 20)
  duration2 <- duration2[[1]][2]
  
  parts <- str_split(myurl, "\r ", n = 20)
  direction2 <- parts[[1]][4]
  direction2 <- str_split(direction2, ": ", n = 20)
  direction2 <- direction2[[1]][2]
  direction2 <- str_split(direction2, "\r", n = 20)
  direction2 <- direction2[[1]][1]
  
  data$Duration[1] <- NA
  data$Duration[2] <- duration2
  
  data$Direction[1] <- NA
  data$Direction[2] <- direction2
  
  myurl <- df %>%
    html_nodes(".biometricsSection") %>%
    html_text2()
  
  myurl2 <- df %>%
    html_nodes(".findingBirdRemarks.spanRow") %>%
    html_text2()
  
  
  parts <- str_split(myurl[2], "\r ", n = 20)
  remarks1 <- parts[[1]][2]
  remarks1 <- str_split(remarks1, ": ", n = 20)
  remarks1 <- remarks1[[1]][2]
  remarks1 <- str_split(remarks1, "\r", n = 20)
  remarks1 <- remarks1[[1]][1]
  
  parts <- str_split(myurl2, "\r ", n = 20)
  remarks2 <- parts[[1]][2]
  remarks2 <- str_split(remarks2, ": ", n = 20)
  remarks2 <- remarks2[[1]][2]
  remarks2 <- str_split(remarks2, "\r", n = 20)
  remarks2 <- remarks2[[1]][1]
  
  data$Remarks[1] <- remarks1
  data$Remarks[2] <- remarks2
  
  myurl <- df %>%
    html_nodes(".findingBirdCondition.spanRow") %>%
    html_text2()
  
  parts <- str_split(myurl, "\r ", n = 20)
  rt2 <- parts[[1]][2:3]
  rt2 <- str_split(rt2, "\r", n = 20)
  rt2 <- paste(rt2[[1]][1],rt2[[2]][1])
  
  
  data$Recovery_type[1] <- NA
  data$Recovery_type[2] <- rt2
  
  return(data)
}