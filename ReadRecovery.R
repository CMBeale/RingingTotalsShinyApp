# Extracting information from BTO DemOn .html recovery reports
# Stephen Vickers - 08/07/2022 (Updated for Linux server locale & UTF-8 robustness)

if (!require('rvest')) install.packages('rvest'); library(rvest)
if (!require('tidyr')) install.packages('tidyr'); library(tidyr)
if (!require('stringr')) install.packages('stringr'); library(stringr)
if (!require('lubridate')) install.packages('lubridate'); library(lubridate)
if (!require('geosphere')) install.packages('geosphere'); library(geosphere)

# Helper function to convert messy HTML DMS coordinate strings to Decimal Degrees
parse_dms_to_dec <- function(coord_str) {
  if (is.na(coord_str) || !nzchar(trimws(coord_str))) return(NA_real_)
  
  # Normalize character encoding & non-breaking spaces
  s <- gsub("\u00a0", " ", coord_str)
  s <- iconv(s, from = "UTF-8", to = "ASCII//TRANSLIT", sub = " ")
  
  # Identify directional sign (South or West indicates negative)
  is_negative <- grepl("[SWsw-]", s)
  
  # Keep only numeric digits, decimals, and spaces
  s_clean <- gsub("[^0-9.]+", " ", s)
  s_clean <- str_squish(s_clean)
  
  nums <- as.numeric(unlist(strsplit(s_clean, " ")))
  nums <- nums[!is.na(nums)]
  
  if (length(nums) == 0) return(NA_real_)
  
  deg <- nums[1]
  min <- ifelse(length(nums) >= 2, nums[2], 0)
  sec <- ifelse(length(nums) >= 3, nums[3], 0)
  
  dec <- deg + (min / 60) + (sec / 3600)
  if (is_negative) dec <- -dec
  
  return(dec)
}

# Helper to robustly parse date strings across different server locales
parse_recovery_date <- function(date_str) {
  if (is.na(date_str) || !nzchar(trimws(date_str))) return(as.Date(NA))
  
  # Clean printable ASCII characters & strip spaces
  d_clean <- gsub("[^\x20-\x7E]", "", date_str)
  d_clean <- str_squish(d_clean)
  
  parsed <- suppressWarnings(lubridate::dmy(d_clean, quiet = TRUE))
  if (is.na(parsed)) {
    parsed <- suppressWarnings(lubridate::ymd(d_clean, quiet = TRUE))
  }
  return(as.Date(parsed))
}

read_recovery <- function(bto_html_file) {
  # Read HTML file using UTF-8 encoding
  df <- read_html(bto_html_file, encoding = "UTF-8")
  
  # Create structured data frame
  data <- data.frame(
    Species = character(2),
    Latin = character(2),
    Event = character(2),
    Lat = numeric(2),
    Lon = numeric(2),
    Distance = character(2),
    Ring_no = character(2),
    Date = as.Date(rep(NA, 2)),
    Site = character(2),
    Age = character(2),
    Sex = character(2),
    Duration = character(2),
    Direction = character(2),
    Remarks = character(2),
    Recovery_type = character(2),
    stringsAsFactors = FALSE
  )
  
  # --- 1. RINGING PLACE COORDINATES & RINGING EVENT ---
  myurl <- df %>% html_nodes(".ringingPlaceSection.spanRow") %>% html_text2()
  parts <- str_split(myurl, "\r ", n = 5)
  
  northing_raw <- tryCatch(str_split(parts[[1]][4], ": ")[[1]][2], error = function(e) NA_character_)
  easting_raw  <- tryCatch(str_split(parts[[1]][5], " Accu")[[1]][1], error = function(e) NA_character_)
  
  data$Lat[1]   <- parse_dms_to_dec(northing_raw)
  data$Lon[1]   <- parse_dms_to_dec(easting_raw)
  data$Event[1] <- 'Ringed'
  
  # --- 2. SPECIES & RING NO ---
  myurl <- df %>% html_nodes(".quickSummarySection") %>% html_text2()
  parts <- str_split(myurl, "\r ", n = 20)
  
  spec_raw <- tryCatch(parts[[1]][6], error = function(e) "")
  spec_split <- str_split(spec_raw, " [(]", n = 2)[[1]]
  data$Species <- spec_split[1]
  data$Latin   <- ifelse(length(spec_split) > 1, gsub("\\)", "", spec_split[2]), NA_character_)
  
  data$Ring_no <- tryCatch(parts[[1]][18], error = function(e) NA_character_)
  
  # --- 3. DATES ---
  myurl <- df %>% html_nodes(".ringingDateSection.spanRow") %>% html_text2()
  parts <- str_split(myurl, " ", n = 20)
  date1_raw <- tryCatch(parts[[1]][4], error = function(e) NA_character_)
  
  myurl_span <- df %>% html_nodes(".spanRow") %>% html_text2()
  date2_raw  <- tryCatch(str_split(myurl_span[9], " ", n = 20)[[1]][4], error = function(e) NA_character_)
  
  data$Date[1] <- parse_recovery_date(date1_raw)
  data$Date[2] <- parse_recovery_date(date2_raw)
  
  # --- 4. FINDING COORDINATES ---
  myurl2 <- df %>% html_nodes(".findingCountyAndCoordsSection.spanRow") %>% html_text2()
  parts  <- str_split(myurl2, "\r ", n = 5)
  
  northing_raw2 <- tryCatch(str_split(parts[[1]][4], ": ")[[1]][2], error = function(e) NA_character_)
  easting_raw2  <- tryCatch(str_split(parts[[1]][5], " Accu")[[1]][1], error = function(e) NA_character_)
  
  data$Lat[2]   <- parse_dms_to_dec(northing_raw2)
  data$Lon[2]   <- parse_dms_to_dec(easting_raw2)
  data$Event[2] <- 'Re-encountered'
  
  # --- 5. SITES ---
  myurl <- df %>% html_nodes(".regPlaceCodeSection") %>% html_text2()
  parts <- str_split(myurl, "\r ", n = 20)
  loc1  <- tryCatch(str_split(str_split(parts[[1]][4], "name: ")[[1]][2], "\r")[[1]][1], error = function(e) NA_character_)
  
  loc2  <- tryCatch(str_split(str_split(str_split(myurl_span[10], "\r ", n = 20)[[1]][4], "name: ")[[1]][2], "\r")[[1]][1], error = function(e) NA_character_)
  data$Site <- c(loc1, loc2)
  
  # --- 6. DISTANCE ---
  data$Distance[1] <- '0km'
  if (!is.na(data$Lon[1]) && !is.na(data$Lat[1]) && !is.na(data$Lon[2]) && !is.na(data$Lat[2])) {
    dist_m <- geosphere::distHaversine(c(data$Lon[1], data$Lat[1]), c(data$Lon[2], data$Lat[2]))
    data$Distance[2] <- paste0(round(dist_m / 1000, 1), 'km')
  } else {
    data$Distance[2] <- NA_character_
  }
  
  # --- 7. AGE & SEX ---
  myurl_age <- df %>% html_nodes(".ageSexSection.spanRow") %>% html_text2()
  myurl_ring_unv <- df %>% html_nodes(".ringNotVerifiedSection.spanRow") %>% html_text2()
  
  parts_age1 <- str_split(myurl_age, "\r ", n = 20)
  age1 <- tryCatch(str_split(parts_age1[[1]][2], ": ")[[1]][2], error = function(e) NA_character_)
  sex1 <- tryCatch(str_split(parts_age1[[1]][3], ": ")[[1]][2], error = function(e) NA_character_)
  
  parts_age2 <- str_split(myurl_ring_unv, "\r ", n = 20)
  age2 <- tryCatch(str_split(parts_age2[[1]][3], ": ")[[1]][2], error = function(e) NA_character_)
  sex2 <- tryCatch(str_split(parts_age2[[1]][4], ": ")[[1]][2], error = function(e) NA_character_)
  
  data$Age <- c(age1, age2)
  data$Sex <- c(sex1, sex2)
  
  # --- 8. DURATION & DIRECTION ---
  myurl_dur <- df %>% html_nodes(".distanceDurationDirectionSection.spanRow") %>% html_text2()
  parts_dur <- str_split(myurl_dur, "\r ", n = 20)
  
  duration2  <- tryCatch(str_split(parts_dur[[1]][2], ": ")[[1]][2], error = function(e) NA_character_)
  direction2 <- tryCatch(str_split(str_split(parts_dur[[1]][4], ": ")[[1]][2], "\r")[[1]][1], error = function(e) NA_character_)
  
  data$Duration  <- c(NA_character_, duration2)
  data$Direction <- c(NA_character_, direction2)
  
  # --- 9. REMARKS & RECOVERY TYPE ---
  myurl_rem <- df %>% html_nodes(".findingBirdRemarks.spanRow") %>% html_text2()
  remarks1  <- tryCatch(str_split(str_split(parts[[1]][2], ": ")[[1]][2], "\r")[[1]][1], error = function(e) NA_character_)
  remarks2  <- tryCatch(str_split(str_split(str_split(myurl_rem, "\r ", n = 20)[[1]][2], ": ")[[1]][2], "\r")[[1]][1], error = function(e) NA_character_)
  
  data$Remarks <- c(remarks1, remarks2)
  
  myurl_cond <- df %>% html_nodes(".findingBirdCondition.spanRow") %>% html_text2()
  parts_cond <- str_split(myurl_cond, "\r ", n = 20)
  rt2 <- tryCatch({
    p <- parts_cond[[1]][2:3]
    p_split <- str_split(p, "\r", n = 20)
    paste(p_split[[1]][1], p_split[[2]][1])
  }, error = function(e) NA_character_)
  
  data$Recovery_type <- c(NA_character_, rt2)
  
  return(data)
}