
# Load libraries
library(tidyverse)
library(leaflet)
library(lubridate)
library(hrbrthemes)

options(stringsAsFactors = FALSE)

# Read in MBTA performance file
rawdata <- readRDS(file = "./TDashboardData_reliability_20160301-20160331.rds")

# Select Peak Service rows for the Green Line, remove columns of data we don't need
reliability <- rawdata %>% 
  filter(ROUTE_TYPE == "Green Line",
         PEAK_OFFPEAK_IND == "Peak Service (Weekdays 6:30-9:30AM, 3:30PM-6:30PM)") %>% 
  select(SERVICE_DATE, ROUTE_TYPE, STOP, OTP_NUMERATOR, OTP_DENOMINATOR)

# Remove  extra spaces around stop
reliability <- reliability %>% 
  mutate(STOP = trimws(STOP, which = c("right")))

# Find the reliability at each station
# Divide the numerator (people who has to wait too long) by the denominator (all riders).
# 1 - this ratio is the percentage of people who didn't wait, round by 4
reliability <-reliability %>% 
  mutate(rely = round(1 - (OTP_NUMERATOR / OTP_DENOMINATOR), 4))

# Import the MBTA station location data
stations <- read_csv("./mbta_stations.csv")

# Select only the Green Line data
locs <- stations %>% 
  filter(LINE == "GREEN") %>% 
  rename("STOP" = STATION)

# Join the location data to the reliability file, select only the columns need
joindata <- left_join(reliability, locs, by = "STOP") %>% 
  select(STOP, SERVICE_DATE, ROUTE_TYPE, rely, LONGITUDE, LATITUDE) %>% 
  filter(!duplicated(STOP))

# Map the Green Line
# Map the stations
# Create a ColorBrewer Greens color palette. 
colorpal <- colorNumeric(palette = "Greens", domain = joindata$rely)

# Lat Long coordinates from www.latlong.net
mbta_subway <- leaflet(joindata) %>%
  addTiles() %>%  
  setView(-71.057083, 42.361145, zoom = 12) %>%
  addCircles(~LONGITUDE, ~LATITUDE, weight = 3, radius = 120, 
             color = ~colorpal(rely), stroke = TRUE, fillOpacity = 0.8) %>% 
  addLegend("topright", colorpal, values = ~rely, 
            title = "MBTA Green Line Reliability<br>Data Source:<br>MBTA Developer Portal")

# Show the map
mbta_subway  
