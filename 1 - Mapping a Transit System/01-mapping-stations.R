
# Load library
library(tidyverse)
library(leaflet)

# Read in MBTA Station file
rawlocs <- readRDS(file = "./MBTA_GTFS/stops_raw.rds")

# Select the columns we want and change columns name to latitude and longitude
station_locs <- rawlocs %>% 
  select("stop_id","stop_name","stop_lat","stop_lon") %>% 
  rename("latitude" = stop_lat,
         "longitude" = stop_lon)

# Convert the columns imported as a factor to characters
station_locs <- station_locs %>% 
  mutate_if(is.factor, as.character)

# Convert the Stop ID column into numbers, remove NAs
station_locs <- station_locs %>% 
  mutate(stop_id = as.numeric(stop_id)) %>% 
  filter(!is.na(stop_id))

# Select columns with MBTA T stations
station_locs <- station_locs %>% 
  filter(stop_id >= 70000 & stop_id <= 70279)

# Saint Paul Street Station names are altered to include their line.
# this change is done to be able to distinguish the two stations
# named Saint Paul Street on the B and C line.
station_locs <- station_locs %>% 
  mutate(stop_name = case_when(stop_id == 70140 ~ "Saint Paul Street B Line",
                               stop_id == 70141 ~ "Saint Paul Street B Line",
                               stop_id == 70217 ~ "Saint Paul Street C Line",
                               stop_id == 70218 ~ "Saint Paul Street C Line",
                               TRUE ~ stop_name))

# Most T stations are defined by multiple station platforms, 
# which might not be at the same latitude/longitude. 
# find the unique station names so we can get one observation per station
station_locs <- station_locs %>% 
  filter(!duplicated(stop_name))

# Select the rows which do not have Outbound in the text
# remove string with dash, extra spaces
station_locs <- station_locs %>% 
  mutate(stop_name = sub("\\-.*", "", stop_name) %>% 
           trimws(., which = c("right")))

# Export the data to a csv file
write.csv(station_locs, "./mbta_stops.csv")

# Map the stations
# Lat Long coordinates from www.latlong.net
mbta_subway <- leaflet(station_locs) %>%
  addTiles() %>%  
  setView(-71.057083, 42.361145, zoom = 12) %>%
  addCircles(~longitude, ~latitude, weight = 3, radius = 120, 
             color = "#0073B2", stroke = TRUE, fillOpacity = 0.8) %>% 
  addLegend("bottomleft", colors = "#0073B2", 
            labels = "Data Source: MBTA Developer Portal", 
            title = "MBTA Subway Stations")
  
# Show the map
mbta_subway  

