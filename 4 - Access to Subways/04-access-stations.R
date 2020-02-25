
# Load libraries
library(tidyverse);library(lubridate)
library(leaflet);library(maps);library(sp)
library(rgdal);library(rgeos);library(sf)

# Read in environmental justice blocks
ej_blocks <- st_read("./ej2010/EJ_POLY.shp")
ej_blocks <- st_transform(ej_blocks, crs = 4326)

# Select block designated as low income
income_blocks <- ej_blocks %>% 
  filter(INCOME == "I" & !is.na(INCOME))

# Color palette
pal <- colorFactor(palette = c("#E69F00"), domain = ej_blocks$INCOME)

# Make leaflet map
access_map <- leaflet() %>% 
  addTiles() %>%
  addPolygons(data = income_blocks, weight = 1, 
              color = ~pal(INCOME), fillOpacity = 1)

access_map

# Read in MBTA Station csv file from Lesson 1
station_locs <- read.csv(file = "./mbta_stops.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)

# Convert to simple features object
station_locs <- st_as_sf(station_locs, coords = c("longitude", "latitude"), crs = 4326)

# Add a buffer of a 1/4 mile around the point
walking_distance_buffer <- station_locs %>% 
  st_transform(crs = 32618) %>%
  st_buffer(dist = 400) %>% # 400 m, ~1/4 mile
  st_transform(crs = 4326)

# Make leaflet map
access_map <- leaflet(station_locs) %>%
  addTiles() %>%
  addPolygons(data = income_blocks, weight = 1, color = ~pal(INCOME), fillOpacity = 1)  %>%
  addCircleMarkers(radius = 1, stroke = 1, color = "#0072B2") %>%
  addPolygons(data = walking_distance_buffer, weight = 1, color = "#0072B2", 
              stroke = FALSE, fillOpacity = 0.5) %>% 
  addLegend("bottomleft",
            colors = c("#E69F00", "#0072B2"),
            labels = c("Low Income Blocks", "1/4 Mile Buffer"),
            title = "Subway Access to<br>Low Income Blocks",
            opacity = 1)

access_map
