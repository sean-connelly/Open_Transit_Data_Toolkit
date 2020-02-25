
# Load base and mapping libraries
library(tidyverse);library(lubridate);library(hrbrthemes)
library(leaflet);library(maps);library(scales);library(sf)
library(rgdal);library(rgeos);library(sp)

options(stringsAsFactors = FALSE)

# Read in Citibike bikeshare csv data file
raw_bike_data <- readRDS(file = "./bikeshare_nyc_raw.rds")

# Create a working data frame, remove any rows which the total docks is zero
bike_data <- raw_bike_data %>% 
  filter(tot_docks != 0)

# Select data for the week of October 9th (10/16 - 10/22)
bike_data <- bike_data %>% 
  filter(mday(date) <= 22 & mday(date) >= 9)

# Create a POSIXct date and time variable using available data
bike_data <- bike_data %>% 
  mutate(date = as.Date(date, format = "%y-%m-%d"),
         hour = sprintf("%02d", hour + (pm * 12) * (hour != 12)),
         minute = sprintf("%02d", minute),
         date_time = as.POSIXct(str_glue("{date} {hour}:{minute}"),
                                format = "%Y-%m-%d %H:%M"))

# Create a variable which measure how 'full' a bikeshare dock is
# 0 = empty, 1.0 = full
# Remove columns of data we don't need
bike_data <- bike_data %>% 
  mutate(avail_ratio = avail_bikes / tot_docks) %>% 
  select(dock_id, dock_name, date_time, avail_bikes, avail_docks, 
         tot_docks, avail_ratio, X_lat, X_long)

# Select times after 6pm ET using the hour function from lubridate
evening_bike_data <- bike_data %>% 
  filter(hour(date_time) >= 18)

# find the mean of the availability ratio and keep the location coordinates
evening_full <- evening_bike_data %>% 
  group_by(dock_name, "latitude" = X_lat, "longitude" = X_long) %>% 
  summarize(avg_avail_ratio = mean(avail_ratio, na.rm = TRUE)) %>% 
  arrange(avg_avail_ratio)

# Draw map of docking stations with availability ratio
palette <- colorNumeric(palette = "Blues", domain = (evening_full$avg_avail_ratio))

# Make leaflet map
access_map <- leaflet(evening_full) %>% 
  addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude, 
  radius = (evening_full$avg_avail_ratio + 0.25) * 100, 
  weight = 1, 
  color = ~palette(evening_full$avg_avail_ratio), 
  fillOpacity = 0.8,
  popup = ~dock_name) %>% 
  addLegend("bottomleft",
            pal = palette, values = ~avg_avail_ratio,
            title = "Citibike Docks<br>Percent Full<br>Evenings 10/2017 ",
            opacity = 1)

access_map

# Filter to one stop
Dwight_VanDyke <- bike_data %>% 
  filter(dock_name == "Dwight St & Van Dyke St")

# Draw plot, allow Default X Axis Labels
ggplot(Dwight_VanDyke, aes(x = date_time, y = avail_ratio)) +
  geom_point(col = "tomato2", size = 1) +
  scale_x_datetime(breaks = date_breaks("1 day")) +
  labs(title = "Dwight St & Van Dyke St Citi Bike Station Available Bicycles", 
       subtitle = "October 9-22, 2017 after 6 PM ET", 
       caption = "Source: Open Bus", 
       y = "Availability Ratio") +
  theme_ipsum()
