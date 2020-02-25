
# Load libraries
library(tidyverse);library(lubridate);library(hrbrthemes)
library(janitor)

options(stringsAsFactors = FALSE)

# Read in MBTA headway for Bronx buses data file (already filtered down from raw MTA NYCT)
raw_bus_times <- readRDS(file = "./MTA-Bus-Time_2014-10-08_Bronx.rds")

# Create a POSIXct date and time variable using available data
# Select Peak time 07:00 EST (11:00 UTC) to 10:00 EST (14:00 UTC)
bus_times <- raw_bus_times %>% 
  mutate(time_received = as.POSIXct(time_received, format = "%Y-%m-%d %H:%M:%S")) %>% 
  filter(hour(time_received) >= 11 & hour(time_received) < 14)

# Create and format our analysis data frame
speed <- bus_times %>% 
  select(inferred_trip_id,
         inferred_route_id, 
         time_received, 
         "max_distance" = distance_along_trip) %>% 
  group_by(inferred_trip_id) %>% 
  arrange(desc(max_distance)) %>% 
  slice(1) %>% 
  ungroup()

speed_min_distance <- bus_times %>%
  select(inferred_trip_id, 
         "time" = time_received, 
         "min_distance" = distance_along_trip) %>% 
  group_by(inferred_trip_id) %>% 
  arrange(min_distance) %>% 
  slice(1) %>% 
  ungroup()

# Join speed and min_speed on inferred_trip_id
speed <- left_join(speed, speed_min_distance, by = "inferred_trip_id")

speed<- speed %>% 
  mutate(time_diff = time_received - time,
         distance = max_distance - min_distance)

# Remove any rows which the time difference or distance travelled is zero
speed <- speed %>% 
  filter(time_diff > 0, distance > 0) %>% 
  mutate(m_per_sec = distance / as.numeric(time_diff),
         mph = m_per_sec * 2.237) # meters per second to miles per hour = 1:2.23694

# Find of average (mean) speed from all the trips of each individual route 
average_speed <- speed %>% 
  group_by(inferred_route_id) %>% 
  summarize(avg_mph = mean(mph, na.rm = TRUE))

# Plot
# Initialize a ggplot, and then create lines graphs for bus speeds throughout the day for each bus route.
speedplot <- speed %>% 
  mutate(inferred_route_id = str_remove(inferred_route_id, "^(MTA NYCT_|MTABC_)")) %>% 
  ggplot(., aes(x = time_received, y = mph)) +
  geom_line() + 
  facet_wrap(~inferred_route_id, ncol = 10) + 
  theme_ipsum()

speedplot

# Replot with outliers remoted
# Initialize a ggplot, and then create lines graphs for bus speeds throughout the day for each bus route.
speedplot <- speed %>% 
  filter(mph < 30) %>% 
  mutate(inferred_route_id = str_remove(inferred_route_id, "^(MTA NYCT_|MTABC_)")) %>% 
  ggplot(., aes(x = time_received, y = mph)) +
  geom_line() + 
  facet_wrap(~inferred_route_id, ncol = 10) + 
  theme_ipsum() +
  ggtitle("Average Speeds for MTA Bronx Buses from 7am-10am 2014-10-08")

speedplot
