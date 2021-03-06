
# Load libraries
library(tidyverse);library(lubridate);library(hrbrthemes)
library(janitor)

options(stringsAsFactors = FALSE)

# Read in MBTA headway data file
raw_bus_times <- readRDS(file = "./m3_rawdata_2017-10_2.rds")

# Create a working dataframe, remove any empty columns 
bus_times <- raw_bus_times %>% 
  remove_empty("cols")

# Convert time to posxict with UTC time, convert to the local time
bus_times <- bus_times %>% 
  mutate(time = as.POSIXct(time, origin = "1970-01-01", tz = "UTC"),
         local_time = with_tz(time, "America/New_York")) %>% 
  select(-time)

# Create new data frame with just the M3 stop_404120
# UNION SQ E/E 15 ST Stopcode 404120
# This is a north bound train
headways <- bus_times %>% 
  select(local_time, stop_404120)

# The index increases incrementally every time a bus is at or approacing at a stop
# Add 1 to make sure the index doesn't start at zero
# Add 1 to the start of the index, and shift the rest of the index down a row
headways <- headways %>% 
  mutate(index = cumsum(stop_404120) + 1) %>% 
  mutate(index = ifelse(row_number() == 1, 1, lag(index)))

# Get the date from index.
headways$last_bus <- c(headways$local_time[1], 
                       headways[which(headways$stop_404120==1), "local_time"])[headways$index]

# Find the difference (in seconds) of the time of the recording and the time of the last bus found
# Multiply by stop_404120 to isolate the time intervals, only if there was a bus at the time of the recording.
headways <- headways %>% 
  mutate(headway = difftime(local_time, last_bus, units = "secs") * stop_404120)

# Remove the first stop time, remove any lastbus from the previous day
headways <- headways %>% 
  filter(index != 1, wday(local_time) == wday(last_bus))

# Select times 7am EDT through 10am EDT using the with function from lubridate
# diff b/w ET and UTC is 4 hours, we want the time to after 11:00 UTC and before 14:00pm UTC.
# select data monday through friday using the with function from lubridate
headways <- headways %>%
  filter(hour(local_time) >= 7 &  hour(local_time) < 14,
         wday(local_time) >= 2 & wday(local_time) <= 6)

# Calculate the average headway. Remove any 0 and null values from the calculation.
headways <- headways %>%
  mutate(headway = as.numeric(headway))
         
headway_mean <- mean(ifelse(headways$headway == 0, NA_real_, headways$headway), na.rm = TRUE) / 60

# Identify the different days
# Convert to Date object (Remove the time and just keep the date)
headways <- headways %>%
  mutate(day = as.Date(local_time, tz = "America/New_York")) %>%
  filter(stop_404120 != 0)
           
# Calcuate the average headway per weekday
headway_mean_by_day <- headways %>% 
  group_by(day) %>% 
  summarize(headway_mean = mean(headway, na.rm = TRUE) / 60) %>% 
  ungroup() 
  
headways <- left_join(headways, headway_mean_by_day, by = "day")

# Plot
# Make Subway Ridership Linegraph
# Initialize a ggplot using railrides dataframe, and define axes 
headwaysgraph <- ggplot(headway_mean_by_day, aes(x = day, y = headway_mean)) +
  geom_bar(fill = rgb(0.9, 0.6, 0), stat = "identity") +
  ggtitle("MBTA M3 Average Headways for October 2017") +
  theme_ipsum()

headwaysgraph


