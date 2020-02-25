
# Load libraries into current session
library(tidyverse)
library(lubridate)
library(hrbrthemes)

# Read in MBTA ridership csv file
rawdata <- read.csv(file = "./MBTA_Average_Monthly_Ridership_by_Mode.csv", head = TRUE, sep = ",")

# Select Rail (subway) rows, limit to 2016
railrides <- rawdata %>%
  rename("service_date" = ï..service_date) %>% 
  mutate(service_date = as.Date(str_sub(as.character(service_date), 1, 10))) %>% 
  filter(year(service_date) == 2016, mode == "Rail")

# Select columns for analysis, rename, convert to characters
railrides <- railrides %>% 
  select("Month" = service_date, 
         "Mode" = mode, 
         "Avg_Weekday_Ridership" = average_monthly_ridership, 
         "Line" = route_or_line) %>% 
  mutate_if(is.factor, as.character)

# Make Subway Ridership Linegraph
subwayridesgraph <- ggplot(railrides, aes(x = Month, y = Avg_Weekday_Ridership, 
                                          group = Line, color = Line)) +
  geom_point() + 
  geom_line() +
  scale_y_comma() +
  scale_color_manual(name = "Line",
                     values = c("Blue Line" = "blue", "Green Line" = "green",
                                "Orange Line" = "orange", "Red Line" = "red")) + 
  ggtitle("Average Weekday MBTA Subway Ridership 2016") +
  theme_ipsum()

plot(subwayridesgraph)
