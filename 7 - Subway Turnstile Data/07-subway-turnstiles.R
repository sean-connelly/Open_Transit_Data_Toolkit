
# Load libraries
library(tidyverse);library(lubridate);library(hrbrthemes)
library(janitor)

options(stringsAsFactors = FALSE, scipen = 999)

# Read in MBTA turnstile csv data file
raw_turnstile <- readRDS(file = "./turnstile_171111.rds")

# Create a working dataframe
turnstile <- raw_turnstile %>% 
  rename("BOOTH" = C.A) %>% 
  filter(DESC %in% c("REGULAR", "RECOVR AUD"))

# Calculate difference, remove negative entries
turnstile <- turnstile %>% 
  group_by(BOOTH, SCP) %>% 
  mutate(diff = ENTRIES - lag(ENTRIES)) %>% 
  ungroup() %>% 
  filter(!is.na(diff), diff > 0)

# Select Terminals
terminals <- turnstile %>% 
  filter(STATION %in% c("34 ST-PENN STA", "42 ST-PORT AUTH",
                        "GRD CNTRL-42 ST", "ATL AV-BARCLAY"))

# Sum all the entries by day
daily_entries <- terminals %>%  
  group_by(STATION, DATE) %>% 
  summarize(ENTRIES = sum(diff, na.rm = TRUE))

# Plot
# Make Subway Ridership Linegraph
# Initialize a ggplot using railrides dataframe, and define axes 
entriesgraph <- ggplot(daily_entries, 
                       aes(x = DATE, y = ENTRIES, 
                           group = STATION, color = STATION)) +
  geom_point() + 
  geom_line() + 
  scale_y_comma() +
  scale_color_manual(name = "LINE", 
                     values = c("42 ST-PORT AUTH" = rgb(0, .45, .70),
                                "34 ST-PENN STA" = rgb(0, .60, .50),
                                "GRD CNTRL-42 ST" = rgb(.90, .60, 0),
                                "ATL AV-BARCLAY" = rgb(.80, .40, 0))) +
  ggtitle("MBTA Subway Daily Turnstile Entries Week of 11-18-2017") + 
  theme_ipsum()

entriesgraph

