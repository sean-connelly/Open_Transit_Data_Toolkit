
# Load libraries
library(tidyverse);library(lubridate);library(hrbrthemes)

options(stringsAsFactors = FALSE)

# Read in MTA SF Survey Results
raw_survey_2017 <- readRDS(file = "./TDS_202017_20Data-WEBPAGE.rds")

# Create a working dataframe
survey_2017 <- raw_survey_2017

# Select the columns for Rideshare useage, age, and income, keep complete responses
rideshare <- survey_2017 %>% 
  select("USAGE" = Q21A, "AGE" = Q27, "INCOME" = Q29) %>% 
  drop_na()

# Separate the rows into age and income groups, reformat usage score
rideshare <- rideshare %>% 
  mutate(AGE_RANGE = case_when(AGE == 1 ~ "18-24 yrs",
                               AGE == 2 ~ "25-34 yrs", 
                               AGE == 3 ~ "35-44 yrs", 
                               AGE == 4 ~ "45-54 yrs", 
                               AGE == 5 ~ "55-64 yrs", 
                               AGE == 6 ~ "65+ yrs", 
                               TRUE ~ NA_character_),
         INCOME_RANGE = case_when(INCOME == 1 ~ "$15,000 or less",
                                  INCOME == 2 ~ "$15,001-$25,000",
                                  INCOME == 3 ~ "$25,001-$35,000",
                                  INCOME == 4 ~ "$35,001-$75,000",
                                  INCOME == 5 ~ "$75,001-$100,000", 
                                  INCOME == 6 ~ "$100,001-$200,000", 
                                  INCOME == 7 ~ "Over $200,000", 
                                  TRUE ~ NA_character_),
         USAGE_SCORE = case_when(USAGE == 2 ~ 5,
                                 USAGE == 3 ~ 4,
                                 USAGE == 4 ~ 3,
                                 USAGE == 5 ~ 2, 
                                 USAGE == 6 ~ 1, 
                                 TRUE ~ 0))

# Find average usage score by age range
rideshare_mean_byage <- rideshare %>% 
  group_by(AGE, AGE_RANGE) %>% 
  summarize(USAGE_SCORE = round(mean(USAGE_SCORE, na.rm = TRUE), 2))

# Find average usage score by income range
rideshare_mean_byincome <- rideshare %>% 
  group_by(INCOME, INCOME_RANGE) %>% 
  summarize(USAGE_SCORE = round(mean(USAGE_SCORE, na.rm = TRUE), 2))

# Plot usage score by age
usagebyage <- ggplot(rideshare_mean_byage, 
                     aes(x = fct_reorder(AGE_RANGE, AGE),
                         y = USAGE_SCORE, label = USAGE_SCORE)) + 
  geom_point(stat = "identity", fill = "black", size = 10)  +
  geom_segment(aes(y = 0, 
                   x = AGE_RANGE, 
                   yend = USAGE_SCORE, 
                   xend = AGE_RANGE), 
               color = "black") +
  geom_text(color = "white", size = 3) +
  labs(title = "Rideshare Usage by Age", 
       subtitle = "SFMTA Transit Decision Survey",
       x = "Age") + 
  ylim(0, 6) +
  coord_flip() +
  theme_ipsum()

usagebyage

# Plot usage score by income
usagebyincome <- ggplot(rideshare_mean_byincome, 
                        aes(x = fct_reorder(INCOME_RANGE, INCOME), 
                            y = USAGE_SCORE, label = USAGE_SCORE)) + 
  geom_point(stat = "identity", fill = "black", size = 10)  +
  geom_segment(aes(y = 0, 
                   x = INCOME_RANGE, 
                   yend = USAGE_SCORE, 
                   xend = INCOME_RANGE), 
               color = "black") +
  geom_text(color = "white", size = 3) +
  labs(title = "Rideshare Usage by Income", 
       subtitle = "SFMTA Transit Decision Survey",
       x = "Income") + 
  ylim(0, 6) +
  coord_flip() + 
  theme_ipsum()

usagebyincome

# Find the mean by age and income
rideshare_mean <- rideshare %>% 
  group_by(AGE, AGE_RANGE, INCOME, INCOME_RANGE) %>% 
  summarize(USAGE_SCORE = round(mean(USAGE_SCORE, na.rm = TRUE), 2))
