library(tidyverse)
coffee_ratings <- read_csv("data/coffee_ratings.csv")
ar <- read_csv("data/arabica_data_cleaned.csv")
library(naniar)
vis_miss(coffee_ratings)
glimpse(coffee_ratings)

# Create transofrmed df ####
country_table<-coffee_ratings %>%
  count(country_of_origin = factor(country_of_origin)) %>% 
  mutate(pct = prop.table(n)) %>%
  arrange(-pct) %>% 
  tibble()

# Plot ####
library(lubridate)
