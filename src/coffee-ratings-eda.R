#Load libraries/csv ####
library(tidyverse)
library(naniar)
library(widyr)
library(ggraph)
library(igraph)
library(here)
library(gt)
library(purrr)
library(rstatix)
library(Hmisc)
library(ggforce)
library(reshape)
library(reshape2)
library(ggpmisc)
library(viridis)

coffee_ratings <- read_csv("data/coffee_ratings.csv")
here()
# Check missing data ####
vis_miss(coffee_ratings)
(na_count <-sapply(coffee_ratings, function(y) sum(length(which(is.na(y))))))
which(is.na(coffee_ratings$country_of_origin), arr.ind=TRUE)
# Check data types ####
glimpse(coffee_ratings)

#Arabic vs Robusta ####
table(coffee_ratings['species'])
# visualize all ####
#Histogram
coffee_ratings %>%
  select(altitude) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

#Create vectors ####
grades <- c("aroma",	"flavor",	"aftertaste",	"acidity",	"body",	"balance",	"uniformity",	"clean_cup",	"sweetness",	"cupper_points",	"total_cup_points")
altitudes <- c("altitude_low_meters",	"altitude_high_meters",	"altitude_mean_meters")
#Density
coffee_ratings %>%
  # select(-c(number_of_bags, category_one_defects, category_two_defects, altitude_low_meters, altitude_high_meters, quakers)) %>% 
  select(grades) %>% 
  # keep(is.numeric) %>% 
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_density()
coffee_ratings %>%
  # select(-c(number_of_bags, category_one_defects, category_two_defects, altitude_low_meters, altitude_high_meters, quakers)) %>% 
  select(altitudes) %>%
  # keep(is.numeric) %>% 
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_density()


# Identify outliers ####
#base r
coffee_ratings %>% select(altitudes) %>% hist.data.frame()
coffee_ratings %>% select(grades) %>% hist.data.frame()
coffee_ratings %>% select(country_of_origin) %>% hist.data.frame(nclass = 10)


#Countries plot ####
coffee_ratings %>% 
  count(country_of_origin, sort=TRUE) %>% 
  top_n(20) %>% 
  ggplot(aes(reorder(country_of_origin, n), (n)))+
  geom_col()+
  coord_flip()+
  ylab("Countries")+xlab("Count")
  #plot


boxplot(coffee_ratings$altitude_mean_meters)$out
outliers
avg = mean(coffee_ratings$altitude_mean_meters[!coffee_ratings$outliers])

coffee_ratings_wo_high_means <- coffee_ratings[!(coffee_ratings$altitude_mean_meters %in% outliers), ]

rstatix::identify_outliers(coffee_ratings, "altitude_mean_meters")


summarize(coffee_ratings, na.rm = TRUE)
summary(coffee_ratings)
library(lubridate)

  coord_flip()

  

# complete graph get flipped with the
# help of coord_flip() function
ggp +  coord_flip()


#test facet ####
library(tidyverse)

coffee_ratings[coffee_ratings$total_cup_points>0,] %>% select(total_cup_points, grades) %>% pivot_longer(-total_cup_points, names_to = "Measures", values_to = "Score")%>%
  ggplot(aes(x = total_cup_points, y = Score))+
  geom_point(alpha = 0.8, size=0.1) +
  geom_smooth(method="lm", na.rm = TRUE, se = FALSE)+
  coord_flip() +
  facet_wrap(vars(Measures)) +
  ylab("Measure")+
  xlab("Total score")+
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = y ~ x),  geom = "text_npc", 
                  aes(label = paste("p=", signif(..p.value.., digits = 0), 
                                    "\nR^2=", signif(..r.squared.., digits = 2), sep = "")),
                  label.x = "right", label.y = "bottom", size = 2)
  
