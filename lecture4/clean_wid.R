
library(tidyverse)

setwd("C:/Users/l.sirugue/Desktop/Teaching/data-analysis-course-gh-pages/lecture4/")

# Rename variables in csv
wid <- read.csv("WID_Data_04072022-093826.csv", sep = ";") %>%
  select(-Percentile) %>% 
  group_by(Country) %>% 
  summarise_all(function(x){mean(x, na.rm = T)}) %>% 
  select(-ends_with("2021")) %>% 
  select(-ends_with("2020")) %>% 
  na.omit() %>%
  mutate(Country = substr(Country, 2, length(Country))) %>%
  mutate(Country = ifelse(Country == "Cote dâ???TIvoire", "Ivory Coast", Country),
         Country = ifelse(Country == "Cabo Verde", "Cape Verde", Country),
         Country = ifelse(Country == "Viet Nam", "Vietnam", Country)) %>%
  left_join(read.csv("continents.csv") %>%
              mutate(Country = ifelse(Country == "US", "USA", Country),
                     Country = ifelse(Country == "Burkina", "Burkina Faso", Country),
                     Country = ifelse(Country == "CZ", "Czech Republic", Country),
                     Country = ifelse(Country == "Congo, Democratic Republic of", "DR Congo", Country),
                     Country = ifelse(Country == "Burma (Myanmar)", "Mynamar", Country))) %>%
  na.omit() %>%
  rename(continent = Continent, country = Country) %>%
  pivot_longer(-c(country, continent), names_to = "vars", values_to = "vals") %>%
  mutate(year = as.numeric(substr(vars, nchar(vars) - 3, nchar(vars))),
         vars = substr(vars, 1, nchar(vars) - 5)) %>%
  pivot_wider(names_from = "vars", values_from = "vals") %>%
  arrange(continent, country, year)

write.csv(wid, "wid.csv", row.names = F)
