library(standard)
library(dplyr)
library(readr)

d1 <- read_csv("Master Panel Auto Factor.csv") %>% 
  select(countryname, year, v2xps_party, PI_7) %>% 
  mutate(normalPS = (PI_7-min(PI_7, na.rm = TRUE))/(max(PI_7, na.rm = TRUE)-min(PI_7, na.rm = TRUE))) %>% 
  filter(year >= 2005) %>% 
  select(-year) %>% 
  group_by(countryname) %>% 
  summarize_all(funs(mean(., na.rm = TRUE)))

d2 <- read_csv("ssj_pop.csv") %>% 
  left_join(d1, by = "countryname") 

d3 <- read_csv("pty.csv") %>% 
  mutate(countryname = country_name(country)) %>% 
  na.omit() %>% 
  select(-country) %>% 
  left_join(d2, by = "countryname") %>% 
  rename(region = LA) %>% 
  mutate(abbr = country_code(countryname, "country", "cowc"))

d4 <- read_csv("hs.csv") %>% 
  mutate(countryname = country_name(Country)) %>% 
  filter(!is.na(countryname)) %>% 
  group_by(countryname) %>% 
  summarize_all(funs(mean)) %>% 
  select(countryname, elec_result_major)

d5 <- left_join(d3, d4, by = "countryname")

write_csv(d5, "panel.csv")


