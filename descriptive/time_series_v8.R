library(tidyverse)

d1 <- read_csv("Master Base_v8.csv") %>% 
  ungroup() %>% 
  filter(polity2 >= 4) %>% 
  select(countryname, year, v2xps_party, PI_7) %>% 
  mutate(normalPS = (PI_7-min(PI_7, na.rm = TRUE))/(max(PI_7, na.rm = TRUE)-min(PI_7, na.rm = TRUE))) %>% 
  group_by(year) %>% 
  mutate(avg_psi = mean(v2xps_party, na.rm = T)) %>% 
  mutate(avg_pi7 = mean(PI_7, na.rm = T)) %>% 
  mutate(avg_nps = mean(normalPS, na.rm = T)) %>% 
  write_csv("time_series_v8.csv")
  
