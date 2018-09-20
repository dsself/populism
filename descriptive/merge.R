library(standard)
library(tidyverse)


d1 <- read_csv("ssj_pop.csv") %>% 
  mutate(dm = ifelse(is.na(avemag_bothtiers), avemag_lower, avemag_bothtiers), tiered = ifelse(is.na(avemag_bothtiers), 0, 1))

d2 <- read_csv("maddison_gdppc.csv") %>% 
  mutate(countryname = country_name(country)) %>% 
  select(-country) %>% 
  mutate(lgdppc = log(cgdppc))
  
sample1 <- d1$countryname
sample2 <- 2005:2012

d3 <- read_csv("Master Panel_v8.csv") %>% 
  select(countryname, year, v2xps_party, PI_7) %>% 
  mutate(normalPS = (PI_7-min(PI_7, na.rm = TRUE))/(max(PI_7, na.rm = TRUE)-min(PI_7, na.rm = TRUE))) %>% 
  filter(year %in% sample2, countryname %in% sample1) %>% 
  group_by(countryname) %>% 
  mutate(lag_psi = lag(v2xps_party), lag_ps = lag(normalPS)) %>% 
  na.omit() %>% 
  left_join(d2, by = c("countryname", "year"))

d4 <- left_join(d1, d3, by = c("countryname", "year"))

d5 <- read_csv("pty.csv") %>% 
  mutate(countryname = country_name(country)) %>% 
  na.omit() %>% 
  select(-country) %>% 
  left_join(d4, by = "countryname") %>% 
  rename(region = LA) %>% 
  mutate(abbr = country_code(countryname, "country", "cowc"))

d6 <- read_csv("hs.csv") %>% 
  mutate(countryname = country_name(Country)) %>% 
  filter(!is.na(countryname)) %>% 
  group_by(countryname) %>% 
  summarize_all(funs(mean)) %>% 
  select(countryname, elec_result_major)

d7 <- left_join(d5, d6, by = "countryname") 

write_csv(d7, "panel_v8.csv")


