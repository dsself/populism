library(readr)
library(dplyr)

d1 <- read_csv("panel.csv") %>% 
  mutate(logdm = log(DM)) %>% 
  select(countryname, year, score, v2xps_party, normalPS, logdm) %>% 
  mutate_if(is.numeric, funs(round(., digits = 2)))


stargazer(d1, summary = F, rownames = F)
