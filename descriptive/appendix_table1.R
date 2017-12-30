library(readr)
library(dplyr)
library(tidyr)

d1 <- read_csv("panel.csv") %>% 
  select(score, election = elec_result_major, pi = v2xps_party, normalPS) %>% 
  summarise_all(funs(min = min, 
                      max = max,
                      mean = mean, 
                      sd = sd, 
                      n = length)) %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  mutate_if(is.numeric, funs(round(., digits = 2))) %>% 
  mutate(row = c(1, 4, 3, 2)) %>% 
  arrange(row) %>% 
  select(var, n, mean, sd, min, max) 



