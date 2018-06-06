#fig 3
#tidyverse 1.20
library(tidyverse)
#setwd("C:/Users/Darin/Documents/populism/descriptive/replication")

d1 <- read_csv("panel.csv") %>% 
  select(PI = v2xps_party, Strength = normalPS, score, elec_result_major, region) %>% 
  group_by(region) %>% 
  summarize_all(funs(avg = mean)) %>% 
  mutate_if(is.numeric, funs(round(., 2)))
