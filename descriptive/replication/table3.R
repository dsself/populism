library(dplyr) 
library(readr)

d1 <- read_csv("spainvotes.csv") %>% 
  mutate(E2011 = E2011*100, E2015 = E2015*100, E2016 = E2016*100)
