#fig 3
#library(hadleyverse)
#library(stargazer)
#setwd("C:/Users/Darin/Documents/populism/descriptive")

#load("C:/Users/Darin/Documents/populism/descriptive/Joined.Rdta")

#dc <- as.data.frame(dc) 

stargazer(dc, title = "Sample Summary Statistics", covariate.labels = c("Vote Share" ,"Populism", "PSI", "Party Strength"), label = "descriptive")

d1 <- dc %>% 
  select(region, elec_result_major, score, PSI, PI_7) %>% 
  group_by(region) %>% 
  summarize_each(funs(median)) %>% 
  reshape2::melt() %>% 
  reshape2::dcast(variable ~ region, value.var = "value") %>% 
  mutate(Variable = c("Vote Share", "Populism", "PSI", "Party Strength")) %>% 
  select(Variable, Americas, Europe)

stargazer(d1, summary = F, title = "Breakdown of Populism and Party System Attributes by Region", rownames = F)
  
  

t1 <- t.test(dc$score, dc$PSI)
t1t <- tidy(t1) %>% 
  mutate(Variable = "PSI") %>% 
  select(Variable, estimate, tstat = statistic, p.value) 

t2 <- t.test(dc$score, dc$PI_7)
t2t <- tidy(t2) %>% 
  mutate(Variable = "Party Strength") %>% 
  select(Variable, estimate, tstat = statistic, p.value)

ts <- rbind(t1t, t2t) %>% 
  mutate(Estimate = round(estimate, digits = 2), TStat = round(tstat, digits = 2), PValue = round(p.value, digits = 4)) %>% 
  select(Variable, Estimate, TStat, PValue)

stargazer(ts, summary = F, rownames = F, title = "Difference of Means - Populism Score")