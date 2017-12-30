library(tidyverse)
library(haven)
library(standard)
library(stargazer)
setwd("descriptive")





d1 <- read_csv("Country_year.csv") %>% 
  mutate(countryname = country_panel(Country, Year)) %>% 
  select(countryname, year = Year)

load("Joined.Rdta")

dc <- dc %>% 
  mutate(countryname = country_name(countryname))

df <- left_join(d1, dc, by = c("countryname")) %>% 
  filter(!is.na(elec_result_major))

ssj <- read_dta("SSJ_2016.dta") %>% 
  select(cname = countryname, year, avemag_bothtiers, avemag_lower) %>% 
  mutate(countryname = country_panel(cname, year), cowcode = country_code(countryname, "country", "cown")) %>% 
  select(countryname, cowcode, year, avemag_bothtiers, avemag_lower) %>% 
  filter(year >= 2006) %>% 
  filter(countryname %in% df$countryname) 

#manually edit to get closest years to Hawkins
#write_csv(ssj, "ssj_pop.csv")
  
panel <- read_csv("panel.csv") %>% 
  mutate(avemag_bothtiers = ifelse(is.nan(avemag_bothtiers), avemag_lower, avemag_bothtiers)) %>% 
  mutate(lavemag_bothtiers = log(avemag_bothtiers)) %>% 
  mutate(invert = lavemag_bothtiers*-1) %>% 
  mutate(invertmag = log(avemag_lower)*-1) %>% 
  #filter(score >= 10.925) %>% 
  #mutate(countryname = ifelse(countryname == "Italy/Sardinia", "Italy", countryname)) %>% 
  #mutate(countryname = ifelse(countryname == "United States of America", "U.S.A.",countryname)) %>% 
  mutate(abbr = country_code(countryname, "country", "cowc")) %>% 
  mutate(Populism = ntile(score,3), Populism = ifelse(Populism == 1, "Low", ifelse(Populism == 2, "Med", "High"))) %>%
  mutate(Populism = factor(Populism, levels = c("High", "Med", "Low"))) %>% 
  mutate(grid = c(1,2,1,1,4,2,1,1,2,3,1,1,2,2,2,1,1,2,2,2,2,4,2,3,1)) %>% 
  unique() %>% 
  group_by(grid) %>% 
  mutate(avgpop = mean(score))

table <- panel %>% 
  ungroup() %>% 
  select(countryname, year, score, v2xps_party, normalPI, lavemag_bothtiers) %>% 
  mutate_if(is.numeric, funs(round(., 2)))

stargazer(table, summary = F, rownames = F)

ggplot(panel, aes(color = Populism)) +
  geom_text(aes(normalPI, lavemag_bothtiers, label = abbr), size=5, angle = 40, hjust=0, vjust=0, show.legend  = F) +
  geom_point(aes(normalPI, lavemag_bothtiers, shape = Populism, label = abbr), size = 2.5) +
  geom_hline(yintercept = 0.4) +
  geom_vline(xintercept = 0.726) +
  theme_bw() + 
  ylab("Logged Weighted District Magnitude") +
  xlab("Party Strength") +
  theme(legend.text=element_text(size=6), legend.title=element_text(size=6, face = "bold"),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size=10, face = "bold")) +
  theme(legend.justification=c(0.95,0), legend.position=c(0.275,.01), legend.direction="horizontal") +
  scale_color_grey() +
  scale_x_continuous(expand = c(.04, .04)) +
  scale_y_continuous(expand = c(.1, .1)) 
  #ylim(c(-0.1,5.25))

ggsave("grid.jpg", dpi = 1000, width = 8, height = 5)

#ggtitle("Environmental Hostility and Populism in Party Systems") +
#scale_color_grey(name = "Populism") +
