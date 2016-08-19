#fig 2
#library(hadleyverse)
#setwd("C:/Users/Darin/Documents/populism/descriptive")

#load("C:/Users/Darin/Documents/populism/descriptive/Joined.Rdta")

#dc <- as.data.frame(dc) %>% 
#  rename(Region = region) %>% 
#  mutate(countryname=replace(countryname, countryname=="German Federal Republic", "Germany")) %>% 
#  mutate(countryname=replace(countryname, countryname=="Italy/Sardinia", "Italy")) 


#d1 <- dc %>% 
#  group_by(region) %>% 
#  summarize_each(funs(mean), -countryname, -region, -median_pop) %>% 
#  mutate(score = round(score, digits = 2), PSI = round(PSI, digits = 2), PI_7 = round(PI_7, digits = 2), votes = round(elec_result_major, digits = 2)) %>% 
#  select(Region = region, Votes = votes, Populism = score, PSI, Strength = PI_7)

#stargazer(d1, summary = F, rownames = FALSE, title = "Tabulation of Populism Scores - Party and Party System Institutionalization")


ggplot(dc, aes(x = PSI, y = score, color = Region, label = countryname)) +
  geom_text(size = 2.5) +
  theme_bw() +
  ylab("Populism Score") +
  xlab("Party System Institutionalization") +
  #ggtitle("Party System Institutionalization and Populism") +
  theme(legend.key.size = unit(.25, "cm")) +
  theme_bw() +
  scale_color_grey(start = 0.0, end = 0.5) +
  theme(legend.position=c(1,1),legend.justification=c(1,1),
        legend.direction="vertical",
        legend.box="horizontal",
        legend.box.just = c("top"), 
        legend.background = element_rect(fill=alpha('white', 0.0))) +
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) 


ggsave("fig2.jpg", width = 5)