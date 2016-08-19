df <- read_csv("C:/Users/Darin/Documents/anocracies/replication/ss2.csv") %>% 
  select(country, year, ave_magT1T2) %>% 
  filter(country %in% c("Spain")) %>% 
  filter(year >= 1980)


df <- read_csv("pty.csv") %>% 
  mutate(ln_DM = log(DM), Region = LA)



ggplot(df, aes(x = ln_DM, y = score, color = Region, label = country)) +
  geom_text(size = 2.5) +
  theme_bw() +
  ylab("Populism Score") +
  xlab("District Magnitude") +
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