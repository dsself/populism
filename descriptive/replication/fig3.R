library(tidyverse)

setwd("C:/Users/Darin/Documents/populism/descriptive/replication")

d1 <- read_csv("panel_v8.csv") %>% 
  mutate(avemag_bothtiers = ifelse(is.nan(avemag_bothtiers), avemag_lower, avemag_bothtiers)) %>% 
  mutate(lavemag_bothtiers = log(DM)) %>% 
  mutate(invert = lavemag_bothtiers*-1) %>% 
  mutate(invertmag = log(avemag_lower)*-1) %>% 
  mutate(Populism = ntile(score,3), Populism = ifelse(Populism == 1, "Low", ifelse(Populism == 2, "Med", "High"))) %>%
  mutate(Populism = factor(Populism, levels = c("High", "Med", "Low"))) %>% 
  mutate(grid = c(1,2,1,1,4,2,1,1,2,3,1,1,2,2,2,1,1,2,2,2,2,4,2,3,1)) %>% 
  unique() %>% 
  group_by(grid) %>% 
  mutate(avgpop = mean(score))


ggplot(d1, aes(color = Populism)) +
  geom_text(aes(normalPS, lavemag_bothtiers, label = abbr), size=5, angle = 40, hjust=0, vjust=0, show.legend  = F) +
  geom_point(aes(normalPS, lavemag_bothtiers, shape = Populism, label = abbr), size = 2.5) +
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

ggsave("fig3.jpg", dpi = 1000, width = 8, height = 5)


