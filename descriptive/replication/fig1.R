#fig 1
library(dplyr)
library(ggplot2)
setwd("C:/Users/Darin/Documents/populism/descriptive")

d1 <- read_csv("panel.csv")

ggplot(d1) +
  geom_density(aes(x = score, fill = region), alpha = 0.4) +
  ylab("Distribution of Populism in Party Systems") +
  xlab("Weighted Populism Score") + 
  guides(fill=guide_legend(title= "Region")) +
  theme_bw() +
  scale_fill_grey() +
  theme(legend.position=c(1,1),legend.justification=c(1,1),
        legend.direction="horizontal",
        legend.box="horizontal",
        legend.box.just = c("top"), 
        legend.background = element_rect(fill=alpha('white', 0.0))) +
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))


ggsave("fig1.jpg", width = 5)
