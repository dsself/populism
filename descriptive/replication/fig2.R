#fig 2
#tidyverse 1.20
library(tidyverse)

d1 <- read_csv("panel.csv")

ggplot(d1, aes(x = v2xps_party, y = score, color = region, label = abbr)) +
  geom_text(size = 2.5) +
  theme_bw() +
  ylab("Populism Score") +
  xlab("Party Institutionalization") +
  #ggtitle("Party System Institutionalization and Populism") +
  theme(legend.key.size = unit(.25, "cm")) +
  theme_bw() +
  scale_color_grey(start = 0.0, end = 0.5, name = "Region") +
  theme(legend.position=c(1,1),legend.justification=c(1,1),
        legend.direction="vertical",
        legend.box="horizontal",
        legend.box.just = c("top"), 
        legend.background = element_rect(fill=alpha('white', 0.0))) +
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  xlim(.35, 1)


ggsave("fig2_1.jpg", width = 5)

ggplot(d1, aes(x = normalPS, y = score, color = region, label = abbr)) +
  geom_text(size = 2.5) +
  theme_bw() +
  ylab("Populism Score") +
  xlab("Party Strength") +
  #ggtitle("Party System Institutionalization and Populism") +
  theme(legend.key.size = unit(.25, "cm")) +
  theme_bw() +
  scale_color_grey(start = 0.0, end = 0.5, name = "Region") +
  theme(legend.position=c(1,1),legend.justification=c(1,1),
        legend.direction="vertical",
        legend.box="horizontal",
        legend.box.just = c("top"), 
        legend.background = element_rect(fill=alpha('white', 0.0))) +
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  xlim(.35, 1)


ggsave("fig2_2.jpg", width = 5)
