#fig 3-8
library(tidyverse)
library(cowplot)
library(zoo)
setwd("C:/Users/Darin/Documents/populism/descriptive/replication")
`%notin%` <- function(x,y) !(x %in% y) 

df <- read_csv("time_series_v8.csv")

###Venezuela####
dv <- df %>% 
  filter(countryname == "Venezuela")

v1 <- ggplot() +
  geom_line(data = dv, aes(x = year, y = v2xps_party, color = "v2xps_party"), size = 1) +
  geom_line(data = dv, aes(x = year, y = avg_psi, color = "average_PSI"), size = 1) +
  theme_bw() +
  geom_rect(aes(xmin = 1999, xmax = 2015, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  ylim(0.35,1) +
  xlim(1985, 2015) +
  ylab("Party Institutionalization") +
  xlab("Year") +
  #ggtitle("Venezuelan Party Institutionalization \nand Party Strength") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("v2xps_party"="black", "average_PSI" = "gray"), labels = c("Average", "PI"), name = "") +
  theme(legend.text=element_text(size=10), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 10)) 

v2 <- ggplot() +
  geom_line(data = dv, aes(x = year, y = normalPS, color = "normalPS"), size = 1) +
  geom_line(data = dv, aes(x = year, y = avg_nps, color = "avg_nps"), size = 1) +
  theme_bw() +
  geom_rect(aes(xmin = 1999, xmax = 2015, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  scale_y_continuous(limits = c(0.35, 1), position = "right") +
  xlim(1985, 2015) +
  ylab("Party Strengh") +
  xlab("Year") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("normalPS"="black", "avg_nps" = "gray"), labels = c("Average", "Party Strength"), name = "") +
  theme(legend.text=element_text(size=10), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 10)) 


ggsave("venezuela1.jpg", plot = v1)
ggsave("venezuela2.jpg", plot = v2)
########

###Bolivia####
db <- df %>% 
  filter(countryname == "Bolivia")

b1 <- ggplot() +
  geom_line(data = db, aes(x = year, y = v2xps_party, color = "v2xps_party"), size = 1) +
  geom_line(data = db, aes(x = year, y = avg_psi, color = "average_PSI"), size = 1) +
  theme_bw() +
  geom_rect(aes(xmin = 2005, xmax = 2016, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  #geom_rect(aes(xmin = 2002, xmax = 2012, ymin = 0, ymax = Inf), fill = "gray", alpha= 0.2) +
  ylim(0.35,1) +
  xlim(1985, 2016) +
  ylab("Party Institutionalization") +
  xlab("Year") +
  #ggtitle("Venezuelan Party Institutionalization \nand Party Strength") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("v2xps_party"="black", "average_PSI" = "gray"), labels = c("Average", "PI"), name = "") +
  theme(legend.text=element_text(size=10), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 10)) 

b2 <- ggplot() +
  geom_line(data = db, aes(x = year, y = normalPS, color = "normalPS"), size = 1) +
  geom_line(data = db, aes(x = year, y = avg_nps, color = "avg_nps"), size = 1) +
  theme_bw() +
  geom_rect(aes(xmin = 2005, xmax = 2016, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  #geom_rect(aes(xmin = 2002, xmax = 2012, ymin = 0, ymax = Inf), fill = "gray", alpha= 0.2) +
  scale_y_continuous(limits = c(0.35, 1), position = "right") +
  xlim(1985, 2016) +
  ylab("Party Strengh") +
  xlab("Year") +
  #ggtitle("Venezuelan Party Institutionalization \nand Party Strength") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("normalPS"="black", "avg_nps" = "gray"), labels = c("Average", "Party Strength"), name = "") +
  theme(legend.text=element_text(size=10), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 10)) 

ggsave("bolivia1.jpg", plot = b1)
ggsave("bolivia2.jpg", plot = b2)

###France####
dfr <- df %>% 
  filter(countryname == "France")

f1 <- ggplot() +
  geom_line(data = dfr, aes(x = year, y = v2xps_party, color = "v2xps_party"), size = 1) +
  geom_line(data = dfr, aes(x = year, y = avg_psi, color = "average_PSI"), size = 1) +
  theme_bw() +
  geom_rect(aes(xmin = 1988, xmax = 2015, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  ylim(0.35,1) +
  xlim(1980, 2015) +
  ylab("Party Institutionalization") +
  xlab("Year") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("v2xps_party"="black", "average_PSI" = "gray"), labels = c("Average", "PI"), name = "") +
  theme(legend.text=element_text(size=10), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 10)) 

f2 <- ggplot() +
  geom_line(data = dfr, aes(x = year, y = normalPS, color = "normalPS"), size = 1) +
  geom_line(data = dfr, aes(x = year, y = avg_nps, color = "avg_nps"), size = 1) +
  theme_bw() +
  geom_rect(aes(xmin = 1988, xmax = 2015, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  scale_y_continuous(limits = c(0.35, 1), position = "right") +
  xlim(1980, 2015) +
  ylab("Party Strengh") +
  xlab("Year") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("normalPS"="black", "avg_nps" = "gray"), labels = c("Average", "Party Strength"), name = "") +
  theme(legend.text=element_text(size=10), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 10)) 

ggsave("france1.jpg", plot = f1)
ggsave("france2.jpg", plot = f2)

###Austria####
da <- df %>% 
  filter(countryname == "Austria")

a1 <- ggplot() +
  geom_line(data = da, aes(x = year, y = v2xps_party, color = "v2xps_party"), size = 1) +
  geom_line(data = da, aes(x = year, y = avg_psi, color = "average_PSI"), size = 1) +
  theme_bw() +
  geom_rect(aes(xmin = 1986, xmax = 2015, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  ylim(0.35,1) +
  xlim(1980, 2015) +
  ylab("Party Institutionalization") +
  xlab("Year") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("v2xps_party"="black", "average_PSI" = "gray"), labels = c("Average", "PI"), name = "") +
  theme(legend.text=element_text(size=10), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 10)) 

a2 <- ggplot() +
  geom_line(data = da, aes(x = year, y = normalPS, color = "normalPS"), size = 1) +
  geom_line(data = da, aes(x = year, y = avg_nps, color = "avg_nps"), size = 1) +
  theme_bw() +
  geom_rect(aes(xmin = 1986, xmax = 2015, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  scale_y_continuous(limits = c(0.35, 1), position = "right") +
  xlim(1980, 2015) +
  ylab("Party Strengh") +
  xlab("Year") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("normalPS"="black", "avg_nps" = "gray"), labels = c("Average", "Party Strength"), name = "") +
  theme(legend.text=element_text(size=10), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 10)) 


ggsave("austria1.jpg", plot = a1)
ggsave("austria2.jpg", plot = a2)

###USA####
du <- df %>% 
  filter(countryname == "United States of America")

u1 <- ggplot() +
  geom_line(data = du, aes(x = year, y = v2xps_party, color = "v2xps_party"), size = 1) +
  geom_line(data = du, aes(x = year, y = avg_psi, color = "average_PSI"), size = 1) +
  theme_bw() +
  geom_rect(aes(xmin = 2008, xmax = 2015, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  ylim(0.35,1) +
  xlim(1990, 2015) +
  ylab("Party Institutionalization") +
  xlab("Year") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("v2xps_party"="black", "average_PSI" = "gray"), labels = c("Average", "PI"), name = "") +
  theme(legend.text=element_text(size=10), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 10)) 

u2 <- ggplot() +
  geom_line(data = du, aes(x = year, y = normalPS, color = "normalPS"), size = 1) +
  geom_line(data = du, aes(x = year, y = avg_nps, color = "avg_nps"), size = 1) +
  theme_bw() +
  geom_rect(aes(xmin = 2008, xmax = 2015, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  scale_y_continuous(limits = c(0.35, 1), position = "right") +
  xlim(1990, 2015) +
  ylab("Party Strengh") +
  xlab("Year") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("normalPS"="black", "avg_nps" = "gray"), labels = c("Average", "Party Strength"), name = "") +
  theme(legend.text=element_text(size=10), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 10)) 


ggsave("usa1.jpg", plot = u1)
ggsave("usa2.jpg", plot = u2)

###Spain####
ds <- df %>% 
  filter(countryname == "Spain")

s1 <- ggplot() +
  geom_line(data = ds, aes(x = year, y = v2xps_party, color = "v2xps_party"), size = 1) +
  geom_line(data = ds, aes(x = year, y = avg_psi, color = "average_PSI"), size = 1) +
  theme_bw() +
  geom_rect(aes(xmin = 2014, xmax = 2015, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  ylim(0.35,1) +
  xlim(1990, 2015) +
  ylab("Party Institutionalization") +
  xlab("Year") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("v2xps_party"="black", "average_PSI" = "gray"), labels = c("Average", "PI"), name = "") +
  theme(legend.text=element_text(size=10), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 10)) 

s2 <- ggplot() +
  geom_line(data = ds, aes(x = year, y = normalPS, color = "normalPS"), size = 1) +
  geom_line(data = ds, aes(x = year, y = avg_nps, color = "avg_nps"), size = 1) +
  theme_bw() +
  geom_rect(aes(xmin = 2014, xmax = 2015, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  scale_y_continuous(limits = c(0.35, 1), position = "right") +
  xlim(1990, 2015) +
  ylab("Party Strengh") +
  xlab("Year") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("normalPS"="black", "avg_nps" = "gray"), labels = c("Average", "Party Strength"), name = "") +
  theme(legend.text=element_text(size=10), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 10)) 


ggsave("spain1.jpg", plot = s1)
ggsave("spain2.jpg", plot = s2)


