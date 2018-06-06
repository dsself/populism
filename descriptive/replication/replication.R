#R version 3.4.2 (2017-09-28)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows >= 8 x64 (build 9200)
#tidyverse 1.20
library(tidyverse)
setwd("replication")

#figure 1####
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

#table 2####
d1 <- read_csv("panel.csv") %>% 
  select(PI = v2xps_party, Strength = normalPS, score, elec_result_major, region) %>% 
  group_by(region) %>% 
  summarize_all(funs(avg = mean)) %>% 
  mutate_if(is.numeric, funs(round(., 2)))

#figure 2####
d1 <- read_csv("panel.csv")

ggplot(d1, aes(x = v2xps_party, y = score, color = region, label = abbr)) +
  geom_text(size = 2.5) +
  theme_bw() +
  ylab("Populism Score") +
  xlab("Party Institutionalization") +
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

ggplot(d1, aes(x = normalPS, y = score, color = region, label = abbr)) +
  geom_text(size = 2.5) +
  theme_bw() +
  ylab("Populism Score") +
  xlab("Party Strength") +
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

#fig 3#####
d1 <- read_csv("panel.csv") %>% 
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

#figure 4#####
dv <- read_csv("time_series.csv") %>% 
  filter(countryname == "Venezuela")

ggplot() +
  geom_line(data = dv, aes(x = year, y = v2xps_party, color = "v2xps_party"), size = 1) +
  geom_line(data = dv, aes(x = year, y = avg_psi, color = "average_PSI"), size = 1) +
  theme_bw() +
  geom_rect(aes(xmin = 1999, xmax = 2015, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  ylim(0.35,1) +
  xlim(1985, 2015) +
  ylab("Party Institutionalization") +
  xlab("Year") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("v2xps_party"="black", "average_PSI" = "gray"), labels = c("Average", "PI"), name = "") +
  theme(legend.text=element_text(size=10), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 10)) 

ggplot() +
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

#figure 5#####
db <- read_csv("time_series.csv") %>% 
  filter(countryname == "Bolivia")

ggplot() +
  geom_line(data = db, aes(x = year, y = v2xps_party, color = "v2xps_party"), size = 1) +
  geom_line(data = db, aes(x = year, y = avg_psi, color = "average_PSI"), size = 1) +
  theme_bw() +
  geom_rect(aes(xmin = 2005, xmax = 2016, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  ylim(0.35,1) +
  xlim(1985, 2016) +
  ylab("Party Institutionalization") +
  xlab("Year") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("v2xps_party"="black", "average_PSI" = "gray"), labels = c("Average", "PI"), name = "") +
  theme(legend.text=element_text(size=10), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 10)) 

ggplot() +
  geom_line(data = db, aes(x = year, y = normalPS, color = "normalPS"), size = 1) +
  geom_line(data = db, aes(x = year, y = avg_nps, color = "avg_nps"), size = 1) +
  theme_bw() +
  geom_rect(aes(xmin = 2005, xmax = 2016, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  scale_y_continuous(limits = c(0.35, 1), position = "right") +
  xlim(1985, 2016) +
  ylab("Party Strengh") +
  xlab("Year") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("normalPS"="black", "avg_nps" = "gray"), labels = c("Average", "Party Strength"), name = "") +
  theme(legend.text=element_text(size=10), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 10)) 

#fig 6#####
ds <- read_csv("time_series.csv") %>% 
  filter(countryname == "Spain")

ggplot() +
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

ggplot() +
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
#table 3####
d1 <- read_csv("spainvotes.csv") %>% 
  mutate(E2011 = E2011*100, E2015 = E2015*100, E2016 = E2016*100)
#fig 8####
da <- read_csv("time_series.csv") %>% 
  filter(countryname == "Austria")

ggplot() +
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

ggplot() +
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

#fig 9####
df <- read_csv("time_series.csv") %>% 
  filter(countryname == "France")

ggplot() +
  geom_line(data = df, aes(x = year, y = v2xps_party, color = "v2xps_party"), size = 1) +
  geom_line(data = df, aes(x = year, y = avg_psi, color = "average_PSI"), size = 1) +
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

ggplot() +
  geom_line(data = df, aes(x = year, y = normalPS, color = "normalPS"), size = 1) +
  geom_line(data = df, aes(x = year, y = avg_nps, color = "avg_nps"), size = 1) +
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

#fig 10####
du <- read_csv("time_series.csv") %>% 
  filter(countryname == "United States of America")

ggplot() +
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

ggplot() +
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
