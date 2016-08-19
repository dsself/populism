#fig 3-8
#library(hadleyverse)
#library(cowplot)
#library(zoo)
#setwd("C:/Users/Darin/Documents/populism/descriptive")
`%notin%` <- function(x,y) !(x %in% y) 

df <- read_csv("Master Panel.csv") %>% 
  select(countryname, year, v2xps_party, PI_7) 


avg <- read_csv("Master Panel.csv") %>% 
  mutate(polityl = lag(polity2, n = 1)) %>% 
  mutate(polity = ifelse(year == 2015, rollapply(data = polityl, width = 5, 
                                                 FUN = mean, 
                                                 align = "right", 
                                                 fill = NA, 
                                                 na.rm = T), polity2)) %>% 
  select(countryname, year, v2xps_party, PI_7, polity) %>% 
  filter(polity >= 6)  %>% 
  group_by(year) %>% 
  mutate(avg_psi = mean(v2xps_party, na.rm = TRUE)) %>% 
  mutate(avg_pi7 = mean(PI_7, na.rm = TRUE)) %>% 
  select(year, avg_psi, avg_pi7) %>% 
  unique() %>% 
  arrange(year) 

df <- read_csv("Master Panel.csv") %>% 
  select(countryname, year, v2xps_party, PI_7) %>% 
  left_join(avg, by = "year")


###Venezuela####
dv <- df %>% 
  filter(countryname == "Venezuela")

v1 <- ggplot() +
  geom_line(data = dv, aes(x = year, y = v2xps_party, color = "v2xps_party"), size = 1) +
  geom_line(data = dv, aes(x = year, y = avg_psi, color = "average_PSI"), size = 1) +
  theme_bw() +
  geom_rect(aes(xmin = 1999, xmax = 2015, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  ylim(0,1) +
  xlim(1985, 2015) +
  ylab("Party Institutionalization") +
  xlab("Year") +
  #ggtitle("Venezuelan Party Institutionalization \nand Party Strength") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("v2xps_party"="black", "average_PSI" = "gray"), labels = c("Average", "PSI"), name = "") +
  theme(legend.text=element_text(size=6), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 10)) 

v2 <- ggplot() +
  geom_line(data = dv, aes(x = year, y = PI_7, color = "PI_7"), size = 1) +
  geom_line(data = dv, aes(x = year, y = avg_pi7, color = "avg_pi7"), size = 1) +
  theme_bw() +
  geom_rect(aes(xmin = 1999, xmax = 2015, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  ylim(-1.5,1.5) +
  xlim(1985, 2015) +
  ylab("Party Strengh") +
  xlab("Year") +
  #ggtitle("Venezuelan Party Institutionalization \nand Party Strength") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("PI_7"="black", "avg_pi7" = "gray"), labels = c("Average", "Party Strength"), name = "") +
  theme(legend.text=element_text(size=6), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 10)) 

v2 <- ggdraw(switch_axis_position(v2, axis = 'y'))

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
  geom_rect(aes(xmin = 2006, xmax = 2012, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  #geom_rect(aes(xmin = 2002, xmax = 2012, ymin = 0, ymax = Inf), fill = "gray", alpha= 0.2) +
  ylim(0,1) +
  xlim(1985, 2012) +
  ylab("Party Institutionalization") +
  xlab("Year") +
  #ggtitle("Venezuelan Party Institutionalization \nand Party Strength") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("v2xps_party"="black", "average_PSI" = "gray"), labels = c("Average", "PSI"), name = "") +
  theme(legend.text=element_text(size=6), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 10)) 

b2 <- ggplot() +
  geom_line(data = db, aes(x = year, y = PI_7, color = "PI_7"), size = 1) +
  geom_line(data = db, aes(x = year, y = avg_pi7, color = "avg_pi7"), size = 1) +
  theme_bw() +
  geom_rect(aes(xmin = 2006, xmax = 2012, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  #geom_rect(aes(xmin = 2002, xmax = 2012, ymin = 0, ymax = Inf), fill = "gray", alpha= 0.2) +
  ylim(-1.5,1.5) +
  xlim(1985, 2012) +
  ylab("Party Strengh") +
  xlab("Year") +
  #ggtitle("Venezuelan Party Institutionalization \nand Party Strength") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("PI_7"="black", "avg_pi7" = "gray"), labels = c("Average", "Party Strength"), name = "") +
  theme(legend.text=element_text(size=6), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 10)) 

b2 <- ggdraw(switch_axis_position(b2, axis = 'y'))

ggsave("bolivia1.jpg", plot = b1)
ggsave("bolivia2.jpg", plot = b2)

###Thailand####
dt <- df %>% 
  filter(countryname == "Thailand")

t1 <- ggplot() +
  geom_line(data = dt, aes(x = year, y = v2xps_party, color = "v2xps_party"), size = 1) +
  geom_line(data = dt, aes(x = year, y = avg_psi, color = "average_PSI"), size = 1) +
  theme_bw() +
  geom_rect(aes(xmin = 2001, xmax = 2006, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  ylim(0,1) +
  xlim(1990, 2015) +
  ylab("Party Institutionalization") +
  xlab("Year") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("v2xps_party"="black", "average_PSI" = "gray"), labels = c("Average", "PSI"), name = "") +
  theme(legend.text=element_text(size=6), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 10)) 

t2 <- ggplot() +
  geom_line(data = dt, aes(x = year, y = PI_7, color = "PI_7"), size = 1) +
  geom_line(data = dt, aes(x = year, y = avg_pi7, color = "avg_pi7"), size = 1) +
  theme_bw() +
  geom_rect(aes(xmin = 2001, xmax = 2006, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  ylim(-1.5,1.5) +
  xlim(1990, 2015) +
  ylab("Party Strengh") +
  xlab("Year") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("PI_7"="black", "avg_pi7" = "gray"), labels = c("Average", "Party Strength"), name = "") +
  theme(legend.text=element_text(size=6), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 10)) 

t2 <- ggdraw(switch_axis_position(t2, axis = 'y'))

ggsave("thailand1.jpg", plot = t1)
ggsave("thailand2.jpg", plot = t2)

###Philippines####
dp <- df %>% 
  filter(countryname == "Philippines")

p1 <- ggplot() +
  geom_line(data = dp, aes(x = year, y = v2xps_party, color = "v2xps_party"), size = 1) +
  geom_line(data = dp, aes(x = year, y = avg_psi, color = "average_PSI"), size = 1) +
  theme_bw() +
  #geom_rect(aes(xmin = 2001, xmax = 2006, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  ylim(0,1) +
  xlim(1990, 2015) +
  ylab("Party Institutionalization") +
  xlab("Year") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("v2xps_party"="black", "average_PSI" = "gray"), labels = c("Average", "PSI"), name = "") +
  theme(legend.text=element_text(size=6), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 10)) 

p2 <- ggplot() +
  geom_line(data = dp, aes(x = year, y = PI_7, color = "PI_7"), size = 1) +
  geom_line(data = dp, aes(x = year, y = avg_pi7, color = "avg_pi7"), size = 1) +
  theme_bw() +
  #geom_rect(aes(xmin = 2001, xmax = 2006, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  ylim(-1.5,1.5) +
  xlim(1990, 2015) +
  ylab("Party Strengh") +
  xlab("Year") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("PI_7"="black", "avg_pi7" = "gray"), labels = c("Average", "Party Strength"), name = "") +
  theme(legend.text=element_text(size=6), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 10)) 

p2 <- ggdraw(switch_axis_position(p2, axis = 'y'))

ggsave("philippines1.jpg", plot = p1)
ggsave("philippines2.jpg", plot = p2)

###France####
dfr <- df %>% 
  filter(countryname == "France")

f1 <- ggplot() +
  geom_line(data = dfr, aes(x = year, y = v2xps_party, color = "v2xps_party"), size = 1) +
  geom_line(data = dfr, aes(x = year, y = avg_psi, color = "average_PSI"), size = 1) +
  theme_bw() +
  geom_rect(aes(xmin = 1988, xmax = 2015, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  ylim(0,1) +
  xlim(1980, 2015) +
  ylab("Party Institutionalization") +
  xlab("Year") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("v2xps_party"="black", "average_PSI" = "gray"), labels = c("Average", "PSI"), name = "") +
  theme(legend.text=element_text(size=6), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 10)) 

f2 <- ggplot() +
  geom_line(data = dfr, aes(x = year, y = PI_7, color = "PI_7"), size = 1) +
  geom_line(data = dfr, aes(x = year, y = avg_pi7, color = "avg_pi7"), size = 1) +
  theme_bw() +
  geom_rect(aes(xmin = 1988, xmax = 2015, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  ylim(-1.5,1.5) +
  xlim(1980, 2015) +
  ylab("Party Strengh") +
  xlab("Year") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("PI_7"="black", "avg_pi7" = "gray"), labels = c("Average", "Party Strength"), name = "") +
  theme(legend.text=element_text(size=6), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 10)) 

f2 <- ggdraw(switch_axis_position(f2, axis = 'y'))

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
  ylim(0,1) +
  xlim(1980, 2015) +
  ylab("Party Institutionalization") +
  xlab("Year") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("v2xps_party"="black", "average_PSI" = "gray"), labels = c("Average", "PSI"), name = "") +
  theme(legend.text=element_text(size=6), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 10)) 

a2 <- ggplot() +
  geom_line(data = da, aes(x = year, y = PI_7, color = "PI_7"), size = 1) +
  geom_line(data = da, aes(x = year, y = avg_pi7, color = "avg_pi7"), size = 1) +
  theme_bw() +
  geom_rect(aes(xmin = 1986, xmax = 2015, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  ylim(-1.5,1.5) +
  xlim(1980, 2015) +
  ylab("Party Strengh") +
  xlab("Year") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("PI_7"="black", "avg_pi7" = "gray"), labels = c("Average", "Party Strength"), name = "") +
  theme(legend.text=element_text(size=6), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 10)) 

a2 <- ggdraw(switch_axis_position(a2, axis = 'y'))

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
  ylim(0,1) +
  xlim(1990, 2015) +
  ylab("Party Institutionalization") +
  xlab("Year") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("v2xps_party"="black", "average_PSI" = "gray"), labels = c("Average", "PSI"), name = "") +
  theme(legend.text=element_text(size=6), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 10)) 

u2 <- ggplot() +
  geom_line(data = du, aes(x = year, y = PI_7, color = "PI_7"), size = 1) +
  geom_line(data = du, aes(x = year, y = avg_pi7, color = "avg_pi7"), size = 1) +
  theme_bw() +
  geom_rect(aes(xmin = 2008, xmax = 2015, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  ylim(-1.5,1.5) +
  xlim(1990, 2015) +
  ylab("Party Strengh") +
  xlab("Year") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("PI_7"="black", "avg_pi7" = "gray"), labels = c("Average", "Party Strength"), name = "") +
  theme(legend.text=element_text(size=6), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 10)) 

u2 <- ggdraw(switch_axis_position(u2, axis = 'y'))

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
  ylim(0,1) +
  xlim(1990, 2015) +
  ylab("Party Institutionalization") +
  xlab("Year") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("v2xps_party"="black", "average_PSI" = "gray"), labels = c("Average", "PSI"), name = "") +
  theme(legend.text=element_text(size=6), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 10)) 

s2 <- ggplot() +
  geom_line(data = ds, aes(x = year, y = PI_7, color = "PI_7"), size = 1) +
  geom_line(data = ds, aes(x = year, y = avg_pi7, color = "avg_pi7"), size = 1) +
  theme_bw() +
  geom_rect(aes(xmin = 2014, xmax = 2015, ymin = -Inf, ymax = Inf), fill = "gray", alpha= 0.2) +
  ylim(-1.5,1.5) +
  xlim(1990, 2015) +
  ylab("Party Strengh") +
  xlab("Year") +
  theme(plot.title = element_text(size = 10, lineheight=.8, face="bold"), legend.position = "bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("PI_7"="black", "avg_pi7" = "gray"), labels = c("Average", "Party Strength"), name = "") +
  theme(legend.text=element_text(size=6), legend.title=element_text(size=10, face = "bold"),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 10)) 

s2 <- ggdraw(switch_axis_position(s2, axis = 'y'))

ggsave("spain1.jpg", plot = s1)
ggsave("spain2.jpg", plot = s2)
