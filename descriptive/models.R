library(tidyverse)
library(stargazer)
library(margins)
library(broom)
library(sandwich)
'%notin%' <- function(x,y)!('%in%'(x,y))


setwd("descriptive/replication")

df <- read_csv("panel_v8.csv") %>% 
  mutate(lgdm = log(dm)) %>% 
  mutate(lgscore = log1p(score)) 

lm1 <- lm(lgscore ~ normalPS, data = df)
sd1 <- sqrt(diag(vcovHC(lm1, "HC1")))
lm2 <- lm(lgscore ~ normalPS + lgdm, data = df)
sd2 <- sqrt(diag(vcovHC(lm2, "HC1")))
lm3 <- lm(lgscore ~ lag_ps + lgdm + lgdppc, data = df)
summary(lm3)
sd3 <- sqrt(diag(vcovHC(lm3, "HC1")))

lm4 <- lm(lgscore ~ v2xps_party, data = df)
sd4 <- sqrt(diag(vcovHC(lm4, "HC1")))
lm5 <- lm(lgscore ~ v2xps_party + lgdm, data = df)
sd5 <- sqrt(diag(vcovHC(lm5, "HC1")))
lm6<- lm(lgscore ~ v2xps_party + lgdm + lgdppc, data = df)
sd6 <- sqrt(diag(vcovHC(lm6, "HC1")))

stargazer(lm1, lm2, lm3, lm4, lm5, lm6, se = list(sd1, sd2, sd3, sd4, sd5, sd6), digits = 2, omit.stat = c("f", "ser"), order = c(1, 4, 2, 3), 
          covariate.labels = c("Party Strength", "Party Institutionalization", "District Magnitude", "GDP PC"))
          
          
ggplot(df) +
  geom_point(aes(x = lag_ps, y = lgscore))

#lm <- lm(score ~ normalPS + avemag_bothtiers + e_migdppcln, data = df)
#summary(lm)

#tidy1 <- tidy(lm) %>% 
#  mutate(std.error = sqrt(diag(vcovHC(lm, "HC1")))) %>% 
#  mutate(lo = estimate - qt(.975, nrow(df))*std.error) %>% 
#  mutate(hi = estimate + qt(.975, nrow(df))*std.error) %>% 
#  filter(term %in% c("normalPS", "avemag_bothtiers", "e_migdppcln")) %>% 
#  mutate(covariate = c("Party Strength", "District Magnitude", "GDP_PC"))

#ggplot(tidy1, aes(x=covariate, y=estimate, ymin=lo, ymax=hi)) +
#  geom_pointrange(show.legend = F) +
#  geom_hline(yintercept=0, linetype = "dashed") +
#  geom_line(aes(y=estimate-100)) +
#  geom_point(aes(y=estimate-100), size=2.5) +
#  theme_bw() +
#  xlab("") +
#  ylab("Estimate") +
#  ylim(-50, 1) +
#  scale_color_grey(guide = guide_legend(title = "Regime")) +
#  theme(legend.background = element_rect(fill = alpha(.1)), legend.position = "bottom") +
#  theme(plot.title = element_text(hjust = 0.5))