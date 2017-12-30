
avg <- read_csv("Master Panel.csv") %>% 
  mutate(normalPI = (PI_7-min(PI_7, na.rm = TRUE))/(max(PI_7, na.rm = TRUE)-min(PI_7, na.rm = TRUE))) %>% 
  mutate(polityl = lag(polity2, n = 1)) %>% 
  mutate(polity = ifelse(year == 2015, rollapply(data = polityl, width = 5, 
                                                 FUN = mean, 
                                                 align = "right", 
                                                 fill = NA, 
                                                 na.rm = T), polity2)) %>% 
  select(countryname, year, v2xps_party, PI_7, polity, normalPI) %>% 
  filter(polity >= 6)  %>% 
  group_by(year) %>% 
  mutate(avg_psi = mean(v2xps_party, na.rm = TRUE)) %>% 
  mutate(avg_pi7 = mean(PI_7, na.rm = TRUE)) %>% 
  mutate(avg_nps = mean(normalPI, na.rm = TRUE)) %>% 
  select(year, avg_psi, avg_pi7, avg_nps) %>% 
  unique() %>% 
  arrange(year) 

df <- read_csv("Master Panel.csv") %>% 
  mutate(normalPS = (PI_7-min(PI_7, na.rm = TRUE))/(max(PI_7, na.rm = TRUE)-min(PI_7, na.rm = TRUE))) %>% 
  select(countryname, year, v2xps_party, PI_7, normalPS) %>% 
  left_join(avg, by = "year") %>% 
  filter(countryname %in% c("Venezuela", "Bolivia", "France", "Austria", "Spain", "United States of America"))


write_csv(df, "time_series.csv")