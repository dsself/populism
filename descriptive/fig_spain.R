#table
#library(hadleyverse)
#library(stargazer)
#setwd("C:/Users/Darin/Documents/populism/descriptive")


df <- read_csv("spainvotes.csv")

stargazer(df, summary = F, title = "Elections Results in Spain (2011-2016)", rownames = F)
