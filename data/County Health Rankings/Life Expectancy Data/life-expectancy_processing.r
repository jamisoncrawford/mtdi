setwd("~/CNYCF/Measuring the Dream Index/Data Sources/County Health Rankings/Mortality Rates Data")

library(readr)
library(dplyr)

expectancy <- bind_rows(
  read_csv("2019-life-expectancy.csv") %>% 
    mutate(Year = 2019) %>% 
    rename(Total = "County Value") %>% 
    select(Year, County, Total, Black, White) %>%
    filter(County == "Onondaga"),
  read_csv("2020-life-expectancy.csv") %>% 
    mutate(Year = 2020) %>%
    rename(Total = "County Value") %>%
    select(Year, County, Total, Black, White) %>%
    filter(County == "Onondaga"))

write_csv(expectancy, "expectancy-by-race_19-20.csv")