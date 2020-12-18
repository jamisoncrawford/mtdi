setwd("~/CNYCF/Measuring the Dream Index/Data Sources/Census Poverty by Race & Age")

library(readr)
library(dplyr)
library(stringr)

black17 <- read_csv("ACSDT5Y2017.B17001B_data_with_overlays_2020-08-15T150052.csv", skip = 1)
black18 <- read_csv("ACSDT5Y2018.B17001B_data_with_overlays_2020-08-15T150037.csv", skip = 1)
white17 <- read_csv("ACSDT5Y2017.B17001A_data_with_overlays_2020-08-15T145855.csv", skip = 1)
white18 <- read_csv("ACSDT5Y2018.B17001A_data_with_overlays_2020-08-15T145818.csv", skip = 1)

black17 <- data.frame(t(black17)) %>% 
  add_rownames("indicator") %>% 
  mutate(race = "Black",
         year = 2017) %>%
  rename(value = `t.black17.`) %>%
  select(year, race, value, indicator)

black18 <- data.frame(t(black18)) %>% 
  add_rownames("indicator") %>% 
  mutate(race = "Black",
         year = 2018) %>%
  rename(value = `t.black18.`) %>%
  select(year, race, value, indicator)

white17 <- data.frame(t(white17)) %>% 
  add_rownames("indicator") %>% 
  mutate(race = "White",
         year = 2017) %>%
  rename(value = `t.white17.`) %>%
  select(year, race, value, indicator)

white18 <- data.frame(t(white18)) %>% 
  add_rownames("indicator") %>% 
  mutate(race = "White",
         year = 2018) %>%
  rename(value = `t.white18.`) %>%
  select(year, race, value, indicator)

all <- bind_rows(black17,
                 black18,
                 white17,
                 white18)

rm(black17, black18, white17, white18)

all <- all %>%
  filter(!grepl(x = indicator, "^Estimate!!Total$"),
         !grepl(x = indicator, "Margin of Error"),
         !grepl(x = indicator, "Geographic Area"),
         !grepl(x = indicator, "^id$"))

all <- all %>%
  bind_cols(data.frame(str_split(string = all$indicator,
                                 pattern = "!!", 
                                 simplify = TRUE))) %>%
  rename(stat = `X1`,
         aggregation = `X2`,
         description = `X3`,
         sex = `X4`,
         group = `X5`) %>%
  filter(group %in% c("Under 5 years",
                      "5 years",
                      "6 to 11 years",
                      "12 to 14 years",
                      "15 years",
                      "16 and 17 years"),
         description == "Income in the past 12 months below poverty level") %>%
  mutate(value = as.numeric(value)) %>%
  group_by(year, race) %>%
  summarize(total = sum(value)) %>%
  ungroup()

write_csv(all, "child-poverty-by-race_2017-2018.csv")
