setwd("~/CNYCF/Measuring the Dream Index/Data Sources/Census Median Household Income by Race")

library(readr)
library(dplyr)
library(stringr)

black17 <- read_csv("ACSDT5Y2017.B19013B_data_with_overlays_2020-08-15T162714.csv", skip = 1)
black18 <- read_csv("ACSDT5Y2018.B19013B_data_with_overlays_2020-08-15T162630.csv", skip = 1)
white17 <- read_csv("ACSDT5Y2017.B19013A_data_with_overlays_2020-08-15T162714.csv", skip = 1)
white18 <- read_csv("ACSDT5Y2018.B19013A_data_with_overlays_2020-08-15T162630.csv", skip = 1)

names(black17) <- c("id", "geo", "estimate", "error")
names(black18) <- c("id", "geo", "estimate", "error")
names(white17) <- c("id", "geo", "estimate", "error")
names(white18) <- c("id", "geo", "estimate", "error")

black17 <- black17 %>%
  mutate(year = 2017,
         race = "Black") %>%
  select(year, race, estimate)

black18 <- black18 %>%
  mutate(year = 2018,
         race = "Black") %>%
  select(year, race, estimate)

white17 <- white17 %>%
  mutate(year = 2017,
         race = "White") %>%
  select(year, race, estimate)

white18 <- white18 %>%
  mutate(year = 2018,
         race = "White") %>%
  select(year, race, estimate)

all <- bind_rows(black17, 
                 black18,
                 white17,
                 white18)

rm(black17, black18, white17, white18)

write_csv(all, "median-household-income-by-race.csv")
