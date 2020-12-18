
# SET WORKIND DIRECTORY & LOAD PACKAGES

rm(list = ls())

setwd("~/CNYCF/Measuring the Dream Index/Data Sources/NYS Arrests & Prison Sentences by Demographic")

library(zoo)
library(readr)
library(dplyr)
library(readxl)
library(stringr)



# READ IN, HONE, & WRITE ARREST BY CRIME & ETHNORACE DATA

arrests16 <- read_excel("arrests-by-ethnorace-2016.xls", 2, 
                        skip = 15, n_max = 11) %>% 
  select(-2) %>% 
  mutate(year = 2016) %>%
  select(year, 1:7)

arrests17 <- read_excel("arrests-by-ethnorace-2017.xls", 2, 
                        skip = 15, n_max = 11) %>% 
  select(-2) %>% 
  mutate(year = 2017) %>%
  select(year, 1:7)

arrests18 <- read_excel("arrests-by-ethnorace-2018.xls", 2, 
                        skip = 15, n_max = 11) %>% 
  select(-2) %>% 
  mutate(year = 2018) %>%
  select(year, 1:7)

all <- bind_rows(arrests16, arrests17, arrests18)

names(all) <- c("year", "category", "total", "white", 
                "black", "hispanic", "asian", "other")

rm(arrests16, arrests17, arrests18)

write_csv(all, "arrests-by-race_16-18.csv")

rm(all)



# READ IN, HONE, & WRITE DISPOSITION BY ETHNORACE DATA

disp16 <- read_excel("dispositions-by-ethnorace_onondaga-2016.xls", 2, skip = 5) %>%
  filter(!is.na(`%...13`))

disp17 <- read_excel("dispositions-by-ethnorace_onondaga-2017.xls", 2, skip = 5) %>%
  filter(!is.na(`%...13`))

disp18 <- read_excel("dispositions-by-ethnorace_onondaga-2018.xls", 2, skip = 5) %>%
  filter(!is.na(`%...13`))

var_names <- c("top_charge", "indicator", "disposition", "white_n", 
               "white_perc", "black_n", "black_perc", "hispanic_n", 
               "hispanic_perc", "other_n", "other_perc", "total_n", 
               "total_perc")

names(disp16) <- var_names
names(disp17) <- var_names
names(disp18) <- var_names

rm(var_names)

disp16 <- disp16 %>% 
  mutate(top_charge = na.locf(top_charge),
         disposition = ifelse(is.na(disposition), indicator, disposition),
         indicator = na.locf(indicator),
         indicator = ifelse(!grepl("^Adult|^Youthful|^Sentence", indicator), 
                            "Overall", indicator),
         indicator = gsub(" for:| to:", "", indicator),
         year = 2016) %>%
  select(year, top_charge:total_perc)

disp17 <- disp17 %>% 
  mutate(top_charge = na.locf(top_charge),
         disposition = ifelse(is.na(disposition), indicator, disposition),
         indicator = na.locf(indicator),
         indicator = ifelse(!grepl("^Adult|^Youthful|^Sentence", indicator), 
                            "Overall", indicator),
         indicator = gsub(" for:| to:", "", indicator),
         year = 2017) %>%
  select(year, top_charge:total_perc)

disp18 <- disp18 %>% 
  mutate(top_charge = na.locf(top_charge),
         disposition = ifelse(is.na(disposition), indicator, disposition),
         indicator = na.locf(indicator),
         indicator = ifelse(!grepl("^Adult|^Youthful|^Sentence", indicator), 
                            "Overall", indicator),
         indicator = gsub(" for:| to:", "", indicator),
         year = 2018) %>%
  select(year, top_charge:total_perc)

all <- bind_rows(disp16, disp17, disp18)

rm(disp16, disp17, disp18)

write_csv(all, "dispositions-by-race_onondaga_16-18.csv")
