
# SET WORKING DIRECTORY, LOAD PACKAGES

rm(list = ls())

setwd("~/CNYCF/Measuring the Dream Index/Data Sources/Graduation Rate Database")

library(readr)
library(dplyr)
library(stringr)



# READ IN DATA

grad15 <- read_csv("GRAD_RATE_AND_OUTCOMES_2015.csv")
grad16 <- read_csv("GRAD_RATE_AND_OUTCOMES_2016.csv")
grad17 <- read_csv("GRAD_RATE_AND_OUTCOMES_2017.csv")
grad18 <- read_csv("GRAD_RATE_AND_OUTCOMES_2018.csv")
grad19 <- read_csv("GRAD_RATE_AND_OUTCOMES_2019.csv")

names(grad15) <- tolower(names(grad15))
names(grad16) <- tolower(names(grad16))
names(grad17) <- tolower(names(grad17))
names(grad18) <- tolower(names(grad18))
names(grad19) <- tolower(names(grad19))

grad15[grad15 == "-"] <- NA
grad16[grad16 == "-"] <- NA
grad17[grad17 == "-"] <- NA
grad18[grad18 == "-"] <- NA
grad19[grad19 == "-"] <- NA

all <- bind_rows(grad15 %>% mutate(enroll_cnt = as.integer(enroll_cnt)),
                 grad16 %>% mutate(enroll_cnt = as.integer(enroll_cnt)) %>% 
                   select(-entity_inactive_date),
                 grad17 %>% mutate(enroll_cnt = as.integer(enroll_cnt)),
                 grad18 %>% mutate(enroll_cnt = as.integer(enroll_cnt)),
                 grad19 %>% mutate(enroll_cnt = as.integer(enroll_cnt)) %>% 
                   select(-entity_inactive_date))

rm(list = ls()[grepl("^grad", ls())])

all <- all %>%
  filter(aggregation_name == "County: ONONDAGA",
         grepl("6 Year Outcome$", membership_desc),
         subgroup_name %in% c("Black", "White", "Black or African American")) %>%
  mutate(report_school_year = substring(report_school_year, 1, 4),
         report_school_year = as.numeric(report_school_year) + 1,
         county = "Onondaga",
         subgroup_name = ifelse(subgroup_name == "Black",
                                "Black or African American",
                                subgroup_name)) %>%
  rename(year = report_school_year,
         aggregation = aggregation_type) %>%
  select(year, aggregation, county, membership_desc, 
         subgroup_name:dropout_pct) %>%
  mutate(grad_pct = as.numeric(gsub("%", "", grad_pct)) / 100,
         local_pct = as.numeric(gsub("%", "", local_pct)) / 100,
         reg_pct = as.numeric(gsub("%", "", reg_pct)) / 100,
         reg_adv_pct = as.numeric(gsub("%", "", reg_adv_pct)) / 100,
         non_diploma_credential_pct = as.numeric(gsub("%", "", non_diploma_credential_pct)) / 100,
         still_enr_pct = as.numeric(gsub("%", "", still_enr_pct)) / 100,
         ged_pct = as.numeric(gsub("%", "", ged_pct)) / 100,
         dropout_pct = as.numeric(gsub("%", "", dropout_pct)) / 100)

write_csv(all, "graduate-database_onondaga_15-19.csv")
