
# ACS 5-YEAR ESTIMATES ON EDUCATIONAL ATTAINMENT

wd <- paste0("~/CNYCF/Measuring the Dream Index/Data Sources/",
             "Census Educational Attainment/5-Year Estimates/5-Year Data")

setwd(wd); rm(wd)

library(readr)
library(dplyr)
library(stringr)

edu10_14 <- bind_rows(
  data.frame(t(read_csv("ACSST5Y2010.S1501_data_with_overlays_2020-08-04T140841.csv"))) %>% mutate(year = 2010),
  data.frame(t(read_csv("ACSST5Y2011.S1501_data_with_overlays_2020-08-04T140841.csv"))) %>% mutate(year = 2011),
  data.frame(t(read_csv("ACSST5Y2012.S1501_data_with_overlays_2020-08-04T140841.csv"))) %>% mutate(year = 2012),
  data.frame(t(read_csv("ACSST5Y2013.S1501_data_with_overlays_2020-08-04T140841.csv"))) %>% mutate(year = 2013),
  data.frame(t(read_csv("ACSST5Y2014.S1501_data_with_overlays_2020-08-04T140841.csv"))) %>% mutate(year = 2014)) %>%
  rename(indicator = `X1`,
         value = `X2`) %>%
  filter(indicator != "id",
         indicator != "Geographic Area Name")

edu15_18 <- bind_rows(
  data.frame(t(read_csv("ACSST5Y2015.S1501_data_with_overlays_2020-08-04T140841.csv"))) %>% mutate(year = 2015),
  data.frame(t(read_csv("ACSST5Y2016.S1501_data_with_overlays_2020-08-04T140841.csv"))) %>% mutate(year = 2016),
  data.frame(t(read_csv("ACSST5Y2017.S1501_data_with_overlays_2020-08-04T140841.csv"))) %>% mutate(year = 2017),
  data.frame(t(read_csv("ACSST5Y2018.S1501_data_with_overlays_2020-08-04T140841.csv"))) %>% mutate(year = 2018)) %>%
  rename(indicator = `X1`,
         value = `X2`) %>%
  filter(indicator != "id",
         indicator != "Geographic Area Name")

all <- bind_rows(edu10_14 %>%
                   bind_cols(data.frame(str_split(string = edu10_14$indicator, 
                                                  pattern = "!!", 
                                                  simplify = TRUE))),
                 edu15_18 %>%
                   bind_cols(data.frame(str_split(string = edu15_18$indicator, 
                                                  pattern = "!!", 
                                                  simplify = TRUE)))) %>%
  rename(aggregation = `X1`,
         statistic = `X2`,
         subgroup_1 = `X3`,
         subgroup_2 = `X4`,
         subgroup_3 = `X5`) %>%
  mutate(subgroup_1 = str_to_title(subgroup_1))

rm(edu10_14, edu15_18)



# FILTERING, GROUPING, & SUMMARIES

all <- all %>%
  filter(grepl("Estimate", indicator),
         grepl("Bachelor's degree or higher", indicator),
         grepl("Total", indicator),
         grepl("Black alone", indicator) | grepl("White alone, not Hispanic or Latino", indicator)) %>%
  mutate(aggregation = ifelse(aggregation == "Estimate", "Total", aggregation),
         statistic = ifelse(statistic == "Total", "Estimate", statistic))

index <- which(all$subgroup_1 == "Race And Hispanic Or Latino Origin By Educational Attainment")

all[index, "subgroup_1"] <- all[index, "subgroup_2"]
all[index, "subgroup_2"] <- all[index, "subgroup_3"]

all <- all %>%
  select(year:subgroup_2, value)

write_csv(all, "bachelors-or-higher_acs-5-year.csv")
