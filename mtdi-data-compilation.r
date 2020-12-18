setwd("~/CNYCF/Measuring the Dream Index")

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(directlabels)



# IMPORT ALL DATA

tests <- read_csv("Data Sources/3-8 Assessment Data/ela4-math8_13-18_grouped.csv")
jails <- read_csv("Data Sources/Annual Survey of Jails/annual-survey-of-jails_cny08-18.csv")
gradr <- read_csv("Data Sources/Graduation Rate Database/graduate-database_onondaga_15-19.csv")
pover <- read_csv("Data Sources/Census Poverty by Race & Age/child-poverty-by-race_2017-2018.csv")
infan <- read_csv("Data Sources/County Health Rankings/Infant Mortality Data/infant-mortality-data.csv")
arrst <- read_csv("Data Sources/NYS Arrests & Prison Sentences by Demographic/arrests-by-race_16-18.csv")
lifex <- read_csv("Data Sources/County Health Rankings/Life Expectancy Data/expectancy-by-race_19-20.csv")
mortr <- read_csv("Data Sources/County Health Rankings/Age Adjusted Mortality/age-adjusted-mortality.csv")
incom <- read_csv("Data Sources/Census Median Household Income by Race/median-household-income-by-race.csv")
dispo <- read_csv("Data Sources/NYS Arrests & Prison Sentences by Demographic/dispositions-by-race_onondaga_16-18.csv")
bachs <- read_csv("Data Sources/Census Educational Attainment/5-Year Estimates/5-Year Data/bachelors-or-higher_acs-5-year.csv")



# STANDARDIZE ALL DATA

arrst <- arrst %>%
  filter(category == "Total Arrests") %>%
  select(year:black) %>%
  gather(key = group, 
         value = value, 
         -year, 
         -category) %>%
  arrange(year)

jails <- jails %>%
  filter(county == "Onondaga County") %>%
  select(year, name:white) %>%
  rename(total = population) %>%
  gather(key = group,
         value = value,
         -year,
         -name) %>%
  group_by(year, group) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  arrange(year) %>%
  filter(year >= 2014) %>%
  mutate(category = "Incarcerated") %>%
  select(year, category, group, value)

bachs <- bachs %>%
  mutate(group = ifelse(subgroup_1 %in% c("Black alone",
                                          "Black Alone"), 
                        yes = "black", 
                        no = "white"),
         category = "Bachelors or Higher") %>%
  select(year, category, group, value)


violent <- dispo %>%
  filter(top_charge == "Violent Felony",
         disposition == "Convicted-Sentenced") %>%
  rename(white = white_n,
         black = black_n,
         category = top_charge) %>%
  select(year, category, white, black) %>%
  gather(key = group, 
         value = value,
         -category,
         -year) %>%
  arrange(year)

probation <- dispo %>%
  filter(disposition == "Probation") %>%
  rename(white = white_n,
         black = black_n,
         category = top_charge) %>%
  select(year, category, white, black) %>%
  gather(key = group, 
         value = value,
         -category,
         -year) %>%
  group_by(year, group) %>%
  summarize(value = sum(value)) %>%
  mutate(category = "Probation") %>%
  select(year, category, group, value)

rm(dispo)

graduation <- gradr %>%
  rename(group = subgroup_name,
         value = grad_cnt) %>%
  mutate(category = "Graduation Rate",
         group = ifelse(group == "White", 
                        yes = "white",
                        no = "black")) %>%
  select(year, category, group, value)

advanced <- gradr %>%
  rename(group = subgroup_name,
         value = reg_adv_cnt) %>%
  mutate(category = "Advanced Placement",
         group = ifelse(group == "White", 
                        yes = "white",
                        no = "black")) %>%
  select(year, category, group, value)

rm(gradr)

incom <- incom %>%
  rename(group = race,
         value = estimate) %>%
  mutate(group = tolower(group),
         category = "Median Income") %>%
  select(year, category, group, value) %>%
  arrange(year)

infan <- infan %>%
  rename(year = Year,
         total = Deaths,
         black = Black,
         white = White) %>%
  select(year, total, black, white) %>%
  gather(key = group,
         value = value,
         -year) %>%
  mutate(category = "Infant Mortality") %>%
  select(year, category, group, value) %>%
  arrange(year)

lifex <- lifex %>%
  rename(year = Year,
         black = Black,
         white = White) %>%
  select(year, black, white) %>%
  gather(key = group,
         value = value,
         -year) %>%
  mutate(category = "Life Expectancy") %>%
  select(year, category, group, value) %>%
  arrange(year)

mortr <- mortr %>%
  rename(year = Year,
         total = Deaths,
         black = Black,
         white = White) %>%
  select(year, total, black, white) %>%
  gather(key = group, 
         value = value,
         -year) %>%
  mutate(category = "Mortality Rate") %>%
  select(year, category, group, value) %>%
  arrange(year)

pover <- pover %>%
  rename(group = race,
         value = total) %>%
  mutate(group = tolower(group),
         category = "Child Poverty") %>%
  select(year, category, group, value) %>%
  arrange(year)

reading <- tests %>%
  filter(item_desc == "Grade 4 ELA") %>%
  rename(group = subgroup_name,
         year = school_year,
         category = item_desc) %>%
  mutate(group = ifelse(group == "All Students",
                        "total", group),
         group = ifelse(group == "Black or African American",
                        "black", group),
         group = ifelse(group == "White",
                        "white", group),
         value = level3_total + level4_total) %>%
  select(year, category, group, value)

math <- tests %>%
  filter(item_desc == "Grade 8 Math") %>%
  rename(group = subgroup_name,
         year = school_year,
         category = item_desc) %>%
  mutate(group = ifelse(group == "All Students",
                        "total", group),
         group = ifelse(group == "Black or African American",
                        "black", group),
         group = ifelse(group == "White",
                        "white", group),
         value = level3_total + level4_total) %>%
  select(year, category, group, value)

rm(tests)



# MERGE DATASETS

all <- bind_rows(advanced,
                 arrst,
                 bachs,
                 graduation,
                 incom,
                 infan,
                 jails,
                 lifex,
                 math,
                 mortr,
                 pover,
                 probation,
                 reading,
                 violent)

rm(advanced, arrst, bachs, graduation, incom, 
   infan, jails, lifex, math, mortr, pover, 
   probation, reading, violent)



# IMPORT & PROCESS POPULATION DATA

population <- read_csv("Data Sources/Total Population by Race/Race and Ethnicity.csv") %>%
  filter(Race %in% c("Black or African American Alone",
                     "White Alone"),
         Ethnicity == "Not Hispanic or Latino") %>%
  rename(group = Race,
         year = Year,
         population = Population) %>%
  mutate(group = ifelse(group == "White Alone",
                        "white", "black")) %>%
  select(year, group, population) %>%
  arrange(year)

imputation <- tibble(year = c(2019, 2019, 2020, 2020),
                     group = c("black", "white", "black", "white"),
                     population = c(53225, 353347, 53225, 353347))

population <- bind_rows(population,
                        imputation)



# MERGE POPULATION DATA

all <- all %>%
  left_join(population) %>%
  filter(group != "total")

rm(imputation, population)



# MTDI CALCULATIONS

all <- all %>%
  mutate(percentage = (value / population),
         score = ifelse(category %in% c("Total Arrests",
                                        "Infant Mortality",
                                        "Incarcerated",
                                        "Mortality Rate",
                                        "Child Poverty",
                                        "Probation",
                                        "Violent Felony"),
                        yes = percentage * -1,
                        no = percentage),
         group = str_to_title(group))

all <- all %>% 
  mutate(domain = NA,
         domain = ifelse(test = category %in% c("Advanced Placement",
                                                "Bachelors or Higher",
                                                "Graduation Rate",
                                                "Grade 8 Math",
                                                "Grade 4 ELA"), 
                         yes = "Education", domain),
         domain = ifelse(test = category %in% c("Total Arrests",
                                                "Incarcerated",
                                                "Probation",
                                                "Violent Felony"), 
                         yes = "Crime", domain),
         domain = ifelse(test = category %in% c("Median Income",
                                                "Child Poverty"), 
                         yes = "Poverty", domain),
         domain = ifelse(test = category %in% c("Infant Mortality",
                                                "Life Expectancy",
                                                "Mortality Rate"), 
                         yes = "Health", domain)) %>%
  select(year, domain, category, group:score) %>%
  rename(indicator = category)

total <- all %>%
  group_by(year, domain, group) %>%
  summarize(mean = mean(score)) %>%
  ungroup()

mtdi <- total %>%
  group_by(year, group) %>%
  summarize(mean = mean(mean)) %>%
  ungroup() %>%
  mutate(domain = "MTD Index") %>%
  select(year, domain, group, mean)

domains <- bind_rows(total, mtdi)

domains[domains == "Black"] <- "Black or African American"



# VISUALIZATION

mtdi_plot <- ggplot(domains, aes(x = year,
                y = mean,
                color = factor(x = domain, 
                               levels = c("MTD Index",
                                          "Crime",
                                          "Education",
                                          "Health",
                                          "Poverty")))) +
  geom_smooth(method = "lm",
              se = FALSE) +
  facet_wrap(~ group) +
  labs(x = "Year",
       y = "Score",
       color = "Domain",
       title = "Measuring the Dream Index, Onondaga County",
       subtitle = "Linear Regression of Scores by Race, 2013-2020") +
  theme_minimal()

ggsave(filename = "mtdi_plot.png", 
       plot = mtdi_plot, 
       width = 7, 
       height = 6, 
       dpi = "retina")



# WRITE TO TABLES

write_csv(domains, "domains.csv")
write_csv(all, "mtdi-all.csv")




# ADDITIONAL PLOTS

mtdi_plain <- ggplot(domains, aes(x = year, 
                    y = mean,
                    color = factor(x = domain, 
                                   levels = c("MTD Index",
                                              "Crime",
                                              "Education",
                                              "Health",
                                              "Poverty")))) +
  geom_path(size = 1) +
  labs(title = "Measuring the Dream Index, Onondaga County",
       subtitle = "Domain & Index Scores by Race, 2013-2020",
       x = "Year",
       y = "Score",
       color = "Domain") +
  theme_minimal() +
  facet_grid(~group)

domains17_18 <- domains %>%
  filter(year %in% c(2017, 2018))

mtdi_17_18 <- ggplot(domains17_18, aes(x = year, 
                                  y = mean,
                                  color = factor(x = domain, 
                                                 levels = c("MTD Index",
                                                            "Crime",
                                                            "Education",
                                                            "Health",
                                                            "Poverty")))) +
  geom_path(size = 1) +
  labs(title = "Measuring the Dream Index, Onondaga County",
       subtitle = "Domain & Index Scores by Race, 2017-2018",
       x = "Year",
       y = "Score",
       color = "Domain") +
  theme_minimal() +
  facet_grid(~group) +
  scale_x_continuous(n.breaks = 2)

ggsave(filename = "mtdi_plain.png", 
       plot = mtdi_plain, 
       width = 7, 
       height = 6, 
       dpi = "retina")

ggsave(filename = "mtdi_17-18.png", 
       plot = mtdi_17_18, 
       width = 7, 
       height = 6, 
       dpi = "retina")



# MTD INDEX OFFICIAL CALCULATION

white_med_income <- all %>%
  filter(indicator == "Median Income",
         group == "White") %>%
  mutate(percentage = 1)

black_med_income <- all %>%
  filter(indicator == "Median Income",
         group == "Black") %>%
  mutate(population = c(62805,
                        64876),
         percentage = value / population)

all <- all %>%
  filter(indicator != "Median Income") %>%
  bind_rows(white_med_income,
            black_med_income) %>%
  arrange(year, domain, indicator)

black <- all %>%
  select(year:group, percentage) %>%
  filter(group == "Black") %>%
  arrange(year, domain, indicator) %>%
  rename(black = percentage) %>%
  select(-group)

white <- all %>%
  select(year:group, percentage) %>%
  filter(group == "White") %>%
  arrange(year, domain, indicator) %>%
  rename(white = percentage) %>%
  select(-group)

alt <- black %>%
  left_join(white)



mtdi <- alt %>%
  mutate(ratio = ifelse(indicator %in% c("Total Arrests",
                                         "Infant Mortality",
                                         "Incarcerated",
                                         "Mortality Rate",
                                         "Child Poverty",
                                         "Probation",
                                         "Violent Felony",
                                         "Life Expectancy"),
                        yes = white / black, 
                        no = black / white),
         ratio = ifelse(ratio > 1, 1, ratio))

mtdi_domains <- mtdi %>%
  group_by(year, domain) %>%
  summarize(value = mean(ratio)) %>%
  ungroup() %>%
  filter(year != 2020)

mtdi_totals <- mtdi_domains %>%
  group_by(year) %>%
  summarize(value = mean(value)) %>%
  ungroup() %>%
  filter(year != 2020) %>%
  mutate(domain = "MTD Index") %>%
  select(year, domain, value)

final <- bind_rows(mtdi_domains, 
                   mtdi_totals) %>%
  arrange(domain, year)

final_plot <- ggplot(final, 
       aes(x = year, 
           y = value,
           color = factor(x = domain, 
                          levels = c("MTD Index",
                                     "Crime",
                                     "Education",
                                     "Health",
                                     "Poverty")))) +
  geom_path(size = 1.05) +
  labs(title = "Measuring the Dream Index",
       subtitle = "Onondaga County, 2013-2019",
       x = "Year",
       y = "Composite Score",
       color = "Domain") +
  theme_minimal()

ggsave(filename = "mtdi-final.png", 
       plot = final_plot,
       width = 5,
       height = 4,
       dpi = "retina")  

write_csv(final, "mtdi-final.csv")
