
library(readxl)
library(tidyverse)


setwd('D:/Daniel/msc_applied_ds/t3_dissertation/data/alt_currencies/')

employment <- read_excel('employment_rate_nuts2_germany.xlsx', sheet = 'Sheet 1',
                         range = 'A10:AW48') %>% 
  select(-c(1, seq(4,48,2))) %>% 
  rename(geop_entity = TIME...2) %>% 
  mutate_at(.vars = vars(as.character(seq(1999,2004,1))), as.numeric) %>% 
  pivot_longer(cols = `1999`:`2022`, names_to = 'years', 
               values_to = 'employment_rate')


freight_air1 <- read_excel('freight_mail_air_transport.xlsx', sheet = 'Sheet 1',
                           range = 'A11:AT34') %>% 
  select(!seq(3,45,2)) %>% 
  rename(city_airport = TIME) %>% 
  mutate_at(.vars = rev(as.character(seq(2000,2022,1))), as.numeric) %>% 
  pivot_longer(cols = `2022`:`2000`, names_to = 'year', 
               values_to = 'freight_ton') %>% 
  arrange(city_airport, year)


freight_tr <- read_excel('freight_air_transport_nuts2.xlsx', sheet = 'Sheet 1',
                         range = 'A9:M42', na = ':') %>% 
  rename(geop_entity = TIME) %>% 
  pivot_longer(cols = `2010`:`2021`, names_to = 'year', 
               values_to = 'freight_transport') %>% 
  na.omit()
  

nat_freight <- read_excel('national_freight_mail_air_transport.xlsx', 
                          sheet = 'Sheet 1', range = 'A9:AT27', na = ':') %>% 
  select(!seq(3,45,2)) %>% 
  rename(city_airport = TIME) %>% 
  pivot_longer(`2022`:`2000`, names_to = 'year', 
               values_to = 'national_freight') %>% 
  arrange(city_airport, year)

gdp_nuts2 <- read_excel('gdp_nuts2_germany.xlsx', sheet = 'Sheet 1',
                        range = 'A8:AR46') %>% 
  select(-seq(3,43,2)) %>% 
  rename(geop_entity = TIME) %>% 
  pivot_longer(cols = `2000`:`2021`, names_to = 'year', 
               values_to = 'gdp_nuts2')
  

pov_risk <- read_excel('persons_at_risk_poverty_social_exclusion.xlsx',
                       sheet = 'Sheet 1', range = 'A8:H46') %>% 
  select(-seq(3,7,2)) %>% 
  rename(geop_entity = TIME) %>% 
  mutate_at(as.character(rev(seq(2016,2019,1))), as.numeric) %>% 
  pivot_longer(cols = `2019`:`2016`, names_to = 'year', 
               values_to = 'poverty_risk') %>% 
  arrange(geop_entity, year)


educ_train <- read_excel('education_training_participation_rate.xlsx', 
                         sheet = 'Sheet 1', range = 'A10:AT48') %>% 
  select(-seq(3,45,2)) %>% 
  rename(geop_entity = TIME) %>% 
  mutate_at(.vars = as.character(seq(2000,2022,1)), as.numeric) %>% 
  pivot_longer(cols = `2000`:`2022`, names_to = 'year', 
               values_to = 'educ&train')


agric_acc <- read_excel('economic_accounts_agriculture_germany.xlsx', 
                        sheet = 'Sheet 1', range = 'A10:U48') %>% 
  select(-c(1, seq(4,20,2))) %>% 
  rename(geop_entity = TIME...2) %>% 
  pivot_longer(cols = `2020`:`2011`, names_to = 'year', 
               values_to = 'agriculture_accounts') %>% 
  arrange(geop_entity, year)
  
  
unemployment <- read_excel('unemployment_rates_germany.xlsx', sheet = 'Sheet 1',
                           range = 'B11:AW49', na = ':') %>% 
  select(-seq(3,47,2)) %>% 
  rename(geop_entity = TIME) %>% 
  pivot_longer(cols = `1999`:`2022`, names_to = 'year', values_to = 'unempl_rate')





