

library(readxl)
library(tidyverse)
library(ggplot2)
library(scales)


# Loading datasets --------------------------------------------------------

setwd('D:/Daniel/msc_applied_ds/t3_dissertation/data/diagnosis/')

gdp <- read_excel('gdp_growth_rate.xlsx', sheet = 'Sheet 1', range = 'A9:Y47',
                  na = ':') %>% 
  select(-c(3,5,7,9,11,13,15,17,19,21,23,25)) %>% 
  rename(country = 'TIME')

waste_gen <- read_excel('waste_generation.xlsx', sheet = 'Sheet 1', 
                      range = 'A11:R48', na = ':') %>% 
  select(!c(seq(3,17,2))) %>% 
  rename(country = 'TIME')

water_abs <- read_excel('freshwater_abstraction.xlsx', sheet = 'Sheet 1', 
                        range = 'A10:T46', na = ':') %>% 
  select(!c(seq(3,19,2))) %>% 
  rename(country = 'TIME')

ghg <- read_excel('net_ghg_emissions_sdg_13.xlsx', sheet = 'Sheet 1', 
                  range = 'A10:BL41', na = ':') %>% 
  select(!c(seq(3,63,2))) %>% 
  rename(country = 'TIME')

nrgy <- read_excel('primary_energy_cons_sdg_07.xlsx', sheet = 'Sheet 1',
                   range = 'A8:AR45', na = ':') %>% 
  select(!c(seq(3,43,2))) %>% 
  rename(country = 'TIME')

wstwater <- read_excel('wastewater_discharge.xlsx', sheet = 'Sheet 1', 
                       range = 'A9:T43', na = ':') %>% 
  select(!c(seq(3,19,2))) %>% 
  rename(country = 'TIME')

years <- rev(names(wstwater)[2:11])

wstwater <- wstwater[,c('country', years)]



# Lengthening dataframes --------------------------------------------------

gdp <- gdp %>% 
  pivot_longer(cols = `2011`:`2022`, names_to = 'year', 
               values_to = 'gdp_growth_rate')

waste_gen <- waste_gen %>% 
  pivot_longer(cols = `2004`:`2020`, names_to = 'year', 
               values_to = 'tonnes_waste')

water_abs <- water_abs %>% 
  pivot_longer(cols = `2012`:`2021`, names_to = 'year',
               values_to = 'million_m3_abs')

ghg <- ghg %>% 
  pivot_longer(cols = `1990`:`2021`, names_to = 'year', 
               values_to = 'ghg_emissions_rate')

nrgy <- nrgy %>% 
  pivot_longer(cols = `2000`:`2021`, names_to = 'year', 
               values_to = 'million_ton_oil_eq')

wstwater <- wstwater %>% 
  pivot_longer(cols = `2012`:`2021`, names_to = 'year', 
               values_to = 'million_m3_disch')



# Filtering countries of interest -----------------------------------------

countries <- c('Germany','United Kingdom','France','Italy','Spain','Switzerland',
               'Bulgaria','Romania','Greece','Hungary','Serbia')

cat2 <- c('Bulgaria','Romania','Greece','Hungary','Serbia')


gdp <- gdp %>% 
  group_by(country, year) %>% 
  filter(country %in% countries) %>% 
  mutate(category = if_else(country %in% cat2, 2, 1),
         category = as.factor(category)) %>% 
  ungroup()


ghg <- ghg %>% 
  group_by(country, year) %>% 
  filter(country %in% countries) %>% 
  mutate(category = if_else(country %in% cat2, 2, 1),
         category = as.factor(category)) %>% 
  ungroup()
  

nrgy <- nrgy %>% 
  group_by(country, year) %>% 
  filter(country %in% countries) %>% 
  mutate(category = if_else(country %in% cat2, 2, 1),
         category = as.factor(category)) %>% 
  ungroup()


waste_gen <- waste_gen %>% 
  group_by(country, year) %>% 
  filter(country %in% countries) %>% 
  mutate(category = if_else(country %in% cat2, 2, 1),
         category = as.factor(category)) %>% 
  ungroup()


water_abs <- water_abs %>% 
  group_by(country, year) %>% 
  filter(country %in% countries) %>% 
  mutate(category = if_else(country %in% cat2, 2, 1),
         category = as.factor(category)) %>% 
  ungroup()


wstwater <- wstwater %>% 
  group_by(country, year) %>% 
  filter(country %in% countries) %>% 
  mutate(category = if_else(country %in% cat2, 2, 1),
         category = as.factor(category)) %>% 
  ungroup()
                                  



# Join de los dataframes --------------------------------------------------


df <- gdp %>% 
  left_join(y = ghg, by = c('country', 'year')) %>% 
  left_join(y = nrgy, by = c('country', 'year')) %>% 
  left_join(y = waste_gen, by = c('country', 'year')) %>% 
  left_join(y = water_abs, by = c('country', 'year')) %>% 
  left_join(y = wstwater, by = c('country', 'year')) %>% 
  relocate(category, .before = 'gdp_growth_rate')


df$year <- as.numeric(df$year)

str(df)

# Filling NA values -------------------------------------------------------


df$tonnes_waste[2:10] <- zoo::na.approx(df$tonnes_waste[2:10])
df$tonnes_waste[14:22] <- zoo::na.approx(df$tonnes_waste[14:22])
df$tonnes_waste[26:34] <- zoo::na.approx(df$tonnes_waste[26:34])
df$tonnes_waste[38:46] <- zoo::na.approx(df$tonnes_waste[38:46])
df$tonnes_waste[50:58] <- zoo::na.approx(df$tonnes_waste[50:58])
df$tonnes_waste[62:70] <- zoo::na.approx(df$tonnes_waste[62:70])
df$tonnes_waste[74:82] <- zoo::na.approx(df$tonnes_waste[74:82])
df$tonnes_waste[86:94] <- zoo::na.approx(df$tonnes_waste[86:94])
df$tonnes_waste[110:116] <- zoo::na.approx(df$tonnes_waste[110:116])
df$tonnes_waste[122:130] <- zoo::na.approx(df$tonnes_waste[122:130])



# Plots -------------------------------------------------------------------


  # Net GHG emissions -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-


ggplot(data = ghg, mapping = aes(x = year, y = ghg_emissions_rate,
                                 colour = category, group = country)) +
  geom_line() +
  geom_text(data = subset(ghg, year == max(year)), aes(label = country), nudge_x = 0.5, nudge_y = 0.1, size = 3, vjust = -0.5) +
  labs(x = "Year", y = "GDP Growth Rate", color = "Category")
         


ggplot(data = ghg, mapping = aes(x = factor(year), y = ghg_emissions_rate, 
                                fill = category)) +
  geom_boxplot(width = 0.7) +
  scale_x_discrete(breaks = seq(1990, 2021, 1)) +
  scale_fill_manual(values = c('1' = alpha('firebrick1', 0.7), 
                               '2' = alpha('darkolivegreen4', 0.8)),
                    labels = c('1' = 'Developed', '2' = 'In transition'))+
  labs(x = '', y = 'Index - 1990 = 100',
       fill = 'Economic status:',
       title = 'Net greenhouse gas emissions',
       subtitle = 'In developed and in transition economy countries of Europe', 
       caption = '') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 19, vjust = 0.4),
        axis.title.y = element_text(size = 22, hjust = 0.7, 
                                    margin = margin(r = 4, l = 3, unit = 'pt')),
        axis.text.y = element_text(size = 20),
        plot.title.position = 'panel',
        plot.subtitle = element_text(size = 11, margin = margin(b = 8)),
        legend.position = 'bottom', legend.margin = margin(t = -8),
        title = element_text(size = 20), legend.text = element_text(size = 18))




  # Primary Energy consumption -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-


ggplot(data = nrgy, mapping = aes(x = factor(year), y = million_ton_oil_eq, 
                                 fill = category)) +
  geom_boxplot(width = 0.7) +
  scale_x_discrete(breaks = seq(1990, 2021, 1)) +
  scale_fill_manual(values = c('1' = alpha('firebrick1', 0.7), 
                               '2' = alpha('darkolivegreen4', 0.8)),
                    labels = c('1' = 'Developed', '2' = 'In transition'))+
  labs(x = '', y = expression('Oil equivalent  [' *x10^6* ' ton]'),
       fill = 'Economic status:',
       title = 'Primary energy consumption',
       subtitle = 'In developed and in transition economy countries of Europe', 
       caption = '') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 19, vjust = 0.4),
        axis.title.y = element_text(size = 22, hjust = 0.7, 
                                    margin = margin(r = 4, l = 3, unit = 'pt')),
        axis.text.y = element_text(size = 20),
        plot.title.position = 'panel',
        plot.subtitle = element_text(size = 11, margin = margin(b = 8)),
        legend.position = 'bottom', legend.margin = margin(t = -8),
        title = element_text(size = 20), legend.text = element_text(size = 18))



  # Freshwater abstraction -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

ggplot(data = water_abs, mapping = aes(x = factor(year), y = million_m3_abs, 
                                fill = category)) +
  geom_boxplot(width = 0.7) +
  scale_x_discrete(breaks = seq(2012, 2022, 1)) +
  scale_y_continuous(labels = scales::label_number(scale = 1/1000))+
  scale_fill_manual(values = c('1' = alpha('firebrick1', 0.7), 
                               '2' = alpha('darkolivegreen4', 0.8)),
                    labels = c('1' = 'Developed', '2' = 'In transition'))+
  labs(x = '', y = expression('Water volume [x  ' * 10^9 * m^3 * ']'),
       fill = 'Economic status',
       title = 'Total waste generation from all NACE activities and households',
       subtitle = 'In developed and in transition economy countries of Europe', 
       caption = 'Incluir info sobre los sectores económicos en el analisis') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 19, vjust = 0.4),
        axis.title.y = element_text(size = 22, hjust = 0.7, 
                                    margin = margin(r = 4, l = 3, unit = 'pt')),
        axis.text.y = element_text(size = 20),
        plot.title.position = 'panel',
        plot.subtitle = element_text(size = 11, margin = margin(b = 8)),
        legend.position = 'bottom', legend.margin = margin(t = -8),
        title = element_text(size = 20), legend.text = element_text(size = 18))




  # Waste generation -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-


ggplot(data = waste_gen, mapping = aes(x = factor(year), y = tonnes_waste, 
                                fill = category)) +
  geom_boxplot(width = 0.7) +
  scale_x_discrete(breaks = seq(2004, 2020, 1)) +
  scale_y_continuous(labels = scales::label_number(scale = 1/1000000)) +
  scale_fill_manual(values = c('1' = alpha('firebrick1', 0.7), 
                               '2' = alpha('darkolivegreen4', 0.8)),
                    labels = c('1' = 'Developed', '2' = 'In transition'))+
  labs(x = '', y = expression('Total waste generation [x  ' * 10^6 * ' t]'),
       fill = 'Economic status',
       title = 'Total waste generation from all NACE activities and households',
       subtitle = 'In developed and in transition economy countries of Europe', caption = 'Hazardous and Non-Hazardous wastes') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 19, vjust = 0.4),
        axis.title.y = element_text(size = 22, hjust = 0.7, 
                                    margin = margin(r = 4, l = 3, unit = 'pt')),
        axis.text.y = element_text(size = 20),
        plot.title.position = 'panel',
        plot.subtitle = element_text(size = 11, margin = margin(b = 8)),
        legend.position = 'bottom', legend.margin = margin(t = -8),
        title = element_text(size = 20), legend.text = element_text(size = 18))




  # Wastewater discharge -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-


ggplot(data = wstwater, mapping = aes(x = factor(year), y = million_m3_disch, 
                                       fill = category)) +
  geom_boxplot(width = 0.7) +
  scale_x_discrete(breaks = seq(2012, 2021, 1)) +
  #scale_y_continuous(labels = scales::label_number(scale = 1/1000000)) +
  scale_fill_manual(values = c('1' = alpha('firebrick1', 0.7), 
                               '2' = alpha('darkolivegreen4', 0.8)),
                    labels = c('1' = 'Developed', '2' = 'In transition'))+
  labs(x = '', y = expression('Total waste generation [x  ' * 10^6 * ' t]'),
       fill = 'Economic status',
       title = 'Total waste generation from all NACE activities and households',
       subtitle = 'In developed and in transition economy countries of Europe', 
       caption = '') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, size = 19, vjust = 0.4),
        axis.title.y = element_text(size = 22, hjust = 0.7, 
                                    margin = margin(r = 4, l = 3, unit = 'pt')),
        axis.text.y = element_text(size = 20),
        plot.title.position = 'panel',
        plot.subtitle = element_text(size = 11, margin = margin(b = 8)),
        legend.position = 'bottom', legend.margin = margin(t = -8),
        title = element_text(size = 20), legend.text = element_text(size = 18))




