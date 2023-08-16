
library(readxl)
library(tidyverse)
library(ggplot2)


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




# Plots -------------------------------------------------------------------


adm_reg <- read.table(file = './../../scripts/adm_regions.txt', sep = ',', 
                      header = TRUE) %>% 
  rename(geop_entity = Region) %>% 
  janitor::clean_names()


gdp_nuts2 <-  gdp_nuts2 %>% 
  left_join(adm_reg, by = 'geop_entity')

gdp_nuts2 <- gdp_nuts2 %>% 
  mutate(state = factor(state))


range(gdp_nuts2$gdp_nuts2)
mean(gdp_nuts2$gdp_nuts2)
median(gdp_nuts2$gdp_nuts2)
sd(gdp_nuts2$gdp_nuts2)
sqrt(var(gdp_nuts2$gdp_nuts2))


top8 <- c('Oberbayern','Stuttgart','Düsseldorf','Köln','Berlin',
          'Hamburg','Karlsruhe')

gdp_nuts2 <- gdp_nuts2 %>%
  mutate(last_year = year == tail(unique(year), n = 2)[2],
         year = as.numeric(year))


tail(unique(gdp_nuts2$year), n = 2)[2]


RColorBrewer::display.brewer.all()

x11()
dev.off()


  # Plot 1 -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

gdp_nuts2 %>% 
  filter(geop_entity %in% top8) %>% 
  ggplot(aes(x = year, y = gdp_nuts2, colour = state, 
             group = geop_entity)) +
  geom_line(lwd = 1.3) +
  geom_text(data = filter(gdp_nuts2, geop_entity %in% top8 & last_year == TRUE),
                          aes(label = geop_entity),
            position = position_nudge(y = c(-5000,5000)),
            check_overlap = T, vjust = 0.5, hjust = 0.05, size = 6, 
            show.legend = F) +
  coord_cartesian(xlim = c(min(gdp_nuts2$year), max(gdp_nuts2$year) + 0.7))+
  scale_x_continuous(breaks = seq(2000, 2021,1))+
  scale_colour_brewer(palette = 'Set1', direction = 1)+
  scale_y_continuous(breaks = seq(75000,300000,20000), 
                     labels = scales::label_number(scale = 1/1000))+
  labs(x = '', y = expression('Billion  \u20AC'), colour = 'State: ') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 20, size = 19, vjust = 0.4,
                                   margin = margin(b = 15)),
        axis.title.y = element_text(size = 22, hjust = 0.7, 
                                    margin = margin(r = 15, l = 8, unit = 'pt')),
        axis.text.y = element_text(size = 20),
        plot.title.position = 'panel',
        plot.subtitle = element_text(size = 11, margin = margin(b = 8)),
        legend.position = 'bottom', legend.margin = margin(t = -8, b = 12),
        title = element_text(size = 20), legend.text = element_text(size = 18),
        panel.background = element_rect(fill = 'gray80')) +
  guides(colour = guide_legend(override.aes = list(linetype = 1, size = 8)))



  # Plot 2 -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-


gdp_nuts2 <- gdp_nuts2 %>%
  group_by(geop_entity) %>%
  arrange(geop_entity, year) %>%
  mutate(gdp_growth_rate = (gdp_nuts2-lag(gdp_nuts2))/lag(gdp_nuts2) * 100,
         combined_label = paste(geop_entity, " - ", state))


gdp_nuts2 %>% 
  filter(geop_entity %in% top8) %>% 
  ggplot(aes(x = year, y = gdp_growth_rate, 
             group = geop_entity)) +
  geom_line(aes(colour = paste(geop_entity, " - ", state)), lwd = 2) +  # Create combined label
  coord_cartesian(xlim = c(min(gdp_nuts2$year) + 1, max(gdp_nuts2$year) + 0.2)) +
  scale_x_continuous(breaks = seq(2001, 2021,1)) +
  scale_colour_brewer(palette = 'Set1', direction = 1) +
  scale_y_continuous(breaks = seq(-10,10,1)) +
  labs(x = '', y = expression('GDP growth rate [%]'), 
       colour = 'Administrative district and State: ') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, size = 19, vjust = 0.4,
                                   margin = margin(b = 15)),
        axis.title.y = element_text(size = 22, hjust = 0.7, 
                                    margin = margin(r = 15, l = 8, unit = 'pt')),
        axis.text.y = element_text(size = 20),
        plot.title.position = 'panel',
        plot.subtitle = element_text(size = 11, margin = margin(b = 8)),
        legend.position = 'right', legend.margin = margin(t = -8),
        title = element_text(size = 20), legend.text = element_text(size = 18), 
        panel.background = element_rect(fill = 'gray82')) +
  guides(colour = guide_legend(override.aes = list(linetype = 1, size = 8)))



  # Plot 3 -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-


pov_risk <- pov_risk %>% 
  left_join(adm_reg, by = 'geop_entity') %>% 
  arrange(geop_entity, year) %>% 
  mutate(year = as.numeric(year),
         last_year = year == tail(year,1))


range(pov_risk$poverty_risk, na.rm = TRUE)

top8_pov_risk <- pov_risk %>% 
  filter(poverty_risk < quantile(poverty_risk, probs = 0.25, na.rm = TRUE)) %>% 
  select(geop_entity) %>% 
  unique() %>% 
  unlist()

top8_pov_risk <- top8_pov_risk[top8_pov_risk != 'Trier']


pov_risk %>% 
  filter(geop_entity %in% top8_pov_risk) %>% 
  ggplot(mapping = aes(x = year, y = poverty_risk, colour = state,
                       group = geop_entity)) +
  geom_line(aes(label = paste(geop_entity, " - ", state)), lwd = 2) +
  geom_text(data = filter(pov_risk, geop_entity %in% top8_pov_risk & 
                            last_year == TRUE 
                          & !(geop_entity %in% c('Darmstadt', 'Freiburg', 
                                                 'Schwaben'))), 
            aes(label = geop_entity), hjust = -0.09,
            position = position_nudge(y = c(0,0)), size = 7, show.legend = F) +
  geom_text(data = filter(pov_risk, geop_entity %in% top8_pov_risk & 
                            last_year == TRUE & geop_entity %in% c('Darmstadt',
                                                                   'Freiburg',
                                                                   'Schwaben')),
            aes(label = geop_entity), 
            hjust = -0.2, position = position_nudge(y = c(0.19,0,0.12),
                                                    x = c(-0.05,0)), size = 7,
            show.legend = F) +
  coord_cartesian(xlim = c(min(pov_risk$year), max(pov_risk$year) + 0.3)) +
  scale_y_continuous(breaks = seq(12,30, 1), limits = c(12,20)) +
  scale_colour_brewer(palette = 'Set1') +
  labs(x = "", y = "Poverty and social exclusion risk [%]", colour = 'State') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, size = 19, vjust = 0.4,
                                   margin = margin(b = 15)),
        axis.title.y = element_text(size = 22, hjust = 0.7, 
                                    margin = margin(r = 15, l = 8, unit = 'pt')),
        axis.text.y = element_text(size = 20),
        plot.title.position = 'panel',
        plot.subtitle = element_text(size = 11, margin = margin(b = 8)),
        legend.position = 'right', legend.margin = margin(t = -8),
        title = element_text(size = 20), legend.text = element_text(size = 19), 
        panel.background = element_rect(fill = 'gray82')) +
  guides(colour = guide_legend(override.aes = list(linetype = 1, size = 8)))
  
  

  # Plot 4 -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-


unemployment <- unemployment %>% 
  left_join(adm_reg, by = 'geop_entity') %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(geop_entity, year) %>% 
  mutate(last_year = year == tail(.$year, 1),
         u8_2005 = if_else(.$year == 2005 & .$unempl_rate < 8, TRUE, FALSE))


unemployment <- unemployment %>% 
  group_by(geop_entity) %>%
  filter(any(year == 2005 & unempl_rate < 8)) %>%
  ungroup()


unemployment %>% 
  ggplot(mapping = aes(x = year, y = unempl_rate, colour = state, 
                       group = geop_entity)) +
  geom_line(lwd = 1.2) +
  geom_text(data = filter(unemployment, last_year == TRUE &
                            (geop_entity %in% c('Oberbayern', 'Freiburg'))),
            aes(label = geop_entity), hjust = -0.35, size = 7,
            position = position_nudge(y = c(0.075, -0.09)), show.legend = F) +
  geom_text(data = filter(unemployment, last_year == TRUE &
                            (geop_entity %in% c('Schwaben', 'Tübingen'))),
            aes(label = geop_entity), hjust = -0.1, size = 7,
            position = position_nudge(y = c(0.15, -0.15)), show.legend = F) +
  geom_text(data = filter(unemployment, last_year == TRUE &
                            !(geop_entity %in% c('Oberbayern', 'Freiburg',
                                                 'Schwaben', 'Tübingen'))),
            aes(label = geop_entity), hjust = -0.18, size = 7, show.legend = F) +
  coord_cartesian(xlim = c(min(unemployment$year), 
                          max(unemployment$year) + 2.5)) +
  scale_x_continuous(breaks = seq(2000,2022,2)) +
  scale_y_continuous(breaks = seq(2,8,1)) +
  labs(x = "", y = "Unemployment rate [%]", colour = "State") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 20, size = 19, vjust = 0.4,
                                   margin = margin(b = 15)),
        axis.title.y = element_text(size = 22, hjust = 0.7, 
                                    margin = margin(r = 15, l = 8, unit = 'pt')),
        axis.text.y = element_text(size = 20),
        plot.title.position = 'panel',
        plot.subtitle = element_text(size = 11, margin = margin(b = 8)),
        legend.position = 'right', legend.margin = margin(t = -8),
        title = element_text(size = 20), legend.text = element_text(size = 19), 
        panel.background = element_rect(fill = 'gray82')) +
  guides(colour = guide_legend(override.aes = list(lty = 1, size = 9)))
    







