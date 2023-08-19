

library(readxl)
library(tidyverse)
library(zoo)  # function na.approx for handling missing data
library(plm)
library(ggplot2)
library(scales)



# Loading datasets --------------------------------------------------------

setwd('D:/Daniel/msc_applied_ds/t3_dissertation/data/diagnosis/')

gdp <- read_excel('gdp_growth_rate.xlsx', sheet = 'Sheet 1', range = 'A9:Y47',
                  na = ':') %>% 
  select(-c(seq(3,25,2))) %>%
  rename(country = 'TIME')

waste_gen <- read_excel('waste_generation.xlsx', sheet = 'Sheet 1', 
                      range = 'A11:R48', na = ':') %>% 
  select(!c(seq(3,17,2))) %>% 
  rename(country = 'TIME')

water_abs <- read_excel('freshwater_abstraction.xlsx', sheet = 'Sheet 1', 
                        range = 'A10:T46', na = ':') %>% 
  select(!c(seq(3,19,2))) %>% 
  rename(country = 'TIME')

ghg <- read_excel('air_emissions.xlsx', sheet = 'Sheet 1', 
                  range = 'A10:T43', na = ':') %>% 
  select(!c(seq(3,19,2))) %>% 
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
  pivot_longer(cols = `2012`:`2021`, names_to = 'year', 
               values_to = 'air_emissions')

nrgy <- nrgy %>% 
  pivot_longer(cols = `2000`:`2021`, names_to = 'year', 
               values_to = 'million_ton_oil_eq')

wstwater <- wstwater %>% 
  pivot_longer(cols = `2012`:`2021`, names_to = 'year', 
               values_to = 'million_m3_disch')




# Full join the datasets --------------------------------------------------


df <- gdp %>%
  full_join(y = ghg, by = c('country', 'year')) %>% 
  full_join(y = nrgy, by = c('country', 'year')) %>% 
  full_join(y = waste_gen, by = c('country', 'year')) %>% 
  full_join(y = water_abs, by = c('country', 'year')) %>% 
  full_join(y = wstwater, by = c('country', 'year'))

df <- df %>% 
  filter(as.numeric(df$year >= 2012) & as.numeric(df$year <= 2022))




# Handling missing values -------------------------------------------------


  # Missing values in waste_gen variable

for (i in 2:(nrow(df) - 1)) {
  if (is.na(df$tonnes_waste[i])) {
    if (!is.na(df$tonnes_waste[i - 1]) && !is.na(df$tonnes_waste[i + 1])) {
      df$tonnes_waste[i] <- (df$tonnes_waste[i - 1] + df$tonnes_waste[i + 1])/2
    }
  }
}


  # Missing values in ghg variable (air_emissions)

for (i in 1:nrow(df)) {
  if (df$year[i] == '2022') {
    df <- df[-i, ]
  }
}




# Filtering countries with not enough data --------------------------------

plyr::count(df$country)

no_data <- c('Ireland','Italy','Liechtenstein','Switzerland','Iceland',
             'Portugal','Norway','Finland', 'Bosnia and Herzegovina', 
             'Montenegro', 'North Macedonia', 'Albania', 'United Kigdom')

df <- df %>% 
  filter(!(country %in% no_data))


  # Getting a dataframe with every row with na values
rows_with_na <- df[apply(is.na(df), 1, any), ]



# Removing missing values again -------------------------------------------

df <- df %>% arrange(country, year)


for (i in 2:(nrow(df) - 1)) {
  if (is.na(df$million_m3_abs[i]) &&
      df$country[i] == df$country[i - 1] &&
      df$country[i] == df$country[i + 1]) {
    
    # Perform linear interpolation
    df$million_m3_abs[i] <- (df$million_m3_abs[i - 1] + df$million_m3_abs[i + 1]) / 2
  }
}


for (i in 2:(nrow(df) - 1)) {
  if (is.na(df$million_m3_disch[i]) &&
      df$country[i] == df$country[i - 1] &&
      df$country[i] == df$country[i + 1]) {
    
    # Perform linear interpolation
    df$million_m3_disch[i] <- (df$million_m3_disch[i - 1] + df$million_m3_disch[i + 1]) / 2
  }
}



# Panel data structures ---------------------------------------------------


df1 <- plm::pdata.frame(x = df, index = c('country', 'year'), drop.index = TRUE)

head(df1)
str(df1)

head(df1)


df1 <- df1 %>% 
  rename(
    prim_energy_cons = million_ton_oil_eq,
    solid_wastes = tonnes_waste,
    freshwater_abs = million_m3_abs,
    wastewater_dis = million_m3_disch
  )


  # Pooled data

df_pool <- plm::plm(gdp_growth_rate ~ air_emissions + prim_energy_cons + 
                                      solid_wastes + freshwater_abs +
                                      wastewater_dis,
                    data = df1, model = 'pooling')

summary(df_pool)



  # Fixed Effects model

df_fe <- plm::plm(gdp_growth_rate ~ air_emissions + prim_energy_cons + 
                    solid_wastes + freshwater_abs +
                    wastewater_dis,
                  data = df1, model = 'within')

summary(df_fe)



  # Compare pool vs fixed effects models

plm::pFtest(df_fe, df_pool)



  # Random effects models - different methods
  
    # Wallace-Hussain method
df_re_walhus <- plm::plm(gdp_growth_rate ~ air_emissions + prim_energy_cons + 
                      solid_wastes + freshwater_abs +
                      wastewater_dis,
                      data = df1, model = 'random', random.method = 'walhus')

summary(df_re_walhus)


    # Amemiya method

df_re_amemiya <- plm::plm(gdp_growth_rate ~ air_emissions + prim_energy_cons + 
                            solid_wastes + freshwater_abs +
                            wastewater_dis,
                          data = df1, model = 'random', random.method = 'amemiya')

summary(df_re_amemiya)


    # Nerlove method

df_re_nerlove <- plm::plm(gdp_growth_rate ~ air_emissions + prim_energy_cons + 
                            solid_wastes + freshwater_abs +
                            wastewater_dis,
                          data = df1, model = 'random', random.method = 'nerlove')

summary(df_re_nerlove)


  # Hausman test: checking models' consistency

    # Null hypothesis - Ho: Random effects model is consistent
    # Alternative hypothesis - Ha: Fixed effects model is consistent

plm::phtest(df_re_walhus, df_fe)
plm::phtest(df_re_amemiya, df_fe)
plm::phtest(df_re_nerlove, df_fe)
  

plm(model)




# Clusterisation model ----------------------------------------------------


  # Further organising the dataframe  -.-.-.-.-.-.-.-

df2 <- df %>% 
  group_by(country) %>% 
  filter(!all(is.na(million_m3_abs) | is.na(million_m3_disch)))

df2 <- df2 %>% 
  mutate(year = as.numeric(year))

df2 <- as.data.frame(df2)

df2 <- df2 %>% 
  rename(
    prim_energy_cons = million_ton_oil_eq,
    solid_wastes = tonnes_waste,
    freshwater_abs = million_m3_abs,
    wastewater_dis = million_m3_disch
  )


  # Multiple Imputation method  -.-.-.-.-.-.-.-

library(Amelia)

    # Multiple imputation to the df2 dataframe
imp_df2 <- amelia(x = df2, m = 5, ts = 'year', cs = 'country')


missmap(imp_df2)
View(imp_df2$imputations$imp2)

    # Saving the first imputed dataframe as a new dataframe
df3 <- imp_df2$imputations$imp1

    # Building the panel data
df3 <- plm::pdata.frame(x = df3, index = c('country','year'), drop.index = T)



  # Clusterisation model  -.-.-.-.-.-.-.-

library(factoextra)

df_scaled <- scale(df3)   # Normalising data

df3_dist <- get_dist(x = df_scaled, method = 'euclidean')

# Distances plot
fviz_dist(dist.obj = df3_dist, gro)

names(df)



  # Cluster dendrogram

df3_clust <- stats::hclust(d = df3_dist, method = "ward.D2")

factoextra::fviz_dend(x = df3_clust)


  # Optimal number of clusters

library(NbClust)


n_clust <- NbClust(data = df_scaled, distance = 'euclidean', min.nc = 2, max.nc = 4, 
        method = 'kmeans', index = 'alllong')



library(ggplot2)
library(glue)


optimal_clusters <- data.frame(num = n_clust$Best.nc['Number_clusters',])
freqs.df <- plyr::count(optimal_clusters)
nk = freqs.df[freqs.df$freq %in% max(freqs.df),1]


ggplot(data = plyr::count(optimal_clusters), mapping = aes(x = num, y = freq))+
  geom_col(fill= 'dodgerblue4', col= 'dodgerblue', lwd= 1)+
  labs(x= 'Number of clusters k', y= 'Frequency among all indices', 
       title= glue("Optimal number of clusters - k = {nk}"))+
  scale_x_continuous(breaks = seq(0,20,1))+
  scale_y_continuous(breaks = seq(0,20,1))+
  theme_classic()+
  theme(panel.grid.major = element_line(colour = 'gray'), 
        plot.title = element_text(margin = margin(b= 12, t= 5), size = 15, 
                                  face = 'bold'), plot.title.position = 'plot')



df3$cluster <- as.factor(stats::cutree(tree = df3_clust, k = 2))



country <- df2$country
year <- df2$year


df3 <- cbind(df3, country, year)

df3 <- df3 %>% 
  relocate(country, .before = gdp_growth_rate) %>% 
  relocate(year, .after = country)



# Plots -------------------------------------------------------------------


# GDP growth rate -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-


ggplot(data = df3, mapping = aes(x = factor(year), y = gdp_growth_rate, 
                                 fill = cluster)) +
  geom_boxplot(width = 0.7) +
  scale_x_discrete(breaks = seq(1990, 2021, 1)) +
  scale_fill_manual(values = c('1' = alpha('firebrick1', 0.7), 
                               '2' = alpha('darkolivegreen4', 0.8)))+
  labs(x = '', y = 'Index - 1990 = 100',
       fill = 'Cluster:',
       title = 'GDP growth rate',
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






  # Net GHG emissions -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-


ggplot(data = df3, mapping = aes(x = factor(year), y = air_emissions, 
                                fill = cluster)) +
  geom_boxplot(width = 0.7) +
  scale_x_discrete(breaks = seq(1990, 2021, 1)) +
  scale_fill_manual(values = c('1' = alpha('firebrick1', 0.7), 
                               '2' = alpha('darkolivegreen4', 0.8)))+
  labs(x = '', y = 'Index - 1990 = 100',
       fill = 'Cluster:',
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

x11()


  # Primary Energy consumption -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-


ggplot(data = df3, mapping = aes(x = factor(year), y = prim_energy_cons, 
                                 fill = cluster)) +
  geom_boxplot(width = 0.7) +
  scale_x_discrete(breaks = seq(1990, 2021, 1)) +
  scale_fill_manual(values = c('1' = alpha('firebrick1', 0.7), 
                               '2' = alpha('darkolivegreen4', 0.8)))+
  labs(x = '', y = expression('Oil equivalent  [' *x10^6* ' ton]'),
       fill = 'Clusters:',
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

ggplot(data = df3, mapping = aes(x = factor(year), y = freshwater_abs, 
                                fill = cluster)) +
  geom_boxplot(width = 0.7) +
  scale_x_discrete(breaks = seq(2012, 2022, 1)) +
  scale_y_continuous(labels = scales::label_number(scale = 1/1000))+
  scale_fill_manual(values = c('1' = alpha('firebrick1', 0.7), 
                               '2' = alpha('darkolivegreen4', 0.8)))+
  labs(x = '', y = expression('Water volume [x  ' * 10^9 * m^3 * ']'),
       fill = 'Cluster: ',
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


ggplot(data = df3, mapping = aes(x = factor(year), y = solid_wastes, 
                                fill = cluster)) +
  geom_boxplot(width = 0.7) +
  scale_x_discrete(breaks = seq(2004, 2020, 1)) +
  scale_y_continuous(labels = scales::label_number(scale = 1/1000000)) +
  scale_fill_manual(values = c('1' = alpha('firebrick1', 0.7), 
                               '2' = alpha('darkolivegreen4', 0.8)))+
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


ggplot(data = df3, mapping = aes(x = factor(year), y = wastewater_dis, 
                                       fill = cluster)) +
  geom_boxplot(width = 0.7) +
  scale_x_discrete(breaks = seq(2012, 2021, 1)) +
  #scale_y_continuous(labels = scales::label_number(scale = 1/1000000)) +
  scale_fill_manual(values = c('1' = alpha('firebrick1', 0.7), 
                               '2' = alpha('darkolivegreen4', 0.8)))+
  labs(x = '', y = expression('Total waste generation [x  ' * 10^6 * ' t]'),
       fill = 'Cluster: ',
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




