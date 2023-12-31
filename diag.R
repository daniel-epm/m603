

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
             'Montenegro', 'North Macedonia', 'Albania')

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


df1 <- df %>% 
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

df3_pool <- plm::plm(gdp_growth_rate ~ air_emissions + prim_energy_cons + 
                      solid_wastes + freshwater_abs +
                      wastewater_dis,
                    data = df3, model = 'pooling')

sum <- summary(df_pool)
summary(df3_pool)

data.frame(Pr_t = sum$coefficients[,'Pr(>|t|)'])


sum$coefficients

xtable::xtable(sum$coefficients)


  # Fixed Effects model

df_fe <- plm::plm(gdp_growth_rate ~ air_emissions + prim_energy_cons + 
                    solid_wastes + freshwater_abs +
                    wastewater_dis,
                  data = df2, model = 'within')

df3_fe <- plm::plm(gdp_growth_rate ~ air_emissions + prim_energy_cons + 
                    solid_wastes + freshwater_abs +
                    wastewater_dis,
                  data = df3, model = 'within')


summary(df_fe)

sum_fe <- summary(df3_fe)



  # Compare pool vs fixed effects models

plm::pFtest(df_fe, df_pool)

plm::pFtest(df3_fe, df3_pool)


  # Random effects models - different methods
  
    # Wallace-Hussain method
df3_re_walhus <- plm::plm(gdp_growth_rate ~ air_emissions + prim_energy_cons + 
                      solid_wastes + freshwater_abs +
                      wastewater_dis,
                      data = df3, model = 'random', random.method = 'walhus')

sum_re1 <- summary(df3_re_walhus)


    # Amemiya method

df_re_amemiya <- plm::plm(gdp_growth_rate ~ air_emissions + prim_energy_cons + 
                            solid_wastes + freshwater_abs +
                            wastewater_dis,
                          data = df1, model = 'random', random.method = 'amemiya')

df3_re_amemiya <- plm::plm(gdp_growth_rate ~ air_emissions + prim_energy_cons + 
                            solid_wastes + freshwater_abs +
                            wastewater_dis,
                          data = df3, model = 'random', random.method = 'amemiya')

summary(df_re_amemiya)

sum_re2 <- summary(df3_re_amemiya)


    # Nerlove method

df_re_nerlove <- plm::plm(gdp_growth_rate ~ air_emissions + prim_energy_cons + 
                            solid_wastes + freshwater_abs +
                            wastewater_dis,
                          data = df1, model = 'random', random.method = 'nerlove')

df3_re_nerlove <- plm::plm(gdp_growth_rate ~ air_emissions + prim_energy_cons + 
                            solid_wastes + freshwater_abs +
                            wastewater_dis,
                          data = df3, model = 'random', random.method = 'nerlove')

summary(df4_re_nerlove)

sum_re3 <- summary(df3_re_nerlove)


  # Hausman test: checking models' consistency

    # Null hypothesis - Ho: Random effects model is consistent
    # Alternative hypothesis - Ha: Fixed effects model is consistent

plm::phtest(df3_re_walhus, df3_fe)
plm::phtest(df3_re_amemiya, df3_fe)
plm::phtest(df3_re_nerlove, df3_fe)


phtes

      # Opening a dataframe for the p-values of each model

p.values <- data.frame(matrix(nrow = 6))

p.values$fixed_effects <- numeric(6)
  
p.values[2:nrow(p.values), 'fixed_effects'] <- round(sum_fe$coefficients[,4], 4)
p.values[1, 'fixed_effects'] <- NA

p.values$matrix.nrow...6. <- NULL

p.values$rand_effects_walhus <- round(sum_re1$coefficients[,4], 4)

p.values$rand_effects_amemiya <- round(sum_re2$coefficients[,4], 4)

p.values$rand_effects_nerlove <- round(sum_re3$coefficients[,4], 4)

row.names(p.values) <- row.names(sum_re1$coefficients)

xtable::xtable(p.values)



p.values[,c(2:4)]



# Clusterisation model ----------------------------------------------------


  # Further organising the dataframe  -.-.-.-.-.-.-.-

df2 <- df1 %>% 
  group_by(country) %>% 
  filter(!all(is.na(freshwater_abs) | is.na(wastewater_dis)))

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

imp_df1 <- amelia(x = df1, m = 5, ts = 'year', cs = 'country')


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
fviz_dist(dist.obj = df3_dist)

names(df)



  # Cluster dendrogram

df3_clust <- stats::hclust(d = df3_dist, method = "ward.D2")

factoextra::fviz_dend(x = df3_clust)



cluster_table <- table(df4$country, df4$cluster)
cluster_matrix <- as.matrix(cluster_table)
dist_matrix <- dist(cluster_matrix)
hierarchical_clustering <- hclust(dist_matrix)

library(dendextend)

horizontal_dendrogram <- as.dendrogram(hierarchical_clustering)


plot(horizontal_dendrogram, horiz = T, xlab = "", ylab = "")


country_clust <- factoextra::fviz_dend(hierarchical_clustering, k= 5, 
                                       labels_track_height= 70, ylab= "", 
                                       main= "", palette= "Accent", lwd = 1.8, 
                                       cex = 1.5, label_cols= 'black', 
                                       type = 'rectangle', horiz= F, 
                      ggtheme= theme_void()) +
  guides(x = "none")


ggsave(filename = "country_clust2.jpeg", plot = country_clust)



RColorBrewer::display.brewer.all()
x11()
dev.off()

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



df4$cluster <- as.factor(stats::cutree(tree = df3_clust, k = 2))




country <- df2[1:(nrow(df2) - 2),'country']
year <- df2[1:(nrow(df2) - 2),'year']


country <- df3[1:(nrow(df3)),'country']
year <- df3[1:(nrow(df3)),'year']



nrow(df2)
nrow(df3)

df4 <- cbind(df3, country, year)

df4 <- df4 %>% 
  relocate(country, .before = gdp_growth_rate) %>% 
  relocate(year, .after = country)




# Plots -------------------------------------------------------------------


# GDP growth rate -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-


gdp_plot <- ggplot(data = df4, mapping = aes(x = factor(year), y = gdp_growth_rate, 
                                 fill = cluster)) +
  geom_boxplot(width = 0.7) +
  scale_x_discrete(breaks = seq(2012, 2021, 1), labels = NULL) +
  scale_fill_manual(values = c('1' = alpha('darkolivegreen4', 0.7), 
                               '2' = alpha('firebrick1', 0.8)))+
  labs(x = '', y = 'Change on previous period [%]',
       fill = 'Cluster: ') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 20, size = 19, vjust = 0.4),
        axis.title.y = element_text(size = 22, hjust = 0.7, 
                                    margin = margin(r = 4, l = 3, unit = 'pt')),
        axis.text.y = element_text(size = 21),
        legend.position = 'top', 
        legend.background = element_rect(colour = 'black'),
        title = element_text(size = 21), legend.text = element_text(size = 22))


ggsave(filename = "1_gdp.jpg", plot = gdp_plot)




# Air emissions -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-


min(ghg$air_emissions, na.rm = T)

View(ghg[ghg['year'] == '2021', ])

air_emissions_plot <- ggplot(data = df4, 
                             mapping = aes(x = factor(year), y = air_emissions, 
                                fill = cluster)) +
  geom_boxplot(width = 0.7, show.legend = F) +
  scale_x_discrete(breaks = seq(1990, 2021, 1)) +
  scale_y_continuous(limits = c(0, 704000000), 
                     labels = scales::label_number(scale = 1/1000000),
                     breaks = seq(0, 705000000, 100000000)) +
  scale_fill_manual(values = c('1' = alpha('darkolivegreen4', 0.7), 
                               '2' = alpha('firebrick1', 0.8))) +
  labs(x = '', y = expression('Ton  '  * CO[2]),
       fill = 'Cluster:') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 20, size = 19, vjust = 0.4),
        axis.title.y = element_text(size = 22, hjust = 0.7, 
                                    margin = margin(r = 4, l = 3, unit = 'pt')),
        axis.text.y = element_text(size = 21),
        legend.background = element_rect(colour = 'black'),
        title = element_text(size = 21), legend.text = element_text(size = 22))

ggsave(filename = "1_air_emissions.jpg", plot = air_emissions_plot)


  # Primary Energy consumption -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

energ_cons_plot <- ggplot(data = df4, 
                          mapping = aes(x = factor(year), y = prim_energy_cons, 
                                 fill = cluster)) +
  geom_boxplot(width = 0.7) +
  scale_x_discrete(breaks = seq(2012, 2021, 1), labels = NULL) +
  scale_fill_manual(values = c('1' = alpha('darkolivegreen4', 0.7), 
                               '2' = alpha('firebrick1', 0.8)))+
  labs(x = '', y = expression('Oil equivalent  [' *x10^6* ' ton]'),
       fill = 'Cluster: ') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 20, size = 19, vjust = 0.4),
        axis.title.y = element_text(size = 22, hjust = 0.7, 
                                    margin = margin(r = 4, l = 3, unit = 'pt')),
        axis.text.y = element_text(size = 21),
        legend.position = 'top', 
        legend.background = element_rect(colour = 'black'),
        title = element_text(size = 21), legend.text = element_text(size = 22))


ggsave(filename = "1_energy_consumption.jpg", plot = energ_cons_plot)



  # Freshwater abstraction -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

freshwater_abs <- ggplot(data = df4, 
                        mapping = aes(x = factor(year), y = freshwater_abs, 
                                fill = cluster)) +
  geom_boxplot(width = 0.7, show.legend = F) +
  scale_x_discrete(breaks = seq(2012, 2022, 1)) +
  scale_y_continuous(labels = scales::label_number(scale = 1/1000), 
                     breaks = seq(0, 70000, 10000), limits = c(0,65000))+
  scale_fill_manual(values = c('1' = alpha('darkolivegreen4', 0.7), 
                               '2' = alpha('firebrick1', 0.8)))+
  labs(x = '', y = expression('Water volume  [x  ' * 10^9 * m^3 * ']'),
       fill = 'Cluster: ') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 20, size = 19, vjust = 0.4),
        axis.title.y = element_text(size = 22, hjust = 0.7, 
                                    margin = margin(r = 4, l = 3, unit = 'pt')),
        axis.text.y = element_text(size = 21),
        legend.background = element_rect(colour = 'black'),
        title = element_text(size = 21), legend.text = element_text(size = 22))


ggsave(filename = "1_freshwater_abs.jpg", plot = freshwater_abs)


  # Waste generation -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

wastes_plot <- ggplot(data = df4, 
                      mapping = aes(x = factor(year), y = solid_wastes, 
                                fill = cluster)) +
  geom_boxplot(width = 0.7) +
  scale_x_discrete(breaks = seq(2004, 2021, 1), labels = NULL) +
  scale_y_continuous(labels = scales::label_number(scale = 1/1000000),
                     limits = c(0,400000000)) +
  scale_fill_manual(values = c('1' = alpha('darkolivegreen4', 0.7), 
                               '2' = alpha('firebrick1', 0.8)))+
  labs(x = '', y = expression('Total waste generation [x  ' * 10^6 * ' t]'),
       fill = 'Cluster: ') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 20, size = 19, vjust = 0.4),
        axis.title.y = element_text(size = 22, hjust = 0.7, 
                                    margin = margin(r = 4, l = 3, unit = 'pt')),
        axis.text.y = element_text(size = 21),
        legend.position = 'top', 
        legend.background = element_rect(colour = 'black'),
        title = element_text(size = 21), legend.text = element_text(size = 22))


ggsave(filename = "1_wastes_gen.jpg", plot = wastes_plot)


  # Wastewater discharge -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

wastewater_plot <- ggplot(data = df4,
                          mapping = aes(x = factor(year), y = wastewater_dis, 
                                       fill = cluster)) +
  geom_boxplot(width = 0.7, show.legend = F) +
  scale_x_discrete(breaks = seq(2012, 2021, 1)) +
  scale_y_continuous(labels = scales::label_number(scale = 1/1000), 
                     breaks = seq(0, 6000, 1000)) +
  scale_fill_manual(values = c('1' = alpha('darkolivegreen4', 0.7), 
                               '2' = alpha('firebrick1', 0.8)))+
  labs(x = '', y = expression('Wastewater volume  [x  ' * 10^9 * m^3 * ']'),
       fill = 'Cluster: ') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 20, size = 19, vjust = 0.4),
        axis.title.y = element_text(size = 22, hjust = 0.7, 
                                    margin = margin(r = 4, l = 3, unit = 'pt')),
        axis.text.y = element_text(size = 21),
        legend.position = 'bottom', legend.margin = margin(t = -15, b = 10),
        title = element_text(size = 21), legend.text = element_text(size = 22))


ggsave(filename = "1_wastewater.jpg", plot = wastewater_plot)






library(patchwork)


gdp_air <- gdp_plot / air_emissions_plot
ggsave(filename = '1_gdp_air.jpg', plot = gdp_air)

nrg_freshwater <- energ_cons_plot / freshwater_abs
ggsave(filename = '1_nrg_freshw.jpg', plot = nrg_freshwater)

waste_w <- wastes_plot / wastewater_plot
ggsave(filename = '1_waste_w.jpg', plot = waste_w)


dev.off()



df4 <- df4[,1:8]

names(df3)



gdp %>% 
  filter(country %in% c('Poland','Germany','France','Spain','Turkey',
                        'United Kingdom')) %>% 
  ggplot(aes(x = year, y = gdp_growth_rate, colour = country, group= country)) +
  geom_line(lwd = 2)



ghg %>% 
  filter(country %in% c('Poland','Germany','France','Spain','Turkey',
                        'United Kingdom')) %>% 
  ggplot(aes(x = year, y = air_emissions, colour = country, group= country)) +
  geom_line(lwd = 2)



waste_gen %>% 
  filter(country %in% c('Poland','Germany','France','Spain','Turkey',
                        'United Kingdom')) %>% 
  ggplot(aes(x = year, y = tonnes_waste, colour = country, group= country)) +
  geom_line(lwd = 2)



water_abs %>% 
  filter(country %in% c('Poland','Germany','France','Spain','Turkey',
                        'United Kingdom')) %>% 
  ggplot(aes(x = year, y = million_m3_abs, colour = country, group= country)) +
  geom_line(lwd = 2)



wstwater %>% 
  filter(country %in% c('Poland','Germany','France','Spain','Turkey',
                        'United Kingdom')) %>% 
  ggplot(aes(x = year, y = million_m3_disch, colour = country, group= country)) +
  geom_line(lwd = 2)



# Population --------------------------------------------------------------


setwd('D:/Daniel/msc_applied_ds/t3_dissertation/data/diagnosis/')

library(readr)

popul <- read_csv('population.csv', skip = 4)


pop.df <- popul %>% 
  select('Country Name', `2012`:`2021`) %>% 
  rename(country = 'Country Name') %>% 
  filter(country %in% unique(df1$country) | country == 'Turkiye' | 
           country == 'Slovak Republic') %>% 
  pivot_longer(cols = `2012`:`2021`, names_to = 'year', 
               values_to = 'population')


pop.df <- pop.df %>% 
  mutate(cluster = if_else(country %in% cluster.a, 'A', 'B'), 
         year = as.numeric(year))

pop_plot <- pop.df %>% 
  group_by(cluster, year) %>% 
  summarise(tot_pop = sum(population)) %>% 
  ggplot(aes(x = year, y = tot_pop, colour = cluster,
             group = cluster)) +
  geom_line(lwd = 2) +
  scale_x_continuous(breaks = seq(2012,2021,1)) +
  scale_y_continuous(labels = scales::label_number(scale = 1/1000000),
                     breaks = seq(220000000, 320000000, 20000000)) +
  scale_color_manual(values = c("A" = "firebrick1", "B" = "darkolivegreen4")) +
  labs(x = '', y = "Population [x million inhabitants]",
       colour = 'Cluster: ') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 20, size = 19, vjust = 0.4),
        axis.title.y = element_text(size = 22, hjust = 0.7, 
                                    margin = margin(r = 4, l = 3, unit = 'pt')),
        axis.text.y = element_text(size = 21),
        legend.position = 'bottom', legend.margin = margin(t = -15, b = 10),
        title = element_text(size = 21), legend.text = element_text(size = 22)) +
  guides(colour = guide_legend(override.aes = list(size = 8)))
  
colour = guide_legend(override.aes = list(linetype = 1, size = 8))


cluster.a <- c('Germany','France','Spain','Turkey','Poland','United Kingdom')

x11()







