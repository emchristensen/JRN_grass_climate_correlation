#' Evaluate correlation between grass and ENSO, PDO, and local climate PC axes
#' EMC 3/1/23
library(dplyr)
library(ggplot2)

# colors for figures
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# read in data
grass = read.csv('data/grass_median_yearly.csv') %>% dplyr::rename(grass = median_grass)
pptdat = read.csv('data/climate_variables.csv')
pcadata = read.csv('data/climate_variables_pca.csv')

combined_data = grass %>%
  merge(pptdat, by.x='project_year', by.y='water_yr', all=T) %>%
  merge(pcadata, by.x='project_year', by.y='X', all=T) %>%
  dplyr::rename(year=project_year)%>%
  mutate(pdo_mean_10y = c(rep(NA,9),
                          zoo::rollmean(pdo, 10))) %>%
  dplyr::filter(year %in% 1916:1979)

# PDO
pdo = ggplot(combined_data, aes(x=pdo, y=grass)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_bw()+
  theme(legend.position = 'none')

# ENSO
enso = ggplot(combined_data, aes(x=nino34, y=grass)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_bw()+
  theme(legend.position = 'none')

# PC1
pc1 = ggplot(combined_data, aes(x=PC1, y=grass)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_bw()+
  theme(legend.position = 'none')

# PC2
pc2 = ggplot(combined_data, aes(x=PC2, y=grass)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_bw()+
  theme(legend.position = 'none')

# PC3
pc3 = ggplot(combined_data, aes(x=PC3, y=grass)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_bw() +
  theme(legend.position = 'none')

# summer precip
ppts = ggplot(combined_data, aes(x=summer_ppt_mm, y=grass)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_bw() +
  theme(legend.position = 'none')


combinedplot = gridExtra::grid.arrange(pc1, pc2, pc3, enso, pdo, nrow=2)

ggsave(combinedplot, filename='Figures/2023_02/grass_climate_scatterplots.png', width=6, height=4)
