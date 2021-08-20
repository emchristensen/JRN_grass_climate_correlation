#' Create plot of median grass and shrub cover over time
#' EMC 8/20/21

library(dplyr)
library(ggplot2)

# colors for figures
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

veg = read.csv('data/grass_shrub_timeseries_imputed.csv')
yearlygrass = read.csv('data/grass_shrub_median_yearly.csv')

grass = dplyr::select(yearlygrass, project_year, cover=median_grass) %>% mutate(veg_type=rep('Grass'))
shrub = dplyr::select(yearlygrass, project_year, cover=median_shrub) %>% mutate(veg_type=rep('Shrub'))
plotdat = rbind(grass, shrub)

trend = ggplot(plotdat, aes(x=project_year, y=cover)) +
  geom_point(data=veg, aes(x=project_year, y=grass_cover, colour='Grass'), alpha=.05) +
  geom_point(data=veg, aes(x=project_year, y=shrub_cover, colour='Shrub'), alpha=.05) +
  geom_line(aes(color=veg_type)) +
  labs(x = '',
       y='Cover per Quadrat (m^2)',
       colour='Vegetation Type',
       title='Quadrat Cover By Vegetation Type') +
  ylim(c(0,.25)) +
  scale_color_manual(values=cbPalette[c(7,6)]) +
  theme_bw()
trend
ggsave('Figures/grass_shrub_trend_yearly_median_31quads.png', plot=trend, width=5, height=3)
