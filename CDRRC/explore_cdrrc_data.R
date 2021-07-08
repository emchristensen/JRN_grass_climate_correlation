#' Look at CDRRC data
#' This dataset is forage production (kg/ha), not cover. May include plants other than perennial grasses.
#' EMC 7/7/21

library(dplyr)
library(ggplot2)

dat = read.csv('CDRRC/CDRRC pasture 1 long term forage production.csv', skip=2)
pdo = read.csv('data/PDO_phases_byyear.csv')

dat = merge(dat, pdo, by='year')

forage = ggplot(dat, aes(x=as.numeric(year), y=forage.production..kg.ha.1.)) +
  geom_point(aes(color=pdo_phase)) +
  geom_line() +
  xlab('') 
forage
ggsave('Figures/CDRRC_forage.png', plot=forage, width=5, height=3)

# best forage years were during the 1978-1999 warm PDO phase. recent PDO phase 2015-2018 very low production

# read in quadrat time series
yearlygrass = read.csv('data/grass_shrub_trends_yearly.csv')

# plot quadrat cover vs forage
quadforage = merge(dat, yearlygrass, by.x='year', by.y='project_year')
quadforageplot = ggplot(quadforage, aes(x=forage.production..kg.ha.1., y=mean_grass)) +
  geom_point() +
  geom_text(aes(label=year)) +
  geom_smooth(method='lm', formula=y~x) +
  xlab('CDRRC forage production (kg/ha)') +
  ylab('quadrat grass cover')
quadforageplot
ggsave('Figures/CDRRC_forage_vs_quad_cover.png', plot=quadforageplot, width=5, height=5)


test = lm(quadforage$mean_grass ~ quadforage$forage.production..kg.ha.1.)
summary(test)
# R-squared on the linear correlation is around 0.3