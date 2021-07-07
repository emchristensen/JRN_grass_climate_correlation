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