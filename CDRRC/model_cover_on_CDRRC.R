#' Model grass cover based on forage production 1980-1994
#' EMC 7/8/21

library(dplyr)
library(ggplot2)

# read in CDRRC forage data
forage = read.csv('CDRRC/CDRRC pasture 1 long term forage production.csv', skip=2)
# read in quadrat time series
yearlygrass = read.csv('data/grass_shrub_trends_yearly.csv')

# combine data; create linear model based on overlap
foragecover = merge(forage, yearlygrass, by.x='year', by.y='project_year') %>%
  dplyr::select(year, mean_grass, forage=forage.production..kg.ha.1.)

model1 = lm(mean_grass ~ forage, data=foragecover)
summary(model1)

# predict cover values 1980-1994
forage_1980_1994 = dplyr::filter(forage, year %in% 1980:1994) %>%
  dplyr::select(year, forage=forage.production..kg.ha.1.) %>%
  dplyr::mutate(mean_grass = NA)

p = predict(test, forage_1980_1994)
forage_1980_1994$mean_grass = p

# combine into grass timeseries
yearlygrass_gapfilled = yearlygrass %>%
  dplyr::select(year=project_year, mean_grass) %>%
  merge(forage_1980_1994, all=T)

# plot new time series
ggplot(yearlygrass_gapfilled, aes(x=as.numeric(year), y=mean_grass)) +
  geom_point() +
  geom_line()
write.csv(yearlygrass_gapfilled[,c('year','mean_grass')], 'CDRRC/yearly_grass_gapfilled.csv', row.names=F)
