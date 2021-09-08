#' look at smoothed grass timeseries
#' EMC 9/8/21

library(dplyr)
library(ggplot2)

source('data_functions.R')

# jornada quadrats
grass = read.csv('data/grass_shrub_median_yearly.csv') %>% rename(grass = median_grass)
grass_hist = dplyr::filter(grass, project_year<1995)

grass_smooth = moving_avg_5y(grass_hist$grass, grass_hist$project_year) %>%
  mutate(index_smooth_norm = normalize(index_smooth))

ggplot(grass_smooth, aes(x=year, y=index_smooth)) +
  geom_point()

# cdrrc
cdrrc = read.csv('CDRRC/CDRRC pasture 1 long term forage production.csv', skip=2) %>% 
  dplyr::filter(year<=2018) %>%
  mutate(year=as.numeric(year))
cdrrc_smooth = moving_avg_5y(cdrrc$forage.production..kg.ha.1., cdrrc$year) %>%
  dplyr::filter(!is.na(year)) %>%
  mutate(index_smooth_norm = normalize(index_smooth))

ggplot(cdrrc_smooth, aes(x=year, y=index_smooth_norm)) +
  geom_line() +
  geom_line(data=grass_smooth)


# sev
sev = read.csv('other data sets/SEV_transects_1989_2019.csv')
sev_dw_spring = dplyr::filter(sev, period==1, location=='FP') %>% arrange(year)
sev_smooth = moving_avg_5y(sev_dw_spring$total_grass, sev_dw_spring$year) %>%
  mutate(index_smooth_norm=normalize(index_smooth))

ggplot(cdrrc_smooth, aes(x=year, y=index_smooth_norm)) +
  geom_line() +
  geom_line(data=grass_smooth, color=2) +
  geom_line(data=sev_smooth, color=3)
