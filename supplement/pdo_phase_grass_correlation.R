#' does total perennial grass cover differ by PDO phase?
#' EMC 4/12/21
#' last run: 10/13/21

library(dplyr)
library(ggplot2)

phases = read.csv('data/PDO_phases_byyear.csv')

veg = read.csv('data/grass_total_timeseries_imputed.csv') 
yearlygrass = read.csv('data/grass_median_yearly.csv')

# merge grass data w pdo data
grass_pdo = merge(yearlygrass, phases, by.x='project_year', by.y='year')


# plot timeseries
pdo_grass_plot = ggplot(grass_pdo, aes(x=project_year, y=median_grass)) +
  geom_line() +
  geom_point(aes(color=pdo_phase)) +
  xlab('') +
  ylab('Median grass cover per m^2') +
  ggtitle('Perennial grass cover by PDO phase') +
  theme_bw()
pdo_grass_plot
ggsave('Figures/grass_median_by_pdo_phase_31quads.png', plot=pdo_grass_plot, width=5, height=3)

#write.csv(grass_pdo, 'data/grass_by_pdo_phase_31quadrats.csv', row.names=F)


# =============================================
# test for differences
# repeated measures anova https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/

# set up data frame
# just use data pre-1995; remove the first year of each new phase: 1925, 1947, 1977, 1999, 2015
grass_pdo_training <- grass_pdo %>% dplyr::filter(project_year<1995) %>% dplyr::filter(!project_year %in% c(1925, 1947, 1977, 1999, 2015))

# summary stats
grass_pdo_training %>% group_by(pdo_phase) %>% rstatix::get_summary_stats(median_grass, type='mean_sd')
phase_boxplot = ggplot(grass_pdo_training, aes(x=pdo_phase, y=median_grass)) +
  geom_boxplot() +
  geom_jitter(width=.1, alpha=.4) +
  xlab('') +
  ylab('median yearly grass cover (m^2)') +
  ggtitle('Grass cover by PDO phase') +
  theme_bw()
phase_boxplot
ggsave('Figures/PDO_phase_grass_median_boxplot.png', plot=phase_boxplot, width=4, height=4)


# plot histograms of grass cover during warm or cool phase respectively
warmpdo = dplyr::filter(grass_pdo_training, pdo_phase=='warm')
hist(warmpdo$median_grass)
coolpdo = dplyr::filter(grass_pdo_training, pdo_phase=='cool')
hist(coolpdo$median_grass)

# anova of grass cover by phase
res.aov <- aov(median_grass ~ pdo_phase, data=grass_pdo_training)
summary(res.aov)
# highly significant difference


# =============================================================
# plot raw PDO index vs grass

# get yearly average 
climatevars = read.csv('data/climate_variables.csv') %>%
  mutate(pdo_nolag = pdo)

# shift pdo index to 1 and 2 year lag
climatevars$pdo1lag = c(NA,climatevars$pdo_nolag[1:105])
climatevars$pdo2lag = c(NA,NA,climatevars$pdo_nolag[1:104])

# look at the time series
ggplot(climatevars, aes(x=water_yr, y=pdo_nolag)) +
  geom_point() +
  geom_line()

# merge with grass
pdo_index_grass = merge(climatevars, grass_pdo, by.x='water_yr', by.y='project_year')
# plot pdo index vs grass
ggplot(pdo_index_grass, aes(x=pdo_nolag, y=median_grass)) +
  geom_point()

ccf(pdo_index_grass$pdo_nolag, pdo_index_grass$median_grass)
