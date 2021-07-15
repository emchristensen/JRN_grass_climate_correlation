#' does total perennial grass cover differ by PDO phase?
#' EMC 4/12/21
#' last run: 7/15/21

library(dplyr)
library(ggplot2)

phases = read.csv('data/PDO_phases_byyear.csv')
# these data files were created in JRN_quadrat_timeseries/trends/overall_grass_shrub_trends_interpolate.R
veg = read.csv('data/grass_shrub_timeseries_imputed.csv') 
yearlygrass = read.csv('data/grass_shrub_trends_yearly.csv')

# merge grass data w pdo data
grass_pdo = merge(yearlygrass, phases, by.x='project_year', by.y='year')


# plot timeseries
pdo_grass_plot = ggplot(grass_pdo, aes(x=project_year, y=mean_grass)) +
  geom_line() +
  geom_point(aes(color=pdo_phase)) +
  xlab('') +
  ylab('Mean grass cover per m^2') +
  ggtitle('Perennial grass cover by PDO phase') +
  theme_bw()
pdo_grass_plot
ggsave('Figures/grass_by_pdo_phase_31quads.png', plot=pdo_grass_plot, width=5, height=3)

write.csv(grass_pdo, 'data/grass_by_pdo_phase_31quadrats.csv', row.names=F)


# =============================================
# test for differences
# repeated measures anova https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/

# set up data frame
# just use data pre-1995; remove the first year of each new phase: 1925, 1947, 1977, 1999, 2015
grass_pdo_training <- grass_pdo %>% dplyr::filter(project_year<1995) #%>% dplyr::filter(!project_year %in% c(1925, 1947, 1977, 1999, 2015))

# summary stats
grass_pdo_training %>% group_by(pdo_phase) %>% rstatix::get_summary_stats(mean_grass, type='mean_sd')
phase_boxplot = ggplot(grass_pdo_training, aes(x=pdo_phase, y=mean_grass)) +
  geom_boxplot() +
  geom_jitter(width=.1, alpha=.4) +
  xlab('') +
  ylab('mean yearly grass cover (m^2)') +
  ggtitle('Mean grass cover by PDO phase') +
  theme_bw()
phase_boxplot
ggsave('Figures/PDO_phase_grass_boxplot.png', plot=phase_boxplot, width=4, height=4)


# plot histograms of grass cover during warm or cool phase respectively
warmpdo = dplyr::filter(grass_pdo_training, pdo_phase=='warm')
hist(warmpdo$mean_grass)
coolpdo = dplyr::filter(grass_pdo_training, pdo_phase=='cool')
hist(coolpdo$mean_grass)

# anova of grass cover by phase
res.aov <- aov(mean_grass ~ pdo_phase, data=grass_pdo_training)
summary(res.aov)
# highly significant difference


# =============================================================
# plot raw PDO index vs grass
pdo_long = read.csv('data/PDO_long_1900_2020.csv')
# get yearly average 
pdo_yearly = pdo_long %>% group_by(year) %>% summarize(pdo_nolag = mean(pdo))

# shift pdo index to 1 and 2 year lag
pdo_yearly$pdo1lag = c(NA,pdo_yearly$pdo_nolag[1:120])
pdo_yearly$pdo2lag = c(NA,NA,pdo_yearly$pdo_nolag[1:119])

# look at the time series
ggplot(pdo_yearly, aes(x=year, y=pdo_nolag)) +
  geom_point() +
  geom_line()

# merge with grass
pdo_index_grass = merge(pdo_yearly, grass_pdo, by.x='year', by.y='project_year')
# plot pdo index vs grass
ggplot(pdo_index_grass, aes(x=pdo2lag, y=mean_grass)) +
  geom_point()
