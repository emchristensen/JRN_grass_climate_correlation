#' does total perennial grass cover differ by PDO phase?
#' EMC 4/12/21
#' last run: 7/2/21

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

write.csv(quads31, 'data/grass_by_pdo_phase_31quadrats.csv', row.names=F)



# test for differences
# repeated measures anova https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/

# set up data frame
quads_imputed <- veg

# look at summary stats
quads_imputed %>% group_by(project_year) %>% rstatix::get_summary_stats(grass_cover, type='mean_sd')
ggpubr::ggboxplot(quads_imputed, x='project_year', y='grass_cover', add='point')

# test for outliers
outliers <- quads_imputed %>% group_by(project_year) %>% rstatix::identify_outliers(grass_cover)
# there are some extreme outliers. should try analysis with and without these. 

# test for normality
normality <- quads_imputed %>% group_by(project_year) %>% rstatix::shapiro_test(grass_cover)
# many of the time points are not normally distributed
ggpubr::ggqqplot(quads_imputed, 'grass_cover', facet.by='project_year')
# most look ok in the qqplot

