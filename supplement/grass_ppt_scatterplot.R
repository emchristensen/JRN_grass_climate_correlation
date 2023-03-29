library(ggplot2)

grass = read.csv('data/grass_median_yearly.csv')

climate = read.csv('data/climate_variables.csv')

both = merge(grass, climate, by.x = 'project_year', by.y='water_yr')

grassppt = ggplot(both, aes(x=yearly_ppt_mm, y = median_grass)) +
  geom_point() +
  geom_text(aes(label = project_year)) +
  geom_vline(xintercept = median(both$yearly_ppt_mm)) +
  #geom_hline(yintercept = median(both$median_grass)) +
  xlab('yearly ppt (mm)') +
  ylab('grass cover') +
  theme_bw()

grassppt
