# calculate median grass instead of mean (some extreme high values may skew data)
# EMC 8/5/21

veg = read.csv('data/grass_shrub_timeseries_imputed.csv')

median_grass = veg %>%
  group_by(project_year) %>%
  summarize(median_grass = median(grass_cover),
            median_shrub = median(shrub_cover),
            median_bare = median(bareground))

medians_tall = tidyr::pivot_longer(median_grass, cols=c('median_grass','median_shrub','median_bare'), names_to='covertype', values_to='cover')

ggplot(dplyr::filter(medians_tall, covertype != 'median_bare'), aes(x=project_year, y=cover, color=covertype)) +
  geom_point() +
  geom_line()

write.csv(median_grass, 'data/grass_shrub_median_yearly.csv', row.names=F)
