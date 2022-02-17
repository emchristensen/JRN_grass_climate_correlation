#' plot the four regional locations from the supplement
#' EMC 2/17/22

library(dplyr)
library(usmap)
library(ggplot2)


# points for data locations
locations = data.frame(lon = c(-110.8529, -106.7425, -106.882, -106.8042),
                       lat = c(31.8331, 32.6159, 34.353, 32.5304),
                       name=c('Santa Rita','Jornada','Sevilleta','NMSU Ranch'))

# convert to usmap projection
locations_transformed = usmap_transform(locations)

regional_map = plot_usmap(include = c('AZ','NM'), labels=T) +
  geom_point(data = locations_transformed, aes(x=lon.1, y=lat.1, shape=name, color=name), size=2) +
  labs(color='', shape='') +
  theme(legend.position='right')
regional_map

ggsave('Figures/regional_map.png', plot=regional_map, width=4, height=4)
