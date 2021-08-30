# Orginal code (by Darren James?) found at: "DataProducts/USDA/Quadrat/Location_conflicts_Adler/Quadrat locations on the Jornada.R"

library(tidyverse)
library(sf)
library(cowplot)

cbPalette <- c("#999999","#D55E00", "#0072B2","#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7")

# Import JER boundary shapefile
JER.border <- read_sf(dsn = "R:/Quadrat/Location_conflicts_Adler", layer = "jer_boundary")

# Import quadrat locations
quad.locations <- read_sf(dsn = "C:/Users/echriste/Desktop/JRN shapefiles", layer = "Study_351_USDA_Permanent_Quadrats")

# get data used in this project
veg_data <- read.csv('data/grass_shrub_timeseries_imputed.csv')
selected_quadrats = unique(veg_data$quadrat)

# which groups will be used in analysis
quad_data = dplyr::filter(quad.locations, NAME %in% selected_quadrats)

# calculate convex hull polygon
hull_poly <- quad_data %>%
  sf::st_combine() %>%
  sf::st_cast('POLYGON') %>%
  sf::st_convex_hull()

sf::st_area(hull_poly)

# Plot JER boundary and quadrat locations
quadmap1 = ggplot() +
  geom_sf(data = hull_poly) +
  geom_sf(data = JER.border) +
  geom_sf(data = quad_data) +
  theme_bw() +
  ggtitle('Permanent Quadrats') +
  theme(axis.text.x=element_text(angle=90))
quadmap1

ggsave(plot=quadmap1, filename='Figures/map_31_quadrats.png', width=4, height=4)

