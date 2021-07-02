#' Look at NPP quadrat data 1989-2016
#' EMC 5/18/21

library(dplyr)
library(ggplot2)

npp = read.csv('data/JRN_011002_npp_quadrat_meas.csv')

# this data is not analogous to the permanent quad data: cover represents canopy cover of individuals, so adding them together 
# does not equal % quadrat cover. also included individuals rooted outside that overhang (cover is overhang portion)

# unique identifiers: year, season, site, quad
# zone = habitat type (Creosote, Grassland, Playa, Tarbush, Mesquite, Dune)
# site = 15 named sites
# quad = number 1-49; there are 49 quads in a grid per site, except site P-COLL which has 48

# which species have the most cover
npp_grass = dplyr::filter(npp, form=='GRASS', habit != 'A') %>%
  group_by(spp, USDA_code, Species_binomial) %>%
  summarize(total_cover = sum(cover, na.rm=T))
