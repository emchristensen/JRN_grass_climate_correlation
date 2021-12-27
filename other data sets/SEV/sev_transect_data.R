#' process SEV transect data to grass cover time series
#' Data consists of two 400m transects; Deep Well (DW) and Five Points (FP)
#'    DW was partially burned in a wildfire in August 2009
#'    DW is Blue/black grama mixed, FP is black grama/creosote
#'    start/stop numbers are in m, measured to nearest cm
#'    sampled in spring (may/june) and fall (sept/oct)
#'    period: 1 = spring, 2 = summer, 3 = fall
#'    condition: 0 = dead, 2 = live, 1 = dead portion of live plant
#'    
#'    
#' EMC 8/11/21

library(dplyr)
library(ggplot2)

rawdat = read.csv('other data sets/SEV/sev004_line_intercept_transects.csv')
splist = read.csv('other data sets/SEV/SevilletaSpeciesList2019.csv')


# look at codes in data that are not in species list
setdiff(rawdat$species, splist$kartez)

# most of these are non-plants (e.g. ROCK), but some are plants not in the species list
# I made the decision not to use the possible grasses here because they may be annuals
# othergrasses = c('BOUTE','MUHLE','GRASS')

# merge raw data with species list
rawdat_sp = merge(rawdat, splist, by.x='species', by.y='kartez', all.x=T)

# just interested in grasses, perennials, and live plants; fall sampling only
rawdat_grass = rawdat_sp %>%
  dplyr::filter(FunctionalGroup %in% c('g',NA), LifeHistory %in% c('p','ap',''), condition==2, period==3)


#' multiply start/stop by 100 to get integers (cm). each plant spans a sequence of integers. get unique 
#' set of integers per transect that are covered by any plant (use unique to get rid of duplicates=overlaps)

rawdat_grass$startcm = rawdat_grass$start*100
rawdat_grass$stopcm = rawdat_grass$stop*100

# unique transect determined by location, year, season

# data frame to hold total grass cover per transect
transects_totalgrass = c()
# loop through each location, year, and season (period)
for (loc in unique(rawdat_grass$location)) {
  locationdat = dplyr::filter(rawdat_grass, location==loc)
  for (y in unique(locationdat$year)) {
    yeardat = dplyr::filter(locationdat, year==y)
    
    # vector to hold sequence of locations covered by grass on this transect
    grass_locations = c() 
    for (n in 1:nrow(yeardat)) {
      grassseq = yeardat$startcm[n]:yeardat$stopcm[n]
      grass_locations = c(grass_locations, grassseq)
    }
    # get unique grass_locations, save to data frame
    totalgrass_m = length(unique(grass_locations))/100
    transects_totalgrass = rbind(transects_totalgrass, data.frame(location=loc, year=y, total_grass=totalgrass_m))
    
  }
}

write.csv(transects_totalgrass,'other data sets/SEV/SEV_transects_fall_1994_2019.csv', row.names=F)

# =================================================================================
# plot time series

transects_totalgrass = read.csv('other data sets/SEV/SEV_transects_fall_1994_2019.csv')

dw = dplyr::filter(transects_totalgrass, location=='DW')
ggplot(dw, aes(x=year, y=total_grass)) +
  geom_line() +
  geom_point() +
  theme_bw()
# clear big dip in 2009 for fire

fp = dplyr::filter(transects_totalgrass, location=='FP')
ggplot(fp, aes(x=year, y=total_grass)) +
  geom_line() +
  geom_point() +
  theme_bw()
# big dip in 2011. Why?