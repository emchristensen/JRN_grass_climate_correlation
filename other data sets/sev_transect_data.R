#' process SEV transect data to grass cover time series
#' Data consists of two 400m transects; Deep Well (DW) and Five Points (FP)
#'    DW was partially burned in a wildfire in August 2009
#'    DW is Blue/black grama mixed, FP is black grama/creosote
#' EMC 8/11/21

library(dplyr)
library(ggplot2)

rawdat = read.csv('other data sets/knb-lter-sev.4.202002/sev004_line_intercept_transects.csv')
splist = read.csv('other data sets/knb-lter-sev.4.202002/SevilletaSpeciesList2019.csv')


# look at codes in data that are not in species list
setdiff(rawdat$species, splist$kartez)

# most of these are non-plants (e.g. ROCK), but some are plants not in the species list
# I made the decision not to use the possible grasses here because they may be annuals
# othergrasses = c('BOUTE','MUHLE','GRASS')

# merge raw data with species list
rawdat_sp = merge(rawdat, splist, by.x='species', by.y='kartez', all.x=T)

# just interested in grasses, perennials, and live plants
rawdat_grass = rawdat_sp %>%
  dplyr::filter(FunctionalGroup %in% c('g',NA), LifeHistory %in% c('p','ap',''), condition==2)


#' try: multiply start/stop by 100 to get integers. each plant spans a sequence of integers. get unique 
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
    for (s in unique(yeardat$period)) {
      perioddat = dplyr::filter(yeardat, period==s)
      
      # vector to hold sequence of locations covered by grass on this transect
      grass_locations = c() 
      for (n in 1:nrow(perioddat)) {
        grassseq = perioddat$startcm[n]:perioddat$stopcm[n]
        grass_locations = c(grass_locations, grassseq)
      }
      # get unique grass_locations, save to data frame
      totalgrass_cm = length(unique(grass_locations))/100
      transects_totalgrass = rbind(transects_totalgrass, data.frame(location=loc, year=y, period=s, total_grass=totalgrass_cm))
    }
  }
}

write.csv(transects_totalgrass,'other data sets/SEV_transects_1989_2019.csv', row.names=F)

# =================================================================================
# plot time series

transects_totalgrass = read.csv('other data sets/SEV_transects_1989_2019.csv')

dw_spring = dplyr::filter(transects_totalgrass, period==1, location=='DW')
ggplot(dw_spring, aes(x=year, y=total_grass)) +
  geom_line() +
  geom_point() +
  theme_bw()
# clear big dip in 2009 for fire

fp_spring = dplyr::filter(transects_totalgrass, period==1, location=='FP')
ggplot(fp_spring, aes(x=year, y=total_grass)) +
  geom_line() +
  geom_point() +
  theme_bw()
# big dip in 2011. Why?