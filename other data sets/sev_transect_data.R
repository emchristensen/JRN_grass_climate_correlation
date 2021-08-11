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

# look at a single transect as a test case
test = rawdat_grass %>%
  dplyr::filter(year==1989, location=='DW',period=='1')

#' try: multiply start/stop by 100 to get integers. each plant spans a sequence of integers. get unique 
#' set of integers per transect that are covered by any plant (use unique to get rid of duplicates=overlaps)
test$startcm = test$start*100
test$stopcm = test$stop*100
grass_cm = c()
for (n in 1:nrow(test)) {
  grassseq = test$startcm[n]:test$stopcm[n]
  grass_cm = c(grass_cm, grassseq)
}
totalgrass_cm = length(unique(grass_cm))/100
