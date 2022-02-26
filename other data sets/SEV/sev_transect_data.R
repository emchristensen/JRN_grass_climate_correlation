#' process SEV transect data to grass cover time series
#' Data consists of two 400m transects; Deep Well (DW) and Five Points (FP)
#'    DW was partially burned in a wildfire in August 2009
#'    DW is Blue/black grama mixed, FP is black grama/creosote
#'    start/stop numbers are in m, measured to nearest cm
#'    sampled in spring (may/june), summer, and fall (sept/oct)
#'    period: 1 = spring, 2 = summer, 3 = fall
#'    condition: 0 = dead, 2 = live, 1 = dead portion of live plant
#'    
#'  Data citation:
#'    Collins, S. 2021. Grassland Vegetation Line-Intercept Transects at the 
#'    Sevilleta National Wildlife Refuge, New Mexico ver 202003. Environmental Data Initiative. 
#'    https://doi.org/10.6073/pasta/4d4b60db003d54fc2a989924d06028a9 (Accessed 2022-01-12).
#'    
#'  The paper Collins et al 2020 used max of spring/fall sampling per year to create timeseries
#'    
#' EMC 8/11/21
#' last run: 2/25/22

library(dplyr)
library(ggplot2)

rawdat = read.csv('other data sets/SEV/sev004_line_intercept_transects.csv')
splist = read.csv('other data sets/SEV/SevilletaSpeciesList2019.csv')


# look at codes in data that are not in species list
setdiff(rawdat$species, splist$kartez)

# most of these are non-plants (e.g. ROCK), but some are plants not in the species list
# I made the decision not to use the possible grasses here because they may be annuals
# othergrasses = c('BOUTE','MUHLE','GRASS')
grasses = dplyr::filter(splist, FunctionalGroup %in% c('g'),
                        LifeHistory %in% c('p','ap',''))

# merge raw data with species list
rawdat_sp = merge(rawdat, splist, by.x='species', by.y='kartez', all.x=T)

# just interested in perennial grasses; live plants (condition=2); spring/fall samplings only (period=1, 3); FP location only
rawdat_grass = rawdat_sp %>%
  dplyr::filter(condition==2, 
                species %in% grasses$kartez,
                period %in% c(1,3), 
                location=='FP')


#' multiply start/stop by 100 to get integers (cm). each plant spans a sequence of integers. get unique 
#' set of integers per transect that are covered by any plant (use unique to not count overlapping plants twice)
rawdat_grass$startcm = rawdat_grass$start*100
rawdat_grass$stopcm = rawdat_grass$stop*100

# unique transect determined by location, year, season

# data frame to hold total grass cover per transect
transects_totalgrass = c()
# loop through each season (period) and year 
for (per in unique(rawdat_grass$period)) {
  perioddat = dplyr::filter(rawdat_grass, period==per)
  for (y in unique(perioddat$year)) {
    yeardat = dplyr::filter(perioddat, year==y)
    
    # vector to hold sequence of locations covered by grass on this transect
    grass_locations = c() 
    for (n in 1:nrow(yeardat)) {
      grassseq = yeardat$startcm[n]:yeardat$stopcm[n]
      grass_locations = c(grass_locations, grassseq)
    }
    # get unique grass_locations, save to data frame
    totalgrass_m = length(unique(grass_locations))/100
    transects_totalgrass = rbind(transects_totalgrass, data.frame(period=per, year=y, total_grass=totalgrass_m))
    
  }
}

# get maximum grass cover per year (following COllins et al 2020)
transect_ts = transects_totalgrass %>%
  group_by(year) %>%
  summarize(total_grass = max(total_grass))


write.csv(transect_ts,'other data sets/SEV/SEV_transects_1989_2019.csv', row.names=F)

# =================================================================================
# plot time series

transect_ts = read.csv('other data sets/SEV/SEV_transects_1989_2019.csv')


ggplot(transect_ts, aes(x=year, y=total_grass)) +
  geom_line() +
  geom_point() +
  theme_bw()

