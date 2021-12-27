#' Look at NPP quadrat data 1989-2016
#' annual summary: https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-jrn.210011003.104 
#' quadrat summary: https://portal.edirepository.org/nis/metadataviewer?packageid=knb-lter-jrn.210011001.1 (1989-2012)
#' *quadrat measurements: https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-jrn.210011002.106 
#' 
#' cover represents % of the quadrat
#' this cover is not analogous to the permanent quad data: cover represents canopy cover of individuals, so adding them together 
#' does not equal % quadrat cover. also included individuals rooted outside that overhang (cover is overhang portion)
#' 
#' quadrats are 1m2
#' there are 49 quadrats per site, arranged in a grid (48 in P-COLL)
#' zones: (vegetation zones) 3 sites per zone
#'   C = creosotebush (CALI, GRAV, SAND)
#'   G = grassland (BASN, IBPE, SUMM)
#'   P = playa (SMAL, TOBO, COLL)
#'   T = tarbush (EAST, TAYL, WEST)
#'   M = mesquite dune (NORT, RABB, WELL)
#' seasons: winter (Feb), spring (May), or fall (Sept-Oct)
#' 
#' I excluded site P-COLL because it contains only 48 quads
#' I include all perennial grasses; this may include genus-only if they were coded as P
#' I do not include Cyperus esculentus
#' There are a lot of NA for cover in 1989. I may exclude this year
#' 
#' EMC 5/18/21
#' last update: 12/20/21

library(dplyr)
library(ggplot2)

npp = read.csv('other data sets/npp/JRN_011002_npp_quadrat_meas.csv')
#npp = read.csv('other data sets/npp/JRN_011001_NPP_quadrat_estimates_SppSiteSeas.csv')


# which species have the most cover
npp_grass = dplyr::filter(npp, form=='GRASS', habit != 'A') %>%
  group_by(USDA_code, Species_binomial) %>%
  summarize(total_cover = sum(cover, na.rm=T))

# restrict to fall (peak perennial biomass)
npp_fall = dplyr::filter(npp, season=='F')

# =========================================================
# data checks

# determine if all sites were sampled in all years, and all quadrats
sites = unique(npp_fall$site)
for (s in sites) {
  sitedat = dplyr::filter(npp_fall, site==s)
  if (length(unique(sitedat$year)) != 30) {
    print(paste0(s, ' sampled in ',length(unique(sitedat$year)),' years'))
  }
  for (yr in unique(sitedat$year)) {
    siteyeardat = dplyr::filter(sitedat, year==yr)
    if (length(unique(siteyeardat$quad)) != 49) {
      print(paste(s, yr, 'not 49 quadrats'))
    }
  }
}

# all quadrats and sites were sampled during fall in all years except COLL


# =================================================

# select data from fall, not site==COLL, only perennial grasses (this will exclude unknown grasses and BOUTE. most BOUTE seem to be annuals)

npp_perenn_grasses = npp_fall %>% 
  dplyr::filter(form=='GRASS', habit=='P', USDA_code != 'CYES', site != 'COLL')

# get species list
species_list = npp_perenn_grasses %>% dplyr::select(USDA_code, Species_binomial, habit, form) %>% unique() %>% arrange(USDA_code)
write.csv(species_list, 'other data sets/npp_quadrats_species_list.csv', row.names=F)

# group cover by site and year
npp_site_year = npp_perenn_grasses %>%
  group_by(year, zone, site) %>%
  summarize(mean_cover = sum(cover, na.rm=T)/49)

ggplot(npp_site_year, aes(x=year, y=mean_cover, color=site)) +
  geom_line()

# get mean of sites by zone
npp_zones = npp_site_year %>%
  ungroup() %>% group_by(year, zone) %>%
  summarize(mean_cover = mean(mean_cover))

ggplot(npp_zones, aes(x=year, y=mean_cover, color=zone)) +
  geom_line()

# get overall mean
npp_mean = npp_site_year %>%
  ungroup() %>% group_by(year) %>%
  summarize(mean_cover = mean(mean_cover))

ggplot(npp_mean, aes(x=year, y=mean_cover)) +
  geom_point() +
  geom_line() +
  xlab('') +
  ylab('Mean % cover') +
  ggtitle('Mean % cover of perennial grasses on NPP quadrats') +
  theme_bw()
