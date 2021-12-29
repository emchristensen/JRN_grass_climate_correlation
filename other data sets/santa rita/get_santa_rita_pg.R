#' get perennial grass cover from Santa Rita quadrat data 1917-1932
#' data paper in Ecology https://doi.org/10.1890/11-2200.1 
#' The quadrats I'm using (40) are mostly grazed during the period. Variety of intensities. 5 quads ungrazed
#' I'm not bothering with the points data table because it only contains one relevant record, and the cover for that record is 0

#' EMC 12/22/21

library(dplyr)
library(ggplot2)

# read in raw data
splist = read.csv('other data sets/santa rita/species_list_perennial_grasses.csv')
quadinventory = read.csv('other data sets/santa rita/quad_inventory_mostcompletequads.csv')
polys = read.csv('other data sets/santa rita/allrecords_polygon_features.csv')
unmapped = read.csv('other data sets/santa rita/unmapped_plants.csv')
quadinfo = read.csv('other data sets/santa rita/quad_info.csv')

# list of quadrats that have data between 1917-1932
selectedquads = names(quadinventory)[2:length(names(quadinventory))]
# divide into heavily/moderately grazed or ungrazed/lightly grazed (based on quad_info.csv)
grazed = c('A1P','A2P','B5P','C1P','C3P','C4P','D1P','D2P','D3P','D4P','D5P','E1P','E2P','P3A','P6A','P6B','WD1',
           'WD11','WD13','WD15','WD17','WD19','WD2','WD25','WD5','P2A')
ungrazed = c('A5P','B1P','B2P','C2P','C5P','E3P','E4P','PP1','PP3','PP6','PP7','PP8','WD9','WD3')

# get data on when each quadrat was sampled
dates = tidyr::pivot_longer(quadinventory, cols=all_of(selectedquads), names_to='quad', values_to='sampled') %>%
  mutate(year = year + 1900)
# convert sampled column to 1/0 binary
dates$sampled[!is.na(dates$sampled)] <- 1
dates$sampled[is.na(dates$sampled)] <- 0

# look at info for the selected quads
quadinfo_selected = dplyr::filter(quadinfo, quad.name %in% selectedquads)

# ========================================
# restrict data to perennial grass species and quadrats I've selected

polys_pg = dplyr::filter(polys, Species %in% splist$species, quad %in% selectedquads) %>%
  dplyr::select(quad, year, species=Species, cover=Area) %>%
  mutate(year = year +1900)
unmapped_pg = dplyr::filter(unmapped, species %in% splist$species, quad %in% selectedquads) %>%
  mutate(cover=as.numeric(cover)) %>% dplyr::select(quad, year, species, cover)

# sum cover by year and quadrat
cover_pg = rbind(polys_pg, unmapped_pg) %>%
  group_by(quad, year) %>%
  summarize(total_cover=sum(cover, na.rm=T))

# merge with dates to find any zeros
cover_dates = merge(cover_pg, dates, by=c('year','quad'), all=T)
cover_dates$total_cover[cover_dates$sampled==1 & is.na(cover_dates$total_cover)] <- 0

# impute to fill in missing years
imputed_pg = c()
for (q in unique(cover_dates$quad)) {
  quaddat_imputed = dplyr::filter(cover_dates, quad==q) %>% 
    imputeTS::na_interpolation(option='linear')
  imputed_pg = rbind(imputed_pg, quaddat_imputed)
  ggplot(quaddat_imputed, aes(x=year, y=total_cover, color=quad)) +
    geom_point() +
    geom_line() +
    ggtitle(q) +
    theme_bw()
}

# get average cover per year 1917-1933
sr_cover_mean = imputed_pg %>%
  dplyr::filter(year>1916, year<1933) %>%
  group_by(year) %>%
  summarize(mean_cover = mean(total_cover),
            median_cover = median(total_cover),
            total_cover = sum(total_cover))

ggplot(sr_cover_mean, aes(x=year, y=total_cover)) +
  geom_point() +
  geom_line() +
  theme_bw()

# write average cover per year to csv
write.csv(sr_cover_mean,'other data sets/santa rita/santa_rita_timeseries_imputed.csv', row.names=F)
