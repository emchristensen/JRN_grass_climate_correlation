#' process raw data files (from data paper in Ecology) and prepare for analyses
#' data decisions:
#'   - combine ARPUL, ARPU9, ARPA9 into ARIST
#'
#' 
#' EMC 9/28/21
#' last run: 2/25/22

library(dplyr)
library(ggplot2)

# read raw files
rawdata = read.csv('data/raw_plant_data/Jornada_quadrat_perennials.csv')
splist = read.csv('data/raw_plant_data/Jornada_quadrat_species_list.csv')
dates = read.csv('data/raw_plant_data/Jornada_quadrat_sampling_dates.csv')

# identify perennial grass species
pg_species = splist$species_code[splist$category %in% c('Cover','') & splist$form=='GRASS']

# list of 31 quadrats with most complete records (not more than 4 years in a row missing)
selected_quads = c('A1','A2','A3','A4','A5','B1','B2','B3','B4','B5',
                   'G1','G2','G3','G4','G5','H1','H2','H3','I1','I2','I3','I4','I5','I6','I7',
                   'J1','J8','J9','J22','K3','N3')

# years for creating imputed timeseries
imputed_years = data.frame(project_year = 1916:1979)

# restrict dates dataframe to quadrats and dates (latest date per year per quadrat) for analysis
dates_for_analysis = dates %>% dplyr::filter(quadrat %in% selected_quads) %>%
  group_by(quadrat, project_year) %>%
  arrange(month, day) %>%
  summarize_all(last)

# merge raw plant data with dates_for_analysis to restrict data
data_for_analysis = merge(dates_for_analysis, rawdata, all.x=T)

# =========================================================================
# species-level imputed timeseries for each quadrat

# combine ARPA9, ARPU9, and ARPUL (very similar species, may have been confused in data collection in early years)
data_for_analysis$species_code[data_for_analysis$species_code %in% c('ARPU9','ARPA9','ARPUL')] <- 'ARIST'

# get total cover by species, quadrat, and year
grass_species_totals = data_for_analysis %>%
  dplyr::filter(species_code %in% pg_species) %>%
  group_by(quadrat, project_year, species_code) %>%
  summarize(total_cover=sum(area))

# loop through each quadrat and species
allquadsallspecies = c()
#quad = 'N3'
for (quad in selected_quads) {
  
  quaddata = dplyr::filter(grass_species_totals, quadrat==quad)
  
  quadratspeciesdata = c()
  #sp = 'ARIST'
  for (sp in unique(grass_species_totals$species_code)) {
    
    quadsp = dplyr::filter(quaddata, species_code==sp)
    
    # merge with dates_for_analysis and fill in zeros
    quadseries = dplyr::filter(dates_for_analysis, quadrat==quad) %>%
      dplyr::select(quadrat, project_year) %>%
      merge(quadsp, all=T) %>%
      dplyr::select(-species_code, -quadrat)
    quadseries$total_cover[is.na(quadseries$total_cover)] <- 0
    
    # merge with imputed_years and impute NAs
    series_imputed = merge(quadseries, imputed_years, all.y=T) %>%
      imputeTS::na_interpolation(option='linear') %>%
      mutate(quadrat=quad, species=sp)
    
    # append to quadrat species data
    quadratspeciesdata = rbind(quadratspeciesdata, series_imputed)
  }
  # append to all quadrats all species data frame
  allquadsallspecies = rbind(allquadsallspecies, quadratspeciesdata)
}

# plot one quadrat's data
ggplot(quadratspeciesdata, aes(x=project_year, y=total_cover, color=species, fill=species)) +
  geom_area()


# get modern values and add to imputed values
modern_grass_sp = expand.grid(quadrat = selected_quads,
                              project_year =c(1995,2001,2006,2011,2016),
                              species_code = unique(allquadsallspecies$species)) %>%
  merge(grass_species_totals, all.x=T) %>%
  dplyr::select(project_year, total_cover, quadrat, species=species_code)
modern_grass_sp$total_cover[is.na(modern_grass_sp$total_cover)] <- 0

all_grass_species = rbind(allquadsallspecies, modern_grass_sp) %>%
  arrange(quadrat, project_year, species)

# save imputed timeseries to file
write.csv(all_grass_species, 'data/grass_species_timeseries_imputed.csv', row.names=F)

# =============================================================
# total grass timeseries

# read in data from above
all_grass_species = read.csv('data/grass_species_timeseries_imputed.csv')

# group by quadrat and year
total_by_quad = all_grass_species %>%
  group_by(quadrat, project_year) %>%
  summarize(grass_cover = sum(total_cover)) %>%
  arrange(quadrat, project_year)

# save to file
write.csv(total_by_quad, 'data/grass_total_timeseries_imputed.csv', row.names=F)


# # ===============================================================
# # mean grass cover over all quadrats by year -- not used
# 
# # read in data from above
# total_by_quad = read.csv('data/grass_total_timeseries_imputed.csv')
# 
# # get mean
# mean_grass = total_by_quad %>%
#   group_by(project_year) %>%
#   summarize(mean_grass = mean(grass_cover))
# 
# ggplot(mean_grass, aes(x=project_year, y=mean_grass)) +
#   geom_point() +
#   geom_line()
# 
# # write to file
# #write.csv(mean_grass, 'data/grass_mean_yearly.csv', row.names=F)

# ===============================================================
# # median grass cover over all quadrats by year
# 
# # read in data from above
# total_by_quad = read.csv('data/grass_total_timeseries_imputed.csv')
# 
# # get median
# median_grass = total_by_quad %>%
#   group_by(project_year) %>%
#   summarize(median_grass = median(grass_cover))
# 
# ggplot(median_grass, aes(x=project_year, y=median_grass)) +
#   geom_point() +
#   geom_line()
# 
# # write to file
# write.csv(median_grass, 'data/grass_median_yearly.csv', row.names=F)
