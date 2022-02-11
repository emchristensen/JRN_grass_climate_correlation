#' plot shrub encroachment timeseries
#' 
#' EMC 12/14/21

library(dplyr)
library(ggplot2)

# read raw files
rawdata = read.csv('data/raw_plant_data/Jornada_quadrat_perennials.csv')
splist = read.csv('data/raw_plant_data/Jornada_quadrat_species_list.csv')
dates = read.csv('data/raw_plant_data/Jornada_quadrat_sampling_dates.csv')

# identify shrub species
shrubspecies = splist$species_code[splist$form=='SHRUB']

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
# total shrub imputed timeseries for each quadrat

# get total cover by quadrat and year
shrub_totals = data_for_analysis %>%
  dplyr::filter(species_code %in% shrubspecies) %>%
  group_by(quadrat, project_year) %>%
  summarize(total_cover=sum(area))

# loop through each quadrat and complete timeseries
allquads = c()
#quad = 'J8'
for (quad in selected_quads) {
  
  quaddata = dplyr::filter(shrub_totals, quadrat==quad)
  
  # merge with dates_for_analysis and fill in zeros
  quadseries = dplyr::filter(dates_for_analysis, quadrat==quad) %>%
    dplyr::select(quadrat, project_year) %>%
    merge(quaddata, all=T) %>%
    dplyr::select(-quadrat)
  quadseries$total_cover[is.na(quadseries$total_cover)] <- 0
  
  # merge with imputed_years and impute NAs
  series_imputed = merge(quadseries, imputed_years, all.y=T) %>%
    imputeTS::na_interpolation(option='linear') %>%
    mutate(quadrat=quad)
  
  # append to all quadrats data frame
  allquads = rbind(allquads, series_imputed)
}

# plot one quadrat's data
ggplot(series_imputed, aes(x=project_year, y=total_cover)) +
  geom_line()

# get modern values and add to imputed values
modern_shrub = expand.grid(quadrat = selected_quads,
                              project_year =c(1995,2001,2006,2011,2016)) %>%
  merge(shrub_totals, all.x=T) %>%
  dplyr::select(project_year, total_cover, quadrat)
modern_shrub$total_cover[is.na(modern_shrub$total_cover)] <- 0

all_shrub = rbind(allquads, modern_shrub) %>%
  arrange(quadrat, project_year)

# ==========================================
# mean shrub cover over all quadrats all years

shrub_timeseries = all_shrub %>%
  group_by(project_year) %>%
  summarize(avg_cover = mean(total_cover),
            median_cover = median(total_cover),
            sd_cover = sd(total_cover),
            sum_cover = sum(total_cover))

shrub_plot = ggplot(shrub_timeseries, aes(x=project_year, y=avg_cover)) +
  geom_point() +
  geom_line() +
  xlab('') +
  #xlim(c(1915,2020)) +
  ylab('Avg. shrub cover per m^2') +
  theme_bw()
shrub_plot
ggsave('Figures/shrub_cover_timeseries.png', plot=shrub_plot, width=4, height=4)


# ===================================================
# look at shrub correlation to PDO

pdo = read.csv('data/climate_variables.csv')

pdo_shrub = merge(shrub_timeseries, pdo, by.x='project_year', by.y='water_yr') %>%
  dplyr::select(year=project_year, pdo, avg_cover)

shrub_pdo_plot = ggplot(pdo_shrub, aes(x=pdo, y=avg_cover)) +
  geom_point() +
  xlab('PDO index') +
  ylab('Avg. shrub cover per m^2') +
  theme_bw()
shrub_pdo_plot

ggsave('Figures/shrub_pdo_plot.png', plot=shrub_pdo_plot, width=4, height=4)


# lm test
model1 = lm(avg_cover ~ pdo, data=pdo_shrub)
summary(model1)
