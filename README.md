# JRN_grass_climate_correlation

Code and data for project investigating cycles of high and low grass cover on the Jornada for the past 100 years.

main model script: grass_pdo_model_selection.Rmd

### Folders
__CDRRC__ data and code exploring an additional data set collected on the college ranch

__data__ contains raw data files and R scripts for handling data

__Figures__ contains final figures for paper

__NDVI__ exploring use of NDVI

__other data sets__ exploring other data sets in the arid SW

__RAP_tool__ exploring use of RAP to get estimated historical veg cover

### Scripts

* __create_quadrat_subset_map.R__ creates map of the quadrats used in this study. Saves to Figures.

* __GAMs_grass_pdo.R__ uses GAM to smooth grass and PDO index timeseries. Saves smoothed timeseries to data folder. Option to save figures to Figures folder.

* __jornada_temperature_trends.Rmd__ Document looking at temperature trends from local weather stations

* __pdo_climate_correlations.Rmd__ Document looking at correlations of local weather to PDO phase. 

* __pdo_phase_grass_correlation.r__ Test for significant difference in grass cover by PDO phase. Saves Figures.

* __pdo_soil_water.R__ tests for differences in soil water content (data from LTER) by PDO phase. No significant differences.

* __plot_grass_func_groups_timeseries.R__ creates plot of grass functional groups timeseries