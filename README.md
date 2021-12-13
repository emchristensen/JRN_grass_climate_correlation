# JRN_grass_climate_correlation

Code and data for project investigating cycles of high and low grass cover on the Jornada for the past 100 years.

__main model script:__ grass_pdo_model_selection.Rmd. Depends on:

* data/grass_median_yearly.csv
* data/climate_variables.csv
* data/PDO_phases_byyear.csv
* data/ENSO_phases_byyear.csv
* data/smoothed_grass_gam.csv
* data/smoothed_climate_variables.csv

### Folders
__data__ contains raw data files and R scripts for handling data

__Figures__ contains final figures for paper

__other data sets__ exploring other data sets in the arid SW


### Scripts

* __create_quadrat_subset_map.R__ creates map of the quadrats used in this study. Saves to Figures.

* __GAMs_grass_pdo.R__ uses GAM to smooth grass and PDO index timeseries. Saves smoothed timeseries to data folder. Option to save figures to Figures folder. Supports main model script. 

* __pdo_climate_correlations.Rmd__ Document looking at correlations of local weather to PDO phase. Included in supplement of paper. 

* __pdo_phase_grass_correlation.r__ Test for significant difference in grass cover by PDO phase. Saves Figures.

* __pdo_soil_water.R__ tests for differences in soil water content (data from LTER) by PDO phase. No significant differences. Included in supplement of paper. 

* __plot_grass_func_groups_timeseries.R__ creates plot of grass functional groups timeseries. Fig. 2 in paper. 