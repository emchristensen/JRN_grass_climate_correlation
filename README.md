# JRN_grass_climate_correlation

Code and data for project investigating cycles of high and low grass cover on the Jornada for the past 100 years.

## Folders
__data__ - Contains raw data files and R scripts for handling data

__Figures__ - Contains final figures for paper

__other data sets__ - Contains data and scripts analyzing data from other locations

__supplement__  - Analyses for supplement of paper

## Workflow
### Prepare data
Run scripts in 'data' folder to prepare quadrat grass data and climate variable data for analyses.
 - __data/process_raw_plant_data.R__
 - __data/get_climate_data.R__
 - __data/determine_enso_categories.R__


### Run GAMs to smooth timeseries
GAMs are used to smooth each of the climate variables. Creates _data/smoothed_climate_variables.csv_.
 - Run __GAMs_smooth.R__


### Main model scripts
The main models correlating grass with climate variables are run in __grass_pdo_model_selection.Rmd__. This script depends on intermediate csv files created in the first two steps. Creates _results_aic_table.csv_. 

The figure of grass functional groups is created by __plot_grass_func_groups_timeseries.R__.

### Additional scripts

* __plot_grass_func_groups_timeseries.R__ creates plot of grass functional groups timeseries. Fig. 2 in paper. 
