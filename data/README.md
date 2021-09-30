# JRN_grass_climate_correlation/data

Data files used in analyses of climate/grass cover correlation.

## Grass data
Raw data downloaded from 2021 Ecology data paper (https://doi.org/10.1002/ecy.3530), files in folder raw_plant_data.

__process_raw_plant_data.R__ takes raw data and prepares for analyses. Imputes data between 1916-1979 for 31 quadrats to get yearly values. Creates csv files:

* __grass_species_timeseries_imputed.csv__ Yearly cover by quadrat and species

* __grass_total_timeseries_imputed.csv__ Yearly total grass cover by quadrat

* __grass_median_yearly.csv__ Yearly median total grass cover over 31 quadrats

Other grass data:

 * __JRN_011002_npp_quadrat_meas.csv__ data from the NPP quadrats (different study). Huge data set.

## Climate data
Precipitation data 1914-2017 comes from the Jornada Headquarters data stream (Wooton et al 2020). More recent data (2017-2021) were downloaded from NOAA. The Jornada data and NOAA data are from the same station, but there may be data entry errors in the NOAA data prior to 2007 (Darren Jams, pers. comm.). 

Temperature data 1914-2012 are from the Berkeley Earth project, which performs quality control and corrections. More recent data (2013-2021) were downladed from NOAA. 

Missing values (monthly) filled in with PRISM data. 78 monthly temp values and 5 monthly precip.

PDSI values (for Dona Ana county) downloaded from https://wrcc.dri.edu/wwdt/time/

__get_climate_data.R__ creates climate_variables_monthly.csv and climate_variables.csv. Raw data from sources described above, in raw_climate_data folder


Files:

* __raw_climate_data/27938-TAVG-Data.txt__ Berkeley Earth data: http://berkeleyearth.lbl.gov/stations/27938

* __raw_climate_data/JRN_379001_NOAA_JER_HQ_daily_climate_data.csv__ raw data file from which precip 2014-2017 is taken. Downloaded from https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-jrn&identifier=210379001

* __raw_climate_data/NOAA_Jornada_precip_downloaded20210809.csv__ raw data file from which precip 2017-2020 is taken. Downloaded from https://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:USC00294426/detail 

* __raw_climate_data/NOAA_Jornada_temp.csv__ raw data file from which temperature data is taken. From https://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:USC00294426/detail 

* __PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_stable_4km_189501_202001_Headquarters.csv__ PRISM data

* __climate_variables.csv__ yearly spei, ppt, yearly mean temp, summer ppt, winter ppt. Summer = May-Sept, Winter = Oct-April (e.g. winter 1923 is the sum of precip from Oct 1922 - April 1923), Yearly = Jan-Dec. Number of missing data days (precip) or months (temperature) are included. File created by get_climate_data.R

* __climate_variables_monthly.csv__ monthly ppt, mean temp, spei1 (1 month), spei12 (12-month)





Citations: 
Wooton, E., National Weather Service, D. Thatcher, and J. Anderson. 2020. Daily temperature and precipitation data from a NOAA weather station at Jornada Experimental Range headquarters, southern New Mexico USA, 1914 - 2017 ver 80. Environmental Data Initiative. https://doi.org/10.6073/pasta/f09060a9ceb136a1d40b5323aaa0d9a6 (Accessed 2021-08-16).
