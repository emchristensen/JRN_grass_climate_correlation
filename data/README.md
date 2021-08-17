# JRN_grass_climate_correlation/data

Data files used in analyses of climate/grass cover correlation.

Precipitation data 1914-2017 comes from the Jornada Headquarters data stream (Wooton et al 2020). More recent data (2017-2021) were downloaded from NOAA. The Jornada data and NOAA data are from the same station, but there may be data entry errors in the NOAA data prior to 2007 (Darren Jams, pers. comm.). 

Temperature data 1914-2012 are from the Berkeley Earth project, which performs quality control and corrections. More recent data (2013-2021) were downladed from NOAA. 

PDSI and SPEI values (for Dona Ana county) downloaded from https://wrcc.dri.edu/wwdt/time/

Code:
 * __get_climate_data.R__ creates yearly_climate_variables.csv and climate_variables.csv. Raw data from sources described above, in raw_climate_data folder

Files:
* __raw_climate_data/JRN_379001_NOAA_JER_HQ_daily_climate_data.csv__ raw data file from which precip 2014-2017 is taken. Downloaded from https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-jrn&identifier=210379001

* __raw_climate_data/NOAA_Jornada_precip_downloaded20210809.csv__ raw data file from which precip 2017-2020 is taken. Downloaded from https://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:USC00294426/detail 

* __raw_climate_data/27938-TAVG-Data.txt__ Berkeley Earth data: http://berkeleyearth.lbl.gov/stations/27938

* __raw_climate_data/NOAA_Jornada_temp.csv__ raw data file from which temperature data is taken. From https://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:USC00294426/detail 

* __climate_variables.csv__ yearly ppt, yearly mean temp, summer ppt, winter ppt. Summer = May-Sept, Winter = Oct-April (e.g. winter 1923 is the sum of precip from Oct 1922 - April 1923), Yearly = Jan-Dec. Number of missing data days (precip) or months (temperature) are included. File created by get_climate_data.R

 * __grass_by_pdo_phase_31quadrats.csv__ mean grass cover and PDO phase (warm/cool) per year for 31 quadrats 1916-2016. Created in pdo_phase_grass_correlation.R

 * __grass_shrub_timeseries_imputed.csv__ imputed timeseries for 31 quadrats 1916-1979 and 1995-2016. Created in JRN_quadrat_timeseries/trends/overall_grass_shrub_trends_interpolate.R

 * __grass_shrub_trends_yearly.csv__ yearly average of grass_shrub_timeseries_imputed.csv. Created by same script. 

 * __JRN_011002_npp_quadrat_meas.csv__ data from the NPP quadrats (different study). Huge data set.

* __yearly_climate_variables.csv__ yearly precip, vpd, tmin, tmax



Wooton, E., National Weather Service, D. Thatcher, and J. Anderson. 2020. Daily temperature and precipitation data from a NOAA weather station at Jornada Experimental Range headquarters, southern New Mexico USA, 1914 - 2017 ver 80. Environmental Data Initiative. https://doi.org/10.6073/pasta/f09060a9ceb136a1d40b5323aaa0d9a6 (Accessed 2021-08-16).
