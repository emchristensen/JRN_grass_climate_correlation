# JRN_grass_climate_correlation/data

Data files used in analyses of climate/grass cover correlation.

Code:
 * __get_climate_data.R__ creates yearly_climate_variables.csv and climate_variables.csv. Data from Jornada Headquarters station data with gaps filled by downloaded PRISM values. Raw data in raw_climate_data folder

Files:
 * __climate_variables.csv__ yearly ppt, max temp, min temp, vpd, summer ppt, winter ppt. Summer = May-Sept, Winter = Oct-April (e.g. winter 1923 is the sum of precip from Oct 1922 - April 1923), Yearly = Jan-Dec. File created by get_climate_data.R

 * __grass_by_pdo_phase_31quadrats.csv__ mean grass cover and PDO phase (warm/cool) per year for 31 quadrats 1916-2016. Created in pdo_phase_grass_correlation.R

 * __grass_shrub_timeseries_imputed.csv__ imputed timeseries for 31 quadrats 1916-1979 and 1995-2016. Created in JRN_quadrat_timeseries/trends/overall_grass_shrub_trends_interpolate.R

 * __grass_shrub_trends_yearly.csv__ yearly average of grass_shrub_timeseries_imputed.csv. Created by same script. 

 * __JRN_011002_npp_quadrat_meas.csv__ data from the NPP quadrats (different study). Huge data set.

* __JRN_temp_NOAA.csv__ daily min and max temperature, downloaded from NOAA, 1915-2021

* __yearly_climate_variables.csv__ yearly precip, vpd, tmin, tmax