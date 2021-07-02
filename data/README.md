# JRN_grass_climate_correlation/data

Data files used in analyses of climate/grass cover correlation.

Files:
 * __climate_variables.csv__ yearly ppt, max temp, min temp, vpd, summer ppt, winter ppt. Winter = Oct-April, Summer = May-Sept, water year = May-April. Data from PRISM. File created by JRN_quadrat_timeseries/climate/get_climate_data.R

 * __grass_by_pdo_phase_31quadrats.csv__ mean grass cover and PDO phase (warm/cool) per year for 31 quadrats 1916-2016. Created in pdo_phase_grass_correlation.R

 * __grass_shrub_timeseries_imputed.csv__ imputed timeseries for 31 quadrats 1916-1979 and 1995-2016. Created in JRN_quadrat_timeseries/trends/overall_grass_shrub_trends_interpolate.R

 * __grass_shrub_trends_yearly.csv__ yearly average of grass_shrub_timeseries_imputed.csv. Created by same script. 

 * __JRN_011002_npp_quadrat_meas.csv__ data from the NPP quadrats (different study). Huge data set.