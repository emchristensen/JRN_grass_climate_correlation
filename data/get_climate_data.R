#' get raw climate data and create monthly timeseries
#' Takes daily headquarters weather station data, aggregate to monthly and yearly. 
#'  - summer = May-Sept
#'  - winter = Oct-April and belongs to the following year (Oct 1923-Apr 1924 is winter 1924)
# EMC
# last run: 8/13/21

library(dplyr)
library(lubridate)



# =========================================================
# Precipitation

# daily precip (in) downloaded from EDIT data portal and NOAA site
pptdat_jrn = read.csv('data/raw_climate_data/JRN_379001_NOAA_JER_HQ_daily_climate_data.csv', stringsAsFactors = F)
pptdat_noaa = read.csv('data/raw_climate_data/NOAA_Jornada_precip_downloaded20210803.csv')

# deal with dates 
pptdat_jrn$Date = as.Date(pptdat_jrn$date)
pptdat_jrn$year = lubridate::year(pptdat_jrn$Date)
pptdat_jrn$month = lubridate::month(pptdat_jrn$Date)
pptdat_noaa$Date = as.Date(pptdat_noaa$DATE)
pptdat_noaa$year = lubridate::year(pptdat_noaa$Date)
pptdat_noaa$month = lubridate::month(pptdat_noaa$Date)

# convert NOAA data from in to mm
pptdat_noaa$prec_mm = pptdat_noaa$PRCP * 25.4

# in JRN data, if prec_comment is "trace", make ppt zero (instead of NA)
pptdat$prec_mm[pptdat$prec_comment=='trace'] <- 0
# in JRN data, if prec_comment is "totaled next day" make zero (the quantity should be included in the next day value, 
#     and I'm aggregating to monthly anyway)
pptdat$prec_mm[pptdat$prec_comment=='totaled next day'] <- 0
# now all NAs in prec_mm are truly missing data points

# combine JRN and NOAA data
pptdat1 = pptdat_jrn %>%
  dplyr::select(Date, year, month, prec_mm)
pptdat2 = pptdat_noaa %>%
  dplyr::select(Date, year, month, prec_mm) %>%
  dplyr::filter(Date>as.Date('2017-10-06'))
pptdat = rbind(pptdat1, pptdat2)

# get monthly ppt
monthlyppt = pptdat %>%
  group_by(year, month) %>%
  summarize(monthly_ppt_mm= sum(prec_mm, na.rm=T), monthly_ppt_nas = sum(is.na(prec_mm)))

# aggregate by year
yearlyppt = monthlyppt %>%
  group_by(year) %>%
  summarize(yearly_ppt_mm = sum(monthly_ppt_mm),
            days_missing_year = sum(monthly_ppt_nas))

# summer and winter totals
# create water_yr column so winter (Oct-Apr) ppt can be calculated
monthlyppt$water_yr = monthlyppt$year
monthlyppt$water_yr[monthlyppt$month %in% c(10,11,12)] <- monthlyppt$year[monthlyppt$month %in% c(10,11,12)] +1

summerppt = monthlyppt %>%
  dplyr::filter(month %in% c(5,6,7,8,9)) %>%
  group_by(year) %>%
  summarize(summer_ppt_mm = sum(monthly_ppt_mm),
            days_missing_summer = sum(monthly_ppt_nas))

winterppt = monthlyppt %>%
  dplyr::filter(month %in% c(1,2,3,4,10,11,12)) %>%
  group_by(water_yr) %>%
  summarize(winter_ppt_mm = sum(monthly_ppt_mm),
            days_missing_winter=sum(monthly_ppt_nas))

precip = merge(yearlyppt, summerppt, all=T) %>%
  merge(winterppt, by.x='year', by.y='water_yr', all=T)

#write.csv(yearly, 'data/yearly_climate_variables.csv', row.names=F)




# ==================================
# temperature

# read temperature from Berkeley http://berkeleyearth.lbl.gov/stations/27938
temp_be = read.table('data/raw_climate_data/27938-TAVG-Data.txt', sep='', skip=87, 
                  col.names=c('year','month','raw_temperature','data_anomaly','qc_failed','cont_breaks','adj_temp','data_anomaly2',
                              'regional_temperature','expectation_anomaly'))
# read temperature from NOAA station (for 2013-2017)
temp_noaa = read.csv('data/raw_climate_data/NOAA_Jornada_temp.csv', stringsAsFactors = F)

# deal with dates 
temp_noaa$Date = as.Date(temp_noaa$DATE)
temp_noaa$year = lubridate::year(temp_noaa$Date)
temp_noaa$month = lubridate::month(temp_noaa$Date)

# get daily mean temp (from max and min) and convert from F to C
temp_noaa$tmean_f = rowMeans(temp_noaa[,c('TMAX','TMIN')])
temp_noaa$tmean_c = (temp_noaa$tmean_f - 32) * (5/9)

# get monthly mean temp from NOAA data
temp_noaa_monthly = temp_noaa %>%
  group_by(year, month) %>%
  summarize(monthly_tmean_c = mean(tmean_c, na.rm=T),
            monthly_temp_nas = sum(is.na(tmean_c)))
# the most NA days after Oct 2013 (the data I will use) is 9; there is no reason to discard any monthly data due to missingness

# combine Berkeley and NOAA data
temp1 = temp_be %>%
  mutate(date = as.Date(paste(year, month, '15', sep='-'))) %>%
  dplyr::filter(date>as.Date('1914-01-01'), date<as.Date('2013-10-01')) %>%
  dplyr::select(year, month, monthly_tmean_c =adj_temp) 
temp2 = temp_noaa_monthly %>%
  mutate(date = as.Date(paste(year, month, '15', sep='-'))) %>%
  dplyr::filter(date>as.Date('2013-09-15')) %>%
  dplyr::select(year, month, monthly_tmean_c)
temp = rbind(temp1, temp2)

# get yearly mean temp
yearly_temp = temp %>%
  group_by(year) %>%
  summarize(mean_temp = mean(monthly_tmean_c, na.rm=T),
            months_missing_tmean = sum(is.na(monthly_tmean_c)))




# ====================================================
# combine precip and temp into one data file
yearlyclimate = merge(precip, yearly_temp, all=T)

write.csv(yearlyclimate, 'data/climate_variables.csv', row.names=F)

