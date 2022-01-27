#' read in raw climate data and create monthly and yearly time series
#' yearly = October-September
#' Takes daily headquarters weather station data, aggregate to monthly and yearly. 
#'  - summer = May-Sept
#'  - winter = Oct-April and belongs to the following year (Oct 1923-Apr 1924 is winter 1924)
#' Precip 1914-2017 from EDI Portal; 2017-2020 from NOAA
#' Temp 1914-2012 from Berkeley Earth project; 2012-2020 from NOAA
#' Gaps in monthly data filled by PRISM
#' 
#' Weather station at JRN began in June 1914, so data 1900-1914 filled by PRISM
#' 
# EMC
# last run: 1/27/22

library(dplyr)
library(lubridate)
library(SPEI)

# read in PDSI data
pdsi = read.csv('data/raw_climate_data/PDSI_DonaAna_1895_2021.csv')

# read in PDO index
pdo = read.csv('data/raw_climate_data/PDO_long_1900_2020.csv')

# read in Nino 3.4 index
nino34 = read.csv('data/raw_climate_data/Nino34_long_1870_2020.csv')

# read in PRISM data
prism = read.csv('data/raw_climate_data/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_stable_4km_189501_202001_Headquarters.csv',
                 skip=10) %>%
  mutate(year=as.numeric(substr(Date, 1,4)),
         month=as.numeric(substr(Date,6,7))) %>%
  dplyr::select(year, month, ppt_prism=ppt..mm., tmean_prism=tmean..degrees.C.)

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
pptdat_jrn$prec_mm[pptdat_jrn$prec_comment=='trace'] <- 0
# in JRN data, if prec_comment is "totaled next day" make zero (the quantity should be included in the next day value, 
#     and I'm aggregating to monthly anyway)
pptdat_jrn$prec_mm[pptdat_jrn$prec_comment=='totaled next day'] <- 0
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


# ==================================
# temperature

# read temperature from Berkeley http://berkeleyearth.lbl.gov/stations/27938
temp_be = read.table('data/raw_climate_data/27938-TAVG-Data.txt', sep='', skip=87, 
                  col.names=c('year','month','raw_temperature','data_anomaly','qc_failed','cont_breaks','adj_temp','data_anomaly2',
                              'regional_temperature','expectation_anomaly'))
# read temperature from NOAA station (for 2013-2017)
temp_noaa = read.csv('data/raw_climate_data/NOAA_Jornada_temp.csv', stringsAsFactors = F) %>%
  mutate(TMIN_C = (TMIN-32) * (5/9),
         TMAX_C = (TMAX-32) * (5/9))

# deal with dates 
temp_noaa$Date = as.Date(temp_noaa$DATE)
temp_noaa$year = lubridate::year(temp_noaa$Date)
temp_noaa$month = lubridate::month(temp_noaa$Date)

# get daily mean temp (from max and min)
temp_noaa$tmean_C = rowMeans(temp_noaa[,c('TMAX_C','TMIN_C')])

# get monthly mean temp from NOAA data
temp_noaa_monthly = temp_noaa %>%
  group_by(year, month) %>%
  summarize(monthly_tmean_c = mean(tmean_C, na.rm=T),
            monthly_temp_nas = sum(is.na(tmean_C)))
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


# ====================================================
# put together final data files

# combine monthly files and in-fill with prism where NA
monthlyclimate = merge(monthlyppt, temp, all=T) %>%
  merge(prism, all=T) %>%
  mutate(ppt_mm = monthly_ppt_mm,
         tmean_c = monthly_tmean_c)

# if whole month of ppt missing, make NA and fill with PRISM
monthlyclimate$ppt_mm[monthlyclimate$monthly_ppt_nas>=28] <- NA
monthlyclimate$ppt_mm[is.na(monthlyclimate$ppt_mm)] <- monthlyclimate$ppt_prism[is.na(monthlyclimate$ppt_mm)]
# fill missing temp with PRISM
monthlyclimate$tmean_c[is.na(monthlyclimate$tmean_c)] <- monthlyclimate$tmean_prism[is.na(monthlyclimate$tmean_c)]

# get 1-month SPEI
monthlyclimate$PET = thornthwaite(monthlyclimate$tmean_c, lat=32.6171)
monthlyclimate$BAL = monthlyclimate$ppt_mm-monthlyclimate$PET
spei1 = spei(monthlyclimate[,'BAL'],1)
monthlyclimate$spei1 = spei1$fitted
# there is a -Inf value in spei1, make NA
monthlyclimate$spei1[is.infinite(monthlyclimate$spei1)] <- NA

# combine with PDSI
monthlyfinal = monthlyclimate %>%
  merge(pdsi) %>%
  merge(pdo) %>%
  merge(nino34) %>%
  dplyr::select(year, month, ppt_mm, tmean_c, spei1, pdsi, nino34_index, pdo)



write.csv(monthlyfinal, 'data/climate_variables_monthly.csv', row.names=F)

# =============================================================================
# get yearly and seasonal summary

# create water_yr column so winter (Oct-Apr) ppt can be calculated
monthlyfinal$water_yr = monthlyfinal$year
monthlyfinal$water_yr[monthlyfinal$month %in% c(10,11,12)] <- monthlyfinal$year[monthlyfinal$month %in% c(10,11,12)] +1

# summer precip
summerppt = monthlyfinal %>%
  dplyr::filter(month %in% c(5,6,7,8,9)) %>%
  group_by(water_yr) %>%
  summarize(summer_ppt_mm = sum(ppt_mm))

# winter precip
winterppt = monthlyfinal %>%
  dplyr::filter(month %in% c(1,2,3,4,10,11,12)) %>%
  group_by(water_yr) %>%
  summarize(winter_ppt_mm = sum(ppt_mm))

# aggregate precip, temp, spei to yearly; merge with summer and winter precip
yearlyclimate = monthlyfinal %>%
  group_by(water_yr) %>%
  summarize(pdo = mean(pdo),
            nino34 = mean(nino34_index),
            pdsi = mean(pdsi),
            spei = mean(spei1, na.rm=T),
            mean_temp=mean(tmean_c),
            yearly_ppt_mm = sum(ppt_mm)) %>%
  merge(summerppt) %>%
  merge(winterppt) %>%
  dplyr::filter(water_yr>=1901)


write.csv(yearlyclimate, 'data/climate_variables.csv', row.names=F)

