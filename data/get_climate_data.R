#' read in raw climate data and create monthly and yearly time series
#' 
#' yearly = October-September
#' Takes daily headquarters weather station data, aggregate to monthly and yearly. 
#'  - summer = May-Sept
#'  - winter = Oct-April and belongs to the following year (Oct 1923-Apr 1924 is winter 1924)
#' Precip 1914-2017 from EDI Portal; 2017-2020 from NOAA
#' Temp 1914-2017 EDI portal; 2017-2020 from NOAA
#' Gaps in monthly data filled by PRISM
#' 
#' Weather station at JRN began in June 1914, so data 1900-1914 filled by PRISM
#' 
# EMC
# last run: 1/18/23

library(dplyr)
library(lubridate)
library(SPEI)
library(rsoi)

# read in PDSI data
pdsi = read.csv('data/raw_climate_data/PDSI_DonaAna_1895_2021.csv')

# download PDO index using rsoi package
pdo = rsoi::download_pdo() %>% rename(year=Year) %>% mutate(month = month(Date))

# read in Nino 3.4 index
nino34 = read.csv('data/raw_climate_data/Nino34_long_1870_2020.csv')

# read in PRISM data
prism = read.csv('data/raw_climate_data/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_stable_4km_189501_202001_Headquarters.csv',
                 skip=10) %>%
  mutate(year=as.numeric(substr(Date, 1,4)),
         month=as.numeric(substr(Date,6,7))) %>%
  dplyr::select(year, month, ppt_prism=ppt..mm., tmin_prism=tmin..degrees.C., tmax_prism=tmax..degrees.C.)

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

# temperature from weather EDI portal data; aggregate to monthly
temp_jrn = pptdat_jrn %>%
  group_by(year, month) %>%
  summarize(monthly_tmax_c = mean(tmax_c, na.rm=T),
            monthly_tmin_c = mean(tmin_c, na.rm=T),
            monthly_temp_nas = sum(is.na(tmin_c)))


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
            monthly_tmax_c = mean(TMAX_C, na.rm=T),
            monthly_tmin_c = mean(TMIN_C, na.rm = T),
            monthly_temp_nas = sum(is.na(tmean_C)))


# combine JRN and NOAA data
temp1 = temp_jrn %>%
  mutate(date = as.Date(paste(year, month, '15', sep='-'))) %>%
  dplyr::select(year, month, monthly_tmax_c, monthly_tmin_c, monthly_temp_nas) 
temp2 = temp_noaa_monthly %>%
  mutate(date = as.Date(paste(year, month, '15', sep='-'))) %>%
  dplyr::filter(date>as.Date('2017-10-15')) %>%
  dplyr::select(year, month, monthly_tmax_c, monthly_tmin_c, monthly_temp_nas)
temp = rbind(temp1, temp2)

# remove values for months missing > 20 data days
temp$monthly_tmax_c[temp$monthly_temp_nas>20] <- NA
temp$monthly_tmin_c[temp$monthly_temp_nas>20] <- NA

# =================================
# El Nino index

# get 3-month rolling average of nino 3.4 index (ONI)
nino = nino34 %>% mutate(ONI=rep(NA))
for (n in 2:length(nino$ONI)) {
  nino$ONI[n] <- mean(nino$nino34_index[(n-1):(n+1)])
} 

# categorize as nino/nina/neutral
nino$enso_phase = rep('Neutral')
nino$enso_phase[nino$ONI< -0.5] <- 'LaNina'
nino$enso_phase[nino$ONI> 0.5] <- 'ElNino'

# ====================================================
# put together final data files

# combine monthly files and in-fill with prism where NA
monthlyclimate = merge(monthlyppt, temp, all=T) %>%
  merge(prism, all=T) %>%
  mutate(ppt_mm = monthly_ppt_mm,
         tmin_c = monthly_tmin_c,
         tmax_c = monthly_tmax_c)

# if whole month of ppt missing, make NA and fill with PRISM
monthlyclimate$ppt_mm[monthlyclimate$monthly_ppt_nas>=28] <- NA
monthlyclimate$ppt_mm[is.na(monthlyclimate$ppt_mm)] <- monthlyclimate$ppt_prism[is.na(monthlyclimate$ppt_mm)]
# fill missing temp with PRISM
monthlyclimate$tmin_c[is.na(monthlyclimate$tmin_c)] <- monthlyclimate$tmin_prism[is.na(monthlyclimate$tmin_c)]
monthlyclimate$tmax_c[is.na(monthlyclimate$tmax_c)] <- monthlyclimate$tmax_prism[is.na(monthlyclimate$tmax_c)]

# get 1-month SPEI
monthlyclimate$PET = thornthwaite(rowMeans(monthlyclimate[,c('tmin_c','tmax_c')]), lat=32.6171)
monthlyclimate$BAL = monthlyclimate$ppt_mm-monthlyclimate$PET
spei1 = spei(monthlyclimate[,'BAL'],1)
monthlyclimate$spei1 = spei1$fitted
# there is a -Inf value in spei1, make NA
monthlyclimate$spei1[is.infinite(monthlyclimate$spei1)] <- NA

# combine with PDSI
monthlyfinal = monthlyclimate %>%
  merge(pdsi) %>%
  merge(pdo) %>%
  merge(nino) %>%
  dplyr::select(year, month, ppt_mm, tmin_c, tmax_c, spei1, pdsi, nino34_index, ONI, enso_phase, pdo=PDO) %>%
  mutate(date = as.Date(paste(year, month, '01',sep='-'))) %>%
  arrange(date)



write.csv(monthlyfinal, 'data/climate_variables_monthly.csv', row.names=F)

# =============================================================================
# get yearly and seasonal summary

# read in PDO phases
pdophase = read.csv('data/PDO_phases_byyear.csv')

# determine if a year was nino/nina/neutral
#    el nino = ONI > 0.5 for 5 months Sept-March
#    la nina = ONI < -0.5 for 5 months Sept-March
ensophases = dplyr::select(monthlyfinal, year, month, ONI, enso_phase) %>% 
  dplyr::filter(month %in% c(1,2,3,9,10,11,12)) %>%
  mutate(winteryear = ifelse (month %in% c(9:12), year+1, year)) %>%
  group_by(winteryear) %>%
  summarize(nino=sum(enso_phase=='ElNino'),
            nina=sum(enso_phase=='LaNina')) %>%
  mutate(enso_phase = ifelse(nino>=5, 'ElNino', ifelse(nina>=5, 'LaNina', 'Neutral'))) %>%
  dplyr::select(winteryear, enso_phase)


# create water_yr column so winter (Oct-Apr) ppt can be calculated
monthlyfinal$water_yr = monthlyfinal$year
monthlyfinal$water_yr[monthlyfinal$month %in% c(10,11,12)] <- monthlyfinal$year[monthlyfinal$month %in% c(10,11,12)] +1

# summer precip, spei, pdsi, tmax, tmin
summerppt = monthlyfinal %>%
  dplyr::filter(month %in% c(5,6,7,8,9)) %>%
  group_by(water_yr) %>%
  summarize(summer_ppt_mm = sum(ppt_mm),
            summer_pdsi = mean(pdsi),
            summer_spei = mean(spei1),
            summer_tmax = mean(tmax_c),
            summer_tmin = mean(tmin_c))

# winter precip
winterppt = monthlyfinal %>%
  dplyr::filter(month %in% c(1,2,3,4,10,11,12)) %>%
  group_by(water_yr) %>%
  summarize(winter_ppt_mm = sum(ppt_mm),
            winter_pdsi = mean(pdsi),
            winter_spei = mean(spei1),
            winter_tmax = mean(tmax_c),
            winter_tmin = mean(tmin_c))



# aggregate precip, temp, spei to yearly; merge with summer and winter precip; merge with pdo and enso phases
yearlyclimate = monthlyfinal %>%
  group_by(water_yr) %>%
  summarize(pdo = mean(pdo),
            nino34 = mean(nino34_index),
            #oni = mean(ONI),
            pdsi = mean(pdsi),
            spei = mean(spei1, na.rm=T),
            #mean_temp=mean(tmean_c),
            yearly_ppt_mm = sum(ppt_mm)) %>%
  merge(summerppt) %>%
  merge(winterppt) %>%
  merge(pdophase, by.x='water_yr', by.y='year') %>%
  merge(ensophases, by.x='water_yr', by.y='winteryear') %>%
  dplyr::filter(water_yr>=1901) #%>%
  #merge(demartonne[,c('year','demartonne')], by.x='water_yr',by.y='year')

write.csv(yearlyclimate, 'data/climate_variables.csv', row.names=F)

