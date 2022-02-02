#' Compare temperature at SEV to temperature at JER
#' 
#' SEV StationID 40 goes back the farthest in time (1988)
#' Looking at data, it seems unlikely that temps of 40 or -40 are accurate.
#' Filtered out temps < -20 and >=40
#' 
#' 
#' Sev data citation: Moore, D.I. 2021. Meteorology Data from the Sevilleta National Wildlife Refuge, 
#'                    New Mexico ver 14. Environmental Data Initiative. https://doi.org/10.6073/pasta/1cbc37ae4d40b3844b5e4be9f6f18073 
#'                    (Accessed 2022-01-27).
#' 
#' EMC 1/27/22

library(dplyr)
library(ggplot2)

# read daily temperature from NOAA station
temp_noaa = read.csv('data/raw_climate_data/NOAA_Jornada_temp.csv', stringsAsFactors = F) %>%
  mutate(TMIN_C = (TMIN-32) * (5/9),
         TMAX_C = (TMAX-32) * (5/9))
# deal with dates 
temp_noaa$Date = as.Date(temp_noaa$DATE)
temp_noaa$year = lubridate::year(temp_noaa$Date)

jrn = temp_noaa %>%
  dplyr::select(year, Date, TMIN_C, TMAX_C) %>%
  dplyr::filter(year>1987)
jrn$tmean = rowMeans(jrn[,c('TMIN_C','TMAX_C')], na.rm=T)

# read in sev data
sev1 = read.csv('data/raw_climate_data/knb-lter-sev.1.14/Sevilleta_LTER_Hourly_Meteorological_Data_1988_1994.csv')
sev2 = read.csv('data/raw_climate_data/knb-lter-sev.1.14/Sevilleta_LTER_Hourly_Meteorological_Data_1995_1999.csv')
sev3 = read.csv('data/raw_climate_data/knb-lter-sev.1.14/Sevilleta_LTER_Hourly_Meteorological_Data_2000_2004.csv')
sev4 = read.csv('data/raw_climate_data/knb-lter-sev.1.14/Sevilleta_LTER_Hourly_Meteorological_Data_2005_2009.csv')
sev5 = read.csv('data/raw_climate_data/knb-lter-sev.1.14/Sevilleta_LTER_Hourly_Meteorological_Data_2010_2014.csv')
sev6 = read.csv('data/raw_climate_data/knb-lter-sev.1.14/Sevilleta_LTER_Hourly_Meteorological_Data_2015_2019.csv')
sev7 = read.csv('data/raw_climate_data/knb-lter-sev.1.14/Sevilleta_LTER_Hourly_Meteorological_Data_2020.csv')

# to match jornada data, get daily avg T of SEV
sev = rbind(sev1, sev2, sev3, sev4, sev5, sev6, sev7) 

sev_daily = sev %>%
  dplyr::filter(StationID==40) %>%
  dplyr::group_by(StationID, Date, Year, Month, Day_of_Month) %>%
  summarize(tmin = min(Min_Temp_C, na.rm=T),
            tmax = max(Max_Temp_C, na.rm=T),
            tmean = mean(Temp_C, na.rm=T),
            tmedian = median(Temp_C, na.rm=T))

ggplot(sev_daily, aes(x=as.Date(Date), y=tmedian)) +
  geom_point()


# combine jornada and sevilleta data in one data frame
jrn_temp = jrn %>%
  group_by(year) %>%
  summarize(mintemp = min(tmean, na.rm=T),
            maxtemp = max(tmean, na.rm=T),
            meantemp = mean(tmean, na.rm=T)) %>%
  mutate(location=rep('Jornada'))

sev_temp = sev_daily %>%
  ungroup() %>%
  group_by(Year) %>%
  summarize(mintemp = min(tmean, na.rm=T),
            maxtemp = max(tmean, na.rm=T),
            meantemp = mean(tmean, na.rm=T)) %>%
  rename(year=Year) %>%
  mutate(location=rep('Sevilleta'))

temps = rbind(jrn_temp, sev_temp) %>%
  dplyr::filter(year<2021)

# timeseries plot
tempts = ggplot(temps, aes(x=year, y=meantemp, color=location)) +
  geom_line(aes(y=meantemp)) +
  #geom_line(aes(y=mintemp)) +
  #geom_line(aes(y=maxtemp)) +
  xlab('') +
  ylab('Temp (C)') +
  coord_cartesian(xlim=c(1987, 2021)) +
  theme_bw()
tempts

# boxplot
meantemp = ggplot(temps, aes(x=location, y=meantemp)) +
  geom_boxplot(na.rm=T) +
  geom_jitter(width=.1, alpha=.4, na.rm=T)+
  ylab('Mean temp (C)') +
  xlab('') +
  theme_bw()
meantemp

minyrtemp = ggplot(temps, aes(x=location, y=mintemp)) +
  geom_boxplot(na.rm=T) +
  geom_jitter(width=.1, alpha=.4, na.rm=T)+
  ylab('Min temp (C)') +
  xlab('') +
  theme_bw()
minyrtemp

maxyrtemp = ggplot(temps, aes(x=location, y=maxtemp)) +
  geom_boxplot(na.rm=T) +
  geom_jitter(width=.1, alpha=.4, na.rm=T)+
  ylab('Max temp (C)') +
  xlab('') +
  theme_bw()
maxyrtemp

# get average yearly temp of each location
mean(jrn_temp$mintemp)
mean(jrn_temp$maxtemp)
mean(jrn_temp$meantemp)

mean(sev_temp$mintemp)
mean(sev_temp$maxtemp)
mean(sev_temp$meantemp)

# combine into one plot
minmaxtemp = ggpubr::ggarrange( minyrtemp, maxyrtemp,
                             nrow=1, ncol=2, common.legend=T, legend='bottom', labels='AUTO')
minmaxtemp
ggsave('Figures/jrn_sev_temp_boxplot.png', plot=minmaxtemp, width=5, height=3)
