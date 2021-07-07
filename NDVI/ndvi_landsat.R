
library(dplyr)
library(ggplot2)


#' @description NDVI data from google earth engine: get date information from system.index and create date column
#' 
#' @param ndviframe data frame of raw ndvi data from google earth engine
get_date_from_index = function(ndviframe) {
  ndviframe$year = substr(ndviframe$system.index,13,16)
  ndviframe$month = substr(ndviframe$system.index,17,18)
  ndviframe$day = substr(ndviframe$system.index,19,20)
  ndviframe$date = as.Date(paste(ndviframe$year, ndviframe$month, ndviframe$day, sep='-'))
  return(ndviframe)
}

# read in files downloaded from gee
ndvi5_raw = read.csv('NDVI/Landsat5_SR_NDVI_jornada_1984_2011.csv')
ndvi7_raw = read.csv('NDVI/Landsat7_SR_NDVI_jornada_1999_2020.csv')

# threshhold for number of good pixels
minpixels5 = 0.75*max(ndvi5_raw$NDVI_count)
minpixels7 = 0.75*max(ndvi7_raw$NDVI_count)

# process raw data
ndvi5 = ndvi5_raw %>% 
  get_date_from_index() %>%
  dplyr::filter(NDVI_count>minpixels5) %>%
  group_by(month, year) %>%
  summarize(NDVI_mean=mean(NDVI_mean)) %>%
  mutate(date=as.Date(paste(year, month, '15',sep='-')))

ndvi7 = ndvi7_raw %>%
  get_date_from_index() %>%
  dplyr::filter(NDVI_count>minpixels7) %>%
  group_by(month, year) %>%
  summarize(NDVI_mean=mean(NDVI_mean)) %>%
  mutate(date=as.Date(paste(year, month,'15',sep='-')))

# plot monthly data compare landsat 5 and 7
ndvimonthly = ggplot(ndvi5, aes(x=date, y=NDVI_mean, color='landsat5')) +
  geom_point() +
  geom_line() +
  geom_point(data=ndvi7, aes(x=date, y=NDVI_mean, color='landsat7')) +
  geom_line(data=ndvi7, aes(x=date, y=NDVI_mean, color='landsat7'))
ndvimonthly
ggsave('Figures/NDVI_monthly.png', plot=ndvimonthly, width=12, height = 4)


# get yearly average
ndvi5yearly = ndvi5 %>%
  group_by(year) %>%
  summarize(NDVI_mean=mean(NDVI_mean)) %>%
  ungroup()
ndvi7yearly = ndvi7 %>%
  group_by(year) %>%
  summarize(NDVI_mean=mean(NDVI_mean)) %>%
  ungroup()

ndviyearly = ggplot(ndvi5yearly, aes(x=as.numeric(year), y=NDVI_mean, group=1)) +
  geom_point() +
  geom_line() +
  geom_point(data=ndvi7yearly, aes(color='landsat7')) +
  geom_line(data=ndvi7yearly, aes(color='landsat7'))
ndviyearly
ggsave('Figures/NDVI_yearly.png', plot=ndviyearly, width=6, height=3)
