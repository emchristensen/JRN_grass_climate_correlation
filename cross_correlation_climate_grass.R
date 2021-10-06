#'  Goal is to find degree of correlation between grass and climate (precip/PDO), and when in the
#' timeseries the relationship breaks down
#' 
#' Negative June-Nov SOI -> higher Oct-March precip in NM
#' PDO cold phase -> drier than normal
#' 
#' EMC 3/23/21
#' last update: 8/18/21

library(dplyr)
library(ggplot2)

# read in veg data
grassts = read.csv('data/grass_median_yearly.csv')

  
ggplot(grassts, aes(x=project_year, y=median_grass)) +
  geom_line() +
  geom_point()

# read in climate data
indices_yearly = read.csv('data/climate_variables.csv')


# ======================================================================
# try plain CCF https://nwfsc-timeseries.github.io/atsa-labs/sec-tslab-correlation-within-and-among-time-series.html

# first combine all data
alldata = indices_yearly %>%
  merge(grassts, by.x='water_yr',by.y='project_year') %>%
  dplyr::filter(water_yr<1995) %>%
  mutate(grass_diff = c(NA,diff(median_grass)))
  
# ccf of all variables with grass cover
ccf(alldata$yearly_ppt_mm, alldata$median_grass)
ccf(alldata$summer_ppt_mm, alldata$median_grass)
ccf(alldata$winter_ppt_mm, alldata$median_grass)
ccf(alldata$mean_temp, alldata$median_grass)
ccf(alldata$soi, alldata$median_grass)
ccf(alldata$pdo, alldata$median_grass)
ccf(alldata$pdsi, alldata$median_grass)
ccf(alldata$spei, alldata$median_grass)
# significance in 0, -1, -5, +7 lag for PDSI
# significance in -1 for SPEI
# significance with PDO for lag 0-10

# ccf of all variables with year to year diff in grass cover
alldata2 = alldata[-1,] # remove first row (diff is NA)
ccf(alldata2$yearly_ppt_mm, alldata2$grass_diff)
ccf(alldata2$summer_ppt_mm, alldata2$grass_diff)
ccf(alldata2$winter_ppt_mm, alldata2$grass_diff)
ccf(alldata2$mean_temp, alldata2$grass_diff)
ccf(alldata2$soi, alldata2$grass_diff)
ccf(alldata2$pdo, alldata2$grass_diff)
ccf(alldata2$pdsi, alldata2$grass_diff)
ccf(alldata2$spei, alldata2$grass_diff)



