#' Compare RAP cover data to quadrat pg gover
#' EMC 4/29/21

library(dplyr)
library(ggplot2)

rap = read.csv('RAP_tool/RAP_VegCover.csv')
# get year column for rap
rap$year = as.numeric(substr(rap$system.index,1,4))

#daub = read.csv('../JRN_quadrat_datapaper/PatchVeg/Jornada_quadrat_patch_vegetation.csv')
veg = read.csv('../JRN_quadrat_timeseries/data/quadrat_veg.csv')


# 1995 point
yr = 1995
veg_combined = dplyr::filter(rap, year==yr) %>%
  dplyr::select(quadrat=NAME, year, perennials_pct=PFGC, shrub_pct=SHR) %>%
  merge(dplyr::filter(veg, project_year==yr), by.x=c('quadrat','year'), by.y=c('quadrat','project_year'))

# plot perennial grass comparison
ggplot(veg_combined, aes(x=grass_cover, y=perennials_pct/100)) +
  geom_point() +
  geom_abline(slope=1) +
  xlab('quadrat') +
  ylab('RAP') +
  ggtitle(paste(yr,'perennial grass'))

# plot shrub comparison
ggplot(veg_combined, aes(x=shrub_cover, y=shrub_pct/100)) +
  geom_point() +
  geom_abline(slope=1) +
  xlab('quadrat') +
  ylab('RAP') +
  ylim(0,1) +
  ggtitle(paste(yr,'shrub'))


# plot time series for one quad
quad = 'N3'
test = dplyr::filter(rap, NAME==quad)
vegtest = dplyr::filter(veg, quadrat==quad, project_year>=1995)
ggplot(test, aes(x=year, y=PFGC, color='RAP')) +
  geom_point() +
  geom_line() +
  xlab('') +
  ylab('grass % cover') +
  geom_point(data=vegtest, aes(x=project_year, y=grass_cover*100, color='quadrat')) +
  ggtitle(quad)

ggplot(rap, aes(x=year, y=PFGC, group=NAME)) +
  geom_line()
