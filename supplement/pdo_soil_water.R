#' test for differences in soil water content by warm/cool PDO
#' soil water content data from https://doi.org/10.6073/pasta/0efc7f8ca440d31e764c961a176c1c14 (Accessed 2021-10-07).
#' 
#' EMC 10/7/21

library(dplyr)
library(ggplot2)

soil = read.csv('data/soil water content/JRNStudy013_npp_soil_water_content_calc.csv')
pdo = read.csv('data/PDO_phases_byyear.csv')

# site closest to quadrats is IBPE
ibpe = dplyr::filter(soil, Site=='IBPE')

# there are 10 tubes/measurements per location. take average
soil_water = ibpe %>% dplyr::group_by(DateFixed, Depth) %>%
  summarize(mean_vwc = mean(VWC)) %>%
  mutate(year=lubridate::year(DateFixed),
         month=lubridate::month(DateFixed))

ggplot(soil_water, aes(x=DateFixed, y=mean_vwc, color=Depth)) +
  geom_point()

# aggregate to yearly
soil_water_yearly = soil_water %>%
  group_by(year, Depth) %>%
  summarize(yearly_vwc = mean(mean_vwc)) %>%
  group_by(Depth)

ggplot(soil_water_yearly, aes(x=year, y=yearly_vwc, color=Depth)) +
  geom_point() 


# combine with PDO  (separate depths
pdo_soil_30cm = dplyr::filter(soil_water_yearly, Depth==30) %>%
  merge(pdo, by='year', all.x=T)
pdo_soil_60cm = dplyr::filter(soil_water_yearly, Depth==60) %>%
  merge(pdo, by='year', all.x=T)
pdo_soil_90cm = dplyr::filter(soil_water_yearly, Depth==90) %>%
  merge(pdo, by='year', all.x=T)
pdo_soil_120cm = dplyr::filter(soil_water_yearly, Depth==120) %>%
  merge(pdo, by='year', all.x=T)
pdo_soil_150cm = dplyr::filter(soil_water_yearly, Depth==150) %>%
  merge(pdo, by='year', all.x=T)
pdo_soil_180cm = dplyr::filter(soil_water_yearly, Depth==180) %>%
  merge(pdo, by='year', all.x=T)
pdo_soil_210cm = dplyr::filter(soil_water_yearly, Depth==210) %>%
  merge(pdo, by='year', all.x=T)
pdo_soil_240cm = dplyr::filter(soil_water_yearly, Depth==240) %>%
  merge(pdo, by='year', all.x=T)

ggplot(pdo_soil_30cm, aes(x=year, y=yearly_vwc, color=pdo_phase)) +
  geom_point()
ggplot(pdo_soil_240cm, aes(x=year, y=yearly_vwc, color=pdo_phase)) +
  geom_point()

# boxplots of soil water by phase at each depth
soil30 = ggplot(pdo_soil_30cm, aes(x=pdo_phase, y=yearly_vwc)) +
  geom_boxplot(na.rm=T) +
  geom_jitter(width=.1, alpha=.4, na.rm=T)+
  ylab('Soil Water Content at 30cm') +
  xlab('') +
  theme_bw()
soil30
soil240 = ggplot(pdo_soil_240cm, aes(x=pdo_phase, y=yearly_vwc)) +
  geom_boxplot(na.rm=T) +
  geom_jitter(width=.1, alpha=.4, na.rm=T)+
  ylab('Soil Water Content at 240cm') +
  xlab('') +
  theme_bw()
soil240


# summary for each depth
dplyr::filter(pdo_soil_30cm, pdo_phase=='warm') %>% dplyr::select(yearly_vwc) %>% unlist() %>%mean()
dplyr::filter(pdo_soil_30cm, pdo_phase=='cool') %>% dplyr::select(yearly_vwc) %>% unlist() %>%mean()

# test for significance
soil30.aov <- aov(yearly_vwc ~ pdo_phase, data=pdo_soil_30cm)
summary(soil30.aov)
soil60.aov <- aov(yearly_vwc ~ pdo_phase, data=pdo_soil_60cm)
summary(soil60.aov)
soil90.aov <- aov(yearly_vwc ~ pdo_phase, data=pdo_soil_90cm)
summary(soil90.aov)
soil120.aov <- aov(yearly_vwc ~ pdo_phase, data=pdo_soil_120cm)
summary(soil120.aov)
soil150.aov <- aov(yearly_vwc ~ pdo_phase, data=pdo_soil_150cm)
summary(soil150.aov)
soil180.aov <- aov(yearly_vwc ~ pdo_phase, data=pdo_soil_180cm)
summary(soil180.aov)
soil210.aov <- aov(yearly_vwc ~ pdo_phase, data=pdo_soil_210cm)
summary(soil210.aov)
soil240.aov <- aov(yearly_vwc ~ pdo_phase, data=pdo_soil_240cm)
summary(soil240.aov)
# not significant

