#' Smooth climate time series using GAMs
#' 
#' 
#' EMC 9/8/21
#' last run: 4/14/22

library(dplyr)
library(ggplot2)
library(mgcv)

# colors for figures
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# read in climate variables
climate_variables = read.csv('data/climate_variables.csv') 


# ===================================================================
# smooth pdo
pdoindex = climate_variables %>%
  dplyr::select(year=water_yr, pdo)

# smooth pdo with a gam
m_pdo <- gam(pdo ~ s(year, k=10),
             data = pdoindex,
             method = 'REML')

# model checks
k.check(m_pdo)
gratia::appraise(m_pdo)

# plot model output
modeloutput_pdo = data.frame(smoothed_pdo = fitted(m_pdo), year = pdoindex$year)

pdogam = ggplot(pdoindex, aes(x=year, y=pdo)) +
  geom_point() +
  xlab('') +
  ylab('PDO index') +
  geom_line(data=modeloutput_pdo, aes(y=smoothed_pdo)) +
  theme_bw()
pdogam

smoothedpdo = merge(modeloutput_pdo, pdoindex) 

# =================================================
# smooth enso

m_nino <- gam(nino34 ~ s(water_yr, k=10),
              data=climate_variables,
              method='REML')

k.check(m_nino)

modeloutput_nino = data.frame(smoothed_nino = fitted(m_nino), year = climate_variables$water_yr)

ninogam = ggplot(climate_variables, aes(x=water_yr, y=nino34)) +
  geom_point() +
  xlab('') +
  ylab('Nino 3.4 index') +
  geom_line(data=modeloutput_nino, aes(y=smoothed_nino, x=year)) +
  theme_bw()
ninogam


# =================================================
# smooth pdsi

m_pdsi <- gam(pdsi ~ s(water_yr, k=10),
              data=climate_variables,
              method='REML')

k.check(m_pdsi)

modeloutput_pdsi = data.frame(smoothed_pdsi = fitted(m_pdsi), year = climate_variables$water_yr)

pdsigam = ggplot(climate_variables, aes(x=water_yr, y=pdsi)) +
  geom_point() +
  xlab('') +
  ylab('PDSI') +
  geom_line(data=modeloutput_pdsi, aes(y=smoothed_pdsi, x=year)) +
  theme_bw()
pdsigam

# =================================================
# smooth spei

m_spei <- gam(spei ~ s(water_yr, k=10),
              data=climate_variables,
              method='REML')

k.check(m_spei)

modeloutput_spei = data.frame(smoothed_spei = fitted(m_spei), year = climate_variables$water_yr)

speigam = ggplot(climate_variables, aes(x=water_yr, y=spei)) +
  geom_point() +
  xlab('') +
  ylab('SPEI') +
  geom_line(data=modeloutput_spei, aes(y=smoothed_spei, x=year)) +
  theme_bw()
speigam

# =================================================
# smooth meantemp

m_temp <- gam(mean_temp ~ s(water_yr, k=10),
              data=climate_variables,
              method='REML')

k.check(m_temp)  
  
modeloutput_temp = data.frame(smoothed_temp = fitted(m_temp), year = climate_variables$water_yr)

tempgam = ggplot(climate_variables, aes(x=water_yr, y=mean_temp)) +
  geom_point() +
  xlab('') +
  ylab('mean_temp') +
  geom_line(data=modeloutput_temp, aes(y=smoothed_temp, x=year)) +
  theme_bw()
tempgam

# =================================================
# smooth yearlyppt

m_yearlyppt <- gam(yearly_ppt_mm ~ s(water_yr, k=10),
                   data=climate_variables,
                   method='REML')

k.check(m_yearlyppt)

modeloutput_yearlyppt = data.frame(smoothed_yearlyppt = fitted(m_yearlyppt), year = climate_variables$water_yr)

pptgam = ggplot(climate_variables, aes(x=water_yr, y=yearly_ppt_mm)) +
  geom_point() +
  xlab('') +
  ylab('yearly ppt') +
  geom_line(data=modeloutput_yearlyppt, aes(y=smoothed_yearlyppt, x=year)) +
  theme_bw()
pptgam

# =================================================
# smooth summer ppt

m_summerppt <- gam(summer_ppt_mm ~ s(water_yr, k=10),
                   data=climate_variables,
                   method='REML')

k.check(m_summerppt)

modeloutput_summerppt = data.frame(smoothed_summerppt = fitted(m_summerppt), year = climate_variables$water_yr)

summergam = ggplot(climate_variables, aes(x=water_yr, y=summer_ppt_mm)) +
  geom_point() +
  xlab('') +
  ylab('summer ppt') +
  geom_line(data=modeloutput_summerppt, aes(y=smoothed_summerppt, x=year)) +
  theme_bw()
summergam

# =================================================
# smooth winter ppt

m_winterppt <- gam(winter_ppt_mm ~ s(water_yr, k=10),
                   data=climate_variables,
                   method='REML')

k.check(m_winterppt)

modeloutput_winterppt = data.frame(smoothed_winterppt = fitted(m_winterppt), year = climate_variables$water_yr)

wintergam = ggplot(climate_variables, aes(x=water_yr, y=winter_ppt_mm)) +
  geom_point() +
  xlab('') +
  ylab('winter ppt') +
  geom_line(data=modeloutput_winterppt, aes(y=smoothed_winterppt, x=year)) +
  theme_bw()
wintergam

# ===========================================
# save smoothed vars to csv
smoothed_climate_vars = merge(modeloutput_pdo, modeloutput_nino) %>%
  merge(modeloutput_pdsi) %>%
  merge(modeloutput_spei) %>%
  merge(modeloutput_temp) %>%
  merge(modeloutput_yearlyppt) %>%
  merge(modeloutput_summerppt) %>%
  merge(modeloutput_winterppt)

write.csv(smoothed_climate_vars, 'data/smoothed_climate_variables.csv', row.names = F)


# # =============================
# # smooth grass with a GAM
# 
# # read in data
# grass = read.csv('data/grass_median_yearly.csv') %>% rename(grass = median_grass)
# 
# #  restrict to historical section of timeseries
# grass_hist = dplyr::filter(grass, project_year<1995)
#
# # gam function from mgcv package
# m1 <- gam(grass ~ s(project_year, k=15), 
#           data=grass_hist, 
#           method='REML')
# 
# # summary of model
# summary(m1)
# # model checks
# k.check(m1) # check that k was large enough
# gratia::appraise(m1)
# 
# # plot model output
# modeloutput = data.frame(fitted_grass = fitted(m1), project_year = grass_hist$project_year)
# 
# grassgam = ggplot(grass_hist, aes(x=project_year, y=grass)) +
#   geom_point() +
#   xlab('') +
#   ylab('grass cover per m^2') +
#   geom_line(data=modeloutput, aes(y=fitted_grass)) +
#   theme_bw()
# grassgam
# 
# ggsave('Figures/grass_smoothed_gam.png', plot=grassgam, width=4, height=3)
# 
# smoothedgrass = merge(modeloutput, grass) %>% rename(smoothed_grass = fitted_grass)
# write.csv(smoothedgrass,'data/smoothed_grass_gam.csv', row.names = F)

