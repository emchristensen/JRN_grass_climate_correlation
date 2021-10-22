#' look at smoothed grass timeseries
#' EMC 9/8/21

library(dplyr)
library(ggplot2)
library(mgcv)

# colors for figures
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# read in data
grass = read.csv('data/grass_median_yearly.csv') %>% rename(grass = median_grass)

# restrict to historical section of timeseries
grass_hist = dplyr::filter(grass, project_year<1995)

# read in climate variables
climate_variables = read.csv('data/climate_variables.csv') 

# =============================
# smooth grass with a GAM

y = grass_hist$grass
xt = grass_hist$project_year
time <- 1:length(y)

# setting k = -1 (knots) turns on generalized cross-validation which automatically balances simplicity and explanatory power
m1 <- gam(y ~ s(xt, k=-1))
summary(m1)

modeloutput = data.frame(fitted_grass = fitted(m1), project_year = xt)

grassgam = ggplot(grass_hist, aes(x=project_year, y=grass)) +
  geom_point() +
  xlab('') +
  ylab('grass cover per m^2') +
  geom_line(data=modeloutput, aes(y=fitted_grass)) +
  theme_bw()
grassgam

#ggsave('Figures/grass_smoothed_gam.png', plot=grassgam, width=4, height=3)

smoothedgrass = merge(modeloutput, grass) %>% rename(smoothed_grass = fitted_grass)
write.csv(smoothedgrass,'data/smoothed_grass_gam.csv', row.names = F)

# ===================================================================
# smooth pdo
pdoindex = climate_variables %>%
  dplyr::select(year=water_yr, pdo)
#pdoindex$pdo_norm = normalize(pdoindex$pdo)

# smooth pdo with a gam
y1 = pdoindex$pdo
xt1 = pdoindex$year
time1 <- 1:length(y1)

# setting k = -1 (knots) turns on generalized cross-validation which automatically balances simplicity and explanatory power
m_pdo <- gam(y1 ~ s(xt1, k=-1))

modeloutput_pdo = data.frame(smoothed_pdo = fitted(m_pdo), year = xt1)

pdogam = ggplot(pdoindex, aes(x=year, y=pdo)) +
  geom_point() +
  xlab('') +
  ylab('PDO index') +
  geom_line(data=modeloutput_pdo, aes(y=smoothed_pdo)) +
  theme_bw()
pdogam


smoothedpdo = merge(modeloutput_pdo, pdoindex) 
write.csv(smoothedpdo,'data/smoothed_pdo_gam.csv', row.names=F)

# =================================================
# smooth enso

y2 = climate_variables$nino34
xt2 = climate_variables$water_yr
time2 <- 1:length(y2)

# setting k = -1 (knots) turns on generalized cross-validation which automatically balances simplicity and explanatory power
m_nino <- gam(y2 ~ s(xt2, k=-1))

modeloutput_nino = data.frame(smoothed_nino = fitted(m_nino), year = xt2)

ninogam = ggplot(climate_variables, aes(x=water_yr, y=nino34)) +
  geom_point() +
  xlab('') +
  ylab('Nino 3.4 index') +
  geom_line(data=modeloutput_nino, aes(y=smoothed_nino, x=year)) +
  theme_bw()
ninogam


# =================================================
# smooth pdsi

y3 = climate_variables$pdsi
xt3 = climate_variables$water_yr
time3 <- 1:length(y3)

# setting k = -1 (knots) turns on generalized cross-validation which automatically balances simplicity and explanatory power
m_pdsi <- gam(y3 ~ s(xt3, k=-1))

modeloutput_pdsi = data.frame(smoothed_pdsi = fitted(m_pdsi), year = xt3)

pdsigam = ggplot(climate_variables, aes(x=water_yr, y=pdsi)) +
  geom_point() +
  xlab('') +
  ylab('PDSI') +
  geom_line(data=modeloutput_pdsi, aes(y=smoothed_pdsi, x=year)) +
  theme_bw()
pdsigam

# =================================================
# smooth spei

y4 = climate_variables$spei
xt4 = climate_variables$water_yr
time4 <- 1:length(y4)

# setting k = -1 (knots) turns on generalized cross-validation which automatically balances simplicity and explanatory power
m_spei <- gam(y4 ~ s(xt4, k=-1))

modeloutput_spei = data.frame(smoothed_spei = fitted(m_spei), year = xt4)

speigam = ggplot(climate_variables, aes(x=water_yr, y=spei)) +
  geom_point() +
  xlab('') +
  ylab('SPEI') +
  geom_line(data=modeloutput_spei, aes(y=smoothed_spei, x=year)) +
  theme_bw()
speigam

# =================================================
# smooth meantemp

y5 = climate_variables$mean_temp
xt5 = climate_variables$water_yr
time5 <- 1:length(y5)

# setting k = -1 (knots) turns on generalized cross-validation which automatically balances simplicity and explanatory power
m_temp <- gam(y5 ~ s(xt5, k=-1))

modeloutput_temp = data.frame(smoothed_temp = fitted(m_temp), year = xt5)

tempgam = ggplot(climate_variables, aes(x=water_yr, y=mean_temp)) +
  geom_point() +
  xlab('') +
  ylab('mean_temp') +
  geom_line(data=modeloutput_temp, aes(y=smoothed_temp, x=year)) +
  theme_bw()
tempgam

# =================================================
# smooth yearlyppt

y6 = climate_variables$yearly_ppt_mm
xt6 = climate_variables$water_yr
time6 <- 1:length(y6)

# setting k = -1 (knots) turns on generalized cross-validation which automatically balances simplicity and explanatory power
m_yearlyppt <- gam(y6 ~ s(xt6, k=-1))

modeloutput_yearlyppt = data.frame(smoothed_yearlyppt = fitted(m_yearlyppt), year = xt6)

pptgam = ggplot(climate_variables, aes(x=water_yr, y=yearly_ppt_mm)) +
  geom_point() +
  xlab('') +
  ylab('yearly ppt') +
  geom_line(data=modeloutput_yearlyppt, aes(y=smoothed_yearlyppt, x=year)) +
  theme_bw()
pptgam

# =================================================
# smooth summer ppt

y7 = climate_variables$summer_ppt_mm
xt7 = climate_variables$water_yr
time7 <- 1:length(y7)

# setting k = -1 (knots) turns on generalized cross-validation which automatically balances simplicity and explanatory power
m_summerppt <- gam(y7 ~ s(xt7, k=-1))

modeloutput_summerppt = data.frame(smoothed_summerppt = fitted(m_summerppt), year = xt7)

summergam = ggplot(climate_variables, aes(x=water_yr, y=summer_ppt_mm)) +
  geom_point() +
  xlab('') +
  ylab('summer ppt') +
  geom_line(data=modeloutput_summerppt, aes(y=smoothed_summerppt, x=year)) +
  theme_bw()
summergam

# =================================================
# smooth winter ppt

y8 = climate_variables$winter_ppt_mm
xt8 = climate_variables$water_yr
time8 <- 1:length(y8)

# setting k = -1 (knots) turns on generalized cross-validation which automatically balances simplicity and explanatory power
m_winterppt <- gam(y8 ~ s(xt8, k=-1))

modeloutput_winterppt = data.frame(smoothed_winterppt = fitted(m_winterppt), year = xt8)

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

# ==================================================================
# plot both together ----
# coeff and intercept from best model in grass_pdo_model_selection.Rmd
coeff <- 0.04319
intercept <- 0.039939

grass_pdo_plot = ggplot() +
  geom_line(data=modeloutput, aes(x=project_year, y=fitted_grass, color='Grass')) +
  geom_line(data=modeloutput_pdo, aes(x=year, y=(fitted_pdo*coeff)+intercept, color='PDO')) +
  scale_y_continuous(
    name='Grass cover',
    sec.axis = sec_axis(~(.-intercept)/coeff, name='PDO index')
  ) +
  theme_bw() +
  scale_color_manual(values=cbPalette[c(1,3)]) +
  xlab('')
grass_pdo_plot

ggsave('Figures/grass_pdo_smoothed_GAM.png', plot=grass_pdo_plot, height=4, width=5)

