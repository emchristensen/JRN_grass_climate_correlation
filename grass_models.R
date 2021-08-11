#' grass model selection
#' EMC 8/3/21

library(dplyr)
library(ggplot2)
library(MuMIn)

# min-max normalization function
normalize <- function(x) {
  return ((x-min(x)) / (max(x) - min(x)))
}

# 10 year moving average function
moving_avg_10y <- function(index, year) {
  smoothed = c()
  for (n in 10:length(index)) {
    avg10y = data.frame(index_smooth = mean(index[(n-9):n]),
                        year=year[n])
    smoothed = rbind(smoothed, avg10y)
  }
  return(smoothed)
}


# read in data
#grass = read.csv('data/grass_shrub_trends_yearly.csv')
grass = read.csv('data/grass_shrub_median_yearly.csv') %>% rename(grass = median_grass)
pdoindex = read.csv('data/PDO_long_1900_2020.csv')
soiindex = read.csv('data/SOI_long_1866_2020.csv')
pptdat = read.csv('data/climate_variables.csv')
pdo_factor = read.csv('data/PDO_phases_byyear.csv')

# convert monthly indices to yearly
pdo_yearly = pdoindex %>% group_by(year) %>%
  summarize(pdo_year = mean(pdo))
soi_yearly = soiindex %>% group_by(year) %>%
  summarise(soi_year=mean(soi))


# normalize timeseries
pdo_yearly$pdo_norm = normalize(pdo_yearly$pdo_year)
soi_yearly$soi_norm = normalize(soi_yearly$soi_year)
grass$grass_norm = normalize(grass$grass)
pptdat$yearly_ppt_norm = normalize(pptdat$yearly_ppt_mm)
pptdat$winter_ppt_norm = normalize(pptdat$winter_ppt_mm)
pptdat$summer_ppt_norm = normalize(pptdat$summer_ppt_mm)

# calculate 10-year moving average of pdo
pdo_10y = moving_avg_10y(pdo_yearly$pdo_year, year=pdo_yearly$year) %>%
  mutate(pdo_smooth=normalize(index_smooth))

# calculate moving average of yearly precip
precip_10y = moving_avg_10y(pptdat$yearly_ppt_mm, year=pptdat$year) %>%
  mutate(precip_smooth=normalize(index_smooth))

# moving avg summer and winter precip
winter_ppt_10y = moving_avg_10y(pptdat$winter_ppt_mm, year=pptdat$year) %>%
  mutate(winter_ppt_smooth=normalize(index_smooth))
summer_ppt_10y = moving_avg_10y(pptdat$summer_ppt_mm, year=pptdat$year) %>%
  mutate(summer_ppt_smooth=normalize(index_smooth))

# create one big data frame
combined_data = merge(pdo_yearly, soi_yearly, all=T) %>%
  merge(grass, by.x='year', by.y='project_year', all=T) %>%
  merge(pptdat, all=T) %>% 
  merge(pdo_10y[,c('year','pdo_smooth')], all=T) %>%
  merge(pdo_factor, all=T) %>%
  merge(precip_10y[,c('year','precip_smooth')], all=T) %>%
  merge(winter_ppt_10y[,c('year','winter_ppt_smooth')], all=T) %>%
  merge(summer_ppt_10y[,c('year','summer_ppt_smooth')], all=T) %>%
  dplyr::select(year, grass_norm, pdo_norm, pdo_smooth, pdo_phase, soi_norm, yearly_ppt_norm, precip_smooth, winter_ppt_norm, winter_ppt_smooth,
                summer_ppt_norm, summer_ppt_smooth)


# plot raw normalized time series
rawplot = ggplot(combined_data, aes(x=year, y=pdo_norm)) +
  #geom_point() +
  geom_line() +
  geom_point(aes(x=year, y=grass_norm),color='blue') +
  geom_line(aes(x=year, y=grass_norm),color='blue') +
  #geom_point(aes(x=year, y=soi_norm), color='purple') +
  geom_line(aes(x=year, y=soi_norm), color='purple') +
  geom_line(aes(y=yearly_ppt_norm), color='red') +
  geom_hline(yintercept=.5) +
  ggtitle('PDO and grass cover (yearly; normalized)') +
  xlab('') +
  theme_bw()
rawplot



# ==========================================
# models

# cross-correlation functions
corr_data = dplyr::filter(combined_data, !is.na(grass_norm))
ccf(corr_data$pdo_norm, corr_data$grass_norm)
ccf(corr_data$soi_norm, corr_data$grass_norm)
ccf(corr_data$yearly_ppt_norm, corr_data$grass_norm)
ccf(corr_data$winter_ppt_norm, corr_data$grass_norm)
ccf(corr_data$summer_ppt_norm, corr_data$grass_norm)

ccf(corr_data$pdo_norm, corr_data$winter_ppt_norm)
ccf(corr_data$pdo_norm, corr_data$yearly_ppt_norm)
ccf(corr_data$pdo_norm, corr_data$summer_ppt_norm)
# only pdo has significant ccf with grass
# up to lag 10 is important for pdo

# create columns for pdo lags
combined_data$pdo_norm_lag1 = c(NA,combined_data$pdo_norm[1:(nrow(combined_data)-1)])
combined_data$pdo_norm_lag2 = c(NA,combined_data$pdo_norm_lag1[1:(nrow(combined_data)-1)])
combined_data$pdo_norm_lag3 = c(NA,combined_data$pdo_norm_lag2[1:(nrow(combined_data)-1)])
combined_data$pdo_norm_lag4 = c(NA,combined_data$pdo_norm_lag3[1:(nrow(combined_data)-1)])
combined_data$pdo_norm_lag5 = c(NA,combined_data$pdo_norm_lag4[1:(nrow(combined_data)-1)])
combined_data$pdo_norm_lag6 = c(NA,combined_data$pdo_norm_lag5[1:(nrow(combined_data)-1)])
combined_data$pdo_norm_lag7 = c(NA,combined_data$pdo_norm_lag6[1:(nrow(combined_data)-1)])
combined_data$pdo_norm_lag8 = c(NA,combined_data$pdo_norm_lag7[1:(nrow(combined_data)-1)])
combined_data$pdo_norm_lag9 = c(NA,combined_data$pdo_norm_lag8[1:(nrow(combined_data)-1)])
combined_data$pdo_norm_lag10 = c(NA,combined_data$pdo_norm_lag9[1:(nrow(combined_data)-1)])

model_data = combined_data %>%
  dplyr::filter(year>=1916, year<1980)

# construct polynomial terms
model_data$pdo_smooth2 = model_data$pdo_smooth^2






# models with single predictors
model1 = lm(grass_norm ~ pdo_norm, data=model_data)
model1a = lm(grass_norm ~ pdo_phase, data=model_data)
model2 = lm(grass_norm ~ pdo_smooth, data=model_data)
model3 = lm(grass_norm ~ soi_norm, data=model_data)
model4 = lm(grass_norm ~ yearly_ppt_norm, data=model_data)
model4a = lm(grass_norm ~ precip_smooth, data=model_data)
model5 = lm(grass_norm ~ winter_ppt_norm, data=model_data)
model5a = lm(grass_norm ~ winter_ppt_smooth, data=model_data)
model6 = lm(grass_norm ~ summer_ppt_norm, data=model_data)
model6a = lm(grass_norm ~ summer_ppt_smooth, data=model_data)

AICc(model1, model1a, model2, model3, model4, model4a, model5, model5a, model6, model6a)

# so far smoothed pdo is by far the best predictor


# models with multiple predictors
model7 = lm(grass_norm ~ pdo_smooth + soi_norm, data=model_data)
model8 = lm(grass_norm ~ pdo_smooth + yearly_ppt_norm, data=model_data)
model8a = lm(grass_norm ~ pdo_smooth + precip_smooth, data=model_data)
model9 = lm(grass_norm ~ pdo_smooth + winter_ppt_norm, data=model_data)
model10 = lm(grass_norm ~ pdo_smooth + summer_ppt_norm, data=model_data)
model11 = lm(grass_norm ~ pdo_smooth + pdo_smooth2, data=model_data)
model12 = lm(grass_norm ~ pdo_smooth + soi_norm + summer_ppt_norm, data=model_data)
model13 = lm(grass_norm ~ pdo_smooth + winter_ppt_smooth, data=model_data)


AICc(model2, model7, model8, model8a, model9, model10, model11, model12, model13)

# model with just smoothed pdo is still the best -- other AIC not more than 2 units lower

# models with pdo and lags
model14 = lm(grass_norm ~ pdo_norm + pdo_norm_lag1, data=model_data)
model15 = lm(grass_norm ~ pdo_norm + pdo_norm_lag1 + pdo_norm_lag2, data=model_data)
model16 = lm(grass_norm ~ pdo_norm + pdo_norm_lag1 + pdo_norm_lag2 + pdo_norm_lag3, data=model_data)
model17 = lm(grass_norm ~ pdo_norm + pdo_norm_lag1 + pdo_norm_lag2 + pdo_norm_lag3 + pdo_norm_lag4, data=model_data)
model18 = lm(grass_norm ~ pdo_norm + pdo_norm_lag1 + pdo_norm_lag2 + pdo_norm_lag3 + pdo_norm_lag4 + pdo_norm_lag5, data=model_data)
model19 = lm(grass_norm ~ pdo_norm + pdo_norm_lag1 + pdo_norm_lag2 + pdo_norm_lag3 + pdo_norm_lag4 + pdo_norm_lag5 +
               pdo_norm_lag6, data=model_data)
model20 = lm(grass_norm ~ pdo_norm + pdo_norm_lag1 + pdo_norm_lag2 + pdo_norm_lag3 + pdo_norm_lag4 + pdo_norm_lag5 +
               pdo_norm_lag6 + pdo_norm_lag7, data=model_data)
model21 = lm(grass_norm ~ pdo_norm + pdo_norm_lag1 + pdo_norm_lag2 + pdo_norm_lag3 + pdo_norm_lag4 + pdo_norm_lag5 +
               pdo_norm_lag6 + pdo_norm_lag7 + pdo_norm_lag8, data=model_data)
model22 = lm(grass_norm ~ pdo_norm + pdo_norm_lag1 + pdo_norm_lag2 + pdo_norm_lag3 + pdo_norm_lag4 + pdo_norm_lag5 +
               pdo_norm_lag6 + pdo_norm_lag7 + pdo_norm_lag8 + pdo_norm_lag9, data=model_data)
model23 = lm(grass_norm ~ pdo_norm + pdo_norm_lag1 + pdo_norm_lag2 + pdo_norm_lag3 + pdo_norm_lag4 + pdo_norm_lag5 +
               pdo_norm_lag6 + pdo_norm_lag7 + pdo_norm_lag8 + pdo_norm_lag9 + pdo_norm_lag10, data=model_data)

AICc(model2, model14, model15, model16, model17, model18, model19, model20, model21, model22, model23)

# model 2 with the smooth is still the best by far


ggplot(model_data, aes(x=year)) + 
  geom_line(aes(y=pdo_smooth)) +
  geom_line(aes(y=grass_norm), color='blue') +
  geom_point(aes(y=grass_norm), color='blue') #+
  geom_line(aes(y=summer_ppt_norm), color='purple')

ggplot(model_data, aes(x=pdo_smooth, y=grass_norm)) +
  geom_point()

# prediction based on best model
newdata = dplyr::filter(combined_data, year>1916) %>%
  dplyr::select(year, pdo_smooth, summer_ppt_norm)
predicted_grass = predict.lm(model10, newdata, se.fit=T)
pred_grass_min = predicted_grass$fit-predicted_grass$se.fit
pred_grass_max = predicted_grass$fit+predicted_grass$se.fit

modelprediction = newdata %>%
  mutate(predicted_grass=predicted_grass$fit,
         pred_grass_min=pred_grass_min,
         pred_grass_max=pred_grass_max)

ggplot(modelprediction, aes(x=year)) +
  geom_line(aes(y=pred_grass_min), color='blue') +
  geom_line(aes(y=pred_grass_max), color='blue') +
  geom_line(aes(y=pdo_smooth)) +
  geom_point(data=combined_data, aes(x=year, y=grass_norm), color='blue') +
  xlim(1916, 2020)

