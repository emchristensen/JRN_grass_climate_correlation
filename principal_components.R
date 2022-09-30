#' principal component regression
#' Using all the short-term climate variables
#' 
#' EMC 9/12/22
#' 
library(dplyr)
library(ggplot2)
library(pls)
set.seed(1000)

# read in climate and grass data
climatedata = read.csv('data/climate_variables.csv') 
grass = read.csv('data/grass_median_yearly.csv') 

# create one big data frame; create lag variables for pdsi (1), spei (1), and summer pdsi (1)
combined_data = grass %>%
  merge(climatedata, by.x='project_year', by.y='water_yr', all=T) %>%
  rename(year=project_year) %>%
  mutate(summerpdsi_lag1=lag(summer_pdsi, 1),
         spei_lag1=lag(spei, 1),
         pdsi_lag1=lag(pdsi,1),
         yearlyppt_lag1=lag(yearly_ppt_mm,1)) 

# select short term variables
combined_data_short = combined_data %>%
  dplyr::filter(year %in% 1916:1979) %>%
  dplyr::select( grass=median_grass, pdsi, spei, mean_temp, summer_pdsi, summer_spei, yearly_ppt_mm, summer_ppt_mm, winter_ppt_mm, demartonne,
                 summerpdsi_lag1, spei_lag1, pdsi_lag1, yearlyppt_lag1)

# pcr model
pcr_model = pcr(grass ~., data = combined_data_short, scale=T, validation='CV')
summary(pcr_model)
validationplot(pcr_model, val.type='RMSEP')
# 2 principal components is best

# predict 1995-2016 grass values from model
test_vars = combined_data %>%
  dplyr::filter(year >1979) %>%
  dplyr::select(-year, -median_grass)
y_test = combined_data %>%
  dplyr::filter(year>1979) %>%
  dplyr::select(grass=median_grass)
pcr_pred <- predict(pcr_model, test_vars, ncomp=2)

#calculate RMSE
prediction = cbind(pcr_pred, y_test)
names(prediction) <- c('pred','actual')
sqrt(mean((prediction$pred - prediction$actual)^2, na.rm=T))
# RMSE is 0.0267


# =====================================================================
# get principal components for regular lm run

# normalize data frame
data_short_norm <- scale(combined_data_short) %>% as.data.frame() %>% dplyr::select(-grass)

# PCA
short_pca1 <- prcomp(data_short_norm, center=TRUE, scale.=TRUE)

# components of the PCA
short_pca1$x

# percent of variance explained by each principal component
plot(summary(short_pca1)$importance[3,])

pcs <- as.data.frame(short_pca1$x)
plot(scale(combined_data_short$grass), pcs$PC1)

# linear model
ols.data <- cbind(scale_grass =scale(combined_data_short$grass), pcs, project_year=1916:1979)
lmodel <- lm(scale_grass ~ PC1 + PC2, data = ols.data)
summary(lmodel)

# write pca data frame to csv
write.csv(ols.data, file='data/climate_variables_pca.csv', row.names=F)
