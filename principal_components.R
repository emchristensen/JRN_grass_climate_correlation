#' principal component analysis
#' Using all the short-term climate variables
#' 
#' Erica Christensen 9/12/22
#' Last update: 2/6/23
#' 
library(dplyr)
library(ggplot2)
library(vegan)
set.seed(1000)

# read in climate data
climatedata = read.csv('data/climate_variables.csv') %>%
  dplyr::rename(year=water_yr) %>%
  dplyr::filter(year %in% 1916:1979) %>%
  dplyr::select(year, summer_ppt_mm, summer_pdsi, summer_spei, summer_tmax, summer_tmin,
                winter_ppt_mm, winter_pdsi, winter_spei, winter_tmax, winter_tmin)

row.names(climatedata) <- climatedata$year

# PCA
climatePCA <- rda(climatedata[,2:ncol(climatedata)], scale=T)
climate_s <- summary(climatePCA) 
climate_s 

# write component axes to csv
write.csv(climate_s$sites, file='data/climate_variables_pca.csv', row.names = T)

# plot pc1 and pc2

biplot(climatePCA)

# trying to do this in ggplot
smry <- summary(climatePCA)
df1  <- data.frame(smry$sites[,1:2])       # PC1 and PC2
df2  <- data.frame(smry$species[,1:2])     # loadings for PC1 and PC2
rda.plot <- ggplot(df1, aes(x=PC1, y=PC2)) + 
  geom_text(aes(label=rownames(df1)),size=4) +
  geom_hline(yintercept=0, linetype="dotted") +
  geom_vline(xintercept=0, linetype="dotted") +
  coord_fixed()
rda.plot


# Parallel Analysis for number of components to retain (Horn, 1965)
#set.seed(921)
hornpa::hornpa(k = 9, size = 37, reps = 1000, seed = 921)
# Calculate Correlations from Loadings 
var_loadings <- vegan::scores(climatePCA, display = "species", scaling = 0, choices=c(1,2,3))
var_loadings
evs <- eigenvals(climatePCA)
evs # eigenvalues of PC1, PC2 and PC3 are greater than .95 intervals
climatedatascaled <- as.data.frame(scale(climatedata[,2:ncol(climatedata)]))

# # correlations:
# # Axis 1
# var_loadings[1,1] * ((evs[1]/var(climatedatascaled$summer_ppt_mm))^1/2)
# var_loadings[2,1] * ((evs[1]/var(climatedatascaled$summer_pdsi))^1/2)
# var_loadings[3,1] * ((evs[1]/var(climatedatascaled$summer_spei))^1/2)
# var_loadings[4,1] * ((evs[1]/var(climatedatascaled$summer_tmax))^1/2)
# var_loadings[5,1] * ((evs[1]/var(climatedatascaled$summer_tmin))^1/2)
# var_loadings[6,1] * ((evs[1]/var(climatedatascaled$winter_ppt_mm))^1/2)
# var_loadings[7,1] * ((evs[1]/var(climatedatascaled$winter_pdsi))^1/2)
# var_loadings[8,1] * ((evs[1]/var(climatedatascaled$winter_spei))^1/2)
# var_loadings[9,1] * ((evs[1]/var(climatedatascaled$winter_tmax))^1/2)
# var_loadings[10,1] * ((evs[1]/var(climatedatascaled$winter_tmin))^1/2)
# # Axis 2
# var_loadings[1,2] * ((evs[2]/var(climatedatascaled$summer_ppt_mm))^1/2)
# var_loadings[2,2] * ((evs[2]/var(climatedatascaled$summer_pdsi))^1/2)
# var_loadings[3,2] * ((evs[2]/var(climatedatascaled$summer_spei))^1/2)
# var_loadings[4,2] * ((evs[2]/var(climatedatascaled$summer_tmax))^1/2)
# var_loadings[5,2] * ((evs[2]/var(climatedatascaled$summer_tmin))^1/2)
# var_loadings[6,2] * ((evs[2]/var(climatedatascaled$winter_ppt_mm))^1/2)
# var_loadings[7,2] * ((evs[2]/var(climatedatascaled$winter_pdsi))^1/2)
# var_loadings[8,2] * ((evs[2]/var(climatedatascaled$winter_spei))^1/2)
# var_loadings[9,2] * ((evs[2]/var(climatedatascaled$winter_tmax))^1/2)
# var_loadings[10,2] * ((evs[2]/var(climatedatascaled$winter_tmin))^1/2)
# # Axis 3
# var_loadings[1,3] * ((evs[3]/var(climatedatascaled$summer_ppt_mm))^1/2)
# var_loadings[2,3] * ((evs[3]/var(climatedatascaled$summer_pdsi))^1/2)
# var_loadings[3,3] * ((evs[3]/var(climatedatascaled$summer_spei))^1/2)
# var_loadings[4,3] * ((evs[3]/var(climatedatascaled$summer_tmax))^1/2)
# var_loadings[5,3] * ((evs[3]/var(climatedatascaled$summer_tmin))^1/2)
# var_loadings[6,3] * ((evs[3]/var(climatedatascaled$winter_ppt_mm))^1/2)
# var_loadings[7,3] * ((evs[3]/var(climatedatascaled$winter_pdsi))^1/2)
# var_loadings[8,3] * ((evs[3]/var(climatedatascaled$winter_spei))^1/2)
# var_loadings[9,3] * ((evs[3]/var(climatedatascaled$winter_tmax))^1/2)
# var_loadings[10,3] * ((evs[3]/var(climatedatascaled$winter_tmin))^1/2)
