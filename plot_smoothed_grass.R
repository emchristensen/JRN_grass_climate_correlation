#' look at smoothed grass timeseries
#' EMC 9/8/21

library(dplyr)
library(ggplot2)

source('data_functions.R')

# read in data
grass = read.csv('data/grass_median_yearly.csv') %>% rename(grass = median_grass)
# restrict to historical section of timeseries
grass_hist = dplyr::filter(grass, project_year<1995) %>%
  mutate(grass_norm = normalize(grass))


# =============================
# smooth with a GAM

library(mgcv)
y = grass_hist$grass_norm
xt = grass_hist$project_year
time <- 1:length(y)

# setting k = -1 (knots) turns on generalized cross-validation which automatically balances simplicity and explanatory power
m1 <- gam(y ~ s(xt, k=-1))

modeloutput = data.frame(fitted_grass = fitted(m1), project_year = xt)

grassgam = ggplot(grass_hist, aes(x=project_year, y=grass_norm)) +
  geom_point() +
  xlab('') +
  ylab('grass cover per m^2') +
  geom_line(data=modeloutput, aes(y=fitted_grass)) +
  theme_bw()
grassgam

ggsave('Figures/grass_smoothed_gam.png', plot=grassgam, width=4, height=3)



# ===================================================================
# plot with pdo
pdoindex = read.csv('data/PDO_long_1900_2020.csv')
pdo_yearly = pdoindex %>% group_by(year) %>%
  summarize(pdo_year = mean(pdo))
pdo_yearly$pdo_norm = normalize(pdo_yearly$pdo_year)
pdo_10y = moving_avg_10y(pdo_yearly$pdo_year, year=pdo_yearly$year) %>%
  mutate(pdo_smooth=normalize(index_smooth))


grassgampdo = ggplot(grass_hist, aes(x=project_year, y=grass_norm)) +
  geom_point() +
  xlab('') +
  ylab('grass cover per m^2') +
  geom_line(data=modeloutput, aes(y=fitted_grass, color='Smoothed grass')) +
  geom_line(data=pdo_10y[pdo_10y$year<1980,], aes(x=year, y=pdo_smooth, color='Smoothed PDO'), show.legend=T) +
  theme_bw()
grassgampdo

ggsave('Figures/grass_pdo_smoothed.png', plot=grassgampdo, width=6, height=3)
