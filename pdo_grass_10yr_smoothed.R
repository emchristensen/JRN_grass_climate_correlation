#' plot 10-year smoothed PDO and grass
#' EMC 7/27/21
#' 

library(dplyr)
library(ggplot2)


# min-max normalization function
normalize <- function(x) {
  return ((x-min(x)) / (max(x) - min(x)))
}

# read in data
grass = read.csv('data/grass_shrub_trends_yearly.csv')
pdoindex = read.csv('data/PDO_long_1900_2020.csv')

pdo_yearly = pdoindex %>% group_by(year) %>%
  summarize(pdo_year = mean(pdo))

# plot raw pdo and grass
rawdata = merge(pdo_yearly, grass, by.x='year', by.y='project_year', all=T)
ggplot(rawdata, aes(x=pdo_year, y=mean_grass)) +
  geom_point()

# normalize pdo and grass
pdo_yearly$pdo_norm = normalize(pdo_yearly$pdo_year)
grass$grass_norm = normalize(grass$mean_grass)

# plot raw normalized pdo and grass
rawplot = ggplot(pdo_yearly, aes(x=year, y=pdo_norm)) +
  geom_point() +
  geom_line() +
  geom_point(data=grass, aes(x=project_year, y=grass_norm),color='blue') +
  geom_line(data=grass, aes(x=project_year, y=grass_norm),color='blue') +
  geom_hline(yintercept=.5) +
  ggtitle('PDO and grass cover (yearly; normalized)') +
  xlab('') +
  theme_bw()
rawplot
ggsave('Figures/PDO_grass_yearly_normalized.png', plot=rawplot, width=6, height=2.5)

# calculate 10-year moving average of pdo
pdo_10y = c()
for (n in 10:nrow(pdo_yearly)) {
  avg10y = data.frame(pdo=mean(pdo_yearly$pdo_year[(n-9):n]),
                      year=pdo_yearly$year[n])
  pdo_10y = rbind(pdo_10y,avg10y)
}
pdo_10y$pdo_smooth_norm = normalize(pdo_10y$pdo)

# plot smoothed timeseries
ggplot(pdo_10y, aes(x=year, y=pdo_smooth_norm)) +
  geom_point() +
  geom_line()



# calculate 10-year moving average of grass
grass_10y = c()
for (n in 10:(nrow(grass)-5)) {
  newline = data.frame(grass=mean(grass$mean_grass[(n-9):n]),
                       year=grass$project_year[n])
  grass_10y = rbind(grass_10y, newline)
}
grass_10y$grass_smooth_norm = normalize(grass_10y$grass)

# plot pdo and grass together
pdo_10y_smooth = pdo_10y %>% dplyr::select(year, value=pdo_smooth_norm)
pdo_10y_smooth$index = rep('PDO 10y smooth') 
grass_10y_smooth = grass_10y %>% dplyr::select(year, value=grass_smooth_norm)
grass_10y_smooth$index = rep('Grass')

# add a 3 year lag to grass
#grass_10y_smooth$year = grass_10y_smooth$year -2

pdograsssmooth = rbind(pdo_10y_smooth, grass_10y_smooth)
pdograss_ts_plot = ggplot(pdograsssmooth, aes(x=year, y=value, color=index)) +
  geom_point() + 
  geom_line() +
  xlab('') +
  ylab('smoothed and normalized PDO and grass') +
  theme_bw()
pdograss_ts_plot 
ggsave('Figures/PDO_grass_10ysmooth_normalized_timeseries.png', plot=pdograss_ts_plot, width=6, height=4)





# ========================================================================

# plot pdo and grass against each other
pdograss = merge(pdo_yearly, grass, by.x='year', by.y='project_year', all.x=T)
ggplot(pdograss, aes(x=pdo_norm, y=grass_norm)) +
  geom_point()

ggplot(pdograss, aes(x=year)) +
  #geom_line() +
  geom_line(aes(y=pdo_norm), color='blue') +
  geom_line(aes(y=grass_norm), color='red')
