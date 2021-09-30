#' plot grass species groupings timeseries
#' 
#' EMC 5/14/21
#' last update: 9/30/21

library(dplyr)
library(ggplot2)

# colors for figures
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

all_grass_species = read.csv('data/grass_species_timeseries_imputed.csv')

# only interested in certain species
grass_species = all_grass_species$species[!all_grass_species$species %in% c('BOUTE','CYPER','SPORO','UNKG')] %>% unique()

# ========================================================================
# plot species composition time series


# get yearly sum for plotting 
yearly_mean_grass_species = all_grass_species %>%
  group_by(project_year, species) %>%
  summarize(mean_cover=sum(total_cover)) %>%
  dplyr::filter(species %in% grass_species)

# PDO time periods
pdophases = data.frame(project_year = 1915:1979,
                       yint = rep(-0.1),
                       pdo_phase = as.factor(c(rep('cool',10), rep('warm',22), rep('cool',30), rep('warm',3))))

# Plot: mean grass species cover per quadrat per year
ts1 = dplyr::filter(yearly_mean_grass_species, project_year<1995)
ts2 = dplyr::filter(yearly_mean_grass_species, project_year>=1995)
grassspecies <- ggplot(ts1, aes(x=project_year)) +
  geom_area(aes(y=mean_cover, fill=species)) +
  xlim(1915,1980) +
  geom_point(data=pdophases, aes(x=project_year, y=yint, color = pdo_phase), size=2, shape=15) +
  labs(x = '',
       y='Total cover (m^2)',
       fill='Species',
       color='PDO phase',
       title='Perennial grass species through time') +
  #  ylim(0,.2) +
  scale_color_manual(values=c('blue','red')) +
  #scale_fill_manual(values=cbPalette, labels=c('Aristida spp.','B. eriopoda','D. pulchella','Muhlenbergia spp.','Other','P. mutica',
  #                                             'S. brevifolius','Sporobolus spp.')) +
  theme_bw()
grassspecies
ggsave('Figures/grass_species_timeseries.png', plot=grassspecies, width=6, height=4)


# =============================================================
# species composition during discrete time periods

# divide into phase sections; chop off first 3 years of each phase
c1 = dplyr::filter(all_grass_species, project_year<1925)
w1 = dplyr::filter(all_grass_species, project_year>=1928, project_year<1947)
c2 = dplyr::filter(all_grass_species, project_year>=1950, project_year<1977)


# first cold period
# get average composition over the time period for each quadrat
c1_comp = c1 %>%
  group_by(quadrat, species) %>%
  summarize(mean_cover=mean(total_cover))
# get average over quadrats for the time period
c1_comp_total = c1_comp %>%
  group_by(species) %>%
  summarize(mean_cover_all=mean(mean_cover),
            sd_cover = sd(mean_cover))

# the error bars are huge
c1_plot = ggplot(c1_comp_total, aes(x=species, y=mean_cover_all, fill=species)) +
  geom_bar(stat='identity') +
  #geom_errorbar(aes(ymin=mean_cover_all-sd_cover, ymax=mean_cover_all+sd_cover)) +
  scale_fill_manual(values=cbPalette, labels=c('Aristida spp.','B. eriopoda','D. pulchella','Muhlenbergia spp.','Other','P. mutica',
                                               'S. brevifolius','Sporobolus spp.')) +
  ylim(0,.06) +
  ggtitle('Cool PDO: 1916-1925') +
  ylab('Cover (m^2)') +
  xlab('') +
  guides(fill='none') +
  theme_bw() +
  theme(axis.text.x=element_blank()) 
c1_plot


# first warm period
# get average composition over the time period for each quadrat
w1_comp = w1 %>%
  group_by(quadrat, species) %>%
  summarize(mean_cover=mean(total_cover))
# get average over quadrats for the time period
w1_comp_total = w1_comp %>%
  group_by(species) %>%
  summarize(mean_cover_all=mean(mean_cover),
            sd_cover = sd(mean_cover))

# the error bars are huge
w1_plot = ggplot(w1_comp_total, aes(x=species, y=mean_cover_all, fill=species)) +
  geom_bar(stat='identity') +
  #geom_errorbar(aes(ymin=mean_cover_all-sd_cover, ymax=mean_cover_all+sd_cover)) +
  scale_fill_manual(values=cbPalette, labels=c('Aristida spp.','B. eriopoda','D. pulchella','Muhlenbergia spp.','Other','P. mutica',
                                               'S. brevifolius','Sporobolus spp.')) +
  ylim(0,.06) +
  ggtitle('Warm PDO: 1928-1946') +
  ylab('Cover (m^2)') +
  xlab('') +
  guides(fill='none') +
  theme_bw() +
  theme(axis.text.x=element_blank()) 
w1_plot

# second cool period
# get average composition over the time period for each quadrat
c2_comp = c2 %>%
  group_by(quadrat, species) %>%
  summarize(mean_cover=mean(total_cover))
# get average over quadrats for the time period
c2_comp_total = c2_comp %>%
  group_by(species) %>%
  summarize(mean_cover_all=mean(mean_cover),
            sd_cover = sd(mean_cover))

# the error bars are huge
c2_plot = ggplot(c2_comp_total, aes(x=species, y=mean_cover_all, fill=species)) +
  geom_bar(stat='identity') +
  #geom_errorbar(aes(ymin=mean_cover_all-sd_cover, ymax=mean_cover_all+sd_cover)) +
  scale_fill_manual(values=cbPalette, labels=c('Aristida spp.','B. eriopoda','D. pulchella','Muhlenbergia spp.','Other','P. mutica',
                                               'S. brevifolius','Sporobolus spp.')) +
  ylim(0,.06) +
  ggtitle('Cool PDO: 1950-1977') +
  ylab('Cover (m^2)') +
  xlab('') +
  guides(fill='none') +
  theme_bw() +
  theme(axis.text.x=element_blank()) 
c2_plot

composition_combined = gridExtra::grid.arrange(c1_plot, w1_plot, c2_plot, nrow=2)

ggsave('Figures/speices_composition_byphase.png', plot=composition_combined, width=6, height=4)
