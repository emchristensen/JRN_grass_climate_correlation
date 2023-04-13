#' plot grass species groupings timeseries
#' 
#' EMC 5/14/21
#' last run: 2/25/22

library(dplyr)
library(ggplot2)
library(ggpubr)

# colors for figures
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

all_grass_species = read.csv('data/grass_species_timeseries_imputed.csv')

# functional groupings
func_groups = read.csv('data/grass_species_functional_grouping.csv')

# ========================================================================
# group data by species functional groups

# merge with func_groups, get total by group and quadrat
yearly_grass_groups = all_grass_species %>%
  merge(func_groups, all.y=T) %>%
  group_by(project_year, quadrat, functional_group) %>%
  summarize(cover=sum(total_cover)) 

# get yearly sum
yearly_mean_groups = yearly_grass_groups %>%
  group_by(project_year, functional_group) %>%
  summarize(sum_cover = sum(cover))

# PDO time periods
pdophases = read.csv('data/PDO_phases_byyear.csv') %>%
  #dplyr::filter(year %in% 1915:1979) %>%
  mutate(yint = rep(-0.1))

# Plot: mean grass species cover per quadrat per year
ts1 = dplyr::filter(yearly_mean_groups, project_year<1995)
ts2 = dplyr::filter(yearly_mean_groups, project_year>=1995)
grassgroups <- ggplot(ts1, aes(x=project_year)) +
  geom_area(aes(y=sum_cover, fill=functional_group), position=position_stack(reverse=T)) +
  xlim(1915,1980) +
  geom_point(data=pdophases, aes(x=year, y=yint, color = pdo_phase), size=2, shape=15) +
  labs(x = '',
       y='Total cover (m^2)',
       fill='Species Group',
       color='PDO phase',
       title='Grass functional groups through time') +
  #  ylim(0,.2) +
  scale_color_manual(values=c('blue','red')) +
  scale_fill_manual(values=cbPalette, breaks=c('transient','core'),labels=c('Transient species','Core species')) +
  theme_bw()
grassgroups
#ggsave('Figures/grass_func_groups_timeseries.png', plot=grassgroups, width=6, height=4)


# =============================================================
# relative cover of functional groups

totalcover = ts1 %>%
  group_by(project_year) %>%
  summarize(totalcover = sum(sum_cover))

relative_cover = merge(ts1, totalcover) %>%
  mutate(rel_cover = sum_cover/totalcover)


# what is the average dominant relative cover pre-1957 and after 1957
relative_cover %>% dplyr::filter(project_year<1957, functional_group=='core') %>% 
  summarize(meanrel = mean(rel_cover), min=min(rel_cover), max=max(rel_cover))
relative_cover %>% dplyr::filter(project_year>1957, functional_group=='core') %>%
  summarize(meanrel=mean(rel_cover), min=min(rel_cover), max=max(rel_cover))


grassgroups_relative <- ggplot(relative_cover) +
  geom_area(aes(x = project_year, y=rel_cover, fill=functional_group), stat='identity', position=position_stack(reverse=T)) +
  xlim(1915,1980) +
  geom_point(data=pdophases, aes(x=year, y=yint, color = pdo_phase), size=2, shape=15) +
  labs(x = '',
       y='Relative cover',
       fill='Species Group',
       color='PDO phase') +
  scale_color_manual(values=c('blue','red')) +
  scale_fill_manual(values=cbPalette, breaks=c('transient','core'),labels=c('Transient\n species','Core species')) +
  theme_bw() +
  theme(axis.text=element_text(size=6), axis.title=element_text(size=8), legend.text=element_text(size=6), legend.title=element_text(size=8))
grassgroups_relative
#ggsave('Figures/grass_func_groups_timeseries_relative_cover.png', plot=grassgroups_relative, width=6, height=4)
ggsave('Figures/2023_04/Fig4.tiff', plot=grassgroups_relative, width=8.5, height=5.7, units='cm', dpi=600)

# =================================
# relative cover of functional groups post-1995

totalcover2 = ts2 %>%
  group_by(project_year) %>%
  summarize(totalcover = sum(sum_cover))

relative_cover2 = merge(ts2, totalcover2) %>%
  mutate(rel_cover = sum_cover/totalcover)

grassgroups_relative2 <- ggplot(rbind(relative_cover,relative_cover2)) +
  geom_area(aes(x = project_year, y=rel_cover, fill=functional_group), stat='identity', position=position_stack(reverse=T)) +
  #geom_point(aes(x=project_year, y=rel_cover)) +
  xlim(1915,2020) +
  geom_point(data=pdophases, aes(x=year, y=yint, color = pdo_phase), size=2, shape=15) +
  labs(x = '',
       y='Relative cover',
       fill='Species Group',
       color='PDO phase') +
  scale_color_manual(values=c('blue','red')) +
  scale_fill_manual(values=cbPalette, breaks=c('transient','core'),labels=c('Transient\n species','Core species')) +
  theme_bw()
grassgroups_relative2
