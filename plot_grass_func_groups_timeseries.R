#' plot grass species groupings timeseries
#' 
#' EMC 5/14/21
#' last update: 12/10/21

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
pdophases = data.frame(project_year = 1915:1979,
                       yint = rep(-0.1),
                       pdo_phase = as.factor(c(rep('cool',10), rep('warm',22), rep('cool',30), rep('warm',3))))

# Plot: mean grass species cover per quadrat per year
ts1 = dplyr::filter(yearly_mean_groups, project_year<1995)
ts2 = dplyr::filter(yearly_mean_groups, project_year>=1995)
grassgroups <- ggplot(ts1, aes(x=project_year)) +
  geom_area(aes(y=sum_cover, fill=functional_group)) +
  xlim(1915,1980) +
  geom_point(data=pdophases, aes(x=project_year, y=yint, color = pdo_phase), size=2, shape=15) +
  labs(x = '',
       y='Total cover (m^2)',
       fill='Species Group',
       color='PDO phase',
       title='Grass functional groups through time') +
  #  ylim(0,.2) +
  scale_color_manual(values=c('blue','red')) +
  scale_fill_manual(values=cbPalette, labels=c('Dominant species','Non-dominant\n species')) +
  theme_bw()
grassgroups
ggsave('Figures/grass_func_groups_timeseries.png', plot=grassgroups, width=6, height=4)


# =============================================================
# functional group breakdown during discrete time periods

#pdo = read.csv('data/PDO_phases_byyear.csv')

pdophases_df = data.frame(year = 1916:1976,
                       pdo_phase = c(rep('Cool PDO \n1916-1924',9),
                                     rep('Warm PDO \n1925-1946',22),
                                     rep('Cool PDO \n1947-1976',30)))

grassgroupspdo =merge(yearly_grass_groups, pdophases_df, by.x='project_year', by.y='year')

grassgroupspdo_mean = grassgroupspdo %>%
  group_by(pdo_phase, project_year, functional_group) %>%
  summarize(mean_cover=mean(cover))

# get relative proportion of dominant/nondominant in each phase
totalbyyear = grassgroupspdo_mean %>%
  group_by(pdo_phase, project_year) %>%
  summarize(totalcover = sum(mean_cover))

relative_cover = merge(grassgroupspdo_mean, totalbyyear) %>%
  mutate(rel_cover =mean_cover/totalcover)

relative_cover_byphase = relative_cover %>%
  group_by(pdo_phase, functional_group) %>%
  summarize(mean_rel_cover = mean(rel_cover))



grassgroups_relative <- ggplot(relative_cover, aes(x=project_year)) +
  geom_area(aes(y=rel_cover, fill=functional_group)) +
  xlim(1915,1980) +
  geom_point(data=pdophases, aes(x=project_year, y=yint, color = pdo_phase), size=2, shape=15) +
  labs(x = '',
       y='Relative cover',
       fill='Species Group',
       color='PDO phase',
       title='Grass functional groups') +
  #  ylim(0,.2) +
  scale_color_manual(values=c('blue','red')) +
  scale_fill_manual(values=cbPalette, labels=c('Dominant species','Non-dominant\n species')) +
  theme_bw()
grassgroups_relative
ggsave('Figures/grass_func_groups_timeseries_relative_cover.png', plot=grassgroups_relative, width=6, height=4)


# # reorder factors
# grassgroupspdo_mean$pdo_phase = factor(grassgroupspdo_mean$pdo_phase, levels=c('Cool PDO \n1916-1924','Warm PDO \n1925-1946','Cool PDO \n1947-1976'))
# grassgroupspdo_mean$functional_group = factor(grassgroupspdo_mean$functional_group, levels=c('climax','seral','early'))
# 
# fungroupbox = ggplot(grassgroupspdo_mean, aes(x=pdo_phase, y=mean_cover, fill=functional_group)) +
#   geom_bar(position='stack', stat='identity') +
#   scale_fill_manual(values=cbPalette, labels=c('Climax','Seral','Early \nsuccessional'), name='Functional Group') +
#   xlab('') +
#   ylab('mean cover (m^2)') +
#   theme_bw() 
# fungroupbox
# 
# ggsave('Figures/species_func_groups_byphase.png', plot=fungroupbox, width=5, height=3)

# old version
# # =======================================================================
# # divide into phase sections; chop off first 3 years of each phase
# c1 = dplyr::filter(yearly_grass_groups, project_year<1925)
# w1 = dplyr::filter(yearly_grass_groups, project_year>=1928, project_year<1947)
# c2 = dplyr::filter(yearly_grass_groups, project_year>=1950, project_year<1977)
# 
# 
# # first cold period
# # get average composition over the time period for each quadrat
# c1_comp = c1 %>%
#   group_by(quadrat, functional_group) %>%
#   summarize(mean_cover=mean(cover))
# # get average over quadrats for the time period
# c1_comp_total = c1_comp %>%
#   group_by(functional_group) %>%
#   summarize(mean_cover_all=mean(mean_cover),
#             sd_cover = sd(mean_cover))
# 
# # the error bars are huge
# c1_plot = ggplot(c1_comp_total, aes(x=functional_group, y=mean_cover_all, fill=functional_group)) +
#   geom_bar(stat='identity') +
#   #geom_errorbar(aes(ymin=mean_cover_all-sd_cover, ymax=mean_cover_all+sd_cover)) +
#   scale_fill_manual(values=cbPalette, labels=c('Aristida spp.','B. eriopoda','D. pulchella','Muhlenbergia spp.','Other','P. mutica',
#                                                'S. brevifolius','Sporobolus spp.')) +
#   ylim(0,.06) +
#   ggtitle('Cool PDO: 1916-1925') +
#   ylab('Cover (m^2)') +
#   xlab('') +
#   guides(fill='none') +
#   theme_bw() +
#   theme(axis.text.x=element_blank()) 
# c1_plot
# 
# 
# # first warm period
# # get average composition over the time period for each quadrat
# w1_comp = w1 %>%
#   group_by(quadrat, functional_group) %>%
#   summarize(mean_cover=mean(cover))
# # get average over quadrats for the time period
# w1_comp_total = w1_comp %>%
#   group_by(functional_group) %>%
#   summarize(mean_cover_all=mean(mean_cover),
#             sd_cover = sd(mean_cover))
# 
# # the error bars are huge
# w1_plot = ggplot(w1_comp_total, aes(x=functional_group, y=mean_cover_all, fill=functional_group)) +
#   geom_bar(stat='identity') +
#   #geom_errorbar(aes(ymin=mean_cover_all-sd_cover, ymax=mean_cover_all+sd_cover)) +
#   scale_fill_manual(values=cbPalette, labels=c('Aristida spp.','B. eriopoda','D. pulchella','Muhlenbergia spp.','Other','P. mutica',
#                                                'S. brevifolius','Sporobolus spp.')) +
#   ylim(0,.06) +
#   ggtitle('Warm PDO: 1928-1946') +
#   ylab('Cover (m^2)') +
#   xlab('') +
#   guides(fill='none') +
#   theme_bw() +
#   theme(axis.text.x=element_blank()) 
# w1_plot
# 
# # second cool period
# # get average composition over the time period for each quadrat
# c2_comp = c2 %>%
#   group_by(quadrat, functional_group) %>%
#   summarize(mean_cover=mean(cover))
# # get average over quadrats for the time period
# c2_comp_total = c2_comp %>%
#   group_by(functional_group) %>%
#   summarize(mean_cover_all=mean(mean_cover),
#             sd_cover = sd(mean_cover))
# 
# # the error bars are huge
# c2_plot = ggplot(c2_comp_total, aes(x=functional_group, y=mean_cover_all, fill=functional_group)) +
#   geom_bar(stat='identity') +
#   #geom_errorbar(aes(ymin=mean_cover_all-sd_cover, ymax=mean_cover_all+sd_cover)) +
#   scale_fill_manual(values=cbPalette, labels=c('Climax species','Early successional species','Seral species')) +
#   ylim(0,.06) +
#   ggtitle('Cool PDO: 1950-1977') +
#   ylab('Cover (m^2)') +
#   xlab('') +
#   #guides(fill='none') +
#   theme_bw() +
#   theme(axis.text.x=element_blank()) 
# c2_plot
# 
# composition_combined = ggarrange(c1_plot, w1_plot, c2_plot, nrow=1, common.legend = T, legend='bottom')
# composition_combined
# 
# #ggsave('Figures/speices_func_groups_byphase.png', plot=composition_combined, width=7.5, height=3)
