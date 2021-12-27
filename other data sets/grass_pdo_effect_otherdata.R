#' Testing for effect of PDO using other vegetation data sets from the area
#' 
#' EMC 12/21/21

library(ggplot2)
library(dplyr)
library(ggpubr)

# read in PDO phases
pdo = read.csv('data/PDO_phases_byyear.csv')
# PDO time periods
pdophases = mutate(pdo, yint=rep(0))

# ==============================================
# Jornada NPP quadrats: 1989-2018
# data consists of cover by species, 1m2 quadrats, 49 quadrats per site, using 14 sites

npp = read.csv('other data sets/npp/JRN_011002_npp_quadrat_meas.csv')

# data filters: fall sampling only, perennial grass species only, no cyperus, exclude site with 48 quadrats
npp_perenn_grasses = npp %>% 
  dplyr::filter(season=='F', form=='GRASS', habit=='P', USDA_code != 'CYES', site != 'COLL')

# get average perenn grass cover by site and year (total/49 quadrats)
npp_site_year = npp_perenn_grasses %>%
  group_by(year, site) %>%
  summarize(mean_cover = sum(cover, na.rm=T)/49)

# get overall mean
npp_mean = npp_site_year %>%
  ungroup() %>% group_by(year) %>%
  summarize(mean_cover = mean(mean_cover)) %>%
  mutate(pct_cover=mean_cover/100)

# plot timeseries
npp_figure = ggplot(npp_mean, aes(x=year, y=pct_cover)) +
  geom_point() +
  geom_line() +
  xlab('') +
  ylab('Percent cover') +
  ggtitle('Jornada NPP quadrats') +
  geom_point(data=pdophases, aes(x=year, y=yint, color = pdo_phase), size=3, shape=15) +
  scale_color_manual(values=c('blue','red')) +
  coord_cartesian(xlim=c(1988,2020)) +
  theme_bw() 
npp_figure

#ggsave('Figures/Jornada_NPP_quadrat_timeseries.png', plot=npp_figure, height=4, width=5)

# boxplot: significant difference between phases?
npp_pdo = merge(npp_mean, pdo, all.x=T)
nppbox = ggplot(npp_pdo, aes(x=pdo_phase, y=pct_cover)) +
  geom_boxplot(na.rm=T) +
  geom_jitter(width=.1, alpha=.4, na.rm=T)+
  ylab('Percent cover') +
  ggtitle('Jornada NPP quadrats') +
  xlab('') +
  theme_bw()
nppbox

# is there a significant difference? aov
npp.aov <- aov(pct_cover ~ pdo_phase, data=npp_pdo)
summary(npp.aov)
# no significant difference in cover between warm/cool
npp_pdo %>% group_by(pdo_phase) %>% summarize(mean=mean(pct_cover))

# ========================================================
# CDRRC college ranch
# data is kg/ha, may include species other than perennial grasses

# forage production data from college ranch
cdrrc = read.csv('other data sets/CDRRC/CDRRC pasture 1 long term forage production.csv', skip=2) %>%
  mutate(year = as.numeric(year)) %>%
  rename(forage = forage.production..kg.ha.1.) %>%
  dplyr::filter(!is.na(year))

# plot timeseries
cdrrc_figure = ggplot(cdrrc, aes(x=year, y=forage)) +
  geom_point() +
  geom_line() +
  xlab('') +
  ylab('Forage production (kg/ha)') +
  ggtitle('CDRRC College Ranch') +
  geom_point(data=pdophases, aes(x=year, y=yint, color = pdo_phase), size=3, shape=15) +
  scale_color_manual(values=c('blue','red')) +
  coord_cartesian(xlim=c(1966,2020)) +
  theme_bw()
cdrrc_figure

#ggsave('Figures/CDRRC_forage_timeseries.png', plot=cdrrc_figure, height=4, width=5)

# boxplot: significant difference between phases?
cdrrc_pdo = merge(cdrrc, pdo, all.x=T)
cdrrcbox = ggplot(cdrrc_pdo, aes(x=pdo_phase, y=forage)) +
  geom_boxplot(na.rm=T) +
  geom_jitter(width=.1, alpha=.4, na.rm=T)+
  ylab('Forage production (kg/ha)') +
  ggtitle('CDRRC College Ranch') +
  xlab('') +
  theme_bw()
cdrrcbox

# is there a significant difference? aov
cdrrc.aov <- aov(forage ~ pdo_phase, data=cdrrc_pdo)
summary(cdrrc.aov)
# significantly different: p = 0.012
cdrrc_pdo %>% group_by(pdo_phase) %>% summarize(mean=mean(forage))


# ========================================================
# Sevilleta 
# line point intercept transects 
# DW location had a fire in 2009. Use FP location instead.

sev = read.csv('other data sets/SEV/SEV_transects_fall_1994_2019.csv')

# restrict to one location, and convert to % cover
sev_fp = sev %>% dplyr::filter(location=='FP') %>%
  mutate(pct_cover = total_grass/400)

# plot timeseries
sev_figure = ggplot(sev_fp, aes(x=year, y=pct_cover)) +
  geom_point() +
  geom_line() +
  xlab('') +
  ylab('Percent cover') +
  ggtitle('Sevilleta LPI transect') +
  geom_point(data=pdophases, aes(x=year, y=yint, color = pdo_phase), size=3, shape=15) +
  scale_color_manual(values=c('blue','red')) +
  coord_cartesian(xlim=c(1993,2020)) +
  theme_bw()
sev_figure

#ggsave('Figures/CDRRC_forage_timeseries.png', plot=cdrrc_figure, height=4, width=5)

# boxplot: significant difference between phases?
sev_pdo = merge(sev_fp, pdo, all.x=T)
sevbox = ggplot(sev_pdo, aes(x=pdo_phase, y=pct_cover)) +
  geom_boxplot(na.rm=T) +
  geom_jitter(width=.1, alpha=.4, na.rm=T)+
  ylab('Percent cover') +
  ggtitle('Sevilleta LPI transect') +
  xlab('') +
  theme_bw()
sevbox

# is there a significant difference? aov
sev.aov <- aov(pct_cover ~ pdo_phase, data=sev_pdo)
summary(sev.aov)
# significantly different: p = 0.012
sev_pdo %>% group_by(pdo_phase) %>% summarize(mean=mean(pct_cover))

# ===========================================================
# combine into multi-part figures
 
timeseries_plots = ggpubr::ggarrange(npp_figure, cdrrc_figure, sev_figure, nrow=1, common.legend = T, legend='bottom')
box_plots = ggpubr::ggarrange(nppbox, cdrrcbox, sevbox, nrow=1)

ggsave('Figures/other_data_timeseries.png', plot=timeseries_plots, width=6.5, height=3)
ggsave('Figures/other_data_boxplots.png', plot=box_plots, width=6.5, height=3)
