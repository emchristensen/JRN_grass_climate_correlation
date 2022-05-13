#' Testing for effect of PDO using other vegetation data sets from the area
#' 
#' Processing of raw data from each site is done in other scripts
#' 
#' last run: 5/13/22

library(ggplot2)
library(dplyr)
library(ggpubr)
library(emmeans)

# read in PDO phases
pdo = read.csv('data/PDO_phases_byyear.csv')
# PDO time periods
pdophases = mutate(pdo, yint=rep(0))


# ========================================================
# NMSU college ranch (CDRRC)
# data is kg/ha, "forage" may include species other than perennial grasses

# forage production data from college ranch
cdrrc = read.csv('other data sets/CDRRC/CDRRC pasture 1 long term forage production.csv', skip=2) %>%
  mutate(year = as.numeric(year)) %>%
  rename(forage = forage.production..kg.ha.1.) %>%
  dplyr::filter(!is.na(year))

# plot timeseries
cdrrc_figure = ggplot(cdrrc, aes(x=year, y=forage)) +
  geom_point(data=pdophases, aes(x=year, y=yint, color = pdo_phase), size=3, shape=15) +
  geom_point() +
  geom_line() +
  xlab('') +
  ylab('Forage production \n(kg/ha)') +
  ggtitle('NMSU Ranch') +
  
  scale_color_manual(values=c('blue','red')) +
  coord_cartesian(xlim=c(1966,2020)) +
  theme_bw()
cdrrc_figure

# boxplot: significant difference between phases?
cdrrc_pdo = merge(cdrrc, pdo, all.x=T)
cdrrcbox = ggplot(cdrrc_pdo, aes(x=pdo_phase, y=forage)) +
  geom_boxplot(na.rm=T) +
  geom_jitter(width=.1, alpha=.4, na.rm=T)+
  #ylab('Forage production (kg/ha)') +
  ylab('') +
  ggtitle('NMSU Ranch') +
  xlab('') +
  theme_bw()
cdrrcbox

# is there a significant difference?
cdrrc.aov <- aov(forage ~ pdo_phase, data=cdrrc_pdo)
summary(cdrrc.aov)
# significantly different

# get estimated marginal means
emmeans(cdrrc.aov, ~ pdo_phase)


# ========================================================
# Sevilleta 
# line point intercept transects 
# used FP location because other location had a fire in 2009
# Sum of all perennial grass cover
# Used max of spring and fall samplings per year (following Collins et al 2020)

sev = read.csv('other data sets/SEV/SEV_transects_1989_2019.csv')

# convert to % cover
sev_pct = sev %>% 
  mutate(pct_cover = total_grass/400) 

# plot timeseries
sev_figure = ggplot(sev_pct, aes(x=year, y=pct_cover)) +
  geom_point() +
  geom_line() +
  xlab('') +
  ylab('Percent cover') +
  ggtitle('Sevilleta') +
  geom_point(data=pdophases, aes(x=year, y=yint, color = pdo_phase), size=3, shape=15) +
  scale_color_manual(values=c('blue','red')) +
  coord_cartesian(xlim=c(1988,2020)) +
  theme_bw()
sev_figure

# boxplot: significant difference between phases?
sev_pdo = merge(sev_pct, pdo, all.x=T)
sevbox = ggplot(sev_pdo, aes(x=pdo_phase, y=pct_cover)) +
  geom_boxplot(na.rm=T) +
  geom_jitter(width=.1, alpha=.4, na.rm=T)+
  #ylab('Percent cover') +
  ylab('') +
  ggtitle('Sevilleta') +
  xlab('') +
  theme_bw()
sevbox

# is there a significant difference? aov
sev.aov <- aov(pct_cover ~ pdo_phase, data=sev_pdo)
summary(sev.aov)
# significantly different

# get estimated marginal means
emmeans(sev.aov, ~ pdo_phase)

# ==========================================================
# santa rita
# these data are based on 8 transects in pasture 8 -- the longest-running data (1953-2021)
# I summed grass species for each transect and took the mean of the 8 transects
sr_dat = read.csv('other data sets/santa rita/transects/pasture8_transects_mean.csv')

# plot timeseries
sr_figure = ggplot(sr_dat, aes(x=year, y=mean_grass)) +
  geom_point() +
  geom_line() +
  xlab('') +
  ylab('Cover (ft) \n along transect') +
  ggtitle('Santa Rita') +
  geom_point(data=pdophases, aes(x=year, y=yint, color = pdo_phase), size=3, shape=15) +
  scale_color_manual(values=c('blue','red')) +
  coord_cartesian(xlim=c(1952,2021)) +
  theme_bw()
sr_figure

# boxplot: significant difference between phases?
sr_pdo = merge(sr_dat, pdo, all.x=T)
srbox = ggplot(sr_pdo, aes(x=pdo_phase, y=mean_grass)) +
  geom_boxplot(na.rm=T) +
  geom_jitter(width=.1, alpha=.4, na.rm=T)+
  #ylab('Percent cover') +
  ylab('') +
  ggtitle('Santa Rita') +
  xlab('') +
  theme_bw()
srbox

# is there a significant difference? aov
sr.aov <- aov(mean_grass ~ pdo_phase, data=sr_pdo)
summary(sr.aov)
# different

# get estimated marginal means
emmeans(sr.aov, ~ pdo_phase)


# =========================================================
# Jornada quadrats for comparison
jrngrass = read.csv('data/grass_median_yearly.csv') %>% dplyr::filter(project_year<1995)

# plot timeseries
jrn_figure = ggplot(jrngrass, aes(x=project_year, y=median_grass)) +
  geom_point(data=pdophases, aes(x=year, y=yint, color = pdo_phase), size=3, shape=15) +
  geom_point() +
  geom_line() +
  xlab('') +
  ylab('Percent cover') +
  ggtitle('Jornada') +
  
  scale_color_manual(values=c('blue','red')) +
  coord_cartesian(xlim=c(1915,1980)) +
  theme_bw()
jrn_figure

# boxplot: significant difference between phases?
jrn_pdo = merge(jrngrass, pdo, by.x = 'project_year', by.y='year', all.x=T)
jrnbox = ggplot(jrn_pdo, aes(x=pdo_phase, y=median_grass)) +
  geom_boxplot(na.rm=T) +
  geom_jitter(width=.1, alpha=.4, na.rm=T)+
  #ylab('Percent cover') +
  ylab('') +
  ggtitle('Jornada') +
  xlab('') +
  theme_bw()
jrnbox

# is there a significant difference? aov
jrn.aov <- aov(median_grass ~ pdo_phase, data=jrn_pdo)
summary(jrn.aov)
# significantly different

# get estimated marginal means
emmeans(jrn.aov, ~ pdo_phase)


# ===========================================================
# combine into multi-part figures

allplots = ggpubr::ggarrange(jrn_figure, jrnbox, cdrrc_figure, cdrrcbox, sr_figure, srbox, sev_figure, sevbox, 
                             nrow=4, ncol=2, common.legend=T, legend='bottom', labels='AUTO')
allplots
ggsave('Figures/other_data_multifigure.png', plot=allplots, width=5, height=8)


