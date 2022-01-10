#' Santa Rita transect data
#' 
#' EMC 1/6/22

library(dplyr)
library(ggplot2)

raw = read.csv('other data sets/santa rita/transects/ltcover_2021Oct11.csv')

# choose which transects to use
#   exclude transects that have been treated to kill mesquite, or burned in any of the 3 fires
transectdat = raw %>% 
  dplyr::filter(MESQSTAT=='LIVE', X1994.Burn=='NO', X2017.Burn=='NO', X2018.Burn=='NO') %>%
  unique()

# grass species codes (from species_code_translation.txt)
grasses = c('ANBA','ANCI','ANSC','ARFE','ARGL','ARGL1','ARIS','ASC','DICA','ERCU','MURE','PAOB','BOCH','BOCU','BOEL','BOER','BOFI',
            'BOHI','BORO','COPA','ENDE','ERAG','ERCH','ERCH1','ERIN','ERIN1','ERLE','ERPU','ERSU','HECO','HECO1','HIBE','LECO',
            'LEDU','LYPH','MUAR','MUEM','MUHL','MUPO','MUPO1','PAHA','PAMU','PECI','SEMA','SIHY','SPCO','SPOR','STIP','TRCA1',
            'TRMU','TRPU')

# restrict transect data to grass species
transectgrass = dplyr::filter(transectdat, SPECIES %in% grasses) %>%
  dplyr::select(PASTTRAN, PASTURE, TRANSECT, SPECIES, YR1953:YR2021)

# pivot long
transectgrasslong = tidyr::pivot_longer(transectgrass, cols=YR1953:YR2021, names_to='y', values_to='cover_raw') %>%
  mutate(year = as.numeric(substr(y,3,6)),
         cover_ft = cover_raw/10)
# fill in NA
transectgrasslong$cover_ft[transectgrasslong$cover_ft<0] <- NA


# determine which transects were sampled in which years
alltransectsallyears = expand.grid(year=1953:2021, PASTTRAN=unique(transectgrasslong$PASTTRAN))

samplingdates = transectgrasslong %>%
  dplyr::filter(!is.na(cover_ft)) %>%
  dplyr::select(PASTTRAN, PASTURE, TRANSECT, year) %>%
  unique() %>%
  merge(alltransectsallyears, all=T) %>%
  mutate(sampled=rep(0))
samplingdates$sampled[!is.na(samplingdates$PASTURE)] <- 1

minmaxyear = samplingdates %>%
  dplyr::filter(sampled==1) %>%
  group_by(PASTTRAN) %>%
  summarize(minyear = min(year),
            maxyear = max(year),
            nyears = n_distinct(year))
# first years range from 1953-1975. 
# Restrict to longest-running, started in 1953 (all pasture 8)

selectedtransects = minmaxyear$PASTTRAN[minmaxyear$minyear<=1953]

# ==============================================================
# look at sampling history of selected transects
samplehist = samplingdates %>% dplyr::filter(PASTTRAN %in% selectedtransects) %>%
  dplyr::select(PASTTRAN, year, sampled) %>%
  tidyr::pivot_wider(id_cols=PASTTRAN, names_from=year, values_from=sampled)

# these transects were all sampled in selected years
selectedyears = c(1953:1959, 1961:1966, 1970,  1984, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021)

# sum all grass species and plot data
dat = transectgrasslong %>% 
  dplyr::filter(PASTTRAN %in% selectedtransects, year %in% selectedyears) %>%
  group_by(PASTTRAN, PASTURE, TRANSECT, year) %>%
  summarize(total_grass = sum(cover_ft, na.rm=T))



ggplot(dat, aes(x=year, y=total_grass, color=PASTTRAN, group=PASTTRAN)) +
  geom_point() +
  geom_line()

# looks like no data fro 1975-1981
test = dplyr::filter(raw, PASTTRAN==selectedtransects[1], SPECIES %in% grasses)
# most grass species were not recorded these years

# get mean of transects
meangrass = dat %>% group_by(year) %>%
  summarize(mean_grass = mean(total_grass))

ggplot(meangrass, aes(x=year, y=mean_grass)) +
  geom_point() +
  geom_line()

# write cleaned data to file
write.csv(meangrass, 'other data sets/santa rita/transects/pasture8_transects_mean.csv', row.names=F)


# ===============================================================
# include more transects