#' Santa Rita transect data
#' Data from https://ag.arizona.edu/SRER/data.html
#' 
#' Data citation: 
#'   McClaran, M. (2012): Santa Rita Experimental Range Long Term Transect Database. – In: Dengler, J., 
#'   Oldeland, J., Jansen, F., Chytrý, M., Ewald, J., Finckh, M., Glöckler, F., Lopez-Gonzalez, G., Peet, R.K., 
#'   Schaminée, J.H.J. [Eds.]: Vegetation databases for the 21st century. – Biodiversity & Ecology 4: 435–435. 
#'   DOI: 10.7809/b-e.00222.
#' 
#' Excluded transects that were treated for Mesquite control or burned
#' Longest running transects are 8 transects from pasture 8 (started in 1953; others began 1959 or later)
#' Summed grass species for each transect, took mean of the 8 transects
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

# grass species codes (from species_code_translation.txt, looked up on USDA PLANTS)
grasses = c('ANBA','ANCI','ANSC','ARFE','ARGL','ARGL1','ARIS','ASC','DICA','ERCU','MURE','PAOB','BOCH','BOCU','BOEL','BOER','BOFI',
            'BOHI','BORO','COPA','ENDE','ERAG','ERCH','ERCH1','ERIN','ERIN1','ERLE','ERPU','ERSU','HECO','HECO1','HIBE','LECO',
            'LEDU','LYPH','MUAR','MUEM','MUHL','MUPO','MUPO1','PAHA','PAMU','PECI','SEMA','SIHY','SPCO','SPOR','STIP','TRCA1',
            'TRMU','TRPU')

# shrub species -- included trees, yucca YUEL, and agave AGAV, AGPA3
shrubspecies = c('ACAN','ACCO','ACGR','AGAV','AGPA3','ALWR','FRAM','ANTH','ATCA','ATCO','BABR','BARA','BASA','CAER','CAAR','CARL',
                 'CEPA','CEPA2','CEFL','CEMI','DAWH','ENFR','EPTR','FOSP','FOSP2','GUSA','GUTI','APLA','HALA','HIEM','KRGR','KRPA',
                 'LATR','LYAN','LYPA','LYCI','LYTO','MIBI','MIBI3','MIDY','NOMI','PRVE','PRJU','PRJUV','QUEM','TRCA2','YUEL','COLY',
                 'COOB','ZIOB','EYPO')

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

# these transects were all sampled in selected years; between 1970 and 1984 shrubs were sampled but not grasses
selectedyears = c(1953:1959, 1961:1966, 1970,  1984, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021)

# sum all grass species and plot data
dat = transectgrasslong %>% 
  dplyr::filter(PASTTRAN %in% selectedtransects, year %in% selectedyears) %>%
  group_by(PASTTRAN, PASTURE, TRANSECT, year) %>%
  summarize(total_grass = sum(cover_ft, na.rm=T))



ggplot(dat, aes(x=year, y=total_grass, color=PASTTRAN, group=PASTTRAN)) +
  geom_point() +
  geom_line()


# get mean of transects
meangrass = dat %>% group_by(year) %>%
  summarize(mean_grass = mean(total_grass))

ggplot(meangrass, aes(x=year, y=mean_grass)) +
  geom_point() +
  geom_line()

# write cleaned data to file
write.csv(meangrass, 'other data sets/santa rita/transects/pasture8_transects_mean.csv', row.names=F)



#================================================================
# look at more pastures

selectedtransects2 = minmaxyear$PASTTRAN[minmaxyear$minyear<=1959]

# look at sampling history of selected transects
samplehist = samplingdates %>% dplyr::filter(PASTTRAN %in% selectedtransects2) %>%
  dplyr::select(PASTTRAN, year, sampled) %>%
  tidyr::pivot_wider(id_cols=PASTTRAN, names_from=year, values_from=sampled)

# these transects were all sampled in selected years
selectedyears = c(1953:1959, 1961:1966, 1970,  
                  1984, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021)

# sum all grass species and plot data
dat2 = transectgrasslong %>% 
  dplyr::filter(PASTTRAN %in% selectedtransects2, year %in% selectedyears) %>%
  group_by(PASTTRAN, PASTURE, TRANSECT, year) %>%
  summarize(total_grass = sum(cover_ft, na.rm=T))

ggplot(dat2, aes(x=year, y=total_grass, group=PASTTRAN)) +
  geom_point() +
  geom_line()

mean2 = dat2 %>% group_by(year) %>% summarize(mean_grass = mean(total_grass)) %>%
  dplyr::filter(year>=1959)

ggplot(mean2, aes(x=year, y=mean_grass)) +
  geom_point() +
  geom_line()

# ===============================================================
# look at shrub cover on pasture 8



pasture8 = dplyr::filter(raw, PASTTRAN %in% selectedtransects, !(SPECIES %in% grasses))
