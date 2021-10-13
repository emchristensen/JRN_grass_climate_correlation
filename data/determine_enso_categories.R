#' Determining El Nino/La Nina/neutral from ENSO indices
#' El Nino: 5 consecutive months < -0.5 from Sept-Mar
#' La Nina: 5 consecutive months > 0.5 from Sept-Mar
#' Neutral: doesn't fit above criteria
#' 
#' EMC 10/13/21

library(dplyr)

# Nino 3.4 index ----
nino34 = read.csv('data/raw_climate_data/Nino34_1870_2020.csv')

# convert to long format, select important months
nino34_long = dplyr::select(nino34, year, jan, feb, mar, sep, oct, nov, dec) %>%
  tidyr::pivot_longer(cols=c('jan','feb','mar','sep','oct','nov','dec'), names_to='month', values_to='nino34')

# adjust year for fall
nino34_long$year[nino34_long$month %in% c('sep','oct','nov','dec')] <- nino34_long$year[nino34_long$month %in% c('sep','oct','nov','dec')] +1

# loop through years and determine if it meets the criteria for nino/nina
#selected_year = 1908
ensocategories = c()
for (selected_year in unique(nino34_long$year)) {
  yearindex = dplyr::filter(nino34_long, year==selected_year)
  
  # get number of months of consecutive nino or nina conditions
  yearindex$ninomonth = yearindex$nino34 < -.5
  yearindex$ninamonth = yearindex$nino34 > .5
  consecutivenina = rle(yearindex$ninamonth)$lengths[rle(yearindex$ninamonth)$values==T]
  consecutivenino = rle(yearindex$ninomonth)$lengths[rle(yearindex$ninomonth)$values==T]
  
  if (max(consecutivenina)>=5) {nina=T} else {nina=F}
  if (max(consecutivenino)>=5) {nino=T} else {nino=F}
  ensocategories = rbind(ensocategories, data.frame(year=selected_year, nino=nino, nina=nina))
}

# designate years as el nino, la nina, or neutral
ensocategories$category_nino34 = NA
ensocategories$category_nino34[ensocategories$nino==T & ensocategories$nina==F] <- 'nino'
ensocategories$category_nino34[ensocategories$nino==F & ensocategories$nina==T] <- 'nina'
ensocategories$category_nino34[ensocategories$nino==F & ensocategories$nina==F] <- 'neutral'


# SOI index ----
soi = read.csv('data/SOI_long_1866_2020.csv')

# adjust year for fall, filter important months
soi_long <- dplyr::filter(soi, month %in% c(1,2,3,9,10,11,12)) 
soi_long$year[soi_long$month %in% c(9,10,11,12)] <- soi_long$year[soi_long$month %in% c(9,10,11,12)] +1

# loop through years and determine if it meets the criteria for nino/nina
soicategories = c()
for (selected_year in unique(soi_long$year)) {
  yearindex = dplyr::filter(soi_long, year==selected_year)
  
  # get number of months of consecutive nino or nina conditions
  yearindex$ninomonth = yearindex$soi < -.5
  yearindex$ninamonth = yearindex$soi > .5
  consecutivenina = rle(yearindex$ninamonth)$lengths[rle(yearindex$ninamonth)$values==T]
  consecutivenino = rle(yearindex$ninomonth)$lengths[rle(yearindex$ninomonth)$values==T]
  
  if (max(consecutivenina)>=5) {nina=T} else {nina=F}
  if (max(consecutivenino)>=5) {nino=T} else {nino=F}
  soicategories = rbind(soicategories, data.frame(year=selected_year, nino=nino, nina=nina))
}

# designate years as el nino, la nina, or neutral
soicategories$category_soi = NA
soicategories$category_soi[soicategories$nino==T & soicategories$nina==F] <- 'nino'
soicategories$category_soi[soicategories$nino==F & soicategories$nina==T] <- 'nina'
soicategories$category_soi[soicategories$nino==F & soicategories$nina==F] <- 'neutral'


# write to csv ----
finalenso = merge(ensocategories, soicategories, by='year', all=T) %>%
  dplyr::select(year, category_nino34, category_soi) %>% arrange(year)

write.csv(finalenso, 'data/ENSO_phases_byyear.csv', row.names=F)
