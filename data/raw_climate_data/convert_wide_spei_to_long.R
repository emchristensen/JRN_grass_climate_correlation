library(dplyr)
library(ggplot2)

spei = read.csv('data/raw_climate_data/SPEI_DonaAna_1895_2021_wide.csv')

spei_long = tidyr::pivot_longer(spei, cols=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'),
                                names_to='monthname', values_to='spei')
spei_long$month = NA
spei_long$month[spei_long$monthname=='Jan'] <- 1
spei_long$month[spei_long$monthname=='Feb'] <- 2
spei_long$month[spei_long$monthname=='Mar'] <- 3
spei_long$month[spei_long$monthname=='Apr'] <- 4
spei_long$month[spei_long$monthname=='May'] <- 5
spei_long$month[spei_long$monthname=='Jun'] <- 6
spei_long$month[spei_long$monthname=='Jul'] <- 7
spei_long$month[spei_long$monthname=='Aug'] <- 8
spei_long$month[spei_long$monthname=='Sep'] <- 9
spei_long$month[spei_long$monthname=='Oct'] <- 10
spei_long$month[spei_long$monthname=='Nov'] <- 11
spei_long$month[spei_long$monthname=='Dec'] <- 12

final = dplyr::select(spei_long, year=Year, month, spei)

write.csv(final, 'data/SPEI_DonaAna_1895_2021.csv')

yearly = final %>%
  group_by(year) %>%
  summarize(meanspei = mean(spei, na.rm=T))

ggplot(yearly, aes(x=year, y=meanspei)) +
  geom_line()
