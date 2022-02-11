#'  Look at temperature trends at Jornada Headquarters
#'  
#'  By Darren James
#'  2/11/22

library("tidyverse") 


# read in gap-filled climate data (created in get_climate_data.R) and select columns
#     restrict to period 1916-2016
#     create column to denote period before/after 1979
mean_annual_temp = read.csv('data/climate_variables.csv') %>%
  dplyr::select(year=water_yr, mean_temp) %>%
  dplyr::filter(year %in% 1916:2017) %>%
  mutate(period = if_else(year < 1979, "1916-1978", "1979-2017"))




library(ggpmisc)
my.formula <- y ~ x
ggplot(mean_annual_temp, aes(x = year, y = mean_temp, col = period)) +
  geom_point() + 
  geom_line() +
  xlab('') +
  geom_smooth(method = lm, se = FALSE) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  scale_x_continuous(limits = c(1913, 2018), breaks = seq(1915, 2015, 5), expand = c(0,0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  #ggtitle("JER HQ data: annual average Temperature",
  #        subtitle = "with linear regression trends") +
  ylab("Annual average Temperature (degrees C)") 
ggsave("Figures/JER HQ annual average T regression.jpg", height = 4, width = 6, units = "in", dpi = 300)
  
# Check regressions
mean_annual_temp %>%
  dplyr::filter(period == "1979-2017") %>%
  lm(mean_temp ~ year, data = .) %>%
  summary()

mean_annual_temp %>%
  dplyr::filter(period == "1916-1978") %>%
  lm(mean_temp ~ year, data = .) %>%
  summary()
