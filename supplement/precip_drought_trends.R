#'  Look at precip and drought trends at Jornada Headquarters
#'  
#'  
#'  Results:
#'    PDSI -- negative trend almost significant ( p = 0.055) 1979-2017; no trend 1915-1978
#'    SPEI -- negative trend significant 1979-2017; no trend 1915-1978
#'    annual precip -- negative trend significant 1979-2017; no trend 1915-1978
#'    summer precip -- no trend either period
#'    winter precip -- negative significant trend 1979-2017; no trend 1915-1978
#'  
#'  
#'  Adapted from Darren's code (HQ temp data.R)
#'  4/7/22

library("tidyverse") 
library(ggpmisc)


# read in gap-filled climate data (created in get_climate_data.R) and select columns
#     restrict to period 1916-2016
#     create column to denote period before/after 1979
mean_annual_climate = read.csv('data/climate_variables.csv') %>%
  dplyr::select(year=water_yr, mean_temp, pdsi, spei, yearly_ppt_mm, summer_ppt_mm, winter_ppt_mm, nino34) %>%
  dplyr::filter(year %in% 1916:2017) %>%
  mutate(period = if_else(year < 1979, "1916-1978", "1979-2017"))



# ========================================
# PDSI ----

my.formula <- y ~ x
pdsi = ggplot(mean_annual_climate, aes(x = year, y = pdsi, col = period)) +
  geom_point() + 
  geom_line() +
  xlab('') +
  geom_smooth(method = lm, se = FALSE) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  scale_x_continuous(limits = c(1913, 2018), breaks = seq(1915, 2020, 5), expand = c(0,0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  #ggtitle("JER HQ data: annual average Temperature",
  #        subtitle = "with linear regression trends") +
  ylab("Annual average PDSI") 
#ggsave("Figures/JER HQ annual average T regression.jpg", height = 4, width = 6, units = "in", dpi = 300)

# Check regressions
mean_annual_climate %>%
  dplyr::filter(period == "1979-2017") %>%
  lm(pdsi ~ year, data = .) %>%
  summary()

mean_annual_climate %>%
  dplyr::filter(period == "1916-1978") %>%
  lm(pdsi ~ year, data = .) %>%
  summary()

# ========================================
# SPEI ----

my.formula <- y ~ x
spei = ggplot(mean_annual_climate, aes(x = year, y = spei, col = period)) +
  geom_point() + 
  geom_line() +
  xlab('') +
  geom_smooth(method = lm, se = FALSE) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  scale_x_continuous(limits = c(1913, 2018), breaks = seq(1915, 2020, 5), expand = c(0,0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ylab("Annual average SPEI") 
#ggsave("Figures/JER HQ annual average T regression.jpg", height = 4, width = 6, units = "in", dpi = 300)

# Check regressions
mean_annual_climate %>%
  dplyr::filter(period == "1979-2017") %>%
  lm(spei ~ year, data = .) %>%
  summary()

mean_annual_climate %>%
  dplyr::filter(period == "1916-1978") %>%
  lm(spei ~ year, data = .) %>%
  summary()

# ========================================
# annual precip ----

my.formula <- y ~ x
annual_precip = ggplot(mean_annual_climate, aes(x = year, y = yearly_ppt_mm, col = period)) +
  geom_point() + 
  geom_line() +
  xlab('') +
  geom_smooth(method = lm, se = FALSE) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  scale_x_continuous(limits = c(1913, 2018), breaks = seq(1915, 2020, 5), expand = c(0,0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ylab("Annual average total precip (mm)") 
#ggsave("Figures/JER HQ annual average T regression.jpg", height = 4, width = 6, units = "in", dpi = 300)

# Check regressions
mean_annual_climate %>%
  dplyr::filter(period == "1979-2017") %>%
  lm(yearly_ppt_mm ~ year, data = .) %>%
  summary()

mean_annual_climate %>%
  dplyr::filter(period == "1916-1978") %>%
  lm(yearly_ppt_mm ~ year, data = .) %>%
  summary()

# ========================================
# summer precip ----

my.formula <- y ~ x
summer_precip = ggplot(mean_annual_climate, aes(x = year, y = summer_ppt_mm, col = period)) +
  geom_point() + 
  geom_line() +
  xlab('') +
  geom_smooth(method = lm, se = FALSE) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  scale_x_continuous(limits = c(1913, 2018), breaks = seq(1915, 2020, 5), expand = c(0,0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ylab("Annual average summer precip (mm)") 
#ggsave("Figures/JER HQ annual average T regression.jpg", height = 4, width = 6, units = "in", dpi = 300)

# Check regressions
mean_annual_climate %>%
  dplyr::filter(period == "1979-2017") %>%
  lm(summer_ppt_mm ~ year, data = .) %>%
  summary()

mean_annual_climate %>%
  dplyr::filter(period == "1916-1978") %>%
  lm(summer_ppt_mm ~ year, data = .) %>%
  summary()

# ========================================
# winter precip ----

my.formula <- y ~ x
winter_precip = ggplot(mean_annual_climate, aes(x = year, y = winter_ppt_mm, col = period)) +
  geom_point() + 
  geom_line() +
  xlab('') +
  geom_smooth(method = lm, se = FALSE) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  scale_x_continuous(limits = c(1913, 2018), breaks = seq(1915, 2020, 5), expand = c(0,0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ylab("Annual average winter precip (mm)") 
#ggsave("Figures/JER HQ annual average T regression.jpg", height = 4, width = 6, units = "in", dpi = 300)

# Check regressions
mean_annual_climate %>%
  dplyr::filter(period == "1979-2017") %>%
  lm(winter_ppt_mm ~ year, data = .) %>%
  summary()

mean_annual_climate %>%
  dplyr::filter(period == "1916-1978") %>%
  lm(winter_ppt_mm ~ year, data = .) %>%
  summary()

# ========================================
# ENSO----

my.formula <- y ~ x
enso = ggplot(mean_annual_climate, aes(x = year, y = nino34_index, col = period)) +
  geom_point() + 
  geom_line() +
  xlab('') +
  geom_smooth(method = lm, se = FALSE) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  scale_x_continuous(limits = c(1913, 2018), breaks = seq(1915, 2020, 5), expand = c(0,0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ylab("Nino 3.4 index") 
#ggsave("Figures/JER HQ annual average T regression.jpg", height = 4, width = 6, units = "in", dpi = 300)

# Check regressions
mean_annual_climate %>%
  dplyr::filter(period == "1979-2017") %>%
  lm(winter_ppt_mm ~ year, data = .) %>%
  summary()

mean_annual_climate %>%
  dplyr::filter(period == "1916-1978") %>%
  lm(winter_ppt_mm ~ year, data = .) %>%
  summary()


# ===============================================
# multifigure plot

multiplot = gridExtra::grid.arrange(pdsi, spei, summer_precip, winter_precip, nrow=2)
multiplot
