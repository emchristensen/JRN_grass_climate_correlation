#' Plot correlation between ENSO, PDO, and climate variables


library(tidyverse)
library(emmeans) # For testing differences between slopes

# read in climate and grass data
climatedata = read.csv('data/climate_variables.csv') 
names(climatedata)

# convert to long form
clim.num.long <- climatedata %>% 
  dplyr::select(where(is.numeric)) %>%
  mutate(period = if_else(water_yr < 1980, "1916-1979", "1995-2016")) %>%
  gather(variable, value, pdsi, spei, yearly_ppt_mm, summer_ppt_mm, summer_pdsi, summer_spei, summer_tmax, 
  summer_tmin, winter_ppt_mm, winter_pdsi, winter_spei, winter_tmax, winter_tmin)

names(clim.num.long)

# figure of regressions between PDO and other numeric variables
ggplot(clim.num.long, aes(x= value, y = pdo, col = period)) +
  geom_point(size = 0.5) +
  geom_smooth(method = lm, se = FALSE) +
  facet_wrap( ~ variable, ncol = 4, scales = "free") +
  xlab("Value of Environmental Variable") +
  ylab("PDO value") +
  #ggtitle("PDO vs. other numeric variables") +
  theme_bw() +
  theme(legend.position = "top")

ggsave("Figures/PDO and climate variables regressions.jpg", height = 7, width = 8, units = "in", dpi = 300)

# # figure of regressions between ENSO (nino34) and other numeric variables
# ggplot(clim.num.long, aes(x= value, y = nino34, col = period)) +
#   geom_point(size = 0.5) +
#   geom_smooth(method = lm, se = FALSE) +
#   facet_wrap( ~ variable, ncol = 4, scales = "free") +
#   theme(legend.position = "top") +
#   xlab("Value of Environmental Variable") +
#   ylab("NINO34 (ENSO) value") +
#   ggtitle("ENSO (nino34) vs. other numeric variables") +
#   theme_bw()
# 
# ggsave("Figures/ENSO and climate variables regressions.jpg", height = 7, width = 8, units = "in", dpi = 300)


# Run regressions and test for different slopes between the two periods----

# Create data.frame for running regressions
clim.num.reg <- clim.num.long %>%
  rename(index_value = pdo) %>%
  mutate(index = "PDO") %>%
  dplyr::select(-nino34) %>% 
  
  bind_rows(
    clim.num.long %>%
      rename(index_value = nino34) %>%
      mutate(index = "ENSO (nino34)") %>%
      dplyr::select(-pdo)
  )

# Loop over the indices and variables
indices <- unique(clim.num.reg$index)
variables <- unique(clim.num.reg$variable)

slopes.df <- data.frame()
slopeDiffs.df <- data.frame()

for(i in 1:length(indices)) {
for(j in 1:length(variables)) {
  
  data.subset <- clim.num.reg %>%
    dplyr::filter(index == indices[i] & variable == variables[j])
  
  reg.lm <- lm(index_value ~ value*period, data = data.subset)
  
  m.lst <- lstrends(reg.lm, "period", var = "value")
  
  slope.collect <- m.lst %>% as_tibble() %>%
    mutate(index = indices[i],
           variable = variables[j]) %>%
    dplyr::select(index, variable, period, slope = value.trend, SE, df, lower.CL, upper.CL)
  
  slopeTest.collect <- pairs(m.lst) %>%
    as_tibble() %>%
    mutate(index = indices[i],
           variable = variables[j]) %>%
    dplyr::select(index, variable, contrast, estimate, SE, df, t.ratio, p.value)
  
  slopes.df <- rbind(slopes.df, slope.collect)
  
  slopeDiffs.df <- rbind(slopeDiffs.df, slopeTest.collect)
  }
}

# Find cases where trends is significantly different between the two time periods
slopeDiffs.df %>%
  dplyr::filter(p.value < 0.05) 

# Only happens with ENSO (never with PDO); occurs with yearly_ppt_mm and summer_ppt_mm



