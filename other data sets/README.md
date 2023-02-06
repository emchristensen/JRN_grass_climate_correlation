# JRN_grass_climate_correlation/other data sets

Analysis of additional regional datasets: PDO effect on perennial grass.

## CDRRC
Chihuahuan Desert Rangeland Research Center
Data consists of annual forage production 1967-2018. 
Data contained in supplement of article: https://doi.org/10.1016/j.rama.2019.06.002 

## santa rita
Santa Rita Experimental Range

Transect data 1953-2021 (not every year has data)

Data downloaded from https://ag.arizona.edu/SRER/data.html

Data citation: 
McClaran, M. (2012): Santa Rita Experimental Range Long Term Transect Database. – In: Dengler, J., Oldeland, J., Jansen, F., Chytrý, M., Ewald, J., Finckh, M., Glöckler, F., Lopez-Gonzalez, G., Peet, R.K., Schaminée, J.H.J. [Eds.]: Vegetation databases for the 21st century. – Biodiversity & Ecology 4: 435–435. DOI: 10.7809/b-e.00222.
Download at https://cals.arizona.edu/srer/content/ongoing-vegetation-transect-data 
Script "sr_transect_data.R" reads in raw data and converts to yearly mean grass cover, saved as "pasture8_transects_mean.csv"

## SEV
Sevilleta National Wildlife Refuge

Transect data 1989-2019

Raw data downloaded from https://doi.org/10.6073/pasta/4d4b60db003d54fc2a989924d06028a9

Data file too large to store in repo

Script "sev_transect_data.R" reads raw data and summarizes as yearly perennial grass cover, saved as "SEV_transects_1989_2019.csv"
