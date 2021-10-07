setwd('~/Downloads')

library(ncdf4)

modis <- nc_open('A2011088184000.L2_LAC_OC.nc')

attributes(modis$var)


lon <- ncvar_get(modis, 'navigation_data/longitude')
lat <- ncvar_get(modis, 'navigation_data/latitude')
chl <- ncvar_get(modis, 'geophysical_data/chlor_a')

F0 <- ncvar_get(modis, 'sensor_band_parameters/F0')



nc_close(modis)