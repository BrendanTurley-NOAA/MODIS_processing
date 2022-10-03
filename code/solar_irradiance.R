library(ncdf4)

### get solar_irradiance values fron netcdf file
# solar_irradiance units: W m^-2 um^-1
# nlw units: W m^-2 um^-1 sr^-1
# coastWatch nLw units: mW cm^-2 um^-1 sr^-1
# RRS units: sr^-1
# see this reference: https://oceancolor.gsfc.nasa.gov/docs/rsr/rsr_tables/
# The relationship between Rrs and nLw is:
# Rrs_nnn = nLw_nnn / F0_nnn* where nnn is the wavelength/band and F0 is the solar_irradiance attribute value for each band.
# *F0_nnn in our use already includes pi

### set working directory
setwd('~/Desktop/professional/projects/Postdoc_FL/hab_index/gpt_process/MODIS_test')
data <- nc_open('AQUA_MODIS.20030108T180001.L2.OC.x.nc')
wvlth <- substr(attributes(data$var)$names[grep('rrs',attributes(data$var)$names,ignore.case = T)],18,24)
bands <- attributes(data$var)$names[grep('rrs',attributes(data$var)$names,ignore.case = T)]
f0 <- rep(NA,10)
for(i in 1:10){
  tmp <- ncatt_get(data,bands[i],'solar_irradiance')
  
  print(ncatt_get(data,bands[i]))
  f0[i] <- as.double(tmp$value)
}

names(f0) <- wvlth

### convert F0 from W m^-2 um^-1 (given by NASA) to mW cm^-2 um^-1 (used by Coastwatch and Dr. Hu)
# watt to mW => 1000*W
# m^-2 to cm^-1 => 1/10000*m^-2
convert <- 1000/10000

f0_convert <- as.data.frame(f0*convert)
colnames(f0_convert) <- c('F0(mW.cm^-2.um^-1)')

write.csv(f0_convert,'f0.csv')
