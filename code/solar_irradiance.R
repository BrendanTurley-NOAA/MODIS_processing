### get solar_irradiance values fron netcdf file
# solar_irradiance units: W/m^2/um/sr
# see this reference: https://oceancolor.gsfc.nasa.gov/docs/rsr/rsr_tables/
# The relationship between Rrs and nLw is:
# Rrs_nnn = nLw_nnn / F0_nnn* where nnn is the wavelength/band and F0 is the solar_irradiance attribute value for each band.
# *F0_nnn in our use already includes pi

data <- nc_open('A2005217182500.L2_LAC_OC.nc')
wvlth <- substr(attributes(data$var)$names[grep('rrs',attributes(data$var)$names,ignore.case = T)],18,24)
bands <- attributes(data$var)$names[grep('rrs',attributes(data$var)$names,ignore.case = T)]
f0 <- rep(NA,10)
for(i in 1:10){
  tmp <- ncatt_get(data,bands[i],'solar_irradiance')
  
  print(ncatt_get(data,bands[i]))
  f0[i] <- tmp$value
}

names(f0) <- wvlth
