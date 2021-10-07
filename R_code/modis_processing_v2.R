library(akima)
library(raster)
library(ncdf4)
library(terra)
library(fields)

### loading bathymetry
setwd('~/Desktop/professional/projects/Postdoc_FL/hab_index/modis/test')
files <- 'A2005217182000.L2_LAC_OC.nc'
modis <- nc_open(files)
attributes(modis$var)
chl_a <- ncvar_get(modis, 'geophysical_data/chlor_a')
latm <- ncvar_get(modis, 'navigation_data/latitude')
lonm <- ncvar_get(modis, 'navigation_data/longitude')
nc_close(modis)

image(chl_a)
image(lonm)
image(latm)


chl_ar <- raster(chl_a)


lonmr <- raster(lonm)

xyz <- data.frame(lon=as.vector(lonm),
                  lat=as.vector(latm),
                  chl_a=as.vector(chl_a))
xyz <- as.matrix.data.frame(xyz)

modis_rast <- rast(x=xyz, type = "xyz", crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

qp <- quilt.plot(as.vector(lonm),
           as.vector(latm),
           log(as.vector(chl_a),base=10),
           nx = 2030, ny = 1354)

xyz <- rast(files)

plot(xyz$chlor_a)

which(is.na)
modis_int <- interp(as.vector(lonm),
       as.vector(latm),
       as.vector(chl_a))

system('cd')
system('ls')
system('gpt')
system2('gpt')

# https://bash-intro.rsquaredacademy.com/index.html



