### https://gis.stackexchange.com/questions/120900/plotting-netcdf-file-using-lat-and-lon-contained-in-variables



library(ncdf4)
library(rgdal)
library(sp)
library(raster)

setwd("~/Desktop/professional/biblioteca/data/shapefiles/ne_10m_admin_0_countries")
world <- readOGR('ne_10m_admin_0_countries.shp')

setwd('~/Desktop/professional/projects/Postdoc_FL/hab_index/modis')
modis_vars <- nc_open('A2011088184000.L2_LAC_OC.nc')
attributes(modis_vars$var)

chl <- ncvar_get(modis_vars, 'geophysical_data/chl_ocx')
lon <- ncvar_get(modis_vars, 'navigation_data/longitude')
lat <- ncvar_get(modis_vars, 'navigation_data/latitude')
chl <- raster(chl)
lon <- raster(lon)
lat <- raster(lat)
pchl <- rasterToPoints(chl)
plat <- rasterToPoints(lat)
plon <- rasterToPoints(lon)
lonlat <- cbind(plon[,3], plat[,3])
lonlat <- SpatialPoints(lonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))

pchl <- getValues(chl)
test <- data.frame(plon[,3], plat[,3], pchl)
names(test) <- c('lon','lat','chl')
coordinates(test) <- lon + lat
proj4string(test) <- "+proj=longlat +datum=WGS84"
# coordinates(chl) <- coordinates(lonlat)
mycrs <- crs(lonlat)
projection(chl) <- mycrs
extent(chl) <- extent(lonlat)

plot(world,xlim=c(-100,-65),ylim=c(20,40))
plot(chl,add=T)

nc_close(modis_vars)


modis <- raster('A2011088184000.L2_LAC_OC.nc',varname='geophysical_data/chl_ocx')
lat <- raster('A2011088184000.L2_LAC_OC.nc',varname='navigation_data/latitude')
lon <- raster('A2011088184000.L2_LAC_OC.nc',varname='navigation_data/longitude')
plat <- rasterToPoints(lat)
plon <- rasterToPoints(lon)
lonlat <- cbind(plon[,3], plat[,3])
# plat <- getValues(lat)
# plon <- getValues(lon)
# lonlat <- cbind(plon, plat)
lonlat <- SpatialPoints(lonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))
mycrs <- crs(lonlat)
# Fix the projection and extent
projection(modis) <- mycrs
extent(modis) <- extent(lonlat)
plot(modis)
# Take a look
new <- getValues(modis)
new1 <- matrix(new,1354,2030)
image(new1)
new1 <- raster(new1)
projection(new1) <- mycrs
extent(new1) <- extent(lonlat)
plot(new1)


plot(world)
plot(modis,add=T)

# Project to long lat grid
r <- projectRaster(pr, crs=CRS("+proj=longlat +datum=WGS84"))
# Take a look
r
plot(r)

new_modis <- stack(modis,lon,lat)

plot(new_modis)


plot(modis)

crs(modis)
crs(modis) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'


plot(world)
plot(modis,add=T)
