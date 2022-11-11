library(lubridate)
library(ncdf4)
library(raster)
library(rgdal)
library(terra)

### load map
setwd("~/Desktop/professional/biblioteca/data/shapefiles/gshhg-shp-2.3.7/GSHHS_shp/h/")
world <- readOGR('GSHHS_h_L1.shp')
world <- crop(world, extent(-87, -79, 24, 31))
w1 <- raster(ncol=400, nrow=400)
extent(w1) <- extent(world)
world_r <- rasterize(world,w1)
world_r@data@values[which(world_r@data@values>1)] <- 1
plot(world_r)
### this reprojection does not work properly
world_r2 <- projectRaster(world_r,crs='+proj=utm +zone=17 +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0')
plot(world_r2)
world_r2@data@values
### this one does
wr <- rast(world_r)
plot(wr)
wr2 <- project(wr,'+proj=utm +zone=17 +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0')
plot(wr2)
writeRaster(wr2, "output.tif", overwrite=TRUE)
