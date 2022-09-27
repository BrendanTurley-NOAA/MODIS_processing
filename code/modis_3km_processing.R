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
### this one does
wr <- rast(world_r)
plot(wr)
wr2 <- project(wr,'+proj=utm +zone=17 +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0')
plot(wr2)
writeRaster(wr2, "output.tif", overwrite=TRUE)


### reference date and julian days
yr <- 2021
last <- ifelse(leap_year(paste0(yr,'-01-01')),366,365)

dates <- data.frame(date=ymd(seq(as.Date(paste0(yr,'-01-01')),as.Date(paste0(yr,'-12-31')),'day')),
                    yday=yday(ymd(seq(as.Date(paste0(yr,'-01-01')),as.Date(paste0(yr,'-12-31')),'day'))))
start <- dates$yday[which(day(dates$date)==1)]
stop <- c(dates$yday[which(day(dates$date)==1)-1],last)
index <- 1
parm <- 'CHL_chlor_a'
# parm <- 'POC_poc'
# parm <- 'FLH_nflh'
# parm <- 'RRS_Rrs_667'
# parm <- 'RRS_Rrs_678'
chl_yday <- array(NA,c(length(index),156,150))

system.time(
  for(i in 1:length(index)){
    url <- paste0('https://oceandata.sci.gsfc.nasa.gov:443/opendap/MODISA/L3SMI/',
                  yr,
                  '/',
                  sprintf("%03d",index[i]),
                  '/A',
                  yr,
                  sprintf("%03d",index[i]),
                  '.L3m_DAY_',
                  parm,
                  '_4km.nc')
    modis <- nc_open(url)
    chl_a <- ncvar_get(modis, 'chlor_a',start=c(2233,1423),count=c(156,150))
    if(i==1){
      lon2 <- ncvar_get(modis, 'lon',start=2233,count=156)
      lat2 <- ncvar_get(modis, 'lat',start=1423,count=150)  
    }
    nc_close(modis)
    chl_yday[i,,] <- chl_a
  }
)

image(log10(apply(chl_yday,c(2,3),mean,na.rm=T)),asp=1)


### create input structure
chl_1 <- list()
chl_1$x <- lon2
chl_1$y <- rev(lat2)
chl_1$z <- apply(chl_yday,c(2,3),mean,na.rm=T)
# chl_1$z <- (chl_yday[,,dim(chl_yday)[3]:1])
image(chl_1,asp=1)
### create raster
r <-raster(t(chl_1$z),
           xmn=min(chl_1$x), xmx=max(chl_1$x),
           ymn=min(chl_1$y), ymx=max(chl_1$y),
           crs=CRS("+proj=longlat +datum=WGS84 +no_defs")
)
plot(r)
### set CRS in kilometers
r2 <- projectRaster(r,crs='+proj=utm +zone=17 +datum=NAD83 +units=km')
crs(r2)
plot(r2)
### histogram stretch
r3 <- stretch(r2,histeq=T)
plot(r3)



chl_anom <- array(NA,c(length(75:365),length(lon2),length(lat2)))
for(i in 1:length(lon2)){
  for(j in 1:length(lat2)){
    n <- 1
    for(k in 75:365){
      ### 15 day lagged anomaly of 60 day mean
      lm_60d <- mean(chl_yday[(k-74):(k-74+59),i,j],na.rm=T)
      chl_anom[n,i,j] <- chl_yday[k,i,j] - lm_60d
      n <- n + 1
    }
  }
}

image(apply(chl_anom,c(2,3),mean,na.rm=T),asp=1,breaks=c(-100,0,1,100),col=c(1,3,2))
