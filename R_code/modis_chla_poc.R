### should I average Terra and Aqua together?
library(fields)
library(lubridate)
library(ncdf4)
library(rgdal)

setwd("~/Desktop/professional/biblioteca/data")
bathy <- nc_open('etopo1.nc')
topo <- ncvar_get(bathy, 'Band1')
topo_lat <- ncvar_get(bathy, 'lat')
topo_lon <- ncvar_get(bathy, 'lon')
nc_close(bathy)

setwd("~/Desktop/professional/biblioteca/data/shapefiles/ne_10m_admin_0_countries")
world <- readOGR('ne_10m_admin_0_countries.shp')


### reference date and julian days
yr <- 2018
last <- ifelse(leap_year(paste0(yr,'-01-01')),366,365)

dates <- data.frame(date=ymd(seq(as.Date(paste0(yr,'-01-01')),as.Date(paste0(yr,'-12-31')),'day')),
                    yday=yday(ymd(seq(as.Date(paste0(yr,'-01-01')),as.Date(paste0(yr,'-12-31')),'day'))))
start <- dates$yday[which(day(dates$date)==1)]
stop <- c(dates$yday[which(day(dates$date)==1)-1],last)
parm1 <- 'CHL_chlor_a'
parm2 <- 'POC_poc'
parm <- 'LAND_ndvi'


chl_a_mth <- array(NA,c(12,156,150))
poc_mth <- array(NA,c(12,156,150))
for(i in 1:length(stop)){
  url <- paste0('https://oceandata.sci.gsfc.nasa.gov:443/opendap/MODISA/L3SMI/',
                yr,
                '/',
                sprintf("%03d",start[i]),
                '/A',
                yr,
                sprintf("%03d",start[i]),
                yr,
                sprintf("%03d",stop[i]),
                '.L3m_MO_',
                parm1,
                '_4km.nc')
  modis <- nc_open(url)
  chl_a <- ncvar_get(modis, 'chlor_a',start=c(2233,1423),count=c(156,150))
  lon2 <- ncvar_get(modis, 'lon',start=2233,count=156)
  lat2 <- ncvar_get(modis, 'lat',start=1423,count=150)
  nc_close(modis)
  chl_a_mth[i,,] <- chl_a
  
  url <- paste0('https://oceandata.sci.gsfc.nasa.gov:443/opendap/MODISA/L3SMI/',
                yr,
                '/',
                sprintf("%03d",start[i]),
                '/A',
                yr,
                sprintf("%03d",start[i]),
                yr,
                sprintf("%03d",stop[i]),
                '.L3m_MO_',
                parm2,
                '_4km.nc')
  modis <- nc_open(url)
  poc <- ncvar_get(modis, 'poc',start=c(2233,1423),count=c(156,150))
  lon2 <- ncvar_get(modis, 'lon',start=2233,count=156)
  lat2 <- ncvar_get(modis, 'lat',start=1423,count=150)
  nc_close(modis)
  poc_mth[i,,] <- poc
}

par(mfrow=c(1,2))
for(i in 1:12){
  chl_a_mth2 <- chl_a_mth[i,,]
  poc_mth2 <- poc_mth[i,,]
  
  imagePlot(lon2,
            rev(lat2),
            log10(chl_a_mth2[,ncol(chl_a_mth2):1]),
            asp=1)
  imagePlot(lon2,
            rev(lat2),
            log10(poc_mth2[,ncol(poc_mth2):1]),
            asp=1)
  
  # imagePlot(lon2,
  #           rev(lat2),
  #           (poc_mth2[,ncol(poc_mth2):1])/(chl_a_mth2[,ncol(chl_a_mth2):1]),
  #           asp=1) 
  
  mtext(paste(yr,month.abb[i],sep='-'))
  # plot(world,add=T,col='gray80')
}


parm <- 'LAND_ndvi'
ndvi_mth <- array(NA,c(12,156,150))
for(i in 1:length(stop)){
url <- paste0('https://oceandata.sci.gsfc.nasa.gov:443/opendap/MODISA/L3SMI/',
              yr,
              '/',
              sprintf("%03d",start[i]),
              '/A',
              yr,
              sprintf("%03d",start[i]),
              yr,
              sprintf("%03d",stop[i]),
              '.L3m_MO_',
              parm,
              '_4km.nc')
modis <- nc_open(url)
ndvi <- ncvar_get(modis, 'ndvi',start=c(2233,1423),count=c(156,150))
lon2 <- ncvar_get(modis, 'lon',start=2233,count=156)
lat2 <- ncvar_get(modis, 'lat',start=1423,count=150)
nc_close(modis)
ndvi_mth[i,,] <- ndvi
}

brks <- seq(-2,1,.1)
cols1 <- colorRampPalette(c('gray20','gray90'))
cols2 <- colorRampPalette(c('gold','green','forestgreen'))
cols3 <- c(cols1(length(which(brks<0))),
           cols2(length(which(brks>=0))-1))
for(i in 1:12){
  ndvi <- ndvi_mth[i,,]
  imagePlot(lon2,
            rev(lat2),
            ndvi[,ncol(ndvi):1],
            asp=1,col=cols3,breaks = brks)
  mtext(paste(yr,month.abb[i],sep='-'))
  # plot(world,add=T,col='gray80')
}



url <- 'http://opendap.cr.usgs.gov/opendap/hyrax/MOD13C2.061/MOD13C2.061.ncml'
modis <- nc_open(url)
