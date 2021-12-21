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

### will use chlor_a not chl_ocx
url <- 'https://oceandata.sci.gsfc.nasa.gov:443/opendap/MODISA/L3SMI/2021/001/A2021001.L3m_DAY_CHL_chlor_a_4km.nc'
url <- 'https://oceandata.sci.gsfc.nasa.gov:443/opendap/MODISA/L3SMI/2021/001/A20210012021031.L3m_MO_CHL_chlor_a_4km.nc'

modis <- nc_open(url)
attributes(modis$var)
chl_a <- ncvar_get(modis, 'chlor_a')
lonm <- ncvar_get(modis, 'lon')
latm <- ncvar_get(modis, 'lat')
nc_close(modis)

lonbox_e <- -80.5 ### Florida Bay
lonbox_w <- -87 ### mouth of Mississippi River
latbox_n <- 30.75 ### northern coast
latbox_s <- 24.5 ### southern edge of Key West

ind1 <- which(lonm<lonbox_e & lonm>lonbox_w)
ind2 <- which(latm<latbox_n & latm>latbox_s)
lon <- lonm[ind1]
lat <- latm[ind2]
chl <- chl_a[ind1,ind2]

setwd("~/Desktop/professional/projects/Postdoc_FL/hab_index")
# {
#   png('chl_test.png',width=10,height=7,units='in',res=300)
#   image(lonm,
#         rev(latm),
#         log10(chl_a[,ncol(chl_a):1]))
#   plot(world,add=T)
#   dev.off()
# }


setwd("~/Desktop/professional/projects/Postdoc_FL/hab_index")
{
  png('chl_test.png',width=10,height=7,units='in',res=300)
  image(lon,
        rev(lat),
        log10(chl[,ncol(chl):1]),
        asp=1)
  plot(world,add=T)
  dev.off()
}


url2 <- 'https://oceandata.sci.gsfc.nasa.gov:443/opendap/MODISA/L3SMI/2021/001/A20210012021031.L3m_MO_CHL_chlor_a_4km.nc'

modis <- nc_open(url2)
attributes(modis$var)
chl_a <- ncvar_get(modis, 'chlor_a',start=c(2233,1423),count=c(156,150))
lon2 <- ncvar_get(modis, 'lon',start=2233,count=156)
lat2 <- ncvar_get(modis, 'lat',start=1423,count=150)
nc_close(modis)

hist(log10(chl_a))

image(lon2,
      rev(lat2),
      log10(chl_a[,ncol(chl_a):1]),asp=1)
plot(world,add=T)



url <- 'https://oceandata.sci.gsfc.nasa.gov:443/opendap/MODISA/L3SMI/2021/001/A20210012021031.L3m_MO_FLH_nflh_4km.nc'

modis <- nc_open(url)
attributes(modis$var)
nflh <- ncvar_get(modis, 'nflh',start=c(2233,1423),count=c(156,150))
lon2 <- ncvar_get(modis, 'lon',start=2233,count=156)
lat2 <- ncvar_get(modis, 'lat',start=1423,count=150)
nc_close(modis)

image(lon2,
      rev(lat2),
      nflh[,ncol(nflh):1],asp=1)
plot(world,add=T)


### reference date and julian days
yr <- 2021
last <- ifelse(leap_year(paste0(yr,'-01-01')),366,365)

dates <- data.frame(date=ymd(seq(as.Date(paste0(yr,'-01-01')),as.Date(paste0(yr,'-12-31')),'day')),
                    yday=yday(ymd(seq(as.Date(paste0(yr,'-01-01')),as.Date(paste0(yr,'-12-31')),'day'))))
start <- dates$yday[which(day(dates$date)==1)]
stop <- c(dates$yday[which(day(dates$date)==1)-1],last)

nflh_mth <- array(NA,c(12,156,150))
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
                '.L3m_MO_FLH_nflh_4km.nc')
  modis <- nc_open(url)
  nflh <- ncvar_get(modis, 'nflh',start=c(2233,1423),count=c(156,150))
  lon2 <- ncvar_get(modis, 'lon',start=2233,count=156)
  lat2 <- ncvar_get(modis, 'lat',start=1423,count=150)
  nc_close(modis)
  nflh_mth[i,,] <- nflh
}

### log
hist(log10(nflh_mth))
range(log10(nflh_mth),na.rm=T)
lnflh <- log10(nflh_mth)
lnflh[which(lnflh<(-2.5))] <- NA
lnflh[which(lnflh>0)] <- NA
breaks <- seq(-2.5,0,.2)
col_fx <- colorRampPalette(c('lightyellow1','khaki2','darkgoldenrod3','firebrick4'))#,'tan4'))
cols <- col_fx(length(breaks)-1)
### linear
hist(nflh_mth)
quantile(nflh_mth,seq(0,1,.05),na.rm=T)
range(nflh_mth,na.rm=T)
nflh_mth[which(nflh_mth<0)] <- 0
nflh_mth[which(nflh_mth>.5)] <-.5
breaks <- seq(0,.5,.02)
# col_fx <- colorRampPalette(c('gray20','dodgerblue3','cadetblue1'))
col_fx <- colorRampPalette(c('gray20','dodgerblue4','indianred3','gold1'))
# col_fx <- colorRampPalette(c('gray20','tan4','indianred3','firebrick2','gold1'))
cols <- col_fx(length(breaks)-1)

### Hu/Chagaris method for red tide detection
### .02 mW cm^-2 um^-1 sr^-1
### NASA data W m^-2 um^-1 sr^-1
### conversion
cutoff <- (.02/1e3)*1e4

setwd("~/Desktop/professional/projects/Postdoc_FL/figures")
png("2021_habs_flh.png", height = 7, width = 7, units = 'in', res=300)
par(mfrow=c(2,2),mar=c(4.5,4,2,2))
for(i in 8:10){
  temp <- nflh_mth[i,,]
  # temp <- lnflh[i,,]
  # imagePlot(lon2,
  #           rev(lat2),
  #           temp[,ncol(temp):1],
  #           asp=1,breaks=breaks,col=cols,
  #           xlab='Longitude',ylab='Latitude')
  image(lon2,
        rev(lat2),
        temp[,ncol(temp):1],
        asp=1,breaks=breaks,col=cols,
        xlab='Longitude',ylab='Latitude')
  contour(lon2,
          rev(lat2),
          temp[,ncol(temp):1],
          level=cutoff,labels='',col='gray80',add=T)
  mtext(paste(yr,month.abb[i],sep='-'))
  plot(world,add=T,col='gray80')
  contour(topo_lon,
          topo_lat,
          topo,
          add=T,levels=c(-100),col='white')
}
dev.off()

setwd("~/Desktop/professional/projects/Postdoc_FL/figures")
png("2021_habs_flh_cb.png", height = 5, width = 1.25, units = 'in', res=300)
par(mar=c(1,1,1,4))
image(1:2,breaks[2:26],rbind(1:25,1:25),col=cols,xaxt='n',yaxt='n',xlab='',ylab='')
axis(4,breaks[seq(1,26,5)]+.01,breaks[seq(1,26,5)],las=2,cex.axis=1)
mtext(expression(paste('W m'^-2, mu, 'm'^-1,'sr'^-1)),4,line=2.5,cex=1)
### NASA data W m^-2 um^-1 sr^-1
dev.off()

### red band difference
### Ruhul Amin, Jing Zhou, Alex Gilerson, Barry Gross, Fred Moshary, and Samir Ahmed, "Novel optical techniques for detecting and classifying toxic dinoflagellate Karenia brevis blooms using satellite imagery," Opt. Express 17, 9126-9144 (2009)
# nLw678 - nLw667
### Karenia brevis bloom index (KBBI)
# (nLw678 - nLw667)/(nLw678 + nLw667)
