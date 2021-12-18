### should I average Terra and Aqua together?

library(lubridate)
library(ncdf4)
library(rgdal)


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
url2 <- 'https://oceandata.sci.gsfc.nasa.gov:443/opendap/MODIST/L3SMI/2021/001/T20210012021031.L3m_MO_CHL_chlor_a_4km.nc'

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
  nc_close(modis)
  nflh_mth[i,,] <- nflh
}

hist(nflh_mth)
range(nflh_mth,na.rm=T)
hist(log10(nflh_mth))
range(log10(nflh_mth),na.rm=T)
lnflh <- log10(nflh_mth)
lnflh[which(lnflh<(-2.5))] <- NA
lnflh[which(lnflh>0)] <- NA
breaks <- seq(-2.5,0,.2)
# breaks <- quantile(nflh_mth,seq(0,1,.05),na.rm=T)
col_fx <- colorRampPalette(c('lightyellow1','khaki2','darkgoldenrod3','firebrick4'))#,'tan4'))
cols <- col_fx(length(breaks)-1)

### Hu/Chagaris method for red tide detection
# nflh_mth[which(nflh_mth<0.02)] <- NA

for(i in 1:12){
  # temp <- nflh_mth[i,,]
  temp <- lnflh[i,,]
  image(lon2,
        rev(lat2),
        temp[,ncol(temp):1],
        asp=1,breaks=breaks,col=cols)
  mtext(paste(yr,month.abb[i],sep='-'))
}

### red band difference
### Ruhul Amin, Jing Zhou, Alex Gilerson, Barry Gross, Fred Moshary, and Samir Ahmed, "Novel optical techniques for detecting and classifying toxic dinoflagellate Karenia brevis blooms using satellite imagery," Opt. Express 17, 9126-9144 (2009)
# nLw678 - nLw667
### Karenia brevis bloom index (KBBI)
# (nLw678 - nLw667)/(nLw678 + nLw667)
