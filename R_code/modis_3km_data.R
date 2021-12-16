library(lubridate)
library(ncdf4)
library(rgdal)

### reference date and julian days
dates <- data.frame(date=ymd(seq(as.Date('2021-01-01'),as.Date('2021-12-31'),'day')),
           yday=yday(ymd(seq(as.Date('2021-01-01'),as.Date('2021-12-31'),'day'))))
dates$yday[which(day(dates$date)==1)]
dates$yday[which(day(dates$date)==1)-1]


setwd("~/Desktop/professional/biblioteca/data/shapefiles/ne_10m_admin_0_countries")
world <- readOGR('ne_10m_admin_0_countries.shp')

### will use chlor_a not chl_ocx
url <- 'https://oceandata.sci.gsfc.nasa.gov:443/opendap/MODISA/L3SMI/2021/001/A2021001.L3m_DAY_CHL_chlor_a_4km.nc'

modis <- nc_open(url)
attributes(modis$var)
chl_a <- ncvar_get(modis, 'chl_ocx')
lonm <- ncvar_get(modis, 'lon')
latm <- ncvar_get(modis, 'lat')
nc_close(modis)

lonbox_e <- -80.5 ### Florida Bay
lonbox_w <- -87 ### mouth of Mississippi River
latbox_n <- 30.5 ### northern coast
latbox_s <- 24.3 ### southern edge of Key West

ind1 <- which(lonm<lonbox_e & lonm>lonbox_w)
ind2 <- which(latm<latbox_n & latm>latbox_s)
lon <- lonm[ind1]
lat <- latm[ind2]
chl <- chl_a[ind1,ind2]

setwd("~/Desktop/professional/projects/Postdoc_FL/hab_index")
{
  png('chl_test.png',width=10,height=7,units='in',res=300)
  image(lonm,
        rev(latm),
        log10(chl_a[,ncol(chl_a):1]))
  plot(world,add=T)
  dev.off()
}


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
chl_a <- ncvar_get(modis, 'chlor_a',start=c(2233,1429),count=c(156,149))
lon2 <- ncvar_get(modis, 'lon',start=2233,count=156)
lat2 <- ncvar_get(modis, 'lat',start=1429,count=149)
nc_close(modis)


image(lon2,
      rev(lat2),
      log10(chl_a[,ncol(chl_a):1]),asp=1)


### red band difference
### Ruhul Amin, Jing Zhou, Alex Gilerson, Barry Gross, Fred Moshary, and Samir Ahmed, "Novel optical techniques for detecting and classifying toxic dinoflagellate Karenia brevis blooms using satellite imagery," Opt. Express 17, 9126-9144 (2009)
# nLw678 - nLw667
### Karenia brevis bloom index (KBBI)
# (nLw678 - nLw667)/(nLw678 + nLw667)
