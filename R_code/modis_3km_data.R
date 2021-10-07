library(ncdf4)
library(rgdal)

setwd("~/Desktop/professional/biblioteca/data/shapefiles/ne_10m_admin_0_countries")
world <- readOGR('ne_10m_admin_0_countries.shp')

url <- 'https://oceandata.sci.gsfc.nasa.gov:443/opendap/MODISA/L3SMI/2003/001/A2003001.L3m_DAY_CHL_chl_ocx_4km.nc'
url <- 'https://oceandata.sci.gsfc.nasa.gov:443/opendap/MODISA/L3SMI/2021/001/A2021001.L3m_DAY_CHL_chl_ocx_4km.nc'

modis <- nc_open(url)
attributes(modis$var)
chl_a <- ncvar_get(modis, 'chl_ocx')
latm <- ncvar_get(modis, 'lat')
lonm <- ncvar_get(modis, 'lon')
nc_close(modis)

lonbox_e <- -81.5 ### Florida Bay
lonbox_w <- -87 ### mouth of Mississippi River
latbox_n <- 30.5 ### northern coast
latbox_s <- 24.3 ### southern edge of Ket West

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
