library(fields)
library(ncdf4)

### 1) download daily 4km Aqua data
# a) what bounding box
# (-87.5 30.7, -81, 24.2)
lonbox_w <- -87.5 ### mouth of Mississippi River
latbox_n <- 30.7 ### northern coast
lonbox_e <- -81 ### Florida Bay
latbox_s <- 24.2 ### southern edge of Ket West

# b) which parameters
# chlor_a
# nflh
# rrs_443
# rrs_488
# rrs_531
# rrs_547
# rrs_555
# rrs_667
# rrs_678

# c) what derived parameters
# ABI - rrs_547 & nflh
# bbp_Morel - chlor_a
# bbp_Carder - rrs_555
# ssnlw488 - nlw_443, nlw_488, nlw_531
# RBD - nlw_667, nlw_678
# KBBI - nlw_667, nlw_678

## red band difference
### Ruhul Amin, Jing Zhou, Alex Gilerson, Barry Gross, Fred Moshary, and Samir Ahmed, "Novel optical techniques for detecting and classifying toxic dinoflagellate Karenia brevis blooms using satellite imagery," Opt. Express 17, 9126-9144 (2009)
# nLw678 - nLw667
### Karenia brevis bloom index (KBBI)
# (nLw678 - nLw667)/(nLw678 + nLw667)
# Rrs = nLw/F0
# Rrs: sr^-1; nLw: mW cm^-2 um^-1 sr^-1; F0: mW cm^-2 um^-1
sat_lll <- c(412,443,469,488,531,547,551,555,645,667,678,748,859,869,1240,1640,2130)
F0 <- c(172.912,187.622,205.878,194.933,185.747,186.539,186.539,183.869,157.811,152.255,148.052,128.065,97.174,95.824,45.467,23.977,9.885)

### reference date and julian days
yr <- 2021
last <- ifelse(leap_year(paste0(yr,'-01-01')),366,365)

dates <- data.frame(date=ymd(seq(as.Date(paste0(yr,'-01-01')),as.Date(paste0(yr,'-12-31')),'day')),
                    yday=yday(ymd(seq(as.Date(paste0(yr,'-01-01')),as.Date(paste0(yr,'-12-31')),'day'))))
parms <- c('CHL_chlor_a','FLH_nflh','RRS_Rrs_443','RRS_Rrs_488','RRS_Rrs_531','RRS_Rrs_547','RRS_Rrs_555','RRS_Rrs_667','RRS_Rrs_678')
parm <- substr(parms,5,11)

url <- 'http://oceandata.sci.gsfc.nasa.gov/opendap/MODISA/L3SMI/2011/002/A2011002.L3m_DAY_CHL_chl_ocx_4km.nc'
modis <- nc_open(url)
lon <- ncvar_get(modis, 'lon')
lon_start <- which(lon>lonbox_w)[1]-1
lon_stop <- which(lon>lonbox_e)[1]
lon_count <- length(lon_start:lon_stop)
lat <- ncvar_get(modis, 'lat')
lat_start <- which(lat<latbox_n)[1]-1
lat_stop <- which(lat<latbox_s)[1]
lat_count <- length(lat_start:lat_stop)
lon2 <- ncvar_get(modis, 'lon',start=lon_start,count=lon_count)
lat2 <- ncvar_get(modis, 'lat',start=lat_start,count=lat_count)

data_yday <- array(NA,c(nrow(dates),9,length(lon2),length(lat2)))
data_yday <- array(NA,c(1,9,length(lon2),length(lat2)))
t1 <- system.time(
  for(i in 1:nrow(dates)){
    for(j in 1:length(parms)){
      url <- paste0('http://oceandata.sci.gsfc.nasa.gov/opendap/MODISA/L3SMI/',
                    yr,
                    '/',
                    sprintf("%03d",dates$yday[i]),
                    '/A',
                    yr,
                    sprintf("%03d",dates$yday[i]),
                    '.L3m_DAY_',
                    parms[j],
                    '_4km.nc')
      modis <- nc_open(url)
      data <- ncvar_get(modis, parm[j],start=c(lon_start,lat_start),count=c(lon_count,lat_count))
      nc_close(modis)
      
      data_yday[i,j,,] <- data
    }
  }
)

par(mfrow=c(2,2))
for(i in 1:9){
  imagePlot(lon2,rev(lat2),
        log10(data_yday[1,i,,(dim(data_yday)[4]:1)]),
        asp=1)
  mtext(parm[i])
}

# how much storage will this take?
data_yday <- array(1000000,c(1,9,length(lon2),length(lat2)))
size <- object.size(data_yday)
 # 1 day ~ 1.8 mb; 
format(size*365*22,units='GB')


# d) what derived parameters post-processing
# chl anomaly
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
# corrected chl anomaly - rrs_667

### 2) match up to HAB monitoring data


### 3) match up to SST and bathymetry


### 4) variable selection


### 5) create model


### 6) valiadate model


