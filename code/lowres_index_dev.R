library(fields)
library(lubridate)
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
dates <- data.frame(date=ymd(seq(as.Date(paste0(yr,'-01-01')),as.Date(paste0(yr,'-12-31')),'day')),
                    yday=yday(ymd(seq(as.Date(paste0(yr,'-01-01')),as.Date(paste0(yr,'-12-31')),'day'))))
parms <- c('CHL_chlor_a','FLH_nflh','RRS_Rrs_443','RRS_Rrs_488','RRS_Rrs_531','RRS_Rrs_547','RRS_Rrs_555','RRS_Rrs_667','RRS_Rrs_678')
parm <- substr(parms,5,11)

url <- 'http://oceandata.sci.gsfc.nasa.gov/opendap/MODISA/L3SMI/2011/002/A2011002.L3m_DAY_RRS_Rrs_443_4km.nc'
modis <- nc_open(url)
ncatt_get(modis,0)
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

data_yday <- array(NA,c(9,length(lon2),length(lat2),nrow(dates)))
# data_yday <- array(NA,c(9,length(lon2),length(lat2),1))
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
      
      data_yday[j,,,i] <- data
    }
  }
)
data_yday <- array(1000000,c(9,length(lon2),length(lat2),365))
### create netcdf file
dimlon <- ncdim_def('Lon','degreesE',lon2)
dimlat <- ncdim_def('Lat','degreesN',lat2)
dates1 <- as.POSIXct(paste0(dates$date,00:00),tz='GMT')
dates2 <- as.numeric(dates1)/86400
# as.Date(dates2[1],origin='1970-01-01')
dimtime <- ncdim_def('Time','days since 1970-01-01',dates2)
chl_a <- ncvar_def('chl_a','mg m^-3',list(dimlat,dimlon,dimtime),-1,"Chlorophyll Concentration, OCI Algorithm",prec='double',compression=5) # prec='float may be smaller'
nflh <- ncvar_def('nflh','W m^-2 um^-1 sr^-1',list(dimlat,dimlon,dimtime),-1,"Normalized Fluorescence Line Height",prec='double',compression=5) # prec='float may be smaller'
rrs_443 <- ncvar_def('rrs_443','sr^-1',list(dimlat,dimlon,dimtime),-1,"Remote sensing reflectance at 443 nm",prec='double',compression=5) # prec='float may be smaller'
rrs_488 <- ncvar_def('rrs_488','sr^-1',list(dimlat,dimlon,dimtime),-1,"Remote sensing reflectance at 488 nm",prec='double',compression=5) # prec='float may be smaller'
rrs_531 <- ncvar_def('rrs_531','sr^-1',list(dimlat,dimlon,dimtime),-1,"Remote sensing reflectance at 531 nm",prec='double',compression=5) # prec='float may be smaller'
rrs_547 <- ncvar_def('rrs_547','sr^-1',list(dimlat,dimlon,dimtime),-1,"Remote sensing reflectance at 547 nm",prec='double',compression=5) # prec='float may be smaller'
rrs_555 <- ncvar_def('rrs_555','sr^-1',list(dimlat,dimlon,dimtime),-1,"Remote sensing reflectance at 555 nm",prec='double',compression=5) # prec='float may be smaller'
rrs_667 <- ncvar_def('rrs_667','sr^-1',list(dimlat,dimlon,dimtime),-1,"Remote sensing reflectance at 667 nm",prec='double',compression=5) # prec='float may be smaller'
rrs_678 <- ncvar_def('rrs_678','sr^-1',list(dimlat,dimlon,dimtime),-1,"Remote sensing reflectance at 678 nm",prec='double',compression=5) # prec='float may be smaller'
test_modis <- nc_create('test_modis.nc',list(chl_a,nflh,rrs_443,rrs_488,rrs_531,rrs_547,rrs_555,rrs_667,rrs_678))
ncvar_put(test_modis,chl_a,data_yday[1,,,])
ncvar_put(test_modis,nflh,data_yday[2,,,])
ncvar_put(test_modis,rrs_443,data_yday[3,,,])
ncvar_put(test_modis,rrs_488,data_yday[4,,,])
ncvar_put(test_modis,rrs_531,data_yday[5,,,])
ncvar_put(test_modis,rrs_547,data_yday[6,,,])
ncvar_put(test_modis,rrs_555,data_yday[7,,,])
ncvar_put(test_modis,rrs_667,data_yday[8,,,])
ncvar_put(test_modis,rrs_678,data_yday[9,,,])
ncatt_put(events,0,"title","HMODISA Level-3 Standard Mapped Image")
ncatt_put(events,0,"platform","Aqua")
ncatt_put(events,0,"l2_flag_names","ATMFAIL,LAND,HILT,HISATZEN,STRAYLIGHT,CLDICE,COCCOLITH,LOWLW,CHLWARN,CHLFAIL,NAVWARN,MAXAERITER,ATMWARN,HISOLZEN,NAVFAIL,FILTER,HIGLINT")
ncatt_put(events,0,"map_projection","Equidistant Cylindrical")
ncatt_put(events,0,"processing_version","2018.0")
ncatt_put(events,0,"spatialResolution","4.64 km")
ncatt_put(events,0,"time_coverage_start",dates1[1])
ncatt_put(events,0,"time_coverage_end",dates1[length(dates1)])
ncatt_put(events,0,"publisher_name","http://oceandata.sci.gsfc.nasa.gov")
ncatt_put(events,0,"publisher_url","NASA/GSFC/OBPG")
ncatt_put(events,0,"last_Modified",Sys.time())
ncatt_put(events,0,"modified_by",'Brendan Turley')
nc_close(test_modis)

saveRDS(data_yday,'test_modis.rds')
data_yday <- readRDS('test_modis.rds')

assign(paste0(yr),data_yday)
rm(data_yday)

par(mfrow=c(2,2))
for(i in 1:9){
  imagePlot(lon2,rev(lat2),
        log10(data_yday[i,,(dim(data_yday)[4]:1),1]),
        asp=1)
  mtext(parm[i])
}

# how much storage will this take?
# data_yday <- array(1000000,c(9,length(lon2),length(lat2),1))
# size <- object.size(data_yday)
#  # 1 day ~ 1.8 mb; 
# format(size*365*22,units='GB')
# format(size*365,units='GB')


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


