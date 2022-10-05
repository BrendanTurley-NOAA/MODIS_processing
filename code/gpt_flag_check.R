### GPT - Graph Processing Tool is distributed with both SNAP and SeaDAS, but this code uses the SNAP distribution
### this code assumes you have downloaded and installed SNAP
### https://step.esa.int/main/download/snap-download/

library(fields)
library(ncdf4)
library(rgdal)

source('~/Documents/R/Github/MODIS_processing/code/l2_flag_check.R')

### load shapfiles for plotting
setwd("~/Desktop/professional/biblioteca/data/shapefiles/ne_10m_admin_0_countries")
world <- readOGR('ne_10m_admin_0_countries.shp')

### set working directory
setwd('~/Desktop/professional/projects/Postdoc_FL/hab_index/gpt_process')
### set files for GPT processing
### Both graph and parameter files where created using the SeaDAS cookbook
### https://seadas.gsfc.nasa.gov/help-8.1.0/GptCookbook/gptCookbook.html
graph_file <- 'reproject_test7.xml'
par_file <-  'ReprojectEx01.par'
in_file <- 'A2005217182500.L2_LAC_OC.nc'
out_file <- 'A2005217182500.L2_LAC_OC_reproject.nc'
### create command with appropriate files
cmd <- paste('/Applications/snap/bin/gpt ',graph_file, ' -p ',par_file ,' -Pifile=',in_file,' -Pofile=',out_file,sep='')
### run command
t1 <- system.time( 
  system(cmd) # calls command in Mac Terminal
)

graph_file <- 'reproject_test7_flagcheck.xml'
par_file <-  'ReprojectEx01.par'
in_file <- 'A2005217182500.L2_LAC_OC.nc'
out_file <- 'A2005217182500.L2_LAC_OC_reproject2.nc'
### create command with appropriate files
cmd <- paste('/Applications/snap/bin/gpt ',graph_file, ' -p ',par_file ,' -Pifile=',in_file,' -Pofile=',out_file,sep='')
### run command
t2 <- system.time( 
  system(cmd) # calls command in Mac Terminal
)


graph_file <- 'test_mosaic2.xml'
par_file <-  'ReprojectEx01.par'
out_file <- 'A2005217182500.L2_LAC_OC_reproject3.nc'
### create command with appropriate files
cmd <- paste0('/Applications/snap/bin/gpt ',graph_file,' -p ',par_file ," `cat infile.txt`",' -Pofile=',out_file)
### run command
t3 <- system.time(
  system(cmd) # calls command in Mac Terminal
)

### open newly created files ans plot to see results
setwd('~/Desktop/professional/projects/Postdoc_FL/hab_index/gpt_process')

reproj1 <- nc_open('A2005217182500.L2_LAC_OC_reproject.nc')
attributes(reproj1$var)
chl_1 <- ncvar_get(reproj1, 'chlor_a')
chl_1 <- chl_1[,ncol(chl_1):1]
chl_1 <- log10(chl_1)
flags_1 <- ncvar_get(reproj1, 'l2_masks')
flags_1 <- flags_1[,ncol(flags_1):1]
lat_1 <- ncvar_get(reproj1, 'lat')
lat_1 <- rev(lat_1)
lon_1 <- ncvar_get(reproj1, 'lon')
nc_close(reproj1)

chl_1[which(flags_1==1)] <- NA

reproj2 <- nc_open('A2005217182500.L2_LAC_OC_reproject2.nc')
attributes(reproj2$var)
chl_2 <- ncvar_get(reproj2, 'chlor_a')
chl_2 <- chl_2[,ncol(chl_2):1]
chl_2 <- log10(chl_2)
flags_2 <- ncvar_get(reproj2, 'l2_masks')
flags_2 <- flags_2[,ncol(flags_2):1]
lat_2 <- ncvar_get(reproj2, 'lat')
lat_2 <- rev(lat_2)
lon_2 <- ncvar_get(reproj2, 'lon')
nc_close(reproj2)

flags_v <- as.vector(flags_2)
flags_1.1 <- lapply(flags_v,l2_flag_check,ref)
flags_1.3 <- structure(unlist(flags_1.1), dim=dim(flags_2))

chl_2[flags_1.3] <- NA


reproj3 <- nc_open('A2005217182500.L2_LAC_OC_reproject3.nc')
attributes(reproj3$var)
chl_3 <- ncvar_get(reproj3, 'chlor_a')
chl_3 <- chl_3[,ncol(chl_3):1]
chl_3[which(chl_3==0)] <- NA
chl_3 <- log10(chl_3)
flags_3 <- ncvar_get(reproj3, 'l2_masks')
flags_3 <- flags_3[,ncol(flags_3):1]
flags_4 <- ncvar_get(reproj3, 'l2_flags')
flags_4 <- flags_4[,ncol(flags_4):1]
lat_3 <- ncvar_get(reproj3, 'lat')
lat_3 <- rev(lat_3)
lon_3 <- ncvar_get(reproj3, 'lon')
nc_close(reproj3)

flags_v <- as.vector(flags_4)
flags_1.1 <- sapply(flags_v,l2_flag_check,ref)
flags_1.3 <- structure(flags_1.1, dim=dim(flags_4))

chl_m <- chl_3
chl_m[flags_1.3] <- NA

chl_3[which(flags_3==1)] <- NA


identical(chl_1,chl_2)
identical(chl_3,chl_m)

sapply(list(chl_1,chl_2,chl_3,chl_m),mean,na.rm=T)
sapply(list(chl_1,chl_2,chl_3,chl_m),sd,na.rm=T)
sapply(list(chl_1,chl_2,chl_3,chl_m),quantile,na.rm=T)
sapply(list(chl_1,chl_2,chl_3,chl_m),function(x) length(!is.na(x)))


par(mfrow=c(2,2))
imagePlot(lon_1,lat_1,chl_1,asp=1)
imagePlot(lon_2,lat_2,chl_2,asp=1)
imagePlot(lon_3,lat_3,chl_3,asp=1)
imagePlot(lon_3,lat_3,chl_m,asp=1)


chl_3[!is.na(chl_3)] <- 1
chl_3[is.na(chl_3)] <- 0
chl_m[!is.na(chl_m)] <- 3
chl_m[is.na(chl_m)] <- 0

image(chl_3-chl_m,asp=1,col=c(1,2,4),breaks=c(-3,-2.5,-1,1))

ind <- which((chl_3-chl_m)==-3)

flags_3[ind]

odd_flags <- sort(unique(flags_4[ind]))

sapply(odd_flags,l2_flag_check,ref)

lapply(odd_flags,function(x) which(intToBits(x)>0))

cbind(intToBits(odd_flags[1]),ref)
