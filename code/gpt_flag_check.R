### GPT - Graph Processing Tool is distributed with both SNAP and SeaDAS, but this code uses the SNAP distribution
### this code assumes you have downloaded and installed SNAP
### https://step.esa.int/main/download/snap-download/

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


imagePlot(lon_1,lat_1,chl_1,asp=1)
imagePlot(lon_2,lat_2,chl_2,asp=1)
