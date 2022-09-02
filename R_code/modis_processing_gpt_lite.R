### GPT - Graph Processing Tool is distributed with both SNAP and SeaDAS, but this code uses the SNAP distribution
### this code assumes you have downloaded and installed SNAP
### https://step.esa.int/main/download/snap-download/

library(fields)
library(maps)
library(ncdf4)

### set working directory
setwd('~/Desktop/professional/projects/Postdoc_FL/hab_index/gpt_process')
### set files for GPT processing
### Both graph and parameter files where created using the SeaDAS cookbook
### https://seadas.gsfc.nasa.gov/help-8.1.0/GptCookbook/gptCookbook.html
graph_file <- 'reproject_test5.xml'
par_file <-  'ReprojectEx01.par'
in_file <- 'A2005217182500.L2_LAC_OC.nc'
out_file <- 'A2005217182500.L2_LAC_OC_reproject.nc'
### create command with appropriate files
cmd <- paste('/Applications/snap/bin/gpt ',graph_file, ' -p ',par_file ,' -Pifile=',in_file,' -Pofile=',out_file,sep='')
### run command
t1 <- system.time(
  system(cmd) # calls command in Mac Terminal
)


### open newly created files ans plot to see results
setwd('~/Desktop/professional/projects/Postdoc_FL/hab_index/gpt_process')

reproj1 <- nc_open('A2005217182500.L2_LAC_OC_reproject.nc')
attributes(reproj1$var)
chl_1 <- ncvar_get(reproj1, 'chlor_a')
lat <- ncvar_get(reproj1, 'lat')
lat <- rev(lat) ### backwards for some reason, plotting hates that
lon <- ncvar_get(reproj1, 'lon')
nc_close(reproj1)


imagePlot(lon,
          lat,
          log(chl_1[,ncol(chl_1):1],base=10),
          asp=1)
map('usa',add=T)
grid()
