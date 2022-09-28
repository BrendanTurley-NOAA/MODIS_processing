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
graph_file <- 'reproject_test7.xml'
par_file <-  'ReprojectEx01.par'
in_file <- 'A2005217182500.L2_LAC_OC.nc'
out_file <- 'A2005217182500.L2_LAC_OC_reproject5.nc'
### create command with appropriate files
cmd <- paste0('/Applications/snap/bin/gpt ',graph_file, ' -p ',par_file ,' -Pifile=',in_file,' -Pofile=',out_file)
### run command
t1 <- system.time(
  system(cmd) # calls command in Mac Terminal
)

### open newly created files ans plot to see results
setwd('~/Desktop/professional/projects/Postdoc_FL/hab_index/gpt_process')

reproj1 <- nc_open('A2005217182500.L2_LAC_OC_reproject5.nc')
attributes(reproj1$var)
flags <- ncvar_get(reproj1, 'l2_masks')
flags <- flags[,ncol(flags):1] # flip the matrix
flags[which(flags==0)] <- NA
flags[which(flags=='NaN')] <- NA
chl_1 <- ncvar_get(reproj1, 'chlor_a')
chl_1[which(chl_1=='NaN')] <- NA
chl_1 <- chl_1[,ncol(chl_1):1] # flip the matrix
abi <- ncvar_get(reproj1, 'ABI')
abi[which(abi=='NaN')] <- NA
abi <- abi[,ncol(abi):1] # flip the matrix
lat <- ncvar_get(reproj1, 'lat')
lat <- rev(lat) ### backwards for some reason, plotting hates that
lon <- ncvar_get(reproj1, 'lon')
nc_close(reproj1)


png('test_flag_0.png',width=10,height=10,units='in',res=300)
imagePlot(lon,
          lat,
          log(chl_1,base=10),
          asp=1)
map('usa',add=T)
grid()
dev.off()


chl_m <- chl_1
chl_m[which(!is.na(flags))] <- NA
png('test_flag_1.png',width=10,height=10,units='in',res=300)
imagePlot(lon,
          lat,
          log(chl_m,base=10),
          asp=1)
map('usa',add=T)
grid()
dev.off()


abi_m <- abi
abi_m[which(!is.na(flags))] <- NA
imagePlot(lon,
          lat,
          abi_m,
          asp=1)
map('usa',add=T)
