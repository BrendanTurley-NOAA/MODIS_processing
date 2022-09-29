### GPT - Graph Processing Tool is distributed with both SNAP and SeaDAS, but this code uses the SNAP distribution
### this code assumes you have downloaded and installed SNAP
### https://step.esa.int/main/download/snap-download/

library(fields)
library(maps)
library(ncdf4)

### set working directory
setwd('~/Desktop/professional/projects/Postdoc_FL/hab_index/gpt_process/modis_test')
### set files for GPT processing
### Both graph and parameter files where created using the SeaDAS cookbook
### https://seadas.gsfc.nasa.gov/help-8.1.0/GptCookbook/gptCookbook.html
graph_file <- 'test_mosaic.xml'
par_file <-  'ReprojectEx01.par'
files <- list.files()
files <- files[grep('.nc',files)]
write.table(in_file,'infile.txt',quote=F,col.names = F,row.names = F)
out_file <- 'mosaic_test2.nc'
### create command with appropriate files
cmd <- paste0('/Applications/snap/bin/gpt ',graph_file,' -p ',par_file ," `cat infile.txt`",' -Pofile=',out_file)
### run command
t1 <- system.time(
  system(cmd) # calls command in Mac Terminal
)
t1


reproj1 <- nc_open('mosaic_test2.nc')
attributes(reproj1$var)
data <- ncvar_get(reproj1, 'Rrs_667')

image(data)
image(log(data))
