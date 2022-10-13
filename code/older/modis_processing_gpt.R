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
graph_file <- 'reproject_test3.xml'
par_file <-  'ReprojectEx01.par'
in_file <- 'A2005217182500.L2_LAC_OC.nc'
out_file <- 'A2005217182500.L2_LAC_OC_reproject2.nc'
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
flags_1 <- ncvar_get(reproj1, 'l2_masks')
lat_1 <- ncvar_get(reproj1, 'lat')
lon_1 <- ncvar_get(reproj1, 'lon')
nc_close(reproj1)

lonbox_e <- -81.5 ### Florida Bay
lonbox_w <- -87 ### mouth of Mississippi River
latbox_n <- 30.5 ### northern coast
latbox_s <- 24.3 ### southern edge of Ket West

ind_x <- which(lon_1<lonbox_e & lon_1>lonbox_w)
ind_y <- which(lat_1<latbox_n & lat_1>latbox_s)
lon_ss <- lon_1[ind_x]
lat_ss <- lat_1[ind_y]
flag_ss <- flags_1[ind_x,ind_y]
chl_ss <- chl_1[ind_x,ind_y]

flags_v <- as.vector(flag_ss)
# res2 <- system.time(lapply(flags_v,l2_flag_check,ref))
flags_1.1 <- lapply(flags_v,l2_flag_check,ref)
flags_1.2 <- matrix(unlist(flags_1.1),nrow(flag_ss),ncol(flag_ss))
flags_1.3 <- structure(unlist(flags_1.1), dim=dim(flag_ss))
flags_1.2[which(flags_1.2==F)] <- NA
flags_1.2[which(flags_1.2==T)] <- 1


# flags_v <- as.vector(flags_1)
# res2 <- system.time(lapply(flags_v,l2_flag_check,ref))
# flags_1.1 <- lapply(flags_v,l2_flag_check,ref)
# flags_1.1 <- structure(flags_1.1, dim=dim(flags_1))


image(lon_ss,rev(lat_ss),log(chl_ss[,ncol(chl_ss):1],base=10),asp=1)
# image(lon_ss,rev(lat_ss),flag_ss[,ncol(flag_ss):1],add=T)
image(lon_ss,rev(lat_ss),flags_1.2[,ncol(flags_1.2):1],add=T,col='white',breaks=c(.5,10))
plot(world,add=T)

png('test_flag.png',width=10,height=10,units='in',res=300)
image(lon_1,rev(lat_1),log(chl_1[,ncol(chl_1):1],base=10),asp=1)
plot(world,add=T)
grid()
dev.off()



### set working directory
setwd('~/Desktop/professional/projects/Postdoc_FL/hab_index/gpt_process')
### set files for GPT processing
### Both graph and parameter files where created using the SeaDAS cookbook
### https://seadas.gsfc.nasa.gov/help-8.1.0/GptCookbook/gptCookbook.html
graph_file <- 'reproject_test5.xml'
par_file <-  'ReprojectEx01.par'
in_file <- 'A2005217182500.L2_LAC_OC.nc'
out_file <- 'A2005217182500.L2_LAC_OC_reproject2.nc'
### create command with appropriate files
cmd <- paste('/Applications/snap/bin/gpt ',graph_file, ' -p ',par_file ,' -Pifile=',in_file,' -Pofile=',out_file,sep='')
### run command
t1 <- system.time( 
  system(cmd) # calls command in Mac Terminal
)

setwd('~/Desktop/professional/projects/Postdoc_FL/hab_index/gpt_process')
bathy <- nc_open('A2005217182500.L2_LAC_OC_reproject2.nc')
attributes(bathy$var)
chl <- ncvar_get(bathy, 'chlor_a')
red1 <- ncvar_get(bathy, 'Rrs_667')
red2 <- ncvar_get(bathy, 'Rrs_678')
blue <- ncvar_get(bathy, 'Rrs_488')
flags <- ncvar_get(bathy, 'l2_masks')
lat <- ncvar_get(bathy, 'lat')
lon <- ncvar_get(bathy, 'lon')
nc_close(bathy)

chl[which(chl<0)] <- NA

tab <- table(red1[which(red1<0)])
fill <- as.numeric(rownames(tab)[1])
# red1[which(red1<0)]
red1[which(red1<=fill)] <- NA

tab <- table(red2[which(red2<0)])
fill <- as.numeric(rownames(tab)[1])
# red2[which(red2<0)] <- NA
red2[which(red2<=fill)] <- NA

tab <- table(blue[which(blue<0)])
fill <- as.numeric(rownames(tab)[1])
# blue[which(blue<0)] <- NA
blue[which(blue<=fill)] <- NA

bathy <- nc_open('A2005217182500.L2_LAC_OC_reproject.nc')
attributes(bathy$var)
chl_0 <- ncvar_get(bathy, 'chlor_a')
flags <- ncvar_get(bathy, 'l2_flags')
lat_0 <- ncvar_get(bathy, 'lat')
lon_0 <- ncvar_get(bathy, 'lon')
nc_close(bathy)

### are the products the same?
res <- as.vector(chl-chl_0)
hist(res)

mask <- which(flags==1)
chl_m <- chl
chl_m[which(flags==1)] <- NA


png('test1.png',width=10,height=10,units='in',res=300)
image(lon,rev(lat),log(chl[,ncol(chl):1],base=10),asp=1)
plot(world,add=T)
grid()
dev.off()

png('test1f.png',width=10,height=10,units='in',res=300)
image(lon,rev(lat),log(chl[,ncol(chl):1],base=10),asp=1)
image(lon,rev(lat),flags[,ncol(flags):1],add=T,breaks=c(1,10),col=1)
plot(world,add=T)
grid()
dev.off()

png('test1m.png',width=10,height=10,units='in',res=300)
image(lon,rev(lat),log(chl_m[,ncol(chl_m):1],base=10),asp=1)
plot(world,add=T)
grid()
dev.off()

png('test2.png',width=10,height=10,units='in',res=300)
image(lon,rev(lat),log(red1[,ncol(red1):1],base=10),asp=1)
plot(world,add=T)
dev.off()

png('test3.png',width=10,height=10,units='in',res=300)
image(lon,rev(lat),log(red2[,ncol(red2):1],base=10),asp=1)
plot(world,add=T)
dev.off()


library(RStoolbox)
?histMatch()

library(raster)
library(sp)
r <-raster(
  # t(mld_kriged$z[,ncol(mld_kriged$z):1]),
  t(chl),
  xmn=range(lon)[1], xmx=range(lon)[2],
  ymn=range(lat)[1], ymx=range(lat)[2], 
  crs=CRS("+proj=longlat +datum=WGS84")
)

r2 <-raster(
  # t(mld_kriged$z[,ncol(mld_kriged$z):1]),
  t(red1),
  xmn=range(lon)[1], xmx=range(lon)[2],
  ymn=range(lat)[1], ymx=range(lat)[2], 
  crs=CRS("+proj=longlat +datum=WGS84")
)
plot(log(r))
plot((r2))
plot(world,add=T)

test <- histMatch(r2,r)
plot(log(test))


graph_file <- 'reproject_test5.xml'
par_file <-  'ReprojectEx01.par'
in_file <- 'A2005217182500.L2_LAC_OC.nc'
out_file <- 'A2005217182500.L2_LAC_OC_reproject2.nc'

cmd <- paste('/Applications/snap/bin/gpt ',graph_file, ' -p ',par_file ,' -Pifile=',in_file,' -Pofile=',out_file,sep='')
# cmd <- paste('/Applications/snap/bin/gpt ',graph_file,' -Pifile=',in_file,' -Pofile=',out_file,sep='')

# cmd <- '/Applications/snap/bin/gpt reproject_test.xml -p ReprojectEx01.par -Pifile=A2005217182500.L2_LAC_OC.nc -Pofile=A2005217182500.L2_LAC_OC_reproject2.nc'

t1 <- system.time(system(cmd))


system2('gpt')
