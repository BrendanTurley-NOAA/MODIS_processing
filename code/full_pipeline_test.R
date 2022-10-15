rm(list=ls())
gc()

library(fields)
library(httr)
library(lubridate)
library(maps)
library(ncdf4)

modis_mask <- function(data, mask){
  tmp <- data
  tmp[which(mask>0)] <- NA
  return(tmp)
}

source('~/Documents/R/Github/MODIS_processing/code/l2_flag_check.R')

### ----------- Distributed Download list ----------- 
setwd('/Users/Brendan/Documents/nasa/')
modis_list <- readLines('aqua_modis_wfs_download_list2.txt')
### sorts list by days; most days have multiple files
dates <- substr(modis_list,12,19)
tab <- table(dates) # summarize number of files per day
# max(tab) # I did explore assigning stratified by number of files per day; by this method seemed just as random but more efficient
core <- 32 # number of cores used
n <- length(tab)
### randomly assign a number to each day
samps <- rep(NA,n)
for(i in 1:n){
  samps[i] <- sample(1:core,1)
}
hist(samps) # visually check random distribution
### assign selected files into download lists randomly distributed by day per core
### each list would be distributed to a core
for(i in 1:core){
  dats <- names(tab[which(samps==i)])
  names <- unlist(lapply(dats,function(x) grep(x,modis_list)))
  assign(paste0('download',i),modis_list[names])
}

### ----------- Download ----------- 

### need unique appkey; keep this a secret!
setwd('/Users/Brendan/Documents/nasa/cookies')
appkey <- readLines('appkey.txt')

#Set up your ~/.netrc file as listed here: https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+cURL+And+Wget or https://oceancolor.gsfc.nasa.gov/data/download_methods/#:~:text=Create%20a%20.netrc%20file.
netrc_path <- "~/.netrc"
cookie_path <- "~/.urs_cookies"


### A list of files could be supplied in a loop to download using options 1-3 listed below.
setwd('/Users/Brendan/Documents/nasa/')
# modis_list <- readLines('aqua_modis_wfs_download_list.txt')
# sel <- 2083:2096 # 8days
# sel <- 2083:2113 # 16days
modis_list <- readLines('aqua_modis_wfs_download_list2.txt')
sel <- 2036:2050
individual_file_url <- paste0('https://oceandata.sci.gsfc.nasa.gov/ob/getfile/',
                              modis_list[sel])

### based upon code here: https://wiki.earthdata.nasa.gov/display/EL/How+to+access+data+with+R
set_config(config(followlocation=1,
                  netrc=1,
                  netrc_file=netrc_path,
                  cookie=cookie_path,
                  cookiefile=cookie_path,
                  cookiejar=cookie_path))

### counter per core
core <- 'c01.'
### emtpy for system time
times1 <- rep(NA,length(individual_file_url))
for(i in 1:length(individual_file_url)){
  downloaded_file_path <- paste0("/Users/Brendan/Documents/nasa/download_test/",core,modis_list[sel[i]])
  t1 <- system.time(
    httr::GET(url = paste0(individual_file_url[i],
                           '?appkey=',
                           appkey),
              write_disk(downloaded_file_path, # this directs and renames downloaded file
                         overwrite = TRUE))
  )
  times1[i] <- t1[3]
}
cat('total time:',sum(times1), 'sec')



### ----------- Processing ----------- 

### set working directory
setwd('/Users/Brendan/Documents/nasa/download_test/')
### set files for GPT processing
### Both graph and parameter files where created using the SeaDAS cookbook
### https://seadas.gsfc.nasa.gov/help-8.1.0/GptCookbook/gptCookbook.html
graph_file <- 'gom_mosaic_derived4.xml'
par_file <-  'gom_mosaic4.par'
core <- 'c01.'

files <- list.files()
files <- files[grep('.nc',files)]

dates <- substr(files,16,23)
days <- sort(unique(dates))

times2 <- rep(NA,length(days))
for(i in 1:length(days)){
  ind <- grep(days[i],files)
  in_file <- files[ind]
  write.table(in_file,'infile.txt',quote=F,col.names = F,row.names = F)
  
  out_file <- paste0(core,'AQUA_MODIS.',days[i],'.OC.WFS.nc')
  
  ### create command with appropriate files
  cmd <- paste0('/Applications/snap/bin/gpt ',graph_file,' -p ',par_file ," `cat infile.txt`",' -Pofile=/Users/Brendan/Documents/nasa/download_test/out/',out_file)
  ### run command
  t2 <- system.time(
    system(cmd) # calls command in Mac Terminal
  )
  times2[i] <- t2[3]
}

cat('Download total time:',sum(times1), 'sec')
cat('Processing total time:',sum(times2), 'sec')

### ----------- Results ----------- 
### benchmark 2022/10/12 for 8 days of data
# download speed: 352.44 Mbps (https://www.speedtest.net)
# download time: 26.589 sec
# download total file size: 580 MB (14 files)
# MacBook Pro (2015); 2.9 GHz Dual-Core Intel Core i5; 16 GB 1867 MHz DDR3
# processing time: 215.385 sec (~3 min 35 sec)
# output total file size: 44.9 MB

### benchmark 2022/10/13 for 8 days of data
### xml file was slightly amended which may account for increased processing time and final output size
# download speed: 349.25 Mbps (https://www.speedtest.net)
# download time: 25.706 sec
# download total file size: 580 MB (14 files)
# MacBook Pro (2015); 2.9 GHz Dual-Core Intel Core i5; 16 GB 1867 MHz DDR3
# processing time: 243.754 sec (~4 min 4 sec)
# output total file size: 45.1 MB

### benchmark 2022/10/13 for 8 days of data
### original xml file, additional processing in second benchmark xml is superfluous, it was removed
# download speed: 350.80 Mbps (https://www.speedtest.net)
# download time: 27.001 sec
# download total file size: 580 MB (14 files)
# MacBook Pro (2015); 2.9 GHz Dual-Core Intel Core i5; 16 GB 1867 MHz DDR3
# processing time: 225.866 sec (~3 min 46 sec)
# output total file size: 44.9 MB

### benchmark 2022/10/14 for 8 days of data
### updated xml file (slightly smaller region of interest and update download list
# download speed: 335.75 Mbps (https://www.speedtest.net)
# download time: 27.238 sec
# download total file size: 602.8 MB (15 files)
# MacBook Pro (2015); 2.9 GHz Dual-Core Intel Core i5; 16 GB 1867 MHz DDR3
# processing time: 246.082 sec (~4 min 6 sec)
# output total file size: 39.2 MB

setwd('/Users/Brendan/Documents/nasa/download_test/out')
files <- list.files()

a_chl <- array(NA,c(8,722,722))
b_chl <- array(NA,c(8,722,722))
pdf('test_all.pdf',width=10,height=10,useDingbats=T)
par(mfrow=c(2,2))
for(i in 1:length(files)){
  data <- nc_open(files[i])
  chl <- ncvar_get(data,'chlor_a')
  mask1 <- ncvar_get(data,'msl12_mask')
  mask2 <- ncvar_get(data,'l2gen_mask')
  mask <- mask1+mask2
  # clouds <- ncvar_get(data,'cloud_mask')
  lon <- ncvar_get(data,'lon')
  lat <- ncvar_get(data,'lat')
  lat <- rev(lat)
  
  ### finding swath
  if(any(mask==0 & is.na(chl))){
    swath <- matrix(0,dim(mask)[1],dim(mask)[2])
    swath[which(mask==0 & is.na(chl))] <- 1
    swath <- swath[,ncol(swath):1]
    # imagePlot(lon,lat,swath,asp=1)
    swath[which(swath==0)] <- NA
  }
  
  
  chl_1 <- modis_mask(chl,mask)
  chl_1 <- chl_1[,ncol(chl_1):1]
  chl_1 <- log10(chl_1)
  
  chl_2 <- modis_mask(chl,mask1)
  chl_2 <- chl_2[,ncol(chl_2):1]
  chl_2 <- log10(chl_2)
  
  imagePlot(lon,lat,chl_1,asp=1)
  if(any(mask==0 & is.na(chl))){
    image(lon,lat,swath,breaks=c(0,1),col=1,add=T)
  }
  map('world',add=T,col='gray20')
  mtext(files[i])
  
  imagePlot(lon,lat,chl_2,asp=1)
  if(any(mask==0 & is.na(chl))){
    image(lon,lat,swath,breaks=c(0,1),col=1,add=T)
  }
  map('world',add=T,col='gray20')
  mtext(files[i])
  
  a_chl[i,,] <- chl_1
  b_chl[i,,] <- chl_2
}
dev.off()

a_chl_8d_m <- apply(a_chl,c(2,3),mean,na.rm=T)
a_chl_8d_sd <- apply(a_chl,c(2,3),sd,na.rm=T)
a_chl_8d_n <- apply(a_chl,c(2,3),function(x) length(which(!is.na(x))))
a_chl_8d_se <- a_chl_8d_sd/a_chl_8d_n

b_chl_8d_m <- apply(b_chl,c(2,3),mean,na.rm=T)
b_chl_8d_sd <- apply(b_chl,c(2,3),sd,na.rm=T)
b_chl_8d_n <- apply(b_chl,c(2,3),function(x) length(which(!is.na(x))))
b_chl_8d_se <- b_chl_8d_sd/b_chl_8d_n

png('test_L2GEN.png',width=10,height=10,res=300,units='in')
par(mfrow=c(2,2),mar=c(5,4,2,4))
imagePlot(lon,lat,a_chl_8d_m,asp=1,
          breaks=pretty(a_chl_8d_m,n=30),
          col = viridis(length(pretty(a_chl_8d_m,n=30))-1))
map('world',add=T,fill=T,col='gray80')
mtext('Log10 Chl 8-day mean')
imagePlot(lon,lat,a_chl_8d_n,asp=1,
          breaks=c(pretty(a_chl_8d_n)-.5,pretty(a_chl_8d_n)[length(pretty(a_chl_8d_n))]+.5),
          col = inferno(length(pretty(a_chl_8d_n))))
map('world',add=T,fill=T,col='gray80')
mtext('Number of samples per pixel')
imagePlot(lon,lat,a_chl_8d_se,asp=1,
          breaks=pretty(a_chl_8d_se,n=10),
          col = plasma(length(pretty(a_chl_8d_se,n=10))-1))
map('world',add=T,fill=T,col='gray80')
mtext('Standard error')
dev.off()

png('test_MLS12.png',width=10,height=10,res=300,units='in')
par(mfrow=c(2,2),mar=c(5,4,2,4))
imagePlot(lon,lat,b_chl_8d_m,asp=1,
          breaks=pretty(b_chl_8d_m,n=30),
          col = viridis(length(pretty(b_chl_8d_m,n=30))-1))
map('world',add=T,fill=T,col='gray80')
mtext('Log10 Chl 8-day mean')
imagePlot(lon,lat,b_chl_8d_n,asp=1,
          breaks=c(pretty(b_chl_8d_n)-.5,pretty(b_chl_8d_n)[length(pretty(b_chl_8d_n))]+.5),
          col = inferno(length(pretty(b_chl_8d_n))))
map('world',add=T,fill=T,col='gray80')
mtext('Number of samples per pixel')
imagePlot(lon,lat,b_chl_8d_se,asp=1,
          breaks=pretty(b_chl_8d_se,n=10),
          col = plasma(length(pretty(b_chl_8d_se,n=10))-1))
map('world',add=T,fill=T,col='gray80')
mtext('Standard error')
dev.off()

lm_neg <- colorRampPalette(c('dodgerblue4','deepskyblue3','lightskyblue1','gray95'))
lm_pos <- colorRampPalette(c('gray95','rosybrown1','tomato2','red4'))

### change in chlorophyll between masks
chl_diff <- b_chl_8d_m-a_chl_8d_m
brks <- pretty(chl_diff,n=30)
cols <- c(lm_neg(length(which(brks<0))),
          lm_pos(length(which(brks>0))))

png('test_c.png',width=12,height=10,res=300,units='in')
par(mfrow=c(1,1),mar=c(5,4,2,4))
imagePlot(lon,lat,chl_diff,asp=1,
          breaks=brks,
          col = cols)
map('world',add=T)
mtext('Log10(chl_MSL12) - log10(chl_L2GEN)')
dev.off()

### change in number of days with data
chl_diff <- b_chl_8d_n-a_chl_8d_n
brks <- pretty(chl_diff,n=30)
cols <- c(lm_neg(length(which(brks<0))),
          lm_pos(length(which(brks>0))))

par(mfrow=c(1,1),mar=c(5,4,2,4))
imagePlot(lon,lat,chl_diff,asp=1,
          breaks=brks,
          col = cols)
# map('world',add=T)

### change in SE
chl_diff <- b_chl_8d_se-a_chl_8d_se
brks <- pretty(chl_diff,n=30)
cols <- c(lm_neg(length(which(brks<0))),
          lm_pos(length(which(brks>0))))

par(mfrow=c(1,1),mar=c(5,4,2,4))
imagePlot(lon,lat,chl_diff,asp=1,
          breaks=brks,
          col = cols)
# map('world',add=T)

a <- a_chl_8d_m
a[which(!is.na(a))] <- 1
a[which(is.na(a))] <- 0
b <- b_chl_8d_m
b[which(!is.na(b))] <- 2
b[which(is.na(b))] <- 0
c <- a+b

png('test_d.png',width=12,height=10,res=300,units='in')
par(mfrow=c(1,1),mar=c(5,4,2,4))
imagePlot(lon,lat,c,asp=1,
          # breaks=pretty(c,n=3),
          breaks=c(pretty(c,n=3)-.5,pretty(c,n=3)[length(pretty(c,n=3))]+.5),
          col = plasma(length(pretty(c,n=3))))
mtext('No data = 0, L2GEN = 1, MSL12 = 2, Both = 3')
dev.off()



imagePlot(lon,lat,a_chl_8d_m,asp=1)
contour(lon,lat,a_chl_8d_n,levels=c(2,10),add=T)

imagePlot(lon,lat,b_chl_8d_m,asp=1)
contour(lon,lat,b_chl_8d_n,levels=c(2,10),add=T)

### encode which days have obs per pixel
chl_01 <- a_chl
chl_01[which(!is.na(chl_01))] <- 1
chl_01[which(is.na(chl_01))] <- 0
barplot(apply(chl_01,1,sum,na.rm=T))
chl_01 <- apply(chl_01,c(2,3),function(x) paste(x,collapse=''))
sort(table(chl_01))

