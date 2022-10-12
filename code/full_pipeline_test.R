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

### ----------- Download ----------- 

### need unique appkey; keep this a secret!
setwd('/Users/Brendan/Documents/nasa/cookies')
appkey <- readLines('appkey.txt')

#Set up your ~/.netrc file as listed here: https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+cURL+And+Wget or https://oceancolor.gsfc.nasa.gov/data/download_methods/#:~:text=Create%20a%20.netrc%20file.
netrc_path <- "~/.netrc"
cookie_path <- "~/.urs_cookies"


### A list of files could be supplied in a loop to download using options 1-3 listed below.
setwd('/Users/Brendan/Documents/nasa/')
modis_list <- readLines('aqua_modis_wfs_download_list.txt')
sel <- 2083:2096 # 8days
# sel <- 2083:2113 # 16days
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



### ----------- Mosaic ----------- 

### set working directory
setwd('/Users/Brendan/Documents/nasa/download_test/')
### set files for GPT processing
### Both graph and parameter files where created using the SeaDAS cookbook
### https://seadas.gsfc.nasa.gov/help-8.1.0/GptCookbook/gptCookbook.html
graph_file <- 'gom_mosaic_derived4.xml'
par_file <-  'ReprojectEx01.par'
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
cat('total time:',sum(times2), 'sec')

### results 2022/10/12 on 8 days of data
# download speed: 352.44 Mbps (https://www.speedtest.net)
# download time: 26.589 sec
# download total file size: 580 MB (14 files)
# processing time: 215.385 sec (~3 min 35 sec)
# output total file size: 44.9 MB

setwd('/Users/Brendan/Documents/nasa/download_test/out')
files <- list.files()

a_chl <- array(NA,c(8,777,777))
b_chl <- array(NA,c(8,777,777))
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
  
  ### finding swath NAs
  # mask4 <- matrix(0,dim(mask)[1],dim(mask)[2])
  # mask4[which(mask==0 & is.na(chl))] <- 1
  # imagePlot(mask4,asp=1)
  
  chl_1 <- modis_mask(chl,mask)
  chl_1 <- chl_1[,ncol(chl_1):1]
  chl_1 <- log10(chl_1)
  
  chl_2 <- modis_mask(chl,mask1)
  chl_2 <- chl_2[,ncol(chl_2):1]
  chl_2 <- log10(chl_2)
  
  imagePlot(lon,lat,chl_1,asp=1)
  map('world',add=T)
  mtext(files[i])
  
  imagePlot(lon,lat,chl_2,asp=1)
  map('world',add=T)
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

png('test_a.png',width=10,height=10,res=300,units='in')
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

png('test_b.png',width=10,height=10,res=300,units='in')
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

a <- a_chl_8d_m
a[which(!is.na(a))] <- 1
a[which(is.na(a))] <- 0
b <- b_chl_8d_m
b[which(!is.na(b))] <- 2
b[which(is.na(b))] <- 0
c <- a+b

png('test_d.png',width=12,height=10,res=300,units='in')
imagePlot(lon,lat,c,asp=1,
          # breaks=pretty(c,n=3),
          breaks=c(pretty(c,n=3)-.5,pretty(c,n=3)[length(pretty(c,n=3))]+.5),
          col = plasma(length(pretty(c,n=3))))
mtext('No data = 0, L2GEN = 1, MSL12 = 2, Both = 3')
dev.off()


imagePlot(lon,lat,chl_8d_n,asp=1)
imagePlot(lon,lat,chl_8d_se,asp=1)
# image(lon,lat,chl_8d_n,add=T,breaks=c(-.5,.5),col=1)

imagePlot(lon,lat,chl_8d_m,asp=1)
contour(lon,lat,chl_8d_n,levels=c(2,10),add=T)

### encode which days have obs per pixel
chl_01 <- a_chl
chl_01[which(!is.na(chl_01))] <- 1
chl_01[which(is.na(chl_01))] <- 0
barplot(apply(chl_01,1,sum,na.rm=T))
chl_01 <- apply(chl_01,c(2,3),function(x) paste(x,collapse=''))
sort(table(chl_01))
