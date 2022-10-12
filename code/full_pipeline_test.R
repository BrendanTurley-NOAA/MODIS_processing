
library(fields)
library(httr)
library(lubridate)
library(maps)
library(ncdf4)


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


setwd('/Users/Brendan/Documents/nasa/download_test/out')
files <- list.files()

a_chl <- array(NA,c(8,777,777))
par(mfrow=c(2,2))
for(i in 1:length(files)){
  data <- nc_open(files[i])
  chl <- ncvar_get(data,'chlor_a')
  chl <- chl[,ncol(chl):1]
  mask <- ncvar_get(data,'l2_masks')
  mask <- mask[,ncol(mask):1]
  lon <- ncvar_get(data,'lon')
  lat <- ncvar_get(data,'lat')
  lat <- rev(lat)
  
  chl[which(chl==0)] <- NA
  chl <- log10(chl)
  # image(log(chl),asp=1)
  
  chl[which(mask>0)] <- NA
  imagePlot(lon,lat,chl,asp=1)
  map('world',add=T)
  mtext(files[i])
  
  a_chl[i,,] <- chl
}

chl_8d_m <- apply(a_chl,c(2,3),mean,na.rm=T)
chl_8d_sd <- apply(a_chl,c(2,3),sd,na.rm=T)
chl_8d_n <- apply(a_chl,c(2,3),function(x) length(which(!is.na(x))))
chl_8d_se <- chl_8d_sd/chl_8d_n

png('test.png',width=10,height=10,res=300,units='in')
par(mfrow=c(2,2),mar=c(5,4,2,4))
imagePlot(lon,lat,chl_8d_m,asp=1,
          breaks=pretty(chl_8d_m,n=30),
          col = viridis(length(pretty(chl_8d_m,n=30))-1))
map('world',add=T,fill=T,col='gray80')
mtext('Log10 Chl 8-day mean')
imagePlot(lon,lat,chl_8d_n,asp=1,
          breaks=c(pretty(chl_8d_n)-.5,pretty(chl_8d_n)[length(pretty(chl_8d_n))]+.5),
          col = inferno(length(pretty(chl_8d_n))))
map('world',add=T,fill=T,col='gray80')
mtext('Number of samples per pixel')
imagePlot(lon,lat,chl_8d_se,asp=1,
          breaks=pretty(chl_8d_se,n=10),
          col = plasma(length(pretty(chl_8d_se,n=10))-1))
map('world',add=T,fill=T,col='gray80')
mtext('Standard error')
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
chl_01 <- apply(chl_01,c(2,3),function(x) paste(x,collapse=''))
sort(table(chl_01))

