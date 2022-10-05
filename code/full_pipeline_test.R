
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
sel <- 2083:2096
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
graph_file <- 'test_mosaic2.xml'
par_file <-  'ReprojectEx01.par'

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
data <- nc_open('c01.AQUA_MODIS.20050801.OC.WFS.nc')
chl <- ncvar_get(data,'chlor_a')
chl2 <- chl1 <- chl
mask <- ncvar_get(data,'l2_masks')
flags <- ncvar_get(data,'l2_flags')

chl[which(chl==0)] <- NA
image(log(chl),asp=1)

chl1[which(mask>0)] <- NA
image(log(chl1),asp=1)


flags_v <- as.vector(flags)
flags_1.1 <- sapply(flags_v,l2_flag_check,ref)
flags_1.2 <- matrix(flags_1.1,nrow(flags),ncol(flags))
flags_1.3 <- structure(flags_1.1, dim=dim(flags))

chl2[flags_1.3] <- NA
image(log(chl2),asp=1)

chlm[flags_1.2] <- NA
image(log(chlm),asp=1)

sapply(list(chl,chl2),mean,na.rm=T)
sapply(list(chl,chl2),sd,na.rm=T)
sapply(list(chl,chl2),quantile,na.rm=T)
sapply(list(chl,chl2),function(x) length(!is.na(x)))

chl1[!is.na(chl1)] <- 1
chl1[is.na(chl1)] <- 0
chl2[!is.na(chl2)] <- 3
chl2[is.na(chl2)] <- 0

image(chl1-chl2,asp=1,col=c(1,2,4),breaks=c(-3,-2.5,-1,1))

which((chl1-chl2)==-3)

odd_flags <- sort(unique(flags[which((chl1-chl2)==-3)]))

sapply(odd_flags,l2_flag_check,ref)

lapply(odd_flags,function(x) which(intToBits(x)>0))

cbind(intToBits(odd_flags[1]),ref)
