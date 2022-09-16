# Issue: ---
# Author: Catalino Cuadrado (catalino.cuadrado@nasa.gov)
# Date: 12-19-2018

library(ncdf4)
library(httr)


### need unique appkey; keep this a secret!
setwd('/Users/Brendan/Documents/nasa/cookies')
appkey <- readLines('appkey.txt')
# appkey <- '5fc9f83939bd1c6f91e9c6b4ab7b9fbfec56d97c'

#Set up your ~/.netrc file as listed here: https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+cURL+And+Wget
netrc_path <- "/Users/Brendan/Documents/nasa/cookies/cookie.netrc"
cookie_path <- "/Users/Brendan/Documents/nasa/cookies/cookie.urs_cookies"


### 1) httr method
url <- 'https://oceandata.sci.gsfc.nasa.gov/ob/getfile/AQUA_MODIS.20100109T000001.L2.OC.nc'

downloaded_file_path <- "/Users/Brendan/Documents/nasa/download_test/test/test1.nc4"
set_config(config(followlocation=1,
                  netrc=1,
                  netrc_file=netrc_path,
                  cookie=cookie_path,
                  cookiefile=cookie_path,
                  cookiejar=cookie_path))
t1.1 <- system.time(
  httr::GET(url = paste0(url,
                         '?appkey=',
                         appkey),
            write_disk(downloaded_file_path, # this directs and renames downloaded file
                       overwrite = TRUE))
)


### 2) curl method
downloaded_file_path <- "/Users/Brendan/Documents/nasa/download_test/test/test2.nc4"
cmd <- paste0('curl -b ',
              cookie_path,
              ' -c ',
              cookie_path,
              ' -L -n ',
              url,
              '?appkey=',
              appkey,
              ' -o ',
              downloaded_file_path) # this directs and renames downloaded file

t2.1 <- system.time(
  system(cmd) # calls command in Mac Terminal
)


### 3) wget method
setwd('/Users/Brendan/Documents/nasa/download_test/')
# downloaded_file_path <- "/Users/Brendan/Documents/nasa/download_test/test"
downloaded_file_path <- "/Users/Brendan/Documents/nasa/download_test/test/test3.nc4"
cmd <- paste0('wget ‐‐directory-prefix=',
              downloaded_file_path,
              ' --load-cookies ',
              cookie_path,
              ' --save-cookies ',
              cookie_path ,
              ' --keep-session-cookies ',
              url,
              '?appkey=',
              appkey,
              ' -O ',
              downloaded_file_path, # this directs and renames downloaded file
              ' -q --show-progress --progress=bar:force 2>&1')

t3.1 <- system.time(
  system(cmd) # calls command in Mac Terminal
)


