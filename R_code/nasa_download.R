
library(ncdf4)
library(httr)


### need unique appkey; keep this a secret!
setwd('/Users/Brendan/Documents/nasa/cookies')
appkey <- readLines('appkey.txt')

#Set up your ~/.netrc file as listed here: https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+cURL+And+Wget
netrc_path <- "~/.netrc"
cookie_path <- "~/.urs_cookies"

### example file to download
url <- 'https://oceandata.sci.gsfc.nasa.gov/ob/getfile/AQUA_MODIS.20100109T000001.L2.OC.nc'

### 1) httr method; based upon code here: https://wiki.earthdata.nasa.gov/display/EL/How+to+access+data+with+R
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



### both the curl and wget method are based upon info found here: https://oceancolor.gsfc.nasa.gov/data/download_methods/

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

# cmd <- paste0('curl -b ',
#               cookie_path,
#               ' -c ',
#               cookie_path,
#               ' -L -n ',
#               manifest_file_name)

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

### downloading orders
setwd('/Users/Brendan/Documents/nasa/')
manifest_file_name <- readLines('http_manifest.txt')
manifest_file_name <- 'http_manifest.txt'
cmd <- paste0('wget',
              ' --load-cookies ',
              cookie_path,
              ' --save-cookies ',
              cookie_path ,
              ' --keep-session-cookies ',
              ' --auth-no-challenge=on --no-check-certificate --content-disposition -i ',
              manifest_file_name)


t4 <- system.time(
  system(cmd) # calls command in Mac Terminal
)
 