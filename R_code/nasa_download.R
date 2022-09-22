
library(ncdf4)
library(httr)

### This method requires the user to register for free at https://urs.earthdata.nasa.gov/. 
### Then get you must generate an appkey as laid out here https://oceancolor.gsfc.nasa.gov/data/download_methods/#:~:text=Generate%20and%20copy%20your%20AppKey or here https://oceandata.sci.gsfc.nasa.gov/appkey/.
### I saved mine as a text file which I load to protect its secrecy.

### need unique appkey; keep this a secret!
setwd('/Users/Brendan/Documents/nasa/cookies')
appkey <- readLines('appkey.txt')

#Set up your ~/.netrc file as listed here: https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+cURL+And+Wget
netrc_path <- "~/.netrc"
cookie_path <- "~/.urs_cookies"


### A list of files could be supplied in a loop to download using options 1-3 listed below.
setwd('/Users/Brendan/Documents/nasa/')
modis_list <- readLines('aqua_modis_wfs_download_list.txt')

### The following examples 1-3 use the url
### example file to download
url <- 'https://oceandata.sci.gsfc.nasa.gov/ob/getfile/AQUA_MODIS.20100109T000001.L2.OC.nc'

###---------------- 1) httr method -------
### based upon code here: https://wiki.earthdata.nasa.gov/display/EL/How+to+access+data+with+R
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



### Both the curl and wget method are based upon info found here: https://oceancolor.gsfc.nasa.gov/data/download_methods/

###---------------- 2) curl method -------
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
  system(cmd)
)


###---------------- 3) wget method -------
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
  system(cmd)
)


###---------------- 4) downloading orders from a manifest -------
### The manifest is the produced by NASA's Ocean Biology Processing Group when you request an order by defining area and time of interest from https://oceancolor.gsfc.nasa.gov/cgi/browse.pl?sen=amod.
### Plus you can request extraction of specific parts of the file and avoid downloading the whole thing.
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
              manifest_file_name,
              ' -q --show-progress --progress=bar:force 2>&1')


t4 <- system.time(
  system(cmd)
)

### renaming code for orders from a manifest
setwd('/Users/Brendan/Documents/nasa/')
files <- list.files() # get list of files
ind_tar <- grep('.tar',files) # which is the tar file downloaded? assumes one tar file
untar(files[ind_tar],verbose=T)
files2 <- list.files() # get list of files
ind_new <- which(!is.element(files2,files)) # which is the newly untarred folder

setwd(paste0('/Users/Brendan/Documents/nasa/',files2[ind_new])) # open folder just untarred
files <- list.files() # get list of files
for(i in 1:length(files)){
  file.rename(files[i],
              paste('marker',# where this could be whatever sequential or predefined maker we decide
                    files[i],sep='.'))
}
