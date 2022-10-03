require(magrittr)

bits <- 0:31
iflags <- rep(NA,32)
for(i_f in bits){
  iflags[i_f+1] <- 2^i_f
}
flags = c(0,1,3,4,5,8,9,10,12,14,15,16,19,21,22,25,26)
### add one because 32-bit flags start at zero but indexing starts at 1
L2GEN <- flags+1
### add them together to get value to mask
ref <- sum(iflags[L2GEN]) %>% intToBits() %>% as.integer()


l2_flag_check <- function(l2flag, ref){
  ### test l2 flags
  test_i <- as.integer(intToBits(l2flag))
  out <- any(rowSums(cbind(test_i,ref))==2)
  return(out)
}


# setwd('~/Desktop/professional/projects/Postdoc_FL/hab_index/gpt_process/MODIS_test')
# data <- nc_open('AQUA_MODIS.20030108T180001.L2.OC.x.nc')
# flag_att <- ncatt_get(data,'geophysical_data/l2_flags')
# flag_description <- data.frame(meaning=unlist(strsplit(flag_att$flag_meanings,' ')),flags=1:32)
# flag_description[L2GEN,]
