### modis level 2 flags
### 32 bits flags
### defined https://oceancolor.gsfc.nasa.gov/atbd/ocl2flags/

flags <- 0:31
iflags <- rep(NA,32)
for(i in flags){
  iflags[i+1] <- 2^i
}

iflags[1]+iflags[4]+iflags[25]+iflags[28]

### add one because 32-bit flags start at zero but indexing starts at 1
L2GEN <- c(0,1,3,4,5,8,9,10,12,14,15,16,19,21,22,25,26)+1
### add them together to get value to mask
ref <- sum(iflags[L2GEN])
ref <- as.integer(intToBits(ref))
which(ref>0)

ind <- sort(sample(1:32,10,replace = F))
test <- sum(iflags[ind])

test_i <- as.integer(intToBits(test))

test_i
ref

match(test_i,ref)

any(rowSums(cbind(test_i,ref))==2)

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

l2_flag_check(test,1)
system.time(l2_flag_check(test,1))

m <- 500
test_v <- rep(NA,m*m)
test_v[] <- test
for(i in 1:length(test_v)){
  ind <- sort(sample(1:32,10,replace = F))
  test_v[i] <- sum(iflags[ind])
}
test <- matrix(test_v,m,m)


ind <- sort(sample(1:32,10,replace = F))
test <- sum(iflags[ind])
m <- 1000
test_v <- rep(NA,m*m)
test_v[] <- test
test <- matrix(test_v,m,m)
### https://stackoverflow.com/questions/8579257/r-applying-function-over-matrix-and-keeping-matrix-dimensions
res <- system.time(vapply(test,l2_flag_check,numeric(1),ref))

x <- as.vector(test)
res2 <- system.time(lapply(x,l2_flag_check,ref))


l2gen_mask <- sum(iflags[L2GEN])
flags <- iflags[L2GEN]

m <- 1
n <- 0
out <- rep(NA,2^17)
for(i in 1:17){
  x <- combn(flags,i)
  n <- n + ncol(x)
  out[m:n] <- colSums(x)
  m <- n + 1
}
which(is.na(out))

length(unique(out))

