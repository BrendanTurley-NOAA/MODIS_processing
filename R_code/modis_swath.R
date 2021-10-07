library(raster)
library(rgdal)
library(maps)

map('world')

### load map
# setwd("~/Desktop/professional/biblioteca/data/shapefiles/ne_10m_admin_0_countries")
# world <- readOGR('ne_10m_admin_0_countries.shp')


setwd('~/Desktop/professional/projects/Postdoc_FL/hab_index/modis')
data <- read.csv('MYD03_2002-07-04_6.txt',skip=2)



cols <- colorRampPalette(c('green','gray50','purple'))
cols1 <- cols(288)

plot(world)

map('world')
for(i in 1:288){
  rect(data$WestBoundingCoord[i],
       data$SouthBoundingCoord[i],
       data$EastBoundingCoord[i],
       data$NorthBoundingCoord[i],
       border=cols1[i])
}

map('world')
points(data[9,10:13],data[9,14:17],col=1:4,lwd=4)


points(data[9,c(6,9)],data[9,7:8],col=3)
rect(data$EastBoundingCoord[9],
     data$NorthBoundingCoord[9],
     data$WestBoundingCoord[9],
     data$SouthBoundingCoord[9],
     border=3)

rect(data$WestBoundingCoord[9],
     data$SouthBoundingCoord[9],
     data$EastBoundingCoord[9],
     data$NorthBoundingCoord[9],
     border=2)
rect(data$GRingLongitude1[9],
     data$GRingLatitude1[9],
     data$GRingLongitude2[9],
     data$GRingLatitude2[9],
     border=3)

rect(data$GRingLongitude3[9],
     data$GRingLatitude3[9],
     data$GRingLongitude4[9],
     data$GRingLatitude4[9],
     border=4)

lon <- -83
lat <- 27

inx <- which(data$EastBoundingCoord>=lon &
      data$WestBoundingCoord<=lon &
        data$NorthBoundingCoord>=lat &
      data$SouthBoundingCoord<=lat)

map('world',xlim=c(-150,-20),ylim=c(0,50))
for(i in 1:length(inx)){
  # points(data[inx[i],10:13],data[inx[i],14:17],col=1:4,lwd=4,pch=i)
  polygon(data[inx[i],10:13],data[inx[i],14:17],border=i)
}






### make swath boundaries a spatial object

setwd('~/Desktop/professional/projects/Postdoc_FL/hab_index/modis')
data <- read.csv('MYD03_2002-07-04_6.txt',skip=2)

#### columns 10-13 and 14-17
poly <- data.frame(lon=as.numeric(paste(data[9,10:13])),
                   lat=as.numeric(paste(data[9,14:17])))
poly <- Polygon(poly)
poly <- Polygons(list(poly),1)
poly <- SpatialPolygons(list(poly))

crs(poly) <- CRS("+proj=longlat +datum=WGS84")

new_crs <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ')
new_crs <- CRS('+proj=moll')

new_poly <- spTransform(poly,new_crs)

world <- map('world')
plot(poly,add=T)
plot(new_poly,add=T,col=2)


library(geosphere)
?greatCircle
?gcIntermediate

epsg <- make_EPSG()
