setwd('~/Desktop/professional/projects/Postdoc_FL/hab_index')

bands <- read.csv('sensor_bands.csv')

cols <- colorRampPalette(c('purple','blue','cyan','green','yellow','orange','red'))

par(mar=c(5,7,1,1))
n <- 10
plot(bands$Center.Wavelength,1:117,
     type='n',xlim=c(350,800),ylim=c(0,90),yaxt='n',
     ylab='')
for(i in unique(bands$Platform)){
  tmp <- bands[which(bands$Platform==i),]
  col1 <- cols(nrow(tmp))
  points(tmp$Center.Wavelength,rep(n,nrow(tmp)),pch=3)
  arrows(tmp$Center.Wavelength-tmp$Width..FWHM./2,rep(n,nrow(tmp)),
         tmp$Center.Wavelength+tmp$Width..FWHM./2,rep(n,nrow(tmp)),
         code=3,length=0,lwd=3,lend=2,col=col1)
  n <- n + 10
}
axis(2,seq(10,80,10),unique(bands$Platform),las=2)
