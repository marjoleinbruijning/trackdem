###

setwd("/mnt/disk/Dropbox/TrackDem")
filestem <- "."
direcPictures <- paste0(filestem,"/00365/")

require(png)
require(SDMTools)
require(raster)

# Load all functions
require(trackdem)

# get filenames
allFiles <- list.files(path=paste(direcPictures,sep='')) 
s1 = sapply (1:length(allFiles),function (x) unlist(strsplit(allFiles[x], split='_', fixed=TRUE))[2])
s2 = as.numeric(sapply (1:length(allFiles),function (x) unlist(strsplit(s1[x], split='.png', fixed=TRUE))[1]))
allFiles <- allFiles[order(s2)]

## run functions
loadAll <- loadImages (direcPictures=direcPictures,filenames=allFiles,nImages=1:30,yranges=1:576,xranges=1:1024)
allFullImages <- loadAll$allFullImages
allFullImagesRGB <- loadAll$allFullImagesRGB
stillBack <- createBackground (allFullImages)
allImages <- substractBackground(background=stillBack,allFullImages)
trackObject <- identifyParticles(allImages,threshold=-0.1,pixelRange=c(3,1000))  # Returns list with particle stats and images
wrongIdentified <- manuallySelect(particleStats=trackObject$particleStats)


################################################
## plot
for (i in nImages) {
	plot(particleStats[[i]]$x,particleStats[[i]]$y,pch='.',xlim=c(0,1024),ylim=c(0,578),cex=5,main=paste0('Image ',nImages[i]),
		xlab='x',ylab='y')
	Sys.sleep(0.5)
}
plot(1:30,sapply(1:30,function(X) length(particleStats[[X]]$patchID)),pch=16,xlab='Image no.',ylab='Number of particles')
abline(h=mean(length(particleStats[[X]]$patchID)),lty=2,col='grey')

