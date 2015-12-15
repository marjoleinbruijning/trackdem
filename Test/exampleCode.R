# Dependencies: png, SDMTools, raster, animation, adimpro,inline
# Rcpp, neuralnet, RccpArmadillo

loadAll <- loadImages (direcPictures=direcPictures,filenames=allFiles,
  nImages=1:30)
allFullImages <- loadAll$allFullImages
allFullImagesRGB <- loadAll$allFullImagesRGB
rm(loadAll)
    
stillBack <- createBackground3(
                             allFullImages[,,1,],
                             allFullImages[,,2,],
                             allFullImages[,,3,],
                             dim(allFullImages[,,1,]))
allImages <- sapply(1:3, 
                   function(x) 
	                  subtractBackground2(allFullImages[,,x,],
	                  stillBack[,,x],dim(allFullImages[,,x,])),
	               simplify='array')
	  
## Create track object
seqq <- seq(0,0.01,0.00001)
pthres <- seqq[which(diff(quantile(
                          allImages[,,1,1],prob=seqq)) < 1E-10)[1]]
trackObject <- identifyParticles2(allImages,
               pthreshold=pthres,
               pixelRange=c(10,500))

nnData <- createNNdata(particleStats=trackObject$particleStats,channel=1,
                       allFullImagesRGB=allFullImagesRGB,allImages=allImages,
                       frames=frames,mId=NULL,training=FALSE)
particleStats <- updateParticles(n1,testData=nnData$testData,
                                 predictors=predictz,Thr=Thr)
                                 
## Track
records <- doTrack(particleStats=particleStats,
                   plot=FALSE,backward=FALSE,L=100)
records2 <- linkTrajec (G=records$G,trackRecord=records$trackRecord,
                        sizeRecord=records$sizeRecord,label=records$label,
                         R=1,plot=FALSE,L=100)
