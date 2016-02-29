##' Load .png images
##'
##' \code{loadImages} is a function to load png images as a three dimensional array.
##' The objects created through the function can be used for image analysis.
##' @param direcPictures The path of the folder where the images can be found.
##' @param filenames Default is null. If not all files should be loaded, here specify
##' which files to use.
##' @param nImages The total number of images to load.
##' @param xranges Default all pixels are loaded; specify to subset the number of columns.
##' @param yranges Default all pixels are loaded; specify to subset the number of rows.
##' @author Marjolein Bruijning & Marco D. Visser
##' @examples
##' \dontrun{
##'
##' loadAll <- loadImages(direcPictures='~/images1/',filenames=NULL,
##'	nImages=1:30,yranges=1:1080,xranges=1:1915)
##'
##'	}
##' @return List two elements: first contains array with all images,
##' subset when relevant. Second element contains all original color images as array.
##' @concept What is the broad searchable concept?
##' @export

loadImages <- function (direcPictures,filenames=NULL,nImages=1:30,
                        xranges=NULL,yranges=NULL) {
  if (is.null(filenames)) {
    allFiles <- list.files(path=direcPictures) # List all files in folder
    allFiles <- allFiles[nImages]
  }
  else {filenames=filenames[nImages]}	
  # Load all images
  allFullImagesRGB <- sapply(nImages,
		function(x) readPNG(paste0(direcPictures,allFiles[x])),
                            simplify='array')

  if (!is.null(xranges) | !is.null(yranges)) {
    allFullImages <- sapply(nImages,function(x) 
		allFullImagesRGB[[x]][yranges,xranges,],simplify='array')	
  } else {allFullImages <- allFullImagesRGB}
 
  allFullImagesRGB <- lapply(nImages,function(x) brick(allFullImagesRGB[,,,x]))
  
  return(list(allFullImages=allFullImages,allFullImagesRGB=allFullImagesRGB))
}

##' Identify moving particles
##'
##' \code{identifyParticles} is a function to identify particles using subtracted
##' background images.
##' @param images Array containing images.
##' @param threshold Threshold for including particles. For a chosen 
##' threshold for each frame, use pthreshold.
##' @param pixelRange Default is NULL. Vector with minimum and maximum particle size, used as a
##' first filter to identify particles.
##' @param pthreshold Default is NULL. If NULL, treshold is used for filter. If
##' not zero, a threshold based on pthreshold quantile is calculated for each
##' frame.
##' @author Marjolein Bruijning & Marco D. Visser
##' @examples
##' \dontrun{
##'
##'   trackObject <- identifyParticles(allImages,pthreshold=0.001,pixelRange=c(3,400))
##'	}
##' @return This function returns a list with two elements: (1) a list with
##' particle statistics with identified particles for each frame. (2) an array
##' containing all binary images.
##' @concept What is the broad searchable concept?
##' @export

identifyParticles <- function (images,threshold=-0.1,pixelRange=NULL,
                               pthreshold=NULL) {
  nImages <- 1:dim(images)[3]
  
  print('Thresholding')
  if(!is.null(pthreshold)){
    allImages <- array(NA,dim=dim(images))
    for (i in 1:3) {
		allImages[,,,i] <- sapply(1:dim(images)[3], function(x)
                           images[,,x,i]<quantile(images[,,x,i],
			               pthreshold),
			               simplify='array')
    }
   
  } else {
    allImages <- images < threshold
  }
  sumRGB <- apply(allImages,c(2,3),rowSums)
  sumRGB <- sumRGB > 0
  print('Labeling particles')
  allImages <- sapply(nImages, function (x) 
	ConnCompLabel(sumRGB[,,x]),simplify='array') # label particles
 
 print('Size filtering')
 if (!is.null(pixelRange)) {
	for (i in nImages) {
		allLabels <- tabulate(as.vector(allImages[,,i]),nbins=max(allImages[,,i]))
		allLabels <- allLabels[allLabels > 0]
		names(allLabels) <- sort(unique(as.numeric(allImages[,,i])))[-1]
		inc <- allLabels >= pixelRange[1] & allLabels <= pixelRange[2]
		allImages[,,i] [!allImages[,,i] %in% names(inc[inc==TRUE])] <- 0
	}
  }
  print('Particle statistics')
  particleStats <- lapply(nImages,function(x) 
	PatchStat(allImages[,,x])[-1,]) # calculate patch statistics
  
  print('Coordinates calculation')
  coords <- lapply(1:dim(allImages)[3],function(x) 
    getCoords(m=allImages[,,x],d=dim(allImages[,,x])))

  particleStats <- lapply(nImages,function(x) {
    rows <- tapply(coords[[x]][,1],allImages[,,x][allImages[,,x]>0],mean)
    cols <- tapply(coords[[x]][,2],allImages[,,x][allImages[,,x]>0],mean)
    #rows <- tapply(coords[[x]][,1],allImages[,,x][allImages[,,x]>0],
     #              function (i) )
    particleStats[[x]] <- cbind(particleStats[[x]],
                                 data.frame(x=cols,y=rows))
    return(particleStats[[x]])                            
    }
  )
  
  print('Major distance calculator')
  for (i in 1:length(particleStats)) {
    if (length(particleStats[[i]]$patchID) > 0) {
      for (X in 1:length(particleStats[[i]]$patchID)) {
        a <- which(allImages[,,i]==particleStats[[i]]$patchID[X],arr.ind=TRUE)
        particleStats[[i]]$majorDist[X] <- maxDist(x=a[,1],y=a[,2])
      }
    }
  }

  # Results
  meanPart <- mean(sapply(nImages,function(X)
	length(particleStats[[X]]$patchID)))
  coeffVar <- sd(sapply(nImages,function(X)
	length(particleStats[[X]]$patchID)) / meanPart)

  attr(particleStats,'Results') <- 
       paste0('Mean number of identified particles equals ',round(meanPart,2),
               '; Coefficient of variation is ',round(coeffVar,3))
  return(list(allImages=allImages,particleStats=particleStats))
}

##' Maximum distance
##'
##' \code{maxDist} is ...
##' @param x x coordinates
##' @param y y coordinates
##' @author Marjolein Bruijning & Marco D. Visser
##' @export
maxDist <- function(x,y) {
  x <- x - min(x)
  y <- y - min(y)
  max(sqrt(x^2 + y^2))
  #mat <- sapply(1:length(y), function(i) sqrt(x^2 + y[i]^2))
  #return(max(mat))
}



