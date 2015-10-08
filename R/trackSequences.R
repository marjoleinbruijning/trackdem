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
##' @author Marjolein Bruining & Marco D. Visser
##' @examples
##' \dontrun{
##'
##' loadAll <- loadImages(direcPictures='~/images1/',filenames=NULL,
##'	nImages=1:30,yranges=1:1080,xranges=600:1440)
##'
##'	}
##' @seealso \code{\link{createBackground}}, \code{\link{summary}},
##' @return List two elements: first contains array with all images,
##' subset when relevant. Second element contains all original color images as array.
##' @concept What is the broad searchable concept?
##' @export

loadImages <- function (direcPictures,filenames=NULL,nImages=1:50,xranges=NULL,yranges=NULL) {
  if (is.null(filenames)) {
    allFiles <- list.files(path=direcPictures) # List all files in folder
    allFiles <- allFiles[nImages]
  }
  else {filenames=filenames[nImages]}	
                                        # Load all images
  allFullImagesRGB <- sapply(nImages,
                             function(x) readPNG(paste0(direcPictures,allFiles[x])),simplify='array')

  if (!is.null(xranges) | !is.null(yranges)) {
    allFullImages <- sapply(nImages,function(x) allFullImagesRGB[[x]][yranges,xranges,],simplify='array')	
  } else {allFullImages <- allFullImagesRGB}
  allFullImagesRGB <- lapply(nImages,function(x) brick(allFullImagesRGB[,,,x]))
  return(list(allFullImages=allFullImages,allFullImagesRGB=allFullImagesRGB))
}

##' Background detection
##'
##' \code{createBackground} is a function to create a still background,
##' exluding movies objects, using loaded image sequences as input.
##' @param allFullImages Array containing all images (and three color layers)
##' @param func Function to be performed over each pixel. Default is mean.
##' @author Marjolein Bruining & Marco D. Visser
##' @examples
##' \dontrun{
##'
##'  stillBack <- createBackground (allFullImages,func=mean)
##'	}
##' @seealso \code{\link{createBackground}}, \code{\link{summary}},
##' @return Returns array with still background.
##' @concept What is the broad searchable concept?
##' @export
createBackground <- function (allFullImages,func=mean) {
  apply(allFullImages,c(1,2,3),func)
}

##' Background subtraction
##'
##' \code{subtractBackground} is a function to subtract each
##' image from the created still background.
##' The objects created through the function contain all moving
##' pixels.
##' @param background Array containing still background.
##' @param images Array containing all images.
##' @author Marjolein Bruining & Marco D. Visser
##' @examples
##' \dontrun{
##'
##'  allImages <- subtractBackground(background=stillBack,allFullImages)

##'	}
##' @seealso \code{\link{createBackground}}, \code{\link{summary}},
##' @return Returns array with same size as images, subtracted from background.
##' @concept What is the broad searchable concept?
##' @export
substractBackground <- function (background,images) {
    #a <- apply(images,c(1,2,3),function(x) x - background)
    im <- array(NA,dim=dim(images))
    for (i in 1:3) {
          im[,,i,] <- sapply(1:dim(images)[4], function(x)
                             images[,,i,x]-background[,,i], simplify='array')
    }
    return(im)
}

##' Informative title
##'
##' \code{identifyParticles} is a function to identify particles using subtracted
##' background images.
##' @param images Array containing images.
##' @param threshold Threshold for including particles. For a threshold for each
##' frame, use pthreshold.
##' @param pixelRange Default is zero. Vector with minimum and maximum particle size, used as a
##' first filter to identify particles.
##' @param pthreshold Default is zero. If zero, treshold is used for filter. If
##' not zero, a threshold based on pthreshold quantile is calculated for each
##' frame.
##' @author Marjolein Bruining & Marco D. Visser
##' @examples
##' \dontrun{
##'
##'   trackObject <- identifyParticles(allImages,pthreshold=0.001,pixelRange=c(3,400))
##'	}
##' @seealso \code{\link{createBackground}}, \code{\link{summary}},
##' @return This function returns a list with two elements: (1) a list with
##' particle statistics with identified particles for each frame. (2) an array
##' containing all binary images.
##' @concept What is the broad searchable concept?
##' @export
identifyParticles <- function (images,threshold=-0.1,pixelRange=NULL,
                               pthreshold=NULL) {
  nImages <- 1:dim(images)[4]
  if(!is.null(pthreshold)){
    allImages <- array(NA,dim=dim(images))
    for (i in 1:3) {
          allImages[,,i,] <- sapply(1:dim(images)[4], function(x)
                                    images[,,i,x]<quantile(images[,,i,x],
                                                           pthreshold),
                                    simplify='array')
    }
   
  } else {
    allImages <- images < threshold
  }
  sumRGB <- apply(allImages,c(2,4),rowSums)
  sumRGB <- sumRGB > 0
  print('Labeling particles')
  allImagesRGB <- sapply(nImages, function (x) ConnCompLabel(sumRGB[,,x]),simplify='array') # label particles
  
  print('Particle statistics')
  particleStatsRGB <- lapply(nImages,function(x) PatchStat(allImagesRGB[,,x])) # calculate patch statistics
  particleStats <- particleStatsRGB
  print('Size filtering')
  if (!is.null(pixelRange)) {
                                        #	for (i in 1:4) {
    particleStats <- lapply(nImages, 
                            function (x) subset(particleStats[[x]],n.cell>pixelRange[1] & n.cell<=pixelRange[2]))
                                        #		particleStats[[i]] <- lapply(nImages, 
                                        #			function (x) subset(particleStats[[i]][[x]],n.cell>pixelRange[1] & n.cell<=pixelRange[2]))
                                        #	}
  }
  
  print('Coordinates calculation')
  tmp <- allImagesRGB
  for (i in nImages) {
    tmp[,,i][(tmp[,,i] %in% particleStats[[i]]$patchID)==F] <- 0
  }
  coords <- lapply(nImages,function (x) getCoords(tmp[,,x]))
                                        #coords <- lapply(nImages, function(x) coords[[x]][coords[[x]]$ID %in% particleStats[[x]]$patchID,])
  particleStats <- lapply(nImages, function(x) merge(particleStats[[x]],coords[[x]],by.x='patchID',by.y='ID'))

  ## Calculate difference between each frame
  print('Movement detection')
  detectMovements <- abs(diffFrame(allFullImages))
  detectMovements <- detectMovements > 0.07
  detectMovements <- sapply(1:dim(detectMovements)[3], function (x) 
	ConnCompLabel(detectMovements[,,x]),simplify='array') # label particles
  tmp <- detectMovements
  coordsMovements <- lapply(1:dim(detectMovements)[3],function (x) getCoords(tmp[,,x]))
  
  a <- lapply(1:length(coordsMovements), function (X) 
	calcDist (particleStats[[X]],coordsMovements[[X]],cn=c('x','y')))
  a[[30]] <- matrix(1,ncol=2,nrow=length(particleStats[[30]]$patchID))
  particleStats <- lapply(1:length(a),function(X) 
	cbind(particleStats[[X]],data.frame(movementDist=apply(a[[X]],1,min))))

  # Results
  meanPart <- mean(sapply(nImages,function(X) length(particleStats[[X]]$patchID)))
  coeffVar <- sd(sapply(nImages,function(X) length(particleStats[[X]]$patchID)) / meanPart)

  print(paste0('Mean number of identified particles equals ',round(meanPart,2),
               '; Coefficient of variation is ',round(coeffVar,3)))
  
  return(list(allImages=tmp,particleStats=particleStats))
}




