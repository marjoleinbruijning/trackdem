##' Manually selection false positives and true positives to create
##' training data. A plot appears, and the user first click on all true
##' positives, and second on all false positives.
##'
##' \code{manuallySelect} is a function to to create training data that
##' by manually selecting false and true positives. The created training
##' data can be implemented in a neural net.
##' @param particleStats A list with particle statistics for each frame.
##' @param colorimages A list with the original full color images, in order to plot
##' on the original images.
##' @param frame A number defining the frame that should be used. Default
##' is NULL; the frame with the maximum number of identified particles is used.
##' @author Marjolein Bruijning & Marco D. Visser
##' @examples
##' \dontrun{
##'
##' manuallySelect(particleStats=trackObject$particleStats,frame=1)
##'
##'	}
##' @seealso \code{\link{identifyParticles}},
##' @return List containing three elements: true positives, false positives,
##' and the evaluated frame.
##' @concept What is the broad searchable concept?
##' @export

manuallySelect <- function (particleStats,colorimages=allFullImagesRGB,
                            frame=NULL) {
  
  nImages <- 1:length(particleStats)
  totx <- nrow(colorimages[[n]])
  toty <- ncol(colorimages[[n]])
  
  if(is.null(frame)){
    n <- which(sapply(nImages,function(X) 
               length(particleStats[[X]]$patchID))==
               max(sapply(nImages,function(X) 
               length(particleStats[[X]]$patchID))))[1]
  } else{n <- frame}
  
  layout(matrix(c(1,2,2,2,2), 1, 5, byrow = TRUE))
  par(mar=c(0,0,0,0))
  plot(100,100,xlab='',ylab='',xaxt='n',yaxt='n',bty='n',xlim=c(0,1),
       ylim=c(0,1))
  polygon(x=c(0,1,1,0,0),y=c(0.7,0.7,0.8,0.8,0.7),col='green')
  text(x=0.5,y=0.75,label='Done',cex=4)
  text(0.5,0.83,label='Correctly identified',cex=1.5)
  polygon(x=c(0,1,1,0,0),y=c(0.5,0.5,0.6,0.6,0.5),col='red')
  text(x=0.5,y=0.55,label='Done',cex=4)
  text(0.5,0.63,label='Wrongly identified',cex=1.5)
  tmp1 <- par('usr') 

  plotRGB(colorimages[[n]],scale=1,bty='n',
	asp=toty/totx)
  points(particleStats[[n]]$x/totx,
       1-particleStats[[n]]$y/toty,col='blue',cex=1.5)
  title("First click on all (or a subset of) correctly identified particles, and press the green button.\n
	Then click on all (or a subset of) wrongly identified particles. Finish by pressing the red button.", 
	line=-4.5,outer = TRUE, cex=1)
  tmp2 <- par('usr') 

  continueCorrect <- TRUE
  correct <- as.numeric()
  while (continueCorrect == TRUE) {
    pick<-locator(1)
    if (pick$x < 0 & pick$y > 0.9 & pick$y < 1.1) {continueCorrect <- FALSE}
    if (pick$x > 0) {
      tmp <- (1-particleStats[[n]]$y/toty - pick$y)^2 + 
             (particleStats[[n]]$x/totx - pick$x)^2
      patches <- particleStats[[n]]$patchID[which(tmp == min(tmp))]
      
      tmp <- particleStats[[n]][particleStats[[n]]$patchID %in% patches,]
      points(tmp$x/totx,1-tmp$y/toty,col='green',cex=2,pch=5)
      correct <- c(correct, tmp$patchID)
    }
  }
  par(mfg=c(1,1)) 
  par(usr=tmp1) 	
  polygon(x=c(-100,1,1,-1,-1),y=c(0.7,0.7,0.8,0.8,0.7),col='grey')
  text(x=0.5,y=0.75,label='Done',cex=4)

  par(mfg=c(1,2)) 
  par(usr=tmp2) 	
  continueWrong <- TRUE
  wrong <- as.numeric()
  while (continueWrong == TRUE) {
    pick<-locator(1)
    if (pick$x < 0 & pick$y > 0.5 & pick$y < 0.7) {continueWrong <- FALSE}
    if (pick$x > 0) {
      tmp <- (1-particleStats[[n]]$y/toty - pick$y)^2 + 
             (particleStats[[n]]$x/totx - pick$x)^2
      patches <- particleStats[[n]]$patchID[which(tmp == min(tmp))]
      
      tmp <- particleStats[[n]][particleStats[[n]]$patchID %in% patches,]
      points(tmp$x/totx,1-tmp$y/toty,col='red',cex=2,pch=4)
      wrong <- c(wrong, tmp$patchID)
    }
  }
  dev.off()
  return(list(wrong=wrong,correct=correct,frame=n))
}



##' Background detection
##'
##' \code{createBackground} is a function to create a still background,
##' exluding movies objects, using loaded image sequences as input.
##' @param allFullImages Array containing all images (and three color layers)
##' @param func Function to be performed over each pixel. Default is NULL,
##' taking mean pixel values.
##' @author Marjolein Bruining & Marco D. Visser
##' @examples
##' \dontrun{
##'
##' stillBack <- createBackground (allFullImages)
##'	}
##' @seealso \code{\link{createBackground}},
##' @return Returns array with still background.
##' @concept What is the broad searchable concept?
##' @export
createBackground <- function (allFullImages,func=NULL) {
  if (is.null(func)) {
	apply(allFullImages,c(1,2,3),function (x) sum(x)/length(x))
  } else {apply(allFullImages,c(1,2,3),func)}
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
##' @seealso \code{\link{createBackground}},
##' @return Returns array with same size as images, subtracted from background.
##' @concept What is the broad searchable concept?
##' @export

subtractBackground <- function (background,images) {
    #a <- apply(images,c(1,2,3),function(x) x - background)
    im <- array(NA,dim=dim(images))
    
    for (i in 1:3) {
          im[,,i,] <- sapply(1:dim(images)[4], function(x)
			images[,,i,x]-background[,,i], simplify='array')
    }
    return(im)
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
    getCoords2(m=allImages[,,x],d=dim(allImages[,,x])))

  particleStats <- lapply(nImages,function(x) {
    rows <- tapply(coords[[x]][,1],allImages[,,x][allImages[,,x]>0],mean)
    cols <- tapply(coords[[x]][,2],allImages[,,x][allImages[,,x]>0],mean)
    particleStats[[x]] <- cbind(particleStats[[x]],
                                 data.frame(x=cols,y=rows))
    return(particleStats[[x]])                            
    }
  )

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



