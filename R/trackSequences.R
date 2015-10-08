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
##' @return what does it return?
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
##' @return what does it return?
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
##' \code{loadImages} is a function that (description)
##' The objects created through the function ...
##' @param direcPictures DESCRIPTION
##' @param filenames DESCRIPTION
##' @param nImages DESCRIPTION
##' @param xranges DESCRIPTION
##' @param yranges DESCRIPTION
##' @param channel DESCRIPTION
##' @author Marjolein Bruining & Marco D. Visser
##' @examples
##' \dontrun{
##'
##'  rnorm(1) # give a example code here
##'
##'
##'
##'
##'	}
##' @seealso \code{\link{createBackground}}, \code{\link{summary}},
##' @return what does it return?
##' @concept What is the broad searchable concept?
##' @export
identifyParticles <- function (images,threshold=-0.1,pixelRange=NULL) {
	nImages <- 1:dim(images)[3]
	allImages <- sapply(nImages, function(x) ifelse(images[,,x] > threshold,0,1), simplify='array') # binary images
	allImages <- sapply(nImages, function (x) ConnCompLabel(allImages[,,x]),simplify='array') # label particles
	particleStats <- lapply(nImages,function(x) PatchStat(allImages[,,x])) # calculate patch statistics																																					
	if (!is.null(pixelRange)) {
		particleStats <- lapply(nImages,function(x) particleStats[[x]] [
			particleStats[[x]]$n.cell>pixelRange[1] & particleStats[[x]]$n.cell<pixelRange[2],]) # select particles based on size settings
		# save only particles from patch statistics
		for (i in nImages) {
			allImages[,,i] [(allImages[,,i] %in% particleStats[[i]]$patchID) == F] <- 0
		}
	}
	for (i in nImages) {
		coords <- getCoords(allImages[,,i])
		particleStats[[i]]$x <- coords$x
		particleStats[[i]]$y <- coords$y		
	}
	meanPart <- mean(sapply(nImages,function(X) length(particleStats[[X]]$patchID)))
	coeffVar <- sd(sapply(nImages,function(X) length(particleStats[[X]]$patchID))) / meanPart
	print(paste0('Mean number of identified particles equals ',round(meanPart,2),
		'; Coefficient of variation is ',round(coeffVar,3)))
	
	return(list(allImages=allImages,particleStats=particleStats))
}

##' Informative title
##'
##' \code{loadImages} is a function that (description)
##' The objects created through the function ...
##' @param direcPictures DESCRIPTION
##' @param filenames DESCRIPTION
##' @param nImages DESCRIPTION
##' @param xranges DESCRIPTION
##' @param yranges DESCRIPTION
##' @param channel DESCRIPTION
##' @author Marjolein Bruining & Marco D. Visser
##' @examples
##' \dontrun{
##'
##'  rnorm(1) # give a example code here
##'
##'
##'
##'
##'	}
##' @seealso \code{\link{createBackground}}, \code{\link{summary}},
##' @return what does it return?
##' @concept What is the broad searchable concept?
##' @export
manuallySelect <- function (particleStats,colorimages=allFullImagesRGB) {
	nImages <- 1:length(particleStats)
	n <- which(sapply(nImages,function(X) length(particleStats[[X]]$patchID))==
		max(sapply(nImages,function(X) length(particleStats[[X]]$patchID))))
	
	layout(matrix(c(1,2,2,2,2), 1, 5, byrow = TRUE))
	par(mar=c(0,0,0,0))
	plot(100,100,xlab='',ylab='',xaxt='n',yaxt='n',bty='n',xlim=c(0,1),ylim=c(0,1))
	polygon(x=c(0,1,1,0,0),y=c(0.5,0.5,0.6,0.6,0.5),col='red')
	text(x=0.5,y=0.55,label='Done',cex=4)
	plotRGB(colorimages[[n]],scale=1,bty='n')
	points(particleStats[[n]]$x/1024,1-particleStats[[n]]$y/576,col='green',cex=1.5)
	title("Click on al wrongly identified particles", outer = TRUE, line = -2,cex=3)
	continue <- TRUE
	wrong <- as.numeric()
	while (continue == TRUE) {
		pick<-locator(1)
		if (pick$x < 0) {continue <- FALSE}
		if (pick$x > 0) {
			tmp <- (1-particleStats[[n]]$y/576 - pick$y)^2 + (particleStats[[n]]$x/1024 - pick$x)^2
			patches <- particleStats[[n]]$patchID[which(tmp == min(tmp))]
			
			tmp <- particleStats[[n]][particleStats[[n]]$patchID %in% patches,]
			points(tmp$x/1024,1-tmp$y/576,col='red',cex=2,pch=4)
			wrong <- c(wrong, tmp$patchID)
		}
	}
	dev.off()
	return(wrong)
}
getCoords <- function (mat) {
	x <- rep(NA,length(unique(as.vector(mat))))[-1]
	y <- rep(NA,length(unique(as.vector(mat))))[-1]
	dims <- dim(mat)

	for (i in 1:length(x)) {
		tmp <- mat == unique(as.vector(mat))[-1][i]
		coords <- colMeans(t(t(which(tmp,TRUE))))
		y[i] <- coords[1]
		x[i] <- coords[2]
	}
	return(data.frame(x=x,y=y))
}


##### Functions not in use ##########
overig <- function () {
updateParticles <- function (particleStats,images,repeats=NULL,colorimages=allFullImagesRGB) {
	n <- which(sapply(nImages,function(X) length(particleStats[[X]]$patchID))==
		max(sapply(nImages,function(X) length(particleStats[[X]]$patchID))))
    y<-ids<-unique(particleStats[[n]]$patchID)[-1]
    if(!is.null(repeats)) {y<-ids<-sample(y,repeats,FALSE)}
    dims <- dim(allImages[,,1])
    for(i in 1:length(ids)){
		tmp <- allImages[,,n] == ids[i]
		coords <- colMeans(t(t(which(tmp,TRUE)) / dims))
		plotRGB(allFullImagesRGB[[n]],scale=1)
        points(coords[2],1-coords[1],col='red',cex=1.5)
        y[i]<-scan(n=1)
     }
    names(y) <- ids
    return(y)	
}

trainCreate<-function(number,repeats){
    y<-ids<-unique(particleStats[[number]]$patchID)[-1]
    dims <- dim(allImages[,,1])
    for(i in 1:length(ids)){
		tmp <- allImages[,,number] == ids[i]
		coords <- colMeans(t(t(which(tmp,TRUE)) / dims))
		plotRGB(allFullImagesRGB[[number]],scale=1)
        points(coords[2],1-coords[1],col='red',cex=1.5)
        y[i]<-scan(n=1)
     }
    y
}
}


