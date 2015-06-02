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
loadImages <- function (direcPictures,filenames=NULL,nImages=1:50,xranges=NULL,yranges=NULL,channel=1) {
	if (is.null(filenames)) {
		allFiles <- list.files(path=direcPictures) # List all files in folder
		allFiles <- allFiles[nImages]
	}
	else {filenames=filenames[nImages]}	
	# Load all images
	allFullImagesRGB <- lapply(nImages,
		function(x) readPNG(paste0(direcPictures,allFiles[x])))
	if (is.null(xranges)) xranges <- 1:ncol(allFullImagesRGB[[1]][,,1])
	if (is.null(yranges)) yranges <- 1:nrow(allFullImagesRGB[[1]][,,1])	
	allFullImages <- sapply(nImages,function(x) allFullImagesRGB[[x]][yranges,xranges,channel],simplify='array')	
	allFullImagesRGB <- lapply(nImages,function (x) brick(allFullImagesRGB[[x]]))
	return(list(allFullImages=allFullImages,allFullImagesRGB=allFullImagesRGB))
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
createBackground <- function (allFullImages) {
	stillBack <- matrix(NA,nrow=dim(allFullImages)[1],ncol=dim(allFullImages)[2])
	for (i in 1:nrow(stillBack)) {
		stillBack[i,] <- apply(allFullImages[i,,],1,median)
	}
	return(stillBack)
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
substractBackground <- function (background,images) {
	images <- sapply(1:dim(images)[3], function(x) images[,,x]-background, simplify='array')
	return(images)
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


