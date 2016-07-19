##' Create image sequence
##'
##' \code{createImageSeq} is a function to create an image sequence (.png) using 
##' videos file as input. All movies within a directory 'Movies' will
##' be exported. A directory 'ImageSequences' must exist in path. For each
##' movie, a new directory is created containing the recorded date and 
##' name of the movie.
##' @param path Path path to location of directory containing directories
##' 'Movies' and 'ImageSequences'.
##' @param x Number of pixels in horizontal direction
##' @param y Number of pixels in vertical direction
##' @param fps Frames per second, default is 15.
##' @param nsec Duration of movie that is exported, default is 2 seconds.
##' The middle 2 seconds of the movie are used.
##' @param ext The extension of the video, in lower case. Default is 'mts'.
##' @examples
##' \dontrun{
##' createImageSeq(path='~/Data/',ext='mp4')
##'	}
##' @author Marjolein Bruijning & Marco D. Visser
##' @export
createImageSeq <- function (path='~/Data/',x=1915,
                            y=1080,fps=15,nsec=2,ext='mts') {
  ext <- paste0("'.",ext,"'")
  old <- getwd()
  setwd(path)
  fileConn<-file(paste(path,'tmp.py',sep=''))
  writeLines(c(
    paste("import os"),
    paste("import subprocess"),
    paste("import string"),
    paste("movieDirname = 'Movies'"),
    paste("sequenceParentDirname = 'ImageSequences'"),
    paste("def conv_command(filename, targetDirName, start, stop):"),
    paste("    inFile = os.path.join(movieDir, filename)"),
    paste("    outFiles = os.path.join(sequenceParentDir, targetDirName, 'image-%03d.png')"),
    paste("    return ['avconv',"),
    paste("            '-loglevel', 'quiet',"),
    paste("            '-i', inFile,"),
    paste("            '-r', '",fps,"',",sep=''),
    paste("            '-s', '",x,"x",y,"',",sep=''),
    paste("            '-ss', str(start),"),
    paste("            '-t', str(stop - start),"),
    paste("            '-f', 'image2',"),
    paste("            outFiles]"),
    paste("startDir = os.path.curdir"),
    paste("movieDir = os.path.abspath(os.path.join(startDir, movieDirname))"),
    paste("sequenceParentDir = os.path.abspath(os.path.join(startDir, sequenceParentDirname))"),
    paste("movieNames = []"),
    paste("for filename in os.listdir(movieDir):"),
    paste("    movieName, movieExtension = os.path.splitext(filename)"),
    paste("    if not os.path.isfile(os.path.join(movieDir, filename)) or not movieName.isdigit() or not movieExtension.lower() == ",ext,":"),
    paste("        print 'File %s has the wrong name or is a directory, therefore skipped' % filename"),
    paste("    else:"),
    paste("        movieNames.append(filename)"),
    paste("if not os.path.exists(sequenceParentDir):"),
    paste("    os.mkdir(sequenceParentDir)"),
    paste("for filename in movieNames:"),
    paste("    movieName, movieExtension = os.path.splitext(filename)"),
    paste("    try:"),
    paste("        targetDirName = string.translate(subprocess.check_output(['exiftool', '-DateTimeOriginal', '-T', os.path.join(movieDir, filename)]).split()[0], None, ':') + '_' + movieName"),
    paste("    except Exception:"),
    paste("        print 'Error in obtaining date in file %s; therefore skipped' % filename"),
    paste("        continue"),
    paste("    try:"),
    paste("        duration = float(string.translate(subprocess.check_output(['exiftool', '-n', '-s3', '-duration', os.path.join(movieDir, filename)]).split()[0], None, ':'))"),
    paste("    except Exception:"),
    paste("        print 'Error in obtaining duration movie in file %s, therefore skipped' % filename"),
    paste("        continue"),
    paste("    if os.path.exists(os.path.join(sequenceParentDir, targetDirName)):"),
    paste("        print 'Directory %s already exists, file %s skipped' % (targetDirName, filename)"),
    paste("    else:"),
    paste("        if duration >",nsec,":",sep=''),
    paste("            start = duration/2 -",nsec/2,sep=''),
    paste("            stop = duration/2 +",nsec/2,sep=''),
    paste("        else:"),
    paste("            start = 0"),
    paste("            stop = duration"),
    paste("        os.mkdir(os.path.join(sequenceParentDir, targetDirName))"),
    paste("        command = conv_command(filename, targetDirName, start, stop)"),
    paste("        try:"),
    paste("            subprocess.call(command)"),
    paste("            print 'File %s done' % filename"),
    paste("        except Exception, e:"),
    paste("            print 'Error in processing file %s, error: %s' % (filename, e)")
  ), fileConn, sep = "\n")
  close(fileConn) 
  system('python tmp.py')
  file.remove(paste(path,'tmp.py',sep=''))
  setwd(old)
}

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
##' loadAll <- loadImages(direcPictures='~/images1/',filenames=NULL,
##'	nImages=1:30,yranges=1:1080,xranges=1:1915)
##'	}
##' @return Array with all images.
##' @export

loadImages <- function (direcPictures,filenames=NULL,nImages=1:30,
                        xranges=NULL,yranges=NULL) {
  if (is.null(filenames)) {
    allFiles <- list.files(path=direcPictures) # List all files in folder
    allFiles <- allFiles[nImages]
  }
  else {filenames=filenames[nImages]}	
  # Load all images
  allFullImages <- sapply(nImages,
		function(x) readPNG(paste0(direcPictures,allFiles[x])),
                            simplify='array')
  # Subset
  if (!is.null(xranges) | !is.null(yranges)) {
    allFullImages <- sapply(nImages,function(x) 
		allFullImages[[x]][yranges,xranges,],simplify='array')	
  }

  attr(allFullImages, "class") <- c('colorimage','array')
  return(allFullImages)
}

##' Background detection
##'
##' \code{createBackground} is a function to create a still background,
##' containing all motionless, by taking
##' mean pixel values over all frames.
##' @param colorimages Array containing all frames, obtained by 
##' \code{\link{loadImages}}
##' @author Marjolein Bruijning & Marco D. Visser
##' @examples
##' \dontrun{
##' stillBack <- createBackground (allFullImages)
##'	}
##' @return Array with still background.
##' @export

createBackground <- function(colorimages) {
  A <- cb(allFullImages[,,1,],
     allFullImages[,,2,],
     allFullImages[,,3,],
     dim(allFullImages[,,1,]))
  class(A) <- c('colorimage','array')
  return(A)
}

##' Background subtraction
##'
##' \code{subtractBackground} is a function to subtract each
##' image from the created still background.
##' The objects created through the function contain all changing
##' pixels.
##' @param bg Array containing still background, as returned from
##' \code{\link{createBackground}}.
##' @param colorimages Array containing all frames, obtained by 
##' \code{\link{loadImages}}
##' @author Marjolein Bruijning & Marco D. Visser
##' @examples
##' \dontrun{
##' allImages <- subtractBackground(stillBack,allFullImages) }
##' @return Returns array with same size as images, subtracted from background.
##' @export

subtractBackground <- function (bg,colorimages) {
  sapply(1:3, function(x) sb(colorimages[,,x,],
	                         bg[,,x],
	                         dim(colorimages[,,x,])),
	     simplify='array')
}

##' Identify moving particles
##'
##' \code{identifyParticles} is a function to identify particles.
##' @param mSub Array containing images containing all moving particles,
##' as obtained by \code{\link{subtractBackground}}
##' @param threshold Threshold for including particles. For a chosen 
##' threshold for each frame, use pthreshold.
##' @param pixelRange Default is NULL. Vector with minimum and maximum particle size, used as a
##' first filter to identify particles.
##' @param pthreshold Default is NULL. If NULL, treshold is used for filter. If
##' not zero, a threshold based on pthreshold quantile is calculated for each
##' frame.
##' @param select Use values smaller ('negative') or larger ('positive') then 
##' threshold. Default is 'negative'.
##' @author Marjolein Bruijning & Marco D. Visser
##' @examples
##' \dontrun{
##'   trackObject <- identifyParticles(allImages,threshold=-0.1,pixelRange=c(3,400))
##'	}
##' @return This function returns a list with two elements: (1) a list with
##' particle statistics with identified particles for each frame. (2) An array
##' containing all binary images.
##' @export

identifyParticles <- function (mSub,threshold=-0.1,pixelRange=NULL,
                               pthreshold=NULL,select='negative') {
  n <- 1:dim(mSub)[3]
  print('Thresholding')
  if(!is.null(pthreshold)) {
    A <- array(NA,dim=dim(mSub))
    for (i in 1:3) {
		A[,,,i] <- sapply(1:dim(mSub)[3], function(x)
                                       mSub[,,x,i] < quantile(mSub[,,x,i],
			                                                  pthreshold),
			              simplify='array')
    }
   
  } else {
    if (select == 'negative') A <- mSub < threshold
    else if (select == 'positive') A <- mSub > threshold
  }
  sumRGB <- apply(A,c(2,3),rowSums)
  sumRGB <- sumRGB > 0
  print('Labeling particles')
  A <- sapply(n, function (x) ConnCompLabel(sumRGB[,,x]),simplify='array')
 
 print('Size filtering')
 if (!is.null(pixelRange)) {
	for (i in n) {
		allLabels <- tabulate(as.vector(A[,,i]),nbins=max(A[,,i]))
		allLabels <- allLabels[allLabels > 0]
		names(allLabels) <- sort(unique(as.numeric(A[,,i])))[-1]
		inc <- allLabels >= pixelRange[1] & allLabels <= pixelRange[2]
		A[,,i] [!A[,,i] %in% names(inc[inc==TRUE])] <- 0
	}
  }
  print('Particle statistics')
  particleStats <- lapply(n,function(x) PatchStat(A[,,x])[-1,])
  
  print('Coordinates calculation')
  coords <- lapply(1:dim(A)[3],function(x) getCoords(m=A[,,x],d=dim(A[,,x])))

  particleStats <- lapply(n,function(x) {
    rows <- tapply(coords[[x]][,1],A[,,x][A[,,x]>0],mean)
    cols <- tapply(coords[[x]][,2],A[,,x][A[,,x]>0],mean)
    particleStats[[x]] <- cbind(particleStats[[x]],
                                 data.frame(x=cols,y=rows))
    return(particleStats[[x]])                            
    }
  )
  attr(particleStats,"images") <- A
  attr(particleStats, "class") <- "particleStatObject"
  return(particleStats)
}

