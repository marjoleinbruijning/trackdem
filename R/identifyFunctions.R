##' Create image sequence
##'
##' \code{createImageSeq} creates an image sequences (.png) using
##'  video files as input. All movies within a directory will  
##' be converted into an image sequence.
##' For each movie, a new directory is created containing the recorded date and 
##' name of the movie.
##' @param moviepath Path to existing directory containing the video files.
##' By default, 'Movies' is used.
##' @param imagepath Path to location of a directory in which image 
##' sequences should
##' be saved. By default, 'ImageSequences' is used (and created if not existing).
##' @param x Number of pixels in horizontal direction; default is 1920 (HD).
##' @param y Number of pixels in vertical direction; default is 1080 (HD).
##' @param fps Frames per second, default is 15.
##' @param nsec Duration of movie that is exported, default is 2 seconds.
##' When movie length is greater than \code{nsec}, the \code{nsec} seconds 
##' in the exact middle of the movie are exported.  
##' @param start Start time (in seconds) from where the video is converted (optional). By 
##' default, the \code{nsec} middle second of the video are used. If a a start 
##' time is specified and no stop time, \code{nsec} seconds starting from \code{start} 
##' are converted.
##' @param stop End time (in seconds) from where the video is converted (optional). By 
##' default, the \code{nsec} middle second of the video are used. When an end time 
##' but no start time are specified, conversion starts at \code{nsec} seconds before 
##' \code{end}.
##' @param ext The extension of the video. Default is \code{'MTS'}. All
##' formats supported by libav are accepted. To convert videos with different 
##' extensions, use for example \code{c('MTS','mp4')}.
##' @param libavpath Path to location where the executable file for libav
##' can be found (named 'avconv.exe'), in case it is not found automatically, 
##' e.g. \code{'C:/Users/libav/usr/bin/avconv.exe'}.
##' @param exiftoolpath Path to location where the executable file for 
##' ExifTool can be found, in case it is not found automatically. 
##' For instance, use \code{'exiftool(-k).exe'}, if located in the working 
##' directory. 
##' @param pythonpath Path to location where the executable file for 
##' Python 2.7 can be found, in case it is not found automatically. For 
##' instance, use \code{'C:/Python27/python.exe'}.
##' @param verbose Logical. By default FALSE. Set to TRUE will print additional information. 
##' @param logfile Logical. By default FALSE. Set to TRUE will create a log file in the
##' working directory. 
##' @author Marjolein Bruijning, Caspar A. Hallmann & Marco D. Visser
##' @examples
##' \dontrun{
##' createImageSeq(moviepath="Movies",imagepath="ImageSequences",
##'                nsec=3,ext="AVI")
##'	}
##' @export
createImageSeq <- function (moviepath='Movies',
                            imagepath='ImageSequences',
                            x=1920,
                            y=1080,
                            fps=15,
                            nsec=2,
                            start=NULL,
                            stop=NULL,
                            ext='MTS',
                            libavpath='avconv',
                            exiftoolpath='exiftool',
                            pythonpath='python',
                            verbose=FALSE,
                            logfile=FALSE)
                    {
                    	if (is.null(moviepath)) {
                    		moviepath = 'NULL'
                    	}
                    	if (is.null(imagepath)) {
                    		imagepath = 'NULL'
                    	}
                    	if (is.null(x)) {
                    		x = 'NULL'
                    	}
                    	if (is.null(y)) {
                    		y = 'NULL'
                    	}
                    	if (is.null(fps)) {
                    		fps = 'NULL'
                    	}
                    	if (is.null(nsec)) {
                    		nsec = 'NULL'
                    	}
                    	if (is.null(start)) {
                    		start = 'NULL'
                    	}
                    	if (is.null(stop)) {
                    		stop = 'NULL'
                    	}
                    	if (is.null(ext)) {
                    		ext = list('NULL')
                    	}
                    	if (is.null(verbose)) {
                    		verbose = 'NULL'
                    	}
                    	if (is.null(logfile)) {
                    		logfile = 'NULL'
                    	}
						system(paste(
							pythonpath,
               				system.file("python", "createImageSeq.py", package = "trackdem"),
							"-moviepath", moviepath,
							"-imagepath", imagepath,
							"-x", x,
							"-y", y,
							"-fps", fps,
							"-nsec", nsec,
							"-start", start,
							"-stop", stop,
							"-ext", paste(ext, collapse=' '),
							"-libavpath", libavpath,
							"-exiftoolpath", exiftoolpath,
							"-verbose", verbose,
							"-logfile", logfile
							))
}

##' Load .png images
##'
##' \code{loadImages} loads png images as three dimensional arrays.
##' The objects created through the function can be used for image analysis.
##' @param dirPictures The path of the folder where the images can be found.
##' @param filenames Default is \code{NULL}, or all files. If all files should 
##' NOT be loaded, here specify which files to use, as a character string.
##' @param nImages Numeric vector specifying which images in the directory 
##' should be loaded; default is \code{1:30}.
##' @param xranges By default the full image is loaded; specify to subset the 
##' number of columns.
##' @param yranges By default the full image is loaded; specify to subset the 
##' number of rows.
##' @examples
##' \dontrun{
##' dir.create("images")
##' ## Create image sequence
##' traj <- simulTrajec(path="images",
##'                     nframes=30,nIndividuals=20,domain="square",
##'                     h=0.01,rho=0.9,
##'                     sizes=runif(20,0.004,0.006))
##' ## Load images
##' dir <- "images"
##' allFullImages <- loadImages (dirPictures=dir,nImages=1:30)
##' plot(allFullImages)
##'	}
##' @return Array of class 'TrDm' and 'colorimages' containing all loaded images.
##' @author Marjolein Bruijning, Caspar A. Hallmann & Marco D. Visser
##' @export

loadImages <- function (dirPictures,filenames=NULL,nImages=1:30,
                        xranges=NULL,yranges=NULL) {

    if (is.null(dirPictures)) {
		dirPictures <- getwd()
	}
	
    if (is.null(filenames)) {
      allFiles <- list.files(path=dirPictures) # List all files in folder
      allFiles <- allFiles[nImages]
    }
    else {filenames <- filenames[nImages]}	
    # First load one to get dimensions
    im1 <- png::readPNG(file.path(dirPictures,allFiles[1]))

    # if only one color layer
    if (length(dim(im1)) == 2) {
      im1 <- array(im1,dim=c(nrow(im1),ncol(im1),1))
    }
    
    nc <- dim(im1)[3] ## number of color channels
    
    # Subset
    if (is.null(xranges)) xranges <- 1:dim(im1)[2]
    if (is.null(yranges)) yranges <- 1:dim(im1)[1]
    im1 <- im1[yranges,xranges,,drop=FALSE]	

    # Then load all images
    allFullImages <- structure(vapply(seq_along(nImages),
                       function(x) {
                                 im <- png::readPNG(file.path(dirPictures,
                                                              allFiles[x]))
                                 if (length(dim(im)) == 2) im <- array(im,dim=c(nrow(im),ncol(im),1))
                                 im <- im[yranges,xranges,]

                                }, numeric(prod(dim(im1)))),
                               dim=c(dim(im1),length(nImages)))

    if (dim(allFullImages)[3] == 3) {
    } else if (dim(allFullImages)[3] == 4) {
      if (all(allFullImages[,,4,] == 1)) {
        warning("Color channel 4 (for transparency) is safely ignored as all 
                 values equaled 1.")
      } else {
        warning("Color channel 4 (for transparency) is ignored. Note that
                 not all values equaled 1.")
      }
      allFullImages <- allFullImages[,,1:3,]
    } else if (dim(allFullImages)[3] == 1) {
        warning("Only 1 color channel detected (grey scale image).")
    } else {
      stop("Wrong number of color channels; only 1 and 3 color layers are
            supported.")
    }

    attr(allFullImages,"settings") <- list(nImages=nImages)
    attr(allFullImages, "class") <- c('TrDm','colorimage','array')
    attr(allFullImages, "originalDirec") <- dirPictures
    return(allFullImages)
}


##' Background detection
##'
##' \code{createBackground} detects the still background,
##' containing all motionless pixels (non particles). Three different methods 
##' to detect the background can be used.
##' @param colorimages Array of class 'TrDm' containing all images, obtained by 
##' \code{\link{loadImages}}.
##' @param method Use \code{method='mean'} to calculate the mean value for each
##' pixel and color.
##' Use \code{method='powerroot'} to deflate dark values (note, this can only be 
##' used for dark particles on a light background). Use \code{method='filter'} to 
##' replace pixels in which movement has occurred with the mode of neighboring 
##' values.
##' Note that \code{method='filter'} is computationally more intensive. 
##' @author Marjolein Bruijning, Caspar A. Hallmann & Marco D. Visser
##' @examples
##' \dontrun{
##' dir.create("images")
##' ## Create image sequence
##' traj <- simulTrajec(path="images",
##'                     nframes=30,nIndividuals=20,domain="square",
##'                     h=0.01,rho=0.9,
##'                     sizes=runif(20,0.004,0.006))
##' ## Load images
##' dir <- "images"
##' allFullImages <- loadImages (dirPictures=dir,nImages=1:30)
##' stillBack <- createBackground(allFullImages,method="mean")
##' plot(stillBack)
##'	}
##' @return Array of class 'TrDm' and 'colorimage' containing detected background.
##' @export
createBackground <- function(colorimages,method='mean') {
    if(!is.TrDm(colorimages)){
        stop("Input does not appear to be of the class \"TrDm\"")
    }
    if (method == 'mean') {
      if (dim(colorimages)[3] == 3) {
        A <- cb(colorimages[,,1,],
                colorimages[,,2,],
                colorimages[,,3,],
                dim(colorimages[,,1,]),
                array(0,dim=dim(colorimages[,,,1])))
      } else if (dim(colorimages)[3] == 1) {
        A <- cb1(colorimages[,,1,],
                dim(colorimages[,,1,]),
                array(0,dim=dim(colorimages[,,,1])))
      }
    } else if (method == 'powerroot') {
      A <- array(NA,dim=dim(colorimages)[1:3])
      for (i in 1:dim(colorimages)[3]) {
        A[,,i] <- apply(colorimages[,,i,],c(1,2),function(x) mean(x^50)^(1/50))
      }

    } else if (method == 'filter') {

        if (dim(colorimages)[3] == 3) {
          rst <-  cb(colorimages[,,1,],
                     colorimages[,,2,],
                     colorimages[,,3,],
                     dim(colorimages[,,1,]),
                     array(0,dim=dim(colorimages[,,,1])))
        } else if (dim(colorimages)[3] == 1) {
          rst <-  cb1(colorimages[,,1,],
                     dim(colorimages[,,1,]),
                     array(0,dim=dim(colorimages[,,,1])))
        }
      
        class(rst) <- class(colorimages)
        subs <- aperm(subtractBackground(bg=rst,colorimages),c(1,2,4,3))
        class(subs) <- class(colorimages)
        attributes(subs) <- attributes(colorimages)
        
        if (dim(colorimages)[3] == 3) {
          SDs <- cb(subs[,,1,]^2,
                   subs[,,2,]^2,
                   subs[,,3,]^2,
                   dim(subs[,,1,]),array(0,dim=dim(colorimages[,,1,]))) * 
                      dim(colorimages)[4]/(dim(colorimages)[4]-1)
          SD <- sqrt(SDs[,,1]^2+SDs[,,2]^2+SDs[,,3]^2)
        } else if (dim(colorimages)[3] == 1) {
          SDs <- cb1(subs[,,1,]^2,
                     dim(subs[,,1,]),array(0,dim=dim(colorimages[,,1,]))) * 
                       dim(colorimages)[4]/(dim(colorimages)[4]-1)
          SD <- sqrt(SDs[,,1]^2)
        }
                

        Threshold <-
         {
	     stats::optimize(function(a,i1){
	     t1 <- (i1 > a)
	     sum((i1[t1]-mean(i1[t1]))^2)+
	     sum((i1[!t1]-mean(i1[!t1]))^2)
	     }
	     ,c(0,max(SD)),i1=c(SD))$min	
	     }
  
         rst[(SD>Threshold)] <- NA

         A <- array(0,c(dim(colorimages)[1:3]))

         for (i in 1:dim(colorimages)[3]) {
           r <- raster::raster(rst[,,i],xmx=ncol(rst),ymx=nrow(rst))	

           f1 <- raster::focal(r,w=matrix(1,31,31),
                               fun=function(x){ raster::modal(x,na.rm=TRUE) },
                               NAonly=TRUE)
           while((!all(is.finite(f1@data@values)))|any(is.na(f1@data@values))) {
             f1@data@values[!is.finite(f1@data@values)] <- NA
             f1 <- raster::focal(f1,w=matrix(1,31,31),
                                 fun=function(x){ raster::modal(x,na.rm=TRUE) },
                                 NAonly=TRUE,pad=TRUE)
           }
           A[,,i] <- matrix(f1[,,1],ncol=ncol(colorimages),
                            nrow=nrow(colorimages),byrow=TRUE)
         }
        
    } else {stop('No valid method for creating background')}

    class(A) <- c('TrDm','colorimage','array')
    attr(A,"originalImages") <- deparse(substitute(colorimages))
    attr(A,"originalDirec") <- attributes(colorimages)$originalDirec
    attr(A,"settings") <- c(attributes(colorimages)$settings,
                            list(BgMethod=method))
    return(A)
}



##' Background subtraction
##'
##' \code{subtractBackground} subtracts each image from a
##'  previously created still background.
##' The objects created through the function contain all changing
##' pixels (i.e. movement).
##' @param bg Array containing still background, as returned from
##' \code{\link{createBackground}}.
##' @param colorimages Array containing all frames, obtained by 
##' \code{\link{loadImages}}. Default is \code{NULL}, in this case the original
##' images are used from the global environment.
##' @author Marjolein Bruijning, Caspar A. Hallmann & Marco D. Visser
##' @examples
##' \dontrun{
##' dir.create("images")
##' ## Create image sequence
##' traj <- simulTrajec(path="images",
##'                     nframes=30,nIndividuals=20,domain="square",
##'                     h=0.01,rho=0.9,
##'                     sizes=runif(20,0.004,0.006))
##' ## Load images
##' dir <- "images"
##' allFullImages <- loadImages (dirPictures=dir,nImages=1:30)
##' stillBack <- createBackground(allFullImages,method="mean")
##' allImages <- subtractBackground(stillBack)
##' plot(allImages)
##'	}
##' @return Returns array of class 'TrDm' and 'sbg' with same size as images, 
##' subtracted from background.
##' @export

subtractBackground <- function (bg,colorimages=NULL) {

    if(is.null(colorimages)) { colorimages <- get(attributes(bg)$originalImages,
                                                  envir=.GlobalEnv) }

    if(!is.TrDm(bg)) {
      ch1 <- all(dim(bg)[1:3] == dim(colorimages)[1:3])
      ch2 <- length(dim(bg)) %in% 3:4
      ch3 <- TRUE
      if (length(dim(bg)) == 4) ch3 <- dim(bg)[4] == dim(colorimages)[4]
      if (!all(ch1,ch2,ch3)) {
        stop(paste("Input does not appear to be of the class \"TrDm\"",
              "and has wrong dimensions."))
      } else {
          message(paste("Input does not appear to be of the class \"TrDm\" but", 
                  "has the correct dimensions and is therefore used."))
      }
    }
    
    if (length(dim(bg)) == 3) { # one background
      sbg <- array(NA,dim=dim(colorimages))
      for (i in 1:(dim(colorimages)[3])) {
        sbg[,,i,] <- sb(colorimages[,,i,],
                        bg[,,i],
                        dim(colorimages[,,i,]),
                        array(0,dim=dim(colorimages)))
      }
    } else if (length(dim(bg)) == 4) { # dynamic background
      sbg <- array(NA,dim=dim(colorimages))
      for (i in 1:(dim(colorimages)[3])) {
        sbg[,,i,] <- sb2(colorimages[,,i,],
                        bg[,,i,],
                        dim(colorimages[,,i,]),
                        array(0,dim=dim(colorimages)))
      }
    } else {stop("Wrong dimensions for background image.")} 
            
    attr(sbg,"background") <- deparse(substitute(bg))
    attr(sbg,"originalImages") <- attributes(bg)$originalImages
    attr(sbg,"originalDirec") <- attributes(bg)$originalDirec
    attr(sbg,"settings") <- attributes(bg)$settings
    
    class(sbg) <- c('TrDm','sbg','array')
    return(sbg)
}

##' Identify moving particles
##'
##' \code{identifyParticles} identifies moving particles using the 
##' subtracted images obtained from \code{\link{subtractBackground}}.
##' @param sbg Array containing images containing all moving particles,
##' as obtained from \code{\link{subtractBackground}}.
##' @param threshold Thresholds for including particles. A numeric vector
##' containing three values; one for each color. Otherwise, supply one value 
##' which is to be used for all three colors. For a chosen quantile
##' for each frame, use \code{qthreshold}. Default is \code{threshold=-0.1}, 
##'  which works for dark particles on a light background. Alternatively,
##' set \code{autoThres} below for an automatic threshold.  
##' @param pixelRange Default is \code{NULL}. Numeric vector with minimum and 
##' maximum particle size, used as a
##' first filter to identify particles. Use if particle of interest are of a 
##' known size range (in pixels).
##' @param qthreshold Default is \code{NULL}. Supply a value, to do thresholding 
##' based on quantile. Quantile is calculated for each
##' frame separately.
##' @param select Select dark particles (\code{'dark'}), light particles 
##' (\code{'light'}), or both (\code{'both'}), compared to background. 
##' Default is \code{'dark'}.
##' @param colorimages Array containing original color images. By default, the 
##' original color images are obtained from global environment.
##' @param autoThres Logical. \code{TRUE} to get an automated threshold for each 
##' color layer. Default is \code{FALSE}.
##' @param perFrame Logical. If \code{autoThres=TRUE}, set at \code{TRUE}
##'  to calculate a threshold for 
##' each frame separately. Default is \code{FALSE}. Note that is can be 
##' computationally intensive to calculate a threshold for each frame.
##' @param frames When \code{autoThres=TRUE} and \code{allFrames=FALSE}, supply a
##' numeric vector specifying over which frames the automated threshold 
##' should be calculated on (e.g. \code{c(1,3,5,7,9,11)} for all odd frames 
##' from 1 to 11). 
##' @author Marjolein Bruijning, Caspar A. Hallmann & Marco D. Visser
##' @examples
##' \dontrun{
##' dir.create("images")
##' ## Create image sequence
##' traj <- simulTrajec(path="images",
##'                     nframes=30,nIndividuals=20,domain="square",
##'                     h=0.01,rho=0.9,
##'                     sizes=runif(20,0.004,0.006))
##' ## Load images
##' dir <- "images"
##' allFullImages <- loadImages (dirPictures=dir,nImages=1:30)
##' stillBack <- createBackground(allFullImages,method="mean")
##' allImages <- subtractBackground(stillBack)
##' partIden <- identifyParticles(allImages,threshold=-0.1,
##'                                    pixelRange=c(3,400))
##' plot(partIden)
##' summary(partIden)
##'	}
##' @return Returns a dataframe of class 'TrDm' and 'particles', containing
##' particle statistics with identified particles for each frame
##' @export

identifyParticles <- function (sbg,threshold=-0.1,pixelRange=NULL,
                               qthreshold=NULL,select='dark',
                               colorimages=NULL,autoThres=FALSE,
                               perFrame=FALSE,frames=NULL) {

    if(!is.TrDm(sbg)){
        stop("Input does not appear to be of the class \"TrDm\"")
    }
    if(is.null(colorimages)) { colorimages <- get(attributes(sbg)$originalImages,
                                                  envir=.GlobalEnv) }
    namesbg <- deparse(substitute(sbg)) ## save for return
    tmp <- attributes(sbg)
    sbg <- aperm(sbg, c(1,2,4,3))
    attributes(sbg)$background <- tmp$background
    attributes(sbg)$originalImages <- tmp$originalImages
    attributes(sbg)$originalDirec <- tmp$originalDirec
    attributes(sbg)$settings <- tmp$settings
        
    cat("\t Particle Identification:  ")
    n <- 1:dim(sbg)[3]
    nc <- dim(sbg)[4]
    cat("\r \t Particle Identification: Thresholding (1 of 5)                 ",
        "           ")

    # if only one value supplied, use same value for each color layer
    if (length(threshold) == 1) {threshold <- rep(threshold,nc)}

    # automated threshold
    if (autoThres) {
      cat("\r \t Particle Identification: Automated thresholding (1 of 5)     ",
          "       ")
	  if (is.null(frames)) { frames <- n }
      threshold <- calcAutoThres(sbg[,,frames,,drop=FALSE],perFrame=perFrame)
	}
    
    if (!is.null(qthreshold)) {
        A <- array(NA,dim=dim(sbg))
        for (i in 1:nc) { # over color layers
            A[,,,i] <- structure(vapply(seq_len(dim(sbg)[3]), function(x)
                sbg[,,x,i] < stats::quantile(sbg[,,x,i],qthreshold),
                numeric(prod(dim(sbg[,,1,1])))),dim=dim(sbg)[1:3])
        }
    } else {
        if (select == 'dark') {
	        A <- structure(vapply(seq_along(threshold), 
                                function(x) sbg[,,,x] < threshold[x],
	                            numeric(prod(dim(sbg[,,,1])))),
	                     dim=dim(sbg))
		}
        else if (select == 'light') {
	        A <- structure(vapply(seq_along(threshold), 
                                function(x) sbg[,,,x] > threshold[x],
	                            numeric(prod(dim(sbg[,,,1])))),
	                     dim=dim(sbg))
        }
        else if (select == 'both') {
	        A <- structure(vapply(seq_along(threshold), 
                                function(x) sbg[,,,x] > threshold[x] |
                                                       sbg[,,,x] < -threshold[x],
	                             numeric(prod(dim(sbg[,,,1])))),
	                     dim=dim(sbg))

        }
        else {stop("Invalid selection, choose 'dark', 'light' or 'both' ")}
    }
    
    # 1 if 1 in at least one color layer
    sumRGB <- apply(A,c(2,3),rowSums)
    sumRGB <- sumRGB > 0

    cat("\r \t Particle Identification: Labeling (2 out of 5)                ",
        "     ")
    A <- array(NA,dim=dim(sumRGB))
    for (i in n) {
      A[,,i] <- SDMTools::ConnCompLabel(sumRGB[,,i])
    }
        
    cat("\r \t Particle Identification: Size filtering (3 out of 5)           ",
       "    ")
    if (!is.null(pixelRange)) {
	  for (i in n) {
        allLabels <- tabulate(as.vector(A[,,i]),nbins=max(A[,,i]))
        allLabels <- allLabels[allLabels > 0]
        names(allLabels) <- sort(unique(as.numeric(A[,,i])))[-1]
        inc <- allLabels >= pixelRange[1] & allLabels <= pixelRange[2]
        A[,,i] [!A[,,i] %in% names(inc[inc==TRUE])] <- 0
	  }
    }

    cat("\r \t Particle Identification: Particle statistics (4 out of 5)      ",
        "    ")

    dA <- dim(A[,,1]) ## get dim
    ## get total number of particles
    totalPart <- apply(A,3,function(x) length(unique(c(x)))-1)
    
    if (all(totalPart == 0)) {
       cat("\n")
       stop(c("\n \t No particle detected in any of the frames. ",
              "Try different thresholds? \n"))
    }
    
    psDim <- SDMTools::PatchStat(A[,,1])[-1,]
    
    particleStats <- matrix(NA,nrow=sum(totalPart),ncol=18)
    colnames(particleStats) <- c(colnames(psDim),
                                 paste0('mu',c('R','G','B')),'x','y','frame')
    
    for (i in n) {
       if (i == 1) loc <- 1:totalPart[i]
       if (i > 1) {
         cum <- sum(totalPart[1:(i-1)])+1
         loc <- cum:(cum+totalPart[i]-1)
       }
       particleStats[loc,1:12] <- as.matrix(SDMTools::PatchStat(A[,,i])[-1,])
       
       particleStats[loc,13:15] <- as.matrix(
                              extractMean(particleStats[loc,'patchID'],
                              colorimages=colorimages[,,,i],
                              images=A[,,i],ncolors=nc))
                              
	     coords <- getCoords(m=A[,,i],d=dA)
       ind <- A[,,i] > 0
       particleStats[loc,'y'] <- tapply(coords[,1],A[,,i][ind],mean)
       particleStats[loc,'x'] <- tapply(coords[,2],A[,,i][ind],mean)
       particleStats[loc,'frame'] <- i
    }    
    particleStats <- as.data.frame(particleStats)
    
    cat("\r \t Particle Identification: Finalizing (5 out of 5)               ",
        "        \n ")
    
    attr(particleStats,"images") <- A
    attr(particleStats, "class") <- c("TrDm","particles","data.frame")
    attr(particleStats,"threshold") <- threshold
    attr(particleStats,"background") <- attributes(sbg)$background
    attr(particleStats,"originalImages") <- attributes(sbg)$originalImages
    attr(particleStats,"originalDirec") <- attributes(sbg)$originalDirec
    attr(particleStats,"subtractedImages") <- namesbg
    
    attr(particleStats,"settings") <- c(attributes(sbg)$settings,
                                        list(threshold=threshold,
                                             pixelRange=pixelRange,
                                             qthreshold=qthreshold,
                                             select=select,
                                             autoThres=autoThres,
                                             perFrame=perFrame,
                                             frames=frames))
    attr(particleStats,"nn") <- FALSE
    
    return(particleStats)
}

## Calculate automated threshold.
##
## @param sbg Array of class 'TrDm' containing subtracted images.
## @param perFrame If TRUE, threshold is calculated for each frame. Default is
## FALSE. 

calcAutoThres <- function(sbg,perFrame=FALSE){

  ncolours <- dim(sbg)[4]
  nframes <- dim(sbg)[3]

  f <- function(a){
    t1 <- (i1 > a)
    sum((i1[t1]-mean(i1[t1]))^2) + sum((i1[!t1]-mean(i1[!t1]))^2)
  }

  if (perFrame) {
    pp <- array(0,c(nframes,ncolours))
    for (j in 1:nframes) {
      for (i in 1:ncolours) {
        i1 <- c(sbg[,,j,i])
        pp[j,i] <- stats::optimize(function(a) {
                   t1 <- (i1 > a) + 1
                   sum((i1[t1==1]-mean(i1[t1==1]))^2) +
                       sum((i1[t1==2]-mean(i1[t1==2]))^2)
                    }, range(i1))$min
      }
    }
  } else {
    pp <- numeric(ncolours)
    for (i in 1:ncolours) {
      i1 <- c(sbg[,,,i])
      pp[i] <- stats::optimize(f,range(i1))$min
    }
  }

 return(pp)
}


##' Find threshold
##'
##' This function can help to find a threshold value to distinguish noise from 
##' particles of interest.
##' @param images Array containing images containing all moving particles,
##' as obtained from \code{\link{subtractBackground}}.
##' @param colorimages Array containing original color images. By default, 
##' the original color images are obtained from the global environment.
##' @param frame Number specifying which frame to use. Default is frame 1.
##' @author Marjolein Bruijning, Caspar A. Hallmann & Marco D. Visser
##' @examples
##' \dontrun{
##' dir.create("images")
##' ## Create image sequence
##' traj <- simulTrajec(path="images",
##'                     nframes=30,nIndividuals=20,domain="square",
##'                     h=0.01,rho=0.9,
##'                     sizes=runif(20,0.004,0.006))
##' ## Load images
##' dir <- "images"
##' allFullImages <- loadImages (dirPictures=dir,nImages=1:30)
##' stillBack <- createBackground(allFullImages,method="mean")
##' allImages <- subtractBackground(stillBack)
##' thr <- findThreshold(allImages,frame=10)
##'	}
##' @return Returns the number that is interactively chosen by the user. Use 
##' this threshold value in \code{identifyParticles}.
##' @export
findThreshold <- function (images,frame=1,colorimages=NULL) {

  if(is.null(colorimages)) {
    colorimages <- get(attributes(images)$originalImages,
                       envir=.GlobalEnv) 
  }

  thr <- runThreshold(frame,images,colorimages)
  return(thr)
}

runThreshold <- function(frame,images,colorimages,ui,server) {
shiny::runApp(
  list(ui=shiny::fluidPage(
  shiny::titlePanel(paste0("Find threshold for particle detection")),
  shiny::fluidRow(
     shiny::column(3,
     'The graph on the right shows the original image that can be used to zoom, by 
     drawing a polygon. Bottom graphs show the original image (left) and 
      a binary image (right) in which all particles are either labeled as zero (white), 
      or as one (black). Set whether particles are light or dark compared to the 
      background. Use the slider to find the best threshold, that results in all 
      focal particles being labeled as one, and all background pixels labeld as 
      zero. When finished, click on "Done".',
      align='left',offset=0),
      shiny::column(2,
       shiny::actionButton("stop", "Done"),
       shiny::br(),shiny::br(),
       shiny::radioButtons("select", shiny::h5("Select:"),
                   choices = list("Dark particles" = 1, 
                                   "Light particles" = 2,
                                   "Both" = 3),selected = 1),   
       shiny::radioButtons("ch", shiny::h5("Color channel:"),
                   choices = list("Red" = 1, 
                                   "Green" = 2,
                                   "Blue" = 3),selected = 1)),
      shiny::column(5,
       shiny::h5('Choose region of interest to zoom (slower for larger images).'),
       shiny::plotOutput("plot3",brush = shiny::brushOpts(id="plot3_brush",
                        resetOnNew = TRUE,clip=TRUE)))
      ),
  shiny::hr(),
    shiny::fluidRow(
     shiny::column(12,shiny::sliderInput("obs", "Threshold:",
                 min=-0.3,max=0.3,value=0,step=0.005,width='1000px',
                ))
     ),
    shiny::fluidRow(
    shiny::column(6,
    shiny::plotOutput("plot2")),
    shiny::column(6	,shiny::plotOutput("plot1"))
    )

),
  server=function(input, output) {
   
   ranges <- shiny::reactiveValues(x=NULL,y=NULL)
   coords <- shiny::reactiveValues(x=NULL,y=NULL)

   output$plot3 <- shiny::renderPlot(suppressWarnings(graphics::plot(colorimages,frame=frame)))

   # update plot range and coordinates
   shiny::observe({
     brush <- input$plot3_brush
     if (!is.null(brush)) {
       ranges$x <- floor(c(brush$xmin, brush$xmax) * ncol(colorimages))
       ranges$y <- nrow(colorimages) - floor(c(brush$ymin, brush$ymax) * 
                   nrow(colorimages))
      
     } else {
       ranges$x <- c(1,ncol(colorimages)) 
       ranges$y <- c(nrow(colorimages),1)
     }

     coords$x <- ranges$x / ncol(colorimages)
     coords$y <- 1 - (ranges$y / nrow(colorimages))

   })

   output$plot2 <- shiny::renderPlot({
      im <- colorimages[c(min(ranges$y):max(ranges$y)),
                        c(min(ranges$x):max(ranges$x)),,frame,drop=FALSE]
      im <- array(im,dim=c(dim(im)[1:3]))
      im <- raster::brick(im)
      
     suppressWarnings(raster::plotRGB(im,scale=1,asp=nrow(im)/ncol(im)))
   })


   shiny::observe({
 
       if (input$select == 1) {  
         if (input$ch == 1) {
           m <- t(apply(images[,,1,frame] < input$obs,2,rev))
         }
         if (input$ch == 2) {
           m <- t(apply(images[,,2,frame] < input$obs,2,rev))
         }
         if (input$ch == 3) {
           m <- t(apply(images[,,3,frame] < input$obs,2,rev))
         }

       } else if (input$select == 2) {  
         if (input$ch == 1) {
           m <- t(apply(images[,,1,frame] > input$obs,2,rev))
         }
         if (input$ch == 2) {
           m <- t(apply(images[,,2,frame] > input$obs,2,rev))
         }
         if (input$ch == 3) {
           m <- t(apply(images[,,3,frame] > input$obs,2,rev))
         }

       } else if (input$select == 3) {  
         if (input$ch == 1) {
           m <- t(apply((images[,,1,frame] > input$obs) | 
                        (images[,,1,frame] < -input$obs),2,rev))
         }
         if (input$ch == 2) {
           m <- t(apply((images[,,2,frame] > input$obs) | 
                        (images[,,2,frame] < -input$obs),2,rev))
         }
         if (input$ch == 3) {
           m <- t(apply((images[,,3,frame] > input$obs) | 
                        (images[,,3,frame] < -input$obs),2,rev))
         }
       }    
          
       output$plot1 <- shiny::renderPlot(graphics::image(m,
                                         xlab='',ylab='',xaxt='n',yaxt='n',
                                         col=c('white','black'),
                                         xlim=coords$x,
                                         ylim=coords$y))      
   })

    shiny::observeEvent(input$stop, shiny::stopApp({
      input$obs
     }))
  }
 ))
}


