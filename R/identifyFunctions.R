##' Create image sequence
##'
##' \code{createImageSeq} creates an image sequences (.png) using
##'  video files as input. All movies within a directory will  
##' be converted into an image sequence.
##' For each movie, a new directory is created containing the recorded date and 
##' name of the movie.
##' @param moviepath Path to directory containing the video files.
##' @param imagepath Path to location of directory in which image sequences should
##' be saved.
##' @param x Number of pixels in horizontal direction; default is 1915 (HD).
##' @param y Number of pixels in vertical direction; default is 1080 (HD).
##' @param fps Frames per second, default is 15.
##' @param nsec Duration of movie that is exported, default is 2 seconds.
##' When movie length is greater than \code{nsec}, the \code{nsec} seconds 
##' in the exact middle of the movie are exported.  
##' @param ext The extension of the video. Default is \code{'MTS'}. All
##' formats supported by "avconv" are accepted. 
##' @param path Path to location where temporary python file will be saved. Default 
##' is working directory.
##' @author Marjolein Bruijning, Caspar A. Hallmann & Marco D. Visser
##' @export
createImageSeq <- function (moviepath,imagepath,x=1915,
                            y=1080,fps=15,nsec=2,ext='MTS',path=getwd()) {
        
    ext <- paste0("'.",tolower(ext),"'")
    fileConn<-file(paste(path,'/tmp.py',sep=''))
    
    writeLines(c(
        paste("import os"),
        paste("import subprocess"),
        paste("import string"),
        paste("movieDirname = os.path.abspath('",moviepath,"')",sep=''),
        paste("sequenceParentDirname = os.path.abspath('",imagepath,"')",sep=''),
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
        
        paste("movieDir = os.path.abspath(movieDirname)"),
        paste("sequenceParentDir = os.path.abspath(sequenceParentDirname)"),
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
    system(paste('python ',path,'/tmp.py',sep=''))
    file.remove(paste('tmp.py',sep=''))
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
##' @param xranges By default the full image is loaded; specify to subset the number of columns.
##' @param yranges By default the full image is loaded; specify to subset the number of rows.
##' @examples
##' \dontrun{
##' images <- loadImages(direcPictures='~/images1/',
##'	                     nImages=1:30,yranges=1:1080,xranges=1:1915)
##' plot(images)
##' images
##'	}
##' @return Array of class 'TrDm' and 'colorimages' containing all loaded images.
##' @author Marjolein Bruijning, Caspar A. Hallmann & Marco D. Visser
##' @export

loadImages <- function (dirPictures,filenames=NULL,nImages=1:30,
                        xranges=NULL,yranges=NULL) {

    if (is.null(dirPictures)) {
		dirtPictures <- getwd()
	}
	
    if (is.null(filenames)) {
        allFiles <- list.files(path=dirPictures) # List all files in folder
        allFiles <- allFiles[nImages]
    }
    else {filenames <- filenames[nImages]}	
    # Load all images
    allFullImages <- sapply(1:length(nImages),
                            function(x) png::readPNG(file.path(dirPictures,allFiles[x])),
                            simplify='array')
    # Subset
    if (!is.null(xranges) | !is.null(yranges)) {
        allFullImages <- sapply(1:length(nImages),function(x) 
            allFullImages[[x]][yranges,xranges,],simplify='array')	
    }

    attr(allFullImages,"settings") <- list(nImages=nImages)
    attr(allFullImages, "class") <- c('TrDm','colorimage','array')
    attr(allFullImages, "originalDirec") <- dirPictures#get(deparse(substitute(dirPictures)))
    return(allFullImages)
}


##' Background detection
##'
##' \code{createBackground} detects the still background,
##' containing all motionless pixels (non particles). Three different methods to detect
##' the background can be used.
##' @param colorimages Array of class 'TrDm' containing all images, obtained by 
##' \code{\link{loadImages}}.
##' @param method Use \code{method='mean'} to calculate the mean value for each pixel and color.
##' Use \code{method='powerroot'} to deflate dark values (note, this can only be 
##' used for dark particles on a light background). Use \code{method='filter'} to 
##' replace pixels in which movement has occurred with the mode of neighboring values.
##' Note that \code{method='filter'} is computationally more intensive. 
##' @author Marjolein Bruijning, Caspar A. Hallmann & Marco D. Visser
##' @examples
##' \dontrun{
##' stillBack <- createBackground (allFullImages)
##' plot(stillBack)
##'	}
##' @return Array of class 'TrDm' and 'colorimage' containing detected background.
##' @export

createBackground <- function(colorimages,method='mean') {
    if(!is.TrDm(colorimages)){
        stop("Input does not appear to be of the class \"TrDm\"")
    }
    if (method == 'mean') {
      A <- cb(colorimages[,,1,],
              colorimages[,,2,],
              colorimages[,,3,],
              dim(colorimages[,,1,]),
              array(0,dim=dim(colorimages[,,,1])))

    } else if (method == 'powerroot') {
      zz <- apply(colorimages[,,1,],c(1,2),function(x)  mean(x^50)^(1/50) )
      zz2 <- apply(colorimages[,,2,],c(1,2),function(x)  mean(x^50)^(1/50) )
      zz3 <- apply(colorimages[,,3,],c(1,2),function(x)  mean(x^50)^(1/50) )
      A <- array(0,c(dim(zz),3))
      A[,,1] <- zz
      A[,,2] <- zz2
      A[,,3] <- zz3

    } else if (method == 'filter') {
        rst <-  cb(colorimages[,,1,],
              colorimages[,,2,],
              colorimages[,,3,],
              dim(colorimages[,,1,]),
              array(0,dim=dim(colorimages[,,,1])))
        class(rst) <- class(colorimages)
        subs <- aperm(subtractBackground(bg=rst,colorimages),c(1,2,4,3))
        class(subs) <- class(colorimages)
        attributes(subs) <- attributes(colorimages)
        SDs <- cb(subs[,,1,]^2,
                 subs[,,2,]^2,
                 subs[,,3,]^2,
                 dim(subs[,,1,]),array(0,dim=dim(colorimages[,,1,]))) * 
                    dim(colorimages)[4]/(dim(colorimages)[4]-1)
        SD <- sqrt(SDs[,,1]^2+SDs[,,2]^2+SDs[,,3]^2)
        Threshold <-
         {
	     stats::optimize(function(a,i1){
	     t1 <-(i1>a)
	     sum((i1[t1]-mean(i1[t1]))^2)+
	     sum((i1[!t1]-mean(i1[!t1]))^2)
	     }
	     ,c(0,max(SD)),i1=c(SD))$min	
	     }
  
         rst[(SD>Threshold)] <- NA

         A <- array(0,c(dim(colorimages)[1:2],3))

         for (i in 1:3) {
           r <- raster::raster(rst[,,i],xmx=ncol(rst),ymx=nrow(rst))	

           f1 <- raster::focal(r,w=matrix(1,31,31),fun=function(x){raster::modal(x,na.rm=T)},
                       NAonly=TRUE)
           while((!all(is.finite(f1@data@values)))|any(is.na(f1@data@values))){
             f1@data@values[!is.finite(f1@data@values)] <- NA
             f1 <- raster::focal(f1,w=matrix(1,31,31),fun=function(x){raster::modal(x,na.rm=T)},
                         NAonly=TRUE,pad=T)
           }

           A[,,i] <- matrix(f1[,,1],ncol=ncol(colorimages),
                            nrow=nrow(colorimages),byrow=TRUE)
         }
        
    } else {stop('No valid method for creating background')}

    class(A) <- c('TrDm','colorimage','array')
    attr(A,"originalImages") <- deparse(substitute(colorimages))
    attr(A,"originalDirec") <- attributes(colorimages)$originalDirec
    attr(A,"settings") <- c(attributes(colorimages)$settings,list(BgMethod=method))
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
##' \code{\link{loadImages}}. Default is \code{NULL}, in this case the original images
##' are used from the global environment.
##' @author Marjolein Bruijning, Caspar A. Hallmann & Marco D. Visser
##' @examples
##' \dontrun{
##' allImages <- subtractBackground(stillBack) }
##' @return Returns array of class 'TrDm' and 'sbg' with same size as images, 
##' subtracted from background.
##' @export

subtractBackground <- function (bg,colorimages=NULL) {

    if(!is.TrDm(bg)){
        stop("Input does not appear to be of the class \"TrDm\"")
    }
    
    if(is.null(colorimages)) { colorimages <- get(attributes(bg)$originalImages,
                                                  envir=.GlobalEnv) }
    
    sb <- sapply(1:3, function(x) sb(colorimages[,,x,],
                                     bg[,,x],
                                     dim(colorimages[,,x,]),
                                     array(0,dim=dim(colorimages))),
                 simplify='array')
    sb <- aperm(sb, c(1,2,4,3))
    attr(sb,"background") <- deparse(substitute(bg))
    attr(sb,"originalImages") <- attributes(bg)$originalImages
    attr(sb,"originalDirec") <- attributes(bg)$originalDirec
    attr(sb,"settings") <- attributes(bg)$settings
    
    class(sb) <- c('TrDm','sbg','array')
    return(sb)
}

##' Identify moving particles
##'
##' \code{identifyParticles} identifies particles using the 
##' subtracted images obtained from \code{\link{subtractBackground}}.
##' @param sbg Array containing images containing all moving particles,
##' as obtained from \code{\link{subtractBackground}}.
##' @param threshold Thresholds for including particles. A numeric vector
##' containing three values; one for each color. Otherwise, supply one value 
##' which is to be used for all three colors. For a chosen quantile
##' for each frame, use \code{qthreshold}. Default is \code{threshold=-0.1}, 
##'  which works for dark particles on a light background. Alternatively,
##' set \code{autoThres} below for an automatic threshold.  
##' @param pixelRange Default is \code{NULL}. Numeric vector with minimum and maximum particle size, used as a
##' first filter to identify particles. Use if particle of interest are of a known size
##' range (in pixels).
##' @param qthreshold Default is \code{NULL}. Supply a value, to do thresholding based on
##' quantile. Quantile is calculated for each
##' frame seperately.
##' @param select Select dark particles (\code{'dark'}), light particles (\code{'light'}), or
##' both (\code{'both'}), compared to background. Default is \code{'dark'}.
##' @param colorimages Array containing original color images. By default, the original
##' color images are obtained from global environment.
##' @param autoThres Logical. \code{TRUE} to get an automated threshold for each color layer.
##' Default is \code{FALSE}.
##' @param perFrame Logical. If \code{autoThres=TRUE}, set at \code{TRUE}
##'  to calculate a threshold for 
##' each frame seperately. Default is \code{FALSE}. Note that is can be computationally intensive 
##' to calculate a threshold for each frame.
##' @param frames When \code{autoThres=TRUE} and \code{allFrames=FALSE}, supply a
##' numeric vector specifying over which frames the automated threshold 
##' should be calculated on (e.g. \code{c(1,3,5,7,9,11)} for all odd frames from 1 to 11). 
##' @author Marjolein Bruijning, Caspar A. Hallmann & Marco D. Visser
##' @examples
##' \dontrun{
##'   trackObject <- identifyParticles(allImages,threshold=-0.1,pixelRange=c(3,400))
##'  plot(trackObject)
##'  summary(trackObject)
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
    cat("\r \t Particle Identification: Thresholding (1 of 5)                            ")

    # if only one value supplied, use same value for each color layer
    if (length(threshold) == 1) {threshold <- rep(threshold,3)}

    # automated threshold
    if(autoThres) {
      cat("\r \t Particle Identification: Automated thresholding (1 of 5)            ")
	  if (is.null(frames)) { frames <- n }
      threshold <- calcAutoThres(sbg[,,frames,],perFrame=perFrame)
	}
    
    if(!is.null(qthreshold)) {
        A <- array(NA,dim=dim(sbg))
        for (i in 1:3) {
            A[,,,i] <- sapply(1:dim(sbg)[3], function(x)
                sbg[,,x,i] < stats::quantile(sbg[,,x,i],
                                       qthreshold),
                simplify='array')
        }
        
    } else {
        if (select == 'dark') {
          A <- sapply(1:length(threshold), function(x) sbg[,,,x] < threshold[x],
                      simplify='array')
		}
        else if (select == 'light') {
          A <- sapply(1:length(threshold), function(x) sbg[,,,x] > threshold[x],
                      simplify='array')
        }
        else if (select == 'both') {
          A <- sapply(1:length(threshold), function(x) sbg[,,,x] > threshold[x] |
                                                       sbg[,,,x] < -threshold[x],
                      simplify='array')

        }
        else {stop("Invalid selection, choose 'dark', 'light' or 'both' ")}
    }
    
    # 1 if 1 in at least one color layer
    sumRGB <- apply(A,c(2,3),rowSums)
    sumRGB <- sumRGB > 0

    cat("\r \t Particle Identification: Labeling (2 out of 5)                     ")
    A <- sapply(n, function (x) SDMTools::ConnCompLabel(sumRGB[,,x]),simplify='array')
    
    cat("\r \t Particle Identification: Size filtering (3 out of 5)               ")
    if (!is.null(pixelRange)) {
	for (i in n) {
            allLabels <- tabulate(as.vector(A[,,i]),nbins=max(A[,,i]))
            allLabels <- allLabels[allLabels > 0]
            names(allLabels) <- sort(unique(as.numeric(A[,,i])))[-1]
            inc <- allLabels >= pixelRange[1] & allLabels <= pixelRange[2]
            A[,,i] [!A[,,i] %in% names(inc[inc==TRUE])] <- 0
	}
    }

    cat("\r \t Particle Identification: Particle statistics (4 out of 5)          ")
    
    for (i in n) {
       ps <- SDMTools::PatchStat(A[,,i])[-1,]
       getMean <- extractMean(ps$patchID,
                              colorimages=colorimages[,,,i],
                              images=A[,,i])
	   colnames(getMean) <- paste0('mu',c('R','G','B'))
       coords <- getCoords(m=A[,,i],d=dim(A[,,i]))
       rows <- tapply(coords[,1],A[,,i][A[,,i]>0],mean)
       cols <- tapply(coords[,2],A[,,i][A[,,i]>0],mean)
       ps <- cbind(ps,data.frame(x=cols,y=rows,frame=i),
                   getMean)
       if (i == 1) particleStats <- ps
       if (i > 1) particleStats <- rbind(particleStats,ps)
    }    

    cat("\r \t Particle Identification: Finalizing (5 out of 5)                       \n ")
    
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
## @param perFrame If TRUE, threshold is calculated for each frame. Default is FALSE. 

calcAutoThres <- function(sbg,perFrame=FALSE){

 ncolours<- dim(sbg)[4]
 nframes<-  dim(sbg)[3]

 f<-function(a){
  t1<-(i1>a)
  sum((i1[t1]-mean(i1[t1]))^2)+
  sum((i1[!t1]-mean(i1[!t1]))^2)
  }

 if(perFrame){
 pp<-array(0,c(nframes,ncolours))
 for(j in 1:nframes){
 for(i in 1:ncolours){
  i1<- c( sbg[,,j,i] )
  pp[j,i]<-
  stats::optimize(function(a){
  t1<-(i1>a)+1
  sum((i1[t1==1]-mean(i1[t1==1]))^2)+
  sum((i1[t1==2]-mean(i1[t1==2]))^2)
  } ,range(i1))$min
 }}
 } else {
 pp<-numeric(ncolours)
 for(i in 1:ncolours){
  i1<- c( sbg[,,,i] )
  pp[i]<- stats::optimize(f,range(i1))$min
 }
 }
 
 return(pp)
}

