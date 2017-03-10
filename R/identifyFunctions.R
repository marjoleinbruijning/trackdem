##' Create image sequence
##'
##' \code{createImageSeq} is a function to create an image sequence (.png) using
##'  video files as input. All movies within a directory will  
##' be exported into a new directory called 'ImageSequences' created in the path.
##' For each movie, a new directory is created containing the recorded date and 
##' name of the movie.
##' @param moviepath Path to location of (stem) directory containing the directory
##' in which the video files are located.
##' @param imagepath Path to location of directory in which image sequences should
##' be placed.
##' @param path Path to location where temporary python file will be saved. Default 
##' is working directory.
##' @param x Number of pixels in horizontal direction (ncol); default is 1915.
##' @param y Number of pixels in vertical direction (nrow); default is 1080.
##' @param fps Frames per second, default is 15.
##' @param nsec Duration of movie that is exported, default is 2 seconds.
##' The middle nsec seconds of the movie are used.
##' @param ext The extension of the video. Default is 'MTS'.
##' @examples
##' \dontrun{
##' createImageSeq(path='~/Data/',ext='mp4')
##'	}
##' @author Marjolein Bruijning & Marco D. Visser
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
        #paste("movieDir = os.path.abspath(os.path.join(startDir, movieDirname))"),
        #paste("sequenceParentDir = os.path.abspath(os.path.join(startDir, sequenceParentDirname))"),
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
##' \code{loadImages} is a function to load png images as a three dimensional array.
##' The objects created through the function can be used for image analysis.
##' @param direcPictures The path of the folder where the images can be found.
##' @param filenames Default is NULL. If not all files should be loaded, here specify
##' which files to use.
##' @param nImages The total number of images to load; default is 1:30.
##' @param xranges Default all pixels are loaded; specify to subset the number of columns.
##' @param yranges Default all pixels are loaded; specify to subset the number of rows.
##' @author Marjolein Bruijning & Marco D. Visser
##' @examples
##' \dontrun{
##' images <- loadImages(direcPictures='~/images1/',filenames=NULL,
##'	nImages=1:30,yranges=1:1080,xranges=1:1915)
##'	}
##' @return Array with all images.
##' @export

loadImages <- function (direcPictures,filenames=NULL,nImages=1:30,
                        xranges=NULL,yranges=NULL) {

    if (is.null(direcPictures)) {
		directPictures <- getwd()
	}
	
    if (is.null(filenames)) {
        allFiles <- list.files(path=direcPictures) # List all files in folder
        allFiles <- allFiles[nImages]
    }
    else {filenames <- filenames[nImages]}	
    # Load all images
    allFullImages <- sapply(1:length(nImages),
                            function(x) readPNG(file.path(direcPictures,allFiles[x])),
                            simplify='array')
    # Subset
    if (!is.null(xranges) | !is.null(yranges)) {
        allFullImages <- sapply(1:length(nImages),function(x) 
            allFullImages[[x]][yranges,xranges,],simplify='array')	
    }

    attr(allFullImages, "class") <- c('TrDm','colorimage','array')
    attr(allFullImages, "originalDirec") <- get(deparse(substitute(direcPictures)))
    return(allFullImages)
}


##' Background detection
##'
##' \code{createBackground} is a function to create a still background,
##' containing all motionless, by taking
##' mean pixel values over all frames.
##' @param colorimages Array containing all frames, obtained by 
##' @param method Use method='mean' to calculate the mean value for each pixel and color.
##' Use 'method='mean2' to deflate dark values (note, this can only be 
##' used for dark particles on a light background). Use method='filter' to 
##' replace pixels in which movement has occurred with the mode of neighboring values.
##' \code{\link{loadImages}}
##' @author Marjolein Bruijning & Marco D. Visser
##' @examples
##' \dontrun{
##' stillBack <- createBackground (allFullImages)
##'	}
##' @return Array with still background.
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

    } else if (method == 'mean2') {
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
	     optimize(function(a,i1){
	     t1 <-(i1>a)
	     sum((i1[t1]-mean(i1[t1]))^2)+
	     sum((i1[!t1]-mean(i1[!t1]))^2)
	     }
	     ,c(0,max(SD)),i1=c(SD))$min	
	     }
  
         rst[(SD>Threshold)] <- NA

         A <- array(0,c(dim(colorimages)[1:2],3))

         for (i in 1:3) {
           r <- raster(rst[,,i],xmx=ncol(rst),ymx=nrow(rst))	

           f1 <- focal(r,w=matrix(1,31,31),fun=function(x){modal(x,na.rm=T)},
                       NAonly=TRUE)
           while((!all(is.finite(f1@data@values)))|any(is.na(f1@data@values))){
             f1@data@values[!is.finite(f1@data@values)] <- NA
             f1 <- focal(f1,w=matrix(1,31,31),fun=function(x){modal(x,na.rm=T)},
                         NAonly=TRUE,pad=T)
           }

           A[,,i] <- matrix(f1[,,1],ncol=ncol(colorimages),
                            nrow=nrow(colorimages),byrow=TRUE)
         }
        
    } else {stop('No valid method for creating background')}

    class(A) <- c('TrDm','colorimage','array')
    attr(A,"originalImages") <- deparse(substitute(colorimages))
    attr(A,"originalDirec") <- attributes(colorimages)$originalDirec
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
##' \code{\link{loadImages}}. Default is NULL, in case the original images
##' are used.
##' @author Marjolein Bruijning & Marco D. Visser
##' @examples
##' \dontrun{
##' allImages <- subtractBackground(stillBack) }
##' @return Returns array with same size as images, subtracted from background.
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
    class(sb) <- c('TrDm','sbg','array')
    return(sb)
}

##' Identify moving particles
##'
##' \code{identifyParticles} is a function to identify particles.
##' @param mSub Array containing images containing all moving particles,
##' as obtained by \code{\link{subtractBackground}}
##' @param threshold Thresholds for including particles. Supply a vector
##' containing three values; one for each color. Otherwise, supply one value 
##' which is to be used for all three colors. For a chosen 
##' threshold for each frame, use qthreshold.
##' @param pixelRange Default is NULL. Vector with minimum and maximum particle size, used as a
##' first filter to identify particles.
##' @param qthreshold Default is NULL. If NULL, treshold is used for filter. If
##' not zero, a threshold based on qthreshold quantile is calculated for each
##' frame.
##' @param select Select dark particles ('dark'), light particles ('light'), or
##' both ('both'), compared to background. Default is dark.
##' @param autoThres TRUE to get an automated threshold for each color layer.
##' Default is FALSE.
##' @param perFrame If autoThres=TRUE, set at TRUE to calculate a threshold for 
##' each frame seperately. Default is FALSE.
##' @param frames Specify if automated threshold should not be calculated based 
##' on all frames.
##' @author Marjolein Bruijning & Marco D. Visser
##' @examples
##' \dontrun{
##'   trackObject <- identifyParticles(allImages,threshold=-0.1,pixelRange=c(3,400))
##'	}
##' @return This function returns a list with two elements: (1) a dataframe with
##' particle statistics with identified particles for each frame. (2) An array
##' containing all binary images.
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

    tmp <- attributes(sbg)
    sbg <- aperm(sbg, c(1,2,4,3))
    attributes(sbg)$background <- tmp$background
    attributes(sbg)$originalImages <- tmp$originalImages
    attributes(sbg)$originalDirec <- tmp$originalDirec
        
    cat("\t Particle Identification:  ")
    n <- 1:dim(sbg)[3]
    cat("\r \t Particle Identification: Thresholding (1 of 5) \t")

    # if only one value supplied, use same value for each color layer
    if (length(threshold) == 1) {threshold <- rep(threshold,3)}

    # automated threshold
    if(autoThres) {
      cat("\r \t Particle Identification: Automated thresholding (1 of 5) \t")
	  if (is.null(frames)) { frames <- n }
      threshold <- calcAutoThres(sbg[,,frames,],perFrame=perFrame)
	}
    
    if(!is.null(qthreshold)) {
        A <- array(NA,dim=dim(sbg))
        for (i in 1:3) {
            A[,,,i] <- sapply(1:dim(sbg)[3], function(x)
                sbg[,,x,i] < quantile(sbg[,,x,i],
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

    cat("\r \t Particle Identification: Labeling (2 out of 5) \t")
    A <- sapply(n, function (x) ConnCompLabel(sumRGB[,,x]),simplify='array')
    
    cat("\r \t Particle Identification: Size filtering (3 out of 5) \t")
    if (!is.null(pixelRange)) {
	for (i in n) {
            allLabels <- tabulate(as.vector(A[,,i]),nbins=max(A[,,i]))
            allLabels <- allLabels[allLabels > 0]
            names(allLabels) <- sort(unique(as.numeric(A[,,i])))[-1]
            inc <- allLabels >= pixelRange[1] & allLabels <= pixelRange[2]
            A[,,i] [!A[,,i] %in% names(inc[inc==TRUE])] <- 0
	}
    }

    cat("\r \t Particle Identification: Particle statistics (4 out of 5) \t")
    particleStats <- lapply(n,function(x) PatchStat(A[,,x])[-1,])

    getMean <- lapply(1:length(particleStats),function(X)
                  extractMean(particleStats[[X]]$patchID,
                             colorimages=colorimages[,,,X],
                             images=A[,,X]))
	                         
    sapply(1:length(getMean),function(X) 
                   colnames(getMean[[X]]) <<- paste0('mu',c('R','G','B')))

    cat("\r \t Particle Identification: Coordinates calculation (5 out of 5) \n ")
    coords <- lapply(1:dim(A)[3],function(x) getCoords(m=A[,,x],d=dim(A[,,x])))

    particleStats <- lapply(n,function(x) {
        rows <- tapply(coords[[x]][,1],A[,,x][A[,,x]>0],mean)
        cols <- tapply(coords[[x]][,2],A[,,x][A[,,x]>0],mean)
        particleStats[[x]] <- cbind(particleStats[[x]],
                                    data.frame(x=cols,y=rows,frame=x),
                                    getMean[[x]])
        return(particleStats[[x]])
       } )

    particleStats <- do.call(rbind,particleStats)

    attr(particleStats,"images") <- A
    attr(particleStats, "class") <- c("TrDm","particles","data.frame")
    attr(particleStats,"threshold") <- threshold
    attr(particleStats,"background") <- attributes(sbg)$background
    attr(particleStats,"originalImages") <- attributes(sbg)$originalImages
    attr(particleStats,"originalDirec") <- attributes(sbg)$originalDirec
    attr(particleStats,"subtractedImages") <- deparse(substitute(sbg))
    attr(particleStats,"nn") <- FALSE
    
    return(particleStats)
}

##' Calculate automated threshold.
##'
##' @export

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
  optimize(function(a){
  t1<-(i1>a)+1
  sum((i1[t1==1]-mean(i1[t1==1]))^2)+
  sum((i1[t1==2]-mean(i1[t1==2]))^2)
  } ,range(i1))$min
 }}
 } else {
 pp<-numeric(ncolours)
 for(i in 1:ncolours){
  i1<- c( sbg[,,,i] )
  pp[i]<- optimize(f,range(i1))$min
 }
 }
 
 return(pp)
}

