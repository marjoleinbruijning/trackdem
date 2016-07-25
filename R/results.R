#' is.TrDm
#'
#' Generic lower-level function to test whether an object
#' is an TrackDem object.

#' @param object Object to test
#' @export
is.TrDm <- function(object) {
  inherits(object, "TrDm")
}


##' @export
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
"#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")) 

## Summary TrDm objects
##' @export
summary.TrDm <- function(x,incThres=10,...) {
  if (sum(class(x) == 'particles') > 0) {
    n <- 1:length(x)
    names(n) <- 1:length(n)
    numbers <- sapply(n,function(X)
                     length(x[[X]]$patchID))
    mu <- mean(numbers)
    sdd <- sd(numbers)
    cv <- sdd/mu
    tab <- cbind(Mean=mu,SD=sdd,CV=cv)
    res <- list(particles=tab,
                n=numbers)
    class(res) <- 'TrDm'
    cat("\t Summary of identified particles (unlinked): \n\n")
    cat("\t Average number of identified particles: ",
        as.numeric(round(res$particles[1],2)) , "( sd =",
        as.numeric(round(res$particles[2],2)), ")\n",
        "\t Coefficient of variation: ",
        as.numeric(round(res$particles[3],2)))
    cat("\n\t Range of particles for each frame ( 1-", length(res$n),"):",
        range(res$n)[1],"-",range(res$n)[2],"\n")
    cat(ifelse(attributes(x)$nn==TRUE,'\t With neural network.\n',
        '\t Without neural network.\n'))
    invisible(res)
  } else if (sum(class(x) == 'tracked') > 0) {
    incLabels <- apply(x$trackRecord[,,1],1,function(x) 
                                               sum(!is.na(x))) > incThres
    tr <- x$trackRecord[incLabels,,,drop=FALSE]
    sr <- x$sizeRecord[incLabels,,drop=FALSE]
    # Distance
    dr <- t(sapply(1:nrow(tr),function (x) 
                               sqrt(diff(tr[x,,1])^2 + diff(tr[x,,2])^2)))

    perID <- apply(sr,1,mean,na.rm=T)
    sdd <- apply(sr,1,sd,na.rm=T)
    muD <- apply(dr,1,sum,na.rm=T) # total movement
    tab <- cbind(ID=which(incLabels),Mean=perID,SD=sdd,movement=muD)
    colnames(tab) <- c('ID','Size','SD size','Total movement')
    res <- list(N=nrow(tab),
                particles=tab,
                trackrecord=tr,
                sizerecord=sr,
                distrecord=dr,
                presence=incThres)
    class(res) <- 'summary.records'
    cat("\t Summary of Linked particles: \n\n")
    cat(paste("\t Total of",res$N,"Identified particles:\n"))
    print(tab)
    cat(paste("\t Minimum presence is set at",res$presence,"frames) \n",sep=' '))
        
    invisible(res)
  } else if (sum(class(x) == 'tfp') > 0) {
    cat("\t Summary of manually identified false and true positives.\n")
    cat(paste("\t True positives:",length(x$wrong),'\n'))
    cat(paste("\t False positives:",length(x$correct),'\n\n'))
  } else if (sum(class(x) == 'neuralnet') > 0) {
    cat("\t Summary of trained neural network: \n")
    cat("\t Confusion table: \n")
    print.default(format(x$confusion,digits = 3)
                 ,print.gap = 2L, 
                  quote = FALSE)
    
    cat("\t F-score:",round(x$fscore,2),'\n')
    cat("\t",x$bestNN$reps," repetitions.\n")
    cat("\t",x$bestNN$hidden," hidden layer(s).\n")

  } else {cat("\t No summary available for this object.\n\n")}
} 

## Print TrDm objects
##' @export
print.TrDm <- function(x,...) {
  if (sum(class(x) == 'colorimage') > 0) {
    d <- dim(x)
    cat("\t Trackdem color image \n")
    cat(paste("\t Images with size:",d[1],"x",d[2]," pixels.\n"))
    if (length(d) == 4) {
      cat(paste("\t Total of",d[4],"images, with",d[3],"color layers. \n\n"))
    } else if (length(d) == 3) {
      cat(paste("\t Total of 1 image, with",d[3],"color layers. \n\n"))
    }
  } else if (sum(class(x) == 'sbg') > 0) {
    d <- dim(x)
    cat("\t Trackdem subtracted background image. \n")
    cat(paste("\t Images with size:",d[1],'x',d[2],' pixels. \n'))
    cat(paste("\t Total of",d[3],"images, with",d[4],"color layers. \n\n"))
  } else if (sum(class(x) == 'particles') > 0) {
    cat("\t Trackdem list with identified particles (without tracking). \n\n")
  } else if (sum(class(x) == 'tracked') > 0) {
    cat("\t Trackdem object. \n \t
       Coordinates and sizes of all identified particles (with tracking) \n\n")
  } else if (sum(class(x) == 'tfp') > 0) {
    cat("\t Trackdem object with manually identified true and false positives. \n\n")
  } else if (sum(class(x) == 'trainingdata') > 0) {
    print.data.frame(x)
  } else if (sum(class(x) == 'neuralnet') > 0) {
      cat("\t Trained neural network with",x$bestNN$hidden,"layers.\n")
  } 
}

## Plot TrDm objects
##' @export
plot.TrDm <- function(x,frame=1,type=NULL,incThres=10,colorimages=NULL,
                      cl=1,path='~',name='animation',...) {
  #oldpar <- par('mar','mfrow')
  if (any(class(x) == 'colorimage')) {  
    if (length(dim(x)) > 3) {
      x <- brick(x[,,,frame])
    } else if (length(dim(x)) == 3) {
      class(x) <- 'array'
      x <- brick(x)
    }
    plotRGB(x,scale=1,asp=nrow(x)/ncol(x),...)
  } else if (any(class(x) == 'tracked')) {
      incLabels <- apply(x$trackRecord[,,1],1,function(x) 
                                             sum(!is.na(x))) > incThres
      if(is.null(colorimages)) { colorimages <- get(attributes(x)$originalImages,
                                                    envir=.GlobalEnv) }
      if (is.null(type)) {
        par(mfrow=c(1,2))
        tmp <- x$trackRecord[incLabels,,,drop=FALSE]
        plot(100,100,xlim=c(0,1),ylim=c(0,1),xlab='x',ylab='y',...)
        for (i in 1:nrow(tmp)) {
          lines(tmp[i,,1]/ncol(colorimages),1-tmp[i,,2]/nrow(colorimages),
               col=paste0(jet.colors(nrow(tmp))[i],'99'),lwd=1.5)
        }
        
        tmp <- x$sizeRecord[incLabels,,drop=FALSE]
        perID <- apply(tmp,1,mean,na.rm=T)
        sdperID <- apply(tmp,1,sd,na.rm=T)
        plot(1:length(perID),perID[order(perID)],cex=1.5,pch=21,bg='#00000050',
             xlab='Labeled particle',ylab='Size (pixels)',...)
        segments(x0=1:length(perID),y0=perID[order(perID)]-sdperID[order(perID)],
                 y1=perID[order(perID)]+sdperID[order(perID)])
      } else {
          if (type == 'sizes') {
          x <- x$sizeRecord[incLabels,,drop=FALSE]
          perID <- apply(x,1,mean,na.rm=T)
          sdperID <- apply(x,1,sd,na.rm=T)
          plot(1:length(perID),perID[order(perID)],cex=1.5,pch=21,bg='#00000050',
               xlab='Labeled particle',ylab='Size (pixels)',...)
          segments(x0=1:length(perID),y0=perID[order(perID)]-sdperID[order(perID)],
                   y1=perID[order(perID)]+sdperID[order(perID)])
        } else if (type == 'trajectories') {
            x <- x$trackRecord[incLabels,,,drop=FALSE]
            plot(colorimages,...)
            for (i in 1:nrow(x)) {
            lines(x[i,,1]/ncol(colorimages),1-x[i,,2]/nrow(colorimages),
                  col=paste0(jet.colors(nrow(x))[i],'99'),lwd=1.5)
            }
          } else if (type == 'animation') {
            oldwd <- getwd()
            setwd(path)
            x <- x$trackRecord[incLabels,,]
            dir.create(paste0('images'))
            width <- ifelse(ncol(colorimages) %% 2 == 0, 
                            ncol(colorimages),ncol(colorimages)+1)
            height <- ifelse(nrow(colorimages) %% 2 == 0, 
                            nrow(colorimages),nrow(colorimages)+1)
            rownames(x) <- 1:nrow(x)
            for (i in 1:ncol(x)) {
              png(paste0('images/images',i,'.png'),width=width,height=height)
              plot(colorimages,frame=i)
              points(x[,i,1]/width,
                     1-x[,i,2]/height,
                     col=rainbow(nrow(x))[as.numeric(rownames(x))],
                     cex=2)
              cat("\r \t Animation:",i,"out of",ncol(x))
              dev.off()
           }
           system(paste0("avconv -loglevel quiet -r 10 -i 'images/images'%d.png -b:v 1000k ",name,".mp4"))
           system("rm -r images")
           setwd(oldwd)
           cat('\n')
        }
     }  
  } else if (any(class(x) == 'nnObject')) {
    plot(cbind(x$trainingData$trY,plogis(x$predicted$net.result)),
         xlab='Identified',ylab='Probability based on neural network',...)
    abline(h=x$thr,lty=2,lwd=2)
  } else if (any(class(x) == 'sbg')) {
    image(x[,,frame,cl])
  }
  #par(oldpar)
}
