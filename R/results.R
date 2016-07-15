
##' @export
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
"#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")) 

##' @export
summary.identifiedParticles <- function(object, ...) {
  n <- 1:length(object$particleStats)
  numbers <- sapply(n,function(X)
                   length(object$particleStats[[X]]$patchID))
  mu <- mean(numbers)
  sdd <- sd(numbers)
  cv <- sdd/mu
  tab <- cbind(Mean=mu,SD=sdd,CV=cv)
  res <- list(particles=tab,
              n=numbers)
  names(res) <- c('Summary of identified particles','Per frame')
  return(res)
}

##' @export
summary.records <- function(object, incThres=10, ...) {
  incLabels <- apply(object$trackRecord[,,1],1,function(x) 
                                               sum(!is.na(x))) > incThres
  tr <- object$trackRecord[incLabels,,,drop=FALSE]
  sr <- object$sizeRecord[incLabels,,drop=FALSE]
  # Distance
  dr <- t(sapply(1:nrow(tr),function (x) 
                             sqrt(diff(tr[x,,1])^2 + diff(tr[x,,2])^2)))

  perID <- apply(sr,1,mean,na.rm=T)
  sdd <- apply(sr,1,sd,na.rm=T)
  muD <- apply(dr,1,sum,na.rm=T) # total movement
  tab <- cbind(ID=which(incLabels),Mean=perID,SD=sdd,movement=muD)
  colnames(tab) <- c('ID','Size','SD','Total movement')
  res <- list(particles=tab,
              trackrecord=tr,
              sizerecord=sr,
              distrecord=dr)
  return(res)
}

##' @export
plot.colorimage <- function (x,frame=1,...) {
  x <- brick(x[,,,frame])
  plotRGB(x,scale=1,asp=nrow(x)/ncol(x),...)
}

##' @export
plot.trackrecord <- function (x,bg=allFullImages,incThres=10,...) {
  incLabels <- apply(x[,,1],1,function(x) sum(!is.na(x))) > incThres
  x <- x[incLabels,,,drop=FALSE]
  plot(bg,...)
   for (i in 1:nrow(x)) {
    lines(x[i,,1]/ncol(bg),1-x[i,,2]/nrow(bg),
         col=paste0(jet.colors(nrow(x))[i],'40'),lwd=1.5)
  }
}

##' @export
plot.sizerecord <- function (x,type='p',bg=allFullImages,incThres=10,...) {
  incLabels <- apply(x,1,function(x) sum(!is.na(x))) > incThres
  x <- x[incLabels,,drop=FALSE]
  perID <- apply(x,1,mean,na.rm=T)
  sdperID <- apply(x,1,sd,na.rm=T)
  if (type == 'p') {
    plot(1:length(perID),perID[order(perID)],cex=1.5,pch=21,bg='#00000050',
         xlab='Labeled particle',ylab='Size (pixels)',...)
    segments(x0=1:length(perID),y0=perID[order(perID)]-sdperID[order(perID)],
     y1=perID[order(perID)]+sdperID[order(perID)])
  }
  if (type == 'd') {
    hist(perID,xlab='Size (pixels)',main='Particle size distribution',...)
  }
}

##' Make animation
##' \code{makeAnimation} is ...
##' @author Marjolein Bruijning & Marco D. Visser
##' @export
makeAnimation <- function(particleStats,images=allFullImagesRGB,
                         path='~',incThr=10,trackRecord=trackRecord) {
  oldwd <- getwd()
  setwd(path)
  incLabels <- apply(trackRecord[,,1],1,function(x) sum(!is.na(x))) > incThres
 
  trackRecord <- trackRecord[incLabels,,]
  dir.create(paste0('images'))
  width <- ncol(images[[i]])
  height <- nrow(images[[i]])
  rownames(trackRecord) <- 1:nrow(trackRecord)
  for (i in 1:length(particleStats)) {
    png(paste0('images/images',i,'.png'),width=850,height=height)
    plot(images,frame=i)
    points(trackRecord[,i,1]/width,
           1-trackRecord[,i,2]/height,
           col=rainbow(nrow(trackRecord))[as.numeric(rownames(trackRecord))],
           cex=2)
    print(i)
    dev.off()
  }
  system("avconv -r 10 -i 'images/images'%d.png -b:v 1000k animation.mp4")
  setwd(oldwd)
}


