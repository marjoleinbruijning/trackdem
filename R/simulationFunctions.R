## Create a simulated set of movement trajectories
##
## \code{createTrajec} simulates movement trajectories within a 
## bounded space, movements are set with speed (\code{h}) and may be correlated
## in direction (\code{rho}). Function simulates movement of particles in a video 
## sequence of certain number of frames (\code{nframes}) in length. 
## @param nframes Number of time frames(steps).
## @param nIndividuals	Number of individual trajectories.
## @param h Displacement speed in pixels.
## @param rho Correlation parameter for angle of displacement.
## @param domain One of \code{"square"} or \code{"circle"}, imposing a 
## [0-1,0-1] rectangle domain, or a circular domain of radius 1, respectively.
## correct boundary ensure individual trajectories do not cross the domain
## @param correct.boundary Logical. \code{TRUE} to make sure that individuals
## cannot leave the image.
## @param sizes Vector of sizes for each simulated particle of length
## nIndividuals.
## @examples
##    traj <- simulateTrajectories(nframes=30,h=0.01,rho=0.9)
## @author Caspar A. Hallmann, Marjolein Bruijning & Marco D. Visser
createTrajec <- function(nframes=20,nIndividuals=10,
                                 h=.05,rho=0,domain=c("square","circle"),
                                  correctBoundary=TRUE,
                                  sizes=stats::runif(nIndividuals)*.012+.01) {

  if (length(domain)==2) {
      domain <- domain[1]
  } else {
      domain <- domain[1]
  }

  res <- do.call(rbind,lapply(seq_len(nIndividuals),function(j) {
           init <- NULL
           if (domain=="square") {
             init <- stats::runif(2,0,1) 
           }
           if (domain=="circle") {
             phi <- stats::runif(1,0,2*pi)
             r <- stats::runif(1,0,1)
             init <-  r*c(cos(phi),sin(phi))
           }
           xy <- samplePolar(nframes,h,rho,init)
           if (correctBoundary & domain=="square") {
             for (i in 2:nframes) {
               ##This ensures they stay in the square 0-1 x 0-1 domain
               if (any(xy[i,1:2]>1) | any(xy[i,1:2]<0)) {
                 cond <- TRUE
                 while (cond) {
                   news <- samplePolar(2,h,rho)
                   xy[i,1:2] <- xy[i-1,1:2] + (news[2,1:2]-news[1,1:2])
                   xy[i,3:4] <- news[1,3:4]
                   cond <- FALSE
                   if (any(xy[i,1:2]>1) | any(xy[i,1:2]<0)) {cond <- TRUE}
                 }
               }
             }
           }
                     
           if (correctBoundary & domain=="circle") {
             for (i in 2:nframes) {
               ##This ensures they stay in the circle with radius 1 domain
               if (sqrt(sum(xy[i,1:2]^2))>1) {	
                 cond <- TRUE
                 while (cond) {
                   news <- samplePolar(2,h,rho)
                   xy[i,] <- xy[i-1,1:2] + (news[2,1:2]-news[1,1:2])
                   xy[i,3:4] <- news[1,3:4]
                   cond <- FALSE
                   if (sqrt(sum(xy[i,1:2]^2))>1) {cond <- TRUE}
                 }
               }
             }
           }
           if (correctBoundary & !domain%in%c("square","circle")) {
             stop("domain not recognized")
           }
           
           cbind(id=j,frame=1:nframes,xy)
          }))
    
  colnames(res) <- c("id","t","x","y","r","phi")
  res <- as.data.frame(res)
  res$size <- sizes[res$id]
  attr(res,"domain") <- domain
  class(res) <- c("TrDm","simtrajectory","data.frame")
  return(res)
}

## Polar sampler used in \code{simulateTracjectories}
##
## helper function for \code{simulateTracjectories} to simulates corrected
## polar coordinates. See source of \code{simulateTracjectories} for details. 
## @param n number of movements
## @param h displacement speed in pixels.
## @param rho correlation parameter for angle of displacement
## @param init initial locations for all particles?
## @author Caspar A. Hallmann, Marjolein Bruijning & Marco D. Visser
samplePolar <- function(n=10,h=.05,rho=0,init=NULL) {
  RHO <- matrix(rho,n,n)
  diag(RHO) <- 1
  corPhiNorm <- MASS::mvrnorm(1,rep(0,n),RHO)
  corPhiUnif <- stats::pnorm(corPhiNorm)*2*pi
  r <- replicate(n,sqrt(sum(stats::rnorm(2,0,h)^2)))
  steps <- cbind(r*cos(corPhiUnif),r*sin(corPhiUnif))
  if (is.null(init)) {init <- stats::runif(2,0,1)}
  RES <- array(,c(n,4))
  RES[1,1:2] <- init
  for (i in 1:(n-1)) {
    RES[i+1,1:2] <- RES[i,1:2]+steps[i,1:2]
  }
  RES[,3] <- r
  RES[,4] <- corPhiUnif
  RES[n,3:4] <- NA
  RES
}


## add.orgamisms
##
## Add organic looking polygons to a plot of simulated trajectories 
## @param traj simulated trajectories from \code{simulateTracjectories} 
## @param col Color of particles; default is 'red'
add.organisms <- function(traj,col="red") {
  n.orgs <- max(traj$id)
  tapply(1:nrow(traj),traj$id,function(i) {
    ii <- which.max(traj[i,"t"])
	graphics::polygon(makeOrg(phi=-mean(traj[i[-ii],"phi"]),
                              x=traj[i[ii],"x"],
                              y=traj[i[ii],"y"],
                              size=traj[i[1],"size"]),col=col,border=NA)
  })
}

## makeOrg
##
## lower-level function to make organic looking polygons
#
## @param length number of polygons
## @param size rough size of each organims (radius)
## @param phi shape parameter
## @param x coordinate
## @param y coordinate
makeOrg <- function(length=30,size=.05,phi=0,x=0,y=0) {
  piseq <- seq(0,2*pi,l=length)
  fac <- c(seq(.1,1,l=length/2),seq(1,.1,l=length/2))
  xyO <- cbind(cos(piseq),fac*sin(piseq))*size
  xy0 <- xyO%*%matrix(c(cos(phi),sin(phi),-sin(phi),cos(phi)),ncol=2)
  xy0[,1] <- xy0[,1]+x
  xy0[,2] <- xy0[,2]+y
  xy0
}



##' Simulate trajectories and save as png files.
##'
##' \code{simulTrajec} simulates movement trajectories within a 
##' bounded space, movements are set with speed (\code{h}) and may be correlated
##' in direction (\code{rho}). Function simulates movement of particles in a video 
##' sequence of certain number of frames (\code{nframes}) in length. Images 
##' are saved as png files.
##' @param nframes Number of time frames(steps).
##' @param nIndividuals	Number of individual trajectories.
##' @param h Displacement speed in pixels.
##' @param rho Correlation parameter for angle of displacement.
##' @param domain One of \code{"square"} or \code{"circle"}, imposing a [0-1,0-1]
##' rectangle domain, or a circular domain of radius 1, respectively.
##' correct boundary ensure individual trajectories do not cross the domain
##' @param correctBoundary Logical. \code{TRUE} to make sure that individuals 
##' cannot leave the image.
##' @param sizes Vector of sizes for each simulated particle of length
##' nIndividuals.
##' @param path to location where the created images should be saved. By default
##' the working directory is used.
##' @param staticNoise Logical. If \code{TRUE}, static noise is added.
##' @param movingNoise Logical. If \code{TRUE}, moving noise is added.
##' @param name Stem of the filename.
##' @param parsStatic List of parameters used to generate static noise.
##' These include the density (per image) of noise particles (\code{density}), 
##' whether spots 
##' look blurry (\code{blur=TRUE} or \code{blur=FALSE}), a blurring coefficient
##' (\code{blurCoef=0.025}), and the size of the spots (with \code{sizes}).
##' @param parsMoving List of parameters used to generate moving noise
##' these include the density of noise particles (\code{density}), their duration
##' (in n frames; \code{duration}), their size (\code{size=1}), 
##' their speed (\code{speed=10}) and the range or colors they are randomly 
##' drawn from (\code{colRange=c(0,1)}).
##' @param width of created png image. By default 480.
##' @param height of create png image. If \code{NULL}, \code{width} is used.
##' @examples
##' \dontrun{
##' dir.create("images")
##' ## Create image sequence and save as png's in the working directory.
##' traj <- simulTrajec(path="images",
##'                     nframes=30,nIndividuals=20,domain="square",
##'                     h=0.01,rho=0.9,
##'                     sizes=runif(20,0.004,0.006))
##'	}
##' @author Caspar A. Hallmann, Marjolein Bruijning & Marco D. Visser
##' @export
simulTrajec <- function(nframes=20,nIndividuals=10,h=0.02,rho=0,
                        domain='square',correctBoundary=TRUE,
                        sizes=stats::runif(nIndividuals)*.012+.01,
                        staticNoise=FALSE,movingNoise=FALSE,
                        name="trajectory",
                        path=NULL,
                        parsMoving=list(density=10,duration=10,size=1,
                                        speed=10,colRange=c(0,1)),
                        parsStatic=list(density=10,blur=TRUE,blurCoef=0.025,
                                        sizes=NULL),
                        width=480,height=NULL) {

  if (is.null(path)) path <- getwd()

  ## simulate trajectories
  traj <- createTrajec(nframes=nframes,nIndividuals=nIndividuals,
                               h=h,rho=rho,domain=domain,
                               correctBoundary=correctBoundary,
                               sizes=sizes)
    
  z <- max(traj$t)
  lim <- ifelse(domain=="circle",-1,0)

  if (staticNoise) {
    background <- generateBackground(spotsDensity=parsStatic$density,
                                    blur=parsStatic$blur,
                                    blurCoef=parsStatic$blurCoef,
                                    domain=domain,
                                    sizes=parsStatic$sizes)
    xx <- background$blurred.x
  }
    
  if (movingNoise) {
    noisep <- addMovingNoiseBg(nframe=nframes,density=parsMoving$density,
                               size=parsMoving$size,
                               duration=parsMoving$duration,
                               speed=parsMoving$speed,
                               colRange=parsMoving$colRange)
    loc <- noisep[[2]]
    part <- noisep[[1]]
  }
    
  for (i in 1:z) {
    Name <- paste(name,formatC(i,width=nchar(z),flag="0"),sep="_")
    if (is.null(height)) height <- width
    grDevices::png(paste(path,'/',Name,".png",sep=""),width=width,
                   height=height)

    graphics::par(mar=c(0,0,0,0))
    graphics::plot(0,type="n",xlim=c(lim,1),ylim=c(lim,1),xlab="",
                   ylab="",asp=1,xaxs="i", yaxs="i")
        
    ## add static noise
    if (staticNoise) {
      graphics::image(xx,xx,background$blurred,add=TRUE,
                      col=grDevices::colorRampPalette(c("transparent","grey"),
                                                      alpha=TRUE)(64))
    }
        
    ## add dynamic noise	
    if (movingNoise) {
      for (mb in 1:nrow(loc)) {
        if (!is.na(loc[mb,i,2])) {
          graphics::points(loc[mb,i,2],loc[mb,i,1],pch=16,cex=part$size[mb],
                           col=part$color[mb])
        }
      }	
    }
        
    if (lim == -1) {
      graphics::lines(cos(seq(0,2*pi,l=300)),sin(seq(0,2*pi,l=300)),lty=3)
    }
        
    ## add particles
    ii <- which(traj$t==i)
    for (j in ii) {
      graphics::polygon(makeOrg(nframes,traj[j,"size"],
                                phi= ifelse(is.na(traj[j,"phi"]),
                                tapply(traj$phi,traj$id,mean,na.rm=TRUE)[j==ii],
                                traj[j,"phi"]),
                                x=traj[j,"x"],y=traj[j,"y"]),
                        col="red",border=NA)
    }
    grDevices::dev.off()
  }
  return (traj)
} 

## Generate a background 
##
## Simulates a background which can contain noise, after generating the trajectories
## using \code{\link{simulateTrajectories}}.
## @param background previously loaded or generated background. 
## @param spotsDensity density of non-moving spots.
## @param clustering spot clustering parameter.
## @param blur do the spots look blurry? (\code{TRUE} or \code{FALSE}).
## @param blurCoef spot blurring coefficient.
## @param domain want a specific domain? can be \code{"square"} of \code{"circle"}.
## @param plot Plot the background?  (\code{TRUE} or \code{FALSE}).
## @param sizes Size of noise particles.
## @author Caspar A. Hallmann, Marjolein Bruijning & Marco D. Visser
generateBackground <- function(spotsDensity=10,
                              blur=TRUE,blurCoef=.025,
                              domain=c("square","circle"),
                              sizes=NULL){

  domain <- domain[1]
  if (domain == "circle") { lim <- -1; area <- 2*pi; offset <- 0 }
  if (domain == "square") { lim <- 0; area <- 1; offset <- .5 }

  if (spotsDensity > 0) {
    n <- round(spotsDensity)
    xysp <- matrix(stats::runif(2*n,lim,1),ncol=2)
    if(domain == "circle") {
      cond <- any(toofar <- sqrt(xysp[,1]^2+xysp[,2]^2)>1)
      while (cond) {
        xysp[toofar,] <- stats::runif(sum(toofar)*2,lim,1)
        cond <- any(toofar <- sqrt(xysp[,1]^2+xysp[,2]^2)>1)
      }	
    }
    if (is.null(sizes)) sizes <- stats::runif(n,.1,1.5)
      if (blur) {
        xx <- seq(lim,1,l=512)
        xtsp <- stats::xtabs(sizes~cut(xysp[,1],c(xx[1]-diff(xx[1:2]),xx)) + 
                             cut(xysp[,2],c(xx[1]-diff(xx[1:2]),xx)))
        ffk <- outer(xx,xx,function(x,y) { stats::dnorm(x,offset,blurCoef) *
			                               stats::dnorm(y,offset,blurCoef) }
			         ) * diff(xx[1:2])^2
        blurred <- Re(stats::fft(stats::fft(ffk)*stats::fft(xtsp),inverse=TRUE)
                      ) / length(xtsp)
        blurred <- blurred[c(257:512,1:256),c(257:512,1:256)]
      }
  }

  ret <- list(xyspots=xysp,sizes=sizes)
  if (blur) {
	ret$blurred <- blurred
	ret$blurred.x <- xx
  }
  invisible(ret)
}


## generate moving "noise" to simulate noise in a video sequence
##
## generates noise (e.g. non-focal species, moving debris, light effects)
## @param nframe number of frames
## @param density number of noise particles (over all frames)
## @param size maximum size of noise in pixels
## @param duration the maximum number of frames any single generated noise
## particle is visible 
## @param bg previously generated background 
## @param speed the maximum speed of the moving noise
## @param col.range color intensity range of noise
addMovingNoiseBg <- function(nframe=30,density=40,size=1,duration=10,speed=10,
                             colRange=c(0,1)) {

  particles <- data.frame(size=stats::runif(density,0,size),
                          duration=sample(1:duration,density,replace=TRUE),
                          color=grDevices::rgb(stats::runif(density,0,0),
                                  stats::runif(density,colRange[1],colRange[2]),
                                  stats::runif(density,colRange[1],colRange[2]),
                                  alpha=stats::runif(density,colRange[1],
                                                     colRange[2])),
                          x=sample(1:512,density,replace=TRUE),
                          y=sample(1:512,density,replace=TRUE))  

  particles$color <- as.character(particles$color)
  locations <- array(dim=c(density,nframe,2))
  locations[,1,1] <- particles$x
  locations[,1,2] <- particles$y

  for (i in 2:(ncol(locations))) {
    locations[,i,1] <- (locations[,i-1,1]+sample((-speed):speed,density,
 	                                              replace=TRUE))
    locations[,i,2] <- (locations[,i-1,2]+sample((-speed):speed,density,
	                                              replace=TRUE))
  }
  locations[,,1] <- locations[,,1]/512
  locations[,,2] <- locations[,,2]/512

  for(i in 1:density) {
	locations[i,sample(1:nframe,ceiling((duration/nframe)*nframe)),] <- NA
  }

  return(list(particles,locations))
}

