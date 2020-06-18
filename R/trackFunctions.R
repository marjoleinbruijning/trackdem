## Track particles
## \code{track} is a function to track particles over subsequent frames.
## Based on tracking algorithm by Sbalzarini and Koumoutsakos (2005).
## @param Phi Cost matrix.
## @param g Start identify matrix.
## @param L Cost to link to dummy particle.
## @author Marjolein Bruijning & Marco D. Visser
## @seealso \code{\link{doTrack}}, \code{\link{linkTrajec}},
## @return List two elements: first contains array with all images,
## subset when relevant. Second element contains all original color images as
## array.
##
track <- function (Phi, g, L=50) {
  totCost <- sum(g*Phi,na.rm=TRUE)
  continue <- 1
  while (continue < 5) {
    a <- sample(1:ncol(g),ncol(g),replace=FALSE)# shuffle columns
    for (i in a) {
      reducedCost <- rep(NA,length(a))
      for (j in 1:nrow(g)) {
        if (is.na(Phi[j,i]) | ((g[j,i] == 1 | Phi[j,i] > L) |
            (g[j,i] == 1 & Phi[j,i] > L))) {

          reducedCost[j] <- NA
        } else if (!is.na(Phi[j,i]) & g[j,i] == 0 & Phi[j,i] <= L) {
            if (i > 1 & j > 1) {
              k <- which(g[j,] == 1)
              l <- which(g[,i] == 1)
              reducedCost[j] <- Phi[j,i] - Phi[j,k] - Phi[l,i] + Phi[l,k]
            } else if (!is.na(Phi[j,i]) & i == 1 & j > 1) {
              k <- which(g[j,] == 1)
              l <- 1
              reducedCost[j] <- Phi[j,i] - Phi[j,k] + Phi[l,k]
            } else if (!is.na(Phi[j,i]) & j == 1 & i > 1) {
              k <- 1
              l <- which(g[,i] == 1)
              reducedCost[j] <- Phi[j,i] - Phi[l,i] + Phi[l,k]
            } else if (!is.na(Phi[j,i]) & j == 1 & i == 1) {
              reducedCost[j] <- 0
            }
        }
      }
      if (sum(!is.na(reducedCost)) > 0) {
        if (min(reducedCost,na.rm=TRUE) < 0) {
          j <- which(reducedCost == min(reducedCost,na.rm=TRUE))[1]
          if (i > 1 & j > 1) {
            k <- which(g[j,] == 1)
            l <- which(g[,i] == 1)
            g[j,i] <- 1
            g[j,k] <- 0
            g[l,i] <- 0
            g[l,k] <- 1
          } else if (i == 1 & j > 1) {
            k <- which(g[j,] == 1)
            l <- 1
            g[j,i] <- 1
            g[j,k] <- 0
            g[l,k] <- 1
          } else if (j == 1 & i > 1) {
            k <- 1
            l <- which(g[,i] == 1)
            g[j,i] <- 1
            g[l,i] <- 0
            g[l,k] <- 1
          }
        }
      }
    }
    totCostNew <- sum(g*Phi,na.rm=TRUE)
    if (totCostNew == totCost) continue <- continue + 1
    totCost <- totCostNew
  }
  return(g)
}
## Calculate costs
## \code{calcCost} is a function to linking particle i to particle j,
## based on particle distance and size.
## @param x1 x coordinates of particles in frame n.
## @param x2 x coordinates of particles in frame n+1.
## @param y1 y coordinates of particles in frame n.
## @param y2 y coordinates of particles in frame n+1.
## @param s1 particle sizes in frame n.
## @param s2 particle sizes in frame n+1.
## @author Marjolein Bruijning & Marco D. Visser
## @seealso \code{\link{doTrack}}, \code{\link{linkTrajec}},
##
calcCost <- function(x1,x2,y1,y2,s1,s2,weight=c(1,1,1),predLoc=FALSE,
                     x0=NULL,y0=NULL,logsizes=logsizes) {


  if (logsizes) {
    s1 <- log(s1)
    s2 <- log(s2)
  }

  if (predLoc) {
    predx <- x1-x0 + x1
    predy <- y1-y0 + y1
    sqrt(weight[1] * (x1-x2)^2 +
         weight[1] * (y1-y2)^2 +
         weight[2] * (s1-s2)^2 +
         weight[3] * (x2-predx)^2 +
         weight[3] * (y2-predy)^2)
  } else {
    sqrt((weight[1]+weight[3]) * (x1-x2)^2 +
         (weight[1]+weight[3]) * (y1-y2)^2 +
         weight[2] * (s1-s2)^2)
  }
}

## Create cost matrix
## \code{phiMat} is a function to create a cost matrix based on defined
## cost function \code{\link{calcCost}}.
## @param coords1 Coordinates of particles in frame n.
## @param coords2 Coordinates of particles in frame n+1.
## @param sizes1 Sizes of particles in frame n.
## @param sizes2 Sizes of particles in frame n+1.
## @param r Default is one; scaling parameter.
## @param L Cost of linking to dummy variable.
## @author Marjolein Bruijning & Marco D. Visser
## @return Cost matrix linking particles.
##
phiMat <- function (coords1,coords2,sizes1,sizes2,r=1,L=50,weight=weight,
                    coords0=NULL,logsizes=logsizes) {

  Phi <- vapply(seq_len(nrow(coords1)),function(x) {
	  if (length(coords0[rownames(coords0) == x,]) > 0) {
        predLoc <- TRUE
      } else {
        predLoc <- FALSE
      }
	  calcCost(x1=coords1[x,1],
	           x2=coords2[,1],
	           y1=coords1[x,2],
             y2=coords2[,2],
		         s1=sizes1[x],
		         s2=sizes2,
		         weight=weight,
		         predLoc=predLoc,
		         x0=coords0[rownames(coords0) == x,1],
		         y0=coords0[rownames(coords0) == x,2],
             logsizes=logsizes)
   }, numeric(length(coords2[,1])))

  Phi <- matrix(Phi,ncol=dim(coords1)[1],nrow=dim(coords2)[1])
  Phi <- cbind(rep(L*r,nrow(Phi)),Phi)
  Phi <- rbind(rep(L*r,ncol(Phi)),Phi)
  return(Phi)
}

## Create random start g matrix
## \code{gMat} is a function to create a random G-matrix, that
## can be implemented in \code{\link{doTrack}}.
## @param Phi Cost matrix.
## @author Marjolein Bruijning & Marco D. Visser
## @return Random G-matrix
##
gMat <- function (Phi) {
  g <- matrix(0,nrow=nrow(Phi),ncol=ncol(Phi))
  sequence <- sample(2:nrow(Phi),ncol(Phi)-1,replace=TRUE)
  for (i in 2:ncol(g)) { g[unique(sequence)[i-1],i] <- 1 }
  g[1,colSums(g) == 0] <- 1
  g[rowSums(g) == 0,1] <- 1
  return(g)
}

## Link created track segments
##
## \code{linkTrajec} is a function to merge track segments, based on
## distance and size of particles
## recordsObject Object of class records.
## particles Object of class particleStatistics.
## @param R Default is one; link to how many subsequent frames?
## @param L Cost of linking to dummy variable, default is 50.
## @author Marjolein Bruijning & Marco D. Visser
## @return Returns a list of class 'records', containing all merged
## track segments. See 'summary' and 'plot'.
## @export
##
linkTrajec <- function (recordsObject,particles,
                        L=50,R=1,weight=weight,costconstant=costconstant,
                        logsizes=logsizes) {

  trackRecord <- recordsObject$trackRecord
  sizeRecord <- recordsObject$sizeRecord
  colorRecord <- recordsObject$colorRecord
  label <- recordsObject$label

  n <- unique(particles$frame)

  cat("\t Link track segments: 0 %            ")

  for (r in 1:R) {
    for (i in 1:(length(n)-1-r)) {
      inc <- particles$frame == i
      inc2 <- particles$frame == (i + r)
      endTrajec <- as.vector(label[apply(
                             trackRecord[,i:(i+1),1],1,function(x)
                                             !is.na(x[1]) & is.na(x[2])),i])
      beginTrajec <- as.vector(label[apply(
                             trackRecord[,(i+r-1):(i+r),1],1,function(x)
                                                is.na(x[1]) & !is.na(x[2])),i+r])
      beginTrajec <- beginTrajec[beginTrajec != 0]

      if (length(endTrajec)>0 & length(beginTrajec)>0) {

        coords1 <- matrix(c(particles[inc,]$x[endTrajec],
                   particles[inc,]$y[endTrajec]),ncol=2,byrow=FALSE)
        sizes1 <- particles[inc,]$n.cell[endTrajec]

        coords2 <- matrix(c(particles[inc2,]$x[beginTrajec],
                   particles[inc2,]$y[beginTrajec]),ncol=2,byrow=FALSE)
        sizes2 <- particles[inc2,]$n.cell[beginTrajec]

        if (costconstant) {
          Lnew <- L
        } else {Lnew <- L*r}


        Phi <- phiMat(coords1,coords2,
                    sizes1=sizes1,
                    sizes2=sizes2,
                    L=Lnew,r=1,weight=weight,coords0=NULL,
                    logsizes=logsizes)
        gstart <- matrix(0,nrow=nrow(Phi),ncol=ncol(Phi))
        gstart[1,] <- 1
        gstart[,1] <- 1
        A <- track(Phi=Phi, g=gstart, L=Lnew) * Phi

        tmp <- data.frame(which(A > 0,TRUE))
        tmp <- tmp[tmp[,1] != 1,]
        tmp <- tmp[tmp[,2] != 1,]

        # if any successful links
        if (dim(tmp)[1] > 0) {
          for (k in 1:(dim(tmp)[1])) {
           ind1 <- which(label[,i] ==  endTrajec[tmp[k,2]-1])
           ind2 <- which(label[,i+r] ==  beginTrajec[tmp[k,1]-1])

            # combine trajectories
            trackRecord[ind1,,1] <- pmin(trackRecord[ind1,,1],trackRecord[ind2,,1],
                                         na.rm=TRUE)
            trackRecord[ind1,,2] <- pmin(trackRecord[ind1,,2],trackRecord[ind2,,2],
                                         na.rm=TRUE)
            label[ind1,] <- pmin(label[ind1,],label[ind2,],na.rm=TRUE)
            sizeRecord[ind1,] <- pmin(sizeRecord[ind1,],sizeRecord[ind2,],
                                      na.rm=TRUE)
            colorRecord[ind1,,1] <- pmin(colorRecord[ind1,,1],colorRecord[ind2,,1],
                                         na.rm=TRUE)
            colorRecord[ind1,,2] <- pmin(colorRecord[ind1,,2],colorRecord[ind2,,2],
                                         na.rm=TRUE)
            colorRecord[ind1,,3] <- pmin(colorRecord[ind1,,3],colorRecord[ind2,,3],
                                         na.rm=TRUE)

            # Take mean values for coordinates in between
            if (r > 1) {
              trackRecord[ind1,(i+1):(i+r-1),1] <- (trackRecord[ind1,i,1] +
                                                   trackRecord[ind1,i+r,1]) / 2
              trackRecord[ind1,(i+1):(i+r-1),2] <- (trackRecord[ind1,i,2] +
                                                   trackRecord[ind1,i+r,2]) / 2
            }
            # Remove extra rows
            trackRecord <- trackRecord[-ind2,,]
            label <- label[-ind2,]
            sizeRecord <- sizeRecord[-ind2,]
            colorRecord <- colorRecord[-ind2,,]

          }
        }
      }
      cat("\r \t Link track segments: ",round(r / R * 100, 1),"%           ")
    }
  }

  inc <- apply(trackRecord[,,1],1,function(x) !all(is.na(x)))
  res <- list(trackRecord=trackRecord[inc,,],
              label=label[inc,],
              sizeRecord=sizeRecord[inc,],colorRecord=colorRecord[inc,,])

  return(res)
}

## Track particles
## \code{doTrack} is a helper function link particles using \code{\link{track}}
## @param particles Object with class particleStatistics,
## obtained using \code{\link{identifyParticles}}.
## @param L Cost for linking to dummy. Default set at 50.
## @param sizeMeasure Measure for size (area, length, etc.).
## Currently not implemented.
## @author Marjolein Bruijning & Marco D. Visser
## @return A list of class 'records'. Use 'summary' and 'plot'.
##

doTrack <- function(particles,L=50,sizeMeasure='n.cell',weight=weight,
                    logsizes=logsizes) {

  n <- unique(particles$frame)

  links <- list()

  cat("\t Create track segments: ","0","%           ")

  for (i in seq_along(n[-1])) {
	  inc <- particles$frame == i
	  inc2 <- particles$frame == (i + 1)
    coords1 <- matrix(c(particles[inc,]$x,
                      particles[inc,]$y),ncol=2,byrow=FALSE)
    sizes1 <- particles[inc,]$n.cell
    coords2 <- matrix(c(particles[inc2,]$x,
                      particles[inc2,]$y),ncol=2,byrow=FALSE)
    sizes2 <- particles[inc2,]$n.cell

    if (i > 1) {
      coords0 <- matrix(c(particles[particles$frame == (i-1),]$x,
                        particles[particles$frame == (i-1),]$y),
                        ncol=2,byrow=FALSE)
      # labels for previous linked frame
      tmp <- links[[i-1]][,2]
      # combine with labels for frame i
      names(tmp) <- links[[i-1]][,1]
      # only succesful links
      tmp <- tmp[tmp > 1] - 1
      tmp <- tmp[names(tmp) > 1]

      coords0 <- coords0[tmp,,drop=FALSE]
      rownames(coords0) <- as.numeric(names(tmp)) - 1

    } else {coords0 <- NULL}

    ## create cost matrix
    Phi <- phiMat(coords1,coords2,
	              sizes1=sizes1,
	              sizes2=sizes2,
	              L=L,r=1,weight=weight,
	              coords0=coords0,
                logsizes=logsizes)

    gstart <- gMat(Phi)

    ## optimize
    G <- track(Phi=Phi, g=gstart, L=L) * (Phi+0.0001)

    links[[i]] <- data.frame(which(G > 0,TRUE))
    if (i > 1) {
      links[[i]] <- links[[i]][links[[i]][,2] != 1,]
    }
    names(links[[i]]) <- c(paste('frame',i+1),paste('frame',i))
    if (i == 1) {allLinks <- links[[1]]
    } else {
        allLinks <- merge(links[[i]],allLinks,by=paste('frame',i),
                          all.x=TRUE,all.y=TRUE,
                          suffixes=c('',paste0('.',i)))
    }

  cat("\r \t Create track segments: ",round(i / (length(n)-1) * 100, 1),
      "%          ")

  }

  ## Get coordinates
  tmp <- as.numeric(unlist(lapply(strsplit(unlist(lapply(
                      strsplit(names(allLinks),'.',fixed=TRUE),
                      function(x) x[1])),'frame'),function(y) y[2])))
  allnames <- sort(unique(tmp))
  for (i in seq_along(allnames)){
    n <- which(tmp == allnames[i])
    if (length(n) > 1) {
      allLinks[,n] <- rowSums(allLinks[,n],na.rm=TRUE)
    }
  }
  allLinks <- allLinks - 1

  allLinks[allLinks == 0] <- NA
  allLinks <- allLinks[,duplicated(tmp)==FALSE]

  trackRecord <- array(NA,dim=c(dim(allLinks)[1],dim(allLinks)[2],2))
  sizeRecord <- matrix(NA,nrow=dim(allLinks)[1],ncol=dim(allLinks)[2])
  colorRecord <- array(NA,dim=c(dim(allLinks)[1],dim(allLinks)[2],3))

  label <- matrix(NA,nrow=dim(allLinks)[1],ncol=dim(allLinks)[2])
  a <- tmp[duplicated(tmp)==FALSE]

  for (i in seq_along(a)) {
	  inc <- particles$frame == i
    label[,i] <- allLinks[,order(a)[i]]
  	trackRecord [,i,1] <- particles[inc,][allLinks[,order(a)[i]],'x']
	  trackRecord [,i,2] <- particles[inc,][allLinks[,order(a)[i]],'y']
	  sizeRecord[,i] <- particles[inc,][allLinks[,order(a)[i]],sizeMeasure]

    colorRecord [,i,1] <- particles[inc,][allLinks[,order(a)[i]],'muR']
  	colorRecord [,i,2] <- particles[inc,][allLinks[,order(a)[i]],'muG']
	  colorRecord [,i,3] <- particles[inc,][allLinks[,order(a)[i]],'muB']

  }
  res <- list(trackRecord=trackRecord,sizeRecord=sizeRecord,
              colorRecord=colorRecord,label=label)
  return(res)
}


##' Merge track records
##'
##' \code{mergeTracks} attempts to merge to two track objects as obtained by
##' \code{\link{trackParticles}}.
##' @param records1 Object of class 'tracked',
##' obtained using \code{\link{trackParticles}}.
##' @param records2 Object of class 'tracked',
##' obtained using \code{\link{trackParticles}} that should be linked to
##' \code{records1}.
##' @param L Numeric. Maximum cost for linking a particle to another particle.
##' When the cost is larger,
##' particles will be not be linked (resulting in the begin or end of a segment).
##' If \code{NULL}, the same \code{L} as used to create
##' \code{records2} is used.
##' @param weight Vector containing 3 weights to calculate costs. Depending
##' on the study system user may want to value certain elements over others.
##' Weights are ordered as follows;
##' first number gives the weight for differences in x and y coordinates;
##' second number
##' gives the weight for particle size differences. Note that the
##' difference between the predicted location and the observed location is
##' not taken into account in this function. If \code{NULL}, the same weights as
##' used to create \code{records2} is used.
##' @param logsizes Logical. Default is \code{FALSE}. Set to \code{TRUE} to take the
##' natural logarithm of body sizes, when calculating the cost of linking two particles.
##' @author Marjolein Bruijning, Caspar A. Hallmann & Marco D. Visser
##' @examples
##' \dontrun{
##' ## Create image sequence
##' dir.create("images")
##' traj <- simulTrajec(path="images",
##'                     nframes=60,nIndividuals=20,domain="square",
##'                     h=0.01,rho=0.9,sizes=runif(20,0.004,0.006))
##' ## Analyse first part
##' dir <- "images"
##' allFullImages1 <- loadImages (dirPictures=dir,nImages=1:30)
##' stillBack1 <- createBackground(allFullImages1)
##' allImages1 <- subtractBackground(bg=stillBack1)
##' partIden1 <- identifyParticles(sbg=allImages1,
##'                               pixelRange=c(1,500),
##'                               threshold=-0.1)
##' records1 <- trackParticles(partIden1,L=20,R=2)
##' ## Analyse second part
##' allFullImages2 <- loadImages (dirPictures=dir,nImages=31:60)
##' stillBack2 <- createBackground(allFullImages2)
##' allImages2 <- subtractBackground(bg=stillBack2)
##' partIden2 <- identifyParticles(sbg=allImages2,
##'                               pixelRange=c(1,500),
##'                               threshold=-0.1)
##' records2 <- trackParticles(partIden2,L=20,R=2)
##' ## Merge tracks
##' records <- mergeTracks(records1,records2)
##' plot(records,colorimages=allFullImages1,type="trajectories",incThres=10)
##'	}
##' @return A list of class 'TrDm' and 'records'. Use 'summary' and 'plot'.
##' @export
##
mergeTracks <- function(records1,records2,L=NULL,weight=NULL,
                        logsizes=FALSE) {

  if (is.null(L)) L <- attributes(records2)$settings$L
  if (is.null(weight)) weight <- attributes(records2)$settings$weight

  coords1 <- records1$trackRecord[,ncol(records1$trackRecord),]
  coords2 <- records2$trackRecord[,1,]
  sizes1 <- records1$sizeRecord[,ncol(records1$sizeRecord)]
  sizes2 <- records2$sizeRecord[,1]
  label1 <- records1$label[,ncol(records1$label)]
  label2 <- records2$label[,1]


  saveNA1 <- is.na(label1)
  saveNA2 <- is.na(label2)

  Phi <- phiMat(coords1[!saveNA1,],coords2[!saveNA2,],
	            sizes1=sizes1[!saveNA1],
	            sizes2=sizes2[!saveNA2],
	            L=L,r=1,weight=weight,
	            coords0=NULL,logsizes=logsizes)

  gstart <- gMat(Phi)

  ## optimize
  G <- array(NA,dim=c(2000,2000))
  G[1:nrow(Phi),1:ncol(Phi)] <- track(Phi=Phi, g=gstart, L=L) * (Phi+0.0001)

  links <- data.frame(which(G > 0,TRUE))
  names(links) <- c(paste('frame',2),paste('frame',1))
  links <- links - 1

  links[links == 0] <- NA
  links <- stats::na.omit(links)

  for (x in 1:nrow(links)) {
    links[x,2] <- seq_len(nrow(coords1))[!saveNA1][links[x,2]]
    links[x,1] <- seq_len(nrow(coords2))[!saveNA2][links[x,1]]
  }

  fillMat <- function(mat1,mat2,l=links) {
    A <- matrix(NA,ncol=ncol(mat1) + ncol(mat2),nrow=nrow(l))
    for (i in 1:nrow(l)) {
      A[i,] <- c(mat1[l[i,2],],mat2[l[i,1],])
    }
    todo1 <- !seq_len(nrow(mat1)) %in% l[,2]
    m <- mat1[todo1,]
    m <- cbind(m,matrix(NA,ncol=ncol(mat2),nrow=nrow(m)))

    todo2 <- !seq_len(nrow(mat2)) %in% l[,1]
    m2 <- mat2[todo2,]
    m2 <- cbind(matrix(NA,ncol=ncol(mat1),nrow=nrow(m2)),m2)

    return(rbind(A,m,m2))
  }

  label <- fillMat(records1$label,records2$label)
  sizeRecord <- fillMat(records1$sizeRecord,records2$sizeRecord)

  trackRecord <- array(NA,dim=c(dim(sizeRecord),2))
  trackRecord[,,1] <- fillMat(records1$trackRecord[,,1],
                              records2$trackRecord[,,1])
  trackRecord[,,2] <- fillMat(records1$trackRecord[,,2],
                              records2$trackRecord[,,2])
  colorRecord <- array(NA,dim=c(dim(sizeRecord),3))
  colorRecord[,,1] <- fillMat(records1$colorRecord[,,1],
                         records2$colorRecord[,,1])
  colorRecord[,,2] <- fillMat(records1$colorRecord[,,2],
                         records2$colorRecord[,,2])
  colorRecord[,,3] <- fillMat(records1$colorRecord[,,3],
                         records2$colorRecord[,,3])

  rec <- list(trackRecord=trackRecord,sizeRecord=sizeRecord,
              colorRecord=colorRecord,label=label)

  class(rec) <- c('TrDm','tracked')
  attr(rec,"background") <- attributes(records1)$background
  attr(rec,"originalImages") <- attributes(records1)$originalImages
  attr(rec,"subtractedImages") <- attributes(records1)$subtractedImages
  attr(rec,"images") <- attributes(records1)$images
  attr(rec,"settings1") <- attributes(records1)$settings

  attr(rec,"background2") <- attributes(records2)$background
  attr(rec,"originalImages2") <- attributes(records2)$originalImages
  attr(rec,"subtractedImages2") <- attributes(records2)$subtractedImages
  attr(rec,"images2") <- attributes(records2)$images
  attr(rec,"settings2") <- attributes(records2)$settings

  return(rec)
}


##' Track particles
##'
##' \code{trackParticles} reconstructs trajectories by linking particles.
##' @param particles Object of class 'particles',
##' obtained using \code{\link{identifyParticles}}.
##' @param L Numeric. Maximum cost for linking a particle to another particle.
##' When the cost is larger,
##' particles will be not be linked (resulting in the begin or end of a segment).
##'  Default set at \code{50}.
##' @param R Integer. Link to how many subsequent frames? Default set
##' at \code{2}.
##' @param weight Vector containing 3 weights to calculate costs. Depending
##' on the study system, users may want to value certain elements over others.
##' For instance, when individuals can vary in size over frames
##' (which happens when objects move away or towards a camera)
##' the "size" weight may be decreased. Weights are ordered as follows;
##' first number gives the weight for differences in x and y coordinates;
##' second number
##' gives the weight for particle size differences; third number gives the
##' difference between the predicted location and the observed location. The
##' latter is calculated using the location of the identified particle in the
##' previous frame.
##' @param costconstant Logical. Default is \code{FALSE}. This increases the maximum
##' cost L proportional to the number of images in between two frames (when R > 1).
##' Set to \code{TRUE} keeps maximum cost L constant for all 1:R frames.
##' @param logsizes Logical. Default is \code{FALSE}. Set to \code{TRUE} to take the
##' natural logarithm of body sizes, when calculating the cost of linking two particles.
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
##' records <- trackParticles(particles,L=40,R=2)
##' summary(records)
##' plot(records,type="trajectories")
##'	}
##' @return A list of class 'TrDm' and 'records'. Use 'summary' and 'plot'.
##' @export
##
trackParticles <- function (particles,L=50,R=2,
                            weight=c(1,1,1),costconstant=FALSE,logsizes=FALSE) {
  records <- doTrack(particles=particles,L=L,weight=weight,logsizes=logsizes)
  cat("\n")
  rec <- linkTrajec (recordsObject=records,
                        particles=particles,
                        R=R,L=L,weight=weight,costconstant=costconstant,
                        logsizes=logsizes)
  cat("\n")
  class(rec) <- c('TrDm','tracked')
  attr(rec,"background") <- attributes(particles)$background
  attr(rec,"originalImages") <- attributes(particles)$originalImages
  attr(rec,"subtractedImages") <- attributes(particles)$subtractedImages
  attr(rec,"images") <- attributes(particles)$images
  attr(rec,"settings") <- c(attributes(particles)$settings,
                            list(R=R,L=L,weight=weight))
  return(rec)
}
