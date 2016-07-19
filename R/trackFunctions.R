##' Track particles
##' \code{track} is a function to track particles over subsequent frames.
##' Based on tracking algorithm by Sbalzarini and Koumoutsakos (2005).
##' @param Phi Cost matrix.
##' @param g Start identify matrix.
##' @param L Cost to link to dummy particle.
##' @author Marjolein Bruijning & Marco D. Visser
##' @seealso \code{\link{doTrack}}, \code{\link{linkTrajec}},
##' @return List two elements: first contains array with all images,
##' subset when relevant. Second element contains all original color images as array.
##' @concept What is the broad searchable concept?
##' @export
## 
track <- function (Phi, g, L=50) {
  totCost <- sum(g*Phi,na.rm=T)
  continue <- 1
  while (continue < 5) {
    a <- sample(1:ncol(g),ncol(g),replace=F)# shuffle columns
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
        if (min(reducedCost,na.rm=T) < 0) {
          j <- which(reducedCost == min(reducedCost,na.rm=T))[1]
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
    totCostNew <- sum(g*Phi,na.rm=T)
    if (totCostNew == totCost) continue <- continue + 1
    totCost <- totCostNew
  }
  return(g)
}
##' Calculate costs
##' \code{calcCost} is a function to linking particle i to particle j,
##' based on particle distance and size.
##' @param x1 x coordinates of particles in frame n.
##' @param x2 x coordinates of particles in frame n+1.
##' @param y1 y coordinates of particles in frame n.
##' @param y2 y coordinates of particles in frame n+1.
##' @param s1 particle sizes in frame n.
##' @param s2 particle sizes in frame n+1.
##' @author Marjolein Bruijning & Marco D. Visser
##' @seealso \code{\link{doTrack}}, \code{\link{linkTrajec}},
##' @export
## 
calcCost <- function(x1,x2,y1,y2,s1,s2) {
  sqrt((x1-x2)^2 + (y1-y2)^2 + (s1-s2)^2)
}

##' Create cost matrix
##' \code{phiMat} is a function to create a cost matrix based on defined
##' cost function \code{\link{calcCost}}.
##' @param coords1 Coordinates of particles in frame n.
##' @param coords2 Coordinates of particles in frame n+1.
##' @param sizes1 Sizes of particles in frame n.
##' @param sizes2 Sizes of particles in frame n+1.
##' @param r Default is one; scaling parameter.
##' @param L Cost of linking to dummy variable.
##' @author Marjolein Bruijning & Marco D. Visser
##' @seealso \code{\link{doTrack}}, \code{\link{linkTrajec}},
##' @return Cost matrix linking particles.
##' @export
## 
phiMat <- function (coords1,coords2,sizes1,sizes2,r=1,L=50) {
  Phi <- sapply(1:nrow(coords1),function(x) calcCost(coords1[x,1],
                                                     coords2[,1],
                                                     coords1[x,2],
                                                     coords2[,2],
		                                             sizes1[x],
		                                             sizes2)
                )
  Phi <- matrix(Phi,ncol=dim(coords1)[1],nrow=dim(coords2)[1])
  Phi <- cbind(rep(L*r,nrow(Phi)),Phi)
  Phi <- rbind(rep(L*r,ncol(Phi)),Phi)
  return(Phi)
}

##' Create random start g matrix
##' \code{gMat} is a function to create a random G-matrix, that
##' can be implemented in \code{\link{doTrack}}.
##' @param Phi Cost matrix.
##' @author Marjolein Bruijning & Marco D. Visser
##' @seealso \code{\link{doTrack}}, \code{\link{linkTrajec}},
##' @return Random G-matrix
##' @export
## 
gMat <- function (Phi) {
  g <- matrix(0,nrow=nrow(Phi),ncol=ncol(Phi))
  sequence <- sample(2:nrow(Phi),ncol(Phi)-1,replace=T)
  sapply(2:ncol(g),function(x) g[unique(sequence)[x-1],x] <<- 1)
  g[1,colSums(g) == 0] <- 1
  g[rowSums(g) == 0,1] <- 1
  return(g)
}

##' Link created track segments
##'
##' \code{linkTrajec} is a function to merge track segments, based on 
##' distance and size of particles, using a record object 
##' provided by \code{\link{doTrack}}.
##' recordsObject Object of class records.
##' particleStatObject Object of class particleStatistics.
##' @param R Default is one; link to how many subsequent frames?
##' @param L Cost of linking to dummy variable, default is 50.
##' @author Marjolein Bruijning & Marco D. Visser
##' @seealso \code{\link{doTrack}}
##' @return Returns a list of class 'records', containing all merged
##' track segments. See 'summary' and 'plot'.
##' @export
## 
linkTrajec <- function (recordsObject,particleStatObject,
                        L=50,R=1,incThres=10) {
 
  trackRecord <- recordsObject$trackRecord
  sizeRecord <- recordsObject$sizeRecord
  label <- recordsObject$label
  G <- recordsObject$G
  trackRecord[is.na(trackRecord)] <- 0                             
  label[is.na(label)] <- 0   
  sizeRecord[is.na(sizeRecord)] <- 0     
 
  for (r in 1:R) {
    A <- array(NA,dim=c(500,500,length(particleStatObject)-r-1))
    links <- list()
      
    for (i in 1:(dim(G)[3]-r)) {
      endTrajec <- as.vector(na.omit(label[apply(
                             trackRecord[,i:(i+1),1],1,function(x) 
                                             x[1] != 0 & x[2] == 0),i]))
      beginTrajec <- as.vector(na.omit(label[apply(
                             trackRecord[,(i+r-1):(i+r),1],1,function(x) 
                                                x[1]==0 & x[2]>0),i+r]))
      beginTrajec <- beginTrajec[beginTrajec != 0]
    
      if (length(endTrajec)>0 & length(beginTrajec)>0) {    
        coords1 <- matrix(c(particleStatObject[[i]]$x[endTrajec],
                   particleStatObject[[i]]$y[endTrajec]),ncol=2,byrow=F)
        sizes1 <- particleStatObject[[i]]$n.cell[endTrajec]           
    
        coords2 <- matrix(c(particleStatObject[[i+r]]$x[beginTrajec],
                   particleStatObject[[i+r]]$y[beginTrajec]),ncol=2,byrow=F)
        sizes2 <- particleStatObject[[i+r]]$n.cell[beginTrajec]
    
        Phi <- phiMat(coords1,coords2,
	                  sizes1=sizes1,
	                  sizes2=sizes2,
	                  L=L/r,r=1)   
        gstart <- gMat(Phi)
        A[1:nrow(Phi),1:ncol(Phi),i] <- track(Phi=Phi, g=gstart, L=L/r) * Phi
      
        tmp <- data.frame(which(A[,,i] > 0,TRUE))
        tmp <- tmp[tmp[,1] != 1,]
        tmp <- tmp[tmp[,2] != 1,]
     
        if (dim(tmp)[1] > 0) {
          for (k in 1:(dim(tmp)[1])) {
           ind1 <- which(label[,i] ==  endTrajec[tmp[k,2]-1])
           ind2 <- which(label[,i+r] ==  beginTrajec[tmp[k,1]-1])
     
            trackRecord[ind1,,1] <- trackRecord[ind1,,1] + trackRecord[ind2,,1]
            trackRecord[ind1,,2] <- trackRecord[ind1,,2] + trackRecord[ind2,,2]
            label[ind1,] <- label[ind1,] + label[ind2,]
            sizeRecord[ind1,] <- sizeRecord[ind1,] + sizeRecord[ind2,]
          
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
          } 
        }                
      } 
    }
  }
 
  trackRecord[trackRecord == 0] <- NA                             
  label[label == 0] <- NA 
  sizeRecord[sizeRecord == 0] <- NA         
  res <- list(trackRecord=trackRecord,A=A,label=label,sizeRecord=sizeRecord)
  attr(res, "class") <- "records"
  return(res) 
}

##' Track particles
##' \code{doTrack} is a function link particles using \code{\link{track}}
##' @param particleStatObject Object with class particleStatistics,
##' obtained using \code{\link{identifyParticles}}.
##' @param L Cost for linking to dummy. Default set at 50.
##' @param backward Reverse frames. Default is FALSE.
##' @param sizeMeasure Measure for size (area, length, etc.).
##' Currently not implemented.
##' @author Marjolein Bruijning & Marco D. Visser
##' @seealso \code{\link{doTrack}}, \code{\link{linkTrajec}},
##' @return A list of class 'records'. Use 'summary' and 'plot'.
##' @export
## 
doTrack <- function(particleStatObject,L=50,
                    backward=FALSE,
                    sizeMeasure='n.cell') {

  G <- array(NA,dim=c(500,500,length(particleStatObject)-1))
  links <- list()
  if (backward == TRUE) {
    n <- length(particleStatObject)
    particleStatObject <- particleStatObject[n:1]
  }

  for (i in 1:(length(particleStatObject)-1)) {
    coords1 <- matrix(c(particleStatObject[[i]]$x,
                      particleStatObject[[i]]$y),ncol=2,byrow=F)
    sizes1 <- particleStatObject[[i]]$n.cell
    coords2 <- matrix(c(particleStatObject[[i+1]]$x,
                      particleStatObject[[i+1]]$y),ncol=2,byrow=F)	
    sizes2 <- particleStatObject[[i+1]]$n.cell
    Phi <- phiMat(coords1,coords2,
	              sizes1=sizes1,
	              sizes2=sizes2,
	              L=L,r=1)
    gstart <- gMat(Phi)
    G[1:nrow(Phi),1:ncol(Phi),i] <- track(Phi=Phi, g=gstart, L=L) * Phi	
  
    links[[i]] <- data.frame(which(G[,,i] > 0,TRUE))
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
  }
  ## Get coordinates
  tmp <- as.numeric(unlist(lapply(strsplit(unlist(lapply(
                      strsplit(names(allLinks),'.',fixed=TRUE),
                      function(x) x[1])),'frame'),function(y) y[2])))
  allnames <- sort(unique(tmp))
  for (i in 1:length(allnames)){ 
    n <- which(tmp == allnames[i])
    if (length(n) > 1) {
      allLinks[,n] <- rowSums(allLinks[,n],na.rm=T)
    }
  }
  allLinks <- allLinks - 1

  allLinks[allLinks == 0] <- NA
  allLinks <- allLinks[,duplicated(tmp)==F]

  trackRecord <- array(NA,dim=c(dim(allLinks)[1],dim(allLinks)[2],2))
  sizeRecord <- matrix(NA,nrow=dim(allLinks)[1],ncol=dim(allLinks)[2])
  label <- matrix(NA,nrow=dim(allLinks)[1],ncol=dim(allLinks)[2])
  a <- tmp[duplicated(tmp)==F]

  for (i in 1:(length(a))) {
    label[,i] <- allLinks[,order(a)[i]]
	trackRecord [,i,1] <- particleStatObject[[i]][allLinks[,order(a)[i]],'x']
	trackRecord [,i,2] <- particleStatObject[[i]][allLinks[,order(a)[i]],'y']
	sizeRecord[,i] <- particleStatObject[[i]][allLinks[,order(a)[i]],sizeMeasure]
  }
  res <- list(trackRecord=trackRecord,sizeRecord=sizeRecord,label=label,G=G)
  attr(res, "class") <- "records"
  return(res)
}

