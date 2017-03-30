## Track particles
## \code{track} is a function to track particles over subsequent frames.
## Based on tracking algorithm by Sbalzarini and Koumoutsakos (2005).
## @param Phi Cost matrix.
## @param g Start identify matrix.
## @param L Cost to link to dummy particle.
## @author Marjolein Bruijning & Marco D. Visser
## @seealso \code{\link{doTrack}}, \code{\link{linkTrajec}},
## @return List two elements: first contains array with all images,
## subset when relevant. Second element contains all original color images as array.
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
                     x0=NULL,y0=NULL) {
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
                    coords0=NULL) {
  
  Phi <- sapply(1:nrow(coords1),function(x) {
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
		       y0=coords0[rownames(coords0) == x,2])
   })
   
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
  sequence <- sample(2:nrow(Phi),ncol(Phi)-1,replace=T)
  sapply(2:ncol(g),function(x) g[unique(sequence)[x-1],x] <<- 1)
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
                        L=50,R=1,weight=weight) {
 
  trackRecord <- recordsObject$trackRecord
  sizeRecord <- recordsObject$sizeRecord
  colorRecord <- recordsObject$colorRecord
  label <- recordsObject$label
  G <- recordsObject$G
  trackRecord[is.na(trackRecord)] <- 0
  label[is.na(label)] <- 0   
  sizeRecord[is.na(sizeRecord)] <- 0
 
  colorRecord[colorRecord == 0 & !is.na(colorRecord)] <- 
         colorRecord[colorRecord == 0 & !is.na(colorRecord)] + 0.000001
  colorRecord[is.na(colorRecord)] <- 0
  
  n <- unique(particles$frame)
  
  cat("\t Link track segments: 0 %")
  
  for (r in 1:R) {
    A <- array(NA,dim=c(1000,1000,length(n)-r-1))
    links <- list()
      
    for (i in 1:(dim(G)[3]-r)) {
      inc <- particles$frame == i
      inc2 <- particles$frame == (i + r)
      endTrajec <- as.vector(na.omit(label[apply(
                             trackRecord[,i:(i+1),1],1,function(x) 
                                             x[1] != 0 & x[2] == 0),i]))
      beginTrajec <- as.vector(na.omit(label[apply(
                             trackRecord[,(i+r-1):(i+r),1],1,function(x) 
                                                x[1]==0 & x[2]>0),i+r]))
      beginTrajec <- beginTrajec[beginTrajec != 0]
    
      if (length(endTrajec)>0 & length(beginTrajec)>0) {    
        
        if (i > 1) {
          tmp <- as.vector(na.omit(label[apply(
                               trackRecord[,i:(i+1),1],1,function(x) 
                                               x[1] != 0 & x[2] == 0),i-1]))
          tmp <- tmp[tmp > 1] - 1
          coords0 <- matrix(c(particles[particles$frame == (i-1),]$x[tmp],
                   particles[particles$frame == (i-1),]$y[tmp]),ncol=2,byrow=F)
          rownames(coords0) <- tmp
        }

        coords1 <- matrix(c(particles[inc,]$x[endTrajec],
                   particles[inc,]$y[endTrajec]),ncol=2,byrow=F)
        sizes1 <- particles[inc,]$n.cell[endTrajec]           
    
        coords2 <- matrix(c(particles[inc2,]$x[beginTrajec],
                   particles[inc2,]$y[beginTrajec]),ncol=2,byrow=F)
        sizes2 <- particles[inc2,]$n.cell[beginTrajec]
    
        Phi <- phiMat(coords1,coords2,
	                  sizes1=sizes1,
	                  sizes2=sizes2,
	                  L=L*r,r=1,weight=weight,coords0=NULL)
        gstart <- gMat(Phi)
        A[1:nrow(Phi),1:ncol(Phi),i] <- track(Phi=Phi, g=gstart, L=L*r) * Phi
      
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
            colorRecord[ind1,,1] <- colorRecord[ind1,,1] + colorRecord[ind2,,1]
            colorRecord[ind1,,2] <- colorRecord[ind1,,2] + colorRecord[ind2,,2]
            colorRecord[ind1,,3] <- colorRecord[ind1,,3] + colorRecord[ind2,,3]
          
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
      cat("\r \t Link track segments: ",round(r / R * 100, 1),"%")
    }
  }
 
  trackRecord[trackRecord == 0] <- NA                             
  label[label == 0] <- NA 
  sizeRecord[sizeRecord == 0] <- NA
  colorRecord[colorRecord == 0] <- NA         
  res <- list(trackRecord=trackRecord,A=A,label=label,
              sizeRecord=sizeRecord,colorRecord=colorRecord)
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
doTrack <- function(particles,L=50,sizeMeasure='n.cell',weight=weight) {

  n <- unique(particles$frame)
  
  G <- array(NA,dim=c(1000,1000,length(n)-1))
  links <- list()

  cat("\t Create track segments: ","0","%")
  
  for (i in 1:(length(n)-1)) {
	inc <- particles$frame == i
	inc2 <- particles$frame == (i + 1)
    coords1 <- matrix(c(particles[inc,]$x,
                      particles[inc,]$y),ncol=2,byrow=F)
    sizes1 <- particles[inc,]$n.cell
    coords2 <- matrix(c(particles[inc2,]$x,
                      particles[inc2,]$y),ncol=2,byrow=F)	
    sizes2 <- particles[inc2,]$n.cell
    
    if (i > 1) {
      coords0 <- matrix(c(particles[particles$frame == (i-1),]$x,
                        particles[particles$frame == (i-1),]$y),
                        ncol=2,byrow=F)
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
	              coords0=coords0)

    gstart <- gMat(Phi)
    
    ## optimize
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

  cat("\r \t Create track segments: ",round(i / (length(n)-1) * 100, 1),"%")

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
  colorRecord <- array(NA,dim=c(dim(allLinks)[1],dim(allLinks)[2],3))

  label <- matrix(NA,nrow=dim(allLinks)[1],ncol=dim(allLinks)[2])
  a <- tmp[duplicated(tmp)==F]

  for (i in 1:(length(a))) {
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
              colorRecord=colorRecord,label=label,G=G)
  return(res)
}


##' Track particles
##' 
##' \code{trackParticles} is a function reconstruct trajectories by linking particles.
##' @param particles Object of class 'particles',
##' obtained using \code{\link{identifyParticles}}.
##' @param L Numeric. Maximum cost for linking to particle to another particle. When the cost is larger, 
##' particle will be not be linked (resulting in the begin or end of a segment).
##'  Default set at \code{50}.
##' @param R Integer. Link to how many subsequent frames? Default set
##' at \code{2}.
##' @param weight Vector containing weights to calculate costs. First number 
##' gives the weight for differences in x and y coordinates; second number 
##' gives the weight for particle size differences; third number gives the 
##' difference bewteen the predicted location and the observed location. The latter 
##' is calculated using the location of the identified particle in the previous frame.
##' @author Marjolein Bruijning, Caspar A. Hallmann & Marco D. Visser
##' @examples
##' \dontrun{
##'    records <- trackParticles(particles)
##'    summary(records)
##'    plot(records,type='trajectories')
##'	}
##' @return A list of class 'TrDm' and 'records'. Use 'summary' and 'plot'.
##' @export
## 
trackParticles <- function (particles,L=50,R=2,
                            weight=c(1,1,1)) {
  records <- doTrack(particles=particles,L=L,weight=weight)
  cat("\n")
  rec <- linkTrajec (recordsObject=records,
                        particles=particles,
                        R=R,L=L,weight=weight)
  cat("\n")
  class(rec) <- c('TrDm','tracked')
  attr(rec,"background") <- attributes(particles)$background
  attr(rec,"originalImages") <- attributes(particles)$originalImages
  attr(rec,"subtractedImages") <- attributes(particles)$subtractedImages
  attr(rec,"images") <- attributes(particles)$images

  return(rec)
}



