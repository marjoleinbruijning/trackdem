##' Track particles
##'
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
			if (sum(!is.na(reducedCost))>0) {
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
		#print(totCost)
	}
	return(g)
}
# Cost function
calcCost <- function(x1,x2,y1,y2,s1,s2) {
	sqrt((x1-x2)^2 + (y1-y2)^2 + (s1-s2)^2)
}

## Create cost matrix
phiMat <- function (coords1,coords2,sizes1,sizes2,r,L=50) {
	Phi <- sapply(1:nrow(coords1),function(x)
		calcCost(coords1[x,1],coords2[,1],coords1[x,2],coords2[,2],
		         sizes1[x],sizes2)
	)
	Phi <- matrix(Phi,ncol=dim(coords1)[1],nrow=dim(coords2)[1])
	Phi <- cbind(rep(L*r,nrow(Phi)),Phi)
	Phi <- rbind(rep(L*r,ncol(Phi)),Phi)
	return(Phi)
}

# Create random start g matrix
gMat <- function (Phi) {
	g <- matrix(0,nrow=nrow(Phi),ncol=ncol(Phi))
	sequence <- sample(2:nrow(Phi),ncol(Phi)-1,replace=T)
	sapply(2:ncol(g),function(x) g[unique(sequence)[x-1],x] <<- 1)
	g[1,colSums(g) == 0] <- 1
	g[rowSums(g) == 0,1] <- 1
	return(g)
}

linkTrajec <- function (G,trackRecord=records$trackRecord,
                        label=records$label,sizeRecord=records$sizeRecord,
                        L=50,plot=FALSE,R=1,images=allFullImagesRGB[[1]]) {
                        
 trackRecord[is.na(trackRecord)] <- 0                             
 label[is.na(label)] <- 0   
 sizeRecord[is.na(sizeRecord)] <- 0     
 
 for (r in 1:R) {
  A <- array(NA,dim=c(500,500,length(particleStats)-r-1))
  links <- list()
      
  for (i in 1:(dim(G)[3]-r-1)) {
    #endTrajec <- which(G[1,,i]!=0)[-1] -1
    endTrajec <- as.vector(na.omit(label[apply(
                           trackRecord[,i:(i+1),1],1,function(x) 
                                x[1] != 0 & x[2] == 0),i]))
    beginTrajec <- as.vector(na.omit(label[apply(
                           trackRecord[,(i+r):(i+r+1),1],1,function(x) 
                                     x[1]==0 & x[2]>0),i+r+1]))
    beginTrajec <- beginTrajec[beginTrajec != 0]
    #beginTrajec <- which(G[,1,i+1,1]!=0)[-1] -1
    
    if (length(endTrajec)>0 & length(beginTrajec)>0) {    
      coords1 <- matrix(c(particleStats[[i]]$x[endTrajec],
                 particleStats[[i]]$y[endTrajec]),ncol=2,byrow=F)
      sizes1 <- particleStats[[i]]$n.cell[endTrajec]           
    
      coords2 <- matrix(c(particleStats[[i+r+1]]$x[beginTrajec],
                 particleStats[[i+r+1]]$y[beginTrajec]),ncol=2,byrow=F)
      sizes2 <- particleStats[[i+r+1]]$n.cell[beginTrajec]
    
      Phi <- phiMat(coords1,coords2,
	                sizes1=sizes1,
	                sizes2=sizes2,
	                L=L,r=1)   
      gstart <- gMat(Phi)
      A[1:nrow(Phi),1:ncol(Phi),i] <- track(Phi=Phi, g=gstart, L=L) * Phi
      
      tmp <- data.frame(which(A[,,i] > 0,TRUE))
      tmp <- tmp[tmp[,1] != 1,]
      tmp <- tmp[tmp[,2] != 1,]
     
      if (dim(tmp)[1] > 0) {
        for (k in 1:(dim(tmp)[1])) {
        ind1 <- which(label[,i] ==  endTrajec[tmp[k,2]-1])
        ind2 <- which(label[,i+r+1] ==  beginTrajec[tmp[k,1]-1])
     
        trackRecord[ind1,,1] <- trackRecord[ind1,,1] + trackRecord[ind2,,1]
        trackRecord[ind1,,2] <- trackRecord[ind1,,2] + trackRecord[ind2,,2]
        label[ind1,] <- label[ind1,] + label[ind2,]
        sizeRecord[ind1,] <- sizeRecord[ind1,] + sizeRecord[ind2,]
        
        # Take mean values for coordinates in between
        trackRecord[ind1,(i+1):(i+r),1] <- (trackRecord[ind1,i,1] +
                                 trackRecord[ind1,i+r+1,1]) / 2
        trackRecord[ind1,(i+1):(i+r),2] <- (trackRecord[ind1,i,2] +
                                 trackRecord[ind1,i+r+1,2]) / 2  
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
                      
  if (plot == TRUE) {
    incLabels <- apply(trackRecord[,,1],1,function(x) sum(!is.na(x))) > 12
    opar <- par()
    par(opar)
    perID <- apply(sizeRecord,1,mean,na.rm=T)[incLabels]
    sdperID <- apply(sizeRecord,1,sd,na.rm=T)[incLabels]	
    plot(1:length(perID),perID[order(perID)],pch=16,xlab='Labeled particle',
      ylab='Size')
    segments(x0=1:length(perID),y0=perID[order(perID)]-sdperID[order(perID)],
     y1=perID[order(perID)]+sdperID[order(perID)])
     
    plotRGB(images,scale=1,
      asp=nrow(images)/ncol(images))
    for (i in 1:nrow(trackRecord[incLabels,,])) {
      lines(trackRecord[incLabels,,][i,,1]/ncol(images),
            1-trackRecord[incLabels,,][i,,2]/nrow(images),
      col=paste0(jet.colors(nrow(trackRecord[incLabels,,]))[i],'40'),lwd=1.5)
     # text(trackRecord[incLabels,,][i,,1]/ncol(images),1-trackRecord[incLabels,,][i,,2]/nrow(images),
      #     labels=i,cex=0.4)
    }
  }
  return(list(trackRecord=trackRecord,A=A,label=label,sizeRecord=sizeRecord))
}
 
##' Track particles
##' \code{doTrack} is a function to track particles over subsequent frames.
##' Based on tracking algorithm by Sbalzarini and Koumoutsakos (2005).
##' @param particleStats blabla
##' @param L Cost to link to dummy particle.
##' @param plot Default is FALSE. Plot results?
##' @param images Original color image (first frame).
##' @param backward Default is FALSE. Perform tracking backward?
##' @author Marjolein Bruijning & Marco D. Visser
##' @seealso \code{\link{doTrack}}, \code{\link{linkTrajec}},
##' @return Bla
##' @concept What is the broad searchable concept?
##' @export
##   
doTrack <- function(particleStats=particleStats,L=50,plot=FALSE,
                    images=allFullImagesRGB[[1]],backward=FALSE) {

G <- array(NA,dim=c(500,500,length(particleStats)-1))
links <- list()
if (backward == TRUE) {particleStats <- particleStats[30:1]}

for (i in 1:(length(particleStats)-1)) {
  coords1 <- matrix(c(particleStats[[i]]$x,
      particleStats[[i]]$y),ncol=2,byrow=F)
  sizes1 <- particleStats[[i]]$n.cell
  coords2 <- matrix(c(particleStats[[i+1]]$x,
          particleStats[[i+1]]$y),ncol=2,byrow=F)	
  sizes2 <- particleStats[[i+1]]$n.cell
  Phi <- phiMat(coords1,coords2,
	              sizes1=sizes1,
	              sizes2=sizes2,
	              L=L,r=1)
  gstart <- gMat(Phi)
  G[1:nrow(Phi),1:ncol(Phi),i] <- track(Phi=Phi, g=gstart, L=L) * Phi	
  
  
  links[[i]] <- data.frame(which(G[,,i] > 0,TRUE))
  if (i > 1) {
    links[[i]] <- links[[i]][links[[i]][,2] != 1,]
    #links[[i]][[r]] <- links[[i]][[r]][links[[i]][[r]][,1] != 1,]
  }
  names(links[[i]]) <- c(paste('frame',i+1),paste('frame',i))
  if (i == 1) {allLinks <- links[[1]]
  } else {
      allLinks <- 
           merge(links[[i]],allLinks,by=paste('frame',i),
           all.x=TRUE,all.y=TRUE,suffixes=c('',paste0('.',i)))
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

#allLinks[allLinks == 1] <- NA
trackRecord <- array(NA,dim=c(dim(allLinks)[1],dim(allLinks)[2],2))
sizeRecord <- matrix(NA,nrow=dim(allLinks)[1],ncol=dim(allLinks)[2])
label <- matrix(NA,nrow=dim(allLinks)[1],ncol=dim(allLinks)[2])
a <- tmp[duplicated(tmp)==F]

for (i in 1:(length(a))) {
    label[,i] <- allLinks[,order(a)[i]]
	trackRecord [,i,1] <- particleStats[[i]][allLinks[,order(a)[i]],'x']
	trackRecord [,i,2] <- particleStats[[i]][allLinks[,order(a)[i]],'y']
	sizeRecord[,i] <- particleStats[[i]][allLinks[,order(a)[i]],'n.cell']
}

if (plot == TRUE) {
  incLabels <- apply(trackRecord[,,1],1,function(x) sum(!is.na(x))) > 10
  opar <- par()
  #incLabels <- rep(TRUE,nrow(trackRecord))
  plotRGB(images,scale=1,
    asp=nrow(images)/ncol(images))
  for (i in 1:nrow(trackRecord[incLabels,,])) {
    lines(trackRecord[incLabels,,][i,,1]/ncol(images),1-trackRecord[incLabels,,][i,,2]/nrow(images),
    col=paste0(jet.colors(nrow(trackRecord[incLabels,,]))[i],'40'),lwd=1.5)
  }
  par(opar)
  perID <- apply(sizeRecord,1,mean,na.rm=T)[incLabels]
  sdperID <- apply(sizeRecord,1,sd,na.rm=T)[incLabels]	
  plot(1:length(perID),perID[order(perID)],pch=16,xlab='Labeled particle',
    ylab='Size')
  segments(x0=1:length(perID),y0=perID[order(perID)]-sdperID[order(perID)],
   y1=perID[order(perID)]+sdperID[order(perID)])
	
}
return(list(trackRecord=trackRecord,sizeRecord=sizeRecord,label=label,G=G))
}
