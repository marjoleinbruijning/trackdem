##' Manual selection of true and false positives.
##'
##' \code{manuallySelect} is a function to to create training data that
##' by manually selecting false and true positives. The created training
##' data can be implemented in a neural net.
##' @param particleStats A list with particle statistics for each frame,
##' obtained by \code{\link{identifyParticles}}.
##' @param images An array with the original full color images, in order to plot
##' on the original images.
##' @param frame A number defining the frame that should be used. Default
##' is NULL; in that case the frame with the maximum number of identified particles is used.
##' @author Marjolein Bruijning & Marco D. Visser
##' @examples
##' \dontrun{
##' manuallySelect(particleStats=trackObject$particleStats,frame=1)
##'	}
##' @return List containing three elements: true positives, false positives,
##' and the evaluated frame.
##' @export

manuallySelect <- function (particleStats,images=allFullImages,
                            frame=NULL) {
  
  nImages <- 1:length(particleStats)
  
  if(is.null(frame)){
    n <- which(sapply(nImages,function(X) 
               length(particleStats[[X]]$patchID))==
               max(sapply(nImages,function(X) 
               length(particleStats[[X]]$patchID))))[1]
  } else{n <- frame}

  totx <- ncol(images)
  toty <- nrow(images)
  
  layout(matrix(c(1,2,2,2,2), 1, 5, byrow = TRUE))
  par(mar=c(0,0,0,0))
  plot(100,100,xlab='',ylab='',xaxt='n',yaxt='n',bty='n',xlim=c(0,1),
       ylim=c(0,1))
  polygon(x=c(0,1,1,0,0),y=c(0.7,0.7,0.8,0.8,0.7),col='green')
  text(x=0.5,y=0.75,label='Done',cex=4)
  text(0.5,0.83,label='Correctly identified',cex=1.5)
  polygon(x=c(0,1,1,0,0),y=c(0.5,0.5,0.6,0.6,0.5),col='red')
  text(x=0.5,y=0.55,label='Done',cex=4)
  text(0.5,0.63,label='Wrongly identified',cex=1.5)
  tmp1 <- par('usr') 

  plot(images,frame=n,bty='n')
  points(particleStats[[n]]$x/totx,
       1-particleStats[[n]]$y/toty,col='blue',cex=1.5)
  title("First click on all (or a subset of) correctly identified particles, and press the green button.\n
	Then click on all (or a subset of) wrongly identified particles. Finish by pressing the red button.", 
	line=-4.5,outer = TRUE, cex=1)
  tmp2 <- par('usr') 

  continueCorrect <- TRUE
  correct <- as.numeric()
  while (continueCorrect == TRUE) {
    pick<-locator(1)
    if (pick$x < 0 & pick$y > 0.9 & pick$y < 1.1) {continueCorrect <- FALSE}
    if (pick$x > 0) {
      tmp <- (1-particleStats[[n]]$y/toty - pick$y)^2 + 
             (particleStats[[n]]$x/totx - pick$x)^2
      patches <- particleStats[[n]]$patchID[which(tmp == min(tmp))]
      
      tmp <- particleStats[[n]][particleStats[[n]]$patchID %in% patches,]
      points(tmp$x/totx,1-tmp$y/toty,col='green',cex=2,pch=5)
      correct <- c(correct, tmp$patchID)
    }
  }
  par(mfg=c(1,1)) 
  par(usr=tmp1) 	
  polygon(x=c(-100,1,1,-1,-1),y=c(0.7,0.7,0.8,0.8,0.7),col='grey')
  text(x=0.5,y=0.75,label='Done',cex=4)

  par(mfg=c(1,2)) 
  par(usr=tmp2) 	
  continueWrong <- TRUE
  wrong <- as.numeric()
  while (continueWrong == TRUE) {
    pick<-locator(1)
    if (pick$x < 0 & pick$y > 0.5 & pick$y < 0.7) {continueWrong <- FALSE}
    if (pick$x > 0) {
      tmp <- (1-particleStats[[n]]$y/toty - pick$y)^2 + 
             (particleStats[[n]]$x/totx - pick$x)^2
      patches <- particleStats[[n]]$patchID[which(tmp == min(tmp))]
      
      tmp <- particleStats[[n]][particleStats[[n]]$patchID %in% patches,]
      points(tmp$x/totx,1-tmp$y/toty,col='red',cex=2,pch=4)
      wrong <- c(wrong, tmp$patchID)
    }
  }
  dev.off()
  return(list(wrong=wrong,correct=correct,frame=n))
}

##' Create trainingdata
##' \code{createTrainingData} creates a dataframe as preparation for 
##' applying a neural net (\code{\link{runNN}}). For all particles over all
##' frames, it collects information on color intensities and neighbor pixels.
##' @param particleStats A list with particle statistics for each frame,
##' as obtained by \code{\link{identifyParticles}}.
##' @param images An array with the original full color images, in order to plot
##' on the original images, obtained by \code{\link{loadImages}}.
##' @param msubs Images subtracted from background, as obtained by 
##' \code{\link{subtractBackground}}.
##' @param frame A number defining the frame that should be used. Default
##' is NULL, in that case all frames are used.
##' @param training Logical. Should identified false and true positives 
##' be combined to this dataframe? Default is FALSE.
##' @param tfp If training=TRUE, give a list with true and false positives, 
##' returned from \code{\link{manuallySelect}}, for each frame.
##' @author Marjolein Bruijning & Marco D. Visser
##' @export
createTrainingData <- function (particleStats=trackObject$particleStats,
    images=allFullImages,
	mSubs=allImages,frames=NULL,tfp=mId,training=FALSE) {
  
  if (is.null(frames)) frames <- 1:length(particleStats)
  temp <- particleStats[frames]
  print('Extract color and neighbor info')
  findMethod('as.array','package:raster')
  testI <- lapply(1:length(temp),function(X)
                  extractRGB(temp[[X]]$x,temp[[X]]$y,
                             im=mSubs[,,frames[X],]))
  sapply(1:length(testI),function(X) 
	            colnames(testI[[X]])<<-paste0("I",colnames(testI[[X]])))
  
  testNeighbor <- lapply(1:length(frames),function(X)
	extractNeighbors(temp[[X]]$x,temp[[X]]$y,images=images[,,,frames[X]],
	                #images=as.array(allFullImagesRGB[[frames[X]]]),
	                #images=aperm(array(values(allFullImagesRGB[[frames[X]]]),
	                #             dim=c(ncol(mSubs),nrow(mSubs),3)),
	                #       perm=c(2,1,3)),
	                frame=frames[X]))
	
  testNeighbor <- lapply(1:length(temp),function(X) 
	t(sapply(1:length(testNeighbor[[X]]),function(i) 
		as.vector(testNeighbor[[X]][[i]])))) 
  sapply(1:length(testNeighbor),function(X) 
	colnames(testNeighbor[[X]])<<-paste0('n',1:27))
  
  # Test data
  print('Create test data')
  testData <- lapply(1:length(temp),function(X)
                     cbind(temp[[X]],testI[[X]],testNeighbor[[X]])
                     )
  ## make training data based on test data and manually identified objects
  if (training == TRUE) {
    print('Create training data')
    for(i in 1:length(frames)){
	  testData[[i]]$D <- NA
	  testData[[i]]$D[testData[[i]]$patchID%in%tfp$correct] <- 1
	  testData[[i]]$D[testData[[i]]$patchID%in%tfp$wrong] <- 0
    }
    testData <- do.call(rbind,testData)
  }
  return(testData)
}

##' optThr
##'
##' @export
optThr <- function(trD=trainingData$D,nnP=plogis(n1com$net.result),
                   stat="accuracy"){

  ln <- function(param,D=trD,P=nnP){
    conf <- table(data.frame(D=as.logical(trD),P=nnP>param))
    
    if(length(attr(conf,"dimnames")$P)<2){
      num <- 1+as.logical(attr(conf,"dimnames")$P)
      temp <- matrix(0,ncol=2,nrow=2)
      temp[,num] <- as.numeric(conf)
      conf <- temp
    }
    
   confuStats(conf)[,stat]
  }

  flipper <- ifelse(stat=="FN",FALSE,TRUE)
  optimize(ln,c(0,1),maximum = flipper)
}

##' Create createNNData
##'
##' @export
confuStats <- function(confusion){
  accuracy <- sum(diag(confusion))/sum(confusion) # correct predictions
  recall <-  diag(confusion)[2]/sum(confusion[,2])# true positive rate
  TN <- diag(confusion)[1]/sum(confusion[,1]) # true negative 
  FN <- confusion[2,1]/sum(confusion[,1]) # false negative
  precision <- confusion[2,2]/sum(confusion[,2]) # prop positive that are correct
  F <-  2*((recall*precision)/(recall+precision))
  data.frame(accuracy,recall,TN,FN,precision,F)
}

##' Create neural net
##'
##' @export
runNN <- function(predictors,trainingData,hiddenLayers=3,reps=5) {
  
  n1 <- neuralnet(as.formula(paste("D ~ ", paste(predictors, collapse= "+"))),
                data=trainingData,hidden=hiddenLayers,rep=reps)
  n1com <- compute(n1,trainingData[,predictors])
  Thr <- optThr(stat="F",nnP=plogis(n1com$net.result))$maximum # find threshold
  res <- list(nn=n1,thr=Thr,predicted=n1com,trainingData=trainingData,
              hl=hiddenLayers,reps=reps,predictors=predictors)
  class(res) <- 'nnTrackdem'
  return(res)
}

##' Update particles
##'
##' @export
updateParticles <- function(object,testData) {
  newParticleStats <- idDaphnia(object$nn,testData,predictors=object$predictors)
  tmp <- lapply(newParticleStats,function(x) x > object$thr)
  prob <- lapply(1:length(newParticleStats),function(X) 
                 newParticleStats[[X]][tmp[[X]]])
  particleStats <- lapply(1:length(testData), function(X) 
                          cbind(data.frame(
                                    include=ifelse(tmp[[X]]==TRUE,1,0)), 
                                data.frame(prob=newParticleStats[[X]]),
                                    testData[[X]]))
  particleStats <- lapply(1:length(particleStats), function(X) 
                   particleStats[[X]][particleStats[[X]]$include == 1,])

   return(particleStats)
}

##' Create createNNData
##'
##' @export
idDaphnia <- function(nn,testData,predictors) {
  lapply(testData,function(X) plogis(compute(nn,X[,predictors])$net.result[,1]))
}

##' Create createNNData
##'
##' @export
resample <- function(x, ...) x[sample.int(length(x), ...)] 

##' Find original R, G, B values
##'
##' @export
extractRGB <- function(x,y,im){
  coords <- cbind(x,y)
  RGBmat <- t(apply(coords,1,function(X) im[round(X[2]),round(X[1]),]))
  colnames(RGBmat) <- c("R","G","B")
  return(RGBmat)
}

##' Get RGB values for neighbor pixels
##'
##' @export
extractNeighbors <- function(x,y,images,frame){
  x<-round(x)
  y<-round(y)
  Xmin<-x-1
  Xmax<-x+1
  Ymin<-y-1
  Ymax<-y+1
  Xmax[Xmax > dim(images)[2]] <- dim(images)[2]
  Ymax[Ymax > dim(images)[1]] <- dim(images)[1]
  Xmin[Xmin < 1] <- 1
  Ymin[Ymin < 1] <- 1
  return(lapply(1:length(x),function(i) 
         images[c(Ymin[i],y[i],Ymax[i]),c(Xmin[i],x[i],Xmax[i]),]))
}



