##' Manual selection of true and false positives.
##'
##' \code{manuallySelect} is a function to to create training data that
##' by manually selecting false and true positives. The created training
##' data can be implemented in a neural net.
##' @param particleStats A list with particle statistics for each frame,
##' obtained by \code{\link{identifyParticles}}.
##' @param colorimages An array with the original full color images, in order to plot
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

manuallySelect <- function (particleStatObject,colorimages,
                            frame=NULL) {
  
  if(is.null(frame)) {
    n <- 1:length(particleStatObject)
    n <- which(sapply(n,function(X) 
               length(particleStatObject[[X]]$patchID))==
               max(sapply(n,function(X) 
               length(particleStatObject[[X]]$patchID))))[1]
  } else { n <- frame }

  totx <- ncol(colorimages)
  toty <- nrow(colorimages)
  
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

  plot(colorimages,frame=n,bty='n')
  points(particleStatObject[[n]]$x/totx,
       1-particleStatObject[[n]]$y/toty,col='blue',cex=1.5)
  title("First click on all (or a subset of) correctly identified particles,and press the green button.\n
	Then click on all (or a subset of) wrongly identified particles. Finish by pressing the red button.", 
	line=-4.5,outer = TRUE, cex=1)
  tmp2 <- par('usr') 

  continueCorrect <- TRUE
  correct <- as.numeric()
  while (continueCorrect == TRUE) {
    pick <- locator(1)
    if (pick$x < 0 & pick$y > 0.9 & pick$y < 1.1) {continueCorrect <- FALSE}
    if (pick$x > 0) {
      tmp <- (1-particleStatObject[[n]]$y/toty - pick$y)^2 + 
             (particleStatObject[[n]]$x/totx - pick$x)^2
      patches <- particleStatObject[[n]]$patchID[which(tmp == min(tmp))]
      tmp <- particleStatObject[[n]][particleStatObject[[n]]$patchID %in% patches,]
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
    pick <- locator(1)
    if (pick$x < 0 & pick$y > 0.5 & pick$y < 0.7) {continueWrong <- FALSE}
    if (pick$x > 0) {
      tmp <- (1-particleStatObject[[n]]$y/toty - pick$y)^2 + 
             (particleStatObject[[n]]$x/totx - pick$x)^2
      patches <- particleStatObject[[n]]$patchID[which(tmp == min(tmp))]
      
      tmp <- particleStatObject[[n]][particleStatObject[[n]]$patchID %in% patches,]
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
##' @param particleStatObject A list with particle statistics for each frame,
##' as obtained by \code{\link{identifyParticles}}.
##' @param images An array with the original full color images, in order to plot
##' on the original images, obtained by \code{\link{loadImages}}.
##' @param mSub Images subtracted from background, as obtained by 
##' \code{\link{subtractBackground}}.
##' @param frame A number defining the frame that should be used. Default
##' is NULL, in that case all frames are used.
##' @param training Logical. Should identified false and true positives 
##' be combined to this dataframe? Default is FALSE.
##' @param mIdObject If training=TRUE, provide a list with true and false positives, 
##' returned from \code{\link{manuallySelect}}, for each frame.
##' @author Marjolein Bruijning & Marco D. Visser
##' @export
createTrainingData <- function (particleStatObject,colorimages,mSub,
                                frames=NULL,mIdObject=NULL,training=FALSE) {

  if (is.null(frames)) frames <- 1:length(particleStatObject)
  stat <- particleStatObject[frames] # Subset
  print('Extract intensity info')
  getI <- lapply(1:length(stat),function(X)
                  extractRGB(stat[[X]]$x,stat[[X]]$y,
                             images=mSub[,,frames[X],]))
  sapply(1:length(getI),function(X) 
	            colnames(getI[[X]]) <<- paste0("I",colnames(getI[[X]])))
  
  print('Extract neighbor info')
  getNeighbor <- lapply(1:length(frames),function(X)
	                       extractNeighbors(stat[[X]]$x,stat[[X]]$y,
	                                        images=colorimages[,,,frames[X]]))
  getNeighbor <- lapply(1:length(stat),function(X) 
                      t(sapply(1:length(getNeighbor[[X]]),function(i) 
		                as.vector(getNeighbor[[X]][[i]])))) 
  sapply(1:length(getNeighbor),function(X) 
	colnames(getNeighbor[[X]]) <<- paste0('n',1:27))
  
  # Test data
  print('Create data')
  dat <- lapply(1:length(stat),function(X)
                     cbind(stat[[X]],getI[[X]],getNeighbor[[X]])
                )
  ## Make training data based on test data and manually identified objects
  if (training == TRUE) {
    print('Create training data')
    for(i in 1:length(frames)){
	  dat[[i]]$trY <- NA
	  dat[[i]]$trY[dat[[i]]$patchID %in% mIdObject$correct] <- 1
	  dat[[i]]$trY[dat[[i]]$patchID %in% mIdObject$wrong] <- 0
    }
    dat <- do.call(rbind,dat)
  }
  return(dat)
}

##' Create neural net
##' \code{runNN} traines an artificial neural network.
##' @param predictors Vector containing predictors of interest, present 
##' as column names in TrainingData.
##' @param trainingData Dataframe containing the variables of the neural
##' network.
##' @param hidden A vector of integers specifying the number of hidden neurons
##' in each layer (see also \code{\link{neuralnet}}). Default is 3.
##' @param reps The number of repetitions. Default is 5.
##' @param stat The statistic to be optimized to calculate the threshold.
##' Either 'accuray', 'recall', or 'F' (F-measure). Default is 'F'.
##' @return An object of class 'nnTrackdem', containing the trained neural
##' net. 'Summary' can be used to obtain a summary of the results. 'Plot' 
##' can be used to plot the results.
##' @seealso \code{\link{neuralnet}}, \code{\link{compute}}
##' @author Marjolein Bruijning & Marco D. Visser
##' @export
runNN <- function(predictors,trainingData,hidden=3,reps=5,stat='F',...) {
  n <- neuralnet(as.formula(paste("trY ~ ", paste(predictors, collapse= "+"))),
                 data=trainingData,hidden=hidden,rep=reps,...)
  nCom <- compute(n,trainingData[,predictors])
  thr <- optThr(stat=stat,nnP=plogis(nCom$net.result))$maximum # find threshold
  res <- list(nn=n,thr=thr,predicted=nCom,trainingData=trainingData,
              hidden=hidden,reps=reps,predictors=predictors,stat=stat)
  class(res) <- 'nnTrackdem'
  return(res)
}

##' Optimize threshold based on precision, recall or F-measure.
##' @param stat The statistic to be optimized to calculate the threshold.
##' Either 'accuray', 'recall', or 'F' (F-measure).
##' nnP Vector containing probabilities.
##' trY Vector containing trained response values (either 0 or 1).
##' @return Returns a threshold probability.
##' @seealso \code{\link{neuralnet}}, \code{\link{compute}}
##' @export
optThr <- function(trY=trainingData$trY,nnP=plogis(nCom$net.result),
                   stat="accuracy"){

  ln <- function(param,D=trY,P=nnP){
    conf <- table(data.frame(Y=as.logical(D),P=P>param))
    
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

##' Calculate different statistics for trained neural network.
##' @param confusion Confusion matrix
##' @export
confuStats <- function(confusion){
  accuracy <- sum(diag(confusion))/sum(confusion) # correct predictions
  recall <-  diag(confusion)[2]/sum(confusion[,2])# true positive rate
  TN <- diag(confusion)[1]/sum(confusion[,1]) # true negative 
  FN <- confusion[2,1]/sum(confusion[,1]) # false negative
  precision <- confusion[2,2]/sum(confusion[,2]) # prop positive that are correct
  F <-  2*((recall*precision)/(recall+precision)) # F-measure
  data.frame(accuracy,recall,TN,FN,precision,F)
}

##' Apply trained artificial neural network to particleStat object.
##' @param nnTrackdemObject Object of class nnTrackdemObject.
##' @param particleStatObject Object of class particleStatObject.
##' @return A list of class particleStatObject.
##' @export
updateParticles <- function(nnTrackdemObject,particleStatObject) {
  newParticleStats <- lapply(particleStatObject,function(X) 
                                      plogis(compute(nnTrackdemObject$nn,
                                             X[,predictors])$net.result[,1]))

  tmp <- lapply(newParticleStats,function(X) X > nnTrackdemObject$thr)
  prob <- lapply(1:length(newParticleStats),function(X) 
                                        newParticleStats[[X]][tmp[[X]]])
  dat <- lapply(1:length(particleStatObject), function(X) 
                          cbind(data.frame(
                                    include=ifelse(tmp[[X]]==TRUE,1,0)), 
                                data.frame(prob=newParticleStats[[X]]),
                                    particleStatObject[[X]]))
  dat <- lapply(1:length(dat), function(X) dat[[X]][dat[[X]]$include == 1,])
  class(dat) <- 'particleStatObject'
  return(dat)
}

##' Find values for R, G and B layer for specified coordinates.
##' @param x Vector containing x coordinates.
##' @param y Vector containing y coordinates.
##' @param images Three dimensional array.
##' @export
extractRGB <- function(x,y,images){
  coords <- cbind(x,y)
  RGBmat <- t(apply(coords,1,function(X) images[round(X[2]),round(X[1]),]))
  colnames(RGBmat) <- c("R","G","B")
  return(RGBmat)
}

##' Get R, G and B values for specified coordinates, and its eight neighbor 
##' pixels.
##' @param x Vector containing x coordinates.
##' @param y Vector containing y coordinates.
##' @param Three dimensional array.
##' @export
extractNeighbors <- function(x,y,images){
  x <- round(x)
  y <- round(y)
  Xmin <- x-1
  Xmax <- x+1
  Ymin <- y-1
  Ymax <- y+1
  Xmax[Xmax > dim(images)[2]] <- dim(images)[2]
  Ymax[Ymax > dim(images)[1]] <- dim(images)[1]
  Xmin[Xmin < 1] <- 1
  Ymin[Ymin < 1] <- 1
  return(lapply(1:length(x),function(i) 
         images[c(Ymin[i],y[i],Ymax[i]),c(Xmin[i],x[i],Xmax[i]),]))
}



