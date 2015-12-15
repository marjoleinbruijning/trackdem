##' Manually selection
##'
##' \code{manuallySelect} is a function to to create training data that
##' by manually selecting false and true positives. The created training
##' data can be implemented in a neural net.
##' @param particleStats A list with particle statistics for each frame.
##' @param colorimages A list with the original full color images, in order to plot
##' on the original images.
##' @param frame A number defining the frame that should be used. Default
##' is NULL; the frame with the maximum number of identified particles is used.
##' @author Marjolein Bruijning & Marco D. Visser
##' @examples
##' \dontrun{
##'
##' manuallySelect(particleStats=trackObject$particleStats,frame=1)
##'
##'	}
##' @seealso \code{\link{identifyParticles}},
##' @return List containing three elements: true positives, false positives,
##' and the evaluated frame.
##' @concept What is the broad searchable concept?
##' @export

manuallySelect <- function (particleStats,colorimages=allFullImagesRGB,
                            frame=NULL) {
  
  nImages <- 1:length(particleStats)
  totx <- nrow(colorimages[[n]])
  toty <- ncol(colorimages[[n]])
  
  if(is.null(frame)){
    n <- which(sapply(nImages,function(X) 
               length(particleStats[[X]]$patchID))==
               max(sapply(nImages,function(X) 
               length(particleStats[[X]]$patchID))))[1]
  } else{n <- frame}
  
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

  plotRGB(colorimages[[n]],scale=1,bty='n',
	asp=toty/totx)
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

getCoords <- function (mat) {
  ID <- unique(as.vector(mat))[-1]
  x <- y <- rep(NA,length(ID))

  for (i in 1:length(x)) {
    tmp <- mat == ID[i]
    coords <- colMeans(t(t(which(tmp,TRUE))))
    y[i] <- coords[1]
    x[i] <- coords[2]
  }
  return(data.frame(x=x,y=y,ID=ID))
}

##' Create trainingdata
##'
##' \code{createTrainingData} is a function to to create training data that
##' by manually selecting false and true positives. The created training
##' data can be implemented in a neural net.
##' @param particleStats A list with particle statistics for each frame.
##' @param colorimages A list with the original full color images, in order to plot
##' on the original images.
##' @param frame A number defining the frame that should be used. Default
##' is NULL; the frame with the maximum number of identified particles is used.
##' @author Marjolein Bruijning & Marco D. Visser
##' @examples
##' \dontrun{
##' manuallySelect(particleStats=trackObject$particleStats,frame=1)
##'	}
##' @seealso \code{\link{manuallySelect}},
##' @return List containing three elements: true positives, false positives,
##' and the evaluated frame.
##' @concept What is the broad searchable concept?
##' @export
createTrainingData <- function (particleStats,
    allFullImagesRGB=allFullImagesRGB,
	allImages=allImages,frames=frames,mId=mId) {

 temp <- particleStats[frames]

 print('Extract color and neighbor info')

  testI <- lapply(1:length(temp),function(X)
                  extractRGB(temp[[X]]$x,temp[[X]]$y,images=allImages,
                             frame=X))
  sapply(1:length(testI),function(X) 
	colnames(testI[[X]])<<-paste0("I",colnames(testI[[X]])))
  
  testNeighbor <- lapply(1:length(temp),function(X)
	extractNeighbors(temp[[X]]$x,temp[[X]]$y,image=allFullImagesRGB,frame=X))
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
  print('Create training data')
  trainingData <- testData
  for(i in 1:length(frames)){
	trainingData[[i]]$D <- NA
	trainingData[[i]]$D[trainingData[[i]]$patchID%in%mId$correct] <- 1
	trainingData[[i]]$D[trainingData[[i]]$patchID%in%mId$wrong] <- 0
  }
  trainingData <- do.call(rbind,trainingData)
  return(trainingData)
}

##' Create trainingdata
##'
##' \code{createNNdata} is a function to to create training data that
##' by manually selecting false and true positives. The created training
##' data can be implemented in a neural net.
##' @param particleStats A list with particle statistics for each frame.
##' @param colorimages A list with the original full color images, in order to plot
##' on the original images.
##' @param frame A number defining the frame that should be used. Default
##' is NULL; the frame with the maximum number of identified particles is used.
##' @author Marjolein Bruijning & Marco D. Visser
##' @examples
##' \dontrun{
##' manuallySelect(particleStats=trackObject$particleStats,frame=1)
##'	}
##' @seealso \code{\link{manuallySelect}},
##' @return List containing three elements: true positives, false positives,
##' and the evaluated frame.
##' @concept What is the broad searchable concept?
##' @export
createNNdata <- function(particleStats,allFullImagesRGB=allFullImagesRGB,
	allImages=allImages,frames=frames,mId=mId,training=TRUE) {
  
  temp <- particleStats
  # Extract RGB values and intensities (from substracted background) for different colors.
  print('Extract color and neighbor info')
  #testRGB <- lapply(1:length(temp),function(X)
  #	   extractRGB(temp[[X]]$x,temp[[X]]$y,images=allFullImagesRGB,
  #				  frame=X))
  testI <- lapply(1:length(temp),function(X)
                  extractRGB(temp[[X]]$x,temp[[X]]$y,images=allImages,
                             frame=X))
  sapply(1:length(testI),function(X) 
	colnames(testI[[X]])<<-paste0("I",colnames(testI[[X]])))
 
  # Test data
  print('Create test data')
  testData <- lapply(1:length(temp),function(X)
                     cbind(temp[[X]],testI[[X]])
                     )
  ## make training data based on test data and manually identified objects
  if (training == TRUE) {
	  print('Create training data')
	  trainingData<-testData[frames]

	  for(i in 1:length(frames)){
		trainingData[[i]]$D <- NA
		trainingData[[i]]$D[trainingData[[i]]$patchID%in%mId[[i]]$correct] <- 1
		trainingData[[i]]$D[trainingData[[i]]$patchID%in%mId[[i]]$wrong] <- 0
	  }
	  trainingData <- do.call(rbind,trainingData)
    return(list(trainingData=trainingData,testData=testData))
  } else {
  return(list(testData=testData))
  }
}

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

confuStats <- function(confusion){
  accuracy <- sum(diag(confusion))/sum(confusion) ## correct predictions
  recall <-  diag(confusion)[2]/sum(confusion[,2])# true positive rate
  TN <- diag(confusion)[1]/sum(confusion[,1]) #Treu negative 
  FN <- confusion[2,1]/sum(confusion[,1]) #FALSE negative
  precision <- confusion[2,2]/sum(confusion[,2]) #prop positive that are correct
  F <-  2*((recall*precision)/(recall+precision))
  data.frame(accuracy,recall,TN,FN,precision,F)
}

runNN <- function(predictors,trainingData,hiddenLayers=3,Thr=0.6) {
  
  n1<-neuralnet(as.formula(paste("D ~ ", paste(predictors, collapse= "+"))),
                data=trainingData,hidden=hiddenLayers)
  n1com<-compute(n1,trainingData[,predictors])
  
  plot(cbind(trainingData$D,plogis(n1com$net.result)),
       xlab='Identified',ylab='Probability based on neural network')
  abline(h=Thr,lty=2,lwd=2)

  confusion <- table(data.frame(D=trainingData$D,P=plogis(n1com$net.result)>Thr))
  return(list(confusion=confusion,n1=n1))
}

updateParticles <- function(nn,testData,predictors,Thr) {
  newParticleStats <- idDaphnia(nn,testData,predictors=predictors,FnTr=Thr)
  tmp <- lapply(newParticleStats,function(x) x > Thr)
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

idDaphnia <- function(nn,testData,FnTr=0.6,predictors) {
  lapply(testData,function(X) plogis(compute(nn,X[,predictors])$net.result[,1]))
}

resample <- function(x, ...) x[sample.int(length(x), ...)] 

## Find original R, G, B values
extractRGB <- function(x,y,images,frame){
  if(class(images)=="list"){
    IM <- as.array(images[[frame]])
  } else {
    IM <- images[,,frame,]
  }
  coords <- cbind(x,y)
  RGBmat <- t(apply(coords,1,function(X) IM[round(X[2]),round(X[1]),]))
  colnames(RGBmat) <- c("R","G","B")
  return(RGBmat)
}

# Get RGB values for neighbor pixels
extractNeighbors <- function(x,y,images,frame){
  IM <- as.array(images[[frame]])
  x<-round(x)
  y<-round(y)
  Xmin<-x-1
  Xmax<-x+1
  Ymin<-y-1
  Ymax<-y+1
  Xmax[Xmax > dim(IM)[2]] <- dim(IM)[2]
  Ymax[Ymax > dim(IM)[1]] <- dim(IM)[1]
  Xmin[Xmin < 1] <- 1
  Ymin[Ymin < 1] <- 1
  return(lapply(1:length(x),function(i) IM[c(Ymin[i],y[i],Ymax[i]),c(Xmin[i],x[i],Xmax[i]),]))
}

jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "red", "#7F0000")) 




