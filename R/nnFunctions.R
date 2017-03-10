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

manuallySelect <- function (particles,colorimages=NULL,
                            frame=NULL) {
                            
  if(!is.TrDm(particles)){
      stop("Input does not appear to be of the class \"TrDm\"")
  }
  if(is.null(colorimages)) { 
    colorimages <- get(attributes(particles)$originalImages,
                       envir=.GlobalEnv)
  }
  if(is.null(frame)) {
    n <- order(tapply(partIden$patchID,partIden$frame,length),
               decreasing=TRUE)[1]
    
  } else { n <- frame }

  totx <- ncol(colorimages)
  toty <- nrow(colorimages)
 
  dev.new(width=10, height=7)
  layout(matrix(c(1:4,rep(5,20)), 6, 4, byrow = TRUE))
  par(mar=c(0,0,0,0))
  buttons <- makeButtons()
  
  plot(colorimages,frame=n,bty='n')
  inc <- particles$frame == n
  points(particles[inc,]$x/totx,
       1-particles[inc,]$y/toty,col='blue',cex=1.5)
  im <- par('usr','fig','plt','mfg') 
  
  options(locatorBell = FALSE)
  
  continue <- TRUE
  id <- st <- as.numeric()
  while (continue == TRUE) {
    par(mfg=c(2,1),usr=im$usr)
    pick <- locator(1)
  
    # calculate devicee coordinates
    b0 <- im$usr[3]
    b1 <- (im$usr[4] - im$usr[3]) / im$fig[4]
    y <- (pick$y-b0) / b1
    
    cols <- c('green','red','black')
    names(cols) <- c('g','f','rm')
    ## true positives  
    if (pick$x > buttons$g$fig[1] & pick$x < buttons$g$fig[2] & 
        y > buttons$g$fig[3] & y < buttons$g$fig[4]) {
      makeButtons()
      par(mfg=c(1,2),usr=buttons$g$usr)
      polygon(x=c(0,1,1,0,0),y=c(0,0,1,1,0),col='grey')
      text(x=0.5,y=0.5,label='True positives',cex=2)
      status <- 'g'
      
    } else if (pick$x > buttons$f$fig[1] & pick$x < buttons$f$fig[2] & 
               y > buttons$f$fig[3] & y < buttons$f$fig[4]) {
      makeButtons()
      par(mfg=c(1,3),usr=buttons$f$usr)
      polygon(x=c(0,1,1,0,0),y=c(0,0,1,1,0),col='grey')
      text(x=0.5,y=0.5,label='False positives',cex=2)
      status <- 'f'
      
    } else if (pick$x > buttons$C$fig[1] & pick$x < buttons$C$fig[2] & 
               y > buttons$C$fig[3] & y < buttons$C$fig[4]) {
      makeButtons()
      par(mfg=c(1,4),usr=buttons$C$usr)
      polygon(x=c(0,1,1,0,0),y=c(0,0,1,1,0),col='grey')
      text(x=0.5,y=0.5,label='Delete',cex=2)
      status <- 'rm'
      
    } else if (pick$x > im$fig[1] & pick$x < im$fig[2] & 
               y > im$fig[3] & y < im$fig[4]) {
      par(mfg=c(2,1),usr=im$usr)
      tmp <- (1-particles[inc,]$y/toty - pick$y)^2 + 
              (particles[inc,]$x/totx - pick$x)^2
      patches <- particles[inc,]$patchID[which(tmp == min(tmp))]
      tmp <- particles[inc,][particles[inc,]$patchID %in% patches,]
      points(tmp$x/totx,1-tmp$y/toty,col=cols[status],
             cex=ifelse(status == 'rm',2.2,2),pch=5)
      if (status == 'g' | status == 'f') {
        id <- c(id, tmp$patchID)
        st <- c(st,status)
      } else if (status == 'rm') {
        st <- st[id != tmp$patchID]
        id <- id[id != tmp$patchID]
      }
      
    } else if (pick$x > buttons$s$fig[1] & pick$x < buttons$s$fig[2] & 
               y > buttons$s$fig[3] & y < buttons$s$fig[4]) {
      continue <- FALSE
    } else {}
  }
  dev.off()

  res <- list(wrong=id[st=='f'],correct=id[st=='g'],frame=n)
  attr(res,"background") <- attributes(particles)$background
  attr(res,"originalImages") <- attributes(particles)$originalImages
  attr(res,"originalDirec") <- attributes(particles)$originalDirec
  attr(res,"subtractedImages") <- attributes(particles)$subtractedImages
  
  class(res) <- c("TrDm","tfp","list")
  return(res)
}

##' Make buttons
makeButtons <- function () {
  par(mfg=c(1,1))
  plot(100,100,xlab='',ylab='',xaxt='n',yaxt='n',bty='n',xlim=c(0,1),
       ylim=c(0,1))
  polygon(x=c(0,1,1,0,0),y=c(0,0,1,1,0),col='black',border=NA)
  text(x=0.5,y=0.5,label='Stop',cex=2,col='white')
  s <- par('usr','fig','plt','mfg') 

  par(mfg=c(1,2))
  plot(100,100,xlab='',ylab='',xaxt='n',yaxt='n',bty='n',xlim=c(0,1),
       ylim=c(0,1))
  polygon(x=c(0,1,1,0,0),y=c(0,0,1,1,0),col='#00FF0050',border=NA)
  text(x=0.5,y=0.5,label='True positives',cex=2)
  g <- par('usr','fig','plt','mfg') 
  
  par(mfg=c(1,3))
  plot(100,100,xlab='',ylab='',xaxt='n',yaxt='n',bty='n',xlim=c(0,1),
       ylim=c(0,1))
  polygon(x=c(0,1,1,0,0),y=c(0,0,1,1,0),col='#FF000050',border=NA)
  text(x=0.5,y=0.5,label='False positives',cex=2)
  f <- par('usr','fig','plt','mfg') 

  par(mfg=c(1,4))
  plot(100,100,xlab='',ylab='',xaxt='n',yaxt='n',bty='n',xlim=c(0,1),
       ylim=c(0,1))
  polygon(x=c(0,1,1,0,0),y=c(0,0,1,1,0),col='#EFFF0050',border=NA)
  text(x=0.5,y=0.5,label='Delete',cex=2)
  C <- par('usr','fig','plt','mfg')
  return(list(f=f,C=C,g=g,s=s))
}


##' Extract info
##' \code{extractInfo} creates a dataframe as preparation for 
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
extractInfo <- function (particles,info=c('intensity','neighbors','sd'),
                         colorimages=NULL,sbg=NULL,
                         frames=NULL,mIdObject=NULL,training=FALSE) {

    if(is.null(colorimages)) { colorimages <- get(attributes(particles)$originalImages,
                                                  envir=.GlobalEnv) }
    if(is.null(sbg)) { sbg <- get(attributes(particles)$subtractedImages,
                                                  envir=.GlobalEnv) }
    if (is.null(frames)) frames <- 1:length(particles)
    
    stat <- particles[particles$frame %in% frames,] # Subset
    
    
    cat("\n")
  if ('intensity' %in% info) {
    cat('\t Extract intensity info')
    getI <- lapply(1:length(frames),function(X) {
                    inc <- stat$frame == frames[X]
                    extractRGB(stat[inc,]$x,stat[inc,]$y,
                             images=sbg[,,frames[X],])})
    sapply(1:length(getI),function(X) 
	              colnames(getI[[X]]) <<- paste0("I",colnames(getI[[X]])))
  }
  if ('neighbors' %in% info) {
    cat('\r \t Extract neighbor info\t')
    getNeighbor <- lapply(1:length(frames),function(X) {
                             inc <- stat$frame == frames[X]
		                     extractNeighbors(stat[inc,]$x,stat[inc,]$y,
	                                        images=colorimages[,,,frames[X]])})
    getNeighbor <- lapply(1:length(frames),function(X) 
                        t(sapply(1:length(getNeighbor[[X]]),function(i) 
		                  as.vector(getNeighbor[[X]][[i]])))) 
    sapply(1:length(getNeighbor),function(X) 
                      colnames(getNeighbor[[X]]) <<- paste0('n',1:27))
  }
  if ('sd' %in% info) {
    cat('\r \t Extract variance particle info\t')
    getVar <- lapply(1:length(frames),function(X) {
		          inc <- stat$frame == frames[X]
                  extractMean(stat[inc,]$patchID,
                              colorimages=colorimages[,,,X],
                              images=attributes(particles)$images[,,frames[X]],
                              fun='sd')})
                                           
    sapply(1:length(getVar),function(X) 
                   colnames(getVar[[X]]) <<- paste0('sd',c('R','G','B')))
  }  

  cat('\r \t Assembling datasets \t \n')
  dat <- lapply(1:length(frames),function(X) {
            inc <- stat$frame == frames[X]
            dat <- stat[inc,]
            if(exists('getI')) dat <- cbind(dat,getI[[X]])
            if(exists('getNeighbor')) dat <- cbind(dat,getNeighbor[[X]])
            if(exists('getMean')) dat <- cbind(dat,getMean[[X]])
            if(exists('getVar')) dat <- cbind(dat,getVar[[X]])
            return(data.frame(dat))
          })
                 
  ## Make training data based on test data and manually identified objects
  if (training == TRUE) {
    for(i in 1:length(frames)){
	  dat[[i]]$trY <- NA
	  dat[[i]]$trY[dat[[i]]$patchID %in% mIdObject$correct] <- 1
	  dat[[i]]$trY[dat[[i]]$patchID %in% mIdObject$wrong] <- 0
    }
  }

  dat <- do.call(rbind,dat)

  attr(dat,"background") <- attributes(particles)$background
  attr(dat,"originalImages") <- attributes(particles)$originalImages
  attr(dat,"originalDirec") <- attributes(particles)$originalDirec
  attr(dat,"subtractedImages") <- attributes(particles)$subtractedImages
  
  class(dat) <- c("TrDm","trainingdata","particles","data.frame")
  
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
runNN <- function(predictors,trainingData,validationData,hidden=3,reps=5,stat='F',...) {
  n <- neuralnet(as.formula(paste("trY ~ ", paste(predictors, collapse= "+"))),
                 data=trainingData,hidden=hidden,rep=reps,...)
  nCom <- compute(n,validationData[,predictors])
  thr <- optThr(trY=validationData$trY,stat=stat,
                nnP=plogis(nCom$net.result))$maximum # find threshold
  res <- list(nn=n,thr=thr,predicted=nCom,trainingData=validationData,
              hidden=hidden,reps=reps,predictors=predictors,stat=stat)
  class(res) <- 'nnTrackdem'
  return(res)
}

##' Optimize threshold based on precision, recall or F-measure.
##'
##' Precision is the number of correct positive results divided by the
##' number of all positive predictions. Recall is the number of correct
##' positive results divided by the number of positive results that
##' could have been returned if the algoritm was perfect.
##' In binary classification, a F1 score (F-score/ F-measure) is statistical
##' measure of accuracy. F1 scores considers both the precision
##' and the recall. A F1 score may be seen as a weighted average
##' (harmonic mean) of the precision and recall.
##' Precision and F1 scores are at best 1 and at worst 0.
##' 
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
    temp <- data.frame(Y=factor(D,levels=c(0,1)),
                       P=factor(as.numeric(P>param),levels=c(0,1)))
    conf <- table(temp)

    #conf <- table(data.frame(Y=as.logical(D),P=P>param))
    
    if(length(attr(conf,"dimnames")$P)<2){
      num <- 1+as.logical(attr(conf,"dimnames")$P)
      temp <- matrix(0,ncol=2,nrow=2)
      temp[,num] <- as.numeric(conf)
      conf <- temp
    }
    
   confuStats(conf)[[stat]]
  }

  flipper <- ifelse(stat=="FN",FALSE,TRUE)
  optimize(ln,c(0,1),maximum = flipper)
}

##' Calculate different statistics for trained neural network.
##' @param confusion Confusion matrix
##' @export
confuStats <- function(confusion){
  accuracy <- sum(diag(confusion))/sum(confusion) # correct predictions
  recall <-  diag(confusion)[2]/sum(confusion[,2])# false positive rate

   ## failed to detect positives?
  if(is.na(recall)) recall <- 0

  TN <- diag(confusion)[1]/sum(confusion[,1]) # true negative
  ## failed to detect any negatives?
  if(is.na(TN)) TN <- 0

  FN <- confusion[2,1]/sum(confusion[,1]) # false negative
  if(is.na(FN)) FN <- 0

  precision <- confusion[2,2]/sum(confusion[,2]) # prop positive that are correct
  if(is.na(precision)) precision <- 0

  F <-  2*((recall*precision)/(recall+precision)) # F-measure
  if(is.na(F)) F <- 0

  list(confusion=confusion, accuracy=accuracy, recall=recall,
        TN=TN, FN=FN, precision=precision, F=F)
}

##' Calculate a confusion matrix for trained neural network.
##' @param Y true values
##' @param P predictions from NN
##' @param stat the statistic of interest
getConfMat <- function(Y,P,thr,stat="F"){

    temp <- data.frame(Y=factor(Y,levels=c(0,1)),
                       P=factor(as.numeric(P>thr),levels=c(0,1)))
    conf <- table(temp)
    
    if(length(attr(conf,"dimnames")$P)<2){
      num <- 1+as.logical(attr(conf,"dimnames")$P)
      temp <- matrix(0,ncol=2,nrow=2)
      temp[,num] <- as.numeric(conf)
      conf <- temp
    }

    if(any(rowSums(conf)==0)) {
      warning("Confusion matrix: data contains no true or false values")
  }
    
   confuStats(conf)[[stat]]

}

##' Apply trained artificial neural network to particleStat object.
##' @param nnTrackdemObject Object of class nnTrackdemObject.
##' @param particleStatObject Object of class particleStatObject.
##' @return A list of class particleStatObject.
##' @export
update.particles <- function(particles,neuralnet,pca=TRUE,...) {
    if(!is.TrDm(neuralnet)){
        stop("Input does not appear to be of the class \"TrDm\"")
    }
    if(!is.TrDm(particles)){
        stop("Input does not appear to be of the class \"TrDm\"")
    }
    
    particles <- extractInfo(particles,training=FALSE,...)
    
    
    if (pca == TRUE) {
	   p <- predict(attributes(neuralnet)$pca,particles)
       
    } else p <- particles
    
  pred <- neuralnet$bestNN$predictors
  newParticleStats <- plogis(compute(neuralnet$bestNN$nn,
                                             p[,pred])$net.result[,1])

  tmp <- newParticleStats > neuralnet$bestNN$thr
  dat <- cbind(data.frame(include=ifelse(tmp,1,0),
                    prob=newParticleStats),particles)
                          
  dat <- dat[dat$include == 1,]
  attr(dat, "class") <- c("TrDm","particles","data.frame")
  attr(dat,"background") <- attributes(particles)$background
  attr(dat,"originalImages") <- attributes(particles)$originalImages
  attr(dat,"originalDirec") <- attributes(particles)$originalDirec
  attr(dat,"subtractedImages") <- attributes(particles)$subtractedImages
  attr(dat,"neuralnet") <- deparse(substitute(neuralnet))
  attr(dat,"nn") <- TRUE
  return(dat)
}

##' Find values for R, G and B layer for specified coordinates.
##' @param x Vector containing x coordinates.
##' @param y Vector containing y coordinates.
##' @param images Three dimensional array.
extractRGB <- function(x,y,images){
  coords <- cbind(x,y)
  RGBmat <- t(apply(coords,1,function(X) images[round(X[2]),round(X[1]),]))
  colnames(RGBmat) <- c("R","G","B")
  return(RGBmat)
}


##' Find mean and sd particle values
##' @param ID Particle ID.
##' @param colorimages Original color images.
##' @param images Array containing identified particles with ID.
##' @export
extractMean <- function(ID,colorimages,images,fun='mean') {
   if (fun=='mean') {
     A <- muP(m=images,id=ID,
              cm1=colorimages[,,1],
              cm2=colorimages[,,2],
              cm3=colorimages[,,3],
              d=dim(images))
   } else if (fun == 'sd') {
     A <- sdP(m=images,id=ID,
              cm1=colorimages[,,1],
              cm2=colorimages[,,2],
              cm3=colorimages[,,3],
              d=dim(images))
   }
  return(A)
}

##' Get R, G and B values for specified coordinates, and its eight neighbor 
##' pixels.
##' @param x Vector containing x coordinates.
##' @param y Vector containing y coordinates.
##' @param Three dimensional array.
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

##' Make a test, validate and training dataset
##'
##' datasets are randomly assigned to each
##' category
##' 
##' @param dat a previously constructed dataset
##' @param prop the proportion for each class
##' c(training, validation,test).
##'
makeTrVaTe <- function(dat,prop=c(.8,.1,.1)){

    ## force proportions
    prop <- prop/sum(prop)
    Ndf <- nrow(dat)
    spl <- seq_len(Ndf)
    Tr.spl <- sample(spl,floor(prop[1]*Ndf),replace=FALSE)
    Va.spl <- sample(spl[-Tr.spl],floor(prop[2]*Ndf),replace=FALSE)
    Te.spl <- spl[-c(Tr.spl,Va.spl)]

    
    Tr <- dat[Tr.spl,]
    Va <- dat[Va.spl,]
    Te <- dat[Te.spl,]

    return(list(Tr=Tr,Va=Va,Te=Te))
}

##' Train, validate and test artificial
##' neural networks
##'
##' The function fits multiple neural networks to
##' a dataset that has been randomly assigned to each
##' of three catergories: train, validate and test.
##' A final neural net is selected based on a fit statistic
##' (either precision, recall or the F1-score). All neural networks
##' are trained to the training dataset. Neural network may vary in
##'  the number of hidden layers. Classification thresholds are selected
##' based on the validation data, and then the final neural network
##' is selected based on the test data.
##'
##' The neural networks may be selcted based on precision, recall or
##' a F1-score (default).
##' In binary classification, precision is the number of correct positive
##' results divided by the number of all positive predictions. Recall is
##' the number of correct positive results divided by the number of positive
##' results that could have been returned if the algoritm was perfect.
##' A F1 score (F-score/ F-measure) is statistical
##' measure of accuracy. F1 scores considers both the precision
##' and the recall. A F1 score may be seen as a weighted average
##' (harmonic mean) of the precision and recall.
##' Precision, recall and F1 scores are at best 1 and at worst 0.
##'
##' @param dat a previously constructed dataset
##' @param fits statistic. May be "precision", "recall", or
##' "F" for the harmonic mean of precision and recall. 
##' @param maxH maximum number of hidden layers to test
##'  note that more layers will require more time to fit.
##' @param repetitions the number of repetitions
##' for the neural network's training.
##' @param prop the proportion or ratio for each class
##' c(training, validation,test).
##' @param predictors Optional. A set of custom predictors
##' for the neural network. Default uses all columns in dat.
##' @param \dots additional parameters, passed to neuralnet.
##' @export
testNN <- function(dat,stat="F",maxH=5,repetitions=3,prop=c(8,1,1),
                   predictors=NULL,pca=TRUE,thr=0.95, ...) {

    datOrig <- dat
    attributes(datOrig) <- attributes(dat)
        
    if(is.null(predictors)){
        predictors <- colnames(dat)
        predictors <- predictors[predictors != 'trY']
        predictors <- predictors[predictors != 'patchID']
    }
    
    if (pca == TRUE) {
      d <- dat[,predictors]
      pc.cr <- princomp(d,cor=TRUE)
      datComp= predict(pc.cr)
      datComp <- datComp[,cumsum(pc.cr$sdev^2/sum(pc.cr$sdev^2)) < thr]
      predictors <- colnames(datComp)
      dat <- as.data.frame(cbind(datComp,dat[,'trY',drop=FALSE]))
    }
    
    Hidd <- 1:maxH

    results <- vector("list", length(maxH))

    DAT <- makeTrVaTe(dat=dat,prop=prop)
    Tr <- DAT$Tr
    Va <- DAT$Va
    Te <- DAT$Te

     cat("\r \t Training, Validating & Testing Neural Networks:  ",
            round(100*0/length(Hidd),2),
         "% Done \t")
    
    for(H in Hidd) {
        
        
        results[[H]] <- runNN(predictors=predictors,Tr,Va,hidden=H,
                              reps=repetitions,
                              stat=stat,...)

         cat("\r \t Training, Validating & Testing Neural Networks:  ",
            round(100*H/length(Hidd),2),
            "% Done \t")
            
    }
    
    cat("\n\n")
    
    ## get predictions from each neural network
    testNNs <- lapply(results, function(X) compute(X$nn,Te[,predictors]))
    valiNNs <- lapply(results, function(X) compute(X$nn,Va[,predictors]))
    trainNNs <- lapply(results, function(X) compute(X$nn,Tr[,predictors]))
    
    ## get optimized prediction/classification thresholds
    thrNNs <- lapply(results, function(X) X$thr)
  
    ## calculate final scores based on the test data
    final <- sapply(Hidd, function(X) getConfMat(Te$trY,testNNs[[X]]$net.result,
                                                 thrNNs[[X]],stat=stat))

    ## calculate scores based on the training data
    finalTr <- sapply(Hidd, function(X) getConfMat(Tr$trY,
                                                   trainNNs[[X]]$net.result,
                                                 thrNNs[[X]],stat=stat))
    ## calculate scores based on the validation data
    finalVa <- sapply(Hidd, function(X) getConfMat(Va$trY,
                                                   valiNNs[[X]]$net.result,
                                                   thrNNs[[X]],stat=stat))

    testTable <- data.frame(layers=Hidd,Training=finalTr,Validation=finalVa,
                            Test=final)
    rownames(testTable) <- paste("ANN",Hidd,sep="-")
    
    bestNN <- which((final-1)==max(final-1))[1]
    cat("\t Neural Network ( ",stat,")  Scores \n")

    print.default(format(as.matrix.data.frame(testTable),digits = 3)
                 ,print.gap = 2L, 
                  quote = FALSE)
    
    cat("\t Neural Net #", bestNN, " selected \n")
    
   conf <- tryCatch(getConfMat(Te$trY,testNNs[[bestNN]]$net.result,
                      results[[bestNN]]$thr,stat='confusion'),
                    error=function(e) Te)

    res <- list(bestNN=results[[bestNN]],
              finalstats=testTable,confusion=conf,fscore=final[bestNN])
              
    attr(res,"background") <- attributes(datOrig)$background
    attr(res,"originalImages") <- attributes(datOrig)$originalImages
    attr(res,"originalDirec") <- attributes(datOrig)$originalDirec
    attr(res,"subtractedImages") <- attributes(datOrig)$subtractedImages
    attr(res,"trainingData") <- deparse(substitute(datOrig))
    attr(res,"data") <- dat
    if(exists('pc.cr')) attr(res,"pca") <- pc.cr
    
    class(res) <- c("TrDm","neuralnet","list")
              
    invisible(res)
}




