##' Manually identify true and false positives with a GUI.
##'
##' \code{manuallySelect} opens a graphic user interface to create
##'  training data for a neural net by manually selecting true and
##'  false positives (i.e. correctly identified particles and noise, respectively).
##' @param particles A data frame of class 'TrDm' with particle statistics for
##' each frame, obtained by \code{\link{identifyParticles}}.
##' @param colorimages An array with the original full color images, in order
##' to plot on the original images. If \code{NULL}, the original color images
##' are used, obtained from the global environment.
##' @param frames A vector defining the frame(s) that should be used. Default
##' is \code{NULL}; in that case the frame with the maximum number of identified
##' particles is used.
##' @author Marjolein Bruijning, Caspar A. Hallmann & Marco D. Visser
##' @examples
##' \dontrun{
##' dir.create("images")
##' ## Create image sequence
##' traj <- simulTrajec(path="images",
##'                     nframes=30,nIndividuals=20,domain='square',
##'                     h=0.01,rho=0.9,movingNoise=TRUE,
##'                     parsMoving = list(density=20, duration=10, size=1,
##'                                       speed = 10, colRange = c(0,1)),
##'                     sizes=runif(20,0.004,0.006))
##' ## Load images
##' dir <- "images"
##' allFullImages <- loadImages (dirPictures=dir,nImages=1:30)
##' stillBack <- createBackground(allFullImages,method="mean")
##' allImages <- subtractBackground(stillBack)
##' partIden <- identifyParticles(allImages,threshold=-0.1,
##'                                    pixelRange=c(3,400))
##' # select the nframes with the most identified particles
##' nframes <- 3
##' frames <- order(tapply(partIden$patchID,partIden$frame,length),
##'                 decreasing=TRUE)[1:nframes]
##' mId <- manuallySelect(particles=partIden,frame=frames)
##'	}
##' @return List containing three elements: true positives, false positives,
##' and the evaluated frame.
##' @export
manuallySelect <- function (particles,colorimages=NULL,
                            frames=NULL) {

  if(!is.TrDm(particles)){
      stop("Input does not appear to be of the class \"TrDm\"")
  }
  if(is.null(colorimages)) {
    colorimages <- get(attributes(particles)$originalImages,
                       envir=.GlobalEnv)
  }
  if(is.null(frames)) {
    n <- order(tapply(particles$patchID,particles$frame,length),
               decreasing=TRUE)[1]

  } else { n <- frames }

  res <- vector(mode="list",length=length(n))
  for (i in seq_along(n)) {
    res[[i]] <- runSelect(n[i],particles,colorimages)
  }

  res <- lapply(seq_along(res),function(x) {
	            list(wrong=res[[x]]$id[res[[x]]$trY == 0],
	            correct=res[[x]]$id[res[[x]]$trY == 1],
	            frame=n[x])
	            })

  dat <- extractInfo(particles=particles,training=TRUE,
                     frames=n,mIdObject=res)

  attr(res,"background") <- attributes(particles)$background
  attr(res,"originalImages") <- attributes(particles)$originalImages
  attr(res,"originalDirec") <- attributes(particles)$originalDirec
  attr(res,"subtractedImages") <- attributes(particles)$subtractedImages
  attr(res,"trainingData") <- dat

  class(res) <- c("TrDm","tfp","list")
  return(res)
}

runSelect <- function(frame,particles,colorimages,ui,server) {
shiny::runApp(
  list(ui=shiny::fluidPage(
  shiny::titlePanel(paste0("Create a training dataset for frame ",frame)),
  shiny::fluidRow(
     shiny::column(6,
     'Manually select false and true positives to create a training data set.
      To do so, select "True positives" (correctly identified particles) or
      "False positives" (noise). Subsequently, click on the correctly or wrongly
      identified particles in the right graph. Use "Remove" to delete particles
      from the selection.
      Click on "Update image" to reload the image and
      selected particles, or select "Update automatically" to update after each
      click. The left graph can be used to zoom; draw a polygon and clik
      on "Update image". Try to select as many particles as possible
      (> 20 if possible), and when done, click on "Done".',
      align='left',offset=0),
      shiny::column(2,shiny::sidebarPanel(shiny::radioButtons("select",
                                                    shiny::h4("Select:"),
                   choices = list("True positives" = 1,
                                   "False positives" = 2,
                                   "Remove" = 3),selected = 1),width=12)),
      shiny::column(2,
       shiny::actionButton("stop", "Done"),
       shiny::checkboxInput("automatic", "Upload automatically.
                     Slower for larger images.",value=FALSE)

      )),
  shiny::hr(),
    shiny::fluidRow(
    shiny::column(3,
    shiny::h5('Choose region of interest and click on update.'),
    shiny::actionButton("update","Update image"),
    shiny::br(),shiny::br(),
    shiny::plotOutput("plot2", brush = shiny::brushOpts(id="plot2_brush",
                        resetOnNew = TRUE,clip=TRUE))),
    shiny::column(9,shiny::plotOutput("plot1", click = "plot_click"))
    ),

  shiny::hr(),
  shiny::fluidRow(
      shiny::sidebarPanel(
        shiny::h4('Selected true positives'),
        shiny::tableOutput("true"),width=5),
      shiny::sidebarPanel(
        shiny::h4('Selected false positives'),
        shiny::tableOutput("false"),width=5
      ),
      shiny::verbatimTextOutput("selected_select")
  )
),
  server=function(input, output) {
  ranges <- shiny::reactiveValues(x=NULL,y=NULL)
  coords <- shiny::reactiveValues(x=NULL,y=NULL)

  falses <- shiny::reactiveValues()
  falses$df <- data.frame(x=numeric(0),y=numeric(0),id=numeric(0),
                          size=numeric(0),inc=logical(0),no=numeric())
  trues <- shiny::reactiveValues()
  trues$df <- data.frame(x=numeric(0),y=numeric(0),id=numeric(0),
                          size=numeric(0),inc=logical(0),no=numeric())

  data <- particles[particles$frame == frame,]
  data$id <- data$patchID

  # update plot range and coordinates
  shiny::observe({
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges$x <- floor(c(brush$xmin, brush$xmax) * ncol(colorimages))
      ranges$y <- nrow(colorimages) - floor(c(brush$ymin, brush$ymax) *
                  nrow(colorimages))

    } else {
      ranges$x <- c(1,ncol(colorimages))
      ranges$y <- c(1,nrow(colorimages))
    }

    coords$x <- (data$x - min(ranges$x)) / length(seq(ranges$x[1],ranges$x[2]))
    coords$y <- 1 - (data$y - min(ranges$y)) / length(seq(ranges$y[1],
                                                          ranges$y[2]))

  })

  shiny::observe({
    if(is.null(input$plot_click$x)) return(NULL)
    click <- c(input$plot_click$x, input$plot_click$y)
    nearest_point <- which.min(apply(cbind(coords$x,coords$y), 1,
                               function(x) sum(((click-x)^2))))

    shiny::isolate({
      if (input$select == 1) {
        trues$df <- rbind(trues$df,data.frame(x=data$x[nearest_point],
                                              y=data$y[nearest_point],
                                              id=data$id[nearest_point],
                                              size=data$n.cell[nearest_point],
                                              inc=TRUE,
                                              no=nearest_point))
      }
      if (input$select == 2) {
        falses$df <- rbind(falses$df,data.frame(x=data$x[nearest_point],
                                              y=data$y[nearest_point],
                                              id=data$id[nearest_point],
                                              size=data$n.cell[nearest_point],
                                              inc=TRUE,
                                              no=nearest_point))
      }
      if (input$select == 3) {
        trues$df$inc[trues$df$id == data$id[nearest_point]] <- FALSE
        falses$df$inc[falses$df$id == data$id[nearest_point]] <- FALSE
      }
    })
  })

  shiny::observe({
    input$automatic
    if (!input$automatic) {
      p <- shiny::eventReactive(input$update,{

      im <- colorimages[c(min(ranges$y):max(ranges$y)),
                        c(min(ranges$x):max(ranges$x)),,frame,drop=FALSE]
      im <- array(im,dim=c(dim(im)[1:3]))
      im <- raster::brick(im)


      suppressWarnings(raster::plotRGB(im,scale=1,asp=nrow(im)/ncol(im)))
      inc <- coords$x > 0 & coords$x < 1 & coords$y > 0 & coords$y < 1
      graphics::points(coords$x[inc],coords$y[inc],cex=1.5,col='blue')

      if (length(trues$df$x) > 0) {
        tmpx <- coords$x[trues$df$no][trues$df$inc]
        tmpy <- coords$y[trues$df$no][trues$df$inc]
        inc <- tmpx > 0 & tmpx < 1 & tmpy > 0 & tmpy < 1
        graphics::points(tmpx[inc],
                         tmpy[inc],
                         col='green',cex=3)
      }
      if (length(falses$df$x) > 0) {
        tmpx <- coords$x[falses$df$no][falses$df$inc]
        tmpy <- coords$y[falses$df$no][falses$df$inc]
        inc <- tmpx > 0 & tmpx < 1 & tmpy > 0 & tmpy < 1
        graphics::points(tmpx[inc],
                         tmpy[inc],
                         col='red',cex=3)
      }

    },ignoreNULL=FALSE)

    output$plot1 <- shiny::renderPlot({p()})

  } else if (input$automatic) {

    output$plot1 <- shiny::renderPlot({

      im <- colorimages[c(min(ranges$y):max(ranges$y)),
                            c(min(ranges$x):max(ranges$x)),,frame,drop=FALSE]
      im <- array(im,dim=c(dim(im)[1:3]))
      im <- raster::brick(im)

      suppressWarnings(raster::plotRGB(im,scale=1,asp=nrow(im)/ncol(im)))
      inc <- coords$x > 0 & coords$x < 1 & coords$y > 0 & coords$y < 1
      graphics::points(coords$x[inc],coords$y[inc],cex=1.5,col='blue')

      if (length(trues$df$x) > 0) {
        tmpx <- coords$x[trues$df$no][trues$df$inc]
        tmpy <- coords$y[trues$df$no][trues$df$inc]
        inc <- tmpx > 0 & tmpx < 1 & tmpy > 0 & tmpy < 1
        graphics::points(tmpx[inc],
                         tmpy[inc],
                         col='green',cex=3)
      }
      if (length(falses$df$x) > 0) {
        tmpx <- coords$x[falses$df$no][falses$df$inc]
        tmpy <- coords$y[falses$df$no][falses$df$inc]
        inc <- tmpx > 0 & tmpx < 1 & tmpy > 0 & tmpy < 1
        graphics::points(tmpx[inc],
                         tmpy[inc],
                         col='red',cex=3,pch=5)
      }
    })
  }
  })

  output$plot2 <- shiny::renderPlot({suppressWarnings(graphics::plot(colorimages,frame=frame))})
  output$true <- shiny::renderTable({trues$df[trues$df$inc &
	                                 !duplicated(trues$df$id),
                                     c('id','x','y','size')]})
  output$false <- shiny::renderTable({falses$df[falses$df$inc &
	                                  !duplicated(falses$df$id),
                                       c('id','x','y','size')]})

  shiny::observeEvent(input$stop, shiny::stopApp({
    data <- rbind(trues$df[trues$df$inc,],falses$df[falses$df$inc,])
    data$trY <- c(rep(1,sum(trues$df$inc)),rep(0,sum(falses$df$inc)))
    data$frame <- frame
    data[,c(1:4,6:7)]
   }))

}
))

}


## Extract info
##
## \code{extractInfo} creates a dataframe as preparation for
## applying a neural net (\code{\link{testNN}}). For all particles over all
## frames, it collects information on color intensities and neighbor pixels.
## @param particles A data frame with particle statistics for each frame,
## as obtained by \code{\link{identifyParticles}}.
## @param info Character string specififying which info should be extracted.
## @param colorimages An array with the original full color images, in order to plot
## on the original images, obtained by \code{\link{loadImages}}. By default
## the original color images are used.
## @param sbg Images subtracted from background, as obtained by
## \code{\link{subtractBackground}}. By default, the original subtracted images
## are used.
## @param frames A number defining the frame that should be used. Default
## is \code{NULL}, in that case all frames are used.
## @param training Logical. Should identified false and true positives
## be combined to this dataframe? Default is \code{FALSE}.
## @param mIdObject If \code{training=TRUE}, provide a list with true and false positives,
## returned from \code{\link{manuallySelect}}, for each frame.
## @examples
## \dontrun{
## extractInfo(particles=partIden,training=TRUE,
##             frames=1,
##             mIdObject=mId)
##	}
## @return Data frame of class 'TrDm' and 'particles' containing original
## particle statistics, combined with
## additional extracted information.
## @author Marjolein Bruijning, Caspar A. Hallmann & Marco D. Visser
extractInfo <- function (particles,info=c('intensity','neighbors','sd'),
                         colorimages=NULL,sbg=NULL,
                         frames=NULL,mIdObject=NULL,training=FALSE) {

    if(is.null(colorimages)) {
	    colorimages <- get(attributes(particles)$originalImages, envir=.GlobalEnv)
	  }
    if(is.null(sbg)) { sbg <- get(attributes(particles)$subtractedImages,
                                                  envir=.GlobalEnv) }
    if (is.null(frames)) frames <- unique(particles$frame)

    stat <- particles[particles$frame %in% frames,] # Subset

    sbg <- aperm(sbg, c(1,2,4,3))

    nc <- dim(colorimages)[3]

    cat("\n")
  if ('intensity' %in% info) {
    cat('\t Extract intensity info \t \t \t')
    getI <- lapply(seq_along(frames),function(X) {
                    inc <- stat$frame == frames[X]
                    im <- sbg[,,frames[X],,drop=FALSE]
                    im <- array(im,dim=c(dim(im)[c(1,2,4)]))
                    extractRGB(stat[inc,]$x,stat[inc,]$y,
                             images=im)})
    for (i in seq_along(getI)) {
      colnames(getI[[i]]) <- paste0("I",1:nc)
    }
  }
  if ('neighbors' %in% info) {
    cat('\r \t Extract neighbor info                       \t \t \t')
    getNeighbor <- lapply(seq_along(frames),function(X) {
                         inc <- stat$frame == frames[X]
                         im <- colorimages[,,,frames[X],drop=FALSE]
                         im <- array(im,dim=c(dim(im)[c(1,2,3)]))
		                     extractNeighbors(stat[inc,]$x,stat[inc,]$y,
	                                        images=im)})
    getNeighbor <- lapply(seq_along(frames),function(X)
                        t(vapply(seq_along(getNeighbor[[X]]),function(i)
		                  as.vector(getNeighbor[[X]][[i]]),numeric(9*nc))))

    for (i in seq_along(getNeighbor)) {
      colnames(getNeighbor[[i]]) <- paste0("n",1:(9*nc))
    }
  }
  if ('sd' %in% info) {
    cat('\r \t Extract variance particle info                   \t \t \t \t')
    getVar <- lapply(seq_along(frames),function(X) {
		            inc <- stat$frame == frames[X]
                extractMean(stat[inc,]$patchID,
                            colorimages=colorimages[,,,X],
                            images=attributes(particles)$images[,,frames[X]],
                            fun='sd',ncolors=nc)})
    for (i in seq_along(getVar)) {
      colnames(getVar[[i]]) <- paste0('sd',1:nc)
    }
  }

  cat('\r \t Assembling datasets                       \t \t \t \t \n')
  dat <- lapply(seq_along(frames),function(X) {
            inc <- stat$frame == frames[X]
            dat <- stat[inc,]
            if(exists('getI')) dat <- cbind(dat,getI[[X]])
            if(exists('getNeighbor')) dat <- cbind(dat,getNeighbor[[X]])
            if(exists('getVar')) dat <- cbind(dat,getVar[[X]])
            return(data.frame(dat))
          })

  ## Make training data based on test data and manually identified objects
  if (training == TRUE) {
    for(i in seq_along(frames)){
	  dat[[i]]$trY <- NA
	  dat[[i]]$trY[dat[[i]]$patchID %in% mIdObject[[i]]$correct] <- 1
	  dat[[i]]$trY[dat[[i]]$patchID %in% mIdObject[[i]]$wrong] <- 0
    }
  }

  dat <- do.call(rbind,dat)

  attr(dat,"background") <- attributes(particles)$background
  attr(dat,"originalImages") <- attributes(particles)$originalImages
  attr(dat,"originalDirec") <- attributes(particles)$originalDirec
  attr(dat,"subtractedImages") <- attributes(particles)$subtractedImages
  attr(dat,"threshold") <- attributes(particles)$threshold
  attr(dat,"settings") <- attributes(particles)$settings
  attr(dat,"images") <- attributes(particles)$images

  class(dat) <- c("TrDm","trainingdata","particles","data.frame")

  return(dat)
}

## Find values for R, G and B layer for specified coordinates.
extractRGB <- function(x,y,images){
  coords <- cbind(x,y)
  RGBmat <- t(apply(coords,1,function(X) images[round(X[2]),round(X[1]),,drop=FALSE]))
  RGBmat <- matrix(RGBmat,nrow=nrow(coords))
  colnames(RGBmat) <- paste0("color",1:ncol(RGBmat))
  return(RGBmat)
}

## Get R, G and B values for specified coordinates, and its eight neighbor
## pixels.
## @param x Vector containing x coordinates.
## @param y Vector containing y coordinates.
## @param Three dimensional array.
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
  return(lapply(seq_along(x),function(i)
         images[c(Ymin[i],y[i],Ymax[i]),c(Xmin[i],x[i],Xmax[i]),]))
}



## Create neural net
## \code{runNN} traines an artificial neural network.
## @param predictors Vector containing predictors of interest, present
## as column names in TrainingData.
## @param trainingData Dataframe containing the variables of the neural
## network.
## @param hidden A vector of integers specifying the number of hidden neurons
## in each layer (see also \code{\link{neuralnet}}). Default is 3.
## @param reps The number of repetitions. Default is 5.
## @param stat The statistic to be optimized to calculate the threshold.
## Either 'accuray', 'recall', or 'F' (F-measure). Default is 'F'.
## @return An object of class 'nnTrackdem', containing the trained neural
## net. 'Summary' can be used to obtain a summary of the results. 'Plot'
## can be used to plot the results.
## @seealso \code{\link{neuralnet}}, \code{\link{compute}}
## @author Marjolein Bruijning & Marco D. Visser
runNN <- function(predictors,trainingData,validationData,
                  hidden=3,reps=5,stat='F',...) {

  n <- neuralnet::neuralnet(stats::as.formula(paste("trY ~ ", paste(predictors,
                                              collapse= "+"))),
                 data=trainingData,hidden=hidden,rep=reps,...)
  nCom <- neuralnet::compute(n,validationData[,predictors])
  thr <- optThr(trY=validationData$trY,stat=stat,
                nnP=stats::plogis(nCom$net.result))$maximum # find threshold
  res <- list(nn=n,thr=thr,predicted=nCom,trainingData=validationData,
              hidden=hidden,reps=reps,predictors=predictors,stat=stat)
  class(res) <- 'nnTrackdem'
  return(res)
}

## Optimize threshold based on precision, recall or F-measure.
##
## Precision is the number of correct positive results divided by the
## number of all positive predictions. Recall is the number of correct
## positive results divided by the number of positive results that
## could have been returned if the algorithm was perfect.
## In binary classification, a F1 score (F-score/ F-measure) is statistical
## measure of accuracy. F1 scores considers both the precision
## and the recall. A F1 score may be seen as a weighted average
## (harmonic mean) of the precision and recall.
## Precision and F1 scores are at best 1 and at worst 0.
##
## @param stat The statistic to be optimized to calculate the threshold.
## Either 'accuray', 'recall', or 'F' (F-measure).
## nnP Vector containing probabilities.
## trY Vector containing trained response values (either 0 or 1).
## @return Returns a threshold probability.
## @seealso \code{\link{neuralnet}}, \code{\link{compute}}
optThr <- function(trY,nnP,
                   stat="accuracy"){

  ln <- function(param,D=trY,P=nnP){
    temp <- data.frame(Y=factor(D,levels=c(0,1)),
                       P=factor(as.numeric(P>param),levels=c(0,1)))
    conf <- table(temp)

    if(length(attr(conf,"dimnames")$P)<2){
      num <- 1+as.logical(attr(conf,"dimnames")$P)
      temp <- matrix(0,ncol=2,nrow=2)
      temp[,num] <- as.numeric(conf)
      conf <- temp
    }

   confuStats(conf)[[stat]]
  }

  flipper <- ifelse(stat=="FN",FALSE,TRUE)
  stats::optimize(ln,c(0,1),maximum = flipper)
}

## Calculate different statistics for trained neural network.
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

  Fm <-  2*((recall*precision)/(recall+precision)) # F-measure
  if(is.na(Fm)) Fm <- 0

  list(confusion=confusion, accuracy=accuracy, recall=recall,
        TN=TN, FN=FN, precision=precision, F=Fm)
}

## Calculate a confusion matrix for trained neural network.
getConfMat <- function(Y,P,thr,stat="F"){

  temp <- data.frame(Actual=factor(Y,levels=c(0,1)),
                     Predicted=factor(as.numeric(P>thr),levels=c(0,1)))
  conf <- table(temp)

  if(length(attr(conf,"dimnames")$Predicted)<2){
    num <- 1+as.logical(attr(conf,"dimnames")$Predicted)
    temp <- matrix(0,ncol=2,nrow=2)
    temp[,num] <- as.numeric(conf)
    conf <- temp
  }

  if(any(rowSums(conf)==0)) {
    warning("Confusion matrix: data contains no true or false values")
  }

  confuStats(conf)[[stat]]
}

##' Update identified particles.
##'
##' Apply trained artificial neural network to particleStat object.
##' @param object Object of class 'nnTrackdemObject'.
##' @param neuralnet Trained neural net obtained from \code{\link{testNN}}
##' @param pca Logical. By default \code{TRUE}, indicating that a principal
##' component analysis is performed on the predictors.
##' @param colorimages An array with the original full color images, in order
##' to plot on the original images, obtained by \code{\link{loadImages}}.
##' By default the original color images are used.
##' @param sbg Images subtracted from background, as obtained by
##' \code{\link{subtractBackground}}. By default, the original subtracted images
##' are used.
##' @param \dots further arguments passed to or from other methods.
##' @examples
##' \dontrun{
##' dir.create("images")
##' ## Create image sequence
##' traj <- simulTrajec(path="images",
##'                     nframes=30,nIndividuals=20,domain='square',
##'                     h=0.01,rho=0.9,movingNoise=TRUE,
##'                     parsMoving = list(density=20, duration=10, size=1,
##'                                       speed = 10, colRange = c(0,1)),
##'                     sizes=runif(20,0.004,0.006))
##' ## Load images
##' dir <- "images"
##' allFullImages <- loadImages (dirPictures=dir,nImages=1:30)
##' stillBack <- createBackground(allFullImages,method="mean")
##' allImages <- subtractBackground(stillBack)
##' partIden <- identifyParticles(allImages,threshold=-0.1,
##'                                    pixelRange=c(3,400))
##' nframes <- 3
##' frames <- order(tapply(partIden$patchID,partIden$frame,length),
##'                 decreasing=TRUE)[1:nframes]
##' mId <- manuallySelect(particles=partIden,frame=frames)
##' finalNN <- testNN(dat=mId,repetitions=10,maxH=4,prop=c(6,2,2))
##' partIdenNN <- update(particles=partIden,neuralnet=finalNN)
##'	}
##' @return Data frame class 'particles', containing updated
##' particle statistics (excluding
##' particles that have been filtered out by the neural net).
##' @author Marjolein Bruijning, Caspar A. Hallmann & Marco D. Visser
##' @export
update.particles <- function(object,neuralnet,pca=TRUE,colorimages=NULL,
                             sbg=NULL, ...) {

  if(is.null(colorimages)) {
   colorimages <- get(attributes(object)$originalImages, envir=.GlobalEnv)
  }
  if(is.null(sbg)) { sbg <- get(attributes(object)$subtractedImages,
                                                  envir=.GlobalEnv) }

  if(!is.TrDm(neuralnet)){
    stop("Input does not appear to be of the class \"TrDm\"")
  }
  if(!is.TrDm(object)){
    stop("Input does not appear to be of the class \"TrDm\"")
  }

  object <- extractInfo(object,training=FALSE,colorimages=colorimages,
                        sbg=sbg)


  if (pca == TRUE) {
    p <- stats::predict(attributes(neuralnet)$pca,object)
  } else p <- object

  pred <- neuralnet$bestNN$predictors
  newParticleStats <- stats::plogis(neuralnet::compute(neuralnet$bestNN$nn,
                                             p[,pred])$net.result[,1])

  tmp <- newParticleStats > neuralnet$bestNN$thr
  dat <- cbind(data.frame(include=ifelse(tmp,1,0),
                    prob=newParticleStats),object)
  dat <- dat[dat$include == 1,]

  attr(dat, "class") <- c("TrDm","particles","data.frame")
  attr(dat,"background") <- attributes(object)$background
  attr(dat,"originalImages") <- attributes(object)$originalImages
  attr(dat,"originalDirec") <- attributes(object)$originalDirec
  attr(dat,"subtractedImages") <- attributes(object)$subtractedImages
  attr(dat,"neuralnet") <- deparse(substitute(neuralnet))
  attr(dat,"nn") <- TRUE
  attr(dat,"settings") <- attributes(object)$settings
  attr(dat,"threshold") <- attributes(object)$threshold
  attr(dat,"images") <- attributes(object)$images

  return(dat)
}

## Find mean and sd particle values
extractMean <- function(ID,colorimages,images,fun='mean',ncolors=3) {
  if (ncolors == 3) {
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
   } else if (ncolors == 1) {
     if (fun=='mean') {
       A <- muP1(m=images,id=ID,
                cm1=colorimages,
                d=dim(images))
     } else if (fun == 'sd') {
       A <- sdP1(m=images,id=ID,
                cm1=colorimages,
                d=dim(images))
     }
  }

  return(A)
}

## Make a test, validate and training dataset
##
## datasets are randomly assigned to each
## category
##
## @param dat a previously constructed dataset
## @param prop the proportion for each class
## c(training, validation,test).
##
makeTrVaTe <- function(dat,prop=c(.8,.1,.1)){

  ## force proportions
  prop <- prop/sum(prop)

  Ndf <- nrow(dat)
  spl <- seq_len(Ndf)

  if (Ndf <= 20) {
    prop <- prop[1:2]
    prop <- prop/sum(prop)
    Tr.spl <- sample(spl,floor(prop[1]*Ndf),replace=FALSE)
    Va.spl <- sample(spl[-Tr.spl],floor(prop[2]*Ndf),replace=FALSE)
    Te.spl <- Va.spl
    warning(paste("\t Less than 20 identified true and false positives. \n",
                  "\t Therefore no separate test and validation data created.",
                  "\n"))
  } else {
    Tr.spl <- sample(spl,floor(prop[1]*Ndf),replace=FALSE)
    Va.spl <- sample(spl[-Tr.spl],floor(prop[2]*Ndf),replace=FALSE)
    Te.spl <- spl[-c(Tr.spl,Va.spl)]
  }

  Tr <- dat[Tr.spl,]
  Va <- dat[Va.spl,]
  Te <- dat[Te.spl,]

  return(list(Tr=Tr,Va=Va,Te=Te))
}

##' Train, validate and test artificial
##' neural networks
##'
##' Fits multiple neural networks to
##' a dataset; data set has been randomly assigned to each
##' of three categories: train, validate and test.
##' A final neural net is selected based on a fit statistic
##' (either precision, recall or the F1-score). All neural networks
##' are trained to the training dataset. Neural network may vary in
##'  the number of hidden layers. Classification thresholds are selected
##' based on the validation data, and then the final neural network
##' is selected based on the test data.
##'
##' The neural networks may be selected based on precision, recall or
##' a F1-score (default).
##' In binary classification, precision is the number of correct positive
##' results divided by the number of all positive predictions. Recall is
##' the number of correct positive results divided by the number of positive
##' results that could have been returned if the algorithm was perfect.
##' A F1 score (F-score/ F-measure) is a statistical
##' measure of accuracy. F1 scores considers both the precision
##' and the recall. A F1 score may be seen as a weighted average
##' (harmonic mean) of the precision and recall.
##' Precision, recall and F1 scores are at best 1 and at worst 0.
##'
##' @param dat a previously constructed dataset obtained from
##' \code{manuallySelect}.
##' @param stat Fit statistic. May be \code{"precision"}, \code{"recall"}, or
##' \code{"F"} for the harmonic mean of precision and recall.
##' @param maxH maximum number of hidden layers to test
##'  note that more layers will require more time to fit.
##' @param repetitions the number of repetitions
##' for the neural network's training.
##' @param prop the proportion or ratio for each class
##' c(training, validation,test).
##' @param predictors Optional. A set of custom predictors
##' for the neural network. Default uses all columns in \code{dat}.
##' @param pca Logical. \code{TRUE} by default. Should the
##' set of predictors be compressed to the most informative? In short,
##' should a principal component analysis be conducted to select axis that
##' explain at least a fraction \code{thr} (see below)
##' of the variance in the full set of predictors?
##' @param thr Threshold for pca (above).
##' @param \dots additional parameters, passed to neuralnet.
##' @return Returns trained artificial neural net.
##' @author Marjolein Bruijning, Caspar A. Hallmann & Marco D. Visser
##' @examples
##' \dontrun{
##' dir.create("images")
##' ## Create image sequence
##' traj <- simulTrajec(path="images",
##'                     nframes=30,nIndividuals=20,domain='square',
##'                     h=0.01,rho=0.9,movingNoise=TRUE,
##'                     parsMoving = list(density=20, duration=10, size=1,
##'                                       speed = 10, colRange = c(0,1)),
##'                     sizes=runif(20,0.004,0.006))
##' ## Load images
##' dir <- "images"
##' allFullImages <- loadImages (dirPictures=dir,nImages=1:30)
##' stillBack <- createBackground(allFullImages,method="mean")
##' allImages <- subtractBackground(stillBack)
##' partIden <- identifyParticles(allImages,threshold=-0.1,
##'                                    pixelRange=c(3,400))
##' nframes <- 3
##' frames <- order(tapply(partIden$patchID,partIden$frame,length),
##'                 decreasing=TRUE)[1:nframes]
##' mId <- manuallySelect(particles=partIden,frame=frames)
##' finalNN <- testNN(dat=mId,repetitions=10,maxH=4,prop=c(6,2,2))
##' summary(finalNN)
##'	}
##' @export
testNN <- function(dat,stat="F",maxH=5,repetitions=3,prop=c(8,1,1),
                   predictors=NULL,pca=TRUE,thr=0.95, ...) {

  datOrig <- dat
  attributes(datOrig) <- attributes(dat)

  dat <- attributes(dat)$trainingData

  if(is.null(predictors)){
    predictors <- colnames(dat)
    predictors <- predictors[unlist(lapply(dat,stats::var,na.rm=TRUE)) != 0]
    predictors <- predictors[predictors != 'trY']
    predictors <- predictors[predictors != 'patchID']
    predictors <- predictors[predictors != 'frame']
    predictors <- predictors[predictors != 'frac.dim.index']
  }

  if (pca == TRUE) {
    d <- dat[,predictors]
    pc.cr <- stats::princomp(d,cor=TRUE)
    datComp <- stats::predict(pc.cr)
    datComp <- datComp[,cumsum(pc.cr$sdev^2/sum(pc.cr$sdev^2)) < thr,drop=FALSE]
    predictors <- colnames(datComp)
    dat <- as.data.frame(cbind(datComp,dat[,'trY',drop=FALSE]))
  }

  Hidd <- 1:maxH

  results <- vector("list", length(maxH))
  DAT <- makeTrVaTe(dat=dat[!is.na(dat$trY),],prop=prop)
  Tr <- DAT$Tr
  Va <- DAT$Va
  Te <- DAT$Te

   cat("\r \t Training, Validating & Testing Neural Networks:  ",
      round(100*0/length(Hidd),2),"% Done \t")

  for(H in Hidd) {
    results[[H]] <- runNN(predictors=predictors,Tr,Va,hidden=H,
                          reps=repetitions,
                          stat=stat,...)

    cat("\r \t Training, Validating & Testing Neural Networks:  ",
     round(100*H/length(Hidd),2),"% Done \t")
  }

  cat("\n\n")

  ## get predictions from each neural network
  testNNs <- lapply(results, function(X) neuralnet::compute(X$nn,
                                                            Te[,predictors]))
  valiNNs <- lapply(results, function(X) neuralnet::compute(X$nn,
                                                            Va[,predictors]))
  trainNNs <- lapply(results, function(X) neuralnet::compute(X$nn,
                                                             Tr[,predictors]))

  ## get optimized prediction/classification thresholds
  thrNNs <- lapply(results, function(X) X$thr)

  ## calculate final scores based on the test data
  final <- vapply(Hidd, function(X) getConfMat(Te$trY,testNNs[[X]]$net.result,
                                               thrNNs[[X]],stat=stat),
                  numeric(1))


  ## calculate scores based on the training data
  finalTr <- vapply(Hidd, function(X) getConfMat(Tr$trY,
                                                 trainNNs[[X]]$net.result,
                                                 thrNNs[[X]],stat=stat),
                    numeric(1))

  ## calculate scores based on the validation data
  finalVa <- vapply(Hidd, function(X) getConfMat(Va$trY,
                                                 valiNNs[[X]]$net.result,
                                                 thrNNs[[X]],stat=stat),
                    numeric(1))

  testTable <- data.frame(layers=Hidd,Training=finalTr,Validation=finalVa,
                          Test=final)
  rownames(testTable) <- paste("ANN",Hidd,sep="-")
  bestNN <- which((final-1)==max(final-1))[1]
  cat("\t Neural Network ( ",stat,")  Scores \n")

  print.default(format(as.matrix.data.frame(testTable),digits = 3),
                print.gap = 2L,
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
  attr(res,"data") <- dat
  if(exists('pc.cr')) attr(res,"pca") <- pc.cr
  class(res) <- c("TrDm","neuralnet","list")

  invisible(res)
}
