# trackdem

---
Particle tracking and demography
---

![](images/animation.gif)

This package is currently being developed and tested.

## Abstract


## Dependencies

Final dependencies will be listed here.


## Installation

We are aiming to have a release on CRAN soon,
however to install the developmental version from github use the **devtools** package to install the current development version from R.

```r
## devtools is required
require(devtools)
install_github("marjoleinbruijning/trackdem")
```

## Examples

```r

## Load package
require(trackdem)

## Create image sequence from movie.
## This function requires Python, Libav and ExifTool.
## Alternatively, if images are already made, this
## step can be skipped.
createImageSeq(path='~/Dropbox/Github/trackdem/Test/')

## Load images
direcPictures <- '~/Dropbox/Github/trackdem/Test/ImageSequences/20150406_50/'
allFullImages <- loadImages (direcPictures=direcPictures,nImages=1:30)

## Create background and subtract
stillBack <- createBackground(allFullImages)
allImages <- subtractBackground(bg=stillBack,colorimages=allFullImages)

## Identify particles
partIden <- identifyParticles(mSub=allImages,
                              pthreshold=0.0041, # chosen threshold
                              pixelRange=c(10,500)) # min and max size
summary(partIden)

## Track (without machine learning steps)
records <- doTrack(particleStatObject=partIden,L=50)
records2 <- linkTrajec (recordsObject=records,
                        particleStatObject=partIden,
                        R=2,L=50)

## Obtain results
incT <- 10
summary(records2,incThres=incT)
pdf('resultsNN.pdf')
plot(records2,type='trajectories',bg=allFullImages,incThres=incT)
plot(records2,type='sizes',incThres=incT)
dev.off()

#########################################################################
## Artificial neural network ############################################
#########################################################################

## Create training data
mId <- manuallySelect(particleStatObject=partIden,colorimages=allFullImages)
trainingData <- createTrainingData(particleStatObject=partIden,
                                   colorimages=allFullImages,
                                   mSub=allImages,
                                   training=TRUE,
                                   frames=mId$frame,
                                   mIdObject=mId)

## Choose predictors and train neural net
predictors <- c("n.cell",'IR','IB','IG',"x","y",'perim.area.ratio')
nn <- runNN(predictors,trainingData,hidden=4,reps=10,stat='F')
plot(nn)
summary(nn)

## Apply neural net
nnData <- createTrainingData(particleStatObject=partIden,
                                   colorimages=allFullImages,
                                   mSub=allImages,
                                   training=FALSE,
                                   frames=1:30)

particleStatsNN <- updateParticles(nn,nnData)

## Repeat tracking on updated particles
records <- doTrack(particleStatsNN,L=50)
records2 <- linkTrajec (records,particleStatsNN,R=2,L=50)

## Obtain results
incT <- 10
summary(records2,incThres=incT)
pdf('resultsNN.pdf')
plot(records2,type='trajectories',bg=allFullImages,incThres=incT)
plot(records2,type='sizes',incThres=incT)
dev.off()

```
## Examples of output
More screenshots to come
![](images/trackingResults.png)
![](images/sizeRecord.png)


