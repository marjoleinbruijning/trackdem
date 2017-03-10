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

To use the automated video to image and metadata creation function from **trackdem** users should download and install Python 2.7,  Libav and ![ExifTool](http://www.sno.phy.queensu.ca/~phil/exiftool/install.html).

Ubuntu user can copy & paste the following commands in a terminal 
```
sudo apt-get update
sudo apt-get install libav-tools
``` 

A guide for Mac and Windows users will follow asap.

## Examples

```r

## Load package
require(trackdem)

## Create image sequence from movie.
## This function requires Python, Libav and ExifTool.
## Alternatively, if images are already made, this
## step can be skipped.
createImageSeq(moviepath='Dropbox/Github/trackdem/Test/Movies',
               imagepath='Dropbox/Github/trackdem/Test/ImageSequences')

## Load images
direcPictures <- 'Test/ImageSequences/002/'
allFullImages <- loadImages (direcPictures=direcPictures,nImages=1:30)

## Create background and subtract
stillBack <- createBackground(allFullImages)
allImages <- subtractBackground(bg=stillBack)

## Identify particles
partIden <- identifyParticles(sbg=allImages,
                              threshold=-0.05, # chosen threshold
                              pixelRange=c(1,500)) # min and max size
summary(partIden)

## Track (without machine learning steps)
records <- trackParticles(partIden)

## Obtain results
incT <- 10
summary(records,incThres=incT)
plot(records,incThres=incT)


#########################################################################
## Artificial neural network ############################################
#########################################################################
## Create training data set
mId <- list()
trainingData <- list()
n <- 5 # top n frames
frames <- order(sapply(1:length(partIden),function(X) nrow(partIden[[X]])),
                decreasing=TRUE)[1:n]
for (i in 1:n) {
  mId[[i]] <- manuallySelect(particles=partIden,frame=frames[i])
  trainingData[[i]] <- extractInfo(particles=partIden,
                                   training=TRUE,
                                   frames=mId[[i]]$frame,
                                   mIdObject=mId[[i]])
}
trainingData <- do.call(rbind,trainingData)

finalNN <- testNN(dat=trainingData,repetitions=5,maxH=4,prop=c(4,3,3))
partIden2 <- update(partIden,finalNN) # update with neural net

## Track (with machine learning steps)
records2 <- trackParticles(partIden2)

## Obtain results
incT <- 10
summary(records2,incThres=incT)
plot(records2,incThres=incT)


```
## Examples of output
![](images/trackingResults.png)
![](images/sizeRecord.png)


