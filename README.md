# trackdem

---
Particle tracking and demography
---

![](images/animation.mp4)

This package is currently being developed and tested.

## Abstract
The aim of **trackdem** is to obtain unbiased automated estimates of population 
densities and body size distributions, using video material or image 
sequences as input. It is meant to assist in evolutionary and ecological studies, which 
often rely on accurate estimates of population size, structure and/or 
individual behaviour. The main functionality of **trackdem** 
includes a set of functions to convert a short video into an image sequence, 
background detection, particle identification and linking, and 
the training of an artifical neural network for noise filtering.


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

########################################################################
## Simulate image sequence
########################################################################
dir.create('images')
a <- getwd()
setwd('images')
set.seed(1000)
traj <- simulTrajec(nframes=30,nIndividuals=10,domain='square',h=0.01,rho=0.9)
setwd(a)

########################################################################
## Analyze image sequence
########################################################################
## Load images
dir <- 'images'
allFullImages <- loadImages (dirPictures=dir,nImages=1:30)
allFullImages
class(allFullImages)
plot(allFullImages,frame=1)

## Detect background
stillBack <- createBackground(allFullImages,method='powerroot')
stillBack
class(stillBack)
plot(stillBack)

## Subtract background
allImages <- subtractBackground(bg=stillBack)
allImages

## Identify moving particles
partIden <- identifyParticles(sbg=allImages,
                              pixelRange=c(30,500),
                              autoThres=FALSE,threshold=-0.1)
attributes(partIden)$threshold # calculated threshold
summary(partIden)
attributes(partIden)$threshold
plot(partIden,frame=10)

## Reconstruct trajectories
records <- trackParticles(partIden,L=60,R=3)
summary(records)
summary(records)$N # population count
summary(records)$particles[,'Size'] # body size distribution
summary(records)$particles[,'Total movement'] # movement distribution
summary(records)$area # area covered by particles
summary(records)$presence # minimum presence
dim(records$trackRecord)
dim(records$sizeRecord)
dim(records$colorRecord)

## Obtain results
## Size distribution
par(mfrow=c(1,2))
plot(sort(unique(traj$size)),cex=2,pch=16,xlab='',ylab='Size')
plot(records,type='sizes')
## Trajectories
par(mfrow=c(1,1))
plot(records,type='trajectories')
sapply(1:length(unique(traj$id)),function(i){
	  lines(traj$x[traj$id==i],traj$y[traj$id==i],col="grey",
	  lty=2,lwd=2)
    })



#########################################################################
## Artificial neural network ############################################
#########################################################################
## Create training data set
mId <- list()
n <- 5 # top n frames
frames <- frames <- order(tapply(partIden$patchID,partIden$frame,length),
                          decreasing=TRUE)[1:n]
                
for (i in 1:n) {
  mId[[i]] <- manuallySelect(particles=partIden,frame=frames[i])
}

finalNN <- testNN(dat=mId,repetitions=5,maxH=4,prop=c(4,3,3))
partIden2 <- update(partIden,finalNN) # update with neural net

## Track (with machine learning steps)
records2 <- trackParticles(partIden2)

## Obtain results
summary(records2)
plot(records2)


```
## Examples of output
![](images/trackingResults.png)
![](images/sizeRecord.png)


