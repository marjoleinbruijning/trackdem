# trackdem

---
Particle tracking and demography
---

![](images/animation.mp4)

This package is currently being developed and tested.

## Abstract
The aim of **Trackdem** is to obtain unbiased automated estimates of population 
densities and body size distributions, using video material or image 
sequences as input. It is meant to assist in evolutionary and ecological studies, which 
often rely on accurate estimates of population size, structure and/or 
individual behaviour. The main functionality of **Trackdem** 
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

## Create image sequence from movie.
## This function requires Python, Libav and ExifTool.
## Alternatively, if images are already made, this
## step can be skipped.
createImageSeq(moviepath='Dropbox/Github/trackdem/Test/Movies',
               imagepath='Dropbox/Github/trackdem/Test/ImageSequences')


## Simulate image sequence
dir.create("images")
a <- getwd()
setwd("images")
traj <- simulTrajec(nframes=30,nIndividuals=10,h=0.01,rho=0.9)
setwd(a)

## Load images
dirPictures <- 'images'
allFullImages <- loadImages (dirPictures=direcPictures,nImages=1:30)
plot(allFullImages,frame=1)
stillBack <- createBackground(allFullImages,method='filter')
class(stillBack)
plot(stillBack)
allImages <- subtractBackground(bg=stillBack)
partIden <- identifyParticles(sbg=allImages,
                              pixelRange=c(1,500),
                              autoThres=FALSE)
attributes(partIden)$threshold
summary(partIden)
records <- trackParticles(partIden,L=20,R=3)
summary(records)
summary(records)$res$N
summary(records)$tab[,'Size']+1
plot(records,type='trajectories')


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


