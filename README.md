# trackdem

---
Particle tracking and demography
---

![](images/animation2.gif) ![](images/animation.gif)

A test version of this package has been released (0.1 on CRAN), bug reports and comments are ![welcome](https://github.com/marjoleinbruijning/trackdem/issues).

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

**trackdem** can now be installed from ![CRAN](https://cran.r-project.org/web/packages/trackdem/index.html) or from github.

```r
## Install from CRAN
install.packages('trackdem')

## Install from Github
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
set.seed(100)
## Create image sequence (this takes a moment)
traj <- simulTrajec(nframes=30,nIndividuals=20,domain='square',
                    h=0.01,rho=0.9,staticNoise=FALSE,
                    sizes=runif(20,0.004,0.006))
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
stillBack <- createBackground(allFullImages,method='filter')
stillBack
class(stillBack)
plot(stillBack)

## Subtract background
allImages <- subtractBackground(bg=stillBack)
allImages

## Identify moving particles
partIden <- identifyParticles(sbg=allImages,
                              pixelRange=c(1,500),
                              autoThres=FALSE,threshold=-0.05)
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
## Trajectories
plot(records,type='trajectories')
for (i in 1:length(unique(traj$id))) {
  lines(traj$x[traj$id==i],traj$y[traj$id==i],col="grey",
	    lty=2,lwd=2)
}



```
## Examples of output
![](images/trackingResults.png)
![](images/sizeRecord.png)


