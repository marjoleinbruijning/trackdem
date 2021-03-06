% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/identifyFunctions.R
\name{createBackground}
\alias{createBackground}
\title{Background detection}
\usage{
createBackground(colorimages, method = "mean")
}
\arguments{
\item{colorimages}{Array of class 'TrDm' containing all images, obtained by
\code{\link{loadImages}}.}

\item{method}{Use \code{method='mean'} to calculate the mean value for each
pixel and color.
Use \code{method='powerroot'} to deflate dark values (note, this can only be
used for dark particles on a light background). Use \code{method='filter'} to
replace pixels in which movement has occurred with the mode of neighboring
values.
Note that \code{method='filter'} is computationally more intensive.}
}
\value{
Array of class 'TrDm' and 'colorimage' containing detected background.
}
\description{
\code{createBackground} detects the still background,
containing all motionless pixels (non particles). Three different methods
to detect the background can be used.
}
\examples{
\dontrun{
dir.create("images")
## Create image sequence
traj <- simulTrajec(path="images",
                    nframes=30,nIndividuals=20,domain="square",
                    h=0.01,rho=0.9,
                    sizes=runif(20,0.004,0.006))
## Load images
dir <- "images"
allFullImages <- loadImages (dirPictures=dir,nImages=1:30)
stillBack <- createBackground(allFullImages,method="mean")
plot(stillBack)
}
}
\author{
Marjolein Bruijning, Caspar A. Hallmann & Marco D. Visser
}
