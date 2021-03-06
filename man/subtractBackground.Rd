% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/identifyFunctions.R
\name{subtractBackground}
\alias{subtractBackground}
\title{Background subtraction}
\usage{
subtractBackground(bg, colorimages = NULL)
}
\arguments{
\item{bg}{Array containing still background, as returned from
\code{\link{createBackground}}.}

\item{colorimages}{Array containing all frames, obtained by
\code{\link{loadImages}}. Default is \code{NULL}, in this case the original
images are used from the global environment.}
}
\value{
Returns array of class 'TrDm' and 'sbg' with same size as images,
subtracted from background.
}
\description{
\code{subtractBackground} subtracts each image from a
 previously created still background.
The objects created through the function contain all changing
pixels (i.e. movement).
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
allImages <- subtractBackground(stillBack)
plot(allImages)
}
}
\author{
Marjolein Bruijning, Caspar A. Hallmann & Marco D. Visser
}
