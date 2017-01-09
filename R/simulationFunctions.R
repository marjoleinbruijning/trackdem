##' Create a simulated set of movement trajectories
##'
##' \code{simulateTracjectories} simulates movement trajectories within a 
##' bounded space, movements are set with speed (h) and may be correlated
##' in direction (rho). Function simulates movement of particles in a video 
##' sequence of certain number of frames (nframes) in length. 
##' @param nframes number of time frames(steps)
##' @param nIndividuals	number of individual trajectories
##' @param h displacement speed in pixels.
##' @param rho correlation parameter for angle of displacement
##' @param domain one of "square" or "circle", imposing a [0-1,0-1] rectangle
##' domain, or a circlular domain of radius 1, respectively.
##' correct boundary ensure individual trajectories do not cross the domain
##' @param sizes Vector of sizes for each simulated particle of length
##' nIndividuals
##' @examples
##' \dontrun{
##' ## ADD EXAMPLE
##'	}
##' @author Caspar A. Hallmann, Marjolein Bruijning & Marco D. Visser
##' @export
simulateTrajectories <- function(nframes=20,nIndividuals=10,
                                 h=.05,rho=0,domain=c("square","circle"),
                                  correct.boundary=TRUE,
                                  sizes=runif(nIndividuals)*.012+.01
                                 ,...){

    if(length(domain)==2){
        domain <- domain[1]
    } else{
        domain <- domain[1]
    }

    res<-do.call(rbind,
                 lapply(1:nIndividuals,function(j){
                     ##xy<-array(,c(nframes,2))
                     ##xy[1,]<-runif(2)
                     ##for(i in 2:nframes){
                     ##xy[i,]<- xy[i-1,] + sampler(2,0,h)
                     init=NULL

                     if( domain=="square") {
                         init<-  runif(2,0,1)
                     }

                     if( domain=="circle") {
                         phi<-runif(1,0,2*pi); r=runif(1,0,1)
                         init<-  r*c(cos(phi),sin(phi))
                     }
                     
                     xy<- samplePolar(nframes,h,rho,init)
                     if(correct.boundary&domain=="square"){
                         for(i in 2:nframes){
                             ##This ensures they stay in the square 0-1 x 0-1 domain
                             if(any(xy[i,1:2]>1)|any(xy[i,1:2]<0)){
                                 
                                 cond=TRUE
                                 while(cond){
                                     news<-samplePolar(2,h,rho)
                                     xy[i,1:2]<- xy[i-1,1:2] + (news[2,1:2]-news[1,1:2])
                                     xy[i,3:4]<- news[1,3:4]
                                     cond<-FALSE
                                     if(any(xy[i,1:2]>1)|any(xy[i,1:2]<0)){ cond=TRUE}
                                 }
                             }
                         }
                     }
                     
                     if(correct.boundary&domain=="circle"){
                         for(i in 2:nframes){
                             ##.. should be replaced with all
                             ##This ensures they stay in the circle with radius 1 domain
                             if(sqrt(sum(xy[i,1:2]^2))>1){	
                                 cond=TRUE
                                 while(cond){
                                     news<-samplePolar(2,h,rho)
                                     xy[i,]<- xy[i-1,1:2] + (news[2,1:2]-news[1,1:2])
                                     xy[i,3:4]<- news[1,3:4]
                                     cond<-FALSE
                                     if(sqrt(sum(xy[i,1:2]^2))>1){ cond=TRUE}
                                 }
                             }
                         }
                     }

                     if(correct.boundary&!domain%in%c("square","circle")) {
                         stop("domain not recognized")
                     }
                     
                     cbind(id=j,frame=1:nframes,xy)
                 }))
    colnames(res)<-c("id","t","x","y","r","phi")
    res<-as.data.frame(res)
    res$size=sizes[res$id]
    attr(res,"domain")<-domain
    class(res) <- "simtrajectory"
    return(res)
}

##' Polar sampler used in \code{simulateTracjectories}
##'
##' helper function for \code{simulateTracjectories} to simulates corrected
##' polar coordinates. See source of \code{simulateTracjectories} for details. 
##' @param n number of movements
##' @param h displacement speed in pixels.
##' @param rho correlation parameter for angle of displacement
##' @param init initial locations for all particles?
##' @author Caspar A. Hallmann, Marjolein Bruijning & Marco D. Visser
##' @export
samplePolar<-function(n=10,h=.05,rho=0,init=NULL){
    RHO<- matrix(rho,n,n)
    diag(RHO)<-1
    corPhiNorm<- MASS:::mvrnorm(1,rep(0,n),RHO)
    corPhiUnif<- pnorm(corPhiNorm)*2*pi
    r<- replicate(n,sqrt(sum(rnorm(2,0,h)^2)))
    steps<- cbind(r*cos(corPhiUnif),r*sin(corPhiUnif))
    if(is.null(init)){init<- runif(2,0,1)}
    RES <- array(,c(n,4))
    RES[1,1:2]<-init
    for(i in 1:(n-1)){
        RES[i+1,1:2]<- RES[i,1:2]+steps[i,1:2]
    }
    RES[,3]<-r
    RES[,4]<-corPhiUnif
    RES[n,3:4]<-NA
    RES
    }


## plot a simulated trajectory object TrDm objects
##' @export
plot.simtrajectory<-function(traj,noise=FALSE,axes=FALSE,...){
    lim<- ifelse(attr(traj,"domain")=="circle",-1,0)

    plot(0,type="n",xlim=c(lim,1),ylim=c(lim,1),xlab="",ylab="",asp=1,axes=axes,frame=attr(traj,"domain")=="square")
    if(noise){
        generate.background(...,domain=attr(traj,"domain"))
    }
    if(lim==-1){
        lines(cos(seq(0,2*pi,l=300)),sin(seq(0,2*pi,l=300)),lty=3)
    }
    tapply(1:nrow(traj),traj$id,function(i){
	lines(traj[i,c("x","y")],col=sample(colors()[-1],1))
    })
    invisible(NULL)
}


##' add.orgamisms
##'
##' Add organic looking polygons to a plot of simulated trajectories 
##' @param traj simulated trajectories from \code{simulateTracjectories} 
##' @export
add.organisms<- function(traj,col="red"){
    n.orgs<- max(traj$id)
    tapply(1:nrow(traj),traj$id,function(i){
	ii<- which.max(traj[i,"t"])
	polygon(
            makeOrg(phi=-mean(traj[i[-ii],"phi"]),x=traj[i[ii],"x"],y=traj[i[ii],"y"],size=traj[i[1],"size"])
           ,col=col,border=NA)
    })
}

##' makeOrg
##'
##' lower-level function to make organic looking polygons
#
##' @param length number of polygons
##' @param size rough size of each organims (radius)
##' @param phi shape parameter
##' @param x coordinate
##' @param y coordinate
##' @export
makeOrg<-function(length=30,size=.05,phi=0,x=0,y=0){
    piseq<-seq(0,2*pi,l=length)
    fac<-c(seq(.1,1,l=length/2),seq(1,.1,l=length/2))
    xyO<-cbind(cos(piseq),fac*sin(piseq))*size
    xy0<-xyO%*%matrix(c(cos(phi),sin(phi),-sin(phi),cos(phi)),nc=2)
    xy0[,1]<-xy0[,1]+x
    xy0[,2]<-xy0[,2]+y
    xy0

}



##' saveTrajectory
##'
##' Function that saves simulated trajectories as png files
##' @param traj simulated trajectories from \code{simulateTracjectories} 
##' @param noise if TRUE, background noise is added
##' #param add.noise if TRUE, moving noise is added
##' @param name stem of the filename 
##' @param axes if TRUE, axes are included
##' @param background use if you have a predifined background image of the
##' format returned by \code{generateBackground}.
##' #param pars parameters used to generate moving noise
##' these include the density (per image) of noise particles and their duration
##' (in n frames)
##' @author Caspar A. Hallmann, Marjolein Bruijning & Marco D. Visser
##' @export
saveTrajectory<-function(traj,noise=FALSE,add.noise=FALSE,name="trajectory",
                          axes=FALSE,background,pars=list(duration=10,density=10),...){

    z=max(traj$t)
    lim<- ifelse(attr(traj,"domain")=="circle",-1,0)

    if(noise){ 
	xx <- background$blured.x
    }
    
    if(add.noise){
        noisep<-addNoiseBg(bg=background,density=pars$density,duration=pars$duration)
	loc<-noisep[[2]]
	part<-noisep[[1]]
    }

    for(i in 1:z){

        Name=paste(name,formatC(i,width=nchar(z),flag="0"),sep="_")
        png(paste(Name,".png",sep=""))

        plot(0,type="n",xlim=c(lim,1),ylim=c(lim,1),xlab="",ylab="",asp=1,axes=axes)
        if(noise){
            image(xx,xx,background$blured,add=TRUE,col=colorRampPalette(c("transparent","grey"),alpha=TRUE)(64))}
        ## add static noise		
        ##f(noise){generate.background(...,domain=attr(traj,"domain"))}
        ## add dynamic noise	
        if(add.noise&noise){
            for(mb in 1:nrow(noisep[[1]])){
                if (!is.na(loc[mb,i,2])) {
                    points(loc[mb,i,2],loc[mb,i,1],pch=16,cex=part$size[mb],col=part$color[mb])
                }
                ##ext(x=loc[mb,i,2],y=loc[mb,i,1],labels=mb,cex=2)		
            }	
            
            if(lim==-1){
		lines(cos(seq(0,2*pi,l=300)),sin(seq(0,2*pi,l=300)),lty=3)
            }
            ii<-which(traj$t==i)
            for(j in ii){
		polygon(
                    makeOrg(30,traj[j,"size"],phi= ifelse(is.na(traj[j,"phi"]),
                                       tapply(traj$phi,traj$id,mean,na.rm=T)[j==ii],traj[j,"phi"]),x=traj[j,"x"],y=traj[j,"y"]),col="red",border=NA)
            }

        }
        dev.off()
    }


} 

##' generate a background 
##'
##' generates background, or otherwise plots an already generated background
##' Function that saves simulated trajectories as png files
##' param background prevously loaded or generated background 
##' param spots.density density of non-moving spots
##' param clustering spot clustering parameter
##' param blur do the spots look burry?(TRUE or FALSE)
##' param blur.coef spot blurring coefficient
##' param domain want a specific domain? can be "square" of "circle".
##' param plot plot the background?  (TRUE or FALSE)
##' @author Caspar A. Hallmann, Marjolein Bruijning & Marco D. Visser
##' @export
generateBackground<- function(background=NULL,spots.density=10,clustering=0,
                               blur=TRUE,blur.coef=.025,domain=c("square","circle"),
                               plot=TRUE,sizes=runif(spots.density,.1,1.5)){


    domain=domain[1]
    if(domain=="circle") { lim=-1; area=2*pi; offset=0 }
    if(domain=="square") { lim=0; area=1; offset=.5 }

    if(is.null(background)){

        ##if(gradient.density){
        ##parsd<-structure(runif(gradient.complexity+3),names=letters[1:(gradient.complexity+3)])
        ##x=y=seq(lim,1,l=reso)
        ##EG<-expand.grid(x=x,y=y)
        ##X<-with(EG,cbind(1,x,y,x*y,x^2,y^2,x^2*y^2,x^3,y^3,x^3*x^4))
        ##xt<- xtabs(I(X[,1:length(parsd)]%*%parsd)~x+y,EG)
        ##class(xt)<-"matrix"
        ##if(domain=="circle"){
        ##xt[with(EG,sqrt(x^2+y^2)>1)]<-NA
        ##}
        ##image(x,y,xt,add=TRUE,col=grey.colors(132)[-(1:64)])
        ##}

	if(spots.density>0){
            n<-round(spots.density)
            if(clustering!=0){stop("Clustering not implemented yet")} else {
                                                                        xysp<-matrix(runif(2*n,lim,1),ncol=2)
                                                                        if(domain=="circle"){
                                                                            cond=any(toofar<-sqrt(xysp[,1]^2+xysp[,2]^2)>1)
                                                                            while(cond){
                                                                                xysp[toofar,]<- runif(sum(toofar)*2,lim,1)
                                                                                cond=any(toofar<-sqrt(xysp[,1]^2+xysp[,2]^2)>1)
                                                                            }	
                                                                        }
                                                                        if(is.null(sizes)) sizes<-runif(n,.1,1.5)
                                                                        if(!blur&plot){ points(xysp,pch=19,cex=sizes) }
                                                                    }

            if(blur){
                xx<-seq(lim,1,l=512)
                xtsp<-xtabs(sizes~cut(xysp[,1],c(xx[1]-diff(xx[1:2]),xx))+cut(xysp[,2],c(xx[1]-diff(xx[1:2]),xx)))
                ffk<- outer(xx,xx,function(x,y){ dnorm(x,offset,blur.coef)*dnorm(y,offset,blur.coef) } )*diff(xx[1:2])^2
                blured<- Re(fft(fft(ffk)*fft(xtsp),i=T))/length(xtsp)
                blured<-blured[c(257:512,1:256),c(257:512,1:256)]
                ##lured[blured<mean(blured)]<-NA
                if(plot){ image(xx,xx,blured,add=TRUE,col=colorRampPalette(c("transparent","grey"),alpha=TRUE)(64))}
            }
	}
    } else {
        
	if(is.null(background$blured)){ points(background$xyspots,pch=19,cex=background$sizes) }
        else {
            image(background$blured.x,background$blured.x,background$blured,
                  add=TRUE,col=colorRampPalette(c("transparent","grey")
                                               ,alpha=TRUE)(64))
        }
	xysp<-background$xyspots
	blured<- background$blured
	xx<- background$blured.x
    }


    ret=list(xyspots=xysp,sizes=sizes)
    if(blur){
	ret$blured<-blured
	ret$blured.x<-xx
    }


    invisible(ret)

}


##' generate moving "noise" to simulate noise in a video sequence
##'
##' generates noise (e.g. non-focal species, moving debris, light effects)
##' param nframe number of frames
##' param density number of noise particles (over all frames)
##' param size maximum size of noise in pixels
##' param duration the maximum number of frames any single generated noise
##' particle is visible 
##' param bg previously generated background 
##' param speed the maximum speed of the moving noise
##' param col.range color intensity range of noise
addNoiseBg <- function(nframe=30,density=40,size=1,duration=10,speed=10,bg,
                      col.range=c(0,1)){

    dims <- dim(bg$blured)
    particles <- data.frame(size=runif(density,0,size),
                            duration=sample(1:duration,density,replace=TRUE),
                            color=rgb(runif(density,0,0),
                                      runif(density,col.range[1],col.range[2]),
                                      runif(density,col.range[1],col.range[2]),
                                      alpha=runif(density,col.range[1],col.range[2])),
                            x=sample(1:dims[2],density,replace=TRUE),
                            y=sample(1:dims[1],density,replace=TRUE))  

    particles$color <- as.character(particles$color)
    locations<-array(dim=c(density,nframe,2))
    locations[,1,1]<-particles$x
    locations[,1,2]<-particles$y

    sapply(2:(ncol(locations)), function(X) {
 	locations[,X,1]<<-(locations[,X-1,1]+sample((-speed):speed,density,replace=TRUE))
	locations[,X,2]<<-(locations[,X-1,2]+sample((-speed):speed,density,replace=TRUE))
    })

    locations[,,1]<-locations[,,1]/dims[1]
    locations[,,2]<-locations[,,2]/dims[2]

    for(i in 1:density){
	locations[i,sample(1:nframe,ceiling((duration/nframe)*nframe)),]<-NA
	
    }

    return(list(particles,locations))
}

