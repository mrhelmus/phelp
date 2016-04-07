optima.dist<-function(dat,hansenfit,method="euclidian",whattraits=NULL)
{
  trait.names<-names(hansenfit$fit)
  
  optima<-sapply(hansenfit$fit,function(x)summary(x)$optima[[1]])
  
  if(!is.null(whattraits)){
        optima<-optima[,whattraits]
        dat<-dat[,whattraits]
        trait.names<-trait.names[whattraits]
      }
	dat<-dat[,trait.names]
	optima<-optima[,trait.names]
	
	if(dim(dat)[2]!=dim(optima)[2])stop("dat, hansenfit or whattraits are not compatable, dat should include all species and traits in hansenfit")

	#calculate distances
	dist.optima<-function(datspp)
	  {
	   as.matrix(dist(rbind(datspp,optima),method=method))[-1,1]
    }
	return(apply(dat,1,dist.optima))
}