
#removes phylogenetic signal according to BM from a trait using regression 
#on phylogenetic eigenvectors 

trait.nosig<-function(traits,tree,alpha=0.05,method="BM",return.what=c("traits","models","both"))
{
  require(adephylo)
  require(phylobase)

  if(method!="BM") stop("only a Brownian Motion model has been implemented")
  ntraits<-dim(traits)[2]
  decomp<-as.matrix(me.phylo(tree)) #phylo decomposition
  traits<-traits[rownames(decomp),] #make sure everything aligns
  traits.nosig<-NULL
  for(i in 1:ntraits)
  {
    nosig<-traits[,i]
    dat<-data.frame(trait=nosig,decomp)
    best<-lm(trait~1,data=dat)
    pval<-0
    while(pval<=alpha){  #!!!!EDIT THE ALPHA HERE!!!!, 0.05 will give no significance according to BM
      dat<-data.frame(trait=nosig,decomp)
      full<-lm(trait~.,data=dat)
      best<-step(best, scope=list(lower=best, upper=full),steps=1,direction="forward",trace = 0)
      nosig<-resid(best)
      p4d.nosig<-phylo4d(tree,nosig)
      pval <- abouheif.moran(p4d.nosig,method="Abouheif")$pvalue
    }
    if(i==1)bestmodels<-list(best)
    bestmodels<-c(bestmodels,best)
    traits.nosig<-cbind(traits.nosig,resid(best))
  }
  colnames(traits.nosig)<-colnames(traits)
  switch(return.what,
         traits = return(traits.nosig),
         models = return(bestmodels),
         both = return(list(traits.nosig,bestmodels)))
}
