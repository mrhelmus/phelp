# to graft multiple tips, supplied by a vector as a polytomy to a node

require(ape)
require(phytools)

#example
tree<-rcoal(9)
plot(tree)
nodelabels()
tips<-c("o","p","i","e")
where<-"t2"

polyXnode<-function(tree,tips,where=NULL,edge.length=NULL,position=0)
  emat<-tree$edge
  efoc<-which.edge(tree, where.tip)
  foctip<-match(where.tip,tree$tip)
  elen<-tree$edge.length
  nedg<-elen[efoc]/2

  txt<-NULL
  for(addtip in tips){txt<-paste(txt,addtip,":",nedg,",",sep="")}
  txt<-substr(txt,1,(nchar(txt)-1))
  txt<-paste("(",txt,");",sep="")
  add<-read.tree(text=txt)

  nieuw<-bind.tree(tree,add,where=foctip,position=nedg)
  return(nieuw)
}


