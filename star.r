#' Collapse a Node in a Tree (ie, create a polytomy)
#'
#' This function collapse a node in a tree to a polytomy
#' @param phy A phylogenetic tree in the class "phylo"
#' @param node A numerical node in the tree to collapse
#' @export
#' @return Returns pruned phylogeny
#' @examples
 plot(tree <- rcoal(10))
 plot(collapse.to.star(tree, 17))

collapse.to.star<-function (tree, node)
{
    tt <- splitTree(tree, split = list(node = node, bp = tree$edge.length[which(tree$edge[,2] == node)]))
    ss <- starTree(species = tt[[2]]$tip.label, branch.lengths = diag(vcv(tt[[2]])))
    ss$root.edge <- 0
    tree <- paste.tree(tt[[1]], ss)
    return(tree)
}
