library(igraph)
library(irlba)
library(Matrix)
library(igraphdata)
library(mclust)
library(RColorBrewer)
#--------------------------------------------
#Community Detection on SBMs

#first generate an SBM with equal within-group and between-group probabilities:
B <- matrix(c(.7,.1,.1,.7),2,2)
n <- 400
pis <- c(1/2,1/2)
assignmentvector1 <- rmultinom(n,1,pis)
block.sizes1 <- rowSums(assignmentvector1)
A1 <- sample_sbm(n,B,block.sizes1)
A.adj1 <- as_adj(A1)
Z <- cbind(c(rep(1,block.sizes1[1]),rep(0,block.sizes1[2])),
                          c(rep(0,block.sizes1[1]),rep(1,block.sizes1[2]))
                          )


#apply a random permutation like we would see in the wild
p1 <- as(sample(n),"pMatrix")

A_perm <- p1 %*% A.adj1 %*% t(p1)


#plot both the matrix of probabilities and the adjacency matrix:
par(mar=c(3,4,2,2))
coul <- colorRampPalette(brewer.pal(4, "Accent"))(50)
heatmap(as.matrix(p1%*% Z %*% B %*% t(Z) %*% t(p1)),symm = TRUE,Rowv=NA, Colv=NA, col=coul,
        labRow=FALSE, labCol=FALSE, revC=TRUE)
heatmap(as.matrix(A_perm),symm = TRUE,Rowv=NA, Colv=NA, col=coul,
        labRow=FALSE, labCol=FALSE, revC=TRUE)

#plot the matrix if the vertices are organized according to communities:
heatmap(as.matrix(Z %*% B %*% t(Z)),symm = TRUE,Rowv=NA, Colv=NA, col=coul,
        labRow=FALSE, labCol=FALSE, revC=TRUE)
heatmap(as.matrix(A.adj1),symm = TRUE,Rowv=NA, Colv=NA, col=coul,
        labRow=FALSE, labCol=FALSE, revC=TRUE)


#run a few of igraph's native cluster methods:
clust1 <- cluster_fast_greedy(A1)

# takes too long: 
 #clust2 <- cluster_edge_betweenness(A1)
clust3 <- cluster_infomap(A1)
clust4 <- cluster_label_prop(A1)
clust5 <- cluster_leading_eigen(A1)
clust6 <- cluster_louvain(A1)

#takes too long:
#clust7 <- cluster_optimal(A1)
#clust8 <- cluster_spinglass(A1)
clust9 <- cluster_walktrap(A1)


# can plot some of these if we'd like:
plot(clust1,A1)
plot(clust9,A1)
#not very informative.

#want to measure correctness: adjusted rand index
true_membership <- c(rep(1,block.sizes1[1]),rep(2,block.sizes1[2]))
adjustedRandIndex(clust1$membership,true_membership)
adjustedRandIndex(clust3$membership,true_membership)
adjustedRandIndex(clust4$membership,true_membership)
adjustedRandIndex(clust5$membership,true_membership)
adjustedRandIndex(clust6$membership,true_membership)
adjustedRandIndex(clust9$membership,true_membership)

#all get the correct memberships...
# what about if we have a really sparse graph? 
B <- 2*matrix(c(.1,.07,.07,.07,.09,.2,.07,.2,.1),3,3)
B
n <- 400
pis <- c(1/3,1/3,1/3)
assignmentvector1 <- rmultinom(n,1,pis)
block.sizes1 <- rowSums(assignmentvector1)
A1 <- sample_sbm(n,B,block.sizes1)
A.adj1 <- as_adj(A1)
Z <- cbind(c(rep(1,block.sizes1[1]),rep(0,block.sizes1[2]),rep(0,block.sizes1[3])),
           c(rep(0,block.sizes1[1]),rep(1,block.sizes1[2]),rep(0,block.sizes1[3])),
           c(rep(0,block.sizes1[1]),rep(0,block.sizes1[2]),rep(1,block.sizes1[3]))
)


#apply a random permutation like we would see in the wild
p1 <- as(sample(n),"pMatrix")

A_perm <- p1 %*% A.adj1 %*% t(p1)


#plot both the matrix of probabilities and the adjacency matrix:
par(mar=c(3,4,2,2))
coul <- colorRampPalette(brewer.pal(4, "Accent"))(50)
heatmap(as.matrix(p1%*% Z %*% B %*% t(Z) %*% t(p1)),symm = TRUE,Rowv=NA, Colv=NA, col=coul,
        labRow=FALSE, labCol=FALSE, revC=TRUE)
heatmap(as.matrix(A_perm),symm = TRUE,Rowv=NA, Colv=NA, col=coul,
        labRow=FALSE, labCol=FALSE, revC=TRUE)

#plot the matrix if the vertices are organized according to communities:
heatmap(as.matrix(Z %*% B %*% t(Z)),symm = TRUE,Rowv=NA, Colv=NA, col=coul,
        labRow=FALSE, labCol=FALSE, revC=TRUE)
heatmap(as.matrix(A.adj1),symm = TRUE,Rowv=NA, Colv=NA, col=coul,
        labRow=FALSE, labCol=FALSE, revC=TRUE)


#run a few of igraph's native cluster methods:
clust1 <- cluster_fast_greedy(A1)

# takes too long: 
#clust2 <- cluster_edge_betweenness(A1)
clust3 <- cluster_infomap(A1)
clust4 <- cluster_label_prop(A1)
clust5 <- cluster_leading_eigen(A1)
clust6 <- cluster_louvain(A1)

#takes too long:
#clust7 <- cluster_optimal(A1)
#clust8 <- cluster_spinglass(A1)
clust9 <- cluster_walktrap(A1)


# can plot some of these if we'd like:
plot(clust1,A1)
plot(clust9,A1)
#not very informative.

#want to measure correctness: adjusted rand index
true_membership <- c(rep(1,block.sizes1[1]),rep(2,block.sizes1[2]),rep(3,block.sizes1[3]))
adjustedRandIndex(clust1$membership,true_membership)
adjustedRandIndex(clust3$membership,true_membership)
adjustedRandIndex(clust4$membership,true_membership)
adjustedRandIndex(clust5$membership,true_membership)
adjustedRandIndex(clust6$membership,true_membership)
adjustedRandIndex(clust9$membership,true_membership)

#why are these bad?
?cluster_fast_greedy
?cluster_leading_eigen
?cluster_louvain

#problem: these work best for assortative graphs or graphs with special structure
# they don't all work for arbitrary B matrices

#spectral clustering (which we will discuss more) is a way to do this for any B matrix:
A_svd <- irlba(A.adj1,3)
Uhat <- A_svd$u 
ind1 <- c(1:block.sizes1[1])
ind2 <- c((block.sizes1[1]+1):(block.sizes1[1] + block.sizes1[2]))
ind3 <- c((block.sizes1[2]+block.sizes1[1]+1):n)

Uhat_new <- as.matrix(Uhat) 
dat <- Uhat_new
dat <- dat * sqrt(n)
dat <-data.frame(dat)
dat$class <- "1"
dat$class[ind2] <- "2"
dat$class[ind3] <- "3"
library(ggplot2)
g <- ggplot(dat ,aes(x = X1,y=X2)) +
  geom_point(aes(shape = class,color=class))
g

#plot the 2nd and third dimensions
g <- ggplot(dat ,aes(x = X2,y=X3)) +
  geom_point(aes(shape = class,color=class))
g

# run spectral clustering with Mclust:
clusts_sp <- Mclust(Uhat_new)
adjustedRandIndex(clusts_sp$classification,true_membership)



#-------------------------------------------------------
#community detection on Karate data
#use built-in clustering
rm(list = ls())
data(karate)
clusts <- cluster_fast_greedy(karate)
membership(clusts)
plot(karate,mark.groups = clusts,vertex.color = vertex_attr(karate,"color"))

clusts <- cluster_optimal(karate)
membership(clusts)
plot(karate,mark.groups = clusts,vertex.color = vertex_attr(karate,"color"))
dat <- as_adjacency_matrix(karate)
#spectral clustering:
getElbows <- function(dat, n = 3, threshold = FALSE, plot = TRUE, main="", ...) {
  ## Given a decreasingly sorted vector, return the given number of elbows
  ##
  ## Args:
  ##   dat: a input vector (e.g. a vector of standard deviations), or a input feature matrix.
  ##   n: the number of returned elbows.
  ##   threshold: either FALSE or a number. If threshold is a number, then all
  ##   the elements in d that are not larger than the threshold will be ignored.
  ##   plot: logical. When T, it depicts a scree plot with highlighted elbows.
  ##
  ## Return:
  ##   q: a vector of length n.
  ##
  ## Reference:
  ##   Zhu, Mu and Ghodsi, Ali (2006), "Automatic dimensionality selection from
  ##   the scree plot via the use of profile likelihood", Computational
  ##   Statistics & Data Analysis, Volume 51 Issue 2, pp 918-930, November, 2006.
  
  #  if (is.unsorted(-d))
  
  if (is.matrix(dat)) {
    d <- sort(apply(dat,2,sd), decreasing=TRUE)
  } else {
    d <- sort(dat,decreasing=TRUE)
  }
  
  if (!is.logical(threshold))
    d <- d[d > threshold]
  
  p <- length(d)
  if (p == 0)
    stop(paste("d must have elements that are larger than the threshold ",
               threshold), "!", sep="")
  
  lq <- rep(0.0, p)                     # log likelihood, function of q
  for (q in 1:p) {
    mu1 <- mean(d[1:q])
    mu2 <- mean(d[-(1:q)])              # = NaN when q = p
    sigma2 <- (sum((d[1:q] - mu1)^2) + sum((d[-(1:q)] - mu2)^2)) /
      (p - 1 - (q < p))
    lq[q] <- sum( dnorm(  d[1:q ], mu1, sqrt(sigma2), log=TRUE) ) +
      sum( dnorm(d[-(1:q)], mu2, sqrt(sigma2), log=TRUE) )
  }
  
  q <- which.max(lq)
  if (n > 1 && q < (p-1)) {
    q <- c(q, q + getElbows(d[(q+1):p], n-1, plot=FALSE))
  }
  
  if (plot==TRUE) {
    if (is.matrix(dat)) {
      sdv <- d # apply(dat,2,sd)
      plot(sdv,type="b",xlab="dim",ylab="stdev",main=main,...)
      points(q,sdv[q],col=2,pch=19)
    } else {
      plot(dat, type="b",main=main,...)
      points(q,dat[q],col=2,pch=19)
    }
  }
  
  return(q)
}
Xhat <- irlba(as_adjacency_matrix(karate),15)
d <- getElbows(Xhat$d)[2]
Xhat <- Xhat$u[,c(1:d)] %*% diag(Xhat$d[c(1:d)])
clusts_sp <- Mclust(Xhat)
clusts_sp <- clusts_sp$classification
toMark <- list()
for (i in c(1:length(unique(clusts_sp)))) {
  toMark[[i]] <- which(clusts_sp == i)
}
plot(karate,vertex.color = vertex_attr(karate,"color"), mark.groups = toMark)
#force it to be 2:
clusts_sp <- Mclust(Xhat,G=2)
clusts_sp <- clusts_sp$classification
plot(karate,vertex.color = vertex_attr(karate,"color"), mark.groups = list(which(clusts_sp==1),which(clusts_sp==2)))


#Modify the layout:
weight.community=function(row,membership,weight.within,weight.between){
  if(as.numeric(membership[which(names(membership)==row[1])])==as.numeric(membership[which(names(membership)==row[2])])){
    weight=weight.within
  }else{
    weight=weight.between
  }
  return(weight)
}

g <- karate
names(clusts_sp) <- vertex_attr(g,"name")
E(g)$weight=apply(get.edgelist(g),1,weight.community,clusts_sp,100,1)
g$layout=layout.fruchterman.reingold(g,weights=E(g)$weight)
plot(g,vertex.color = vertex_attr(karate,"color"), mark.groups = list(which(clusts_sp==1),which(clusts_sp==2)))


clusts_sp <- Mclust(Xhat)
clusts_sp <- clusts_sp$classification
g <- karate
names(clusts_sp) <- vertex_attr(g,"name")
E(g)$weight=apply(get.edgelist(g),1,weight.community,clusts_sp,10,1)
g$layout=layout.fruchterman.reingold(g,weights=E(g)$weight)

toMark <- list()
for (i in c(1:length(unique(clusts_sp)))) {
  toMark[[i]] <- which(clusts_sp == i)
}
plot(g,vertex.color = vertex_attr(karate,"color"), mark.groups = toMark)

clusts_sp <- Mclust(Xhat)
clusts_sp <- clusts_sp$classification
g <- karate
names(clusts_sp) <- vertex_attr(g,"name")
org <- vertex_attr(karate,"color")
names(org)<- vertex_attr(g,"name")
E(g)$weight=apply(get.edgelist(g),1,weight.community,org,10,1)
g$layout=layout.fruchterman.reingold(g,weights=E(g)$weight)

toMark <- list()
for (i in c(1:length(unique(clusts_sp)))) {
  toMark[[i]] <- which(clusts_sp == i)
}
plot(g,vertex.color = vertex_attr(karate,"color"), mark.groups = toMark)




#-------------------------------------------------
#clustering US airports
rm(list=ls())
data("USairports")
adj <- as_adj(USairports)

#make it undirected:
adj <-.5*(adj + t(adj))
getElbows <- function(dat, n = 3, threshold = FALSE, plot = TRUE, main="", ...) {
  ## Given a decreasingly sorted vector, return the given number of elbows
  ##
  ## Args:
  ##   dat: a input vector (e.g. a vector of standard deviations), or a input feature matrix.
  ##   n: the number of returned elbows.
  ##   threshold: either FALSE or a number. If threshold is a number, then all
  ##   the elements in d that are not larger than the threshold will be ignored.
  ##   plot: logical. When T, it depicts a scree plot with highlighted elbows.
  ##
  ## Return:
  ##   q: a vector of length n.
  ##
  ## Reference:
  ##   Zhu, Mu and Ghodsi, Ali (2006), "Automatic dimensionality selection from
  ##   the scree plot via the use of profile likelihood", Computational
  ##   Statistics & Data Analysis, Volume 51 Issue 2, pp 918-930, November, 2006.
  
  #  if (is.unsorted(-d))
  
  if (is.matrix(dat)) {
    d <- sort(apply(dat,2,sd), decreasing=TRUE)
  } else {
    d <- sort(dat,decreasing=TRUE)
  }
  
  if (!is.logical(threshold))
    d <- d[d > threshold]
  
  p <- length(d)
  if (p == 0)
    stop(paste("d must have elements that are larger than the threshold ",
               threshold), "!", sep="")
  
  lq <- rep(0.0, p)                     # log likelihood, function of q
  for (q in 1:p) {
    mu1 <- mean(d[1:q])
    mu2 <- mean(d[-(1:q)])              # = NaN when q = p
    sigma2 <- (sum((d[1:q] - mu1)^2) + sum((d[-(1:q)] - mu2)^2)) /
      (p - 1 - (q < p))
    lq[q] <- sum( dnorm(  d[1:q ], mu1, sqrt(sigma2), log=TRUE) ) +
      sum( dnorm(d[-(1:q)], mu2, sqrt(sigma2), log=TRUE) )
  }
  
  q <- which.max(lq)
  if (n > 1 && q < (p-1)) {
    q <- c(q, q + getElbows(d[(q+1):p], n-1, plot=FALSE))
  }
  
  if (plot==TRUE) {
    if (is.matrix(dat)) {
      sdv <- d # apply(dat,2,sd)
      plot(sdv,type="b",xlab="dim",ylab="stdev",main=main,...)
      points(q,sdv[q],col=2,pch=19)
    } else {
      plot(dat, type="b",main=main,...)
      points(q,dat[q],col=2,pch=19)
    }
  }
  
  return(q)
}
Xhat <- irlba(adj,25)
d <- getElbows(Xhat$d)[2]
Xhat <- Xhat$u[,c(1:d)] %*% diag(Xhat$d[c(1:d)])
clusts_sp <- Mclust(Xhat)
summary(clusts_sp)

classes <- clusts_sp$classification
V(USairports)[classes==3]

plot(Xhat[,1],Xhat[,2])
V(USairports)[which(Xhat[,1] > 60)]
plot(Xhat[,2],Xhat[,3])
V(USairports)[which(Xhat[,3] < -10)] #these are definitely big airports
V(USairports)[which(Xhat[,2] > 25 | Xhat[,2] < -15)] 


