library(igraph)
library(irlba)
library(Matrix)


#---------------------------community detection
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
B <- matrix(c(.7,.1,.1,.7),2,2)
n <- 400
pis <- c(1/2,1/2)

assignmentvector1 <- rmultinom(n,1,pis)
block.sizes1 <- rowSums(assignmentvector1)
A1 <- sample_sbm(n,B,block.sizes1)
A.adj1 <- as_adj(A1)
p1 <- as(sample(n),"pMatrix")

A_svd <- irlba(A.adj1,2)
Uhat <- A_svd$u 
ind1 <- c(1:block.sizes1[1])
ind2 <- setdiff(c(1:n),ind1)


Uhat_new <- as.matrix(Uhat) 
dat <- Uhat_new
dat <- dat * sqrt(n)
dat <-data.frame(dat)
dat$class <- "1"
dat$class[ind2] <- "2"
library(ggplot2)
g <- ggplot(dat ,aes(x = X1,y=X2)) +
  geom_point(aes(shape = class,color=class))
g

#plot just the second eigenvector
g <- ggplot(dat, aes(x= X2)) + geom_histogram(aes(color=class,fill=class),bins=50)
g



#heatmap of adjacency matrix before organizing by community:
library(RColorBrewer)
par(mar=c(3,4,2,2))
coul <- colorRampPalette(brewer.pal(4, "Accent"))(50)
A_perm <- p1 %*% A.adj1 %*% t(p1)
heatmap(as.matrix(A_perm),symm = TRUE,Rowv=NA, Colv=NA, col=coul,
        labRow=FALSE, labCol=FALSE, revC=TRUE)
heatmap(as.matrix(A.adj1),symm = TRUE,Rowv=NA, Colv=NA, col=coul,
        labRow=FALSE, labCol=FALSE, revC=TRUE)

#plot using igraph plotting:
vertex_attr(A1,"class") <- c(rep("1",block.sizes1[1]),rep("2",block.sizes1[2]))
plot(A1,vertex.color = vertex_attr(A1,"class"),vertex.label=NA,vertex.size=3)


comms <-  cluster_fast_greedy(A1)
plot(comms,A1,vertex.label=NA,vertex.size=3)
adjustedRandIndex(comms$membership,dat$class)

comms <-  cluster_louvain(A1)
plot(comms,A1,vertex.label=NA,vertex.size=3)
adjustedRandIndex(comms$membership,dat$class)

comms <- Mclust(Uhat,G=2)
adjustedRandIndex(comms$classification,dat$class)




