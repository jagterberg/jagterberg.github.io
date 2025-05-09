library(igraph)
library(irlba)
library(mclust)
library(ggplot2)
#------------------------------------
#load getelbows function
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

##################simulated DCSBM###############
generate_adj_matrix_DCSBM <- function(B,n,memberships,degreecorrections) {
  A <- matrix(0,n,n)
  #generate A:
  for(i in c(1:n)) {
    for(j in c(1:(i-1))) {
      prob <- degreecorrections[i]*degreecorrections[j]*B[memberships[i],memberships[j]]
      A[i,j] <- rbinom(1,1,prob)
      A[j,i] <- A[i,j]
    }
  }
  return(A)
}

n <- 1000
a <- 1
b <- .5
c <- 1
B <- matrix(c(a,b,b,c),2,2)
B
degreecorrections <- runif(n,min=0,max=1)
memberships <- c(rep(1,n/2),rep(2,n/2))

A <- generate_adj_matrix_DCSBM(B,n,memberships,degreecorrections)
heatmap(A,symm = TRUE,Rowv=NA, Colv=NA,labRow=FALSE, labCol=FALSE, revC=TRUE)
perms <- sample(n)
perms
p1 <- as(perms,"pMatrix")
A_perm <- p1 %*% A %*% t(p1)
heatmap(A_perm,symm = TRUE,Rowv=NA, Colv=NA,labRow=FALSE, labCol=FALSE, revC=TRUE)
true_memberships <- memberships[perms]


eigs <- eigen(A_perm)
plot(eigs$values)
getElbows(eigs$values)
A_svd <- irlba(A_perm,2)
graph_embedding <- A_svd$u 

#now we're going to plot the graph_embedding
dat <- as.data.frame(graph_embedding)
dat$community <- as.factor(true_memberships)
library(ggplot2)
g <- ggplot(data=dat,aes(x= V1,y=V2))
g + geom_point(aes(color=community))

##################Harry Potter###################
#now we will load a graph dataset
hpnames <- read.csv("https://jagterberg.github.io/assets/HEART2022/harrypotter/characters.csv")
hpgraph <- read.csv("https://jagterberg.github.io/assets/HEART2022/harrypotter/relations.csv")

#subset to enemies 
hpgraph <- hpgraph[which(hpgraph$type == "-"),]
A <- as.undirected(graph_from_data_frame(hpgraph))
components <- igraph::clusters(A, mode="weak")
biggest_cluster_id <- which.max(components$csize)

# ids
vert_ids <- V(A)[components$membership == biggest_cluster_id]

# subgraph
A <- igraph::induced_subgraph(A, vert_ids)
#A2 <- induced.subgraph(A,largest.independent.vertex.sets(A)[[1]])
V(A)$char <- hpnames[match(names(V(A)),as.character(hpnames$id)),"name"]
Adj <- as_adjacency_matrix(A)
svdA <- svd(Adj)
plot(svdA$d) #take d = 2
Xhat <- svdA$u[,c(1:2)] # %*% diag(svdA$d[c(1:2)]^(1/2))
#plot(Xhat)
Xhat_new <- Xhat
for(i in c(1:nrow(Xhat_new))) {
  Xhat_new[i,] <- Xhat[i,]/sqrt(sum(Xhat[i,]^2))
}
dat <- as.data.frame(Xhat_new)
dat$names <- V(A)$char

dat2 <- as.data.frame(Xhat)
dat2$names <- V(A)$char

ggplot(dat2, aes(x= V1,y=V2,  label=names))+
  geom_point(size=.5) +
  geom_text(hjust=1, vjust=-1,size=2,angle=45)


###################marvel data!########################
edges <- read.csv("https://raw.githubusercontent.com/melaniewalsh/sample-social-network-datasets/master/sample-datasets/marvel/marvel-unimodal-edges.csv")
characters <- read.csv("https://raw.githubusercontent.com/melaniewalsh/sample-social-network-datasets/master/sample-datasets/marvel/marvel-unimodal-nodes.csv")

edges$weight <- edges$Weight
G <- graph_from_data_frame(edges,vertices=characters)
G <- as.undirected(G,mode="collapse")
summary(edges$Weight)

#could delete edges below:
#G <- delete.edges(G,which(E(G)$weight <= 7))
lcc <- clusters(G)
lcc
whichtoKeep <- which(lcc$csize ==max(lcc$csize))
vert_ids <- V(G)[lcc$membership == whichtoKeep]
vert_ids
indsToKeep <- names(V(G))
indsToKeep <- indsToKeep[indsToKeep %in% names(vert_ids)]
G <- igraph::induced_subgraph(G, indsToKeep)

A <- as_adjacency_matrix(G)
all_eigen <- svd(A) #could also run eigen instead 
getElbows(all_eigen$d) #all_eigen$values
r <- 16#100000000 #fill this in based on getElbows or eyeballing
embedding <- all_eigen$u[,c(1:r)] #dat$vectors
dat <- as.data.frame(embedding)
dat$names <- names(V(G))
 ggplot(dat, aes(x= V1,y=V2,  label=names))+
   geom_point(size=.5) +
   geom_text(hjust=1, vjust=-1,size=2,angle=45)
 

 #normalize
embed2 <- matrix(0,nrow(dat),r)
for ( i in c(1:nrow(embedding))) {
  embed2[i,] <- embedding[i,]/sqrt(sum(embedding[i,]^2))
}
dat[,c("V3","V4")] <- embed2
ggplot(dat, aes(x= V3,y=V4,  label=names))+
  geom_point(size=.5) +
  geom_text(hjust=1, vjust=-1,size=2,angle=45)

toclust <- embed2 #could also use embedding without normalization
clusts <- Mclust(as.matrix(toclust)) #could also use Kmeans
clusts$classification #clusts$clusters if using kmeans
clustvals <- clusts$classification
dat[which(clustvals==1),"names"]
dat[which(clustvals==2),"names"]
dat[which(clustvals==3),"names"]
dat[which(clustvals==4),"names"]
dat[which(clustvals==5),"names"]
dat[which(clustvals==6),"names"]
dat[which(clustvals==7),"names"]
dat[which(clustvals==8),"names"]
dat[which(clustvals==9),"names"]
dat[which(clustvals==10),"names"]
dat[which(clustvals==11),"names"]
dat[which(clustvals==12),"names"]
dat[which(clustvals==13),"names"]
dat[which(clustvals==14),"names"]
dat[which(clustvals==15),"names"]
dat[which(clustvals==16),"names"]



