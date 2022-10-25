#implement spectral clustering for an SBM
#first, we need to simulate an SBM
library(igraph)
library(irlba)
library(Matrix)
generate_adj_matrix_SBM <- function(B,n,memberships) {
  A <- matrix(0,n,n)
  #generate A:
  for(i in c(1:n)) {
    for(j in c(1:(i-1))) {
      prob <- B[memberships[i],memberships[j]]
      A[i,j] <- rbinom(1,1,prob)
      A[j,i] <- A[i,j]
    }
  }
  return(A)
}

n <- 1000
a <- .1 #don't change yet!
b <- .05 #don't change yet!
c <- .1 #don't change yet!
B <- matrix(c(
  a,b,
  b,c
),2,2)
B
c1 <- floor(n/2)
c2 <- n - c1
#c3 <- n - c1 - c2
memberships <- c(rep(1,c1),rep(2,c2))
A <- generate_adj_matrix_SBM(B,n,memberships)
heatmap(A,symm = TRUE,Rowv=NA, Colv=NA,labRow=FALSE, labCol=FALSE, revC=TRUE)
perms <- sample(n)
perms
p1 <- as(perms,"pMatrix")
A_perm <- p1 %*% A %*% t(p1)
heatmap(A_perm,symm = TRUE,Rowv=NA, Colv=NA,labRow=FALSE, labCol=FALSE, revC=TRUE)
true_memberships <- memberships[perms]
true_memberships

#first, lets cluster by degrees.
degs <- A_perm %*% rep(1,n)
estimated_memberships_degreeclustering <- kmeans(degs,2)
library(mclust)
adjustedRandIndex(estimated_memberships_degreeclustering$cluster,
                  true_memberships)

#----------------------
#now that we have the matrix, we will do dimensionality reduction
A_svd <- irlba(A_perm,2)
graph_embedding <- A_svd$u %*% diag(sqrt(A_svd$d))

#now we're going to plot the graph_embedding
dat <- as.data.frame(graph_embedding)
dat$community <- as.factor(true_memberships)
library(ggplot2)
g <- ggplot(data=dat,aes(x= V1,y=V2))
g + geom_point(aes(color=community))

#-------------------------------------
#now that we have the graph embedding, we want to cluster
# the rows of it.
estimated_cluster <- kmeans(graph_embedding,2)
dat$estimated_community_kmeans <- as.factor(estimated_cluster$cluster)
g <- ggplot(data=dat,aes(x= V1,y=V2))
g + geom_point(aes(color=community,shape=estimated_community_kmeans))

estimated_cluster_mclust <- Mclust(graph_embedding,2)
dat$estimated_community_mclust <- as.factor(estimated_cluster_mclust$classification)
g <- ggplot(data=dat,aes(x= V1,y=V2))
g + geom_point(aes(color=community,shape=estimated_community_mclust))

#calculate the error:
library(mclust)
adjustedRandIndex(dat$estimated_community_kmeans,dat$community)
adjustedRandIndex(dat$estimated_community_mclust,dat$community)


#now repeat the experiment with different n and connection probabilities







