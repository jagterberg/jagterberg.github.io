#Erdos Renyi Example
generate_adj_matrix_ER <- function(p,n) {
  A <- matrix(0,n,n)
  #generate A:
  for(i in c(1:n)) {
    for(j in c(1:(i-1))) {
      A[i,j] <- rbinom(1,1,p)
      A[j,i] <- A[i,j]
    }
  }
  return(A)
}

p <- .2
n <- 100
A <- generate_adj_matrix_ER(p,n)
heatmap(A,symm = TRUE,Rowv=NA, Colv=NA,labRow=FALSE, labCol=FALSE, revC=TRUE)

#examine degree (fill in):
degs <- A %*% rep(1,n)#why is this the degree?
hist(degs)     
# what is the mean?
# what is the standard deviation (approximately)?

tris <- A%*% A %*% A
tridist <- diag(tris)
heatmap(tris,symm=TRUE,Rowv=NA,Colv=NA,labRow=FALSE,labCol=FALSE,revC=TRUE)
hist(tridist)

#---------------------------------
#SBM example
rm(list=ls())
#first we generate one adjacency matrix from E-R graph
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
#play with these:
a <- .08
b <- .07
c <- .1
d <- .06
B <- matrix(c(
  a,b,b,
  b,c,b,
  b,b,d
),3,3)
B
c1 <- floor(n/3)
c2 <- floor(n/3)
c3 <- n - c1 - c2
memberships <- c(rep(1,c1),rep(2,c2),rep(3,c3))
A <- generate_adj_matrix_SBM(B,n,memberships)
heatmap(A,symm = TRUE,Rowv=NA, Colv=NA,labRow=FALSE, labCol=FALSE, revC=TRUE)


#now look at a histogram of the degree of A
#fill this in:
degs <- A %*% rep(1,n)
hist(degs)

#what is the approximate mean?
#what is the approximate standard deviation?
#what happens if n is larger? 
#repeat with n = 1000. what is the shape of the histogram?

#cluster by the degree:
memberships_estimated <- kmeans(degs,3)

#to measure cluster memberships we use the Adjusted rand index (ARI):
library(mclust)
adjustedRandIndex(memberships_estimated$cluster,memberships)

#in other words, the degrees reveal the cluster memberships.  what happens
# to the clustering if n is small? large?  

#now, repeat the excperiment with B having the same row sums; e.g.
a <- .8
b <- .2
B <- matrix(c(
  a,b,b,
  b,a,b,
  b,b,a
),3,3)
B
A <- generate_adj_matrix_SBM(B,n,memberships)
degs <- A %*% rep(1,n)
hist(degs)

memberships_estimated <- kmeans(degs,3)
adjustedRandIndex(memberships_estimated$cluster,memberships) 
#what is the ARI now? 
#what does this exercise tell you about:
#-  clustering by degree for SBMs with different row sums?
#- clustering by degree for SBMs with the same row sums?

#---------------------------------------
#clustering harry potter dataset by degrees
rm(list=ls())
library(igraph)

#now we will load a graph dataset
hpnames <- read.csv("https://jagterberg.github.io/assets/HEART2022/harrypotter/characters.csv")
hpgraph <- read.csv("https://jagterberg.github.io/assets/HEART2022/harrypotter/relations.csv")

#subset to enemies 
hpgraph <- hpgraph[which(hpgraph$type == "-"),]
A <- as.undirected(graph_from_data_frame(hpgraph))
components <- igraph::clusters(A, mode="weak")
biggest_cluster_id <- which.max(components$csize)
vert_ids <- V(A)[components$membership == biggest_cluster_id]
A <- igraph::induced_subgraph(A, vert_ids)
V(A)$char <- hpnames[match(names(V(A)),as.character(hpnames$id)),"name"]
Adj <- as_adjacency_matrix(A)
hp_degs <- Adj %*% rep(1,length(V(A)))
hist(as.numeric(hp_degs))

#cluster by degrees"
memberships <- kmeans(hp_degs,2)
V(A)[which(memberships$cluster == 1)]$char
V(A)[which(memberships$cluster == 2)]$char
#what do we get from this clustering procedure?

#if you have time try out other clustering procedures:
clusts <- igraph::cluster_edge_betweenness(A)
V(A)[which(clusts$membership == 1)]$char
V(A)[which(clusts$membership == 2)]$char
V(A)[which(clusts$membership == 3)]$char

clusts <- igraph::cluster_leading_eigen(A)
V(A)[which(clusts$membership == 1)]$char
V(A)[which(clusts$membership == 2)]$char
V(A)[which(clusts$membership == 3)]$char
V(A)[which(clusts$membership == 4)]$char
#etc.
#other algorithms:
igraph::cluster_label_prop()
igraph::cluster_leiden()
igraph::cluster_spinglass()
igraph::cluster_walktrap()

