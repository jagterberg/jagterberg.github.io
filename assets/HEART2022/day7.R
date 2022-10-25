#examine eigenvalues of an ER graph
library(igraph)
library(irlba)
library(Matrix)
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

n <- 1000
p <- .1
A <- generate_adj_matrix_ER(p,n)
eigs <- eigen(A)
eigs$values
plot(eigs$values)
#what do you see?
hist(eigs$vectors[,1])

#let's do the same for an SBM

##########################################
rm(list=ls())
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
a <- .2 #don't change yet!
b <- .1 #don't change yet!
c <- .2 #don't change yet!
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

#plot the eigenvalues
eigs <- eigen(A_perm)
plot(eigs$values)

#first, lets cluster by degrees.
degs <- A_perm %*% rep(1,n)
estimated_memberships_degreeclustering <- kmeans(degs,2)
library(mclust)
adjustedRandIndex(estimated_memberships_degreeclustering$cluster,
                  true_memberships)

#----------------------
#now that we have the matrix, we will do dimensionality reduction
A_svd <- irlba(A_perm,2)
graph_embedding <- A_svd$u 

#now we're going to plot the graph_embedding
dat <- as.data.frame(sqrt(n)*graph_embedding)
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


#now repeat the experiment with different:
# sizes of B matrix
# values of n
# community sizes


###################################
#DCSBM example (only if  you do all of the above)
checkstudent <- function() {
  val1 <- (readline(prompt = "did you do all of the above things? (y/n)"))
  if (val1 == "n") {
    print("Please make sure to run all the examples above before moving on.")
  } else {
    val2 <- (readline(prompt = "are you sure you are not just blindly running the code? (y/n)"))
    if(val2 == "n") {
      print("Thank you for being honest. but please go back and run the other examples.")
    } else {
      print("Ok, good. Carry on.")
    }
  }
  
 
}
checkstudent()



#--------------
# DCSBM example
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
A_svd <- irlba(A_perm,2)
graph_embedding <- A_svd$u 

#now we're going to plot the graph_embedding
dat <- as.data.frame(graph_embedding)
dat$community <- as.factor(true_memberships)
library(ggplot2)
g <- ggplot(data=dat,aes(x= V1,y=V2))
g + geom_point(aes(color=community))


