#load libraries:
library(igraph)
library(irlba)
library(Matrix)
library(igraphdata)
library(mclust)
library(RColorBrewer)
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

#-----------------------------
#Simulate 2-block SBM:
n <- 1000
B <- matrix(c(.6,.3,.3,.5),2,2)
pis <- c(1/2,1/2)
block.sizes1 <-  rowSums(rmultinom(n,1,pis))
block.sizes1
A1 <- sample_sbm(n,B,block.sizes1)
A.adj1 <- as_adj(A1)
Z <- cbind(c(rep(1,block.sizes1[1]),rep(0,block.sizes1[2])),
           c(rep(0,block.sizes1[1]),rep(1,block.sizes1[2]))
)

#Let's examine E(A) and A with their heatmaps:
P <- Z %*% B %*% t(Z)
par(mar=c(3,4,2,2))
coul <- colorRampPalette(brewer.pal(3, "Accent"))(50)
heatmap(as.matrix(Z %*% B %*% t(Z)),symm = TRUE,Rowv=NA, Colv=NA, col=coul,
        labRow=FALSE, labCol=FALSE, revC=TRUE)
heatmap(as.matrix(A.adj1),symm = TRUE,Rowv=NA, Colv=NA, col=coul,
        labRow=FALSE, labCol=FALSE, revC=TRUE)

#compute two graph embeddings of P:
P_eigen <- irlba(P,2)
U <- P_eigen$u #this grabs just the eigenvectors
dim(U) # why is it this dimension?
X <- P_eigen$u %*% diag(P_eigen$d^(1/2))
dim(X)

#compute two actual graph embeddings for simulated A:
A_eigen <- irlba(A.adj1,2)
Uhat <- A_eigen$u
Xhat <- A_eigen$u %*% diag(A_eigen$d^(1/2))

# we will now plot the first embedding, but first let's make it nicer:
dat <- data.frame(Uhat)
names(dat) <- c("dim1","dim2")
#add community labels:
dat$community <- "2"
dat$community[c(1:block.sizes1[1])] <- "1"

#use ggplot2 to plot the data:
plt <- ggplot(dat,aes(x= dim1, y= dim2,color=community)) + geom_point()
plt

#do the same thing but with Xhat instead:
dat <- data.frame(Xhat)
names(dat) <- c("dim1","dim2")
dat$community <- "2"
dat$community[c(1:block.sizes1[1])] <- "1"
plt <- ggplot(dat,aes(x= dim1, y= dim2,color=community)) + geom_point()
plt


#----------------------------------------
#now let's try for a three-block stochastic blockmodel
n <- 1000
B <- matrix(c(.7,.3,.2,
              .3,.5,.2,
              .2,.2,.1),3,3)
pis <- c(1/4,1/2,1/4)
block.sizes1 <-  rowSums(rmultinom(n,1,pis))
block.sizes1
A1 <- sample_sbm(n,B,block.sizes1)
A.adj1 <- as_adj(A1)
Z <- cbind(c(rep(1,block.sizes1[1]),rep(0,block.sizes1[2]),rep(0,block.sizes1[3])),
           c(rep(0,block.sizes1[1]),rep(1,block.sizes1[2]),rep(0,block.sizes1[3])),
           c(rep(0,block.sizes1[1]),rep(0,block.sizes1[2]),rep(1,block.sizes1[3]))
          
)

#Let's examine E(A) and A with their heatmaps:
P <- Z %*% B %*% t(Z)
par(mar=c(3,4,2,2))
coul <- colorRampPalette(brewer.pal(6, "Accent"))(50)
heatmap(as.matrix(Z %*% B %*% t(Z)),symm = TRUE,Rowv=NA, Colv=NA, col=coul,
        labRow=FALSE, labCol=FALSE, revC=TRUE)
heatmap(as.matrix(A.adj1),symm = TRUE,Rowv=NA, Colv=NA, col=coul,
        labRow=FALSE, labCol=FALSE, revC=TRUE)

#compute two graph embeddings of P:
P_eigen <- irlba(P,3)
U <- P_eigen$u #this grabs just the eigenvectors
dim(U) # why is it this dimension?
X <- P_eigen$u %*% diag(P_eigen$d^(1/2))
dim(X)

#compute two actual graph embeddings for simulated A:
A_eigen <- irlba(A.adj1,3)
Uhat <- A_eigen$u
Xhat <- A_eigen$u %*% diag(A_eigen$d^(1/2))

# we will now plot the first embedding, but first let's make it nicer:
dat <- data.frame(Xhat)
names(dat) <- c("dim1","dim2","dim3")
#add community labels:
dat$community <- "3"
dat$community[c(1:block.sizes1[1])] <- "1"
dat$community[c((block.sizes1[1]+1):(block.sizes1[1]+block.sizes1[2]))] <- "2"
#use ggplot2 to plot the data:
plt <- ggplot(dat,aes(x= dim1, y= dim2,color=community)) + geom_point()
plt
#plot dimensions 2 and 3
plt <- ggplot(dat,aes(x= dim2, y= dim3,color=community)) + geom_point()
plt

#--------------------------------------------------
# now we do this with a degree-corrected stochastic blockmodel:
n <- 1000
B <- matrix(c(.7,.4,.4,.6),2,2)
pis <- c(1/2,1/2)
block.sizes1 <-  rowSums(rmultinom(n,1,pis))
block.sizes1
Z <- cbind(c(rep(1,block.sizes1[1]),rep(0,block.sizes1[2])),
           c(rep(0,block.sizes1[1]),rep(1,block.sizes1[2]))
)
#degree correction parameters:
Theta <- runif(n,.2,.9)
#create probability matrix:
P <- diag(Theta)%*% Z %*% B %*% t(Z) %*% diag(Theta)
coul <- colorRampPalette(brewer.pal(6, "Accent"))(50)
heatmap(P,symm = TRUE,Rowv=NA, Colv=NA, col=coul,
        labRow=FALSE, labCol=FALSE, revC=TRUE)

#sample A:
A <- matrix(0,n,n)
for (i in c(1:n)) {
  for (j in c(1:i)) {
    A[i,j] <- rbinom(1,1,P[i,j])
    A[j,i] <- A[i,j]
  }
}
#remove self-loops:
diag(A) <- 0
heatmap(as.matrix(A),symm = TRUE,Rowv=NA, Colv=NA, col=coul,
        labRow=FALSE, labCol=FALSE, revC=TRUE)

A_eigen <- irlba(A,2)
Xhat <- A_eigen$u %*% diag(A_eigen$d^(1/2))
dat <- data.frame(Xhat)
names(dat) <- c("dim1","dim2")
dat$community <- "2"
dat$community[c(1:block.sizes1[1])] <- "1"
plt <- ggplot(dat,aes(x= dim1, y= dim2,color=community)) + geom_point()
plt

#if you got to here, go ahead to the previous R script community_detection.R beginning at line 320 and see if you
#understand what it's doing


