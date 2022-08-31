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
football <- read_graph("football.gml",format= "gml")
summary(football)
labs <- vertex.attributes(football)
labs

#now let's choose d:
football_eigen <- svd(as_adj(football)) #n = 115, so we can do a full SVD here
elbows <- getElbows(football_eigen$d)
elbows #first is 11 but there are 11 different conferences
d <- elbows[1] 

Xhat <- football_eigen$u[,c(1:d)] %*% diag(sqrt(football_eigen$d[c(1:d)]))
dim(Xhat)

clusts <- Mclust(Xhat,G=c(10:20)) #cluster with between 10 and 20 components
summary(clusts)
adjustedRandIndex(clusts$classification,labs$value) #not bad!

dat <- data.frame(labs$label)
names(dat) <- "college"
dat$truelab <- as.factor(as.numeric(labs$value) + 1)
dat$mclustlab <- as.factor(as.numeric(clusts$classification))

dat[which(dat$truelab == 1),c("mclustlab","college")]
dat[which(dat$truelab == 2),c("mclustlab","college")]
dat[which(dat$truelab == 3),c("mclustlab","college")]
dat[which(dat$truelab == 4),c("mclustlab","college")]
dat[which(dat$truelab == 5),c("mclustlab","college")]
dat[which(dat$truelab == 6),c("mclustlab","college")]
dat[which(dat$truelab == 7),c("mclustlab","college")]
dat[which(dat$truelab == 8),c("mclustlab","college")]
dat[which(dat$truelab == 9),c("mclustlab","college")]
dat[which(dat$truelab == 10),c("mclustlab","college")]
dat[which(dat$truelab == 11),c("mclustlab","college")]
dat[which(dat$truelab == 12),c("mclustlab","college")]

#-------------------------------
polblogs <- read_graph("polblogs.gml",format="gml")

polblogs <- simplify(polblogs)
labs <- vertex.attributes(polblogs)
labs$value
is.directed(polblogs)
polblogs_eigen <-irlba(as_adj(polblogs),200) #this may take a second
elbows <- getElbows(polblogs_eigen$d)
elbows #only liberal or republican, so d = 2 seems reasonable
d <- elbows[1] 

Xhat <- polblogs_eigen$u[,c(1:d)] %*% diag(sqrt(polblogs_eigen$d[c(1:d)]))
dim(Xhat)

clusts <- Mclust(Xhat) #cluster it!
summary(clusts)
adjustedRandIndex(clusts$classification,labs$value) #kind of bad... 

clusts <- Mclust(Xhat,G = 2) #cluster it!
summary(clusts)
adjustedRandIndex(clusts$classification,labs$value) #improves a bit here.

#try with bigger d:
d <- elbows[2]
Xhat <- polblogs_eigen$u[,c(1:d)] %*% diag(sqrt(polblogs_eigen$d[c(1:d)]))
dim(Xhat)

clusts <- Mclust(Xhat,G=2) #this might take a while
summary(clusts)
adjustedRandIndex(clusts$classification,labs$value) #much better

#can do faster with k-means:
clusts_kmeans <- kmeans(Xhat,2)
summary(as.factor(clusts_kmeans$cluster))
adjustedRandIndex(clusts_kmeans$cluster,labs$value) #this is now much worse.



