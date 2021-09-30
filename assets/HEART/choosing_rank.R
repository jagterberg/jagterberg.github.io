n <- 1000
p <- .5

A <- matrix(0,n,n)
#generate A:
for(i in c(1:n)) {
  for(j in c(1:(i-1))) {
    A[i,j] <- rbinom(1,1,p)
    A[j,i] <- A[i,j]
  }
}


library(irlba)
A_eigen <- irlba(A,100)
plot(A_eigen$d)


library(igraphdata)
data("enron")
dat <-as_adjacency_matrix(enron)
eig_data <- irlba(dat,100)
plot(eig_data$d)
rm(list= ls())


data("UKfaculty")
dat <-as_adjacency_matrix(UKfaculty)
eig_data <- irlba(dat,30)
plot(eig_data$d)
rm(list= ls())


data("USairports")
dat <-as_adjacency_matrix(USairports)
eig_data <- irlba(dat,100)
plot(eig_data$d)
rm(list=ls())

data("immuno")
dat <-as_adjacency_matrix(immuno)
eig_data <- irlba(dat,100)
plot(eig_data$d)
rm(list=ls())



#------------------------------------
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

data("enron")
dat <-as_adjacency_matrix(enron)
eig_data <- irlba(dat,100)
getElbows(eig_data$d)

data("UKfaculty")
dat <-as_adjacency_matrix(UKfaculty)
eig_data <- irlba(dat,30)
getElbows(eig_data$d)



data("USairports")
dat <-as_adjacency_matrix(USairports)
eig_data <- irlba(dat,100)
getElbows(eig_data$d)


data("immuno")
dat <-as_adjacency_matrix(immuno)
eig_data <- irlba(dat,100)
getElbows(eig_data$d)


#-------------------------------------



