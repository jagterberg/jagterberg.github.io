#CLT Example
#---------------------------------------
#we will be generating nMC samples of size n = 100 and n = 1000 to see the effect of the CLT
ns <- seq(500,2000,500)
p <- .4
binom_var <- p*(1-p)
nMC <- 1000


#we will save the data for each different time
clt_demonstration <- c()
ns2 <- c()
for (n in ns) {
  temp <- rep(0,nMC)
  for (i in c(1:nMC)) {
    temp[i] <- sqrt(n)*(mean(rbinom(n,1,p)) - p)/sqrt(binom_var)
  }
  clt_demonstration <- c(clt_demonstration, temp)
  ns2 <- c(ns2,rep(n,nMC))
}



dat <- data.frame(x = clt_demonstration,n=as.factor(ns2))

#you may need:
#install.packages(ggplot2)
library(ggplot2) 
g <- ggplot(dat,aes(x=x)) + geom_histogram(bins=30,alpha=.8,aes(color=n,y=..density..,fill=n)) +
  stat_function(fun=dnorm, args= list(mean=0,sd=1),lwd=1.1,linetype="dashed")
g

#ER Example
#-------------------------------------
p <- .4
ns <- c(100,200)
nMC <- 100
toPlot <- c()
ns2 <- c()
for (n in ns) {
  temp <- rep(0,nMC)
  for (reps in c(1:nMC)) {
    A <- matrix(0,n,n)
    #generate A:
    for(i in c(1:n)) {
      for(j in c(1:(i-1))) {
        A[i,j] <- rbinom(1,1,p)
        A[j,i] <- A[i,j]
      }
    }
    
    phat <- mean(A[upper.tri(A)])
    temp[reps] <- sqrt(n*(n-1)/2) * (phat - p)/sqrt(binom_var)
    
  }
  
  toPlot <- c(toPlot, temp)
  ns2 <- c(ns2,rep(n,nMC))
  
  
}


dat <- data.frame(x = toPlot,n=as.factor(ns2))

#you may need:
#install.packages(ggplot2)
library(ggplot2) 
g <- ggplot(dat,aes(x=x)) + geom_histogram(bins=30,alpha=.8,aes(color=n,y=..density..,fill=n)) +
  stat_function(fun=dnorm, args= list(mean=0,sd=1),lwd=1.1,linetype="dashed")
g
  
  
#----- eigenvectors CLT ----------
ns <- c(500,750,1000)
toPlot<- c()
ns2 <- c()
for (n in ns) {
  A <- matrix(0,n,n)
  #generate A:
  for(i in c(1:n)) {
    for(j in c(1:(i-1))) {
      A[i,j] <- rbinom(1,1,p)
      A[j,i] <- A[i,j]
    }
  }
  
  eig_vecs <- irlba(A,1)
  eig_vecs <- eig_vecs$u  
  #account for scaling:
  eig_vecs <- eig_vecs*sqrt(n)
  #account for sign:
  eig_vecs <- abs(eig_vecs) - 1
  toPlot <- c(toPlot, eig_vecs)
  ns2 <- c(ns2,rep(n,n))
  
}

dat <- data.frame(x= toPlot,n=as.factor(ns2))
g <- ggplot(dat,aes(x=x))  + geom_histogram(bins=30,alpha=.8,aes(color=n,y=..density..,fill=n)) 
g
