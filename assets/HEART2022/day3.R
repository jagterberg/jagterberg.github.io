#First, we generate n observations of a bernoulli random 
#variable with parameter p:
rm(list=ls())
n <- 1000
p <- .2
#fill this in:
# we want dat to be n observations of random bernoulli variables
# with parameter ps
dat <- rbinom( n,1,p )
summary(dat)

#to visualize the CLT, we need to do this lots of times
n <- 10
p <- .2
ntimes <- 1000
xbars <- rep(0,ntimes)
for (i in c(1:ntimes)) {
  #fill this in:
  dat <- rbinom(n,1,p)
  
  #fill this in: we want to store the mean of dat
  xbars[i] <- mean(dat)
}
hist(xbars)

#now what happens if n changes? (rerun with different values of n)

#---------------------------------
#we will see if we can make this more explicit.
# we will do this for several different values of n
rm(list= ls())

#using the seq function, generate a sequence of n's from 40 to 200 by 40
ns <- seq(40,200,40)
p <- .2

#the variance of the binomial
binom_var <- p*(1-p)

#the number of times we do this
ntimes <- 1000

#we will save the data for each different time
clt_demonstration <- c()
nsrepeated <- c()
for (n in ns) {
  #we will store each output in a vector called temp
  # we need to i nitialize with zeros
  #fill this in:
  temp <- rep(0,ntimes)
  for (i in c(1:ntimes)) {
    xbar <- mean(rbinom(n,1,p))
    
    #from CLT formula:
    nearlygaussian <- sqrt(n)*(xbar  - p)/sqrt(binom_var)
    
    #we now store nearlygaussian as the i'th element of the temp vector
    #fill in:
    temp[i] <- nearlygaussian
  }
  
  #we have done this ntimes for this value of n, so we store it
  clt_demonstration <- c(clt_demonstration, temp)
  nsrepeated <- c(nsrepeated,rep(n,ntimes))
}

# we now store it all as a data frame
dat <- data.frame(x = clt_demonstration,n=as.factor(nsrepeated))

#you may need:
#install.packages(ggplot2)
library(ggplot2) 
g <- ggplot(dat,aes(x=x)) + 
  geom_histogram(bins=30,alpha=.8,aes(color=n,y=..density..,fill=n)) +
  stat_function(fun=dnorm, args= list(mean=0,sd=1),lwd=1.1,linetype="dashed") +
  facet_wrap(~n)
g


#-----------------------------------
#Erdos-Renyi Graph Example
#-------------------------------------
rm(list=ls())
#first we generate one adjacency matrix from E-R graph
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

#now we want to do this lots of times and look at the CLT
binom_var <- p*(1-p)
ns <- c(100,200)
ntimes <- 100
toPlot <- c()
nsrepeated <- c()
for (n in ns) {
  temp <- rep(0,ntimes)
  for (reps in c(1:ntimes)) {
    
    A <- generate_adj_matrix_ER(p,n)
    phat <- mean(A[upper.tri(A)])
    temp[reps] <-  phat
    
  }
  
  toPlot <- c(toPlot, temp)
  nsrepeated <- c(nsrepeated,rep(n,ntimes))
  
  
}


dat <- data.frame(x = toPlot,n=as.factor(nsrepeated))

#you may need:
#install.packages(ggplot2)
library(ggplot2) 
g <- ggplot(dat,aes(x=x)) + geom_histogram(bins=30,alpha=.8,aes(color=n,y=..density..,fill=n)) +
  stat_function(fun=dnorm, args= list(mean=p,sd=sqrt(binom_var/(n*(n-1)/2))),lwd=1.1,linetype="dashed") +
  facet_wrap(~n)
g


#----------------------------------
#SBM Example: only if there is time, otherwise we do it next week
#----------------------------------
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

n <- 100
B <- matrix(c(.8,.6,.6,.8),2,2)
B
memberships <- c(rep(1,n/2),rep(2,n/2))
A <- generate_adj_matrix_SBM(B,n,memberships)
heatmap(A,symm = TRUE,Rowv=NA, Colv=NA,labRow=FALSE, labCol=FALSE, revC=TRUE)

#what happens if you change B (need it to be symmetric still!)
#now modify to allow self-loops!




