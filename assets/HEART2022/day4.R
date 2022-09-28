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

#now we want to do this lots of times and look at the CLT
binom_var <- p*(1-p)
ns <- seq(100,400,100)
ntimes <- 100
toPlot <- c()
nsrepeated <- c()
for (n in ns) {
  temp <- rep(0,ntimes)
  for (reps in c(1:ntimes)) {
    A <- generate_adj_matrix_ER(p,n)
    phat <- mean(A[upper.tri(A)])
    temp[reps] <- sqrt(n*(n-1)/2) * (phat - p)/sqrt(binom_var)
  }
  toPlot <- c(toPlot, temp)
  nsrepeated <- c(nsrepeated,rep(n,ntimes))
}

dat <- data.frame(x = toPlot,n=as.factor(nsrepeated))

library(ggplot2) 
g <- ggplot(dat,aes(x=x)) + geom_histogram(bins=30,alpha=.8,aes(color=n,y=..density..,fill=n)) +
  stat_function(fun=dnorm, args= list(mean=0,sd=1),lwd=1.1,linetype="dashed") +
  facet_wrap(~n)
g

#----------------------------------
#SBM Example
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
a <- .99
b <- .03
c <- sqrt(.7)
d <- sqrt(2)/2
e <- exp(1)/3
B <- matrix(c(
  a,b,c,
  b,d,a,
  c,a,e
),3,3)
B
c1 <- n/2
c2 <- floor(n/exp(1))
c3 <- n - c1 - c2
memberships <- c(rep(1,c1),rep(2,c2),rep(3,c3))
A <- generate_adj_matrix_SBM(B,n,memberships)
heatmap(A,symm = TRUE,Rowv=NA, Colv=NA,labRow=FALSE, labCol=FALSE, revC=TRUE)

#what happens if you change a and b?

#how would we modify this to allow for three communities?

#in practice, we do not observe all the vertices
# sorted by their communities.
p1 <- as(sample(n),"pMatrix")
A_perm <- p1 %*% A %*% t(p1)
heatmap(A_perm,symm = TRUE,Rowv=NA, Colv=NA,labRow=FALSE, labCol=FALSE, revC=TRUE)


#-----------------------------------------
#SBM vs ER probability matrices
#----------------------------------------
#let's look at the probability matrix for an  ER graph:
generate_prob_matrix_SBM <- function(B,n,memberships) {
  P <- matrix(0,n,n)
  #generate A:
  for(i in c(1:n)) {
    for(j in c(1:(i-1))) {
      prob <- B[memberships[i],memberships[j]]
      P[i,j] <- prob
      P[j,i] <- P[i,j]
    }
  }
  return(P)
}

n <- 100
a <- .8
b <- .4
c <- .8
B <- matrix(c(a,b,b,c),2,2)
B
memberships <- c(rep(1,n/2),rep(2,n/2))
P <- generate_prob_matrix_SBM(B,n,memberships)
heatmap(P,symm = TRUE,Rowv=NA, Colv=NA,labRow=FALSE, labCol=FALSE, revC=TRUE)

#ER graph:
p <- .2
P <- matrix(p,n,n)
diag(P) <- 0
heatmap(P,symm = TRUE,Rowv=NA, Colv=NA,labRow=FALSE, labCol=FALSE, revC=TRUE)

##########################################
#DCSBM
#################################
rm(list=ls())
#first we generate one adjacency matrix from E-R graph
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

n <- 100
a <- 1
b <- .5
c <- 1
B <- matrix(c(a,b,b,c),2,2)
B
degreecorrections <- runif(n,min=0,max=1)
memberships <- c(rep(1,n/2),rep(2,n/2))
A <- generate_adj_matrix_DCSBM(B,n,memberships,degreecorrections)
heatmap(A,symm = TRUE,Rowv=NA, Colv=NA,labRow=FALSE, labCol=FALSE, revC=TRUE)

#what happens if you change a,b, and c?

#what happens if you change degreecorrections?

#in practice, we do not observe all the vertices
# sorted by their communities.
p1 <- as(sample(n),"pMatrix")
A_perm <- p1 %*% A %*% t(p1)
heatmap(A_perm,symm = TRUE,Rowv=NA, Colv=NA,labRow=FALSE, labCol=FALSE, revC=TRUE)

#let's find the probability matrix:
generate_prob_matrix_DCSBM <- function(B,n,memberships,degreecorrections) {
  P <- matrix(0,n,n)
  #generate A:
  for(i in c(1:n)) {
    for(j in c(1:(i-1))) {
      prob <- degreecorrections[i]*degreecorrections[j]*B[memberships[i],memberships[j]]
      P[i,j] <- prob
      P[j,i] <- P[i,j]
    }
  }
  return(P)
}

P <- generate_prob_matrix_DCSBM(B,n,memberships,degreecorrections)
heatmap(P,symm = TRUE,Rowv=NA, Colv=NA,labRow=FALSE, labCol=FALSE, revC=TRUE)



