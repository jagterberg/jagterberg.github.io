library(igraph)
library(ggplot2)
library(mclust)
#------------------------------------
#getelbows function: minimize this!
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
#--------------------------------------
#process data
nodes <- read.delim("https://jagterberg.github.io/assets/HEART2022/fao/fao_trade_nodes.txt",sep = "")
layers <- read.delim("https://jagterberg.github.io/assets/HEART2022/fao/fao_trade_layers.txt",sep="")
edges <- read.table("https://jagterberg.github.io/assets/HEART2022/fao/fao_trade_multiplex.edges", sep="")

#now we run through each trade dataset and update our embedding:
all_Graphs <- list()
edges$weight <- edges$V4

for ( i in c(1:nrow(layers))) {
  layer_dat <- edges[which(edges$V1 == i),c("V2","V3","weight")]
  G <- graph_from_data_frame(layer_dat,vertices= nodes)
  G <-  as.undirected(G,mode="collapse")
  #G <- delete.edges(G,which(E(G)$weight <= 8))
  all_Graphs[[i]] <- G
}

nLayers <- nrow(layers)
nLayers
length(V(G))
toKeep <- list()
layers_kept <- c()
j <- 1
for (i in c(1:nLayers)) {
  comps <- igraph::clusters(all_Graphs[[i]], mode="weak")
  if (max(comps$csize) >= 150) {
    toKeep[[j]] <- all_Graphs[[i]]
    layers_kept <- c(layers_kept,i)
    j <- j+1
  }
}
nLayers <- length(toKeep)
nLayers
indsToKeep <- nodes$nodeID
 for (i in c(1:nLayers)) {
   comps <- igraph::clusters(toKeep[[i]], mode="weak")
   whichtoKeep <- which(comps$csize ==max(comps$csize))
   vert_ids <- V(toKeep[[i]])[comps$membership == whichtoKeep]
#   vert_ids
   indsToKeep <- indsToKeep[indsToKeep %in%vert_ids]
 }

for (i in c(1:nLayers)) {
  toKeep[[i]] <-  igraph::induced_subgraph(toKeep[[i]], indsToKeep)
}

nV <- length(V(toKeep[[1]]))
nLayers <- length(toKeep)

tens <- array(0,dim=c(nLayers,nV,nV))
As <- toKeep

#for ( i in c(1:nLayers)) {
  #A <- as_adjacency_matrix(toKeep[[i]],attr="weight")
  #rownames(A) <- V(toKeep[[i]])$nodeLabel
  #colnames(A) <- V(toKeep[[i]])$nodeLabel
  #As[[i]] <- A
  #A <- A[order(rownames(A)),]
  #A <- A[,order(colnames(A))]
  #tens[i,,] <- as.matrix(A)
  #As[[i]] <- tens[i,]
#}


#######################################################
#season parameter governs which season to analyze
#################
season <- 1
layers$layerLabel[season]
seasonadj <- as_adjacency_matrix(As[[season]],attr='weight')

#As[[i]] <- A
#uncomment for unweighted graph:
#seasonadj <- as_adjacency_matrix(As[[season]])
rownames(seasonadj) <- V(toKeep[[i]])$nodeLabel
colnames(seasonadj) <- V(toKeep[[i]])$nodeLabel

#embedding
vals <- svd(seasonadj)
getElbows(vals$d) 
K <- 5 #can change this
embedding <- vals$u[,c(1:K)]
normalize <- TRUE #set to false if no normalization
if(normalize) {
  for (i  in c(1:nrow(embedding))) {
    val <- sum(embedding[i,]^2)
    embedding[i,] <- embedding[i,]/sqrt(val)
  }
}

clusts <- Mclust(embedding) #G = 10 argument specifies the # of clusters
#can also run
#clusts <- kmeans(embedding,10)  #need to specify the # of clusters
clust <- clusts$classification
#if you use kmeans, use
#clust <- clusts$cluster

rownames(seasonadj)[which(clust == 1)]
rownames(seasonadj)[which(clust == 2)]
rownames(seasonadj)[which(clust == 3)]
rownames(seasonadj)[which(clust == 4)]
rownames(seasonadj)[which(clust == 5)]
rownames(seasonadj)[which(clust == 6)]
rownames(seasonadj)[which(clust == 7)]
rownames(seasonadj)[which(clust == 8)]
rownames(seasonadj)[which(clust == 9)]
rownames(seasonadj)[which(clust == 10)]



print("STOP! Now repeat for seasons 1-5 by changing the season argument above!")

#########################################
#multilayer analysis
####################################
#list of different embedding dimensions
weighted = TRUE

Ks <- rep(12,length(As))
#we now embed each matrix
#choose 3 arbitrarily
Xhats <- list()
normalize= FALSE #set to true if you think DCSBM
for (i in c(1:length(As))) {
  if (weighted) {
    A <- as_adjacency_matrix(As[[i]],attr= 'weight')
  } else {
    A <- as_adjacency_matrix(As[[i]])
  }
  rownames(A) <- V(toKeep[[i]])$nodeLabel
  colnames(A) <- V(toKeep[[i]])$nodeLabel
  
  A <- A[order(rownames(A)),]
  A <- A[,order(colnames(A))]
  
  Xhats[[i]] <- irlba::irlba(A,Ks[i])$u
  if(normalize) {
    for (j in c(1:nrow(Xhats[[i]]))) {
      val <- sqrt(sum(Xhats[[i]][j,]^2))
      if (val <- .0000000001) {
        Xhats[[i]][j,] <- 1
      } else {
        Xhats[[i]][j,] <- Xhats[[i]][j,]/val
      }
    }
  }
  
}

Yhat <- Xhats[[1]]
for ( i in c(2:length(As))) {
  Yhat <- cbind(Yhat,Xhats[[i]])
}

getElbows(svd(Yhat)$d)
Uhat <- irlba::irlba(Yhat,31)$u

dat <- as.data.frame(Uhat)
dat$names <- rownames(A)
library(ggplot2)
ggplot(dat, aes(x= V1,y=V2,  label=names))+
  geom_point(size=.5) +
  geom_text(hjust=1, vjust=-1,size=2,angle=45)

clusts <- Mclust(Uhat)
clust = clusts$classification
dat$names[which(clust == 1)]
dat$names[which(clust == 2)]
dat$names[which(clust == 3)]



