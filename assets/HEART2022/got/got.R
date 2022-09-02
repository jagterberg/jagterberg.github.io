library(igraph)

for (i in c(1:5)) {
  eval(parse(text=paste0("gotname",i,"<-read.csv('got-s",i,"-nodes.csv')")))
  eval(parse(text=paste0("gotgraph",i,"<-read.csv('got-s",i,"-edges.csv')")))
}

#subset to LCC in each graph
for ( i in c(1:5)) {
  eval(parse(text=paste0("A",i,"<-graph_from_data_frame(gotgraph",i,",directed=FALSE)")))
}

As <- list()
for ( i in c(1:5)) {
  eval(parse(text=paste0("As[[",i,"]] <- A",i)))
}

for (i in c(1:5)) {
  components <- igraph::clusters(As[[i]], mode="weak")
  biggest_cluster_id <- which.max(components$csize)
  vert_ids <- V(As[[i]])[components$membership == biggest_cluster_id]
  As[[i]] <- igraph::induced_subgraph(As[[i]], vert_ids)
}

verticestokeep <- names(V(As[[1]]))
for (i in c(2:5)) {
  verticestokeep <- intersect(verticestokeep,names(V(As[[i]])))
}

for ( i in c(1:5)) {
  As[[i]] <- igraph::induced_subgraph(As[[i]], verticestokeep)
}

#we now embed each matrix
#choose 3 arbitrarily
Xhats <- list()
for (i in c(1:5)) {
  A <- as_adjacency_matrix(As[[i]])
  Xhats[[i]] <- irlba::irlba(A,3)
  Xhats[[i]] <- Xhats[[i]]$u %*% diag(Xhats[[i]]$d^(1/2))
  for (j in c(1:nrow(Xhats[[i]]))) {
    val <- sqrt(sum(Xhats[[i]][j,]^2))
    if (val <- .0000000001) {
      Xhats[[i]][j,] <- 1
    } else {
      Xhats[[i]][j,] <- Xhats[[i]][j,]/val
    }
  }
}

Yhat <- Xhats[[1]]
for ( i in c(2:5)) {
  Yhat <- cbind(Yhat,Xhats[[i]])
}

Uhat <- irlba::irlba(Yhat,3)$u
plot(Uhat[,2],Uhat[,3])
