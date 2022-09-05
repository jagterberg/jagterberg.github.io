library(igraph)
hpnames <- read.csv("characters.csv")
hpgraph <- read.csv("relations.csv")

#subset to enemies 
hpgraph <- hpgraph[which(hpgraph$type == "-"),]
A <- as.undirected(graph_from_data_frame(hpgraph))
components <- igraph::clusters(A, mode="weak")
biggest_cluster_id <- which.max(components$csize)

# ids
vert_ids <- V(A)[components$membership == biggest_cluster_id]

# subgraph
A <- igraph::induced_subgraph(A, vert_ids)
#A2 <- induced.subgraph(A,largest.independent.vertex.sets(A)[[1]])
V(A)$char <- hpnames[match(names(V(A)),as.character(hpnames$id)),"name"]
Adj <- as_adjacency_matrix(A)
svdA <- svd(Adj)
#plot(svdA$d) #take d = 2
Xhat <- svdA$u[,c(1:2)] # %*% diag(svdA$d[c(1:2)]^(1/2))
#plot(Xhat)
Xhat_new <- Xhat
for(i in c(1:nrow(Xhat_new))) {
  Xhat_new[i,] <- Xhat[i,]/sqrt(sum(Xhat[i,]^2))
}

dat <- as.data.frame(Xhat_new)
dat$names <- V(A)$char

dat2 <- as.data.frame(Xhat)
dat2$names <- V(A)$char

library(ggplot2)
ggplot(dat2, aes(x= V1,y=V2,  label=names))+
  geom_point(size=.5) +
  geom_text(hjust=1, vjust=-1,size=2,angle=45)

ggplot(dat, aes(x= V1,y=V2,  label=names))+
  geom_point(size=.5) +
  geom_text(hjust=1, vjust=-1,size=2,angle=45)
dat[which(dat$V2 < 0),'names']
dat[which(dat$V2 > 0),'names']

