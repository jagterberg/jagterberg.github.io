#first, load the igraph library
library(igraph)

#now we will load a graph dataset
hpnames <- read.csv("https://jagterberg.github.io/assets/HEART2022/harrypotter/characters.csv")
hpgraph <- read.csv("https://jagterberg.github.io/assets/HEART2022/harrypotter/relations.csv")

#view each of these
View(hpgraph)
View(hpnames)

#subset to enemies 
hpgraph <- hpgraph[which(hpgraph$type == "-"),]

#now fill this in: we want to use the graph_from_data_frame function in 
#igraph to get an adjacency matrix 
A <- graph

#subset to largest connected component
components <- igraph::clusters(A, mode="weak")
biggest_cluster_id <- which.max(components$csize)
vert_ids <- V(A)[components$membership == biggest_cluster_id]
vert_ids

#fill this in: we want to use the induced_subgraph function to get the 
#induced subgraph from A associated to vert_ids
#A <-
  
#associate the names to each vertex ID
V(A)$charactername <- hpnames[match(names(V(A)),as.character(hpnames$id)),"name"]

#now we want to get the adjacency matrix from the graph 
#adj.mat <- 
View(as.matrix(as_adjacency_matrix(A)))
rownames(adj.mat) <- V(A)$charactername
colnames(adj.mat) <- V(A)$charactername
View(as.matrix(adj.mat))

# for you to attempt:
#1) a histogram of the degrees
#2) Average degree
#3) Edge Density
#4) calculate the Laplacian matrix
#5) calculate the normalized Laplacian matrix
#6) rerun it all again without weighted edges






