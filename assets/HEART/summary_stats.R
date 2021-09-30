library(igraphdata)
library(igraph)
library(Matrix)
library(irlba)
library(mclust)




#--------------------
#Karate club dataset
data("karate")
dat <- as_adjacency_matrix(karate)

#here we obtain degrees by multiplying by the ones vector
degrees <- dat %*% rep(1,nrow(dat))
hist(as.vector(degrees))

#here we obtain degrees using the built-in function
hist(degree(karate))

#we can also look at triangles:
hist(count_triangles(karate))

#enron email dataset
data("enron")
dat <- as_adjacency_matrix(enron)
degrees <- dat %*% rep(1,nrow(dat))
hist(as.vector(degrees))
#using built in function:
hist(degree(enron))
hist(count_triangles(enron))
edge_density(enron)


#-------------explore enron data a bit---------------------
?enron
vertex_attr_names(enron)
edge_attr_names(enron)
graph_attr_names(enron)
vertex_attr(enron,'Name', index = c(1))
vertex_attr(enron,'Name', index = c(2))
vertex_attr(enron,'Email', index = c(1,2))
vertex_attr(enron,'Note', index = c(1,2))
edge_attr(enron,'Time', index = 1:3)
edge_attr(enron,'Topic', index = 1:3)
edge_attr(enron,'LDC_topic', index = 1:3)
edge_attr(enron,'Reciptype', index = 1:3)
graph_attr(enron,'LDC_names')

# explore embedded enron data
library(irlba)
dat <-as_adjacency_matrix(enron)
eig_data <- irlba(dat,30)
d <- getElbows(eig_data$d)[2]
Xhat <- cbind(eig_data$u[,c(1:d)]%*% diag(eig_data$d[c(1:d)]),eig_data$v[,c(1:d)]%*% diag(eig_data$d[c(1:d)]))
Xhat
plot(Xhat) #there are two outliers

#first reduce to LCC:
components <- igraph::clusters(enron, mode="weak")
biggest_cluster_id <- which.max(components$csize)

# ids
vert_ids <- V(enron)[components$membership == biggest_cluster_id]

# subgraph
dat_new <- igraph::induced_subgraph(enron, vert_ids)
eig_data <- irlba(as_adjacency_matrix(dat_new),30)
d <- getElbows(eig_data$d)[2]
Xhat <- cbind(eig_data$u[,c(1:d)]%*% diag(eig_data$d[c(1:d)]),eig_data$v[,c(1:d)]%*% diag(eig_data$d[c(1:d)]))
plot(Xhat) #there are two outliers
ind1 <- which(Xhat[,1] < -9000)
vertex_attr(dat_new,'Name', index = c(ind1))

ind2 <- which(Xhat[,2] < -5000)
vertex_attr(dat_new,'Name', index = c(ind2))

g <- delete_vertices(dat_new,c(ind1,ind2))
eig_data <- irlba(as_adjacency_matrix(g),30)
d <- getElbows(eig_data$d)[2]
Xhat <- cbind(eig_data$u[,c(1:d)]%*% diag(eig_data$d[c(1:d)]),eig_data$v[,c(1:d)]%*% diag(eig_data$d[c(1:d)]))
plot(Xhat) #not as many outliers

