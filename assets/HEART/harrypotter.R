#load libraries:
library(igraph)
library(irlba)
library(Matrix)
library(igraphdata)
library(mclust)
library(RColorBrewer)
library(ggplot2)
library(devtools)
library(jsonlite)

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
#get and clean data
hp1 <- as.matrix(read.table("https://raw.githubusercontent.com/dravesb/OmniAnalysis/master/analysis/Harry_Potter/adj_matrices/Philosophers_Stone_adj_matrix"))
hp2 <- as.matrix(read.table("https://raw.githubusercontent.com/dravesb/OmniAnalysis/master/analysis/Harry_Potter/adj_matrices/Chamber_of_Secrets_adj_matrix"))
hp3 <- as.matrix(read.table("https://raw.githubusercontent.com/dravesb/OmniAnalysis/master/analysis/Harry_Potter/adj_matrices/Prisoner_of_Azkaban_adj_matrix"))
hp4 <- as.matrix(read.table("https://raw.githubusercontent.com/dravesb/OmniAnalysis/master/analysis/Harry_Potter/adj_matrices/Goblet_of_Fire_adj_matrix"))
hp5 <- as.matrix(read.table("https://raw.githubusercontent.com/dravesb/OmniAnalysis/master/analysis/Harry_Potter/adj_matrices/Order_of_the_Phoenix_adj_matrix"))
hp6 <- as.matrix(read.table("https://raw.githubusercontent.com/dravesb/OmniAnalysis/master/analysis/Harry_Potter/adj_matrices/Half_Blood_Prince_adj_matrix"))
hp7 <- as.matrix(read.table("https://raw.githubusercontent.com/dravesb/OmniAnalysis/master/analysis/Harry_Potter/adj_matrices/Deathly_Hallows_adj_matrix"))

chars <- c('Aberforth Dumbledore', 'Alastor (Mad-Eye) Moody', 'Albert Runcorn', 'Albus Dumbledore', 'Albus Severus Potter', 'Alecto Carrow', 'Alice Longbottom', 'Alicia Spinnet', 'Amelia Bones', 'Amos Diggory', 'Amycus Carrow', 'Andromeda Tonks', 'Angelina Johnson', 'Anthony Goldstein', 'Antioch Peverell', 'Antonin Dolohov', 'Arabella Figg', 'Aragog', 'Argus Filch', 'Ariana Dumbledore', 'Arthur Weasley', 'Augusta Longbottom', 'Augustus Rookwood', 'Aurora Sinistra', 'Bane', 'Barty Crouch Jr', 'Barty Crouch Sr', 'Bathilda Bagshot', 'Beedle the Bard', 'Bellatrix Lestrange', 'Bertha Jorkins', 'Bill Weasley', 'Blaise Zabini', 'Bob Ogden', 'Bogrod', 'Buckbeak', 'Cadmus Peverell', 'Cedric Diggory', 'Charity Burbage', 'Charlie Weasley', 'Cho Chang', 'Colin Creevey', 'Corban Yaxley', 'Cormac McLaggen', 'Cornelius Fudge', 'Crabbe', 'Crookshanks', 'Cuthbert Binns', 'Dean Thomas', 'Dedalus Diggle', 'Demelza Robins', 'Dennis Creevey', 'Dirk Cresswell', 'Dobby', 'Dolores Umbridge', 'Draco Malfoy', 'Dudley Dursley', 'Eloise Midgen', 'Elphias Doge', 'Emmeline Vance', 'Ernie Macmillan', 'Errol', 'Fang', 'Fawkes', 'Fenrir Greyback', 'Filius Flitwick', 'Firenze', 'Fleur Delacour', 'Florean Fortescue', 'Fluffy', 'Frank Bryce', 'Frank Longbottom', 'Fred Weasley', 'Gabrielle Delacour', 'Garrick Ollivander', 'Gellert Grindelwald', 'George Weasley', 'Gilderoy Lockhart', 'Ginny Weasley', 'Godric Gryffindor', 'Gornuk', 'Goyle', 'Graham Montague', 'Grawp', 'Great Aunt Muriel', 'Gregorovitch', 'Gregory Goyle', 'Griphook', 'Griselda Marchbanks', 'Hannah Abbott', 'Harry Potter', 'Hedwig', 'Helena Ravenclaw', 'Helga Hufflepuff', 'Hermione Granger', 'Hokey', 'Horace Slughorn', 'Hugo Weasley', 'Ignotus Peverell', 'Igor Karkaroff', 'Irma Pince', 'James Potter', 'James Sirius Potter', 'Justin Finch-Fletchley', 'Katie Bell', 'Kendra Dumbledore', 'Kingsley Shacklebolt', 'Kreacher', 'Lavender Brown', 'Lee Jordan', 'Lily Luna Potter', 'Lily Potter', 'Lord Voldemort', 'Lucius Malfoy', 'Ludo Bagman', 'Luna Lovegood', 'Madam Malkin', 'Madam Rosmerta', 'Mafalda Hopkirk', 'Magorian', 'Marcus Flint', 'Marge Dursley', 'Marietta Edgecombe', 'Marvolo Gaunt', 'Mary Cattermole', 'Mary Riddle', 'Merope Gaunt', 'Michael Corner', 'Millicent Bulstrode', 'Minerva McGonagall', 'Molly Weasley', 'Morfin Gaunt', 'Mrs. Cole', 'Mundungus Fletcher', 'Myrtle Warren', 'Nagini', 'Narcissa Malfoy', 'Nearly Headless Nick', 'Neville Longbottom', 'Newt Scamander', 'Nicolas Flamel', 'Norbert', 'Nott Sr', 'Nymphadora Tonks', 'Oliver Wood', 'Olympe Maxime', 'Padma Patil', 'Pansy Parkinson', 'Parvati Patil', 'Peeves', 'Penelope Clearwater', 'Percival Dumbledore', 'Percy Weasley', 'Peter Pettigrew', 'Petunia Dursley', 'Phineas Nigellus Black', 'Pigwidgeon', 'Pius Thicknesse', 'Pomona Sprout', 'Poppy Pomfrey', 'Quirinus Quirrell', 'Rabastan Lestrange', 'Reginald Cattermole', 'Remus Lupin', 'Rita Skeeter', 'Rodolphus Lestrange', 'Roger Davies', 'Rolanda Hooch', 'Romilda Vane', 'Ron Weasley', 'Ronan', 'Rose Weasley', 'Rowena Ravenclaw', 'Rubeus Hagrid', 'Rufus Scrimgeour', 'Salazar Slytherin', 'Scabbers', 'Scabior', 'Scorpius Malfoy', 'Seamus Finnigan', 'Septima Vector', 'Severus Snape', 'Silvanus Kettleburn', 'Sir Cadogan', 'Sirius Black', 'Stan Shunpike', 'Sturgis Podmore', 'Susan Bones', 'Sybill Trelawney', 'Ted Tonks', 'Teddy Lupin', 'Terry Boot', 'The Bloody Baron', 'The Fat Friar', 'The Fat Lady', 'Theodore Nott', 'Thomas Marvolo Riddle', 'Thomas Riddle Jr.', 'Thomas Riddle Sr.', 'Thorfinn Rowle', 'Travers', 'Trevor', 'Vernon Dursley', 'Viktor Krum', 'Vincent Crabbe', 'Walburga Black', 'Walden Macnair', 'Wilkie Twycross', 'Winky', 'Xenophilius Lovegood', 'Zacharias Smith')

graphs <- list(hp1,hp2,hp3,hp4,hp5,hp6,hp7)

#keep only those with at least 20 appearances on average
to_keep <- c()
for (i in c(1:length(graphs))) {
  degs <- rowSums(graphs[[i]])
  to_keep <- rbind(to_keep,degs)
}
to_keep <- t(to_keep)

inds_to_keep <- which(rowMeans(to_keep) > 20) #needs to appear at least 

for (i in c(1:length(graphs))) {
  graphs[[i]] <- graphs[[i]][inds_to_keep,inds_to_keep]
}

rm(hp1,hp2,hp3,hp4,hp5,hp6,hp7)
chars <- chars[inds_to_keep]

#--------------------------------------------
#cluster using omni method
compute_OMNIBUS_mat <- function(Adjacencies) {
  m = length(Adjacencies)
  n = ncol(Adjacencies[[1]])
  Omni1 <- lapply(Adjacencies, function(A) (Reduce(function(U,j) cbind(U, A), c(list(A), lapply(1:(m-1), function(i) i) ))))
  Omni2 <- lapply(Adjacencies, function(A) (Reduce(function(U,j) rbind(U, A), c(list(A), lapply(1:(m-1), function(i) i) ))))
  Omni = (Reduce(rbind, Omni1) + Reduce(cbind, Omni2))/2
  return(Omni)
}

omni_mat <- compute_OMNIBUS_mat(graphs)

hp_eigen <- irlba(omni_mat,200)
elbs <- getElbows(hp_eigen$d)
d <- elbs[2]

n <- length(chars)
Xhat <- hp_eigen$u[,c(1:d)] %*% diag(sqrt(hp_eigen$d[c(1:d)]))
dim(Xhat)
Xhat_new <- Xhat[c(1:n),]
for (i in c(2:length(graphs))) {
  Xhat_new <- Xhat_new + Xhat[c(
    ((i-1)*n+1):(i*n)
  )]
}

Xhat_new <- Xhat_new/7
dim(Xhat_new)

clusts <- Mclust(Xhat_new)
dat <- data.frame(chars)
names(dat) <- "character"
dat$clust <- clusts$classification

summary(clusts)
dat[which(dat$clust == 1),]
dat[which(dat$clust == 2),]
dat[which(dat$clust == 3),]


#--------------------------------
#try a different graph embedding (note: this embedding is experimental, but we expect to have the paper out next spring)
for (l in c(1:length(graphs))) {
  print(paste("l = ",l))
  
  current_eigen <- irlba(graphs[[l]],d)
  Xhat <- current_eigen$u %*% diag(sqrt(current_eigen$d))
  Xhat <- apply(Xhat,1,function(x) {
    temp <- norm(as.matrix(x),"F")
    if(any(is.nan(x/temp))) {
      return(rep(1,d))
    } 
    
    return(x/temp)
  })
  
  if(l ==1) {
    toSvd <- t(Xhat)
  } else {
    toSvd <- cbind(toSvd,t(Xhat))
    
  }
  
}

final_embed <- irlba(toSvd,d)$u
clusts <- Mclust(final_embed)
dat <- data.frame(chars)
names(dat) <- "character"
dat$clust <- clusts$classification
summary(clusts)
dat[which(dat$clust == 1),] #just seems like miscellaneous
dat[which(dat$clust == 2),] #book 4 people?
dat[which(dat$clust == 3),] #important characters
dat[which(dat$clust == 4),] #quidditch players
dat[which(dat$clust == 5),] #evil people
dat[which(dat$clust == 6),] #weasleys and a couple others
dat[which(dat$clust == 7),] #gryffindor people and also both thomas riddles
dat[which(dat$clust == 8),] #dursleys lol
dat[which(dat$clust == 9),] #potters lol

#-------------------------------------------
#one more embedding:
new_A <- matrix(0,n,n)
for (l in c(1:length(graphs))) {
  new_A <- graphs[[l]] %*% graphs[[l]] + new_A
}

#bias-adj spectral clustering
diag(new_A) <- 0

hp_eigen <- svd(new_A)
elbs <- getElbows(hp_eigen$d)
d <- elbs[3]

Uhat <- hp_eigen$u[,c(1:d)] 
dim(Uhat)
clusts <- Mclust(Uhat,G=3)
summary(clusts)
dat <- data.frame(chars)
names(dat) <- "character"
dat$clust <- clusts$classification

summary(clusts)
dat[which(dat$clust == 1),] #tertiary characters
dat[which(dat$clust == 2),] #secondary characters
dat[which(dat$clust == 3),] #most important characters

