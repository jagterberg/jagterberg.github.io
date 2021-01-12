x <- 5
if(x > 0) {
  print("Positive number")
} #end if(x>0)

x<- -5
if( x>0) {
  print("Positive number")
} else {
  print("Non-positive number")
}

x <- -5
y <- if(x>0) 5 else 6


if(x<0){
  print("Negative number")
} else if(x>0){
  print("Positive number")
} else {
  print("Zero")
}

a <- c(1,2,3); b <- letters[1:3]; x <- -1:1
y <- ifelse(x<0, a, b)

x <- c(-1,-2,3,-4)
count <- 0
for(val in x){
  if(val < 0) {
    count <- count+1
  }
}
count


i <- 1
while(i<6){
  print(i)
  i <- i+1
}

i <- 1
while(i<6){
  print(paste("i is",i))
  i <- i+1
}

x <- 1:5
for(val in x){
  if(val==3){
    break
  }
  print(val)
}

for(val in x){
  if(val ==3){next}
  print(val)
}

x <- 1
repeat{
  print(x)
  x=x+1
  if(x==6){break}
}


hello <- function(){
  print(paste('hello world'))
}

hello()

hello <- function(name="Joshua") {
  print(paste('hello world, my name is',name))
}

hello("Joshua")
hello("JBerg")
hello("not Sam")

pow <- function(x, y){
  result = x^y
  print(paste(x, "raised to the power", y, "is", result))
}

pow(8, 2)
pow(x = 8, y = 2)
pow(y = 2, x = 8)

pow <- function(x, y = 2){
    result = x^y
    print(paste(x, "raised to the power", y, "is", result))
}

pow(3)
pow(3, 1)

check <- function(x){
  if(x>0){
    result <- "Positive"
  }
  else if(x<0){
    result <- "Negative"
  }
  else {
    result <- "Zero"
  }
  return(result)
}

check(0)
check(1)

multi_return = function(color = "red",size = c(20,10),shape = "round"){
  my_list = list(color= color, size= size, shape= shape)
  return(my_list)
}

result <- multi_return()
result
result <- multi_return(color = c("red","blue"),size = c(1:10),shape = "square")
result
result$color

recursive.factorial <- function(x){
  if(x==0) {
    return(1)
  }
  else {
    return(x*recursive.factorial(x-1))
  } 
}

recursive.factorial(3) 
recursive.factorial(100) #slow for big numbers

`%squared_dist%` <- function(x,y){
  return((x-y)^2)
}

13 %squared_dist% 25

exhibit_clt <- function(no_means = 1000,sample_size = 10) {
  sample_means<- c()
  for(i in 1:no_means){
    sample_means[i] <- mean(rexp(sample_size))
  }
  plot(density(sample_means))
  
}

exhibit_clt(sample_size = 10)
exhibit_clt(sample_size = 40)
exhibit_clt(sample_size = 200)
exhibit_clt(sample_size = 1000)

exhibit_clt <- function(no_means = 1000,sample_size = 10,plot=TRUE) {
  sample_means<- c()
  for(i in 1:no_means){
    sample_means[i] <- mean(rexp(sample_size))
  }
  
  if(plot) {
    plot(density(sample_means))
  } else {
    return(sample_means)
  }
  
}


test <- exhibit_clt(sample_size= 10,plot=FALSE)
qqnorm(test)
qqline(test)

test <- exhibit_clt(sample_size= 1000,plot=FALSE)
qqnorm(test)
qqline(test)


dnewdist <- function(x){
  vals <- c(0,1,2)
  pmf <- c(.3,.2,.5)
  if(x==0 | x==1 | x==2) {
    return(pmf[x==vals])
  }
  else {
    return(0)
  }
}

pnewdist <- function(x){
  vals <- c(0,1,2)
  prob <- ifelse(x<0, 0, ifelse(x<1,.3,ifelse(x<2,.5,1)))
  return(prob)
}

rnewdist <- function(n){ 
  vals <- runif(n)
  vec <- ifelse(vals<.3, 0, ifelse(vals<.5,1,2))
  return(vec)
}

means=c(); std_devs=c()
for(i in 1:10000){
  vec = rnewdist(10)
  means[i] = mean(vec); std_devs[i] = sd(vec)
}
hist(means, freq=F); lines(density(means))
hist(std_devs, freq =F); lines(density(std_devs))

make_hist <- function(n=10,mean=TRUE) {
  means=c(); std_devs=c()
  for(i in 1:10000){
    vec = rnewdist(n)
    means[i] = mean(vec); std_devs[i] = sd(vec)
  }
  if(mean) {
    hist(means, freq=F); lines(density(means))
  } else {
    hist(std_devs, freq =F); lines(density(std_devs))
  }

}

make_hist(10)
make_hist(100)
make_hist(1000)

library(UsingR)
hauls = with(bycatch, rep(no.albatross,no.hauls))
n = length(hauls)

boot.means = c()
for(i in 1:1000){
  boot.samp = sample(hauls,n,replace=T)
  boot.means[i]= mean(boot.samp)
}
quantile(boot.means, c(.05,.95))




