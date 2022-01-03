1 + 2
4 - 5
1 + 2 * 3

sqrt(4)
exp(1)
exp(0)
log(exp(1))
sin(pi)
log(10,base=10)
log(10,10)

?log
?sin
??log
??sin

squareroot(2)
sqrt(-2)


x <- 2
x = 2
x <- 2*x + 1
(x <- 2 * x + 1)


x<-2
n<-25
h.big.heckin.number <- 123456789
IttyBitty <- 0.00000001


whales <- c(74, 122, 235, 111, 292, 111, 211, 133, 156, 79)
x <- c(74, 122, 235, 111, 292); y <- c(111, 211, 133, 156, 79)
(z <- c(x,y))

simpsons <- c("Homer","Marge","Bart","Lisa","Maggie")
names(simpsons) <- c("dad","mom","son", "daughter 1","daughter 2")


sum(whales)
length(whales)
sum(whales)/length(whales)
mean(whales)

sort(whales)

whales.fla <- c(89,254,306,292,274,233,294,204,204,90)
whales + whales.fla
whales - whales.fla
whales - mean(whales)

x <- c(2, 3, 5, 7, 11)
xbar <- mean(x)
n <- length(x)
s_squared <- sum((x-xbar)^2)/(n-1)
s_squared

var(x)



simple_seq <- 1:10
1:10
rev(1:10)
10:1
a=1; h=4; n=5
a+h*(0:(n-1))
seq(1, 9, by=2)
seq(1,10, length=5)
rep(1,10)
n <- 100
rep(1:3,n)
ebay <- c(88.8, 88.3, 90.2, 93.5, 94.7, 99.2, 99.4, 101.6)
ebay[1]
ebay[9]
ebay[length(ebay)]

ebay[1:4]
ebay[c(1,5,9)]

ebay[-1]
ebay[-c(1:4)]

simpsons['dad']

ebay[9] <- 88.0
ebay[10:13] <- c(97.0, 99.3, 102.0, 101.8)
ebay


ebay > 100
ebay[ebay > 100]
which(ebay > 100)
sum(ebay > 100) 
sum(ebay > 100)/length(ebay) 


shuttle <- c(0,1,0,NA,0,0)
shuttle > 0
is.na(shuttle)
mean(shuttle)
mean(shuttle, na.rm = T)
mean(shuttle[!is.na(shuttle)])

ls()
rm(ebay)

range(lynx)
sum(lynx)

install.packages("MASS")
library('MASS')
names(geyser)
geyser$waiting 
attach(geyser)
waiting
detach(geyser)

getwd()
setwd("C:/Users/joshu/Dropbox/Documents/Research/misc_and_old/website/jagterberg.github.io/assets/r_2022")
getwd()

dat <- read.table("train.csv", sep = ",")
dat



names(dat)
names(dat)[1]
dat["V1"]
summary(dat)
summary(dat[c("V1","V2","V3")])

v7 <- dat["V7"]
summary(v7)

simpsons[1]
simpsons[1:2]
simpsons["dad"]
instructor <- c("Joshua")
names(instructor) <- "instructor"
simpsons.instructor <- c(simpsons,instructor)
names(simpsons.instructor)

     