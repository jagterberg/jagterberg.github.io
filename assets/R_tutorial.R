#' FM Orientation Summer 2018
#' @description these are R code review for the FM orientation Summer 2018
#' @author Joshua Agterberg
#' adapted from Long Wang, AMS Department
#' 
#' 
#' Note: RStudio can be downloaded from:
#' https://www.rstudio.com/
#' 
#' 
#---------------------------------------------------------- 
#'
# basic R commands:
rm(list = ls()) #removes everything

#-----------------------------------------------------------
# ### one sample testing ### (see 2.1 in notes)
#initialize variables:
n <- 100
mu <- 0
sigma <- 1

#generate 100 normally distributed rv with mean 0 and sd 1: 
set.seed(1) #set for reproducibility
x <- rnorm(n = n, mean = mu, sd = sigma)
summary(x)


# Confidence Interval for mu with known variance
#see formula in notes
alpha <- 0.05
#use built-in R functions qnorm, mean, etc.
mu_LB <- mean(x) - qnorm(alpha/2, lower.tail = FALSE) * sigma / sqrt(n)
mu_UB <-mean(x) + qnorm(alpha/2, lower.tail = FALSE) * sigma / sqrt(n)
cat('C.I. for mu:', mu_LB, mu_UB)

#' Testing for mu with known variance
#' define a function that tests the hypothesis
#' that mu_0 = .1 with a known sigma = 1 and alpha =.05
test_hypothesis_known_variance <- function(mu_0 = .1,dat = NULL,alpha = .05, sigma = 1) {
  #error handling:
  if (is.null(dat)) {
    error('data not provided')
  }
  
  toReturn <- list()
  #list access:
  toReturn[[1]] <- paste0('H0: mu_0 = ',mu_0)
  n <- length(dat)
  test_stat <- (mean(dat) - mu_0) / (sigma / sqrt(n))
  
  
  if (abs(test_stat) > qnorm(alpha/2, lower.tail = FALSE)){
    toReturn[[2]] <- 'Yes'
  } else {
    toReturn[[2]] <- 'No'
  }
  
  toReturn[[3]] <- 2 * pnorm(abs(test_stat), lower.tail = FALSE)
  names(toReturn) <- c('Null Hypothesis'
                              ,paste0("Do We Reject Null at alpha = ",alpha,"?")
                              , "P-Value")  
  return(toReturn)
                       
}

result <- test_hypothesis_known_variance(mu_0=.1,dat = x)
result
result <- test_hypothesis_known_variance(mu_0=.001,dat = x)
result


rm(list = ls())
#----------------------------------
#Now, we do not know the variance (2.2 in notes):
#initialize variables:

n <- 100
mu <- 0
sigma <- 1

#generate 100 normally distributed rv with mean 0 and sd 1: 
set.seed(2) #set for reproducibility
x <- rnorm(n = n, mean = mu, sd = sigma)
summary(x)
alpha <- 0.05


# C.I. for mu with unknown variance
# however, we know the distribution is t-distributed
# so we can usej qt instead of qnorm
s2 <- sd(x)
mu_LB <- mean(x) - qt(alpha/2, df = n-1, lower.tail = FALSE) * sqrt(s2 / n)
mu_UB <-mean(x) + qt(alpha/2, df = n-1, lower.tail = FALSE) * sqrt(s2 / n)
cat('C.I. for mu:', mu_LB, mu_UB)

# C.I. for variance
sigma2_LB <- (n-1) * s2 / qchisq(alpha/2, df = n-1, lower.tail = FALSE)
sigma2_UB <- (n-1) * s2 / qchisq(alpha/2, df = n-1, lower.tail = TRUE)
cat('C.I. for sigma2:', sigma2_LB, sigma2_UB)

# Testing for mu with unknown variance
test_hypothesis_unknown_variance <- function(dat, mu_0 = .1,alpha = .05) {
  #error handling:
  if (is.null(dat)) {
    error('data not provided')
  }
  toReturn <- list()
  toReturn[[1]] <- paste0('H0: mu_0 = ',mu_0)
  s2 <- sd(dat)
  n <- length(dat)
  test_stat <- (mean(x) - mu_0) / (s2 / sqrt(n))
  if (abs(test_stat) > qt(alpha/2, df = n-1, lower.tail = FALSE)){
    toReturn[[2]] <- 'Yes'
  }else {
    toReturn[[2]] <- 'No'
  }
  toReturn[[3]] <- 2 * pt(abs(test_stat), df = n-1, lower.tail = FALSE)
 
  names(toReturn) <- c('Null Hypothesis'
                              ,paste0("Do We Reject Null at alpha = ",alpha,"?")
                              , "P-Value")
  
  return(toReturn)
}

result <- test_hypothesis_unknown_variance(dat = x, mu_0 = .2)
result
result <- test_hypothesis_unknown_variance(dat = x, mu_0 = .0001)
result


#-----------------------------------two sample testing (Section3 in the notes)-----------------
rm(list = ls())
# true parameters
mu_X <- 1
mu_Y <- 1
sigma_X <- 1
sigma_Y <- 2
n_X <- 1000
n_Y <- 1000

# generate data
data_X <- rnorm(n_X, mean = mu_X, sd = sigma_X)
data_Y <- rnorm(n_Y, mean = mu_Y, sd = sigma_Y)
#generate histograms of data:
hist(data_X, breaks = 20, freq = FALSE)
hist(data_Y, breaks = 20, freq = FALSE)

# point estimation
x_bar <- mean(data_X)
y_bar <- mean(data_Y)
cat("point estimation:", x_bar - y_bar)

# interval estimation
LCB <- x_bar - y_bar + qnorm(0.025) * sqrt(sigma_X^2 / n_X + sigma_Y^2 / n_Y)
UCB <- x_bar - y_bar + qnorm(0.975) * sqrt(sigma_X^2 / n_X + sigma_Y^2 / n_Y)
cat("interval estimation", LCB, UCB)

s_X <- sd(data_X)
s_Y <- sd(data_Y)
LCB <- x_bar - y_bar + qnorm(0.025) * sqrt(s_X^2 / n_X + s_Y^2 / n_Y)
UCB <- x_bar - y_bar + qnorm(0.975) * sqrt(s_X^2 / n_X + s_Y^2 / n_Y)
cat("interval estimation", LCB, UCB)



#------------------------------------------------------------- Bootstrap!---------------
rm(list = ls())
x <- rnorm(1000000,mean = .05,sd = 2)

#number of samples
nreps <- 1000
s <- rep(0,nreps)

#size of each sample
size <- 100

#sample and process nreps times
for (i in 1:nreps) {
  s[i] <- mean(sample(x,size,replace = FALSE))
}

#generate a histogram. should be normal:
hist(s,breaks = 200,freq = FALSE)
xfit = seq(min(s),max(s),.01)
yfit <- dnorm(xfit,.05,2/10)
lines(xfit,yfit, col = 'purple', lwd = 2)
abline(v=.05,col = 'black',lwd = 4)


#redo, only generate a histogram of the chi-sq dist
s <- rep(0,nreps)
size <- 100

for (i in 1:nreps) {
  s[i] <- (99/4)*var(sample(x,size,replace = TRUE))
}

hist(s,breaks = 200,freq = FALSE)
xfit = seq(min(s),max(s),.01)
yfit <- dchisq(xfit,99)
lines(xfit,yfit, col = 'purple', lwd = 2)

#-------------------------------------------------- Corresponding to Section 5 in the Notes:--------
#' ANOVA tests
# one-way ANOVA
#create some GRE data:
LS_GRE<-c(630,660,640,660,470,480,600,650,580,710)
PS_GRE<-c(660,760,640,670,720,700,690,710,530,450)
Soc_GRE<-c(440,540,330,450,670,570,570,530,590,630)

#useful function: rep:
LS<-rep("LS",10)
PS<-rep("PS",10)
SocS<-rep("SocS",10) 

Score<-c(LS_GRE,PS_GRE,Soc_GRE)
Major<-c(LS,PS,SocS)

Scores_by_Major<-data.frame(Score,Major)
summary(Scores_by_Major)

GRE_anova<-aov(Score~Major,data=Scores_by_Major)

summary(GRE_anova)

TukeyHSD(GRE_anova, conf.level = 0.95)


# two-way ANOVA
rm(list =ls())
Day<-c(571,610,625,480,516,465)
Swing<-c(480,474,540,625,600,581)
Night<-c(470,430,450,630,680,661)
Productivity<-c(Day, Swing, Night)

D<-rep("D",6)
S<-rep("S",6)
N<-rep("N",6)
Shift<-c(D,S,N)

sup1<-rep("1", 3)
sup2<-rep("2", 3)
sup3<-c(sup1, sup2)
Supervisor <-rep(sup3, 3)

Productivity_by_Shift_Super<-data.frame(Productivity, Shift, Supervisor)
summary(Productivity_by_Shift_Super)
#intro to data.table:
library(data.table)
Productivity_by_Shift_Super <-data.table(Productivity_by_Shift_Super)
Productivity_by_Shift_Super[,list(Productivity_level=mean(Productivity),
                                  Productivity_variance=var(Productivity))
                            ,by = list(Shift,Supervisor)]


Productivity_ANOVA<-aov(Productivity~Shift * Supervisor, data=Productivity_by_Shift_Super)
summary(Productivity_ANOVA)
TukeyHSD(Productivity_ANOVA)


#-----------------------------
rm(list = ls())
# --------------Linear Regression-----------------------
#' Section 6 in the notes;
### linear regression ###

library(MASS)
data(cars)
plot(cars$speed, cars$dist)
speed_dist_reg <- lm(cars$dist ~ cars$speed)
abline(speed_dist_reg)
summary(speed_dist_reg)
speed_dist_aov <- aov(speed_dist_reg)
summary(speed_dist_aov)

x <- cars$speed
y <- cars$dist
n <- length(x)
Sxy <- sum(x*y) - sum(x) * sum(y) / n
Sxx <- sum(x^2) - sum(x)^2 / n
Syy <- sum(y^2) - sum(y)^2 / n

# estimate
beta_hat <- Sxy / Sxx
alpha_hat <- mean(y) - beta_hat * mean(x)
alpha_hat
beta_hat
#check:
speed_dist_reg$coefficients

# beta testing
TSS <- Syy
SSR <- Sxy^2 / Sxx
SSE <- Syy - Sxy^2 / Sxx
TSS - SSR - SSE

MSR <- SSR / 1
MSE <- SSE / (n-2)
beta_F_value <- MSR / MSE
beta_F_value
beta_F_p_vlaue <- 1 - pf(beta_F_value, df1 = 1, df2 = n-2)
beta_F_p_vlaue

beta_SE <- sqrt(MSE / Sxx)
beta_SE

beta_t_value <- beta_hat / beta_SE
beta_t_value
beta_t_value^2 - beta_F_value

beta_t_p_value <- 2 * (1 - pt(beta_t_value, df = n-2))
beta_t_p_value

# alpha testing
alpha_SE <- sqrt(MSE * (sum(x^2)/n) / Sxx)
alpha_SE
alpha_t_value <- alpha_hat / alpha_SE
alpha_t_value
alpha_t_p_value <- 2 * pt(alpha_t_value, df = n-2)
alpha_t_p_value

# R^2
SSR / TSS
1 - MSE/(TSS/(n-1))n

#----------------- Typical R Linear Regression Code ------------
rm(list = ls())
#Useful R code:
getwd()
#set this to where your training and testing data are stored
#setwd('./School/Teaching/FM_Orientation_2018/all') 
setwd('./all')
#code taken from: https://www.kaggle.com/notaapple/detailed-exploratory-data-analysis-using-r


train <- read.csv("train.csv")
library(data.table)
train <- data.table(train)
na_vals <- sapply(train,function(x) {
  return(!any(is.na(x)))
})
train <- train[,..na_vals]


#-- cleaned code --
names(train)
summary(train)
train[,Id:= NULL]

#split to train and test
set.seed(4443)
training <- sample(c(1:nrow(train)),floor(2/3*nrow(train)))
testing <- setdiff(c(1:nrow(train)),training)
train[, `:=` (Utilities = NULL
            ,Condition2 = NULL
            ,RoofStyle = NULL
            ,RoofMatl = NULL
            ,ExterCond = NULL
            ,HeatingQC  = NULL
            )]
test <- train[testing]
train <- train[training]



train <- as.data.frame(unclass(train))
test <- as.data.frame(unclass(test))
#run a linear regression:
lm_basic <- lm(SalePrice~.,data=train)
plot(lm_basic)


#------------------------------- ML Concepts--------------------------------
#stepwise regression:
library(MASS)
step <- stepAIC(lm_basic)
summary(step)
plot(step)

#check the AIC:
AIC(lm_basic)
AIC(step)

#ridge regression
library(glmnet)
library(glmnetUtils)

lambdas <- 10^seq(3, -2, by = -.1)

#need to turn back to data.frame
train <- as.data.frame(unclass(train))
test <- as.data.frame(unclass(test))
fit <- glmnet(SalePrice~.,data=train, alpha = 0, lambda = lambdas)
summary(fit)
ridge <- cv.glmnet(SalePrice~.,data=train, alpha = 0, lambda = lambdas)


#lasso
fit <- glmnet(SalePrice~.,data=train, alpha = 1, lambda = lambdas)
summary(fit)
lasso <- cv.glmnet(SalePrice~.,data=train, alpha = 1, lambda = lambdas)


#random forest for the kicks:
library(randomForest)
library(caret)
rf <- randomForest(SalePrice~.,data=train)
res <- tuneRF(train[,names(train)[!names(train)%in%"SalePrice"]],train$SalePrice)
mtry_opt <- res[,"mtry"][which.min(res[,"OOBError"])]
rf <- randomForest(SalePrice~.,data=train,mtry = mtry_opt,ntree=1000)

models <- list(lm_basic,step,ridge,lasso,rf)

getBestModel <- function(models,tests = test) {
  mse <- unlist(lapply(models,function(x) {
    predicted <- predict(x,tests)
    return( sum((predicted - tests$SalePrice)^2) )
  }))
  
  return(models[[which.min(mse)]])
}

bestModel <- getBestModel(models)
summary(bestModel)
bestModel

#-------------dimensionality reduction--------------
rm(list = ls())
dat <- iris
#pca:
numeric_vars <- sapply(dat,is.numeric)
new_dat <- prcomp(dat[,numeric_vars])
summary(new_dat)
dat <- cbind(dat,new_dat$x)




#---------------------classification-----------------
dat$setosa_ind <- factor(ifelse(dat$Species == 'setosa',1,0))
dat[,'Species'] <- NULL

summary(dat)
View(dat)

#split data into train and test: (no CV here)
set.seed(1234)
train <- sample(c(1:nrow(dat)),floor(2/3*nrow(dat)))
test <- setdiff(c(1:nrow(dat)),train)
dat_train <- dat[train,]
dat_test <- dat[test,]


#logistic regression:
#Note: we need a variable to try to classify

logistic <- glm(setosa_ind~.,data=dat_train,family = binomial(link = 'logit'))
summary(logistic)

if(!require(randomForest)) {
  install.packages('randomForest')
  library(randomForest)
}

rf <- randomForest(setosa_ind~.,data=dat_train)
rf

if(!require(tree)) {
  install.packages('tree')
  library(tree)
}

dt <- tree(setosa_ind~.,data=dat_train)

get_error <- function(model,test = dat_test,var = 'setosa_ind') {
  predicted <- predict(model,test)
  if("glm" %in% class(model)) {
    predicted <- ifelse(predicted > .5,1,0)
  }
  error <- sum(predicted != test[,var])
  error
}

get_error(rf)
get_error(logistic)
get_error(dt)


logistic2 <- glm(setosa_ind~PC1 + PC2, data =dat,family = binomial)
summary(logistic2)
summary(logistic)

rf_new <- randomForest(setosa_ind~PC1 + PC2, data =dat)
get_error(rf_new)





