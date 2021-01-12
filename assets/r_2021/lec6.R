rm(list=ls())
#get the data
train <- read.csv("https://jagterberg.github.io/assets/masters2020/train.csv")

#alternative to dplyr: data.table
library(data.table)
train <- data.table(train)
na_vals <- sapply(train,function(x) {
  return(!any(is.na(x)))
})

#the train[..na_vals] removes the ones with na_vals
train <- train[,..na_vals]

#-- cleaned code --
names(train)
summary(train)

#this removes Id
train[,Id:= NULL]

#split to train and test
set.seed(4442)
training <- sample(c(1:nrow(train)),floor(2/3*nrow(train)))
testing <- setdiff(c(1:nrow(train)),training)
train[, `:=` (Utilities = NULL
              ,Condition2 = NULL
              ,RoofStyle = NULL
              ,RoofMatl = NULL
              ,ExterCond = NULL
              ,HeatingQC  = NULL
              ,Exterior1st = NULL
              ,Heating=NULL
)]
test <- train[testing]
train <- train[training]


#convert to dataframe
train <- as.data.frame(unclass(train),stringsAsFactors = T)
test <- as.data.frame(unclass(test),stringsAsFactors = T)
test <- rbind(xtrain[1, ] , xtest)
test <- test[-1,]

#run a linear regression:
lm_basic <- lm(SalePrice~.,data=train)
summary(lm_basic)
plot(lm_basic)


#------------------------------- ML Concepts--------------------------------
#stepwise regression:
library(MASS)
step <- stepAIC(lm_basic)
summary(step)
plot(step)

#check the AIC:
#AIC(lm_basic)
#AIC(step)

#ridge regression
library(glmnet)
library(glmnetUtils)

lambdas <- 10^seq(3, -2, by = -.1)

#need to turn back to data.frame
train <- as.data.frame(unclass(train),stringsAsFactors = T)
test <- as.data.frame(unclass(test),stringsAsFactors = T)
test <- rbind(train[1, ] , test)
test <- test[-1,]
fit <- glmnet(SalePrice~.,data=train, alpha = 0, lambda = lambdas)
summary(fit)
ridge <- glmnetUtils::cv.glmnet(SalePrice~.,data=train, alpha = 0, lambda = lambdas)


#lasso
fit <- glmnet(SalePrice~.,data=train, alpha = 1, lambda = lambdas)
summary(fit)
lasso <- glmnetUtils::cv.glmnet(SalePrice~.,data=train, alpha = 1, lambda = lambdas)


#random forest for the kicks:
library(randomForest)
library(caret)
rf <- randomForest(SalePrice~.,data=train)
#res <- tuneRF(train[,names(train)[!names(train)%in%"SalePrice"]],train$SalePrice)
#mtry_opt <- res[,"mtry"][which.min(res[,"OOBError"])]
#rf <- randomForest(SalePrice~.,data=train,mtry = mtry_opt,ntree=1000)

models <- list(lm_basic,step,
               ridge,lasso,
               rf)


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

#if(!require(tree)) {
#  install.packages('tree')
#  library(tree)
#}

#dt <- tree(setosa_ind~.,data=dat_train)

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
#get_error(dt)


logistic2 <- glm(setosa_ind~PC1 + PC2, data =dat_train,family = binomial)
summary(logistic2)
summary(logistic)
get_error(logistic2)

rf_new <- randomForest(setosa_ind~PC1 + PC2, data =dat)
get_error(rf_new)



library(MASS)
attach(Cars93)
plot(MPG.city,Price, pch = as.numeric(Type))
legend("topright",
       legend=c(levels(Type)),
       pch=c(1:6))



