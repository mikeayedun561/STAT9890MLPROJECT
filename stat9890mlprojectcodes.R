######################
## STA 9890 Project ##
##   Erik Carrion   ##
##  Michael Ayedun  ##
######################

#################################
##      Code for Plots         ##
## written by Michael Ayedun   ##
#################################


rm(list=ls())
cat("\014")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(glmnet)
library(randomForest)



df = read.csv("C:/Users/micha/Downloads/ailerons.csv")

K= 100
Rsq.train.ls = rep(0,K)
Rsq.test.ls = rep(0,K)
Rsq.test.rid = rep(0,K)
Rsq.train.rid = rep(0,K)
Rsq.test.en = rep(0,K)
Rsq.train.en = rep(0,K)
Rsq.test.tr = rep(0,K)
Rsq.train.tr = rep(0,K)


n = dim(df)[1]
p = dim(df)[2]-1
ntrain = floor(0.8*n)
ntest = n - ntrain
# lasso
for (k in c(1:K)) {
  shuffled_indexes  =     sample(n)
  train             =     shuffled_indexes[1:ntrain]
  test              =     shuffled_indexes[(1+ntrain):n]
  Dtrain            =     df[train, ]
  Dtest             =     df[test,]
  X.train           =     as.matrix(Dtrain[,-41])
  X.test            =     as.matrix(Dtest[,-41])
  y.train           =     Dtrain[,41]
  y.test            =     Dtest[,41]
  
  cv.fit.ls         =     cv.glmnet(X.train, y.train, alpha = 1, nfolds = 10)
  fit.ls            =     glmnet(X.train, y.train, alpha = 1, lambda = cv.fit.ls$lambda.min)
  y.train.hat.ls    =     predict(fit.ls, newx = X.train, type = "response")
  y.test.hat.ls     =     predict(fit.ls, newx = X.test, type = "response")
  Rsq.test.ls[k]    =     1-mean((y.test - y.test.hat.ls)^2)/mean((y.test - mean(y.test))^2)
  Rsq.train.ls[k]   =     1-mean((y.train - y.train.hat.ls)^2)/mean((y.train - mean(y.train))^2)
  
  cv.fit.en         =     cv.glmnet(X.train, y.train, alpha = 0.5, nfolds = 10)
  fit.en            =     glmnet(X.train, y.train, alpha = 0.5, lambda = cv.fit.en$lambda.min)
  y.train.hat.en    =     predict(fit.en, newx = X.train, type = "response")
  y.test.hat.en     =     predict(fit.en, newx = X.test, type = "response") 
  Rsq.test.en[k]    =     1-mean((y.test - y.test.hat.en)^2)/mean((y.test - mean(y.test))^2)
  Rsq.train.en[k]   =     1-mean((y.train - y.train.hat.en)^2)/mean((y.train - mean(y.train))^2)
  
  cv.fit.rid        =     cv.glmnet(X.train, y.train, alpha = 0, nfolds = 10)
  fit.rid           =     glmnet(X.train, y.train, alpha = 0, lambda = cv.fit.rid$lambda.min)
  y.train.hat.rid   =     predict(fit.rid, newx =X.train, type = "response")
  y.test.hat.rid    =     predict(fit.rid, newx =X.test, type = "response")
  Rsq.test.rid[k]   =     1-mean((y.test - y.test.hat.rid)^2)/mean((y.test - mean(y.test))^2)
  Rsq.train.rid[k]  =     1-mean((y.train - y.train.hat.rid)^2)/mean((y.train - mean(y.train))^2)
  
  
}
for (k in c(1:K)) {
  shuffled_indexes  =     sample(n)
  train             =     shuffled_indexes[1:ntrain]
  test              =     shuffled_indexes[(1+ntrain):n]
  Dtrain            =     df[train, ]
  Dtest             =     df[test,]
  X.train           =     as.matrix(Dtrain[,-41])
  X.test            =     as.matrix(Dtest[,-41])
  y.train           =     Dtrain[,41]
  y.test            =     Dtest[,41]
  
  fit.tree          =     randomForest(goal~.,data=df,subset=train,ntree=500,mtry=sqrt(40))
  y.train.hat.tr    =     predict(fit.tree,newdata=df[train,])
  y.test.hat.tr     =     predict(fit.tree,newdata=df[test,])
  
  Rsq.train.tr[k]   =      1-mean((y.train - y.train.hat.tr)^2)/mean((y.train - mean(y.train))^2)
  Rsq.test.tr[k]    =      1-mean((y.test - y.test.hat.tr)^2)/mean((y.test - mean(y.test))^2)
}

# when I get Rsq save the data
ltrain = data.frame(group="lasso",value=Rsq.train.ls)
rtrain = data.frame(group="ridge",value=Rsq.train.rid)
etrain = data.frame(group="elastic-net",value=Rsq.train.en)
rftrain = data.frame(group="RF",value=Rsq.train.tr)

ltest = data.frame(group="lasso",value=Rsq.test.ls)
rtest = data.frame(group="ridge",value=Rsq.test.rid)
etest = data.frame(group="elastic-net",value=Rsq.test.en)
rftest = data.frame(group="RF",value=Rsq.test.tr)

Rstrain = rbind(ltrain,rtrain,etrain,rftrain)
Rstest = rbind(ltest,rtest,etest,rftest)
Rstrain$group = factor(Rstrain$group)
ggplot(Rstrain, aes(x=group, y=value,fill=group)) + 
  geom_boxplot()+labs(x="models",y="Rsq")+ggtitle("Rsq for train")

Rstest$group = factor(Rstest$group)
ggplot(Rstest, aes(x=group, y=value,fill=group)) + 
  geom_boxplot()+labs(x="models",y="Rsq")+ggtitle("Rsq for test")
# 4
train             =     c(1:ntrain)
test              =     c((1+ntrain):n)

e.en.train = rep(0,length(train))
e.en.test = rep(0,length(test))
e.las.train = rep(0,length(train))
e.las.test = rep(0,length(test))
e.rid.train = rep(0,length(train))
e.rid.test = rep(0,length(test))
e.tree.train = rep(0,length(train))
e.tree.test = rep(0,length(test))
for (k in c(1:K)){
  shuffled_indexes =     sample(n)
  train            =     shuffled_indexes[1:ntrain]
  test             =     shuffled_indexes[(1+ntrain):n]
  Dtrain           =     df[train, ]
  Dtest            =     df[test,]
  X.train          =     as.matrix(Dtrain[,-41])
  X.test           =     as.matrix(Dtest[,-41])
  y.train          =     Dtrain[,41]
  y.test           =     Dtest[,41]
  if (k == 50){
    cv.fit.l = cv.glmnet(X.train, y.train, alpha = 1, nfolds = 10)
    fit.l = glmnet(X.train,y.train,alpha=1,lambda=cv.fit.l$lambda.min)
    y.train_hat.ls = predict(fit.l,newx=X.train,type="response")
    y.test_hat.ls = predict(fit.l,newx=X.test,type="response")
    e.las.train = y.train - y.train_hat.ls 
    e.las.test = y.test - y.test_hat.ls
    
    cv.fit.r = cv.glmnet(X.train, y.train, alpha = 0, nfolds = 10)
    fit.r = glmnet(X.train,y.train,alpha=0,lambda=cv.fit.r$lambda.min)
    y.train_hat.r = predict(fit.r,newx=X.train,type="response")
    y.test_hat.r = predict(fit.r,newx=X.test,type="response")
    e.rid.train =   y.train - y.train_hat.r
    e.rid.test = y.test - y.test_hat.r  
    
    cv.fit.e= cv.glmnet(X.train, y.train, alpha = 0.5, nfolds = 10)
    fit.e = glmnet(X.train,y.train,alpha=0.5,lambda=cv.fit.e$lambda.min)
    y.train_hat.e = predict(fit.e,newx=X.train,type="response")
    y.test_hat.e = predict(fit.e,newx=X.test,type="response")
    e.en.train = y.train - y.train_hat.e 
    e.en.test = y.test - y.test_hat.e 
    
    fit.tree          =     randomForest(goal~.,data=df,subset=train,ntree=500,mtry=sqrt(40))
    y.train.hat.tr    =     predict(fit.tree,newdata=df[train,])
    y.test.hat.tr     =     predict(fit.tree,newdata=df[test,])
    e.tree.train      =     y.train - y.train.hat.tr 
    e.tree.test       =     y.test - y.test.hat.tr 
    
  }
}
library(gridExtra)
par(mfrow=c(1,2))
eltrain = data.frame(group="lasso",value=e.las.train)
ertrain = data.frame(group="ridge",value=e.rid.train)
eentrain = data.frame(group="elastic-net",value=e.en.train)
erftrain = data.frame(group="RF",value=e.tree.train)
colnames(erftrain)[2] = "s0"

eltest = data.frame(group="lasso",value=e.las.test)
ertest = data.frame(group="ridge",value=e.rid.test)
eentest = data.frame(group="elastic-net",value=e.en.test)
erftest = data.frame(group="RF",value=e.tree.test)
colnames(erftest)[2] = "s0"

errortrain = rbind(eltrain,ertrain,eentrain,erftrain)
errortest = rbind(eltest,ertest,eentest,erftest)
errortrain$group = factor(errortrain$group)
ggplot(errortrain, aes(x=group, y=s0,fill=group)) + 
  geom_boxplot()+labs(x="models",y="Residuals")+ggtitle("Residuals for train")

errortest$group = factor(errortest$group)
ggplot(errortest, aes(x=group, y=s0,fill=group)) + 
  geom_boxplot()+labs(x="models",y="Residuals")+ggtitle("Residuals for test")

#e.rid.train = rep(0,length(X.train))
#e.rid.test = rep(0,length(X.test))
for (k in c(1:K)){
  shuffled_indexes =     sample(n)
  train            =     shuffled_indexes[1:ntrain]
  test             =     shuffled_indexes[(1+ntrain):n]
  Dtrain           =     df[train, ]
  Dtest            =     df[test,]
  X.train          =     as.matrix(Dtrain[,-41])
  X.test           =     as.matrix(Dtest[,-41])
  y.train          =     Dtrain[,41]
  y.test           =     Dtest[,41]
  if (k == 50){
    cv.fit = cv.glmnet(X.train, y.train, alpha = 0, nfolds = 10)
    plot(cv.fit)+title("Cross Validation for ridge")
    fit = glmnet(X.train,y.train,alpha=0,lambda=cv.fit$lambda.min)
    
  }
}





for (k in c(1:K)){
  shuffled_indexes =     sample(n)
  train            =     shuffled_indexes[1:ntrain]
  test             =     shuffled_indexes[(1+ntrain):n]
  Dtrain           =     df[train, ]
  Dtest            =     df[test,]
  X.train          =     as.matrix(Dtrain[,-41])
  X.test           =     as.matrix(Dtest[,-41])
  y.train          =     Dtrain[,41]
  y.test           =     Dtest[,41]
  if (k == 50){
    cv.fit.e= cv.glmnet(X.train, y.train, alpha = 0.5, nfolds = 10)
    plot(cv.fit.e)+title("Cross Validation for elastic net")
    fit.e = glmnet(X.train,y.train,alpha=0.5,lambda=cv.fit.e$lambda.min)
    
  }
}





for (k in c(1:K)){
  shuffled_indexes =     sample(n)
  train            =     shuffled_indexes[1:ntrain]
  test             =     shuffled_indexes[(1+ntrain):n]
  Dtrain           =     df[train, ]
  Dtest            =     df[test,]
  X.train          =     as.matrix(Dtrain[,-41])
  X.test           =     as.matrix(Dtest[,-41])
  y.train          =     Dtrain[,41]
  y.test           =     Dtest[,41]
  if (k == 50){
    cv.fit.las = cv.glmnet(X.train, y.train, alpha = 1, nfolds = 10)
    plot(cv.fit.las)+title("Cross validation for lasso")
  }
}



#5


X = as.matrix(df[,-41])
y = df[,41]
p = dim(X)[2]
# lasso
s = apply(X,2,sd)

cv.fit.ls = cv.glmnet(X,y,alpha=1,nfolds=10)
fit.ls = glmnet(X,y,alpha=1,lambda=cv.fit.ls$lambda.min)
#yhat.test = predict(fit.ls,newx=X.test)


cv.fit.ridge = cv.glmnet(X,y,alpha=0,nfolds=10)
fit.ridge = glmnet(X,y,alpha=0,lambda=cv.fit.ridge$lambda.min)


cv.fit.elastic = cv.glmnet(X,y,alpha=0.5,nfolds=10)
fit.elastic = glmnet(X,y,alpha=0.5,lambda=cv.fit.elastic$lambda.min)



fit.rf = randomForest(goal~.,data=df,mtry=sqrt(40))
imp = importance(fit.rf)

betaS.ls               =     data.frame(c(1:p), as.vector(fit.ls$beta)*s)
colnames(betaS.ls)     =     c( "feature", "value")
betaS.ls <-  betaS.ls %>% 
  mutate(pos = value >= 0)

betaS.ridge               =     data.frame(c(1:p), as.vector(fit.ridge$beta)*s)
colnames(betaS.ridge)     =     c( "feature", "value")
betaS.ridge <-  betaS.ridge %>% 
  mutate(pos = value >= 0)

betaS.elastic             =     data.frame(c(1:p), as.vector(fit.elastic$beta)*s)
colnames(betaS.elastic)     =     c( "feature", "value")
betaS.elastic <-  betaS.elastic %>% 
  mutate(pos = value >= 0)

betaS.rf                    =   data.frame(c(1:p), as.vector(imp[,1]))
colnames(betaS.rf)     =     c( "feature", "value")
betaS.rf<-  betaS.rf %>% 
  mutate(pos = value >= 0)
# we need to change the order of factor levels by specifying the order explicitly.
betaS.ls$feature     =  factor(betaS.ls$feature, levels = betaS.elastic$feature[order(betaS.elastic$value, decreasing = TRUE)])
betaS.ridge$feature  =  factor(betaS.ridge$feature, levels = betaS.elastic$feature[order(betaS.elastic$value, decreasing = TRUE)])
betaS.rf$feature     =  factor(betaS.rf$feature, levels = betaS.elastic$feature[order(betaS.elastic$value, decreasing = TRUE)])
betaS.elastic$feature=  factor(betaS.elastic$feature, levels = betaS.elastic$feature[order(betaS.elastic$value, decreasing = TRUE)])



lsPlot =  ggplot(betaS.ls, aes(x=feature, y=value,fill=pos)) +
  geom_col(position= "identity", colour="black",size = 0.25)+
  ggtitle("Coefficent Plots for lasso ")+labs(y="Coefficents",x="features")+
  scale_fill_manual(values = c("#CCEEFF", "#FFDDDD"), guide = FALSE)

enPlot =  ggplot(betaS.elastic, aes(x=feature, y=value,fill=pos)) +
  geom_col(position = "identity", colour="black")+
  ggtitle("Coefficent Plots for Elastic net ")+labs(y="Coefficents",x="features")+
  scale_fill_manual(values = c("#CCEEFF", "#FFDDDD"), guide = FALSE)

ridPlot = ggplot(betaS.ridge, aes(x=feature, y=value,fill=pos)) +
  geom_col(position= "identity", colour="black",size = 0.25)+
  ggtitle("Coefficent Plots for ridge ")+labs(y="Coefficents",x="features")+
  scale_fill_manual(values = c("#CCEEFF", "#FFDDDD"), guide = FALSE)

rfPlot = ggplot(betaS.rf, aes(x=feature, y=value,fill=pos)) +
  geom_col(position = "identity", colour="black",size = 0.25)+
  ggtitle("Feature Importance Plots for Random Forest ")+labs(y="Coefficents",x="features")+
  scale_fill_manual(values = c("#CCEEFF", "#FFDDDD"), guide = FALSE)


library(gridExtra)
par(mfrow=c(1,2))
grid.arrange(lsPlot, ridPlot, enPlot,rfPlot,nrow=4)


# 5a
lasR = quantile(Rsq.test.ls,probs=c(.05,.95))

lasso = function(d){
  X = as.matrix(d[,-41])
  y = d[,41]
  cv.l = cv.glmnet(X,y,alpha=1,nfolds=10)
  l = glmnet(X,y,alpha=1,lambda=cv.l$lambda.min)
  
}
ridR = quantile(Rsq.test.rid,probs=c(.05,.95))
ridge = function(d){
  X = as.matrix(d[,-41])
  y = d[,41]
  cv.r=cv.glmnet(X,y,alpha=0,nfolds=10)
  r=glmnet(X,y,alpha=0,lambda=cv.r$lambda.min)
}
elasticR = quantile(Rsq.test.en,probs=c(.05,.95))

elastic = function(d){
  X = as.matrix(d[,-41])
  y = d[,41]
  cv.e = cv.glmnet(X,y,alpha=0.5,nfolds=10)
  e=glmnet(X,y,alpha=0.5,lambda=cv.e$lambda.min)
}
rR.r = quantile(Rsq.test.tr,probs=c(.05,.95))
rForest = function(d){
  X = as.matrix(d[,-41])
  y = d[,41]
  r=randomForest(goal~.,data=d,mtry=sqrt(40))
}

lastime = system.time(lasso(df))
ridtime = system.time(ridge(df))
elastime = system.time(elastic(df))
rfortime = system.time(rForest(df))


#################################
##      Code for Times         ##
##  written by Erik Carrion    ##
#################################
library(ggplot2)
library(glmnet)
library(readr)
library(randomForest)
library(tidyverse)
library(gridExtra)
rf.data <- read_csv("C:/Users/eriks/OneDrive - Smart City Real Estate/Personal/Baruch/S4/STA 9890 - Machine Learning/Project/ailerons.csv")
ailerons <- as.matrix(rf.data)

# Reproducibility
RNGkind(sample.kind = "Rounding")
set.seed(10)


n   = nrow(ailerons)
p   = ncol(ailerons[,-1])
X   = ailerons[,1:40]
y   = ailerons[,41]

# Store the Test MSE, Variance, and R^2 for ridge, lasso, elastic net

mse.ridge = matrix(0, nrow = 100, 1)
mse.lasso = matrix(0, nrow = 100, 1)
mse.elnet = matrix(0, nrow = 100, 1)

var.ridge = matrix(0, nrow = 100, 1)
var.lasso = matrix(0, nrow = 100, 1)
var.elnet = matrix(0, nrow = 100, 1)

rsq.ridge = matrix(0, 100, 1)
rsq.lasso = matrix(0, 100, 1)
rsq.elnet = matrix(0, 100, 1)

# Calculate Training R^2 for ridge, lasso, and elnet 
mse.ridge.train = matrix(0, 100, 1)
mse.lasso.train = matrix(0, 100, 1)
mse.elnet.train = matrix(0, 100, 1)

var.ridge.train = matrix(0, 100, 1)
var.lasso.train = matrix(0, 100, 1)
var.elnet.train = matrix(0, 100, 1)

rsq.ridge.train = matrix(0, 100, 1)
rsq.lasso.train = matrix(0, 100, 1)
rsq.elnet.train = matrix(0, 100, 1)


# capture the time it takes to execute cross validation
time.cv.ridge = matrix(0, 100, 1)
time.cv.lasso = matrix(0, 100, 1)
time.cv.elnet = matrix(0, 100, 1)

# Store the mean CV times in a data frame
cv.times = data.frame(Ridge = mean(time.cv.ridge), Lasso = mean(time.cv.lasso), ElNet = mean(time.cv.elnet))


# Capture the residuals for one of the samples for both the test data and training data
res.ridge = matrix(0,2750)
res.lasso = matrix(0,2750)
res.elnet = matrix(0,2750)


res.ridge.train = matrix(0,11000)
res.lasso.train = matrix(0,11000)
res.elnet.train = matrix(0,11000)

for(i in 1:100){
  
  train          =  sample(1:n, ceiling(.8*n))
  X.train        =  X[train,]
  y.train        =  y[train]
  ybar.train     =  mean(y.train)
  n.train        =  nrow(X.train)
  
  X.val          =  X[-train,]
  y.val          =  y[-train]
  ybar.val       =  mean(y.val)
  n.test         =  nrow(X.val)
  
  ridge.start       =  proc.time()
  cv.ridge          =  cv.glmnet(X.train, y.train, alpha = 0)
  ridge.end         =  proc.time()
  time.cv.ridge[i]  =  ridge.end[3] - ridge.start[3]
  
  lasso.start       =  proc.time()
  cv.lasso          =  cv.glmnet(X.train, y.train, alpha = 1)
  lasso.end         =  proc.time()
  time.cv.lasso[i]  =  lasso.end[3] - lasso.start[3]
  
  elnet.start       =  proc.time()
  cv.elnet          =  cv.glmnet(X.train, y.train, alpha =.5)
  elnet.end         =  proc.time()
  time.cv.elnet[i]  =  elnet.end[3] - elnet.start[3]
  
  
  # Extract the lambda chosen by CV and fit the model
  lambda.ridge      =  cv.ridge$lambda.min
  lambda.lasso      =  cv.lasso$lambda.min
  lambda.elnet      =  cv.elnet$lambda.min
  
  ridge.fit         =  glmnet(X.train, y.train, alpha =  0, lambda = lambda.ridge)
  lasso.fit         =  glmnet(X.train, y.train, alpha =  1, lambda = lambda.lasso)
  elnet.fit         =  glmnet(X.train, y.train, alpha = .5, lambda = lambda.elnet)
  
  y.hat.ridge       =  predict(ridge.fit, newx = X.val)
  y.hat.lasso       =  predict(lasso.fit, newx = X.val)
  y.hat.elnet       =  predict(elnet.fit, newx = X.val)
  
  # Capture the residuals and plot the cross validation curves
  if(i == 5){
    res.ridge = y.val - y.hat.ridge
    res.lasso = y.val - y.hat.lasso
    res.elnet = y.val - y.hat.elnet
    
    plot(cv.ridge)
    title("CV Curve - Ridge", line = 3)
    plot(cv.lasso)
    title("CV Curve - Lasso", line = 3)
    plot(cv.elnet) 
    title("CV Curve - Elastic Net", line = 3)
  }
  
  # Store the MSE and Variance for calculation of R^2
  mse.ridge[i]   =  mean((y.val - y.hat.ridge)^2)
  mse.lasso[i]   =  mean((y.val - y.hat.lasso)^2)
  mse.elnet[i]   =  mean((y.val - y.hat.elnet)^2)
  
  var.ridge[i]   = mean((y.val - ybar.val)^2)
  var.lasso[i]   = mean((y.val - ybar.val)^2)
  var.elnet[i]   = mean((y.val - ybar.val)^2)
  
  rsq.ridge[i]   = 1 - mse.ridge[i]/var.ridge[i]
  rsq.lasso[i]   = 1 - mse.lasso[i]/var.lasso[i]
  rsq.elnet[i]   = 1 - mse.elnet[i]/var.elnet[i]
  
  #Store the training residuals
  if(i == 5){
    res.ridge.train = y.val - yhat.ridge.train
    res.lasso.train = y.val - yhat.lasso.train
    res.elnet.train = y.val - yhat.elnet.train
  }
  
  # Calculate R^2 for the training data set
  yhat.ridge.train     = predict(ridge.fit, newx = X.train)
  yhat.lasso.train     = predict(lasso.fit, newx = X.train)
  yhat.elnet.train     = predict(elnet.fit, newx = X.train)
  
  mse.ridge.train[i]   = mean((y.train - yhat.ridge.train)^2)
  mse.lasso.train[i]   = mean((y.train - yhat.ridge.train)^2)
  mse.elnet.train[i]   = mean((y.train - yhat.ridge.train)^2)
  
  var.ridge.train[i]   = mean((y.train - ybar.train)^2)
  var.lasso.train[i]   = mean((y.train - ybar.train)^2)
  var.elnet.train[i]   = mean((y.train - ybar.train)^2)
  
  rsq.ridge.train[i]   = 1 - mse.ridge.train[i, ]/var.ridge.train[i,]
  rsq.lasso.train[i]   = 1 - mse.lasso.train[i, ]/var.lasso.train[i,]
  rsq.elnet.train[i]   = 1 - mse.elnet.train[i, ]/var.elnet.train[i,]
  
}


# Random Forest Test MSE and R^2
mse.rf    = matrix(0, 100, 1)
var.rf    = matrix(0, 100, 1)
rsq.rf    = matrix(0, 100, 1)

# Random Forest Training MSE and R^2 
mse.rf.train = matrix(0,100,1)
var.rf.train = matrix(0,100,1)
rsq.rf.train = matrix(0,100,1)

# Test Residuals and Training Residuals
res.rf = matrix(0, 2750)
res.rf.train = matrix(0, 11000)

# Random Forest - put into its own loop to conserve time
for(i in 1:100){
  # Training Set
  train          =  sample(1:n, ceiling(.8*n))
  d.train        =  rf.data[train,]
  y.train        =  as.matrix(rf.data[train,41])
  ybar.train     =  mean(y.train)
  n.train        =  nrow(d.train)
  
  
  # Test Set
  d.test         =  rf.data[-train, ]
  y.test         =  as.matrix(rf.data[-train,41])
  ybar.test      =  mean(y.test)
  n.test         =  nrow(d.test)
  
  # Train a full tree on the training data
  base.forest    =  randomForest(goal~., d.train, mtry = sqrt(40))
  
  
  # Make predictions using the test data and calculate the test R^2
  pred.test      =  predict(base.forest, newdata = d.test)
  mse.rf[i]      =  mean((y.test - pred.test)^2)
  var.rf[i]      =  mean((y.test - ybar.test)^2)
  rsq.rf[i,1]    =  1 - mse.rf[i]/var.rf[i]
  
  if(i == 5){
    res.rf = pred.test - y.test
    res.rf.train = predict(base.forest, newdata = d.train) - y.train
  }
  
  # Calculate the training MSE and test R^2
  pred.train         = predict(base.forest, newdata = d.train)
  mse.rf.train[i]    = mean((y.train - pred.train)^2)
  var.rf.train[i]    = mean((y.train - ybar.train)^2)
  
  rsq.rf.train[i]    = 1 - mse.rf.train[i]/var.rf.train[i]
}



## Part 5 - model the entire dataset
model         = c("Ridge", "Lasso", "Elastic Net", "Random Forest")
time.full.fit = matrix(0,4)
rsq.5         = matrix(0,4)
rsq.95        = matrix(0,4)
rsq.5[1]      = quantile(rsq.ridge, probs=c(.05))
rsq.5[2]      = quantile(rsq.lasso, probs=c(.05))
rsq.5[3]      = quantile(rsq.elnet, probs=c(.05))
rsq.5[4]      = quantile(rsq.rf,    probs=c(.05))

rsq.95[1]     = quantile(rsq.ridge, probs=c(.95))
rsq.95[2]     = quantile(rsq.lasso, probs=c(.95))
rsq.95[3]     = quantile(rsq.elnet, probs=c(.95))
rsq.95[4]     = quantile(rsq.rf,    probs=c(.95))


# Record the time it takes to cross validate the entire data set and fit the model

#############
##  Ridge  ##
#############
start.full.ridge    = proc.time()
full.cv.ridge       = cv.glmnet(X, y, alpha = 0)
full.ridge          = glmnet(X, y, alpha = 0, lambda = full.cv.ridge$lambda.min)
end.full.ridge      = proc.time()
time.full.fit[1]    = end.full.ridge[3]-start.full.ridge[3]


#############
##  Lasso  ##
#############

start.full.lasso    = proc.time()
full.cv.lasso       = cv.glmnet(X, y, alpha = 1)
full.lass           = glmnet(X, y, alpha = 1, lambda = full.cv.lasso$lambda.min)
end.full.lasso      = proc.time()
time.full.fit[2]    = end.full.lasso[3]-start.full.lasso[3]


#############
##  Elnet  ##
#############

start.full.elnet    = proc.time()
full.cv.elnet       = cv.glmnet(X, y, alpha = 0.5)
full.elnet          = glmnet(X, y, alpha = 0.5, lambda = full.cv.elnet$lambda.min)
end.full.elnet      = proc.time()
time.full.fit[3]    = end.full.elnet[3]-start.full.elnet[3]


####################
##  Random Forest ##
####################


start.full.rf       = proc.time()
full.rf.model       = randomForest(goal~., rf.data, mtry = sqrt(40))
end.full.rf         = proc.time()
time.full.fit[4]    = end.full.rf[3]-start.full.rf[3]



# Collect the R^2 and full fit timing in a single data frame
table.df    =  data.frame(Model = model, RsqLowerQuantile = rsq.5, RsqUpperQuantile = rsq.95,
                          Time = time.full.fit)


# Put together barplots of the standardized coefficients
s = apply(X, 2, sd)

ridge.coef = as.vector(full.ridge$beta * s)
lasso.coef = as.vector(full.lass$beta  * s)
elnet.coef = as.vector(full.elnet$beta * s)


g = importance(full.rf.model)
rownames(g) <- NULL
f = seq(1:40)
VarName = colnames(X)
imp.df = data.frame(Name = VarName, Variable = f,
                    Ridge = ridge.coef, Lasso = lasso.coef,
                    Elnet = elnet.coef, Importance = g)


imp.df = arrange(imp.df, desc(Elnet))

# Set variable numbers as factor so that plotting will preserve the order of the variable
imp.df$Variable = factor(imp.df$Variable, levels =imp.df$Variable)

# Create the plots for ridge, lasso, elastic net, and random forest
ridgePlot = imp.df %>% ggplot(aes(x = as.factor(Variable), y = Ridge)) + geom_col() + 
  labs(title = "Standardized Ridge Coefficients", x = "Variable", y = "Coefficient") + 
  theme_bw()

lassoPlot = imp.df %>% ggplot(aes(x = as.factor(Variable), y = Lasso)) + geom_col()+ 
  labs(title = "Standardized Lasso Coefficients", x = "Variable", y = "Coefficient")+ 
  theme_bw()

elnetPlot = imp.df %>% ggplot(aes(x = as.factor(Variable), y = Elnet)) + geom_col()+ 
  labs(title = "Standardized Elastic Net Coefficients", x = "Variable", y = "Coefficient")+ 
  theme_bw()

rfPlot = imp.df %>% ggplot(aes(x = as.factor(Variable), y = IncNodePurity)) + geom_col()+
  labs(title = "Random Forest - Variable Importance", x = "Variable", y = "Coefficient")+ 
  theme_bw()


grid.arrange(ridgePlot, lassoPlot, elnetPlot, rfPlot, nrow = 4 )


# Part 4 - Side by Side box plots of R^2 Test and Train

rsq.test.df = data.frame(Sample = rep("Test", 400), Ridge = rsq.ridge,
                         Lasso = rsq.lasso, ElNet = rsq.elnet,
                         RandomForest = rsq.rf)

rsq.train.df = data.frame(Sample = rep("Train", 400), Ridge = rsq.ridge.train,
                          Lasso = rsq.lasso.train, ElNet = rsq.elnet.train,
                          RandomForest = rsq.rf.train)
rsq.df = rbind(rsq.test.df, rsq.train.df)


# R^2 BoxPlots
ridge.rsq.plot = rsq.df %>% ggplot(aes(x = Sample, y = Ridge)) + geom_boxplot() + 
  labs(title = "Ridge R^2", y = "R^2") + theme_bw()
lasso.rsq.plot =rsq.df %>% ggplot(aes(x = Sample, y = Lasso)) + geom_boxplot() + 
  labs(title = "Lasso R^2", y = "R^2") + theme_bw()
elnet.rsq.plot =rsq.df %>% ggplot(aes(x = Sample, y = ElNet)) + geom_boxplot() + 
  labs(title = "Elastic Net R^2", y = "R^2") + theme_bw()
rf.rsq.plot =rsq.df %>% ggplot(aes(x = Sample, y = RandomForest)) + geom_boxplot() + 
  labs(title = "Random Forest R^2", y = "R^2") + theme_bw()

grid.arrange(ridge.rsq.plot,lasso.rsq.plot,elnet.rsq.plot, rf.rsq.plot, nrow = 2, ncol =2)


# Plot the barplots of the residuals for Test and Train. 
names.res = c("Sample", "Ridge", "Lasso", "ElNet", "RandomForest")

test = rep("Test", 2750)
train = rep("Train", 11000)
res.df.test = data.frame(Sample = test,
                         Ridge  = res.ridge, 
                         Lasso  = res.lasso, 
                         ElNet  = res.elnet, 
                         RandomForest = res.rf)
colnames(res.df.test) = names.res

res.df.train = data.frame(Sample = train,
                          Ridge = res.ridge.train,
                          Lasso = res.lasso.train,
                          ElNet = res.elnet.train,
                          RandomForest = res.rf.train)
colnames(res.df.train) = names.res

residuals.df = rbind(res.df.test, res.df.train)
colnames(residuals.df) = names.res

# Residual Box Plots

ridge.box = residuals.df %>% ggplot(aes(x = Sample, y = Ridge)) + geom_boxplot() + 
  labs(title="Ridge Residuals", y = "Residuals") + theme_bw()
lasso.box = residuals.df %>% ggplot(aes(x = Sample, y = Lasso)) + geom_boxplot() + 
  labs(title="Lasso Residuals", y = "Residuals")+ theme_bw()
elnet.box = residuals.df %>% ggplot(aes(x = Sample, y = ElNet)) + geom_boxplot() +
  labs(title="Elastic Net Residuals", y = "Residuals")+ theme_bw()
rf.box = residuals.df %>% ggplot(aes(x = Sample, y = RandomForest)) + geom_boxplot() +
  labs(title="Random Forest Residuals", y = "Residuals")+ theme_bw()
grid.arrange(ridge.box, lasso.box, elnet.box, rf.box, nrow = 2, ncol =2)


© 2021 GitHub, Inc.
Terms
Privacy
Security
Status
Docs
Contact GitHub
Pricing
API
Training
Blog
About
