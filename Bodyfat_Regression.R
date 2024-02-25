### Read the data 
library(dplyr) 
library(ggplot2) 
library(reshape2) 
library(corrplot) 
library(outliers) 
library(DAAG) 

### Suppose you save the data file ‘‘fat.csv" in the folder ‘‘C://Temp" of your laptop, 
fat <- read.table(file = "fat.csv", sep=",", header=TRUE); 

### Split the data as in Part (a) 
n = dim(fat)[1]; ### total number of observations 
n1 = round(n/10); ### number of observations randomly selected for testing data 

#Check for NAs 
summary(fat) 

#no NAs 

#multicorrlineary 

corr_mat <- round(cor(fat),2); 

corrplot(corr_mat, method="shade") 



x1 = fat$free 

x2 = fat$height 

x3 = fat$ankle 



x4 = fat$siri 

x5 = fat$density 

x6 = fat$abdom 



y = fat$brozek 



EDA_lm <- lm(brozek ~., data = fat) 

vif(EDA_lm) 



#VIF shows that age, height, neck, thigh, knee, ankle, biceps, forearm, and wrist  

#show moderate to low levels of multicolinnarity, while all other variables are show high levels. 

#The modelling process will remove the highly correlated variables. 







#weak corralated maps 

plot(x1, y, main="Free and Brozek", xlab="Free", ylab="brozek") 

plot(x2, y, main="Height and Brozek", xlab="Height", ylab="brozek") 

plot(x3, y, main="Ankle and Brozek", xlab="Ankle", ylab="brozek") 





#strongest correlated 

plot(x4, y, main="Siri and Brozek", xlab="Siri", ylab="brozek") 

plot(x5, y, main="Density and Brozek", xlab="Density", ylab="brozek") 

plot(x6, y, main="Abdom and Brozek", xlab="Abdom", ylab="brozek") 



#Search for outliers 

boxplot(y) 

max(y) 



grubbs.test(y) 

#The highest value could be considered an outlier.  

#Since we have few observations it is more beneficial to keep the observation included.  

#the p-value of .08146 means the null hypothesis is rejected if the threshold is .05, but it is accepted if null is .1. 

#For these two reasons the observation will remain.  









## To fix our ideas, let the following 25 rows of data as the testing subset: 

flag = c(1, 21, 22, 57, 70, 88, 91, 94, 121, 127, 149, 151, 159, 162, 
         
         164, 177, 179, 194, 206, 214, 215, 221, 240, 241, 243); 

fat1train = fat[-flag,]; 

fat1test = fat[flag,]; 





###In Part (b)-(d), Please see the R code for linear regression at Canvas. 

ytrue    <- fat1test$brozek 

#Linear Regression Model 

MSEtrain <- NULL; 

MSEtest  <- NULL; 



lm_model <- lm(brozek ~., data = fat1train) 

summary(lm_model) 



## Model 1: Training error 

MSEmod1train <-   mean( (resid(lm_model) )^2); 

MSEtrain <- c(MSEtrain, MSEmod1train); 

# Model 1: testing error  

pred1a <- predict(lm_model, fat1test[,-1]); 

MSEmod1test <- mean((pred1a - ytrue)^2); 

MSEmod1test 

MSEtest <- c(MSEtest, MSEmod1test);  









#s <- fat1train$siri+fat1train$weight+ fat1train$free+ fat1train$knee +fat1train$thigh 



#Model 2: Best subset k = 5 

library(leaps) 

leaps <- regsubsets(brozek ~ ., data= fat1train, nbest= 100, really.big= TRUE);  



## Record useful information from the output 
models <- summary(leaps)$which; 

models.size <- as.numeric(attr(models, "dimnames")[[1]]); 

models.rss <- summary(leaps)$rss; 



## 2A:  The following are to show the plots of all subset models  

##   and the best subset model for each subset size k  

plot(models.size, models.rss);  

## find the smallest RSS values for each subset size  

models.best.rss <- tapply(models.rss, models.size, min);  

## Also add the results for the only intercept model 

model0 <- lm( brozek ~ 1, data = fat1train);  

models.best.rss <- c( sum(resid(model0)^2), models.best.rss);  

## plot all RSS for all subset models and highlight the smallest values  

plot( 0:8, models.best.rss, type = "b", col= "red", xlab="Subset Size k", ylab="Residual Sum-of-Square") 

points(models.size, models.rss) 



# 2B: What is the best subset with k=5 

op2 <- which(models.size == 5);  

flag2 <- op2[which.min(models.rss[op2])];  

flag2 

## we can auto-find the best subset with k=5 

##   this way will be useful when doing cross-validation  

mod2selectedmodel <- models[flag2,];  

mod2selectedmodel 

mod2Xname <- paste(names(mod2selectedmodel)[mod2selectedmodel][-1], collapse="+");  

mod2Xname 

mod2form <- paste ("brozek ~", mod2Xname); 



## To auto-fit the best subset model with k=5 to the data 

model2 <- lm( as.formula(mod2form), data= fat1train);  

# Model 2: training error  

MSEmod2train <- mean(resid(model2)^2); 

## save this training error to the overall training error vector  

MSEtrain <- c(MSEtrain, MSEmod2train); 

MSEtrain 

## Model 2:  testing error  

pred2 <- predict(model2, fat1test[,-1]); 

MSEmod2test <-   mean((pred2 - ytrue)^2); 

MSEtest <- c(MSEtest, MSEmod2test); 

MSEtest 



#Model 3 Step wise regression 

intercept_only <- lm(brozek ~ 1, data=fat1train) 



#Forwards 

forward <- step(intercept_only, scope = formula(lm_model), direction='forward', trace=0) 

forward$anova 

forward$coefficients 



MSforwardstrain <- mean(resid(forward)^2); 

pred3 <- predict(forward, fat1test[,-1]); 

MSforwardstest <-   mean((pred3 - ytrue)^2); 

MSforwardstest 

#Backwards  

backward <- step(lm_model, scope = formula(lm_model), direction='backward', trace=0) 

backward$anova 

backward$coefficients 



MSbackwardstrain <- mean(resid(backward)^2); 

MSEmod3train <- min(c(MSbackwardstrain, MSforwardstrain)) 

pred3 <- predict(backward, fat1test[,-1]); 

MSbackwardstest <-   mean((pred3 - ytrue)^2); 

MSEmod3test <- min(c(MSbackwardstest, MSforwardstest)) 

MSbackwardstest 



MSEtrain <- c(MSEtrain, MSEmod3train); 

MSEtrain 

MSEtest <- c(MSEtest, MSEmod3test); 

MSEtrain;  



#Rdige Regression 

library(MASS) 

ridge <- lm.ridge( brozek ~ ., data = fat1train, lambda= seq(0,10,0.001)); 



plot(ridge) 

matplot(ridge$lambda, t(ridge$coef), type="l", lty=1, xlab=expression(lambda), ylab=expression(hat(beta))) 



indexopt <-  which.min(ridge$GCV);  

indexopt 

ridge$coef[,indexopt] 

## However, this coefficeints are for the the scaled/normalized data  

##      instead of original raw data  

## We need to transfer to the original data  

## Y = X \beta + \epsilon, and find the estimated \beta value  

##        for this "optimal" Ridge Regression Model 

## For the estimated \beta, we need to sparate \beta_0 (intercept) with other \beta's 

ridge.coeffs = ridge$coef[,indexopt]/ ridge$scales; 

intercept = -sum( ridge.coeffs  * colMeans(fat1train[,-1] )  )+ mean(fat1train[,1]); 

## If you want to see the coefficients estimated from the Ridge Regression 

##   on the original data scale 

c(intercept, ridge.coeffs); 



## Model 4 (Ridge): training errors  

yhat4train <- as.matrix( fat1train[,-1]) %*% as.vector(ridge.coeffs) + intercept; 

MSEmod4train <- mean((yhat4train - fat1train$brozek)^2);  

MSEtrain <- c(MSEtrain, MSEmod4train);  

MSEtrain 

## Model 4 (Ridge):  testing errors in the subset "test"  

pred4test <- as.matrix( fat1test[,-1]) %*% as.vector(ridge.coeffs) + intercept 

MSEmod4test <-  mean((pred4test - ytrue)^2);  

MSEtest <- c(MSEtest, MSEmod4test); 

MSEtest 



#Model 5: Lasso 

library(lars) 

lars <- lars( as.matrix(fat1train[,-1]), fat1train[,1], type= "lasso", trace= TRUE); 

## 5A: some useful plots for LASSO for all penalty parameters \lambda  

plot(lars) 





Cp1  <- summary(lars)$Cp 



index1 <- which.min(Cp1) 

index1 

lasso.coeffs <- lars$beta[index1,] 



lasso.lambda <- lars$lambda[index1] 



LASSOintercept = mean(fat1train[,1]) -sum( lars$beta[index1,]  * colMeans(fat1train[,-1] )); 

c(LASSOintercept, lars$beta[index1,])s 





## Model 5:  training error for lasso 

##  

pred5train  <- predict(lars, as.matrix(fat1train[,-1]), s=lasso.lambda, type="fit", mode="lambda"); 

yhat5train <- pred5train$fit;  

MSEmod5train <- mean((yhat5train - fat1train$brozek)^2);  

MSEtrain <- c(MSEtrain, MSEmod5train);  

MSEtrain 



## Model 5:  training error for lasso   

pred5test <- predict(lars, as.matrix(fat1test[,-1]), s=lasso.lambda, type="fit", mode="lambda"); 

yhat5test <- pred5test$fit;  

MSEmod5test <- mean( (yhat5test - fat1test$brozek)^2);  

MSEtest <- c(MSEtest, MSEmod5test);  

MSEtest; 



#Model 6 PCA: 

library(pls) 

pca <- pcr(brozek~., data=fat1train, validation="CV");   

validationplot(pca); 

summary(pca); 



ncompopt <- which.min(pca$validation$adj); 

pca$validation$adj 



ypred6train <- predict(pca, ncomp = ncompopt, newdata = fat1train[-1]);  



## 6B(iv) Training Error with the optimal choice of PCs 

MSEmod6train <- mean( (ypred6train - fat1train$brozek)^2);  

MSEtrain <- c(MSEtrain, MSEmod6train);  

MSEtrain; 

## 6B(v) Testing Error with the optimal choice of PCs 

ypred6test <- predict(pca, ncomp = ncompopt, newdata = fat1test[,-1]);  

MSEmod6test <- mean( (ypred6test - fat1test$brozek)^2);  

MSEtest <- c(MSEtest, MSEmod6test);  

MSEtest; 

MSEmod6test 





### Model 7. Partial Least Squares (PLS) Regression  

### 

###  The idea is the same as the PCR and can be done by "pls" package 

###  You need to call the fuction "plsr"  if you the code standalone  

#  library(pls) 

pls <- plsr(brozek ~ ., data = fat1train, validation="CV"); 



### 7(i) auto-select the optimal # of components of PLS  

## choose the optimal # of components   

mod7ncompopt <- which.min(pls$validation$adj); 

## The opt # of components, it turns out to be 8 for this dataset, 

##       and thus PLS also reduces to the full model!!!     

validationplot(pls) 

# 7(ii) Training Error with the optimal choice of "mod7ncompopt"  

# note that the prediction is from "pls" with "mod7ncompopt"  

ypred7train <- predict(pls, ncomp = mod7ncompopt, newdata = fat1train[,-1]);  

MSEmod7train <- mean( (ypred7train - fat1train$brozek)^2);  

MSEtrain <- c(MSEtrain, MSEmod7train);  



## 7(iii) Testing Error with the optimal choice of "mod7ncompopt"  

ypred7test <- predict(pls, ncomp = mod7ncompopt, newdata = fat1test[,-1]);  

MSEmod7test <- mean( (ypred7test - fat1test$brozek)^2);  

MSEtest <- c(MSEtest, MSEmod7test);  



## Check your answers 

MSEtrain 

## Training errors of these 7 models/methods 



MSEtest 





### Part (e): the following R code might be useful, and feel free to modify it. 

### save the TE values for all models in all $B=100$ loops 

B= 100; ### number of loops 

TEALL = NULL; ### Final TE values 

set.seed(7406); ### You might want to set the seed for randomization 

for (b in 1:B){ 
  
  ### randomly select 25 observations as testing data in each loop 
  
  flag <- sort(sample(1:n, n1)); 
  
  fattrain <- fat[-flag,]; 
  
  fattest <- fat[flag,]; 
  
  ### you can write your own R code here to first fit each model to "fattrain" 
  
  ### then get the testing error (TE) values on the testing data "fattest" 
  
  ### Suppose that you save the TE values for these five models as 
  
  ### te1, te2, te3, te4, te5, te6, te7, respectively, within this loop 
  
  ### Then you can save these 5 Testing Error values by using the R code 
  
  ### 
  
  lm_model <- lm(brozek ~., data = fattrain) 
  
  
  
  # Model 1: testing error  
  
  pred1a <- predict(lm_model, fattest[,-1]); 
  
  te1 <- mean((pred1a - ytrue)^2); 
  
  
  
  #Model 2: Best subset k = 5 
  
  library(leaps) 
  
  leaps <- regsubsets(brozek ~ ., data= fattrain, nbest= 100, really.big= TRUE);  
  
  
  
  ## Record useful information from the output 
  
  models <- summary(leaps)$which; 
  
  models.size <- as.numeric(attr(models, "dimnames")[[1]]); 
  
  models.rss <- summary(leaps)$rss; 
  
  
  
  
  
  models.best.rss <- tapply(models.rss, models.size, min);  
  
  ## Also add the results for the only intercept model 
  
  model0 <- lm( brozek ~ 1, data = fattrain);  
  
  models.best.rss <- c( sum(resid(model0)^2), models.best.rss);  
  
  
  
  # 2B: What is the best subset with k=5 
  
  op2 <- which(models.size == 5);  
  
  flag2 <- op2[which.min(models.rss[op2])];  
  
  ## we can auto-find the best subset with k=5 
  
  
  
  mod2selectedmodel <- models[flag2,];  
  
  mod2Xname <- paste(names(mod2selectedmodel)[mod2selectedmodel][-1], collapse="+");  
  
  mod2form <- paste ("brozek ~", mod2Xname); 
  
  ## To auto-fit the best subset model with k=5 to the data 
  
  model2 <- lm( as.formula(mod2form), data= fattrain);  
  
  
  
  ## Model 2:  testing error  
  
  pred2 <- predict(model2, fattest[,-1]); 
  
  te2 <-   mean((pred2 - ytrue)^2); 
  
  
  
  #Model 3 Step wise regression 
  
  intercept_only <- lm(brozek ~ 1, data=fattrain) 
  
  
  
  #Forwards 
  
  forward <- step(intercept_only, scope = formula(lm_model), direction='forward', trace=0) 
  
  
  
  pred3 <- predict(forward, fattest[,-1]); 
  
  MSforwardstest <- mean((pred3 - ytrue)^2); 
  
  
  
  #Backwards  
  
  backward <- step(lm_model, scope = formula(lm_model), direction='backward', trace=0) 
  
  
  
  pred3 <- predict(backward, fattest[,-1]) 
  
  MSbackwardstest <-  mean((pred3 - ytrue)^2) 
  
  te3 <- min(c(MSbackwardstest, MSforwardstest)) 
  
  
  
  #Rdige Regression 
  
  library(MASS) 
  
  ridge <- lm.ridge( brozek ~ ., data = fattrain, lambda= seq(0,100,0.001)); 
  
  indexopt <-  which.min(ridge$GCV);  
  
  
  
  ridge.coeffs = ridge$coef[,indexopt]/ ridge$scales; 
  
  intercept = -sum( ridge.coeffs  * colMeans(fattrain[,-1] )  )+ mean(fattrain[,1]); 
  
  
  
  
  
  ## Model 4 (Ridge):  testing errors in the subset "test"  
  
  pred4test <- as.matrix( fattest[,-1]) %*% as.vector(ridge.coeffs) + intercept 
  
  MSEmod4test <-  mean((pred4test - ytrue)^2);  
  
  te4 <- c(MSEtest, MSEmod4test); 
  
  
  
  #Model 5: Lasso 
  
  library(lars) 
  
  lars <- lars( as.matrix(fattrain[,-1]), fattrain[,1], type= "lasso", trace= TRUE); 
  
  ## 5A: some useful plots for LASSO for all penalty parameters \lambda  
  
  
  
  Cp1  <- summary(lars)$Cp 
  
  index1 <- which.min(Cp1) 
  
  lasso.coeffs <- lars$beta[index1,] 
  
  lasso.lambda <- lars$lambda[index1] 
  
  LASSOintercept = mean(fattrain[,1]) -sum( lars$beta[index1,]  * colMeans(fattrain[,-1] )); 
  
  c(LASSOintercept, lars$beta[index1,]) 
  
  
  
  ## Model 5:  training error for lasso   
  
  pred5test <- predict(lars, as.matrix(fattest[,-1]), s=lasso.lambda, type="fit", mode="lambda"); 
  
  yhat5test <- pred5test$fit;  
  
  MSEmod5test <- mean( (yhat5test - fattest$brozek)^2);  
  
  te5 <- c(MSEtest, MSEmod5test);  
  
  
  
  
  
  #Model 6 PCA: 
  
  library(pls) 
  
  pca <- pcr(brozek~., data=fattrain, validation="CV");   
  
  
  
  ncompopt <- which.min(pca$validation$adj); 
  
  pca$validation$adj 
  
  
  
  ypred6train <- predict(pca, ncomp = ncompopt, newdata = fattrain[-1]);  
  
  
  
  ## 6B(iv) Training Error with the optimal choice of PCs 
  
  MSEmod6train <- mean( (ypred6train - fattrain$brozek)^2);  
  
  MSEtrain <- c(MSEtrain, MSEmod6train);  
  
  MSEtrain; 
  
  ## 6B(v) Testing Error with the optimal choice of PCs 
  
  ypred6test <- predict(pca, ncomp = ncompopt, newdata = fattest[,-1]);  
  
  MSEmod6test <- mean( (ypred6test - fattest$brozek)^2);  
  
  te6 <- c(MSEtest, MSEmod6test);  
  
  
  
  ### Model 7. Partial Least Squares (PLS) Regression  
  
  pls <- plsr(brozek ~ ., data = fattrain, validation="CV"); 
  
  
  
  mod7ncompopt <- which.min(pls$validation$adj); 
  
  
  
  ## 7(iii) Testing Error with the optimal choice of "mod7ncompopt"  
  
  ypred7test <- predict(pls, ncomp = mod7ncompopt, newdata = fattest[,-1]);  
  
  MSEmod7test <- mean( (ypred7test - fattest$brozek)^2);  
  
  te7 <- c(MSEtest, MSEmod7test);  
  
  
  
  TEALL = rbind( TEALL, cbind(te1, te2, te3, te4, te5, te6, te7) ); 
  
} 

dim(TEALL); ### This should be a Bx7 matrices 

### if you want, you can change the column name of TEALL 

colnames(TEALL) <- c("Regression", "k=5", "Stepwise", "Ridge", "Lasso", "PCA", "LS"); 

## You can report the sample mean and sample variances for the seven models 

apply(TEALL, 2, mean)

apply(TEALL, 2, var)



### END ### 