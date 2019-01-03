#Question 1
library('ISLR')
library('caTools')
library("Rmisc")
library("lattice")
library("plyr")
data1<-College
str(data1)

set.seed(123)
#Splitting training and test data in 80:20 ratio
sample = sample.split(data1,SplitRatio = 0.80)
train1 =subset(data1,sample ==TRUE) 
na.omit(train1)
test1=subset(data1,sample==FALSE)
na.omit(test1)
names(test1)
#Fitting least square model
lsq<-lm(Apps~.,data=train1)
predict_variables=test1[,c("Private","Accept","Enroll","Top10perc","Top25perc","F.Undergrad","P.Undergrad","Outstate","Room.Board","Books","Personal","PhD","Terminal","S.F.Ratio","perc.alumni","Expend","Grad.Rate" )]
prediction_lsq<-predict(lsq,predict_variables)

#RMSE
library('Metrics')
error_lsq_model<-RMSE(test1$Apps,prediction_lsq)
print(error_lsq_model)
#1385.618

#Ridge
mat_train <- model.matrix(Apps ~ ., data = train1)
mat_test <- model.matrix(Apps ~ ., data = test1)
grid = 10^seq(10, -2, length=100)
plot(grid,type = 'l')

#Cross Validation
cv_ridge_model  <- cv.glmnet(mat_train, train1[, "Apps"], alpha=0)
quartz()
plot(cv_ridge_model)
names(cv_ridge_model)
print(cv_ridge_model$lambda.min)

#Ridge Model
ridge_model <- glmnet(mat_train, train1$Apps, alpha = 0, lambda = grid)
names(ridge_model)
coefficients(ridge_model)

#Prediction
ridge_predict <- predict(ridge_model, newx = mat_test, s = cv_ridge_model$lambda.min)

#RMSE for ridge
error_ridge<-rmse(test1$Apps,ridge_predict) 
print(error_ridge)  
#1353.271

#LASSO
#Cross Validation
cv_lasso_model  <- cv.glmnet(mat_train, train1[, "Apps"], alpha=1)
lasso_model <- glmnet(mat_train, train1$Apps, alpha = 1, lambda = grid)
lasso_predict <- predict(lasso_model, newx = mat_test, s = cv_lasso_model$lambda.min)
error_lasso<-rmse(test1$Apps,lasso_predict) 
print(error_lasso)
#1380.748



