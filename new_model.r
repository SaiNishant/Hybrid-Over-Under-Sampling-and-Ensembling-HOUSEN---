library(imbalance)
library(smotefamily)
library(ROSE)
library(caret)
library(gbm)
library(fastAdaboost)
library(pROC)
library(e1071)

View(banana)
data<-banana
target<-(data$Class)
db.smote<-DBSMOTE(data[,-3], target = target )

banana_dbsmote<-db.smote$data

hybrid_data<- ovun.sample(class ~ .,data = banana_dbsmote, method = "under")

hybridized<-hybrid_data$data

hybridized$class<-as.factor(hybridized$class)

rows <- createDataPartition(hybridized$class,p=0.7,list = FALSE)

train <- hybridized[rows,]
test <- hybridized[-rows,]
fitControl <- trainControl(method = "repeatedcv",  number = 10, repeats = 5,classProbs = TRUE)

ada_model<- adaboost(class~.,train,20)
ada_pred <- predict(ada_model,test)
a<-confusionMatrix(ada_pred$class,test$class)
a


rf_model <- train(class ~., data = train,method = "rf", trControl = fitControl,verbose = FALSE)
rf_pred <- predict(rf_model,test)
r<-confusionMatrix(rf_pred,test$class)
r


gbmFit1 <- train(class ~., data = train, method = "gbm", trControl = fitControl,verbose = FALSE)
p_gbm <-predict(gbmFit1,test,type = "raw")
confusionMatrix(p_gbm,test$class)

svm_model <- svm(class~.,data = train)
svm_pred <- predict(svm_model,test)
confusionMatrix(svm_pred,test$class)

pred_majority<-as.factor(ifelse
                         (rf_pred == "positive" & ada_pred == "positive" , "positive" ,
                           ifelse(rf_pred == "positive" & p_gbm == "positive" , "positive" ,
                                  ifelse( ada_pred == "positive" & p_gbm == "positive" , "positive" ,"negative"))))

confusionMatrix(pred_majority,test$class)

####################################################################################################

library(imbalance)
library(smotefamily)
library(ROSE)
library(caret)
library(gbm)
View(haberman)
data<- haberman
target<-(data$Class)
db.smote<-DBSMOTE(data[,-4], target = target )

haberman_dbsmote<-db.smote$data

hybrid_data<- ovun.sample(class~.,data = haberman_dbsmote, method = "under")

hybridized<-hybrid_data$data

#View(hybridized)
hybridized$class<-as.factor(hybridized$class)

rows <- createDataPartition(hybridized$class,p=0.80,list = FALSE)

train <- hybridized[rows,]
test <- hybridized[-rows,]
fitControl <- trainControl(method = "repeatedcv", 
                           number = 20, repeats = 10,classProbs = TRUE)
test$class<-as.factor(test$class)
ada_model<- adaboost(class~.,train,20)
ada_pred <- predict(ada_model,test)
a1<-confusionMatrix(ada_pred$class,test$class)
a1

rf_model <- train(class ~., data = train,method = "rf", trControl = fitControl,verbose = FALSE)
rf_pred <- predict(rf_model,test)
r<-confusionMatrix(rf_pred,test$class)
r


gbmFit1 <- train(class ~., data = train, method = "gbm", trControl = fitControl,verbose = FALSE)
p_gbm <-predict(gbmFit1,test,type = "raw")
confusionMatrix(p_gbm,test$class)

svm_model <- svm(class~.,data = train)
svm_pred <- predict(svm_model,test)
confusionMatrix(svm_pred,test$class)

pred_majority<-as.factor(ifelse(rf_pred == "positive" & ada_pred == "positive" , "positive" ,ifelse(rf_pred == "positive" & p_gbm == "positive" , "positive" ,ifelse( ada_pred == "positive" & p_gbm == "positive" , "positive" ,"negative"))))

confusionMatrix(pred_majority,test$class)

##############################################################################################################

library(imbalance)
library(smotefamily)
library(ROSE)
library(caret)
library(gbm)
View(glass0)
data<-glass0
target<-(data$Class)
db.smote<-DBSMOTE(data[,-10], target = target )

glass_dbsmote<-db.smote$data

hybrid_data<- ovun.sample(class~.,data = glass_dbsmote, method = "under")

hybridized<-hybrid_data$data

hybridized$class<-as.factor(hybridized$class)

rows <- createDataPartition(hybridized$class,p=0.80,list = FALSE)

train <- hybridized[rows,]
test <- hybridized[-rows,]
fitControl <- trainControl(method = "repeatedcv", 
                           number = 10, repeats = 5,classProbs = TRUE)

ada_model<- adaboost(class~.,train,10)
ada_pred <- predict(ada_model,test)
a<-confusionMatrix(ada_pred$class,test$class)
a

rf_model <- train(class ~., data = train,method = "rf", trControl = fitControl,verbose = FALSE)
rf_pred <- predict(rf_model,test)
r<-confusionMatrix(rf_pred,test$class)
r

gbmFit1 <- train(class ~., data = train, method = "gbm", trControl = fitControl,verbose = FALSE)
p_gbm <-predict(gbmFit1,test,type = "raw")
confusionMatrix(p_gbm,test$class)

svm_model <- svm(class~.,data = train)
svm_pred <- predict(svm_model,test)
confusionMatrix(svm_pred,test$class)
pred_majority<-as.factor(ifelse(rf_pred == "positive" & ada_pred == "positive" , "positive" ,ifelse(rf_pred == "positive" & p_gbm == "positive" , "positive" ,ifelse( ada_pred == "positive" & p_gbm == "positive" , "positive" ,"negative"))))

confusionMatrix(pred_majority,test$class)

##################################################################################################

library(imbalance)
library(smotefamily)
library(ROSE)
library(caret)
library(gbm)
View(yeast4)
data<- yeast4
target<-(data$Class)
data$Erl<-as.numeric(data$Erl)
db.smote<-DBSMOTE(data[,1:8], target = target )

yeast_dbsmote<-db.smote$data

hybrid_data<- ovun.sample(class~.,data = yeast_dbsmote, method = "under")

hybridized<-hybrid_data$data

#View(hybridized)
hybridized$class<-as.factor(hybridized$class)

rows <- createDataPartition(hybridized$class,p=0.80,list = FALSE)

train <- hybridized[rows,]
test <- hybridized[-rows,]
fitControl <- trainControl(method = "repeatedcv", 
                           number = 10, repeats = 4,classProbs = TRUE)
test$class<-as.factor(test$class)
ada_model<- adaboost(class~.,train,10)
ada_pred <- predict(ada_model,test)
a1<-confusionMatrix(ada_pred$class,test$class)
a1


rf_model <- train(class ~., data = train,method = "rf", trControl = fitControl,verbose = FALSE)
rf_pred <- predict(rf_model,test)
r<-confusionMatrix(rf_pred,test$class)
r


gbmFit1 <- train(class ~., data = train, method = "gbm", trControl = fitControl,verbose = FALSE)
p_gbm <-predict(gbmFit1,test,type = "raw")
confusionMatrix(p_gbm,test$class)

svm_model <- svm(class~.,data = train)
svm_pred <- predict(svm_model,test)
confusionMatrix(svm_pred,test$class)

pred_majority<-as.factor(ifelse(rf_pred == "positive" & ada_pred == "positive" , "positive" ,ifelse(rf_pred == "positive" & p_gbm == "positive" , "positive" ,ifelse( ada_pred == "positive" & p_gbm == "positive" , "positive" ,"negative"))))

confusionMatrix(pred_majority,test$class)


#################################################################################################

library(imbalance)
library(smotefamily)
library(ROSE)
library(caret)
library(gbm)
View(ecoli1)
data<- ecoli1
target<-(data$Class)

data$Lip<-as.numeric(data$Lip)
data$Chg<-as.numeric(data$Chg)

db.smote<-DBSMOTE(data[,-8], target = target )

ecoli_dbsmote<-db.smote$data

hybrid_data<- ovun.sample(class~.,data = ecoli_dbsmote, method = "under")

hybridized<-hybrid_data$data

#View(hybridized)
hybridized$class<-as.factor(hybridized$class)

rows <- createDataPartition(hybridized$class,p=0.75,list = FALSE)

train <- hybridized[rows,]
test <- hybridized[-rows,]

fitControl <- trainControl(method = "repeatedcv", 
                           number = 10, repeats = 10,classProbs = TRUE)
test$class<-as.factor(test$class)

ada_model<- adaboost(class~.,train,20)
ada_pred <- predict(ada_model,test)
a1<-confusionMatrix(ada_pred$class,test$class)
a1

rf_model <- train(class ~., data = train,method = "rf", trControl = fitControl,verbose = FALSE)
rf_pred <- predict(rf_model,test)
r<-confusionMatrix(rf_pred,test$class)
r


gbmFit1 <- train(class ~., data = train, method = "gbm", trControl = fitControl,verbose = FALSE)
p_gbm <-predict(gbmFit1,test,type = "raw")
confusionMatrix(p_gbm,test$class)

svm_model <- svm(class~.,data = train)
svm_pred <- predict(svm_model,test)
confusionMatrix(svm_pred,test$class)

pred_majority<-as.factor(ifelse(rf_pred == "positive" & ada_pred == "positive" , "positive" ,ifelse(rf_pred == "positive" & p_gbm == "positive" , "positive" ,ifelse( ada_pred == "positive" & p_gbm == "positive" , "positive" ,"negative"))))

confusionMatrix(pred_majority,test$class)


######################################################################################


