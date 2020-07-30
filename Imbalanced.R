
library(imbalance)

banana_data <- banana
View(banana_data)
names(banana)
nrow(banana)
library(dplyr)
banana %>% group_by(Class)%>% count()

library(caret)
#Basic classifiers

rows <- createDataPartition(banana_data$Class,p=0.8,list = FALSE)

train_banana <- banana_data[rows,]
test_banana <- banana_data[-rows,]

fitControl <- trainControl(method = "repeatedcv", 
                           number = 4, repeats = 4,classProbs = TRUE)
library(gbm)
gbmFit1 <- train(Class ~., data = train_banana, 
                 method = "gbm", trControl = fitControl,verbose = FALSE)

p_gbm <-predict(gbmFit1,test_banana[,-3],type = "raw")

confusionMatrix(p_gbm,test_banana$Class)


#ADA BOOST
library(fastAdaboost)
ada_model<- adaboost(Class~.,train_banana,10)

ada_pred <- predict(ada_model,test_banana[,-3])

confusionMatrix(ada_pred$class,test_banana[,3])

library(randomForest)

rf_model <- train(Class ~., data = train_banana, 
                 method = "rf", trControl = fitControl,verbose = FALSE)
rf_pred <- predict(rf_model,test_banana[,-3])
confusionMatrix(rf_pred,test_banana[,3])

#svm
library(e1071)
svm_model <- svm(Class~.,data = train_banana)
svm_pred <- predict(svm_model,test_banana[,-3])
confusionMatrix(svm_pred,test_banana[,3])


#AFTER SAMPLING TECHNIQUES

library(DMwR)

smote_train <- SMOTE(Class~.,train_banana,perc.over = 600,k=5,perc.under = 300)
nrow(smote_train)

gbmFit1 <- train(Class ~., data = smote_train, 
                 method = "gbm", trControl = fitControl,verbose = FALSE)

p_gbm <-predict(gbmFit1,test_banana[,-3],type = "raw")

confusionMatrix(p_gbm,test_banana$Class)


#ADA BOOST
library(fastAdaboost)
ada_model<- adaboost(Class~.,smote_train,10)

ada_pred <- predict(ada_model,test_banana[,-3])

confusionMatrix(ada_pred$class,test_banana[,3])

library(randomForest)

rf_model <- train(Class ~., data = smote_train, 
                  method = "rf", trControl = fitControl,verbose = FALSE)
rf_pred <- predict(rf_model,test_banana[,-3])
confusionMatrix(rf_pred,test_banana[,3])

#svm
library(e1071)
svm_model <- svm(Class~.,data = smote_train)
svm_pred <- predict(svm_model,test_banana[,-3])
confusionMatrix(svm_pred,test_banana[,3])



