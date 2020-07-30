
library(imbalance)

banana_data <- haberman

nrow(banana_data)
View(banana_data)
names(banana_data)
nrow(banana_data)
library(dplyr)
banana_data %>% group_by(Class)%>% count()

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

p_gbm <-predict(gbmFit1,test_banana[,-4],type = "raw")

confusionMatrix(p_gbm,test_banana$Class)


#ADA BOOST
library(fastAdaboost)
ada_model<- adaboost(Class~.,train_banana,10)

ada_pred <- predict(ada_model,test_banana[,-4])

confusionMatrix(ada_pred$class,test_banana[,4])

library(randomForest)

rf_model <- train(Class ~., data = train_banana, 
                  method = "rf", trControl = fitControl,verbose = FALSE)
rf_pred <- predict(rf_model,test_banana[,-4])
confusionMatrix(rf_pred,test_banana[,4])

test_banana %>% group_by(Class)%>% count

#svm
library(e1071)

svm_model <- svm(Class~.,data = train_banana)
svm_pred <- predict(svm_model,test_banana[,-4])
confusionMatrix(svm_pred,test_banana[,4])


#AFTER SAMPLING TECHNIQUES

library(smotefamily)

smote_train <- SMOTE(train_banana[,-4],train_banana$Class, K=5,dup_size = 1)

smote_train <- smote_train$data
nrow(smote_train)
smote_train %>% group_by(class)%>% count()

gbmFit1 <- train(class ~., data = smote_train, 
                 method = "gbm", trControl = fitControl,verbose = FALSE)

p_gbm <-predict(gbmFit1,test_banana[,-4],type = "raw")

confusionMatrix(p_gbm,test_banana$Class)


#ADA BOOST
library(fastAdaboost)
ada_model<- adaboost(class~.,smote_train,10)

ada_pred <- predict(ada_model,test_banana[,-4])

confusionMatrix(ada_pred$class,test_banana[,4])

library(randomForest)

rf_model <- train(class ~., data = smote_train, 
                  method = "rf", trControl = fitControl,verbose = FALSE)
rf_pred <- predict(rf_model,test_banana[,-4])
confusionMatrix(rf_pred,test_banana[,4])

#svm
library(e1071)
svm_model <- svm(class~.,data = smote_train)
svm_pred <- predict(svm_model,test_banana[,-4])
confusionMatrix(svm_pred,test_banana[,4])






#ADASYN

library(smotefamily)

x<- train_banana[,-4]
class(x)
target <- factor(train_banana[,4])

nrow(train_banana)

ADAS.data<- ADAS(x,target = target,6)
class(ADAS.data)
View(ADAS.data)

ADAS.data <- ADAS.data$data
nrow(ADAS.data)
View(ADAS.data)

names(ADAS.data)

#change target value column names
ADAS.data$class<- as.factor(ADAS.data$class)

smote_train<-ADAS.data

smote_train %>% group_by(class)%>% count()


library(caret)

library(gbm)
gbmFit1 <- train(class ~., data = smote_train, 
                 method = "gbm", trControl = fitControl,verbose = FALSE)

p_gbm <-predict(gbmFit1,test_banana[,-4],type = "raw")

confusionMatrix(p_gbm,test_banana$Class)


#ADA BOOST
library(fastAdaboost)
ada_model<- adaboost(class~.,smote_train,10)

test<- test_banana
ada_pred <- predict(ada_model,test[,-4])

confusionMatrix(ada_pred$class,test[,4])

library(randomForest)



rf_model <- train(class ~., data = smote_train, 
                  method = "rf", trControl = fitControl,verbose = FALSE)
rf_pred <- predict(rf_model,test_banana[,-4])
confusionMatrix(rf_pred,test_banana[,4])

#svm
library(e1071)
svm_model <- svm(class~.,data = smote_train)
svm_pred <- predict(svm_model,test_banana[,-4])
confusionMatrix(svm_pred,test_banana[,4])






