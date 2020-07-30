
library(imbalance)

banana_data <- glass0 
nrow(banana_data)
View(banana_data)
names(banana)
nrow(banana)
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

p_gbm <-predict(gbmFit1,test_banana[,-10],type = "raw")

confusionMatrix(p_gbm,test_banana$Class)


#ADA BOOST
library(fastAdaboost)
ada_model<- adaboost(Class~.,train_banana,10)

ada_pred <- predict(ada_model,test_banana[,-10])

confusionMatrix(ada_pred$class,test_banana[,10])

library(randomForest)

rf_model <- train(Class ~., data = train_banana, 
                  method = "rf", trControl = fitControl,verbose = FALSE)
rf_pred <- predict(rf_model,test_banana[,-10])
confusionMatrix(rf_pred,test_banana[,10])

#svm
library(e1071)
svm_model <- svm(Class~.,data = train_banana)
svm_pred <- predict(svm_model,test_banana[,-10])
confusionMatrix(svm_pred,test_banana[,10])


#AFTER SAMPLING TECHNIQUES

library(DMwR)

smote_train <- SMOTE(Class~.,train_banana,perc.over = 600,k=5,perc.under = 300)
nrow(smote_train)
smote_train %>% group_by(Class)%>% count()

gbmFit1 <- train(Class ~., data = smote_train, 
                 method = "gbm", trControl = fitControl,verbose = FALSE)

p_gbm <-predict(gbmFit1,test_banana[,-10],type = "raw")

confusionMatrix(p_gbm,test_banana$Class)


#ADA BOOST
library(fastAdaboost)
ada_model<- adaboost(Class~.,smote_train,10)

ada_pred <- predict(ada_model,test_banana[,-10])

confusionMatrix(ada_pred$class,test_banana[,10])

library(randomForest)

rf_model <- train(Class ~., data = smote_train, 
                  method = "rf", trControl = fitControl,verbose = FALSE)
rf_pred <- predict(rf_model,test_banana[,-10])
confusionMatrix(rf_pred,test_banana[,10])

#svm
library(e1071)
svm_model <- svm(Class~.,data = smote_train)
svm_pred <- predict(svm_model,test_banana[,-10])
confusionMatrix(svm_pred,test_banana[,10])






#ADASYN

library(smotefamily)

x<- train_banana[,-10]
target <-train_banana[,10]
nrow(train_banana)
db.smote<-DBSMOTE(x, target = target, MinPts = NULL, eps = NULL)
ADAS.data<- ADAS(x,target = target,5)
class(ADAS.data)
View(ADAS.data)

ADAS.data <- ADAS.data$data
nrow(ADAS.data)
View(ADAS.data)

names(ADAS.data)

#change target value column names
ADAS.data$class<- as.factor(ADAS.data$class)

smote_train<-ADAS.data
smote_train<-db.smote$data
smote_train %>% group_by(class)%>% count()


library(caret)

library(gbm)
gbmFit1 <- train(class ~., data = smote_train, 
                 method = "gbm", trControl = fitControl,verbose = FALSE)

p_gbm <-predict(gbmFit1,test_banana[,-10],type = "raw")

confusionMatrix(p_gbm,test_banana$Class)


#ADA BOOST
library(fastAdaboost)
ada_model<- adaboost(class~.,smote_train,10)

ada_pred <- predict(ada_model,test_banana[,-10])

confusionMatrix(ada_pred$class,test_banana[,10])

library(randomForest)



rf_model <- train(class ~., data = smote_train, 
                  method = "rf", trControl = fitControl,verbose = FALSE)
rf_pred <- predict(rf_model,test_banana[,-10])
confusionMatrix(rf_pred,test_banana[,10])

#svm
library(e1071)
svm_model <- svm(class~.,data = smote_train)
svm_pred <- predict(svm_model,test_banana[,-10])
confusionMatrix(svm_pred,test_banana[,10])






