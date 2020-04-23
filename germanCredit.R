#Importing all the necessary libraries
library(rpart)
library(rpart.plot)
library(caret)
#Reading file from the system
german_credit <- read.csv(file.choose(), header = T)
str(german_credit)

#Converting the columns to factors
german_credit[, c(2,4,5,6,7,8,9,10,12,13,15,16,17,18,19,20,21,22,24,25,26,28,30,31,32)] <-
  lapply(german_credit[, c(2,4,5,6,7,8,9,10,12,13,15,16,17,18,19,20,21,22,24,25,26,28,30,31,32)], as.factor)
#Checking if the data set contains any NAs and removing them if there are any.
is.na(german_credit)
german_credit <- na.omit(german_credit)
set.seed(123)
#Creating samples for training and test data.
index = sample(2, nrow(german_credit), replace = T, prob = c(0.7,0.3))

#Training data set
german_credit_training = german_credit[index == 1,]
#Test data set
german_credit_test = german_credit[index == 2,]
nrow(german_credit) #1000
nrow(german_credit_training) #705
nrow(german_credit_test) #295
#Generating decision tree model without using Opportunity Cost Table
german_credit_model = rpart(german_credit_training$RESPONSE ~.,data = german_credit_training, method = "class")
rpart.plot(german_credit_model)

#Setting up prediction for training data
predict_train = predict(german_credit_model, german_credit_training, type = "class")

#Table to compare predicted and actual values (Confusion Matrix)
table(predict_train, german_credit_training$RESPONSE, dnn = c("Predicted", "Actual"))
#Setting up prediction for test data
predict_test = predict(german_credit_model, german_credit_test, type = "class")
#Table to compare predicted and actual values (Confusion Matrix)
table(predict_test, german_credit_test$RESPONSE, dnn = c("Predicted", "Actual"))
#Creating opportunity cost matrix
opportunity_cost <- matrix(c(0,100,500,0), byrow=TRUE, nrow = 2)
opportunity_cost

#Generating decision tree model using Opportunity Cost Table
german_credit_model_loss = rpart(german_credit_training$RESPONSE ~.,data = german_credit_training, method = "class", parms=list(loss = opportunity_cost))
rpart.plot(german_credit_model_loss)

#Setting up prediction for training data
predict_train_loss = predict(german_credit_model_loss, german_credit_training, type = "class")
#Table to compare predicted and actual values (Confusion Matrix)
table(predict_train_loss, german_credit_training$RESPONSE, dnn = c("Predicted", "Actual"))
#Setting up prediction for test data
predict_test_loss = predict(german_credit_model_loss, german_credit_test, type = "class")

#Table to compare predicted and actual values (Confusion Matrix)
table(predict_test_loss, german_credit_test$RESPONSE, dnn = c("Predicted", "Actual"))
#Comparing the 2 models
#Model without using Opportunity Cost table
#On Training Data
confusionMatrix(predict_train, german_credit_training$RESPONSE)
#On Test Data
confusionMatrix(predict_test, german_credit_test$RESPONSE)

#Model using Opportunity Cost table
#On Training Data
confusionMatrix(predict_train_loss, german_credit_training$RESPONSE)
#On Test Data
confusionMatrix(predict_test_loss, german_credit_test$RESPONSE)

#To compare the models based on the evidences present in the snapshots of confusion matrices above, we summarize the data in the table below:
#As we can see in the data present in the table above, even though the accuracy in the model using Opportunity Cost Table got lower by about 3% in both training and test data, the second model shows a significant decrease in False Alarm Rate and hence an increase in Specificity from 91.57% to 99.56% in training data and from 86.14% to 96.04% in test data. The false alarm rate decreased from 8.43% to 0.44% in training data and from 13.86% to 3.96% in test data. Since this change is much more significant and improves our data model, hence we can conclude that the model using Opportunity Cost performs better than the one not using the Opportunity Cost table.
                