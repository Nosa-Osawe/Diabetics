# Load in packages
library(tidyverse)

library(stats)
library(haven)
library(randomForest)
library(class)
library(caret)
library(pROC)
library(mlbench)
library(caTools)
library(party)
library(magrittr)

resampled_diabetics <- read.csv("C:\\Users\\HP\\Documents\\Diabetics\\Data\\diabetics_resampled.csv")
view(resampled_diabetics)
attach(resampled_diabetics)

resampled_diabetics$BMI <-scale(resampled_diabetics$BMI)
resampled_diabetics$Age <- scale(resampled_diabetics$Age)
resampled_diabetics$PhysHlth <- scale(resampled_diabetics$PhysHlth)
resampled_diabetics$MentHlth <- scale(resampled_diabetics$MentHlth)

clean_Data <- resampled_diabetics

attach(clean_Data)

clean_Data <- clean_Data[, c("Diabetes_binary", "BMI", "HighBP", "HighChol",
                             "Age", "Fruits", "Education", "Sex", "HvyAlcoholConsump",
                             "AnyHealthcare")]
view(clean_Data)


###############################################################################

#       Now construct the random forest

rf_diabetics <- clean_Data

rf_diabetics$Diabetes_binary <- factor(rf_diabetics$Diabetes_binary)
#Random sampling 
R_RF_samplesize = 0.70*nrow(rf_diabetics)
set.seed(9999)
RRF_index = sample(seq_len(nrow(rf_diabetics)), size = R_RF_samplesize)
#Creating training and test set 
R_train_RF <- rf_diabetics[RRF_index,]
R_test_RF <- rf_diabetics[-RRF_index,]

str(R_train_RF)
str(R_test_RF)

Binary_modelRandom <- randomForest(Diabetes_binary ~.,
                                   data = R_train_RF)

RF_pred2 <- predict(Binary_modelRandom, R_test_RF)
R_test_RF$pred_diabetics <- RF_pred2

##        create a confusion matrix
CFM2 <- table(R_test_RF$Diabetes_binary, R_test_RF$pred_diabetics)
CFM2

accuracy2 <- sum(diag(CFM2)/sum(CFM2))
print(accuracy2)  

PrecisionRF2 <- (8146/(3514+8146))
print(PrecisionRF2)     # precision score

Recall2 <- (8146/(2415+8146))
Recall2

F_measure2 <- 2*((PrecisionRF2*Recall2)/(PrecisionRF2+Recall2))
F_measure2
#####################################################################################

##          logistic regression (using the data set of the Random forest)


log_model1 <- glm(Diabetes_binary ~.,
                  data = R_train_RF, 
                  family = "binomial")

summary(log_model1)

coefficients(log_model1)


predic_diabetics <- predict(log_model1, R_test_RF, type = "response")

predic_diabetics <- ifelse(predic_diabetics >=.5, "1","0")
head(predic_diabetics)

R_test_RF$diabetics <- predic_diabetics

CFM3 <- table(R_test_RF$Diabetes_binary, R_test_RF$diabetics)
CFM3


accuracy3 <- sum(diag(CFM3)/sum(CFM3))
print(accuracy3)  

PrecisionRF3 <- (7887/(3253+7887))
print(PrecisionRF3)     # precision score

Recall3 <- (7887/(7887+2674))
Recall3

F_measure3 <- 2 * (PrecisionRF3*Recall3)/(PrecisionRF3+Recall3)
F_measure3
###   #################################################################

####   Create the decision tree model

dt_diabetics <- clean_Data


dt_diabetics$Diabetes_binary <- factor(dt_diabetics$Diabetes_binary)
#Random sampling 
dt_samplesize = 0.70*nrow(dt_diabetics)
set.seed(9999)
dt_index = sample(seq_len(nrow(dt_diabetics)), size = dt_samplesize)
#Creating training and test set 
dt_train <- dt_diabetics[dt_index,]
dt_test <- dt_diabetics[-dt_index,]

str(dt_train)
str(dt_test)

dtmodel<- ctree(Diabetes_binary ~ ., dt_train)

dt_pred2 <- predict(dtmodel, dt_test)
dt_test$dt_pred2 <- dt_pred2

##        create a confusion matrix
CFM6 <- table(dt_test$Diabetes_binary, dt_test$dt_pred2)
CFM6

accuracy2 <- sum(diag(CFM6)/sum(CFM6))
print(accuracy2)  

(Precision_dt <- (7978/(3384+7978)))

Recalldt <- 7978/(7978 + 2583)
Recalldt

F_measure <- 2* ((Precision_dt*Recalldt)/(Precision_dt+Recalldt))
F_measure
