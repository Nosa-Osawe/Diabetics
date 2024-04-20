# Load in packages
library(tidyverse)
library(MASS)
library(stats)
library(haven)
library(randomForest)
library(class)
library(caret)
library(pROC)
library(mlbench)

## Load in data
Diabetics <- read.csv("C:\\Users\\HP\\Documents\\Diabetics\\Data\\diabetes.csv")
view(Diabetics)

# Check for missing values 
sum(is.na(Diabetics))

attach(Diabetics)

Diabetics <-Diabetics %>% 
  mutate(Diabetics = factor(Diabetes_012))

head(Diabetics)
str(Diabetics)
#Dividing data into training and test set
#Random sampling 
samplesize = 0.70*nrow(Diabetics)
set.seed(9999)
index_m = sample(seq_len(nrow(Diabetics)), size = samplesize)
#Creating training and test set 
train_diabetics <- Diabetics[index_m,]
test_diabetics <- Diabetics[-index_m,]

str(train_diabetics)
str(test_diabetics)

##############################################################################
###         Ordinal Regression
Ord_model1 <- polr(Diabetics ~ BMI+HighBP+HighChol+Age+Fruits+
                     Education+Sex+HvyAlcoholConsump+AnyHealthcare,
                   data = train_diabetics ,
                   Hess = TRUE)

diabetics_pred <- predict(Ord_model1, test_diabetics)
test_diabetics$pred_diabetics <- diabetics_pred

##        create a confusion matrix
CFM1 <- table(test_diabetics$Diabetics, test_diabetics$pred_diabetics)
CFM1

accuracy1 <- sum(diag(CFM1)/sum(CFM1))
print(accuracy1)   # 84.19
################################################################################

## Use 1/10 of the origninal data for RF 

samplesize = 0.10*nrow(Diabetics)
set.seed(999)
index_rf = sample(seq_len(nrow(Diabetics)), size = samplesize)
#Creating training and test set 
Rf_data <- Diabetics[index_rf,]
view(Rf_data)
#   lets split the new RF data into training and testing set
#Random sampling 
RF_samplesize = 0.70*nrow(Rf_data)
set.seed(9999)
RF_index = sample(seq_len(nrow(Rf_data)), size = RF_samplesize)
#Creating training and test set 
train_RF <- Rf_data[RF_index,]
test_RF <- Rf_data[-RF_index,]

str(train_RF)
str(test_RF)

Ord_modelRandom <- randomForest(Diabetics ~ BMI+HighBP+HighChol+Age+Fruits+
                                  Education+Sex+HvyAlcoholConsump+AnyHealthcare,
                                data = train_RF)

RF_pred1 <- predict(Ord_modelRandom, test_RF)
test_RF$pred_diabetics <- RF_pred1

##        create a confusion matrix
CFM2 <- table(test_RF$Diabetics, test_RF$pred_diabetics)
CFM2

accuracy2 <- sum(diag(CFM2)/sum(CFM2))
print(accuracy2)   # 84.68%

#####################################################################################################




