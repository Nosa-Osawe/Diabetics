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

resampled_diabetics <- clean_Data
resampled_diabetics$Diabetes_binary <- factor(resampled_diabetics$Diabetes_binary)
#Random sampling 
R_RF_samplesize = 0.70*nrow(resampled_diabetics)
set.seed(9999)
RRF_index = sample(seq_len(nrow(resampled_diabetics)), size = R_RF_samplesize)
#Creating training and test set 
R_train_RF <- resampled_diabetics[RRF_index,]
R_test_RF <- resampled_diabetics[-RRF_index,]

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


PrecisionRF2 <- (8261/(3086+8261))
print(PrecisionRF2)     # precision score


#####################################################################################

##          logistic regression


log_model1 <- glm(Diabetes_binary ~.,
                  data = R_train_RF, 
                  family = "binomial")
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

###   #################################################################

#       Lets do a KNN


KNN_data<- KNN_data1

KNN_data$Diabetes_binary <- factor(KNN_data$Diabetes_binary)

KNN1_samplesize = 0.70*nrow(KNN_data)
set.seed(9999)
KNN1_index = sample(seq_len(nrow(KNN_data)), size = KNN1_samplesize)
#Creating training and test set 

KNN_train1 <- KNN_data[KNN1_index,]
KNN_test1 <- KNN_data[-KNN1_index,]

KNN_train1_label <- KNN_train1[,1]
KNN_test1_label <-KNN_test1[,1]


k_value = 5
KNN_test_pred <- knn(train = KNN_train1, test = KNN_test1,
                     cl = KNN_train1_label, k=k_value)


CrossTable(x = KNN_test1_label, y = KNN_test_pred,
           prop.chisq=FALSE)    ##### good performance

CFM4 <- table(x = KNN_test1_label, y = KNN_test_pred)
CFM4
accuracy4 <- sum(diag(CFM4)/sum(CFM4))
print(accuracy4)

##############################################################################
#### Testing on the original data set

Diabetics <- read.csv("C:\\Users\\HP\\Documents\\Diabetics\\Data\\diabetes.csv")

test_diabetics <- Diabetics[, c("Diabetes_012", "BMI", "HighBP", "HighChol",
                                       "Age", "Fruits", "Education", "Sex", "HvyAlcoholConsump",
                                       "AnyHealthcare")]
test_diabetics <- test_diabetics %>% 
  filter(Diabetes_012!= "1")

table(test_diabetics$Diabetes_012)

test_diabetics$Diabetes_012 <- ifelse(test_diabetics$Diabetes_012 == "0", "0", "1")

test_diabetics$Diabetes_012 <- factor(test_diabetics$Diabetes_012)
is.factor(test_diabetics$Diabetes_012)

test_diabetics$Diabetes_binary  <- test_diabetics$Diabetes_012

RF_pred_original_data <- predict(Binary_modelRandom, test_diabetics)
head(RF_pred_original_data)


test_diabetics$predicted <- RF_pred_original_data

##        create a confusion matrix
CFM5 <- table(test_diabetics$predicted, test_diabetics$Diabetes_binary )
CFM5

accuracy5 <- sum(diag(CFM5)/sum(CFM5))
print(accuracy5)  



