# Load in packages
library(tidyverse)
library(MASS)
library(stats)
library(haven)

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
samplesize = 0.70*nrow(df_filtered)
set.seed(100)
index_m = sample(seq_len(nrow(df_filtered)), size = samplesize)
#Creating training and test set 
datatrain_music <- df_filtered[index_m,]
datatest_music <- df_filtered[-index_m,]