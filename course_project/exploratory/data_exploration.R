# Libraries
library("ggplot2")
library("rpart")
library("randomForest")
library("caret")
library("gbm")
library("glmnet")

# Set working directory
setwd("/Users/Kyle Jones/OneDrive/Desktop/GT-7406-StatisticalLearning/course_project")

# Read in Data
dat <- read.table(file="./data_files/train.csv", 
                  sep = ",", header=TRUE)
