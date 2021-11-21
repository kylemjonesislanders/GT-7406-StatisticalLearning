# Final Ridge Model, with predictions on Test Set
rm(list=ls())

# Libraries
library("ggplot2")
library("scales")
library("Hmisc")

# Set working directory
setwd("/Users/kjone332/Desktop/RStudio_Projects/GT-7406-StatisticalLearning/course_project")

# Read in training and test data
train_dat <- read.table(file="./data_files/train.csv", sep = ",", header=TRUE)
test_dat <- read.table(file="./data_files/test.csv", sep = ",", header=TRUE)

# Initial Feature Engineering
## Train Data
train_dat$LogSalePrice <- log(train_dat$SalePrice)
train_dat$TotalSquareFootage <- train_dat$TotalBsmtSF + train_dat$GrLivArea
train_dat$BsmtBathrooms <- train_dat$BsmtFullBath + train_dat$BsmtHalfBath
train_dat$FullBathrooms <- train_dat$FullBath + train_dat$HalfBath
train_dat$TotalBathooms <- train_dat$BsmtBathrooms + train_dat$FullBathrooms
## Test Data
test_dat$TotalSquareFootage <- test_dat$TotalBsmtSF + test_dat$GrLivArea
test_dat$BsmtBathrooms <- test_dat$BsmtFullBath + test_dat$BsmtHalfBath
test_dat$FullBathrooms <- test_dat$FullBath + test_dat$HalfBath
test_dat$TotalBathooms <- test_dat$BsmtBathrooms + test_dat$FullBathrooms

# Remove outliers from train data
train_dat <- train_dat[train_dat$TotalSquareFootage<7000, ]

# Initialize model_dat with continuous x predictors and y
model_dat_train <- train_dat[ , c("LogSalePrice", "TotalSquareFootage", "YearRemodAdd")]
model_dat_test <- test_dat[ , c("TotalSquareFootage", "YearRemodAdd")]

# Scale continuous x predictors
train_mean <- apply(model_dat_train[, c("TotalSquareFootage", "YearRemodAdd")],2,mean)
train_sd <- apply(model_dat_train[, c("TotalSquareFootage", "YearRemodAdd")],2,sd)
model_dat_train[,c("TotalSquareFootage", "YearRemodAdd")] <- sweep(sweep(model_dat_train[,c("TotalSquareFootage", "YearRemodAdd")], 2L, train_mean), 2, train_sd, "/")
model_dat_test[,c("TotalSquareFootage", "YearRemodAdd")] <- sweep(sweep(model_dat_test[,c("TotalSquareFootage", "YearRemodAdd")], 2L, train_mean), 2, train_sd, "/")

# Final Feature Engineering
# Train
model_dat_train$CollgCr <- ifelse(train_dat$Neighborhood=="CollgCr", 1, 0)
model_dat_train$Veenker <- ifelse(train_dat$Neighborhood=="Veenker", 1, 0)
model_dat_train$Crawfor <- ifelse(train_dat$Neighborhood=="Crawfor", 1, 0)
model_dat_train$NoRidge <- ifelse(train_dat$Neighborhood=="NoRidge", 1, 0)
model_dat_train$Mitchel <- ifelse(train_dat$Neighborhood=="Mitchel", 1, 0)
model_dat_train$Somerst <- ifelse(train_dat$Neighborhood=="Somerst", 1, 0)
model_dat_train$NWAmes <- ifelse(train_dat$Neighborhood=="NWAmes", 1, 0)
model_dat_train$BrkSide <- ifelse(train_dat$Neighborhood=="BrkSide", 1, 0)
model_dat_train$Sawyer <- ifelse(train_dat$Neighborhood=="Sawyer", 1, 0)
model_dat_train$NridgHt <- ifelse(train_dat$Neighborhood=="NridgHt", 1, 0)
model_dat_train$NAmes <- ifelse(train_dat$Neighborhood=="NAmes", 1, 0)
model_dat_train$SawyerW <- ifelse(train_dat$Neighborhood=="SawyerW", 1, 0)
model_dat_train$Timber <- ifelse(train_dat$Neighborhood=="Timber", 1, 0)
model_dat_train$Gilbert <- ifelse(train_dat$Neighborhood=="Gilbert", 1, 0)
model_dat_train$StoneBr <- ifelse(train_dat$Neighborhood=="StoneBr", 1, 0)
model_dat_train$ClearCr <- ifelse(train_dat$Neighborhood=="ClearCr", 1, 0)
model_dat_train$NPkVill <- ifelse(train_dat$Neighborhood=="NPkVill", 1, 0)
model_dat_train$Blmngtn <- ifelse(train_dat$Neighborhood=="Blmngtn", 1, 0)
model_dat_train$SWISU <- ifelse(train_dat$Neighborhood=="SWISU", 1, 0)
model_dat_train$OldTown <- ifelse(train_dat$Neighborhood=="OldTown", 1, 0)
model_dat_train$Edwards <- ifelse(train_dat$Neighborhood=="Edwards", 1, 0)
model_dat_train$SaleConditionAbnorml <- ifelse(train_dat$SaleCondition=="Abnorml", 1, 0)
model_dat_train$SaleConditionFamily <- ifelse(train_dat$SaleCondition=="Family", 1, 0)
model_dat_train$SaleConditionPartial <- ifelse(train_dat$SaleCondition=="Partial", 1, 0)
model_dat_train$TotalBathooms2 <- ifelse(train_dat$TotalBathooms==2, 1, 0)
model_dat_train$TotalBathooms3 <- ifelse(train_dat$TotalBathooms==3, 1, 0)
model_dat_train$TotalBathooms4 <- ifelse(train_dat$TotalBathooms>=4, 1, 0)
model_dat_train$FenceGoodPrivacy <- ifelse(train_dat$Fence%in%c("GdPrv"), 1, 0)
model_dat_train$BasementGoodCondition <- ifelse(train_dat$BsmtCond%in%c("Ex", "Gd"), 1, 0)
model_dat_train$GarageCar1 <- ifelse(train_dat$GarageCars==1, 1, 0)
model_dat_train$GarageCar2 <- ifelse(train_dat$GarageCars==2, 1, 0)
model_dat_train$GarageCar3 <- ifelse(train_dat$GarageCars>=3, 1, 0)
model_dat_train$FirePlace <- ifelse(train_dat$Fireplaces>=1, 1, 0)
model_dat_train$PartialxTotalSquareFootage <- model_dat_train$TotalSquareFootage*model_dat_train$SaleConditionPartial

# Test
model_dat_test$CollgCr <- ifelse(test_dat$Neighborhood=="CollgCr", 1, 0)
model_dat_test$Veenker <- ifelse(test_dat$Neighborhood=="Veenker", 1, 0)
model_dat_test$Crawfor <- ifelse(test_dat$Neighborhood=="Crawfor", 1, 0)
model_dat_test$NoRidge <- ifelse(test_dat$Neighborhood=="NoRidge", 1, 0)
model_dat_test$Mitchel <- ifelse(test_dat$Neighborhood=="Mitchel", 1, 0)
model_dat_test$Somerst <- ifelse(test_dat$Neighborhood=="Somerst", 1, 0)
model_dat_test$NWAmes <- ifelse(test_dat$Neighborhood=="NWAmes", 1, 0)
model_dat_test$BrkSide <- ifelse(test_dat$Neighborhood=="BrkSide", 1, 0)
model_dat_test$Sawyer <- ifelse(test_dat$Neighborhood=="Sawyer", 1, 0)
model_dat_test$NridgHt <- ifelse(test_dat$Neighborhood=="NridgHt", 1, 0)
model_dat_test$NAmes <- ifelse(test_dat$Neighborhood=="NAmes", 1, 0)
model_dat_test$SawyerW <- ifelse(test_dat$Neighborhood=="SawyerW", 1, 0)
model_dat_test$Timber <- ifelse(test_dat$Neighborhood=="Timber", 1, 0)
model_dat_test$Gilbert <- ifelse(test_dat$Neighborhood=="Gilbert", 1, 0)
model_dat_test$StoneBr <- ifelse(test_dat$Neighborhood=="StoneBr", 1, 0)
model_dat_test$ClearCr <- ifelse(test_dat$Neighborhood=="ClearCr", 1, 0)
model_dat_test$NPkVill <- ifelse(test_dat$Neighborhood=="NPkVill", 1, 0)
model_dat_test$Blmngtn <- ifelse(test_dat$Neighborhood=="Blmngtn", 1, 0)
model_dat_test$SWISU <- ifelse(test_dat$Neighborhood=="SWISU", 1, 0)
model_dat_test$OldTown <- ifelse(test_dat$Neighborhood=="OldTown", 1, 0)
model_dat_test$Edwards <- ifelse(test_dat$Neighborhood=="Edwards", 1, 0)
model_dat_test$SaleConditionAbnorml <- ifelse(test_dat$SaleCondition=="Abnorml", 1, 0)
model_dat_test$SaleConditionFamily <- ifelse(test_dat$SaleCondition=="Family", 1, 0)
model_dat_test$SaleConditionPartial <- ifelse(test_dat$SaleCondition=="Partial", 1, 0)
model_dat_test$TotalBathooms2 <- ifelse(test_dat$TotalBathooms==2, 1, 0)
model_dat_test$TotalBathooms3 <- ifelse(test_dat$TotalBathooms==3, 1, 0)
model_dat_test$TotalBathooms4 <- ifelse(test_dat$TotalBathooms>=4, 1, 0)
model_dat_test$FenceGoodPrivacy <- ifelse(test_dat$Fence%in%c("GdPrv"), 1, 0)
model_dat_test$BasementGoodCondition <- ifelse(test_dat$BsmtCond%in%c("Ex", "Gd"), 1, 0)
model_dat_test$GarageCar1 <- ifelse(test_dat$GarageCars==1, 1, 0)
model_dat_test$GarageCar2 <- ifelse(test_dat$GarageCars==2, 1, 0)
model_dat_test$GarageCar3 <- ifelse(test_dat$GarageCars>=3, 1, 0)
model_dat_test$FirePlace <- ifelse(test_dat$Fireplaces>=1, 1, 0)
model_dat_test$PartialxTotalSquareFootage <- model_dat_test$TotalSquareFootage*model_dat_test$SaleConditionPartial

# Build model
ridge_model <- lm.ridge(LogSalePrice ~ ., data = model_dat_train, lambda= seq(0,100,0.001))
optimal_lambda_index <- which.min(ridge_model$GCV) 
ridge.coeffs = ridge_model$coef[ , optimal_lambda_index]/ridge_model$scales # Unscale x vars
intercept = -sum( ridge.coeffs * colMeans(model_dat_train[,2:ncol(model_dat_train)] ) ) + mean(model_dat_train[,1])
predictions_train <- as.matrix(model_dat_train[,2:ncol(model_dat_train)]) %*% as.vector(ridge.coeffs) + intercept
rmse_train <- sqrt(mean((model_dat_train$LogSalePrice - predictions_train)^2))

# Predict on Test Set

# Fill in missing data with median value
for(i in 1:ncol(model_dat_test)){
  model_dat_test[is.na(model_dat_test[,i]), i] <- median(model_dat_test[,i], na.rm = TRUE)
}
ids <- test_dat$Id
predictions_test_log <- as.matrix(model_dat_test) %*% as.vector(ridge.coeffs) + intercept
predictions_test_normal <- exp(predictions_test_log)
final_predictions <- data.frame(ids, predictions_test_normal)
colnames(final_predictions) <- c("Id", "SalePrice")

# Write to csv
write.csv(final_predictions, './modeling/final_results.csv', row.names=FALSE)
