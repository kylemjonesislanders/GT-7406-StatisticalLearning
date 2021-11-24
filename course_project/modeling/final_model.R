# Final Ensemble Model (Lasso + Random Forest), with predictions on Test Set
rm(list=ls())

# Libraries
library("ggplot2")
library("scales")
library("Hmisc")
library("leaps")
library("MASS")
library("lars")
library("randomForest")
library("caret")

# Set working directory
setwd("/Users/kjone332/Desktop/RStudio_Projects/GT-7406-StatisticalLearning/course_project")

# Read in training and test data
train_dat1 <- read.table(file="./data_files/train.csv", sep = ",", header=TRUE)
test_dat1 <- read.table(file="./data_files/test.csv", sep = ",", header=TRUE)

# Feature Engineering
source('./modeling/feature_engineering.R')
train_dat <- feature_engineering(train_dat1)
train_dat <- cbind(LogSalePrice = log(train_dat1$SalePrice), train_dat)
test_dat <- feature_engineering(test_dat1)

# Remove outliers from train data
train_dat <- train_dat[train_dat$TotalSquareFootage<7000, ]

# Scale continuous x predictors using train as scale method
continuous_predictors <- c("LotFrontage", "YearRemodAdd", "NeighborhoodInt", "ExterQualInt",
                           "ExterCondInt", "HeatingQCInt", "KitchenQualInt", "TotalSquareFootage",
                           "TotalBathooms", "PartialxTotalSquareFootage", "GarageCars")
for(col in continuous_predictors){
  # Train
  col_train_vals <- train_dat[ , col]
  mean_val <- mean(col_train_vals)
  sd_val <- sd(col_train_vals)
  scaled_train_vals <- (col_train_vals-mean_val)/sd_val
  train_dat[ , col] <- scaled_train_vals
  # Test
  col_test_vals <- test_dat[ , col]
  scaled_test_vals <- (col_test_vals-mean_val)/sd_val
  test_dat[ , col] <- scaled_test_vals
}

# Build ensemble model (Best CV validation hyperparameters on train for Lasso + Random Forest)
## Lasso
lasso_model <- lars(as.matrix(train_dat[,2:ncol(train_dat)]), train_dat$LogSalePrice, 
                    type= "lasso", trace= TRUE)
Cps <- summary(lasso_model)$Cp
optimal_lambda_index <- which.min(Cps)
optimal_lambda <- lasso_model$lambda[optimal_lambda_index]
## Random Forest
modellist <- list()
control <- trainControl(method='boot',
                        number=5,
                        search='grid')
tunegrid <- expand.grid(.mtry = (1:10))
for (ntree in c(3, 10, 100, 500)){
  fit <- train(LogSalePrice ~ .,
               data = train_dat,
               method = 'rf',
               metric = 'RMSE',
               tuneGrid = tunegrid,
               trControl = control,
               ntree = ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
results_3 <- modellist$`3`$results[, c('mtry', 'RMSE')]
results_3$ntree <- 3
results_10 <- modellist$`10`$results[, c('mtry', 'RMSE')]
results_10$ntree <- 10
results_100 <- modellist$`100`$results[, c('mtry', 'RMSE')]
results_100$ntree <- 100
results_500 <- modellist$`500`$results[, c('mtry', 'RMSE')]
results_500$ntree <- 500
final_df <- rbind(results_3, results_10, results_100, results_500)
best_result <- final_df[which(final_df$RMSE==min(final_df$RMSE)), ]
optimal_mtry <- best_result$mtry
optimal_ntree <- best_result$ntree
rf_optimal <- randomForest(LogSalePrice ~.,
                           data=train_dat,
                           ntree= optimal_ntree,
                           mtry=optimal_mtry,
                           importance=TRUE)

# Predictions on Train Set
predictions_train_RF <- predict(rf_optimal, train_dat)
predictions_train_Lasso <- predict(lasso_model, 
                                   as.matrix(train_dat[,2:ncol(train_dat)]), s=optimal_lambda, type="fit", mode="lambda")$fit
predictions_train_ensemble <- rowMeans(cbind(predictions_train_RF,predictions_train_Lasso))
rmse_train <- sqrt(mean((train_dat$LogSalePrice - predictions_train_ensemble)^2))

# Predict on Test Set
predictions_test_RF <- predict(rf_optimal, test_dat)
predictions_test_Lasso <- predict(lasso_model, 
                                  as.matrix(test_dat[,1:ncol(test_dat)]), s=optimal_lambda, type="fit", mode="lambda")$fit
predictions_test_ensemble <- rowMeans(cbind(predictions_test_RF, predictions_test_Lasso))

# Create submission csv file
ids <- test_dat1$Id
predictions_test_normal <- exp(predictions_test_ensemble)
final_predictions <- data.frame(ids, predictions_test_normal)
colnames(final_predictions) <- c("Id", "SalePrice")

# Write to csv
write.csv(final_predictions, './modeling/final_results.csv', row.names=FALSE)
