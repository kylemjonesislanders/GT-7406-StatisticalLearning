# Model Development
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

# Read in training data and initial feature engineering
train_dat <- read.table(file="./data_files/train.csv", sep = ",", header=TRUE)

# Feature Engineering
source('./modeling/feature_engineering.R')
model_dat <- feature_engineering(train_dat)
model_dat <- cbind(LogSalePrice = log(train_dat$SalePrice), model_dat)

# Remove outlier based on square footage
model_dat <- model_dat[model_dat$TotalSquareFootage<7000, ]

# Scale continuous x predictors
continuous_predictors <- c("LotFrontage", "YearRemodAdd", "NeighborhoodInt", "ExterQualInt",
                           "ExterCondInt", "HeatingQCInt", "KitchenQualInt", "TotalSquareFootage",
                           "TotalBathooms", "PartialxTotalSquareFootage", "GarageCars")
for(col in continuous_predictors){
  model_dat[ , col] <- scale(model_dat[ , col])
}

# Build Test Model (R2 = 0.895, adjusted = .892)
mod <- lm(LogSalePrice ~., model_dat)
summary(mod)

# Monte Carlo Cross Validation
n1 = 1166
n2 = 292
n = dim(model_dat)[1]
# Set seed for randomization
set.seed(70)
# Define number of iterations to perform during CV
B = 100
# Initialize matrix to hold values in loop
error_results <- matrix(ncol=4, nrow=7*B*2)
iterator <- 1

for (b in 1:B){
  # Isolate train and test sets
  flag <- sort(sample(1:n, n1))
  traintemp <- model_dat[flag,]
  ytrain <- traintemp$LogSalePrice
  testtemp <- model_dat[-flag,]
  ytest <- testtemp$LogSalePrice

  # 1) Linear regression
  model1 <- lm(LogSalePrice ~ ., data = traintemp)
  predictions_train_LR <- predict(model1, traintemp[ , 2:ncol(traintemp)])
  rmse_train <- sqrt(mean((ytrain - predictions_train_LR)^2))
  predictions_test_LR <- predict(model1, testtemp[ , 2:ncol(testtemp)])
  rmse_test <- sqrt(mean((ytest - predictions_test_LR)^2))

  error_results[iterator, 1:4] <- c('Linear\nRegression', b, rmse_train, 'Train')
  iterator <- iterator + 1
  error_results[iterator, 1:4] <- c('Linear\nRegression', b, rmse_test, 'Test')
  iterator <- iterator + 1
  
  # 2) Linear regression significant vars
  significnat_vars <-c("LogSalePrice", "NeighborhoodInt", "TotalSquareFootage")
  traintemp_subset <- traintemp[ , significnat_vars]
  testtemp_subset <- testtemp[ , significnat_vars]
  model1b <- lm(LogSalePrice ~ ., data = traintemp_subset)
  predictions_train_LRb <- predict(model1b, traintemp_subset[ , 2:ncol(traintemp_subset)])
  rmse_train <- sqrt(mean((ytrain - predictions_train_LRb)^2))
  predictions_test_LRb <- predict(model1b, testtemp_subset[ , 2:ncol(testtemp_subset)])
  rmse_test <- sqrt(mean((ytest - predictions_test_LRb)^2))
  
  error_results[iterator, 1:4] <- c('Linear\nRegression\nSubset', b, rmse_train, 'Train')
  iterator <- iterator + 1
  error_results[iterator, 1:4] <- c('Linear\nRegression\nSubset', b, rmse_test, 'Test')
  iterator <- iterator + 1
  
  # 3) Ridge Regression
  model2 <- lm.ridge(LogSalePrice ~ ., data = traintemp, lambda= seq(0,100,0.001))
  optimal_lambda_index <- which.min(model2$GCV) 
  ridge.coeffs = model2$coef[,optimal_lambda_index]/ model2$scales # Unscale x vars
  intercept = -sum( ridge.coeffs * colMeans(traintemp[,2:ncol(traintemp)] ) )+ mean(traintemp[,1])
  predictions_train_Ridge <- as.matrix(traintemp[,2:ncol(traintemp)]) %*% as.vector(ridge.coeffs) + intercept
  rmse_train <- sqrt(mean((ytrain - predictions_train_Ridge)^2))
  predictions_test_Ridge <- as.matrix(testtemp[,2:ncol(testtemp)]) %*% as.vector(ridge.coeffs) + intercept
  rmse_test <- sqrt(mean((ytest - predictions_test_Ridge)^2))
  
  error_results[iterator, 1:4] <- c('Ridge\nRegression', b, rmse_train, 'Train')
  iterator <- iterator + 1
  error_results[iterator, 1:4] <- c('Ridge\nRegression', b, rmse_test, 'Test')
  iterator <- iterator + 1
  
  # 4) Lasso Regression
  model3 <- lars(as.matrix(traintemp[,2:ncol(traintemp)]), traintemp$LogSalePrice, type= "lasso", trace= TRUE)
  Cps <- summary(model3)$Cp
  optimal_lambda_index <- which.min(Cps)
  optimal_lambda <- model3$lambda[optimal_lambda_index]
  predictions_train_Lasso <- predict(model3, as.matrix(traintemp[,2:ncol(traintemp)]), s=optimal_lambda, type="fit", mode="lambda")$fit
  rmse_train <- sqrt(mean((ytrain - predictions_train_Lasso)^2))
  predictions_test_Lasso <- predict(model3, as.matrix(testtemp[,2:ncol(testtemp)]), s=optimal_lambda, type="fit", mode="lambda")$fit
  rmse_test <- sqrt(mean((ytest - predictions_test_Lasso)^2))
  
  error_results[iterator, 1:4] <- c('Lasso\nRegression', b, rmse_train, 'Train')
  iterator <- iterator + 1
  error_results[iterator, 1:4] <- c('Lasso\nRegression', b, rmse_test, 'Test')
  iterator <- iterator + 1
  
  # 5) Random Forest
  modellist <- list()
  control <- trainControl(method='boot',
                          number=5,
                          search='grid')
  tunegrid <- expand.grid(.mtry = (1:10))
  for (ntree in c(3, 10, 100, 500)){
    fit <- train(LogSalePrice ~ .,
                 data = traintemp,
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
                             data=traintemp,
                             ntree= optimal_ntree,
                             mtry=optimal_mtry,
                             importance=TRUE)
  predictions_train_RF <- predict(rf_optimal, traintemp)
  rmse_train <- sqrt(mean((ytrain - predictions_train_RF)^2))
  predictions_test_RF <- predict(rf_optimal, testtemp)
  rmse_test <- sqrt(mean((ytest - predictions_test_RF)^2))
  
  error_results[iterator, 1:4] <- c('Random\nForest', b, rmse_train, 'Train')
  iterator <- iterator + 1
  error_results[iterator, 1:4] <- c('Random\nForest', b, rmse_test, 'Test')
  iterator <- iterator + 1
  
  # 6) KNN
  trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
  knn_fit <- train(LogSalePrice ~., data = traintemp, method = "knn",
                   trControl=trctrl,
                   preProcess = c("center", "scale"),
                   tuneLength = 20)
  predictions_train_KNN <- predict(knn_fit, newdata = traintemp)
  rmse_train <- sqrt(mean((ytrain - predictions_train_KNN)^2))
  predictions_test_KNN <- predict(knn_fit, newdata = testtemp)
  rmse_test <- sqrt(mean((ytest - predictions_test_KNN)^2))
  error_results[iterator, 1:4] <- c('KNN', b, rmse_train, 'Train')
  iterator <- iterator + 1
  error_results[iterator, 1:4] <- c('KNN', b, rmse_test, 'Test')
  iterator <- iterator + 1
  
  # 7) Ensemble Method
  predictions_train_ensemble <- rowMeans(cbind(predictions_train_Ridge,
                                               predictions_train_RF))
  rmse_train <- sqrt(mean((ytrain - predictions_train_ensemble)^2))
  predictions_test_ensemble <- rowMeans(cbind(predictions_test_Ridge,
                                              predictions_test_RF))
  rmse_test <- sqrt(mean((ytest - predictions_test_ensemble)^2))
  error_results[iterator, 1:4] <- c('Ensemble', b, rmse_train, 'Train')
  iterator <- iterator + 1
  error_results[iterator, 1:4] <- c('Ensemble', b, rmse_test, 'Test')
  iterator <- iterator + 1
}

# Create dataframe for plot
plot_df <- data.frame(error_results)
plot_df <- plot_df[complete.cases(plot_df), ]
plot_df[, 3] <- sapply(plot_df[, 3], as.character)
plot_df[, 3] <- sapply(plot_df[, 3], as.numeric)
colnames(plot_df) <- c("Model", "B", "Value", 'Group')
plot_df$Group <- as.character(plot_df$Group)
plot_df$Group <- factor(plot_df$Group,
                        levels = c('Train','Test'))
# Plot
jpeg(file="./modeling/images/MonteCarloResults.jpeg",
     width=750, height=450)
ggplot(plot_df, aes(x = Model, y = Value, fill=Group)) +
       geom_boxplot() +
       theme_bw(base_size = 16) + 
       ylab("RMSE") +
       xlab("Model") +
       theme(legend.title=element_blank())
dev.off()

# Save median errors for each model
error_df <- as.data.frame(error_results)
colnames(error_df) <- c("model", 'iteration', 'error', 'group')
error_df <- transform(error_df, iteration = as.numeric(iteration), error = as.numeric(error))
unique_models <- unique(error_df$model)
median_results <- matrix(ncol=3, nrow=7)
iterator <- 1
for(model in unique_models){
  temp_dat_train <- error_df[which(error_df$model==model&error_df$group=='Train'), ]
  temp_dat_test <- error_df[which(error_df$model==model&error_df$group=='Test'), ]
  median_results[iterator, 1:3] <- c(model, median(temp_dat_train$error), median(temp_dat_test$error))
  iterator <- iterator + 1
}
median_results <- as.data.frame(median_results)
colnames(median_results) <- c("model", 'train_median_error', 'test_median_error')
write.csv(median_results, './modeling/monte_carlo_errors.csv', row.names=FALSE)