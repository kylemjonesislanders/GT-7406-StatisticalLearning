# Final Ensemble Model (Lasso + Random Forest), with predictions on Test Set
rm(list=ls())

# Libraries
library("ggplot2")
library("scales")
library("randomForest")
library("caret")
library("glmnet")

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
## 1) Lasso
cv_lasso <- glmnet::cv.glmnet(as.matrix(train_dat[,2:ncol(train_dat)]), train_dat$LogSalePrice)
### Plot of Variable Importance
lasso_coefficients <- coef(cv_lasso, s = "lambda.1se")
summ <- summary(lasso_coefficients)
plot_df <- data.frame(Predictor = rownames(lasso_coefficients)[summ$i],
                      Coefficient = summ$x)
plot_df <- plot_df[which(plot_df$Predictor!='(Intercept)'), ]
plot_df$AbsCoefficient <- abs(plot_df$Coefficient)
plot_df <- plot_df[order(-plot_df$AbsCoefficient),]
plot_df$Num <- c(1:nrow(plot_df))
jpeg(file="./modeling/images/VarImportanceLasso.jpeg",
     width=500, height=500)
ggplot(plot_df, aes(x=reorder(Predictor, -Num), y=Coefficient)) +
  geom_bar(stat='identity') +
  coord_flip() +
  xlab("Predictor") +
  ylab("Lasso Scaled Coefficient") +
  theme_bw(base_size = 16)
dev.off()

## 2) Random Forest
modellist <- list()
control <- trainControl(method='boot', number=10, search='grid')
tunegrid <- expand.grid(.mtry = (5:15))
for (ntree in c(100, 500, 1000)){
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
results_100 <- modellist$`100`$results[, c('mtry', 'RMSE')]
results_100$ntree <- 100
results_500 <- modellist$`500`$results[, c('mtry', 'RMSE')]
results_500$ntree <- 500
results_1000 <- modellist$`1000`$results[, c('mtry', 'RMSE')]
results_1000$ntree <- 1000
final_df <- rbind(results_100, results_500, results_1000)
best_result <- final_df[which(final_df$RMSE==min(final_df$RMSE)), ]
optimal_mtry <- best_result$mtry
optimal_ntree <- best_result$ntree
rf_optimal <- randomForest(LogSalePrice ~.,
                           data=train_dat,
                           ntree= optimal_ntree,
                           mtry=optimal_mtry,
                           importance=TRUE)
### Plot of cross validation error
jpeg(file="./modeling/images/RFOptimization.jpeg",
     width=750, height=450)
final_df$ntree <- factor(final_df$ntree, levels = c('100', '500', '1000'))
ggplot(final_df, aes(x = mtry, y = RMSE, color = ntree, group = ntree)) +
  geom_point() + geom_line() + theme_bw(base_size = 20) +
  scale_x_continuous(breaks = c(5:15),
                     labels = c('5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15'))
dev.off()
### Plot of variable importance
var_importance_rf <- data.frame(importance(rf_optimal))
var_importance_rf <- var_importance_rf[which(var_importance_rf$X.IncMSE>=5), ]
var_importance_rf <- var_importance_rf[order(-var_importance_rf$X.IncMSE),]
var_importance_rf$Num <- c(1:nrow(var_importance_rf))
var_importance_rf$Predictor <- rownames(var_importance_rf)
jpeg(file="./modeling/images/VarImportanceRF.jpeg",
     width=500, height=500)
ggplot(var_importance_rf, aes(x=reorder(Predictor, -Num), y=X.IncMSE)) +
  geom_bar(stat='identity') +
  coord_flip() +
  xlab("Predictor") +
  ylab("Mean Decrease in MSE") +
  theme_bw(base_size = 16)
dev.off()

# Predictions on Train Set
predictions_train_RF <- predict(rf_optimal, train_dat)
predictions_train_Lasso <- predict(cv_lasso, as.matrix(train_dat[,2:ncol(train_dat)]), s = "lambda.1se")
predictions_train_ensemble <- rowMeans(cbind(predictions_train_RF,predictions_train_Lasso))
rmse_train <- sqrt(mean((train_dat$LogSalePrice - predictions_train_ensemble)^2))
## Mean Absolute Deviation
mean_deviation <- mean(abs(exp(train_dat$LogSalePrice) - exp(predictions_train_ensemble)))
mean_pct_error <- mean(abs((exp(train_dat$LogSalePrice) - exp(predictions_train_ensemble))/exp(train_dat$LogSalePrice)))


# Predict on Test Set
predictions_test_RF <- predict(rf_optimal, test_dat)
predictions_test_Lasso <- predict(cv_lasso, as.matrix(test_dat[,1:ncol(test_dat)]), s = "lambda.1se")
predictions_test_ensemble <- rowMeans(cbind(predictions_test_RF, predictions_test_Lasso))

# Create submission csv file
ids <- test_dat1$Id
predictions_test_normal <- exp(predictions_test_ensemble)
final_predictions <- data.frame(ids, predictions_test_normal)
colnames(final_predictions) <- c("Id", "SalePrice")

# Write to csv
write.csv(final_predictions, './modeling/final_results.csv', row.names=FALSE)