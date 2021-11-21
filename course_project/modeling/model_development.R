# Model Development
rm(list=ls())

# Libraries
library("ggplot2")
library("scales")
library("Hmisc")

# Set working directory
setwd("/Users/kjone332/Desktop/RStudio_Projects/GT-7406-StatisticalLearning/course_project")

# Read in training data and initial feature engineering
dat <- read.table(file="./data_files/train.csv", sep = ",", header=TRUE)
dat$LogSalePrice <- log(dat$SalePrice)
dat$TotalSquareFootage <- dat$TotalBsmtSF + dat$GrLivArea
dat$BsmtBathrooms <- dat$BsmtFullBath + dat$BsmtHalfBath
dat$FullBathrooms <- dat$FullBath + dat$HalfBath
dat$TotalBathooms <- dat$BsmtBathrooms + dat$FullBathrooms

# Remove outliers
dat <- dat[dat$TotalSquareFootage<7000, ]

# Initialize model_dat with continuous x predictors and y
model_dat <- dat[ , c("LogSalePrice", "TotalSquareFootage", "YearRemodAdd")]

# Scale continuous x predictors
model_dat$TotalSquareFootage <- scale(model_dat$TotalSquareFootage)
model_dat$YearRemodAdd <- scale(model_dat$YearRemodAdd)

# User created categorical variables for model
## Neighborhood
model_dat$CollgCr <- ifelse(dat$Neighborhood=="CollgCr", 1, 0)
model_dat$Veenker <- ifelse(dat$Neighborhood=="Veenker", 1, 0)
model_dat$Crawfor <- ifelse(dat$Neighborhood=="Crawfor", 1, 0)
model_dat$NoRidge <- ifelse(dat$Neighborhood=="NoRidge", 1, 0)
model_dat$Mitchel <- ifelse(dat$Neighborhood=="Mitchel", 1, 0)
model_dat$Somerst <- ifelse(dat$Neighborhood=="Somerst", 1, 0)
model_dat$NWAmes <- ifelse(dat$Neighborhood=="NWAmes", 1, 0)
model_dat$BrkSide <- ifelse(dat$Neighborhood=="BrkSide", 1, 0)
model_dat$Sawyer <- ifelse(dat$Neighborhood=="Sawyer", 1, 0)
model_dat$NridgHt <- ifelse(dat$Neighborhood=="NridgHt", 1, 0)
model_dat$NAmes <- ifelse(dat$Neighborhood=="NAmes", 1, 0)
model_dat$SawyerW <- ifelse(dat$Neighborhood=="SawyerW", 1, 0)
model_dat$Timber <- ifelse(dat$Neighborhood=="Timber", 1, 0)
model_dat$Gilbert <- ifelse(dat$Neighborhood=="Gilbert", 1, 0)
model_dat$StoneBr <- ifelse(dat$Neighborhood=="StoneBr", 1, 0)
model_dat$ClearCr <- ifelse(dat$Neighborhood=="ClearCr", 1, 0)
model_dat$NPkVill <- ifelse(dat$Neighborhood=="NPkVill", 1, 0)
model_dat$Blmngtn <- ifelse(dat$Neighborhood=="Blmngtn", 1, 0)
model_dat$SWISU <- ifelse(dat$Neighborhood=="SWISU", 1, 0)
model_dat$OldTown <- ifelse(dat$Neighborhood=="OldTown", 1, 0)
model_dat$Edwards <- ifelse(dat$Neighborhood=="Edwards", 1, 0)
#model_dat$BrDale <- ifelse(dat$Neighborhood=="BrDale", 1, 0)
#model_dat$Blueste <- ifelse(dat$Neighborhood=="Blueste", 1, 0)
#model_dat$IDOTRR <- ifelse(dat$Neighborhood=="IDOTRR", 1, 0)
#model_dat$MeadowV <- ifelse(dat$Neighborhood=="MeadowV", 1, 0)
# Sale Condition
model_dat$SaleConditionAbnorml <- ifelse(dat$SaleCondition=="Abnorml", 1, 0)
model_dat$SaleConditionFamily <- ifelse(dat$SaleCondition=="Family", 1, 0)
model_dat$SaleConditionPartial <- ifelse(dat$SaleCondition=="Partial", 1, 0)
# Total Bathrooms
model_dat$TotalBathooms2 <- ifelse(dat$TotalBathooms==2, 1, 0)
model_dat$TotalBathooms3 <- ifelse(dat$TotalBathooms==3, 1, 0)
model_dat$TotalBathooms4 <- ifelse(dat$TotalBathooms>=4, 1, 0)
# Fence
model_dat$FenceGoodPrivacy <- ifelse(dat$Fence%in%c("GdPrv"), 1, 0)
# Basement
model_dat$BasementGoodCondition <- ifelse(dat$BsmtCond%in%c("Ex", "Gd"), 1, 0)
# GarageCarSize
model_dat$GarageCar1 <- ifelse(dat$GarageCars==1, 1, 0)
model_dat$GarageCar2 <- ifelse(dat$GarageCars==2, 1, 0)
model_dat$GarageCar3 <- ifelse(dat$GarageCars>=3, 1, 0)
# FirePlaces
model_dat$FirePlace <- ifelse(dat$Fireplaces>=1, 1, 0)

# Interaction Term
model_dat$PartialxTotalSquareFootage <- model_dat$TotalSquareFootage*model_dat$SaleConditionPartial

# Build Test Model (R2 = 0.87)
#mod <- lm(LogSalePrice ~., model_dat)
#summary(mod)

# Monte Carlo Cross Validation
n1 = 1166
n2 = 292
n = dim(model_dat)[1]
# Set seed for randomization
set.seed(70)
# Define number of iterations to perform during CV
B = 100
# Initialize matrix to hold values in loop
error_results <- matrix(ncol=4, nrow=1*B*2)
iterator <- 1

for (b in 1:B){
  # Isolate train and test sets
  flag <- sort(sample(1:n, n1))
  traintemp <- model_dat[flag,]
  ytrain <- traintemp$LogSalePrice
  testtemp <- model_dat[-flag,]
  ytest <- testtemp$LogSalePrice

  # 1) Linear regression with all predictors
  model1 <- lm(LogSalePrice ~ ., data = traintemp)
  predictions_train <- predict(model1, traintemp[ , 2:ncol(traintemp)])
  rmse_train <- sqrt(mean((ytrain - predictions_train)^2))
  predictions_test <- predict(model1, testtemp[ , 2:ncol(testtemp)])
  rmse_test <- sqrt(mean((ytest - predictions_test)^2))

  error_results[iterator, 1:4] <- c('Linear Regression', b, rmse_train, 'Train')
  iterator <- iterator + 1
  error_results[iterator, 1:4] <- c('Linear Regression', b, rmse_test, 'Test')
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
ggplot(plot_df, aes(x = Model, y = Value, fill=Group)) +
       geom_boxplot() +
       ylab("RMSE") +
       xlab("Model") +
       theme(legend.title=element_blank())