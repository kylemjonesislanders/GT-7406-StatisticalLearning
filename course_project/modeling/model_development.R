# Model Development

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

# Initialize model_dat with y and continuous predictors
model_dat <- dat[ , c("LogSalePrice", "TotalSquareFootage", "YearRemodAdd")]

# User created categorical variables for model
## Neighborhood
model_dat$CollgCr <- ifelse(dat$Neighborhood=="CollgCr", 1, 0)
model_dat$Veenker <- ifelse(dat$Neighborhood=="Veenker", 1, 0)
model_dat$Crawfor <- ifelse(dat$Neighborhood=="Crawfor", 1, 0)
model_dat$NoRidge <- ifelse(dat$Neighborhood=="NoRidge", 1, 0)
model_dat$Mitchel <- ifelse(dat$Neighborhood=="Mitchel", 1, 0)
model_dat$Somerst <- ifelse(dat$Neighborhood=="Somerst", 1, 0)
model_dat$NWAmes <- ifelse(dat$Neighborhood=="NWAmes", 1, 0)
model_dat$OldTown <- ifelse(dat$Neighborhood=="OldTown", 1, 0)
model_dat$BrkSide <- ifelse(dat$Neighborhood=="BrkSide", 1, 0)
model_dat$Sawyer <- ifelse(dat$Neighborhood=="Sawyer", 1, 0)
model_dat$NridgHt <- ifelse(dat$Neighborhood=="NridgHt", 1, 0)
model_dat$NAmes <- ifelse(dat$Neighborhood=="NAmes", 1, 0)
model_dat$SawyerW <- ifelse(dat$Neighborhood=="SawyerW", 1, 0)
model_dat$IDOTRR <- ifelse(dat$Neighborhood=="IDOTRR", 1, 0)
model_dat$MeadowV <- ifelse(dat$Neighborhood=="MeadowV", 1, 0)
model_dat$Edwards <- ifelse(dat$Neighborhood=="Edwards", 1, 0)
model_dat$Timber <- ifelse(dat$Neighborhood=="Timber", 1, 0)
model_dat$Gilbert <- ifelse(dat$Neighborhood=="Gilbert", 1, 0)
model_dat$StoneBr <- ifelse(dat$Neighborhood=="StoneBr", 1, 0)
model_dat$ClearCr <- ifelse(dat$Neighborhood=="ClearCr", 1, 0)
model_dat$NPkVill <- ifelse(dat$Neighborhood=="NPkVill", 1, 0)
model_dat$Blmngtn <- ifelse(dat$Neighborhood=="Blmngtn", 1, 0)
model_dat$BrDale <- ifelse(dat$Neighborhood=="BrDale", 1, 0)
model_dat$Blueste <- ifelse(dat$Neighborhood=="Blueste", 1, 0)
# Sale Condition
model_dat$SaleConditionAbnorml <- ifelse(dat$SaleCondition=="Abnorml", 1, 0)
model_dat$SaleConditionFamily <- ifelse(dat$SaleCondition=="Family", 1, 0)
model_dat$SaleConditionPartial <- ifelse(dat$SaleCondition=="Partial", 1, 0)
# Total Bathrooms
model_dat$TotalBathooms2 <- ifelse(dat$TotalBathooms==2, 1, 0)
model_dat$TotalBathooms3 <- ifelse(dat$TotalBathooms==3, 1, 0)
model_dat$TotalBathooms4 <- ifelse(dat$TotalBathooms>=4, 1, 0)

# Build Model (R2 = 0.85)
mod <- lm(LogSalePrice ~., model_dat)
summary(mod)