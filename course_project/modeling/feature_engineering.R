# Clear workspace
rm(list=ls())

# Set working directory
setwd("/Users/kjone332/Desktop/RStudio_Projects/GT-7406-StatisticalLearning/course_project")

# Read in data
dat <- read.table(file="./data_files/train.csv", sep = ",", header=TRUE)

# Drop ID column
dat$Id <- NULL

# Handle NAN issue
## List of NaN including columns where NaNs mean none.
none_cols <- c('Alley', 'PoolQC', 'Fence', 'FireplaceQu', 'GarageType',
               'GarageFinish', 'GarageQual', 'GarageCond', 'BsmtQual', 'BsmtCond',
               'BsmtExposure', 'BsmtFinType1', 'MasVnrType')
for(col in none_cols){
  dat[is.na(dat[,col]), col] <- 'None'
}
## List of NAN including columns where NaNs mean 0.
zero_cols = c('BsmtFinSF1', 'BsmtFullBath', 'BsmtHalfBath', 
              'GarageArea', 'GarageCars')
for(col in zero_cols){
  dat[is.na(dat[,col]), col] <- 0
}
## List of NAN including columns where we want to fill with mode.
freq_cols = c('Electrical', 'Exterior1st', 'Exterior2nd', 
              'Functional', 'KitchenQual', 'SaleType', 'Utilities')
for(col in freq_cols){
  ux <- unique(dat[,col])
  most_common <- ux[which.max(tabulate(match(dat[,col], ux)))]
  dat[is.na(dat[,col]), col] <- most_common
}
