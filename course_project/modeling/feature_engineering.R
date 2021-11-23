# Clear workspace
rm(list=ls())

# Set working directory
setwd("/Users/kjone332/Desktop/RStudio_Projects/GT-7406-StatisticalLearning/course_project")

# Read in data
dat_train <- read.table(file="./data_files/train.csv", sep = ",", header=TRUE)
dat_train$SalePrice <- NULL
dat_test <- read.table(file="./data_files/test.csv", sep = ",", header=TRUE)
dat <- rbind(dat_train, dat_test)

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
## Filling missing MSZoning according to MSSubClass
unique_SubClasses <- unique(dat$MSSubClass)
for(class in unique_SubClasses){
  temp_dat <- dat[which(dat$MSSubClass==class), ]
  unique_zones <- unique(temp_dat$MSZoning)
  most_common <- unique_zones[which.max(tabulate(match(temp_dat[,"MSZoning"], unique_zones)))]
  dat[which(is.na(dat$MSZoning)&dat$MSSubClass==class), "MSZoning"] <- most_common
}
## Filling LotFrontage according to Neighborhood
unique_neighborhoods <- unique(dat$Neighborhood)
for(neighborhood in unique_neighborhoods){
  temp_dat <- dat[which(dat$Neighborhood==neighborhood), ]
  median_val <- median(temp_dat$LotFrontage, na.rm = TRUE)
  dat[which(is.na(dat$LotFrontage)&dat$Neighborhood==neighborhood), "LotFrontage"] <- median_val
}

# Converting categorical values to numeric
## Neighborhood
neighborhoods <- c('MeadowV', 'IDOTRR', 'BrDale', 'BrkSide', 'OldTown',
                   'Edwards', 'Sawyer', 'Blueste', 'SWISU', 'NPkVill',
                   'NAmes', 'Mitchel', 'SawyerW', 'NWAmes', 'Gilbert',
                   'Blmngtn', 'CollgCr', 'ClearCr', 'Crawfor', 'Veenker',
                   'Somerst', 'Timber', 'StoneBr', 'NridgHt', 'NoRidge')
ratings <- c(1,1,1,2,2,
             2,3,3,3,3,
             3,4,5,5,5,
             5,5,6,6,7,
             7,8,9,10,10)
neighborhood_rating <- data.frame(neighborhoods, ratings)
dat$NeighborhoodInt <- 0
unique_neighborhoods <- unique(dat$Neighborhood)
for(neighborhood in unique_neighborhoods){
  rating <- neighborhood_rating[which(neighborhood_rating$neighborhoods==neighborhood), ]$ratings
  dat[which(dat$Neighborhood==neighborhood), "NeighborhoodInt"] <- rating
}
## Quality
qualities <- c('Po', 'Fa', 'TA', 'Gd', 'Ex')
ratings <- c(1,2,3,4,5)
rating_map <- data.frame(qualities, ratings)
### ExterQual, ExterCond, HeatingQC, KitchenQual
dat$ExterQualInt <- 0
dat$ExterCondInt <- 0
dat$HeatingQCInt <- 0
dat$KitchenQualInt <- 0
for(quality in qualities){
  rating <- rating_map[which(rating_map$qualities==quality), ]$ratings
  dat[which(dat$ExterQual==quality), "ExterQualInt"] <- rating
  dat[which(dat$ExterCond==quality), "ExterCondInt"] <- rating
  dat[which(dat$HeatingQC==quality), "HeatingQCInt"] <- rating
  dat[which(dat$KitchenQual==quality), "KitchenQualInt"] <- rating
}