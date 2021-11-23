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
none_cols <- c('Fence')
for(col in none_cols){
  dat[is.na(dat[,col]), col] <- 'None'
}
## List of NAN including columns where NaNs mean 0.
zero_cols = c('TotalBsmtSF', 'BsmtFullBath', 'BsmtHalfBath', 'GarageCars')
for(col in zero_cols){
  dat[is.na(dat[,col]), col] <- 0
}
## List of NAN including columns where we want to fill with mode.
freq_cols = c('KitchenQual', 'SaleType')
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

# Creating new features
dat$TotalSquareFootage <- dat$TotalBsmtSF + dat$GrLivArea
BsmtBathrooms <- dat$BsmtFullBath + 0.5*dat$BsmtHalfBath
FullBathrooms <- dat$FullBath + 0.5*dat$HalfBath
dat$TotalBathooms <- BsmtBathrooms + FullBathrooms
dat$SaleConditionAbnorml <- ifelse(dat$SaleCondition=="Abnorml", 1, 0)
dat$SaleConditionFamily <- ifelse(dat$SaleCondition=="Family", 1, 0)
dat$SaleConditionPartial <- ifelse(dat$SaleCondition=="Partial", 1, 0)
dat$FenceGoodPrivacy <- ifelse(dat$Fence%in%c("GdPrv"), 1, 0)
dat$BasementGoodCondition <- ifelse(dat$BsmtCond%in%c("Ex", "Gd"), 1, 0)
dat$GarageCar1 <- ifelse(dat$GarageCars==1, 1, 0)
dat$GarageCar2 <- ifelse(dat$GarageCars==2, 1, 0)
dat$GarageCar3 <- ifelse(dat$GarageCars>=3, 1, 0)
dat$FirePlace <- ifelse(dat$Fireplaces>=1, 1, 0)
dat$CentralAirConditioning <- ifelse(dat$CentralAir=='Y', 1, 0)
dat$PartialxTotalSquareFootage <- dat$TotalSquareFootage*dat$SaleConditionPartial
PorchSF <- dat$OpenPorchSF + dat$X3SsnPorch + dat$EnclosedPorch + dat$ScreenPorch + dat$WoodDeckSF
dat$Porch <- ifelse(PorchSF > 0, 1, 0)
unique_zones <- unique(dat$MSZoning)
for(zone in unique_zones){
  dummy_code <- ifelse(dat$MSZoning==zone, 1, 0)
  col_name <- paste0("MSZone_", zone)
  dat[,col_name] <- dummy_code
}
unique_SubClasses <- unique(dat$MSSubClass)
for(class in unique_SubClasses){
  dummy_code <- ifelse(dat$MSSubClass==class, 1, 0)
  col_name <- paste0("MSSubClass_", class)
  dat[,col_name] <- dummy_code
}
railroad_codes <- c('RRNn', 'RRAn', 'RRNe', 'RRAe')
dat$Railroad <- 0
dat[which(dat$Condition1%in%railroad_codes), "Railroad"] <- 1
dat[which(dat$Condition2%in%railroad_codes), "Railroad"] <- 1
dat$Park <- 0
dat[which(dat$Condition1=="PosN"), "Park"] <- 1
dat[which(dat$Condition2=="PosN"), "Park"] <- 1