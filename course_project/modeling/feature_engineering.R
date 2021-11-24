# Function that creates predictor columns
feature_engineering <- function(dat){
  # Drop ID column
  dat$SalePrice <- NULL
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
  freq_cols = c('KitchenQual')
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
  neighborhoods <- c('SWISU', 'IDOTRR',
                     'OldTown', 'Edwards', 'BrDale', 'MeadowV',  
                     'BrkSide',  'NAmes', 'NPkVill', 
                     'Sawyer', 'NWAmes', 'Mitchel', 'Blueste', 'Blmngtn', 'ClearCr', 
                     'SawyerW', 'CollgCr', 
                     'Crawfor', 'Gilbert', 'Timber', 'Veenker',
                     'NoRidge', 'Somerst',   
                     'StoneBr', 'NridgHt')
  ratings <- c(1,1,
               2,2,2,2,
               3,3,3,
               4,4,4,4,4,4,
               5,5,
               6,6,6,6,
               7,7,
               8,8)
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
  dat$TotalBathooms <- ifelse(dat$TotalBathooms > 3.5, 3.5, dat$TotalBathooms)
  dat$SaleConditionAbnorml <- ifelse(dat$SaleCondition=="Abnorml", 1, 0)
  dat$SaleConditionFamily <- ifelse(dat$SaleCondition=="Family", 1, 0)
  dat$SaleConditionPartial <- ifelse(dat$SaleCondition=="Partial", 1, 0)
  dat$SaleConditionNormal <- ifelse(dat$SaleCondition=="Normal", 1, 0)
  dat$FenceGoodPrivacy <- ifelse(dat$Fence%in%c("GdPrv"), 1, 0)
  dat$BasementGoodCondition <- ifelse(dat$BsmtCond%in%c("Ex", "Gd"), 1, 0)
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

  # Create final dataframe
  final_cols <- c("LotFrontage",
                  "YearRemodAdd",
                  "NeighborhoodInt",
                  "ExterQualInt",
                  "ExterCondInt",
                  "HeatingQCInt",
                  "KitchenQualInt",
                  "TotalSquareFootage",
                  "TotalBathooms",
                  "SaleConditionAbnorml",
                  "SaleConditionFamily",
                  "SaleConditionPartial",
                  "SaleConditionNormal",
                  "FenceGoodPrivacy",
                  "BasementGoodCondition",
                  "GarageCars",
                  "FirePlace",
                  "CentralAirConditioning",
                  "PartialxTotalSquareFootage",
                  "Porch",
                  "Railroad",
                  "Park",
                  "MSZone_RL", 
                  "MSZone_RM", 
                  "MSZone_C (all)",
                  "MSZone_FV",
                  "MSSubClass_60",             
                  "MSSubClass_20",
                  "MSSubClass_70",
                  "MSSubClass_50",
                  "MSSubClass_190",            
                  "MSSubClass_45",              
                  "MSSubClass_90",              
                  "MSSubClass_120",             
                  "MSSubClass_30",             
                  "MSSubClass_85",              
                  "MSSubClass_80",              
                  "MSSubClass_160",             
                  "MSSubClass_75",             
                  "MSSubClass_180")
  final_dat <- dat[ , final_cols]
  colnames(final_dat)[which(colnames(final_dat)=="MSZone_C (all)")] <- "MSZone_C"
  
  # Return final_dat
  return(final_dat)
}