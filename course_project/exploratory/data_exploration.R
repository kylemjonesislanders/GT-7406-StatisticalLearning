# Libraries
library("ggplot2")
library("scales")
library("Hmisc")

# Set working directory
setwd("/Users/kjone332/Desktop/RStudio_Projects/GT-7406-StatisticalLearning/course_project")

# Read in training data
dat <- read.table(file="./data_files/train.csv", sep = ",", header=TRUE)

# User created variables
dat$LogSalePrice <- log(dat$SalePrice)
dat$TotalSquareFootage <- dat$TotalBsmtSF + dat$GrLivArea
dat$PricePerSqFoot <- dat$SalePrice/dat$TotalSquareFootage
dat$LogPricePerSqFoot <- dat$LogSalePrice/dat$TotalSquareFootage

# Exploratory Analysis
width_image <- 600
height_image <- 350

## 1) Distribution of response variable before and after log transformation
### Raw Sale Price
jpeg(file="./exploratory/images/density_SalePrice.jpeg",
    width=width_image, height=height_image)
ggplot(dat, aes(x=SalePrice)) + 
  geom_density() +
  geom_vline(aes(xintercept=mean(SalePrice)),
             color="blue", linetype="dashed", size=1) +
  scale_x_continuous(labels = comma) +
  xlab("Sale Price") +
  ylab("Density") +
  theme_bw(base_size = 16)
dev.off()
### Log Sale Price
jpeg(file="./exploratory/images/density_LogSalePrice.jpeg",
     width=width_image, height=height_image)
ggplot(dat, aes(x=LogSalePrice)) + 
  geom_density() +
  geom_vline(aes(xintercept=mean(LogSalePrice)),
             color="blue", linetype="dashed", size=1) +
  xlab("Log of Sale Price") +
  ylab("Density") +
  theme_bw(base_size = 16)
dev.off()

## 2) Relationship between LogSalePrice and square footage
### Just Square Footage - r2 = 0.598
jpeg(file="./exploratory/images/square_footage_basic.jpeg",
     width=width_image, height=height_image)
ggplot(dat, aes(TotalSquareFootage, LogSalePrice)) +
  geom_point(color='red') +
  geom_smooth(method='lm') +
  geom_text(x = 1500, y = 15.25, label = "r2 = 0.60") +
  xlab("Total Square Footage") +
  ylab("Log of Sale Price") +
  theme_bw(base_size = 16)
dev.off()

### Just Square Footage with outliers removed - r2 = 0.67
jpeg(file="./exploratory/images/square_footage_basic_wo_outliers.jpeg",
     width=width_image, height=height_image)
ggplot(dat[dat$TotalSquareFootage<7000, ], aes(TotalSquareFootage, LogSalePrice)) +
  geom_point(color='red') +
  geom_smooth(method='lm') +
  geom_text(x = 1500, y = 13.5, label = "r2 = 0.67") +
  xlab("Total Square Footage") +
  ylab("Log of Sale Price") +
  theme_bw(base_size = 16)
dev.off()


### Remove outliers
dat <- dat[dat$TotalSquareFootage<7000, ]

### Square Footage by SaleCondition
# Basic Scatter Plot, colored by condition
jpeg(file="./exploratory/images/square_footage_byCondition.jpeg",
     width=width_image, height=height_image)
names(dat)[names(dat) == "SaleCondition"] <- "Sale Condition"
ggplot(dat, aes(TotalSquareFootage, LogSalePrice, colour = `Sale Condition`)) +
  geom_point() +
  xlab("Total Square Footage") +
  ylab("Log of Sale Price") + 
  xlim(0,7000) + 
  theme_bw(base_size = 16)
dev.off()
# Price per square foot by condition
plot_df <- rbind(dat[dat$`Sale Condition`=="Normal", c("Sale Condition", "PricePerSqFoot")],
                 dat[dat$`Sale Condition`=="Abnorml", c("Sale Condition", "PricePerSqFoot")],
                 dat[dat$`Sale Condition`=="Partial", c("Sale Condition", "PricePerSqFoot")],
                 dat[dat$`Sale Condition`=="AdjLand", c("Sale Condition", "PricePerSqFoot")],
                 dat[dat$`Sale Condition`=="Alloca", c("Sale Condition", "PricePerSqFoot")],
                 dat[dat$`Sale Condition`=="Family", c("Sale Condition", "PricePerSqFoot")])
jpeg(file="./exploratory/images/SaleCondition_ppsqf.jpeg",
     width=width_image, height=height_image)
ggplot(plot_df, aes(x = `Sale Condition`, y = PricePerSqFoot, colour = `Sale Condition`)) +
  geom_boxplot() + 
  theme_bw(base_size = 16) + 
  xlab(" ") +
  ylab("Price per Square Foot") +
  scale_x_discrete(breaks = c('Abnorml', 'AdjLand', 'Alloca', 
                              'Family', 'Normal', 'Partial'), 
                   label = c('Abnorml\nN=100', 'AdjLand\nN=4', 'Alloca\nN=12', 
                             'Family\nN=20', 'Normal\nN=1198', 'Partial\nN=123')) +
  scale_y_continuous(labels=function(x) paste0('$',x))
dev.off()

### Price per square foot by Zone
unique_zones <- unique(as.character(dat$MSZoning))
output <- matrix(ncol=3, nrow=length(unique_zones))
iterator <- 1
for (zone in unique_zones){
  temp_dat <- dat[dat$MSZoning==zone, ]
  number_obs <- nrow(temp_dat)
  output[iterator, 1] <- zone
  output[iterator, 2] <- median(temp_dat$PricePerSqFoot)
  output[iterator, 3] <- number_obs
  iterator <- iterator+1
}
output <- as.data.frame(output)
colnames(output) <- c("zones", 'ppsqft', 'n')
output <- transform(output, ppsqft = as.numeric(ppsqft), n = as.numeric(n))
jpeg(file="./exploratory/images/Zone_ppsqf.jpeg",
     width=width_image, height=height_image)
ggplot(dat, aes(x = MSZoning, y = PricePerSqFoot, group = MSZoning)) +
  geom_boxplot() + 
  theme_bw(base_size = 16) + 
  xlab("MS Zone") +
  ylab("Price per Square Foot") +
  scale_y_continuous(labels=function(x) paste0('$',x))
dev.off()

### Price per square foot by Neighborhood
unique_neighborhoods <- unique(as.character(dat$Neighborhood))
output <- matrix(ncol=3, nrow=25)
iterator <- 1
for (neighborhood in unique_neighborhoods){
  temp_dat <- dat[dat$Neighborhood==neighborhood, ]
  number_obs <- nrow(temp_dat)
  output[iterator, 1] <- neighborhood
  output[iterator, 2] <- median(temp_dat$PricePerSqFoot)
  output[iterator, 3] <- number_obs
  iterator <- iterator+1
}
# Convert to dataframe
output <- as.data.frame(output)
colnames(output) <- c("neighborhood", 'ppsqft', 'n')
output <- transform(output, ppsqft = as.numeric(ppsqft), n = as.numeric(n))
# Get neighborhoods with min and max cost
max_neighborhood <- output[output$ppsqft == max(output$ppsqft), "neighborhood"]
min_neighborhood <- output[output$ppsqft == min(output$ppsqft), "neighborhood"]
# Create Dataframe for plot
plot_df <- rbind(dat[dat$Neighborhood==max_neighborhood, c("Neighborhood", "PricePerSqFoot")],
                 dat[dat$Neighborhood==min_neighborhood, c("Neighborhood", "PricePerSqFoot")])
jpeg(file="./exploratory/images/Neighborhood_ppsqf.jpeg",
     width=width_image, height=height_image)
ggplot(plot_df, aes(x = Neighborhood, y = PricePerSqFoot, colour = Neighborhood)) +
      geom_boxplot() + 
      theme_bw(base_size = 16) + 
      ylab("Price per Square Foot") +
      scale_y_continuous(labels=function(x) paste0('$',x))
dev.off()


### Price per square foot by year of remodel
unique_years <- unique(as.character(dat$YearRemodAdd))
output <- matrix(ncol=3, nrow=61)
iterator <- 1
for (year in unique_years){
  temp_dat <- dat[dat$YearRemodAdd==year, ]
  number_obs <- nrow(temp_dat)
  output[iterator, 1] <- year
  output[iterator, 2] <- median(temp_dat$PricePerSqFoot)
  output[iterator, 3] <- number_obs
  iterator <- iterator+1
}
# Convert to dataframe
output <- as.data.frame(output)
colnames(output) <- c("yearremod", 'ppsqft', 'n')
output <- transform(output, 
                    yearremod = as.numeric(yearremod), 
                    ppsqft = as.numeric(ppsqft), 
                    n = as.numeric(n))
jpeg(file="./exploratory/images/YearRemod_ppsqft.jpeg",
     width=width_image, height=height_image)
ggplot(output, aes(yearremod, ppsqft)) +
  geom_point() +
  xlab("Year of Remodel") +
  ylab("Price Per Square Foot") + 
  theme_bw(base_size = 16) + 
  scale_y_continuous(labels=function(x) paste0('$',x))
dev.off()

### Number of Bathrooms
BsmtBathrooms <- dat$BsmtFullBath + 0.5*dat$BsmtHalfBath
FullBathrooms <- dat$FullBath + 0.5*dat$HalfBath
dat$TotalBathooms <- BsmtBathrooms + FullBathrooms
unique_bathrooms <- unique(dat$TotalBathooms)
output <- matrix(ncol=3, nrow=length(unique_bathrooms))
iterator <- 1
for (bathroom in unique_bathrooms){
  temp_dat <- dat[dat$TotalBathooms==bathroom, ]
  number_obs <- nrow(temp_dat)
  output[iterator, 1] <- bathroom
  output[iterator, 2] <- median(temp_dat$PricePerSqFoot)
  output[iterator, 3] <- number_obs
  iterator <- iterator+1
}
# Convert to dataframe
output <- as.data.frame(output)
colnames(output) <- c("bathrooms", 'ppsqft', 'n')
output <- transform(output, 
                    bathrooms = as.numeric(bathrooms), 
                    ppsqft = as.numeric(ppsqft), 
                    n = as.numeric(n))
# Adjust for 4+ bathrooms
dat$TotalBathooms <- ifelse(dat$TotalBathooms > 3.5, 3.5, dat$TotalBathooms)
# Create DataFrame for plot
plot_df <- rbind(dat[dat$TotalBathooms==1, c("TotalBathooms", "PricePerSqFoot")],
                 dat[dat$TotalBathooms==2, c("TotalBathooms", "PricePerSqFoot")],
                 dat[dat$TotalBathooms==3, c("TotalBathooms", "PricePerSqFoot")],
                 dat[dat$TotalBathooms==4, c("TotalBathooms", "PricePerSqFoot")])
jpeg(file="./exploratory/images/TotalBathrooms_ppsqf.jpeg",
     width=width_image, height=height_image)
ggplot(dat, aes(x = TotalBathooms, y = PricePerSqFoot, group = TotalBathooms)) +
  geom_boxplot() + 
  theme_bw(base_size = 16) + 
  xlab("Total Bathrooms") +
  ylab("Price per Square Foot") +
  scale_x_continuous(breaks = c(1, 1.5, 2, 2.5, 3, 3.5), 
                     label = c('1', '1.5', '2', '2.5', '3', '3.5+')) +
  scale_y_continuous(labels=function(x) paste0('$',x))
dev.off()

# Garage Cars
unique_garages <- unique(dat$GarageCars)
output <- matrix(ncol=3, nrow=length(unique_garages))
iterator <- 1
for (garage in unique_garages){
  temp_dat <- dat[dat$GarageCars==garage, ]
  number_obs <- nrow(temp_dat)
  output[iterator, 1] <- garage
  output[iterator, 2] <- median(temp_dat$PricePerSqFoot)
  output[iterator, 3] <- number_obs
  iterator <- iterator+1
}
# Convert to dataframe
output <- as.data.frame(output)
colnames(output) <- c("garages", 'ppsqft', 'n')
output <- transform(output, 
                    garages = as.numeric(garages), 
                    ppsqft = as.numeric(ppsqft), 
                    n = as.numeric(n))
# Adjust for 3+ garages
dat$TotalGarages <- ifelse(dat$GarageCars > 3, 3, dat$GarageCars)
# Create DataFrame for plot
plot_df <- rbind(dat[dat$TotalGarages==0, c("TotalGarages", "PricePerSqFoot")],
                 dat[dat$TotalGarages==1, c("TotalGarages", "PricePerSqFoot")],
                 dat[dat$TotalGarages==2, c("TotalGarages", "PricePerSqFoot")],
                 dat[dat$TotalGarages==3, c("TotalGarages", "PricePerSqFoot")])
jpeg(file="./exploratory/images/TotalGarages_ppsqf.jpeg",
     width=width_image, height=height_image)
ggplot(plot_df, aes(x = TotalGarages, y = PricePerSqFoot, group = TotalGarages)) +
  geom_boxplot() + 
  theme_bw(base_size = 16) + 
  xlab("Total Garages") +
  ylab("Price per Square Foot") +
  scale_x_continuous(breaks = c(0,1,2,3), label = c('0', '1', '2', '3+')) +
  scale_y_continuous(labels=function(x) paste0('$',x))
dev.off()