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

### Just Square Footage with outliers removed - r2 = 0.598
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

### Square Footage by SaleCondition
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
dat$BsmtBathrooms <- dat$BsmtFullBath + dat$BsmtHalfBath
dat$FullBathrooms <- dat$FullBath + dat$HalfBath
dat$TotalBathooms <- dat$BsmtBathrooms + dat$FullBathrooms
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
dat$TotalBathooms <- ifelse(dat$TotalBathooms > 4, 4, dat$TotalBathooms)
# Create DataFrame for plot
plot_df <- rbind(dat[dat$TotalBathooms==1, c("TotalBathooms", "PricePerSqFoot")],
                 dat[dat$TotalBathooms==2, c("TotalBathooms", "PricePerSqFoot")],
                 dat[dat$TotalBathooms==3, c("TotalBathooms", "PricePerSqFoot")],
                 dat[dat$TotalBathooms==4, c("TotalBathooms", "PricePerSqFoot")])
jpeg(file="./exploratory/images/TotalBathrooms_ppsqf.jpeg",
     width=width_image, height=height_image)
ggplot(plot_df, aes(x = TotalBathooms, y = PricePerSqFoot, group = TotalBathooms)) +
  geom_boxplot() + 
  theme_bw(base_size = 16) + 
  xlab("Total Bathrooms") +
  ylab("Price per Square Foot") +
  scale_x_continuous(breaks = c(1, 2, 3, 4), label = c('1', '2', '3', '4+')) +
  scale_y_continuous(labels=function(x) paste0('$',x))
dev.off()