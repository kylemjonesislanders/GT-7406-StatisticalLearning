# Model Development

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