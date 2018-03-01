#------------------- Global Sales Case-Study ---------------------#

# "Global Mart" is a giant online super store having worldwide operations #
# The major product categories that it deals with are consumer, corporate & home office #
# The store caters to 7 different market segments and in 3 major categories #
# As we have to forecast at this granular level, we need to subset the data into 21 (7*3) #
# buckets before analysing the data #

# All of these 21 market buckets are not important from the store's point of view #
# We need to find out 2 most profitable (and consistent) segment from these 21 and #
# forecast the sales and quantity for these segments #

#----------------------------------------------------------------#

# Loading the required library
library(dplyr)
library(ggplot2)
library(cowplot)
library(corrplot)
library(lubridate)
library(tidyr)
library(raster)
library(graphics)
library(forecast)
library(tseries)

#----------------------------------------------------------------#

# Reading the data file into R
globalsales <- read.csv("Global Superstore.csv",sep = ",", header = TRUE)
View(globalsales)

# Examining the structure of the dataset #
str(globalsales)

# Exploring the dataset #
summary(globalsales)

#----------------------------------------------------------------#

# Dropping or removing the first columns that has information about row ids #
globalsales <- globalsales[,-1]

# Checking if there are any missing value in the datasets #
sapply(globalsales, function(x) sum(is.na(x)))

# We see that there are 41296 missing values in postal code column #
# This will not cause any issues in our analysis, so we will leave it as it is # 

#---------------------------------------------------------------#

# The raw file has different date formats in it, hence we will first #
# make them into single format#
# Changing the date format in Order.Date column #
globalsales$Order.Date <- as.Date(globalsales$Order.Date, "%d-%m-%Y")

# Changing the date format in Ship.Date column #
globalsales$Ship.Date <- as.Date(globalsales$Ship.Date, "%d-%m-%Y")

#---------------------------------------------------------------#

# Using the lubridate package here #
# Getting the month number out of the order.date column #
globalsales$month <- month(globalsales$Order.Date, label = FALSE)

# Getting the quater information out of the order.date column #
globalsales$quater <- quarter(globalsales$Order.Date, with_year = TRUE)

# We will combine the month and year columns into one column #
globalsales$quater_month <- paste(globalsales$quater, globalsales$month, sep="_")

# Dropping the month & year columns as we already have merged them into one #
globalsales <- globalsales[,-c(24,25)]

#---------------------------------------------------------------#

# To segment the whole dataset into the 21 subsets based on the market #
# and the customer segment level we need to first create a concat field of both #
globalsales$market_customer_segment <- paste(globalsales$Market, globalsales$Segment , sep="_")

#---------------------------------------------------------------#

# Before making the 21 subsets we need to first check what is the profit that each #  
# market, customer segment combination has earned without removing the negative profit numbers #
market_customer_sum <- summarise(group_by(globalsales, market_customer_segment), sum(Profit,na.rm = TRUE))

# Below are the top two profit making segments, when negative profits are included # 
# Market_customer_segment sum #
# APAC_Consumer           222817.560 #
# EU_Consumer             188687.707 #

# Now we will see the segments that make most of the profit without negative profits #
only_positive_profit <- globalsales[which(globalsales$Profit >= 0),]

market_customer_positive_profit <- summarise(group_by(only_positive_profit, market_customer_segment), sum(Profit,na.rm = TRUE))

# Below are the top two profit making segments, when negative profits are excluded # 
# Market_customer_segment sum #
# APAC_Consumer           322691.22 #
# EU_Consumer             287211.94 #

# We see that the top two segments remians same with or without the negative numbers #
# but we do not know if they are consistently profitable segments so for that we need to #
# subset the data into 21 segments #
#---------------------------------------------------------------#
# Now we will create the 21 subsets by taking into consideration only the positive profit numbers #
#---------------------------------------------------------------# 

# Subsetting APAC_Consumer from only_positive_profit dataframe #
APAC_Consumer <- subset(only_positive_profit, market_customer_segment == "APAC_Consumer")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty # 
APAC_Consumer_summary <- summarise(group_by(APAC_Consumer, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
APAC_Consumer_summary$month_ID <- seq.int(nrow(APAC_Consumer_summary))

# Using the raster package to find the coefficient of variation
APAC_Consumer_profit_cv <- cv(APAC_Consumer_summary$`sum(Profit)`)
# Profit CV for Consumer segment in APAC: 53.94%

# Checking the mean of profit for the subset #
APAC_Consumer_profit_mean <- mean(APAC_Consumer_summary$`sum(Profit)`)
# Mean Profit: 6722.73

#---------------------------------------------------------------#

# Subsetting APAC_Corporate from only_positive_profit dataframe #
APAC_Corporate <- subset(only_positive_profit, market_customer_segment == "APAC_Corporate")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
APAC_Corporate_summary <- summarise(group_by(APAC_Corporate, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
APAC_Corporate_summary$month_ID <- seq.int(nrow(APAC_Corporate_summary))

# Using the raster package to find the coefficient of variation
APAC_Corporate_profit_cv <- cv(APAC_Corporate_summary$`sum(Profit)`)
# Profit CV for Corporate segment in APAC: 51.89% 

# Checking the mean of profit for the subset #
APAC_Corporate_profit_mean <- mean(APAC_Corporate_summary$`sum(Profit)`)
# Mean Profit: 3933.04

#---------------------------------------------------------------#

# Subsetting APAC_Home_Office from only_positive_profit dataframe #
APAC_Home_Office <- subset(only_positive_profit, market_customer_segment == "APAC_Home Office")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
APAC_Home_Office_summary <- summarise(group_by(APAC_Home_Office, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
APAC_Home_Office_summary$month_ID <- seq.int(nrow(APAC_Home_Office_summary))

# Using the raster package to find the coefficient of variation
APAC_Home_Office_profit_cv <- cv(APAC_Home_Office_summary$`sum(Profit)`)
# Profit CV for Home Office segment in APAC: 71.31%

# Checking the mean of profit for the subset #
APAC_Home_Office_profit_mean <- mean(APAC_Home_Office_summary$`sum(Profit)`)
# Mean Profit: 2606.05


#---------------------------------------------------------------#

# Subsetting EU_Consumer from only_positive_profit dataframe #
EU_Consumer <- subset(only_positive_profit, market_customer_segment == "EU_Consumer")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
EU_Consumer_summary <- summarise(group_by(EU_Consumer, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
EU_Consumer_summary$month_ID <- seq.int(nrow(EU_Consumer_summary))

# Using the raster package to find the coefficient of variation
EU_Consumer_profit_cv <- cv(EU_Consumer_summary$`sum(Profit)`)
# Profit CV for Consumer segment in EU: 49.63%

# Checking the mean of profit for the subset #
EU_Consumer_profit_mean <- mean(EU_Consumer_summary$`sum(Profit)`)
# Mean Profit: 5983.58

#---------------------------------------------------------------#

# Subsetting EU_Corporate from only_positive_profit dataframe #
EU_Corporate <- subset(only_positive_profit, market_customer_segment == "EU_Corporate")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
EU_Corporate_summary <- summarise(group_by(EU_Corporate, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
EU_Corporate_summary$month_ID <- seq.int(nrow(EU_Corporate_summary))

# Using the raster package to find the coefficient of variation
EU_Corporate_profit_cv <- cv(EU_Corporate_summary$`sum(Profit)`)
# Profit CV for Corporate segment in EU: 61.35%

# Checking the mean of profit for the subset #
EU_Corporate_profit_mean <- mean(EU_Corporate_summary$`sum(Profit)`)
# Mean Profit: 3706.25

#---------------------------------------------------------------#

# Subsetting EU_Home_Office from only_positive_profit dataframe #
EU_Home_Office <- subset(only_positive_profit, market_customer_segment == "EU_Home Office")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
EU_Home_Office_summary <- summarise(group_by(EU_Home_Office, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
EU_Home_Office_summary$month_ID <- seq.int(nrow(EU_Home_Office_summary))

# Using the raster package to find the coefficient of variation
EU_Home_Office_profit_cv <- cv(EU_Home_Office_summary$`sum(Profit)`)
# Profit CV for Home Office segment in EU: 75.42%

# Checking the mean of profit for the subset #
EU_Home_Office_profit_mean <- mean(EU_Home_Office_summary$`sum(Profit)`)
# Mean Profit: 1956.38

#---------------------------------------------------------------#

# Subsetting US_Consumer from only_positive_profit dataframe #
US_Consumer <- subset(only_positive_profit, market_customer_segment == "US_Consumer")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
US_Consumer_summary <- summarise(group_by(US_Consumer, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
US_Consumer_summary$month_ID <- seq.int(nrow(US_Consumer_summary))

# Using the raster package to find the coefficient of variation
US_Consumer_profit_cv <- cv(US_Consumer_summary$`sum(Profit)`)
# Profit CV for Consumer segment in US: 69.07%

# Checking the mean of profit for the subset #
US_Consumer_profit_mean <- mean(US_Consumer_summary$`sum(Profit)`)
# Mean Profit: 4563.85

#---------------------------------------------------------------#

# Subsetting US_Corporate from only_positive_profit dataframe #
US_Corporate <- subset(only_positive_profit, market_customer_segment == "US_Corporate")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
US_Corporate_summary <- summarise(group_by(US_Corporate, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
US_Corporate_summary$month_ID <- seq.int(nrow(US_Corporate_summary))

# Using the raster package to find the coefficient of variation
US_Corporate_profit_cv <- cv(US_Corporate_summary$`sum(Profit)`)
# Profit CV for Corporate segment in US: 74.36%

# Checking the mean of profit for the subset #
US_Corporate_profit_mean <- mean(US_Corporate_summary$`sum(Profit)`)
# Mean Profit: 2849.29

#---------------------------------------------------------------#

# Subsetting US_Home_Office from only_positive_profit dataframe #
US_Home_Office <- subset(only_positive_profit, market_customer_segment == "US_Home Office")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
US_Home_Office_summary <- summarise(group_by(US_Home_Office, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
US_Home_Office_summary$month_ID <- seq.int(nrow(US_Home_Office_summary))

# Using the raster package to find the coefficient of variation
US_Home_Office_profit_cv <- cv(US_Home_Office_summary$`sum(Profit)`)
# Profit CV for Home Office segment in US: 83.29%

# Checking the mean of profit for the subset #
US_Home_Office_profit_mean <- mean(US_Home_Office_summary$`sum(Profit)`)
# Mean Profit: 1806.18

#---------------------------------------------------------------#

# Subsetting LATAM_Consumer from only_positive_profit dataframe #
LATAM_Consumer <- subset(only_positive_profit, market_customer_segment == "LATAM_Consumer")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
LATAM_Consumer_summary <- summarise(group_by(LATAM_Consumer, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
LATAM_Consumer_summary$month_ID <- seq.int(nrow(LATAM_Consumer_summary))

# Using the raster package to find the coefficient of variation
LATAM_Consumer_profit_cv <- cv(LATAM_Consumer_summary$`sum(Profit)`)
# Profit CV for Consumer segment in LATAM: 53.89%

# Checking the mean of profit for the subset #
LATAM_Consumer_profit_mean <- mean(LATAM_Consumer_summary$`sum(Profit)`)
# Mean Profit: 4307.09

#---------------------------------------------------------------#

# Subsetting LATAM_Corporate from only_positive_profit dataframe #
LATAM_Corporate <- subset(only_positive_profit, market_customer_segment == "LATAM_Corporate")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
LATAM_Corporate_summary <- summarise(group_by(LATAM_Corporate, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
LATAM_Corporate_summary$month_ID <- seq.int(nrow(LATAM_Corporate_summary))

# Using the raster package to find the coefficient of variation
LATAM_Corporate_profit_cv <- cv(LATAM_Corporate_summary$`sum(Profit)`)
# Profit CV for Corporate segment in LATAM: 66.75

# Checking the mean of profit for the subset #
LATAM_Corporate_profit_mean <- mean(LATAM_Corporate_summary$`sum(Profit)`)
# Mean Profit: 2256.76

#---------------------------------------------------------------#

# Subsetting LATAM_Home_Office from only_positive_profit dataframe #
LATAM_Home_Office <- subset(only_positive_profit, market_customer_segment == "LATAM_Home Office")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
LATAM_Home_Office_summary <- summarise(group_by(LATAM_Home_Office, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
LATAM_Home_Office_summary$month_ID <- seq.int(nrow(LATAM_Home_Office_summary))

# Using the raster package to find the coefficient of variation
LATAM_Home_Office_profit_cv <- cv(LATAM_Home_Office_summary$`sum(Profit)`)
# Profit CV for Home Office segment in LATAM: 68.38%

# Checking the mean of profit for the subset #
LATAM_Home_Office_profit_mean <- mean(LATAM_Home_Office_summary$`sum(Profit)`)
# Mean Profit: 1508.24

#---------------------------------------------------------------#

# Subsetting Africa_Consumer from only_positive_profit dataframe #
Africa_Consumer <- subset(only_positive_profit, market_customer_segment == "Africa_Consumer")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
Africa_Consumer_summary <- summarise(group_by(Africa_Consumer, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
Africa_Consumer_summary$month_ID <- seq.int(nrow(Africa_Consumer_summary))

# Using the raster package to find the coefficient of variation
Africa_Consumer_profit_cv <- cv(Africa_Consumer_summary$`sum(Profit)`)
# Profit CV for Consumer segment in Africa: 64.29%

# Checking the mean of profit for the subset #
Africa_Consumer_profit_mean <- mean(Africa_Consumer_summary$`sum(Profit)`)
# Mean Profit: 1988.61

#---------------------------------------------------------------#

# Subsetting Africa_Corporate from only_positive_profit dataframe #
Africa_Corporate <- subset(only_positive_profit, market_customer_segment == "Africa_Corporate")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
Africa_Corporate_summary <- summarise(group_by(Africa_Corporate, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
Africa_Corporate_summary$month_ID <- seq.int(nrow(Africa_Corporate_summary))

# Using the raster package to find the coefficient of variation
Africa_Corporate_profit_cv <- cv(Africa_Corporate_summary$`sum(Profit)`)
# Profit CV for Corporate segment in Africa: 81.08%

# Checking the mean of profit for the subset #
Africa_Corporate_profit_mean <- mean(Africa_Corporate_summary$`sum(Profit)`)
# Mean Profit: 978.96

#---------------------------------------------------------------#

# Subsetting Africa_Home_Office from only_positive_profit dataframe #
Africa_Home_Office <- subset(only_positive_profit, market_customer_segment == "Africa_Home Office")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
Africa_Home_Office_summary <- summarise(group_by(Africa_Home_Office, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
Africa_Home_Office_summary$month_ID <- seq.int(nrow(Africa_Home_Office_summary))

# Using the raster package to find the coefficient of variation
Africa_Home_Office_profit_cv <- cv(Africa_Home_Office_summary$`sum(Profit)`)
# Profit CV for Home Office segment in Africa: 95.67%

# Checking the mean of profit for the subset #
Africa_Home_Office_profit_mean <- mean(Africa_Home_Office_summary$`sum(Profit)`)
# Mean Profit: 735.48

#---------------------------------------------------------------#

# Subsetting EMEA_Consumer from only_positive_profit dataframe #
EMEA_Consumer <- subset(only_positive_profit, market_customer_segment == "EMEA_Consumer")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
EMEA_Consumer_summary <- summarise(group_by(EMEA_Consumer, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
EMEA_Consumer_summary$month_ID <- seq.int(nrow(EMEA_Consumer_summary))

# Using the raster package to find the coefficient of variation
EMEA_Consumer_profit_cv <- cv(EMEA_Consumer_summary$`sum(Profit)`)
# Profit CV for Consumer segment in EMEA: 68.77%

# Checking the mean of profit for the subset #
EMEA_Consumer_profit_mean <- mean(EMEA_Consumer_summary$`sum(Profit)`)
# Mean Profit: 1786.61

#---------------------------------------------------------------#

# Subsetting EMEA_Corporate from only_positive_profit dataframe #
EMEA_Corporate <- subset(only_positive_profit, market_customer_segment == "EMEA_Corporate")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
EMEA_Corporate_summary <- summarise(group_by(EMEA_Corporate, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
EMEA_Corporate_summary$month_ID <- seq.int(nrow(EMEA_Corporate_summary))

# Using the raster package to find the coefficient of variation
EMEA_Corporate_profit_cv <- cv(EMEA_Corporate_summary$`sum(Profit)`)
# Profit CV for Corporate segment in EMEA: 68.07%

# Checking the mean of profit for the subset #
EMEA_Corporate_profit_mean <- mean(EMEA_Corporate_summary$`sum(Profit)`)
# Mean Profit: 1093.61

#---------------------------------------------------------------#

# Subsetting EMEA_Home_Office from only_positive_profit dataframe #
EMEA_Home_Office <- subset(only_positive_profit, market_customer_segment == "EMEA_Home Office")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
EMEA_Home_Office_summary <- summarise(group_by(EMEA_Home_Office, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
EMEA_Home_Office_summary$month_ID <- seq.int(nrow(EMEA_Home_Office_summary))

# Using the raster package to find the coefficient of variation
EMEA_Home_Office_profit_cv <- cv(EMEA_Home_Office_summary$`sum(Profit)`)
# Profit CV for Home Office segment in EMEA: 92.37%

# Checking the mean of profit for the subset #
EMEA_Home_Office_profit_mean <- mean(EMEA_Home_Office_summary$`sum(Profit)`)
# Mean Profit: 598.18

#---------------------------------------------------------------#

# Subsetting Canada_Consumer from only_positive_profit dataframe #
Canada_Consumer <- subset(only_positive_profit, market_customer_segment == "Canada_Consumer")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
Canada_Consumer_summary <- summarise(group_by(Canada_Consumer, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
Canada_Consumer_summary$month_ID <- seq.int(nrow(Canada_Consumer_summary))

# Using the raster package to find the coefficient of variation
Canada_Consumer_profit_cv <- cv(Canada_Consumer_summary$`sum(Profit)`)
# Profit CV for Consumer segment in Canada: 139.53%

# Checking the mean of profit for the subset #
Canada_Consumer_profit_mean <- mean(Canada_Consumer_summary$`sum(Profit)`)
# Mean Profit: 230.42

#---------------------------------------------------------------#

# Subsetting Canada_Corporate from only_positive_profit dataframe #
Canada_Corporate <- subset(only_positive_profit, market_customer_segment == "Canada_Corporate")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
Canada_Corporate_summary <- summarise(group_by(Canada_Corporate, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
Canada_Corporate_summary$month_ID <- seq.int(nrow(Canada_Corporate_summary))

# Using the raster package to find the coefficient of variation
Canada_Corporate_profit_cv <- cv(Canada_Corporate_summary$`sum(Profit)`)
# Profit CV for Corporate segment in Canada: 155.27%

# Checking the mean of profit for the subset #
Canada_Corporate_profit_mean <- mean(Canada_Corporate_summary$`sum(Profit)`)
# Mean Profit: 148.13

#---------------------------------------------------------------#

# Subsetting Canada_Home_Office from only_positive_profit dataframe #
Canada_Home_Office <- subset(only_positive_profit, market_customer_segment == "Canada_Home Office")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
Canada_Home_Office_summary <- summarise(group_by(Canada_Home_Office, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
Canada_Home_Office_summary$month_ID <- seq.int(nrow(Canada_Home_Office_summary))

# Using the raster package to find the coefficient of variation
Canada_Home_Office_profit_cv <- cv(Canada_Home_Office_summary$`sum(Profit)`)
# Profit CV for Home Office segment in Canada: 224.34% 

# Checking the mean of profit for the subset #
Canada_Home_Office_profit_mean <- mean(Canada_Home_Office_summary$`sum(Profit)`)
# Mean Profit: 124.12

#---------------------------------------------------------------#
# Now we will create the 21 subsets by taking into consideration both the positive & negative profit numbers #
#---------------------------------------------------------------# 

# Subsetting APAC_Consumer from globalsales dataframe #
APAC_Consumer_all <- subset(globalsales, market_customer_segment == "APAC_Consumer")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty # 
APAC_Consumer_summary_all <- summarise(group_by(APAC_Consumer_all, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
APAC_Consumer_summary_all$month_ID <- seq.int(nrow(APAC_Consumer_summary_all))

# Using the raster package to find the coefficient of variation
APAC_Consumer_profit_cv_all <- cv(APAC_Consumer_summary_all$`sum(Profit)`)
# Profit CV for Consumer segment in APAC: 63.21%

# Checking the mean of profit for the subset #
APAC_Consumer_profit_mean_all <- mean(APAC_Consumer_summary_all$`sum(Profit)`)
# Mean Profit: 4642.03

#---------------------------------------------------------------#

# Subsetting APAC_Corporate from globalsales dataframe #
APAC_Corporate_all <- subset(globalsales, market_customer_segment == "APAC_Corporate")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
APAC_Corporate_summary_all <- summarise(group_by(APAC_Corporate_all, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
APAC_Corporate_summary_all$month_ID <- seq.int(nrow(APAC_Corporate_summary_all))

# Using the raster package to find the coefficient of variation
APAC_Corporate_profit_cv_all <- cv(APAC_Corporate_summary_all$`sum(Profit)`)
# Profit CV for Corporate segment in APAC: 69.80%

# Checking the mean of profit for the subset #
APAC_Corporate_profit_mean_all <- mean(APAC_Corporate_summary_all$`sum(Profit)`)
# Mean Profit: 2702.85

#---------------------------------------------------------------#

# Subsetting APAC_Home_Office from globalsales dataframe #
APAC_Home_Office_all <- subset(globalsales, market_customer_segment == "APAC_Home Office")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
APAC_Home_Office_summary_all <- summarise(group_by(APAC_Home_Office_all, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
APAC_Home_Office_summary_all$month_ID <- seq.int(nrow(APAC_Home_Office_summary_all))

# Using the raster package to find the coefficient of variation
APAC_Home_Office_profit_cv_all <- cv(APAC_Home_Office_summary_all$`sum(Profit)`)
# Profit CV for Home Office segment in APAC: 104.59%

# Checking the mean of profit for the subset #
APAC_Home_Office_profit_mean_all <- mean(APAC_Home_Office_summary_all$`sum(Profit)`)
# Mean Profit: 1738.44


#---------------------------------------------------------------#

# Subsetting EU_Consumer from globalsales dataframe #
EU_Consumer_all <- subset(globalsales, market_customer_segment == "EU_Consumer")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
EU_Consumer_summary_all <- summarise(group_by(EU_Consumer_all, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
EU_Consumer_summary_all$month_ID <- seq.int(nrow(EU_Consumer_summary_all))

# Using the raster package to find the coefficient of variation
EU_Consumer_profit_cv_all <- cv(EU_Consumer_summary_all$`sum(Profit)`)
# Profit CV for Consumer segment in EU: 62.43%

# Checking the mean of profit for the subset #
EU_Consumer_profit_mean_all <- mean(EU_Consumer_summary_all$`sum(Profit)`)
# Mean Profit: 3930.99

#---------------------------------------------------------------#

# Subsetting EU_Corporate from globalsales dataframe #
EU_Corporate_all <- subset(globalsales, market_customer_segment == "EU_Corporate")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
EU_Corporate_summary_all <- summarise(group_by(EU_Corporate_all, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
EU_Corporate_summary_all$month_ID <- seq.int(nrow(EU_Corporate_summary_all))

# Using the raster package to find the coefficient of variation
EU_Corporate_profit_cv_all <- cv(EU_Corporate_summary_all$`sum(Profit)`)
# Profit CV for Corporate segment in EU: 76.38

# Checking the mean of profit for the subset #
EU_Corporate_profit_mean_all <- mean(EU_Corporate_summary_all$`sum(Profit)`)
# Mean Profit: 2570.70

#---------------------------------------------------------------#

# Subsetting EU_Home_Office from oglobalsales dataframe #
EU_Home_Office_all <- subset(globalsales, market_customer_segment == "EU_Home Office")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
EU_Home_Office_summary_all <- summarise(group_by(EU_Home_Office_all, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
EU_Home_Office_summary_all$month_ID <- seq.int(nrow(EU_Home_Office_summary_all))

# Using the raster package to find the coefficient of variation
EU_Home_Office_profit_cv_all <- cv(EU_Home_Office_summary_all$`sum(Profit)`)
# Profit CV for Home Office segment in EU: 111.65%

# Checking the mean of profit for the subset #
EU_Home_Office_profit_mean_all <- mean(EU_Home_Office_summary_all$`sum(Profit)`)
# Mean Profit: 1265.58

#---------------------------------------------------------------#

# Subsetting US_Consumer from globalsales dataframe #
US_Consumer_all <- subset(globalsales, market_customer_segment == "US_Consumer")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
US_Consumer_summary_all <- summarise(group_by(US_Consumer_all, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
US_Consumer_summary_all$month_ID <- seq.int(nrow(US_Consumer_summary_all))

# Using the raster package to find the coefficient of variation
US_Consumer_profit_cv_all <- cv(US_Consumer_summary_all$`sum(Profit)`)
# Profit CV for Consumer segment in US: 101.23%

# Checking the mean of profit for the subset #
US_Consumer_profit_mean_all <- mean(US_Consumer_summary_all$`sum(Profit)`)
# Mean Profit: 2794.15

#---------------------------------------------------------------#

# Subsetting US_Corporate from globalsales dataframe #
US_Corporate_all <- subset(globalsales, market_customer_segment == "US_Corporate")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
US_Corporate_summary_all <- summarise(group_by(US_Corporate_all, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
US_Corporate_summary_all$month_ID <- seq.int(nrow(US_Corporate_summary_all))

# Using the raster package to find the coefficient of variation
US_Corporate_profit_cv_all <- cv(US_Corporate_summary_all$`sum(Profit)`)
# Profit CV for Corporate segment in US: 100.24%

# Checking the mean of profit for the subset #
US_Corporate_profit_mean_all <- mean(US_Corporate_summary_all$`sum(Profit)`)
# Mean Profit: 1916.23

#---------------------------------------------------------------#

# Subsetting US_Home_Office from globalsales dataframe #
US_Home_Office_all <- subset(globalsales, market_customer_segment == "US_Home Office")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
US_Home_Office_summary_all <- summarise(group_by(US_Home_Office_all, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
US_Home_Office_summary_all$month_ID <- seq.int(nrow(US_Home_Office_summary_all))

# Using the raster package to find the coefficient of variation
US_Home_Office_profit_cv_all <- cv(US_Home_Office_summary_all$`sum(Profit)`)
# Profit CV for Home Office segment in US: 109.61 

# Checking the mean of profit for the subset #
US_Home_Office_profit_mean_all <- mean(US_Home_Office_summary_all$`sum(Profit)`)
# Mean Profit: 1256.22

#---------------------------------------------------------------#

# Subsetting LATAM_Consumer from globalsales dataframe #
LATAM_Consumer_all <- subset(globalsales, market_customer_segment == "LATAM_Consumer")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
LATAM_Consumer_summary_all <- summarise(group_by(LATAM_Consumer_all, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
LATAM_Consumer_summary_all$month_ID <- seq.int(nrow(LATAM_Consumer_summary_all))

# Using the raster package to find the coefficient of variation
LATAM_Consumer_profit_cv_all <- cv(LATAM_Consumer_summary_all$`sum(Profit)`)
# Profit CV for Consumer segment in LATAM: 66.14%

# Checking the mean of profit for the subset #
LATAM_Consumer_profit_mean_all <- mean(LATAM_Consumer_summary_all$`sum(Profit)`)
# Mean Profit: 2513.18

#---------------------------------------------------------------#

# Subsetting LATAM_Corporate from globalsales dataframe #
LATAM_Corporate_all <- subset(globalsales, market_customer_segment == "LATAM_Corporate")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
LATAM_Corporate_summary_all <- summarise(group_by(LATAM_Corporate_all, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
LATAM_Corporate_summary_all$month_ID <- seq.int(nrow(LATAM_Corporate_summary_all))

# Using the raster package to find the coefficient of variation
LATAM_Corporate_profit_cv_all <- cv(LATAM_Corporate_summary_all$`sum(Profit)`)
# Profit CV for Corporate segment in LATAM: 81.11%

# Checking the mean of profit for the subset #
LATAM_Corporate_profit_mean_all <- mean(LATAM_Corporate_summary_all$`sum(Profit)`)
# Mean Profit: 1205.73

#---------------------------------------------------------------#

# Subsetting LATAM_Home_Office from globalsales dataframe #
LATAM_Home_Office_all <- subset(globalsales, market_customer_segment == "LATAM_Home Office")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
LATAM_Home_Office_summary_all <- summarise(group_by(LATAM_Home_Office_all, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
LATAM_Home_Office_summary_all$month_ID <- seq.int(nrow(LATAM_Home_Office_summary_all))

# Using the raster package to find the coefficient of variation
LATAM_Home_Office_profit_cv_all <- cv(LATAM_Home_Office_summary_all$`sum(Profit)`)
# Profit CV for Home Office segment in LATAM: 117.56

# Checking the mean of profit for the subset #
LATAM_Home_Office_profit_mean_all <- mean(LATAM_Home_Office_summary_all$`sum(Profit)`)
# Mean Profit: 898.64

#---------------------------------------------------------------#

# Subsetting Africa_Consumer from globalsales dataframe #
Africa_Consumer_all <- subset(globalsales, market_customer_segment == "Africa_Consumer")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
Africa_Consumer_summary_all <- summarise(group_by(Africa_Consumer_all, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
Africa_Consumer_summary_all$month_ID <- seq.int(nrow(Africa_Consumer_summary_all))

# Using the raster package to find the coefficient of variation
Africa_Consumer_profit_cv_all <- cv(Africa_Consumer_summary_all$`sum(Profit)`)
# Profit CV for Consumer segment in Africa: 131.95%

# Checking the mean of profit for the subset #
Africa_Consumer_profit_mean_all <- mean(Africa_Consumer_summary_all$`sum(Profit)`)
# Mean Profit: 995.25

#---------------------------------------------------------------#

# Subsetting Africa_Corporate from globalsales dataframe #
Africa_Corporate_all <- subset(globalsales, market_customer_segment == "Africa_Corporate")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
Africa_Corporate_summary_all <- summarise(group_by(Africa_Corporate_all, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
Africa_Corporate_summary_all$month_ID <- seq.int(nrow(Africa_Corporate_summary_all))

# Using the raster package to find the coefficient of variation
Africa_Corporate_profit_cv_all <- cv(Africa_Corporate_summary_all$`sum(Profit)`)
# Profit CV for Corporate segment in Africa: 177.61

# Checking the mean of profit for the subset #
Africa_Corporate_profit_mean_all <- mean(Africa_Corporate_summary_all$`sum(Profit)`)
# Mean Profit: 430.97

#---------------------------------------------------------------#

# Subsetting Africa_Home_Office from globalsales dataframe #
Africa_Home_Office_all <- subset(globalsales, market_customer_segment == "Africa_Home Office")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
Africa_Home_Office_summary_all <- summarise(group_by(Africa_Home_Office_all, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
Africa_Home_Office_summary_all$month_ID <- seq.int(nrow(Africa_Home_Office_summary_all))

# Using the raster package to find the coefficient of variation
Africa_Home_Office_profit_cv_all <- cv(Africa_Home_Office_summary_all$`sum(Profit)`)
# Profit CV for Home Office segment in Africa: 179%

# Checking the mean of profit for the subset #
Africa_Home_Office_profit_mean_all <- mean(Africa_Home_Office_summary_all$`sum(Profit)`)
# Mean Profit: 425.26

#---------------------------------------------------------------#

# Subsetting EMEA_Consumer from globalsales dataframe #
EMEA_Consumer_all <- subset(globalsales, market_customer_segment == "EMEA_Consumer")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
EMEA_Consumer_summary_all <- summarise(group_by(EMEA_Consumer_all, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
EMEA_Consumer_summary_all$month_ID <- seq.int(nrow(EMEA_Consumer_summary_all))

# Using the raster package to find the coefficient of variation
EMEA_Consumer_profit_cv_all <- cv(EMEA_Consumer_summary_all$`sum(Profit)`)
# Profit CV for Consumer segment in EMEA: 218.82% 

# Checking the mean of profit for the subset #
EMEA_Consumer_profit_mean_all <- mean(EMEA_Consumer_summary_all$`sum(Profit)`)
# Mean Profit: 531.92

#---------------------------------------------------------------#

# Subsetting EMEA_Corporate from globalsales dataframe #
EMEA_Corporate_all <- subset(globalsales, market_customer_segment == "EMEA_Corporate")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
EMEA_Corporate_summary_all <- summarise(group_by(EMEA_Corporate_all, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
EMEA_Corporate_summary_all$month_ID <- seq.int(nrow(EMEA_Corporate_summary_all))

# Using the raster package to find the coefficient of variation
EMEA_Corporate_profit_cv_all <- cv(EMEA_Corporate_summary_all$`sum(Profit)`)
# Profit CV for Corporate segment in EMEA: 446.71%

# Checking the mean of profit for the subset #
EMEA_Corporate_profit_mean_all <- mean(EMEA_Corporate_summary_all$`sum(Profit)`)
# Mean Profit: 260.39

#---------------------------------------------------------------#

# Subsetting EMEA_Home_Office from globalsales dataframe #
EMEA_Home_Office_all <- subset(globalsales, market_customer_segment == "EMEA_Home Office")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
EMEA_Home_Office_summary_all <- summarise(group_by(EMEA_Home_Office_all, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
EMEA_Home_Office_summary_all$month_ID <- seq.int(nrow(EMEA_Home_Office_summary_all))

# Using the raster package to find the coefficient of variation
EMEA_Home_Office_profit_cv_all <- cv(EMEA_Home_Office_summary_all$`sum(Profit)`)
# Profit CV for Home Office segment in EMEA: 588.07%

# Checking the mean of profit for the subset #
EMEA_Home_Office_profit_mean_all <- mean(EMEA_Home_Office_summary_all$`sum(Profit)`)
# Mean Profit: 122.21

#---------------------------------------------------------------#

# Subsetting Canada_Consumer from globalsales dataframe #
Canada_Consumer_all <- subset(globalsales, market_customer_segment == "Canada_Consumer")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
Canada_Consumer_summary_all <- summarise(group_by(Canada_Consumer_all, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
Canada_Consumer_summary_all$month_ID <- seq.int(nrow(Canada_Consumer_summary_all))

# Using the raster package to find the coefficient of variation
Canada_Consumer_profit_cv_all <- cv(Canada_Consumer_summary_all$`sum(Profit)`)
# Profit CV for Consumer segment in Canada: 139.53%

# Checking the mean of profit for the subset #
Canada_Consumer_profit_mean_all <- mean(Canada_Consumer_summary_all$`sum(Profit)`)
# Mean Profit: 230.42

#---------------------------------------------------------------#

# Subsetting Canada_Corporate from globalsales dataframe #
Canada_Corporate_all <- subset(globalsales, market_customer_segment == "Canada_Corporate")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
Canada_Corporate_summary_all <- summarise(group_by(Canada_Corporate_all, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
Canada_Corporate_summary_all$month_ID <- seq.int(nrow(Canada_Corporate_summary_all))

# Using the raster package to find the coefficient of variation
Canada_Corporate_profit_cv_all <- cv(Canada_Corporate_summary_all$`sum(Profit)`)
# Profit CV for Corporate segment in Canada: 155.27%

# Checking the mean of profit for the subset #
Canada_Corporate_profit_mean_all <- mean(Canada_Corporate_summary_all$`sum(Profit)`)
# Mean Profit: 148.13

#---------------------------------------------------------------#

# Subsetting Canada_Home_Office from globalsales dataframe #
Canada_Home_Office_all <- subset(globalsales, market_customer_segment == "Canada_Home Office")

# Using quater_month to group and summarise all the three entities, namely sales, profit and qty #
Canada_Home_Office_summary_all <- summarise(group_by(Canada_Home_Office_all, quater_month), sum(Sales),sum(Profit),sum(Quantity))

# Creating a sequence column that will hold the month id, which will in turn help in the #
# creation of the time series #
Canada_Home_Office_summary_all$month_ID <- seq.int(nrow(Canada_Home_Office_summary_all))

# Using the raster package to find the coefficient of variation
Canada_Home_Office_profit_cv_all <- cv(Canada_Home_Office_summary_all$`sum(Profit)`)
# Profit CV for Home Office segment in Canada: 224.34%

# Checking the mean of profit for the subset #
Canada_Home_Office_profit_mean_all <- mean(Canada_Home_Office_summary_all$`sum(Profit)`)
# Mean Profit: 124.12

#---------------------------------------------------------------#

# We used two approaches to check which two market segment combination are the most #
# profitable and consistently profitable, the two approaches were 1. where we took all #
# the transactions that included both positive and negative profits, 2. where we took #
# transactions that included only positive profit numbers #

# In both the cases the coefficient of variation came the least for APAC consumer #
# followed by EU consumer and the average was maximum for APAC consumer followed by #
# EU consumer, hence we will be taking these two subsets for model building stage #

# MARKET_SEGMENT combination selected are:- #
# APAC_CONSUMER
# EU_CONSUMER

#---------------------------------------------------------------#
# USING Classical Decomposition FOR FORECASTING FOR APAC CONSUMER & EU CONSUMER
#---------------------------------------------------------------#

# As filter function of tseries was getting affected due to dplyr filter function #
# hence detaching the dplyr package here #
detach("package:dplyr", unload=TRUE)

# APAC_CONSUMER
# Taking the full dataset and creating a time series out of it # 
Total_APAC_Consumer_sales <- APAC_Consumer_summary_all[,c(5,2)]
Total_APAC_Consumer_sales_timeser <-  ts(Total_APAC_Consumer_sales$`sum(Sales)`)

# Creating a dataframe with only month id and sales numbers for APAc Consumer with first 42 rows #
APAC_Consumer_sales <- APAC_Consumer_summary_all[1:42,c(5,2)]

# creating a time series to check the patterns in APAC Consumer sales number #  
APAC_Consumer_sales_timeser <- ts(APAC_Consumer_sales$`sum(Sales)`)

# Plotting the sales time series
plot(APAC_Consumer_sales_timeser)
# Looking at the time series we see that there is no seasonality, but there is an # 
# upward trend, we will now check for global trend and locally predictable part #
# but first we will smoothen the time series #

# Smoothing the APAC_Consumer_sales_timeser using Moving Average Smoothing #
w <- 1
smoothed_APAC_Con_Sales <- filter(APAC_Consumer_sales_timeser, filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)

# Smoothing left end of the time series #
diff_APAC_con_sales_left <- smoothed_APAC_Con_Sales[w+2] - smoothed_APAC_Con_Sales[w+1]

for (i in seq(w,1,-1)) {
  smoothed_APAC_Con_Sales[i] <- smoothed_APAC_Con_Sales[i+1] - diff_APAC_con_sales_left
}

#  Smoothing right end of the time series #
n <- length(APAC_Consumer_sales_timeser)

diff_APAC_con_sales_right <- smoothed_APAC_Con_Sales[n-w] - smoothed_APAC_Con_Sales[n-w-1]

for (i in seq(n-w+1, n)) {
  smoothed_APAC_Con_Sales[i] <- smoothed_APAC_Con_Sales[i-1] + diff_APAC_con_sales_right
}

# Checking the smoothness curve over the original line graph # 
lines(smoothed_APAC_Con_Sales, col="blue", lwd=2)

# Now we will convert the time series to a dataframe #
timevals_APAC_con_sales <- APAC_Consumer_sales$month_ID

APAC_con_sales_df <- as.data.frame(cbind(timevals_APAC_con_sales, as.vector(smoothed_APAC_Con_Sales)))
colnames(APAC_con_sales_df) <- c('Month', 'Sales')

# There seems to be no seasonality in the data #
# Checking the global trend in the APAC Consumer sales time series
APAC_Consumer_sales_lmfit <- lm(Sales ~ Month, data = APAC_con_sales_df)
APAC_Consumer_sales_globalpred <- predict(APAC_Consumer_sales_lmfit, Month = timevals_APAC_con_sales)

# Plotting the global trend of sales time series #
plot(APAC_Consumer_sales_globalpred, col='blue', type = "l")

# Now, let's inspect the locally predictable part of the time series #
APAC_Consumer_sales_localpred <- APAC_Consumer_sales_timeser - APAC_Consumer_sales_globalpred

# Plotting the local predictable part of sales time series #
plot(APAC_Consumer_sales_localpred, col='blue', type = "l")

# Compute and plot the ACF for the time series #
acf(APAC_Consumer_sales_localpred, level=95, lag.max=42, main="ACF Plot for White Noise")

# Compute and plot the PACF for the time series #
pacf(APAC_Consumer_sales_localpred, level=95, lag.max=42, main="PACF Plot for White Noise")

# Modelling the locally predictable part as an ARMA model #
armafit_APAC_con_sales <- auto.arima(APAC_Consumer_sales_localpred)

# checking if the residual series is white noise #
resi_APAC_con_sales <- APAC_Consumer_sales_localpred - fitted(armafit_APAC_con_sales)

# Finding the type of stationarity that the local perdictable part of the time series has #
# Deploying ADF test
adf.test(resi_APAC_con_sales, alternative="stationary")
# Lag order = 3, p-value = 0.01562
# we have to reject the null hypothesis here and go with alternative hypothesis # 
# as p-value is less than 0.05 #

# Deploying KPSS test
kpss.test(resi_APAC_con_sales)
# Truncation lag parameter = 1, p-value = 0.1
# we have to accept the null hypothesis here, as p-value is more than 0.05 # 

# Now we will make a prediction for the last 6 months #
test_APAC_Consumer_sales <- APAC_Consumer_summary_all[43:48,c(5,2)]

# creating a time values vector to be used in preciction # 
test_timevals_APAC_con_sales <- test_APAC_Consumer_sales$month_ID

# creating the global pred for the test data #
test_APAC_con_sales_global_pred <- predict(APAC_Consumer_sales_lmfit, data.frame(Month = test_timevals_APAC_con_sales))
fcast_APAC_con_sales <- test_APAC_con_sales_global_pred

# Now, we will compare our prediction with the actual values, using MAPE #
MAPE_APAC_con_sales <- accuracy(fcast_APAC_con_sales,test_APAC_Consumer_sales$`sum(Sales)`)[5]
# MAPE 25.51458

# plotting the predictions along with original values, to understand the fit #
class_pred_APAC_con_sales <- c(ts(APAC_Consumer_sales_globalpred),ts(test_APAC_con_sales_global_pred))
plot(Total_APAC_Consumer_sales_timeser, col = "black")
lines(class_pred_APAC_con_sales, col = "red")

#---------------------------------------------------------------#

# Taking the full dataset and creating a time series out of it # 
Total_APAC_Consumer_qty <- APAC_Consumer_summary_all[,c(5,4)]
Total_APAC_Consumer_qty_timeser <-  ts(Total_APAC_Consumer_qty$`sum(Quantity)`)

# Creating a dataframe with only month id and qty numbers for APAc Consumer with first 42 rows #
APAC_Consumer_qty <- APAC_Consumer_summary_all[1:42,c(5,4)]

# creating a time series to check the patterns in APAC Consumer quantity #
APAC_Consumer_quantity_timeser <- ts(APAC_Consumer_qty$`sum(Quantity)`)

# Plotting the quantity time series
plot(APAC_Consumer_quantity_timeser)
# Looking at the time series we see that there is no seasonality, but there is an # 
# upward trend, we will now check for global trend and locally predictable part #
# but first we will smoothen the time series #

# Smoothing the APAC_Consumer_qty_timeser using Moving Average Smoothing #
smoothed_APAC_Con_qty <- filter(APAC_Consumer_quantity_timeser, filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)

#Smoothing left end of the time series
diff_APAC_con_qty_left <- smoothed_APAC_Con_qty[w+2] - smoothed_APAC_Con_qty[w+1]

for (i in seq(w,1,-1)) {
  smoothed_APAC_Con_qty[i] <- smoothed_APAC_Con_qty[i+1] - diff_APAC_con_qty_left
}

#Smoothing right end of the time series
n <- length(APAC_Consumer_quantity_timeser)

diff_APAC_con_qty_right <- smoothed_APAC_Con_qty[n-w] - smoothed_APAC_Con_qty[n-w-1]

for (i in seq(n-w+1, n)) {
  smoothed_APAC_Con_qty[i] <- smoothed_APAC_Con_qty[i-1] + diff_APAC_con_qty_right
}

# Checking the smoothness curve over the original line graph # 
lines(smoothed_APAC_Con_qty, col="blue", lwd=2)

# Now we will convert the time series to a dataframe
timevals_APAC_con_qty <- APAC_Consumer_qty$month_ID

APAC_con_qty_df <- as.data.frame(cbind(timevals_APAC_con_qty, as.vector(smoothed_APAC_Con_qty)))
colnames(APAC_con_qty_df) <- c('Month', 'Quantity')

# There seems to be no seasonality in the data #
# Checking the global trend in the APAC Consumer qty time series
APAC_Consumer_qty_lmfit <- lm(Quantity ~ Month, data = APAC_con_qty_df)
APAC_Consumer_qty_globalpred <- predict(APAC_Consumer_qty_lmfit, Month = timevals_APAC_con_qty)

# Plotting the global trend of qty time series #
plot(APAC_Consumer_qty_globalpred, col='blue', type = "l")

# Now, let's inspect the locally predictable part of the time series #
APAC_Consumer_qty_localpred <- APAC_Consumer_quantity_timeser - APAC_Consumer_qty_globalpred

# Plotting the local predictable part of quantity time series #
plot(APAC_Consumer_qty_localpred, col='blue', type = "l")

# Compute and plot the ACF for the time series #
acf(APAC_Consumer_qty_localpred, level=95, lag.max=42, main="ACF Plot for White Noise")

# Compute and plot the PACF for the time series #
pacf(APAC_Consumer_qty_localpred, level=95, lag.max=42, main="PACF Plot for White Noise")

# Modelling the locally predictable part as an ARMA model #
armafit_APAC_con_qty <- auto.arima(APAC_Consumer_qty_localpred)

# checking if the residual series is white noise #
resi_APAC_con_qty <- APAC_Consumer_qty_localpred - fitted(armafit_APAC_con_qty)

# Finding the type of stationarity that the local perdictable part of the time series has #
# Deploying ADF test
adf.test(resi_APAC_con_qty, alternative="stationary")
# Lag order = 3, p-value = 0.01
# we have to reject the null hypothesis here and go with alternative hypothesis # 
# as p-value is less than 0.05 #

# Deploying KPSS test
kpss.test(resi_APAC_con_qty)
# Truncation lag parameter = 1, p-value = 0.1 
# we have to accept the null hypothesis here, as p-value is more than 0.05 # 

# Now we will make a prediction for the last 6 months #
test_APAC_Consumer_qty <- APAC_Consumer_summary_all[43:48,c(5,4)]

# creating a time values vector to be used in preciction # 
test_timevals_APAC_con_qty <- test_APAC_Consumer_qty$month_ID

# creating the global pred for the test data #
test_APAC_con_qty_global_pred <- predict(APAC_Consumer_qty_lmfit, data.frame(Month = test_timevals_APAC_con_qty))
fcast_APAC_con_qty <- test_APAC_con_qty_global_pred

# Now, we will compare our prediction with the actual values, using MAPE #
MAPE_APAC_con_qty <- accuracy(fcast_APAC_con_qty,test_APAC_Consumer_qty$`sum(Quantity)`)[5]
# MAPE 29.52093

# plotting the predictions along with original values, to understand the fit #
class_pred_APAC_con_qty <- c(ts(APAC_Consumer_qty_globalpred),ts(test_APAC_con_qty_global_pred))
plot(Total_APAC_Consumer_qty_timeser, col = "black")
lines(class_pred_APAC_con_qty, col = "red")

#---------------------------------------------------------------#

# EU_CONSUMER

# Taking the full dataset and creating a time series out of it # 
Total_EU_Consumer_sales <- EU_Consumer_summary_all[,c(5,2)]
Total_EU_Consumer_sales_timeser <-  ts(Total_EU_Consumer_sales$`sum(Sales)`)

# Creating a dataframe with only month id and qty numbers for EU Consumer with first 42 rows #
EU_Consumer_sales <- EU_Consumer_summary_all[1:42,c(5,2)]

# creating a time series to check the patterns in EU Consumer sales number #  
EU_Consumer_sales_timeser <- ts(EU_Consumer_sales$`sum(Sales)`)

# Plotting the sales time series
plot(EU_Consumer_sales_timeser)
# Looking at the time series we see that there is no seasonality, but there is an # 
# upward trend, we will now check for global trend and locally predictable part #
# but first we will smoothen the time series #

# Smoothing the EU_Consumer_sales_timeser using Moving Average Smoothing #
smoothed_EU_Con_Sales <- filter(EU_Consumer_sales_timeser, filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)

#Smoothing left end of the time series
diff_EU_con_sales_left <- smoothed_EU_Con_Sales[w+2] - smoothed_EU_Con_Sales[w+1]
for (i in seq(w,1,-1)) {
  smoothed_EU_Con_Sales[i] <- smoothed_EU_Con_Sales[i+1] - diff_EU_con_sales_left
}

#Smoothing right end of the time series
n <- length(EU_Consumer_sales_timeser)
diff_EU_con_sales_right <- smoothed_EU_Con_Sales[n-w] - smoothed_EU_Con_Sales[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothed_EU_Con_Sales[i] <- smoothed_EU_Con_Sales[i-1] + diff_EU_con_sales_right
}

# Checking the smoothness curve over the original line graph # 
lines(smoothed_EU_Con_Sales, col="blue", lwd=2)

# Now we will convert the time series to a dataframe
timevals_EU_con_sales <- EU_Consumer_sales$month_ID

EU_con_sales_df <- as.data.frame(cbind(timevals_EU_con_sales, as.vector(smoothed_EU_Con_Sales)))
colnames(EU_con_sales_df) <- c('Month', 'Sales')

# There seems to be no seasonality in the data #
# Checking the global trend in the EU Consumer sales time series
EU_Consumer_sales_lmfit <- lm(Sales ~ Month, data = EU_con_sales_df)
EU_Consumer_sales_globalpred <- predict(EU_Consumer_sales_lmfit, Month = timevals_EU_con_sales)

# Plotting the global trend of sales time series #
plot(EU_Consumer_sales_globalpred, col='blue', type = "l")

# Now, let's inspect the locally predictable part of the time series #
EU_Consumer_sales_localpred <- EU_Consumer_sales_timeser - EU_Consumer_sales_globalpred

# Plotting the local predictable part of sales time series #
plot(EU_Consumer_sales_localpred, col='blue', type = "l")

# Compute and plot the ACF for the time series #
acf(EU_Consumer_sales_localpred, level=95, lag.max=42, main="ACF Plot for White Noise")

# Compute and plot the PACF for the time series #
pacf(EU_Consumer_sales_localpred, level=95, lag.max=42, main="PACF Plot for White Noise")

# Modelling the locally predictable part as an ARMA model #
armafit_EU_con_sales <- auto.arima(EU_Consumer_sales_localpred)

# checking if the residual series is white noise #
resi_EU_con_sales <- EU_Consumer_sales_localpred - fitted(armafit_EU_con_sales)

# Finding the type of stationarity that the local perdictable part of the time series has #
# Deploying ADF test
adf.test(resi_EU_con_sales, alternative="stationary")
# Lag order = 3, p-value = 0.03254 
# we have to reject the null hypothesis here and go with alternative hypothesis # 
# as p-value is less than 0.05 #

# Deploying KPSS test
kpss.test(resi_EU_con_sales)
# Truncation lag parameter = 1, p-value = 0.1
# we have to accept the null hypothesis here, as p-value is more than 0.05 # 

# Now we will make a prediction for the last 6 months #
test_EU_Consumer_sales <- EU_Consumer_summary_all[43:48,c(5,2)]

# creating a time values vector to be used in preciction # 
test_timevals_EU_con_sales <- test_EU_Consumer_sales$month_ID

# creating the global pred for the test data #
test_EU_con_sales_global_pred <- predict(EU_Consumer_sales_lmfit, data.frame(Month = test_timevals_EU_con_sales))
fcast_EU_con_sales <- test_EU_con_sales_global_pred

# Now, we will compare our prediction with the actual values, using MAPE #
MAPE_EU_con_sales <- accuracy(fcast_EU_con_sales,test_EU_Consumer_sales$`sum(Sales)`)[5]
# MAPE 28.10294

# plotting the predictions along with original values, to understand the fit #
class_pred_EU_con_sales <- c(ts(EU_Consumer_sales_globalpred),ts(test_EU_con_sales_global_pred))
plot(Total_EU_Consumer_sales_timeser, col = "black")
lines(class_pred_EU_con_sales, col = "red")

#---------------------------------------------------------------#

# Taking the full dataset and creating a time series out of it # 
Total_EU_Consumer_qty <- EU_Consumer_summary_all[,c(5,4)]
Total_EU_Consumer_qty_timeser <-  ts(Total_EU_Consumer_qty$`sum(Quantity)`)

# Creating a dataframe with only month id and qty numbers for EU Consumer with first 42 rows #
EU_Consumer_qty <- EU_Consumer_summary_all[1:42,c(5,4)]

# creating a time series to check the patterns in EU Consumer quantity #
EU_Consumer_quantity_timeser <- ts(EU_Consumer_qty$`sum(Quantity)`)

# Plotting the quantity time series
plot(EU_Consumer_quantity_timeser)
# Looking at the time series we see that there is no seasonality, but there is an # 
# upward trend, we will now check for global trend and locally predictable part #
# but first we will smoothen the time series #

# Smoothing the EU_Consumer_qty_timeser using Moving Average Smoothing #
smoothed_EU_Con_qty <- filter(EU_Consumer_quantity_timeser, filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)

#Smoothing left end of the time series
diff_EU_con_qty_left <- smoothed_EU_Con_qty[w+2] - smoothed_EU_Con_qty[w+1]
for (i in seq(w,1,-1)) {
  smoothed_EU_Con_qty[i] <- smoothed_EU_Con_qty[i+1] - diff_EU_con_qty_left
}

#Smoothing right end of the time series
n <- length(EU_Consumer_quantity_timeser)
diff_EU_con_qty_right <- smoothed_EU_Con_qty[n-w] - smoothed_EU_Con_qty[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothed_EU_Con_qty[i] <- smoothed_EU_Con_qty[i-1] + diff_EU_con_qty_right
}

# Checking the smoothness curve over the original line graph # 
lines(smoothed_EU_Con_qty, col="blue", lwd=2)

# Now we will convert the time series to a dataframe
timevals_EU_con_qty <- EU_Consumer_qty$month_ID

EU_con_qty_df <- as.data.frame(cbind(timevals_EU_con_qty, as.vector(smoothed_EU_Con_qty)))
colnames(EU_con_qty_df) <- c('Month', 'Quantity')

# There seems to be no seasonality in the data #
# Checking the global trend in the EU Consumer qty time series
EU_Consumer_qty_lmfit <- lm(Quantity ~ Month, data = EU_con_qty_df)
EU_Consumer_qty_globalpred <- predict(EU_Consumer_qty_lmfit, Month = timevals_EU_con_qty)

# Plotting the global trend of qty time series #
plot(EU_Consumer_qty_globalpred, col='blue', type = "l")

# Now, let's inspect the locally predictable part of the time series #
EU_Consumer_qty_localpred <- EU_Consumer_quantity_timeser - EU_Consumer_qty_globalpred

# Plotting the local predictable part of qty time series #
plot(EU_Consumer_qty_localpred, col='blue', type = "l")

# Compute and plot the ACF for the time series #
acf(EU_Consumer_qty_localpred, level=95, lag.max=42, main="ACF Plot for White Noise")

# Compute and plot the PACF for the time series #
pacf(EU_Consumer_qty_localpred, level=95, lag.max=42, main="PACF Plot for White Noise")

# Modelling the locally predictable part as an ARMA model #
armafit_EU_con_qty <- auto.arima(EU_Consumer_qty_localpred)

# checking if the residual series is white noise #
resi_EU_con_qty <- EU_Consumer_qty_localpred - fitted(armafit_EU_con_qty)

# Finding the type of stationarity that the local perdictable part of the time series has #
# Deploying ADF test
adf.test(resi_EU_con_qty, alternative="stationary")
# Lag order = 3, p-value = 0.01015 
# we have to reject the null hypothesis here and go with alternative hypothesis # 
# as p-value is less than 0.05 #

# Deploying KPSS test
kpss.test(resi_EU_con_qty)
# Truncation lag parameter = 1, p-value = 0.1 
# we have to accept the null hypothesis here, as p-value is more than 0.05 # 

# Now we will make a prediction for the last 6 months #
test_EU_Consumer_qty <- EU_Consumer_summary_all[43:48,c(5,4)]

# creating a time values vector to be used in preciction # 
test_timevals_EU_con_qty <- test_EU_Consumer_qty$month_ID

# creating the global pred for the test data #
test_EU_con_qty_global_pred <- predict(EU_Consumer_qty_lmfit, data.frame(Month = test_timevals_EU_con_qty))
fcast_EU_con_qty <- test_EU_con_qty_global_pred

# Now, we will compare our prediction with the actual values, using MAPE #
MAPE_EU_con_qty <- accuracy(fcast_EU_con_qty,test_EU_Consumer_qty$`sum(Quantity)`)[5]
# MAPE 30.1843

# plotting the predictions along with original values, to understand the fit #
class_pred_EU_con_qty <- c(ts(EU_Consumer_quantity_timeser),ts(test_EU_con_qty_global_pred))
plot(Total_EU_Consumer_qty_timeser, col = "black")
lines(class_pred_EU_con_qty, col = "red")


#---------------------------------------------------------------#
# USING AUTO ARIMA FOR FORECASTING FOR APAC CONSUMER & EU CONSUMER
#---------------------------------------------------------------#

# First we will split the four subsets into train and test datasets #
# The first 42 months will be used for training and the last 6 will be kept for test #

train_APAC_consumer_sales_arima <- APAC_Consumer_summary_all[1:42 , c(5,2)]
train_APAC_consumer_qty_arima <- APAC_Consumer_summary_all[1:42 , c(5,4)]
train_EU_consumer_sales_arima <- EU_Consumer_summary_all[1:42, c(5,2)]
train_EU_consumer_qty_arima <- EU_Consumer_summary_all[1:42, c(5,4)]

test_APAC_consumer_sales_arima <- APAC_Consumer_summary_all[43:48 , c(5,2)]
test_APAC_consumer_qty_arima <- APAC_Consumer_summary_all[43:48 , c(5,4)]
test_EU_consumer_sales_arima <- EU_Consumer_summary_all[43:48 , c(5,2)]
test_EU_consumer_qty_arima <- EU_Consumer_summary_all[43:48 , c(5,4)]

#---------------------------------------------------------------#

# Creating all the four time series
APAC_Consumer_sales_arima_timeser <- auto.arima(train_APAC_consumer_sales_arima$`sum(Sales)`)
APAC_Consumer_qty_arima_timeser <- auto.arima(train_APAC_consumer_qty_arima$`sum(Quantity)`)
EU_Consumer_sales_arima_timeser <-  auto.arima(train_EU_consumer_sales_arima$`sum(Sales)`)
EU_Consumer_qty_arima_timeser <- auto.arima(train_EU_consumer_qty_arima$`sum(Quantity)`)

# Checking the ARIMA (p,d,q) for all the four time series #

APAC_Consumer_sales_arima_timeser
# ARIMA(0,1,1)
# AIC=898.23   AICc=898.55   BIC=901.66
# log likelihood=-447.11

APAC_Consumer_qty_arima_timeser
# ARIMA(0,1,0)
# AIC=534.14   AICc=534.24   BIC=535.85
# log likelihood=-266.07

EU_Consumer_sales_arima_timeser
# ARIMA(2,1,0)
# AIC=897.67   AICc=898.32   BIC=902.81
# log likelihood=-445.84

EU_Consumer_qty_arima_timeser
# ARIMA(2,1,0)
# AIC=529.8   AICc=530.44   BIC=534.94
# log likelihood=-261.9

#---------------------------------------------------------------#

# Now we will look at the actual verses the fitted time series #
# For APAC_Consumer_sales_arima_timeser
plot(APAC_Consumer_sales_arima_timeser$x, col="green")
lines(fitted(APAC_Consumer_sales_arima_timeser), col="red")

# For APAC_Consumer_qty_arima_timeser
plot(APAC_Consumer_qty_arima_timeser$x, col="green")
lines(fitted(APAC_Consumer_qty_arima_timeser), col="red")

# For EU_Consumer_sales_arima_timeser
plot(EU_Consumer_sales_arima_timeser$x, col="green")
lines(fitted(EU_Consumer_sales_arima_timeser), col="red")

# For EU_Consumer_qty_arima_timeser
plot(EU_Consumer_qty_arima_timeser$x, col="green")
lines(fitted(EU_Consumer_qty_arima_timeser), col="red")

#---------------------------------------------------------------#

# Now we will forecast the values for the last 6 months for all four #
fcast_APAC_Consumer_sales_arima_timeser <- predict(APAC_Consumer_sales_arima_timeser, n.ahead = 7)
fcast_APAC_Consumer_qty_arima_timeser <- predict(APAC_Consumer_qty_arima_timeser, n.ahead = 7)
fcast_EU_Consumer_sales_arima_timeser <- predict(EU_Consumer_sales_arima_timeser, n.ahead = 7)
fcast_EU_Consumer_qty_arima_timeser <- predict(EU_Consumer_qty_arima_timeser, n.ahead = 7)

#---------------------------------------------------------------#

# Now lets evaluate the 4 models using MAPE #
MAPE_APAC_Consumer_sales_arima_timeser <- accuracy(fcast_APAC_Consumer_sales_arima_timeser$pred, test_APAC_consumer_sales_arima$`sum(Sales)`)[5]
# MAPE: 27.68
MAPE_APAC_Consumer_qty_arima_timeser <- accuracy(fcast_APAC_Consumer_qty_arima_timeser$pred, test_APAC_consumer_qty_arima$`sum(Quantity)`)[5]
# MAPE: 26.24
MAPE_EU_Consumer_sales_arima_timeser <- accuracy(fcast_EU_Consumer_sales_arima_timeser$pred, test_EU_consumer_sales_arima$`sum(Sales)`)[5]
# MAPE: 28.92
MAPE_EU_Consumer_qty_arima_timeser <- accuracy(fcast_EU_Consumer_qty_arima_timeser$pred, test_EU_consumer_qty_arima$`sum(Quantity)`)[5]
# MAPE: 30.14

#-------------------------END OF CODE-------------------------#


















