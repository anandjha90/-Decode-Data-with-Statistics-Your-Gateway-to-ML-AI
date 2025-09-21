## Dataset Description
# This is the historical data which covers sales from 2010-02-05 to 2012-11-01, in the file Walmart_Store_sales. 
# Within this file you will find the following fields:

# Store - the store number
# Date - the week of sales
# Weekly_Sales -  sales for the given store
# Holiday_Flag - whether the week is a special holiday week 1 – Holiday week 0 – Non-holiday week
# Temperature - Temperature on the day of sale
# Fuel_Price - Cost of fuel in the region
# CPI – Prevailing consumer price index
# Unemployment - Prevailing unemployment rate

getwd()
walmart_data <- read.csv("Walmart_Store_sales.csv",header = T)
walmart_data
View(walmart_data)
str(walmart_data)

library(dplyr)
library(lubridate)
str(walmart_data)
class(walmart_data$Date)
walmart_data$Date <- as.Date(walmart_data$Date,format = "%d-%m-%Y")
# standard date format output %Y-%m-%d

## Analysis Tasks
# Basic Statistics tasks

# Which store has maximum sales
store_max_sales <- walmart_data %>% group_by(Store) %>% summarise(tot_sales = sum(Weekly_Sales)) %>% 
  filter(tot_sales == max(tot_sales))
store_max_sales # 20 301397792

# Which store has maximum standard deviation i.e., the sales vary a lot. 
# Also, find out the coefficient of mean to standard deviation
max_std_dev <- walmart_data %>% group_by(Store) %>% summarise(stan_dev = sd(Weekly_Sales)) %>% 
  filter(stan_dev == max(stan_dev))
max_std_dev # 14 317570

coeff_vari <- walmart_data %>%  group_by(Store) %>% summarise(coeff_varia = sd(Weekly_Sales)/mean(Weekly_Sales)) %>% 
  filter(coeff_varia == max(coeff_varia))
coeff_vari # 35 0.230

# Which store/s has good quarterly growth rate in Q3’2012
# Hint: Growth Rate = Weekly_Sales.Q3_2012-
# Weekly_Sales.Q2_2012)/Weekly_Sales.Q2_2012
# try mutate and if else to create a new column
# 1st April - 30th June - Q2
# 1st July - 30th Sep - Q3
# otherwise
# Use quarter(ymd(date), with_year = true ) this will give year and quarter value like 2010.1 , 2010.2 etc .
# You can use mutate and add this as a column to the df.

# fetching 2012 Q2 data
weekly_Sales_Q2_2012 <- walmart_data %>% group_by(Store) %>% 
  filter(Date >= as.Date("2012-04-01") & Date <= as.Date("2012-06-30")) %>% 
  summarise(sum(Weekly_Sales))
head(weekly_Sales_Q2_2012)

#    Store `sum(Weekly_Sales)`
#     <int>          <dbl>
#1     1           20978760.
#2     2           25083605.
#3     3            5620316.
#4     4           28454364.
#5     5            4466364.
#6     6           20833910.

# fetching 2012 Q3 data
weekly_Sales_Q3_2012 <- walmart_data %>% group_by(Store) %>% 
  filter(Date >= as.Date("2012-07-01") & Date <= as.Date("2012-09-30")) %>% 
  summarise(sum(Weekly_Sales))
head(weekly_Sales_Q3_2012)

#     Store ` sum(Weekly_Sales)`
#    <int>           <dbl>
#1     1           20253948.
#2     2           24303355.
#3     3            5298005.
#4     4           27796792.
#5     5            4163791.
#6     6           20167312.


# Growth Rate = (Weekly_Sales.Q3_2012 - Weekly_Sales.Q2_2012) / Weekly_Sales.Q2_2012
growth_rate_q3_2012 <- mutate(weekly_Sales_Q3_2012,Performance = 
                                ((weekly_Sales_Q3_2012$`sum(Weekly_Sales)` - weekly_Sales_Q2_2012$`sum(Weekly_Sales)`) / 
                                   weekly_Sales_Q2_2012$`sum(Weekly_Sales)`) * 100)

head(arrange(growth_rate_q3_2012,desc(Performance)))

#     Store `   sum(Weekly_Sales)`  Performance
#    <int>           <dbl>          <dbl>
#1     7            8262787.        13.3 
#2    16            7121542.        8.49
#3    35           11322421.        4.47
#4    26           13675692.        3.96
#5    39           20715116.        2.48
#6    41           18093844.        2.46


## Holiday Events
# Super Bowl: 12-Feb-10, 11-Feb-11, 10-Feb-12, 8-Feb-13
# Labour Day: 10-Sep-10, 9-Sep-11, 7-Sep-12, 6-Sep-13
# Thanksgiving: 26-Nov-10, 25-Nov-11, 23-Nov-12, 29-Nov-13
# Christmas: 31-Dec-10, 30-Dec-11, 28-Dec-12, 27-Dec-13

# Some holidays have a negative impact on sales. Find out holidays which have higher sales than the mean sales 
# in non-holiday season for all stores together
# Hint : You need to find dates where weekly_sales > avg_non_holiday_sales & holiday_flag == 1

mean_non_holiday_sales<- walmart_data %>% filter(Holiday_Flag == '0') %>% 
  summarise(total_non_holiday_sales = mean(Weekly_Sales)) 
mean_non_holiday_sales #  1041256

holiday_sales <- walmart_data %>% group_by(Date)%>% filter(Holiday_Flag == '1') %>% 
  summarise(total_holiday_sales = sum(Weekly_Sales)) %>% 
  mutate(holiday_higher_sales_than_mean_non_holidays = total_holiday_sales > mean_non_holiday_sales)
head(holiday_sales)

# Date            total_holiday_sales holiday_higher_sales_than_mean_non_holidays
#   <date>                <dbl>       <lgl>                                      
#1 2010-02-12           48336678.     TRUE                                       
#2 2010-09-10           45634398.     TRUE                                       
#3 2010-11-26           65821003.     TRUE                                       
#4 2010-12-31           40432519      TRUE                                       
#5 2011-02-11           47336193.     TRUE                                       
#6 2011-09-09           46763228.     TRUE    

holiday_sales$Holiday <- ifelse(month(ymd(holiday_sales$Date)) == 2,"Super Bowl" ,
                                ifelse(month(ymd(holiday_sales$Date)) == 9,"Labour Day" ,
                                       ifelse(month(ymd(holiday_sales$Date)) == 11,"Thanksgiving" ,"Christmas")))
holiday_sales

#Date       total_holiday_sales holiday_higher_sales_than_mean_non_holidays Holiday     
#   <date>                <dbl>   <lgl>                     <chr>       
#1 2010-02-12           48336678. TRUE                      Super Bowl  
#2 2010-09-10           45634398. TRUE                      Labour Day  
#3 2010-11-26           65821003. TRUE                      Thanksgiving
#4 2010-12-31           40432519  TRUE                      Christmas   
#5 2011-02-11           47336193. TRUE                      Super Bowl  
#6 2011-09-09           46763228. TRUE                      Labour Day  
#7 2011-11-25           66593605. TRUE                      Thanksgiving
#8 2011-12-30           46042461. TRUE                      Christmas   
#9 2012-02-10           50009408. TRUE                      Super Bowl  
#10 2012-09-07           48330059. TRUE                     Labour Day  

# Provide a monthly and semester view of sales in units and give insights
# spliting of date
# Monthwise and Yearwise weekly sales. 
# For eg your output would look like this
#: -- DUMMY OUTPUT
# Hint :  Month Year Weekly_Sales
#           12  2015   60533

# monthly view of sales
month_year_view <- walmart_data %>% mutate(Month = month(Date) , Year = year(Date)) %>% 
  group_by(Month,Year) %>% summarise(Weekly_Sales = sum(Weekly_Sales)) %>% 
  arrange(Year)
head(month_year_view)

#    Month    Year     Weekly_Sales
#   <dbl> <dbl>     <dbl>
#1     2  2010   190332983.
#2     3  2010   181919802.
#3     4  2010   231412368.
#4     5  2010   186710934.
#5     6  2010   192246172.
#6     7  2010   232580126.


# semester view of sales
sem_view <- walmart_data %>% mutate(Semester = semester(Date,2010)) %>% group_by(Semester) %>% 
  summarise(Weekly_Sales_Fig = sum(Weekly_Sales))
head(sem_view)

#    Semester   Weekly_Sales_Fig
#     <dbl>         <dbl>
#1    2010.       982622260.
#2    2010.      1306263860.
#3    2011.      1127339797.
#4    2011.      1320860210.
#5    2012.      1210765416.
#6    2012.       789367443.

# Additional Work done by Me just to see how the variables are correlated 
# Coorelation
# The corrplot package is a graphical display of a correlation matrix, 
# confidence interval. It also contains some algorithms to do matrix reordering. 
# In addition, corrplot is good at details, including choosing color, text labels, 
# color labels, layout, etc. The correlation matrix can be reordered according to the correlation coefficient.
# This is important to identify the hidden structure and pattern in the matrix.

subset2 <- subset(walmart_data, select = c('Weekly_Sales','Temperature','Fuel_Price','Unemployment','CPI'))
res <- cor(subset2)
head(res)

library(corrplot)
corrplot(res, type = 'upper', order = 'hclust', tl.col = 'black', tl.srt = 45)

# Statistical Model
# For Store 1 – Build  prediction models to forecast demand
# Hint : Linear Models

# H0 : There is no relation of Temperature,fuel_price,CPI,Unemployent on weekly sales of store1
# H1 : There is a relation and it affects the weekly sales of store 1 base on above indicators

store_1_data <- filter(walmart_data, Store == 1)
head(store_1_data)

model_store_1 <- lm(formula = Weekly_Sales ~ Temperature + Fuel_Price + CPI + Unemployment,data = store_1_data)
model_store_1

# Coefficients:
#  (Intercept)   Temperature     Fuel_Price        CPI      Unemployment  
#   -2727200         -2426        -31637          17872         90632  



# y = b0 + b1x1 + b2x2 + b3x3 + b4x4

# Weekly_Sales = (-2727200) +  Temp * (-2426 ) + Fuel_Price * (-31637) + CPI * 17872 + Unemployment * 90632
weekly_sales_1 <- 2727200 + 38.51 * (-2426 ) + 2.572 * (-31637) + 211.0964 * 17872 + 8.106 * 90632 # 72,22,523
weekly_sales_1
weekly_sales_2 <- 2727200 + 39.51 * (-2426 ) + 2.572 * (-31637) + 211.0964 * 17872 + 8.106 * 90632 # 72,20,097

weekly_sales_1 - weekly_sales_2
# Inference : With 1 degree increase in temperature,weekly sales of store 1 got decreased by 2,426

weekly_sales_3 <- 2727200 + 38.51 * (-2426 ) + 2.572 * (-31637) + 211.0964 * 17872 + 8.106 * 90632 # 72,22,523
weekly_sales_4 <- 2727200 + 39.51 * (-2426 ) + 2.572 * (-31637) + 211.0964 * 17872 + 7.106 * 90632 # 71,29,465

weekly_sales_3 - weekly_sales_4 #93058
# Inference : With 1 unit fall in unemployment,weekly sales of store 1 got decreased by 93058

summary(model_store_1)
rsqd_model_store_1 <- summary(model_store_1)$r.squared
rsqd_model_store_1 #0.1290992

# Call:
#  lm(formula = Weekly_Sales ~ Temperature + Fuel_Price + CPI + 
#     Unemployment, data = store_1_data)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -316968  -85750  -15239   51482  844800 

# Coefficients:
# Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  -2727200.0  1759518.7  -1.550  0.12344   
# Temperature     -2426.5      917.8  -2.644  0.00915 **
# Fuel_Price     -31637.1    47551.8  -0.665  0.50696   
# CPI             17872.1     6807.0   2.626  0.00963 **
# Unemployment    90632.0    58925.1   1.538  0.12632   
--------------------------------------------------------------------------------------------------
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  
  # Residual standard error: 147700 on 138 degrees of freedom
  # Multiple R-squared:  0.1291,	Adjusted R-squared:  0.1039 
  # F-statistic: 5.114 on 4 and 138 DF,  p-value: 0.0007142
  ---------------------------------------------------------------------------------------------------
  # p-value_temp -> 0.00915 < alpha(0.05) -> Rej Ho, temp has effect on Sales
  # p-value_fuel_price -> 0.50696 > alpha(0.05) -> Do not rej Ho, Fuel_Price has no effect on Sales
  # p_value_cpi -> 0.00963 < alpha(0.05) -> Rej Ho, CPI has effect on Sales  
  # p_value_unemployment -> 0.12632 > alpha(0.05)  -> Do not rej Ho, Fuel_Price has no effect on Sales
  
  predicted_sales_model_store_1 <- predict(model_store_1, store_1_data) 
predicted_sales_model_store_1[1:10] # vector[first 10 values] 

library(caret)
library(lattice)
library(ggplot2)

rmse(model_store_1$coefficients) # 1220423
rmse(model2_store_1$residuals) # 145054.6
rmse(model_store_1$rank) # 5

model2_store_1 <- lm(formula = Weekly_Sales ~ Fuel_Price + CPI + Unemployment,data = store_1_data)
model2_store_1

weekly_sales_1 <- (-3887096) + 2.572 * (-64838) + 211.0964 * (21792) + 8.106 * 124064 

weekly_sales_2 <- (-3887096) + 3.575 * (-64838) + 211.0964 * (21792) + 8.106 * 124064 

weekly_sales_1 - weekly_sales_2

summary(model2_store_1)

rsqd_model2_store_1 <- summary(model2_store_1)$r.squared
rsqd_model2_store_1


# Linear Regression – Utilize variables like date and restructure dates as 1 for 5 Feb 2010 
# (starting from the earliest date in order). 
# Hypothesize if CPI, unemployment, and fuel price have any impact on sales.
# Change dates into days by creating new variable.

# Hint: Convert Dates to days using - mutate and Days = yday(Date).
# Then subtract the number of days such that day 1 is 05-02-2010.
# Hint :Restructure dates as 1 for 5 Feb 2010

x <- as.Date("2009-09-02")
yday(x) # 245
mday(x) # 2
wday(x) # 4

# walmart_data$Date <- as.Date(walmart_data$Date,format = "%d-%m-%Y")

str(store_1_data)
store_1_data$Date <- as.Date(store_1_data$Date,format = "%d-%m-%Y")

mutate(store_1_data,date_to_days = yday(store_1_data$Date))
head(store_1_data)

library(caret)


# Select the model which gives best accuracy.
# accuracy of different model

# load the library
library(mlbench)
library(caret)
View(store_1_data)
str(store_1_data)
dim(store_1_data)

# load the dataset
store_1_data <- filter(walmart_data, Store == 1)
head(store_1_data)

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
summary(control)

# train the knn model
set.seed(7)
modelknn <- train(Weekly_Sales~ Temperature + Fuel_Price + CPI + Unemployment, data=store_1_data, method="knn", trControl=control)

# k-Nearest Neighbors 

#143 samples
#4 predictor

#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 3 times) 
#Summary of sample sizes: 128, 129, 127, 127, 130, 129, ... 
#Resampling results across tuning parameters:

#  k  RMSE      Rsquared    MAE     
#  5  153847.5  0.08426447  113749.3
#  7  145339.6  0.10447335  108990.8
#  9  143350.5  0.11385641  107428.6

#RMSE was used to select the optimal model using the smallest value.
#The final value used for the model was k = 9.

# train the SVM model
set.seed(7)
modelSvm <- train(Weekly_Sales~ Temperature + Fuel_Price + CPI + Unemployment, data=store_1_data, method="svmRadial", trControl=control)

# Support Vector Machines with Radial Basis Function Kernel 

#143 samples
#4 predictor

#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 3 times) 
#Summary of sample sizes: 128, 129, 127, 127, 130, 129, ... 
#Resampling results across tuning parameters:

#  C     RMSE      Rsquared   MAE      
# 0.25  139328.5  0.1456579   98954.88
# 0.50  141059.5  0.1337216  100307.49
# 1.00  143872.8  0.1233928  102696.92

# Tuning parameter 'sigma' was held constant at a value of 0.437534
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were sigma = 0.437534 and C = 0.25.

set.seed(7)
library(randomForest)
modelRF = randomForest(Weekly_Sales ~ Temperature + Fuel_Price + CPI + Unemployment,data = store_1_data,importance = TRUE,trControl=control)

summary(modelRF)
# No. of variables tried at each split: mtry
# No of trees to grow : ntree

# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 1

# Mean of squared residuals: 22782540734
# % Var explained: 5.7

modelRF.pred = predict(modelRF,type = 'class')
head(modelRF.pred)
rmse(modelRF$rsq)

varImpPlot(modelRF)


# collect resamples
model_comp <- resamples(list(KNN = modelknn,SVM = modelSvm))

# summarize the distributions
summary(model_comp)

#Models: KNN, SVM 
#Number of resamples: 30 

#MAE 
#Min.  1st Qu.    Median      Mean  3rd Qu.     Max. NA's
#KNN 50374.11 92792.31 107333.20 107428.63 124325.6 164640.1    0
#SVM 58488.21 78533.76  97221.75  98954.88 114552.6 161452.3    0

#RMSE 
#        Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
#KNN 60410.38 108696.2 132367.0 143350.5 171839.3 289116.0    0
#SVM 74040.06 105896.2 125043.8 139328.5 167254.5 296708.6    0

#Rsquared 
#Min.    1st Qu.     Median      Mean   3rd Qu.      Max. NA's
#KNN 6.220629e-05 0.03920810 0.08371538 0.1138564 0.1700934 0.3568544    0
#SVM 1.884200e-04 0.04563576 0.11154572 0.1456579 0.2059452 0.7038076    0


# boxplots of results
bwplot(model_comp)
# dot plots of results
dotplot(model_comp)

#########################################################################################
# Linear Regression – Utilize variables like date and restructure dates as 1 for 5 Feb 2010 
# (starting from the earliest date in order). 
# Hypothesize if CPI, unemployment, and fuel price have any impact on sales.
# Change dates into days by creating new variable.

# Hint: Convert Dates to days using - mutate and Days = yday(Date).
# Then subtract the number of days such that day 1 is 05-02-2010.
# Hint :Restructure dates as 1 for 5 Feb 2010

x <- as.Date("2009-09-02")
yday(x) # 245
mday(x) # 2
wday(x) # 4

View(walmart_data)
dates_to_days <- mutate(walmart_data,Days = yday(Date - 35))
head(dates_to_days)
dim(dates_to_days)


ls()












