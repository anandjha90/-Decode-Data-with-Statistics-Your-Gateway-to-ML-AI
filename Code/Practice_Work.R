# TO OPEN FILES

# ctrl+shift+c select and then for multiple line comment
data <- read.csv(file.choose(), header=T)
data2 <- read.excel(file.choose(), header=T)



v <- c(2,5,5,6,9,10)
t <- c(8,2.514,9)

## Key Board Shortcuts for R Studio
Alt+Shift+k

## KeyBoard Shortcut for Pipe
cmd+shift+m

v>t
v==t
print(v)
print(t)

################  practice problem self learning ##################################
vector1 <- c(4,2,1)
vector2 <- c(22,34,76,88,98,65)
column.names <- c("col1","col2","col3")
row.names <- c("row1","row2","row3") 
matrix.names <- c("Matrix1","Matrix2")
result <- array(c(vector1,vector2), dim = c(3,3,2),dimnames = list(row.names,column.names,matrix.names))
print(result)
print(result[2,,1])


x <- factor(c("male","female","female","male"),levels = c("male","female"));
x
class(x) 
mode(x)
 
p <- c(1,2,3,4,5L)
class(p)
my.name <- readline(prompt = "Enter Your Name : ")
my.name
p[0]
p[-1]
p[-c(2,4)]

vec <- c(2,4,0.89,15.86,26,8,9.7,5.90,6.34,3)
sort(vec)
sort(vec,decreasing = TRUE)
head(vec)
tail(vec)

data1 <- rep(c(7,10,9),times = 5) 
data1

data2 <- rep(1:3,each = 5)
data2

rbind(1:3,4:6)
cbind(1:3,4:6)
rbind(1:3,4:10)

x <- matrix(c(1:9),nrow = 3,byrow = T)
x
dim(x)


arr <- array(c(2:15),dim = c(2,4,3))
arr

arr[2,3,2]

m <- matrix(c(1,2,3,4),2,2)
m
apply(m,1,sum)
apply(m,2,sum)

list <- list(a=c(1,1),b=c(2,2),c=c(3,3))
list                 
lapply(list,sum)
lapply(list, mean)

sapply(list, sum)
list <- list(a=c(1,2),b=c(1,2,3),c=c(1,2,3,4))
list
sapply(list, range)

View(mtcars)
df1 = read.table("fastfood-1.txt",stringsAsFactors = FALSE,sep = "\t", fill = TRUE,header = TRUE)
df1
r = c(t(as.matrix(df1)))
r

getwd()
setwd()
library(MASS)
head(survey)
tbl = table(survey$Smoke,survey$Exer)
tbl
chisq.test(tbl)

RSC <- read.csv("RetailScoreData.csv",header = TRUE)
View(RSC)

###################### APPLY FUNC ###############################
data_matrix <- matrix(c(1:10,21:30),nrow = 5,ncol = 4)
data_matrix
class(data_matrix)

apply(data_matrix, 1,mean)
apply(data_matrix, 2,mean)
apply(data_matrix, 1,sd)
apply(data_matrix, 2,sd)

apply(data_matrix, 1, function(x){sum(x>7)}) # 2 2 3 3 3 no of data in each row which are greater than 7

###################### LAPPLY FUNC ###############################
data_list <- list(a = 1:5,b = 6:10,c = 11:15)
data_list

lapply(data_list, mean)
lapply(data_list, range)

###################### SAPPLY FUNC ###############################
sapply(data_list, mean)
mode(sapply(data_list, mean))

sapply(data_list,range)
class(sapply(data_list, range))

###################### AGGREGATE FUNC ###############################
aggregate(income ~ branch,RSC,mean)
aggregate(income ~ branch,RSC,sum)
aggregate(income ~ branch,RSC,median)

## DataSets ##
library(datasets)
data()
View(iris)
?iris
View(mtcars)
?mtcars
head(mtcars)
library(dplyr)
select (mtcars,mpg,wt)
select (mtcars,mpg : wt)
mtcars <- tbl_df(mtcars)
mtcars
select (mtcars,mpg,hp)
summary(mtcars)
filter(mtcars,cyl == 8)

filter(mtcars,cyl < 6 & vs == 1)
library(hflights)
install.packages('hflights',dependencies = T)
library(hflights)
??hflights
head(hflights)

hflights <- tbl_df(hflights)
hflights


student_data <- read.csv("student.csv")


mode <- function(x,na.rm = FALSE)
{
  if(na.rm)
  {
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x,ux)))])
}

vec_test <- c(1,3,5,7,7,8,8,23,34,54,45,45)
vec_test
vec_unique <- unique(vec_test)
vec_unique
vec_match <- match(vec_test,vec_unique)
tabulate(vec_match)

View(mtcars)
str(mtcars)
mtcars$mpg <- impute(mtcars$mpg,mode)
mtcars <- tbl_df(mtcars)


age <- c(23,33,28,21,20,19,34)
gender <- c("m","m","m","f","f","f","m")
tapply(age, gender, mean) # to get mean of age gender wise
tapply(gender, age, mean)

RSC <-read.csv("RetailScoreData.csv")
str(RSC)
tapply(RSC$income,RSC$branch,mean)

tapply(age,gender,FUN = function(x) x= mean(x)+median(x)) # mean and median of age gender wise
library(datasets)
data()
View(iris)
select(iris,iris$Sepal.Length,iris$Petal.Length)



library(dplyr)
select(iris,starts_with("Petal"))
remove.packages("MASS")

library(dplyr)
View(mtcars)
arrange(mtcars,cyl,disp)

select(mtcars,mpg,cyl)

select(mtcars,mpg,cyl)[11:20,]

summarise(group_by(mtcars,cyl),mean(wt))

summarise(group_by(mtcars,cyl),mean_wt = mean(wt),max_wt = max(wt),min_wt = min(wt),sd_wt = sd(wt))

library(hflights)
View(hflights)
head(hflights)
hflights <- tbl_df(hflights)
hflights

stud_data <- read.csv("student.csv",header = T)
stud_data
summary(stud_data)
trun_stud <- tbl_df(stud_data)
trun_stud

sum(is.na(stud_data$ID))
complete.cases(stud_data$ID)
levels(stud_data$Pstatus)
head(stud_data$ID,10)
hist(stud_data$ID,10)
sum(is.na(stud_data$ID))


install.packages("pscl") # once per machine
library("pscl")          # in each relevant script
View(presidentialElections)

# A function that returns the value in a vector furthest from 50
furthest_from_50 <- function(vec) 
  { 
  # Subtract 50 from each value 
  adjusted_values <- vec - 50
  
  # Return the element with the largest absolute difference from 50
  vec[abs(adjusted_values) == max(abs(adjusted_values))] 
 }

# Summarize the data frame, generating a column `biggest_landslide` 
# that stores the value furthest from 50%
summarize (presidentialElections,biggest_landslide = furthest_from_50(demVote))


## “Which state had the highest percentage of votes for the Democratic Party 
## candidate (Barack Obama) in 2008?”

# Use a sequence of steps to find the state with the highest 2008
# `demVote` percentage

# 1. Filter down to only 2008 votes
votes_2008 <- filter(presidentialElections, year == 2008) 

# 2. Filter down to the state with the highest `demVote`
most_dem_votes <- filter(votes_2008, demVote == max(demVote))

# Ask the same question of our data using the pipe operator 
most_dem_state <- presidentialElections %>% filter(year == 2008) %>% filter(demVote == max(demVote)) %>% select(state) 
most_dem_state

# 3. Select name of the state
most_dem_state <- select(most_dem_votes, state)
most_dem_state

state_voting_summary <- presidentialElections %>% group_by(state) %>% 
   summarize(mean_dem_vote = mean(demVote),mean_other_parties = mean(demVote))

str(presidentialElections)
View(presidentialElections)

library(nycflights13)
View(nycflights13)
?nycflights13
str(nycflights13)

# Compute the average delay by destination airport, omitting NA results
most_early <- flights %>%
  group_by(dest) %>% # group by destination
  summarize(delay = mean(arr_delay, na.rm = TRUE)) # compute mean delay
most_early

# Identify the month in which flights tend to have the longest delays
flights %>%
  group_by(month) %>% # group by selected feature
  summarize(delay = mean(arr_delay, na.rm = TRUE)) %>% # summarize delays filter(delay == max(delay)) %>% # filter for the record of interest 
  select(month) %>% # select the column that answers the question
  print() # print the tibble out directly
# A tibble: 1 x 1
#  month
#  <int>
#1   7

# Compute delay by month, adding month names for visual display 
# Note, `month.name` is a variable built into R

delay_by_month <- flights %>%
  group_by(month) %>%
  summarize(delay = mean(arr_delay, na.rm = TRUE)) %>% select(delay) %>%
  mutate(month = month.name)

delay_by_month

summarise(filter(hflights, Diverted==1), max_div=max(Distance))

# Remove rows that have NA ArrDelay: temp1
temp1 <- filter(hflights, !is.na(ArrDelay))

# Generate summary about ArrDelay column of temp1
summarise(temp1, earliest=min(ArrDelay), average=mean(ArrDelay), latest=max(ArrDelay), sd=sd(ArrDelay))

# Keep rows that have no NA TaxiIn and no NA TaxiOut: temp2
temp2 <- filter(hflights, !is.na(TaxiIn), !is.na(TaxiOut))

# Print the maximum taxiing difference of temp2 with summarise()
summarise(temp2, max_taxi_diff=max(TaxiOut-TaxiIn))

##############################################################################################################      
#Take the hflights data set and then .
#Add a variable named diff that is the result of subtracting TaxiIn from TaxiOut, and then .
#Pick all of the rows whose diff value does not equal NA, and then .
#Summarise the data set with a value named avg that is the mean diff value.
hflights %>%
  mutate(diff=(TaxiIn-TaxiOut)) %>%
  filter(is.na(diff)) %>%
  summarise(avg=mean(diff))

#####################################################################################################      

## Starting with hflights, create a data frame d with the following variables:
  # Dest, UniqueCarrier, Distance, and ActualElapsedTime,
  # RealTime: the actual elapsed time plus 100 minutes. This will be an estimate of how much time a person spends getting from point A to point B while flying, including getting to the airport, security checks, etc.
  # mph: the speed with which you travel if you do the calculations with RealTime. ####Filter d to only keep observations for which mph is not NA and for which mph is below 70. Pipe the result to a summarise() call with the following variables:
  # n_less, the number of flights whose with non-NA mph under 70,
  # n_dest, the number of destinations that were traveled to under these conditions;
  # min_dist, the minimum distance of these flights;
  # max_dist, the maximum distance of these flights.

# Part 1, concerning the selection and creation of columns
d <- hflights %>%
  select(Dest, UniqueCarrier, Distance, ActualElapsedTime) %>%  
  mutate(RealTime = ActualElapsedTime + 100, mph = Distance / RealTime * 60)    

# Part 2, concerning flights that had an actual average speed of < 70 mph.
d %>%
  filter(!is.na(mph), mph < 70) %>%
  summarise( n_less = n(), 
             n_dest = n_distinct(Dest), 
             min_dist = min(Distance), 
             max_dist = max(Distance))

# n_non - the number of non-preferable flights in hflights,
# p_non - the percentage of non-preferable flights in hflights,
# n_dest - the number of destinations that non-preferable flights traveled to,
# min_dist - the minimum distance that non-preferable flights traveled,
# max_dist - the maximum distance that non-preferable flights traveled.
# Solve the exercise using a combination of dplyr verbs and %>%
hflights %>%
  mutate(RealTime = ActualElapsedTime + 100, mph = Distance / RealTime * 60) %>%
  filter(mph < 105 | Cancelled == 1 | Diverted == 1) %>%
  summarise(n_non = n(), 
            p_non = n_non / nrow(hflights) * 100, 
            n_dest = n_distinct(Dest), 
            min_dist = min (Distance), 
            max_dist = max(Distance))


#Generate a per-carrier summary of hflights with the following variables:
# n_flights, the number of flights flown by the carrier;
# n_canc, the number of cancelled flights;
# p_canc, the percentage of cancelled flights;
# avg_delay, the average arrival delay of flights whose delay does not equal NA.

hflights %>%
  group_by(UniqueCarrier) %>%
  summarise(n_flights = n(), 
            n_canc = sum(Cancelled == 1), 
            p_canc = mean(Cancelled == 1) * 100, 
            avg_delay = mean(ArrDelay, na.rm = TRUE)) %>%
  arrange(avg_delay, p_canc)

hflights %>% 
  group_by(DayOfWeek) %>%
  summarise(avg_taxi = mean(TaxiIn + TaxiOut, na.rm=TRUE)) %>%
  arrange(desc(avg_taxi))

## First, discard flights whose arrival delay equals NA. Next, create a by-carrier summary with a single variable: p_delay, the proportion of flights which are delayed at arrival. Next, create a new variable rank in the summary which is a rank according to p_delay. Finally, arrange the observations by this new rank.
#  hflights %>%

  filter(!is.na(ArrDelay)) %>%
  group_by(UniqueCarrier) %>%
  summarise(p_delay = mean(ArrDelay > 0)) %>%
  mutate(rank = rank(p_delay)) %>%
  arrange(rank)
  
  #First, discard flights whose arrival delay equals NA. 
  #Next, create a by-carrier summary with a single variable: p_delay, the proportion of flights which are delayed at arrival. 
  #Next, create a new variable rank in the summary which is a rank according to p_delay. 
  #Finally, arrange the observations by this new rank.
  
hflights %>%
    filter(!is.na(ArrDelay)) %>%
    group_by(UniqueCarrier) %>%
    summarise(p_delay = mean(ArrDelay > 0)) %>%
    mutate(rank = rank(p_delay)) %>%
    arrange(rank)


hflights %>% 
    group_by(DayOfWeek) %>%
    summarise(avg_taxi = mean(TaxiIn + TaxiOut, na.rm=TRUE)) %>%
    arrange(desc(avg_taxi))
  
  rank(c(21, 22, 24, 23))
  
#################################################################################################
# Which plane (by tail number) flew out of Houston the most times? How many times? Name the column with this frequency n. 
# Assign the result to adv1. To answer this question precisely, you will have to filter() as a final step 
# to end up with only a single observation in adv1.
# Which plane (by tail number) flew out of Houston the most times? How many times? adv1
  adv1 <- hflights %>%
    group_by(TailNum) %>%
    summarise(n = n()) %>%
    filter(n == max(n))

#################################################################################################
# How many airplanes only flew to one destination from Houston? Save the resulting dataset in adv2, 
# that contains only a single column, named nplanes and a single row.
# How many airplanes only flew to one destination from Houston? adv2
  adv2 <- hflights %>%
    group_by(TailNum) %>%
    summarise(ndest = n_distinct(Dest)) %>%
    filter(ndest == 1) %>%
    summarise(nplanes = n())

#################################################################################################    
# Find the most visited destination for each carrier and save your solution to adv3. 
# Your solution should contain four columns:
# UniqueCarrier and Dest,n, how often a carrier visited a particular destination,
# rank, how each destination ranks per carrier. rank should be 1 for every row, as you want 
# to find the most visited destination for each carrier.
# Find the most visited destination for each carrier: adv3
  
  adv3 <- hflights %>% 
    group_by(UniqueCarrier, Dest) %>%
    summarise(n = n()) %>%
    mutate(rank = rank(desc(n))) %>%
    filter(rank == 1)

#################################################################################################      
  
# For each destination, find the carrier that travels to that destination the most. 
# Store the result in adv4. Again, your solution should contain 4 columns: 
# Dest, UniqueCarrier, n and rank.
# Find the carrier that travels to each destination the most: adv4
  adv4 <- hflights %>% 
    group_by(Dest, UniqueCarrier) %>%
    summarise(n = n()) %>%
    mutate(rank = rank(desc(n))) %>%
    filter(rank == 1)
  
#################################################################################################        
# hflights2 is a copy of hflights that is saved as a data table. 
# hflights2 was made available in the background using the following code:
# library(data.table)
  ## Warning: package 'data.table' was built under R version 3.2.2
  ## 
  ## Attaching package: 'data.table'
  ## 
  ## The following objects are masked from 'package:dplyr':
  ## 
  ##     between, last
  
  hflights2 <- as.data.table(hflights)
#################################################################################################
  # hflights2 contains all of the same information as hflights, 
# but the information is stored in a different data structure. 
# You can see this structure by typing hflights2 at the command line. 
# Even though hflights2 is a different data structure, you can use the same 
# dplyr functions to manipulate hflights2 as you used to manipulate hflights.
# Use summarise to calculate n_carrier
  s2 <- summarise(hflights2, n_carrier = n_distinct(UniqueCarrier))  
  
#################################################################################################  
# For example, we can easily retrieve a summary of how many carriers and how many flights 
# flew in and out of New York City in 2013 with the code (note that in nycflights, the UniqueCarrier variable is named carrier):
  summarise(nycflights13,n_carriers = n_distinct(carrier),n_flights = n())  

    
#################################################################################################
# Try to understand the already available code on the right. This code will create a reference to a tbl that resides on DataCamp’s servers.
# Glimpse at nycflights. Although nycflights is a reference to a tbl in a remote database, there is no difference in syntax nor output!
# Group the nycflights data by carrier, then create a grouped summary of the data that shows 
# the number of flights (n_flights) flown by each carrier and the average arrival delay (avg_delay) 
# of flights flown by each carrier. Finally, arrange the carriers by average delay from low to high.
  # set up a src that connects to the mysql database (src_mysql is provided by dplyr)
  library(dplyr)
  my_db <- src_mysql(dbname = "dplyr", 
                     host = "dplyr.csrrinzqubik.us-east-1.rds.amazonaws.com", 
                     port = 3306,
                     user = "dplyr",
                     password = "dplyr")
  
# and reference a table within that src: nycflights is now available as an R object that 
# references to the remote nycflights table
  nycflights <- tbl(my_db, "dplyr")
  
# glimpse at nycflights
 glimpse(nycflights)
  
# Calculate the grouped summaries detailed in the instructions
nycflights %>% group_by(carrier) %>% summarise(n_flights = n(), avg_delay = mean(arr_delay)) %>% arrange(avg_delay)  
  
#######################################################################################################################################  
library(pscl) 
 
# A function that returns the value in a vector furthest from 50
furthest_from_50 <- function(vec) { 
  # Subtract 50 from each value 
  adjusted_values <- vec - 50
  
  # Return the element with the largest absolute difference from 50
  vec[abs(adjusted_values) == max(abs(adjusted_values))] 
}

# Summarize the data frame, generating a column `biggest_landslide` 
# that stores the value furthest from 50%
summarize(
  presidentialElections,
  biggest_landslide = furthest_from_50(demVote) 
)
#######################################################################################################################################
#tot_sales_fig <- aggregate(Weekly_Sales ~ Store, walmart_data, sum)  
#class(tot_sales_fig)  
#library(ggplot2)
#store_tot_sales <- walmart_data %>% group_by(Store) %>% summarise(tot_sales = sum(Weekly_Sales)) %>% 
#ggplot(aes(x = Store, y = tot_sales, fill = Store)) +
#geom_bar(stat = "identity") +
#theme_classic() +
#labs(
# x = "Store Number",
#  y = "Total Weekly Sales",
#  title = paste(
#    "Walmat Retail Weekly Total Sales By Store Number"
# )
#)

#######################################################################################################################################  
library(dplyr)
# Step 1
data <- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/lahman-batting.csv") %>% 
  
# Step 2
  select(c(playerID, yearID, AB, teamID, lgID, G, R, HR, SH)) %>% 
  
# Step 3
  arrange(playerID, teamID, yearID)

glimpse(data)
# Use a sequence of steps to find the state with the highest 2008
# `demVote` percentage

# 1. Filter down to only 2008 votes
votes_2008 <- filter(presidentialElections, year == 2008) 

# 2. Filter down to the state with the highest `demVote`
most_dem_votes <- filter(votes_2008, demVote == max(demVote))

# 3. Select name of the state
most_dem_state <- select(most_dem_votes, state)

getwd()
summary(cars_data)

###################################################################################################################################################################
# dplyr in Action: Analyzing Flight Data

# Load the `nycflights13` package to access the `flights` data frame
install.packages("nycflights13") # once per machine
library("nycflights13")          # in each relevant script
library("dplyr")                 # load the dplyr library
library("ggplot2")               # for plotting

str(flights)
# Getting to know the data set
?flights          # read the available documentation
dim(flights)      # check the number of rows/columns
colnames(flights) # inspect the column names
View(flights)      # look at the data frame in the RStudio Viewer

# Identify the airline (`carrier`) that has the highest number of delayed flights
has_most_delays <- flights %>%            # start with the flights
  group_by(carrier) %>%                   # group by airline (carrier)
  filter(dep_delay > 0) %>%               # find only the delays
  summarize(num_delay = n()) %>%          # count the observations
  filter(num_delay == max(num_delay)) %>% # find most delayed
  select(carrier)                         # select the airline

# Get name of the most delayed carrier
most_delayed_name <- has_most_delays %>%  # start with the previous answer
  left_join(airlines, by = "carrier") %>% # join on airline ID
  select(name)                            # select the airline name

print(most_delayed_name$name) # access the value from the tibble

# Calculate the average arrival delay (`arr_delay`) for each destination (`dest`)
most_early <- flights %>%
  group_by(dest) %>% # group by destination
  summarize(delay = mean(arr_delay)) # compute mean delay

# Compute the average delay by destination airport, omitting NA results
most_early <- flights %>%
  group_by(dest) %>% # group by destination
  summarize(delay = mean(arr_delay, na.rm = TRUE)) # compute mean delay

# Identify the destination where flights, on average, arrive most early
most_early <- flights %>%
  group_by(dest) %>% # group by destination
  summarize(delay = mean(arr_delay, na.rm = TRUE)) %>% # compute mean delay, ignore NA
  filter(delay == min(delay, na.rm = TRUE)) %>% # filter for the *least* delayed
  select(dest, delay) %>% # select the destination (and delay to store it)
  left_join(airports, by = c("dest" = "faa")) %>% # join on `airports`data frame
  select(dest, name, delay) # select output variables of interest

print(most_early)

# Identify the month in which flights tend to have the longest delays
flights %>%
  group_by(month) %>% # group by selected feature
  summarize(delay = mean(arr_delay, na.rm = TRUE)) %>% # summarize value of interest
  filter(delay == max(delay)) %>% # filter for the record of interest
  select(month) %>% # select the column that answers the question
  print() # print the tibble out directly

# Compute delay by month, adding month names for visual display
# Note, `month.name` is a variable build into R
delay_by_month <- flights %>%
  group_by(month) %>%
  summarize(delay = mean(arr_delay, na.rm = TRUE)) %>%
  select(delay) %>%
  mutate(month = month.name)

# Create a plot using the ggplot2 package (described in Chapter 17)
ggplot(data = delay_by_month) +
  geom_point(
    mapping = aes(x = delay, y = month), 
    color = "blue",
    alpha = .4, 
    size = 3
  ) +
  geom_vline(xintercept = 0, size = .25) +
  xlim(c(-20, 20)) +
  scale_y_discrete(limits = rev(month.name)) +
  labs(title = "Average Delay by Month", y = "", x = "Delay (minutes)")


most_early <- flights %>%
  group_by(dest) %>% # group by destination
  summarize(delay = mean(arr_delay, na.rm = TRUE)) %>% # compute mean delay 
  filter(delay == min(delay, na.rm = TRUE)) %>% # filter for least delayed 
  select(dest, delay) %>% # select the destination (and delay to store it) 
  left_join(airports, by = c("dest" = "faa")) %>% # join on `airports` data 
  select(dest, name, delay) # select output variables of interest

print(most_early) 

###################################################################################################################################################################
# Date function
###################################################################################################################################################################

date_formatter <- as.Date("23/06/89", "%d/%m/%y")
weekdays(date_formatter)
quarters(date_formatter)
months(date_formatter)

seq(Sys.Date(),by = "month",length.out = 4)

###################################################################################################################################################################


###################################################################################################################################################################

stud_data <- read.csv("Student.csv")
colnames(stud_data)
hist(stud_data$absences)
boxplot(stud_data$absences,col = 'light blue')
bp = boxplot(stud_data$absences,horizontal = F, col = 'light blue')
bp$out
bp$stats
plot(density(log(stud_data$absences)))
boxplot(log(stud_data$absences),col = 'light blue')
library(CatEncoders)
library(dummies)
library(dplyr)

pnorm(1) - pnorm(-1) # LHS probability
pnorm(-1,lower.tail = F) - pnorm(1,lower.tail = F) #RHS probability
pnorm(115,100,15) - pnorm(85,100,15)

###################################################################################################################################################################
# Steps for Hypothesis Testing:
#   
#   1. Hypothesize about a population by stating H0 and H1.
#   2. Calculate the test statistic 
#   3. Specify the level of significance (alpha) and calculate the pvalue or the critical value
#   4. Compare the p_value with alpha; if p_value is less than alpha, Rej Ho
#                        OR 
#   4. Compare the critical value; Check if it lies in rejection region, if yes, reject ho.
#      Looking at at the normal curve.
#   5. Result of the test

# Problem
# Suppose the mean weight of King Penguins found in an Antarctic colony last year 
# was 15.4 kg. In a sample of 35 penguins same time this year in the same colony, 
# the mean penguin weight is 14.6 kg. Assume the population standard deviation is 2.5 kg. 


n <- 35        # sample size
sigma <- 2.5   #standard deviation
alpha <- 0.05  #level of significance
mu <- 15.4     #population mean
xbar <- 14.6   #sample mean

z_calx <- (xbar - mu)/(sigma/sqrt(n))
crit_val_neg <- qnorm(alpha/2)
crit_val_pos <- qnorm(alpha/2,lower.tail = F)
conf_int <- c(crit_val_neg,crit_val_pos)
# z lies in between conf_int  so we accept the NULL Hypothesis

# Problem
# Suppose the manufacturer claims that the mean lifetime of a light bulb is more than 10,000 hours.
# In a sample of 30 light bulbs, it was found that they only last 9,900 hours on average. 
# Assume the population standard deviation is 120 hours. 
# At .05 significance level, can we reject the claim by the manufacturer?

n <- 30        # sample size
sigma <- 120   #standard deviation
alpha <- 0.05  #level of significance # the claim is that no of hours > 10000 so i will reject if its < 10000 so we will be working with alpha not alpha/2.
mu <- 10000     #population mean
xbar <- 9900   #sample mean

z_calx <- (xbar - mu)/(sigma/sqrt(n))
crit_val_neg <- qnorm(alpha)
crit_val_pos <- qnorm(alpha,lower.tail = F)
conf_int <- c(crit_val_neg,crit_val_pos)
# z > crit_val_neg so we reject the NULL Hypothesis

# Suppose the food label on a cookie bag states that there is at most 2 grams of saturated 
# fat in a single cookie. In a sample of 35 cookies, it is found that the mean amount of 
# saturated fat per cookie is 2.1 grams. on food label?
# Assume that the population standard deviation is 
# 0.25 grams. At .05 significance level, can we reject the claim on food label?

# Ho : <= 2gm 
# H1 : > 2gm
n <- 35        # sample size
sigma <- 0.25   #standard deviation
alpha <- 0.05  #level of significance # the claim is that no of hours > 10000 so i will reject if its < 10000 so we will be working with alpha not alpha/2.
mu <- 2     #population mean
xbar <- 2.1   #sample mean

z_calx <- (xbar - mu)/(sigma/sqrt(n))
crit_val_neg <- qnorm(alpha)
crit_val_pos <- qnorm(alpha,lower.tail = F)
conf_int <- c(crit_val_neg,crit_val_pos)
# z value > critical value so we reject the null hypothesis

# Problem
# Suppose the food label on a cookie bag states that there is at most 2 grams
# of saturated fat in a single cookie. In a sample of 35 cookies, it is found
# that the mean amount of saturated fat per cookie is 2.1 grams. Assume that the
# sample standard deviation is 0.3 gram.
#At .05 significance level, can we reject
# the claim on food label?

#Assumption,xbar follows t-distribution t(mu,s/sqrt(n)) --- Central Limit Theorem
n <- 35        # sample size
sigma <- 0.3   # sample standard deviation
alpha <- 0.05  #level of significance 
xbar <- 2.1   #sample mean

t_calc <- (xbar - mu)/(sigma/sqrt(n))
pvalue <- pt(t_calc,lower.tail = F,n-1) #0.0283
pvalue < alpha # since p-value < alpha we reject H0:saturated fat in a single cookie > 2 grms

##################################################################################################################
# ANOVA Test - Analysis Of Variance

df3 <- read.csv("trainl.csv",header = T)
str(df3)
summary(df3)

# alpha - 0.05
# H0 : There is no significant difference of Applicant Income on Gender and Education
# H1 : There is a significant difference of Applicant Income on Gender and Education
av <- aov(ApplicantIncome ~ Gender + Education,df3)
summary(av) # So we reject on Gender but accept on Education

#################################################################################################################
## Various Error & Type
#################################################################################################################
# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

# Example data
actual <- c(4, 6, 9, 10, 4, 6, 4, 7, 8, 7)
predicted <- c(5, 6, 8, 10, 4, 8, 4, 9, 8, 9)

# Calculate error
error <- actual - predicted

# Example of invocation of functions
rmse(error)
mae(error)

# Example in a linear model
## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
## Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
rmse(lm.D9$residuals) # root mean squared error


#################################################################################################################
## Advertising Data
#################################################################################################################

data_adv <- read.csv('Advertising.csv',header = T)
summary(data_adv)
View(data_adv)
str(data_adv)
head(data_adv)
nrow(data_adv)
dim(data_adv)

# What are the features/ independent variables/ explainatory variables?
# - **TV:** advertising dollars spent on TV for a single product in a given market (in thousands of dollars)
# - **Radio:** advertising dollars spent on Radio
# - **Newspaper:** advertising dollars spent on Newspaper
# 
# What is the response/ dependent variable/ predictor variable?
# - **Sales:** sales of a single product in a given market (in thousands of items)
# 
# What else do we know?
# - Because the response variable is continuous, this is a **regression** problem.
# - There are 200 **observations** (represented by the rows), and each observation is a single market.


###############################################################################
#### Binning Of Data

# Discretizes all numerical data in a data frame into categorical bins of equal length or content or based on 
# automatically determined clusters. In statistics, data binning is a way to categorise a number of continuous 
# values into a smaller number of buckets (bins). Each bucket defines an numerical interval. 
# For example, if there is a variable about house-based education levels which are measured by continuous 
# values ranged between 0 and 19, data binning will place each value into one bucket if the value falls into 
# the interval that the bucket covers. This post shows data binning in R as well as visualizing the bins.


data_train <- read.csv("train.csv",header = T)
View(data_train)

data_train$hr_bins <- ifelse (data_train$hr < 6 , 'late night',
                              ifelse (data_train$hr < 9 , 'office going',
                                      ifelse (data_train$hr < 17 ,'mid day',
                                              ifelse (data_train$hr < 20 ,'office returning','night'))))

class(data_train$hr)

data_train$hr_bins <- as.factor(data_train$hr_bins)
class(data_train$hr_bins)

head(data_train)

com_test <- data_train[,c('hr', 'hr_bins')]
head(com_test)

# install.packages("dummies",dependencies = T)

library(dummies)
data_train <- dummy.data.frame(data_train,names = c('hr_bins',sep = '-'))

com_data <- data_train[,c('hr_bins - late_night', 'hr_bins - mid_day', 
                          'hr_bins - night', 'hr_bins - office_going ','hr_bins - office_returning')] 


# BINNING OF DATA
walmart_data <- read.csv("Walmart_Store_sales.csv",header = T)
walmart_data
View(walmart_data)
str(walmart_data)

library(dplyr)
str(walmart_data)
class(walmart_data$Date)
walmart_data$Date <- as.Date(walmart_data$Date,format = "%d-%m-%Y")

library(classInt)
bin_data <- walmart_data$Weekly_Sales
# EQUAL WIDTH
classIntervals(bin_data,5,style = "equal")

classIntervals(bin_data,5,style = "quantile")

##############################################################################################################
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

###############################################################################
# Heatmap
# A heatmap is a graphical representation of data where the individual values contained in a matrix 
# are represented as colors. The term ‘heat map’ was originally coined and trademarked by software designer 
# Cormac Kinney in 1991, to describe a 2D display depicting financial market information, 
# though similar plots such as shading matrices have existed for over a century.

subset2 <- subset(walmart_data, select = c('Weekly_Sales','Temperature','Fuel_Price','Unemployment','CPI'))
res <- cor(subset2)
head(res)
col <- colorRampPalette(c('blue','white','red'))(20)
heatmap(x = res, col = col, symm = TRUE )


###############################################################################














###############################################################################
#### First Linear Model - Simple Linear Model
###############################################################################

model <- lm(Sales ~ TV, data = data_adv) # lm(dependent ~ independent, data)

formula(model)

# Model Evaluation Technique - R-Squared Value

Rsqd <- summary(model)$r.squared
Rsqd # 0.6118751

# Model Evaluation Technique - RMSE

# requires predicted values

predicted_sales <- predict(model, data_adv) # yhat - equation is provided in model from data it gets independent variable
predicted_sales[1:10] # vector[first 10 values] # yhat

data_adv$Sales[1:10] # vector[first 10 values] # yi

(data_adv$Sales - predicted_sales)[1:10] # error
((data_adv$Sales - predicted_sales)^2)[1:10] # squared error
mean((data_adv$Sales - predicted_sales)^2) # # mean squared error
sqrt(mean((data_adv$Sales - predicted_sales)^2)) # sqrt(mean squared error)

RMSE = sqrt(mean((data_adv$Sales - predicted_sales)^2))
RMSE # 3.242322 - same units as your dependent variable

rmse(model$residuals)
rmse(model$coefficients)

# To Do :: Build a model for predicting Sales from Radio and Sales from Newspaper
model_rad_news <- lm(Sales ~ TV + Newspaper, data = data_adv)
formula(model_rad_news)
rsqd_rad_news <- summary(model_rad_news)$r.squared
predicted_sales_rad_news <- predict(model_rad_news, data_adv)[1:10]
rmse(model_rad_news$residuals)


###############################################################################
### Multiple Linear Regression
###############################################################################

model1 <- lm(Sales ~ TV + Newspaper + Radio, data = data_adv) # dependent variable ~ independent variables

#model1 <- lm(Sales ~ ., data = data_adv)

###############################################################

# Model Evaluation Technique - R-Squared Value

Rsqd <- summary(model1)$r.squared # explained variation of the model
Rsqd

# 0.8972106 = 89%

# Model Evaluation Technique - RMSE

formula(model1) 

predicted_sales <- predict(model1, data_adv)
predicted_sales[1:10]
length(predicted_sales)

data_adv$Sales[1:10]

# data$Sales - predicted_sales # error
# (data$Sales - predicted_sales)^2 # squared error
# mean((data$Sales - predicted_sales)^2) # mean squared error

RMSE = sqrt(mean((data$Sales - predicted_sales)^2)) # root mean squared error
RMSE # 1.66857 - same units as your dependent variable

rmse(model1$residuals)
#==========================#
# with TV    61%    3.23
# with ALL   89%    1.66
#==========================#

summary(model1)

# Ho: There is no linear relation between dependent and independent variables
# Ha: There is linear relation between dependent and independent variables

pvalue <- 2.2e-16
alpha <-  0.05

pvalue < alpha # if this is true = whenever p_value is less than alpha; we reject the null hypothesis

# Rej the null hypothesis and there is linear relation between dependent and independent variables

###############################################################

# Ho: b1 = 0 ; TV has no effect on Sales
# Ha: b1 != 0 ; TV has effect on Sales

pvalue <- 2.2e-16
alpha <-  0.05

pvalue < alpha # if this is true = whenever p_value is less than alpha; we reject the null hypothesis

# Rej Ho, TV has effect on Sales

###############################################################

# Ho: b2 = 0 ; Newspaper has no effect on Sales
# Ha: b2 != 0 ; Newspaper has effect on Sales

pvalue <- 0.86 
alpha <-  0.05

pvalue < alpha # if this is true = whenever p_value is less than alpha; we reject the null hypothesis

# Do not rej Ho, Newspaper has no effect on Sales

###############################################################

# Ho: b3 = 0 ; Radio has no effect on Sales
# Ha: b3 != 0 ; Radio has effect on Sales

pvalue <- 2e-16
alpha <-  0.05

pvalue < alpha # if this is true = whenever p_value is less than alpha; we reject the null hypothesis

# Rej Ho, Radio has effect on Sales

###############################################################

# y = b0 + b1x1 + b2x2 + b3x3

# Sales = 2.938889 + 0.045765 * TV + (-0.001037) * Newspaper + 0.188530 * Radio

Sales_1 = 2.938889 + 0.045765 * 1 + (-0.001037) * 1 + 0.188530 * 1
Sales_1

Sales_1001 = 2.938889 + 0.045765 * 1001 + (-0.001037) * 1 + 0.188530 * 1
Sales_1001

Sales_1001 - Sales_1 # 45.765 = b1 * 1000

Sales_2 = 2.938889 + 0.045765 * 2 + (-0.001037) * 1 + 0.188530 * 1
Sales_2

Sales_2 - Sales_1 # b1

# How do we interpret the **TV coefficient** ?

# - For a given amount of Radio and Newspaper ad spending, **a "unit" increase in TV ad spending** 
# is associated with a **0.0475 (b1) "unit" increase in Sales**.

# **an additional $1,000 spent on TV ads** is associated with an **increase in sales of 47.5 items**.

# install.packages('caTools',dependencies = T)

library(caTools)
# install.packages("ROCR",dependencies = T)
library(ROCR)

getwd()
View(mtcars)
library(e1071)
library(caret)
library(mlbench)


##################################################################################################
# Support Vector Machine (SVM) Model
##################################################################################################
# support vectors are data points that are closer to hyperplane nd influence the position 
# and orientation of the hyperplane.Using these support vectors,


titanic_data <- read.csv('titanic_data.csv')
View(titanic_data)

# Survived   - The passenger survived or not; 0 - did not, 1 - Survived

# Pclass_1   - The passenger was travelling in 1st class or not
# Pclass_2   - The passenger was travelling in 2nd class or not
# Sex_female - 1 - Female; 0 - Male
# Age        - Passenger's Age
# SibSp      - Number of Siblings/Spouse the passenger travelling with 
# Parch      - Number of Parents/Childern the passenger travelling with
# Fare       - Passenger's Ticket Fare
# Embarked_C - Did the passenger embark from C? 1 yes and 0 no  
# Embarked_Q - Did the passenger embark from Q? 1 yes and 0 no  

head(titanic_data)
str(titanic_data)
dim(titanic_data)

View(titanic_data)
str(titanic_data)

library(e1071)
train = titanic_data[split,]
head(train)
nrow(train)

test = titanic_data[!split,]
head(test)
nrow(test)

svm_fit1 <- svm(Survived~.,data = train,kernel = 'linear')
summary(svm_fit1)

# kernels available in SVM
# linear
# sigmoid
# radial basis
# polynomial

pred <- predict(svm_fit1,test) 
tbl1 <- table(pred,test$Survived)
accuracy <- mean(pred == test$Survived)
accuracy

set.seed(1)
tune.out <- tune(svm,Survived~.,data = train,kernel = 'linear',ranges = list(cost = c(0.1,1,10),
                                                                             gamma = c(0.5,1,2,3,4)))
tune.out
# Parameter tuning of ‘svm’:
# - sampling method: 10-fold cross validation 
# - best parameters:
#   cost gamma
#   0.1   0.5
# - best performance: 0.2024184  misclassification error

summary(tune.out)
# - Detailed performance results:
#     cost    gamma     error       dispersion
# 1   0.1      0.5      0.2024184   0.03125947
# 2   1.0      0.5      0.2024337   0.03129001
# 3  10.0      0.5      0.2024474   0.03128164
# 4   0.1      1.0      0.2024184   0.03125947
# 5   1.0      1.0      0.2024337   0.03129001
# 6  10.0      1.0      0.2024474   0.03128164
# 7   0.1      2.0      0.2024184   0.03125947
# 8   1.0      2.0      0.2024337   0.03129001
# 9  10.0      2.0      0.2024474   0.03128164
# 10  0.1      3.0      0.2024184   0.03125947
# 11  1.0      3.0      0.2024337   0.03129001
# 12 10.0      3.0      0.2024474   0.03128164
# 13  0.1      4.0      0.2024184   0.03125947
# 14  1.0      4.0      0.2024337   0.03129001
# 15 10.0      4.0      0.2024474   0.03128164

svm_fit2 <- svm(Survived~.,data = train,kernel = 'linear',gamma = 0.5,cost = 0.1)
summary(svm_fit2)

# Parameters:
#  SVM-Type:  eps-regression 
#   SVM-Kernel:  linear 
#   cost:  0.1 
#   gamma:  0.5 
#   epsilon:  0.1 

# Number of Support Vectors:  339

set.seed(1)
tune.out2 <- tune(svm,Survived~.,data = train,kernel = 'sigmoid',ranges = list(cost = c(0.1,1,10),
                                                                               gamma = c(0.5,1,2,3,4)))
tune.out2
summary(tune.out2)

tune.out2$best.model

svm_fit3 <- svm(Survived~.,data = train,kernel = 'sigmoid',gamma = 0.5,cost = 0.1)
summary(svm_fit3)

pred3 <- predict(svm_fit3,test) 
tbl3 <- table(pred,test$Survived)
accuracy3 <- mean(pred == test$Survived) #0.7191011

set.seed(1)
tune.out.model <- tune(svm,Survived~.,data = train,ranges = list(cost = c(0.1,1),
                                                                 gamma = c(0.5,1),kernel = c("linear","sigmoid","polynomial")))

summary(tune.out.model) # check lowest error

tune.out.model$best.model



###############################################################
# Naive Bayes Non Linear Classification Model
###############################################################
library(e1071)
#load and verify the bank data
trainl_data <-  read.csv("trainl.csv",header = T)
str(trainl_data$Dependents)
View(trainl_data)

# convert dependent from into to factor
trainl_data$Dependents <- sapply(trainl_data$Dependents,factor)

# build the model
naive_model <- naiveBayes(Dependents ~.,data = trainl_data)
print(naive_model)


# The model creates conditional probability for each feature separately 
# We also have apriori probabilities which indicates the distribution of our data

# predict
naive_predict <- predict(naive_model,trainl_data)
naive_predict

table(naive_predict,trainl_data$Dependents)
# naive_predict   
#     0    1
# 0  330   39
# 1  30    215


####################################################################################################
# Decision Tree Classification model # White Box Model 
####################################################################################################
library(rpart)
# Bank is interested in knowing the factors which influences the default of a customer
# Bank will utilize this information in future to avoid giving loans to the customer who are more risky to the business.

#load and verify the bank data
bank_loan <-  read.csv("trainl.csv",header = T)
str(bank_loan)
View(bank_loan)

# convert dependent from int to factor
bank_loan$Dependents <- sapply(bank_loan$Dependents,factor)

# build the model
tree_model <- rpart(Dependents ~.,data = bank_loan,method = "class")
tree_model
summary(tree_model)

# analyze results
printcp(tree_model)
plotcp(tree_model)
plot(tree_model)

# Classification tree:
# rpart(formula = Dependents ~ ., data = bank_loan, method = "class")

# Variables actually used in tree construction:
# [1] Loan_ID

# Root node error: 254/614 = 0.41368

# n= 614 

# CP nsplit rel error  xerror     xstd
# 1 1.00      0         1 1.00000 0.048045
# 2 0.01      1         0 0.95276 0.047672


library(tree)

tree.Survived <- tree(Survived~.,data = train)
summary(tree.Survived)

dim(train)
# 711 10

tree.pred <- predict(tree.Survived,test)

tree.pred[1:10]
test$Survived[1:10]


accuracy <- mean(tree.pred == test$Survived)

####################################################################################################
# Random Forest Validation # Black Box Model # Interpretation
####################################################################################################

library(randomForest)
bag = randomForest(Survived ~.,data = train,importance = TRUE)
bag

bag_tried = randomForest(Survived ~.,data = train,ntree = 100,mtry = 6,importance = TRUE)
summary(bag_tried)
# No. of variables tried at each split: mtry
# No of trees to grow : ntree

bag.pred = predict(bag_tried,test,type = 'class')
table(bag.pred,test$Survived)

accuracy <- mean(bag.pred == test$Survived)

varImpPlot(bag_tried)


####################################################################################################
# K- Fold Cross Validation
####################################################################################################

# A Bank wants to understand if its campaign was effective and the 
# factors that decide the success of the campaign
# Dataset Used : Bank Customer Data

library(plyr)
library(dplyr)
library(caret)
str(bank_loan)

#k-fold cross validation
folded_up <- createFolds(bank_loan,k = 10,list = TRUE,returnTrain = FALSE)
train_set <- names(folded_up[1])
bank_loan[folded_up$train_set,]

########################################################################################################
# accuracy of different model

# load the library
library(mlbench)
library(caret)
View(PimaIndiansDiabetes)
str(PimaIndiansDiabetes)

# load the dataset
data(PimaIndiansDiabetes)

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the knn model
set.seed(7)
modelknn <- train(diabetes~., data=PimaIndiansDiabetes, method="knn", trControl=control)

# train the LogitBoost model
set.seed(7)
modelLogitBoost <- train(diabetes~., data=PimaIndiansDiabetes, method="LogitBoost", trControl=control, verbose=FALSE)

# train the SVM model
set.seed(7)
modelSvm <- train(diabetes~., data=PimaIndiansDiabetes, method="svmRadial", trControl=control)

# train the naive_bayes	model
set.seed(7)
	
modelNaive_bayes <- train(diabetes~., data=PimaIndiansDiabetes, method="naive_bayes", trControl=control)

# collect resamples
results <- resamples(list(KNN=modelknn, LogitBoost=modelLogitBoost, SVM=modelSvm,NB=modelNaive_bayes))

# summarize the distributions
summary(results)

# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)

str(store_1_data)

##################################################################################################
View(store_1_data)
Weekly_Sales <- store_1_data$Weekly_Sales

normalize <- function(x) 
{
  num <- x - min(x)
  denom <- max(x) - min(x)
  return(num/denom)
}

store_1_data$Weekly_Sales <-  Weekly_Sales

dim(store_1_data)
summary(store_1_data)

# Spliting dataset
train <- store_1_data[1:95,] #2/3 rd
test <- store_1_data[96:143,] #1/3rd

head(train)
head(test)

mode(store_1_data)

lapply(store_1_data[,-3], normalize)

normalize_data <- as.data.frame((lapply(store_1_data[,1:8], normalize)))
head(data)

########################################################################################
## Build a kNN model (with k = 10) on the training dataset in R to predict the diabetes 
## (pos or neg). So here we will consider "diabetes" as Class variable. Then test the 
## model on the testing dataset. Calculate accuracy and error rate.


cl <- train$Weekly_Sales # defining class - predictor variable

control <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the knn model
set.seed(7)
modelknn <- train(Weekly_Sales~ Temperature + Fuel_Price + CPI + Unemployment, data=store_1_data, method="knn", trControl=control)


Accuracy <- mean(modelknn == test$Weekly_Sales)
Accuracy#  0.7678571

# Even number k can give different accuracy at each iteration.

model <- knn(train[-9], test[-9], cl, k = 11)
Accuracy <- mean(model == test$diabetes)
Accuracy

c2

############################################################################
## Perform k-fold validation (with k = 10) on PimaIndiansDiabetes data.

# K fold with k = 10

library(caret)

control <- trainControl(method = "cv", number = 10, classProbs=TRUE,
                        summaryFunction = twoClassSummary)

# will compute the sensitivity, specificity and area under the ROC curve


fit_knn <- train(diabetes ~ ., data=PimaIndiansDiabetes, method="knn",
                 metric="ROC", trControl=control)

fit_knn

# k-Nearest Neighbors 
# 
# 768 samples
# 8 predictors
# 2 classes: 'neg', 'pos' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 691, 691, 691, 691, 691, 691, ... 
# Resampling results across tuning parameters:
#   
# k  ROC        Sens   Spec     
# 5  0.7405755  0.816  0.5146724
# 7  0.7639858  0.830  0.5484330
# 9  0.7805883  0.838  0.5330484
# 
# ROC was used to select the optimal model using the largest value.
# The final value used for the model was k = 9.

###############################################################################################3
library(arules)
library(arulesViz)
data("Groceries")
View(Groceries)
?Groceries

itemFrequencyPlot(Groceries)
itemFrequencyPlot(Groceries, topN = 20)
itemFrequencyPlot(Groceries, topN = 20,type = "absolute")

# We will always have to pass the minimum required support and confidence
# We set the minimum support to 0.001
# We set the minimum confidence of 0.8
# We then show the top 5 rules

# Get the rules
rules <- apriori(Groceries,parameter = list(supp = 0.001,conf = 0.80,maxlen = 3),control = list(verbose = F))
inspect(rules[1:5])
summary(rules)

rules <- sort(rules,by = "confidence",decreasing = TRUE)
inspect(rules[1:5])


rules <- sort(rules,by = "support",decreasing = TRUE)
inspect(rules[1:5])










c = "Apples" = "Bananas"

c(0,1,2,3,4,5)[2:4]
c(1:3)

for (num in c(1,2,3)) {
print(num)  
}
getwd()
read.csv("/users/anandjha/Walmart_Store_sales.csv")

grep("milk.+", c("cow's milk", "milkshake", "milky", "cat", "milk1", "milk"), value = T)


fullname <- "John Doe"
strsplit(fullname," ")



drinks <- factor(c("tea", "coffee", "soft drink", "tea", "hot chocolate", "hot chocolate", "coffee"))
levels(drinks)


library(help = datasets) # viewing all the default datasets in R
# install.packages("tidyverse",dependencies = T)
# install.packages("pacman",dependencies = T)

pacman::p_load(pacman,tidyverse)
library(datasets)

# time series data
?uspop
uspop
plot(uspop)
ts.plot(uspop)
plot.ts(uspop)

?EuStockMarkets
plot(EuStockMarkets) # diffent graph for different index
ts.plot(EuStockMarkets) # all stacked together


walmart_data <- read.csv("Walmart_Store_sales.csv",header = T)
fivenum(walmart_data$Weekly_Sales)
fivenum(walmart_data$Fuel_Price) # min,lowe-rhinge,median,upper-hinge,max
boxplot.stats(walmart_data$Fuel_Price)
boxplot(walmart_data$Fuel_Price,notch = T,horizontal = T)

cor.test(walmart_data$Weekly_Sales,walmart_data$Unemployment)

# rcorr works on matrix not dataframe to get both a correlation matrix and p-values table
#library(Hmisc)

boxplot(mtcars$mpg,col = "red")

hist(mtcars$mpg , col = "red")
hist(mtcars$mpg , col = "blue" , breaks = 25)

barplot(table(mtcars$carb),col = "green")

boxplot(mpg~gear,data = mtcars , col = "red")

hist(subset(mtcars, cyl == 4)$mpg , col = "red")
hist(subset(mtcars, cyl == 8)$mpg , col = "green")

#### scatter plot
with(mtcars,plot(mpg,qsec))





