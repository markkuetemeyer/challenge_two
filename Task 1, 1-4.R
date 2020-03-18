library(data.table)
library(pROC)
library(tidyverse)

machines = fread(file = "machine_data.csv",
                 sep = ',',
                 header = TRUE,
                 stringsAsFactors = FALSE,
                 dec = '.')

products = fread(file = "product_data.csv",
                 sep = ',',
                 header = TRUE,
                 stringsAsFactors = FALSE,
                 dec = '.')

transaction = fread(file = "transactional_data.csv",
                    sep = ',',
                    header = TRUE,
                    stringsAsFactors = FALSE,
                    dec = '.')

mfailures = fread(file = "machine_failures.csv",
                  sep = ',',
                  header = TRUE,
                  stringsAsFactors = FALSE,
                  dec = '.')

machines
products
transaction
mfailures

#P1################################################################################################################################################################

#1. Merge the transactional dataset with the machine failures data set setting failure variable to 0 when no failure is recorded
#Hint: The syntax to do merges is merge(dt1,dt2, by=c(‘field1’,’field2’),all.x=T) and to set to 0 try dt[is.na(failure),failure:=0]
summary(mfailures)
summary(transaction)

#merge transaction and mfailures data.tables
merge_failures = merge(transaction,mfailures, by = c('machine', 'timestamp'), all.x=T)

#replace NAs with 0 in new data.table "merge_failures"
merge_failures[is.na(failure),failure:=0]

#drop column.y because is a copy of column.x 
#where column.x is the column named "column" in dt transation AND column.y is the column named "column" in dt mfailures
merge_failures = merge_failures[,-6]
merge_failures

#-------------------------------------------------------------------------------------------------------------------------------
#2. In the transactional data table, create a variable called “last_vend” containing the timestamp of the previous sale of each machine
#Hint: Remember you can use the function “shift” once the data is ordered according to machine and date with function “order” i.e. dt = dt[order(x,y)], where x and y are column names of dt

#order by machine and date
transaction = transaction[order(machine,date)] 

#create "last stand colum" with shift (one down):
transaction[, last_vend := shift(timestamp)]    #as DT.     #As DF: transaction$last_vend=transaction$shift(timestamp)
summary(transaction)

# If we would like an empty first row for each machine:
#transaction[, last_vend := c(NA, timestamp[-.N]), by=machine] 

#-------------------------------------------------------------------------------------------------------------------------------
#3. Create a new variable in the transactional data table called “deltahours” containing, for every sale, the hours that passed since the last sale
#Hint: Check function “difftime”
help("difftime")  

transaction[, deltahours := difftime(timestamp,last_vend,units = mins)]   
#check if in minutes (could be in auto, hours, secs, etc)

#-------------------------------------------------------------------------------------------------------------------------------
#4. Create an auxiliary data table called “machine_daily_average” 
#with the average daily sales per machine. Use this auxiliary table to 
#attach to every row of the transactional data table the the average 
#daily sales per machine. You can do this by doing a merge.
#Hint: Check function merge again via help(merge)

machine_daily_average = data.table(transaction[, .(avg_daily_sales = length(product_name)/uniqueN(date)),by=machine])
machine_daily_average 

#P2################################################################################################################################################################

#-------------------------------------------------------------------------------------------------------------------------------
#5. Create a new variable called “delta” in the transactional data table
#containing a normalized version of deltahours consisting on the deltahours 
#associated with each sale divided by the average deltahours of each machine 
#i.e. delta = deltahours /(24/daily_sales_machine). The interpretation of delta 
#is the amount of “missed sales” if the machine was selling at a constant rate


#-------------------------------------------------------------------------------------------------------------------------------
#6. Select 30% of the machines in the transactional data for testing and 70% of the machines 
#for training and train a linear logistic regression model called “m” to predict whether
#a machine has a failure as a function of variable delta. What is the value of the intercept 
#and the coefficient accompanying variable delta?
#Hint: Recall the syntax for the linear model is glm(target~ variable,data,family=’binomial’).
#You can select 70% machines out of a machine list v, for example, with function 
#sample(v,round(0.7*length(v),0),replace=F). Use help(sample) for more information

#P3################################################################################################################################################################

#a.[1 point] What’s the AUC, a measure of quality, of the model you have built on the train set? and on test set?

#-------------------------------------------------------------------------------------------------------------------------------
#b. [1 point] Plot the function of probability of failure with respect to delta to gain intuition:
#Hint: You can plot a function easily with function curve i.e. curve(x^2+x,c(0,20) if the function was x^2+x. Remember the form of the logistic regression function is prob = 1/(1+exp(-(intercept + coefficient*delta))).


#P4################################################################################################################################################################

#c. [1 point] Let us create alarms with two levels of priority: med-risk and high-risk. Med-risk alarms will fire when the probability of failure is >=60% and High-risk when that probability is >=80%.
  # i.What are the threshold deltas for each type of alarm to fire?  #Hint (check the case)
  # ii. How many of these alarms would be fired per day on average according to your model?
    #Hint: Divide the number of alarms (transactions where the delta exceeds the threshold delta for each type of alarm) by the total number of days
  #iii. What % of these will be “false alarms” i.e. failure variable is equal to 0, for each level of priority?


#P5################################################################################################################################################################

#d.[2 points] In this exercise we will estimate the profit impact of our EWS system vs the current system:
  # Assumptions in case!! 
  #i. If we set the EWS only with the med-risk alarms, what is the annual profit we will generate vs the current system as a % of the total profit? [For simplicity, consider the total profit to be the margin per item times the number of items in the period]
  #ii. And if we set the EWS only with the high-risk alarms?
    

#more HINTS IN CASE! (at the bottom)
