#Removing all the workspace content 
rm(list=ls())
#Getting and setting the path
getwd()
setwd("C:/Users/Deepa Srinivas/Desktop/Courses/IndiviualProject/Bike-Sharing-Dataset")
getwd()

#reading csv file
data=read.csv("hour.csv")
#Structure of the data
str(data)
#checking if the there are any missing values
table(is.na(data))
#par(mfrow=c(4,2))
#par(mar = rep(2, 4))
#Exploring the data to understand the distribution
hist(data$season)
hist(data$weather)
hist(data$hum)
hist(data$holiday)
hist(data$workingday)
hist(data$temp)
hist(data$atemp)
hist(data$windspeed)

prop.table(table(data$weather))

#converting the values to factors
data$season=as.factor(data$season)
data$weather=as.factor(data$weather)
data$holiday=as.factor(data$holiday)
data$workingday=as.factor(data$workingday)

#Dividing the dataset as train and test.First 20days of the month are considered as train data and remaining days are the test data
train=data[as.integer(substr(data$dteday,9,10))<20,]
test=data[as.integer(substr(data$dteday,9,10))>19,]
str(data)

#par(mfrow=c(1,1))
#To see on what hours there is high demand for bikes
boxplot(train$cnt~train$hr,xlab="hour", ylab="count of users")

#To see the hourly distribution among Casual Users and Registered Users
boxplot(train$casual~train$hr,xlab="hour",ylab="Casual users")
boxplot(train$registered~train$hr,xlab="hour",ylab="Registered users")
boxplot(log(train$cnt)~train$hr,xlab="hour",ylab="log(count)")

date=substr(data$dteday,1,10)

#Extracting the days of the week information from the data
days<-weekdays(as.Date(date))
data$day=days
data$day
boxplot(data$registered~data$day,xlab="Days",ylab="Registered users")
boxplot(data$casual~data$day,xlab="Days",ylab="Casual Users")
#need to plot graphs for days of the week with registered and casual,temp,rain,hum

#train <- data[as.integer(substr(data$dteday,9,10))<20,]
#test <- data[as.integer(substr(data$dteday,9,10))>19,]

#correlation between temp ,windspeed,hum
sub=data.frame(train$registered,train$casual,train$cnt,train$temp,train$hum,train$atemp,train$windspeed)
cor(sub)
#time
data$yr=substr(data$dteday,1,4)
data$yr=as.factor(data$yr)
data$yr
train=data[as.integer(substr(data$dteday,9,10))<20,]
test=data[as.integer(substr(data$dteday,9,10))>19,]
boxplot(train$cnt~train$yr,xlab="year", ylab="count")

#convert hr to integer
train$hr=as.integer(train$hr)
test$hr=as.integer(test$hr)
#loading the libraries required for random forest model
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
d=rpart(registered~hr,data=train)
rpart.plot(d)

#hour buckets for registerd users
data <- rbind(train,test)
data$dp_reg=0
data$dp_reg[data$hr<8]=1
data$dp_reg[data$hr>=22]=2
data$dp_reg[data$hr>9 & data$hr<18]=3
data$dp_reg[data$hr==8]=4
data$dp_reg[data$hr==9]=5
data$dp_reg[data$hr==20 | data$hr==21]=6
data$dp_reg[data$hr==19 | data$hr==18]=7
table(data$dp_reg)

#hour bucket for casual users
data$dp_cas = 0 
data$dp_cas[data$hr<8]=1
data$dp_cas[data$hr>=22]=2
data$dp_cas[data$hr>9 & data$hr<18]=3
data$dp_cas[data$hr==8]=4
data$dp_cas[data$hr==9]=5
data$dp_cas[data$hr==20 | data$hr==21]=6
data$dp_cas[data$hr==19 | data$hr==18]=7
table(data$dp_cas)

# creating bins for temperature for both registered and casuals users
data$temp_reg = 0 
data$temp_reg[data$hr<8]=1
data$temp_reg[data$hr>=22]=2
data$temp_reg[data$hr>9 & data$hr<18]=3
data$temp_reg[data$hr==8]=4
data$temp_reg[data$hr==9]=5
data$temp_reg[data$hr==20 | data$hr==21]=6
data$temp_reg[data$hr==19 | data$hr==18]=7
table(data$temp_reg)

data$temp_cas = 0
data$temp_cas[data$hr<8]=1
data$temp_cas[data$hr>=22]=2
data$temp_cas[data$hr>9 & data$hr<18]=3
data$temp_cas[data$hr==8]=4
data$temp_cas[data$hr==9]=5
data$temp_cas[data$hr==20 | data$hr==21]=6
data$temp_cas[data$hr==19 | data$hr==18]=7
table(data$temp_cas)

#Creating bins
data$year_part[data$yr=='2011']=1
data$year_part[data$yr=='2011' & data$mnth>3]=2
data$year_part[data$yr=='2011' & data$mnth>6]=3
data$year_part[data$yr=='2011' & data$mnth>9]=4
data$year_part[data$yr=='2012']=5
data$year_part[data$yr=='2012' & data$mnth>3]=6
data$year_part[data$yr=='2012' & data$mnth>6]=7
data$year_part[data$yr=='2012' & data$mnth>9]=8
table(data$year_part)

#creating a variable for weehday weekend and holiday
data$day_type <-""
data$day_type[data$holiday==0 & data$workingday==0] <-"weekend"
data$day_type[data$holiday==1]="holiday"
data$day_type[data$holiday==0 & data$workingday==1]="working day"
table(data$day_type)


#creating variable for weekend
data$weekend=0
data$weekend[data$day=="Sunday" | data$day=="Saturday" ]=1
table(data$weekend)

#building the model
#data$season <- as.factor(data$season)
#data$holiday <- as.factor(data$holiday)
#data$workingday=as.factor(data$workingday)
#data$weather=as.factor(data$weather)
#data$hour=as.factor(data$hr)
#data$day_part=as.factor(data$dp_cas)
#data$day_type=as.factor(data$dp_reg)
#data$days=as.factor(data$days)
#data$temp_cas=as.factor(data$temp_cas)
#data$temp_reg=as.factor(data$temp_reg)

#after creation of bins we need to divide the data 
train <- data[as.integer(substr(data$dteday,9,10))<20,]
test <- data[as.integer(substr(data$dteday,9,10))>19,]
names(train)

#conversion of independent variables into factors
train$hr = as.factor(train$hr)
test$hr = as.factor(test$hr)

train$workingday = as.factor(train$workingday)
test$workingday = as.factor(test$workingday)

train$weather = as.factor(train$weather)
test$weather = as.factor(test$weather)

train$season = as.factor(train$season)
test$season = as.factor(test$season)

train$holiday = as.factor(train$holiday)
test$holiday = as.factor(test$holiday)

train$mnth = as.factor(train$mnth)
test$mnth = as.factor(test$mnth)

train$day= as.factor(train$day)
test$day = as.factor(test$day)

train$day_type = as.factor(train$day_type)
test$day_type = as.factor(test$day_type)

str(train)

#Adding one to deal with zeros values present in the casual and registered users
train$reg1=train$registered+1
train$cas1=train$casual+1
train$logcas=log(train$cas1)
train$logreg=log(train$reg1)
test$logreg=0
test$logcas=0

library(randomForest)
names(train)
names(test)

#checking for any missing values
table(is.na(train))
table(is.na(test))
is.na(data$day)
is.nan(data$day)
is.finite(data$day)
str(train)

#Training the model
set.seed(415)
fit1 <- randomForest(logreg ~ hr +workingday+day+holiday+ day_type +temp_reg+hum+atemp+windspeed+season+weather+dp_reg+weekend+yr+year_part, data=train,importance=TRUE, ntree=250)
fit1
#predicting the demand considering last ten days of the month
pred1 <- predict(fit1,test)
test$logreg=pred1


# Predicting the log for casual users
set.seed(415)
fit2 <- randomForest(logcas ~hr + day_type+day+hum+atemp+temp_cas+windspeed+season+weather+holiday+workingday+dp_cas+weekend+yr+year_part, data=train,importance=TRUE, ntree=250)
fit2
pred2 <- predict(fit2,test)
test$logcas <- pred2

# Re-transforming the predicted variables and then writing the output of count to the file result.csv
test$registered <- exp(test$logreg) - 1
test$casual <- exp(test$logcas) - 1
test$count <- test$registered + test$casual

s <- data.frame(dteday = test$dteday, cnt = test$cnt)
write.csv(s, file = "result.csv", row.names = FALSE)
head(data)
