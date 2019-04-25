library("anytime")  #for date and time parsing
library("forecast") #for forecasting from time series
library("ggplot2") #for data visualisation
library("xts") #for time series manipulation

#importing training and testing datasets
train <- read.csv("E:/project/new one/train.csv")
test <- read.csv("E:/project/new one/test.csv")

#displaying the simplified dataset
head(train)
testdata <- test[,5]

#assigning variables using anytime library 
train$Date <- as.Date(anytime(train$Date))
test$Date <- as.Date(anytime(test$Date))
train$Volume <- gsub(",", "", train$Volume) #removing commas to make dataset look better
train$Market.Cap <- gsub(",", "", train$Market.Cap)
train$Market.Cap <- as.numeric(train$Market.Cap)
train$Volume <- as.numeric(train$Volume)
head(train)
#adding new column as the difference between high and low features to fill the
#null and blank values in volume column
a <- matrix(c(0), nrow = 0, ncol = 1)
for(i in 1:nrow(train)){
  a <- rbind(a, train[i,3] - train[i,4])
  i <- i + 1
}
train<-cbind(train,a)

#determining certain mean values for null datas
fifty_x <- round(mean(train$Volume[train$a < 50], na.rm = TRUE), digits = 2)
hundred_x<- round(mean(train$Volume[train$a > 50 & train$a < 100], na.rm = TRUE), digits = 2)
hunfif_x <- round(mean(train$Volume[train$a > 100 & train$a < 150], na.rm = TRUE), digits = 2)
threefif_x <- round(mean(train$Volume[train$a > 150 & train$a < 350], na.rm = TRUE), digits = 2)

#using for loop, we are assigning values according to the 
#rows in which the specified blank value is placed
for(i in 1:nrow(train)){
  if(is.na(train[i,6])){
    if(train$a[i] < 50){
      train$Volume[i] <- fifty_x
    } else if(train$a[i] < 100){
      train$Volume[i] <- hundred_x
    } else if(train$a[i] < 150){
      train$Volume[i] <- hunfif_x
    } else if(train$a[i] < 350){
      train$Volume[i] <- threefif_x
    }else
      print("dataset has an error")
  }
}
train <- train[, - 8] #removing the newly added column
head(train)

#using ggplot, we plot the closing price of the training dataset
ggplot(train, aes(Date, Close)) + geom_line() + scale_x_date("year") + ylim(0,10000) + ylab("Closing Price")

#the data is presented over a period
Train <- xts(train[, -1], order.by = as.POSIXct(train$Date))
plot(Train$Close,type='l',lwd = 1.5,col='red', ylim = c(0,10000), main = "Bitcoin Closing Price")

#the data is converted to time series 
tsr <- ts(Train[,4], frequency = 365.25,start = c(2013,4,27))

#checking for trends and seasonality
dects <- decompose(tsr)
plot(dects)

# holt's model for forecasting
holtt <-  holt(Train[1:1403,'Close'], type = "additive", damped = F) 
holtf <- forecast(holtt, h = 10)
holtdf <- as.data.frame(holtf)
holtdf

#plot for the predicted values of price with time
plot(holtf, ylim = c(0,10000)) 
holtfdf <- cbind(test, holtdf[,1])
holtfdf
accuracy(holtfdf[,8],testdata)


#testing the test data for the given model
ggplot() + geom_line(data = holtfdf, aes(Date, holtfdf[,2]), color = "blue") + geom_line(data = holtfdf, aes(Date, holtfdf[,3]), color = "Dark Red")



#changing the datasets
#importing training and testing datasets
train1 <- read.csv("E:/project/new one/training.csv")
test1 <- read.csv("E:/project/new one/testing.csv")
head(train1)
testdata1 <- test1[,2]

#Converting data for analysis
train1$Date <- as.Date(anytime(train1$Date))
test1$Date <- as.Date(anytime(test1$Date))
train1$Volume <- gsub(",", "", train1$Volume)
train1$Market.Cap <- gsub(",", "", train1$Market.Cap)
train1$Market.Cap <- as.numeric(train1$Market.Cap)
train1$Volume <- as.numeric(train1$Volume)
a1 <- matrix(c(0), nrow = 0, ncol = 1)
for(i in 1:nrow(train1)){
  a1 <- rbind(a1, train1[i,3] - train1[i,4])
  i <- i + 1
}
train1 <- cbind(train1,a1)


#Volume has missing values#
#Data Manipulation#
fifty_avg <- round(mean(train1$Volume[train1$a1 < 50], na.rm = TRUE), digits = 2)
hun_avg <- round(mean(train1$Volume[train1$a > 50 & train1$a < 100], na.rm = TRUE), digits = 2)
hf_avg <- round(mean(train1$Volume[train1$a > 100 & train1$a < 150], na.rm = TRUE), digits = 2)
th_avg <- round(mean(train1$Volume[train1$a > 150 & train1$a < 350], na.rm = TRUE), digits = 2)
for(i in 1:nrow(train1)){
  if(is.na(train[i,6])){
    if(train1$a1[i] < 50){
      train1$Volume[i] <- fifty_avg
    } else if(train1$a1[i] < 100){
      train1$Volume[i] <- hun_avg
    } else if(train1$a[i] < 150){
      train1$Volume[i] <- hf_avg
    } else if(train1$a[i] < 350){
      train1$Volume[i] <- th_avg
    }else
      print("Uncaught Title")
  }
}
train1 <- train1[, - 8] #Removing column 8

ggplot(train1, aes(Date, Close)) + geom_line() + scale_x_date("year") + ylim(0,10000) + ylab("Closing Price")
Train1 <- xts(train1[, -1], order.by = as.POSIXct(train1$Date)) 
tsr1 <- ts(Train1[,4], frequency = 365.25,start = c(2013,4,27))
plot(Train1$Close,type='l',lwd = 1.5,col='red', ylim = c(0,10000), main = "Bitcoin Closing Price")

#checking for trends and seasonality
dects1 <- decompose(tsr1) #Obtaining the trends and seasonality
plot(dects1)

holtt1 <-  holt(Train1[1:1555,'Close'], type = "additive", damped = F) #holt forecast values
holtf1 <- forecast(holtt1, h = 7)
holtdf1 <- as.data.frame(holtf1)
plot(holtf1, ylim = c(0,10000)) 
holtfdf1 <- cbind(test1, holtdf1[,1])
accuracy(holtdf1[,1], testdata1)
head(holtfdf1)
ggplot() + geom_line(data = holtfdf1, aes(Date, holtfdf1[,2]), color = "blue") + geom_line(data = holtfdf1, aes(Date, holtfdf1[,3]), color = "Dark Red")


#exponential triple smoothening
ETS1 <- ets((Train1[,'Close'])) # ETS forecast values
ETSf1 <- forecast(ETS1, h = 10)
etsdf1 <- as.data.frame(ETSf1)

plot(forecast(ETS1, h = 10), ylim = c(0,10000)) #ETS forecast plot works perfectly
etsp1 <- predict(ETS1, n.ahead = 10, prediction.interval = T, level = 0.95)
accuracy(etsdf1[,1], testdata1)
