# Some historical data for a product
text <- "June-12  	425
July-12		425
August-12		418.7614
September-12		347.8309
October-12		356.1947
November-12		390.12
December-12		381.4749
January-13		379.3391
February-13		360.3756
March-13		306.9137
April-13		253.4519
May-13		199.99
June-13		232.2142
July-13		264.4383
August-13		296.6625
September-13		266.7646
October-13		228.6928
November-13		194.5333
December-13		188.9278
January-14		173.5415
February-14		187.6606
March-14		186.1225
April-14		177.9256
May-14		175.5771
June-14		169.5018
July-14		152.5103
August-14		163.2633
September-14		146.5872
October-14		134.8909
November-14		115.4235
December-14		114.2371"

data <- read.table(text=text)
names(data) <- c("date", "price")
data$month <- as.numeric(row.names(data))

#############################################################################
# prediction_length: how far away you want to predict
# history_length: how many months of history you want to use
# For example: You have 24 months of data
# <prediction_length=4>: you are trying to predict the pricing 4 month later
# then the last four records will be assume unknown, then the data usable will
# be from month 1 to 20, and your goal is to predict the 24th record
# <history_length=8>: you are going to use the latest 8 months data
# from the data usable, i.e month 13, 14, ... , 20. 
##############################################################################
myfunction <- function(prediction_length, history_length, mydata){
  data.train <- head(mydata, nrow(mydata) - prediction_length)
  data.train.used <- tail(data.train, history_length)
  # build an exponential model where
  # price=exp(const_a * month + const_b)
  m <- lm(data=data.train.used, log(price) ~ month)
  #print(row.names(data.train.used))
  prediction_bin <- function(x) {exp(m$coefficients[1] + x * m$coefficients[2])}
  prediction <- prediction_bin( nrow(data) )
  actual <- tail(data.train, 1)$price
  # negative error rate means under predict the pricing 
  error <- (prediction - actual) / actual
  return(error)
}


N <- nrow(mydata)
# generate all the possible combinations between prediction_length and history_length
test.data <- data.frame(prediction_length=rep(1:N, each=N), history_length=rep(1:N, times=N))
test.data <- subset(test.data, (prediction_length+history_length)  <= N)

library(ggplot2)
library(scales)

result <- data.frame(prediction_length=c(), history_length=c(), error_rate=c())
for(my_prediction_length in 1:(N-1)){
  for(my_history_length in 1:(N - my_prediction_length)){
    myresult <- myfunction(mydata=data, prediction_length=my_prediction_length, history_length=my_history_length)
    result <-rbind(result, c(my_prediction_length, my_history_length, myresult))      
  }
}

names(result) <- c("prediction_length","history_length","error_rate")

ggplot(data=subset(result, prediction_length %in% c(1, 3, 6, 9, 12, 15, 18, 21)), aes(x=history_length, y=error_rate, color=as.factor(prediction_length))) + 
  geom_point() + geom_line() + scale_y_continuous(limits=c(-1, 1), labels=percent)

