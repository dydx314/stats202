# Load the data
data <- read.csv("train_data_open_transformed.csv")
library(ISLR)

# Define the shift function to move the open price to an earlier day.
shift <- function(x, n){
  c(tail(x, -n), rep(NA, n))
}

# 9 models (exclude data from the first 9 days for NAs in predictors)
data.sub <- data[data["symbol"]=="A", 4:30]
data.sub$open <- shift(data.sub$open, 5040)
train <- (5040 * 9 + 1):(5040 * 78 - 5040)
test <- (5040 * 78 - 5040 + 1):(5040 * 78)
pred <- (5040 * 86 + 1):(5040 * 87)
data.train <- data.sub[train,]
data.test <- data.sub[test,]
data.pred <- data.sub[pred,]
lm1 <- lm(open ~ ., data = data.train)
test1 <- predict(lm1, data.test)
pred1 <- predict(lm1, data.pred)

data.sub <- data[data["symbol"]=="A", 4:30]
data.sub$open <- shift(data.sub$open, 5040 * 2)
train <- (5040 * 9 + 1):(5040 * 78 - 5040 * 2)
test <- (5040 * 78 - 5040 * 2 + 1):(5040 * 78)
pred <- (5040 * 85 + 1):(5040 * 87)
data.train <- data.sub[train,]
data.test <- data.sub[test,]
data.pred <- data.sub[pred,]
lm2 <- lm(open ~ ., data = data.train)
test2 <- predict(lm2, data.test)
pred2 <- predict(lm2, data.pred)

data.sub <- data[data["symbol"]=="A", 4:30]
data.sub$open <- shift(data.sub$open, 5040 * 3)
train <- (5040 * 9 + 1):(5040 * 78 - 5040 * 3)
test <- (5040 * 78 - 5040 * 3 + 1):(5040 * 78)
pred <- (5040 * 84 + 1):(5040 * 87)
data.train <- data.sub[train,]
data.test <- data.sub[test,]
data.pred <- data.sub[pred,]
lm3 <- lm(open ~ ., data = data.train)
test3 <- predict(lm3, data.test)
pred3 <- predict(lm3, data.pred)

data.sub <- data[data["symbol"]=="A", 4:30]
data.sub$open <- shift(data.sub$open, 5040 * 4)
train <- (5040 * 9 + 1):(5040 * 78 - 5040 * 4)
test <- (5040 * 78 - 5040 * 4 + 1):(5040 * 78)
pred <- (5040 * 83 + 1):(5040 * 87)
data.train <- data.sub[train,]
data.test <- data.sub[test,]
data.pred <- data.sub[pred,]
lm4 <- lm(open ~ ., data = data.train)
test4 <- predict(lm4, data.test)
pred4 <- predict(lm4, data.pred)

data.sub <- data[data["symbol"]=="A", 4:30]
data.sub$open <- shift(data.sub$open, 5040 * 5)
train <- (5040 * 9 + 1):(5040 * 78 - 5040 * 5)
test <- (5040 * 78 - 5040 * 5 + 1):(5040 * 78)
pred <- (5040 * 82 + 1):(5040 * 87)
data.train <- data.sub[train,]
data.test <- data.sub[test,]
data.pred <- data.sub[pred,]
lm5 <- lm(open ~ ., data = data.train)
test5 <- predict(lm5, data.test)
pred5 <- predict(lm5, data.pred)

data.sub <- data[data["symbol"]=="A", 4:30]
data.sub$open <- shift(data.sub$open, 5040 * 6)
train <- (5040 * 9 + 1):(5040 * 78 - 5040 * 6)
test <- (5040 * 78 - 5040 * 6 + 1):(5040 * 78)
pred <- (5040 * 81 + 1):(5040 * 87)
data.train <- data.sub[train,]
data.test <- data.sub[test,]
data.pred <- data.sub[pred,]
lm6 <- lm(open ~ ., data = data.train)
test6 <- predict(lm6, data.test)
pred6 <- predict(lm6, data.pred)

data.sub <- data[data["symbol"]=="A", 4:30]
data.sub$open <- shift(data.sub$open, 5040 * 7)
train <- (5040 * 9 + 1):(5040 * 78 - 5040 * 7)
test <- (5040 * 78 - 5040 * 7 + 1):(5040 * 78)
pred <- (5040 * 80 + 1):(5040 * 87)
data.train <- data.sub[train,]
data.test <- data.sub[test,]
data.pred <- data.sub[pred,]
lm7 <- lm(open ~ ., data = data.train)
test7 <- predict(lm7, data.test)
pred7 <- predict(lm7, data.pred)

data.sub <- data[data["symbol"]=="A", 4:30]
data.sub$open <- shift(data.sub$open, 5040 * 8)
train <- (5040 * 9 + 1):(5040 * 78 - 5040 * 8)
test <- (5040 * 78 - 5040 * 8 + 1):(5040 * 78)
pred <- (5040 * 79 + 1):(5040 * 87)
data.train <- data.sub[train,]
data.test <- data.sub[test,]
data.pred <- data.sub[pred,]
lm8 <- lm(open ~ ., data = data.train)
test8 <- predict(lm8, data.test)
pred8 <- predict(lm8, data.pred)

data.sub <- data[data["symbol"]=="A", 4:30]
data.sub$open <- shift(data.sub$open, 5040 * 9)
train <- (5040 * 9 + 1):(5040 * 78 - 5040 * 9)
test <- (5040 * 78 - 5040 * 9 + 1):(5040 * 78)
pred <- (5040 * 78 + 1):(5040 * 87)
data.train <- data.sub[train,]
data.test <- data.sub[test,]
data.pred <- data.sub[pred,]
lm9 <- lm(open ~ ., data = data.train)
test9 <- predict(lm9, data.test)
pred9 <- predict(lm9, data.pred)

# Test the model:
A.day78 <- 9/45*test1 + 8/45*test2[1:5040] + 7/45*test3[1:5040] + 6/45*test4[1:5040] + 5/45*test5[1:5040] + 4/45*test6[1:5040] + 3/45*test7[1:5040] + 2/45*test8[1:5040] + 1/45*test9[1:5040]
A.day79 <- 8/36*test2[5041:10080] + 7/36*test3[5041:10080] + 6/36*test4[5041:10080] + 5/36*test5[5041:10080] + 4/36*test6[5041:10080] + 3/36*test7[5041:10080] + 2/36*test8[5041:10080] + 1/36*test9[5041:10080]
A.day80 <- 7/28*test3[10081:15120] + 6/28*test4[10081:15120] + 5/28*test5[10081:15120] + 4/28*test6[10081:15120] + 3/28*test7[10081:15120] + 2/28*test8[10081:15120] + 1/28*test9[10081:15120]
A.day81 <- 6/21*test4[15121:20160] + 5/21*test5[15121:20160] + 4/21*test6[15121:20160] + 3/21*test7[15121:20160] + 2/21*test8[15121:20160] + 1/21*test9[15121:20160]
A.day82 <- 5/15*test5[20161:25200] + 4/15*test6[20161:25200] + 3/15*test7[20161:25200] + 2/15*test8[20161:25200] + 1/15*test9[20161:25200]
A.day83 <- 4/10*test6[25201:30240] + 3/10*test7[25201:30240] + 2/10*test8[25201:30240] + 1/10*test9[25201:30240]
A.day84 <- 3/6*test7[30241:35280] + 2/6*test8[30241:35280] + 1/6*test9[30241:35280]
A.day85 <- 2/3*test8[35281:40320] + 1/3*test9[35281:40320]
A.day86 <- test9[40321:45360]

A.test <- c(A.day78, A.day79, A.day80, A.day81, A.day82, A.day83, A.day84, A.day85, A.day86)

plot(A.test, main = "A test")

A.data.test <- data[data["symbol"]=="A",][(5040*78+1):(5040*87),4]
A.testMSE <- mean((A.data.test - A.test) ^ 2)
A.testMSE

# Forecast for the next 9 days:
A.day87 <- 9/45*pred1 + 8/45*pred2[1:5040] + 7/45*pred3[1:5040] + 6/45*pred4[1:5040] + 5/45*pred5[1:5040] + 4/45*pred6[1:5040] + 3/45*pred7[1:5040] + 2/45*pred8[1:5040] + 1/45*pred9[1:5040]
A.day88 <- 8/36*pred2[5041:10080] + 7/36*pred3[5041:10080] + 6/36*pred4[5041:10080] + 5/36*pred5[5041:10080] + 4/36*pred6[5041:10080] + 3/36*pred7[5041:10080] + 2/36*pred8[5041:10080] + 1/36*pred9[5041:10080]
A.day89 <- 7/28*pred3[10081:15120] + 6/28*pred4[10081:15120] + 5/28*pred5[10081:15120] + 4/28*pred6[10081:15120] + 3/28*pred7[10081:15120] + 2/28*pred8[10081:15120] + 1/28*pred9[10081:15120]
A.day90 <- 6/21*pred4[15121:20160] + 5/21*pred5[15121:20160] + 4/21*pred6[15121:20160] + 3/21*pred7[15121:20160] + 2/21*pred8[15121:20160] + 1/21*pred9[15121:20160]
A.day91 <- 5/15*pred5[20161:25200] + 4/15*pred6[20161:25200] + 3/15*pred7[20161:25200] + 2/15*pred8[20161:25200] + 1/15*pred9[20161:25200]
A.day92 <- 4/10*pred6[25201:30240] + 3/10*pred7[25201:30240] + 2/10*pred8[25201:30240] + 1/10*pred9[25201:30240]
A.day93 <- 3/6*pred7[30241:35280] + 2/6*pred8[30241:35280] + 1/6*pred9[30241:35280]
A.day94 <- 2/3*pred8[35281:40320] + 1/3*pred9[35281:40320]
A.day95 <- pred9[40321:45360]

A.pred <- c(A.day87, A.day88, A.day89, A.day90, A.day91, A.day92, A.day93, A.day94, A.day95)
plot(A.pred, main = "A pred")