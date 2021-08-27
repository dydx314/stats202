data2 <- read.csv("train_data_open_transformed.csv")

data <- read.csv("train_data_open_transformed.csv")

data[is.na(data)] = -1

# ditch first 9 days
data <- data[data$day >= 9,]


shift <- function(x, n){
  c(tail(x, -n), rep(NA, n))
}

get.data.sub <- function(sym, days.ahead)
{
  data.sub = data[data["symbol"]==sym, 4:30]
  data.sub$open_next <- shift(data.sub$open, 5040*days.ahead)
  return(data.sub)
}

evaluate <- function(y, ypred) {
  return(mean((y - ypred)^2))
}


library(ranger)
library(randomForest)
library(ISLR)
library(caret)
library(ggplot2)


## hyperparam search for nt

find.best.nt <- function(data.sub)
{
  cv.errs.nt = rep(0, 20)
  for (nt in seq(5, 100, 5))
  {
    cv.errs = rep(0, 5)
    for (i in c(1:5)) {
      offset = 1
      train <- 1:((i+offset)*5040)
      test <- ((i+offset)*5040+1):((i+offset+1)*5040)
      data.train <- data.sub[train,]
      data.test <- data.sub[test,]
      rf = ranger(open_next ~ ., data = data.train, num.trees = nt)
      rf.pred = predict(rf, data = data.test, type = "response")
      cv.errs[i] = evaluate(data.test$open_next, rf.pred$predictions)
    }
    cv.errs.nt[nt/5] = mean(cv.errs)
  }
  return(cv.errs.nt)
}

cv.errs.nt.c = find.best.nt(get.data.sub("C", 1))
cv.errs.nt.e = find.best.nt(get.data.sub("E", 1))

plot(seq(5, 100, 5), cv.errs.nt.c, xlab="Number of trees", ylab="CV MSE for ticker C 1-day-ahead")
plot(seq(5, 100, 5), cv.errs.nt.e, xlab="Number of trees", ylab="CV MSE for ticker E 1-day-ahead")


## hyperparam search for m

find.best.m <- function(data.sub)
{
  cv.errs.m = rep(0, 5)
  for (mi in c(1:10))
  {
    m = mi + 2
    cv.errs = rep(0, 5)
    for (i in c(1:5)) {
      train <- 1:((i+1)*5040)
      test <- ((i+1)*5040+1):((i+2)*5040)
      data.train <- data.sub[train,]
      data.test <- data.sub[test,]
      rf = ranger(open_next ~ ., data = data.train, num.trees = 50, mtry = m)
      rf.pred = predict(rf, data = data.test, type = "response")
      cv.errs[i] = evaluate(data.test$open_next, rf.pred$predictions)
    }
    cv.errs.m[mi] = mean(cv.errs)
  }
  return(cv.errs.m)
}

cv.errs.m.c = find.best.m(get.data.sub("C", 1))
cv.errs.m.e = find.best.m(get.data.sub("E", 1))

plot(c(3:12), cv.errs.m.c, xlab="Number of features considered at split (m)", ylab="CV MSE for ticker C 1-day-ahead")
plot(c(3:12), cv.errs.m.e, xlab="Number of features considered at split (m)", ylab="CV MSE for ticker E 1-day-ahead")


## cross-validation on just A, one day ahead
data.sub = get.data.sub("A", 1)

rf.cv.errs = rep(0, 10)
lm.cv.errs = rep(0, 10)
baseline.cv.errs = rep(0, 10)

for (i in c(1:10)) {
  offset = 30
  train <- 1:((i+offset)*5040)
  test <- ((i+offset)*5040+1):((i+offset+1)*5040)
  data.train <- data.sub[train,]
  data.test <- data.sub[test,]
  rf = ranger(open_next ~ ., data = data.train, num.trees = 50)
  rf.pred = predict(rf, data = data.test, type = "response")
  
  lm <- lm(open_next ~ ., data = data.train)
  lm.pred <- predict(lm, data = data.test)
  
  rf.cv.errs[i] = evaluate(data.test$open_next, rf.pred$predictions)
  lm.cv.errs[i] = evaluate(data.test$open_next, lm.pred)
  baseline.cv.errs[i] = evaluate(data.test$open_next, rep(data.train$open[length(data.train$open)], length(test)))
}
mean(rf.cv.errs)
mean(lm.cv.errs)
mean(baseline.cv.errs)


## feature importance on just A, full training data, one day ahead

data.train <- data.sub[1:(dim(data.sub)[1]-5040*9),]
data.test <- data.sub[(dim(data.sub)[1]-5040*9+1):(dim(data.sub)[1]),]
rf = ranger(open_next ~ ., data = data.train, num.trees = 50, importance = 'impurity')

varimp.df = as.data.frame(rf$variable.importance)
varimp.df = cbind(variable = rownames(varimp.df), varimp.df)
colnames(varimp.df)[2] = 'importance'

ggplot(varimp.df, aes(x=reorder(variable,importance), y=importance,fill=importance))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Information Value Summary")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")


# cross validation on A, all 9 models
rf.cv.errs.day = rep(0, 9)
lm.cv.errs.day = rep(0, 9)
baseline.cv.errs.day = rep(0, 9)
for (d in c(1:9)) {
  data.sub = get.data.sub("A", d)

  rf.cv.errs = rep(0, 10)
  lm.cv.errs = rep(0, 10)
  baseline.cv.errs = rep(0, 10)
  
  for (i in c(1:10)) {
    offset = 30
    train <- 1:((i+offset)*5040)
    test <- ((i+offset)*5040+1):((i+offset+1)*5040)
    data.train <- data.sub[train,]
    data.test <- data.sub[test,]
    rf = ranger(open_next ~ ., data = data.train)
    rf.pred = predict(rf, data = data.test, type = "response")
    
    lm <- lm(open_next ~ ., data = data.train)
    lm.pred <- predict(lm, data = data.test)
    
    rf.cv.errs[i] = evaluate(data.test$open_next, rf.pred$predictions)
    lm.cv.errs[i] = evaluate(data.test$open_next, lm.pred)
    baseline.cv.errs[i] = evaluate(data.test$open_next, rep(data.train$open[length(data.train$open)], length(test)))
  }
  rf.cv.errs.day[d] = mean(rf.cv.errs)
  lm.cv.errs.day[d] = mean(lm.cv.errs)
  baseline.cv.errs.day[d] = mean(baseline.cv.errs)
}

rf.cv.errs.day
lm.cv.errs.day
baseline.cv.errs.day

symbols = 'ABCDEFGHIJ'

# actual test performance on all 10 tickers, all 9 models
rf.test.perf.df = data.frame()
baseline.test.perf.df = data.frame()

syms = strsplit(symbols, "")[[1]]

for (sym in syms) {
  for (d in c(1:9)) {
    data.sub = get.data.sub(sym, d)
    # filter out last 9 days as they have NA response
    data.sub = data.sub[1:(dim(data.sub)[1]-5040*9),]
    
    data.train <- data.sub[1:(dim(data.sub)[1]-5040*9),]
    data.test <- data.sub[(dim(data.sub)[1]-5040*9+1):(dim(data.sub)[1]),]
    rf = ranger(open_next ~ ., data = data.train, num.trees = 50)
    rf.pred = predict(rf, data = data.test, type = "response")
    rf.test.perf.df[d,sym] = evaluate(data.test$open_next, rf.pred$predictions)
    
    baseline.pred = rep(data.train$open[length(data.train$open)], length(data.test$open_next))
    baseline.test.perf.df[d,sym] = evaluate(data.test$open_next, baseline.pred)
  }
}
rf.test.perf.df
baseline.test.perf.df



