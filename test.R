data <- read.csv("train_data_open_transformed.csv")

data[is.na(data)] = -1

## ditch first 9 days
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

### START RUNNING HERE ###

evaluate <- function(y, ypred) {
  return(mean((y - ypred)^2))
}

library(ranger)
library(ISLR)
library(caret)

symbols = 'EFGHIJ' # TODO: CHANGE

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
