data <- read.csv("train_data.csv")
head(data)
dim(data)
summary(data)


## convert time from string to time
library(chron)
data$time <- chron(times=data$time)
# add columns for hour and minutes
data["hour"]<-hours(data$time)
data["minute"]<-minutes(data$time)

boxplot(data[data["hour"]==6,]$open,
data[data["hour"]==7,]$open,
data[data["hour"]==8,]$open,
data[data["hour"]==9,]$open,
data[data["hour"]==10,]$open,
data[data["hour"]==11,]$open,
data[data["hour"]==12,]$open
)

data["dayOfWeek"]<-(data$day)%%5
boxplot(data[data["dayOfWeek"]==0,]$open,
data[data["dayOfWeek"]==1,]$open,
data[data["dayOfWeek"]==2,]$open,
data[data["dayOfWeek"]==3,]$open,
data[data["dayOfWeek"]==4,]$open
)

A.data<-data[data["symbol"]=="A",]
A.data.ts<-ts(A.data["close"], start=c(5040,1), frequency=87)
library(fpp2)
autoplot(A.data.ts)

B.data<-data[data["symbol"]=="B",]
B.data.ts<-ts(B.data["close"], start=c(5040,1), frequency=87)
autoplot(B.data.ts)

C.data<-data[data["symbol"]=="C",]
C.data.ts<-ts(C.data["close"], start=c(5040,1), frequency=87)
autoplot(C.data.ts)


D.data<-data[data["symbol"]=="D",]
D.data.ts<-ts(D.data["close"], start=c(5040,1), frequency=87)
autoplot(D.data.ts)

E.data<-data[data["symbol"]=="E",]
E.data.ts<-ts(E.data["close"], start=c(5040,1), frequency=87)
autoplot(E.data.ts)


F.data<-data[data["symbol"]=="F",]
F.data.ts<-ts(F.data["close"], start=c(5040,1), frequency=87)
autoplot(F.data.ts)


G.data<-data[data["symbol"]=="G",]
G.data.ts<-ts(G.data["close"], start=c(5040,1), frequency=87)
autoplot(G.data.ts)

H.data<-data[data["symbol"]=="H",]
H.data.ts<-ts(H.data["close"], start=c(5040,1), frequency=87)
autoplot(H.data.ts)

I.data<-data[data["symbol"]=="I",]
I.data.ts<-ts(I.data["close"], start=c(5040,1), frequency=87)
autoplot(I.data.ts)

J.data<-data[data["symbol"]=="J",]
J.data.ts<-ts(J.data["close"], start=c(5040,1), frequency=87)
autoplot(J.data.ts)
