
##########################################################
###########Reading the apple's stock data#################
##########################################################
data = read.csv("AAPL.csv", header = T, stringsAsFactors = F)
data$Date = as.Date(data$Date, format = "%d-%m-%Y")           
str(data)

##########################################################
############### DATA PRE- PROCESSING #####################
##########################################################

##########################################################
######### creating a sequence of date from min to max ####
######### and then takin left join with the original #####
######### so to have holiday's date (with NA values ######
######### and then filling it with previous day close ####
######### values----done so as to have continuous plot ###
##########################################################

#install.packages("dplyr")
library(dplyr)
ts <- seq.POSIXt(as.POSIXct("2008-08-07","%m-%d-%Y"), 
            as.POSIXct("2016-06-30","%m-%d-%Y"), by="day")
df <- data.frame(Date=ts)
df$Date = as.Date(df$Date, format = "%Y-%m-%d")
data1 <- left_join(df,data)
data2 = data1
i = 1
while (i<=2885)
{
  if(is.na(data1[i,5]))
  data1[i,5] = data1[i-1,5]
i = i+1
}
##########################################################
##### Now converting dataframe into Time series object ###
##########################################################
inds <- seq(as.Date("2008-08-07"), as.Date("2016-06-30"), 
            by = "day")
TSdata = ts (data1[c(5)], start = c(2008, as.numeric(format
                  (inds[1], "%j"))) ,frequency = 365)
plot.ts(TSdata)


##########################################################
########## partitioning it to test and train data ########
########## and converting it to TS object ################
##########################################################

###########################
######### TRAIN ###########
###########################
train = subset(data1,Date<='2015-12-31')
inds1 <- seq(as.Date("2008-08-07"), as.Date("2015-12-31"),
             by = "day")
TStrain = ts (train[c(5)], start = c(2008, as.numeric(format
                  (inds1[1], "%j"))),frequency = 365)
plot.ts(TStrain)
summary(TStrain)

###########################
######### TEST ############
###########################
test = subset(data1,Date>='2016-01-01')
str(test)
inds2 <- seq(as.Date("2016-01-01"), as.Date("2016-06-30"),
             by = "day")
TStest = ts (test[c(5)], start = c(2016, as.numeric(format
                    (inds2[1], "%j"))),frequency = 365)
plot.ts(TStest)


##########################################################
########### TIME SERIES ANALYSIS #########################
##########################################################

#install.packages("forecast")
library(forecast)

tp = log(TStrain)
###### tAKING LOG TO MINIMIZE HETEROSKADECITY############
ts.plot(tp)
PP.test(tp)
###### ADF test of stationarity##########################
###### High p-value thus rejecting Ho for homoscadicity##

plot.ts(diff(tp))
PP.test(diff(tp))
###### Rejecting Ho for significance of .05 #############
###### Thus stationarity achieved by differnecing #######


########################################################
####### FITTING AUTO ARIMA MODEL #######################
####### ANALYSING ACF AND PACF OF DIFF- DATA ###########
########################################################
auto.arima(tp, max.p = 10, max.Q = 10, max.d = 5, seasonal = TRUE)
tpcheck = as.data.frame(diff(tp))
acf(tpcheck,lag.max = 10)
pacf(tpcheck, lag.max = 300)
######## ARIMA (1,1,0) WITH TP DATA ####################
#install.packages("matplotlib")


model = Arima(tp,order = c(1,1,0))
ts.plot((residuals(model)))
modelfit = tp - residuals(model)

modelg = exp(modelfit)
ts.plot(modelg)
ts.plot(TStrain)
ts.plot(TSdata, modelg, gpars = list(col = c("black", "red")))
predict1 = predict(model, n.ahead=182)
pre = exp(predict1$pred)
and = as.data.frame(pre)
and$date = test$Date
and$indi = 0
i = 1
while(i<182)
{
  if (and[i+1,1]>and[i,1])
  and[i+1,3] = 1
  i = i+1
}

ts.plot(pre)

ts.plot(TStrain, modelg, gpars = list(col = c("black", "red")))
ts.plot(TStest,pre, log = "y", col = c(3,1), lty = c(5,1))

#########################
#Without Drift
par(mfrow = c(2,1))
auto.arima(tp, max.p = 10, max.Q = 10, max.d = 5, seasonal = TRUE)
fit1 = Arima(TStrain, order = c(1,1,0), 
             include.drift =F, lambda = 0) 
future = forecast(fit1, h = 182)
#future$fitted = exp(future$fitted)
plot(future)
tail(future$fitted)
lines(TStest)

am = as.data.frame(future$mean)
am$date = test$Date
am$indi = 0
i = 1
while(i<182)
{
  if (am[i+1,1]>am[i,1])
  {am[i+1,3] = 1}
  i= i+1
}

#With Drift
auto.arima(tp, max.p = 10, max.Q = 10, max.d = 5, seasonal = TRUE)
fit2 = Arima(TStrain, order = c(1,1,0), 
             include.drift =T, lambda = 0) 
future2 = forecast(fit2, h = 182)
#future$fitted = exp(future$fitted)
plot(future2)
tail(future2$fitted)
lines(TStest)



ad = as.data.frame(future2$mean)
ad$date = test$Date
ad$indi = 0
i =1
while(i<182)
{
  if (ad[i+1,1]>ad[i,1])
  {ad[i+1,3] = 1}
  i= i+1
}
ad
##Combining two models apple news data and arima without drift(for apple stock)
all = read.csv("predictions_apple.csv")
all$date = as.Date(all$date, format = "%d-%m-%Y")
zy = inner_join(ad,all)
xx = inner_join(zy,test)
test$date=test$Date
xx[,c(1,5:13)]=NULL
p=0.1
xx$adj1 = NA
xx$adj2 = NA
xx$adj3 = NA
xx$adj4 = NA
xx$adj5 = NA
xx$adj6 = NA
xx$adj7 = NA
xx$adj8 = NA
xx$adj9 = NA

p = 0.1
while(p<1)
{i=1
  j=p*10+4
while(i<42)
  {
    xx[i,j] = p*xx[i,2]+(1-p)*xx[i,3]
    if(xx[i,j]>0.5)
    {xx[i,j]=1}
    else{xx[i,j]=0}
    i = i+1}
  p = p + 0.1
}
mean(xx[,4]==xx[,14])
xx$V14=NULL
###Combining two models apple news data and arima with drift(apple stock data)
zy1 = inner_join(am,all)
xx1 = inner_join(zy1,test)

xx1[,c(1,5:13)]=NULL

p=0.1
xx1$adj1 = NA
xx1$adj2 = NA
xx1$adj3 = NA
xx1$adj4 = NA
xx1$adj5 = NA
xx1$adj6 = NA
xx1$adj7 = NA
xx1$adj8 = NA
xx1$adj9 = NA

p = 0.1
while(p<1)
{i=1
j=p*10+4
while(i<42)
{
  xx1[i,j] = p*xx1[i,2]+(1-p)*xx1[i,3]
  if(xx1[i,j]>0.5)
  {xx1[i,j]=1}
  else{xx1[i,j]=0}
  i = i+1}
p = p + 0.1
}
mean(xx1[,4]==xx1[,14])

