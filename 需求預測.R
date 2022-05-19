Marriott<- read.csv("C:/Users/Eason/Desktop/R/練習資料集/需求預測/UV0353.csv",head= TRUE)
Marriott<-Marriott[1:87,]
Marriott<-Marriott[,-1]
Marriott$day <- c(1:87)

mse<-function(true_value,pred_value){
  MSE <- sum((true_value-pred_value)**2)/length(true_value)
  return(MSE)
}

library(ggplot2)
plotttt <-function(data,method){
  ggplot(data , aes(x=day) )+
    geom_line(aes(y= Demand,colour='demand'),size=1)+
    geom_line(aes(y= method,colour='method'),size=1)+
    ggtitle('data')
}

#折線圖
ggplot(Marriott, aes(x =1:87, y = Demand))+ geom_line()

ts_Marriott<-ts(Marriott,frequency = 7)

#去季節指數
decom_Marriott<-decompose(ts_Marriott[,'Demand'],type='mult')


plot(decom_Marriott)
decom_Marriott$seasonal

Marriott$nosea_demand<-Marriott$Demand/Marriott$Dow.Index
Marriott$nosea_thuesday<-Marriott$Tuesday.Bookings/Marriott$Dow.Index

#切分訓練資料
train_Marriott <- Marriott[1:61,]
ts_train_Marriott <- ts(train_Marriott,frequency = 7)
decom_train<-decompose(ts_train_Marriott,type='mult')

#測試資料
test_Marriott <- Marriott[61:87,]

#naive
naive <- snaive(decom_train$x[,2],h=27)
test_Marriott$naive <-naive$mean
plotttt(test_Marriott, test_Marriott$naive)
rmse(test_Marriott$naive)
mse(test_Marriott$naive) #479.9789

library(forecast)
#MA Demand
Marriott$ma3 <-ma(Marriott$Demand,3)
Marriott$ma5 <-ma(Marriott$Demand,5)

ggplot(Marriott,aes(x=1:87))+
  geom_line(aes(y=Demand,colour='demand'),size=1)+
  geom_line(aes(y=ma3,colour='ma3'),size=1)+
  geom_line(aes(y=ma10,colour='ma10'),size=1)+
  ggtitle('data ma simulation')


mse(Marriott$ma3) #479.9789


#MA pickup.ratio
Marriott$ma3 <-ma(Marriott$Pickup.Ratio,3)*Marriott$Tuesday.Bookings
Marriott$ma10 <-ma(Marriott$Pickup.Ratio,10)*Marriott$Tuesday.Bookings

ggplot(Marriott,aes(x=1:87))+
  geom_line(aes(y=Demand,colour='demand'),size=1)+
  geom_line(aes(y=ma3,colour='ma3'),size=1)+
  geom_line(aes(y=ma10,colour='ma10'),size=0.01)+
  ggtitle('data ma simulation')

#MSE
mse_MA3_Marriott

#簡回歸
#train data
lm_simple <-lm(nosea_demand ~ nosea_thuesday,data=train_Marriott)
summary(lm_simple)$r.squared
train_Marriott$simple_lm_pred <- predict(lm_simple,newdata = train_Marriott)*train_Marriott$Dow.Index

plotttt(train_Marriott, train_Marriott$simple_lm_pred)

#test data
test_Marriott$simple_lm_pred <- predict(lm_simple,newdata = test_Marriott)*test_Marriott$Dow.Index
plotttt(test_Marriott, test_Marriott$simple_lm_pred)

#MSE
mse(test_Marriott$Demand , test_Marriott$simple_lm_pred)



#多元回歸
#train data
lm_marriott <-lm(nosea_demand ~  nosea_thuesday, data=train_Marriott)
summary(lm_marriott)$r.squared
train_Marriott$lm_pred <- predict(lm_marriott,newdata = train_Marriott)*train_Marriott$Dow.Index

plotttt(train_Marriott, train_Marriott$lm_pred)

#test data
test_Marriott$lm_pred <- predict(lm_marriott,newdata = test_Marriott)*test_Marriott$Dow.Index
plotttt(test_Marriott, test_Marriott$lm_pred)

#MSE
mse_lm_Marriott = mse(test_Marriott$lm_pred)


#SVR
library(e1071)
tune.model_marri <- tune.svm(Demand ~ nosea_thuesday ,
                       data=train_Marriott,
                       type="eps-regression",kernel="radial",
                       range=list(cost = 2^c(-8,-4,-2,0), epsilon = seq(0,10,0.1)))

tune.model_marri$best.model
#100, 0.1 ,0.05
regressor_marri <- svm(formula=Demand ~ nosea_thuesday ,
                 data=train_Marriott,type= 'eps-regression',
                 kernel='radial',cost=1,epsilon=0.1,gamma=1)
#train data
train_Marriott$svr <- predict(regressor_marri,train_Marriott)

plotttt(train_Marriott, train_Marriott$svr)

#MSE
mse_svm_train_Marriott <- sum((train_Marriott$Demand-train_Marriott$svm)**2)/length(train_Marriott$Demand)


#test data
test_Marriott$svm <- predict(regressor_marri,test_Marriott)
plotttt(test_Marriott, test_Marriott$svr)
#MSE
mse_svm_test_Marriott <- sum((test_Marriott$Demand-test_Marriott$svm)**2)/length(test_Marriott$Demand)

#ARMA
library(forecast)
library(tseries)
adf.test(ts_train_Marriott[,'Pickup.Ratio'])#非定態
adf.test(ts_train_Marriott[,'Demand'])#非定態

#Demand
auto.arima(ts_train_Marriott[,'Demand'] ,stepwise = F,d = 1,trace = T,stationary = T,ic=c("aic"))
fit4 = arima(ts_train_Marriott[,'Demand'],order=c(1,0,0),seasonal=list(order=c(2,0,0),period=7),include.mean = TRUE)
p4<-forecast(fit4,h=27)
test_Marriott$arima<-p4$mean

#Pickup.Ratio
auto.arima(ts_train_Marriott[,'Pickup.Ratio'] ,stepwise = F,d = 1,trace = T,stationary = T,ic=c("aic"))
fit3 = arima(ts_train_Marriott[,'Pickup.Ratio'],order=c(1,1,1),seasonal=list(order=c(2,1,1),period=7),include.mean = FALSE) 
p3<-forecast(fit3,h=27)
test_Marriott$arima<-p3$mean*test_Marriott$Tuesday.Bookings

library(ggplot2)
ggplot(test_Marriott,aes(x=61:87))+
  geom_line(aes(y=Demand,colour='demand'),size=1)+
  geom_line(aes(y=arima,colour='ARIMA'),size=1)+
  ggtitle('test data arima simulation')

#MSE
mse_arima_test_Marriott <- sum((test_Marriott$Demand-test_Marriott$arima)**2)/length(test_Marriott$Demand)
#test data RMSE=176.6736
mse_arima_test_Marriott**0.5


#xgboost
library(xgboost)
#輸入值為train data的thuesday.booking,Dow.Indicator,Dow.index
xgb_regressor_marri  <- xgboost(data=as.matrix(data.frame(train_Marriott[,c(1,5,10)])),
                         label=as.matrix(train_Marriott$nosea_demand),
                         booster= 'gbtree',objective= 'reg:linear',
                         nrounds=100)
#train data
train_Marriott$xgb_pred <-predict(xgb_regressor_marri,
                                  as.matrix(data.frame(train_Marriott$Dow.Indicator,train_Marriott$Dow.Index,train_Marriott$nosea_thuesday)))

plotttt(train_Marriott,train_Marriott$xgb_pred)
#RMSE=0.011681
xgb_regressor_marri$evaluation_log[100]

#test data
test_Marriott$xgb <-predict(xgb_regressor_marri,
                            as.matrix(data.frame(test_Marriott[,c(1,5,10)])))*test_Marriott$Dow.Index
plotttt(test_Marriott,test_Marriott$xgb_pred)
#MSE
mse_xgb_test_Marriott <- sum((test_Marriott$Demand-test_Marriott$xgb)**2)/length(test_Marriott$Demand)


#決策樹
library(rpart) 
library(rpart.plot)
M2 <- rpart(formula = Demand ~ Tuesday.Bookings+Dow.Indicator
            ,data = train_Marriott,method  = "anova",
             control = list(minsplit = 1, maxdepth = 100, xval = 10))

train_Marriott$tree<-predict(M2,train_Marriott)
plotttt(train_Marriott,train_Marriott$tree)
#MSE
mse_tree_train_Marriott <- sum((train_Marriott$Demand-train_Marriott$tree)**2)/length(train_Marriott$Demand)


#test data
test_Marriott$tree <-predict(M2,test_Marriott)
plotttt(test_Marriott,test_Marriott$tree)
#MSE
mse_tree_test_Marriott <- sum((test_Marriott$Demand-test_Marriott$tree)**2)/length(test_Marriott$Demand)


#randomForest
library(randomForest)
set.seed(123)
rf_marri <- randomForest(Demand ~ Tuesday.Bookings+Dow.Indicator
                 ,data = train_Marriott,importance=T,
                 proximity=T,do.trace = 10)
par(mfrow=c(1,1))
plot(rf_marri)
set.seed(123)
rf_marri <- randomForest(Demand ~ Tuesday.Bookings+Dow.Indicator, data = train_Marriott,
                   ntree = 200, mtry = 2,do.trace = 10)

train_Marriott$rf<- predict(rf_marri,train_Marriott)
plotttt(train_Marriott,train_Marriott$rf)

#MSE
mse_rf_train_Marri <-sum((train_Marriott$rf-train_Marriott$Demand)**2)/length(train_Marriott$Demand)


#test data
test_Marriott$rf <- predict(rf_marri,test_Marriott)
plotttt(test_Marriott,test_Marriott$rf)

#MSE
mse_rf_test_Marri <-sum((test_Marriott$rf-test_Marriott$Demand)**2)/length(test_Marriott$Demand)



#霍爾指數平滑預測，用Pickup.Ratio預測
library(forecast)
#gamma=TRUE(Seasonal)、beta=FALSE(指數平滑)
ex_demand <- HoltWinters(ts_train_Marriott[,'Pickup.Ratio'], gamma=TRUE,beta=FALSE)
plot(ex_demand)
ex_demand$fitted
train_Marriott$expo[8:61] <-ex_demand$fitted[,1]*train_Marriott$Tuesday.Bookings[8:61]

#train data
plotttt(train_Marriott,train_Marriott$expo)

#MSE
mse_expo_train_Marri <-sum((train_Marriott$expo-train_Marriott$Demand)**2)/length(train_Marriott$Demand)


#預測27期的資料
library(forecast)
expo_forecast<-forecast(ex_demand,h=27)
plot(expo_forecast)
accuracy(expo_forecast)
test_Marriott$expo<-expo_forecast$mean*test_Marriott$Tuesday.Bookings

plotttt(test_Marriott,test_Marriott$expo)

#MSE
mse_expo_test_Marri <-sum((test_Marriott$expo-test_Marriott$Demand)**2)/length(test_Marriott$Demand)



#TSLM，用Pickup.Ratio預測
library(forecast)
tslm_Marriott <-tslm(Pickup.Ratio ~ season+trend ,data=ts_train_Marriott)
summary(tslm_Marriott)$r.squared

train_Marriott$tslm<-predict(tslm_Marriott,train_Marriott)*train_Marriott$Tuesday.Bookings
#train data
plotttt(train_Marriott,train_Marriott$tslm)

#test data
facst2<-forecast(tslm_Marriott,h=27)
test_Marriott$tslm <- facst2$mean* test_Marriott$Tuesday.Bookings
plotttt(test_Marriott,test_Marriott$tslm)

#MSE
mse_tslm_test_Marri <-sum((test_Marriott$tslm-test_Marriott$Demand)**2)/length(test_Marriott$Demand)

Marriott_method<-c('SVR','XGBOOST','simple lm','TSLM','lm','decision tree','RF','ARIMA','exponential')

Marriott_MSE<-c(mse_svm_test_Marriott,mse_xgb_test_Marriott,
                mse_simple_lm_Marriott ,mse_tslm_test_Marri,
                mse_lm_Marriott,mse_tree_test_Marriott
       ,mse_rf_test_Marri,mse_arima_test_Marriott,mse_expo_test_Marri)
testing_marriott<-data.frame(Marriott_method,Marriott_MSE)


