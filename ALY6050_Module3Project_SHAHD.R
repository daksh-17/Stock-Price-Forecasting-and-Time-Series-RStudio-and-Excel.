library(quantmod)
library(forecast)
library(highcharter)
##GETTING DATA OF KO AND COST from yahoo finance

getSymbols("COST",src="yahoo", from = "2017-01-01", to = "2022-01-01") 
getSymbols("KO",src="yahoo", from = "2017-01-01", to = "2022-01-01") 
wine <- read.csv("dry_wine.csv")


chartSeries(COST, name = "COSTCO WHOLESALE Price")
closing_cc <- Cl(to.monthly(COST))
dc<- decompose(as.ts(closing_cc, start =c(2017,1)))
plot(dc)

chartSeries(KO, name = "COCO-COLA Price")
closing_ko <- Cl(to.monthly(KO))
dc1<- decompose(as.ts(closing_ko, start =c(2017,1)))
plot(dc1)


highchart(type = "stock")%>%
  hc_add_series(Cl(COST), name="COSTCO")%>%
  hc_add_series(Cl(KO), name = "COCO_COLA")%>%
  hc_title(text="<b>COSTvsKO</b>")



#Forecasting
n <- 100

#Splitting
train<- head(Cl(COST), length(Cl(COST))-n)
test <- tail(Cl(COST), n)

train1<- head(Cl(KO), length(Cl(KO))-n)
test1 <- tail(Cl(KO), n)

#Forecast using naive method
fc_na1 <- naive(train, h=n)

fc_na2 <- naive(train1, h=n)

summary(fc_na1)

autoplot(fc_na1) +
  autolayer(ts(test, start=length(train)), series = "Test Data")

autoplot(fc_na2) +
  autolayer(ts(test1, start=length(train1)), series = "Test Data")


#write.csv(club,"club_stock.csv")

#Forecasting using AR1 model
fc_arima1 <- arima(train, order = c(1,0,0))
summary(fc_arima1)
forecast_COSTO <- forecast(fc_arima1, h=100)
plot(forecast_COSTO)


auto_model_arima <- auto.arima(train)
fc_auto1 <- forecast(auto_model_arima, h=n) 

autoplot(fc_auto1)+
  autolayer(ts(test, start = length(train)), series = "Test Data")

checkresiduals(fc_auto1)
accuracy(fc_auto1)


#KO
fc_arima2 <- arima(train1, order = c(1,0,0))
summary(fc_arima2)
forecast_KO <- forecast(fc_arima2, h=100)
plot(forecast_KO)


auto_model_arima1 <- auto.arima(train1)
fc_auto2 <- forecast(auto_model_arima1, h=n) 

autoplot(fc_auto2)+
  autolayer(ts(test1, start = length(train1)), series = "Test Data")

checkresiduals(fc_auto2)
accuracy(fc_auto2) 



####Wine 
m<-36
ts_wine <- ts(wine, start = c(1980,1), freq = 12)
ts_wine <- ts_wine[,2]
train_w<- head(ts_wine, length(ts_wine)-m)
test_w <- tail(ts_wine, m)

model_w <- auto.arima(train_w)
accuracy(model_w)

fc_s_w <- plot(forecast(model_w,12), xlab = "Date", ylab = "Price", main = "ARIMA Forecast for Wine")


pred_wine <- forecast(model_w,12)
pred_wine
