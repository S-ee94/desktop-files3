#Question1
#converting the non stationary model to stationary
AirPassengers
plot(AirPassengers)
abline(lm(AirPassengers~time(AirPassengers)))
plot(decompose(AirPassengers))
boxplot(AirPassengers~cycle(AirPassengers))
plot(AirPassengers)
plot(log(AirPassengers))
abline(lm(log(AirPassengers)~time(log(AirPassengers))))
plot(diff(log(AirPassengers)))
abline(lm(diff(log(AirPassengers))~time(diff(log(AirPassengers)))))

#AR  I   MA
#p   d   q
#0   1   1

acf(AirPassengers)
acf(diff(log(AirPassengers)))
pacf(diff(log(AirPassengers)))

#building aroma model
arima(log(AirPassengers), c(0,1,1), seasonal = list(order=c(0,1,0),period=12))-> mod_time
predict(mod_time, 20*12)-> pred_time
pred_time

#e value is 2.718 
2.718^pred_time$pred -> pred_time
pred_time
ts.plot(AirPassengers, pred_time, log="y", lty=c(1,3))






