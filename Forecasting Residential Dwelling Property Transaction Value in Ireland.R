#Predicting Irish Household Sale Value using Time Series Analysis
library(tseries)

#Dataset---------------------
dat = read.csv("C:/Users/.../Irish Household Sale Data.csv")[,"VALUE"]/1000 
#divide by 1000 to express the data in terms of 000's

#Split Data into Train and Test sets
tseries.testset <- ts(dat[(length(dat)-36):length(dat)], start = c(2018,12), end = c(2021,12), frequency = 12)
tseries <- ts(dat[1:(length(dat)-36)], start = c(2010,1), end = c(2018,12), frequency = 12)

#Reduction to stationarity----------------
plot(tseries , main = "Mean Household Sale Price (Jan 2010-Dec 2018)" , ylab = "Euro ('000 s)" , lwd = 2)

plot(decompose(tseries))
decompose((tseries))$seasonal 
# NOTE: seasonal component peaks in August and reaches a minimum in May

#Test data for stationarity 
adf.test(tseries , alternative = "stationary")

#difference dataset
plot(diff(tseries , differences = 1) , main = "First Difference Mean Household Sale Price (Jan 2010 - Dec 2018) " , ylab = "Euro ('000 s)" , lwd = 2)

adf.test(diff(tseries , differences = 1) , alternative = "stationary")$p.value
#reject null hypothesis- data is stationary

#Model fitting ---------------------------
#investigate acf and pacf
par(mfrow = c(1,2))
acf(diff(tseries) , lag.max = 36 , main = "Auto-correllation Function of Integrated Time Series")
pacf(diff(tseries), lag.max = 36 , main = "Partial Auto-correllation Function of Integrated Time Series" )

#Choosing seasonal component
t.smodel = arima(tseries, order = c(0,1,0) , seasonal = c(0,0,1))
AIC(t.smodel)

t.smodel.2 = arima(tseries, order = c(0,1,0) , seasonal = c(1,0,1))
AIC(t.smodel.2)

#Choosing non-seasonal component
t.model = arima(tseries , order = c(0,1,1), seasonal = c(1,0,1))
AIC(t.model)

t.model.2 = arima(tseries, order = c(1,1,1) , seasonal = c(1,0,1))
AIC(t.model.2)

#chosen model:
t.model

#grid search of hyperparameters
#seasonal
#Commented this so that it is not run automatically - this takes a little while - feel free to uncomment and run to verify this works :)
'ij.min = c(1,1)
min.AIC = AIC(t.smodel.2)
for (i in c(0,1:5) ){
  for (j in c(0,1:5)){
    t.model.ij = arima(tseries , order = c(0,1,0), seasonal = c(i,0,j))
    AIC.ij = AIC(t.model.ij)
    
    if(AIC.ij < min.AIC){
      min.AIC = AIC.ij
      ij.min = c(i,j)
    }
  }
}
print(ij.min)
'

#non-seasonal
'ij.min = c(0,1)
min.AIC = AIC(t.model)
for (i in c(0,1:5) ){
  for (j in c(0,1:5)){
    t.model.ij = arima(tseries , order = c(i,1,j), seasonal = c(1,0,1))
    AIC.ij = AIC(t.model.ij)
    
    if(AIC.ij < min.AIC){
      min.AIC = AIC.ij
      ij.min = c(i,j)
    }
  }
}
print(ij.min)
'

#Model Criticism ------------------------
par(mfrow = c(1,2) , mar = c(1,1,1,1))
tsdiag(t.model , gof.lag = 12)

par(mfrow = c(1,2) , mar = c(5.1,4.1,4.1,2.1))
cpgram(ts = tseries , main = "Mean Household Sale Price")
cpgram(ts = t.model$residuals , main = "T.model Residuals")

#Forecasting---------------------------------
preds = predict(object = t.model , n.ahead = 12*3 )

plot(tseries , xlim = c(2010,2022) , ylim =c(140 , 350) , main = "Mean Household Sale Price (Jan 2010 - Dec 2021)" , ylab = "Euro ('000 s)" , lwd = 2)
lines(preds$pred , col = "red" , lty = 1 , lwd = 2 )
lines( tseries.testset , lty = 5, lwd = 2 , col = "black")
lines(preds$pred + 2*preds$se , col = "blue" , lty = 4, lwd = 2)
lines(preds$pred - 2*preds$se , col = "blue" , lty = 4, lwd = 2)
legend("topleft" , legend = c("Training set" , "Observed Test Set" , "Predictions" , "C.I Bounds") , col = c("black" , "black" , "red" , "blue") , lwd = 2 , lty = c(1,5,1,4))

res = c(tseries.testset - preds$pred)
hist(res , probability = T , col = "light blue", main = "Histogram of Prediction errors")

#mean
mean(res)
#standard deviation
sd(res)
#model estimate for standard deviation
sqrt(t.model$sigma2)

#rerun analysis for model excluding seasonal MA
#residuals of other model------
t.model.3 = arima(tseries, order = c(0,1,1) , seasonal = c(1,0,0))
preds.3 = predict(object = t.model.3 , n.ahead = 36 )

res3 = c(tseries.testset - preds.3$pred)
#mean
mean(res3)
#standard deviation
sd(res3)

#Discussion ---------------
predict(object = t.model , n.ahead = 12*6 )

predict(object = t.model , n.ahead = 12*6 )$pred[12*6]/min(tseries) -1


#Cubic polynomial model 
m.mth = mean(time(tseries))
mth = time(tseries) - m.mth
low.ts = lowess(c(tseries) ~ mth + I(mth)^2 + I(mth)^3)

par(mfrow = c(1,2))
plot(c(time(tseries)) , low.ts$y , ylim = c(140,350) , 'l' , lty = 10 , col = "red")
lines(tseries)

low.res = c(tseries - low.ts$y)
plot(x = c(time(tseries)), y = low.res ,type=  'l' , main = "Residuals of cubic polynomial fit" , ylab = "Residual" , lwd = 2)

#test residuals for stationarity
adf.test(low.res)

#Ratio of Mean Sale values
frac.ts = ts(data = tseries[2:length(tseries)]/tseries[1:(length(tseries)-1)] , start = c(2010,2) , end = c(2018,12) , frequency = 12)

par(mfrow = c(1,1))
plot((frac.ts) , type = 'l' , lwd = 2 , main = "Ratio of Mean Sale Value to Previous Month" , ylab = "Ratio")

#test for stationarity
adf.test((frac.ts))
