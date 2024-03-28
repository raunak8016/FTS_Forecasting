library(forecast)

setwd("~/Desktop/UNI/4th YR/Winter/MATH 518/FTS_Forecasting/")
vec = read.csv("ProcessedCSVData/electricity_prices.csv")
x <- as.vector(vec[,3])[1:1392]

ts.plot(x, xlab="Hour", ylab="Price")

# plot single year
par( mfrow= c(1,2) )
ts.plot(x[1:1200], main="Train", xlab="Hour", ylab="Price")
ts.plot(x[1201:1392], main="Test", xlab="Hour", ylab="Price")
dev.off()
source("functions.R")

z <- x[1:1200]
y <- x[1201:1392]
ts.plot(z, xlab="Hour", ylab="Price", main="Train Data")
ts.plot(y, xlab="Hour", ylab="Price", main="Test Data")


diff1= 1
mytsplot(z, ind=0, d=diff1, lag = 70)
mytsplot(diff(z, d=diff1), ind=0, d = 24, lag = 50)


z.fit <- arima(z, order=c(24,1,0), seasonal=
                    list(order=c(0,1,1), period = 24) )

tsdiag(z.fit)
pacf(z.fit$resi, ylim=c(-1,1))
qqnorm(z.fit$resi)

vec.cast <- predict(z.fit, 192)  
total_x <- ts(x)
plot.ts(total_x, ylim=c(0, 500), xlab="Hour", ylab="Price", main="Model Forecast")
a <- seq(1201,1392,length=193)[-193]
b <- vec.cast$pred
d <- vec.cast$se
points(a, b, type="b", col = 2)
points(a, b+2*d, type="b", col = 4)
points(a, b-2*d, type="b", col = 6)

dev.off()
plot(a, y, type="l", col=1, ylim=c(0, 500), xlab="Hour", ylab="Price", main="Model Forecast")
points(a, b, type="b", col = 2)
# points(a, b+2*d, type="b", col = 4)
# points(a, b-2*d, type="b", col = 6)

boxtr(z, d = 3)

# MAE
mean(abs(b-y))

# MAPE
mean(abs(b-y)/y)

# MSE
mean((b-y)^2)

error_by_curve = matrix(nrow=2, ncol = 8)

i=1
for (i in 1:ncol(pred)) {
  start = (24*(i-1))
  end = (24*i)
  error_by_curve[1,i] = mean(abs(b[start:end]-y[start:end]))
  error_by_curve[2,i] = mean((b[start:end]-y[start:end])^2)
}

write.table(error_by_curve,file='time_series_pred.csv',sep=',')

