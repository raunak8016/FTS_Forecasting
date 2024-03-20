# Dataset originally from https://www.iea.org/data-and-statistics/data-product/monthly-electricity-statistics#overview

vec = read.csv("ProcessedCSVData/PJME_prices.csv")
x <- as.vector(vec[,2])[1:1392]

ts.plot(x, xlab="Hour", ylab="Price", main="PJME (Hourly)")

# plot single year
par( mfrow= c(1,2) )
ts.plot(x[1:1200])
ts.plot(x[1201:1392])
dev.off()
source("functions.R")

z <- x[1:1200]
y <- x[1201:1392]
ts.plot(z, xlab="Hour", ylab="Price", main="Train Data")
ts.plot(y, xlab="Hour", ylab="Price", main="Test Data")



diff1= 1
mytsplot(z, ind=0, d=diff1, lag = 40)
mytsplot(diff(z, d=diff1), ind=0, d = 24, lag = 100)


z.fit <- arima(z, order=c(12,1,0), seasonal=
                    list(order=c(2,1,2), period = 24) )

tsdiag(z.fit)
pacf(z.fit$resi, ylim=c(-1,1))
qqnorm(z.fit$resi)

vec.cast <- predict(z.fit, 192)  
total_x <- ts(x)
plot.ts(total_x, ylim=c(20000, 50000), xlab="Year", ylab="Price", main="Model Forecast")
a <- seq(1201,1392,length=193)[-193]
b <- vec.cast$pred
d <- vec.cast$se
points(a, b, type="b", col = 2)
points(a, b+2*d, type="b", col = 4)
points(a, b-2*d, type="b", col = 6)

dev.off()
plot(a, y, type="l", col=1, ylim=c(20000, 50000), xlab="Year", ylab="Electricity Consumption (GWh)", main="Model Forecast for 2021-2022")
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


