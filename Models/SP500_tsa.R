# Dataset originally from https://www.iea.org/data-and-statistics/data-product/monthly-electricity-statistics#overview

vec = read.csv("ProcessedCSVData/2020_SPY_cumulative_returns_2020-07-06_2020-09-02.csv")
x <- as.vector(vec[,2])

ts.plot(x, xlab="Time", ylab="Cumulative Return", main="SPY")

# plot single year
par( mfrow= c(1,2) )
ts.plot(x[1:1483])
ts.plot(x[1484:1678])
dev.off()
source("functions.R")

z <- x[1:1483]
y <- x[1484:1678]
ts.plot(z, xlab="Time", ylab="Cumulative Return", main="Train Data")
ts.plot(y, xlab="Time", ylab="Cumulative Return", main="Test Data")



diff1= 1
mytsplot(z, ind=0, d=diff1, lag = 40)


z.fit <- arima(z, order=c(0,1,5))

tsdiag(z.fit)
pacf(z.fit$resi, ylim=c(-1,1))
qqnorm(z.fit$resi)

vec.cast <- predict(z.fit, 195)  
total_x <- ts(x)
plot.ts(total_x, ylim=c(-0.05, 0.2), xlab="Year", ylab="Price", main="Model Forecast")
a <- seq(1484,1678,length=196)[-196]
b <- vec.cast$pred
d <- vec.cast$se
points(a, b, type="b", col = 2)
points(a, b+2*d, type="b", col = 4)
points(a, b-2*d, type="b", col = 6)

dev.off()
plot(a, y, type="l", col=1, ylim=c(-0.05, 0.2), xlab="Year", ylab="Electricity Consumption (GWh)", main="Model Forecast for 2021-2022")
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


