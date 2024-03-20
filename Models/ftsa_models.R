library("fda")
library("ftsa")
library("rainbow")

# mat =load("ProcessedRData/2020_SPY_cidr_2020-06-15_2020-08-11_matrix.RData")
mat =load("ProcessedRData/2020_SPY_cidr_2020-07-06_2020-09-02_matrix.RData")
mat =as.matrix(returns_matrix)

matplot(returns_matrix, type="l")

# differencing

first_difference_matrix = matrix(nrow=40,ncol=ncol(returns_matrix)-1)
for (i in 1:ncol(first_difference_matrix)) {
  first_difference_matrix[,i] = returns_matrix[,i+1] - returns_matrix[,i]
}

matplot(first_difference_matrix, type="l")  

returns_matrix = first_difference_matrix

# splitting  
  
returns_matrix_test = returns_matrix[,39:43]
returns_matrix = returns_matrix[,1:38]

matplot(returns_matrix, type="l")
matplot(returns_matrix_test, type="l")

# smooth returns_matrix

nbasis = 40
norder = 4

ReturnsDayTime = 0:39;
ReturnsDayRng = c(0,39);

ReturnsBasis = create.bspline.basis(ReturnsDayRng, nbasis, norder)

D2fdPar = fdPar(ReturnsBasis, lambda=0.8)

EquityReturnsMatfd = smooth.basis(ReturnsDayTime, returns_matrix, D2fdPar)$fd

returns_matrix = eval.fd(c(0:39), EquityReturnsMatfd)

matplot(returns_matrix, type="l")

EquityReturnsMatTestfd = smooth.basis(ReturnsDayTime, returns_matrix_test, D2fdPar)$fd

returns_matrix_test = eval.fd(c(0:39), EquityReturnsMatTestfd)

matplot(returns_matrix_test, type="l")

# models

# T_stationary(returns_matrix)

colnames(returns_matrix) <- c(1:38)


# forecasting

fts_returns = fts(c(1:40), returns_matrix, xname="Time", yname="Return")

fit = ftsm(y = fts_returns, order=6)
# forecast = forecast(fit,h=5)

forecast = forecast(fit,h=5, method="arima")

plot(forecast, "components")


pred = forecast$mean$y

# plotting fit
temp = fit$coeff %*% t(fit$basis)
plot(temp[1,],type='l')
lines(returns_matrix[,1], col='red')


plot(temp[3,],type='l')
lines(returns_matrix[,3],col='red')


# plotting pred
quartz()
par( mfrow= c(3,2) )
for (i in 1:ncol(returns_matrix_test)) {
  plot(returns_matrix_test[,i],type='l', ylim=c(-0.5,0.5))
  lines(pred[,i], col='red')
  lines(forecast$lower$y[,i], col = 3); lines(forecast$upper$y[,i], col = 3)
}

write.table(pred,file='ftsamodel_pred.csv',sep=',')

# plot(forecast(ftsm(fts_prices, order=4), h=45), "components")

# plot(fts_prices, col = gray(0.8), xlab = "Time",
#      ylab = "Percentage Return",
#      main = "Forecasted return")
# plot(forecast(ftsm(fts_prices, order=4), h = 4), add=TRUE)
# par(new=TRUE)
# plot(returns_matrix_test[,5], type="l")
# legend("topright", c("20", "23"), col = c("red", "blue"), lty = 1)


pred2 = forecastfplsr(object = fts_returns, components=6, h = 5)$y

quartz()
par( mfrow= c(3,2) )
for (i in 1:ncol(returns_matrix_test)) {
  plot(returns_matrix_test[,i],type='l')
  lines(pred2[,i], col='red')
}

write.table(pred2,file='plsrmodel_pred.csv',sep=',')


