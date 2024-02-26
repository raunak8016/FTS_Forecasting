library("ftsa")
library("rainbow")

mat =load("ProcessedRData/2020_SPY_cidr_2020-06-15_2020-08-11_matrix.RData")
mat =as.matrix(returns_matrix)

returns_matrix_test = returns_matrix[,36:40]
returns_matrix = returns_matrix[,1:35]

matplot(returns_matrix, type="l")

T_stationary(returns_matrix)

colnames(returns_matrix) <- c(1:35)


# forecasting

fts_returns = fts(c(1:40), returns_matrix, xname="Time", yname="Return")

fit = ftsm(y = fts_returns)
pred = forecast(fit,h=5)$mean$y

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
  plot(returns_matrix_test[,i],type='l')
  lines(pred[,i], col='red')
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


pred2 = forecastfplsr(object = fts_returns,components=6,  h = 5)$y

quartz()
par( mfrow= c(3,2) )
for (i in 1:ncol(returns_matrix_test)) {
  plot(returns_matrix_test[,i],type='l')
  lines(pred2[,i], col='red')
}

write.table(pred2,file='plsrmodel_pred.csv',sep=',')


