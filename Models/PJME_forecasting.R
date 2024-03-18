library("fda")
library("ftsa")
library("rainbow")

setwd("/Users/raunaksandhu/Desktop/UNI/4th YR/Winter/MATH 518/FTS_Forecasting")
vec = read.csv("ProcessedCSVData/PJME_prices.csv")
vec =as.vector(vec[,2])
returns_matrix = matrix(vec, nrow=24, ncol = 59)
matplot(returns_matrix, type="l", xlab="Hour", ylab="Price")

# differencing

# first_difference_matrix = matrix(nrow=40,ncol=ncol(returns_matrix)-1)
# for (i in 1:ncol(first_difference_matrix)) {
#   first_difference_matrix[,i] = returns_matrix[,i+1] - returns_matrix[,i]
# }
# 
# matplot(first_difference_matrix, type="l")
# 
# returns_matrix = first_difference_matrix

# splitting  
  
returns_matrix_test = returns_matrix[,51:58]
returns_matrix = returns_matrix[,1:50]

matplot(returns_matrix, type="l")
matplot(returns_matrix_test, type="l")

# smooth returns_matrix

nbasis = 24
norder = 4

ReturnsDayTime = 0:23;
ReturnsDayRng = c(0,23);

ReturnsBasis = create.bspline.basis(ReturnsDayRng, nbasis, norder)

D2fdPar = fdPar(ReturnsBasis, lambda=0.1)

EquityReturnsMatfd = smooth.basis(ReturnsDayTime, returns_matrix, D2fdPar)$fd

# plotfit.fd(returns_matrix, ReturnsDayTime, EquityReturnsMatfd)

plot.fd(EquityReturnsMatfd, xlab = "Hour", ylab="Price")

returns_matrix = eval.fd(c(0:23), EquityReturnsMatfd)

matplot(returns_matrix, type="l")

EquityReturnsMatTestfd = smooth.basis(ReturnsDayTime, returns_matrix_test, D2fdPar)$fd

returns_matrix_test = eval.fd(c(0:23), EquityReturnsMatTestfd)

matplot(returns_matrix_test, type="l")

# viewing variation surface
# logprecvar.bifd = var.fd(EquityReturnsMatfd)
# daytime = seq(0,23,length=24)
# logprecvar_mat = eval.bifd(daytime, daytime,
#                            logprecvar.bifd)
# persp(daytime, daytime, logprecvar_mat, phi=25, r=3, expand = 0.2, theta=-40,
#       ticktype = "detailed",
#       xlab="Time (24 Hours)",
#       ylab="Time (24 hours)",
#       zlab="")
# contour(daytime, daytime, logprecvar_mat)
# plot(logprecvar_mat[,1])
# 
# # plot mean curve
# plot(mean.fd(EquityReturnsMatfd), ylab= "Price", xlab="Time of Day")
# 
# # plot fpca 
# dev.off()
# fpca = pca.fd(EquityReturnsMatfd,2)
# 
# harmfd = fpca$harmonics
# harmvals = eval.fd(0:23,harmfd)
# dim(harmvals)
# plot(1:24,harmvals[,1],xlab='time of day',ylab='PCs',
#      lwd=4,lty=1,cex.lab=2,cex.axis=2,type='l')
# plot(1:24,harmvals[,2],xlab='time of day',ylab='PCs',
#      lwd=4,lty=1,cex.lab=2,cex.axis=2,type='l')
# 
# plot.pca.fd(fpca, xlab="Time of Day")
# models

# T_stationary(returns_matrix)

colnames(returns_matrix) <- c(1:50)


# forecasting

fts_returns = fts(c(1:24), returns_matrix, xname="Time", yname="Price")

fit = ftsm(y = fts_returns, order=6)
forecast = forecast(fit,h=8, method="arima")
# forecast = forecast(fit,h=8)

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
par( mfrow= c(4,2) )
for (i in 1:ncol(returns_matrix_test)) {
  plot(returns_matrix_test[,i],type='l', ylim=c(25000,45000))
  lines(pred[,i], col='red')
  lines(forecast$lower$y[,i], col = 3); lines(forecast$upper$y[,i], col = 3)
}

plot(fts_returns, col = gray(0.8), xlab = "Hour",
     ylab = "Price",
     main = "Forecasted Price Curves")
# Plot the forecasts in rainbow color for Fig. 4(a)
plot(forecast, add = TRUE)

# legend("topright", c("2007", "2026"), col = c("red", "blue"), lty = 1)

write.table(pred,file='ftsamodel_pred.csv',sep=',')

# plot(forecast(ftsm(fts_prices, order=4), h=45), "components")

# plot(fts_prices, col = gray(0.8), xlab = "Time",
#      ylab = "Percentage Return",
#      main = "Forecasted return")
# plot(forecast(ftsm(fts_prices, order=4), h = 4), add=TRUE)
# par(new=TRUE)
# plot(returns_matrix_test[,5], type="l")
# legend("topright", c("20", "23"), col = c("red", "blue"), lty = 1)


pred2 = forecastfplsr(object = fts_returns, components=6, h = 8)$y

quartz()
par( mfrow= c(4,2) )
for (i in 1:ncol(returns_matrix_test)) {
  plot(returns_matrix_test[,i],type='l')
  lines(pred2[,i], col='red')
}

plot(fts_returns, col = gray(0.8), xlab = "Hour",
     ylab = "Price",
     main = "Forecasted Price Curves")
# Plot the forecasts in rainbow color for Fig. 4(a)
matplot(pred2, add = TRUE, type="l")

write.table(pred2,file='plsrmodel_pred.csv',sep=',')


