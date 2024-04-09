library(fda)
library(tidyverse)


# data processing
vec = read.csv("ProcessedCSVData/electricity_prices.csv")
vec =as.vector(vec[,3])
prices_matrix = matrix(vec, nrow=24, ncol = 59)

dim(prices_matrix)

EquityReturnsMat = as.matrix(prices_matrix)

# training set
matplot(EquityReturnsMat[,1:50], type="l", xlab="Time (Hour)", ylab="Price")

# testing set
matplot(EquityReturnsMat[,51:58], type="l")

TestingEquityReturnsMat = EquityReturnsMat[,51:58]

EquityReturnsMat = EquityReturnsMat[,1:50]

matplot(EquityReturnsMat, type="l")

T_stationary(EquityReturnsMat)
# generate functional time series

# number of time points in a day (10 minute intervals over trading hours)
nbasis = 24
norder = 4

ReturnsDayTime = 0:23;
ReturnsDayRng = c(0,23);

ReturnsBasis = create.bspline.basis(ReturnsDayRng, nbasis, norder)

D2fdPar = fdPar(ReturnsBasis, lambda=0.1)

EquityReturnsMatfd = smooth.basis(ReturnsDayTime, EquityReturnsMat, D2fdPar)$fd

returns_matrix = eval.fd(c(0:23), EquityReturnsMatfd)

plot.fd(EquityReturnsMatfd, xlab="Hour", ylab="Price")

matplot(returns_matrix, type="l")

EquityReturnsMatTestfd = smooth.basis(ReturnsDayTime, returns_matrix_test, D2fdPar)$fd


# view fit of each functional time series curve

# plotfit.fd(EquityReturnsMat, ReturnsDayTime, EquityReturnsMatfd)

# Set up regression coefficients

nbasis_regression = 24
ReturnsRng = c(0,23)

ReturnsBetaBasis = create.bspline.basis(ReturnsRng,nbasis_regression, norder=4)

ReturnsBeta0Par = fdPar(ReturnsBetaBasis, 2, 1e-10)

ReturnsBeta1fd  = bifd(matrix(0,24,24), ReturnsBetaBasis, ReturnsBetaBasis)

ReturnsBeta1Par = bifdPar(ReturnsBeta1fd, 2, 2, 1e3, 1e3)

ReturnsBetaList = list(ReturnsBeta0Par, ReturnsBeta1Par)

#  Define the response and explanatory vars
NextYear = EquityReturnsMatfd[2:ncol(EquityReturnsMat)] # X_(i+1)(t)
LastYear = EquityReturnsMatfd[1:(ncol(EquityReturnsMat)-1)] # X_i(t)

#  Function to Functional Linear Model

Returns.linmod = linmod(NextYear, LastYear, ReturnsBetaList)

Returns.times = seq(0, 23, 1)
Returns.beta1mat = eval.bifd(Returns.times, Returns.times, Returns.linmod$beta1estbifd)

par(mfrow=c(1,2))
persp(Returns.times, Returns.times, matrix(data=0, nrow=length(Returns.times), ncol=length(Returns.times)),
      zlim = c(-0.5, 1.5), # Example z-axis limits
      cex.lab=1, cex.axis=1.5, theta=-30,
      xlab="", ylab="", zlab="", ticktype = "simple", axes = FALSE)

par(new=TRUE)
persp(Returns.times, Returns.times, Returns.beta1mat,
      xlab="Time (h)", ylab="Time (h)",zlab="beta(s,t)",
      ticktype = "detailed",
      cex.lab=1.5,cex.axis=1.2, theta=-30, font.lab = 2)

persp(Returns.times, Returns.times, matrix(data=0, nrow=length(Returns.times), ncol=length(Returns.times)),
      zlim = c(-0.5, 1.5), # Example z-axis limits
      cex.lab=1, cex.axis=1.5, theta=150,
      xlab="", ylab="", zlab="", ticktype = "simple", axes = FALSE)

par(new=TRUE)
persp(Returns.times, Returns.times, Returns.beta1mat,
      xlab="Time (h)", ylab="Time (h)",zlab="beta(s,t)",
      ticktype = "detailed",
      cex.lab=1.5,cex.axis=1.2, theta=150, font.lab = 2)


dev.off()
# setting up prediction

Yhat_fd <- fd(Returns.linmod$yhatfdobj$coefs, Returns.linmod$yhatfdobj$basis)
plot.fd(Yhat_fd)

beta0mat = eval.fd(Returns.times, Returns.linmod$beta0estfd)
par(mfrow=)
plot(beta0mat, type="l", xlab='Time (h)', ylab='Price', font.lab = 2, cex.lab=1.3)


b1_s <- fd(Returns.linmod$beta1estbifd$coefs, Returns.linmod$beta1estbifd$sbasis)
b1_t <- fd(Returns.linmod$beta1estbifd$coefs, Returns.linmod$beta1estbifd$tbasis)
plot.fd(b1_s)
plot.fd(b1_t)

# checking forecasting setup
integral_estimate <- inprod(b1_s,LastYear)
Forecasted_next_year <- integral_estimate+beta0mat[1:24]

Yhat_mat <- eval.fd(Returns.times, Yhat_fd)
quartz()
par(mfrow=c(4,2))
for (i in 1:8) {
  matplot(EquityReturnsMat[,i+1], type="l", ylim=c(0,400), main=paste(i), col="blue")
  lines(Forecasted_next_year[,i+1], col="green")
  lines(Yhat_mat[,i], col="red")
}
dev.off()

# forecast for test dataset
prev_curve = EquityReturnsMat[,50]
par(mfrow= c(4,2))
forecast_mat = matrix(data=NA, nrow = 24, ncol = 8)
for (i in 1:8) {
  prev_curve_fd = smooth.basis(ReturnsDayTime, prev_curve, D2fdPar)$fd
  
  f_integral_estimate <- inprod(b1_s,prev_curve_fd)
  forecasted_test_years <- f_integral_estimate+beta0mat
  forecast_mat[,i] = forecasted_test_years
  plot.fd(smooth.basis(ReturnsDayTime, forecasted_test_years, D2fdPar)$fd, col="red", ylim=c(50,400), main=paste("Curve", i), xlab="Time (h)", ylab="Price", cex.lab=1.2, font.lab=2, cex.main=2)
  lines(0:23, TestingEquityReturnsMat[,i])
  
  prev_curve = forecasted_test_years
}

# MAE
mean(abs(forecast_mat-TestingEquityReturnsMat))

# MAPE
mean(abs(forecast_mat-TestingEquityReturnsMat)/TestingEquityReturnsMat)

# MSE
mean((forecast_mat-TestingEquityReturnsMat)^2)

error_by_curve = matrix(nrow=2, ncol = 8)

for (i in 1:ncol(forecast_mat)) {
  error_by_curve[1,i] = mean(abs(forecast_mat[,i]-TestingEquityReturnsMat[,i]))
  error_by_curve[2,i] = mean((forecast_mat[,i]-TestingEquityReturnsMat[,i])^2)
}

write.table(error_by_curve,file='functionalfunctional_amodel_pred.csv',sep=',')








