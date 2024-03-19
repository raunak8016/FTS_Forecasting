library(fda)
library(tidyverse)


# data processing
vec = read.csv("ProcessedCSVData/PJME_prices.csv")
vec =as.vector(vec[,2])
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

persp(Returns.times, Returns.times, Returns.beta1mat,
      xlab="Time (h)", ylab="Time (h)",zlab="beta(s,t)",
      cex.lab=1,cex.axis=1.5, theta=-20)


# setting up prediction

Yhat_fd <- fd(Returns.linmod$yhatfdobj$coefs, Returns.linmod$yhatfdobj$basis)
plot.fd(Yhat_fd)

beta0mat = eval.fd(Returns.times, Returns.linmod$beta0estfd)
plot(beta0mat, type="l", xlab="Time (h)", ylab="Price")


b1_s <- fd(Returns.linmod$beta1estbifd$coefs, Returns.linmod$beta1estbifd$sbasis)
b1_t <- fd(Returns.linmod$beta1estbifd$coefs, Returns.linmod$beta1estbifd$tbasis)
plot.fd(b1_s)
plot.fd(b1_t)

# checking forecasting setup
integral_estimate <- inprod(b1_s,LastYear)
Forecasted_next_year <- integral_estimate+beta0mat[1:24]

Yhat_mat <- eval.fd(Returns.times, Yhat_fd)

for (i in 1:5) {
  matplot(EquityReturnsMat[,i], type="l", ylim=c(23000,50000), main=paste(i), col="blue")
  lines(Forecasted_next_year[,i+1], col="green")
  lines(Yhat_mat[,i], col="red")
}


# forecast for test dataset
prev_curve = EquityReturnsMat[,50]
par(mfrow= c(2,4))
for (i in 1:8) {
  prev_curve_fd = smooth.basis(ReturnsDayTime, prev_curve, D2fdPar)$fd

  f_integral_estimate <- inprod(b1_s,prev_curve_fd)
  forecasted_test_years <- f_integral_estimate+beta0mat
  
  plot.fd(smooth.basis(ReturnsDayTime, forecasted_test_years, D2fdPar)$fd, col="red", ylim=c(20000,50000), main=paste("Forecast Num:", i), xlab="Time (h)", ylab="Price")
  lines(TestingEquityReturnsMat[,i])
  
  prev_curve = TestingEquityReturnsMat[,i]
}









