library(fda)
library(tidyverse)

equity_returns_matrix_name = "2020_SPY_cidr_2020-07-06_2020-09-02_matrix"

# data processing
mat =load(paste("ProcessedRData/", equity_returns_matrix_name, ".RData", sep=""))
mat=as.matrix(returns_matrix)

dim(returns_matrix)

EquityReturnsMat = as.matrix(returns_matrix)

# training set
matplot(EquityReturnsMat[,1:38], type="l", xlab="Time (15*x min)", ylab="CIDR (%)")

# testing set
matplot(EquityReturnsMat[,39:43], type="l")

TestingEquityReturnsMat = EquityReturnsMat[,39:43]

EquityReturnsMat = EquityReturnsMat[,1:38]

matplot(EquityReturnsMat, type="l")

T_stationary(EquityReturnsMat)
# generate functional time series

# number of time points in a day (10 minute intervals over trading hours)
nbasis = 40
norder = 4

ReturnsDayTime = 0:39;
ReturnsDayRng = c(0,39);

ReturnsBasis = create.bspline.basis(ReturnsDayRng, nbasis, norder)

D2fdPar = fdPar(ReturnsBasis, lambda=0.8)

EquityReturnsMatfd = smooth.basis(ReturnsDayTime, EquityReturnsMat, D2fdPar)$fd

# view fit of each functional time series curve

# plotfit.fd(EquityReturnsMat, ReturnsDayTime, EquityReturnsMatfd)

# Set up regression coefficients

nbasis_regression = 40
ReturnsRng = c(0,39)

ReturnsBetaBasis = create.bspline.basis(ReturnsRng,nbasis_regression, norder=4)

ReturnsBeta0Par = fdPar(ReturnsBetaBasis, 2, 1e-10)

ReturnsBeta1fd  = bifd(matrix(0,40,40), ReturnsBetaBasis, ReturnsBetaBasis)

ReturnsBeta1Par = bifdPar(ReturnsBeta1fd, 2, 2, 1e3, 1e3)

ReturnsBetaList = list(ReturnsBeta0Par, ReturnsBeta1Par)

#  Define the response and explanatory vars
NextYear = EquityReturnsMatfd[2:ncol(EquityReturnsMat)] # X_(i+1)(t)
LastYear = EquityReturnsMatfd[1:(ncol(EquityReturnsMat)-1)] # X_i(t)

#  Function to Functional Linear Model

Returns.linmod = linmod(NextYear, LastYear, ReturnsBetaList)

Returns.times = seq(0, 39, 1)
Returns.beta1mat = eval.bifd(Returns.times, Returns.times, Returns.linmod$beta1estbifd)

persp(Returns.times, Returns.times, Returns.beta1mat,
      xlab="time", ylab="time",zlab="beta(s,t)",
      cex.lab=1.5,cex.axis=1.5)


# setting up prediction

Yhat_fd <- fd(Returns.linmod$yhatfdobj$coefs, Returns.linmod$yhatfdobj$basis)
plot.fd(Yhat_fd)

beta0mat = eval.fd(Returns.times, Returns.linmod$beta0estfd)
plot(beta0mat, type="l")


b1_s <- fd(Returns.linmod$beta1estbifd$coefs, Returns.linmod$beta1estbifd$sbasis)
b1_t <- fd(Returns.linmod$beta1estbifd$coefs, Returns.linmod$beta1estbifd$tbasis)
plot.fd(b1_s)
plot.fd(b1_t)

# checking forecasting setup
integral_estimate <- inprod(b1_s,LastYear)
Forecasted_next_year <- integral_estimate+beta0mat[1:40]

Yhat_mat <- eval.fd(Returns.times, Yhat_fd)

for (i in 1:8) {
  matplot(EquityReturnsMat[,i+1], type="l", ylim=c(-2,2), main=paste(i), col="blue")
  lines(Forecasted_next_year[,i+1], col="green")
  lines(Yhat_mat[,i], col="red")
}


# forecast for test dataset
prev_curve = EquityReturnsMat[,38]

for (i in 1:8) {
  prev_curve_fd = smooth.basis(ReturnsDayTime, prev_curve, D2fdPar)$fd
  plot.fd(prev_curve_fd)
  
  f_integral_estimate <- inprod(b1_s,prev_curve_fd)
  forecasted_test_years <- f_integral_estimate+beta0mat
  
  matplot(TestingEquityReturnsMat[,i], type="l", ylim=c(-2,2), main=paste("Forecast", i))
  lines(forecasted_test_years, col="red")
  
  prev_curve = TestingEquityReturnsMat[,i]
}








