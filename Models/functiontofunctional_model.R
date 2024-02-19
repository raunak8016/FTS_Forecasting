library(fda)
library(tidyverse)

equity_returns_matrix_name = "2020_SPY_returns_2020-08-03_2020-09-30_matrix"

# data processing
mat =load(paste("ProcessedRData/", equity_returns_matrix_name, ".RData", sep=""))
mat=as.matrix(returns_matrix)

dim(returns_matrix)

EquityReturnsMat = as.matrix(returns_matrix)


# generate functional time series

# number of time points in a day (10 minute intervals over trading hours)
nbasis = 40
norder = 4

ReturnsDayTime = 0:39;
ReturnsDayRng = c(0,39);

ReturnsBasis = create.bspline.basis(ReturnsDayRng, nbasis, norder)

D2fdPar = fdPar(ReturnsBasis, lambda=0.0001)

EquityReturnsMatfd = smooth.basis(ReturnsDayTime, EquityReturnsMat, D2fdPar)$fd

# view fit of each functional time series curve

# plotfit.fd(EquityReturnsMat, ReturnsDayTime, EquityReturnsMatfd)

# Set up regression coefficients

nbasis_regression = 23
ReturnsRng = c(0,39)

ReturnsBetaBasis = create.bspline.basis(ReturnsRng,nbasis_regression, norder=4)

ReturnsBeta0Par = fdPar(ReturnsBetaBasis, 2, 1e-10)

ReturnsBeta1fd  = bifd(matrix(0,23,23), ReturnsBetaBasis, ReturnsBetaBasis)

ReturnsBeta1Par = bifdPar(ReturnsBeta1fd, 2, 2, 1e3, 1e3)

ReturnsBetaList = list(ReturnsBeta0Par, ReturnsBeta1Par)

#  Define the response and explanatory vars
NextYear = EquityReturnsMatfd[2:ncol(EquityReturnsMat)] # Y(t)
LastYear = EquityReturnsMatfd[1:(ncol(EquityReturnsMat)-1)] # X(t)

#  Function to Functional Linear Model

Returns.linmod = linmod(NextYear, LastYear, ReturnsBetaList)

Returns.times = seq(0, 39, 1)
Returns.beta1mat = eval.bifd(Returns.times, Returns.times, Returns.linmod$beta1estbifd)

persp(Returns.times, Returns.times, Returns.beta1mat,
      xlab="time", ylab="time",zlab="beta(s,t)",
      cex.lab=1.5,cex.axis=1.5)


# setting up prediction

# predict next year
last_year = EquityReturnsMat[,ncol(EquityReturnsMat)]

lastYearNew <- smooth.basis(c(0:39),last_year,ReturnsBasis)
lastYearNew <- lastYearNew$fd
lastYearmat <- eval.fd(c(0:39), lastYearNew)

# estimate integral of beta1(s,t)*x_i(t) using inner product
dim(Returns.beta1mat)
dim(lastYearmat)
integral_estimate <-  Returns.beta1mat %*% lastYearmat

# add beta_0 to integral estimate
Returns.beta0 = eval.fd(Returns.times, Returns.linmod$beta0estfd)
dim(integral_estimate)
dim(Returns.beta0)
function_output <- Returns.beta0 + integral_estimate

plot(EquityReturnsMatfd)
plot(function_output, type="l")









