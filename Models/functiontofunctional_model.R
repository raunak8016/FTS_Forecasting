library(fda)
library(tidyverse)
library(lubridate)
library(tseries)



# data processing
prices = read.csv("ProcessedCSVData/2020_AAPL_november_returns.csv", stringsAsFactors = FALSE)
prices$returns <- as.numeric(prices$returns)



summary(prices)

any(is.na(prices))

matplot(prices$returns, type="l")

prices$TimeStamp <- ymd_hms(prices$TimeStamp)
prices$Date <- as.Date(prices$TimeStamp)
prices$Time <- format(prices$TimeStamp, "%H:%M:%S")

prices_matrix = matrix(0, length(unique(prices$Time)), length(unique(prices$Date)))

# iterate over each day
for (i in 1:ncol(prices_matrix)) {
  # iterate over each 10-minute interval
  for (j in 1:nrow(prices_matrix)) {
    prices_matrix[j, i] <- prices$returns[(i-1)*nrow(prices_matrix) + j]
  }
}

matplot(prices_matrix, type="l")

save(prices_matrix, file="ProcessedRData/prices_matrix.RData") 

adf.test(prices$returns)

dim(prices_matrix)

AAPLNovPricesMat = as.matrix(prices_matrix)

length(unique(prices$Date))

dimnames(AAPLNovPricesMat)[[2]] <- paste('b', unique(prices$Date), sep='')


# plot specific functions




# smooth the intra-day pricing observations

nbasis = 40
norder = 4

PriceTime = 0:39;
PricesRng = c(0,39);

PriceBasis = create.bspline.basis(PricesRng, nbasis, norder)

D2fdPar = fdPar(PriceBasis, lambda=0.5)

AAPLNovPricesMatfd = smooth.basis(PriceTime, AAPLNovPricesMat, D2fdPar)$fd


# view indivudal fit

plotfit.fd(AAPLNovPricesMat, PriceTime, AAPLNovPricesMatfd)

# Set up regression coefficients

nbasis     = 23
SwedeRng   = c(0,39)
PricesBetaBasis = create.bspline.basis(SwedeRng,nbasis, norder=4)

PricesBeta0Par = fdPar(PricesBetaBasis, 2, 1e-10)
# PricesBeta0Par = fdPar(PriceBasis, 2, 1e-5)

PricesBeta1fd  = bifd(matrix(0,23,23), PricesBetaBasis, PricesBetaBasis)
# PricesBeta1fd  = bifd(matrix(0,40,40), PriceBasis, PriceBasis)

PricesBeta1Par = bifdPar(PricesBeta1fd, 2, 2, 1e3, 1e3)

PricesBetaList = list(PricesBeta0Par, PricesBeta1Par)

#  Define the dependent and independent variable objects

NextYear = AAPLNovPricesMatfd[2:19] # Y(t)
LastYear = AAPLNovPricesMatfd[1:18] # X(t)

#  Do the regression analysis

Prices.linmod = linmod(NextYear, LastYear, PricesBetaList)

Prices.times = seq(1, 39, 1)
Prices.beta1mat = eval.bifd(Prices.times, Prices.times, Prices.linmod$beta1estbifd)

persp(Prices.times, Prices.times, Prices.beta1mat,
      xlab="time", ylab="time",zlab="beta(s,t)",
      cex.lab=1.5,cex.axis=1.5)

# setting up prediction

# predict next year
last_year = AAPLNovPricesMat[,19]

lastYearNew <- smooth.basis(c(0:39),last_year,PriceBasis)
lastYearNew <- lastYearNew$fd
lastYearmat <- eval.fd(c(1:39), lastYearNew)

# calculate integral of beta1(s,t)*x_i(t) using inner product
dim(Prices.beta1mat)
dim(lastYearmat)
integral_estimate <-  Prices.beta1mat %*% lastYearmat

# add beta_0 to integral estimate
Prices.beta0 = eval.fd(Prices.times, Prices.linmod$beta0estfd)
dim(integral_estimate)
dim(Prices.beta0)
function_output <- Prices.beta0 + integral_estimate

plot(AAPLNovPricesMatfd)
plot(function_output, type="l")









